
(cl:in-package :cl-polyhedral)

(defun map-split (split-with function &rest sequences)
  "A helper function merging A B C -> A, B, C"
  (apply
   #'concatenate
   'string
   (butlast
    (alexandria:flatten
     (apply
      #'map
      'list
      #'(lambda (&rest args)
	  (list
	   (apply function args)
	   split-with))
      sequences)))))

(declaim (inline unique-identifier))
(defun unique-identifier ()
  (string-downcase (gensym "id")))

(declaim (ftype (function (Kernel) string) get-params-str))
(defun get-params-str (kernel)
  "Returns a string representing a list of constants in the kernel
e.g.: [A, B, C]"
  (declare (type Kernel kernel)
	   (optimize (speed 3)))
  (let ((consts (kernel-constants kernel)))
    (with-output-to-string (out)
      (format out "[")
      (loop for const across consts
	    for n fixnum upfrom 0
	    for lastp = (= (1- (array-total-size consts)) n) do
	      (format out "~a" (buffer-name const))
	      (unless lastp
		(format out ", ")))
      (format out "]"))))

(declaim (ftype (function (Instruction Kernel) string) get-related-domains))
(defun get-related-domains (instruction kernel)
  "Helper to get instruction related domains"
  (declare (type Kernel kernel)
	   (type Instruction instruction)
	   (optimize (speed 3)))
  (with-output-to-string (out)
    (format out "[")
    (loop with count fixnum = 1
	  for domain across (kernel-domains kernel) do
	    (loop for iname in (inst-depends-on instruction)
		  if (eql (domain-subscript domain) iname) do
		    (when (not (= count 1))
		      (format out ", "))
		    (incf count)
		    (format out "~a" iname)))
    (format out "]")))

(declaim (ftype (function (List Kernel) string) get-instructions-domains))
(defun get-instructions-domains (Instructions Kernel)
  "Helper to get the domain related to instructions(s) in the ISL Format.
e.g.: 0 <= i <= n and 0 <= j <= n"
  (declare (optimize (speed 3))
	   (type List Instructions)
	   (type Kernel Kernel))
  (with-output-to-string (out)
    (loop with count fixnum = 1
	  for domain across (kernel-domains kernel) do
	    (dolist (inst instructions)
	      (dolist (dep (inst-depends-on inst))
		(when (not (= count 1))
		  (format out " and "))
		(incf count)
		(flet ((instcond ()
			 (if (inst-conds inst)
			     (format nil " and ~a " (map-split " and " #'lisp->isl (inst-conds inst)))
			     " ")))
		  (if (= 1 (domain-by domain))
		      (format out "~a <= ~a <= ~a~a"
			      (domain-from domain)
			      (domain-subscript domain)
			      (domain-to domain)
			      (instcond))
		      (let ((ident (unique-identifier)))
			(format out "exists ~a: ~a = ~a~a + ~a and ~a <= ~a <= ~a~a"
				ident
				(domain-subscript domain)
				(domain-by domain)
				ident
				(domain-from domain)
				(domain-from domain)
				(domain-subscript domain)
				(domain-to domain)
				(instcond))))))))))  

(declaim (ftype (function (Kernel) String) Kernel->ISL))
(defun Kernel->ISL (kernel &aux (changed-p nil))
  "Constructs the instructions' domains representation in ISL Syntax Form.
[A, B, C]"
  (declare (type Kernel kernel)
	   (optimize (speed 3)))
  (with-output-to-string (out)
    (format out "~a -> {" (get-params-str kernel))
    
    (loop with icount fixnum = 1
	  for instruction across (kernel-instructions kernel)
	  for name = (format nil "~a~a" ;; e.g.: name=mult[i, j, k]
			     (inst-op instruction)
			     (get-related-domains instruction kernel))
	  for conds = (get-instructions-domains (list instruction) kernel)
	  for count = 1 do
	    (setf changed-p t)
	    (unless (= 1 icount)
	      (format out "; "))
	    (incf icount)
	    (if (string= "" conds)
		(format out "~a" name)
		(format out "~a : ~a" name conds)))
    (unless changed-p
      (format out ":"))
    (format out "}")))

(defun expr-accesses (list-buffer)
  (declare (type list list-buffer))
  (loop for expr in list-buffer
	if (eql (car expr) 'aref)
	  collect (format nil "~a[~a] " (second expr) (map-split ", " #'lisp->isl (third expr)))))

(declaim (ftype (function (Kernel) (values String String)) access-isl-rep))
(defun access-isl-rep (kernel)
  "construct the access relations map in ISL syntax form.
Return: (values may-read may-write)"
  (declare (type kernel kernel)
	   (optimize (speed 3)))
  (let ((params (get-params-str kernel))
	(reads  (make-string-output-stream))
	(writes (make-string-output-stream))
	(wcount 1)
	(rcount 1))
    (declare (type fixnum wcount rcount))
    
    (format reads  "~a -> {" params)
    (format writes "~a -> {" params)
    
    (loop for inst across (kernel-instructions kernel)
	  for ds      = (get-related-domains inst kernel)
	  for tgts    = (expr-accesses (list (inst-target inst)))
	  for sources = (expr-accesses (coerce (inst-sources inst) 'list))
	  for body    = (inst-body inst) do
	    (assert (symbolp (car body)) () "Assertion Failed: car is missed from body: ~a" body)
	    (when (not (eql 'setf (car body)))
	      (if sources
		  ;; Also reads from LHS
		  (setf sources `(,@sources ,@tgts))
		  (setf sources `(,@tgts))))

	    (macrolet ((write-helper (counter target-to io-list &aux (ref (gensym)))
			 `(dolist (,ref ,io-list)
			    (unless (= 1 ,counter)
			      (format ,target-to "; "))
			    (incf ,counter)
			    (format
			     ,target-to
			     "~a~a -> ~a"
			     (inst-op inst)
			     ds
			     ,ref)
			    (let ((conds (get-instructions-domains (list inst) kernel)))
			      (unless (string= "" conds)
				(format ,target-to " : ~a" conds))))))
	      (write-helper wcount writes tgts)
	      (write-helper rcount reads sources)))
    (format reads  "}")
    (format writes "}")
    (values
     (get-output-stream-string reads)
     (get-output-stream-string writes))))

(declaim (ftype (function (isl-ctx Domain Kernel &optional list) t) schedule-tree-isl-rep))
(defun schedule-tree-isl-rep (ctx domain kernel &optional parent-doms)
  "Constructs an original shcedule for dependency analysis"
  (declare (type isl-ctx ctx)
	   (type domain domain)
	   (type kernel kernel)
	   (type list parent-doms)
	   (optimize (speed 3)))

  (let ((parent-doms `(,domain ,@parent-doms))
	(schedule :nothing))
    (loop for inst across (domain-instructions domain)
	  if (instruction-p inst) do
	    ;; Construct Instruction Schedule
	    (let* ((tmp-kernel (make-kernel
				(make-array `(1) :initial-element inst)
				(kernel-domains kernel)
				(kernel-args kernel)
				(kernel-constants kernel)))
		   (inst-dom   (isl-union-set-read-from-str
				ctx
				(Kernel->ISL tmp-kernel)))
		   (inst-schedule (isl-schedule-from-domain inst-dom)))
	      (if (eql schedule :nothing)
		  (setf schedule inst-schedule)
		  (setf schedule (isl-schedule-sequence schedule inst-schedule))))
	  else do
	    ;; Domain
	    (let ((dom-schedule
		    (schedule-tree-isl-rep ctx inst kernel parent-doms)))
	      (if (eql schedule :nothing)
		  (setf schedule dom-schedule)
		  (setf schedule (isl-schedule-sequence schedule dom-schedule)))))
    (let ((insts))
      (loop for inst across (kernel-instructions kernel)
	    if (find (domain-subscript domain) (inst-depends-on inst)) do
	      (push insts inst))
      (let ((partial (partial-schedule-isl (list domain) insts kernel)))
	(when (not (string= partial "[]"))
	  (setf partial (isl-multi-union-pw-aff-read-from-str ctx partial)
		schedule (isl-schedule-insert-partial-schedule ctx partial)))
	(assert (not (eql schedule :nothing))
		()
		"Assertion Failed with schedule != :nothing. Ensure that instructions are properly nested!")

	schedule))))

;; TODO: Optimize this function
(declaim (ftype (function (list list Kernel) (simple-array character (*))) partial-schedule-isl))
(defun partial-schedule-isl (domains instructions kernel)
  "Constructs a partial schedule for ISL rep"
  (declare (type list domains instructions)
	   (type Kernel kernel)
	   (optimize (speed 3)))
  (with-output-to-string (out)
    (princ "[" out)
    (loop with dcount fixnum = 0
	  for domain in domains do
	    (let ((set ""))
	      (loop with icount fixnum = 0
		    for inst in instructions
		    if (find (domain-subscript domain) (inst-depends-on inst)) do
		      (when (not (= icount 0))
			(setf set (format nil "~a; " set)))
		      (incf icount)
		      (setf set (format nil "~a~a~a->[(~a)]" set (inst-op inst) (get-related-domains inst kernel) (domain-subscript domain))))
	      (when (not (string= set ""))
		(when (not (= dcount 0))
		  (princ ", " out))
		(incf dcount)
		(format out "{~a}" set))))
    (princ "]" out)))


