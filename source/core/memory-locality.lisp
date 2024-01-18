
(in-package :cl-polyhedral)

(defun striding-level (kernel instruction iter &aux (total 0))
  (declare (type Instruction instruction)
	   (type symbol iter)
	   (type Kernel kernel)
	   (type fixnum total)
	   (optimize (speed 3)))
  (labels ((fixnum= (x y)
	     (declare (type fixnum x y))
	     (= x y))
	   (ranking (stride-list)
	     (declare (type list stride-list))
	     ;; (20 10 1)
	     ;; (2th-largest 1th-largest 0th-largest)
	     ;; -> Return: (2 1 0)
	     (let ((order (sort (copy-list stride-list) #'<)))
	       (map
		'list
		#'(lambda (x)
		    (declare (type fixnum x))
		    (if (= x 0)
			0 ;; 0 = the best condition in terms of memory-locality
			;; ignoring 0. it is asserted that:
			;; (=
			;;  (length (remove-duplicate stride))
			;;  (length stride)) == nil
			(position x stride-list :test #'fixnum=)))
		order))))
    
    ;; Costs for source is important
    (loop for src across (inst-sources instruction)
	  ;; src is either of:
	  ;;  src = (aref :X (refs))
	  ;;  src = 0.0
	  if (listp src) do
	    ;; -> src is determined as (aref ...)
	    (multiple-value-bind (id refs) (apply #'values (cdr src))
	      (let ((found-buffer
		      (or
		       ;; TODO: Optimize this T_T
		       (find id (coerce (kernel-constants kernel) 'list) :key #'buffer-name)
		       (error "striding-level: The buffer ~a isn't declared." id))))
		(loop with buffer = (if (>= (length (buffer-strides found-buffer)) 1)
					found-buffer
					(error "Cannot be referenced as a tensor; because the buffer ~a is declared as a scalar." found-buffer))
		      for ranked-level in (ranking (buffer-strides buffer))
		      for ref-iter     in refs
		      ;; If ref-iter contains iter, count it as a stride-level
		      ;; ref-iter here could be a or (+ a 1) so first flatten
		      if (or
			  (eql ref-iter iter)
			  (and
			   (listp ref-iter)
			   (find iter (the list (alexandria:flatten ref-iter)))))
			do (incf total (the fixnum ranked-level))))))
    total))

(declaim (ftype (function (Kernel) hash-table) get-loop-stride-counts))
(defun get-loop-stride-counts (kernel)
  (declare (type Kernel kernel))
  (let ((out (make-hash-table :test #'eql)))
    (loop for domain across (kernel-domains kernel) do
      (setf (gethash (domain-subscript domain) out)
	    (loop for inst across (kernel-instructions kernel)
		  if (instruction-p inst)
		    ;; If there's no any matrices -> return 0
		    sum (or (striding-level kernel inst (domain-subscript domain)) 0))))
    out))

(declaim (ftype (function (Kernel &optional Boolean) List) get-best-nesting-orders))
(defun get-best-nesting-orders (kernel &optional display-progress-p)
  "determines the best valid loop nesting order for groupings of domains (all in continuous nest (one sub domain per domain))"
  (declare (optimize (speed 3))
	   (type kernel kernel))
  (let* ((stride-counts (get-loop-stride-counts kernel))
	 (pair-of-stride-counts
	   (loop for key being the hash-keys in stride-counts
		 collect
		 (cons key (gethash key stride-counts))))
	 (stride-order (sort pair-of-stride-counts #'< :key #'cdr))
	 (seen)
	 (nesting-orders))
    (declare (type list seen nesting-orders))
    (when display-progress-p
      (format t "~%Striding Levels:~%")
      (maphash
       #'(lambda (k v)
	   (format t "~a -> ~a~%" k v))
       stride-counts))
    (loop for domain across (kernel-domains kernel)
	  if (null (find (the symbol (domain-subscript domain)) seen :test #'eql)) do
	    (let ((count 0))
	      (declare (type fixnum count))
	      (loop for child across (domain-instructions domain)
		    if (domain-p child) do (incf count))
	      (when (= count 1)
		(let ((order (get-best-nesting-order-on-domains domain stride-order kernel)))
		  (declare (type list order))		  
		  (setf seen (if seen (append seen order) order))
		  (push order nesting-orders)))))
    (reverse nesting-orders)))

(declaim (ftype (function (Domain list list) boolean) can-insert-domain))
(defun can-insert-domain (domain nesting-order consts)
  (declare (type domain domain)
	   (type list nesting-order consts))
  (let ((symbols (remove-duplicates
		  `(,@(get-expr-all-symbols (domain-from domain))
		    ,@(get-expr-all-symbols (domain-to   domain))
		    ,@(get-expr-all-symbols (domain-by   domain))))))
    (dolist (sym symbols)
      (when (not
	     (or
	      (find sym nesting-order)
	      (find sym consts :key #'buffer-name)))
	(return-from can-insert-domain)))
    t))	 
  
(declaim (ftype (function (Domain List Kernel) List) get-best-nesting-order-on-domains))
(defun get-best-nesting-order-on-domains (parent-domain stride-order kernel)
  "Helper for nesting order"
  (declare (type Domain parent-domain)
	   (type list stride-order)
	   (type kernel kernel)
	   (optimize (speed 3)))
  (let* ((nesting-order nil)
	 (kernel-constants (coerce (kernel-constants kernel) 'list))
	 (parent parent-domain)
	 (child (or
		 (find-if
		  #'domain-p
		  (coerce (domain-instructions parent) 'list))
		 :none))
	 (can-reorder nil))
    (declare (type list can-reorder))

    (if (and (eql child :none)
	     (= (array-total-size (domain-instructions parent)) 1))
	(setf can-reorder (list parent))
	(setf nesting-order (list (domain-subscript parent))))

    ;; Keep Reordering while subdomains exist
    (loop while (not (eql child :none)) do
      (if (= (array-total-size (domain-instructions parent)) 1)
	  (setf can-reorder (if can-reorder (append can-reorder (list child)) (list child)))
	  (progn
	    (loop while (> (length can-reorder) 0)
		  for best-stride = (find-if
				     ;; Reading pair from lower levels -> higher levels
				     #'(lambda (pair &aux (iter (car pair)))
					 (loop named reorder-loop
					       for domain in can-reorder
					       if (and
						   (eql (domain-subscript domain) iter)
						   (can-insert-domain domain nesting-order kernel-constants))
						 do (setf can-reorder (delete domain can-reorder))
						    (return-from reorder-loop iter)))
				     stride-order)
		  do (setf nesting-order (if nesting-order (append nesting-order (list (car best-stride))) (list (car best-stride)))))
	    ;; add children in the right order
	    (loop for sub across (domain-instructions parent)
		  if (domain-p sub) do
		    (setf nesting-order (if nesting-order (append nesting-order (list (domain-subscript sub))) (list (domain-subscript sub)))))))
      (setf parent child
	    child (or (find-if #'domain-p (coerce (domain-instructions parent) 'list)) :none)))

    ;; Done, add remaining parent and rest in reorder set
    (loop named reminder
	  while (> (length can-reorder) 0)
	  for best-stride = (find-if
			     #'(lambda (pair &aux (iter (car pair)))
				 (loop named reorder-loop
				       for domain in can-reorder
				       if (and
					   (eql (domain-subscript domain) iter)
					   (can-insert-domain domain nesting-order kernel-constants))
					 do (setf can-reorder (delete domain can-reorder))
					    (return-from reorder-loop iter)))
			     stride-order)
	  if (null best-stride)
	    do (return-from reminder)
	  else
	    do (setf nesting-order (if nesting-order (append nesting-order (list (car best-stride))) (list (car best-stride)))))

    (unless (find (domain-subscript parent) nesting-order :test #'eql)
      (setf nesting-order (if nesting-order (append nesting-order (list (domain-subscript parent))) (list (domain-subscript parent)))))
    nesting-order))

(defun reorder-band! (band loop-orders ctx)
  (declare (type list loop-orders)
	   (type isl-ctx ctx))
  (macrolet ((% (&rest args) `(foreign-funcall ,@args)))
    (let* ((sched
	     (%"isl_schedule_node_band_get_partial_schedule" :pointer band :pointer))
	   (sched-str
	     (%"isl_multi_union_pw_aff_to_str" :pointer sched :string))
	   (sched-copy
	     (%"isl_multi_union_pw_aff_read_from_str" :pointer (isl-ctx-ptr ctx) :string sched-str :pointer))
	   (n
	     (%"isl_multi_union_pw_aff_dim"
	       :pointer sched-copy
	       :int 3
	       :int))
	   (count 0)
	   (upas (loop for i upfrom 0 below n
		       collect
		       (cons nil (%"isl_multi_union_pw_aff_get_union_pw_aff" :string sched-copy :int i :pointer))))
	   (upa-str (%"isl_union_pw_aff_to_str" :pointer (cdr (car upas)) :string))
	   (new-upa (%"isl_union_pw_aff_read_from_str" :pointer (isl-ctx-ptr ctx) :string upa-str :pointer)))
      (%"isl_multi_union_pw_aff_set_union_pw_aff" :pointer sched :int 0 :pointer new-upa :void)

      (loop named top-loop for ordering in loop-orders do
	(loop for iname in ordering do
	  (loop for i upfrom 0
		for (used . upa) in upas do
		  (when used (return-from top-loop))
		  (let* ((upa-str (%"isl_union_pw_aff_to_str" :pointer upa :string))
			 (upa-iname-str (car (last (cl-ppcre:split "->" upa-str))))
			 (upa-iname-str (car (cl-ppcre:split ":" upa-iname-str))))
		    (when (cl-ppcre:scan (format nil "~(~a~)" iname) upa-iname-str)
		      (let ((new-upa (%"isl_union_pw_aff_read_from_str" :pointer (isl-ctx-ptr ctx) :string upa-str :pointer)))
			(%"isl_multi_union_pw_aff_set_union_pw_aff" :pointer sched :int count :pointer new-upa :void)
			(incf count)
			(setf (nth i upas) (cons t upa))))))))

      (loop for (used . upa) in upas do
	(when used
	  (return-from reorder-band!))
	(let* ((upa-str (%"isl_union_pw_aff_to_str" :pointer upa :string))
	       (new-upa (%"isl_union_pw_aff_read_from_str" :pointer (isl-ctx-ptr ctx) :pointer upa-str :pointer)))
	  (%"isl_multi_union_pw_aff_set_union_pw_aff" :pointer sched :int count :pointer new-upa :void)
	  (incf count))))))

(defun apply-reorder-schedule-loops! (schedule ctx loop-orders)
  (declare (type isl-ctx ctx)
	   (type list loop-orders)
	   (optimize (speed 3)))
  (let* ((root
	   (foreign-funcall "isl_schedule_get_root"
			    :pointer schedule :pointer))
	 (node root)
	 (next-nodes nil))
    
    (flet ((isl-schedule-node-get-child (schedule i)
	     (foreign-funcall "isl_schedule_node_get_child"
			      :pointer schedule
			      :int i
			      :pointer))
	   (isl-schedule-node-get-type (obj)
	     (foreign-funcall "isl_schedule_node_get_type"
			      :pointer obj
			      :int)))
      (loop named find-node
	    while (> (the fixnum (%isl-schedule-node-n-children node)) 0) do
	      (loop named band-loop
		    for i fixnum upfrom 0 below (the fixnum (%isl-schedule-node-n-children node))
		    for band = (isl-schedule-node-get-child node i)
		    if (eql (isl-schedule-node-get-type band) 0)
		      do (reorder-band! band loop-orders ctx)
			 (setf next-nodes (if next-nodes
					      (append next-nodes (list (isl-schedule-node-get-child band 0)))
					      (list (isl-schedule-node-get-child band 0))))
			 (return-from band-loop)
		    else
		      do (setf next-nodes (if next-nodes (append next-nodes (list band)) (list band))))
	      (when (= (length next-nodes) 0)
		(return-from find-node))
	      (setf node (car (last next-nodes)))
	      (setf next-nodes (butlast next-nodes)))))
  nil)

