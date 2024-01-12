
(in-package :cl-polyhedral/test)

;; ~~ Utils ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; TODO: Supress numcl:array printing when error is occured!

(defmacro benchmark (n &body body)
  (alexandria:with-gensyms (t1 t2 i)
    `(let ((,t1 (get-internal-real-time)))
       (dotimes (,i ,n) ,@body)
       (let ((,t2 (get-internal-real-time)))
	 (float
	  (/
	   (float
	    (/ (- ,t2 ,t1) internal-time-units-per-second))
	   ,n))))))

(defun compare-results (error-rate allow-mse-error-rate naive-time total-time iters n-flop)
  (let* ((accuracy-p  (<= error-rate allow-mse-error-rate))
	 (improved-p (<= total-time naive-time))
	 (format-error-rate
	   (if accuracy-p
	       #'cl-ansi-text:cyan
	       #'cl-ansi-text:magenta))
	 (format-speed
	   (if improved-p
	       #'cl-ansi-text:cyan
	       #'cl-ansi-text:magenta)))
	 

    (format
     t
     "        | ~a~a | ~a(s) (~a~a(s)) | ~a GFlops | ~a"
     (format nil "error=~a" error-rate)
     (funcall format-error-rate (if accuracy-p "(ok)" "(ng)"))
     (format nil "time=~a" total-time)
     (funcall format-speed
	      (if improved-p
		  "improve↑: -"
		  "degrade↓: +"))
     (format nil "~a" (abs (- naive-time total-time)))
     (float
      (/ (* (apply #'* iters) n-flop total-time)) ;; Flops -> GFlops
      1.0e10)
     (funcall format-speed (format nil "~a%" (* 100 (float (/ naive-time total-time))))))
    (unless accuracy-p (return-from compare-results :incorrect))
    (unless improved-p (return-from compare-results :get-slower))
    :improved))

(defun MSE (x-use-nth y-use-nth X Y)
  ;; use-nth = 0 -> numcl array
  ;; use-nth = 1 -> pointer  
  (let ((X (ecase x-use-nth
	     (0 (numcl:reshape X `(-1)))
	     (1 (numcl:asarray X))))
	(Y (ecase y-use-nth
	     (0 (numcl:reshape Y `(-1)))
	     (1 (numcl:asarray Y)))))
    ;; Supress numcl style-warning
    #+sbcl(declare (sb-ext:muffle-conditions cl:style-warning))
;;    (print X)
;;    (print Y)
    (numcl:mean (numcl:- X Y))))

(defun copy-helper (use-nth X)
  (ecase use-nth
    (0 (numcl:copy X))
    (1 (numcl:copy (numcl:asarray X)))))

(defmacro define-bench ((name n-iters &key (allow-mse-error 1e-5) (n 10) (init-nth -1)) initializer (use-nth naive-impl n-flop) &rest test-pair)
  "Defines a pair of benchmarking and accuracy testing.
- name: the test is named after name
- allow-mse-error
- initializer: a lambda function generating array for the testing function, returning ((arg1-array arg1-storage-pointer) (arg2-array arg2-storage-pointer) ...)
    - initializer reinit: ...
    - reinit=t, recreates a pair of (arr pointer)
- use-nth: 0 to use array, 1 to use storage-pointer
- naive-impl: a lambda function operating a test.
- test-pair: a pair of (use-nth naive-impl n-flop) but the accuracy is measured between the returned array and naive-impl's one.
- init-nth: the nth argument is initialized again in each test. (avoiding reduce to produce a wrong result)
"
  (declare (ignore n-flop))
  (flet ((def-helper (initial-p name use-nth1 func)
	   (alexandria:with-gensyms (initialized-args compare-to args result error total tmp nth)
	     `(defun ,name ,@(if initial-p
				 `((,initialized-args))
				 `((,compare-to ,initialized-args)))
	        ;; Initial-p=T   -> (values result error benchmark-result)
		;; Initial-p=NIL -> (values result benchmark-result)
		;; First, measure the accuracy. (to avoid destructive ops)
		(let* ((,args
			 (loop for ,tmp in ,initialized-args
			       for ,nth upfrom 0
			       if (= ,init-nth ,nth)
				 collect (nth ,use-nth1 (funcall ,initializer t))
			       else
				 collect (nth ,use-nth1 ,tmp)))
		       (,result  (copy-helper ,use-nth1 (apply #',func ,args)))
		       ;; MSE Error is measured only after initial computation was finished.
		       ,@(when (not initial-p)
			   `((,error   (MSE ,use-nth ,use-nth1 ,compare-to ,result))))
		       (,total   (benchmark ,n (apply #',func ,args))))
		  ,(if initial-p
		       `(values ,result ,total)
		       `(values ,result ,error ,total)))))))
    (let ((naive-name
	    (gensym (symbol-name naive-impl)))
	  (tmp-names
	    (map
	     'list
	     (alexandria:compose #'gensym #'symbol-name #'second)
	     test-pair)))
      (alexandria:with-gensyms (naive-result naive-time default-args)
	`(progn
	   ;; Initial
	   ,(def-helper t naive-name use-nth naive-impl)
	   ,@(loop for defined-as in tmp-names
		   for packs in test-pair
		   for use-nth = (first  packs)
		   for func    = (second packs)
		   collect
		   (def-helper nil defined-as use-nth func))
	   (let ((,default-args (funcall ,initializer)))
	     (multiple-value-bind (,naive-result ,naive-time)
		 (,naive-name ,default-args)
	       (deftest ,name
		 ,@(loop for packs in test-pair
			 for defined-as in tmp-names
			 for func-name = (second packs)
			 for n-flop    = (third packs)
			 collect
			 `(testing ,(symbol-name func-name)
			    (multiple-value-bind (_ error-rate total-time)
				(,defined-as ,naive-result ,default-args)
			      (declare (ignore _))
			      (let ((result (compare-results
					     error-rate
					     ,allow-mse-error
					     ,naive-time
					     total-time
					     ',n-iters
					     ,n-flop)))
				(ok (eql result :improved))))))))))))))


;; ~~ Utils (Random Generators) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defun make-random-initializer (&rest shapes)
  #'(lambda (&optional recall)
      (if recall
	  (multiple-value-list (numcl:zeros (car (last shapes)) :type 'single-float))
	  (loop for shape in shapes
		for nth upfrom 0
		if (= nth 2)
		  collect (multiple-value-list (numcl:zeros shape :type 'single-float))
		else
		  collect
		  (multiple-value-list (numcl:uniform -3.0 3.0 shape 'single-float))))))

(setf lparallel:*kernel* (lparallel:make-kernel (cl-cpus:get-number-of-processors)))

;; ~~ Gemm 8x8 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(progn
  (define-poly-func %gemm-8x8-lisp-poly
      ((:X `(8 8) :float) (:Y `(8 8) :float) (:Z `(8 8) :float))
      (:backend :lisp :tile nil :verbose 0)
    (for (i 8)
	 (for (j 8)
	      (for (k 8)
		   (incf (aref :Z i k) (* (aref :X i j) (aref :Y j k)))))))

  (defun gemm-8x8-lisp-poly (X Y Z)
    (%gemm-8x8-lisp-poly X Y Z)
    Z))

(defun gemm-8x8-lisp-naive (X Y Z)
  (dotimes (i 8)
    (dotimes (j 8)
      (dotimes (k 8)
	(incf (aref Z i k) (* (aref X i j) (aref Y j k))))))
  Z)

(defun gemm-8x8-lisp-optimized (X Y Z)
  (dotimes (i 8)
    (dotimes (j 8)
      (dotimes (k 8)
	(incf (aref Z i k) (* (aref X i j) (aref Y j k))))))
  Z)

(defun gemm-8x8-numcl (X Y Z)
  (numcl:matmul X Y Z)
  Z)

(define-bench (gemm-8x8 (8 8 8) :allow-mse-error 0 :n 10000 :init-nth 2)
	      (make-random-initializer `(8 8) `(8 8) `(8 8))
    (0 gemm-8x8-lisp-naive     2)
    (1 gemm-8x8-lisp-poly      2)
    (0 gemm-8x8-lisp-optimized 2)
    (0 gemm-8x8-numcl          2))

;; ~~ Gemm 256x256 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(progn
  (define-poly-func %gemm-256x256-lisp-poly
      ((:X `(256 256) :float) (:Y `(256 256) :float) (:Z `(256 256) :float))
      (:backend :lisp :tile nil :verbose 0)
    (for (i 256)
	 (for (j 256)
	      (for (k 256)
		   (incf (aref :Z i k) (* (aref :X i j) (aref :Y j k)))))))

  (defun gemm-256x256-lisp-poly (X Y Z)
    (%gemm-256x256-lisp-poly X Y Z)
    Z))

(defun gemm-256x256-lisp-naive (X Y Z)
  (dotimes (i 256)
    (dotimes (j 256)
      (dotimes (k 256)
	(incf (aref Z i k) (* (aref X i j) (aref Y j k))))))
  Z)

(defun gemm-256x256-lisp-optimized (X Y Z)
  (dotimes (i 256)
    (dotimes (j 256)
      (dotimes (k 256)
	(incf (aref Z i k) (* (aref X i j) (aref Y j k))))))
  Z)

(defun gemm-256x256-numcl (X Y Z)
  (numcl:matmul X Y Z)
  Z)

(define-bench (gemm-256x256 (256 256 256) :allow-mse-error 0 :n 10 :init-nth 2)
	      (make-random-initializer `(256 256) `(256 256) `(256 256))
    (0 gemm-256x256-lisp-naive     2)
    (1 gemm-256x256-lisp-poly      2)
    (0 gemm-256x256-lisp-optimized 2)
    (0 gemm-256x256-numcl          2))


