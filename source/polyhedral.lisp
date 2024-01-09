
(cl:in-package :cl-polyhedral)

(defparameter *verbose* nil "")

(defun run-polyhedral
    (kernel
     &key
       (verbose *verbose*)
       (stream t)
       (threads nil))
  "Gains the optimized kernel obtained from transforming the given kernel using Polyhedral Model.
Does the following:
- 1. Creates a context and space for ISL
- 
"
  (declare (type Kernel kernel))
  
  (with-isl-ctx ctx
    (let* ((initial-problem (Kernel->ISL kernel))
	   (instruction     (cl-isl:isl-union-set-read-from-str ctx initial-problem)))
      (when verbose
	(format stream "Initial Problem: ~a~%" initial-problem))
      
      (print instruction))))

;; Running example
#+(or)
(run-polyhedral
 (make-kernel-from-dsl
  (list
   (make-buffer :X `(10 10) :FLOAT)
   (make-buffer :Y `(10 10) :FLOAT)
   (make-buffer :Z `(10 10) :FLOAT))
  `(for (i 10)
	(for (j 0 1)
	     (when (> j 0)
	       (setf (aref :X i j) (aref :Y i j)))
	     (setf (aref :X i) 0)))
  `(for (i 0 10 1)
	(for (j 0 10 1)
	     (setf (aref :X i j) (+ (aref :Y i j) (aref :Z i j))))))
 :verbose t)

;; Gemm
#+(or)
(run-polyhedral
 (make-kernel-from-dsl
  (list
   (make-buffer :X `(10 10) :FLOAT)
   (make-buffer :Y `(10 10) :FLOAT)
   (make-buffer :Z `(10 10) :FLOAT))
  `(for (i 10)
	(for (j 0 10)
	     (for (k 0 10)
		  (setf (aref :Z i k) (mulf (aref :X i j) (aref :Y j k) (aref :Z i k)))))))
 :verbose t)
