
(in-package :cl-polyhedral/test)

;; result = :improved | nil de test suru

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

(define-poly-func gemm-256x256-poly
    ((:X `(256 256) :float) (:Y `(256 256) :float) (:Z `(256 256) :float))
    (:backend :lisp :tile t :verbose 3)
  (for (i 0 10)
       (for (j 0 10)
	    (for (k 0 10)
		 (incf (aref :Z i k) (mul (aref :X i j) (aref :Y j k)))))))

