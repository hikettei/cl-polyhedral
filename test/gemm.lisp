
(in-package :cl-polyhedral/test)

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


;; ~~ Gemm 8x8 (Tiled) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(progn
  (define-poly-func %gemm-8x8-lisp-poly-tiled
      ((:X `(8 8) :float) (:Y `(8 8) :float) (:Z `(8 8) :float))
      (:backend :lisp :tile t :verbose 0)
    (for (i 8)
	 (for (j 8)
	      (for (k 8)
		   (incf (aref :Z i k) (* (aref :X i j) (aref :Y j k)))))))

  (defun gemm-8x8-lisp-poly-tiled (X Y Z)
    (%gemm-8x8-lisp-poly-tiled X Y Z)
    Z))

(define-bench (gemm-8x8-tiled (8 8 8) :allow-mse-error 0 :n 10000 :init-nth 2)
	      (make-random-initializer `(8 8) `(8 8) `(8 8))
    (0 gemm-8x8-lisp-naive      2)
    (1 gemm-8x8-lisp-poly-tiled 2)
    (0 gemm-8x8-lisp-optimized  2)
    (0 gemm-8x8-numcl           2))

;; ~~ Gemm 256x256 (Tiled) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(progn
  (define-poly-func %gemm-256x256-lisp-poly-tiled
      ((:X `(256 256) :float) (:Y `(256 256) :float) (:Z `(256 256) :float))
      (:backend :lisp :tile t :verbose 0)
    (for (i 256)
	 (for (j 256)
	      (for (k 256)
		   (incf (aref :Z i k) (* (aref :X i j) (aref :Y j k)))))))

  (defun gemm-256x256-lisp-poly-tiled (X Y Z)
    (%gemm-256x256-lisp-poly-tiled X Y Z)
    Z))

(define-bench (gemm-256x256-tiled (256 256 256) :allow-mse-error 0 :n 10 :init-nth 2)
	      (make-random-initializer `(256 256) `(256 256) `(256 256))
    (0 gemm-256x256-lisp-naive      2)
    (1 gemm-256x256-lisp-poly-tiled 2)
    (0 gemm-256x256-lisp-optimized  2)
    (0 gemm-256x256-numcl           2))



