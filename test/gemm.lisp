
(in-package :cl-polyhedral/test)


;; ~~ Gemm 8x8 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(progn
  (define-poly-func ((%gemm-8x8-lisp-poly :lisp))
      ((:X `(8 8) :float) (:Y `(8 8) :float) (:Z `(8 8) :float))
      (:tile nil :verbose 0)
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
  (declare (type (simple-array single-float (*)) X Y Z)
	   (optimize (speed 3) (safety 0)))
  (dotimes (i 8)
    (dotimes (j 8)
      (dotimes (k 8)
	(incf (aref Z (+ (* 8 i) k))
	      (* (aref X (+ (* 8 i) j))
		 (aref Y (+ (* 8 j) k)))))))
  Z)

(defun gemm-8x8-numcl (X Y Z)
  (numcl:matmul X Y Z)
  Z)

(defun gemm-libblas (X Y Z)
  (lla:gemm! 1.0 X Y 0.0 Z)
  Z)

(define-bench (gemm-8x8-float32 (8 8 8) :allow-mse-error 1e-5 :n 100000 :init-nth 2)
	      (make-random-initializer `(8 8) `(8 8) `(8 8))
    (0 gemm-8x8-lisp-naive     2)
    (1 gemm-8x8-lisp-poly      2)
    (1 gemm-8x8-lisp-optimized 2)
    (0 gemm-8x8-numcl          2)
    (0 gemm-libblas            2))

;; ~~ Gemm 256x256 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(progn
  (define-poly-func ((%gemm-256x256-lisp-poly :lisp))
      ((:X `(256 256) :float) (:Y `(256 256) :float) (:Z `(256 256) :float))
      (:tile nil :verbose 0)
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
  (declare (type (simple-array single-float (*)) X Y Z)
	   (optimize (speed 3) (safety 0)))
  (dotimes (i 256)
    (dotimes (j 256)
      (dotimes (k 256)
	(incf (aref Z (+ (* 256 i) k))
	      (* (aref X (+ (* 256 i) j))
		 (aref Y (+ (* 256 j) k)))))))
  Z)

(defun gemm-256x256-numcl (X Y Z)
  (numcl:matmul X Y Z)
  Z)

(define-bench (gemm-256x256-float32 (256 256 256) :allow-mse-error 1e-7 :n 100 :init-nth 2)
	      (make-random-initializer `(256 256) `(256 256) `(256 256))
    (0 gemm-256x256-lisp-naive     2)
    (1 gemm-256x256-lisp-poly      2)
    (1 gemm-256x256-lisp-optimized 2)
    (0 gemm-256x256-numcl          2)
    (0 gemm-libblas                2))


;; ~~ Gemm 8x8 (Tiled) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(progn
  (define-poly-func ((%gemm-8x8-lisp-poly-tiled :lisp))
      ((:X `(8 8) :float) (:Y `(8 8) :float) (:Z `(8 8) :float))
      (:tile t :verbose 0)
    (for (i 8)
	 (for (j 8)
	      (for (k 8)
		   (incf (aref :Z i k) (* (aref :X i j) (aref :Y j k)))))))

  (defun gemm-8x8-lisp-poly-tiled (X Y Z)
    (%gemm-8x8-lisp-poly-tiled X Y Z)
    Z))

(define-bench (gemm-8x8-tiled-float32 (8 8 8) :allow-mse-error 1e-5 :n 100000 :init-nth 2)
	      (make-random-initializer `(8 8) `(8 8) `(8 8))
    (0 gemm-8x8-lisp-naive      2)
    (1 gemm-8x8-lisp-poly-tiled 2)
    (1 gemm-8x8-lisp-optimized  2)
    (0 gemm-8x8-numcl           2)
    (0 gemm-libblas             2))

;; ~~ Gemm 256x256 (Tiled) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(progn
  (define-poly-func ((%gemm-256x256-lisp-poly-tiled :lisp))
      ((:X `(256 256) :float) (:Y `(256 256) :float) (:Z `(256 256) :float))
      (:tile t :verbose 0)
    (for (i 256)
	 (for (j 256)
	      (for (k 256)
		   (incf (aref :Z i k) (* (aref :X i j) (aref :Y j k)))))))

  (defun gemm-256x256-lisp-poly-tiled (X Y Z)
    (%gemm-256x256-lisp-poly-tiled X Y Z)
    Z))

(define-bench (gemm-256x256-tiled-float32 (256 256 256) :allow-mse-error 1e-5 :n 100 :init-nth 2)
	      (make-random-initializer `(256 256) `(256 256) `(256 256))
    (0 gemm-256x256-lisp-naive      2)
    (1 gemm-256x256-lisp-poly-tiled 2)
    (1 gemm-256x256-lisp-optimized  2)
    (0 gemm-256x256-numcl           2)
    (0 gemm-libblas                 2))


