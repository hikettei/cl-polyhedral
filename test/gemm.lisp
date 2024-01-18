
(in-package :cl-polyhedral/test)


;; ~~ Gemm 8x8 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-poly-func ((gemm-8x8-lisp-poly :lisp)
		   (gemm-8x8-gcc-poly  :gcc))
    ((:X `(8 8) :float) (:Y `(8 8) :float) (:Z `(8 8) :float))
    (:tile nil :verbose 0)
  (for (i 8)
       (for (j 8)
	    (for (k 8)
		 (incf (aref :Z i k) (* (aref :X i j) (aref :Y j k)))))))

(define-poly-func ((gemm-8x8-lisp-poly-tiled :lisp)
		   (gemm-8x8-gcc-poly-tiled  :gcc))
    ((:X `(8 8) :float) (:Y `(8 8) :float) (:Z `(8 8) :float))
    (:tile t :verbose 0)
  (for (i 8)
       (for (j 8)
	    (for (k 8)
		 (incf (aref :Z i k) (* (aref :X i j) (aref :Y j k)))))))

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

(defun gemm-numcl (X Y Z)
  (numcl:matmul X Y Z)
  Z)

(defun gemm-libblas (X Y Z)
  (lla:gemm! 1.0 X Y 0.0 Z)
  Z)

(define-bench (gemm-8x8-float32 (8 8 8) :allow-mse-error 1e-5 :n 1000 :init-nth 2)
	      (make-random-initializer `(8 8) `(8 8) `(8 8))
    (1 gemm-8x8-lisp-optimized  2)
    (1 gemm-8x8-gcc-poly        2)
    (1 gemm-8x8-gcc-poly-tiled  2)
    (1 gemm-8x8-lisp-poly       2)
    (1 gemm-8x8-lisp-poly-tiled 2)
    (0 gemm-numcl               2 nil :get-slower)
    (0 gemm-libblas             2 nil :get-slower))

;; ~~ Gemm 256x256 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-poly-func ((gemm-256x256-lisp-poly :lisp)
		   (gemm-256x256-gcc-poly  :gcc))
    ((:X `(256 256) :float) (:Y `(256 256) :float) (:Z `(256 256) :float))
    (:tile nil :verbose 0)
  (for (i 256)
       (for (j 256)
	    (for (k 256)
		 (incf (aref :Z i k) (* (aref :X i j) (aref :Y j k)))))))

(define-poly-func ((gemm-256x256-lisp-poly-tiled :lisp)
		   (gemm-256x256-gcc-poly-tiled  :gcc))
    ((:X `(256 256) :float) (:Y `(256 256) :float) (:Z `(256 256) :float))
    (:tile t :verbose 0)
  (for (i 256)
       (for (j 256)
	    (for (k 256)
		 (incf (aref :Z i k) (* (aref :X i j) (aref :Y j k)))))))

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

(define-bench (gemm-256x256-float32 (256 256 256) :allow-mse-error 1e-7 :n 100 :init-nth 2)
	      (make-random-initializer `(256 256) `(256 256) `(256 256))
    (1 gemm-256x256-lisp-optimized  2)
    (1 gemm-256x256-gcc-poly        2)
    (1 gemm-256x256-gcc-poly-tiled  2)
    (1 gemm-256x256-lisp-poly       2)
    ;;(1 gemm-256x256-lisp-poly-tiled 2)
    (0 gemm-numcl                   2)
    (0 gemm-libblas                 2))

;; ~~ Gemm 1024x1024 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-poly-func ((gemm-1024x1024-lisp-poly :lisp)
		   (gemm-1024x1024-gcc-poly  :gcc))
    ((:X `(1024 1024) :float) (:Y `(1024 1024) :float) (:Z `(1024 1024) :float))
    (:tile nil :verbose 0)
  (for (i 1024)
       (for (j 1024)
	    (for (k 1024)
		 (incf (aref :Z i k) (* (aref :X i j) (aref :Y j k)))))))

(define-poly-func ((gemm-1024x1024-lisp-tiled-poly :lisp)
		   (gemm-1024x1024-gcc-tiled-poly  :gcc))
    ((:X `(1024 1024) :float) (:Y `(1024 1024) :float) (:Z `(1024 1024) :float))
    (:tile t :verbose 0)
  (for (i 1024)
       (for (j 1024)
	    (for (k 1024)
		 (incf (aref :Z i k) (* (aref :X i j) (aref :Y j k)))))))

(defun gemm-1024x1024-lisp-optimized (X Y Z)
  (declare (type (simple-array single-float (*)) X Y Z)
	   (optimize (speed 3) (safety 0)))
  (dotimes (i 1024)
    (dotimes (j 1024)
      (dotimes (k 1024)
	(incf (aref Z (+ (* 1024 i) k))
	      (* (aref X (+ (* 1024 i) j))
		 (aref Y (+ (* 1024 j) k)))))))
  Z)

(define-bench (gemm-1024x1024-float32 (1024 1024 1024) :allow-mse-error 1e-5 :n 1 :init-nth 2)
	      (make-random-initializer `(1024 1024) `(1024 1024) `(1024 1024))
    (1 gemm-1024x1024-lisp-optimized  2)
    (1 gemm-1024x1024-gcc-poly        2)
    (1 gemm-1024x1024-gcc-tiled-poly  2)
    (1 gemm-1024x1024-lisp-poly        2)
    ;;(1 gemm-1024x1024-lisp-tiled-poly  2)        
    (0 gemm-numcl                     2)
    (0 gemm-libblas                   2))


;; To Add: 1024x1024
;;         4096x4096

