
(in-package :cl-polyhedral)

(defmethod device-write-simd-packed-simd-type
    ((arch (eql :arm64))
     kernel
     buffer)
  (let ((n-bit (buffer-n-byte buffer))
	(reg   (config-of kernel :simd-n-bit)))
    ;; FLOATXX_X$_T
    (format nil "~(~a~)~ax~a_t"
	    (buffer-dtype buffer)
	    (* 8 n-bit)
	    (round (/ reg (* 8 n-bit))))))

(defmethod device-write-simd-pack
    ((arch (eql :arm64)) kernel buffer index scalar-p)
  (format nil "vld1q_~a~a(~a + ~a)"
	  (ecase (buffer-dtype buffer)
	    (:float "f32")
	    (:double "f64")
	    (:float16 "f16"))
	  (if scalar-p "n_" "")
	  (buffer-name buffer)
	  index))

(defmethod device-write-simd-unpack
    ((arch (eql :arm64)) kernel buffer index body scalar-p)
  (format nil "vst1q_~a~a(~a + ~a, ~a)"
	  (ecase (buffer-dtype buffer)
	    (:float "f32")
	    (:double "f64")
	    (:float16 "f16"))
	  (if scalar-p "n_" "")
	  (buffer-name buffer)
	  index
	  body))

(defmethod device-write-simd-intrinsics
    ((arch (eql :arm64)) op target-buffer source-buffer lhs rhs kernel)
  ;; TODO: SLEEF
  (let* ((p  (ecase (buffer-dtype target-buffer)
	       (:float16 "f16")
	       (:float "f32")
	       (:double "f64")
	       ;; TODO: Sparse Matrices: int16 -> s16, uint16 -> u16
	       ))
	 (f  (ecase op
	       (+ "vaddq")
	       (- "vsubq")
	       (* "vmulq")
	       (/ "vdivq"))))
    (format nil "~a_~a(~a, ~a)" f p lhs rhs)))
   

(defmethod device-write-simd-related-headers ((arch (eql :arm64)) kernel)
  ;; TODO: SLEEF, FP16
  "#include <arm_neon.h>
")

