
(in-package :cl-polyhedral)

;; backend = :gcc

;; Vectorized C -> GCC -> Dylib -> cl-polyhedral
;; TODO: FastMath enabled by SLEEF.h
;; TODO: AMD Neon, AVX, etc...
;; TODO: SIMD

;; Writing a symbol
(defmethod codegen-write-id ((backend (eql :gcc)) id kernel)
  ;; Replace Hyphen -> Underscore
  ;; downcase all.
  (cl-ppcre:regex-replace-all "-" (format nil id "~(~a~)" id) "_"))

;; Writing a number
;; nothing to refer to.
(defmethod codegen-write-num ((backend (eql :gcc)) num kernel)
  (format nil "~a" num))

(defmethod codegen-write-binary-op ((backend (eql :gcc)) op lhs rhs kernel)
  ;; LHS/RHS is asserted to be an integer.
  (assert (typep op 'binary-op-t) () "gcc: Unknown op: ~a" op)
  (ecase op
    (:and (format nil "~a && ~a" lhs rhs))
    (:or  (format nil "~a || ~a" lhs rhs))
    (:max (format nil "max(~a, ~a)" lhs rhs))
    (:min (format nil "min(~a, ~a)" lhs rhs))
    (:+ (format nil   "~a+~a" lhs rhs))
    (:- (format nil   "~a-~a" lhs rhs))
    (:* (format nil   "~a*~a" lhs rhs))
    (:/ (format nil   "~a/~a" lhs rhs))
    (:floor-div-cast-to-int (format nil "~a/~a" lhs rhs))
    (:% (format nil "~a % ~a" lhs rhs))
    (:equal (format nil "~a == ~a" lhs rhs))
    (:> (format nil "~a>~a" lhs rhs))
    (:>= (format nil "~a>=~a" lhs rhs))
    (:< (format nil "~a<~a" lhs rhs))
    (:<= (format nil "~a<=~a" lhs rhs))))

(defmethod codegen-write-minus ((backend (eql :gcc)) lhs kernel)
  (format nil "-~a" lhs))

(defmethod codegen-write-for ((backend (eql :gcc))
			      kernel name
			      from to by
			      body execute-once outermost-p n-indent)
  ;; outermost-p && OpenMP
  
  )

(defmethod codegen-check-configs ((backend (eql :gcc)) kernel)
  (declare-config kernel backend :openmp   "Set T to use OpenMP. (default: T)" t t)
  (declare-config kernel backend :fastmath "Set T to use SLEEF FastMath. (default: T)" t t)
  )


