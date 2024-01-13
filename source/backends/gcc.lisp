
(in-package :cl-polyhedral)

;; backend = :gcc

;; Vectorized C -> GCC -> Dylib -> cl-polyhedral
;; TODO: FastMath enabled by SLEEF.h
;; TODO: AMD Neon, AVX, etc...
;; TODO: SIMD

;; Adding 1 for writing a function header.
(defun indent (&optional (offset 0)) (with-output-to-string (out) (dotimes (i (* 2 (+ *indent-level* offset 1))) (princ " " out))))

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
			      body execute-once outermost-p)
  (let ((indexing
	  (if (config-of kernel :int64)
	      "long"
	      "int32"))
	(+indent+ (indent)))
    ;; outermost-p && OpenMP
    (if execute-once ;; == niter is 1
	(format nil "~a~a ~a=~a;~%~a~a"
		+indent+
		indexing
		name
		from
		+indent+
		body)
	(format nil "~a~afor(~a ~a=~a; ~a<=~a; ~a+=~a) {~%~a~%~a} "
		+indent+
		;; TODO: Add Reduction if there's any!!
		(if (and outermost-p (config-of kernel :openmp)) ;; OpenMP Exists
		    (format nil "#pragma omp parallel for num_threads(~a)~%~a" (config-of kernel :omp-n-threads) +indent+)
		    "")
		indexing
		name
		from
		name
		to
		name
		by
		body
		+indent+))))

(defmethod codegen-write-block ((backend (eql :gcc)) instructions kernel)
  (with-output-to-string (out)
    (format out "~a{~%" (indent))
    (dolist (i instructions)
      (format out "~a~a" (indent 1) i))
    (format out "~a}~%" (indent))))

(defmethod codegen-write-set-scalar ((backend (eql :gcc)) callexpr body target-buffer source kernel)
  ;; x[...] <- scalar
  (format nil "~a~a[~a]~a~a;~%"
	  (indent)
	  (buffer-name target-buffer)
	  (codegen-write-index-ref backend (cddr (second body)) target-buffer kernel)
	  (ecase callexpr
	    (:setf '=)
	    (:incf '+=)
	    (:decf '-=)
	    (:mulcf '*=)
	    (:divcf '/=))
	  source))

(defmethod codegen-write-array-move ((backend (eql :gcc)) callexpr body target-buffer source-buffer kernel)
  (format nil "~a~a[~a]~a~a[~a];"
	  (indent)
	  (buffer-name target-buffer)
	  (codegen-write-index-ref backend (cddr (second body)) target-buffer kernel)
	  (ecase callexpr
	    (:setf '=)
	    (:incf '+=)
	    (:decf '-=)
	    (:mulcf '*=)
	    (:divcf '/=))
	  (buffer-name source-buffer)
	  (codegen-write-index-ref backend (cddr (third body))  source-buffer kernel)))

(defmethod codegen-write-instruction ((backend (eql :gcc)) callexpr body target-buffer source-buffers kernel)
  (flet ((buffer->aref (buffer aref)
	   (format nil "~a[~a]"
		   (buffer-name buffer)
		   (codegen-write-index-ref backend (cddr aref) buffer kernel))))
    (format nil "~a~a[~a]~a~a;"
	    (indent)
	    (buffer-name target-buffer)
	    (codegen-write-index-ref backend (cddr (second body)) target-buffer kernel)
	    (ecase callexpr
	      (:setf '=)
	      (:incf '+=)
	      (:decf '-=)
	      (:mulcf '*=)
	      (:divcf '/=))
	    (let ((op (car (third body))))
	      (if (find op `(+ - * /) :test #'eql)
		  (concatenate
		   'string
		   (map-split
		    (format nil "~a" op)
		    #'buffer->aref
		    source-buffers
		    (cdr (third body))))
		  (let ((args
			  (map-split
			   ", "
			   #'buffer->aref
			   source-buffers
			   (cdr (third body)))))
		    (format nil "~(~a~)(~a)"
			    op
			    (concatenate 'string args))))))))

(defmethod codegen-function ((backend (eql :gcc)) body kernel)
  (with-output-to-string (out)
    
    ))

(defmethod load-optimized-function ((backend (eql :gcc)) body kernel)  
  #'(lambda ())
  )

(defmethod codegen-check-configs ((backend (eql :gcc)) config)
  (declare-config config :openmp "Set T to use OpenMP." t t)
  (declare-config config
		  :omp-n-threads
		  "Specify the number of cores."
		  t (cl-cpus:get-number-of-processors))
  (declare-config config :int64 "Set T to use integer64 indexing" t t)
  
  (declare-config config :fastmath "Set T to use SLEEF FastMath." t t))


;; Running gemm
#+(or)
(time
 (run-polyhedral
  (make-kernel-from-dsl
   (list
    (make-buffer :X `(100 256) :FLOAT)
    (make-buffer :Y `(256 512) :FLOAT)
    (make-buffer :Z `(100 512) :FLOAT))
   `(for (i 100)
	 (for (j 0 256)
	      (for (k 0 512)
		   ;; TODO: setf -> incf
		   (incf (aref :Z i k) (* (aref :X i j) (aref :Y j k)))))))
  :verbose 3
  :tile t
  :backend :gcc))
