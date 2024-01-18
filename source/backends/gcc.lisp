
(in-package :cl-polyhedral)

;; backend = :gcc

;; Vectorized C -> GCC -> Dylib -> cl-polyhedral
;; TODO: FastMath enabled by SLEEF.h
;; TODO: AMD Neon, AVX, etc...
;; TODO: SIMD

;; Indent to right for inserting a function header.
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
    (:<= (format nil "~a<=~a" lhs rhs))
    (:incf-pointer (format nil "~a[~a]" lhs rhs))))

(defmethod codegen-write-minus ((backend (eql :gcc)) lhs kernel)
  (format nil "-~a" lhs))

(defun multithread-threshold-p (from to threshold)
  (let* ((from (read-from-string from))
	 (to   (read-from-string to)))
    (and
     (numberp from)
     (numberp to)
     (> (abs (- from to)) threshold))))

(defmethod codegen-write-for ((backend (eql :gcc))
			      kernel name
			      from to by
			      body execute-once outermost-p)
  (let ((indexing
	  (if (config-of kernel :int64)
	      "int"
	      "long"))
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
		(if (and outermost-p
			 (config-of kernel :omp))
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
	   (if (buffer-shape buffer) ;; -> array
	       (format nil "~a[~a]"
		       (buffer-name buffer)
		       (codegen-write-index-ref backend (cddr aref) buffer kernel))
	       (format nil "~a" (buffer-name buffer)))))
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

(defun dtype->ctype (keyword)
  (case keyword
    (:float "float")
    (:double "double")
    (T (if (stringp keyword) keyword (error "gcc: unknown kw ~a" keyword)))))

(defmethod codegen-write-setf ((backend (eql :gcc)) dtype variable form body pointer-p)
  (format nil "~a~a~a ~a = ~a;~%~a" (indent) (dtype->ctype dtype) (if pointer-p "*" "") variable form body))  

(defmethod codegen-function ((backend (eql :gcc)) body kernel)
  (with-output-to-string (out)
    ;; TODO: (format out "#pragma simd")
    ;; TODO: #DEFINE MAX(a, b) etc...
    
    (when (config-of kernel :omp)
      (format out "#include <omp.h>~%"))

    (format out "~a" (device-write-simd-related-headers (config-of kernel :simd-type) kernel))

    (format out "#define min(a, b) ((a) < (b) ? (a) : (b))~%#define max(a, b) ((a) > (b) ? (a) : (b))~%")

    (let* ((fname  (config-of kernel :function-name))
	   (header (format nil "void ~a(~a)"
			   fname
			   (map-split
			    ", "
			    #'(lambda (buffer)
				(format
				 nil
				 ;; (const | nil) dtype * (restrict | nil) name
				 "~a ~a * ~a ~a"
				 (ecase (buffer-io buffer)
				   (:io "")
				   (:out "")
				   (:in "const"))
				 (dtype->ctype (buffer-dtype buffer))
				 (ecase (buffer-io buffer)
				   (:io "")
				   (:out "restrict")
				   (:in  "restrict"))
				 (buffer-name buffer)))				 
			    (kernel-args kernel)))))
      (format out "~a;~%" header)
      (format out "~a {~%~a~%}" header body))))

(defun %load-foreign-gcc-code
    (source
     kernel
     &key
       gcc
       (flags))
  (declare (type string source gcc))

  (uiop:with-temporary-file (:pathname sharedlib :type "so" :keep t)
    :close-stream
    (let* ((cmd
	     ;; gcc -shared -o sharedlib
	     (append
	      (list
	       gcc
	       "-shared" "-x" "c")
	      (when (config-of kernel :omp)
		(list "-fopenmp"))
	      flags
	      (list "-o" (uiop:native-namestring sharedlib) "-")))
	   (process-info (uiop:launch-program
			  cmd
			  :input :stream
			  :error-output :stream))
	   (input (uiop:process-info-input process-info))
	   (error-output (uiop:process-info-error-output process-info)))
      (unwind-protect (princ source input)
	(close input))
      (unless (zerop (uiop:wait-process process-info))
	(error "cl-polyhedral: Failed to compile a shared library:~%~a~%

Command: ~a

Configs: ~a"
	       (alexandria:read-stream-content-into-string error-output)
	       (with-output-to-string (out)
		 (dolist (c cmd) (princ c out) (princ " " out)))
	       (with-output-to-string (out)
		 (loop for (k . v) in (config-configs (kernel-config kernel))
		       do (format out "~a -> ~a~%" k v))))))
    (cffi:load-foreign-library sharedlib)))

(defmethod load-optimized-function ((backend (eql :gcc)) body kernel)
  ;; Compiling the body
  (%load-foreign-gcc-code
   body
   kernel
   :gcc    (config-of kernel :cx)
   :flags  (config-of kernel :flags))

  (compile
   nil
   `(lambda (,@(loop for buffer across (kernel-args kernel)
		     collect
		     (intern (symbol-name (buffer-name buffer)))))
      (cl-polyhedral:with-arrays-to-pointers
	  (,@(loop for buffer across (kernel-args kernel)
		   for sym = (intern (symbol-name (buffer-name buffer)))
		   collect (list sym sym)))
	(cffi:foreign-funcall
	 ,(config-of kernel :function-name)
	 ,@(loop for arg across (kernel-args kernel)
		 append
		 (list :pointer (intern (symbol-name (buffer-name arg)))))
	 :void)
	nil))))

(defun determine-simd-type ()
  "Determines the arch of cpus"
  (let ((arch (string-downcase (machine-type))))
    (cond
      ((cl-ppcre:scan "arm64" arch) :arm64)
      ;; TO ADD: AVX512, etc...
      
      ;; Failed to determine:
      (T arch))))

(defmethod codegen-check-configs ((backend (eql :gcc)) config)
  (declare-config config :omp "Set T to use OpenMP." t t)
  (declare-config config
		  :omp-n-threads
		  "Specify the number of cores."
		  t (1+ (cl-cpus:get-number-of-processors)))
  (declare-config config :int64 "Set T to use integer64 indexing" t t)
  (declare-config config :fastmath "Set T to use SLEEF FastMath." t t)
  (declare-config config :cx "Set the compiler to use" t "gcc")
  (declare-config config :flags "Additional flags for gcc" t '("-fPIC" "-O3" "-march=native"))
  (declare-config config :function-name "The name of a generated function." t (format nil "~a" (gensym "KID")))
  (declare-config config :simd-type
		  "Indicates the arch of machine e.g.: arm64, x86" t (determine-simd-type))
  (declare-config config :simd-n-bit
		  "Indicates the bits of simd register in fixnum" t 128))

(defmethod codegen-write-packed-simd-type ((backend (eql :gcc)) kernel buffer)
  (device-write-simd-packed-simd-type
   (config-of kernel :simd-type)
   kernel
   buffer))

(defmethod codegen-write-simd-pack ((backend (eql :gcc)) kernel buffer index scalar-p)
  (device-write-simd-pack
   (config-of kernel :simd-type)
   kernel
   buffer
   index
   scalar-p))

(defmethod codegen-write-simd-unpack ((backend (eql :gcc)) kernel buffer index body scalar-p)
  (device-write-simd-unpack
   (config-of kernel :simd-type)
   kernel
   buffer
   index
   body
   scalar-p))

(defmethod codegen-write-simd-stride ((backend (eql :gcc)) buffer kernel)
  (round (/ (* 8 (cffi:foreign-type-size (buffer-dtype buffer))) (config-of kernel :simd-n-bit))))

(defmethod codegen-write-simd-intrinsics ((backend (eql :gcc)) callexpr body target-buffer source-buffers kernel)
  (flet ((buffer->aref (buffer aref)
	   ;; TODO: Allowing Scalar as an argument (null (buffer-shape buffer))
	   (if (buffer-shape buffer)
	       (codegen-write-simd-pack
		backend
		kernel
		buffer
		(codegen-write-index-ref backend (cddr aref) buffer kernel)
		nil)
	       (buffer-name buffer)))		
	 (call-bop (op lhs rhs)
	   (device-write-simd-intrinsics
	    (config-of kernel :simd-type)
	    op
	    target-buffer
	    target-buffer
	    lhs
	    rhs
	    kernel)))
    (let* ((tgt  (buffer->aref target-buffer (second body)))
	   (srcs (map 'list #'buffer->aref source-buffers (cdr (third body))))
	   (op   (car (third body)))
	   (res  (reduce
		  #'(lambda (x y) (call-bop op x y))
		  srcs)))
      (format
       nil
       "~a~a;"
       (indent)
       (codegen-write-simd-unpack
	backend
	kernel
	target-buffer
	;; body (setf (aref :X (...)))
	(codegen-write-index-ref backend (cddr (second body)) target-buffer kernel)
	(ecase callexpr
	  (:setf op)
	  (:incf (call-bop '+ tgt res))
	  (:decf (call-bop '- tgt res))
	  (:mulcf (call-bop '* tgt res))
	  (:divcf (call-bop '/ tgt res)))
	nil)))))

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
		   (incf (aref :Z i k) (* (aref :X i j) (aref :Y j k)))))))
  :verbose 2
  :tile t
  :backend :gcc))

#+(or)
(time
 (run-polyhedral
  (make-kernel-from-dsl
   (list
    (make-buffer :X `(256 256) :FLOAT)
    (make-buffer :Y `(256 256) :FLOAT)
    (make-buffer :Z `(256 256) :FLOAT))
   `(for (i 256)
	 (for (j 0 256)
	      (for (k 0 256)
		   (incf (aref :Z i k) (* (aref :X i j) (aref :Y j k)))))))
  :verbose 2
  :tile nil
  :simd t
  :backend :gcc))

