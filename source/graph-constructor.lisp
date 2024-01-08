
(in-package :cl-polyhedral)


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;      Input                                       Output
;; [Lisp-Like DSL] -> Polyhedral Compilation -> [gencode method]
;;                                           -> Optimized and fused Lisp, Clang, CUDA, etc...
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defun make-kernel-from-dsl (buffers &rest body
			     &aux instructions domains)
  "
Constructs a internal kernel-representation which cl-polyhedral can handle, from the given body.
Body is represented as a simple DSL consisted of only:

- (for (bind from to by) &rest body)
- (when expression &rest body)
- (aref tensor-id &rest references)

Args ... (:X :Y :Z), Buffers ... a list of buffers"
  (declare (type list buffers body)
	   (optimize (speed 3)))

  (labels ((to-array (type x)
	     (make-array `(,(length x)) :initial-contents x :element-type type))
	   (expr-depends-on (expr)
	     ;; Expr -> (values expr symbols-depending-on)
	     (trivia:ematch expr
	       ((list* 'aref (type Variable-T)  _)
		(let ((depends))
		  (declare (type list depends))
		  ;; Exploring subscripts
		  (loop for exp in (cddr expr)
			if (symbolp exp)
			  do (push exp depends)
			if (listp exp)
			  do (setf depends `(,@depends ,@(expr-depends-on exp))))
		  depends))
	       ((list* (type symbol) _)
		;; (car arg1 arg2 ...)
		(let ((depends))
		  (declare (type list depends))
		  (loop for exp in (cdr expr)
			if (symbolp exp)
			  do (push exp depends)
			if (listp exp)
			  do (setf depends `(,@depends ,@(expr-depends-on exp))))
		  depends))
	       ((type Variable-T)
		;; Symbol, or keyword
		`(,expr))
	       ((type number)
		nil)
	       (_
		(error "make-kernel-from-dsl: Detected Illegal Syntax:~%~a.~%Expression is consisted of:~%- (aref id ...)~%- (op arg1 arg2 ...)" expr))))
	   (helper (expr &optional parents)
	     (trivia:ematch expr
	       ((list* 'for
		       (list bind from to by)
		       _)
		(make-domain
		 (to-array 'Instruction nil)
		 bind
		 from to by
		 `(,@(expr-depends-on from) ,@(expr-depends-on to) ,@(expr-depends-on by))))
	       (_

		))))
    (mapc #'helper body)
    
    (make-kernel
     (to-array 'Instruction instructions)
     (to-array 'Domain      domains)
     (to-array 'Buffer      buffers)
     (to-array 'Buffer      buffers))))

;; Output. Kernel(a list of instructions, a list of domains
;; To Support: If (Conditional)

#+(and)
(print
 (make-kernel-from-dsl
  (list
   (make-buffer :X `(10 10) :FLOAT)
   (make-buffer :Y `(10 10) :FLOAT)
   (make-buffer :Z `(10 10) :FLOAT))
  `(for (i 0 10 1)
	(for (j 0 1 1)
	     (= (aref :X i j) (+ (aref :Y i j)))))
  `(for (i 0 10 1)
	(for (j 0 10 1)
	     (= (aref :X i j) (+ (aref :Y i j)))))))

;; TODO: Several Kernels Fusion?

