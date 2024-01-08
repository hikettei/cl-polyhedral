
(in-package :cl-polyhedral)


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;      Input                                       Output
;; [Lisp-Like DSL] -> Polyhedral Compilation -> [gencode method]
;;                                           -> Optimized and fused Lisp, Clang, CUDA, etc...
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; TODO: FuseOp Scheduler
;; TODO: Codegen Header部分とBody部分に分ける
;; Since the goal is to work on any backends

(trivia:defpattern range-pattern
    (bind from to by)
    ;; Range ... (from to by) (from to) (to)
    `(or (list ,bind ,from ,to ,by)
	 (and
	  (list ,bind ,from ,to)
	  (trivia:<> ,by 1))
	 (and
	  (list ,bind ,to)
	  (trivia:<> ,from 0) (trivia:<> ,by 1))))

(trivia:defpattern aref-pattern
    (aref-expr id-bind subscript-bind)
    `(and
      (list* 'aref (or (type keyword) (type symbol)) _)
      (trivia:<>
       (list ,id-bind ,subscript-bind)
       (list (second ,aref-expr) (cddr ,aref-expr)))))
		   
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
	   ;;(optimize (speed 3))
	   )
  ;; TODO: Writing a graph of dependencies

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
	   (helper (expr &optional (conditioned nil) (parent-doms nil))
	     (declare (type list conditioned))
	     (trivia:ematch expr
	       ((list* 'for
		       (range-pattern bind from to by)
		       _)
		(let* ((domain-tmp
			 (make-domain
			  (to-array 'Instruction nil)
			  bind
			  from to by
			  (remove-duplicates `(,@(expr-depends-on from) ,@(expr-depends-on to) ,@(expr-depends-on by)))))
		       (domain-instructions
			 (loop for subbody in (cddr expr)
			       for result = (helper subbody conditioned `(,@parent-doms ,domain-tmp))
			       if (Instruction-p result)
				 collect
				 (progn
				   (push result instructions)
				   (setf (inst-depends-on result)
					 (remove-duplicates
					  `(,@(inst-depends-on result) ,bind)))
				   result)
				 ;; If the result is nested; flatten and list up instructions.
			       if (listp result)
				 append  
				 (loop for x in (alexandria:flatten result)
				       if (instruction-p x)
					 collect (progn
						   (setf (inst-depends-on x)
							 (remove-duplicates
							  `(,@(inst-depends-on x) ,bind)))
						   (push x instructions)
						   x)))))
		  (setf (domain-instructions domain-tmp) (to-array 'Instruction domain-instructions))
		  (push
		   domain-tmp
		   domains)))
	       ((list*
		 'when
		 condition
		 _)
		;; Conditional Instructions
		(map
		 'list
		 #'(lambda (exp)
		     (helper exp `(,@conditioned ,condition) parent-doms))
		 (cddr expr)))
	       ;; Data Movements:
	       ;; (setf (aref target id1 id2) scalar)
	       ;; or (setf (aref target id1 id2) (aref source id1 id2))
	       ((list
		 'setf
		 (aref-pattern (second expr) target-id target-subscripts)
		 (or
		  (and
		   (type number)
		   (trivia:<> (list tgt) `(,(third expr))))
		  (and
		   (aref-pattern (third expr) source-id source-subscripts)
		   (trivia:<> (list tgt) `(,(list 'aref source-id source-subscripts))))))
		(let ((inst
			(make-instruction
			 :setf
			 expr
			 (list 'aref target-id target-subscripts)
			 (to-array 'Expr (list tgt))			 
			 (remove-duplicates
			  (map 'list #'domain-subscript parent-doms))
			 conditioned)))
		  (push inst instructions)
		  inst))
	       ((list*
		 'setf
		 (aref-pattern (second expr) target-id target-subscripts)
		 (list* op-function _))
		;; Interpreted as an operation.
		;; operation is a form defined as:
		;; (setf (aref target id1 id2) (op-function arg1 arg2 arg3... ))
		(let ((inst
			(make-instruction
			 (car op-function)
			 expr
			 (list 'aref target-id target-subscripts)
			 (to-array
			  'Expr
			  (map
			   'list
			   #'(lambda (arg-form)
			       (trivia:ematch arg-form
				 ((aref-pattern arg-form tgt-id tgt-sub)
				  (list 'aref tgt-id tgt-sub))
				 ((type number)
				  arg-form)))
			   (cdr (third expr))))
			 (remove-duplicates
			  (map 'list #'domain-subscript parent-doms))
			 conditioned)))
		  (push inst instructions)
		  inst))	       
	       (_
		(error "make-kernel-from-dsl: Detected Illegal Syntax:~%    ~a~%Expected one of:~%    - (when condition &body body)~%    - (for (bind from to expr) &body body)~%    - (setf arg_t (op arg_s1 arg_s2 ...))~%    - arg_t := (aref tensor-name &rest subscripts)" expr)))))
    (mapc #'helper body)
    
    (make-kernel
     (to-array 'Instruction (remove-duplicates instructions))
     (to-array 'Domain      (remove-duplicates domains))
     ;;TODO:  Buffer VS Constants?
     (to-array 'Buffer      buffers)
     (to-array 'Buffer      buffers))))

;; Output. Kernel(a list of instructions, a list of domains
;; To Support: If (Conditional)

#+(or)
(print
 (make-kernel-from-dsl
  (list
   (make-buffer :X `(10 10) :FLOAT)
   (make-buffer :Y `(10 10) :FLOAT)
   (make-buffer :Z `(10 10) :FLOAT))
  `(for (i 0 10 1)
	(for (j 0 1 1)
	     (setf (aref :X i j) (aref :Y i j))
	     (setf (aref :X i) 0)
	     ))
  `(for (i 0 10 1)
	(for (j 0 10 1)
	     (setf (aref :X i j) (+ (aref :Y i j)))))))

;; TODO: Several Kernels Fusion?

