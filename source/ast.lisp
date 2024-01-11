
(in-package :cl-polyhedral)

(defparameter *indent-level* 0)
(defmacro with-deeper-indent (&body body)
  `(let ((*indent-level* (1+ *indent-level*)))
     ,@body))

(defcenum :isl-ast-node-type
  (:isl_ast_node_error -1)
  (:isl_ast_node_for 1)
  :isl_ast_node_if
  :isl_ast_node_block
  :isl_ast_node_mark
  :isl_ast_node_user)

(defcenum :isl-ast-expr-type
  (:isl_ast_expr_error -1)
  :isl_ast_expr_op
  :isl_ast_expr_id
  :isl_ast_expr_int)

(defcenum :isl-ast-expr-op-type
  (:isl_ast_expr_op_error -1)
  :isl_ast_expr_op_and
  :isl_ast_expr_op_and_then
  :isl_ast_expr_op_or
  :isl_ast_expr_op_or_else

  ;; maxmin
  :isl_ast_expr_op_max
  :isl_ast_expr_op_min

  ;; (- a)
  :isl_ast_expr_op_minus

  ;; Binary_Ops
  :isl_ast_expr_op_add
  :isl_ast_expr_op_sub
  :isl_ast_expr_op_mul
  :isl_ast_expr_op_div
  
  :isl_ast_expr_op_fdiv_q
  :isl_ast_expr_op_pdiv_q
  :isl_ast_expr_op_pdiv_r
  :isl_ast_expr_op_zdiv_r
  :isl_ast_expr_op_cond
  :isl_ast_expr_op_select
  :isl_ast_expr_op_eq
  :isl_ast_expr_op_le
  :isl_ast_expr_op_lt
  :isl_ast_expr_op_ge
  :isl_ast_expr_op_gt
  :isl_ast_expr_op_call
  :isl_ast_expr_op_access
  :isl_ast_expr_op_member
  :isl_ast_expr_op_address_of)

(defun parse-isl-ast (backend ex kernel &optional outermost-p)
  (declare (type Kernel kernel))
  (with-inlined-foreign-funcall-mode
    (let ((type (%"isl_ast_node_get_type":isl-ast-node-type :pointer ex)))
      (print type)
      (ecase type
	(:isl_ast_node_error
	 (error ":isl-ast-node-error"))
	(:isl_ast_node_for
	 (parse-isl-ast-for   backend ex kernel outermost-p))
	(:isl_ast_node_if
	 (parse-isl-ast-if    backend ex kernel))
	(:isl_ast_node_block
	 (parse-isl-ast-block backend ex kernel outermost-p))
	(:isl_ast_node_mark
	 (parse-isl-ast-mark  backend ex kernel))
	(:isl_ast_node_user
	 ;;(parse-isl-ast-user  backend ex kernel)
	 "")))))

(defun parse-isl-ast-block (backend ex kernel outermost-p)
  (with-inlined-foreign-funcall-mode
    (let* ((children (%"isl_ast_node_block_get_children":pointer :pointer ex))
	   (n        (%"isl_ast_node_list_n_ast_node":int :pointer children)))
      (codegen-write-block
       backend
       (loop for i upfrom 0 below n
	     for child = (%"isl_ast_node_list_get_at":pointer :pointer children :int i)
	     collect
	     (parse-isl-ast backend child kernel outermost-p))
       kernel))))

(defun parse-isl-expr (backend ast kernel &key (determine-upper-bound-p nil))
  (declare (type foreign-pointer ast))
  (with-inlined-foreign-funcall-mode
    (let ((type (%"isl_ast_expr_get_type":isl-ast-expr-type :pointer ast)))
      (ecase type
	(:isl_ast_expr_error
	 (error ":isl_ast_expr_error"))
	(:isl_ast_expr_id
	 (let* ((id (%"isl_ast_expr_id_get_id":pointer :pointer ast))
		(name (prog1
			  (%"isl_id_get_name":string :pointer id)
			(%"isl_id_free":void :pointer id))))
	   (codegen-write-id backend name kernel)))
	(:isl_ast_expr_int
	 (let* ((id (%"isl_ast_expr_int_get_val":pointer :pointer ast))
		(num (prog1
			 (%"isl_val_get_d":double :pointer id)
		       (%"isl_val_free":void :pointer id))))
	   (let ((num (round num)))
	     (codegen-write-num backend num kernel))))
	(:isl_ast_expr_op
	 (let ((n-arg (%"isl_ast_expr_get_op_n_arg":int :pointer ast)))
	   (assert (or (= n-arg 1) (= n-arg 2)) () "Assertion Failed with nargs == 1 or 2")
	   (multiple-value-bind (lhs rhs)
	       (values
		(parse-isl-expr backend (%"isl_ast_expr_op_get_arg":pointer :pointer ast :int 0) kernel)
		(when (= n-arg 2)
		  (parse-isl-expr backend (%"isl_ast_expr_op_get_arg":pointer :pointer ast :int 1) kernel)))
	     (let ((op-type (%"isl_ast_expr_op_get_type":isl-ast-expr-op-type :pointer ast)))
	       (assert (not (eql op-type :isl_ast_expr_op_error)) () ":isl_ast_expr_op_error")
	       (when determine-upper-bound-p
		 ;; i < 10, here 10 is needed if determine-up-bound-p is set to t
		 (ecase op-type
		   ;; a < b
		   ;; ->
		   ;; a <= b-1
		   (:isl_ast_expr_op_lt
		    (return-from parse-isl-expr
		      (if (numberp (read-from-string rhs))
			  (codegen-write-num backend (1- (read-from-string rhs)) kernel)
			  (codegen-write-binary-op
			   backend
			   :-
			   rhs
			   (codegen-write-num backend 1 kernel)
			   kernel))))
		   ;; <=
		   (:isl_ast_expr_op_le
		    (return-from parse-isl-expr rhs))))
	       (ecase op-type
		 (:isl_ast_expr_op_and
		  ;; LHS && RHS
		  (codegen-write-binary-op backend :and lhs rhs kernel))
		 (:isl_ast_expr_op_and_then
		  (codegen-write-binary-op backend :and lhs rhs kernel))
		 (:isl_ast_expr_op_or
		  ;; LHS || RHS
		  (codegen-write-binary-op backend :or lhs rhs kernel))
		 (:isl_ast_expr_op_or_else
		  (codegen-write-binary-op backend :or lhs rhs kernel))
		 ;; maxmin
		 (:isl_ast_expr_op_max
		  (codegen-write-binary-op backend :max lhs rhs kernel))
		 (:isl_ast_expr_op_min
		  (codegen-write-binary-op backend :min lhs rhs kernel))
		 
		 ;; (- a)
		 (:isl_ast_expr_op_minus
		  (codegen-write-minus backend lhs kernel))

		 ;; Binary_Ops
		 (:isl_ast_expr_op_add
		  (codegen-write-binary-op backend :+ lhs rhs kernel))
		 (:isl_ast_expr_op_sub
		  (codegen-write-binary-op backend :- lhs rhs kernel))
		 (:isl_ast_expr_op_mul
		  (codegen-write-binary-op backend :* lhs rhs kernel))
		 (:isl_ast_expr_op_div
		  (codegen-write-binary-op backend :/ lhs rhs kernel))
		 
		 (:isl_ast_expr_op_fdiv_q
		  (codegen-write-binary-op backend :floor-div-cast-to-int lhs rhs kernel))
		 (:isl_ast_expr_op_pdiv_q
		  (codegen-write-binary-op backend :/ lhs rhs kernel))		  
		 (:isl_ast_expr_op_pdiv_r
		  (codegen-write-binary-op backend :% lhs rhs kernel))
		 (:isl_ast_expr_op_zdiv_r
		  (codegen-write-binary-op backend :% lhs rhs kernel))
		 ;;(:isl_ast_expr_op_cond)
		 ;;(:isl_ast_expr_op_select)
		 (:isl_ast_expr_op_eq
		  (codegen-write-binary-op backend :equal lhs rhs kernel))
		 (:isl_ast_expr_op_le
		  (codegen-write-binary-op backend :<= lhs rhs kernel))
		 (:isl_ast_expr_op_lt
		  (codegen-write-binary-op backend :< lhs rhs kernel))
		 (:isl_ast_expr_op_ge
		  (codegen-write-binary-op backend :<= lhs rhs kernel))
		 (:isl_ast_expr_op_gt
		  (codegen-write-binary-op backend :> lhs rhs kernel))
		 ;;(:isl_ast_expr_op_call)
		 ;;(:isl_ast_expr_op_access)
		 ;;(:isl_ast_expr_op_member)
		 ;;(:isl_ast_expr_op_address_of)
		 )))))))))


(defun parse-isl-ast-for (backend ex kernel outermost-p)
  (with-inlined-foreign-funcall-mode
    (let* ((execute-once (%"isl_ast_node_for_is_degenerate":boolean :pointer ex))
	   (iter         (%"isl_ast_node_for_get_iterator":pointer  :pointer ex))
	   (id           (%"isl_ast_expr_get_id":pointer :pointer iter))
	   (name         (prog1
			     (%"isl_id_get_name":string :pointer id)
			   (%"isl_id_free":void :pointer id)))
	   (from          (parse-isl-expr 
			   backend
			   (%"isl_ast_node_for_get_init":pointer
			     :pointer ex)
			   kernel))
	   (body          (with-deeper-indent
			    (parse-isl-ast
			     backend
			     (%"isl_ast_node_for_get_body":pointer :pointer ex)
			     kernel)))
	   (by           (parse-isl-expr
			  backend
			  (%"isl_ast_node_for_get_inc":pointer :pointer ex)
			  kernel))
	   (to           (parse-isl-expr
			  backend
			  (%"isl_ast_node_for_get_cond":pointer :pointer ex)
			  kernel
			  :determine-upper-bound-p t)))
      (codegen-write-for
       backend kernel
       name from to by body execute-once outermost-p *indent-level*))))

      