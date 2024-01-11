
(in-package :cl-polyhedral)

;; Abstract Code Generator from Kernel -> Lisp

;; A Function is consisted from two parts:
;;  Headers (writing a function args etc, ref: AbstractNode.lisp)
;;  For
;;  Body

;; (defgeneric write-operation
;; (defgeneric write-header

;; TODO: FuseOp Scheduler
;; TODO: Codegen Header部分とBody部分に分ける
;; Since the goal is to work on any backends

(defgeneric codegen-write-id (backend id kernel)
  (:documentation
   "Rewrites the given string ID")
  (:method ((backend (eql :lisp)) (id string) kernel)
    (format nil "~a" id)))

(defgeneric codegen-write-num (backend num kernel)
  (:documentation
   "Rewrites the given string num")
  (:method ((backend (eql :lisp)) num kernel)
    (format nil "~a" num)))

(deftype binary-op-t ()
  `(and keyword
	(member
	 :and :or :max :min
	 :+ :- :* :/ :floor-div-cast-to-int :%
	 :equal :< :> :<= :>=)))

(defgeneric codegen-write-binary-op (backend op lhs rhs kernel)
  (:documentation
   "
Writes `lhs op rhs`

where both of lhs, rhs are the type of string indicating an integer.
   op is one of:

(also described in binary-op-t)

- :and
- :or
- :max
- :min
- :+
- :-
- :*
- :/ ... Int(lhs / rhs)
- :floor-div-cast-to-int ... Floor(Int(lhs / rhs))
- :% (mod lhs rhs)
- :equal (equal lhs rhs)
- :>
- :<
- :>=
- :<=
")
  (:method ((backend (eql :lisp)) op lhs rhs kernel)
    (declare (type binary-op-t op))
    (ecase op
      (:and (format nil "(and ~a ~a)" lhs rhs))
      (:or  (format nil "(or ~a ~a)" lhs rhs))
      (:max (format nil "(max ~a ~a)" lhs rhs))
      (:min (format nil "(min ~a ~a)" lhs rhs))
      (:+ (format nil   "(+ ~a ~a)" lhs rhs))
      (:- (format nil   "(- ~a ~a)" lhs rhs))
      (:* (format nil   "(* ~a ~a)" lhs rhs))
      (:/ (format nil   "(round ~a ~a)" lhs rhs))
      (:floor-div-cast-to-int (format nil "(floor ~a ~a)" lhs rhs))
      (:% (format nil "(mod ~a ~a)" lhs rhs))
      (:equal (format nil "(= ~a ~a)" lhs rhs))
      (:> (format nil "(> ~a ~a)" lhs rhs))
      (:>= (format nil "(>= ~a ~a)" lhs rhs))
      (:< (format nil "(< ~a ~a)" lhs rhs))
      (:<= (format nil "(<= ~a ~a)" lhs rhs)) )))

(defgeneric codegen-write-minus (backend lhs kernel)
  (:documentation "Writes (- lhs)")
  (:method ((backend (eql :lisp)) lhs kernel)
    (format nil "(- ~a)" lhs)))

(defgeneric codegen-write-for (backend kernel name from to by body execute-once outermost-p n-indent)
  (:documentation "Writes an iteration
<- n-indent -> |for(int name=from; name<=to; name+=by) {
               |    body
<- n-indent -> |}

- outermost-p[boolean] Set T if the iteration is placed in the outermost.

- n-indent[fixnum] counts the depth of indentation

- execute-once[boolean] Set to T if the body is executed only once.
")
  (:method ((backend (eql :lisp)) kernel name from to by body execute-once outermost-p n-indent)
    (if execute-once
	(format nil "(let ((~a 0)) ~a)" name body)
	(format nil "(~a (~a (- ~a ~a)) (let ((~a (+ (* ~a ~a) ~a))) ~a))"
		(if outermost-p
		    "lparallel:pdotimes"
		    "dotimes")
		name
		to from
		name
		by name
		from
		body))))


(defgeneric codegen-write-block (backend instructions kernel)
  (:documentation "Writes a codeblock
instructions = list")
  (:method ((backend (eql :lisp)) instructions kernel)
    (with-output-to-string (out)
      (format out "(progn~%")
      (dolist (i instructions)
	(format out "~a~%" i))
      (format out ")"))))

(defun codegen-write-index-ref (backend access-list buffer kernel)
  "A helper function to render the strided index reference"
  (codegen-write-expr
   backend
   `("+"
     ,@(loop for ref-of in access-list
	     for stride in (buffer-strides buffer)
	     for ref = (codegen-write-expr backend `("*" ,ref-of ,stride) kernel)
	     collect ref))
   kernel))

(defun codegen-write-expr (backend expr kernel)
  "A helper function to write a expr"
  (trivia:ematch expr
    ((type list)
     (flet ((helper (x y)
	      (codegen-write-binary-op
	       backend
	       (intern (format nil "~a" (car expr)) "KEYWORD")
	       (codegen-write-expr backend x kernel)
	       (codegen-write-expr backend y kernel)
	       kernel)))
       (reduce #'helper (cdr expr))))
    ((type string) expr)
    ((type symbol) (codegen-write-id backend expr kernel))
    ((type number) (codegen-write-num backend expr kernel))))  

;; TODO codegen-write-index-ref
(defgeneric codegen-write-set-scalar (backend callexpr body target-buffer source kernel)
  (:documentation "Writes an instruction expressed as:
`target[index] = source`

- callexpr[keyword] one of :setf :incf :decf :mulcf :divcf, represents =, +=, -=, *=, /= respectively.

- body[list] a list of (setf (aref :X c0 c1 c2...) scalar)

- target-buffer[Buffer] coressponding buffer

- source[number or symbol] a scalar object to write with

- kernel[Kernel] corresponding kernel
")
  (:method ((backend (eql :lisp)) callexpr body target-buffer source kernel)
    (format nil "(~a (aref ~a ~a) ~a)"
	    callexpr
	    (buffer-name target-buffer)
	    (codegen-write-index-ref backend (cddr (second body)) target-buffer kernel)
	    source)))

(defgeneric codegen-write-array-move (backend callexpr body target-buffer source-buffer kernel)
  (:documentation "Writes an instruction expressed as:
`target[index] = source[index]`

- callexpr[keyword] one of :setf :incf :decf :mulcf :divcf, represents =, +=, -=, *=, /= respectively.

- body[list] a list of original ast, i.e.: (setf (aref :X c0 c1 c2...) (aref :Y c0 c1 c2..))

- target-buffer[Buffer] coressponding buffer

- source-buffer[Buffer] corresponding buffer

- kernel[Kernel] corresponding kernel
")
  (:method ((backend (eql :lisp)) callexpr body target-buffer source-buffer kernel)
    (format nil "(~a (aref ~a ~a) (aref ~a ~a))"
	    callexpr
	    (buffer-name target-buffer)
	    (codegen-write-index-ref backend (cddr (second body)) target-buffer kernel)
	    (buffer-name source-buffer)
	    (codegen-write-index-ref backend (cddr (third body))  source-buffer kernel))))

(defgeneric codegen-write-instruction (backend callexpr body target-buffer source-buffers kernel)
  (:documentation "Writes an instruction expressed as:
`target[index] = OP(source_1[index_1], source_2[index_2], ...)`

- callexpr[keyword] one of :setf :incf :decf :mulcf :divcf, represents =, +=, -=, *=, /= respectively.

- body[list] a list of original ast, i.e.: (setf (aref :X c0 c1 c2...) (op (aref :Y c0 c1 c2..) (aref :Z c0 c1 c2 ..)))

- target-buffer[Buffer] coressponding buffer

- source-buffers[List] a list consisted of: Buffer (->aref) , Symbol (->integer), Number.

- kernel[Kernel] corresponding kernel
")
  (:method ((backend (eql :lisp)) callexpr body target-buffer source-buffers kernel)
    (format nil "(~a (aref ~a ~a) (~a ~a))"
	    callexpr
	    (buffer-name target-buffer)
	    (codegen-write-index-ref backend (cddr (second body)) target-buffer kernel)
	    (car (third body))
	    (with-output-to-string (out)
	      (loop for aref in (cdr (third body))
		    for buffer in source-buffers
		      if (buffer-p buffer) do
			(format out "(aref ~a ~a) "
				(buffer-name buffer)
				(codegen-write-index-ref backend (cddr aref) buffer kernel))
		    else do
		      (format out "~a" (codegen-write-expr backend buffer kernel)))))))


