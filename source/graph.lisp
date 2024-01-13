
(cl:in-package :cl-polyhedral)

(deftype Variable-T () `(or symbol keyword))
(deftype Expr () `(or list number symbol))

;; TODO: Writing sections for
;;   - Graph Structure
;;   - Graph Construction (Make-Kernel-From-DSL)
;;   - User-defined codegen backend

(defsection @graph-representation (:title "Graph Representations")
  (Instruction struct)
  (Buffer      struct)
  (Domain      struct)
  (Kernel      struct))

(defun select-memory-order (order)
  (case order
    (:row #'row-major-calc-strides)
    (:column #'column-major-calc-strides)
    (T
     (error "select-memory-order: order should be a one of :column/:row~%butgot: ~a" order))))

(defstruct (Buffer
	    (:constructor make-buffer (name shape element-cffi-type
				       &key
					 (order   :column)
					 (strides (funcall (select-memory-order order) shape))
					 (n-byte  (foreign-type-size element-cffi-type)))))
  "Represents an array in the kernel.
- name: a name of buffer
- shape:
- strides:
- dtype:
- n-byte
- io: one of :not-traced :in :out :io
"
  (name name   :type Variable-T)
  (shape shape :type list)
  (strides strides :type list)
  (dtype   element-cffi-type :type Variable-T)
  (n-byte  n-byte :type fixnum)
  (io      :not-traced :type (and keyword (member :not-traced :in :out :io))))

(defun update-io! (buffer in-or-out)
  "Updates the state of io depending on the in-or-out"
  (declare (type Buffer buffer)
	   (type (and keyword (member :in :out)) in-or-out))
  (setf
   (buffer-io buffer)
   (trivia:ematch (list (buffer-io buffer) in-or-out)
     ((list :not-traced (or :in :out))
      ;; not-traced -> create a state anyway
      in-or-out)
     ((list :io         (or :in :out))
      ;; io supercedes in/out
      :io)
     ((or (list :in :in)
	  (list :out :out))
      ;; consistent use of in/out
      in-or-out)
     ((or (list :in :out)
	  (list :out :in))
      ;; mixed :in and :out
      :io))))

(defstruct (Instruction
	    (:conc-name inst-)
	    (:constructor make-instruction (op body target sources &optional (depends-on nil) (conds nil))))
  "Represents an instruction.
- op[variable-t]     a symbol representing the operation
- body[list]         expressing in the op
- depends-on[list] ids of instructions that this instruction depends on
- conds[list]        expression for condition of this instruction, if instruction is placed in an if/elseif/else block"
  (op         op      :type Variable-T)
  (body       body    :type Expr)
  (target     target  :type Expr)
  (sources    sources :type (simple-array Expr (*)))
  (depends-on depends-on :type list)
  (conds      conds :type Expr)
  (id         (gensym "OPID") :type Expr))

(defstruct (Domain
	    (:constructor make-domain (instructions subscript from to &optional (by 1) (depends-on nil))))
  "Represents an iteration domain.
- instructions[one dimensional array of Instruction] list of instructions in the kernel
- subscript[symbol] a symbol indicating the loop iterator
- from, to, by[one of list, fixnum, or symbol] the bound of iteration. [from, to) by `by`
- depends-on[list] a list of symbol depends on."
  (instructions instructions :type (simple-array (or Domain Instruction) (*)))
  (subscript    subscript    :type Variable-T)
  (from         from :type Expr)
  (to           to   :type Expr)
  (by           by   :type Expr)
  (depends-on   depends-on :type list))

(defstruct (Kernel
	    (:constructor make-kernel (instructions domains args constants &optional (config (make-config)))))
  "Represents a kernel.
- Instructions[list] list of instructions in kernel. Includes assignments, variables, function calls, etc
- Domains[list] a list of ISL SimpleSet Domains
- args[list] arguments to the kernel
- argtypes[list] types of the argument kernel
- consts[list] a list of constants in the kernel"
  (config       config       :type Config)
  (instructions instructions :type (simple-array Instruction (*)))
  (domains      domains      :type (simple-array Domain (*)))
  (args         args         :type (simple-array Buffer (*)))
  (constants constants       :type (simple-array Buffer (*))))

(defvar *default-config* nil "This parameter declares a common Config for all kernels. Local declarations are overwritten with warnigns.
e.g.: *default-config* = `(:openmp t :fastmath nil ...)")

(defstruct (Config
	    (:constructor make-config (&rest kv-pairs
				       &aux
					 (configs
					  (loop for i upfrom 0 below (length *default-config*) by 2
						for k = (nth i *default-config*)
						for v = (nth (1+ i) *default-config*)
						collect
						(cons k v)))
					 (configs
					  (progn
					    (assert
					     (mod (length kv-pairs) 2)
					     ()
					     "make-config: Odd number of arguments ~a.~%Keys and values must correspond one-to-one."
					     kv-pairs)
					    (assert
					     (mod (length *default-config*) 2)
					     ()
					     "make-config: Odd number of *default-config* ~a.~% Keys and values must correspond one-to-one."
					     *default-config*)
					    (append
					     configs
					     (loop for i upfrom 0 below (length kv-pairs) by 2
						   for key = (nth i kv-pairs)
						   for val = (nth (1+ i) kv-pairs)
						   unless (find key *default-config*)
						     collect (cons key val)
						   else
						     collect
						     (let ((pos (position key *default-config*)))
						       (when (not (eql val (nth (1+ pos) *default-config*)))
							 ;; Changed
							 (warn "~a=~a was discarded because `*default-config*` supersedes against it. now it is replaced with ~a" key val (nth (1+ pos) *default-config*)))
						       (cons (nth pos *default-config*) (nth (1+ pos) *default-config*))))))))))
  "Represents a configuration.
e.g.: (make-config :openmp t)
after then:
  - (declare-config kernel :openmp \"Docs\" ...)
  - (config-of kernel :openmp) -> T
"
  (backend)
  (configs configs :type list))

(defparameter *config-ls-mode* nil)
(defun declare-config (config config-keyword description &optional (optional nil) (default nil))
  "TODO: Docs"
  (declare (type config config)
	   (type keyword config-keyword)
	   (type (or nil string) description))

  (when *config-ls-mode*
    (format *config-ls-mode* "  - :~(~a~) -> ~a ~a~%"
	    config-keyword
	    description
	    (if optional
		(format nil "(Optional, default=~a)" default)
		(format nil "")))
    (return-from declare-config))

  (let* ((pair (find config-keyword (config-configs config) :key #'car :test #'eql)))
    (if (null pair)
	(if optional
	    (push (cons config-keyword default) (config-configs config))
	    (error "declare-config: The backend ~a requires ~a as a config:~%~a~%Butgot:~%~a" (config-backend config) config-keyword description
		   (with-output-to-string (out)
		     (loop for (k . v) in (config-configs config) do
		       (format out "~a -> ~a~%" k v)))))
	T)))

(defun config-of (kernel keyword)
  "TODO: Docs"
  (declare (type Kernel kernel)
	   (type keyword keyword))
  (or
   (let ((pair
	   (find keyword (config-configs (kernel-config kernel)) :key #'car :test #'eql)))
     (and pair (return-from config-of (cdr pair))))
   (error "config-of: The config ~a is missing from~%~a"
	  keyword
	  (with-output-to-string (out)
	    (loop for (k . v) in (config-configs (kernel-config kernel)) do
	      (format out "~a -> ~a~%" k v))))))


(defun config-ls (backend &key (stream t))
  "Describes a list of configuration in the backend"
  (declare (type keyword backend))

  (format
   stream
   "Options for ~a:~%~a"
   backend
   (with-output-to-string (*config-ls-mode*)
     (codegen-check-configs backend (make-config)))))

