
(cl:in-package :cl-polyhedral)

(deftype Variable-T () `(or symbol keyword))
(deftype Expr () `(or list fixnum symbol))

(defsection @graph-representation (:title "Graph Representations")
  (instruction struct)
  (Domain      struct))

(defstruct (Buffer
	    (:constructor make-buffer (name shape dtype)))
  "Represents an array in the kernel."
  (name name   :type Variable-T)
  (shape shape :type list)
  (dtype dtype :type Variable-T))

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
  (sources    sources :type (array Expr (*)))
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
  (instructions instructions :type (array Instruction (*)))
  (subscript    subscript    :type Variable-T)
  (from         from :type Expr)
  (to           to   :type Expr)
  (by           by   :type Expr)
  (depends-on   depends-on :type list))

(defstruct (Kernel
	    (:constructor make-kernel (instructions domains args constants)))
  "Represents a kernel.
- Instructions[list] list of instructions in kernel. Includes assignments, variables, function calls, etc
- Domains[list] a list of ISL SimpleSet Domains
- args[list] arguments to the kernel
- argtypes[list] types of the argument kernel
- consts[list] a list of constants in the kernel"
  (instructions instructions :type (array Instruction (*)))
  (domains      domains      :type (array Domain (*)))
  (args         args         :type (array Buffer (*)))
  (constants constants       :type (array Buffer (*))))

;; Graph Constructors

;; Creating a DSL?
