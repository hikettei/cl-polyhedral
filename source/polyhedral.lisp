
(cl:in-package :cl-polyhedral)

(defun run-polyhedral
    (kernel
     &key
       (verbose nil)
       (threads nil))
  "Gains the optimized kernel obtained from transforming the given kernel using Polyhedral Model.
Does the following:
- 1. Creates a context and space for ISL
- 
"
  (declare (type Kernel kernel))
  (with-isl-ctx ctx

    (let ((instructions
	    (cl-isl:isl-union-set-read-from-str
	     ctx
	     (KernelInstruction->ISL-Rep kernel))))

      )))

(defun KernelInstruction->ISL-Rep (kernel)
  "Constructs the instructions' domains representation in ISL Syntax Form.
[A, B, C]"
  (declare (type Kernel kernel))
  
  )

(defun get-params-str (kernel)
  "Returns a string representing a list of constants in the kernel
e.g.: [A, B, C]"
  (declare (type Kernel kernel))
  (let ((consts (kernel-constants kernel)))
    (with-output-to-string (out)

      )))

#+(and)
(run-polyhedral nil)
  
