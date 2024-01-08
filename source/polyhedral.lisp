
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

  )

