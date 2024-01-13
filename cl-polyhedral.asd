(asdf:defsystem :cl-polyhedral
  :description "Abstract Polyhedral Compiler for Common Lisp"
  :author      "hikettei <ichndm@gmail.com>"
  :licence     "MIT"
  :depends-on (#:mgl-pax
	       #:cffi
	       #:cffi-libffi
	       #:alexandria
	       #:trivia
	       #:cl-ppcre
	       #:cl-cpus
	       #:lparallel
	       #:rove
	       #:cl-ansi-text
	       #:numcl)
  :serial t
  :components
  ((:file "source/package")
   (:file "source/graph")
   (:file "source/graph-constructor")
   (:file "source/codegen")
   (:file "source/isl")
   (:file "source/hwloc")
   (:file "source/utils")
   (:file "source/tiling")
   (:file "source/memory-locality")
   (:file "source/ast")
   (:file "source/polyhedral")
   (:file "source/backends/gcc")
   (:file "source/backends/opt-lisp")
   (:file "source/backends/metal"))
  :in-order-to
  ((test-op (asdf:test-op cl-polyhedral/test))))    

(asdf:defsystem :cl-polyhedral/test
  :description "Tests the accuracy and performance up compared to: OpenBLAS, Numcl, Vectorized C, etc. Including benchmarks."
  :author      "hikettei <ichndm@gmail.com>"
  :licence     "MIT"
  :depends-on (#:cl-polyhedral #:rove #:numcl #:cl-ansi-text #:lla)
  :components
  ((:file "test/package")
   (:file "test/dsl")
   (:file "test/benchmark")
   (:file "test/gemm"))
  :perform
  (asdf:test-op (o s)
		(uiop:symbol-call (find-package :rove) :run :cl-polyhedral/test)))
