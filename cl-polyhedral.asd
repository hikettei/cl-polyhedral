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
  ((:file "source/core/package")
   (:file "source/core/graph")
   (:file "source/core/graph-constructor")
   (:file "source/core/codegen")
   (:file "source/core/isl")
   (:file "source/core/hwloc")
   (:file "source/core/utils")
   (:file "source/core/tiling")
   (:file "source/core/simd")
   (:file "source/core/memory-locality")
   (:file "source/core/ast")
   (:file "source/core/polyhedral")

   (:file "source/simd/neon")
   
   (:file "source/backends/gcc")
   (:file "source/backends/opt-lisp")
   (:file "source/backends/metal")

   )
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
