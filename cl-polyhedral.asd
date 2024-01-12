(asdf:defsystem :cl-polyhedral
  :description "Abstract Polyhedral Compiler for Common Lisp"
  :author      "hikettei <ichndm@gmail.com>"
  :licence     "MIT"
  :depends-on (#:mgl-pax #:cffi #:cffi-libffi #:alexandria #:trivia #:cl-ppcre #:cl-cpus #:lparallel)
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
   (:file "source/polyhedral"))
  :in-order-to
  ((test-op (asdf:test-op cl-polyhedral/test))))    

(asdf:defsystem :cl-polyhedral/test
  :description ""
  :author      "hikettei <ichndm@gmail.com>"
  :licence     "MIT"
  :depends-on (#:cl-polyhedral #:rove #:numcl)
  :components
  ((:file "test/package")
   (:file "test/dsl")
   (:file "test/benchmark"))
  :perform
  (asdf:test-op (o s)
		(uiop:symbol-call (find-package :rove) :run :cl-polyhedral/test)))
