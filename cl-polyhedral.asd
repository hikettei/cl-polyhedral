(asdf:defsystem :cl-polyhedral
  :description "Abstract Polyhedral Compiler for Common Lisp"
  :author      "hikettei <ichndm@gmail.com>"
  :licence     "MIT"
  :depends-on (#:mgl-pax #:cffi #:cffi-libffi #:alexandria #:trivia #:cl-ppcre)
  :components
  ((:file "source/package")
   (:file "source/graph")
   (:file "source/graph-constructor")
   (:file "source/isl")
   (:file "source/utils")
   (:file "source/tiling")
   (:file "source/memory-locality")
   (:file "source/polyhedral")
   (:file "source/codegen"))
  :in-order-to
  ((test-op (asdf:test-op cl-polyhedral/test))))    

(asdf:defsystem :cl-polyhedral/test
  :description ""
  :author      "hikettei <ichndm@gmail.com>"
  :licence     "MIT"
  :depends-on (#:cl-polyhedral #:rove)
  :components
  ((:file "test/package"))
  :perform
  (asdf:test-op (o s)
		(uiop:symbol-call (find-package :rove) :run :cl-polyhedral/test)))
