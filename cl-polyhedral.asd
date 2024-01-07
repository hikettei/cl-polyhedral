
(asdf:defsystem :cl-polyhedral
  :description "Abstract Polyhedral Compiler for Common Lisp"
  :author      "hikettei <ichndm@gmail.com>"
  :licence     "MIT"
  :depends-on (#:linear-programming
	       #:mgl-pax
	       #:cl-isl)
  :components
  ((:file "source/package"))
  :in-order-to
  ((test-op (asdf:test-op cl-metal/test))))    

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
