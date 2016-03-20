;;;; cl-vhdl.asd

(asdf:defsystem #:cl-vhdl
  :description "My attempt to understand VHDL, and basicly make VHDL with Lisp-macro"
  :author "Alexandr Popolitov <popolit@gmail.com>"
  :version "0.1"
  :license "MIT"
  :serial t
  :depends-on (#:iterate #:cl-itertools #:defmacro-enhance
			 #:quasiquote-2.0 #:cl-interpol #:esrap-liquid)
  :components ((:file "package")
	       (:file "parsing-macro")
	       (:file "parsing")
               (:file "cl-vhdl")
	       (:static-file "sketches")))

(defsystem :cl-vhdl-tests
  :description "Tests for CL-VHDL."
  :licence "MIT"
  :depends-on (:cl-vhdl :fiveam :cl-interpol)
  :components ((:file "tests")))

(defmethod perform ((op test-op) (sys (eql (find-system :cl-vhdl))))
  (load-system :cl-vhdl-tests)
  (funcall (intern "RUN-TESTS" :cl-vhdl-tests)))
