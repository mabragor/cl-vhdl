;;;; cl-vhdl.asd

(asdf:defsystem #:cl-vhdl
  :description "My attempt to understand VHDL, and basicly make VHDL with Lisp-macro"
  :author "Alexandr Popolitov <popolit@gmail.com>"
  :version "0.1"
  :license "MIT"
  :serial t
  :depends-on (#:iterate #:cl-itertools #:cl-ppcre #:cl-interpol #:esrap-liquid
			 #:alexandria)
  :components ((:file "package")
	       (:module "parse"
			:pathname "src/parse/"
			:serial t
			:components ((:file "parsing-macro")
				     (:file "parsing-ebnf")
				     (:file "parsing")
				     (:module "grammar"
					      :pathname ""
					      :serial nil
					      :components ((:file "parse-design-file")
							   (:file "parse-decls")
							   (:file "parse-concurrent")
							   (:file "parse-sequential")
							   (:file "parse-interfaces")
							   (:file "parse-type")
							   (:file "parse-expressions")
							   (:file "parse-misc")))
				     (:file "parsing-api")))
	       (:module "emit"
			:pathname "src/emit/"
			:serial t
			:components ((:static-file "emitting")))
               (:file "cl-vhdl")
	       (:static-file "sketches")))

(defsystem :cl-vhdl-tests
  :description "Tests for CL-VHDL."
  :licence "MIT"
  :depends-on (:cl-vhdl :fiveam :cl-interpol :optima :fare-quasiquote-optima)
  :components ((:file "tests")))

(defmethod perform ((op test-op) (sys (eql (find-system :cl-vhdl))))
  (load-system :cl-vhdl-tests)
  (funcall (intern "RUN-TESTS" :cl-vhdl-tests)))
