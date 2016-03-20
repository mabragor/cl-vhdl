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
               (:file "cl-vhdl")))

