;;;; package.lisp

(defpackage #:cl-vhdl
  (:use #:cl #:esrap-liquid #:iterate #:cg-common-ground #:cl-itertools)
  (:shadowing-import-from #:alexandria #:with-gensyms #:once-only)
  (:export #:vhdl-parse #:*vhdl-version* #:*vhdl-strict*
	   #:s-exp<-ebnf
	   #:* #:|| #:? #:new
	   ))

