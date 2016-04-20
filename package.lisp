;;;; package.lisp

(defpackage #:cl-vhdl
  (:use #:cl #:esrap-liquid #:iterate #:cl-itertools)
  (:shadowing-import-from #:alexandria #:with-gensyms #:once-only)
  (:export #:vhdl-parse #:*vhdl-version* #:*vhdl-strict*
	   #:parse-vhdl-file
	   #:s-exp<-ebnf
	   #:* #:|| #:? #:new
	   ))

