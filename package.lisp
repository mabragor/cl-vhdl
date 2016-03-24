;;;; package.lisp

(defpackage #:cl-vhdl
  (:use #:cl #:esrap-liquid #:defmacro-enhance #:cl-read-macro-tokens #:lol-re #:iterate #:cg-common-ground
	#:cl-itertools)
  (:export #:vhdl-parse #:*vhdl-version* #:*vhdl-strict*
	   #:s-exp<-ebnf
	   #:* #:|| #:? #:new
	   ))

