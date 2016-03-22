;;;; package.lisp

(defpackage #:cl-vhdl
  (:use #:cl #:esrap-liquid #:defmacro-enhance #:cl-read-macro-tokens #:lol-re #:iterate #:cg-common-ground)
  (:export #:vhdl-parse #:*vhdl-version* #:*vhdl-strict*
	   #:s-exp<-ebnf))

