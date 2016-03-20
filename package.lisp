;;;; package.lisp

(defpackage #:cl-vhdl
  (:use #:cl #:esrap-liquid #:defmacro-enhance #:cl-read-macro-tokens #:lol-re #:iterate)
  (:export #:vhdl-parse #:*vhdl-version* #:*vhdl-strict*))

