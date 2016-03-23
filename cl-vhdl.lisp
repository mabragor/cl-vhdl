;;;; cl-vhdl.lisp

(in-package #:cl-vhdl)

;;; "cl-vhdl" goes here. Hacks and glory await!

(defun foo ()
  (vhdl-parse 'constant-declaration "constant number_of_bytes : integer := 4;"))
