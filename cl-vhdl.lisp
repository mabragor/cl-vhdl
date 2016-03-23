;;;; cl-vhdl.lisp

(in-package #:cl-vhdl)

;;; "cl-vhdl" goes here. Hacks and glory await!

(defun foo ()
  (vhdl-parse 'constant-declaration "constant number_of_bits : integer := 8 * number_of_bytes;"))

(defun foo ()
  (vhdl-parse 'use-clause
	      "use work.int_types.all;"))
