;;;; cl-vhdl.lisp

(in-package #:cl-vhdl)

;;; "cl-vhdl" goes here. Hacks and glory await!

(defun foo ()
  (vhdl-parse 'constant-declaration "constant number_of_bits : integer := 8 * number_of_bytes;"))

(defun foo ()
  (vhdl-parse 'type-definition
	      "range 0 to 1E9
                   units
                     ohm;
                   end units resistance;" :junk-allowed t))
	      
;; TODO : check that expressions in type definition are locally static

;; TODO : add maximum and minimum built-in functions -- for VHDL 2008
;; TODO : predefined type REAL, predefined type INTEGER
