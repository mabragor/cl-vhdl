;;;; cl-vhdl.lisp

(in-package #:cl-vhdl)

;;; "cl-vhdl" goes here. Hacks and glory await!

(defun foo ()
  (vhdl-parse 'constant-declaration "constant number_of_bits : integer := 8 * number_of_bytes;"))

(defun foo ()
  (vhdl-parse 'type-declaration
	      "type octal_digit is ('0', '1', '2', '3', '4', '5', '6', '7');"))
	      
;; TODO : actually have a VHDL type-table

#+nil
(define-vhdl-type time (_ _) ; implementation defined
  fs
  (:= ps (fs 1000))
  (:= ns (ps 1000))
  (:= us (ns 1000))
  (:= ms (us 1000))
  (:= sec (ms 1000))
  (:= min (sec 60))
  (:= hr (min 60)))

#+nil(define-vhdl-type severity-level (:enum note warning error failure))
#+nil(define-vhdl-type file-open-status (:enum open-ok status-error name-error mode-error))
#+nil(define-vhdl-type file-open-kind (:enum read-mode write-mode append-mode))

;; TODO : check that expressions in type definition are locally static

;; TODO : add maximum and minimum built-in functions -- for VHDL 2008
;; TODO : predefined type REAL, predefined type INTEGER
