;;;; cl-vhdl.lisp

(in-package #:cl-vhdl)

;;; "cl-vhdl" goes here. Hacks and glory await!

(defun foo (&optional junk-allowed)
  (vhdl-parse 'if-statement
	      "if en then
                   ...
                   -- stored_value := data_in;
               end if;"))


	      
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
#+nil(define-vhdl-type boolean (:enum false true))
#+nil(define-vhdl-type bit (:enum #\0 #\1))
#+nil(define-vhdl-type std-ulogic (:enum #\U #\X #\0 #\1 #\Z #\W #\L #\H #\-))

;; TODO : predefined character enum

;; TODO : check that expressions in type definition are locally static

;; TODO : add maximum and minimum built-in functions -- for VHDL 2008
;; TODO : predefined type REAL, predefined type INTEGER

;; TODO : various restrictions on possible attribute names
