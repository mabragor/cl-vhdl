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

;; TODO : separate := and ::=

;; (define-vhdl-rule physical-literal (&optional hint)
;;   (let ((res (list (? (most-full-parse (v decimal-literal)
;; 				       (v based-literal)))
;; 		   (v name :unit))))
;;     res))

(defun foo ()
  (vhdl-parse 'design-unit "
library ieee ;
use ieee.std_logic_1164.all ;
use ieee.std_logic_unsigned.all;

library EXEMPLAR;                                       -- for pin_number
use EXEMPLAR.EXEMPLAR_1164.ALL;

entity pld_init is
/*
  port (
    clk: in std_logic;
    nreset: in std_logic;

    all: out std_logic_vector(17 downto 0);       -- FLASH adr
  noe_in: in std_logic;                                                 --
                                                                        --input
                                                                        --from
                                                                        --ACEX--
                                                                        --not used
nce_in: in std_logic;                                                   --
                                                                        --input
                                                                        --from
                                                                        --ACEX--
                                                                        --not used
noe: out std_logic;                                             -- output to FLASH
nce: out std_logic;                                             -- output to FLASH
d0in: in std_logic;                                                     -- D0
                                                                        -- from FLASH
d0out: out std_logic;                                           -- reseved
                                                                -- DATA0 to ACEX

nconf: out std_logic;                                           -- ACEX nConfig
nstatus: in std_logic;                                                  -- ACEX
                                                                        --
                                                                        --nStatus--
                                                                        --not used
conf_done: in std_logic;                                                -- ACEX
                                                                        -- conf_done

csacx: out std_logic;                                           -- ACEX CS ???
nws: out std_logic;                                             -- ACEX nWS
nbsy: in std_logic;                                                     -- ACEX
                                                                        --
                                                                        --RDYnBSY--
                                                                        --not used

resacx: out std_logic_misc-- ACEX reset line

    );
  attribute pin_number of clk   : signal is \"37\";
  attribute pin_number of nreset        : signal is \"43\";
  attribute array_pin_number of a       : signal is (
    \"5\", \"18\", \"35\", \"34\", \"33\", \"31\", \"30\", \"28\", \"19\",
    \"21\", \"22\", \"25\", \"27\", \"23\", \"20\", \"15\", \"8\", \"14\"
    );
  attribute pin_number of noe   : signal is \"44\";
  attribute pin_number of nce   : signal is \"12\";
  attribute pin_number of d0in  : signal is \"2\";
  attribute pin_number of d0out         : signal is \"13\";
  attribute pin_number of nconf         : signal is \"6\";
  attribute pin_number of conf_done     : signal is \"38\";
  attribute pin_number of csacx         : signal is \"10\";
  attribute pin_number of nws   : signal is \"11\";
  attribute pin_number of resacx        : signal is \"42\";
*/
end pld_init ;"))
