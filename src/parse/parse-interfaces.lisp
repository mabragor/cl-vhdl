
(in-package #:cl-vhdl)

;;; Interfaces and Associations

(define-ebnf-rule interface-list
  ("( interface-sig-var-con-declaration"
   "   | interface-file-declaration | _interface-type-declaration | _interface-subprogram-declaration"
   "   | _interface-package-declaration ) { ; ... }"))

;; We can't really determine (unless it's explicitly written), what's the type of this construction
;; "[ CONSTANT ] identifier {, ... } : [ IN ] subtype-indication [ := STATIC-expression ]")
;; "[ SIGNAL ] identifier {, ... } : [ mode ] subtype-indication [ BUS ] [ := STATIC-expression ]")
(define-ebnf-rule interface-sig-var-con-declaration
    ("[ VARIABLE | CONSTANT | SIGNAL ] identifier {, ... } : [ mode ] subtype-indication "
     "[ BUS ] [ := STATIC-expression ]")
  `(,(or 1st :sig-var-con) ,5th ,(if 6th (cadr 6th)) ,@(if 4th `(,4th)) ,@2nd))

(define-ebnf-rule mode "IN | OUT | INOUT | BUFFER | LINKAGE")

(define-ebnf-rule interface-file-declaration "FILE identifier {, ... } : subtype-indication")

(define-ebnf-rule interface-type-declaration "TYPE identifier")

(define-ebnf-rule interface-subprogram-declaration
    "(interface-procedure-declaration | interface-function-declaration) [ IS ( SUBPROGRAM-name | <> ) ]"
  (if 2nd
      `(,@1st ,2nd)
      1st))

(define-ebnf-rule interface-procedure-declaration
    "PROCEDURE identifier [ [ PARAMETER ] (( PARAMETER_interface_list )) ]"
  `(:procedure ,2nd (:parameter ,@(if 3rd (caddr 3rd)))))


(define-ebnf-rule interface-function-declaration
    ("[ PURE | IMPURE ] FUNCTION ( identifier | operator-symbol )"
     "[ [ PARAMETER ] (( PARAMETER-interface-list )) ] RETURN type-mark")
  `(,(cond ((eq :pure 1st) :pure-function)
	   ((eq :impure 1st) :impure-function)
	   (t :function))
     ,3rd
     (:parameter ,@(if 4th (caddr 4th)))
     (:return-type ,6th)))



(define-ebnf-rule interface-package-declaration
  ("PACKAGE identifier IS NEW UNINSTANTIATED-PACKAGE-name"
   "GENERIC MAP (( ( GENERIC-association-list | <> | DEFAULT ) ))"))

(define-ebnf-rule association-list "( [ formal-part => ] actual-part ) {, ... }"
  (mapcar (lambda (x)
	    (if (car x)
		`(:=> ,(caar x) ,(cadr x))
		(cadr x)))
	  res))

(define-ebnf-rule formal-part
  ("GENERIC-name | PORT-name | PARAMETER-name | FUNCTION-name (( ( GENERIC-name | PORT-name | PARAMETER-name ) ))"
   "| type-mark (( ( GENERIC-name | PORT-name | PARAMETER-name ) ))"))

(define-ebnf-rule actual-part
  ("inertial-part | SIGNAL-name | VARIABLE-name | FILE-name | _subtype-indication"
   "| _SUBPROGRAM-name | _PACKAGE-name | OPEN | FUNCTION-name (( ( SIGNAL-name | VARIABLE-name ) ))"
   "| type-mark (( ( SIGNAL-name | VARIABLE-name ) ))"))

;; (%define-ebnf-rule actual-part
;;     (|| (inertial-part) (name :signal) (name :variable) (name :file)
;; 	(new (subtype-indication)) (new (name :subprogram)) (new (name :package))
;; 	:open ((name :function) "(" (|| (name :signal) (name :variable)) ")")
;; 	((type-mark) "(" (|| (name :signal) (name :variable)) ")")))


(define-ebnf-rule inertial-part "_[ INERTIAL ] expression"
  (if 1st
      `(:inertial ,2nd)
      2nd))

;; (PROGN
 ;;  (UPDATE-GREEDY-CHAR-SEQ-TABLE "(")
 ;; (UPDATE-GREEDY-CHAR-SEQ-TABLE ")")
 ;; (UPDATE-GREEDY-CHAR-SEQ-TABLE "(")
 ;; (UPDATE-GREEDY-CHAR-SEQ-TABLE ")")
 ;; (MACROLET ((WH? (&BODY X)
 ;;              `(PROGN (? whitespace) ,.X))
 ;;            (NEW (X &OPTIONAL (Y NIL Y-P))
 ;;              (IF Y-P
 ;;                  `(IF (OR (NOT *VHDL-VERSION*) (<= ,X *VHDL-VERSION*))
 ;;                       ,Y
 ;;                       (FAIL-PARSE
 ;;                        "Version we are trying to parse doesn't support this"))
 ;;                  `(IF (OR (NOT *VHDL-VERSION*) (<= 2008 *VHDL-VERSION*))
 ;;                       ,X))))
 ;;   (DEFINE-VHDL-RULE ACTUAL-PART
 ;;       (&OPTIONAL HINT)
 ;;     (LET ((RES
 ;;            (WH?
 ;;             (MOST-FULL-PARSE (WH? (V INERTIAL-PART)) (WH? (V NAME :SIGNAL))
 ;;                              (WH? (V NAME :VARIABLE)) (WH? (V NAME :FILE))
 ;;                              (NEW (WH? (V SUBTYPE-INDICATION)))
 ;;                              (NEW (WH? (V NAME :SUBPROGRAM)))
 ;;                              (NEW (WH? (V NAME :PACKAGE)))
 ;;                              (WH? (V CASE-INSENSITIVE-STRING "open")
 ;;                               (! (CHARACTER-RANGES (#\a #\z) (#\A #\Z)))
 ;;                               :OPEN)
 ;;                              (WH?
 ;;                               (LIST (WH? (V NAME :FUNCTION))
 ;;                                     (WH? (V GREEDY-CHAR-SEQ "("))
 ;;                                     (WH?
 ;;                                      (MOST-FULL-PARSE (WH? (V NAME :SIGNAL))
 ;;                                                       (WH?
 ;;                                                        (V NAME :VARIABLE))))
 ;;                                     (WH? (V GREEDY-CHAR-SEQ ")"))))
 ;;                              (WH?
 ;;                               (LIST (WH? (V TYPE-MARK))
 ;;                                     (WH? (V GREEDY-CHAR-SEQ "("))
 ;;                                     (WH?
 ;;                                      (MOST-FULL-PARSE (WH? (V NAME :SIGNAL))
 ;;                                                       (WH?
 ;;                                                        (V NAME :VARIABLE))))
 ;;                                     (WH? (V GREEDY-CHAR-SEQ ")"))))))))
 ;;       (WITH-LIST-PLACES (RES)
 ;;         RES)))))

;; OK, now I understand very well, what is it that takes so much time to compile
;; (and also a lot of memory)
;; -- it's expansion of multiple (? whitespace) things that I have
;; I need to think of a way to organize ESRAP in a way that such extensive macroexpansion
;; is *unnecessary*
