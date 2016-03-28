
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
  ("( PROCEDURE identifier [ [ PARAMETER ] ( PARAMETER_interface_list ) ]"
   "   | [ PURE | IMPURE ] FUNCTION (( identifier | operator-symbol ))"
   "                       [ [ PARAMETER ] ( PARAMETER-interface-list ) ] RETURN type-mark )"
   "[ IS ( SUBPROGRAM-name | <> ) ]"))

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

(define-ebnf-rule inertial-part "_[ INERTIAL ] expression"
  (if 1st
      `(:inertial ,2nd)
      2nd))
