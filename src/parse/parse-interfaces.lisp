
(in-package #:cl-vhdl)

;;; Interfaces and Associations

(define-ebnf-rule interface-list
  ("( interface-constant-declaration | interface-signal-declaration | interface-variable-declaration"
   "   | interface-file-declaration | _interface-type-declaration | _interface-subprogram-declaration"
   "   | _interface-package-declaration ) { ; ... }"))

(define-ebnf-rule interface-constant-declaration
    ("[ CONSTANT ] identifier {, ... } : [ IN ] subtype-indication [ := STATIC-expression ]")
  `(:constant ,5th ,(if 6th (cadr 6th)) ,@(if 4th `(:in)) ,@2nd))

(define-ebnf-rule interface-signal-declaration
    ("[ SIGNAL ] identifier {, ... } : [ mode ] subtype-indication [ BUS ] [ := STATIC-expression ]")
  `(:signal ,5th ,(if 7th (cadr 7th)) ,@(if 4th `(,4th)) ,@(if 6th `(,6th)) ,@2nd))

(define-ebnf-rule interface-variable-declaration
    ("[ VARIABLE ] identifier {, ... } : [ mode ] subtype-indication [ := STATIC-expression ]")
  `(:variable ,5th ,(if 6th (cadr 6th)) ,@(if 4th `(,4th)) ,@2nd))

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

(define-ebnf-rule association-list "( [ formal-part => ] actual-part ) {, ... }")

(define-ebnf-rule formal-part
  ("GENERIC-name | PORT-name | PARAMETER-name | FUNCTION-name (( ( GENERIC-name | PORT-name | PARAMETER-name ) ))"
   "| type-mark (( ( GENERIC-name | PORT-name | PARAMETER-name ) ))"))

(define-ebnf-rule actual-part
  ("_[ INERTIAL ] expression | SIGNAL-name | VARIABLE-name | FILE-name | _subtype-indication"
   "| _SUBPROGRAM-name | _PACKAGE-name | OPEN | FUNCTION-name (( ( SIGNAL-name | VARIABLE-name ) ))"
   "| type-mark (( ( SIGNAL-name | VARIABLE-name ) ))"))


