
(in-package #:cl-vhdl)
(enable-read-macro-tokens)

;;; Type Declarations

(define-ebnf-rule enumeration-type-definition "(( ( identifier | character-literal ) {, ...} ))")

(define-ebnf-rule integer-type-definition ("RANGE ( RANGE-attribute-name"
					   "        | simple-expression ( TO | DOWNTO ) simple-expression )"))

(define-ebnf-rule floating-type-definition ("RANGE ( RANGE-attribute-name"
					    "        | simple-expression ( TO | DOWNTO ) simple-expression )"))

(define-ebnf-rule physical-type-definition ("RANGE ( RANGE-attribute-name"
					    "        | simple-expression ( TO | DOWNTO ) simple-expression )"
					    "   UNITS identifier { identifier = physical-literal ; }"
					    "   END UNITS [ identifier ]"))

(define-ebnf-rule array-type-definition ("ARRAY (( ( type-mark RANGE <> ) {, ...} )) OF ELEMENT-subtype-indication"
					 "| ARRAY (( discrete-range {, ...} )) OF ELEMENT-subtype-indication"))

(define-ebnf-rule record-type-definition ("RECORD ( identifier {, ...} : subtype-indication ; ) { ... }"
					  "END RECORD [ identifier ]"))

(define-ebnf-rule access-type-definition "ACCESS subtype-indication")

(define-ebnf-rule file-type-definition "FILE OF type-mark")

(define-ebnf-rule protected-type-declaration ("PROTECTED { protected-type-declarative-item }"
					      "END PROTECTED [ identifier ]"))

(define-ebnf-rule protected-type-declarative-item ("subprogram-declaration | _subprogram-instantiation-declaration"
						   "| attribute-specification | use-clause"))

(define-ebnf-rule protected-type-body ("PROTECTED BODY { protected-type-body-declarative-item }"
				       "END PROTECTED BODY [ identifier ]"))

(define-ebnf-rule protected-type-body-declarative-item ("subprogram-declaration | subprogram-body"
							"| _subprogram-instantiation-declaration"
							"| _package-declaration | _package-body"
							"| _package-instantiation-declaration"
							"| type-declaration | subtype-declaration"
							"| constant-declaration | variable-declaration"
							"| file-declaration | alias-declaration"
							"| attribute-declaration | attribute-specification"
							"| use-clause"
							"| group-template-declaration | group-declaration"))

(define-ebnf-rule subtype-declaration "SUBTYPE identifier IS subtype-indication ;")

(define-ebnf-rule subtype-indication "[ resolution-indication ] type-mark [ constraint ]")

;; This _( is interesting -- I wonder, how I would parse it
(define-ebnf-rule resolution-indication ("RESOLUTION-FUNCTION-name | _(( _resolution-indication"
					 "| ( _RECORD-ELEMENT-identifier resolution-indication ) {, ...} ))"))

(define-ebnf-rule constraint
  ("RANGE ( RANGE-attribute-name | simple-expression ( TO | DOWNTO ) simple-expression )"
   "| array-constraint | _record-constraint"))

(define-ebnf-rule array-constraint ("(( discrete-range {, ... } )) [ array-constraint | record-constraint ]"
				    "| (( OPEN )) [ array-constraint | record-constraint ]"))

(define-ebnf-rule record-constraint
  "(( ( RECORD-ELEMENT-identifier ( array-constraint | record-constrant ) ) {, ... } ))")

(define-ebnf-rule discrete-range
  "DISCRETE-subtype-indication | RANGE-attribute-name | simple-expression ( TO | DOWNTO ) simple-expression")

(define-ebnf-rule type-mark "TYPE-name | SUBTYPE-name")


