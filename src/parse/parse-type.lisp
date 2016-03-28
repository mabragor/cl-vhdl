
(in-package #:cl-vhdl)

;;; Type Declarations

(define-ebnf-rule enumeration-type-definition "(( ( identifier | character-literal ) {, ...} ))"
  `(:enum ,@2nd))

(define-ebnf-rule range-definition ("RANGE ( RANGE-attribute-name"
				    "        | simple-expression ( TO | DOWNTO ) simple-expression )")
  (if (< 2 (length 2nd))
       `(:range ,(car 2nd) ,(caddr 2nd))
       res))

(define-ebnf-rule integer-type-definition "range-definition"
  (if (< 2 (length res))
      (cons :integer (cdr res))
      res))
  
(define-ebnf-rule floating-type-definition "range-definition"
  (if (< 2 (length res))
      (cons :float (cdr res))
      res))

(define-ebnf-rule physical-type-definition ("range-definition"
					    "   UNITS identifier ; { identifier = physical-literal ; }"
					    "   END UNITS [ identifier ]")
  `(:physical ,(if (< 2 (length 1st))
		   (cdr 1st)
		   1st)
	      ,3rd
	      ,@(mapcar (lambda (x)
			  `(:= ,(car x) ,(caddr x)))
			5th)))


(define-ebnf-rule array-type-definition ("ARRAY (( ( type-mark RANGE <> ) {, ...} )) OF ELEMENT-subtype-indication"
					 "| ARRAY (( discrete-range {, ...} )) OF ELEMENT-subtype-indication")
  `(:array ,6th ,@3rd))

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

(define-ebnf-rule subtype-declaration "SUBTYPE identifier IS subtype-indication ;"
  `(:subtype ,2nd ,@4th))

(define-ebnf-rule subtype-indication (;; Clearly, the hints in BOLD are important for parsing not to be recursive
				      ;; "[ resolution-indication ]"
				      "type-mark [ constraint ]")
  (aif 2nd
       `(,1st (:constraint ,it))
       1st))

;; This _( is interesting -- I wonder, how I would parse it
(define-ebnf-rule resolution-indication ("RESOLUTION-FUNCTION-name | _(( _resolution-indication"
					 "| ( _RECORD-ELEMENT-identifier resolution-indication ) {, ...} ))"))

(define-ebnf-rule constraint "range-definition | array-constraint | _record-constraint")

(define-ebnf-rule array-constraint ("(( discrete-range {, ... } )) [ array-constraint | record-constraint ]"
				    "| (( OPEN )) [ array-constraint | record-constraint ]"))

(define-ebnf-rule record-constraint
  "(( ( RECORD-ELEMENT-identifier ( array-constraint | record-constrant ) ) {, ... } ))")

(define-ebnf-rule discrete-range "simple-discrete-range | subtype-range | attribute-range")

(define-ebnf-rule subtype-range "DISCRETE-subtype-indication"
  `(:subtype ,res))

(define-ebnf-rule attribute-range "RANGE-attribute-name"
  `(:attribute ,res))

(define-ebnf-rule simple-discrete-range "simple-expression ( TO | DOWNTO ) simple-expression"
  `(,2nd ,1st ,3rd))
	   
(define-ebnf-rule type-mark "TYPE-name | SUBTYPE-name")


