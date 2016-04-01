
(in-package #:cl-vhdl)

;;; Type Declarations

(define-ebnf-rule enumeration-type-definition "(( ( identifier | character-literal ) {, ...} ))"
  `(:enum ,@2nd))

(define-ebnf-rule range-definition ("RANGE ( RANGE-attribute-name"
				    "        | simple-expression ( TO | DOWNTO ) simple-expression )")
  (if (< 2 (length 2nd))
       `(:range ,(car 2nd) ,(caddr 2nd))
       res))

(define-ebnf-rule int-or-float-type-definition "range-definition"
  (if (< 2 (length res))
      (cons (cond ((and (integerp (cadr res)) (integerp (caddr res))) :integer)
		  ((and (numberp (cadr res)) (numberp (caddr res))) :float)
		  (t :int-or-float))
	    (cdr res))
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


(define-ebnf-rule array-type-definition ("ARRAY (( boxed-range {, ...} )) OF ELEMENT-subtype-indication"
					 "| ARRAY (( discrete-range {, ...} )) OF ELEMENT-subtype-indication")
  `(:array ,6th ,@3rd))

(define-ebnf-rule boxed-range " type-mark RANGE <> "
  `(:<> ,1st))

(define-ebnf-rule record-type-definition ("RECORD ( identifier {, ...} : subtype-indication ; ) { ... }"
					  "END RECORD [ identifier ]")
  `(:record ,@(mapcar (lambda (x)
			`(,(caddr x) ,@(car x))) ; subtype first, names -- rest
		      2nd)))

(define-ebnf-rule access-type-definition "ACCESS subtype-indication")

(define-ebnf-rule file-type-definition "FILE OF type-mark"
  `(:file ,3rd))

(define-ebnf-rule protected-type-declaration ("PROTECTED { protected-type-declarative-item }"
					      "END PROTECTED [ identifier ]")
  `(:protected ,@2nd))

(define-ebnf-rule protected-type-declarative-item ("subprogram-declaration | _subprogram-instantiation-declaration"
						   "| attribute-specification | use-clause"))

(define-ebnf-rule protected-type-body ("PROTECTED BODY { protected-type-body-declarative-item }"
				       "END PROTECTED BODY [ identifier ]")
  `(:protected-body ,@3rd))

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
  `(:subtype ,2nd ,@(if (keywordp (car 4th)) `(,4th) 4th)))

(define-ebnf-rule subtype-indication ("resolution-indication type-mark [ constraint ]"
				      "| type-mark [ constraint ]")
  (if (equal 3 (length res))
      `(,2nd ,@(if 1st `((:resolution ,1st))) ,@(if 3rd `((:constraint ,3rd))))
      (if (not 2nd)
	  1st
	  `(,1st (:constraint ,2nd)))))

;; This _( is interesting -- I wonder, how I would parse it
(define-ebnf-rule resolution-indication "atomic-resolution | compound-resolution")

(define-ebnf-rule atomic-resolution "RESOLUTION-FUNCTION-name")

(define-ebnf-rule compound-resolution "sub-array-resolution | sub-record-resolution")

(define-ebnf-rule sub-array-resolution "(( resolution-indication ))"
  `(:sub ,2nd))

(define-ebnf-rule sub-record-resolution "(( ( _RECORD-ELEMENT-identifier resolution-indication ) {, ...} ))"
  `(:fields ,@2nd))

(define-ebnf-rule constraint "range-definition | array-constraint | _record-constraint")

(define-ebnf-rule array-constraint ("(( discrete-range {, ... } )) [ array-constraint | record-constraint ]"
				    "| (( OPEN )) [ array-constraint | record-constraint ]"))

(define-ebnf-rule record-constraint
  "(( ( RECORD-ELEMENT-identifier ( array-constraint | record-constrant ) ) {, ... } ))")

(define-ebnf-rule discrete-range "simple-discrete-range | subtype-range | attribute-range")

;; We can't really tell subtype from attribute-range at this stage -- so just
;; return it "vanilla"
(define-ebnf-rule subtype-range "DISCRETE-subtype-indication")

(define-ebnf-rule attribute-range "RANGE-attribute-name")

(define-ebnf-rule simple-discrete-range "simple-expression ( TO | DOWNTO ) simple-expression"
  `(,2nd ,1st ,3rd))
	   
(define-ebnf-rule type-mark "TYPE-name | SUBTYPE-name")


