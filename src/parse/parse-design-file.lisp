
(in-package #:cl-vhdl)

;;; Design file

(define-ebnf-rule design-file "design-unit { ... }")

;; List of multiple strings is concatenated -- to make linebreaking more convenient
;; Things introduced in VHDL-2008 begin with underline
(define-ebnf-rule design-unit ("{ library-clause | use-clause | _context-reference }"
			       "library-unit")
  `(:design-unit ,@1st ,2nd))

;; I should also be able to discern PSL_* rules
(define-ebnf-rule library-unit ("entity-declaration | architecture-body | package-declaration"
				"| package-body | package-instantiation-declaration | configuration-declaration"
				"| context-declaration | PSL-verification-unit"))

;; CAPS-names denote literal symbols, instead of going down the rules	  
(define-ebnf-rule library-clause "LIBRARY identifier {, ... } ;"
  `(:library ,@2nd))

(define-ebnf-rule context-reference "CONTEXT selected-name {, ... };"
  `(:context ,@2nd))


;;; Library unit declarations

;; Grammar uses the following delimiters: (), {}, [], and | and also triple-dot symbol ...
;; But sometimes actual syntax contains these symbols.
;; In these cases we escape them by ((, )), {{, }}, [[, ]], || and ......

;; Let's for now stick to convention that grouping brackets of grammar would be (( and ))

;; In general CAPSed names in the beginning denote italicized extra info
;; (I need to figure out how to actually use it)

(define-ebnf-rule entity-declaration ("ENTITY identifier IS"
				      "   [ GENERIC (( GENERIC-interface-list )) ; ]"
				      "   [ PORT (( PORT-interface-list )) ; ]"
				      "   { entity-declarative-item }"
				      "[ BEGIN"
				      "   { concurrent-assertion-statement"
				      "     | PASSIVE-concurrent-procedure-call-statement"
				      "     | PASSIVE-process-statement"
				      ;; I don't know if this line is actually correct
				      ;; and, furthermore, now it deviates from the book
				      "     | _PSL-psl-directive } ]"
				      "END [ ENTITY ] [ identifier ] ;")
  `(:entity ,2nd
	    ,@(if 4th `((:generic ,@(caddr 4th))))
	    ,@(if 5th `((:port ,@(caddr 5th))))
	    ,@6th
	    ,@(if 7th (cadr 7th))))


(define-ebnf-rule entity-declarative-item ("subprogram-declaration | subprogram-body"
					   "| subprogram-instantiation-declaration"
					   "| package-declaration | package-body"
					   "| package-instantiation-declaration"
					   "| type-declaration | subtype-declaration"
					   "| constant-declaration | signal-declaration"
					   "| SHARED-variable-declaration | file-declaration"
					   "| alias-declaration | attribute-declaration | attribute-specification"
					   "| disconnection-specification | use-clause"
					   "| group-template-declaration | group-declaration"
					   "| _PSL-property-declaration | _PSL-sequence-declaration"
					   "| _PSL-clock-declaration"))

(define-ebnf-rule architecture-body ("ARCHITECTURE identifier OF ENTITY-name IS"
				     "   { block-declarative-item }"
				     "BEGIN"
				     "   { concurrent-statement }"
				     "END [ ARCHITECTURE ] [ identifier ] ;")
  `(:architecture ,2nd ,4th ,@6th ,@8th))

(define-ebnf-rule configuration-declaration ("CONFIGURATION identifier OF ENTITY-name IS"
					     "   { use-clause | attribute-specification | group-declaration }"
					     "   { USE VUNIT VERIFICATION-UNIT-name {, ... } ; }"
					     "   block-configuration"
					     "END [ CONFIGURATION ] [ identifier ] ;"))

(define-ebnf-rule block-configuration ("FOR ( ARCHITECTURE-name | BLOCK-STATEMENT-label"
				       "       | GENERATE-STATEMENT-label"
				       "         [ (( ( STATIC-discrete-range | STATIC-expression"
				       "                | _ALTERNATIVE-label ) )) ] )"
				       "END FOR ;"))

(define-ebnf-rule component-configuration ("FOR component-specification"
					   "    [ binding-indication ; ]"
					   "    { USE VUNIT VERIFICATION-UNIT-name {, ... } ; }"
					   "    [ block-configuration ]"
					   "END FOR ;")
  `(:for ,2nd ,@(if 3rd `(,(car 3rd)))
	 ,@(mapcar (lambda (x)
		     `(:use-vunit ,@(caddr x)))
		   4th)
	 ,@(if 5th `(,5th))))

(define-ebnf-rule context-declaration ("CONTEXT identifier IS"
				       "    { library-clause | use-clause | context-reference }"
				       "END [ CONTEXT ] [ identifier ] ;")
  `(:context ,2nd
	     ,@4th))


