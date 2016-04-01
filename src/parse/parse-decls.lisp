
(in-package #:cl-vhdl)

;;; Declarations and Specifications

(define-ebnf-rule package-declaration ("PACKAGE identifier IS"
				       "    [ GENERIC (( GENERIC-interface-list )) ;"
				       "      [ GENERIC MAP (( GENERIC-association-list )) ; ] ]"
				       "    { package-declarative-item }"
				       "END [ PACKAGE ] [ identifier ] ;")
  `(:package ,2nd
	     ,@(if 4th `((:generic ,@(caddr 4th))))
	     ,@(if (and 4th (nth 5 4th))
		   `((:generic-map ,@(cadddr (nth 5 4th)))))
	     ,@5th))

(define-ebnf-rule package-declarative-item ("subprogram-declaration | subprogram-instantiation-declaration"
					    "| package-declaration | package-instantiation-declaration"
					    "| type-declaration | subtype-declaration"
					    "| constant-declaration | signal-declaration"
					    "| variable-declaration | file-declaration"
					    "| alias-declaration | component-declaration"
					    "| attribute-declaration | attribute-specification"
					    "| disconnection-specification | use-clause"
					    "| group-template-declaration | group-declaration"
					    "| _PSL-property-declaration | _PSL-sequence-declaration"))

(define-ebnf-rule package-body ("PACKAGE BODY identifier IS"
				"    { package-body-declarative-item }"
				"END [ PACKAGE BODY ] [ identifier ] ;")
  `(:package-body ,3rd ,@5th))

(define-ebnf-rule package-body-declarative-item ("subprogram-declaration | subprogram-body"
						 "| _subprogram-instantiation-declaration"
						 "| _package-declaration | _package-body"
						 "| _package-instantiation-declaration"
						 "| type-declaration | subtype-declaration"
						 "| constant-declaration | variable-declaration"
						 "| file-declaration | alias-declaration"
						 "| _attribute-declaration | _attribute-specification"
						 "| use-clause | group-template-declaration | group-declaration"
						 "| triple-dot-statement" ))

(define-ebnf-rule package-instantiation-declaration ("PACKAGE identifier IS NEW UNINSTANTIATED-PACKAGE-name"
						     "    [ GENERIC MAP (( GENERIC-association-list )) ] ;")
  `(:new-package ,2nd ,5th ,@(if 6th (cadddr 6th))))

(define-ebnf-rule subprogram-specification "procedure-specification | function-specification")

(define-ebnf-rule procedure-specification ("PROCEDURE identifier"
					   "    [ GENERIC (( GENERIC-interface-list ))"
					   "      [ GENERIC MAP (( GENERIC-association-list )) ] ]"
					   "    [ [ PARAMETER ] (( PARAMETER-interface-list )) ]")
  `(:procedure ,2nd ,@(if 3rd `((:generic ,@(caddr 3rd))))
	       ,@(if (and 3rd (nth 4 3rd))
		     `((:generic-map ,@(cadddr (nth 4 3rd)))))
	       (:parameter ,@(if 4th (caddr 4th)))))

(define-ebnf-rule function-specification ("[ PURE | IMPURE ] FUNCTION ( identifier | operator-symbol )"
					  "    [ GENERIC (( GENERIC-interface-list ))"
					  "      [ GENERIC MAP (( GENERIC-association-list )) ] ]"
					  "    [ [ PARAMETER ] (( PARAMETER-interface-list )) ] RETURN type-mark")
  `(,(cond ((eq :pure 1st) :pure-function)
	   ((eq :impure 1st) :impure-function)
	   (t :function))
     ,3rd
     ,@(if 4th `((:generic ,@(caddr 4th))))
     ,@(if (and 4th (nth 4 4th))
	   `((:generic-map ,@(cadddr (nth 4 4th)))))
     (:parameter ,@(if 5th (caddr 5th)))
     (:return-type ,7th)))

(define-ebnf-rule subprogram-declaration "subprogram-specification ;"
  1st)

(define-ebnf-rule subprogram-body ("subprogram-specification IS"
				   "    { subprogram-declarative-item }"
				   "BEGIN { sequential-statement }"
				   "END [ PROCEDURE | FUNCTION ] [ identifier | operator-symbol ] ;")
  `(,@1st ,@3rd ,@5th))

(define-ebnf-rule subprogram-declarative-item ("subprogram-declaration | subprogram-body"
					       "| _subprogram-instantiation-declaration"
					       "| _package-declaration | _package-body"
					       "| _package-instantiation-declaration"
					       "| type-declaration | subtype-declaration"
					       "| constant-declaration | variable-declaration"
					       "| file-declaration | alias-declaration"
					       "| attribute-declaration | attribute-specification"
					       "| use-clause | group-template-declaration | group-declaration"
					       "| triple-dot-statement"))

(define-ebnf-rule subprogram-instantiation-declaration ("( PROCEDURE | FUNCTION ) identifier IS"
							 "    NEW UNINSTANTIATED-SUBPROGRAM-name [ signature ]"
							 "        [ GENERIC MAP (( GENERIC-association-list )) ] ;")
  `(,(if (eq :procedure 1st) :new-procedure :new-function)
     ,2nd ,5th ,@(if 6th `((:signature ,6th))) ,@(if 7th (cadddr 7th))))

(define-ebnf-rule type-declaration ("TYPE identifier IS type-definition ;"
				    "| TYPE identifier ; ")
  (if (< 2 (length res))
      `(:type ,2nd ,4th)
      res))

(define-ebnf-rule type-definition ("physical-type-definition | int-or-float-type-definition"
				   "| array-type-definition | record-type-definition"
				   "| access-type-definition | file-type-definition"
				   "| protected-type-body | protected-type-declaration"
				   "| enumeration-type-definition"
				   ))

(define-ebnf-rule constant-declaration "CONSTANT identifier {, ... } : subtype-indication [ := expression ] ;"
  `(:constant ,4th ,(aif 5th (cadr it)) ,@2nd))

(define-ebnf-rule signal-declaration ("SIGNAL identifier {, ... } : subtype-indication [ REGISTER | BUS ]"
				      "                             [ := expression ] ;")
  `(:signal ,4th ,(aif 6th (cadr it)) ,@(aif 5th `((,it t))) ,@2nd))

(define-ebnf-rule variable-declaration ("[ SHARED ] VARIABLE identifier {, ... } : subtype-indication"
					"                                          [ := expression ] ;")
  `(,(if 1st
	 :shared-variable
	 :variable)
    ,5th ,(aif 6th (cadr it)) ,@3rd))

(define-ebnf-rule file-declaration ("FILE identifier {, ... } : subtype-indication"
				    "    [ [ OPEN FILE-OPEN-KIND-expression ] IS STRING-expression ] ;")
  `(:file ,4th ,@(if 5th `((:path ,(caddr 5th) ,@(if (car 5th) `(,(cadar 5th))))))))

(define-ebnf-rule alias-declaration ("ALIAS ( identifier | character-literal | operator-symbol )"
				     "    [ : subtype-indication ] IS name [ signature ] ;")
  `(:alias ,2nd ,5th
	   ,@(if 3rd `((:subtype ,(cadr 3rd))))
	   ,@(if 6th `((:signature ,@6th)))))

(define-ebnf-rule component-declaration ("COMPONENT identifier [ IS ]"
					 "    [ GENERIC (( GENERIC-interface-list )) ; ]"
					 "    [ PORT (( PORT-interface-list )) ; ]"
					 "END COMPONENT [ identifier ] ;")
  `(:component ,2nd ,@(if 4th `((:generic ,@(caddr 4th))))
	       ,@(if 5th `((:port ,@(caddr 5th))))))

(define-ebnf-rule attribute-declaration "ATTRIBUTE identifier : type-mark ;"
  `(:attribute ,2nd ,4th))

(define-ebnf-rule attribute-specification "ATTRIBUTE identifier OF entity-name-list : entity-class IS expression ;"
  `(:attribute-spec ,2nd ,4th ,6th ,8th))

(define-ebnf-rule entity-name-list ("( ( identifier | character-literal | operator-symbol )[ signature ]){, ...}"
				    "| OTHERS | ALL")
  (if (or (eq :others res) (eq :all res))
      res
      (mapcar (lambda (x)
		(if (not (cadr x))
		    (car x)
		    x))
	      res)))
      

(define-ebnf-rule entity-class ("ENTITY | ARCHITECTURE | CONFIGURATION | PACKAGE | PROCEDURE | FUNCTION"
				"| TYPE | SUBTYPE | CONSTANT | SIGNAL | VARIABLE | FILE | COMPONENT | LABEL"
				"| LITERAL | UNITS | GROUP | _PROPERTY | _SEQUENCE"))

(define-ebnf-rule configuration-specification ("FOR component-specification binding-indication ;"
					       "    { USE VUNIT VERIFICATION-UNIT-name {, ... } ; }"
					       "[ END FOR ; ]"))

(define-ebnf-rule component-specification "( INSTANTIATION-label {, ...} | OTHERS | ALL ) : COMPONENT-name"
  `(,3rd ,@(if (atom 1st) `(,1st) 1st)))

(define-ebnf-rule binding-indication ("USE ( entity-binding-head | configuration-binding-head  | OPEN )"
				      "[ GENERIC MAP (( GENERIC-association-list )) ]"
				      "[ PORT MAP (( PORT-association-list )) ]")
  `(:use ,2nd ,@(if 3rd `((:generic-map ,@(cadddr 3rd))))
	 ,@(if 4th `((:port-map ,@(cadddr 4th))))))


(define-ebnf-rule entity-binding-head "ENTITY ENTITY-name [ (( ARCHITECTURE-identifier )) ]"
  `(:entity ,2nd ,@(if 3rd `(,(cadr 3rd)))))

(define-ebnf-rule configuration-binding-head "CONFIGURATION CONFIGURATION-name")


(define-ebnf-rule disconnection-specification ("DISCONNECT ( SIGNAL-name {, ...} | OTHERS | ALL ) : type-mark"
					       "    AFTER TIME-expression ;"))

(define-ebnf-rule group-template-declaration "GROUP identifier IS (( ( entity-class [ <> ] ) {, ...} )) ;")

(define-ebnf-rule group-declaration
  "GROUP identifier : GROUP-TEMPLATE-name (( ( name | character-literal ) {, ...} )) ;")

(define-ebnf-rule use-clause "USE selected-name {, ...} ;"
  `(:use ,@2nd))
