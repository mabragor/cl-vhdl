
(in-package #:cl-vhdl)
(enable-read-macro-tokens)

;;; Declarations and Specifications

(define-ebnf-rule package-declaration ("PACKAGE identifier IS"
				       "    [ GENERIC (( GENERIC-interface-list )) ;"
				       "      [ GENERIC MAP (( GENERIC-association-list )) ; ] ]"
				       "    { package-declarative-item }"
				       "END [ PACKAGE ] [ identifier ] ;"))

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
				"END [ PACKAGE BODY ] [ identifier ] ;"))

(define-ebnf-rule package-body-declarative-item ("subprogram-declaration | subprogram-body"
						 "| _subprogram-intantiation-declaration"
						 "| _package-declaration | _package-body"
						 "| _package-instantiation-declaration"
						 "| type-declaration | subtype-declaration"
						 "| constant-declaration | variable-declaration"
						 "| file-declaration | alias-declaration"
						 "| _attribute-declaration | _attribute-specification"
						 "| use-clause | group-template-declaration | group-declaration"))

(define-ebnf-rule package-instantiation-declaration ("PACKAGE identifier IS NEW UNINSTANTIATED-PACKAGE-name"
						      "    [ GENERIC MAP (( GENERIC-association-list )) ] ;"))

(define-ebnf-rule subprogram-specification "procedure-specification | function-specification")

(define-ebnf-rule procedure-specification ("PROCEDURE identifier"
					   "    [ GENERIC (( GENERIC-interface-list ))"
					   "      [ GENERIC MAP (( GENERIC-association-list )) ] ]"
					   "    [ [ PARAMETER ] (( PARAMETER-interface-list )) ]"))

(define-ebnf-rule function-specification ("[ PURE | IMPURE ] FUNCTION ( identifier | operator-symbol )"
					  "    [ GENERIC (( GENERIC-interface-list ))"
					  "      [ GENERIC MAP (( GENERIC-association-list )) ] ]"
					  "    [ [ PARAMETER ] (( PARAMETER-interface-list )) ] RETURN type-mark"))

(define-ebnf-rule subprogram-declaration "subprogram-specification ;")

(define-ebnf-rule subprogram-body ("subprogram-specification IS"
				   "    { subprogram-declarative-item }"
				   "BEGIN { sequential-statement }"
				   "END [ PROCEDURE | FUNCTION ] [ identifier | operator-symbol ] ;"))

(define-ebnf-rule subprogram-declarative-item ("subprogram-declaration | subprogram-body"
					       "| _subprogram-instantiation-declaration"
					       "| _package-declaration | _package-body"
					       "| _package-instantiation-declaration"
					       "| type-declaration | subtype-declaration"
					       "| constant-declaration | variable-declaration"
					       "| file-declaration | alias-declaration"
					       "| attribute-declaration | attribute-specification"
					       "| use-clause | group-template-declaration | group-declaration"))

(define-ebnf-rule subprogram-instantiation-declaration ("( PROCEDURE | FUNCTION ) identifier IS"
							 "    NEW UNINSTANTIATED-SUBPROGRAM-name [ signature ]"
							 "        [ GENERIC MAP (( GENERIC-association-list )) ] ;"))

(define-ebnf-rule type-declaration ("TYPE identifier IS type-definition ;"
				    "| TYPE identifier ; "))

(define-ebnf-rule type-definition ("enumeration-type-definition | integer-type-definition"
				   "| floating-type-definition | physical-type-definition"
				   "| array-type-definition | record-type-definition"
				   "| access-type-definition | file-type-definition"
				   "| protected-type-declaration | protected-type-body"))

(define-ebnf-rule constant-declaration "CONSTANT identifier {, ... } : subtype-indication [ := expression ] ;"
  `(:constant ,4th ,(aif 5th (cadr it)) ,@2nd))

(define-ebnf-rule signal-declaration ("SIGNAL identifier {, ... } : subtype-indication [ REGISTER | BUS ]"
				      "                             [ := expression ] ;"))

(define-ebnf-rule variable-declaration ("[ SHARED ] VARIABLE identifier {, ... } : subtype-indication"
					"                                          [ := expression ] ;")
  `(,(if 1st
	 :shared-variable
	 :variable)
    ,5th ,(aif 6th (cadr it)) ,@3rd))

(define-ebnf-rule file-declaration ("FILE identifier {, ... } : subtype-indication"
				    "    [ [ OPEN FILE-OPEN-KIND-expression ] IS STRING-expression ] ;"))

(define-ebnf-rule alias-declaration ("ALIAS ( identifier | character-literal | operator-symbol )"
				     "    [ : subtype-indication ] IS name [ signature ] ;"))

(define-ebnf-rule component-declaration ("COMPONENT identifier [ IS ]"
					 "    [ GENERIC (( GENERIC-interface-list )) ; ]"
					 "    [ PORT (( PORT-interface-list )) ; ]"
					 "END COMPONENT [ identifier ] ;"))

(define-ebnf-rule attribute-declaration "ATTRIBUTE identifier | type-mark ;")

(define-ebnf-rule attribute-specification "ATTRIBUTE identifier OF entity-name-list | entity-class IS expression ;")

(define-ebnf-rule entity-name-list ("( ( identifier | character-literal | operator-symbol )[ signature ]){, ...}"
				    "| OTHERS | ALL"))

(define-ebnf-rule entity-class ("ENTITY | ARCHITECTURE | CONFIGURATION | PACKAGE | PROCEDURE | FUNCTION"
				"| TYPE | SUBTYPE | CONSTANT | SIGNAL | VARIABLE | FILE | COMPONENT | LABEL"
				"| LITERAL | UNITS | GROUP | _PROPERTY | _SEQUENCE"))

(define-ebnf-rule configuration-specification ("FOR component-specification binding-indication ;"
					       "    { USE VUNIT VERIFICATION-UNIT-name {, ... } ; }"
					       "[ END FOR ; ]"))

(define-ebnf-rule component-specification "( INSTANTIATION-label {, ...} | OTHERS | ALL ) : COMPONENT-name")

(define-ebnf-rule binding-indication ("USE ( ENTITY ENTITY-name [ (( ARCHITECTURE-identifier )) ]"
				      "       | CONFIGURATION CONFIGURATION-name | OPEN )"
				      "[ GENERIC MAP (( GENERIC-association-list )) ]"
				      "[ PORT MAP (( PORT-association-list )) ]"))

(define-ebnf-rule disconnection-specification ("DISCONNECT ( SIGNAL-name {, ...} | OTHERS | ALL ) : type-mark"
					       "    AFTER TIME-expression ;"))

(define-ebnf-rule group-template-declaration "GROUP identifier IS (( ( entity-class [ <> ] ) {, ...} )) ;")

(define-ebnf-rule group-declaration
  "GROUP identifier : GROUP-TEMPLATE-name (( ( name | character-literal ) {, ...} )) ;")

(define-ebnf-rule use-clause "USE selected-name {, ...} ;")
