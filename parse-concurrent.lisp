
(in-package #:cl-vhdl)
(enable-read-macro-tokens)

;;; Concurrent Statements

(define-ebnf-rule concurrent-statement
  ("block-statement | process-statement | concurrent-procedure-call-statement | concurrent-assertion-statement"
   "| concurrent-signal-assignment-statement | component-instantiation-statement | generate-statement"
   "| PSL-psl-directive"))

(define-ebnf-rule block-statement ("BLOCK-label : BLOCK [ (( GUARD-expression )) ] [ IS ]"
				   "    [ GENERIC (( GENERIC-interface-list )) ;"
				   "    [ GENERIC MAP (( GENERIC-association-list )) ; ] ]"
				   "    [ PORT (( PORT-interface-list )) ;"
				   "      [ PORT MAP (( PORT-association-list )) ; ] ]"
				   "    { block-declarative-item }"
				   "BEGIN { concurrent-statement } END BLOCK [ BLOCK-label ] ;"))

(define-ebnf-rule block-declarative-item ("subprogram-declaration | subprogram-body"
					  "| _subprogram-instantiation-declaration"
					  "| _package-declaration | _package-body"
					  "| _package-instantiation-declaration"
					  "| type-declaration | subtype-declaration | constant-declaration"
					  "| signal-declaration | SHARED-variable-declaration | file-declaration"
					  "| alias-declaration | component-declaration | attribute-declaration"
					  "| attribute-specification | configuration-specification"
					  "| disconnection-specification | use-clause | group-template-declaration"
					  "| group-declaration | _PSL-property-declaration"
					  "| _PSL-sequence-declaration | _PSL-clock-declaration"))

(define-ebnf-rule process-statement
  ("[ PROCESS-label : ] [ POSTPONED ] PROCESS [ ( (( SIGNAL-name {, ...} )) | ALL ) ] [ IS ]"
   "    { process-declarative-item } BEGIN { sequential-statement } END [ POSTPONED ] PROCESS [ PROCESS-label ] ;"))

(define-ebnf-rule process-declarative-item
  ("subprogram-declarative | subprogram-body | _subprogram-instantiation-declaration"
   "| _package-declaration | _package-body | _package-instantiation-declaration"
   "| type-declaration | subtype-declaration | constant-declaration | variable-declaration"
   "| file-declaration | alias-declaration | attribute-declaration | attribute-specification | use-clause"
   "| group-template-declaration | group-declaration"))

(define-ebnf-rule concurrent-procedure-call-statement
  "[ label : ] [ POSTPONED ] PROCEDURE-name [ (( PARAMETER-association-list )) ] ;")

(define-ebnf-rule concurrent-assertion-statement
  "[ label : ] [ POSTPONED ] ASSERT condition [ REPORT expression ] [ SEVERITY expression ] ;")

(define-ebnf-rule concurrent-signal-assignment-statement
  "[ label : ] [ POSTPONED ] concurrent-simple-signal-assignment"
  "| [ label : ] [ POSTPONED ] concurrent-conditional-signal-assignment"
  "| [ label : ] [ POSTPONED ] concurrent-selected-signal-assignment")

(define-ebnf-rule concurrent-simple-signal-assignment "[ GUARDED ] [ delay-mechanism ] waveform ;")

(define-ebnf-rule concurrent-conditional-signal-assignment
  "[ GUARDED ] [ delay-mechanism ] waveform WHEN condition { ELSE waveform WHEN condition } [ ELSE waveform ] ;")

(define-ebnf-rule concurrent-selected-signal-assignment
  ("WITH expression SELECT _[ ? ] target <= [ GUARDED ] [ delay-mechanism ]"
   "    { waveform WHEN choices , } waveform WHEN choices ;"))

(define-ebnf-rule component-instantiation-statement
  ("INSTANTIATION-label : ( [ COMPONENT ] COMPONENT-name | ENTITY ENTITY-name [ (( ARCHITECTURE-identifier )) ]"
   "    | CONFIGURATION CONFIGURATION-name )"
   "    [ GENERIC MAP (( GENERIC-association-list )) ] [ PORT MAP (( PORT-association-list )) ] ;"))

(define-ebnf-rule generate-statement ("for-generate-statement | if-generate-statement | _case-generate-statement"))

(define-ebnf-rule for-generate-statement
  ("GENERATE-label : FOR identifier IN discrete-range GENERATE generate-statement-body"
   "END GENERATE [ GENERATE-label ] ;"))

(define-ebnf-rule if-generate-statement
  ("GENERATE-label : IF [ ALTERNATIVE-label : ] condition GENERATE generate-statement-body"
   "{ ELSIF [ ALTERNATIVE-label : ] condition GENERATE generate-statement-body }"
   "[ ELSE [ ALTERNATIVE-label : ] GENERATE generate-statement-body ]"
   "END GENERATE [ GENERATE-label ] ;"))

(define-ebnf-rule _case-generate-statement
  ("GENERATE-label : CASE expression GENERATE"
   "  ( WHEN [ ALTERNATIVE-label : ] choices => generate-statement-body ) { ... }"
   "END GENERATE [ GENERATE-label ] ;"))

(define-ebnf-rule generate-statement-body
  ("[ { block-declarative-item } BEGIN ] { concurrent-statement } [ END [ ALTERNATIVE-label ] ; ]"))


