
(in-package #:cl-vhdl)
(enable-read-macro-tokens)

;;; Sequential Statements

(define-ebnf-rule sequential-statement
  ("wait-statement | assertion-statement | report-statement | signal-assignment-statement"
   "| variable-assignment-statement | procedure-call-statement | if-statement | case-statement"
   "| loop-statement | next-statement | exit-statement | return-statement | null-statement"))

(define-ebnf-rule wait-statement
  "[ label : ] WAIT [ ON SIGNAL-name {, ...} ] [ UNTIL condition ] [ FOR TIME-expression ] ;")

(define-ebnf-rule assertion-statement
  "[ label : ] ASSERT condition [ REPORT expression ] [ SEVERITY expression ] ;")

(define-ebnf-rule report-statement "[ label : ] REPORT expression [ SEVERITY expression ] ;")

(define-ebnf-rule signal-assignment-statement
  ("[ label : ] simple-signal-assignment | [ label : ] conditional-signal-assignment"
   "| [ label : ] selected-signal-assignment"))

(define-ebnf-rule simple-signal-assignment
  ("( name | aggregate ) <= [ delay-mechanism ] waveform ;"
   "| name <= FORCE [ IN | OUT ] expression ; | name <= RELEASE [ IN | OUT ] ;"))

(define-ebnf-rule conditional-signal-assignment "conditional-waveform-assignment | conditional-force-assignment")

(define-ebnf-rule conditional-waveform-assignement
  ("[ label : ] ( name | aggregate ) <= [ delay-mechanism ] waveform WHEN condition"
   "{ ELSE waveform WHEN condition } [ ELSE waveform ] ;"))

(define-ebnf-rule conditional-force-assignement
  ("[ label : ] name <= FORCE [ IN | OUT ] expression WHEN condition { ELSE expression WHEN condition }"
   "[ ELSE expression ] ;"))

(define-ebnf-rule selected-signal-assignment "selected-waveform-assignment | selected-force-assignment")

(define-ebnf-rule selected-waveform-assignment
  ("[ label : ] WITH expression SELECT [ ? ] ( name | aggregate ) <= [ delay-mechanism ]"
   "{ waveform WHEN choices , } waveform WHEN choices ;"))

(define-ebnf-rule selected-force-assignment
  ("[ label : ] WITH expression SELECT [ ? ] name <= FORCE [ IN | OUT ]"
   "{ expression WHEN choices , } expression WHEN choices ;"))

(define-ebnf-rule delay-mechanism "TRANSPORT | [ REJECT TIME-expression ] INERTIAL")

(define-ebnf-rule waveform
  "( VALUE-expression [ AFTER TIME-expression ] | NULL [ AFTER TIME-expression ] ) {, ...} | UNAFFECTED")

(define-ebnf-rule variable-assignment-statement
  ("[ label : ] simple-variable-assignment _| [ label : ] conditional-variable-assignment"
   "_| [ label : ] selected-variable-assignment"))

(define-ebnf-rule simple-variable-assignment "( name | aggregate ) := expression ;"
  `(::= ,1st ,3rd))

(define-ebnf-rule conditional-variable-assignment
  "( name | aggregate ) := expression WHEN condition { ELSE expression WHEN condition } [ ELSE expression ] ;")

(define-ebnf-rule selected-variable-assignment
  ("WITH expression SELECT [ ? ] ( name | aggregate ) :="
   "{ expression WHEN choices , } expression WHEN choices ;"))

(define-ebnf-rule procedure-call-statement "[ label : ] PROCEDURE-name [ (( PARAMETER-association-list )) ] ;")

(define-ebnf-rule if-statement
  ("[ IF-label : ] IF condition THEN { sequential-statement } { ELSIF condition THEN { sequential-statement } }"
   "[ ELSE { sequential-statement } ] END IF [ IF-label ] ;"))

(define-ebnf-rule case-statement
  ("[ CASE-label : ] CASE _[ ? ] expression IS ( WHEN choices => { sequential-statement } ) { ... }"
   "END CASE _[ ? ] [ CASE-label ] ;"))

(define-ebnf-rule loop-statement
  ("[ LOOP-label : ] [ WHILE condition | FOR identifier IN discrete-range ] LOOP { sequential-statement }"
   "END LOOP [ LOOP-label ] ;"))

(define-ebnf-rule next-statement "[ label : ] NEXT [ LOOP-label ] [ WHEN condition ] ;")

(define-ebnf-rule exit-statement "[ label : ] EXIT [ LOOP-label ] [ WHEN condition ] ;")

(define-ebnf-rule return-statement "[ label : ] RETURN [ expression ] ;")

(define-ebnf-rule null-statement "[ label : ] NULL ;")


