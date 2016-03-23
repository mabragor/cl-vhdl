
(in-package #:cl-vhdl)
(enable-read-macro-tokens)



;;; Expressions and Names

(define-ebnf-rule condition "expression")

(define-ebnf-rule expression "( ?? primary ) | logical-expression")

(define-ebnf-rule logical-expression
  ("relation { AND relation } | relation [ NAND relation ] | relation { OR relation }"
   "| relation [ NOR relation ] | relation { XOR relation } | relation { XNOR relation }"))

(define-ebnf-rule relation
  "shift-expression [ ( = | /= | < | <= | > | >= | ?= | _?= | _?/= | _?< | _?<= | _?> | _?>= ) shift-expression ]")

(define-ebnf-rule shift-expression
  "simple-expression [ ( SLL | SRL | SLA | SRA | ROL | ROR ) simple-expression ]")

(define-ebnf-rule simple-expression "[ + | - ] term { ( + | - | & ) term }")

(define-ebnf-rule term "factor { ( * | / | MOD | REM ) factor }")

(define-ebnf-rule factor
  ("primary [ ** primary ] | ABS primary | NOT primary | AND primary | NAND primary | OR primary"
   "| NOR primary | XOR primary | XNOR primary"))

(define-ebnf-rule primary
  ("name | literal | aggregate | function-call | qualified-expression | type-mark (( expression ))"
   "| NEW subtype-indication | NEW qualified-expression | (( expression ))"))

(define-ebnf-rule function-call "FUNCTION-name [ (( PARAMETER-association-list )) ]")

(define-ebnf-rule qualified-expression "type-mark ' (( expression )) | type-mark ' aggregate")

(define-ebnf-rule name
  ("identifier | operator-symbol | character-literal | selected-name"
   "| ( name | function-call ) (( expression {, ...} ))"
   "| ( name | function-call ) (( discrete-range ))"
   "| attribute-name | _external-name"))

(define-ebnf-rule selected-name
  "( name | function-call ) . ( identifier | character-literal | operator-symbol | ALL )")

(define-ebnf-rule operator-symbol "\"{ graphic-character }\"")

(define-ebnf-rule attribute-name
  "( name | function-call ) [ signature ] ' identifier [ (( expression )) ]")

;; Apparently, [[ and ]] should denote [ and ] in text
(define-ebnf-rule signature
  "[[ [ type-mark {, ... } ] [ RETURN type-mark ] ]]")

(define-ebnf-rule _external-name
  ("<< CONSTANT external-pathname : subtype-indication >>"
   "| << SIGNAL external-pathname : subtype-indication >>"
   "| << VARIABLE external-pathname : subtype-indication >>"))

(define-ebnf-rule _external-pathname
  "absolute-pathname | relative-pathname | package-pathname")

(define-ebnf-rule _absolute-pathname ". { pathname-element . } OBJECT-identifier")

(define-ebnf-rule _relative-pathname "{ ^ . } { pathname-element . } OBJECT-identifier")

(define-ebnf-rule pathname-element
    ("ENTITY-identifier | COMPONENT-INSTANTIATION-label | BLOCK-label"
     "| GENERATE-STATEMENT-label [ (( STATIC-expression )) ] | PACKAGE-identifier"))

(define-ebnf-rule _package-pathname "@ LIBRARY-identifier . { PACKAGE-identifier . } OBJECT-identifier")

(define-ebnf-rule literal
  ("decimal-literal | based-literal | physical-literal | identifier"
   "| character-literal | string-literal | bit-string-literal | NULL"))

(define-ebnf-rule aggregate "(( ( [ choices -> ] expression ) {, ... } ))")

(define-ebnf-rule choices "( simple-expression | discrete-range | identifier | OTHERS ) { || ... }")

(define-ebnf-rule label "identifier")

(define-ebnf-rule _tool-directive "` identifier { graphic-character }")
