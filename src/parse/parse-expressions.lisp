
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

(define-ebnf-rule qualified-expression "type-mark ' (( expression )) | type-mark ' aggregate")

(define-ebnf-rule name "compound-name | atomic-name | _external-name")

(define-ebnf-rule atomic-name "identifier | operator-symbol | character-literal"
  (when (reserved-word-p res)
    (fail-parse "Name can't be a reserved word"))
  res)

(define-ebnf-rule compound-name ("atomic-name name-tail { ... }")
  (cons 1st 2nd))

(define-ebnf-rule name-tail ("(( expression {, ...} ))"
			     "| (( discrete-range ))"
			     "| selected-tail"
			     "| attribute-tail"
			     "| funcall-tail"))

(define-ebnf-rule selected-tail ". ( atomic-name | ALL )")
(define-ebnf-rule attribute-tail "[ signature ] ' identifier [ (( expression )) ]")
(define-ebnf-rule funcall-tail "(( PARAMETER-association-list ))")

(define-ebnf-rule selected-name "compound-name"
  (if (not (string= "." (caar (last res))))
      (fail-parse "Not a selected name")
      res))
(define-ebnf-rule attribute-name "compound-name"
  (if (not (string= "'" (cadar (last res))))
      (fail-parse "Not an attribute name")
      res))

(define-ebnf-rule function-call "compound-name"
  ;; TODO : how to make a finer-grained check here?
  (if (not (string= "(" (cadar (last res))))
      (fail-parse "Not a function call")
      res))
  
(define-ebnf-rule operator-symbol "\"{ graphic-character }\"")


;; Apparently, [[ and ]] should denote [ and ] in text
(define-ebnf-rule signature
  "[[ [ type-mark {, ... } ] [ RETURN type-mark ] ]]")

(define-ebnf-rule external-name
  ("<< CONSTANT external-pathname : subtype-indication >>"
   "| << SIGNAL external-pathname : subtype-indication >>"
   "| << VARIABLE external-pathname : subtype-indication >>"))

(define-ebnf-rule external-pathname
  "absolute-pathname | relative-pathname | package-pathname")

(define-ebnf-rule absolute-pathname ". { pathname-element . } OBJECT-identifier")

(define-ebnf-rule relative-pathname "{ ^ . } { pathname-element . } OBJECT-identifier")

(define-ebnf-rule pathname-element
    ("ENTITY-identifier | COMPONENT-INSTANTIATION-label | BLOCK-label"
     "| GENERATE-STATEMENT-label [ (( STATIC-expression )) ] | PACKAGE-identifier"))

(define-ebnf-rule package-pathname "@ LIBRARY-identifier . { PACKAGE-identifier . } OBJECT-identifier")

(define-ebnf-rule literal
  ("based-literal | physical-literal | decimal-literal | identifier"
   "| character-literal | string-literal | bit-string-literal | NULL"))

(define-ebnf-rule aggregate "(( ( [ choices -> ] expression ) {, ... } ))")

(define-ebnf-rule choices "( simple-expression | discrete-range | identifier | OTHERS ) { || ... }")

(define-ebnf-rule label "identifier")

(define-ebnf-rule tool-directive "` identifier { graphic-character }")
