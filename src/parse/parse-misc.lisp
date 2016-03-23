
(in-package #:cl-vhdl)
(enable-read-macro-tokens)

(define-ebnf-rule physical-literal "[ decimal-literal | based-literal ] UNIT-name"
  `(,2nd ,(or 1st 1)))

(define-vhdl-rule decimal-literal () dec-number-literal)
(define-vhdl-rule based-literal () explicit-base-number-literal)

(define-ebnf-rule character-literal "' graphic-character '"
  2nd)

(define-vhdl-rule string-literal () vhdl-string)

;; These are PSL rules which for now are out of my scope
(define-vhdl-rule property-declaration ()
  (fail-parse "Not implemented."))
(define-vhdl-rule sequence-declaration ()
  (fail-parse "Not implemented."))

