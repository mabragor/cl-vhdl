
(in-package #:cl-vhdl)
(enable-read-macro-tokens)

(define-ebnf-rule physical-literal "[ decimal-literal | based-literal ] UNIT-name")

(define-vhdl-rule decimal-literal () dec-number-literal)
(define-vhdl-rule based-literal () explicit-base-number-literal)

(define-ebnf-rule character-literal "' graphic-character '")

(define-vhdl-rule string-literal () vhdl-string)

;; These are PSL rules which for now are out of my scope
(define-vhdl-rule property-declaration ()
  (fail-parse "Not implemented."))
(define-vhdl-rule sequence-declaration ()
  (fail-parse "Not implemented."))

