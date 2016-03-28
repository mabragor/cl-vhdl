
(in-package #:cl-vhdl)

(define-ebnf-rule physical-literal "[ decimal-literal | based-literal ] UNIT-name"
  ;; At this lexical stage of parsing we can't tell whether something is a unit or a variable name
  (if (not 1st)
      (fail-parse "Implicit physical literals can't be resolved at this stage.")
      `(,2nd ,1st)))

(define-vhdl-rule decimal-literal () (v dec-number-literal))
(define-vhdl-rule based-literal () (v explicit-base-number-literal))

(define-ebnf-rule character-literal "' graphic-character '"
  2nd)

(define-vhdl-rule string-literal () (v vhdl-string))

;; These are PSL rules which for now are out of my scope
(define-vhdl-rule property-declaration (&optional hint)
  (fail-parse "Not implemented."))
(define-vhdl-rule sequence-declaration (&optional hint)
  (fail-parse "Not implemented."))
(define-vhdl-rule clock-declaration (&optional hint)
  (fail-parse "Not implemented."))
(define-vhdl-rule verification-unit (&optional hint)
  (fail-parse "Not implemented."))
(define-vhdl-rule psl-directive (&optional hint)
  (fail-parse "Not implemented."))

