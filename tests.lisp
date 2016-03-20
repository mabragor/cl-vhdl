(in-package :cl-user)

(defpackage :cl-vhdl-tests
  (:use :alexandria :cl :cl-vhdl :fiveam :iterate :cl-read-macro-tokens)
  (:export #:run-tests))

(in-package :cl-vhdl-tests)

(cl-interpol:enable-interpol-syntax)
(enable-read-macro-tokens)

(def-suite vhdl)
(in-suite vhdl)

(defun run-tests ()
  (let ((results (run 'vhdl)))
    (fiveam:explain! results)
    (unless (fiveam:results-status results)
      (error "Tests failed."))))

(test whitespace
  (is (equal '(:comment " asdf") (vhdl-parse 'one-line-comment "-- asdf")))
  (is (equal '((:comment " asdf *a ") 13)
	     (multiple-value-list (vhdl-parse 'multi-line-comment "/* asdf *a */ " :junk-allowed t)))))

(test reserved-words
  (is (equal 'cl-vhdl::default (let ((*vhdl-version* nil)) (vhdl-parse 'reserved-word "default"))))
  (is (equal :caboom!
	     (handler-case (let ((*vhdl-version* 87)) (vhdl-parse 'reserved-word "default"))
	       (esrap-liquid::esrap-error () :caboom!)))))


