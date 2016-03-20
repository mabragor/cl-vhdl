
(in-package #:cl-vhdl)

(enable-read-macro-tokens)


(define-vhdl-rule one-line-comment ()
  ;; TODO : maybe a bug with unused NREVERSE
  "--" (times (!! (|| #\newline #\return)))
  (literal-char #\space))

(define-vhdl-rule multi-line-comment ()
  ;; TODO : does !! allow multiple characters ?
  "/*" (times (!! "*/")) "*/"
  (literal-char #\space))

(define-vhdl-rule whitespace ()
  (postimes (|| #\space #\tab #\newline #\return
		one-line-comment))
  (literal-char #\space))


