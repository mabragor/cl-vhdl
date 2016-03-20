
(in-package #:cl-vhdl)

(enable-read-macro-tokens)


(define-vhdl-rule one-line-comment ()
  ;; TODO : maybe a bug with unused NREVERSE
  "--" `(:comment ,(text (times (!! (|| #\newline #\return))))))

(define-vhdl-rule multi-line-comment ()
  ;; TODO : does !! allow multiple characters ?
  "/*" (let ((it (times (!! "*/"))))
	 "*/"
	 `(:comment ,(text it))))

(define-vhdl-rule whitespace ()
  (postimes (|| #\space #\tab #\newline #\return
		one-line-comment))
  (literal-char #\space))


