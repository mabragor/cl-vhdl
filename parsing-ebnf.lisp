
(in-package #:cl-vhdl)

(define-ebnf-aux-rule big-letter () (character-ranges (#\A #\Z)))
(define-ebnf-aux-rule small-letter () (character-ranges (#\a #\z)))
(define-ebnf-aux-rule digit () (character-ranges (#\0 #\9)))
(define-ebnf-aux-rule whitespace () (postimes #\space))

(define-ebnf-aux-rule big-word ()
  (text (postimes big-letter)))

(define-ebnf-aux-rule small-word ()
  (text (postimes small-letter)))

(define-ebnf-aux-rule kwd-token ()
  (intern (prog1 big-word (& (|| #\space 'eof)))
	  (literal-string "KEYWORD")))

(defmacro!! define-plural-rule (name single delim) ()
  `(define-ebnf-aux-rule ,name ()
     (cons ,single
	   (times (progn ,delim ,single)))))

(define-plural-rule raw-rule-name small-word "-")
(define-plural-rule raw-rule-advice big-word "-")

(define-ebnf-aux-rule rule-name ()
  (let* ((advice (? (prog1 raw-rule-advice "-")))
	 (name (joinl (literal-string "-") raw-rule-name)))
    `(,(intern (string-upcase (text name)))
       ,@(if advice `(,(intern (joinl (literal-string "-") advice)
			       (literal-string "KEYWORD")))))))

(define-ebnf-aux-rule escape-char ()
  (|| (progn #\( #\() (progn #\) #\)) (progn #\[ #\[) (progn #\] #\]) (progn #\{ #\{) (progn #\} #\})
      (progn #\| #\|) (progn #\. #\. #\. (text #\. #\. #\.))))
      

(define-ebnf-aux-rule new-in-version ()
  (|| (let* ((version (? (postimes digit))))
	"_"
	(if version
	    (parse-integer (text version))
	    t))
      nil))

(define-ebnf-aux-rule %non-or-atom ()
  (|| ;; atomic expressions
   (|| rule-name
       kwd-token
       misc-char-sequence)
   ;; compound expressions
   (|| paren-expression
       optional-expression
       times-expression)))

(define-ebnf-aux-rule non-or-atom ()
  (? whitespace) c!-1-new-in-version c!-2-%non-or-atom
  (cond ((null c!-1) c!-2)
	((eq 't c!-1) `(new ,c!-2))
	(t `(new ,c!-1 ,c!-2))))

(define-ebnf-aux-rule or-rhs-sequence ()
  (? whitespace) c!-1-new-in-version #\| (? whitespace) c!-2-non-or-sequence
  (cond ((null c!-1) c!-2)
	((eq 't c!-1) `(new ,c!-2))
	(t `(new ,c!-1 ,c!-2))))


(define-ebnf-aux-rule non-or-sequence ()
  (let ((first non-or-atom)
	(rest (times (progn (? whitespace) non-or-atom))))
    (cons first rest)))

(define-ebnf-aux-rule or-expression ()
  (let ((first non-or-sequence)
	(rest (postimes or-rhs-sequence)))
    `((|| ,first ,@rest))))

(define-ebnf-aux-rule expression ()
  (prog1 (|| or-expression
	     non-or-sequence)
    (? whitespace)))

(define-ebnf-aux-rule top-expression ()
  (transform-vararg-times expression))

(defun token-p (x)
  (and (listp x)
       (symbolp (car x))
       (not (keywordp (car x)))
       (not (find (car x) '(? * + ||)))
       (or (= 1 (length x))
	   (= 2 (length x)))))

(defun special-form-p (x)
  (and (listp x)
       (find (car x) '(? * + ||))))
	     
(defun transform-vararg-times (expr)
  "Here we recursively look for all { ... }'s and replace them with +'s"
  (if (atom expr)
      expr
      (if (and (equal 1 (length expr))
	       (not (token-p expr)))
	  (transform-vararg-times (car expr))
	  (iter (generate elt on expr)
		(if (and (consp (cadr (next elt)))
			 (eq '|...| (car (cadr elt))))
		    (progn (collect (let ((elt (transform-vararg-times (car elt)))
					  (delim (transform-vararg-times (cadr (cadr elt)))))
				      (if (or (token-p elt)
					      (special-form-p elt)
					      (atom elt))
					  `(+ ,delim ,elt)
					  `(+ ,delim ,@elt))))
			   (next elt))
		    (collect (transform-vararg-times (car elt))))))))


(define-ebnf-aux-rule paren-expression ()
  (progm #\( expression #\)))

(define-ebnf-aux-rule optional-expression ()
  `(? ,@(progm #\[ expression #\])))

(define-ebnf-aux-rule triple-dot ()
  (let ((dots (postimes #\.)))
    (if (equal 3 (length dots))
	'|...|
	(fail-parse "Not a triple dot"))))

(define-ebnf-aux-rule times-expression ()
  (|| `(* ,@(progm #\{ expression #\}))
      `(|...| ,(progm #\{ (? expression) (progn (? whitespace) triple-dot (? whitespace) #\})))))

(define-ebnf-aux-rule special-char ()
  (|| #\( #\) #\[ #\] #\{ #\} #\| triple-dot whitespace))

(define-ebnf-aux-rule misc-char-sequence ()
  (text (postimes (|| escape-char
		      (!! special-char)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun s-exp<-ebnf (syntax)
    (let ((syntax (if (consp syntax)
		      (joinl " " syntax)
		      syntax)))
      (ebnf-aux-parse 'top-expression syntax))))

(defun maybe-list (thing)
  (if (equal 1 (length thing))
      (esrap-liquid-body (car thing))
      `(list ,@(mapcar #'esrap-liquid-body thing))))

(defun! esrap-liquid-body (thing)
  (if (atom thing)
      (cond ((keywordp thing) `(wh? (descend-with-rule 'case-insensitive-string ,(string-downcase thing)) ,thing))
	    ((stringp thing) `(wh? (descend-with-rule 'case-insensitive-string ,thing)))
	    (t (error "Don't know how to generate ESRAP-LIQUID code for atom: ~a" thing)))
      (cond ((eq '|| (car thing)) `(wh? (|| ,@(mapcar #'esrap-liquid-body (cdr thing)))))
	    ((eq '+ (car thing)) (if (null (cadr thing))
				     `(postimes ,(maybe-list (cddr thing)))
				     `(let* ((,g!-first ,(maybe-list (cddr thing)))
					     (,g!-rest (times (wh? (progn ,(esrap-liquid-body (cadr thing))
									  ,(maybe-list (cddr thing)))))))
					(cons ,g!-first ,g!-rest))))
	    ((eq '* (car thing)) `(wh? (times ,(maybe-list (cdr thing)))))
	    ((eq '? (car thing)) `(wh? (? ,(maybe-list (cdr thing)))))
	    ((token-p thing) `(wh? ,(car thing)))
	    (t `(wh? ,(maybe-list thing))))))
	     
;;(t (error "Don't know how to generate ESRAP-LIQUID code for cons: ~a" thing)))))
	
