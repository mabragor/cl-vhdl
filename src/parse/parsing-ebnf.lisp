
(in-package #:cl-vhdl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *greedy-char-map* nil
    "In this, all the dynamically found misc keywords would be placed, for later greedy parsing"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun joinl (joinee lst)
    (format nil (concatenate 'string "~{~a~^" joinee "~}") lst)))

(define-ebnf-aux-rule big-letter () (character-ranges (#\A #\Z)))
(define-ebnf-aux-rule small-letter () (character-ranges (#\a #\z)))
(define-ebnf-aux-rule digit () (character-ranges (#\0 #\9)))
(define-ebnf-aux-rule whitespace () (postimes #\space))

(define-ebnf-aux-rule big-word ()
  (text (postimes big-letter)))

(define-ebnf-aux-rule small-word ()
  (text (postimes small-letter)))

(define-ebnf-aux-rule kwd-token ()
  (intern (prog1 (v big-word) (& (|| #\space 'eof)))
	  "KEYWORD"))

(defmacro define-plural-rule (name single delim) ()
  `(define-ebnf-aux-rule ,name ()
     (cons ,(maybe-wrap-in-descent single)
	   (times (progn ,(maybe-wrap-in-descent delim) ,(maybe-wrap-in-descent single))))))

(define-plural-rule raw-rule-name small-word "-")
(define-plural-rule raw-rule-advice big-word "-")

(define-ebnf-aux-rule rule-name ()
  (let* ((advice (? (prog1 (v raw-rule-advice) (v "-"))))
	 (name (joinl "-" (v raw-rule-name))))
    `(,(intern (string-upcase (text name)))
       ,@(if advice `(,(intern (joinl "-" advice)
			       "KEYWORD"))))))

(define-ebnf-aux-rule escape-char ()
  (|| (progn (v #\() (v #\()) (progn (v #\)) (v #\)))
      (progn (v #\[) (v #\[)) (progn (v #\]) (v #\]))
      (progn (v #\{) (v #\{)) (progn (v #\}) (v #\}))
      (progn (v #\|) (v #\|)) (progn (v #\.) (v #\.) (v #\.) (text (v #\.) (v #\.) (v #\.)))))
      

(define-ebnf-aux-rule new-in-version ()
  (|| (let* ((version (? (postimes digit))))
	(v "_")
	(if version
	    (parse-integer (text version))
	    t))
      'nil))

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
  (? whitespace) (cap new new-in-version) (cap thing %non-or-atom)
  (cond ((null (recap new)) (recap thing))
	((eq 't (recap new)) `(new ,(recap thing)))
	(t `(new ,(recap new) ,(recap thing)))))

(define-ebnf-aux-rule or-rhs-sequence ()
  (? whitespace) (cap new new-in-version) (v #\|) (? whitespace) (cap thing non-or-sequence)
  (cond ((null (recap new)) (recap thing))
	((eq 't (recap new)) `(new ,(recap thing)))
	(t `(new ,(recap new) ,(recap thing)))))


(define-ebnf-aux-rule non-or-sequence ()
  (let ((first (v non-or-atom))
	(rest (times (progn (? whitespace) (v non-or-atom)))))
    (cons first rest)))

(define-ebnf-aux-rule or-expression ()
  (let ((first (v non-or-sequence))
	(rest (postimes or-rhs-sequence)))
    `((|| ,first ,@rest))))

(define-ebnf-aux-rule expression ()
  (prog1 (|| or-expression
	     non-or-sequence)
    (? whitespace)))

(define-ebnf-aux-rule top-expression ()
  (transform-vararg-times (v expression)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *special-symbols* '(? * + || new)))

(defun token-p (x)
  (and (listp x)
       (symbolp (car x))
       (not (keywordp (car x)))
       (not (find (car x) *special-symbols*))
       (or (= 1 (length x))
	   (= 2 (length x)))))

(defun special-form-p (x)
  (and (listp x)
       (find (car x) *special-symbols*)))
	     
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
      `(|...| ,(progm #\{ (? expression) (progn (? whitespace) (v triple-dot) (? whitespace) (v #\}))))))

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

(defun update-greedy-char-seq-table (str)
  (let ((it (assoc (char str 0) *greedy-char-map* :test #'char=)))
    (when (not it)
      (setf it (list (char str 0)))
      (push it *greedy-char-map*))
    (iter (for char in-string str)
	  (if-first-time nil
			 (let ((new-it (assoc char (cdr it) :test #'char=)))
			   (when (not new-it)
			     (push (list char) (cdr it))
			     (setf new-it (cadr it)))
			   (setf it new-it))))
    :success!))

(define-vhdl-rule greedy-char-seq (desired-str)
  (let ((cur-map *greedy-char-map*)
	sym)
    (let ((str (loop (let* ((char (? (& character))) ; here we 'pick' the character
			    (it (assoc char cur-map)))
		       (if (or (not char) (not it))
			   (if (not sym)
			       (fail-parse "Greedy char seq failed")
			       (return (text (nreverse sym))))
			   (progn (push (v character) sym) ; and here, if it's the right one, we actually advance
				  (setf cur-map (cdr it))))))))
      (if (string= desired-str str)
	  (intern str "KEYWORD")
	  (fail-parse-format "Greedy char seq ~s doesn't match the desired one ~s" str desired-str)))))
	    
(defun esrap-liquid-body (thing)
  (let (greedy-char-seqs)
    (labels ((maybe-list (thing)
	       (if (equal 1 (length thing))
		   (rec (car thing))
		   `(list ,@(mapcar #'rec thing))))
	     (rec (thing)
	       (if (atom thing)
		   (cond ((keywordp thing) `(wh? (v case-insensitive-string
						    ,(string-downcase thing))
						 (! (character-ranges (#\a #\z) (#\A #\Z)))
						 ,thing))
			 ((stringp thing) (progn (push thing greedy-char-seqs)
						 `(wh? (v greedy-char-seq ,thing))))
			 (t (error "Don't know how to generate ESRAP-LIQUID code for atom: ~a" thing)))
		   (cond ((eq '|| (car thing)) `(wh? (most-full-parse ,@(mapcar #'rec (cdr thing)))))
			 ((eq '+ (car thing))
			  (if (null (cadr thing))
			      `(prog1 (postimes ,(maybe-list (cddr thing)))
				 (? whitespace))
			      (with-gensyms (g!-first g!-rest g!-extra)
				`(let* ((,g!-first ,(maybe-list (cddr thing)))
					(,g!-rest (times (wh? (progn ,(rec (cadr thing))
								     ,(maybe-list (cddr thing))))))
					(,g!-extra (wh? (? ,(rec (cadr thing))))))
				   (declare (ignore ,g!-extra))
				   (cons ,g!-first ,g!-rest)))))
			 ((eq '* (car thing)) `(wh? (times ,(maybe-list (cdr thing)))))
			 ((eq '? (car thing)) `(wh? (? ,(maybe-list (cdr thing)))))
			 ((eq 'new (car thing)) (if (equal 2 (length thing))
						    `(new ,(rec (cadr thing)))
						    `(new ,(cadr thing) ,(rec (caddr thing)))))
			 ((token-p thing) `(wh? (v ,@thing)))
			 (t `(wh? ,(maybe-list thing)))))))
      ;; (format t "~a~%" greedy-char-seqs)
      (values (rec thing) (nreverse greedy-char-seqs)))))
	     
;;(t (error "Don't know how to generate ESRAP-LIQUID code for cons: ~a" thing)))))
	
