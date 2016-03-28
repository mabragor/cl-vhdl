
(in-package #:cl-vhdl)

(cl-interpol:enable-interpol-syntax)

(defparameter *vhdl-version* nil)

(defparameter *vhdl-strict* t
  "When this parameter is NIL, some restrictions on possible VHDL syntax are relaxed,
   so as to make it more lispy")

(define-vhdl-rule one-line-comment ()
  (v "--") `(:comment ,(text (times (!! (|| #\newline #\return))))))

(define-vhdl-rule multi-line-comment ()
  (v "/*") (let ((it (times (!! "*/"))))
	     (v "*/")
	     `(:comment ,(text it))))

;; TODO : content of comments may be saved, to make it easier to understand how to
;;        emit comments when going S-expr -> text.

(define-vhdl-rule comment ()
  (if (find *vhdl-version* '(87 93 2002) :test #'equal)
      (v one-line-comment)
      (|| one-line-comment
	  multi-line-comment)))

(define-vhdl-rule whitespace ()
  (postimes (|| #\space #\tab #\newline #\return
		comment))
  #\space)


(define-vhdl-rule identifier ()
  (if (find *vhdl-version* '(87) :test #'equal)
      (v basic-identifier)
      (|| basic-identifier
	  extended-identifier)))

(define-vhdl-rule basic-identifier-string ()
  (text (postimes (|| (character-ranges (#\A #\Z) (#\a #\z) (#\0 #\9))
		      #\_))))

(define-vhdl-rule escaped-slash-char ()
  (v #\\) (v #\\))

(define-vhdl-rule extended-identifier-string ()
  (progm #\\
	 (text (postimes (|| escaped-slash-char
			     (!! #\\))))
	 #\\))

;; KLUDGE to get around ESRAP-LIQUID expanding these undefined variables of LOL-RE
(defvar $0)
(defvar $+0)
(defvar $-0)

(defun intern-here (x)
  (intern x "CL-VHDL"))

(define-vhdl-rule strict-basic-identifier ()
  (let ((str (v basic-identifier-string)))
    (if (not (cl-ppcre:all-matches-as-strings #?r"(?i)^[a-z]" str))
	(fail-parse "Basic identifier should start with a letter."))
    (if (cl-ppcre:all-matches-as-strings #?r"_$" str)
	(fail-parse "Basic identifier should not end with an underline char."))
    (if (cl-ppcre:all-matches-as-strings #?r"__" str)
	(fail-parse "Basic identifier should not contain two successive underlines."))
    (intern-here (cl-ppcre:regex-replace-all "_"
					     (string-upcase str)
					     "-"))))

(define-vhdl-rule relaxed-basic-identifier ()
  (let ((str (v basic-identifier-string)))
    (intern-here (cl-ppcre:regex-replace-all "_"
					     (string-upcase str)
					     "-"))))

(define-vhdl-rule unguarded-basic-identifier ()
  (if *vhdl-strict*
      (v strict-basic-identifier)
      (v relaxed-basic-identifier)))

(define-vhdl-rule basic-identifier ()
  (let ((it (v unguarded-basic-identifier)))
    (if (reserved-word-p it)
	(fail-parse "Identifier can't be reserved word")
	it)))

(define-vhdl-rule extended-identifier ()
  (let ((str (v extended-identifier-string)))
    ;; We add #\@, so that every extended identifier is distinct from every basic one
    (intern-here (concatenate 'string "@" str))))

(defparameter vhdl-reserved '((87
			       abs access after alias all and architecture array
			       attribute
			       begin block body buffer bus
			       case component configuration constant
			       disconnect downto
			       else elsif end entity exit
			       file for function
			       generate generic guarded
			       if in inout is
			       label library linkage loop
			       map mod
			       nand new next nor not null
			       of on open or others out
			       package port procedure process
			       range record register rem report return
			       select severity signal subtype
			       then to transport type
			       units untill use
			       variable
			       wait when while with
			       xor)
			      (93
			       group
			       impure inertial
			       literal
			       postponed pure
			       reject rol ror
			       shared sla sll sra srl
			       unaffected
			       xnor)
			      (2002
			       protected)
			      (2008
			       assert assume assume-guarantee
			       context cover
			       default
			       fairness force
			       parameter property
			       release restrict restrict-guarantee
			       sequence strong
			       vmode vprop vunit)))

(defun reserved-word-p (sym)
  (iter outer (for (version . kwds) in vhdl-reserved)
	(when (and *vhdl-version*
		   (> version *vhdl-version*))
	  ;; (format t "Not my version!~%")
	  (next-iteration))
	(if (find sym kwds :test #'eq)
	    (return-from outer sym))
	(finally (return-from outer nil))))
  

(define-vhdl-rule reserved-word ()
  (let ((it (v unguarded-basic-identifier)))
    (or (reserved-word-p it)
	(fail-parse "Not a reserved word."))))
	
			      
(define-vhdl-rule special-symbol ()
  (|| one-letter-special-symbol
      multi-letter-special-symbol))

(define-vhdl-rule one-letter-special-symbol ()
  (|| #\" #\# #\& #\' #\( #\) #\* #\+ #\- #\, #\. #\/ #\: #\; #\< #\= #\> #\? #\@ #\[ #\] #\` #\|))

(define-vhdl-rule multi-letter-special-symbol ()
  ;; TODO : I don't know what Lisp representation to pick for these symbols actually...
  (|| "=>" "**" ":=" "/=" ">=" "<=" "<>" "??" "?=" "?/=" "?>" "?<" "?>=" "?<=" "<<" ">>"))



(define-vhdl-rule dec-digit-elt-string ()
  (postimes (character-ranges (#\0 #\9))))

(define-vhdl-rule hex-digit-elt-string ()
  (postimes (character-ranges (#\0 #\9) (#\a #\f) (#\A #\F))))

(define-vhdl-rule dec-digit-string ()
  (let* ((first (v dec-digit-elt-string))
	 (rest (times (progn (v #\_) (v dec-digit-elt-string)))))
    (text first rest)))

(define-vhdl-rule hex-digit-string ()
  (let* ((first (v hex-digit-elt-string))
	 (rest (times (progn (v #\_) (v hex-digit-elt-string)))))
    (text first rest)))

(define-vhdl-rule generic-digit-elt-string ()
  (postimes (!! (|| #\_ #\newline #\return #\"))))

(define-vhdl-rule generic-digit-string ()
  (let* ((first (v generic-digit-elt-string))
	 (rest (times (progn (v #\_) (v generic-digit-elt-string)))))
    (text first rest)))

(define-vhdl-rule sign ()
  (|| #\+ #\-))

(define-vhdl-rule exponent ()
  (|| #\e #\E)
  (let* ((sign (? sign))
	 (str (v dec-digit-string)))
    (parse-integer (text sign str))))

(define-vhdl-rule integer-dec-number-literal ()
  (let* ((str (v dec-digit-string))
	 (expt (? exponent)))
    (cond ((not expt) (parse-integer str))
	  ((< expt 0) (fail-parse "Exponent of an integer literal can't be negative"))
	  (t (* (parse-integer str) (expt 10 expt))))))

;; TODO : maybe Lisp's infinite precision works against me here -- introducing subtle rounding errors
(define-vhdl-rule real-dec-number-literal ()
  (let* ((int (v dec-digit-string))
	 (rem (progn (v #\.) (v dec-digit-string)))
	 (expt (? exponent))
	 (base (float (+ (parse-integer int)
			 (* (expt 10 (- (length rem)))
			    (parse-integer rem))))))
    (if (not expt)
	base
	(float (* base (expt 10 expt))))))


(define-vhdl-rule dec-number-literal ()
  (most-full-parse integer-dec-number-literal
		   real-dec-number-literal))

(define-vhdl-rule number-literal ()
  (most-full-parse dec-number-literal
		   explicit-base-number-literal))

(define-vhdl-rule explicit-base-meat (base)
  (most-full-parse (v explicit-base-meat-integer base)
		   (v explicit-base-meat-real base)))

(define-vhdl-rule explicit-base-meat-integer (base)
  ;; TODO : check that actually only symbols which are valid for this base, are in the string
  (parse-integer (v hex-digit-string) :radix base))

(define-vhdl-rule explicit-base-meat-real (base)
  ;; TODO : check that actually only symbols which are valid for this base, are in the string
  (let* ((int (v hex-digit-string))
	 (rem (progn (v #\.) (v hex-digit-string))))
    (float (+ (parse-integer int :radix base)
	      (* (expt base (- (length rem)))
		 (parse-integer rem :radix base))))))

(define-vhdl-rule explicit-base-number-literal ()
  (let ((base (parse-integer (v dec-digit-string))))
    (declare (special base))
    (if (or (< base 2) (> base 16))
	(fail-parse-format "Explicit base should be between 2 and 16, but got ~a" base))
    (let* ((meat (progm #\# (v explicit-base-meat base) #\#))
	   (expt (? exponent)))
      (if (not expt)
	  meat
	  (if (integerp meat)
	      (if (< expt 0)
		  (fail-parse-format "Exponent for integer literal should be >= 0, but got ~a" expt)
		  (* meat (expt base expt)))
	      (float (* meat (expt base expt))))))))

(define-vhdl-rule vhdl-char ()
  (progm #\' character #\'))

(define-vhdl-rule string-char ()
  (|| (progn (v #\") (v #\"))
      (!! (|| #\newline #\return #\"))))

(define-vhdl-rule vhdl-string ()
  (text (progm #\" (times string-char) #\")))

(defun binary-char-p (x)
  (find x '(#\0 #\1) :test #'char=))

(defun octal-char-p (x)
  (find x '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7) :test #'char=))

(defun hex-char-p (x)
  (let ((code (char-code x)))
    (or (and (<= 48 code) (<= code 57))
	(and (<= 97 code) (<= code 102))
	(and (<= 65 code) (<= code 70)))))

(defun convert-to-binary-string (str init-radix &optional expand-unknown-chars)
  (macrolet ((handle-radix (radix name)
	       `(when (equal ,radix init-radix)
		  ;; TODO : Some syntax sugar for such cases (name mangling) should clearly be introduced
		  (cond ((,(intern #?"$(name)-CHAR-P") char)
			 (appending (coerce (format nil ,#?"~$((floor (log radix 2))),'0b"
						    (parse-integer (string char) :radix ,radix))
					    'list)))
			(expand-unknown-chars
			 (appending (make-list ,(floor (log radix 2)) :initial-element char)))
			(t (fail-parse-format ,#?"Binary string with radix $(radix) contains non-$((string-downcase name)) char ~a" char))))))
    (text (iter (for char in-string str)
		(handle-radix 2 binary)
		(handle-radix 8 octal)
		(handle-radix 16 hex)))))

(define-vhdl-rule simple-binary-string ()
  (let* ((radix (|| (progn (|| #\b #\B) 2)
		    (progn (|| #\o #\O) 8)
		    (progn (|| #\x #\X) 16))))
    `(:bin ,(convert-to-binary-string (progm #\" hex-digit-string #\")
				      radix))))

(define-vhdl-rule decimal-binary-string (explicit-length)
  (let ((it (format nil "~d" (parse-integer (progm #\" dec-digit-string #\")))))
    (cond ((not explicit-length) it)
	  ((> (length it) explicit-length) (fail-parse "Can't truncate decimal binary string"))
	  (t (concatenate 'string
			  (make-string (- explicit-length (length it)) :initial-element #\0)
			  it)))))

(define-vhdl-rule %convolved-binary-string (radix explicit-length signed-p)
  (let ((it (convert-to-binary-string (progm #\" generic-digit-string #\") radix t)))
    `(:bin ,(cond ((not explicit-length) it)
		  ((> explicit-length (length it))
		   (concatenate 'string
				(make-string (- explicit-length (length it))
					     :initial-element (if (not signed-p)
								  #\0
								  (char it 0)))
				it))
		  ((< explicit-length (length it))
		   ;; (format t (literal-string "Truncating ~a to ~a~%") it explicit-length)
		   (let ((minus (subseq it 0 (- (length it) explicit-length)))
			 (rest (subseq it (- (length it) explicit-length))))
		     ;; (format t (literal-string "~a ~a~%") minus rest)
		     (if (not (string= minus
				       (make-string (- (length it) explicit-length)
						    :initial-element (if (not signed-p)
									 #\0
									 (char rest 0)))))
			 (progn ;; (format t (literal-string "I'm here~%"))
				(fail-parse-format "Piece to truncate '~a' is not multiple of chars ~a"
						   minus
						   (char rest 0)))
			 (progn ;; (format t (literal-string "I'm there~%"))
				rest))))
		  (t it)))))
    
(define-vhdl-rule convolved-binary-string ()
  (let* ((explicit-length (let ((it (? dec-digit-string)))
			    (if it
				(parse-integer it)))))
    ;; TODO : this is really ugly way to do this dispatching -- I should have case-insensitive
    (|| (progn (|| #\d #\D) (v decimal-binary-string explicit-length))
	(progn (|| #\b #\B "UB" "uB" "Ub" "ub") (v %convolved-binary-string 2 explicit-length nil))
	(progn (|| "SB" "sB" "Sb" "sb") (v %convolved-binary-string 2 explicit-length t))
	(progn (|| #\o #\O "UO" "uO" "Uo" "uo") (v %convolved-binary-string 8 explicit-length nil))
	(progn (|| "SO" "sO" "So" "so") (v %convolved-binary-string 8 explicit-length t))
	(progn (|| #\x #\X "UX" "uX" "Ux" "ux") (v %convolved-binary-string 16 explicit-length nil))
	(progn (|| "SX" "sX" "Sx" "sx") (v %convolved-binary-string 16 explicit-length t)))))
	

(define-vhdl-rule binary-string ()
  (if (and *vhdl-version* (< *vhdl-version* 2008))
      (v simple-binary-string)
      (v convolved-binary-string)))

(define-vhdl-rule case-insensitive-string (str)
  (let ((res (string-downcase (v any-string (length str)))))
    (if (not (string= (string-downcase str) res))
	(fail-parse-format "Case-insensitive string ~s is different from expected ~s" res (string-downcase str))
	res)))

;;;; OK, let's write all the rules in EBNF, then figure out how to parse this condensed descrption
;;;; what semantics of all this is

(defmacro with-list-places ((lst) &body body)
  `(symbol-macrolet ((1st (first ,lst)) (2nd (second ,lst)) (3rd (third ,lst)) (4th (fourth ,lst))
		     (5th (fifth ,lst)) (6th (sixth ,lst)) (7th (seventh ,lst)) (8th (eighth ,lst))
		     (9th (ninth ,lst)) (10th (tenth ,lst)))
     ,@body))

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it
	 ,then
	 ,else)))

;; We will change this to something meaningful later
(defmacro define-ebnf-rule (name syntax &body body)
  `(%define-ebnf-rule ,name ,(s-exp<-ebnf syntax) ,@body))

(defmacro %define-ebnf-rule (name syntax &body body)
  (multiple-value-bind (liquid-body greedy-char-seqs)
      (esrap-liquid-body syntax)
    `(progn ,@(mapcar (lambda (x)
			`(update-greedy-char-seq-table ,x))
		      greedy-char-seqs)
	    (macrolet ((wh? (&body x) `(progn (? whitespace) ,@x))
		       (new (x &optional (y nil y-p))
			 (if y-p
			     `(if (or (not *vhdl-version*)
				      (<= ,x *vhdl-version*))
				  ,y
				  (fail-parse "Version we are trying to parse doesn't support this"))
			     `(if (or (not *vhdl-version*)
				      (<= 2008 *vhdl-version*)) ; current version as these lines are written
				  ,x))))
	      (define-vhdl-rule ,name (&optional hint)
		(let ((res ,liquid-body))
		  (with-list-places (res)
		    ,@(or body `(res)))))))))
    

;; OK, now that I have all the syntactic rules explicitly written down,
;; I need to figure out a way, how to generate their parsed versions

;; * just a S-exp parsed version of EBNF
;; * parser code
;; * way to do custom adjustments to this code

;; For example, where does this descrption map?
;; "INSTANTIATION-label : ( [ COMPONENT ] COMPONENT-name"
;; "                        | ENTITY ENTITY-name [ (( ARCHITECTURE-identifier )) ]"
;; "                        | CONFIGURATION CONFIGURATION-name )"
;; "[ GENERIC MAP (( GENERIC-association-list )) ] [ PORT MAP (( PORT-association-list )) ] ;"))

;; We are after "things"
;; Tokens we encode as keywords or strings

;; ((label :instantiation)
;;  ":"
;;  (|| ((? :component) (name :component))
;;      (:entity (name :entity) (? "(" (identifier :architecture) ")"))
;;      (:configuration (name :configuration)))
;;  (? :generic :map "(" (association-list :generic) ")")
;;  (? :port :map "(" (association-list :port) ")"))

;; Let's do some more examples

;; descents *should* be inside lists, as otherwise I can't tell whether
;; (name :kwd) means list of descent 'name' and a KWD, or is KWD an advice for name.

;; (|| (decimal-literal) based-literal physical-literal identifier
;;     character-literal string-literal bit-string-literal :null) ; here this :null can't be discarded

;; if CAR is a symbol not from KEYWORD package and not a reserved symbol, then it's interpreted as descent

;; (shift-expression (? (|| "=" "/=" "<" ... (new "?=") ...)
;;                      shift-expression))

;; (|| (relation (* :and relation))
;;     (relation (? :nand relation))
;;     (relation (* :or relation))
;;     (relation (? :nor relation))
;;     (relation (* :xor relation))
;;     (relation (* :xnor relation))) ; none of these keywords in sub-clauses can be discarded

;; (|| ((new (? :inertial)) (expression))
;;     ((name :signal))
;;     ((name :variable))
;;     ((name :file))
;;     (new (subtype-indication))
;;     (new (name :subprogram))
;;     (new (name :package))
;;     :open
;;     ((name :function) "(" (|| (name :signal) (name :variable)) ")")
;;     ((type-mark) "(" (|| (name :signal) (name :variable)) ")"))

;; First argument of + is always the delimiter
;; (|| (+ "," (|| ((expression :value) (? :after (expression :time)))
;;                (:null (? :after (expression :time)))))
;;     :unaffected)

;; ((? (label) ":") :with (expression) :select (? "?")
;;  (name) "<=" :force (? (|| :in :out))
;;              (* (expression) :when choices ",")
;;              (expression) :when choices ";")

;; Ok, and now what will be the structure of the parsed data
;; This should omit some symbols (which are necessary for grammar), but leave
;; symbols, that convey some useful information

;; I'd better write a checker, that forbids me to define a given rule twice
;; (because there are misprints in the book)


;; ((? (label) ":") :with (expression) :select (? "?")
;;  (name) "<=" :force (? (|| :in :out)) (* (expression) :when choices ",") (expression) :when choices ";")

;; No, apparently, I need to actually read about how some feature is used to develop
;; a good Lisp syntax for it

;; Ok, how do I transform my intermediate s-exp-form into something ESRAP-LIQUID would actually understand?
;; Let's do couple of examples manually...

;; * For now we ignore the hints for sub-rules
;; * Obviously, all this relies on the rule 'whitespace' to be already defined
;; * NEW will be just a new macrolet, which I will define around DEFINE-VHDL-RULE
;; * if I assume macrolet WH? --> (progn (? whitespace ...)), then everything is a little bit shorter

;; (wh? (|| (wh? (list (wh? (? (wh? (descend-with-rule 'case-insensitive-string "inertial") :inertial)))
;; 		    (wh? expression)))
;; 	 (wh? name)
;; 	 (new (wh? subtype-indication))
;; 	 (new (wh? name))
;; 	 (wh? (descend-with-rule 'case-insensitive-string "open") :open)
;; 	 (list (wh? name)
;; 	       (wh? (descend-with-rule 'case-insensitive-string "("))
;; 	       (|| (wh? name)
;; 		   (wh? name))
;; 	       (wh? (descend-with-rule 'case-insensitive-string ")")))
;; 	 (list (wh? type-mark)
;; 	       (wh? (descend-with-rule 'case-insensitive-string "("))
;; 	       (|| (wh? name) (wh? name))
;; 	       (wh? (descend-with-rule 'case-insensitive-string ")")))))

;; (WH? (|| (WH? IDENTIFIER)
;; 	 (WH? LABEL)
;; 	 (WH? LABEL)
;; 	 (WH? (LIST (WH? LABEL)
;; 		    (WH? (? (LIST (WH? (DESCEND-WITH-RULE 'CASE-INSENSITIVE-STRING "("))
;; 				  (WH? EXPRESSION)
;; 				  (WH? (DESCEND-WITH-RULE 'CASE-INSENSITIVE-STRING ")")))))))
;; 	 (WH? IDENTIFIER)))

;; TODO : what are these graphic characters, really?
(define-vhdl-rule graphic-character ()
  (v character))

(define-vhdl-rule bit-string-literal ()
  (v binary-string))

;; (!! (|| whitespace
;; 	;; #\;
;; 	)))

(defmacro wrapping-in-label (&optional (expr `(cdr res)))
  (with-gensyms (g!-it)
    `(let ((,g!-it ,expr))
       (if 1st
	   `(:label ,(car 1st) ,,g!-it)
	   ,g!-it))))

