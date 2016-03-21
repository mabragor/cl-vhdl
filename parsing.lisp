
(in-package #:cl-vhdl)

(enable-read-macro-tokens)
(cl-interpol:enable-interpol-syntax)

(defparameter *vhdl-version* nil)

(defparameter *vhdl-strict* nil
  "When this parameter is NIL, some restrictions on possible VHDL syntax are relaxed,
   so as to make it more lispy")

(define-vhdl-rule one-line-comment ()
  "--" `(:comment ,(text (times (!! (|| #\newline #\return))))))

(define-vhdl-rule multi-line-comment ()
  "/*" (let ((it (times (!! "*/"))))
	 "*/"
	 `(:comment ,(text it))))

;; TODO : content of comments may be saved, to make it easier to understand how to
;;        emit comments when going S-expr -> text.

(define-vhdl-rule comment ()
  (if (find *vhdl-version* '(87 93 2002) :test #'equal)
      one-line-comment
      (|| one-line-comment
	  multi-line-comment)))

(define-vhdl-rule whitespace ()
  (postimes (|| #\space #\tab #\newline #\return
		comment))
  (literal-char #\space))


(define-vhdl-rule identifier ()
  (if (find *vhdl-version* '(87) :test #'equal)
      basic-identifier
      (|| basic-identifier
	  extended-identifier)))

(define-vhdl-rule basic-identifier-string ()
  (text (postimes (|| (character-ranges (#\A #\Z) (#\a #\z) (#\0 #\9))
		      #\_))))

(define-vhdl-rule escaped-slash-char ()
  #\\ #\\)

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
  (let ((str basic-identifier-string))
    (if (not (m~ "(?i)^[a-z]" str))
	(fail-parse "Basic identifier should start with a letter."))
    (if (m~ "_$" str)
	(fail-parse "Basic identifier should not end with an underline char."))
    (if (m~ "__" str)
	(fail-parse "Basic identifier should not contain two successive underlines."))
    (intern-here (s~ "_" "-" (string-upcase str)))))

(define-vhdl-rule relaxed-basic-identifier ()
  (let ((str basic-identifier-string))
    (intern-here (s~ "_" "-" (string-upcase str)))))

(define-vhdl-rule basic-identifier ()
  (if *vhdl-strict*
      strict-basic-identifier
      relaxed-basic-identifier))

(define-vhdl-rule extended-identifier ()
  (let ((str extended-identifier-string))
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

(define-vhdl-rule reserved-word ()
  (let ((it basic-identifier))
    (iter outer (for (version . kwds) in vhdl-reserved)
	  (when (and *vhdl-version*
		     (> version *vhdl-version*))
	    (format t "Not my version!~%")
	    (next-iteration))
	  (if (find it kwds :test #'eq)
	      (return-from outer it))
	  (finally (fail-parse "Not a reserved word")))))
	  
			      
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
  (let* ((first dec-digit-elt-string)
	 (rest (times (progn #\_ dec-digit-elt-string))))
    (text first rest)))

(define-vhdl-rule hex-digit-string ()
  (let* ((first hex-digit-elt-string)
	 (rest (times (progn #\_ hex-digit-elt-string))))
    (text first rest)))

(define-vhdl-rule generic-digit-elt-string ()
  (postimes (!! (|| #\_ #\newline #\return #\"))))

(define-vhdl-rule generic-digit-string ()
  (let* ((first generic-digit-elt-string)
	 (rest (times (progn #\_ generic-digit-elt-string))))
    (text first rest)))

(define-vhdl-rule sign ()
  (|| #\+ #\-))

(define-vhdl-rule exponent ()
  (|| #\e #\E)
  (let* ((sign (? sign))
	 (str dec-digit-string))
    (parse-integer (text sign str))))

(define-vhdl-rule integer-dec-number-literal ()
  (let* ((str dec-digit-string)
	 (expt (? exponent)))
    (cond ((not expt) (parse-integer str))
	  ((< expt 0) (fail-parse "Exponent of an integer literal can't be negative"))
	  (t (* (parse-integer str) (expt 10 expt))))))

;; TODO : maybe Lisp's infinite precision works again me here -- introducing subtle rounding errors
(define-vhdl-rule real-dec-number-literal ()
  (let* ((int dec-digit-string)
	 (rem (progn #\. dec-digit-string))
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
  (most-full-parse (descend-with-rule 'explicit-base-meat-integer base)
		   (descend-with-rule 'explicit-base-meat-real base)))

(define-vhdl-rule explicit-base-meat-integer (base)
  ;; TODO : check that actually only symbols which are valid for this base, are in the string
  (parse-integer hex-digit-string :radix base))

(define-vhdl-rule explicit-base-meat-real (base)
  ;; TODO : check that actually only symbols which are valid for this base, are in the string
  (let* ((int hex-digit-string)
	 (rem (progn #\. hex-digit-string)))
    (float (+ (parse-integer int :radix base)
	      (* (expt base (- (length rem)))
		 (parse-integer rem :radix base))))))

(define-vhdl-rule explicit-base-number-literal ()
  (let ((base (parse-integer dec-digit-string)))
    (declare (special base))
    (if (or (< base 2) (> base 16))
	(fail-parse-format "Explicit base should be between 2 and 16, but got ~a" base))
    (let* ((meat (progm #\# (descend-with-rule 'explicit-base-meat base) #\#))
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
  (|| (progn #\" #\")
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
			  (make-string (- explicit-length (length it)) :initial-element (literal-char #\0))
			  it)))))

(define-vhdl-rule %convolved-binary-string (radix explicit-length signed-p)
  (let ((it (convert-to-binary-string (progm #\" generic-digit-string #\") radix t)))
    `(:bin ,(cond ((not explicit-length) it)
		  ((> explicit-length (length it))
		   (concatenate 'string
				(make-string (- explicit-length (length it))
					     :initial-element (if (not signed-p)
								  (literal-char #\0)
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
									 (literal-char #\0)
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
    (|| (progn (|| #\d #\D) (descend-with-rule 'decimal-binary-string explicit-length))
	(progn (|| #\b #\B "UB" "uB" "Ub" "ub") (descend-with-rule '%convolved-binary-string 2 explicit-length nil))
	(progn (|| "SB" "sB" "Sb" "sb") (descend-with-rule '%convolved-binary-string 2 explicit-length t))
	(progn (|| #\o #\O "UO" "uO" "Uo" "uo") (descend-with-rule '%convolved-binary-string 8 explicit-length nil))
	(progn (|| "SO" "sO" "So" "so") (descend-with-rule '%convolved-binary-string 8 explicit-length t))
	(progn (|| #\x #\X "UX" "uX" "Ux" "ux") (descend-with-rule '%convolved-binary-string 16 explicit-length nil))
	(progn (|| "SX" "sX" "Sx" "sx") (descend-with-rule '%convolved-binary-string 16 explicit-length t)))))
	

(define-vhdl-rule binary-string ()
  (if (and *vhdl-version* (< *vhdl-version* 2008))
      simple-binary-string
      convolved-binary-string))
      

;;;; OK, let's write all the rules in EBNF, then figure out how to parse this condensed descrption
;;;; what semantics of all this is

;; We will change this to something meaningful later
(defmacro define-ebnf-rule (syntax &body body)
  (declare (ignore syntax body))
  nil)

(define-ebnf-aux-rule big-letter () (character-ranges (#\A #\Z)))
(define-ebnf-aux-rule small-letter () (character-ranges (#\a #\z)))
(define-ebnf-aux-rule digit () (character-ranges (#\0 #\9)))

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

(define-ebnf-aux-rule escape-chars ()
  (|| (progn #\( #\() (progn #\) #\)) (progn #\[ #\[) (progn #\] #\]) (progn #\{ #\{) (progn #\} #\})
      (progn #\| #\|) (progn #\. #\. #\. (text #\. #\. #\.))))
      

(define-ebnf-aux-rule new-in-version ()
  (let* ((version (? (postimes digit))))
    "_"
    (if version
	(parse-integer (text version))
	t)))
	 

;;; Design file

(define-ebnf-rule design-file "design-unit { ... }")

;; List of multiple strings is concatenated -- to make linebreaking more convenient
;; Things introduced in VHDL-2008 begin with underline
(define-ebnf-rule design-unit ("{ library-clause | use-clause | _context-reference }"
			       "library-unit"))

;; I should also be able to discern PSL_* rules
(define-ebnf-rule library-unit ("entity-declaration | architechture-body | package-declaration"
				"| package-body | package-installation-declaration | configuration-declaration"
				"| context-declaration | PSL-verification-unit"))

;; CAPS-names denote literal symbols, instead of going down the rules	  
(define-ebnf-rule library-clause "LIBRARY identifier {, ... } ;")

(define-ebnf-rule context-reference "CONTEXT selected-name {, ... };")


;;; Library unit declarations

;; Grammar uses the following delimiters: (), {}, [], and | and also triple-dot symbol ...
;; But sometimes actual syntax contains these symbols.
;; In these cases we escape them by ((, )), {{, }}, [[, ]], || and ......

;; Let's for now stick to convention that grouping brackets of grammar would be (( and ))

;; In general CAPSed names in the beginning denote italicized extra info
;; (I need to figure out how to actually use it)

(define-ebnf-rule entity-declaration ("ENTITY identifier IS"
				      "   [ GENERIC (( GENERIC-interface-list )) ; ]"
				      "   [ PORT (( PORT-interface-list )) ; ]"
				      "   { entity-declarative-item }"
				      "[ BEGIN"
				      "   { concurrent-assertion-statement"
				      "     | PASSIVE-concurrent-procedure-call-statement"
				      "     | PASSIVE-process-statement } ]"
				      "| _PSL-psl-directive" ; I don't know if this line is actually correct
				      "END [ ENTITY ] [ identifier ] ;"))

(define-ebnf-rule entity-declarative-item ("subprogram-declaration | subprogram-body"
					   "| subprogram-instantiation-declaration"
					   "| package-declaration | package-body"
					   "| package-instantiation-declaration"
					   "| type-declaration | subtype-declaration"
					   "| constant-declaration | signal-declaration"
					   "| SHARED-variable-declaration | file-declaration"
					   "| alias-declaration | attribute-declaration | attribute-specification"
					   "| disconnection-specification | use-clause"
					   "| group-template-declaration | group-declaration"
					   "| _PSL-property-declaration | _PSL-sequence-declaration"
					   "| _PSL-clock-declaration"))

(define-ebnf-rule architecture-body ("ARCHITECTURE identifier OF ENTITY-name IS"
				     "   { block-declarative-item }"
				     "BEGIN"
				     "   { concurrent-statement }"
				     "END [ ARCHITECTURE ] [ identifier ] ;"))

(define-ebnf-rule configuration-declaration ("CONFIGURATION identifier OF ENTITY-name IS"
					     "   { use-clause | attribute-specification | group-declaration }"
					     "   { USE VUNIT VERIFICATION-UNIT-name {, ... } ; }"
					     "   block-configuration"
					     "END [ CONFIGURATION ] [ identifier ] ;"))

(define-ebnf-rule block-configuration ("FOR ( ARCHITECTURE-name | BLOCK-STATEMENT-label"
				       "       | GENERATE-STATEMENT-label"
				       "         [ (( ( STATIC-discrete-range | STATIC-expression"
				       "                | _ALTERNATIVE-label ) )) ] ))"
				       "END FOR ;"))

(define-ebnf-rule component-configuration ("FOR component-specification"
					   "    [ binding-indication ; ]"
					   "    { USE VUNIT VERIFICATION-UNIT-name {, ... } ; }"
					   "    [ block-configuration ]"
					   "END FOR ;"))

(define-ebnf-rule context-declaration ("CONTEXT identifier IS"
				       "    { library-clause | use-clause | context-reference }"
				       "END [ CONTEXT ] [ identifier ] ;"))


;;; Declarations and Specifications

(define-ebnf-rule package-declaration ("PACKAGE identifier IS"
				       "    [ GENERIC (( GENERIC-interface-list )) ;"
				       "      [ GENERIC MAP (( GENERIC-association-list )) ; ] ]"
				       "    { package-declarative-item }"
				       "END [ PACKAGE ] [ identifier ] ;"))

(define-ebnf-rule package-declarative-item ("subprogram-declaration | subprogram-instantiation-declaration"
					    "| package-declaration | package-instantiation-declaration"
					    "| type-declaration | subtype-declaration"
					    "| constant-declaration | signal-declaration"
					    "| variable-declaration | file-declaration"
					    "| alias-declaration | component-declaration"
					    "| attribute-declaration | attribute-specification"
					    "| disconnection-specification | use-clause"
					    "| group-template-declaration | group-declaration"
					    "| _PSL-property-declaration | _PSL-sequence-declaration"))

(define-ebnf-rule package-body ("PACKAGE BODY identifier IS"
				"    { package-body-declarative-item }"
				"END [ PACKAGE BODY ] [ identifier ] ;"))

(define-ebnf-rule package-body-declarative-item ("subprogram-declaration | subprogram-body"
						 "| _subprogram-intantiation-declaration"
						 "| _package-declaration | _package-body"
						 "| _package-instantiation-declaration"
						 "| type-declaration | subtype-declaration"
						 "| constant-declaration | variable-declaration"
						 "| file-declaration | alias-declaration"
						 "| _attribute-declaration | _attribute-specification"
						 "| use-clause | group-template-declaration | group-declaration"))

(define-ebnf-rule _package-instantiation-declaration ("PACKAGE identifier IS NEW UNINSTANTIATED-PACKAGE-name"
						      "    [ GENERIC MAP (( GENERIC-association-list )) ] ;"))

(define-ebnf-rule subprogram-specification "procedure-specification | function-specification")

(define-ebnf-rule procedure-specification ("PROCEDURE identifier"
					   "    [ GENERIC (( GENERIC-interface-list ))"
					   "      [ GENERIC MAP (( GENERIC-association-list )) ] ]"
					   "    [ [ PARAMETER ] (( PARAMETER-interface-list )) ]"))

(define-ebnf-rule function-specification ("[ PURE | IMPURE ] FUNCTION ( identifier | operator-symbol )"
					  "    [ GENERIC (( GENERIC-interface-list ))"
					  "      [ GENERIC MAP (( GENERIC-association-list )) ] ]"
					  "    [ [ PARAMETER ] (( PARAMETER-interface-list )) ] RETURN type-mark"))

(define-ebnf-rule subprogram-declaration "subprogram-specification ;")

(define-ebnf-rule subprogram-body ("subprogram-specification IS"
				   "    { subprogram-declarative-item }"
				   "BEGIN { sequential-statement }"
				   "END [ PROCEDURE | FUNCTION ] [ identifier | operator-symbol ] ;"))

(define-ebnf-rule subprogram-declarative-item ("subprogram-declaration | subprogram-body"
					       "| _subprogram-instantiation-declaration"
					       "| _package-declaration | _package-body"
					       "| _package-instantiation-declaration"
					       "| type-declaration | subtype-declaration"
					       "| constant-declaration | variable-declaration"
					       "| file-declaration | alias-declaration"
					       "| attribute-declaration | attribute-specification"
					       "| use-clause | group-template-declaration | group-declaration"))

(define-ebnf-rule _subprogram-instantiation-declaration ("( PROCEDURE | FUNCTION ) identifier IS"
							 "    NEW UNINSTANTIATED-SUBPROGRAM-name [ signature ]"
							 "        [ GENERIC MAP (( GENERIC-association-list )) ] ;"))

(define-ebnf-rule type-declaration ("TYPE identifier IS type-definition ;"
				    "| TYPE identifier ; "))

(define-ebnf-rule type-definition ("enumeration-type-definition | integer-type-definition"
				   "| floating-type-definition | physical-type-definition"
				   "| array-type-definition | record-type-definition"
				   "| access-type-definition | file-type-definition"
				   "| protected-type-declaration | protected-type-body"))

(define-ebnf-rule constant-declaration "CONSTANT identifier {, ... } : subtype-indication [ := expression ] ;")

(define-ebnf-rule signal-declaration ("SIGNAL identifier {, ... } : subtype-indication [ REGISTER | BUS ]"
				      "                             [ := expression ] ;"))

(define-ebnf-rule variable-declaration ("[ SHARED ] VARIABLE identifier {, ... } : subtype-indication"
					"                                          [ := expression ] ;"))

(define-ebnf-rule file-declaration ("FILE identifier {, ... } : subtype-indication"
				    "    [ [ OPEN FILE-OPEN-KIND-expression ] IS STRING-expression ] ;"))

(define-ebnf-rule alias-declaration ("ALIAS ( identifier | character-literal | operator-symbol )"
				     "    [ : subtype-indication | IS name [ signature ] ;"))

(define-ebnf-rule component-declaration ("COMPONENT identifier [ IS ]"
					 "    [ GENERIC (( GENERIC-interface-list )) ; ]"
					 "    [ PORT (( PORT-interface-list )) ; ]"
					 "END COMPONENT [ identifier ] ;"))

(define-ebnf-rule attribute-declaration "ATTRIBUTE identifier | type-mark ;")

(define-ebnf-rule attribute-specification "ATTRIBUTE identifier OF entity-name-list | entity-class IS expression ;")

(define-ebnf-rule entity-name-list ("( ( identifier | character-literal | operator-symbol )[ signature ]){, ...}"
				    "| OTHERS | ALL"))

(define-ebnf-rule entity-class ("ENTITY | ARCHITECTURE | CONFIGURATION | PACKAGE | PROCEDURE | FUNCTION"
				"| TYPE | SUBTYPE | CONSTANT | SIGNAL | VARIABLE | FILE | COMPONENT | LABEL"
				"| LITERAL | UNITS | GROUP | _PROPERTY | _SEQUENCE"))

(define-ebnf-rule configuration-specification ("FOR component-specification binding-indication ;"
					       "    { USE VUNIT VERIFICATION-UNIT-name {, ... } ; }"
					       "[ END FOR ; ]"))

(define-ebnf-rule component-specification "( INSTANTIATION-label {, ...} | OTHERS | ALL ) : COMPONENT-name")

(define-ebnf-rule binding-indication ("USE ( ENTITY ENTITY-name [ (( ARCHITECTURE-identifier )) ]"
				      "       | CONFIGURATION CONFIGURATION-name | OPEN )"
				      "[ GENERIC MAP (( GENERIC-association-list )) ]"
				      "[ PORT MAP (( PORT-association-list )) ]"))

(define-ebnf-rule disconnection-specification ("DISCONNECT ( SIGNAL-name {, ...} | OTHERS | ALL ) : type-mark"
					       "    AFTER TIME-expression ;"))

(define-ebnf-rule group-template-declaration "GROUP identifier IS (( ( entity-class [ <> ] ) {, ...} )) ;")

(define-ebnf-rule group-declaration
  "GROUP identifier : GROUP-TEMPLATE-name (( ( name | character-literal ) {, ...} )) ;")

(define-ebnf-rule use-clause "USE selected-name {, ...} ;")


;;; Type Declarations

(define-ebnf-rule enumeration-type-definition "(( ( identifier | character-literal ) {, ...} ))")

(define-ebnf-rule integer-type-definition ("RANGE ( RANGE-attribute-name"
					   "        | simple-expression ( TO | DOWNTO ) simple-expression )"))

(define-ebnf-rule floating-type-definition ("RANGE ( RANGE-attribute-name"
					    "        | simple-expression ( TO | DOWNTO ) simple-expression )"))

(define-ebnf-rule physical-type-definition ("RANGE ( RANGE-attribute-name"
					    "        | simple-expression ( TO | DOWNTO ) simple-expression )"
					    "   UNITS identifier { identifier = physical-literal ; }"
					    "   END UNITS [ identifier ]"))

(define-ebnf-rule array-type-definition ("ARRAY (( ( type-mark RANGE <> ) {, ...} )) OF ELEMENT-subtype-indication"
					 "| ARRAY (( discrete-range {, ...} )) OF ELEMENT-subtype-indication"))

(define-ebnf-rule record-type-definition ("RECORD ( identifier {, ...} : subtype-indication ; ) { ... }"
					  "END RECORD [ identifier ]"))

(define-ebnf-rule access-type-definition "ACCESS subtype-indication")

(define-ebnf-rule file-type-definition "FILE OF type-mark")

(define-ebnf-rule protected-type-declaration ("PROTECTED { protected-type-declarative-item }"
					      "END PROTECTED [ identifier ]"))

(define-ebnf-rule protected-type-declarative-item ("subprogram-declaration | _subprogram-instantiation-declaration"
						   "| attribute-specification | use-clause"))

(define-ebnf-rule protected-type-body ("PROTECTED BODY { protected-type-body-declarative-item }"
				       "END PROTECTED BODY [ identifier ]"))

(define-ebnf-rule protected-type-body-declarative-item ("subprogram-declaration | subprogram-body"
							"| _subprogram-instantiation-declaration"
							"| _package-declaration | _package-body"
							"| _package-instantiation-declaration"
							"| type-declaration | subtype-declaration"
							"| constant-declaration | variable-declaration"
							"| file-declaration | alias-declaration"
							"| attribute-declaration | attribute-specification"
							"| use-clause"
							"| group-template-declaration | group-declaration"))

(define-ebnf-rule subtype-declaration "SUBTYPE identifier IS subtype-indication ;")

(define-ebnf-rule subtype-indication "[ resolution-indication ] type-mark [ constant ]")

;; This _( is interesting -- I wonder, how I would parse it
(define-ebnf-rule resolution-indication ("RESOLUTION-FUNCTION-name | _(( _resolution-indication"
					 "| ( _RECORD-ELEMENT-identifier resolution-indication ) {, ...} ))"))

(define-ebnf-rule constraint
  ("RANGE ( RANGE-attribute-name | simple-expression ( TO | DOWNTO ) simple-expression )"
   "| array-constraint | _record-constraint"))

(define-ebnf-rule array-constraint ("(( discrete-range {, ... } )) [ array-constraint | record-constraint ]"
				    "| (( OPEN )) [ array-constraint | record-constraint ]"))

(define-ebnf-rule record-constraint
  "(( ( RECORD-ELEMENT-identifier ( array-constraint | record-constrant ) ) {, ... } ))")

(define-ebnf-rule discrete-range
  "DISCRETE-subtype-indication | RANGE-attribute-name | simple-expression ( TO | DOWNTO ) simple-expression")

(define-ebnf-rule type-mark "TYPE-name | SUBTYPE-name")


;;; Concurrent Statements

(define-ebnf-rule concurrent-statement
  ("block-statement | process-statement | concurrent-procedure-call-statement | concurrent-assertion-statement"
   "| concurrent-signal-assignment-statement | component-instantiation-statement | generate-statement"
   "| PSL-psl-directive"))

(define-ebnf-rule block-statement ("BLOCK-label : BLOCK [ (( GUARD-expression )) ] [ IS ]"
				   "    [ GENERIC (( GENERIC-interface-list )) ;"
				   "    [ GENERIC MAP (( GENERIC-association-list )) ; ] ]"
				   "    [ PORT (( PORT-interface-list )) ;"
				   "      [ PORT MAP (( PORT-association-list )) ; ] ]"
				   "    { block-declarative-item }"
				   "BEGIN { concurrent-statement } END BLOCK [ BLOCK-label ] ;"))

(define-ebnf-rule block-declarative-item ("subprogram-declaration | subprogram-body"
					  "| _subprogram-instantiation-declaration"
					  "| _package-declaration | _package-body"
					  "| _package-instantiation-declaration"
					  "| type-declaration | subtype-declaration | constant-declaration"
					  "| signal-declaration | SHARED-variable-declaration | file-declaration"
					  "| alias-declaration | component-declaration | attribute-declaration"
					  "| attribute-specification | configuration-specification"
					  "| disconnection-specification | use-clause | group-template-declaration"
					  "| group-declaration | _PSL-property-declaration"
					  "| _PSL-sequence-declaration | _PSL-clock-declaration"))

(define-ebnf-rule process-statement
  ("[ PROCESS-label : ] [ POSTPONED ] PROCESS [ ( (( SIGNAL-name {, ...} )) | ALL ) ] [ IS ]"
   "    { process-declarative-item } BEGIN { sequential-statement } END [ POSTPONED ] PROCESS [ PROCESS-label ] ;"))

(define-ebnf-rule process-declarative-item
  ("subprogram-declarative | subprogram-body | _subprogram-instantiation-declaration"
   "| _package-declaration | _package-body | _package-instantiation-declaration"
   "| type-declaration | subtype-declaration | constant-declaration | variable-declaration"
   "| file-declaration | alias-declaration | attribute-declaration | attribute-specification | use-clause"
   "| group-template-declaration | group-declaration"))

(define-ebnf-rule concurrent-procedure-call-statement
  "[ label : ] [ POSTPONED ] PROCEDURE-name [ (( PARAMETER-association-list )) ] ;")

(define-ebnf-rule concurrent-assertion-statement
  "[ label : ] [ POSTPONED ] ASSERT condition [ REPORT expression ] [ SEVERITY expression ] ;")

(define-ebnf-rule concurrent-signal-assignment-statement
  "[ label : ] [ POSTPONED ] concurrent-simple-signal-assignment"
  "| [ label : ] [ POSTPONED ] concurrent-conditional-signal-assignment"
  "| [ label : ] [ POSTPONED ] concurrent-selected-signal-assignment")

(define-ebnf-rule concurrent-simple-signal-assignment "[ GUARDED ] [ delay-mechanism ] waveform ;")

(define-ebnf-rule concurrent-conditional-signal-assignment
  "[ GUARDED ] [ delay-mechanism ] waveform WHEN condition { ELSE waveform WHEN condition } [ ELSE waveform ] ;")

(define-ebnf-rule concurrent-selected-signal-assignment
  ("WITH expression SELECT _[ ? ] target <= [ GUARDED ] [ delay-mechanism ]"
   "    { waveform WHEN choices , } waveform WHEN choices ;"))

(define-ebnf-rule component-instantiation-statement
  ("INSTANTIATION-label : ( [ COMPONENT ] COMPONENT-name | ENTITY ENTITY-name [ (( ARCHITECTURE-identifier )) ]"
   "    | CONFIGURATION CONFIGURATION-name )"
   "    [ GENERIC MAP (( GENERIC-association-list )) ] [ PORT MAP (( PORT-association-list )) ] ;"))

(define-ebnf-rule generate-statement ("for-generate-statement | if-generate-statement | _case-generate-statement"))

(define-ebnf-rule for-generate-statement
  ("GENERATE-label : FOR identifier IN discrete-range GENERATE generate-statement-body"
   "END GENERATE [ GENERATE-label ] ;"))

(define-ebnf-rule if-generate-statement
  ("GENERATE-label : IF [ ALTERNATIVE-label : ] condition GENERATE generate-statement-body"
   "{ ELSIF [ ALTERNATIVE-label : ] condition GENERATE generate-statement-body }"
   "[ ELSE [ ALTERNATIVE-label : ] GENERATE generate-statement-body ]"
   "END GENERATE [ GENERATE-label ] ;"))

(define-ebnf-rule _case-generate-statement
  ("GENERATE-label : CASE expression GENERATE"
   "  ( WHEN [ ALTERNATIVE-label : ] choices => generate-statement-body ) { ... }"
   "END GENERATE [ GENERATE-label ] ;"))

(define-ebnf-rule generate-statement-body
  ("[ { block-declarative-item } BEGIN ] { concurrent-statement } [ END [ ALTERNATIVE-label ] ; ]"))


;;; Sequential Statements

(define-ebnf-rule sequential-statement
  ("wait-statement | assertion-statement | report-statement | signal-assignment-statement"
   "| variable-assignment-statement | procedure-call-statement | if-statement | case-statement"
   "| loop-statement | next-statement | exit-statement | return-statement | null-statement"))

(define-ebnf-rule wait-statement
  "[ label : ] WAIT [ ON SIGNAL-name {, ...} ] [ UNTIL condition ] [ FOR TIME-expression ] ;")

(define-ebnf-rule assertion-statement
  "[ label : ] ASSERT condition [ REPORT expression ] [ SEVERITY expression ] ;")

(define-ebnf-rule report-statement "[ label : ] REPORT expression [ SEVERITY expression ] ;")

(define-ebnf-rule signal-assignment-statement
  ("[ label : ] simple-signal-assignment | [ label : ] conditional-signal-assignment"
   "| [ label : ] selected-signal-assignment"))

(define-ebnf-rule simple-signal-assignment
  ("( name | aggregate ) <= [ delay-mechanism ] waveform ;"
   "| name <= FORCE [ IN | OUT ] expression ; | name <= RELEASE [ IN | OUT ] ;"))

(define-ebnf-rule conditional-signal-assignment "conditional-waveform-assignment | conditional-force-assignment")

(define-ebnf-rule conditional-waveform-assignement
  ("[ label : ] ( name | aggregate ) <= [ delay-mechanism ] waveform WHEN condition"
   "{ ELSE waveform WHEN condition } [ ELSE waveform ] ;"))

(define-ebnf-rule conditional-force-assignement
  ("[ label : ] name <= FORCE [ IN | OUT ] expression WHEN condition { ELSE expression WHEN condition }"
   "[ ELSE expression ] ;"))

(define-ebnf-rule selected-signal-assignment "selected-waveform-assignment | selected-force-assignment")

(define-ebnf-rule selected-waveform-assignment
  ("[ label : ] WITH expression SELECT [ ? ] ( name | aggregate ) <= [ delay-mechanism ]"
   "{ waveform WHEN choices , } waveform WHEN choices ;"))

(define-ebnf-rule selected-force-assignment
  ("[ label : ] WITH expression SELECT [ ? ] name <= FORCE [ IN | OUT ]"
   "{ expression WHEN choices , } expression WHEN choices ;"))

(define-ebnf-rule delay-mechanism "TRANSPORT | [ REJECT TIME-expression ] INERTIAL")

(define-ebnf-rule waveform
  "( VALUE-expression [ AFTER TIME-expression ] | NULL [ AFTER TIME-expression ] ) {, ...} | UNAFFECTED")

(define-ebnf-rule variable-assignment-statement
  ("[ label : ] simple-variable-assignment _| [ label : ] conditional-variable-assignment"
   "_| [ label : ] selected-variable-assignment"))

(define-ebnf-rule simple-variable-assignment "( name | aggregate ) := expression ;")

(define-ebnf-rule _conditional-variable-assignment
  "( name | aggregate ) := expression WHEN condition { ELSE expression WHEN condition } [ ELSE expression ] ;")

(define-ebnf-rule _selected-variable-assignment
  ("WITH expression SELECT [ ? ] ( name | aggregate ) :="
   "{ expression WHEN choices , } expression WHEN choices ;"))

(define-ebnf-rule procedure-call-statement "[ label : ] PROCEDURE-name [ (( PARAMETER-association-list )) ] ;")

(define-ebnf-rule if-statement
  ("[ IF-label : ] IF condition THEN { sequential-statement } { ELSIF condition THEN { sequential-statement } }"
   "[ ELSE { sequential-statement } ] END IF [ IF-label ] ;"))

(define-ebnf-rule case-statement
  ("[ CASE-label : ] CASE _[ ? ] expression IS ( WHEN choices => { sequential-statement } ) { ... }"
   "END CASE _[ ? ] [ CASE-label ] ;"))

(define-ebnf-rule loop-statement
  ("[ LOOP-label : ] [ WHILE condition | FOR identifier IN discrete-range ] LOOP { sequential-statement }"
   "END LOOP [ LOOP-label ] ;"))

(define-ebnf-rule next-statement "[ label : ] NEXT [ LOOP-label ] [ WHEN condition ] ;")

(define-ebnf-rule exit-statement "[ label : ] EXIT [ LOOP-label ] [ WHEN condition ] ;")

(define-ebnf-rule return-statement "[ label : ] RETURN [ expression ] ;")

(define-ebnf-rule null-statement "[ label : ] NULL ;")


;;; Interfaces and Associations

(define-ebnf-rule interface-list
  ("( interface-constant-declaration | interface-signal-declaration | interface-variable-declaration"
   "   | interface-file-declaration | _interface-type-declaration | _interface-subprogram-declaration"
   "   | _interface-package-declaration ) { ; ... }"))

(define-ebnf-rule interface-constant-declaration
  ("[ CONSTANT ] identifier {, ... } : [ IN ] subtype-indication [ := STATIC-expression ]"))

(define-ebnf-rule interface-signal-declaration
  ("[ SIGNAL ] identifier {, ... } : [ mode ] subtype-indication [ BUS ] [ := STATIC-expression ]"))

(define-ebnf-rule interface-variable-declaration
  ("[ VARIABLE ] identifier {, ... } : [ mode ] subtype-indication [ := STATIC-expression ]"))

(define-ebnf-rule mode "IN | OUT | INOUT | BUFFER | LINKAGE")

(define-ebnf-rule interface-file-declaration "FILE identifier {, ... } : subtype-indication")

(define-ebnf-rule _interface-type-declaration "TYPE identifier")

(define-ebnf-rule _interface-subprogram-declaration
  ("( PROCEDURE identifier [ [ PARAMETER ] ( PARAMETER_interface_list ) ]"
   "   | [ PURE | IMPURE ] FUNCTION (( identifier | operator-symbol ))"
   "                       [ [ PARAMETER ] ( PARAMETER-interface-list ) ] RETURN type-mark )"
   "[ IS ( SUBPROGRAM-name | <> ) ]"))

(define-ebnf-rule _interface-package-declaration
  ("PACKAGE identifier IS NEW UNINSTANTIATED-PACKAGE-name"
   "GENERIC MAP (( ( GENERIC-association-list | <> | DEFAULT ) ))"))

(define-ebnf-rule association-list "( [ format-part => ] actual-part ) {, ... }")

(define-ebnf-rule formal-part
  ("GENERIC-name | PORT-name | PARAMETER-name | FUNCTION-name (( ( GENERIC-name | PORT-name | PARAMETER-name ) ))"
   "| type-mark (( ( GENERIC-name | PORT-name | PARAMETER-name ) ))"))

(define-ebnf-rule actual-part
  ("_[ INERTIAL ] expression | SIGNAL-name | VARIABLE-name | FILE-name | _subtype-indication"
   "| _SUBPROGRAM-name | _PACKAGE-name | OPEN | FUNCTION-name (( ( SIGNAL-name | VARIABLE-name ) ))"
   "| type-mark (( ( SIGNAL-name | VARIABLE-name ) ))"))


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

(define-ebnf-rule function-call "FUNCTION-name [ (( PARAMETER-association-list )) ]")

(define-ebnf-rule qualified-expression "type-mark ' (( expression )) | type-mark ' aggregate")

(define-ebnf-rule name
  ("identifier | operator-symbol | character-literal | selected-name"
   "| ( name | function-call ) (( expression {, ...} ))"
   "| ( name | function-call ) (( discrete-range ))"
   "| attribute-name | _external-name"))

(define-ebnf-rule selected-name
  "( name | function-call ) . ( identifier | character-literal | operator-symbol | ALL )")

(define-ebnf-rule operator-symbol "{ graphic-character }")

(define-ebnf-rule attribute-name
  "( name | function-call ) [ signature ] ' identifier [ (( expression )) ]")

;; Apparently, [[ and ]] should denote [ and ] in text
(define-ebnf-rule signature
  "[[ [ type-mark {, ... } ] [ RETURN type-mark ] ]]")

(define-ebnf-rule _external-name
  ("<< CONSTANT external-pathname : subtype-indication >>"
   "| << SIGNAL external-pathname : subtype-indication >>"
   "| << VARIABLE external-pathname : subtype-indication >>"))

(define-ebnf-rule _external-pathname
  "absolute-pathname | relative-pathname | package-pathname")

(define-ebnf-rule _absolute-pathname ". { pathname-element . } OBJECT-identifier")

(define-ebnf-rule _relative-pathname "{ ^ . } { pathname-element . } OBJECT-identifier")

(define-ebnf-rule pathname-element
  ("ENTITY-identifier | COMPONENT-INSTANTIATION-label | BLOCK-label"
   "| GENERATE-STATEMENT-label [ (( STATIC-expression )) ] | PACKAGE-identifier"))

(define-ebnf-rule _package-pathname "@ LIBRARY-identifier . { PACKAGE-identifier . } OBJECT-identifier")

(define-ebnf-rule literal
  ("decimal-literal | based-literal | physical-literal | identifier"
   "| character-literal | string-literal | bit-string-literal | NULL"))

(define-ebnf-rule aggregate "(( ( [ choices -> ] expression ) {, ... } ))")

(define-ebnf-rule choices "( simple-expression | discrete-range | identifier | OTHERS ) { || ... }")

(define-ebnf-rule label "identifier")

(define-ebnf-rule _tool-directive "` identifier { graphic-character }")


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


