
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

;; Let's for now stick to convention that grouping brackets of grammar would be (( and ))

;; In general CAPSed names in the beginning denote italicized extra info
;; (I need to figure out how to actually use it)

(define-ebnf-rule entity-declaration ("ENTITY identifier IS"
				      "   [ GENERIC ( GENERIC-interface-list ) ; ]"
				      "   [ PORT ( PORT-interface-list ) ; ]"
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

(define-ebnf-rule block-configuration ("FOR (( ARCHITECTURE-name | BLOCK-STATEMENT-label"
				       "       | GENERATE-STATEMENT-label"
				       "         [ ( (( STATIC-discrete-range | STATIC-expression"
				       "                | _ALTERNATIVE-label )) ) ] ))"
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
				       "    [ GENERIC ( GENERIC-interface-list ) ;"
				       "      [ GENERIC MAP ( GENERIC-association-list ) ; ] ]"
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
						      "    [ GENERIC MAP ( GENERIC-association-list ) ] ;"))

(define-ebnf-rule subprogram-specification "procedure-specification | function-specification")

(define-ebnf-rule procedure-specification ("PROCEDURE identifier"
					   "    [ GENERIC ( GENERIC-interface-list )"
					   "      [ GENERIC MAP ( GENERIC-association-list ) ] ]"
					   "    [ [ PARAMETER ] ( PARAMETER-interface-list ) ]"))

(define-ebnf-rule function-specification ("[ PURE | IMPURE ] FUNCTION (( identifier | operator-symbol ))"
					  "    [ GENERIC ( GENERIC-interface-list )"
					  "      [ GENERIC MAP ( GENERIC-association-list ) ] ]"
					  "    [ [ PARAMETER ] ( PARAMETER-interface-list ) ] RETURN type-mark"))

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

(define-ebnf-rule _subprogram-instantiation-declaration ("(( PROCEDURE | FUNCTION )) identifier IS"
							 "    NEW UNINSTANTIATED-SUBPROGRAM-name [ signature ]"
							 "        [ GENERIC MAP ( GENERIC-association-list ) ] ;"))

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

(define-ebnf-rule alias-declaration ("ALIAS (( identifier | character-literal | operator-symbol ))"
				     "    [ : subtype-indication | IS name [ signature ] ;"))

(define-ebnf-rule component-declaration ("COMPONENT identifier [ IS ]"
					 "    [ GENERIC ( GENERIC-interface-list ) ; ]"
					 "    [ PORT ( PORT-interface-list ) ; ]"
					 "END COMPONENT [ identifier ] ;"))

(define-ebnf-rule attribute-declaration "ATTRIBUTE identifier | type-mark ;")

(define-ebnf-rule attribute-specification "ATTRIBUTE identifier OF entity-name-list | entity-class IS expression ;")

(define-ebnf-rule entity-name-list ("(( (( identifier | character-literal | operator-symbol ))[ signature ])){, ...}"
				    "| OTHERS | ALL"))

(define-ebnf-rule entity-class ("ENTITY | ARCHITECTURE | CONFIGURATION | PACKAGE | PROCEDURE | FUNCTION"
				"| TYPE | SUBTYPE | CONSTANT | SIGNAL | VARIABLE | FILE | COMPONENT | LABEL"
				"| LITERAL | UNITS | GROUP | _PROPERTY | _SEQUENCE"))

(define-ebnf-rule configuration-specification ("FOR component-specification binding-indication ;"
					       "    { USE VUNIT VERIFICATION-UNIT-name {, ... } ; }"
					       "[ END FOR ; ]"))

(define-ebnf-rule component-specification "(( INSTANTIATION-label {, ...} | OTHERS | ALL )) : COMPONENT-name")

(define-ebnf-rule binding-indication ("USE (( ENTITY ENTITY-name [ ( ARCHITECTURE-identifier ) ]"
				      "       | CONFIGURATION CONFIGURATION-name | OPEN ))"
				      "[ GENERIC MAP ( GENERIC-association-list ) ]"
				      "[ PORT MAP ( PORT-association-list ) ]"))

(define-ebnf-rule disconnection-specification ("DISCONNECT (( SIGNAL-name {, ...} | OTHERS | ALL )) : type-mark"
					       "    AFTER TIME-expression ;"))

(define-ebnf-rule group-template-declaration "GROUP identifier IS ( (( entity-class [ <> ] )) {, ...} ) ;")

(define-ebnf-rule group-declaration
  "GROUP identifier : GROUP-TEMPLATE-name ( (( name | character-literal )) {, ...} ) ;")

(define-ebnf-rule use-clause "USE selected-name {, ...} ;")


;;; Type Declarations
