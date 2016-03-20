
(in-package #:cl-vhdl)

(enable-read-macro-tokens)

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
      
      
