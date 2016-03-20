
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
	  
			      
