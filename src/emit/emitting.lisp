
(in-package #:cl-vhdl)

(cl-interpol:enable-interpol-syntax)

(defmacro try-opt-emit (thing emit-rule)
  "Emit with leading space if the thing is non-NIL"
  `(if ,thing #?" $((try-emit ,thing ,emit-rule))" ""))

(defmacro try-opt-kwd-emit (thing emit-rule)
  "Like TRY-OPT-EMIT, but for arguments with a keyword"
  `(if ,thing #?" $((try-emit (car ,thing) symbol-literal)) $((try-emit (cadr ,thing) ,emit-rule))" ""))

;; OK, let's try to emit *some* VHDL from s-exp and see how far will it go...
;; The pattern for if (cond) would look something like

;; The second component is destructuring of initial list

;; Let's try something more complicated than if

;; (with-smart-destructuring (:if (cond (cdr then)) (collect-while ((not 't) (cdr _))) (maybe ('t (cdr else)))) expr
;; 			  ...)

;; " -- first line just to make indents look similar
;; {$label : }if $cond then [if $label] -- curly brackets
;;   $then
;; elsif $a then [for (a . b) in elsifs -- control statements in brackets afterwards
;;   $b            ]
;; else          [if else
;;   $else         ]
;; end if{ $label}; [if label]
;; "

;; (with-smart-destructuring (:<= name (cdr rest)) expr
;;  (smart-match ((:release (cap dir (maybe (or :in :out)))) "$name <= release{ $dir}; [if $dir]")
;; 	      ((:force (cap dir (maybe (or :in :out))) sub-expr) "$name <= force{ $dir} $sub-expr; [if $dir]")
;; 	      ((len 2 (delay waveform)) "$name <= $delay $waveform;")
;; 	      ((waveform) "$name <= $waveform;")))

;; (with-smart-destructuring (:generate-if label
;; 					(then-alt-label cond (cdr then-expr))
;; 					(collect-until (_ 't (cdr _)))
;; 					(maybe (else-alt-label 't (cdr else-expr))))
;;   ...)

	 
;; Looks like I understand now how this destructuring should work
;; -- all the "variables" inside body should be grepped beforehand, and be "unbound" outside
;; -- we should not mix in dots (or any reader syntax) -- instead relying on "special forms" like CDR

;; OK, now we need to make emitting language equally flexible

;; (defun foo ()
;;   (let ((x 1))
;;     (+ (let ((x 3))
;; 	 (makunbound 'x)
;; 	 x)
;;        x)))

  
  
;; Let's first write the dumbest emitting ever -- the one that doesn't make indents
;; and just breaks lines at fixed length. This will already allow to interface with
;; existing VHDL tools (simulators, analyzers and synthetizers) and switch to a more
;; interesting task of writing higher-levels of lisp-VHDL.

(defgeneric elt-emit (x))

(defmethod elt-emit ((x string))
  x)

(defmethod elt-emit ((x symbol))
  (cl-ppcre:regex-replace-all "-" (string-downcase x) "_"))

(def-launched-iter fixwid-token-emitter (stream &optional (width 80))
  (let ((cur-width 0))
    (iter (while t)
	  (when (> cur-width width)
	    (format stream "~%")
	    (setf cur-width 0))
	  (let ((str (elt-emit (yield :ok))))
	    (format stream " ~a" str)
	    (incf cur-width (1+ (length str)))))))

;; Let's start from the beginning and serialize some elementary things

(define-condition emit-error (error) ())
(defun fail-emit ()
  (error 'emit-error))

(defparameter *emit-rules* (make-hash-table :test #'eq))

(defmacro def-emit-rule (name pattern &body body)
  `(setf (gethash ',name *emit-rules*)
	 (named-lambda ,name (whole) ; yes, we intentionally leak this variable into the BODY
	   (let ((it (handler-case (with-match ,pattern whole
				     ,@body)
		       (fail-match () (fail-emit)))))
	     (if (not (stringp it))
		 (error "We should emit strings, but rule ~a returned something else : ~a" ',name it)
		 it)))))

(defmacro try-emit (thing &rest alternatives)
  (once-only (thing)
    (with-gensyms (g!-outer)
      `(block ,g!-outer
	 ,@(mapcar (lambda (x)
		     (with-gensyms (g!-fun)
		       `(handler-case (let ((,g!-fun (gethash ',x *emit-rules*)))
					(if (not ,g!-fun)
					    (error "Emission function: ~a is not defined" ',x)
					    (funcall ,g!-fun ,thing)))
			  (emit-error () nil)
			  (:no-error (x) (return-from ,g!-outer x)))))
		   alternatives)
	 (fail-emit)))))

(defmacro ecase-emit (thing &rest clauses)
  "If clause succeeds, IT is bound inside clause to result of emission."
  (once-only (thing)
    (with-gensyms (g!-outer)
      `(block ,g!-outer
	 ,@(mapcar (lambda (x)
		     `(handler-case (funcall (gethash ',(car x) *emit-rules*) ,thing)
			(emit-error () nil)
			(:no-error (it) (return-from ,g!-outer (progn ,@(cdr x))))))
		   clauses)
	 (fail-emit)))))

(def-emit-rule character-literal x_characterp #?"'$(x)'")

(def-emit-rule bit-string-literal (:bin it_stringp) #?"B\"$(it)\"")

(def-emit-rule number-literal x_numberp (format nil "~s" x))

(def-emit-rule symbol-literal sym_symbolp
  (cl-ppcre:regex-replace-all "-" (string-downcase sym) "_"))

(def-emit-rule physical-literal (unit_symbolp n_numberp)
  ;; TODO : somehow remove duplication of guards
  #?"$((try-emit n number-literal)) $((try-emit unit symbol-literal))")

(def-emit-rule string-literal x_stringp
  (format nil "\"~{~a~^\"\"~}\"" (cl-ppcre:split "\"" x)))

(def-emit-rule identifier x (try-emit x symbol-literal))

(def-emit-rule literal x
  (try-emit x identifier character-literal number-literal string-literal physical-literal bit-string-literal))

(def-emit-rule label x
  (try-emit x identifier))


;; I guess, there's no other way than first learn to emit most elementary constructs (done),
;; then more and more compound ones, building to general expressions
;; only then I can address control flow constructs -- otherwise I lose the power of interactive testing.
;; So, let's slowly move along this path...

;;; primary expressions
(def-emit-rule primary x
  (try-emit x
	    literal qualified-expression
	    name aggregate function-call
	    new-primary typemark-primary
	    parenthesized-expression
	    ))

(def-emit-rule new-primary (:new x)
  #?"new $((try-emit x subtype-indication qualified-expression))")

(defparameter *expression* nil)

(def-emit-rule parenthesized-expression x
  ;; Do not allow infinite recursion
  (if (eq *expression* x)
      (fail-emit)
      ;; if we are at this stage of emitting expression *necessarily* must be bracketed
      #?"($((try-emit x expression)))"))

(def-emit-rule typemark-primary x
  ;; TODO : actually implement this -- which I don't know how to do now
  (fail-emit))

(def-emit-rule unary-op ((cap op (or :abs :not :and :nand :or :nor :xor :xnor)) x)
  #?"$((try-emit op symbol-literal)) $((try-emit x primary))")

(def-emit-rule exponentiation (:** x y)
  #?"$((try-emit x primary)) ** $((try-emit y primary))")

(def-emit-rule factor x
  (try-emit x exponentiation unary-op primary))

(def-emit-rule term x
  (try-emit x real-term factor))

(def-emit-rule real-term ((cap op (or :* :/ :mod :rem)) (cdr lst))
  (format nil #?"~{~a~^ $((try-emit op symbol-literal)) ~}"
	  (mapcar (lambda (x)
		    (try-emit x factor))
		  lst)))

(def-emit-rule qualified-expression (:qualified thing type)
  (let ((it (ecase-emit thing (aggregate it) (expression #?"($(it))"))))
    #?"$((try-emit type type))'$(it)"))

(def-emit-rule simple-expression x
  (try-emit x real-simple-expression term))

(def-emit-rule real-simple-expression ((cap op (or :+ :- :&)) (cdr lst))
  ;; (format t "I'm there1 ~a~%" lst)
  (if (equal 1 (length lst))
      #?"$((try-emit op symbol-literal)) $((try-emit (car lst) term))"
      (ecase-match (car lst)
		   (((cap sub-op (or :+ :-)) x)
		    ;; (format t "I'm here~%")
		    (format nil #?"$((try-emit sub-op symbol-literal)) ~{~a~^ $((try-emit op symbol-literal)) ~}"
			    (mapcar (lambda (x)
				      (try-emit x term))
				    (cons x (cdr lst)))))
		   (_
		    ;; (format t "I'm there ~a~%" lst)
		    (format nil #?"~{~a~^ $((try-emit op symbol-literal)) ~}"
			    (mapcar (lambda (x)
				      (try-emit x term))
				    lst))))))

(defun emit-aggregate-elt (thing)
  (ecase-match thing ((:=> choices expr) #?"$((try-emit choices choices)) => $((try-emit expr expression))")
	       (expr (try-emit expr expression))))

(def-emit-rule aggregate (:aggregate (cdr things))
  (format nil "~{~a~^, ~}" (mapcar #'emit-aggregate-elt things)))

(defun emit-elt-choice (x)
  ;; TODO : keywords are special rules that match this precise keyword
  (try-emit x discrete-range identifier simple-expression :others))
	    

(def-emit-rule choices x
  (ecase-match x
	       ((:|| (cdr alts)) (format nil "~{~a~^|| ~}" (mapcar #'emit-elt-choice alts)))
	       (x (emit-elt-choice x))))

(def-emit-rule discrete-range x
  (try-emit x simple-discrete-range attribute-range subtype-range))

(def-emit-rule simple-discrete-range ((cap dir (or :to :downto)) x y)
  #?"$((try-emit x simple-expression)) $((try-emit dir symbol-literal)) $((try-emit y simple-expression))")

(def-emit-rule attribute-range x_attribute-name-p
  (try-emit x name))

(def-emit-rule attribute-name x_attribute-name-p
  (try-emit x name))

(def-emit-rule subtype-range x
  (try-emit x subtype-indication))

(defun sym-not-kwd-p (x)
  (and (symbolp x)
       (not (keywordp x))))

(defun name-p (x)
  (or (sym-not-kwd-p x) ; identifier
      (characterp x) ; character literal
      (stringp x) ; operator-symbol
      (match-p (:compound (cdr _)) x) ; shallow testing of the correct compound name structure
      ))
	       
(def-emit-rule type-mark x_name-p
  (try-emit x name))

(def-emit-rule subtype-indication x
  (try-emit x type-mark compound-subtype-indication))

(def-emit-rule compound-subtype-indication (type-mark_name-p  (cap res (maybe (:resolution _)))
							      (cap con (maybe (:constraint _))))
  (when (and (not res) (not con))
    (fail-emit))
  (format nil "~a ~a ~a"
	  (if res (try-emit res resolution-indication) "")
	  (try-emit type-mark type-mark)
	  (if con (try-emit res constraint) "")))

(def-emit-rule resolution-indication x
  (try-emit x name sub-array-resolution sub-record-resolution))

(def-emit-rule sub-array-resolution (:sub x)
  #?"($((try-emit x resolution-indication)))")

(def-emit-rule sub-record-resolution (:fields (cdr lst))
  (format nil "(~{~a~^, ~})" (mapcar (lambda (x)
				       (with-match (1st 2nd) x
					 #?"$((try-emit 1st identifier)) $((try-emit 2nd resolution-indication))"))
				     lst)))

(def-emit-rule constraint x
  (try-emit x range-definition array-constraint record-constraint))

;; TODO : do record-constraint and array-constraint

(def-emit-rule range-definition x
  (ecase-match x (((cap dir (or :to :downto)) x y)
		  (format t "I'm here~%")
		  (format nil "range (~a ~a ~a)"
			  (try-emit x simple-expression)
			  (try-emit dir symbol-literal)
			  (try-emit y simple-expression)))
	       ((:range x_attribute-name-p) #?"range $((try-emit x name))")))

(def-emit-rule name x
  (try-emit x compound-name atomic-name external-name))

(def-emit-rule operator-symbol x_stringp
  (try-emit x string-literal))

(def-emit-rule atomic-name x
  (try-emit x identifier operator-symbol character-literal))

(def-emit-rule external-name ((cap op (or :constant :signal :variable)) subtype pathname)
  (format nil "<< ~a ~a : ~a >>"
	  (try-emit op symbol-literal)
	  (try-emit pathname external-pathname)
	  (try-emit subtype subtype-indication)))

(def-emit-rule external-pathname x
  (try-emit x absolute-pathname relative-pathname package-pathname))

(def-emit-rule absolute-pathname (:abs-path (cdr path))
  (format nil ". ~{~a~^ . ~} . ~a"
	  (mapcar (lambda (x)
		    (try-emit x pathname-element))
		  (butlast path))
	  (try-emit (car (last path)) identifier)))

(def-emit-rule relative-pathname (:rel-path (cdr path))
  (format nil "^. ~{~a~^ . ~} . ~a"
	  (mapcar (lambda (x)
		    (try-emit x pathname-element))
		  (butlast path))
	  (try-emit (car (last path)) identifier)))

(def-emit-rule package-pathname (:package-path (cdr path))
  (format nil "@ ~a . ~{~a~^ . ~} . ~a"
	  (try-emit (car path) identifier)
	  (mapcar (lambda (x)
		    (try-emit x identifier))
		  (butlast (cdr path)))
	  (try-emit (car (last (cdr path))) identifier)))

(def-emit-rule pathname-element x
  ;; I removed all identically looking options -- all are symbol-literals
  (try-emit x identifier generate-pathname-element))

(def-emit-rule generate-pathname-element (:generate label (cap expr (maybe x)))
  (if expr
      #?"$((try-emit label label)) ($((try-emit expr expression)))"
      #?"$((try-emit label label))"))

(def-emit-rule compound-name (:compound atomic (cdr tails))
  (format nil "~a~{~a~}" (try-emit atomic atomic-name)
	  (mapcar (lambda (x)
		    (try-emit x name-tail))
		  tails)))

(def-emit-rule name-tail x (try-emit x parenthesized-compound-tail discrete-range-tail selected-tail
				     attribute-tail funcall-tail))

(def-emit-rule parenthesized-compound-tail (:paren (cdr exprs))
  (format nil "(~{~a~^, ~})" (mapcar (lambda (x)
				       (try-emit x expression))
				     exprs)))

(def-emit-rule discrete-range-tail x
  #?"($((try-emit x discrete-range)))")

(def-emit-rule selected-tail (:dot x)
  #?".$((try-emit x atomic-name))")

(def-emit-rule attribute-tail (:attribute id (cap sig (maybe (:signature _))) (cap expr (maybe _)))
  (format nil "~a'~a~a"
	  (if sig
	      (try-emit (cadr sig) signature)
	      "")
	  (try-emit id symbol-literal)
	  (if expr
	      #?"($((try-emit expr expression)))"
	      "")))

(def-emit-rule funcall-tail (:paren (cdr lst))
  #?"($((try-emit lst association-list)))")

(def-emit-rule association-list lst
  (format nil "~{~a~^, ~}" (mapcar (lambda (x)
				     (try-emit x association-elt))
				   lst)))

(def-emit-rule association-elt x
  (ecase-match x ((:=> x y) #?"$((try-emit x formal-part)) => $((try-emit y actual-part))")
	       (_ #?"$((try-emit x actual-part))")))

(def-emit-rule formal-part x
  ;; TODO : actually implement this
  (fail-emit))

(def-emit-rule actual-part x
  ;; TODO : actually implement this
  (fail-emit))

(def-emit-rule function-call x_function-call-p
  (try-emit x compound-name))

;; OK, let's write up to most general expressions

(defmacro def-funforward-rule (name forward-name pattern &body body)
  "The rule that sometimes implements 'function forward' -- sends responsibility to another rule"
  (with-gensyms (g!-real-name)
    `(progn (def-emit-rule ,g!-real-name ,pattern ,@body)
	    (def-emit-rule ,name _
	      (try-emit whole ,g!-real-name ,forward-name)))))

(defmacro def-op-expr-rule (name sub-name &rest ops)
  `(def-funforward-rule ,name ,sub-name ((cap op (or ,@ops)) x y)
     (format nil "~a ~a ~a"
	     (try-emit x ,sub-name)
	     (try-emit op symbol-literal)
	     (try-emit y ,sub-name))))

(def-op-expr-rule relation relation-one :?< :?<= :?> :?>=)
(def-op-expr-rule relation-one relation-two :?= :?/=)
(def-op-expr-rule relation-two relation-three :<  :<= :> :>=)
(def-op-expr-rule relation-three shift-expression := :/=)
(def-op-expr-rule shift-expression sub-shift-expression :sla :sra)
(def-op-expr-rule sub-shift-expression simple-expression :sll :srl :rol :ror)

(def-funforward-rule logical-expression relation ((cap op (or :and :nand :or :nor :xor :xnor)) x y (cdr z))
  (joinl #?" $((try-emit op symbol-literal)) "
	 `(,(try-emit x relation)
	    ,(try-emit y relation)
	    ,.(mapcar (lambda (x)
			(try-emit x relation))
		      z))))

(def-emit-rule condition _
  (try-emit whole expression))

(def-emit-rule expression _
  (let ((*expression* whole))
    (try-emit whole bin-coerce-primary logical-expression)))

(def-emit-rule bin-coerce-primary (:?? x)
  #?"(?? $((try-emit x primary)))")

;; Let's move on to compound constructs

(def-funforward-rule labeled-sequential-statement sequential-statement (:label label statement)
  #?"$((try-emit label symbol-literal)): $((try-emit statement sequential-statement))")

(def-emit-rule sequential-statement _
  ;; TODO : do a logarithmic dispatch here, instead of linear one
  (try-emit whole
	    wait-statement assertion-statement report-statement signal-assignment-statement
	    variable-assignment-statement procedure-call-statement if-statement case-statement
	    loop-statement next-statement exit-statement return-statement null-statement
	    triple-dot-statement))

(def-emit-rule wait-statement (:wait (cap on (maybe (:on _)))
				     (cap until (maybe (:until _)))
				     (cap for (maybe (:for _))))
  ;; (format t "I'm here!~%")
  (format nil "wait~a~a~a;"
	  (try-opt-kwd-emit on names)
	  (try-opt-kwd-emit until condition)
	  (try-opt-kwd-emit for expression)))

(def-emit-rule names _
  (joinl ", " (mapcar (lambda (x)
			(try-emit x name))
		      whole)))

;; There's, apparently, a lot of code-duplication, but I don't know how to fix this
;; in a uniform way -- yet =)
(def-emit-rule assertion-statement (:assert condition (cap report (maybe (:report _)))
					    (cap severity (maybe (:severity _))))
  (format nil "assert ~a~a~a;"
	  (try-emit condition condition)
	  (try-opt-kwd-emit report expression)
	  (try-opt-kwd-emit severity expression)))

(def-emit-rule report-statement (:report report (cap severity (maybe (:severity _))))
  (format nil "report ~a~a;"
	  (try-emit report expression)
	  (try-opt-kwd-emit severity expression)))

(def-emit-rule triple-dot-statement :|...|
	       "...")

(def-emit-rule null-statement :null "null;")

(def-emit-rule return-statement (:return (cap return (maybe _)))
  (format nil "return~a;"
	  (try-opt-emit return expression)))

(def-emit-rule exit-statement (:exit (cap label (maybe _symbolp)) (cap when (maybe (:when _))))
  (format nil "exit~a~a;"
	  (try-opt-emit label symbol-literal)
	  (try-opt-kwd-emit when condition)))

(def-emit-rule next-statement (:next (cap label (maybe _symbolp)) (cap when (maybe (:when _))))
  (format nil "next~a~a;"
	  (try-opt-emit label symbol-literal)
	  (try-opt-kwd-emit when condition)))

;; TODO : I vaguely remember that VHDL strings can't contain newlines ... look if it's true

(def-emit-rule loop-statement (:loop (cap head (maybe ((or :while :for) (cdr _))))
				     (cdr rest))
  (format nil "~aloop~%~{~a~%~}end loop;"
	  (if head #?"$((try-emit head iteration-head)) " "")
	  (mapcar (lambda (x)
		    (try-emit x sequential-statement))
		  rest)))
  
(def-emit-rule iteration-head _
  (try-emit whole while-head for-head))

(def-emit-rule while-head (:while x)
  #?"while $((try-emit x condition))")
(def-emit-rule for-head (:for id :in range)
  #?"for $((try-emit id identifier)) in $((try-emit range discrete-range))")
	    
(defmacro def-notimplemented-emit-rule (name)
  `(def-emit-rule ,name _ (fail-emit)))

(def-emit-rule case-statement ((cap op (or :case? :case)) expr (cdr rest))
  (format nil "~a ~a is ~{~a~%~}end case;"
	  (if (eq :case op)
	      "case"
	      "case ?")
	  (try-emit expr expression)
	  (mapcar (lambda (x)
		    (format nil "when ~a => ~{~a~^~%~}"
			    (try-emit (car x) choices)
			    (mapcar (lambda (y)
				      (try-emit y sequential-statement))
				    (cdr x))))
		  rest)))

(def-emit-rule if-statement (:cond (then-cond (cdr then-forms))
				   (cap elsifs (collect-until ('t (cdr _))))
				   (cap else (maybe ('t (cdr _)))))
  ;; (format t "I'm here~%")
  (format nil "if ~a then ~%~{~a~%~}~a~%~a~%end if;"
	  (try-emit then-cond condition)
	  (mapcar (lambda (x)
	  	    (try-emit x sequential-statement))
	  	  then-forms)
	  (joinl "~%" (mapcar (lambda (elsif-form)
				(format nil "elsif ~a then~%~a"
					(try-emit (car elsif-form) condition)
					(joinl "~%" (mapcar (lambda (x)
							      (try-emit x sequential-statement))
							    (cdr elsif-form)))))
			      elsifs))
	  (if else
	      (format nil "else ~a"
		      (joinl "~%" (mapcar (lambda (x)
					    (try-emit x sequential-statement))
					  (cdr else))))
	      "")))

(def-emit-rule procedure-call-statement _
  #?"$((try-emit whole compound-name));") ;; this should be just (:COMPOUND name ... (:PAREN ...))

(def-emit-rule variable-assignment-statement _
  (try-emit whole when-variable-assignment select-variable-assignment simple-variable-assignment))

(def-emit-rule when-variable-assignment (:setf x (:when (cond-1 expr-1)
						   (cap elsifs (collect-until ('t _)))
						   (cap else (maybe ('t _)))))
  (format nil "~a := ~a when~%~a~%~a~%~a;"
	  (try-emit x name aggregate)
	  (try-emit expr-1 expression)
	  (try-emit cond-1 condition)
	  (joinl "~%" (mapcar (lambda (x)
				#?"else $((try-emit (cadr x) expression)) when $((try-emit (car x) condition))")
			      elsifs))
	  (if else
	      #?"else $((try-emit (cadr else) expression))"
	      "")))

(def-emit-rule select-variable-assignment (:setf x ((cap op (or :select? :select)) expr
						    (cdr rest)))
  (format nil "with ~a select ~a~a :=~%~a;"
	  (try-emit expr expression)
	  (if (eq :select? op) "? " "")
	  (try-emit x name aggregate)
	  (joinl ",~%" (mapcar (lambda (x)
				 #?"$((try-emit (cadr x) expression)) when $((try-emit (car x) choices))")
			       rest))))

(def-emit-rule simple-variable-assignment (:setf x y)
  #?"$((try-emit x name aggregate)) := $((try-emit y expression));")

(def-emit-rule signal-assignment-statement (:<= (cdr rest))
  (try-emit rest simple-signal-assignment conditional-signal-assignment selected-signal-assignment))

(def-emit-rule simple-signal-assignment _
  (try-emit whole
	    really-simple-signal-assignment
	    force-simple-signal-assignment
	    release-simple-signal-assignment))

(def-emit-rule really-simple-signal-assignment (name (cap delay (maybe _delay-mechanism-p)) x)
  (format nil "~a <= ~a~a;"
	  (try-emit name name aggregate)
	  (if delay #?"(try-emit delay delay-mechanism) " "")
	  (try-emit x waveform)))

(def-emit-rule force-simple-signal-assignment (name x_force-p y)
  (format nil "~a <= ~a ~a;"
	  (try-emit name name)
	  (try-emit x force-statement)
	  (try-emit y expression)))

(def-emit-rule release-simple-signal-assignment (name (:release (cdr x)))
  (format nil "~a <= release~a;"
	  (try-emit name name)
	  (if x #?" $((try-emit x symbol-literal))" "")))

(defun force-p (x)
  (match-p (:force (cdr _)) x))

(defun delay-mechanism-p (x)
  (match-p (or :transport (:inertial (cdr _))) x))

(def-emit-rule force-statement (:force (cdr x))
  (format nil "force~a"
	  (if x #?" $((try-emit (car x) symbol-literal))" "")))

(def-emit-rule conditional-signal-assignment (name (cap delay (maybe _delay-mechanism-p))
						   (cap force (maybe _force-p))
						   (:when (cond-1 expr-1)
						     (cap elsifs (collect-until ('t _)))
						     (cap else (maybe ('t _)))))
  (format nil " ~a <= ~a~a ~a when ~a~%~a~%~a;"
	  (try-emit name name aggregate)
	  (if delay #?"$((try-emit delay delay-mechanism)) " "")
	  (if force #?"$((try-emit force force-statement)) " "")
	  (try-emit expr-1 waveform expression)
	  (try-emit cond-1 condition)
	  (joinl "~%" (mapcar (lambda (x)
				(format nil "else ~a when ~a" 
					(try-emit (cadr x) waveform expression)
					(try-emit (car x) choices)))
			      elsifs))
	  (if else
	      #?"else $((try-emit (cadr else) waveform expression))"
	      "")))

(def-emit-rule selected-signal-assignment (name (cap delay (maybe _delay-mechanism-p))
						(cap force (maybe _force-p))
						((cap op (or :select? :select))
						 selector
						 (cdr clauses)))
  (format nil "with ~a select~a ~a <= ~a~a~%~a;"
	  (try-emit selector expression)
	  (if (eq :select? op) " ?" "")
	  (try-emit name name aggregate)
	  (if delay #?"$((try-emit delay delay-mechanism)) " "")
	  (if force #?"$((try-emit force force-statement)) " "")
	  (joinl ",~%" (mapcar (lambda (x)
				 #?"$((try-emit (cadr x) waveform expression)) when $((try-emit (car x) choices))")
			       clauses))))


(def-emit-rule delay-mechanism _
  (ecase-match whole
	       (:transport (try-emit whole symbol-literal))
	       ((:inertial (cdr x)) (format nil "~ainertial"
					    (if x #?"reject $((try-emit (car x) expression)) " "")))))



(def-emit-rule waveform (:waveform (cdr rest))
  (try-emit rest unaffected-waveform usual-waveform))

(def-emit-rule unaffected-waveform (:unaffected)
  "unaffected")

(def-emit-rule usual-waveform _
  (joinl ",~%"
	 (mapcar (lambda (x)
		   (ecase-match x ((x (:after y)) #?"$((try-emit x expression)) after $((try-emit y expression))")
				((x) #?"$((try-emit x expression))")))
		 whole)))
	    
