
(in-package #:cl-vhdl)

(cl-interpol:enable-interpol-syntax)

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
	 (lambda (whole) ; yes, we intentionally leak this variable into the BODY
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
		     `(handler-case (funcall (gethash ',x *emit-rules*) ,thing)
			(emit-error () nil)
			(:no-error (x) (return-from ,g!-outer x))))
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

(def-emit-rule physical-literal (n_numberp unit_symbolp)
  ;; TODO : somehow remove duplication of guards
  #?"$((try-emit n number-literal)) $((try-emit unit symbol-literal))")

(def-emit-rule string-literal x_stringp
  (format nil "\"~{~a~^\"\"~}\"" (cl-ppcre:split "\"" x)))

(def-emit-rule identifier x (try-emit x symbol-literal))

(def-emit-rule literal x
  (try-emit x identifier character-literal number-literal string-literal physical-literal bit-string-literal))

;; I guess, there's no other way than first learn to emit most elementary constructs (done),
;; then more and more compound ones, building to general expressions
;; only then I can address control flow constructs -- otherwise I lose the power of interactive testing.
;; So, let's slowly move along this path...

;;; primary expressions
;; (def-emit-rule primary x
;;   (try-emit x
;; 	    literal qualified-expression
;; 	    ...))

(def-emit-rule qualified-expression (:qualified thing type)
  (let ((it (ecase-emit thing (aggregate it) (expression #?"($(it))"))))
    #?"$((try-emit type type))'$(it)"))

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
  #?"$((try-emit x simple-expression)) $(dir) $((try-emit y simple-expression))")

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
