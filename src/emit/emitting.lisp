
(in-package #:cl-vhdl)

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

(define-condition fail-cons-parse (error) ())
(define-condition no-advance (error)
  ((value :initform nil :initarg :value :accessor no-advance-value)))
(defun fail-cons-parse ()
  (error 'fail-cons-parse))
(defun no-advance (&optional value)
  (error 'no-advance :value value))
(define-condition rebind-error (error simple-condition)
  ((opcode :initform (error "You should specify what upper layer needs to do") :initarg :opcode)))

(defun rebind-expr (opcode)
  (error 'rebind-error :opcode opcode))

(defun codewalk-pattern-at-the-car (pattern)
  (case (car pattern)
    (or `(block outer
	   ,@(mapcar (lambda (x)
		       `(handler-case ,(codewalk-pattern x t)
			  (:no-error () (return-from outer *expr*)))) ; short-circuit
		     (cdr pattern))
	   (fail-cons-parse)))
    (cap `(handler-case ,(codewalk-pattern (third pattern) t)
	    (no-advance (e) (setf (gethash ',(second pattern) *cap*)
				  (no-advance-value e))
			(error e))
	    (:no-error (x) (setf (gethash ',(second pattern) *cap*) x))))
    (len `(progn (if (not (and (listp *expr*))
			  (equal ,(second pattern) (length *expr*)))
		     (fail-cons-parse))
		 ,(codewalk-pattern (third pattern) t)
		 *expr*))
    (not `(handler-case ,(codewalk-pattern (second pattern) t)
	    (fail-cons-parse () *expr*)
	    (:no-error () (fail-cons-parse))))
    (t `(progn ,(codewalk-pattern pattern)
	       *expr*))))

(defun codewalk-list-subpattern (pattern)
  (with-gensyms (g!-res)
    (if (consp pattern)
	(case (car pattern)
	  (collect-until (codewalk-pattern `(not ,(second pattern)) t))
	  (collect-while `(let ((,g!-res nil))
			    (loop (push (handler-case ,(codewalk-pattern (second pattern) t)
					  (fail-cons-parse () (no-advance (nreverse ,g!-res))))
					,g!-res)
			       (rebind-expr :next))))
	  ;; This does advance the cursor when we are traversing the list -- how to do it?
	  (maybe `(handler-case ,(codewalk-pattern (second pattern) t)
		    (fail-cons-parse () (no-advance))))
	  (cdr `(progn (rebind-expr :cdr) ; this is a huge hack with restarts
		       ,(codewalk-pattern (second pattern) t)
		       (no-advance *expr*)))
	  (car `(progn (rebind-expr :car)
		       ,(codewalk-pattern (second pattern) t)
		       (no-advance *expr*)))
	  (t (codewalk-pattern pattern t)))
	(codewalk-pattern pattern t))))

(defvar *vars*)

(defun codewalk-pattern (pattern &optional at-the-car)
  (if (atom pattern)
      (cond ((keywordp pattern) `(if (not (eq ,pattern *expr*))
				     (fail-cons-parse)))
	    ((stringp pattern) `(if (or (not (stringp *expr*))
					(not (string= ,pattern *expr*)))
				    (fail-cons-parse)))
	    ((symbolp pattern) (when (not (eq '_ pattern))
				 (setf (gethash pattern *vars*) t)
				 `(setf (gethash ',pattern *cap*) *expr*)))
	    (t (error "Don't know how to codewalk this atomic pattern")))
      (if at-the-car
	  (codewalk-pattern-at-the-car pattern)
	  (with-gensyms (g!-stash-expr g!-cur-expr)
	    `(let ((,g!-stash-expr *expr*)
		   (,g!-cur-expr *expr*))
	       ,@(mapcar (lambda (x)
			   `(let ((*expr* (car ,g!-cur-expr)))
			      (handler-bind ((rebind-error
					      (lambda (c)
						(with-slots (opcode) c
						  (ecase opcode
						    (:car nil) ; do nothing here -- already at CAR
						    (:cdr (setf *expr* (cdr ,g!-cur-expr)))
						    (:next (setf ,g!-cur-expr (cdr ,g!-cur-expr)
								 *expr* (car ,g!-cur-expr)))))
						(invoke-restart 'continue c))))
				,(codewalk-list-subpattern x))
			      (setf ,g!-cur-expr (cdr ,g!-cur-expr))))
			 pattern)
	       (if ,g!-cur-expr
		   (fail-cons-parse))
	       ,g!-stash-expr)))))

(defun %codewalk-pattern (pattern)
  (let ((*vars* (make-hash-table :test #'eq)))
    (values (codewalk-pattern pattern t) *vars*)))


(defmacro with-smart-destructuring (pattern thing &body body)
  (once-only (thing)
    (multiple-value-bind (code vars) (%codewalk-pattern pattern)
      `(let ((*expr* ,thing)
	     (*cap* (make-hash-table :test #'eq)))
	 ,code
	 (symbol-macrolet ,(iter (for (key nil) in-hashtable vars)
				 (collect `(,key (gethash ',key *cap*))))

	   ,@body)))))
	 
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
