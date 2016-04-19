
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
			  (:no-error () (return-from outer expr)))) ; short-circuit
		     (cdr pattern))
	   (fail-cons-parse)))
    (len `(progn (if (not (and (listp expr))
			  (equal ,(second pattern) (length expr)))
		     (fail-cons-parse))
		 ,(codewalk-pattern (third pattern) t)
		 expr))
    (not `(handler-case ,(codewalk-pattern (second pattern) t)
	    (fail-cons-parse () expr)
	    (:no-error () (fail-cons-parse))))
    (t `(progn ,(codewalk-pattern pattern)
	       expr))))

(defun codewalk-list-subpattern (pattern)
  (with-gensyms (g!-res)
    (if (consp pattern)
	(case (car pattern)
	  (collect-until (codewalk-pattern `(not ,(second pattern)) t))
	  (collect-while `(let ((,g!-res nil))
			    (iter (while t)
				  (handler-case (rebind-expr-next)
				    (fail-cons-parse () (terminate)))
				  (push (handler-case ,(codewalk-pattern (second pattern) t)
					  (fail-cons-parse () (rebind-expr-prev) (terminate)))
					,g!-res))
			    (nreverse ,g!-res)))
	  (maybe `(progn (handler-case (rebind-expr-next)
			   (fail-cons-parse () nil))
			 (handler-case ,(codewalk-pattern (second pattern) t)
			   (fail-cons-parse () (rebind-expr-prev)))))
	  (cdr `(progn (rebind-expr-cdr)
		       ,(codewalk-pattern (second pattern) t)))
	  (car `(progn (rebind-expr-car)
		       ,(codewalk-pattern (second pattern) t)))
	  (cap `(setf (gethash ',(second pattern) cap)
		      ,(codewalk-list-subpattern (third pattern))))
	  (t `(progn (rebind-expr-next)
		     ,(codewalk-pattern pattern))))
	`(progn (rebind-expr-next)
		,(codewalk-pattern pattern)))))

(defvar *vars*)

(defun codewalk-atomic-pattern (pattern)
  (cond ((keywordp pattern) `(if (not (eq ,pattern expr))
				 (fail-cons-parse)))
	((stringp pattern) `(if (or (not (stringp expr))
				    (not (string= ,pattern expr)))
				(fail-cons-parse)))
	((symbolp pattern) (when (not (eq '_ pattern))
			     (setf (gethash pattern *vars*) t)
			     `(setf (gethash ',pattern cap) expr)))
	(t (error "Don't know how to codewalk this atomic pattern"))))

(defun codewalk-cons-pattern (pattern)
  (with-gensyms (g!-expr-iter)
    `(progn (when (not (consp expr))
	      (fail-cons-parse))
	    (let ((,g!-expr-iter (mk-expr-iter expr)))
	      (macrolet ((rebind-expr-car () `(setf expr (funcall ,',g!-expr-iter :car)))
			 (rebind-expr-cdr () `(setf expr (funcall ,',g!-expr-iter :cdr)))
			 (rebind-expr-next () `(setf expr (funcall ,',g!-expr-iter :next)))
			 (rebind-expr-prev () `(setf expr (funcall ,',g!-expr-iter :prev))))
		,@(mapcar (lambda (x)
			    `(let (expr)
			       ,(codewalk-list-subpattern x)))
			  pattern)
		,@(if (or (atom (car (last pattern)))
			  (not (eq 'cdr (car (car (last pattern))))))
		      `((if (not (end-of-iter-p ,g!-expr-iter))
			    (fail-cons-parse)))))
	      expr))))

(defun end-of-iter-p (iter)
  (funcall iter :end-p))

;; ,@(mapcar (lambda (x)
;; 		  `(handler-bind ((rebind-error
;; 				   (lambda (c)
;; 				     (with-slots (opcode) c
;; 				       (ecase opcode
;; 					 (:car (setf ,g!-cur-expr (car (or ,g!-cur-expr
;; 									   ,g!-stash-expr))
;; 						     expr ,g!-cur-expr))
;; 					 (:cdr (setf ,g!-cur-expr (cdr (or ,g!-cur-expr
;; 									   ,g!-stash-expr))
;; 						     expr ,g!-cur-expr))
;; 					 (:next (setf ,g!-cur-expr (if ,g!-cur-expr
;; 								       (cdr ,g!-cur-expr)
;; 								       ,g!-stash-expr)
;; 						      expr (car ,g!-cur-expr)))))
;; 				     (invoke-restart 'continue c))))
;; 		     ,(codewalk-list-subpattern x)))


(defun codewalk-pattern (pattern &optional at-the-car)
  (if (atom pattern)
      (codewalk-atomic-pattern pattern)
      (if at-the-car
	  (codewalk-pattern-at-the-car pattern)
	  (codewalk-cons-pattern pattern))))

(defun %codewalk-pattern (pattern)
  (let ((*vars* (make-hash-table :test #'eq)))
    (values (codewalk-pattern pattern t) *vars*)))

(defun mk-expr-iter (thing)
  (let ((inner-thing thing)
	prev-cur-thing
	cur-thing
	(index -1))
    (lambda (cmd)
      ;; (format t "inner : ~a, prev-cur-thing : ~a, cur-thing : ~a, index : ~a~%"
      ;; 	      inner-thing prev-cur-thing cur-thing index)
      (ecase cmd
	(:next (if (equal -1 index)
		   (if (not (consp inner-thing))
		       (fail-cons-parse))
		   (if (or (not (consp cur-thing))
			   (not (consp (cdr cur-thing))))
		       (fail-cons-parse)))
	       (if (equal -1 index)
		   (setf index 0
			 cur-thing inner-thing)
		   (setf index (1+ index)
			 prev-cur-thing cur-thing
			 cur-thing (cdr cur-thing)))
	       (car cur-thing))
	(:prev (if (equal -1 index)
		   (error "Something went horribly wrong -- requesting prev when index -1"))
	       (if (eq :prev-used prev-cur-thing)
		   (error "Something went horribly wrong -- requesting two prevs in a row"))
	       (setf index (1- index)
		     cur-thing prev-cur-thing
		     prev-cur-thing :prev-used)
	       (car cur-thing))
	(:car (if (equal -1 index)
		  (when (or (not (consp inner-thing))
			    (cdr inner-thing))
		    (fail-cons-parse))
		  (when (or (not (consp cur-thing))
			    (cdr cur-thing))
		    (fail-cons-parse)))
	      (if (equal -1 index)
		  (setf index 0
			cur-thing (car inner-thing))
		  (setf index (1+ index)
			prev-cur-thing cur-thing
			cur-thing (car cur-thing))))
	(:cdr (if (equal -1 index)
		  (setf index 0
			cur-thing (cdr inner-thing))
		  (setf index (1+ index)
			prev-cur-thing cur-thing
			cur-thing (cdr cur-thing))))
	(:end-p (if (equal -1 index)
		    (not (cdr inner-thing))
		    (not (cdr cur-thing))))))))

		  
		     

(defmacro with-smart-destructuring (pattern thing &body body)
  (once-only (thing)
    (multiple-value-bind (code vars) (%codewalk-pattern pattern)
      `(let ((cap (make-hash-table :test #'eq))
	     (expr ,thing))
	 ,code
	 (symbol-macrolet ,(iter (for (key nil) in-hashtable vars)
				 (collect `(,key (gethash ',key cap))))
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
