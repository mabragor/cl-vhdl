
(in-package #:cl-vhdl)

;; OK, let's try to emit *some* VHDL from s-exp and see how far will it go...
;; The pattern for if (cond) would look something like

;; The second component is destructuring of initial list

;; Let's try something more complicated than if

;; (with-smart-destructuring (:if (cond (cdr then)) (collect-while ((not 't) (cdr _))) (maybe ('t (cdr else)))) expr
;; 			  ...)

" -- first line just to make indents look similar
{$label : }if $cond then [if $label] -- curly brackets
  $then
elsif $a then [for (a . b) in elsifs -- control statements in brackets afterwards
  $b            ]
else          [if else
  $else         ]
end if{ $label}; [if label]
"

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
(define-condition no-advance (error) ())
(defun fail-cons-parse ()
  (error 'fail-cons-parse))
(defun no-advance ()
  (error 'no-advance))


(defun codewalk-pattern-at-the-car (pattern)
  (case (car pattern)
    (or `(block outer
	   ,@(mapcar (lambda (x)
		       `(handler-case ,(codewalk-pattern x t)
			  (:no-error () (return-from outer)))) ; short-circuit
		     (cdr pattern))
	   (fail-cons-parse)))
    (cap `(progn ,(codewalk-pattern (third pattern) t)
		 (setf (gethash ',(second pattern) *cap*) *expr*)))
    (len `(progn (if (not (and (listp *expr*))
			  (equal ,(second pattern) (length *expr*)))
		     (fail-cons-parse))
		 ,(codewalk-pattern (third pattern) t)))
    (not `(handler-case ,(codewalk-pattern (second pattern) t)
	    (fail-cons-parse () nil)
	    (:no-error () (fail-cons-parse))))
    (t (codewalk-pattern pattern))))

(defun codewalk-list-subpattern (pattern)
  (if (consp pattern)
      (case (car pattern)
	(collect-until ...)
	(collect-while ...)
	;; This does advance the cursor when we are traversing the list -- how to do it?
	(maybe `(handler-case (let ((*expr* (car *expr*)))
				,(codewalk-pattern (second pattern) t))
		  (:no-error () (setf *expr* (cdr *expr*)))))
	(cdr `(let ((*expr* (cdr *expr*)))
		,(codewalk-pattern (second pattern) t)))
	(car `(let ((*expr* (car *expr*)))
		,(codewalk-pattern (second pattern) t)))
	(t `(handler-case (let ((*expr* (car *expr*)))
			    ,(codewalk-pattern x t))
	      (:no-error () (setf *expr* (cdr *expr*))))))
      `(handler-case (let ((*expr* (car *expr*)))
		       ,(codewalk-pattern x t))
	 (:no-error () (setf *expr* (cdr *expr*))))))



(defun codewalk-pattern (pattern &optional at-the-car)
  (if (atom pattern)
      (cond ((keywordp pattern) `(if (not (eq ,pattern *expr*))
				     (fail-cons-parse)))
	    ((stringp pattern) `(if (or (not (stringp *expr*))
					(not (string= ,pattern *expr*)))
				    (fail-cons-parse)))
	    ((symbolp pattern) (when (not (eq '_ pattern))
				 (push pattern *vars*)
				 `(setf (gethash ',pattern *cap*) *expr*)))
	    (t (error "Don't know how to codewalk this atomic pattern")))
      (if at-the-car
	  (codewalk-pattern-at-the-car pattern)
	  `(progn ,(mapcar #'codewalk-list-subpattern pattern)))))


(defmacro with-smart-destructuring (pattern thing &body body)
  (once-only (thing)
    ...))

;; Looks like I understand now how this destructuring should work
;; -- all the "variables" inside body should be grepped beforehand, and be "unbound" outside
;; -- we should not mix in dots (or any reader syntax) -- instead relying on "special forms" like CDR

;; OK, now we need to make emitting language equally flexible

(defun foo ()
  (let ((x 1))
    (+ (let ((x 3))
	 (makunbound 'x)
	 x)
       x)))

  
  
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
