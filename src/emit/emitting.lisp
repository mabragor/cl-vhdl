
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
		 (error "We should emit strings, but rule ~a returned something else..." ',name)
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

(def-emit-rule character-literal x_characterp #?"'$(x)'")

(def-emit-rule bit-string-literal (:bin it_stringp) #?"B\"$(it)\"")


