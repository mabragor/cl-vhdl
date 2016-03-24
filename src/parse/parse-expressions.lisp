
(in-package #:cl-vhdl)
(enable-read-macro-tokens)

(defun populate-precedence (lst)
  (let ((res (make-hash-table :test #'eq)))
    (iter (for elt in lst)
	  (for i from 1)
	  (if (atom elt)
	      (setf (gethash (intern (string elt) "KEYWORD") res) i)
	      (iter (for sub-elt in elt)
		    (setf (gethash (intern (string sub-elt) "KEYWORD") res) i))))
    res))

(defun shunting-yard (lst ops)
  "I actually don't know if it's an actual shunting yard of Dijkstra -- I just thought about my own solution."
  (let ((precedence (populate-precedence ops))
	(lst-iter (mk-iter lst)))
    (declare (special precedence))
    (%shunting-yard nil (list (inext-or-error lst-iter)) lst-iter t)))

;; (defun %shunting-yard (my-iter)
;;   (iter (for elt in-it my-iter)
;; 	(collect elt)))

(defun hash<-assoc (x)
  (iter (for (key val) in-hashtable x)
	(collect (cons key val))))

(defun %shunting-yard (cur-op cur-lst lst-iter &optional toplevel-p)
  (declare (special precedence))
  ;; (format t "precedence : ~a~%" (hash<-assoc precedence))
  (labels ((rec (new-op new-rhs)
	     ;; (format t "cur-op : ~a cur-lst : ~a~%" cur-op cur-lst)
	     (flet ((accumulate ()
		      (setf cur-lst `((,cur-op ,.(nreverse cur-lst)))
			    cur-op new-op)
		      (push new-rhs cur-lst)))
	       (cond ((null cur-op) (progn (setf cur-op new-op)
					   (push new-rhs cur-lst)))
		     ((eq new-op cur-op) (push new-rhs cur-lst))
		     (t (let ((cur-precedence (gethash cur-op precedence))
			      (new-precedence (gethash new-op precedence)))
			  (cond ((null new-precedence) (error "Unexpected operator : ~a" new-op))
				((= cur-precedence new-precedence)
				 (accumulate))
				((< cur-precedence new-precedence)
				 (if toplevel-p
				     (accumulate)
				     ;; Report to higher levels
				     (let ((new-lhs `(,cur-op ,.(nreverse cur-lst))))
				       ;; (format t "About to report new-op : ~a lhs : ~a rhs : ~a~%"
				       ;; 	       new-op new-lhs new-rhs)
				       (return-from %shunting-yard (values new-op new-lhs new-rhs)))))
				((> cur-precedence new-precedence)
				 (multiple-value-bind (op lhs rhs)
				     (%shunting-yard new-op (list new-rhs (pop cur-lst)) lst-iter)
				   ;; Recompare with new operator and rhs at hand
				   (if (not (null op))
				       (progn (push lhs cur-lst)
					      (rec op rhs))
				       (progn (push lhs cur-lst)
					      (return-from %shunting-yard
						(if toplevel-p
						    (cons cur-op (nreverse cur-lst))
						    (values nil (cons cur-op (nreverse cur-lst)) nil))))))))
			  ))))))
    (iter (for elt in-it lst-iter)
	  (rec (intern (string-upcase (car elt)) "KEYWORD") (cadr elt)))
    (if toplevel-p
	(cons cur-op (nreverse cur-lst))
	(values nil (cons cur-op (nreverse cur-lst)) nil))
    ))
		 
    

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

(define-ebnf-rule term "factor { ( * | / | MOD | REM ) factor }"
  (shunting-yard res '(* / (:mod :rem))))

(define-ebnf-rule factor "exponentiation | unary-op")

(define-ebnf-rule exponentiation "primary [** primary ]"
  (if (< 1 length)
      `(** ,1st ,(cadr 2nd))
      res))

(define-ebnf-rule unary-op ("ABS primary | NOT primary | AND primary | NAND primary | OR primary"
			    "| NOR primary | XOR primary | XNOR primary"))

(define-ebnf-rule primary
  ("name | literal | aggregate | function-call | qualified-expression | type-mark (( expression ))"
   "| NEW subtype-indication | NEW qualified-expression | (( expression ))"))

(define-ebnf-rule qualified-expression "type-mark ' (( expression )) | type-mark ' aggregate"
  (if (< 3 (length res))
      `(:qualified ,4th ,1st)
      `(:qualified ,3rd ,1st)))

(define-ebnf-rule name "compound-name | atomic-name | _external-name")

(define-ebnf-rule atomic-name "identifier | operator-symbol | character-literal"
  (when (reserved-word-p res)
    (fail-parse "Name can't be a reserved word"))
  res)

(define-ebnf-rule compound-name ("atomic-name name-tail { ... }")
  (cons 1st 2nd))

(define-ebnf-rule name-tail ("(( expression {, ...} ))"
			     "| (( discrete-range ))"
			     "| selected-tail"
			     "| attribute-tail"
			     "| funcall-tail"))

(define-ebnf-rule selected-tail ". ( atomic-name | ALL )"
  `(:dot ,2nd))
(define-ebnf-rule attribute-tail "[ signature ] ' identifier [ (( expression )) ]"
  `(:attribute ,3rd ,@(if 1st `((:signature ,1st))) ,@(if 4th `(,(cadr 4th)))))
(define-ebnf-rule funcall-tail "(( PARAMETER-association-list ))"
  `(:call ,@2nd))

(define-ebnf-rule selected-name "compound-name"
  (if (not (eq :dot (caar (last res))))
      (fail-parse "Not a selected name")
      res))
(define-ebnf-rule attribute-name "compound-name"
  (if (not (eq :attribute (caar (last res))))
      (fail-parse "Not an attribute name")
      res))

(define-ebnf-rule function-call "compound-name"
  ;; TODO : how to make a finer-grained check here?
  (if (not (string= "(" (cadar (last res))))
      (fail-parse "Not a function call")
      res))
  
(define-ebnf-rule operator-symbol "\"{ graphic-character }\"")


;; Apparently, [[ and ]] should denote [ and ] in text
(define-ebnf-rule signature
  "[[ [ type-mark {, ... } ] [ RETURN type-mark ] ]]")

(define-ebnf-rule external-name
  ("<< CONSTANT external-pathname : subtype-indication >>"
   "| << SIGNAL external-pathname : subtype-indication >>"
   "| << VARIABLE external-pathname : subtype-indication >>"))

(define-ebnf-rule external-pathname
  "absolute-pathname | relative-pathname | package-pathname")

(define-ebnf-rule absolute-pathname ". { pathname-element . } OBJECT-identifier")

(define-ebnf-rule relative-pathname "{ ^ . } { pathname-element . } OBJECT-identifier")

(define-ebnf-rule pathname-element
    ("ENTITY-identifier | COMPONENT-INSTANTIATION-label | BLOCK-label"
     "| GENERATE-STATEMENT-label [ (( STATIC-expression )) ] | PACKAGE-identifier"))

(define-ebnf-rule package-pathname "@ LIBRARY-identifier . { PACKAGE-identifier . } OBJECT-identifier")

(define-ebnf-rule literal
  ("based-literal | physical-literal | decimal-literal | identifier"
   "| character-literal | string-literal | bit-string-literal | NULL"))

(define-ebnf-rule aggregate "(( ( [ choices -> ] expression ) {, ... } ))")

(define-ebnf-rule choices "( simple-expression | discrete-range | identifier | OTHERS ) { || ... }")

(define-ebnf-rule label "identifier")

(define-ebnf-rule tool-directive "` identifier { graphic-character }")
