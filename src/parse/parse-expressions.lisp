
(in-package #:cl-vhdl)


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
  (macrolet ((return-statement ()
	       `(if toplevel-p
		    (if (null cur-op)
			(if (< 1 (length cur-lst))
			    (error "Something went wrong : NIL operator and several operands : ~a"
				   cur-lst)
			    (car cur-lst))
			(cons cur-op (nreverse cur-lst)))
		    (values nil (cons cur-op (nreverse cur-lst)) nil))))
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
						(return-from %shunting-yard (return-statement))))))
				  ))))
		 )))
      (iter (for elt in-it lst-iter)
	    (rec (intern (string-upcase (car elt)) "KEYWORD") (cadr elt)))
      (return-statement))))
		 
    

;;; Expressions and Names

(define-ebnf-rule condition "expression")

(define-ebnf-rule expression "( ?? primary ) | logical-expression")

;; (define-ebnf-rule logical-expression
;;   ("relation { AND relation } | relation [ NAND relation ] | relation { OR relation }"
;;    "| relation [ NOR relation ] | relation { XOR relation } | relation { XNOR relation }"))

(define-ebnf-rule logical-expression "relation { ( AND | NAND | OR | NOR | XOR | XNOR ) relation }"
  ;; (format t "~a" res)
  (shunting-yard (cons 1st 2nd) '(and nand or nor xor xnor)))

(defmacro maybe-a-binary-operator ()
  `(if 2nd
       `(,(car 2nd) ,1st ,(cadr 2nd))
       1st))

(define-ebnf-rule relation
    "relation-one [ ( _?< | _?<= | _?> | _?>= ) relation-one ]"
  (maybe-a-binary-operator))

(define-ebnf-rule relation-one "relation-two [ ( _?= | _?/= ) relation-two ]"
  (maybe-a-binary-operator))

(define-ebnf-rule relation-two "relation-three [ ( < | <= | > | >= ) relation-three ]"
  (maybe-a-binary-operator))

(define-ebnf-rule relation-three "shift-expression [ ( = | /= ) shift-expression ]"
  (maybe-a-binary-operator))

(define-ebnf-rule shift-expression "sub-shift-expression [ ( SLA | SRA ) sub-shift-expression ]"
  (maybe-a-binary-operator))

(define-ebnf-rule sub-shift-expression
    "simple-expression [ ( SLL | SRL | ROL | ROR ) simple-expression ]"
  (maybe-a-binary-operator))
  
(define-ebnf-rule simple-expression "[ + | - ] term { ( + | - | & ) term }"
  (shunting-yard (cons (if 1st
			   (list 1st 2nd)
			   2nd)
		       3rd)
		 '((+ -) &)))

(define-ebnf-rule term "factor { ( * | / | MOD | REM ) factor }"
  (shunting-yard (cons 1st 2nd) '(* / (:mod :rem))))

(define-ebnf-rule factor "exponentiation | unary-op")

(define-ebnf-rule exponentiation "primary [** primary ]"
  (if 2nd
      `(:** ,1st ,(cadr 2nd))
      1st))

(define-ebnf-rule unary-op ("ABS primary | NOT primary | AND primary | NAND primary | OR primary"
			    "| NOR primary | XOR primary | XNOR primary"))

(define-ebnf-rule primary
  ("literal | name | aggregate | function-call | qualified-expression | type-mark (( expression ))"
   "| NEW subtype-indication | NEW qualified-expression | parenthesized-expression"))

(define-ebnf-rule parenthesized-expression "(( expression ))"
  2nd)

(define-ebnf-rule qualified-expression "type-mark ' (( expression )) | type-mark ' aggregate"
  (if (< 3 (length res))
      `(:qualified ,4th ,1st)
      `(:qualified ,3rd ,1st)))

(define-ebnf-rule name "compound-name | atomic-name | _external-name")

(define-c-ebnf-rule atomic-name "identifier | operator-symbol | character-literal"
  (if (eq toplevel :t)
      (fail-if-reserved)
      res))

(define-ebnf-rule name-tails "name-tail { ... }")

(define-vhdl-rule compound-name (&optional hint)
  (let* ((atomic (v atomic-name))
	 (toplevel :nil))
    (let ((tails (v name-tails)))
      `(:compound ,atomic ,@tails))))

(define-ebnf-rule name-tail ("parenthesized-compound-tail"
			     "| discrete-range-tail"
			     "| selected-tail"
			     "| attribute-tail"
			     "| funcall-tail"))

(define-ebnf-rule expressions "expression {, ...}")

(define-vhdl-rule parenthesized-compound-tail (&optional hint)
  ;; With respect to expression parsing we are again at "toplevel"
  (let ((toplevel :t))
    `(:paren ,@(progm #\( expressions #\)))))


(define-vhdl-rule discrete-range-tail (&optional hint)
  (let ((toplevel :t))
    (progm #\( discrete-range #\))))

(define-ebnf-rule selected-tail ". ( atomic-name | ALL )"
  `(:dot ,2nd))
(define-vhdl-rule attribute-tail (&optional hint) ;; "[ signature ] ' identifier [ (( expression )) ]"
  (cap sig (? signature)) (v #\') (cap id identifier) (cap expr (? (let ((toplevel :t))
								     (progm #\( expression #\)))))
  `(:attribute ,(recap id)
	       ,@(if (recap? sig)
		     `((:signature ,(recap? sig))))
	       ,@(if (recap? expr) `(,(recap? expr)))))

(define-vhdl-rule funcall-tail (&optional hint)
  (let ((toplevel :t))
    `(:paren ,@(progm #\( (v association-list :parameter) #\)))))

(define-ebnf-rule selected-name "compound-name"
  (if (not (eq :dot (caar (last res))))
      (fail-parse "Not a selected name")
      res))
(define-ebnf-rule attribute-name "compound-name"
  (if (not (eq :attribute (caar (last res))))
      (fail-parse "Not an attribute name")
      res))

(define-ebnf-rule function-call "compound-name"
  (if (not (eq :paren (caar (last res))))
      (fail-parse "Not a function call")
      res))

(defun not-double-quote (x)
  (not (char= #\" x)))

(define-vhdl-rule operator-symbol ()
  (text (progm #\" (times (pred #'not-double-quote graphic-character)) #\")))


;; Apparently, [[ and ]] should denote [ and ] in text
(define-ebnf-rule signature
    "[[ [ type-mark {, ... } ] [ RETURN type-mark ] ]]"
  `(,@2nd ,@(if 3rd `(,3rd))))

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
  ("bit-string-literal | based-literal | physical-literal | decimal-literal | identifier"
   "| character-literal | string-literal | NULL"))

(define-ebnf-rule aggregate "(( ( [ choices => ] expression ) {, ... } ))"
  `(:aggregate ,@(mapcar (lambda (x)
			   (if (car x)
			       `(:=> ,(caar x) ,(cadr x))
			       (cadr x)))
			 2nd)))

(define-ebnf-rule choices "( discrete-range | simple-expression | identifier | OTHERS ) { || ... }"
  (if (equal 1 (length res))
      1st
      `(:|| ,. res)))

(define-ebnf-rule label "identifier"
  (fail-if-reserved))
    

(define-ebnf-rule tool-directive "` identifier { graphic-character }")
