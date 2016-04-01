
(in-package #:cl-vhdl)

;;; Sequential Statements

(define-ebnf-rule sequential-statement
  ("wait-statement | assertion-statement | report-statement | signal-assignment-statement"
   "| variable-assignment-statement | procedure-call-statement | if-statement | case-statement"
   "| loop-statement | next-statement | exit-statement | return-statement | null-statement"
   "| triple-dot-statement"))
  

(define-ebnf-rule wait-statement
    "[ label : ] WAIT [ ON SIGNAL-name {, ...} ] [ UNTIL condition ] [ FOR TIME-expression ] ;"
  (wrapping-in-label `(:wait ,@(if 3rd `((:on ,@(cadr 3rd))))
			     ,@(if 4th `((:until ,(cadr 4th))))
			     ,@(if 5th `((:for ,(cadr 5th)))))))

(define-ebnf-rule assertion-statement
    "[ label : ] ASSERT condition [ REPORT expression ] [ SEVERITY expression ] ;"
  (wrapping-in-label `(:assert ,3rd
			       ,@(if 4th `(,4th))
			       ,@(if 5th `(,5th)))))

(define-ebnf-rule report-statement "[ label : ] REPORT expression [ SEVERITY expression ] ;"
  (wrapping-in-label `(:report ,3rd ,@(if 4th `(,4th)))))

(define-ebnf-rule signal-assignment-statement
  ("[ label : ] simple-signal-assignment | [ label : ] conditional-signal-assignment"
   "| [ label : ] selected-signal-assignment")
  (wrapping-in-label 2nd))

(define-ebnf-rule simple-signal-assignment
    "really-simple-signal-assignment | force-simple-signal-assignment | release-simple-signal-assignment")

(define-ebnf-rule really-simple-signal-assignment "( name | aggregate ) <= [ delay-mechanism ] waveform ;"
  `(:<= ,1st ,@(if 3rd `(,3rd)) ,4th))
  
(define-ebnf-rule force-simple-signal-assignment "name <= FORCE [ IN | OUT ] expression ;"
  `(:<= ,1st (:force ,@(if 4th `(,4th))) ,5th))

(define-ebnf-rule release-simple-signal-assignment "name <= RELEASE [ IN | OUT ] ;"
  `(:<= ,1st (:release ,@(if 4th `(,4th)))))
  
(define-ebnf-rule conditional-signal-assignment "conditional-waveform-assignment | conditional-force-assignment")

(define-ebnf-rule conditional-waveform-assignment
  ("[ label : ] ( name | aggregate ) <= [ delay-mechanism ] waveform WHEN condition"
   "{ ELSE waveform WHEN condition } [ ELSE waveform ] ;")
  (wrapping-in-label `(:<= ,2nd ,@(if 4th `(,4th)) (:when (,7th ,5th)
						     ,@(mapcar (lambda (x)
								 `(,(cadddr x) ,(cadr x)))
							       8th)
						     ,@(if 9th `((t ,(cadr 9th))))))))

(define-ebnf-rule conditional-force-assignment
  ("[ label : ] name <= FORCE [ IN | OUT ] expression WHEN condition { ELSE expression WHEN condition }"
   "[ ELSE expression ] ;")
  (wrapping-in-label `(:<= ,2nd (:force ,@(if 5th `(,5th)))
			   (:when (,8th ,6th)
			     ,@(mapcar (lambda (x)
					 `(,(cadddr x) ,(cadr x)))
				       9th)
			     ,@(if 10th `((t ,(cadr 10th))))))))



(define-ebnf-rule selected-signal-assignment "selected-waveform-assignment | selected-force-assignment")

(define-ebnf-rule selected-waveform-assignment
  ("[ label : ] WITH expression SELECT [ ? ] ( name | aggregate ) <= [ delay-mechanism ]"
   "{ waveform WHEN choices , } waveform WHEN choices ;")
  (wrapping-in-label `(:<= ,6th ,@(if 8th `(,8th)) (,(if 5th :select? :select)
						     ,3rd
						     ,@(mapcar (lambda (x)
								 `(,(caddr x) ,(car x)))
							       9th)
						     ;; NTH has a shift of index by one
						     (,(nth 11 res) ,10th)))))


(define-ebnf-rule selected-force-assignment
  ("[ label : ] WITH expression SELECT [ ? ] name <= FORCE [ IN | OUT ]"
   "{ expression WHEN choices , } expression WHEN choices ;")
  (wrapping-in-label `(:<= ,6th (:force ,@(if 9th `(,9th)))
			   (,(if 5th :select? :select)
			     ,3rd
			     ,@(mapcar (lambda (x)
					 `(,(caddr x) ,(car x)))
				       10th)
			     ;; NTH has a shift of index by one
			     (,(nth 12 res) ,(nth 10 res))))))


(define-ebnf-rule delay-mechanism "TRANSPORT | [ REJECT TIME-expression ] INERTIAL"
  (if (and (consp res) (< 1 (length res)))
      `(:inertial ,@(if 1st `(,(cadr 1st))))
      res))

(define-ebnf-rule waveform
    "( VALUE-expression [ AFTER TIME-expression ] | NULL [ AFTER TIME-expression ] ) {, ...} | UNAFFECTED"
  `(:waveform ,@(if (eq :unaffected res)
		    `(,res)
		    (mapcar (lambda (x)
			      (if (cadr x)
				  x
				  `(,(car x))))
			    res))))

(define-ebnf-rule variable-assignment-statement
  ("[ label : ] simple-variable-assignment _| [ label : ] conditional-variable-assignment"
   "_| [ label : ] selected-variable-assignment")
  (wrapping-in-label 2nd))

(define-ebnf-rule simple-variable-assignment "( name | aggregate ) := expression ;"
  `(::= ,1st ,3rd))

(define-ebnf-rule conditional-variable-assignment
    "( name | aggregate ) := expression WHEN condition { ELSE expression WHEN condition } [ ELSE expression ] ;"
  `(:= ,1st (:when (,5th ,3rd)
	      ,@(mapcar (lambda (x)
			  `(,(cadddr x) ,(cadr x)))
			6th)
	      ,@(if 7th
		    `((t ,(cadr 7th)))))))


(define-ebnf-rule selected-variable-assignment
  ("WITH expression SELECT [ ? ] ( name | aggregate ) :="
   "{ expression WHEN choices , } expression WHEN choices ;")
  `(::= ,5th (,(if 4th :select? :select)
	       ,@(mapcar (lambda (x)
			   `(,(caddr x) ,(car x)))
			 7th)
	       (,10th ,8th))))
				  

(define-ebnf-rule procedure-call-statement "[ label : ] PROCEDURE-name [ (( PARAMETER-association-list )) ] ;")

(define-ebnf-rule if-statement
  ("[ IF-label : ] IF condition THEN { sequential-statement } { ELSIF condition THEN { sequential-statement } }"
   "[ ELSE { sequential-statement } ] END IF [ IF-label ] ;")
  (wrapping-in-label `(:cond (,3rd ,@5th)
			     ,@(mapcar (lambda (x)
					 `(,(cadr x) ,@(cadddr x)))
				       6th)
			     ,@(if 7th
				   `((t ,@(cadr 7th)))))))

(define-ebnf-rule case-statement
  ("[ CASE-label : ] CASE _[ ? ] expression IS ( WHEN choices => { sequential-statement } ) { ... }"
   "END CASE _[ ? ] [ CASE-label ] ;")
  (wrapping-in-label `(,(if 3rd :case? :case) ,4th
			,@(mapcar (lambda (x)
				    `(,(cadr x) ,@(cadddr x)))
				  6th))))

(define-ebnf-rule loop-statement
  ("[ LOOP-label : ] [ WHILE condition | FOR identifier IN discrete-range ] LOOP { sequential-statement }"
   "END LOOP [ LOOP-label ] ;")
  (wrapping-in-label `(:loop ,@(if 2nd `(,2nd))
			     ,@4th)))

(define-ebnf-rule next-statement "[ label : ] NEXT [ LOOP-label ] [ WHEN condition ] ;"
  (wrapping-in-label `(:next ,@(if 3rd `(,3rd))
			     ,@(if 4th `(,4th)))))

(define-ebnf-rule exit-statement "[ label : ] EXIT [ LOOP-label ] [ WHEN condition ] ;"
  (wrapping-in-label `(:exit ,@(if 3rd `(,3rd))
			     ,@(if 4th `(,4th)))))

(define-ebnf-rule return-statement "[ label : ] RETURN [ expression ] ;"
  (wrapping-in-label `(:return ,@(if 3rd `(,3rd)))))

(define-ebnf-rule null-statement "[ label : ] NULL ;"
  (wrapping-in-label 2nd))

(define-ebnf-rule triple-dot-statement " ...... ")

