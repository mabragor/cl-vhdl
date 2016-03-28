(in-package :cl-user)

(defpackage :cl-vhdl-tests
  (:use :alexandria :cl :cl-vhdl :fiveam :iterate :optima :fare-quasiquote)
  (:shadowing-import-from :fiveam :fail)
  ;; these are symbols that occasionally end up in CL-VHDL, but we want them here
  (:shadowing-import-from :cl-vhdl :default :case-insensitive-string)
  (:export #:run-tests))

(in-package :cl-vhdl-tests)

(cl-interpol:enable-interpol-syntax)

(def-suite vhdl)
(in-suite vhdl)

(defun run-tests ()
  (let ((results (run 'vhdl)))
    (fiveam:explain! results)
    (unless (fiveam:results-status results)
      (error "Tests failed."))))

(test whitespace
  (is (equal '(:comment " asdf") (vhdl-parse 'one-line-comment "-- asdf")))
  (is (equal '((:comment " asdf *a ") 13)
	     (multiple-value-list (vhdl-parse 'multi-line-comment "/* asdf *a */ " :junk-allowed t)))))

(test reserved-words
  (is (equal 'default (let ((*vhdl-version* nil)) (vhdl-parse 'reserved-word "default"))))
  (is (equal :caboom!
	     (handler-case (let ((*vhdl-version* 87)) (vhdl-parse 'reserved-word "default"))
	       (esrap-liquid::esrap-error () :caboom!)))))

(test dec-number-literals
  (macrolet ((frob (x y) `(is (equal ',x (vhdl-parse 'number-literal ,y)))))
    (frob 23 "23")
    (frob 0 "0")
    (frob 146 "146")
    (frob 23.1 "23.1")
    (frob 0.0 "0.0")
    (frob 3.14159 "3.14159")
    (frob 4600000 "46E5")
    (frob 1000000000000 "1E+12")
    (frob 19 "19e00")
    (frob 1.234e9 "1.234E09")
    (frob 9.86e22 "98.6E+21")
    (frob 3.3999999e-7 "34.0e-08")))

(test explicit-base-number-literals
  (macrolet ((frob (x y) `(is (equal ',x (vhdl-parse 'number-literal ,y)))))
    (frob 253 "2#11111101#")
    (frob 253 "16#FD#")
    (frob 253 "16#0fd#")
    (frob 253 "8#0375#")
    (frob 0.5 "2#0.100#")
    (frob 0.5 "8#0.4#")
    (frob 0.5 "12#0.6#")
    (frob 1024 "2#1#E10")
    (frob 1024 "16#4#E2")
    (frob 1024 "10#1024#E+00")
    (frob 123456 "123_456")
    (frob 3.1415926 "3.141_592_6")
    (frob 64512 "2#1111_1100_0000_0000#")))

(test character
  (macrolet ((frob (x y) `(is (equal ',x (vhdl-parse 'vhdl-char ,y)))))
    (frob #\A "'A'")
    (frob #\z "'z'")
    (frob #\, "','")
    (frob #\' "'''")
    (frob #\space "' '")))
  
(test string
  (macrolet ((frob (x y) `(is (equal ',x (vhdl-parse 'vhdl-string ,y)))))
    (frob "A string" "\"A string\"")
    (frob "A string can include any printing characters (e.g., &%@^*)."
	  "\"A string can include any printing characters (e.g., &%@^*).\"")
    (frob "00001111ZZZZ" "\"00001111ZZZZ\"")
    (frob "" "\"\"")
    (frob "A string in a string: \"A string\". "
	  "\"A string in a string: \"\"A string\"\". \"")))
	  

(test simple-binary-string
  (macrolet ((frob (x y) `(is (equal ',x (vhdl-parse 'simple-binary-string ,y)))))
    (frob (:bin "011111010") "O\"372\"")
    (frob (:bin "000000") "o\"00\"")
    (frob (:bin "11111010") "X\"FA\"")
    (frob (:bin "00001101") "x\"0d\"")))

(test convolved-binary-string-unknown
  (macrolet ((frob (x y) `(is (equal ',x (vhdl-parse 'convolved-binary-string ,y)))))
    (frob (:bin "011XXXZZZ100") "O\"3XZ4\"")
    (frob (:bin "10100011--------") "X\"A3--\"")
    (frob (:bin "0000####????1111") "X\"0#?F\"")
    (frob (:bin "00UU") "B\"00UU\"")
    (frob (:bin "011XXX") "O\"3_X\"")
    (signals (error) (vhdl-parse 'convolved-binary-string "D\"23Z9\""))))

(test convolved-binary-string-explicit-length
  (macrolet ((frob (x y) `(is (equal ',x (vhdl-parse 'convolved-binary-string ,y)))))
    (frob (:bin "0111100") "7X\"3C\"")
    (frob (:bin "00000101") "8O\"5\"")
    (frob (:bin "000000000X") "10B\"X\"")
    (signals (error) (vhdl-parse 'convolved-binary-string "8X\"90\"F"))))

(test convolved-binary-string-signed
  (macrolet ((frob (x y) `(is (equal ',x (vhdl-parse 'convolved-binary-string ,y)))))
    (frob (:bin "0001110001") "10SX\"71\"")
    (frob (:bin "1110001000") "10SX\"88\"")
    (frob (:bin "WWWWWW0000") "10SX\"W0\"")
    (frob (:bin "010110") "6SX\"16\"")
    (frob (:bin "101000") "6SX\"E8\"")
    (frob (:bin "HH0011") "6SX\"H3\"")
    (signals (error) (vhdl-parse 'convolved-binary-string "6SX\"28\""))
    (frob (:bin "101000") "6SX\"E8\"")))

(test ebnf
      (macrolet ((frob (x y) `(is (equal ',x (s-exp<-ebnf ',y)))))
	(frob (:entity (identifier) :is
		       (? :generic "(" (interface-list :generic) ")" ";")
		       (? :port "(" (interface-list :port) ")" ";")
		       (* (entity-declarative-item))
		       (? :begin
			  (* (|| (concurrent-assertion-statement)
				 (concurrent-procedure-call-statement :passive)
				 (process-statement :passive)
				 (new (psl-directive :psl)))))
		       :end (? :entity) (? (identifier)) ";")
	      ("ENTITY identifier IS"
	       "   [ GENERIC (( GENERIC-interface-list )) ; ]"
	       "   [ PORT (( PORT-interface-list )) ; ]"
	       "   { entity-declarative-item }"
	       "[ BEGIN"
	       "   { concurrent-assertion-statement"
	       "     | PASSIVE-concurrent-procedure-call-statement"
	       "     | PASSIVE-process-statement"
	       ;; I don't know if this line is actually correct
	       ;; and, furthermore, now it deviates from the book
	       "     | _PSL-psl-directive } ]"
	       "END [ ENTITY ] [ identifier ] ;"))
	(frob (|| (identifier)
		  (operator-symbol)
		  (character-literal)
		  (selected-name)
		  ((|| (name) (function-call)) "(" (+ "," (expression)) ")")
		  ((|| (name) (function-call)) "(" (discrete-range) ")") (attribute-name) (new (external-name)))
	      ("identifier | operator-symbol | character-literal | selected-name"
	       "| ( name | function-call ) (( expression {, ...} ))"
	       "| ( name | function-call ) (( discrete-range ))"
	       "| attribute-name | _external-name"))
	(frob ((shift-expression) (? (|| "=" "/=" "<" "<=" ">" ">=" "?="
					 (new "?=") (new "?/=") (new "?<") (new "?<=") (new "?>") (new "?>="))
				     (shift-expression)))
	      "shift-expression [ ( = | /= | < | <= | > | >= | ?= | _?= | _?/= | _?< | _?<= | _?> | _?>= ) shift-expression ]")
	(frob ((? :signal)
	       (+ "," (identifier)) ":" (? (mode)) (subtype-indication) (? :bus) (? ":=" (expression :static)))
	      ("[ SIGNAL ] identifier {, ... } : [ mode ] subtype-indication [ BUS ] [ := STATIC-expression ]"))))


(test case-insensitive-string
  (is (equal "asdf" (vhdl-parse '(esrap-liquid::descend-with-rule
				  'case-insensitive-string "asdf") "AsdF"))))

(defmacro with-optima-frob ((what) &body body)
  `(macrolet ((frob (x y) `(let ((expr (vhdl-parse ',',what ,y))
				 (theor ',x))
			     (match expr
			       (,x (pass))
			       (otherwise (fail "Expr: ~a didn't match a pattern ~a" expr theor))))))
     ,@body))
  

(test constant-decls
  (with-optima-frob (constant-declaration)
    (frob (list :constant 'integer _ 'cl-vhdl::number-of-bytes)
	  "constant number_of_bytes : integer := 4;")
    (frob (list :constant 'integer _ 'cl-vhdl::number-of-bits)
	  "constant number_of_bits : integer := 8 * number_of_bytes;")
    (frob (list :constant 'real _ 'cl-vhdl::e)
	  "constant e : real := 2.718281828;")
    (frob (list :constant 'integer _ 'cl-vhdl::size-limit 'cl-vhdl::count-limit)
	  "constant size_limit, count_limit : integer := 255 ;")
    (frob (list :constant 'time _ 'cl-vhdl::prop-delay)
    	  "constant prop_delay : time := 3 ns;")
    ))

(test variable-decls
  (with-optima-frob (variable-declaration)
    (frob (list :variable 'integer _ 'cl-vhdl::index) "variable index : integer := 0;")
    (frob (list :variable 'real _ 'cl-vhdl::sum 'cl-vhdl::average 'cl-vhdl::largest)
	  "variable sum, average, largest : real;")
    (frob (list :variable 'time _ 'cl-vhdl::start 'cl-vhdl::finish)
	  "variable start, finish : time := 0 ns;")
    ))

(test simple-variable-assignment
  (with-optima-frob (simple-variable-assignment)
    (frob (list ::= 'cl-vhdl::program-counter _) "program_counter := 0;")
    (frob (list ::= 'cl-vhdl::index _) "index := index + 1;")
    (frob (list ::= 'cl-vhdl::stored-value 'cl-vhdl::data-in) "stored_value := data_in;")
    ))
  

(test type-declaration
  (with-optima-frob (type-declaration)
    (frob (list :type 'cl-vhdl::apples (list :integer _ _))
	  "type apples is range 0 to 100;")
    (frob (list :type 'cl-vhdl::resistance
		(list :physical (list _ _) 'cl-vhdl::ohm
		      (list := 'cl-vhdl::kohm (list 'cl-vhdl::ohm 1000))
		      (list := 'cl-vhdl::mohm (list 'cl-vhdl::kohm 1000))))
	  "type resistance is range 0 to 1E9
               units
                   ohm;
                   kohm = 1000 ohm;
                   Mohm = 1000 kohm; -- probably later there will be problem with case-sensitivity
               end units resistance;")
    (frob (list :type 'cl-vhdl::alu-function
		(list :enum 'cl-vhdl::disable 'cl-vhdl::pass 'cl-vhdl::add 'cl-vhdl::subtract 'multiply
		      'cl-vhdl::divide))
	  "type alu_function is (disable, pass, add, subtract, multiply, divide);")
    (frob (list :type 'cl-vhdl::octal-digit (list :enum #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7))
	  "type octal_digit is ('0', '1', '2', '3', '4', '5', '6', '7');")
    ))

(test package-declaration
  (with-optima-frob (package-declaration)
    (frob (list :package 'cl-vhdl::int-types (list :type 'cl-vhdl::small-int
						   (list :integer  _ _)))
	  "package int_types is
               type small_int is range 0 to 255;
           end package int_types;")))

(test use-clause
  (with-optima-frob (use-clause)
    (frob (list :use (list :compound 'cl-vhdl::work (list :dot 'cl-vhdl::int-types) (list :dot :all)))
	  "use work.int_types.all;")
    (frob (list :use (list :compound 'cl-vhdl::ieee (list :dot 'cl-vhdl::std-logic-1164) (list :dot :all)))
	  "use ieee.std_logic_1164.all;")
    ))

(test type-definition
  (with-optima-frob (type-definition)
    (frob (list :physical (list _ _) 'cl-vhdl::ohm)
	  "range 0 to 1E9
                   units
                     ohm;
                   end units resistance")))

(test physical-literal
  (with-optima-frob (physical-literal)
    (frob (list 'cl-vhdl::ohm 1) "ohm")
    (frob (list 'cl-vhdl::ohm 2) "2 ohm")))

(test library-clause
  (with-optima-frob (library-clause)
    (frob (list :library 'cl-vhdl::ieee) "library ieee;")))

(test signal-declaration
  (with-optima-frob (signal-declaration)
    (frob (list :signal 'cl-vhdl::std-ulogic nil (list :bus t) 'cl-vhdl::cs1 'cl-vhdl::ncs2 'cl-vhdl::cs3)
	  "signal cs1, ncs2, cs3 : std_ulogic bus;")
    ))

(test if-statement
  (with-optima-frob (if-statement)
    (frob (list :cond (list _ :|...|))
	  "if cs1 and not cs2 and cs3 then
               ...
           end if;")
    (frob '(:cond ((:= cl-vhdl::mode cl-vhdl::immediate) (::= cl-vhdl::operand cl-vhdl::immed-operand))
	    ((:or (:= cl-vhdl::opcode cl-vhdl::load) (:= cl-vhdl::opcode cl-vhdl::add)
	      (:= cl-vhdl::opcode cl-vhdl::subtract))
	     (::= cl-vhdl::operand cl-vhdl::memory-operand))
	    (t (::= cl-vhdl::operand cl-vhdl::address-operand)))
	  "if mode = immediate then
               operand := immed_operand;
           elsif opcode = load or opcode = add or opcode = subtract then
               operand := memory_operand;
           else
               operand := address_operand;
           end if;")
    (frob (list :cond (list (list := 'cl-vhdl::opcode 'cl-vhdl::halt-opcode)
			    (list ::= 'cl-vhdl::pc 'cl-vhdl::effective-address)
			    (list ::= 'cl-vhdl::executing 'cl-vhdl::false)
			    _))
	  "if opcode = halt_opcode then
               PC := effective_address;
               executing := false;
               halt_indicator <= true;
           end if;")
    (frob (list :cond (list (list := 'cl-vhdl::phase 'cl-vhdl::wash)
			    (list :cond (list (list := 'cl-vhdl::cycle-select 'cl-vhdl::delicate-cycle)
					      _)
				  (list t (list _)))
			    _))
	  "if phase = wash then
               if cycle_select = delicate_cycle then
                   agitator_speed <= slow;
               else
                   agitator_speed <= fast;
               end if;
               agitator_on <= true;
           end if;")
    ))
  
(test sequential-statement
  (with-optima-frob (sequential-statement)
    (frob :|...| "...")
    (frob (list ::= 'cl-vhdl::stored-value 'cl-vhdl::data-in) "stored_value := data_in;")))

(test wait-statement
  (with-optima-frob (wait-statement)
    (frob (list :wait (list :until _))
	  "wait until clk;")))

(test subtype-declaration
  (with-optima-frob (subtype-declaration)
    (frob (list :subtype 'cl-vhdl::small-int 'cl-vhdl::integer (list :constraint (list :range _ _)))
	  "subtype small_int is integer range -128 to 127;")))

(test qualified-expression
  (with-optima-frob (qualified-expression)
    (frob (list :qualified _ 'cl-vhdl::logic-level)
	  "logic_level'(unknown)")))

(test attribute-name
  (with-optima-frob (attribute-name)
    (frob (list :compound 'cl-vhdl::logic-level (list :attribute 'cl-vhdl::value _))
	  "logic_level'value(\"Low\")")
    (frob (list :compound 'cl-vhdl::arith-op
		(list :attribute 'cl-vhdl::base)
		(list :attribute 'cl-vhdl::succ _))
	  "arith_op'base'succ(negate)")
    ))

(test shunting-yard
  (is (equal '(:+ (:* a b c) (:* d e f)) (cl-vhdl::shunting-yard '(a (* b) (* c) (+ d) (* e) (* f)) '(* +))))
  (is (equal '(:+ (:* a b c) (:* d e f) (:* g h i))
	     (cl-vhdl::shunting-yard '(a (* b) (* c) (+ d) (* e) (* f) (+ g) (* h) (* i))
				     '(* +))))
  (is (equal 'a
	     (cl-vhdl::shunting-yard '(a)
				     '(* +)))))
  
(test expression
  (with-optima-frob (primary)
    (frob 'cl-vhdl::a "a"))
  (with-optima-frob (unary-op)
    (frob (list :not 'cl-vhdl::a) "not a"))
  (with-optima-frob (factor)
    (frob (list :not 'cl-vhdl::a) "not a")
    (frob (list :abs 'cl-vhdl::b) "abs b")
    (frob (list :** 'cl-vhdl::a 'cl-vhdl::b) "a ** b"))
  (with-optima-frob (term)
    (frob (list :* 'cl-vhdl::a 'cl-vhdl::b 'cl-vhdl::c) "a * b * c")
    (frob (list :/ (list :* 'cl-vhdl::a 'cl-vhdl::b)
		'cl-vhdl::c
		(list :* 'cl-vhdl::d 'cl-vhdl::e))
	  "a * b / c / d * e")
    (frob (list :mod (list :* 'cl-vhdl::a 'cl-vhdl::b)
		(list :/ 'cl-vhdl::c 'cl-vhdl::d))
	  "a * b mod  c / d"))
  (with-optima-frob (simple-expression)
    (frob (list :- (list :+ (list :+ 'cl-vhdl::a) 'cl-vhdl::b)
		'cl-vhdl::c 'cl-vhdl::d)
	  "+ a + b - c - d")
    (frob (list :- (list :+ (list :+ 'cl-vhdl::a) (list :* 'cl-vhdl::b 'cl-vhdl::e))
		(list :rem 'cl-vhdl::c 'cl-vhdl::f) (list :not 'cl-vhdl::d))
	  "+ a + b * e - c rem f - not d"))
  (with-optima-frob (sub-shift-expression)
    (frob 'cl-vhdl::a "a")
    (frob (list :sll 'cl-vhdl::a 'cl-vhdl::b) "a sll b")
    (frob (list :sll (list :* 'cl-vhdl::a 'cl-vhdl::c) (list :+ 'cl-vhdl::b 'cl-vhdl::d))
	  "a*c sll b + d"))
  (with-optima-frob (shift-expression)
    (frob (list :* 'cl-vhdl::a 'cl-vhdl::c) "a*c"))
  (with-optima-frob (expression)
    (frob (list :and 'cl-vhdl::a 'cl-vhdl::b 'cl-vhdl::c) "a and b and c")
    (frob (list :and 'cl-vhdl::a (list :not 'cl-vhdl::b) 'cl-vhdl::c) "a and not b and c")
    (frob (list :and 'cl-vhdl::a (list :not 'cl-vhdl::b) (list := 'cl-vhdl::state 'cl-vhdl::idle))
	  "a and not b and state = idle")
    (frob (list :and (list := 'cl-vhdl::a #\1)
		(list := 'cl-vhdl::b #\0)
		(list := 'cl-vhdl::state 'cl-vhdl::idle))
	  "a = '1' and b = '0' and state = idle")
    ))
    
(test conditional-variable-assignment
  (with-optima-frob (conditional-variable-assignment)
    (frob '(::= cl-vhdl::result (:when ((:= cl-vhdl::mode cl-vhdl::subtract)
					(:- cl-vhdl::a cl-vhdl::b))
				  (t (:+ cl-vhdl::a cl-vhdl::b))))
	  "result := a - b when mode = subtract else a + b;")))

(test case-statement
  (with-optima-frob (case-statement)
    (frob (list :case 'cl-vhdl::func
		(list _ (list ::= 'cl-vhdl::result 'cl-vhdl::operand1))
		(list _ (list ::= 'cl-vhdl::result 'cl-vhdl::operand2))
		(list _ (list ::= 'cl-vhdl::result (list :+ 'cl-vhdl::operand1 'cl-vhdl::operand2)))
		(list _ (list ::= 'cl-vhdl::result (list :- 'cl-vhdl::operand1 'cl-vhdl::operand2))))
	  "case func is
               when pass1 =>
                   result := operand1;
               when pass2 =>
                   result := operand2;
               when add =>
                   result := operand1 + operand2;
               when subtract =>
                   result := operand1 - operand2;
           end case;")
    ))

(test choices
  (with-optima-frob (choices)
    (frob (list :|| _ _ _)
	  "load | add | subtract")
    (frob '(:to cl-vhdl::add cl-vhdl::load)
	  "add to load")
    (frob '(:downto cl-vhdl::branch cl-vhdl::store)
	  "branch downto store")
    (frob :others
	  "others")
    ))

(test selected-variable-assignment
  (with-optima-frob (selected-variable-assignment)
    (frob (list ::= 'cl-vhdl::result (list :select (list _ 'cl-vhdl::operand1)
					   (list _ 'cl-vhdl::operand2)
					   (list _ (list :+ 'cl-vhdl::operand1 'cl-vhdl::operand2))
					   (list _ (list :- 'cl-vhdl::operand1 'cl-vhdl::operand2))))
	  "with func select
               result := operand1            when pass1,
                         operand2            when pass2,
                         operand1 + operand2 when add,
                         operand1 - operand2 when subtract;")
    ))

(test null-statement (with-optima-frob (null-statement)
		       (frob :null "null ;")))

(test loop-statement
  (with-optima-frob (loop-statement)
    (frob (list :loop (list :wait (list :until 'cl-vhdl::clk))
		(list := 'cl-vhdl::count-value (list :mod _ 16))
		_)
	  "loop
               wait until clk;
               count_value := (count_value + 1) mod 16;
               count <= count_value;
           end loop;")
    (frob '(:loop (:while (:> cl-vhdl::index 0)) :|...|)
	  "while index > 0 loop
               ... -- statement A : do something with index
           end loop;")
    (frob (list :loop (list :for 'cl-vhdl::count-value :in (list :to 0 127))
		_
		(list :wait (list :for (list 'cl-vhdl::ns 5))))
	  "for count_value in 0 to 127 loop
               count_out <= count_value;
               wait for 5 ns;
           end loop;")
    ))

(test exit-statement
  (with-optima-frob (exit-statement)
    (frob '(:exit) "exit;")
    (frob '(:exit (:when cl-vhdl::reset)) "exit when reset;")
    (frob '(:exit cl-vhdl::outer (:when cl-vhdl::reset)) "exit outer when reset;")
    ))

(test next-statement
  (with-optima-frob (next-statement)
    (frob '(:next) "next;")
    (frob '(:next (:when cl-vhdl::reset)) "next when reset;")
    (frob '(:next cl-vhdl::outer (:when cl-vhdl::reset)) "next outer when reset;")
    ))

(test assert-statement
  (with-optima-frob (assertion-statement)
    (frob '(:assert (:<= cl-vhdl::initial-value cl-vhdl::max-value))
	  "assert initial_value <= max_value;")
    (frob '(:assert (:<= cl-vhdl::initial-value cl-vhdl::max-value)
	    (:report "initial value too large"))
	  "assert initial_value <= max_value
             report \"initial value too large\";")
    (frob '(:assert (:>= cl-vhdl::free-memory cl-vhdl::low-water-limit)
	    (:report "low on memory, about to start garbage collect")
	    (:severity cl-vhdl::note))
	  "assert free_memory >= low_water_limit
             report \"low on memory, about to start garbage collect\"
             severity note;")
    ))

(test report-statement
  (with-optima-frob (report-statement)
    (frob '(:report "low on memory, about to start garbage collect"
	    (:severity cl-vhdl::note))
	  "report \"low on memory, about to start garbage collect\"
             severity note;")
    ))

(test array-type-definition
  (with-optima-frob (array-type-definition)
    (frob '(:array cl-vhdl::bit (:to 0 31))
	  "array (0 to 31) of bit")
    (frob '(:array cl-vhdl::natural (:to cl-vhdl::idle cl-vhdl::error))
	  "array (idle to error) of natural")
    (frob '(:array cl-vhdl::natural (:subtype (cl-vhdl::controller-state (:constraint (:range cl-vhdl::idle
											      cl-vhdl::error)))))
	  "array (controller_state range idle to error) of natural")
    (frob '(:array cl-vhdl::real (:subtype cl-vhdl::coeff-ram-address))
	  "array (coeff_ram_address) of real")
    ))

(test simple-variable-assignment-2
  (with-optima-frob (simple-variable-assignment)
    (frob '(::= (:compound cl-vhdl::counters (:paren cl-vhdl::active)) (:+ (:compound cl-vhdl::counters
									    (:paren cl-vhdl::active))
									1))
	  "counters(active) := counters(active) + 1;")))
