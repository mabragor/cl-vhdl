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
    (frob '(:integer 0 100)
	  "range 0 to 100")
    (frob (list :physical (list _ _) 'cl-vhdl::ohm)
	  "range 0 to 1E9
                   units
                     ohm;
                   end units resistance")))

(test physical-literal
  (with-optima-frob (physical-literal)
    (signals (esrap-liquid::simple-esrap-error) (vhdl-parse 'physical-literal "ohm"))
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
    (frob '(:cond ((:= cl-vhdl::opcode cl-vhdl::halt-opcode)
		   (::= cl-vhdl::pc cl-vhdl::effective-address)
		   (::= cl-vhdl::executing cl-vhdl::false)
		   (:<= cl-vhdl::halt-indicator (:waveform (cl-vhdl::true)))))
	  "if opcode = halt_opcode then
               PC := effective_address;
               executing := false;
               halt_indicator <= true;
           end if;")
    (frob '(:cond ((:= cl-vhdl::phase cl-vhdl::wash)
		   (:cond ((:= cl-vhdl::cycle-select cl-vhdl::delicate-cycle)
			   (:<= cl-vhdl::agitator-speed (:waveform (cl-vhdl::slow))))
			  (t (:<= cl-vhdl::agitator-speed (:waveform (cl-vhdl::fast)))))
		   (:<= cl-vhdl::agitator-on (:waveform (cl-vhdl::true)))))
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
	  "wait until clk;")
    (frob '(:wait (:on cl-vhdl::a cl-vhdl::b))
	  "wait on a, b;")
    (frob '(:wait (:on cl-vhdl::clk) (:until (:not cl-vhdl::reset)))
	  "wait on clk until not reset;")
    (frob '(:wait (:until cl-vhdl::trigger) (:for (cl-vhdl::ms 1)))
	  "wait until trigger for 1 ms;")
    (frob '(:wait) "wait;")
    ))

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
    (frob '(:array cl-vhdl::natural (cl-vhdl::controller-state (:constraint (:range cl-vhdl::idle
										    cl-vhdl::error))))
	  "array (controller_state range idle to error) of natural")
    (frob '(:array cl-vhdl::real cl-vhdl::coeff-ram-address)
	  "array (coeff_ram_address) of real")
    (frob '(:array cl-vhdl::state cl-vhdl::state cl-vhdl::symbol)
	  "array (state, symbol) of state")
    ))

(test simple-variable-assignment-2
  (with-optima-frob (simple-variable-assignment)
    (frob (list ::= (list :compound 'cl-vhdl::counters
			  (list _ 'cl-vhdl::active))
		(list :+ (list :compound 'cl-vhdl::counters
			       (list _ 'cl-vhdl::active))
		      1))
	  "counters(active) := counters(active) + 1;"))
  (with-optima-frob (name)
    (frob (list :compound 'cl-vhdl::transition-table (list _ 5 #\d))
	  "transition_table(5, 'd')")
    ))

(test aggregate
  (with-optima-frob (aggregate)
    (frob '(:aggregate 0.0 0.0 0.0)
	  "(0.0, 0.0, 0.0)")
    (frob '(:aggregate (:=> 1 10.0) (:=> 2 20.0) (:=> 3 0.0))
	  "(1 => 10.0, 2 => 20.0, 3 => 0.0)")
    (frob '(:aggregate (:=> 0 1.6) (:=> 1 2.3) (:=> 2 1.6) (:=> (:to 3 63) 0.0))
	  "(0 => 1.6, 1 => 2.3, 2 => 1.6, 3 to 63 => 0.0)")
    (frob '(:aggregate (:=> 0 1.6) (:=> 1 2.3) (:=> 2 1.6) (:=> :others 0.0))
	  "(0 => 1.6, 1 => 2.3, 2 => 1.6, others => 0.0)")
    (frob '(:aggregate (:=> (:|| 0 2) 1.6) (:=> 1 2.3) (:=> :others 0.0))
	  "(0 | 2 => 1.6, 1 => 2.3, others => 0.0)")
    (frob '(:aggregate (:=> 0 (:aggregate (:=> #\a 1) (:=> :others 6)))
	    (:=> 1 (:aggregate (:=> #\t 2) (:=> :others 6)))
	    (:=> 2 (:aggregate (:=> #\d 3) (:=> #\h 5) (:=> :others 6))))
	  "( 0 => ('a' => 1, others => 6),
             1 => ('t' => 2, others => 6),
             2 => ('d' => 3, 'h' => 5, others => 6) )"))
  (with-optima-frob (simple-signal-assignment)
    (frob (list :<= (list :aggregate 'cl-vhdl::z-flag 'cl-vhdl::n-flag 'cl-vhdl::v-flag 'cl-vhdl::c-flag)
		_)
	  "( z_flag, n_flag, v_flag, c_flag ) <= flag_reg;")
    ))

(test unconstrained-arrays
  (with-optima-frob (type-declaration)
    (frob '(:type cl-vhdl::sample (:array cl-vhdl::integer (:<> cl-vhdl::natural)))
	  "type sample is array (natural range <>) of integer;"))
  (with-optima-frob (variable-declaration)
    (frob '(:variable (:compound cl-vhdl::sample (:to 0 63)) nil cl-vhdl::short-sample-buf)
	  "variable short_sample_buf : sample(0 to 63);")
    (frob '(:variable (:compound cl-vhdl::sample-set (:to 1 100) (:to 1 20)) nil cl-vhdl::main-sample-set)
	  "variable main_sample_set : sample_set(1 to 100)(1 to 20);")
    (frob (list :variable (list :compound 'cl-vhdl::dozen-samples _ (list :to 0 9)) nil 'cl-vhdl::bakers-samples)
	  "variable bakers_samples : dozen_samples(open)(0 to 9);")
    ))

(test matching-case
  (with-optima-frob (case-statement)
    (frob '(:case? cl-vhdl::request
	    ("1---" (:= cl-vhdl::grant "1000"))
	    ("01--" (:= cl-vhdl::grant "0100"))
	    ("001-" (:= cl-vhdl::grant "0010"))
	    ("0001" (:= cl-vhdl::grant "0001"))
	    (:others (:= cl-vhdl::grant "0000")))
	  "case? request is
               when \"1---\" => grant := \"1000\";
               when \"01--\" => grant := \"0100\";
               when \"001-\" => grant := \"0010\";
               when \"0001\" => grant := \"0001\";
               when others   => grant := \"0000\";
           end case?;")))

(test matching-select
  (with-optima-frob (selected-variable-assignment)
    (frob '(::= cl-vhdl::grant (:select? ("1---" "1000")
				("01--" "0100")
				("001-" "0010")
				("0001" "0001")
				(:others "0000")))
	  "with request select?
               grant := \"1000\" when \"1---\",
                        \"0100\" when \"01--\",
                        \"0010\" when \"001-\",
                        \"0001\" when \"0001\",
                        \"0000\" when others;")))

(test record
  (with-optima-frob (type-declaration)
    (frob '(:type cl-vhdl::time-stamp (:record ((cl-vhdl::integer (:constraint (:range 0 59))) cl-vhdl::seconds)
				       ((cl-vhdl::integer (:constraint (:range 0 59))) cl-vhdl::minutes)
				       ((cl-vhdl::integer (:constraint (:range 0 23))) cl-vhdl::hours)))
	  "type time_stamp is record
               seconds : integer range 0 to 59;
               minutes : integer range 0 to 59;
               hours   : integer range 0 to 23;
           end record time_stamp;"))
  (with-optima-frob (variable-declaration)
    (frob (list :variable (list :compound 'cl-vhdl::test-vector
				(list _ '(:compound cl-vhdl::stimulus (:to 0 7))
				      '(:compound cl-vhdl::response (:to 0 9))))
		nil 'cl-vhdl::next-test-vector)
	  "variable next_test_vector : test_vector(stimulus(0 to 7),
                                                   response(0 to 9));")
    ))

;; TODO : actually make the in-ports not to be constant -- I wonder how to do this at this level...
(test entity-declaration
  (with-optima-frob (entity-declaration)
    (frob '(:entity cl-vhdl::top-level)
	  "entity top_level is end entity top_level;")
    (frob '(:entity cl-vhdl::adder (:port (:sig-var-con cl-vhdl::word nil :in cl-vhdl::a)
				    (:sig-var-con cl-vhdl::word nil :in cl-vhdl::b)
				    (:sig-var-con cl-vhdl::word nil :out cl-vhdl::sum)))
	  "entity adder is
               port ( a : in word;
                      b : in word;
                      sum : out word );
           end entity adder;")
    (frob '(:entity cl-vhdl::program-rom
	    (:port (:sig-var-con (:compound cl-vhdl::std-ulogic-vector (:downto 14 0)) nil :in cl-vhdl::address)
	     (:sig-var-con (:compound cl-vhdl::std-ulogic-vector (:downto 7 0)) nil :out cl-vhdl::data)
	     (:sig-var-con cl-vhdl::std-ulogic nil :in cl-vhdl::enable))
	    (:subtype cl-vhdl::instruction-byte (:compound cl-vhdl::bit-vector (:downto 7 0)))
	    (:type cl-vhdl::program-array (:array cl-vhdl::instruction-byte (:to 0 (:- (:** 2 14) 1))))
	    (:constant cl-vhdl::program-array (:aggregate (:bin "00110010") (:bin "00111111")
					       (:bin "00000011") (:bin "01110001") (:bin "00100011"))
	     cl-vhdl::program))
	  "entity program_ROM is
               port ( address : in std_ulogic_vector(14 downto 0);
                      data : out std_ulogic_vector(7 downto 0);
                      enable : in std_ulogic );
               subtype instruction_byte is bit_vector(7 downto 0);
               type program_array is
                      array (0 to 2**14 - 1) of instruction_byte;
               constant program : program_array -- := X\"33\";
                 := ( X\"32\", X\"3F\", X\"03\",   -- LDA $3F03
                      X\"71\", X\"23\");            -- BLT $23
          end entity program_ROM;")
    ))

(test architecture-body
  (with-optima-frob (architecture-body)
    (frob (list :architecture 'cl-vhdl::abstract 'cl-vhdl::adder
		(list :label 'cl-vhdl::add-a-b
		      (list :process (list 'cl-vhdl::a 'cl-vhdl::b)
			    _)))
	  "architecture abstract of adder is
           begin
               add_a_b : process (a, b) is
               begin
                 sum <= a + b;
               end process add_a_b;
           end architecture abstract;")))

(test simple-signal-assignment
  (with-optima-frob (simple-signal-assignment)
    (frob '(:<= cl-vhdl::y (:waveform ((:not cl-vhdl::or-a-b) (:after (cl-vhdl::ns 5)))))
	  "y <= not or_a_b after 5 ns;")
    (frob '(:<= cl-vhdl::clk (:waveform (#\1 (:after cl-vhdl::t-pw)) (#\0 (:after (:* 2 cl-vhdl::t-pw)))))
	  "clk <= '1' after T_pw, '0' after 2*T_pw;")
    (frob '(:<= cl-vhdl::device-req (:waveform :unaffected))
	  "device_req <= unaffected;")
    ))

(test conditional-signal-assignment
  (with-optima-frob (conditional-signal-assignment)
    (frob '(:<= cl-vhdl::q (:when (cl-vhdl::reset (:waveform ((:aggregate (:=> :others #\0)))))
			     (t (:waveform (cl-vhdl::d)))))
	  "q <= (others => '0') when reset else d;")
    (frob '(:<= cl-vhdl::req (:when (cl-vhdl::fixed-delay-mode (:waveform (#\1) (#\0 (:after cl-vhdl::t-fixed))))
			       (t (:waveform (#\1) (#\0 (:after (:compound cl-vhdl::next-random-delay
									   (:paren cl-vhdl::ran-seed))))))))
	  "req <= '1', '0' after T_fixed when fixed_delay_mode else
                  '1', '0' after next_random_delay(ran_seed);")
    ))

(test selected-signal-assignment
  (with-optima-frob (selected-signal-assignment)
    (frob '(:<= cl-vhdl::q (:select cl-vhdl::d-sel ("00" (:waveform (cl-vhdl::source0)))
			    ("01" (:waveform (cl-vhdl::source1))) ("10" (:waveform (cl-vhdl::source2)))
			    ("11" (:waveform (cl-vhdl::source3)))))
	  "with d_sel select
             q <= source0 when \"00\",
                  source1 when \"01\",
                  source2 when \"10\",
                  source3 when \"11\";")
    (frob '(:<= cl-vhdl::grant (:select? cl-vhdl::request
				("1---" (:waveform ("1000"))) ("01--" (:waveform ("0100")))
				("001-" (:waveform ("0010"))) ("0001" (:waveform ("0001")))
				(:others (:waveform ("0000")))))
	  "with request select?
             grant <= \"1000\" when \"1---\",
                      \"0100\" when \"01--\",
                      \"0010\" when \"001-\",
                      \"0001\" when \"0001\",
                      \"0000\" when others;")
    ))

(test delayed-signal-assignments
  (with-optima-frob (signal-assignment-statement)
    (frob '(:<= cl-vhdl::line-out :transport (:waveform (cl-vhdl::line-in (:after (cl-vhdl::ps 500)))))
	  "line_out <= transport line_in after 500 ps;")
    (frob '(:<= cl-vhdl::y (:inertial (cl-vhdl::ns 2)) (:waveform ((:not cl-vhdl::a) (:after (cl-vhdl::ns 3)))))
	  "y <= reject 2 ns inertial not a after 3 ns;")
    ))

(test architecture-body-2
  (with-optima-frob (architecture-body)
    (frob '(:architecture cl-vhdl::rtl cl-vhdl::dff
	    (:label cl-vhdl::ff (:process (cl-vhdl::clk)
				 (:cond (cl-vhdl::clk (:<= cl-vhdl::q (:waveform (cl-vhdl::d)))))))
	    (:<= cl-vhdl::q-n (:waveform ((:not cl-vhdl::q)))))
	  "architecture rtl of Dff is
           begin
             ff : process (clk) is
             begin
               if clk then
                 q <= d;
               end if;
             end process ff;
             q_n <= not q;
           end architecture rtl;")
    ))
       
(test concurrent-conditional-signal-assignment
  (with-optima-frob (concurrent-signal-assignment-statement)
    (frob '(:label cl-vhdl::zmux
	    (:<= cl-vhdl::z
	     (:when
		 ((:and (:not cl-vhdl::sel1) (:not cl-vhdl::sel0)) (:waveform (cl-vhdl::d0)))
	       ((:and (:not cl-vhdl::sel1) cl-vhdl::sel0) (:waveform (cl-vhdl::d1)))
	       ((:and cl-vhdl::sel1 (:not cl-vhdl::sel0)) (:waveform (cl-vhdl::d2)))
	       ((:and cl-vhdl::sel1 cl-vhdl::sel0) (:waveform (cl-vhdl::d3))))))
	  "zmux : z <= d0 when not sel1 and not sel0 else
                       d1 when not sel1 and     sel0 else
                       d2 when     sel1 and not sel0 else
                       d3 when     sel1 and     sel0;")
    (frob '(:label cl-vhdl::zmux
	    (:<= cl-vhdl::z
	     (:when
		 ((:and (:not cl-vhdl::sel1) (:not cl-vhdl::sel0)) (:waveform (cl-vhdl::d0)))
	       ((:and (:not cl-vhdl::sel1) cl-vhdl::sel0) (:waveform (cl-vhdl::d1)))
	       ((:and cl-vhdl::sel1 (:not cl-vhdl::sel0)) (:waveform (cl-vhdl::d2)))
	       (t (:waveform (cl-vhdl::d3))))))
	  "zmux : z <= d0 when not sel1 and not sel0 else
                       d1 when not sel1 and     sel0 else
                       d2 when     sel1 and not sel0 else
                       d3;")
    ))

(test concurrent-selected-signal-assignment
  (with-optima-frob (concurrent-signal-assignment-statement)
    (frob '(:label cl-vhdl::alu (:<= cl-vhdl::result
				 (:select cl-vhdl::alu-function
				  ((:|| cl-vhdl::alu-add cl-vhdl::alu-add-unsigned)
				   (:waveform ((:+ cl-vhdl::a cl-vhdl::b) (:after cl-vhdl::tpd))))
				  ((:|| cl-vhdl::alu-sub cl-vhdl::alu-sub-unsigned)
				   (:waveform ((:- cl-vhdl::a cl-vhdl::b) (:after cl-vhdl::tpd))))
				  (cl-vhdl::alu-and
				   (:waveform ((:and cl-vhdl::a cl-vhdl::b) (:after cl-vhdl::tpd))))
				  (cl-vhdl::alu-or
				   (:waveform ((:or cl-vhdl::a cl-vhdl::b) (:after cl-vhdl::tpd))))
				  (cl-vhdl::alu-pass-a (:waveform (cl-vhdl::a (:after cl-vhdl::tpd)))))))
	  "alu : with alu_function select
                   result <= a + b after Tpd   when alu_add | alu_add_unsigned,
                             a - b after Tpd   when alu_sub | alu_sub_unsigned,
                             a and b after Tpd when alu_and,
                             a or b after Tpd  when alu_or,
                             a after Tpd       when alu_pass_a;")))

(test concurrent-assert
  (with-optima-frob (concurrent-assertion-statement)
    (frob '(:label cl-vhdl::check
	    (:assert (:not (:and cl-vhdl::s cl-vhdl::r))
	     (:report (:& "Incorrect use of S_R_flip_flop: " "s and r both '1'"))))
	  "check : assert not (s and r)
                     report \"Incorrect use of S_R_flip_flop: \" &
                            \"s and r both '1'\";")))

(test passive-processes
  (with-optima-frob (entity-declaration)
    (frob '(:entity cl-vhdl::s-r-flipflop
		   (:port (:sig-var-con bit nil :in cl-vhdl::s cl-vhdl::r)
			  (:sig-var-con bit nil :out cl-vhdl::q cl-vhdl::q-n))
		   (:label cl-vhdl::check
			   (:assert (:not (:and cl-vhdl::s cl-vhdl::r))
				    (:report (:& "Incorrect use of S_R_flip_flop: " "s and r both '1'")))))
	  "entity S_R_flipflop is
             port ( s, r : in bit; q, q_n : out bit );

           begin
             check : assert not (s and r)
                       report \"Incorrect use of S_R_flip_flop: \" &
                              \"s and r both '1'\";
           end entity S_R_flipflop;")))
                         
(test component-instantiation-statement
  (with-optima-frob (component-instantiation-statement)
    (frob '(:instance cl-vhdl::main-mem-controller
	    (:entity
	     (:compound cl-vhdl::work (:dot cl-vhdl::dram-controller)
	      (:paren cl-vhdl::fpld)))
	    (:port-map cl-vhdl::cpu-rd cl-vhdl::cpu-wr cl-vhdl::cpu-mem cl-vhdl::mem-ras
	     cl-vhdl::mem-cas cl-vhdl::mem-we cl-vhdl::cpu-rdy))
	  "main_mem_controller : entity work.DRAM_controller(fpld)
             port map ( cpu_rd, cpu_wr, cpu_mem, mem_ras, mem_cas, mem_we, cpu_rdy );")
    (frob '(:instance cl-vhdl::main-mem-controller
	    (:entity
	     (:compound cl-vhdl::work (:dot cl-vhdl::dram-controller)
	      (:paren cl-vhdl::fpld)))
	    (:port-map (:=> cl-vhdl::rd cl-vhdl::cpu-rd) (:=> cl-vhdl::wr cl-vhdl::cpu-wr)
	     (:=> cl-vhdl::mem cl-vhdl::cpu-mem) (:=> cl-vhdl::ready cl-vhdl::cpu-rdy)
	     (:=> cl-vhdl::ras cl-vhdl::mem-ras) (:=> cl-vhdl::cas cl-vhdl::mem-cas)
	     (:=> cl-vhdl::we cl-vhdl::mem-we)))
	  "main_mem_controller : entity work.DRAM_controller(fpld)
             port map ( rd => cpu_rd, wr => cpu_wr,
                        mem => cpu_mem, ready => cpu_rdy,
                        ras => mem_ras, cas => mem_cas, we => mem_we );")
    (frob '(:instance cl-vhdl::f-cell (:entity (:compound cl-vhdl::work (:dot cl-vhdl::and-or-inv)))
	    (:port-map (:=> cl-vhdl::a1 cl-vhdl::a) (:=> cl-vhdl::a2 cl-vhdl::b)
	     (:=> cl-vhdl::b1 cl-vhdl::c) (:=> cl-vhdl::b2 :open)
	     (:=> cl-vhdl::y cl-vhdl::f)))
	  "f_cell : entity work.and_or_inv
             port map ( a1 => A, a2 => B, b1 => C, b2 => open, y => F );")
    ))

(test context-declaration
  (with-optima-frob (context-declaration)
    (frob '(:context cl-vhdl::widget-context (:library cl-vhdl::ieee)
	    (:use (:compound cl-vhdl::ieee (:dot cl-vhdl::std-logic-1164) (:dot :all)))
	    (:use (:compound cl-vhdl::widget-lib (:dot :all))))
	  "context widget_context is
             library ieee;
             use ieee.std_logic_1164.all;
             use widget_lib.all;
           end context widget_context;")))

(test context-reference
  (with-optima-frob (context-reference)
    (frob '(:context (:compound cl-vhdl::widget-lib (:dot cl-vhdl::widget-context)))
	  "context widget_lib.widget_context;")))

(test procedure-specification
  (with-optima-frob (procedure-specification)
    (frob '(:procedure cl-vhdl::average-samples (:parameter))
	  "procedure average_samples")))
	  
(test subprogram-declarative-item
  (with-optima-frob (subprogram-declarative-item)
    (frob '(:variable cl-vhdl::real 0.0 cl-vhdl::total)
	  "variable total : real := 0.0;")))

(test sequential-statement-2
  (with-optima-frob (sequential-statement)
    (frob '(:assert (:> (:compound cl-vhdl::samples (:attribute cl-vhdl::length)) 0) (:severity cl-vhdl::failure))
	  "assert samples'length > 0 severity failure;")
    (frob '(::= cl-vhdl::average
	    (:/ cl-vhdl::total
	     (:compound real (:paren (:compound cl-vhdl::samples (:attribute length))))))
	  "average := total / real(samples'length);"))
  (with-optima-frob (loop-statement)
    (frob '(:loop (:for cl-vhdl::index :in (:compound cl-vhdl::samples (:attribute cl-vhdl::range)))
	    (:= cl-vhdl::total
	     (:+ cl-vhdl::total (:compound cl-vhdl::samples (:paren cl-vhdl::index)))))
	  "for index in samples'range loop
               total := total + samples(index);
           end loop;")
    ))

(test return-statement
  (with-optima-frob (return-statement)
    (frob '(:return) "return;")))

(test operator-symbol
  (with-optima-frob (operator-symbol)
    (frob "+" "\"+\"")))

(test subprogram-body
  (with-optima-frob (subprogram-body)
    (frob '(:procedure cl-vhdl::average-samples (:parameter)
	    (:variable real 0.0 cl-vhdl::total)
	    (:assert (:> (:compound cl-vhdl::samples (:attribute length)) 0)
	     (:severity cl-vhdl::failure))
	    (:loop (:for cl-vhdl::index :in (:compound cl-vhdl::samples (:attribute cl-vhdl::range)))
	     (:= cl-vhdl::total (:+ cl-vhdl::total (:compound cl-vhdl::samples (:paren cl-vhdl::index)))))
	    (:= cl-vhdl::average
	     (:/ cl-vhdl::total
	      (:compound real (:paren (:compound cl-vhdl::samples (:attribute length)))))))
	  "procedure average_samples is
             variable total : real := 0.0;
           begin
             assert samples'length > 0 severity failure;
             for index in samples'range loop
               total := total + samples(index);
             end loop;
             average := total / real(samples'length);
           end procedure average_samples;")
    (frob '(:procedure cl-vhdl::do-arith-op
	    (:parameter (:sig-var-con cl-vhdl::func-code nil :in cl-vhdl::op))
	    (:variable integer nil cl-vhdl::result)
	    (:case cl-vhdl::op
	      (cl-vhdl::add (:= cl-vhdl::result (:+ cl-vhdl::op1 cl-vhdl::op2)))
	      (cl-vhdl::subtract (:= cl-vhdl::result (:- cl-vhdl::op1 cl-vhdl::op2))))
	    (:<= cl-vhdl::dest (:waveform (cl-vhdl::result (:after cl-vhdl::tpd))))
	    (:<= cl-vhdl::z-flag (:waveform ((:= cl-vhdl::result 0) (:after cl-vhdl::tpd)))))
	  "procedure do_arith_op ( op : in func_code ) is
             variable result : integer;
           begin
             case op is
               when add =>
                 result := op1 + op2;
               when subtract =>
                 result := op1 - op2;
             end case;
             dest <= result after Tpd;
             Z_flag <= result = 0 after Tpd;
           end procedure do_arith_op;")
    (frob '(:function cl-vhdl::limit (:parameter (:sig-var-con integer nil cl-vhdl::value min max))
	    (:return-type integer)
	    (:cond ((:> cl-vhdl::value max) (:return max))
		   ((:< cl-vhdl::value min) (:return min))
		   (t (:return cl-vhdl::value))))
	  "function limit ( value, min, max : integer ) return integer is
           begin
             if value > max then
               return max;
             elsif value < min then
               return min;
             else
               return value;
             end if;
           end function limit;")
    (frob '(:function "+" (:parameter (:sig-var-con bit-vector nil :in cl-vhdl::left cl-vhdl::right))
	    (:return-type bit-vector) :|...|)
	  "function \"+\" ( left, right : in bit_vector ) return bit_vector is
           begin
             ...
           end function \"+\";")
    ))

(test package-declaration-2
  (with-optima-frob (package-declaration)
    (frob '(:package cl-vhdl::cpu-types
	    (:constant cl-vhdl::positive 16 cl-vhdl::word-size)
	    (:constant cl-vhdl::positive 24 cl-vhdl::address-size)
	    (:subtype cl-vhdl::word (:compound bit-vector (:downto (:- cl-vhdl::word-size 1) 0)))
	    (:subtype cl-vhdl::address (:compound bit-vector (:downto (:- cl-vhdl::address-size 1) 0)))
	    (:type cl-vhdl::status-value
	     (:enum cl-vhdl::halted cl-vhdl::idle cl-vhdl::fetch cl-vhdl::mem-read
	      cl-vhdl::mem-write cl-vhdl::io-read cl-vhdl::io-write cl-vhdl::int-ack)))
	  "package cpu_types is
             constant word_size : positive := 16;
             constant address_size : positive := 24;

             subtype word is bit_vector(word_size - 1 downto 0);
             subtype address is bit_vector(address_size - 1 downto 0);

             type status_value is ( halted, idle, fetch, 
                                    mem_read, mem_write,
                                    io_read, io_write, int_ack );
           end package cpu_types;")
    ))

(test subprogram-declaration
  (with-optima-frob (package-body-declarative-item)
    (frob '(:function "+" (:parameter (:sig-var-con cl-vhdl::bit-vector nil cl-vhdl::bv1 cl-vhdl::bv2))
	    (:return-type cl-vhdl::bit-vector))
	  "function \"+\" ( bv1, bv2 : bit_vector )
             return bit_vector;")
    (frob '(:function "-" (:parameter (:sig-var-con cl-vhdl::bit-vector nil cl-vhdl::bv))
	    (:return-type cl-vhdl::bit-vector))
	  "function \"-\" ( bv : bit_vector )
             return bit_vector;")
    (frob '(:function cl-vhdl::mult-unsigned (:parameter (:sig-var-con bit-vector nil cl-vhdl::bv1 cl-vhdl::bv2))
	    (:return-type bit-vector) :|...| :|...|)
	  "function mult_unsigned ( bv1 , bv2 : bit_vector )
             return bit_vector is ...
           begin
             ...
           end function mult_unsigned;")
    ))

	  
(test package-body
  (with-optima-frob (package-body)
    (frob '(:package-body cl-vhdl::bit-vector-signed-arithmetic
	    (:function "+" (:parameter (:sig-var-con bit-vector nil cl-vhdl::bv1 cl-vhdl::bv2))
	     (:return-type bit-vector))
	    (:function "-" (:parameter (:sig-var-con bit-vector nil cl-vhdl::bv))
	     (:return-type bit-vector))
	    (:function cl-vhdl::mult-unsigned (:parameter (:sig-var-con bit-vector nil cl-vhdl::bv1 cl-vhdl::bv2))
	     (:return-type bit-vector) :|...| :|...|)
	    (:function "*" (:parameter (:sig-var-con bit-vector nil cl-vhdl::bv1 cl-vhdl::bv2))
	     (:return-type bit-vector)
	     (:cond ((:and (:not (:compound cl-vhdl::bv1 (:paren (:compound cl-vhdl::bv1
									    (:attribute cl-vhdl::left)))))
			   (:not (:compound cl-vhdl::bv2 (:paren (:compound cl-vhdl::bv2
									    (:attribute cl-vhdl::left))))))
		     (:return (:compound cl-vhdl::mult-unsigned (:paren cl-vhdl::bv1 cl-vhdl::bv2))))
		    ((:and (:not (:compound cl-vhdl::bv1 (:paren (:compound cl-vhdl::bv1
									    (:attribute cl-vhdl::left)))))
			   (:compound cl-vhdl::bv2 (:paren (:compound cl-vhdl::bv2 (:attribute cl-vhdl::left)))))
		     (:return (:- (:compound cl-vhdl::mult-unsigned (:paren cl-vhdl::bv1 (:- cl-vhdl::bv2))))))
		    ((:and (:compound cl-vhdl::bv1 (:paren (:compound cl-vhdl::bv1 (:attribute cl-vhdl::left))))
			   (:not (:compound cl-vhdl::bv2 (:paren (:compound cl-vhdl::bv2
									    (:attribute cl-vhdl::left))))))
		     (:return (:- (:- cl-vhdl::mult) (:compound cl-vhdl::unsigned
								(:paren (:- cl-vhdl::bv1) cl-vhdl::bv2)))))
		    (t (:return (:compound cl-vhdl::mult-unsigned (:paren (:- cl-vhdl::bv1) (:- cl-vhdl::bv2)))))))
	    :|...|)
	  "package body bit_vector_signed_arithmetic is

             function \"+\" ( bv1, bv2 : bit_vector )
               return bit_vector;

             function \"-\" ( bv : bit_vector )
               return bit_vector;

             function mult_unsigned ( bv1 , bv2 : bit_vector )
               return bit_vector is
               ...
             begin
               ...
             end function mult_unsigned;

             function \"*\" ( bv1, bv2 : bit_vector )
               return bit_vector is
             begin
               if not bv1(bv1'left) and not bv2(bv2'left) then
                 return mult_unsigned(bv1, bv2);
               elsif not bv1(bv1'left) and bv2(bv2'left) then
                 return -mult_unsigned(bv1, -bv2);
               elsif bv1(bv1'left) and not bv2(bv2'left) then
                 return -mult-unsigned(-bv1, bv2);
               else
                 return mult_unsigned(-bv1, -bv2);
               end if;
             end function \"*\";

             ...

           end package body bit_vector_signed_arithmetic;"))
  (with-optima-frob (process-statement)
    (frob '(:label cl-vhdl::stim-gen (:process nil
				      (:package cl-vhdl::id-manager
				       (:impure-function cl-vhdl::get-id (:parameter)
							 (:return-type cl-vhdl::natural)))
				      :|...|))
	  "stim_gen : process is

             package ID_manager is
               impure function get_ID return natural;
             end package ID_manager;

           begin
             ...
           end process stim_gen;")
    ))

(test signal-declaration-2
  (with-optima-frob (signal-declaration)
    (frob '(:signal (cl-vhdl::tri-state-logic (:resolution cl-vhdl::resolve-tri-state-logic)) nil
	    cl-vhdl::s1)
	  "signal s1 : resolve_tri_state_logic tri_state_logic;")
    ))

(test resolution-indication
  (with-optima-frob (resolution-indication)
    (frob 'cl-vhdl::asdf
	  "asdf")
    (frob '(:sub cl-vhdl::resolve-mvl4)
	  "(resolve_MVL4)")
    (frob '(:sub (:sub cl-vhdl::resolve-mvl4))
	  "((resolve_MVL4))")
    (frob '(:fields (cl-vhdl::valid cl-vhdl::wired-and))
	  "(valid wired_and)")
    (frob '(:fields (cl-vhdl::valid cl-vhdl::wired-and)
	    (cl-vhdl::dirty cl-vhdl::wired-or))
	  "(valid wired_and, dirty wired_or)")
    (frob '(:fields (cl-vhdl::tag (:sub cl-vhdl::resolve-mvl4)))
	  "(tag(resolve_mvl4))")
    (frob '(:fields (cl-vhdl::tag (:sub cl-vhdl::resolve-mvl4))
	    (cl-vhdl::valid cl-vhdl::wired-and)
	    (cl-vhdl::dirty cl-vhdl::wired-or))
	  "( tag(resolve_mvl4),
             valid wired_and,
             dirty wired_or )")
    ))
	  

(test subtype-declaration-2
  (with-optima-frob (subtype-declaration)
    (frob '(:subtype cl-vhdl::mvl4-logic-vector cl-vhdl::std-ulogic-vector
	    (:resolution (:sub cl-vhdl::resolve-mvl4)))
	  "subtype MVL4_logic_vector is (resolve_MVL4) std_ulogic_vector;")
    ))

(test case-study-mac-multiplier
  (with-optima-frob (design-unit)
    (frob '(:design-unit (:library cl-vhdl::ieee)
	    (:use (:compound cl-vhdl::ieee (:dot cl-vhdl::std-logic-1164) (:dot :all))
	     (:compound cl-vhdl::ieee (:dot cl-vhdl::fixed-pkg) (:dot :all)))
	    (:entity cl-vhdl::mac
	     (:port (:sig-var-con cl-vhdl::std-ulogic nil :in cl-vhdl::clk cl-vhdl::reset)
	      (:sig-var-con (:compound cl-vhdl::u-sfixed (:downto 0 (:- 15))) nil :in
			    cl-vhdl::x-real)
	      (:sig-var-con (:compound cl-vhdl::u-sfixed (:downto 0 (:- 15))) nil :in
			    cl-vhdl::x-imag)
	      (:sig-var-con (:compound cl-vhdl::u-sfixed (:downto 0 (:- 15))) nil :in
			    cl-vhdl::y-real)
	      (:sig-var-con (:compound cl-vhdl::u-sfixed (:downto 0 (:- 15))) nil :in
			    cl-vhdl::y-imag)
	      (:sig-var-con (:compound cl-vhdl::u-sfixed (:downto 0 (:- 15))) nil :out
			    cl-vhdl::s-real)
	      (:sig-var-con (:compound cl-vhdl::u-sfixed (:downto 0 (:- 15))) nil :out
			    cl-vhdl::s-imag)
	      (:sig-var-con cl-vhdl::std-ulogic nil :out cl-vhdl::ovf))))
 "
library ieee;

use ieee.std_logic_1164.all, ieee.fixed_pkg.all;

entity mac is
  port ( clk, reset : in std_ulogic;
         x_real : in u_sfixed(0 downto -15);
         x_imag : in u_sfixed(0 downto -15);
         y_real : in u_sfixed(0 downto -15);
         y_imag : in u_sfixed(0 downto -15);
         s_real : out u_sfixed(0 downto -15);
         s_imag : out u_sfixed(0 downto -15);
         ovf : out std_ulogic );
end entity mac;"))
  (with-optima-frob (use-clause)
    (frob '(:use (:compound cl-vhdl::ieee (:dot cl-vhdl::math-complex) (:dot :all)))
	  "use ieee.math_complex.all;"))
  (with-optima-frob (block-declarative-item)
    (frob '(:signal cl-vhdl::complex nil cl-vhdl::x-complex cl-vhdl::y-complex cl-vhdl::s-complex)
	  "signal x_complex, y_complex, s_complex : complex;"))
  (with-optima-frob (concurrent-statement)
    (frob '(:<= cl-vhdl::x-complex (:waveform ((:aggregate (:compound cl-vhdl::to-real (:paren cl-vhdl::x-real))
							   (:compound cl-vhdl::to-real (:paren cl-vhdl::x-imag))))))
	  "x_complex <= ( to_real(x_real), to_real(x_imag) );")
    (frob '(:<= cl-vhdl::y-complex (:waveform ((:aggregate (:compound cl-vhdl::to-real (:paren cl-vhdl::y-real))
							   (:compound cl-vhdl::to-real (:paren cl-vhdl::y-imag))))))
	  "y_complex <= ( to_real(y_real), to_real(y_imag) );")
    (frob '(:label cl-vhdl::behavior
	    (:process (cl-vhdl::clk)
	     (:variable complex (:aggregate 0.0 0.0) cl-vhdl::input-x cl-vhdl::input-y)
	     (:variable real 0.0 cl-vhdl::real-part-product-1 cl-vhdl::real-part-product-2
	      cl-vhdl::imag-part-product-1 cl-vhdl::imag-part-product-2)
	     (:variable complex (:aggregate 0.0 0.0) cl-vhdl::product sum)
	     (:variable boolean cl-vhdl::false cl-vhdl::real-accumulator-ovf
	      cl-vhdl::imag-accumulator-ovf)
	     (:cond
	       ((:compound cl-vhdl::rising-edge (:paren cl-vhdl::clk))
		(:cond
		  (cl-vhdl::reset (:= sum (:aggregate 0.0 0.0))
				  (:= cl-vhdl::real-accumulator-ovf cl-vhdl::false)
				  (:= cl-vhdl::imag-accumulator-ovf cl-vhdl::false))
		  (t (:= sum (:+ cl-vhdl::product sum))
		     (:= cl-vhdl::real-accumulator-ovf
			 (:or cl-vhdl::real-accumulator-ovf
			      (:< (:compound sum (:dot cl-vhdl::re)) (:- 16.0))
			      (:>= (:compound sum (:dot cl-vhdl::re)) (:+ 16.0))))
		     (:= cl-vhdl::imag-accumulator-ovf
			 (:or cl-vhdl::imag-accumulator-ovf
			      (:< (:compound sum (:dot cl-vhdl::im)) (:- 16.0))
			      (:>= (:compound sum (:dot cl-vhdl::im)) (:+ 16.0))))))
		(:<= cl-vhdl::s-complex (:waveform (sum)))
		(:<= cl-vhdl::ovf
		     (:when
			 ((:or cl-vhdl::real-accumulator-ovf cl-vhdl::imag-accumulator-ovf
			       (:< (:compound sum (:dot cl-vhdl::re)) (:- 1.0))
			       (:>= (:compound sum (:dot cl-vhdl::re)) (:+ 1.0))
			       (:< (:compound sum (:dot cl-vhdl::im)) (:- 1.0))
			       (:>= (:compound sum (:dot cl-vhdl::im)) (:+ 1.0)))
			  (:waveform (#\1)))
		       (t (:waveform (#\0)))))
		(:= (:compound cl-vhdl::product (:dot cl-vhdl::re))
		    (:- cl-vhdl::real-part-product-1 cl-vhdl::real-part-product-2))
		(:= (:compound cl-vhdl::product (:dot cl-vhdl::im))
		    (:+ cl-vhdl::imag-part-product-1 cl-vhdl::imag-part-product-2))
		(:= cl-vhdl::real-part-product-1
		    (:* (:compound cl-vhdl::input-x (:dot cl-vhdl::re))
			(:compound cl-vhdl::input-y (:dot cl-vhdl::re))))
		(:= cl-vhdl::real-part-product-2
		    (:* (:compound cl-vhdl::input-x (:dot cl-vhdl::im))
			(:compound cl-vhdl::input-y (:dot cl-vhdl::im))))
		(:= cl-vhdl::imag-part-product-1
		    (:* (:compound cl-vhdl::input-x (:dot cl-vhdl::re))
			(:compound cl-vhdl::input-y (:dot cl-vhdl::im))))
		(:= cl-vhdl::imag-part-product-2
		    (:* (:compound cl-vhdl::input-x (:dot cl-vhdl::im))
			(:compound cl-vhdl::input-y (:dot cl-vhdl::re))))
		(:= cl-vhdl::input-x cl-vhdl::x-complex)
		(:= cl-vhdl::input-y cl-vhdl::y-complex)))))
	  "
behavior : process (clk) is
    variable input_x, input_y : complex := (0.0, 0.0);
    variable real_part_product_1, real_part_product_2,
             imag_part_product_1, imag_part_product_2 : real := 0.0;
    variable product, sum : complex := (0.0, 0.0);
    variable real_accumulator_ovf,
             imag_accumulator_ovf : boolean := false;
  begin
    if rising_edge(clk) then
      if reset then
        sum := (0.0, 0.0);
        real_accumulator_ovf := false;
        imag_accumulator_ovf := false;
      else
        sum := product + sum;
        real_accumulator_ovf := real_accumulator_ovf 
                                or sum.re < -16.0
                                or sum.re >= +16.0;
        imag_accumulator_ovf := imag_accumulator_ovf 
                                or sum.im < -16.0
                                or sum.im >= +16.0;
      end if;
      s_complex <= sum;
      ovf <= '1' when ( real_accumulator_ovf or imag_accumulator_ovf
                        or sum.re < -1.0 or sum.re >= +1.0
                        or sum.im < -1.0 or sum.im >= +1.0 ) else '0';

      product.re := real_part_product_1 - real_part_product_2;
      product.im := imag_part_product_1 + imag_part_product_2;

      real_part_product_1 := input_x.re * input_y.re;
      real_part_product_2 := input_x.im * input_y.im;
      imag_part_product_1 := input_x.re * input_y.im;
      imag_part_product_2 := input_x.im * input_y.re;

      input_x := x_complex;
      input_y := y_complex;
    end if;
  end process behavior;"))
  (with-optima-frob (design-unit)
    (frob '(:design-unit (:use (:compound cl-vhdl::ieee (:dot cl-vhdl::math-complex) (:dot :all)))
	    (:architecture cl-vhdl::behavioral cl-vhdl::mac
	     (:signal complex nil cl-vhdl::x-complex cl-vhdl::y-complex
	      cl-vhdl::s-complex)
	     (:<= cl-vhdl::x-complex
	      (:waveform
	       ((:aggregate (:compound cl-vhdl::to-real (:paren cl-vhdl::x-real))
			    (:compound cl-vhdl::to-real (:paren cl-vhdl::x-imag))))))
	     (:label cl-vhdl::behavior
	      (:process (cl-vhdl::clk)
			(:variable complex (:aggregate 0.0 0.0) cl-vhdl::input-x cl-vhdl::input-y)
			(:cond
			  ((:compound cl-vhdl::rising-edge (:paren cl-vhdl::clk))
			   (nil cl-vhdl::asdf nil :|;|)))))
	     (:<= cl-vhdl::s-imag
	      (:waveform
	       ((:compound cl-vhdl::to-sfixed
			   (:paren (:compound cl-vhdl::s-complex (:dot cl-vhdl::im))
				   cl-vhdl::s-imag)))))))
	  "
use ieee.math_complex.all;

architecture behavioral of mac is
  signal x_complex, y_complex, s_complex : complex;
begin
  x_complex <= ( to_real(x_real), to_real(x_imag) );
  behavior : process (clk) is
    variable input_x, input_y : complex := (0.0, 0.0);
  begin
    if rising_edge(clk) then
      asdf;
    end if;
  end process behavior;
  s_imag <= to_sfixed(s_complex.im, s_imag);
end architecture behavioral;")))


(test alias-declaration
  (with-optima-frob (alias-declaration)
    (frob '(:alias cl-vhdl::alu-data-width (:compound cl-vhdl::work (:dot cl-vhdl::alu-types)
					    (:dot cl-vhdl::data-width)))
	  "alias alu_data_width is work.alu_types.data_width;")
    (frob '(:alias cl-vhdl::interrupt-level (:compound cl-vhdl::psw (:downto 30 26)))
	  "alias interrupt_level is PSW(30 downto 26);")
    (frob '(:alias cl-vhdl::interrupt-level (:compound cl-vhdl::psw (:downto 30 26))
	    (:subtype (:compound cl-vhdl::bit-vector (:downto 4 0))))
	  "alias interrupt_level : bit_vector(4 downto 0) is PSW(30 downto 26);")
    (frob '(:alias cl-vhdl::bv-increment (:compound cl-vhdl::work (:dot cl-vhdl::arithmetic-ops)
					  (:dot cl-vhdl::increment))
	    (:signature bit-vector integer))
	  "alias bv_increment is work.arithmetic_ops.increment [ bit_vector, integer ];")
    (frob '(:alias "*" "and" (:signature bit bit (:return bit)))
	  "alias \"*\" is \"and\" [ bit, bit return bit ];")
    ))

(test interface-subprogram-declaration
  (with-optima-frob (interface-subprogram-declaration)
    (frob '(:function cl-vhdl::increment (:parameter (:sig-var-con cl-vhdl::count-type nil cl-vhdl::x))
	    (:return-type cl-vhdl::count-type))
	  "function increment ( x : count_type )
                                          return count_type")
    (frob '(:function cl-vhdl::minimum (:parameter (:sig-var-con t nil cl-vhdl::l cl-vhdl::r))
	    (:return-type t) (:is :<>))
	  "function minimum ( L, R : T ) return T is <>")
    ))
	  
(test generic-entities
  (with-optima-frob (entity-declaration)
    (frob '(:entity cl-vhdl::and2 (:generic (:sig-var-con time nil cl-vhdl::tpd))
	    (:port (:sig-var-con bit nil :in cl-vhdl::a cl-vhdl::b)
	     (:sig-var-con bit nil :out cl-vhdl::y)))
	  "entity and2 is
             generic ( Tpd : time );
             port ( a, b : in bit; y : out bit );
           end entity and2;")
    (frob '(:entity cl-vhdl::generic-mux2 (:generic (:type cl-vhdl::data-type))
	    (:port (:sig-var-con bit nil :in cl-vhdl::sel)
	     (:sig-var-con cl-vhdl::data-type nil :in cl-vhdl::a cl-vhdl::b)
	     (:sig-var-con cl-vhdl::data-type nil :out cl-vhdl::z)))
	  "entity generic_mux2 is
             generic ( type data_type );
             port    ( sel : in bit; a, b : in data_type;
                       z : out data_type );
           end entity generic_mux2;")
    (frob '(:entity cl-vhdl::generic-counter
	    (:generic (:type cl-vhdl::count-type)
	     (:constant cl-vhdl::count-type nil cl-vhdl::reset-value)
	     (:function cl-vhdl::increment
	      (:parameter (:sig-var-con cl-vhdl::count-type nil cl-vhdl::x))
	      (:return-type cl-vhdl::count-type)))
	    (:port (:sig-var-con bit nil :in cl-vhdl::clk cl-vhdl::reset)
	     (:sig-var-con cl-vhdl::count-type nil :out cl-vhdl::data)))
	  "entity generic_counter is
             generic ( type count_type;
                       constant reset_value : count_type;
                       function increment ( x : count_type )
                                          return count_type );
             port ( clk, reset : in bit;
                    data : out count_type );
           end entity generic_counter;"))
  (with-optima-frob (component-instantiation-statement)
    (frob '(:instance cl-vhdl::gate1
	    (:entity (:compound cl-vhdl::work (:dot cl-vhdl::and2) (:paren cl-vhdl::simple)))
	    (:generic-map (:=> cl-vhdl::tpd (cl-vhdl::ns 2)))
	    (:port-map (:=> cl-vhdl::a cl-vhdl::sig1) (:=> cl-vhdl::b cl-vhdl::sig2)
	     (:=> cl-vhdl::y cl-vhdl::sig-out)))
	  "gate1 : entity work.and2(simple)
             generic map ( Tpd => 2 ns )
             port map ( a => sig1, b => sig2, y => sig_out );")
    ))

(test generic-packages
  (with-optima-frob (package-declaration)
    (frob '(:package cl-vhdl::generic-stacks (:generic (:sig-var-con cl-vhdl::positive nil cl-vhdl::size)
					      (:type cl-vhdl::element-type))
	    (:type cl-vhdl::stack-array
	     (:array cl-vhdl::element-type (:to 0 (:- cl-vhdl::size 1)))))
	  "package generic_stacks is
             generic ( size : positive; type element_type );
             type stack_array is array (0 to size-1) of element_type;
           end package generic_stacks;"))
  (with-optima-frob (package-instantiation-declaration)
    (frob '(:new-package cl-vhdl::address-stacks (:compound cl-vhdl::work (:dot cl-vhdl::generic-stacks))
	    (:=> cl-vhdl::size 8) (:=> cl-vhdl::element-type (:compound cl-vhdl::unsigned (:downto 23 0))))
	  "package address_stacks is new work.generic_stacks
             generic map ( size => 8, element_type => unsigned(23 downto 0) );")
    ))

(test generic-subprograms
  (with-optima-frob (subprogram-body)
    (frob '(:procedure cl-vhdl::swap (:generic (:type t))
	    (:parameter (:sig-var-con t nil :inout cl-vhdl::a cl-vhdl::b))
	    (:variable t nil cl-vhdl::temp) (:= cl-vhdl::temp cl-vhdl::a)
	    (:= cl-vhdl::a cl-vhdl::b) (:= cl-vhdl::b cl-vhdl::temp))
	  "procedure swap
             generic (type T)
             parameter ( a, b : inout T ) is
             variable temp : T;
           begin
             temp := a; a := b; b := temp;
           end procedure swap;"))
  (with-optima-frob (subprogram-instantiation-declaration)
    (frob '(:new-procedure cl-vhdl::int-swap cl-vhdl::swap (:=> t integer))
	  "procedure int_swap is new swap
             generic map ( T => integer );")
    (frob '(:new-procedure cl-vhdl::combine-vec-with-bit cl-vhdl::combine
	    (:signature (t bit)) (:=> t bit-vector))
	  "procedure combine_vec_with_bit is new combine[T, bit]
             generic map ( T => bit_vector );")
    ))

(test component-declaration
  (with-optima-frob (component-declaration)
    (frob '(:component cl-vhdl::flipflop
	    (:generic (:sig-var-con cl-vhdl::delay-length nil cl-vhdl::tprop cl-vhdl::tsetup cl-vhdl::thold))
	    (:port (:sig-var-con bit nil :in cl-vhdl::clk)
	     (:sig-var-con bit nil :in cl-vhdl::clr) (:sig-var-con bit nil :in cl-vhdl::d)
	     (:sig-var-con bit nil :out cl-vhdl::q)))
	  "component flipflop is
             generic ( Tprop, Tsetup, Thold : delay_length );
             port ( clk : in bit; clr : in bit; d : in bit;
                    q : out bit );
           end component flipflop;"))
  (with-optima-frob (component-instantiation-statement)
    (frob '(:instance cl-vhdl::bit0 (:component cl-vhdl::flipflop)
	    (:generic-map (:=> cl-vhdl::tprop (cl-vhdl::ns 2))
	     (:=> cl-vhdl::tsetup (cl-vhdl::ns 2)) (:=> cl-vhdl::thold (cl-vhdl::ns 1)))
	    (:port-map (:=> cl-vhdl::clk cl-vhdl::clk) (:=> cl-vhdl::clr cl-vhdl::clr)
	     (:=> cl-vhdl::d (:compound cl-vhdl::d (:paren 0)))
	     (:=> cl-vhdl::q (:compound cl-vhdl::q (:paren 0)))))
	  "bit0 : component flipflop
               generic map ( Tprop => 2 ns, Tsetup => 2 ns, Thold => 1 ns )
               port map ( clk => clk, clr => clr, d => d(0), q => q(0) );")
    ))

(test binding-for
  (with-optima-frob (component-configuration)
    (frob '(:for (cl-vhdl::flipflop cl-vhdl::bit0 cl-vhdl::bit1)
	    (:use (:entity (:compound cl-vhdl::work (:dot cl-vhdl::edge-triggered-dff)
				      (:paren cl-vhdl::basic)))))
	  "for bit0, bit1 : flipflop
             use entity work.edge_triggered_Dff(basic);
           end for;")
    (frob '(:for (cl-vhdl::serial-interface :all) (:use :open))
	  "for all : serial_interface
             use open;
           end for;")
    ))

(test configuration-declaration
  (with-optima-frob (configuration-declaration)
    (frob '(:configuration cl-vhdl::reg4-gate-level cl-vhdl::reg4
	    (:for cl-vhdl::struct
	     (:for (cl-vhdl::flipflop cl-vhdl::bit0)
	      (:use (:entity (:compound cl-vhdl::edge-triggered-dff (:paren cl-vhdl::hi-fanout)))))
	     (:for (cl-vhdl::flipflop :others)
	      (:use (:entity (:compound cl-vhdl::edge-triggered-dff (:paren cl-vhdl::basic)))))))
	  "configuration reg4_gate_level of reg4 is
             for struct
                 for bit0 : flipflop
                     use entity edge_triggered_Dff(hi_fanout);
                 end for;

                 for others : flipflop
                     use entity edge_triggered_Dff(basic);
                 end for;
             end for;
           end configuration reg4_gate_level;")
    (frob '(:configuration cl-vhdl::full cl-vhdl::counter
	    (:for cl-vhdl::registered
	     (:for (cl-vhdl::digit-register :all)
	      (:use (:entity (:compound cl-vhdl::work (:dot cl-vhdl::reg4) (:paren cl-vhdl::struct))))
	      (:for cl-vhdl::struct
		    (:for (cl-vhdl::flipflop cl-vhdl::bit0)
			  (:use (:entity (:compound cl-vhdl::edge-triggered-dff (:paren cl-vhdl::hi-fanout)))))
		    (:for (cl-vhdl::flipflop :others)
			  (:use (:entity (:compound cl-vhdl::edge-triggered-dff (:paren cl-vhdl::basic)))))))))
	  "configuration full of counter is
             for registered
               for all : digit_register
                 use entity work.reg4(struct);

                 for struct
                   for bit0 : flipflop
                     use entity edge_triggered_Dff(hi_fanout);
                   end for;

                   for others : flipflop
                     use entity edge_triggered_Dff(basic);
                   end for;
                 end for;
               end for;
             end for;
           end configuration full;")
    ))

(test binding-indication
  (with-optima-frob (component-configuration)
    (frob '(:for (cl-vhdl::reg4 cl-vhdl::flag-reg)
	    (:use (:configuration (:compound cl-vhdl::work (:dot cl-vhdl::reg4-gate-level)))))
	  "for flag_reg : reg4
             use configuration work.reg4_gate_level;
           end for;")))

(test architecture-body-3
  (with-optima-frob (architecture-body)
    (frob '(:architecture cl-vhdl::top-level cl-vhdl::alarm-clock
	    (:use (:compound cl-vhdl::work (:dot cl-vhdl::counter-types) (:dot cl-vhdl::digit)))
	    (:signal bit nil cl-vhdl::reset-to-midnight cl-vhdl::seconds-clk)
	    (:signal cl-vhdl::digit nil cl-vhdl::seconds-units cl-vhdl::seconds-tens)
	    (:instance cl-vhdl::seconds
	     (:configuration (:compound cl-vhdl::work (:dot cl-vhdl::counter-down-to-gate-level)))
	     (:port-map (:=> cl-vhdl::clk cl-vhdl::seconds-clk)
	      (:=> cl-vhdl::clr cl-vhdl::reset-to-midnight)
	      (:=> cl-vhdl::q0 cl-vhdl::seconds-units)
	      (:=> cl-vhdl::q1 cl-vhdl::seconds-tens))))
	  "architecture top_level of alarm_clock is
             use work.counter_types.digit;
             signal reset_to_midnight, seconds_clk : bit;
             signal seconds_units, seconds_tens : digit;
             -- ...
           begin
             seconds : configuration work.counter_down_to_gate_level
               port map ( clk => seconds_clk, clr => reset_to_midnight,
                          q0 => seconds_units, q1 => seconds_tens );
             -- ...
           end architecture top_level;")))

(test generate
  (with-optima-frob (generate-statement)
    (frob '(:generate-for cl-vhdl::cell-array cl-vhdl::bit-index (:to 0 (:- cl-vhdl::width 1))
	    (:signal cl-vhdl::std-ulogic nil cl-vhdl::data-unbuffered)
	    (:instance cl-vhdl::cell-storage (:component cl-vhdl::d-flipflop)
	     (:port-map (:=> cl-vhdl::clk cl-vhdl::clock)
	      (:=> cl-vhdl::d (:compound cl-vhdl::data-in (:paren cl-vhdl::bit-index)))
	      (:=> cl-vhdl::q cl-vhdl::data-unbuffered))))
	  "cell_array : for bit_index in 0 to width - 1 generate
             signal data_unbuffered : std_ulogic;
           begin
             cell_storage : component D_flipflop
               port map ( clk => clock, d => data_in(bit_index),
                          q => data_unbuffered );
           end generate;")
    (frob '(:generate-if cl-vhdl::reg
	    (nil (:= cl-vhdl::index 0)
	     (:instance cl-vhdl::cell (:component cl-vhdl::master-slave-flipflop)
	      (:port-map cl-vhdl::phi1 cl-vhdl::phi2
			 (:=> cl-vhdl::d cl-vhdl::serial-data-in)
			 (:=> cl-vhdl::q
			      (:compound cl-vhdl::normalized-parallel-data
					 (:paren cl-vhdl::index))))))
	    (nil t (:instance cl-vhdl::cell (:component cl-vhdl::master-slave-flipflop)
		    (:port-map cl-vhdl::phi1 cl-vhdl::phi2
			       (:=> cl-vhdl::d
				    (:compound cl-vhdl::normalized-parallel-data
					       (:paren (:- cl-vhdl::index 1))))
			       (:=> cl-vhdl::q
				    (:compound cl-vhdl::normalized-parallel-data
					       (:paren cl-vhdl::index)))))))
	  "reg : if index = 0 generate
             cell : component master_slave_flipflop
               port map ( phi1, phi2, d => serial_data_in, q => normalized_parallel_data(index) );
           else generate
             cell : component master_slave_flipflop
               port map ( phi1, phi2, d => normalized_parallel_data(index - 1),
                          q => normalized_parallel_data(index) );
           end generate;")
    (frob '(:generate-case cl-vhdl::mult-structure cl-vhdl::implementation
	    (nil cl-vhdl::single-cycle
	     (:signal cl-vhdl::asdf nil cl-vhdl::real-pp1 cl-vhdl::real-pp2)
	     (:instance cl-vhdl::real-mult1 (:component cl-vhdl::multiplier)
	      (:port-map (:=> cl-vhdl::a cl-vhdl::b))))
	    (nil cl-vhdl::multicycle
	     (:signal cl-vhdl::asdf nil cl-vhdl::real-pp1 cl-vhdl::real-pp2)
	     (:instance cl-vhdl::real-mult1 (:component cl-vhdl::multiplier)
	      (:port-map (:=> cl-vhdl::c cl-vhdl::d)))))
	  "mult_structure : case implementation generate
             when single_cycle =>
                 signal real_pp1, real_pp2 : asdf;
               begin
                 real_mult1 : component multiplier
                   port map (a => b);
               end;
             when multicycle =>
                 signal real_pp1, real_pp2 : asdf;
               begin
                 real_mult1 : component multiplier
                   port map (c => d);
               end;
           end generate mult_structure;")
    ))

(test recursive-clock-fanout
  (with-optima-frob (design-unit)
    (frob '(:design-unit
	    (:library cl-vhdl::ieee)
	    (:use (:compound cl-vhdl::ieee (:dot cl-vhdl::std-logic-1164) (:dot :all)))
	    (:entity cl-vhdl::fanout-tree
	     (:generic (:sig-var-con cl-vhdl::natural nil cl-vhdl::height))
	     (:port (:sig-var-con cl-vhdl::std-logic nil :in cl-vhdl::input)
	      (:sig-var-con (:compound cl-vhdl::std-ulogic-vector
				       (:to 0 (:- (:** 2 cl-vhdl::height) 1)))
			    nil :out cl-vhdl::output))))
	  "library ieee; use ieee.std_logic_1164.all;

           entity fanout_tree is
             generic ( height : natural );
             port ( input : in std_logic;
                    output : out std_ulogic_vector (0 to 2**height - 1) );
           end entity fanout_tree;")
    (frob '(:design-unit
	    (:architecture cl-vhdl::recursive cl-vhdl::fanout-tree
	     (:generate-if cl-vhdl::tree
	      (nil (:= cl-vhdl::height 0)
		   (:<= (:compound cl-vhdl::output (:paren 0)) (:waveform (cl-vhdl::input))))
	      (nil t
		   (:signal cl-vhdl::std-ulogic nil cl-vhdl::buffered-input-0
			    cl-vhdl::buffered-input-1)
		   (:instance cl-vhdl::buf-0
			      (:entity (:compound cl-vhdl::work (:dot cl-vhdl::buf) (:paren cl-vhdl::basic)))
			      (:port-map (:=> cl-vhdl::a cl-vhdl::input)
					 (:=> cl-vhdl::y cl-vhdl::buffered-input-0)))
		   (:instance cl-vhdl::subtree-0
			      (:entity (:compound cl-vhdl::work (:dot cl-vhdl::fanout-tree)
						  (:paren cl-vhdl::recursive)))
			      (:generic-map (:=> cl-vhdl::height (:- cl-vhdl::height 1)))
			      (:port-map (:=> cl-vhdl::input cl-vhdl::buffered-input-0)
					 (:=> cl-vhdl::output
					      (:compound cl-vhdl::output
							 (:to 0 (:- (:** 2 (:- cl-vhdl::height 1)) 1))))))
		   (:instance cl-vhdl::buf-1
			      (:entity (:compound cl-vhdl::work (:dot cl-vhdl::buf) (:paren cl-vhdl::basic)))
			      (:port-map (:=> cl-vhdl::a cl-vhdl::input)
					 (:=> cl-vhdl::y cl-vhdl::buffered-input-1)))
		   (:instance cl-vhdl::subtree-1
			      (:entity (:compound cl-vhdl::work (:dot cl-vhdl::fanout-tree)
					  (:paren cl-vhdl::recursive)))
			      (:generic-map (:=> cl-vhdl::height (:- cl-vhdl::height 1)))
			      (:port-map (:=> cl-vhdl::input cl-vhdl::buffered-input-1)
					 (:=> cl-vhdl::output
					      (:compound cl-vhdl::output
							 (:to (:** 2 (:- cl-vhdl::height 1))
							      (:- (:** 2 cl-vhdl::height) 1))))))))))
	  "architecture recursive of fanout_tree is
           begin
             tree : if height = 0 generate
               output(0) <= input;
             else generate
                 signal buffered_input_0, buffered_input_1 : std_ulogic;
               begin
                 buf_0 : entity work.buf(basic)
                   port map ( a => input, y => buffered_input_0 );
                 subtree_0 : entity work.fanout_tree(recursive)
                   generic map ( height => height - 1 )
                   port map ( input => buffered_input_0, 
                              output => output(0 to 2**(height - 1) - 1) );
                 buf_1 : entity work.buf(basic)
                   port map ( a => input, y => buffered_input_1 );
                 subtree_1 : entity work.fanout_tree(recursive)
                   generic map ( height => height - 1 )
                   port map ( input => buffered_input_1, 
                              output => output(2**(height - 1) to 2**height - 1) );
               end;
             end generate tree;
           end architecture recursive;")
    ))
	  

(test configuration-declaration-2
  (with-optima-frob (configuration-declaration)
    (frob '(:configuration cl-vhdl::widget-cfg cl-vhdl::arith-unit
	    (:for cl-vhdl::ripple-adder
	     (:for cl-vhdl::adder
	      (:for (:compound cl-vhdl::adder-cell (:paren cl-vhdl::most-significant))
		    (:for (cl-vhdl::full-adder cl-vhdl::add-bit)
			  (:use
			   (:entity
			    (:compound cl-vhdl::widget-lib (:dot cl-vhdl::full-adder)
				       (:paren cl-vhdl::asic-cell))))))
	      (:for (:compound cl-vhdl::adder-cell (:paren cl-vhdl::middle))
		    (:for (cl-vhdl::full-adder cl-vhdl::add-bit)
			  (:use
			   (:entity
			    (:compound cl-vhdl::widget-lib (:dot cl-vhdl::full-adder)
				       (:paren cl-vhdl::asic-cell))))))
	      (:for (:compound cl-vhdl::adder-cell (:paren cl-vhdl::least-significant))
		    (:for (cl-vhdl::half-adder cl-vhdl::add-bit)
			  (:use
			   (:entity
			    (:compound cl-vhdl::widget-lib (:dot cl-vhdl::half-adder)
				       (:paren cl-vhdl::asic-cell)))))))))
	  "configuration widget_cfg of arith_unit is
             for ripple_adder
               for adder
                 for adder_cell(most_significant)
                   for add_bit: full_adder
                     use entity widget_lib.full_adder(asic_cell);
                   end for;
                 end for;

                 for adder_cell(middle)
                   for add_bit: full_adder
                     use entity widget_lib.full_adder(asic_cell);
                   end for;
                 end for;

                 for adder_cell(least_significant)
                   for add_bit: half_adder
                     use entity widget_lib.half_adder(asic_cell);
                   end for;
                 end for;
               end for;
            end for;
          end configuration widget_cfg;")
    ))
