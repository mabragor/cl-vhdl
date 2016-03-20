
;; OK, here we will write what a S-exp code snippets of VHDL would look like

(define-entity reg4
    ;; Does it make sense to write important things (like type and var-type) first?
    (port (in bit (d0 d1 d2 d3 en clk))
	  (out bit (q0 q1 q2 q3))))

;; THOUGHT: I could simplify code, if I include "patterns" for symbol-names in it

(define-architechture behav reg4
  (process storage (variable (bit (stored-d0 stored-d1 stored-d2 stored-d3)))
	   (wait until clk)
	   (if en
	       ;; := should be multiple, like SETF
	       (:= stored-d0 d0
		   stored-d1 d1
		   stored-d2 d2
		   stored-d3 d3))
	   ;; this smells like special reading syntax
	   (<= q0 stored-d0 after 5ns)
	   (<= q1 stored-d1 after 5ns)
	   (<= q2 stored-d2 after 5ns)
	   (<= q3 stored-d3 after 5ns)))
	   

(define-entity d-ff
    ;; When I write arguments, whose number can vary, last, I can omit the brackets -- very convenient
    (port (in bit d clk) (out bit q)))

(define-architechture basic d-ff
  (process ff-behavior
	   (wait until clk)
	   (<= q d after 2ns)))

(define-entity and2
    (port (in bit a b) (out bit y)))
(define-architechture basic and2
  (process and2-behavior
	   (<= y (and a b) after 2ns)
	   (wait on a b) ; ??? what the hell is this ???
	   ))


(define-architechture struct reg4
  (signal bit int-clk)
  (entity bit0 (work d-ff basic) ; I don't understand, why it's "work" here
	  (port-map d0 int-clk q0))
  (entity bit1 (work d-ff basic) ; I don't understand, why it's "work" here
	  (port-map d1 int-clk q1))
  (entity bit2 (work d-ff basic) ; I don't understand, why it's "work" here
	  (port-map d2 int-clk q2))
  (entity bit3 (work d-ff basic) ; I don't understand, why it's "work" here
	  (port-map d3 int-clk q3))
  (entity gate (work and2 basic)
	  (port-map en clk int-clk)))

;; Probably, I can rewrite this better
(define-architechture struct reg4
  (signal bit int-clk)
  (entity bit$a (work d-ff basic) ; I don't understand, why it's "work" here
	  (port-map d$a int-clk q$a))(0 3)
  (entity gate (work and2 basic)
	  (port-map en clk int-clk)))


(define-entity multiplier
    (port (in bit clk reset)
	  (in integer multiplicand multiplier)
	  (out integer product)))

(define-architechture mixed multiplier
  (signal integer partial-product full-product)
  (signal bit arith-control result-en mult-bit mult-load)
  (entity arith-unit (work shift-adder behavior)
	  (port-map (addend multiplicand) (augend full-product)
		    (sum partial-product) (add-control arith-control)))
  (entity result (work reg behavior)
	  (port-map (d partial-product) (q full-product)
		    (en result-en) (reset reset)))
  (entity multiplier-sr (work shift-reg behavior)
	  (port-map (d multiplier) (q mult-bit)
		    (load mult-load) (clk clk)))
  (<= product full-product)
  (process control-section
	   ...
	   (wait on clk reset)))

(define-entity test-bench)

;; THOUGHT : I need a good mechanism for splicing macro for this to work

(define-architechture test-reg4 test-bench
  (signal bit d$a(0 3) en clk q$a(0 3))
  (entity dut (work reg4 behav)
	  (port-map d$a(0 3) en clk q$a(0 3)))
  (process stimulus
	   (<= d$a 1)(0 3)
	   (<= en 0) (<= clk 0)
	   (wait for 20ns)
	   (<= en 1) (wait for 20 ns)
	   (<= clk 1) (wait for 20 ns)
	   ;; perhaps, pattern forms should have special syntax?
	   [<= d$a 0](0 3) (wait for 20 ns)
	   (<= en 0) (wait for 20 ns)
	   ...
	   (wait)))

	  
