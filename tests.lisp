(in-package :cl-user)

(defpackage :cl-vhdl-tests
  (:use :alexandria :cl :cl-vhdl :fiveam :iterate :cl-read-macro-tokens)
  (:export #:run-tests))

(in-package :cl-vhdl-tests)

(cl-interpol:enable-interpol-syntax)
(enable-read-macro-tokens)

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
  (is (equal 'cl-vhdl::default (let ((*vhdl-version* nil)) (vhdl-parse 'reserved-word "default"))))
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
