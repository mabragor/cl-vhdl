cl-vhdl
-------

Parser of VHDL (VHSIC Hardware Description Language) into very low-level s-exp representation.
Can be used as a basis for simulators/synthetizers.

To be actually useful, must be complemented with the following:
  * emitter from low-level s-exp back to VHDL source text.
  * hi-level s-exp syntax, that is more convenient than the low-level one
    and allows for incremental compilation of parts of the design.


Main functions are VHDL-PARSE and PARSE-VHDL-FILE
```lisp
(vhdl-parse 'design-file str) ; will parse a string, containing VHDL source
(parse-vhdl-file "example.vhd") ; will parse a VHDL file
```

TODO:
  -- now, when parsing fails (usually, due to some minor discrepancy in a file),
     error message is completely non-illuminating.
     The reason is, right now error reporting in ESRAP-LIQUID is not good.
     You may help a lot by designing a better one.

For examples, see tests.lisp