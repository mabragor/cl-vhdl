cl-vhdl
-------

Parser of VHDL (VHSIC Hardward Description Language) into very low-level s-exp representation.
Can be used as a basis for simulators/synthetizers.

To be actually useful, must be complemented with the following:
  * emitter from low-level s-exp back to VHDL source text.
  * hi-level s-exp syntax, that is more convenient than the low-level one
    and allows for incremental compilation of parts of the design.


Main function is VHDL-PARSE
```lisp
(vhdl-parse 'design-file str) ; will parse a string, containing VHDL source
```

For examples, see tests.lisp