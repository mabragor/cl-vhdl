
(in-package #:cl-vhdl)

(defun parse-vhdl-file (file-name &key junk-allowed)
  (with-open-file (stream file-name)
    (vhdl-parse-stream 'design-file stream :junk-allowed junk-allowed)))
