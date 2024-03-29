
(cl:defpackage #:advent-of-code
  (:use #:common-lisp)
  (:export #:*input-directory*
           #:with-open-input
           #:read-file-lines
           #:with-stream-lines
           #:with-file-lines
           #:trim-whitespace
           #:split)
  (:documentation
   "The ADVENT-OF-CODE package exports a few utilities used to solve the
puzzles at https://adventofcode.com/."))
