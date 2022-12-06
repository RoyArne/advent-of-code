
(cl:defpackage #:aoc-2022-day-6
  (:use #:common-lisp
        #:advent-of-code)
  (:export #:first-start-of-packet-marker
           #:first-start-of-message-marker)
  (:documentation
   "Solutions for the Advent of Code 2022 day 6 puzzles found at
https://adventofcode.com/2022/day/6

Part 1 is solved by
  first-start-of-packet-marker
and part 2 by
  first-start-of-message-marker.

Both look for a file named day-6 in the *INPUT-DIRECTORY*.

See also
https://www.reddit.com/r/adventofcode/comments/zdw0u6/2022_day_6_solutions/"))

(cl:in-package #:aoc-2022-day-6)

(defun match-index (string start end)
  (loop for i from end downto start
        do (loop for j from (1- i) downto start
                 when (char= (char string i) (char string j))
                 do (return-from match-index j))))

(defun index-of-first-n-distinct-chars (string n)
  (loop with i = 0
        with j = (1- n)
        for match = (match-index string i (+ i j))
        do (if match
               (setf i (1+ match))
               (return i))))
  
(defun first-start-of-packet-marker ()
  (with-open-input (stream "day-6")
    (+ 4 (index-of-first-n-distinct-chars (read-line stream nil nil) 4))))

(defun first-start-of-message-marker ()
  (with-open-input (stream "day-6")
    (+ 14 (index-of-first-n-distinct-chars (read-line stream nil nil) 14))))
