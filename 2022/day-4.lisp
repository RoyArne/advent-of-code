
(cl:defpackage #:aoc-2022-day-4
  (:use #:common-lisp
        #:advent-of-code)
  (:export #:count-fully-contained-pairs
           #:count-overlapping-pairs)
  (:documentation
   "Solutions for the Advent of Code 2022 day 4 puzzles found at
https://adventofcode.com/2022/day/4

Part 1 is solved by
  count-fully-contained-pairs
and part 2 by
  count-overlapping-pairs.

Both look for a file named day-4 in the *INPUT-DIRECTORY*.

The solutions depend on the cl-ppcre library: \(ql:quickload \"cl-ppcre\"\)

See also
https://www.reddit.com/r/adventofcode/comments/zc0zta/2022_day_4_solutions/"))

(cl:in-package #:aoc-2022-day-4)

(defun read-assignment-pairs ()
  (with-open-input (stream "day-4")
    (loop for line = (read-line stream nil nil)
          while line
          collect (cl-ppcre:register-groups-bind ((#'parse-integer start1 end1 start2 end2))
	              ("(\\d+)-(\\d+),(\\d+)-(\\d+)" line)
                    (list start1 end1 start2 end2)))))

(defun containsp (start1 end1 start2 end2)
  (and (<= start1 start2)
       (>= end1 end2)))
  
(defun fully-contains-p (assignment-pair)
  (destructuring-bind (start1 end1 start2 end2)
      assignment-pair
    (or (containsp start1 end1 start2 end2)
        (containsp start2 end2 start1 end1))))

(defun count-fully-contained-pairs ()
  "In how many assignment pairs does one range fully contain the other?"
  (count-if #'fully-contains-p (read-assignment-pairs)))

(defun overlapsp (start1 end1 start2 end2)
  (and (<= start1 end2)
       (<= start2 end1)))

(defun pair-overlaps-p (assignment-pair)
  (destructuring-bind (start1 end1 start2 end2)
      assignment-pair
    (overlapsp start1 end1 start2 end2)))

(defun count-overlapping-pairs ()
  "In how many assignment pairs do the ranges overlap?"
  (count-if #'pair-overlaps-p (read-assignment-pairs)))
