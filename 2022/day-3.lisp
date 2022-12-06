
(cl:defpackage #:aoc-2022-day-3
  (:use #:common-lisp
        #:advent-of-code)
  (:export #:sum-priorities
           #:sum-badge-priorities)
  (:documentation
   "Solutions for the Advent of Code 2022 day 3 puzzles found at
https://adventofcode.com/2022/day/3

Part 1 is solved by
  sum-priorities
and part 2 by
  sum-badge-priorities.

Both look for a file named day-3 in the *INPUT-DIRECTORY*.

See also
https://www.reddit.com/r/adventofcode/comments/zb865p/2022_day_3_solutions/"))

(cl:in-package #:aoc-2022-day-3)

;;; Day 3
;;;
;;; https://adventofcode.com/2022/day/3

;;; Puzzle 1

(defun item-priority (item)
  "Items are characters in the range a-z or A-Z.

The a-z range has priorities 1-26, and A-Z has priorities 27-52."
  (- (char-code item)
     (if (lower-case-p item)
         96
         38)))

(defun common-item (rucksack)
  (loop with mid = (floor (length rucksack) 2)
        for i from 0 below mid
        when (find (char rucksack i) rucksack :start mid :test #'char=)
        do (return (char rucksack i))))

(defun common-item-priority (rucksack)
  (item-priority (common-item rucksack)))

(defun sum-priorities ()
  "Find the item type that appears in both compartments of each rucksack. What
is the sum of the priorities of those item types?"
  (with-file-lines (rucksacks "day-3")
    (reduce #'+ rucksacks :key #'common-item-priority)))

;;; Puzzle 2

(defun badge-item (rucksack1 rucksack2 rucksack3)
  (loop for item across rucksack1
        when (and (find item rucksack2 :test #'char=)
                  (find item rucksack3 :test #'char=))
        do (return item)))

(defun sum-badge-priorities ()
  "Find the item type that corresponds to the badges of each three-Elf
group. What is the sum of the priorities of those item types?"
  (with-file-lines (rucksacks "day-3")
    (loop for (r1 r2 r3) on rucksacks by #'cdddr
          summing (item-priority (badge-item r1 r2 r3)))))
