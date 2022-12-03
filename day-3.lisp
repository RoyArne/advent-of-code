
(cl:in-package #:advent-of-code)

;;; Day 3
;;;
;;; https://adventofcode.com/2022/day/3

;;; Puzzle 1

(defun read-rucksacks ()
  (with-open-input (stream "day-3")
    (loop for line = (read-line stream nil nil)
          while line
          collect line)))

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
  (reduce #'+ (read-rucksacks) :key #'common-item-priority))

;;; Puzzle 2

(defun badge-item (rucksacks)
  (loop for item across (first rucksacks)
        when (and (find item (second rucksacks) :test #'char=)
                  (find item (third rucksacks) :test #'char=))
        do (return item)))

(defun sum-badge-priorities ()
  "Find the item type that corresponds to the badges of each three-Elf
group. What is the sum of the priorities of those item types?"
  (loop for group = (read-rucksacks) then (cdddr group) while group
        summing (item-priority (badge-item group)))) 
