
(cl:defpackage #:aoc-2022-day-1
  (:use #:common-lisp
        #:advent-of-code)
  (:export #:calories-carried-by-elf-carrying-most
           #:calories-carried-by-top-three-elves-carrying-most)
  (:documentation
   "Solutions for the Advent of Code 2022 day 1 puzzles found at
https://adventofcode.com/2022/day/1

Part 1 is solved by
  calories-carried-by-elf-carrying-most
and part 2 by
  calories-carried-by-top-three-elves-carrying-most.

Both look for a file named day-1 in the *INPUT-DIRECTORY*.

See also
https://www.reddit.com/r/adventofcode/comments/z9ezjb/2022_day_1_solutions/"))

(cl:in-package #:aoc-2022-day-1)

(defun read-bag-contents (stream)
  "Read text lines from STREAM until end of file or an empty line is
found.

Each line contains a single integer.

Returns a list of integers."
  (loop for line = (trim-whitespace (read-line stream nil ""))
        while (plusp (length line))
        collect (parse-integer line)))

(defun read-bags (stream)
  (loop until (null (peek-char nil stream nil nil))
        collect (read-bag-contents stream)))

(defun total-calories (bag)
  (reduce #'+ bag))

(defun sort-total-calories (bags)
  (sort (mapcar #'total-calories bags) #'>))

(defun calories-carried-by-elf-carrying-most ()
  "Find the Elf carrying the most Calories. How many total Calories is that
Elf carrying?"
  (with-open-input (stream "day-1")
    (first (sort-total-calories (read-bags stream)))))

(defun calories-carried-by-top-three-elves-carrying-most ()
  "Find the top three Elves carrying the most Calories. How many Calories are
those Elves carrying in total?"
  (with-open-input (stream "day-1")
    (reduce #'+ (sort-total-calories (read-bags stream)) :start 0 :end 3)))
