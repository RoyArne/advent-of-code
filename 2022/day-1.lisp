
(cl:in-package #:advent-of-code)

;;; Day 1 puzzles
;;;
;;; https://adventofcode.com/2022/day/1
;;;
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
    (reduce #'+ (subseq (sort-total-calories (read-bags stream)) 0 3))))
