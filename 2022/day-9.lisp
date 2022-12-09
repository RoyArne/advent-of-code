
(cl:defpackage #:aoc-2022-day-9
  (:use #:common-lisp
        #:advent-of-code)
  (:export #:how-many-positions-does-the-tail-of-the-rope-visit-at-least-once?)
  (:documentation
   "Solutions for the Advent of Code 2022 day 9 puzzles found at
https://adventofcode.com/2022/day/9

Part 1 is solved by
  \(how-many-positions-does-the-tail-of-the-rope-visit-at-least-once? 1\)
and part 2 by
  \(how-many-positions-does-the-tail-of-the-rope-visit-at-least-once? 2\).

Both look for a file named day-9 in the *INPUT-DIRECTORY*.

See also"))

(cl:in-package #:aoc-2022-day-9)

(defun read-motions (stream)
  (loop for line = (read-line stream nil nil)
        while line
        collect (list (ecase (char line 0)
                        (#\U #c(0 1))
                        (#\R #c(1 0))
                        (#\D #c(0 -1))
                        (#\L #c(-1 0)))
                      (parse-integer (subseq line 2)))))

(defun sign (distance)
  (cond
    ((minusp distance)
     1)
    ((plusp distance)
     -1)
    (t
     0)))

(defun adjust-tail (tail head)
  (let ((distance (- tail head)))
    (if (or (> (abs (realpart distance)) 1)
            (> (abs (imagpart distance)) 1))
        (+ tail
           (complex (* 1 (sign (realpart distance)))
                    (* 1 (sign (imagpart distance)))))
        tail)))

(defun simulate-motions (motions n)
  (loop with knots = (make-array n :initial-element #c(0 0))
        with tail-positions = (list (aref knots (1- n)))
        for motion in motions
        do (destructuring-bind (direction distance)
               motion
             (loop repeat distance
                   do (incf (aref knots 0) direction)
                   do (loop for i from 1 below n
                            do (setf (aref knots i) (adjust-tail (aref knots i)
                                                                 (aref knots (1- i))))
                            finally (pushnew (aref knots (1- n)) tail-positions :test #'=))))
        finally (return tail-positions)))

(defun how-many-positions-does-the-tail-of-the-rope-visit-at-least-once?

    (part-number &optional filename)

  (with-open-input (stream (or filename "day-9"))
    (ecase part-number
      (1 (length (simulate-motions (read-motions stream) 2)))
      (2 (length (simulate-motions (read-motions stream) 10))))))
