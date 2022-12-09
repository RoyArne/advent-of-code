
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

(defun adjust-tail (tail head)
  ;; Tail and head must always be adjacent or on top of each other.
  ;; If head is ever two steps away horizontally or vertically, then tail must
  ;; move one step towards it.
  ;; If head and tail are not touching and are not in the same row or column,
  ;; then tail must move one step diagonally towards it.
  (flet ((adjustment (part)  ; part is either realpart or imagpart.
           (cond
             ((minusp part) 1)   ; increment the tail part
             ((plusp part) -1)   ; decrement the tail part
             (t 0))))            ; tail part is unchanged.
    (let ((distance (- tail head)))
      (if (or (> (abs (realpart distance)) 1)
              (> (abs (imagpart distance)) 1))
          ;; At this point at least one part of distance is two or higher.
          ;; and the other part is zero or one.
          (+ tail
             ;; This becomes a diagonal move if one part is two and the other
             ;; is one. Otherwise we move either horizontally or vertically.
             (complex (adjustment (realpart distance))
                      (adjustment (imagpart distance))))
          ;; We return tail unchanged if neither part of distance is two or higher.
          tail))))

(defun simulate-motions (motions n)
  (loop with knots = (make-array n :initial-element #c(0 0))
        with tail-positions = (list (aref knots (1- n)))
        for (direction distance) in motions
        do (loop repeat distance
                 do (incf (aref knots 0) direction)
                 do (loop for i from 1 below n
                          do (setf (aref knots i) (adjust-tail (aref knots i)
                                                               (aref knots (1- i))))
                          finally (pushnew (aref knots (1- n)) tail-positions :test #'=)))
        finally (return tail-positions)))

(defun how-many-positions-does-the-tail-of-the-rope-visit-at-least-once?

    (part-number &optional filename)

  (with-open-input (stream (or filename "day-9"))
    (ecase part-number
      (1 (length (simulate-motions (read-motions stream) 2)))
      (2 (length (simulate-motions (read-motions stream) 10))))))
