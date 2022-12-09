
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
  "Return a list of motions in the form \(<direction> <distance>\)."
  (loop for line = (read-line stream nil nil)
        while line
        collect (list (ecase (char line 0)
                        (#\U :up)
                        (#\R :right)
                        (#\D :down)
                        (#\L :left))
                      (parse-integer (subseq line 2)))))

(defun position= (position1 position2)
  (and (= (first position1) (first position2))
       (= (rest position1) (rest position2))))

(defun move (position direction)
  (ecase direction
    (:up (cons (first position) (incf (rest position))))
    (:right (cons (incf (first position)) (rest position)))
    (:down (cons (first position) (decf (rest position))))
    (:left (cons (decf (first position)) (rest position)))))

(defun distance (coordinate1 coordinate2)
  (let* ((d (- coordinate2 coordinate1)))
    (values (abs d)
            (cond
              ((plusp d) 1)
              ((minusp d) -1)
              (t 0)))))

(defun update (tail head)
  (multiple-value-bind (dx sx)
      (distance (first tail) (first head))
    (multiple-value-bind (dy sy)
        (distance (rest tail) (rest head))
      (cons (+ (first tail)
               (if (zerop dx)
                   0
                   (case dy
                     ((0 1) (* (1- dx) sx))
                     (2 (* 1 sx)))))
            (+ (rest tail)
               (if (zerop dy)
                   0
                   (case dx
                     ((0 1) (* (1- dy) sy))
                     (2 (* 1 sy)))))))))
          
(defun simulate-motions (motions)
  (loop with head = (cons 0 0) ; (cons <horizontal> <vertical>)
        with tail = (cons 0 0)
        with tail-positions = (list tail)
        for motion in motions
        do (destructuring-bind (direction distance)
               motion
             (loop repeat distance
                   do (setf head (move head direction)
                            tail (update tail head))
                   do (push tail tail-positions)))
        finally (return tail-positions)))

(defun simulate-motions-on-larger-rope (motions)
  (loop with n = 10
        with knots = (loop repeat n collect (cons 0 0))
        with tail-positions = (list (nth (1- n) knots))
        for motion in motions
        do (destructuring-bind (direction distance)
               motion
             (loop repeat distance
                   do (setf (nth 0 knots) (move (nth 0 knots) direction))
                   do (loop for tail from 1 upto (- n 1)
                            do (setf (nth tail knots) (update (nth tail knots)
                                                              (nth (1- tail) knots)))
                            do (push (nth (1- n) knots) tail-positions))))
        finally (return tail-positions)))

(defun how-many-positions-does-the-tail-of-the-rope-visit-at-least-once? (part-number
                                                                          &optional filename)
  (with-open-input (stream (or filename "day-9"))
    (ecase part-number
      (1 (length (remove-duplicates (simulate-motions (read-motions stream))
                                    :test #'position=)))
      (2 (length (remove-duplicates (simulate-motions-on-larger-rope (read-motions stream))
                                    :test #'position=))))))
