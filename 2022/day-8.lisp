
(cl:defpackage #:aoc-2022-day-8
  (:use #:common-lisp
        #:advent-of-code)
  (:export #:how-many-trees-are-visible-from-outside-the-grid?
           #:highest-possible-scenic-score)
  (:documentation
   "Solutions for the Advent of Code 2022 day 8 puzzles found at
https://adventofcode.com/2022/day/8

Part 1 is solved by
  how-many-trees-are-visible-from-outside-the-grid?
and part 2 by
  highest-possible-scenic-score.

Both look for a file named day-8 in the *INPUT-DIRECTORY*.

See also"))

(cl:in-package #:aoc-2022-day-8)

(defun read-grid (stream)
  (let ((grid (loop for line = (read-line stream nil nil)
                    while line
                    collect (map 'vector #'sb-unicode:digit-value line))))
    (make-array (list (length grid)           ; heigth
                      (length (first grid)))  ; width
                :element-type 'fixnum
                :initial-contents grid)))

(defun make-same-size-array (grid)
  (make-array (array-dimensions grid) :element-type 'fixnum :initial-element 0))

(defun height (array)
  (array-dimension array 0))

(defun width (array)
  (array-dimension array 1))

(defun grid-dimension (grid direction)
  (ecase direction
    (:vertical (height grid))
    (:horizontal (width grid))))

(defun grid-major-index (grid direction index offset)
  "Return a row major index for index + offset in the given direction.

If direction is :vertical then index is used as column and offset as row.
If direction is :horizontal then index is used as row and offset as column."
  (ecase direction
    (:vertical (array-row-major-index grid offset index))
    (:horizontal (array-row-major-index grid index offset))))

(defun grid-step-by (grid direction)
  "Return the distance between row major indices in the given direction."
  (ecase direction
    (:vertical (width grid))
    (:horizontal 1)))

(defun scan-line (function grid direction index offset)
  (loop with start = (grid-major-index grid direction index offset)
        with end = (grid-major-index grid direction index
                                     (- (grid-dimension grid direction) 1 offset))
        with step = (grid-step-by grid direction)
        for left from start upto end by step
        for right from end downto start by step
        do (funcall function left right)))

(defun visible-from-outside-p (grid i max-i)
  "True if the tree at I is taller than the tree at MAX-I."
  (> (row-major-aref grid i) (row-major-aref grid max-i)))

(defun mark-as-visible-from-outside (marks index mark)
  (setf (row-major-aref marks index)
        (logior (row-major-aref marks index) mark)))

(defun mark-trees-visible-from-outside (grid direction marks start-mark end-mark)
  (loop for index from 1 below (1- (grid-dimension grid direction))
        for start-max = (grid-major-index grid direction index 0)
        for end-max = (grid-major-index grid direction index (1- (grid-dimension grid direction)))
        do (scan-line #'(lambda (start end)
                          (when (visible-from-outside-p grid start start-max)
                            (setf start-max start)
                            (mark-as-visible-from-outside marks start start-mark))
                          (when (visible-from-outside-p grid end end-max)
                            (setf end-max end)
                            (mark-as-visible-from-outside marks end end-mark)))
                      grid direction index 1)))


(defparameter *left-mark* #b1000)
(defparameter *right-mark* #b0001)
(defparameter *top-mark* #b0100)
(defparameter *bottom-mark* #b0010)

(defun mark-edges (marks)
  (scan-line #'(lambda (start end)
                 (declare (ignore end))
                 (mark-as-visible-from-outside marks start *top-mark*))
             marks :horizontal 0 0)
  (scan-line #'(lambda (start end)
                 (declare (ignore end))
                 (mark-as-visible-from-outside marks start *bottom-mark*))
             marks :horizontal (1- (height marks)) 0)
  (scan-line #'(lambda (start end)
                 (declare (ignore end))
                 (mark-as-visible-from-outside marks start *left-mark*))
             marks :vertical 0 0)
  (scan-line #'(lambda (start end)
                 (declare (ignore end))
                 (mark-as-visible-from-outside marks start *right-mark*))
             marks :vertical (1- (width marks)) 0)
  marks)

(defun scan-and-mark-visible-trees (grid)
  (let ((marks (mark-edges (make-same-size-array grid))))
    (mark-trees-visible-from-outside grid :horizontal marks *left-mark* *right-mark*)
    (mark-trees-visible-from-outside grid :vertical marks *top-mark* *bottom-mark*)
    marks))

(defun how-many-trees-are-visible-from-outside-the-grid? (&optional filename)
  (with-open-input (stream (or filename "day-8"))
    (loop with marks = (scan-and-mark-visible-trees (read-grid stream))
          for h from 0 below (height marks)
          summing (loop for w from 0 below (width marks)
                        counting (plusp (aref marks h w))))))


(defun blocking-visibility-p (grid h w test-h test-w)
  "True if the tree at test-h,test-w blocks visibility for h,w."
  (>= (aref grid test-h test-w) (aref grid h w)))

(defun horizontal-view (grid h w)
  (values (loop for i from (1- w) downto 0
                when (blocking-visibility-p grid h w h i)
                do (return (- w i))
                finally (return w))
          (loop for i from (1+ w) below (width grid)
                when (blocking-visibility-p grid h w h i)
                do (return (- i w))
                finally (return (- (width grid) w 1)))))

(defun vertical-view (grid h w)
  (values (loop for i from (1- h) downto 0
                when (blocking-visibility-p grid h w i w)
                do (return (- h i))
                finally (return h))
          (loop for i from (1+ h) below (height grid)
                when (blocking-visibility-p grid h w i w)
                do (return (- i h))
                finally (return (- (height grid) h 1)))))
        
(defun compute-scenic-score (grid h w)
  (multiple-value-bind (l r)
      (horizontal-view grid h w)
    (multiple-value-bind (u d)
        (vertical-view grid h w)
      (* l r u d))))

(defun highest-possible-scenic-score (&optional filename)
  (with-open-input (stream (or filename "day-8"))
    (let ((grid (read-grid stream)))
      (loop for h from 0 below (height grid)
            maximizing (loop for w from 0 below (width grid)
                             maximizing (compute-scenic-score grid h w))))))
