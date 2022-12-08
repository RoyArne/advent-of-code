
(cl:defpackage #:aoc-2022-day-8
  (:use #:common-lisp
        #:advent-of-code)
  (:export #:count-visible-trees-from-outside-the-grid
           #:highest-possible-scenic-score)
  (:documentation
   "Solutions for the Advent of Code 2022 day 8 puzzles found at
https://adventofcode.com/2022/day/8

Part 1 is solved by
  count-visible-trees-from-outside-the-grid
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

(defun outside-visible-p (array h w max-h max-w)
  "True if the tree at h,w is taller than the tree at max-h,max-w."
  (> (aref array h w) (aref array max-h max-w)))

(defparameter *left-mark* #b1000)
(defparameter *right-mark* #b0001)
(defparameter *top-mark* #b0100)
(defparameter *bottom-mark* #b0010)

(defun make-mark (array h w mark)
  (setf (aref array h w) (logior (aref array h w) mark)))

(defun mark-edges (array)
  (assert (= (array-dimension array 0) (array-dimension array 1)))
  (loop with edge = (1- (height array))
        for i from 0 upto edge
        do (progn
             (make-mark array i 0 *left-mark*)
             (make-mark array i edge *right-mark*)
             (make-mark array 0 i *top-mark*)
             (make-mark array edge i *bottom-mark*)))
  array)

(defun wscan (h grid marks)
  (loop with edge = (width grid)
        with i-max = 0
        with j-max = (1- edge)
        for i from 1 below (1- edge)
        for j from (- edge 2) downto 1
        do (when (outside-visible-p grid h i h i-max)
             (setf i-max i)
             (make-mark marks h i *left-mark*))
        do (when (outside-visible-p grid h j h j-max)
             (setf j-max j)
             (make-mark marks h j *right-mark*))))

(defun hscan (w grid marks)
  (loop with edge = (height grid)
        with i-max = 0
        with j-max = (1- edge)
        for i from 1 below (1- edge)
        for j from (- edge 2) downto 1
        do (when (outside-visible-p grid i w i-max w)
             (setf i-max i)
             (make-mark marks i w *left-mark*))
        do (when (outside-visible-p grid j w j-max w)
             (setf j-max j)
             (make-mark marks j w *right-mark*))))

(defun outside-visibility-scan (grid)
  (let ((marks (mark-edges (make-same-size-array grid))))
    (loop for h from 1 below (height grid)
          do (wscan h grid marks))
    (loop for w from 1 below (width grid)
          do (hscan w grid marks))
    marks))

(defun count-visible-trees-from-outside-the-grid ()
  "how many trees are visible from outside the grid?"
  (with-open-input (stream "day-8")
    (loop with marks = (outside-visibility-scan (read-grid stream))
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

(defun highest-possible-scenic-score ()
  (with-open-input (stream "day-8")
    (let ((grid (read-grid stream)))
      (loop for h from 0 below (height grid)
            maximizing (loop for w from 0 below (width grid)
                             maximizing (compute-scenic-score grid h w))))))
