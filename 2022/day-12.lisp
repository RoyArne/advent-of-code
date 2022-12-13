
(cl:defpackage #:aoc-2022-day-12
  (:use #:common-lisp
        #:advent-of-code)
  (:export #:fewest-steps?
           #:fewest-steps-from-any-a?)
  (:documentation
   "Solutions for the Advent of Code 2022 day  puzzles found at
https://adventofcode.com/2022/day/12

Part 1 is solved by
  fewest-steps?
and part 2 by
  fewest-steps-from-any-a?

Both look for a file named day-12 in the *INPUT-DIRECTORY*.

See also"))

(cl:in-package #:aoc-2022-day-12)

(defun character-position (char lines &optional replacement-char)
  "Return the position of CHAR in LINES as a complex number. If given,
replacement-char replaces the character at that position."
  (loop for line in lines
        for y = 0 then (1+ y)
        for x = (position char line :test #'char=)
        do (when x
             (when replacement-char
               (setf (aref line x) replacement-char))
             (return (complex y x)))))

(defparameter *up* #c(-1 0))
(defparameter *right* #c(0 1))
(defparameter *down* #c(1 0))
(defparameter *left* #c(0 -1))

(defclass problem ()
  ((start :reader start-position
          :initarg :start
          :initform #c(0 0))
   (end :reader end-position
        :initarg :end
        :initform #c(0 0))
   (actions :reader problem-actions
            :initform (list *up* *right* *down* *left*)
            :documentation "A list of relative positions.")
   (heightmap :reader heightmap
              :initarg :heightmap
              :initform (make-array '(0 0) :element-type 'character)
              :documentation "A 2d array of characters in the range a \(low\) - z \(high\).")))

(defun make-heightmap (lines)
  (make-array (list (length lines) (length (first lines)))
              :element-type 'character
              :initial-contents lines))

(defun height (heightmap position)
  (aref heightmap (realpart position) (imagpart position)))

(defun inside-map-p (heightmap position)
  (and (< -1 (realpart position) (array-dimension heightmap 0))
       (< -1 (imagpart position) (array-dimension heightmap 1))))

(defun height-difference (heightmap position1 position2)
  (- (char-code (height heightmap position2)) (char-code (height heightmap position1))))

(defun make-problem (lines)
  (let ((start (character-position #\S lines #\a))
        (end (character-position #\E lines #\z)))
    (make-instance 'problem
                   :start start
                   :end end
                   :heightmap (make-heightmap lines))))

(defclass location ()
  ((this :reader this-position
         :initarg :this
         :initform #c(0 0))
   (cost :reader path-cost
         :initarg :cost
         :initform 0)
   (problem :reader path-problem
            :initarg :problem
            :initform (make-instance 'problem))))

(defmethod print-object ((location location) stream)
  (print-unreadable-object (location stream :type t)
    (with-slots (this cost problem) location
      (format stream "~A ~A" (height (heightmap problem) this) cost))))

(defgeneric make-origin (problem)
  (:method ((problem problem))
    (make-instance 'location
                   :this (start-position problem)
                   :problem problem)))


(defgeneric make-destination (direction origin)
  (:documentation
   "Return a destination location or NIL if one cannot travel in DIRECTION from ORIGIN.")
   
  (:method ((direction number) (origin location))
    (with-slots (this) origin
      (with-slots (heightmap) (path-problem origin)
        (let ((position (+ this direction)))
          (when (and (inside-map-p heightmap position)
                     (>= 1 (height-difference heightmap this position)))
            (make-instance 'location
                           :this position
                           :cost (1+ (path-cost origin))
                           :problem (path-problem origin))))))))
        

(defgeneric successors (location)
  (:documentation
   "Return a list of locations one can move to from LOCATION.")
  
  (:method ((location location))
    (remove-if #'null
               (loop for direction in (problem-actions (path-problem location))
                     collect (make-destination direction location)))))


(defgeneric (setf visitedp) (location table)
  (:documentation
   "Adds LOCATION to TABLE, replacing any existing locations with the same position.")
  
  (:method ((location location) (table hash-table))
    (setf (gethash (this-position location) table) location)))


(defgeneric visitedp (location table)
  (:documentation
   "If TABLE contains a location with the same position as LOCATION then return that location;
otherwise, return NUL.")
  
  (:method ((location location) (table hash-table))
    (gethash (this-position location) table)))


(defgeneric solutionp (location)
  (:documentation
   "True if LOCATION is the end position of its path-problem.")
  
  (:method ((location location))
    (= (this-position location) (end-position (path-problem location)))))


(defgeneric better-solution-p (location solution)
  (:documentation
   "True if SOLUTION is NIL, or if LOCATIONs path-cost is less than SOLUTIONs path-cost.")
  
  (:method ((location location) (solution null))
    t)
  (:method ((location location) (solution location))
    (< (path-cost location) (path-cost solution))))


(defgeneric uniform-cost-search (problem)
  (:method ((problem problem))
    (loop with solution = nil
          with table = (make-hash-table)
          with frontier = (list (make-origin problem))

          while (and frontier
                     (better-solution-p (first frontier) solution))

          for current = (pop frontier)
          
          do (loop for next in (successors current)
                   for old = (visitedp next table)

                   do (when (better-solution-p next old)
                        (setf (visitedp table) next)
                        (push next frontier)
                        (when (and (solutionp next)
                                   (better-solution-p next solution))
                          (setf solution next)))
                   finally (setf frontier (sort frontier #'< :key #'path-cost)))

          finally (return solution))))

(defun fewest-steps? ()
  (with-open-input (stream "day-12")
    (uniform-cost-search (make-problem (read-file-lines stream)))))

(defun make-starting-positions (problem)
  (with-slots (heightmap) problem
    (apply #'append
           (loop for row from 0 below (array-dimension heightmap 0)
                 collect (loop for column from 0 below (array-dimension heightmap 0)
                               when (char= #\a (height heightmap (complex row column)))
                               collect (make-instance 'problem
                                                      :start (complex row column)
                                                      :end (end-position problem)
                                                      :heightmap heightmap))))))

(defun fewest-steps-from-any-a? ()
  (with-open-input (stream "day-12")
    (let ((original (make-problem (read-file-lines stream))))
      (first (sort (remove-if #'null
                              (map 'list
                                   #'uniform-cost-search
                                   (make-starting-positions original)))
                   #'<
                   :key #'path-cost)))))

