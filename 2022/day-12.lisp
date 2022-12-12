
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

(defparameter *up* #c(-1 0))
(defparameter *right* #c(0 1))
(defparameter *down* #c(1 0))
(defparameter *left* #c(0 -1))

(defparameter *moves* (list *up* *right* *down* *left*))

(defun find-char (char lines &optional replacement-char)
  (loop for line in lines
        for y = 0 then (1+ y)
        for x = (position char line :test #'char=)
        when x
        do (progn
             (when replacement-char (setf (aref line x) replacement-char))
             (return (complex y x)))))

(defclass node ()
  ((start :reader start-position
          :initarg :start
          :initform #c(0 0)
          :allocation :class)
   (end :reader end-position
        :initarg :end
        :initform #c(0 0)
        :allocation :class)
   (this :reader this-position
         :initarg :this
         :initform #c(0 0))
   (cost :reader path-cost
         :initarg :cost
         :initform 0)
   (heightmap :reader heightmap
              :initarg :heightmap
              :initform (make-array '(0 0) :element-type 'character)
              :allocation :class)))

(defmethod print-object ((node node) stream)
  (print-unreadable-object (node stream :type t)
    (with-slots (heightmap this) node
      (format stream "~A ~A"
              (aref heightmap (realpart this) (imagpart this))
              (path-cost node)))))

(defun read-start-node (stream)
  (let* ((lines (read-file-lines stream))
         (start (find-char #\S lines #\a))
         (end (find-char #\E lines #\z)))
    (make-instance 'node
                   :start start
                   :end end
                   :this start
                   :heightmap (make-array (list (length lines) (length (first lines)))
                                          :element-type 'character
                                          :initial-contents lines))))

(defun height (heightmap position)
  (aref heightmap (realpart position) (imagpart position)))

(defun valid-position-p (heightmap position)
  (flet ((valid-coordinate-p (coordinate axis-number)
           (< -1 coordinate (array-dimension heightmap axis-number))))
    (and (valid-coordinate-p (realpart position) 0)
         (valid-coordinate-p (imagpart position) 1))))

(defun movement-cost (height1 height2)
  (- (char-code height2) (char-code height1)))

(defun valid-move-p (heightmap position1 position2)
  (and (valid-position-p heightmap position2)
       (>= 1 (movement-cost (height heightmap position1) (height heightmap position2)))))

(defun compute-moves (node)
  (with-slots (heightmap this) node
    (remove-if #'(lambda (new)
                   (not (valid-move-p heightmap this new)))
               (mapcar #'(lambda (direction)
                           (+ this direction))
                       *moves*))))

(defun successors (node)
  (loop for new-position in (compute-moves node)
        collect (make-instance 'node
                               :this new-position
                               :cost (1+ (path-cost node)))))

(defun reachedp (node reached)
  (gethash (this-position node) reached))

(defun note-as-reached (node reached)
  (setf (gethash (this-position node) reached) node))

(defun solutionp (node)
  (= (this-position node) (end-position node)))

(defun uniform-cost-search (start)
  "function UNIFORM-COST-SEARCH(problem) returns a solution, or failure
 if problem's initial state is a goal then return empty path to initial state

frontier ← a priority queue ordered by pathCost, with a node for the initial state
reached ← a table of {state: the best path that reached state}; initially empty
solution ← failure

while frontier is not empty and top(frontier) is cheaper than solution do
  parent ← pop(frontier)
  for child in successors(parent) do
    s ← child.state
    if s is not in reached or child is a cheaper path than reached[s] then
      reached[s] ← child
      add child to the frontier
      if child is a goal and is cheaper than solution then
        solution = child
return solution

From https://github.com/aimacode/aima-pseudocode/blob/master/md/Uniform-Cost-Search.md."
  (loop with solution = nil
        with frontier = (list start)
        with reached = (make-hash-table)

        initially (setf (gethash (this-position start) reached) start)
        
        while frontier
        while (or (not solution)
                  (< (path-cost (first frontier)) (path-cost solution)))

        for current = (pop frontier)
        
        do (loop for next in (successors current)
                 for old = (gethash (this-position next) reached)

                 do (when (or (not old)
                              (< (path-cost next) (path-cost old)))
                      (setf (gethash (this-position next) reached) next)
                      (push next frontier)
                      (when (and (solutionp next)
                                 (or (not solution)
                                     (< (path-cost next) (path-cost solution))))
                        (setf solution next)))

                 finally (setf frontier (sort frontier #'< :key #'path-cost)))

        finally (return solution)))
        

(defun fewest-steps? ()
  (with-open-input (stream "day-12")
    (uniform-cost-search (read-start-node stream))))

(defun make-starting-positions (start)
  (with-slots (heightmap) start
    (apply #'append
           (loop for row from 0 below (array-dimension heightmap 0)
                 collect (loop for column from 0 below (array-dimension heightmap 0)
                               when (char= #\a (height heightmap (complex row column)))
                               collect (make-instance 'node
                                                      :this (complex row column)))))))

(defun fewest-steps-from-any-a? ()
  (with-open-input (stream "day-12")
    (let ((start (read-start-node stream)))
      (first (sort (remove-if #'null
                              (map 'list
                                   #'uniform-cost-search
                                   (make-starting-positions start)))
                   #'<
                   :key #'path-cost)))))
