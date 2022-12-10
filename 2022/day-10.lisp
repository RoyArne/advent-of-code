
(cl:defpackage #:aoc-2022-day-10
  (:use #:common-lisp
        #:advent-of-code)
  (:export #:what-is-the-sum-of-these-six-signal-strengths?
           #:what-eight-capital-letters-appear-on-your-crt?)
  (:documentation
   "Solutions for the Advent of Code 2022 day 10 puzzles found at
https://adventofcode.com/2022/day/10

Part 1 is solved by
  what-is-the-sum-of-these-six-signal-strengths?
and part 2 by
  what-eight-capital-letters-appear-on-your-crt?.

Both look for a file named day-10 in the *INPUT-DIRECTORY*.

Depends on the split-sequence library: \(ql:quickload \"split-sequence\"\)

See also"))

(cl:in-package #:aoc-2022-day-10)

(defun read-instructions (stream)
  (loop for line = (read-line stream nil nil)
        while line
        collect (let ((instr (split-sequence:split-sequence #\Space line
                                                            :remove-empty-subseqs t)))
                  (if (second instr)
                      (list :addx (parse-integer (second instr)))
                      (list :noop)))))

(defun run-machine (instructions times)
  (let ((cycle 1)
        (x 1)
        (log-cycles (sort times #'<))
        (logged-x-values (list)))

    (flet ((increment-cycle ()
             (when (and log-cycles (= cycle (first log-cycles)))
               (pop log-cycles)
               (push x logged-x-values))
             (incf cycle)
             (values)))
      
      (loop for instruction in instructions
            while log-cycles
            
            do (increment-cycle)
            
            do (when (eql :addx (first instruction))
                 (increment-cycle)
                 (incf x (second instruction)))))

    (reverse logged-x-values)))

(defun sum-signal-strengths (instructions times)
  (reduce #'+ (mapcar #'* times (run-machine instructions times))))

(defun what-is-the-sum-of-these-six-signal-strengths? (&optional filename)
  (with-open-input (stream (or filename "day-10"))
    (sum-signal-strengths (read-instructions stream) (list 20 60 100 140 180 220))))


(defun make-crt (&key (height 6) (width 40))
  (make-array (list height width) :element-type 'character :initial-element #\.))

(defun pprint-crt (crt &optional (stream *standard-output*))
  (pprint-logical-block (stream nil)
    (pprint-indent :current 0 stream)
    (loop with rows = (array-dimension crt 0)
          with columns = (array-dimension crt 1)
          for row from 0 below rows
          do (loop initially (pprint-newline :mandatory stream)
                   for column from 0 below columns
                   do (princ (aref crt row column) stream)))))

(defun draw (x-values crt)
  (loop with row = -1
        for x in x-values
        for column = 0 then (if (= column (1- (array-dimension crt 1)))
                                0
                                (1+ column))
        do (when (= column 0)
             (incf row))
        do (when (and (<= (1- column) x (1+ column)))
             (setf (aref crt row column) #\#))
        finally (return crt)))

(defun what-eight-capital-letters-appear-on-your-crt? (&optional filename)
  (with-open-input (stream (or filename "day-10"))
    (format t "~&")
    (pprint-crt (draw (run-machine (read-instructions stream)
                                   (loop for i from 1 upto 240 collect i))
                      (make-crt)))))
