
(cl:in-package #:advent-of-code)

(defparameter *sample-06*
  '("Time:      7  15   30"
    "Distance:  9  40  200"))

(defparameter *input-06*
  (read-input-file-lines :day-number 6 :year-number 2023))

(defun parse-input-06 (input)
  (loop for line in input
        collect (parse-numbers (rest (split #\Space line)))))

(defun distance (total-time charge-time)
  (* (- total-time charge-time) charge-time))

(defun minimum-charge-time-to-beat-distance (total-time distance &optional maximum-charge-time)
  (loop for charge-time from 0 upto (or maximum-charge-time total-time)
        for test-distance = (distance total-time charge-time)
        when (> test-distance distance)
        do (return charge-time)))

(defun maximum-charge-time-to-beat-distance (total-time distance &optional minimum-charge-time)
  (loop for charge-time from total-time downto (or minimum-charge-time 0)
        for test-distance = (distance total-time charge-time)
        when (> test-distance distance)
        do (return charge-time)))

(defun ways-to-beat-distance (total-time distance)
  (let ((min-charge-time (minimum-charge-time-to-beat-distance total-time distance)))
    (if min-charge-time
        (1+ (- (maximum-charge-time-to-beat-distance total-time distance) min-charge-time))
        0)))
    
(defun parse-input-06-part-2 (input)
  (loop for line in input
        collect (parse-integer (apply #'concatenate 'string (rest (split #\Space line))))))
  
(defun compute-part-1 (input)
  (let ((in (parse-input-06 input)))
    (reduce #'* (map 'list #'ways-to-beat-distance (first in) (second in)))))

(defun compute-part-2 (input)
  (let ((in (parse-input-06-part-2 input)))
    (ways-to-beat-distance (first in) (second in))))
