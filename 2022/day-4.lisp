
(cl:in-package #:advent-of-code)

(defun read-assignment-pairs ()
  (with-open-input (stream "day-4")
    (loop for line = (read-line stream nil nil)
          while line
          collect (cl-ppcre:register-groups-bind ((#'parse-integer start1 end1 start2 end2))
	              ("(\\d+)-(\\d+),(\\d+)-(\\d+)" line)
                    (list start1 end1 start2 end2)))))

(defun containsp (start1 end1 start2 end2)
  (and (<= start1 start2)
       (>= end1 end2)))
  
(defun fully-contains-p (assignment-pair)
  (destructuring-bind (start1 end1 start2 end2)
      assignment-pair
    (or (containsp start1 end1 start2 end2)
        (containsp start2 end2 start1 end1))))

(defun count-fully-contained-pairs ()
  "In how many assignment pairs does one range fully contain the other?"
  (count-if #'fully-contains-p (read-assignment-pairs)))

(defun overlapsp (start1 end1 start2 end2)
  (and (<= start1 end2)
       (<= start2 end1)))

(defun pair-overlaps-p (assignment-pair)
  (destructuring-bind (start1 end1 start2 end2)
      assignment-pair
    (overlapsp start1 end1 start2 end2)))

(defun count-overlapping-pairs ()
  "In how many assignment pairs do the ranges overlap?"
  (count-if #'pair-overlaps-p (read-assignment-pairs)))
