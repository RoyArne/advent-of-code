
(cl:in-package #:advent-of-code)

(defun parse-range (line &optional (start 0) end)
  (let ((mid (position #\- line :test #'char= :start start :end end)))
    (cons (parse-integer line :start start :end mid)
          (parse-integer line :start (1+ mid) :end end))))

(defun parse-assignment-pair (line)
  (let ((mid (position #\, line :test #'char=)))
    (list (parse-range line 0 mid)
          (parse-range line (1+ mid)))))
        
(defun read-assignment-pairs ()
  (with-open-input (stream "day-4")
    (loop for line = (read-line stream nil nil)
          while line
          collect (parse-assignment-pair line))))

(defun contained-range-p (range1 range2)
  (and (<= (first range1) (first range2))
       (>= (rest range1) (rest range2))))

(defun fully-contained-range-pair-p (range1 range2)
  (or (contained-range-p range1 range2)
      (contained-range-p range2 range1)))

(defun count-fully-contained-pairs ()
  "In how many assignment pairs does one range fully contain the other?"
  (loop for pair in (read-assignment-pairs)
        count (fully-contained-range-pair-p (first pair) (second pair))))

(defun in-range-p (n range)
  (<= (first range) n (rest range)))

(defun overlapping-range-pair-p (range1 range2)
  (or (in-range-p (first range2) range1)
      (in-range-p (rest range2) range1)
      (in-range-p (first range1) range2)
      (in-range-p (rest range1) range2)))

(defun count-overlapping-pairs ()
  "In how many assignment pairs do the ranges overlap?"
  (loop for pair in (read-assignment-pairs)
        count (overlapping-range-pair-p (first pair) (second pair))))
