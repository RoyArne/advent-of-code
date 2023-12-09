
(cl:in-package #:advent-of-code)

(defparameter *sample-09*
  '("0 3 6 9 12 15"
    "1 3 6 10 15 21"
    "10 13 16 21 30 45"))

(defparameter *input-09*
  (read-input-file-lines :day-number 9 :year-number 2023))

(defun parse-input-09 (lines)
  (map 'list #'list-numbers lines))

(defun compute-prediction-base (history)
  (flet ((differences (h)
           (loop for (n1 n2) on h while n2 collect (- n2 n1))))
    (reverse (cons history
                   (loop for differences = (differences history) then (differences differences)
                         collecting differences
                         until (every #'zerop differences))))))

(defun predict-next-value (history)
  (flet ((last-number (list)
           (first (last list))))
    (loop for n in (map 'list #'last-number (compute-prediction-base history))
          for bottom = n then (+ bottom n)
          finally (return bottom))))

(defun predict-previous-value (history)
  (loop for n in (map 'list #'first (compute-prediction-base history))
        for bottom = n then (- n bottom)
        finally (return bottom)))
                             
(defun sum-history-part-1 (input)
  (reduce #'+ (map 'list #'predict-next-value (parse-input-09 input))))

(defun sum-history-part-2 (input)
  (reduce #'+ (map 'list #'predict-previous-value (parse-input-09 input))))
