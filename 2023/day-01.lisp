
(cl:in-package #:advent-of-code)

(defun digit (char)
  (and (digit-char-p char)
       char))

(defun parse-line (string)
  (remove-if #'null (map 'list #'digit string)))

(defun combine-first-and-last (list)
  (concatenate 'string (string (first list)) (string (first (last list)))))

(defun compute-calibration-value (string)
  (parse-integer (combine-first-and-last (parse-line string))))

(defun sum-calibration-values (lines)
  (reduce #'+ (map 'list #'compute-calibration-value lines)))

(defparameter *names* '(("one" . "1")
                        ("two" . "2")
                        ("three" . "3")
                        ("four" . "4")
                        ("five" . "5")
                        ("six" . "6")
                        ("seven" . "7")
                        ("eight" . "8")
                        ("nine" . "9")))


(defun digit/name-p (string start)
  (if (digit-char-p (char string start))
      (char string start)
      (loop with max = (- (length string) start)
            for name in *names*
            when (and (<= (length (first name)) max)
                      (string= (first name) string
                               :start2 start :end2 (+ start (length (first name)))))
            do (return (char (rest name) 0)))))

(defun extract-digits (string)
  (loop for i from 0 below (length string)
        for digit = (digit/name-p string i)
        when digit
        collect digit))

(defun compute-calibration-value (string)
  (parse-integer (combine-first-and-last (extract-digits string))))

(defun sum-calibration-values (lines)
  (reduce #'+ (map 'list #'compute-calibration-value lines)))
