
(cl:in-package #:advent-of-code)

(defparameter *location-list-1* '())
(defparameter *location-list-2* '())

(defun load-2024-day-01! ()
  (with-input-file-lines (lines :day-number 1 :year-number 2024 :parse-line #'list-numbers)
    (loop for (a b) in lines
          collect a into l1
          collect b into l2
          finally (setf *location-list-1* (sort l1 #'<)
                        *location-list-2* (sort l2 #'<))))
  (values *location-list-1* *location-list-2*))

(defun sum-distances ()
  (flet ((distance (a b)
           (abs (- a b))))
    (loop for a in *location-list-1*
          for b in *location-list-2*
          sum (distance a b))))

(defun sum-similarity ()
  (flet ((similarity (a)
           (* a (count a *location-list-2* :test #'=))))
    (loop for a in *location-list-1*
          sum (similarity a))))
