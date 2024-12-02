
(cl:in-package #:advent-of-code)

(defun load-day-02-2024 ()
  (with-input-file-lines (lines :day-number 2 :year-number 2024 :parse-line #'list-numbers)
    lines))

(defparameter *reports* (load-day-02-2024))

(defun direction (a b)
  (cond
    ((> a b) 1)
    ((< a b) -1)
    (t 0)))

(defun safe-direction-p (a b direction)
  (or (null direction)
      (= (direction a b) direction)))

(defun difference (a b)
  (abs (- a b)))

(defun safe-difference-p (a b)
  (<= 1 (level-difference a b) 3))

(defun safe-transition-p (a b direction)
  (and (safe-direction-p a b direction)
       (safe-difference-p a b)))

(defun safe-report-p (report)
  (labels ((safep (a b remaining direction)
             (or (null b)
                 (and (safe-transition-p a b direction)
                      (safep b (first remaining) (rest remaining) direction)))))
    (let ((a (first report))
          (b (second report))
          (remaining (rest (rest report))))
      (or (null a)
          (null b)
          (safep a b remaining (direction a b))))))

(defun count-safe-reports ()
  (count-if #'safe-report-p *reports*))

(defun safe-report-with-dampener-p (report)
  (or (safe-report-p report)
      (loop for i from 0 below (length report)
            when (safe-report-p (append (subseq report 0 i) (subseq report (1+ i))))
            do (return t))))

(defun count-safe-reports-with-dampener ()
  (count-if #'safe-report-with-dampener-p *reports*))

