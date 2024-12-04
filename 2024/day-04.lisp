
(cl:in-package #:advent-of-code)

(defparameter *text* (with-input-file-lines (lines :day-number 4 :year-number 2024)
                       (coerce lines 'vector)))

(defun in-bounds-p (x y text)
  (and (array-in-bounds-p text y)
       (array-in-bounds-p (aref text y) x)))

(defun found-string-p (string x y delta-x delta-y text)
  (loop for c across string
        for h = x then (+ h delta-x)
        for v = y then (+ v delta-y)
        unless (in-bounds-p h v text)
        do (return nil)
        unless (char= c (char (aref text v) h))
        do (return nil)
        finally (return t)))

(defun count-xmas (text)
  (loop for y from 0 below (length text)
        summing (loop for x from 0 below (length (aref text y))
                      count (found-string-p "XMAS" x y 1 0 text)
                      count (found-string-p "SAMX" x y 1 0 text)
                      count (found-string-p "XMAS" x y 0 1 text)
                      count (found-string-p "SAMX" x y 0 1 text)
                      count (found-string-p "XMAS" x y -1 1 text)
                      count (found-string-p "SAMX" x y -1 1 text)
                      count (found-string-p "XMAS" x y 1 1 text)
                      count (found-string-p "SAMX" x y 1 1 text))))

(defun found-x-mas-p (x y text)
  (and (char= #\A (char (aref text y) x))
       (or (found-string-p "MAS" (1- x) (1- y) 1 1 text)
           (found-string-p "SAM" (1- x) (1- y) 1 1 text))
       (or (found-string-p "MAS" (1+ x) (1- y) -1 1 text)
           (found-string-p "SAM" (1+ x) (1- y) -1 1 text))))

(defun count-x-mas (text)
  (loop for y from 0 below (length text)
        summing (loop for x from 0 below (length (aref text y))
                      count (found-x-mas-p x y text))))
  
