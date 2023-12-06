
(cl:in-package #:advent-of-code)

(defparameter *sample-04*
  (list "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
        "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
        "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
        "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
        "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
        "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"))

(defparameter *input-04*
  (read-input-file-lines :day-number 4 :year-number 2023))


(defun list-numbers (numbers)
  (map 'list #'parse-integer (split #\Space numbers)))

(defun split-winner/card-numbers (numbers)
  (let ((ns (split #\| numbers)))
    (values (list-numbers (first ns))
            (list-numbers (second ns)))))

(defun parse-card (line)
  (split-winner/card-numbers (second (split #\: line))))

(defun have-winning-numbers (line)
  (multiple-value-bind (winners have)
      (parse-card line)
    (intersection winners have :test #'=)))

(defun card-score (line)
  (let ((ns (have-winning-numbers line)))
    (if ns
        (expt 2 (1- (length ns)))
        0)))

(defun total-card-score (input)
  (reduce #'+
          (map 'list
               #'card-score
               input)))

(defun make-copy-counts (input)
  (make-array (length input) :element-type 'integer :initial-element 1))

(defun number-of-cards-copied (line)
  (length (have-winning-numbers line)))

(defun compute-copies (input)
  (loop with counts = (make-copy-counts input)
        for i from 0 below (length input)
        for line in input
        for card-count across counts
        for copy-length = (number-of-cards-copied line)
        do (loop for count-index from (1+ i) below (min (+ 1 i copy-length) (length counts))
                 do (incf (aref counts count-index) card-count))
        finally (return (reduce #'+ counts))))
