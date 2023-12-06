
(cl:in-package #:advent-of-code)
      
(defparameter *sample-input-03*
  (coerce '("467..114.."
            "...*......"
            "..35..633."
            "......#..."
            "617*......"
            ".....+.58."
            "..592....."
            "......755."
            "...$.*...."
            ".664.598..")
          'vector))

(defparameter *input-03*
  (coerce (read-input-file-lines :day-number 3 :year-number 2023) 'vector))

(defun schematic-digit-p (input line column)
  (digit-char-p (char (aref input line) column)))

(defun schematic-symbol-p (input line column)
  (and (char/= #\. (char (aref input line) column))
       (not (schematic-digit-p input line column))))

(defun adjacent-lines (input line)
  (let ((max (length input)))
    (remove-if #'(lambda (l)
                   (or (minusp l)
                       (>= l max)))
               (list (1- line) line (1+ line)))))

(defun adjacent-columns (line column)
  (let ((max (length line)))
    (remove-if #'(lambda (c)
                   (or (minusp c)
                       (>= c max)))
               (list (1- column) column (1+ column)))))

(defun adjacent-to-symbol-p (input line column)
  (loop with columns = (adjacent-columns (aref input line) column)
        for l in (adjacent-lines input line)
        when (find-if #'(lambda (c)
                          (schematic-symbol-p input l c))
                      columns)
        do (return t)))

(defun part-number-start (input line column)
  (1+ (or (position-if-not #'digit-char-p (aref input line) :end column :from-end t)
          -1)))

(defun part-number-end (input line column)
  (or (position-if-not #'digit-char-p (aref input line) :start column)
      (length (aref input line))))

(defun part-numbers-in-line (input line)
  (loop for column from 0 below (length (aref input line))
        when (and (schematic-digit-p input line column)
                  (adjacent-to-symbol-p input line column))
        collect (let ((start (part-number-start input line column))
                      (end (part-number-end input line column)))
                  (setf column end)
                  (subseq (aref input line) start end))))

(defun part-numbers-in-input (input)
  (map 'list
       #'parse-integer
       (apply #'append
              (remove-if #'null
                         (loop for line from 0 below (length input)
                               collect (part-numbers-in-line input line))))))

(defun sum-part-numbers-in-input (input)
  (reduce #'+ (part-numbers-in-input input)))

(defun schematic-gear-p (input line column)
  (char= #\* (char (aref input line) column)))

(defun adjacent-to-gear-p (input line column)
  (loop with columns = (adjacent-columns (aref input line) column)
        for l in (adjacent-lines input line)
        when (find-if #'(lambda (c)
                          (schematic-gear-p input l c))
                      columns)
        do (return t)))


(defun part-number-locations-in-line (input line)
  (loop for column from 0 below (length (aref input line))
        when (and (schematic-digit-p input line column)
                  (adjacent-to-gear-p input line column))
        collect (let ((start (part-number-start input line column))
                      (end (part-number-end input line column)))
                  (setf column end)
                  (list line start end))))

(defun part-number-locations (input)
  (apply #'append
         (remove-if #'null
                    (loop for line from 0 below (length input)
                          collect (part-number-locations-in-line input line)))))

(defun gear-locations (input)
  (apply #'append
         (remove-if #'null
                    (loop for line from 0 below (length input)
                          collect (remove-if #'null
                                             (loop for column from 0 below (length (aref input line))
                                                   when (schematic-gear-p input line column)
                                                   collect (list line column)))))))

(defun part-number-location (input line column)
  (list line
        (part-number-start input line column)
        (part-number-end input line column)))

(defun location= (loc1 loc2)
  (and (= (first loc1) (first loc2))
       (= (second loc1) (second loc2))
       (= (third loc1) (third loc2))))

(defun adjacent-part-numbers (input line column)
  (remove-duplicates (apply #'append
                            (remove-if #'null
                                       (loop with columns = (adjacent-columns (aref input line) column)
                                             for l in (adjacent-lines input line)
                                             collect (loop for c in columns
                                                           when (digit-char-p (char (aref input l) c))
                                                           collect (part-number-location input l c)))))
                     :test #'location=))

(defun remove-wrong-number-of-adjacent-part-numbers (input)
  (let ((gears (gear-locations input)))
    (remove-if #'(lambda (locs)
                   (/= 2 (length locs)))
               (loop for gear in gears
                     collect (adjacent-part-numbers input (first gear) (second gear))))))

(defun part-number (input loc)
  (parse-integer (subseq (aref input (first loc)) (second loc) (third loc))))

(defun gear-ratio (input locs)
  (* (part-number input (first locs))
     (part-number input (second locs))))

(defun sum-all-gear-ratios (input)
  (reduce #'+
          (map 'list
               #'(lambda (locs)
                   (gear-ratio input locs))
               (remove-wrong-number-of-adjacent-part-numbers input))))
