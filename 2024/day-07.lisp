
(cl:in-package #:advent-of-code)

(defun parse-equation (line)
  (let ((e (split #\: line)))
    (cons (parse-integer (first e))
          (map 'list #'parse-integer (split #\Space (second e))))))

(defparameter *equations* (with-input-file-lines (lines :day-number 7 :year-number 2024)
                            (map 'list #'parse-equation lines)))


(defun try (target total remaining)
  (if (null remaining)
      (= target total)
      (when (<= total target)
        (or (try target (* total (first remaining)) (rest remaining))
            (try target (+ total (first remaining)) (rest remaining))))))

(defun calibration-result (equation)
  (if (try (car equation) (cadr equation) (cddr equation))
      (car equation)
      0))

(defun total-calibration-result (equations)
  (reduce #'+ (map 'list #'calibration-result equations)))


(defun concat (n1 n2)
  (parse-integer (format nil "~D~D" n1 n2)))

(defun try/concat (target total remaining)
  (if (null remaining)
      (= target total)
      (when (<= total target)
        (or (try/concat target (concat total (first remaining)) (rest remaining))
            (try/concat target (* total (first remaining)) (rest remaining))
            (try/concat target (+ total (first remaining)) (rest remaining))))))

(defun calibration-result/concat (equation)
  (if (try/concat (car equation) (cadr equation) (cddr equation))
      (car equation)
      0))

(defun total-calibration-result/concat (equations)
  (reduce #'+ (map 'list #'calibration-result/concat equations)))
