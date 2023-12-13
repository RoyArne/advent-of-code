
(cl:in-package #:advent-of-code)

(defparameter *sample-11*
  '("...#......"
    ".......#.."
    "#........."
    ".........."
    "......#..."
    ".#........"
    ".........#"
    ".........."
    ".......#.."
    "#...#....."))

(defparameter *input-11*
  (read-input-file-lines :day-number 11 :year-number 2023))

(defparameter *part-1-expansion* 2)
(defparameter *part-2-expansion* 1000000)

(defun empty-row-p (line)
  (every #'(lambda (c)
             (char= #\. c))
         line))
             
(defun empty-column-p (column-number lines)
  (every #'(lambda (line)
             (char= #\. (char line column-number)))
         lines))

(defun x (c)
  "Return the X coordinate part of the complex number C."
  (realpart c))

(defun y (c)
  "Return the Y coordinate part of the complex number C."
  (imagpart c))

(defun make-point (x y)
  (complex x y))

(defun manhattan-distance (point1 point2)
  (+ (abs (- (x point1) (x point2)))
     (abs (- (y point1) (y point2)))))

(defun parse-image (lines expansion-factor)
  ;; we increment expansion by (1- expansion-factor) because the y and x counts
  ;; are included in the final coordinate.
  (flatten (loop with y-expansion = 0

                 for y from 0 below (length lines)
                 for line in lines

                 when (empty-row-p line)
                 do (incf y-expansion (1- expansion-factor))
                 else
                 collect (loop with x-expansion = 0

                               for x from 0 below (length line)
                              
                               when (empty-column-p x lines)
                               do (incf x-expansion (1- expansion-factor))

                               when (char= #\# (char line x))
                               collect (make-point (+ x x-expansion) (+ y y-expansion))))))

(defun sum-shortest-pair-paths (points)
  (reduce #'+
          (flatten (loop for p1 in points
                         for remaining-points = (remove p1 points :test #'=)
                         then (remove p1 remaining-points :test #'=)
                         collect (loop for p2 in remaining-points
                                       collect (manhattan-distance p1 p2))))))

(defun solve-day-11-part-1 (lines)
  (sum-shortest-pair-paths (parse-image lines *part-1-expansion*)))

(defun solve-day-11-part-2 (lines)
  (sum-shortest-pair-paths (parse-image lines *part-2-expansion*)))
