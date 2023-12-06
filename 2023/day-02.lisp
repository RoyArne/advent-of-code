
(cl:in-package #:advent-of-code)
      
(defparameter *sample-02*
  '("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
    "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
    "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
    "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
    "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"))

(defun parse-input-02-line (line)
  (declare (type string line))
  (flet ((game-id (l)
           (cl-ppcre:register-groups-bind ((#'parse-integer id))
               ("Game (\\d+)" (first l))
             id))
         (game-draws (l)
           (split #\; (second l)))
         (draw-cubes (d)
           (loop for cube in (split #\, d)
                 for elts = (split #\Space cube)
                 collect (list (parse-integer (first elts))
                               (cond
                                 ((string= "red" (second elts)) 'red)
                                 ((string= "green" (second elts)) 'green)
                                 ((string= "blue" (second elts)) 'blue))))))
    (let ((game-line (split #\: line)))
      (list (game-id game-line)
            (loop for draw in (game-draws game-line)
                  collect (draw-cubes draw))))))

(defparameter *input-02*
  (read-input-file-lines :day-number 2 :year-number 2023 :parse-line #'parse-input-02-line))

(defun possible-game-p (draws max-red max-green max-blue)
  (loop with green = 0
        with red = 0
        with blue = 0
        for draw in draws
        do (loop for cube in draw
                 do (case (second cube)
                      (green (setf green (max green (first cube))))
                      (red (setf red (max red (first cube))))
                      (blue (setf blue (max blue (first cube))))))
        when (or (> green max-green)
                 (> red max-red)
                 (> blue max-blue))
        do (return nil)
        finally (return t)))
  
(defun sum-possible-game-ids (games red green blue)
  (loop for line in games
        when (possible-game-p (second line) red green blue)
        summing (first line)))
        
(defun maximum-cubes-of-color (color draws)
  (apply #'max
         (loop for draw in draws
               collect (or (first (find color draw :key #'second))
                           0))))

(defun power-of-minimum-set (game)
  (apply #'*
         (list (maximum-cubes-of-color 'red game)
               (maximum-cubes-of-color 'green game)
               (maximum-cubes-of-color 'blue game))))

(defun sum-of-powers (games)
  (loop for game in games
        summing (power-of-minimum-set (second game))))
