
(cl:in-package #:advent-of-code)

;;; Day 2 puzzles
;;;
;;; https://adventofcode.com/2022/day/2
;;;
;;; Part 1
(defun lookup-opponents-shape (char)
  (ecase char
    (#\A 'rock)
    (#\B 'paper)
    (#\C 'scissors)))

(defun lookup-my-shape (char)
  (ecase char
    (#\X 'rock)
    (#\Y 'paper)
    (#\Z 'scissors)))

(defun read-strategy-guide (stream)
  "Return a list of game strategies.

Each strategy is a list of two elements where the first element is the
opponents hand and the second element is my hand."
  (loop for line = (read-line stream nil nil)
        while line
        collect (list (lookup-opponents-shape (char line 0))
                      (lookup-my-shape (char line 2)))))

(defun outcome (shape1 shape2)
  "Return the outcome for the player throwing SHAPE1."
  (ecase shape1
    (rock (ecase shape2
            (rock 'draw)
            (paper 'lost)
            (scissors 'won)))
    (paper (ecase shape2
             (rock 'won)
             (paper 'draw)
             (scissors 'lost)))
    (scissors (ecase shape2
                (rock 'lost)
                (paper 'won)
                (scissors 'draw)))))

(defun shape-score (shape)
  (ecase shape
    (rock 1)
    (paper 2)
    (scissors 3)))

(defun outcome-score (outcome)
  (ecase outcome
    (lost 0)
    (draw 3)
    (won 6)))

(defun strategy-score (shape outcome)
  (+ (shape-score shape) (outcome-score outcome)))

(defun opponents-strategy (strategy)
  (first strategy))

(defun my-strategy (strategy)
  (second strategy))

(defun score-according-to-strategy-guide ()
  "What would your total score be if everything goes exactly according to your
strategy guide?"
  (with-open-input (stream "day-2")
    (reduce #'+
            (loop for s in (read-strategy-guide stream)
                  collecting (strategy-score (my-strategy s)
                                             (outcome (my-strategy s)
                                                      (opponents-strategy s)))))))



;;; Part 2
(defun required-outcome (char)
  (ecase char
    (#\X 'lost)
    (#\Y 'draw)
    (#\Z 'won)))

(defun read-required-outcomes (stream)
  "Return a list of game strategies.

Each strategy is a list of two elements where the first element is the
opponents hand and the second element is the required outcome."
  (loop for line = (read-line stream nil nil)
        while line
        collect (list (lookup-opponents-shape (char line 0))
                      (required-outcome (char line 2)))))

(defun shape-for-outcome (opponents-shape outcome)
  (ecase opponents-shape
    (rock (ecase outcome
            (draw 'rock)
            (lost 'scissors)
            (won 'paper)))
    (paper (ecase outcome
             (draw 'paper)
             (lost 'rock)
             (won 'scissors)))
    (scissors (ecase outcome
                (draw 'scissors)
                (lost 'paper)
                (won 'rock)))))

(defun score-according-to-required-outcomes ()
  "What would your total score be if everything goes exactly according to your
strategy guide?"
  (with-open-input (stream "day-2")
    (reduce #'+
            (loop for s in (read-required-outcomes stream)
                  collecting (strategy-score (shape-for-outcome (opponents-strategy s)
                                                                (my-strategy s))
                                             (my-strategy s))))))
