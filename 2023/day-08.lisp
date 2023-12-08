
(cl:in-package #:advent-of-code)

(defparameter *sample-08-1*
  '("RL"
    ""
    "AAA = (BBB, CCC)"
    "BBB = (DDD, EEE)"
    "CCC = (ZZZ, GGG)"
    "DDD = (DDD, DDD)"
    "EEE = (EEE, EEE)"
    "GGG = (GGG, GGG)"
    "ZZZ = (ZZZ, ZZZ)")
  "Sample 1 ends up at ZZZ after going through the instructions once.")

(defparameter *sample-08-2*
  '("LLR"
    ""
    "AAA = (BBB, BBB)"
    "BBB = (AAA, ZZZ)"
    "ZZZ = (ZZZ, ZZZ)")
  "Sample 2 ends up at ZZZ after going through the instructions twice.")

(defparameter *sample-08-3*
  '("LR"
    ""
    "11A = (11B, XXX)"
    "11B = (XXX, 11Z)"
    "11Z = (11B, XXX)"
    "22A = (22B, XXX)"
    "22B = (22C, 22C)"
    "22C = (22Z, 22Z)"
    "22Z = (22B, 22B)"
    "XXX = (XXX, XXX)"))

(defparameter *input-08*
  (read-input-file-lines :day-number 8 :year-number 2023))

(defun parse-instructions (line)
  line)

(defstruct (place (:type list)
                  (:conc-name nil))
  (label)
  (left)
  (right))

(defun parse-place (line)
  (split #\Space (remove-if #'(lambda (char)
                                (member char '(#\= #\( #\, #\)) :test #'char=))
                            line)))

(defun parse-map (lines)
  (loop with map = (make-hash-table :test #'equal)
        for place in (map 'list #'parse-place lines)
        do (setf (gethash (label place) map) place)
        finally (return map)))

(defun parse-input-08 (lines)
  (values (parse-instructions (first lines))
          (parse-map (rest (rest lines)))))

(defun lookup-place (label map)
  (gethash label map))

(defun move (direction from map)
  (lookup-place (case direction
                  (#\L (left from))
                  (#\R (right from)))
                map))

(defun follow-directions (directions from map)
  (loop for direction across directions
        for place = (move direction from map)
        then (move direction place map)
        finally (return place)))

(defun part-1-target-p (place)
  (string= (label place) "ZZZ"))

(defun solve-day-08-part-1 (lines)
  (multiple-value-bind (directions map)
      (parse-input-08 lines)
    (loop with steps = (length directions)
          for place = (follow-directions directions (lookup-place "AAA" map) map)
          then (follow-directions directions place map)
          until (part-1-target-p place)
          do (incf steps (length directions))
          finally (return steps))))

(defun last-char= (string char)
  (char= char (char string (1- (length string)))))

(defun part-2-target-p (place)
  (last-char= (label place) #\Z))

(defun places-that-end-with-char (map char)
  (loop for k being the hash-key using (hash-value v) of map
        when (last-char= k char)
        collect v))

(defun path-to-target (directions from map targetp)
  (loop for step = (follow-directions directions from map)
        then (follow-directions directions step map)
        collecting step
        until (funcall targetp step)))

(defun compute-path-table (directions map targetp)
  (loop with table = (make-hash-table :test #'equal)
        for place being the hash-values in map
        for path = (path-to-target directions place map targetp)
        do (setf (gethash (label place) table)
                 (list (length path) (first (last path))))
        finally (return table)))

(defun paths-from-places (table places)
  (loop for place in places
        collect (gethash (label place) table)))
          

;;; Brute force solution didn't work well for part 2, so I looked at some of
;;; the data.

(defparameter *directions* nil
  "A string of L and R characters.")

(defparameter *map* nil
  "A list of PLACE structures.")

(multiple-value-bind (directions map)
		    (parse-input-08 *input-08*)
		  (setf *directions* directions
			*map* map))

(defparameter *table* (compute-path-table *directions* *map* #'part-2-target-p)
  "A hash table using PLACE labels as key and values being a list. The list
contains the number of iterations through *DIRECTIONS* needed to reach the
following PLACE.")

(defparameter *initial-places*
  (places-that-end-with-char *map* #\A)
  "A list of PLACE structures. We begin at these.")

(defparameter *paths-from-initial-places*
  (paths-from-places *table* *initial-places*)
  "A list of paths from *INITIAL-PLACES* to *FINAL-PLACES*.")

(defparameter *final-places*
  (places-that-end-with-char *map* #\Z)
  "A list of PLACE structures. We want to end up at these.")

(defparameter *paths-from-final-places*
  (paths-from-places *table* *final-places*)
  "A list of paths from *FINAL-PLACES* to *FINAL-PLACES*.")

;;; I wanted to find a cycle length, where *PATHS-FROM-INITIAL-PLACES* all
;;; meet with the same number of iterations through *DIRECTIONS*. Turns out
;;; all path lengths are prime numbers, and *PATHS-FROM-FINAL-PLACES* has the
;;; same paths as *PATHS-FROM-INITIAL-PLACES*.

(defun compute-day-08-part-2-solution ()
  "Finds the least common multiple of the number of iterations through
*DIRECTIONS* for the paths in *PATHS-FROM-INITIAL-PLACES*, and multiplies it
by the length of *DIRECTIONS*.

The least common multiple of iterations through *DIRECTIONS* is where all the
paths end up at a target PLACE toghether."
  (* (reduce #'* (map 'list #'first *paths-from-initial-places*))
     (length *directions*)))
