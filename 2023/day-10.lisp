
(cl:in-package #:advent-of-code)

(defparameter *sample-10-1*
  '("-L|F7"
    "7S-7|"
    "L|7||"
    "-L-J|"
    "L|-JF"))

(defparameter *sample-10-2*
  '("7-F7-"
    ".FJ|7"
    "SJLL7"
    "|F--J"
    "LJ.LJ"))

(defparameter *input-10*
  (read-input-file-lines :day-number 10 :year-number 2023))


;;; Directions and locations are complex numbers. The realpart is the X
;;; coordinate, and the imagpart is the Y coordinate.
(defun x (c)
  "Return the X coordinate part of the complex number C."
  (realpart c))

(defun y (c)
  "Return the Y coordinate part of the complex number C."
  (imagpart c))

(defun make-location (x y)
  (complex x Y))

(defun x-axis (map)
  (array-dimension map 0))

(defun y-axis (map)
  (array-dimension map 1))

(defun lookup (location map)
  (aref map (x location) (y location)))

;;; Directions. Add a direction to a location to get a new location.
(defparameter *north* #c(0 -1))
(defparameter *east* #c(1 0))
(defparameter *south* #c(0 1))
(defparameter *west* #c(-1 0))

;;; Tiles. Each tile is a list of directions.
(defparameter *north-south* (list *north* *south*))
(defparameter *east-west* (list *east* *west*))
(defparameter *north-east* (list *north* *east*))
(defparameter *north-west* (list *north* *west*))
(defparameter *south-west* (list *south* *west*))
(defparameter *south-east* (list *south* *east*))
(defparameter *ground* nil)
(defparameter *start-location* (list *north* *east* *south* *west*))

(defparameter *pipe-tiles* (list *north-south* *east-west* *south-east*
                                 *north-east* *south-west* *south-east*))

(defun lookup-tile (character)
  (case character
    (#\| *north-south*)
    (#\- *east-west*)
    (#\L *north-east*)
    (#\J *north-west*)
    (#\7 *south-west*)
    (#\F *south-east*)
    (#\. *ground*)
    (#\S *start-location*)))

(defun reverse-lookup-tile (tile)
  (cond
    ((eql tile *north-south*) #\|)
    ((eql tile *east-west*) #\-)
    ((eql tile *north-east*) #\L)
    ((eql tile *north-west*) #\J)
    ((eql tile *south-west*) #\7)
    ((eql tile *south-east*) #\F)
    ((eql tile *ground*) #\.)
    ((eql tile *start-location*) #\S)))

(defun north-south-p (location map)
  (eql (lookup location map) *north-south*))

(defun north-west-p (location map)
  (eql (lookup location map) *north-west*))

(defun south-west-p (location map)
  (eql (lookup location map) *south-west*))

(defun north-east-p (location map)
  (eql (lookup location map) *north-east*))

(defun south-east-p (location map)
  (eql (lookup location map) *south-east*))

(defun start-location-p (tile)
  (eql tile *start-location*))

(defun location-in-bounds-p (location map)
  (array-in-bounds-p map (x location) (y location)))

(defun mark (location map &optional (mark t))
  (setf (aref map (x location) (y location)) mark))

(defun make-map (lines)
  (make-array (list (length (first lines)) (length lines))))

(defun make-visited-map (map)
  (make-array (list (x-axis map) (y-axis map)) :initial-element nil))

(defun visitedp (location visited)
  (lookup location visited))

(defun destinations (location map)
  "Return a list of locations that can be reached from LOCATION on MAP."
  (remove-if #'(lambda (destination)
                 (not (location-in-bounds-p destination map)))
             (map 'list
                  #'(lambda (direction)
                      (+ location direction))
                  (lookup location map))))

(defun destinations-from-start (start map)
  "Return a list of locations that are destinations from START, and that has
START as one of their destinations. There could be more than two."
  (remove-if #'(lambda (destination)
                 (not (find start (destinations destination map) :test #'=)))
             (destinations start map)))

(defun compute-start-tile (start map)
  "Return the tile that belongs on the START location on MAP. This returns the
first tile in *PIPE-TILES* that fits if there are more than one possibility."
  (loop with frontier = (destinations-from-start start map)
        for tile in *pipe-tiles*
        for destinations = (map 'list
                                #'(lambda (direction)
                                    (+ start direction))
                                tile)
        when (and (find (first frontier) destinations :test #'=)
                  (find (second frontier) destinations :test #'=))
        do (return tile)))

(defun parse-map (lines)
  (let ((start (make-location 0 0))
        (map (make-map lines)))
    (loop for y from 0 below (length lines)
          for line in lines
          do (loop for x from 0 below (length line)
                   for char across line
                   for tile = (lookup-tile char)
                   do (mark (make-location x y) map tile)
                   when (start-location-p tile)
                   do (setf start (make-location x y))))
    (mark start map (compute-start-tile start map))
    (values start map)))

(defun destinations-from-frontier (frontier map visited)
  "Return a list of locations that can be reached directly from FRONTIER and
that has not been visited before."
  (remove-if #'(lambda (location)
                 (visitedp location visited))
             (flatten (map 'list
                           #'(lambda (location)
                               (destinations location map))
                           frontier))))

(defun mark-as-visited (frontier visited)
  "Mark every location in FRONTIER on the VISITED map."
  (loop for location in frontier
        do (mark location visited)))

(defun compute-point-farthest-from-start (start map)
  ;; Assumes the START location on MAP has been marked with the correct tile.
  ;; Walks two ways from start until frontier contains duplicate locations.
  (let ((visited (make-visited-map map)))
    (values (loop for steps = 0 then (1+ steps)
                  for frontier = (list start) then (destinations-from-frontier frontier map visited)
                  for duplicates = (duplicates frontier #'=)
        
                  do (mark-as-visited frontier visited)

                  when duplicates
                  do (return steps))
            visited)))

(defun solve-day-10-part-1 (lines)
  (multiple-value-bind (start map)
      (parse-map lines)
    (nth-value 0 (compute-point-farthest-from-start start map))))

(defun count-enclosed-tiles (map visited)
  ;; Needs a MAP where the start location has been marked with the correct tile.
  ;; VISITED is the second result from COMPUTE-POINT-FARTHEST-FROM-START and
  ;; marks the enclosing loop.
  ;; Uses the Ray Casting Algorithm from https://en.wikipedia.org/wiki/Point_in_polygon.
  (let ((enclosed-tiles 0))
    (loop for y from 0 below (y-axis visited)

          do (loop with start-of-line = nil ; used to check if 2 corners make a vertical line.
                   with line-intersections = 0 ; odd number means inside the loop.

                   for x from 0 below (x-axis visited)
                   for location = (make-location x y)

                   do (if (visitedp location visited)
                          (cond ((or (north-east-p location map)  ; L 
                                     (south-east-p location map)) ; F
                                 (setf start-of-line location))
                                ((north-west-p location map) ; FJ makes a vertical line.
                                 (when (south-east-p start-of-line map)
                                   (incf line-intersections))
                                 (setf start-of-line nil))
                                ((south-west-p location map) ; L7 makes a vertical line.
                                 (when (north-east-p start-of-line map)
                                   (incf line-intersections))
                                 (setf start-of-line nil))
                                ((north-south-p location map)
                                 (incf line-intersections)))
                          (when (oddp line-intersections)
                            (incf enclosed-tiles)))))
    enclosed-tiles))

(defun solve-day-10-part-2 (lines)
  (multiple-value-bind (start map)
      (parse-map lines)
    (count-enclosed-tiles map (nth-value 1 (compute-point-farthest-from-start start map)))))
