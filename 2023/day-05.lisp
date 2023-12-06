
(cl:in-package #:advent-of-code)

(defparameter *sample-05*
  '("seeds: 79 14 55 13"                 ; (55 13) (79 14)
    ""
    "seed-to-soil map:"
    "50 98 2"                            
    "52 50 48"                           ; (55 -> 57 13 -> 13) (79 -> 81 14 -> 14)
    ""
    "soil-to-fertilizer map:"
    "0 15 37"
    "37 52 2"
    "39 0 15"
    ""
    "fertilizer-to-water map:"
    "49 53 8"
    "0 11 42"
    "42 0 7"
    "57 7 4"
    ""
    "water-to-light map:"
    "88 18 7"
    "18 25 70"
    ""
    "light-to-temperature map:"
    "45 77 23"
    "81 45 19"
    "68 64 13"
    ""
    "temperature-to-humidity map:"
    "0 69 1"
    "1 0 69"
    ""
    "humidity-to-location map:"
    "60 56 37"
    "56 93 4"))

(defparameter *input-05*
  (read-input-file-lines :day-number 5 :year-number 2023))

(defun parse-input-05-line (line)
  (let ((elts (split #\Space line)))
    (cond
      ((string-equal "seeds:" (first elts))
       (cons 'seeds (parse-numbers (rest elts))))
      ((string-equal "seed-to-soil" (first elts))
       'seed-to-soil)
      ((string-equal "soil-to-fertilizer" (first elts))
       'soil-to-fertilizer)
      ((string-equal "fertilizer-to-water" (first elts))
       'fertilizer-to-water)
      ((string-equal "water-to-light" (first elts))
       'water-to-light)
      ((string-equal "light-to-temperature" (first elts))
       'light-to-temperature)
      ((string-equal "temperature-to-humidity" (first elts))
       'temperature-to-humidity)
      ((string-equal "humidity-to-location" (first elts))
       'humidity-to-location)
      (t
       (parse-numbers elts)))))

(defclass input-05 ()
  ((seeds :reader seeds
          :initform '())
   (seed-to-soil :reader seed-to-soil
                 :initform '())
   (soil-to-fertilizer :reader soil-to-fertilizer
                       :initform '())
   (fertilizer-to-water :reader fertilizer-to-water
                        :initform '())
   (water-to-light :reader water-to-light
                   :initform '())
   (light-to-temperature :reader light-to-temperature
                         :initform '())
   (temperature-to-humidity :reader temperature-to-humidity
                            :initform '())
   (humidity-to-location :reader humidity-to-location
                         :initform '())))

(defun parse-input-05 (lines)
  (loop with input = (make-instance 'input-05)
        with current-input = 'seeds
        for line in (map 'list #'parse-input-05-line lines)
        do (typecase line
             (symbol (setf current-input line))
             (null (setf current-input nil))
             (list (if (eql 'seeds (first line))
                       (setf (slot-value input 'seeds) (rest line))
                       (push line (slot-value input current-input)))))
        finally (return input)))

(defparameter *parsed-05* (parse-input-05 *sample-05*))

(defstruct (dest/src-range (:type list)
                           (:conc-name nil))
  (destination-start)
  (source-start)
  (dest/src-length))
  

;; (defun destination-start (list)
;;   (first list))

;; (defun source-start (list)
;;   (second list))

;; (defun range-length (list)
;;   (third list))

(defun compute-range-end (start length)
  (+ start (1- length)))

(defun destination-end (range)
  (range-end (destination-start range) (dest/src-length range)))

(defun source-end (range)
  (compute-range-end (source-start range) (dest/src-length range)))

(defun distance-from-source (point range)
  (- point (source-start range)))

(defun destination-value (point range)
  (let ((distance (distance-from-source point range)))
    (when (< -1 distance (dest/src-length range))
      (+ distance (destination-start range)))))

(defun source-to-destination (source destinations)
  (loop for range in destinations
        for destination = (destination-value source range)
        when destination
        do (return destination)
        finally (return source)))

(defparameter *slots*
  '(seed-to-soil soil-to-fertilizer fertilizer-to-water water-to-light light-to-temperature
    temperature-to-humidity humidity-to-location))

(defun trace-location (seed input)
  (loop for slot in *slots*
        for dest = (source-to-destination seed (slot-value input slot))
        then (source-to-destination dest (slot-value input slot))
        finally (return dest)))

(defun part-1-lowest-location (input)
  (first (sort (loop for seed in (seeds input)
                     collect (trace-location seed input))
               #'<)))


;;; PART 2

(defstruct (range (:type list))
  (start)
  (length))

(defun range-end (range)
  (compute-range-end (range-start range) (range-length range)))

(defun make-seed-ranges (seeds)
  (loop for (start length) on seeds by #'cddr
        collect (list start length)))

(defun parse-and-preprocess-input-05 (lines)
  (let ((input (parse-input-05 lines)))
    (setf (slot-value input 'seeds) (sort (make-seed-ranges (seeds input)) #'< :key #'first))
    (loop for slot in *slots*
          do (setf (slot-value input slot) (sort (slot-value input slot) #'< :key #'source-start)))
    input))

(defun part-below (range dest/src-range)
  ;; part below translates to itself
  (when (< (range-start range) (source-start dest/src-range))
    (list (range-start range)
          (min (- (source-start dest/src-range) (range-start range))
               (range-length range)))))

(defun overlapping-part (range dest/src-range)
  (when (and (<= (source-start dest/src-range) (range-end range))
             (<= (range-start range) (source-end dest/src-range)))
    (let* ((start (max (source-start dest/src-range) (range-start range)))
           (length (min (range-length range)
                        (1+ (- (source-end dest/src-range) start)))))
      (list (+ (destination-start dest/src-range) (- start (source-start dest/src-range)))
            length))))

(defun part-above (range dest/src-range)
  ;; part above translates to itself
  (when (< (source-end dest/src-range) (range-end range))
    (list (max (range-start range)
               (1+ (source-end dest/src-range)))
          (min (range-length range)
               (- (range-end range) (source-end dest/src-range))))))


(defun source-range-to-destination-ranges (source destinations)
  (let ((remainder nil))
    (append (loop for dest in destinations
                  for below = (part-below source dest) then (part-below above dest)
                  for overlap = (overlapping-part source dest) then (overlapping-part above dest)
                  for above = (part-above source dest) then (part-above above dest)
                  when below collect below
                  when overlap collect overlap
                  while above
                  finally (when above (setf remainder (list above))))
            remainder)))

(defun sources-to-destinations (sources destinations)
  (sort (apply #'append
               (loop for source in sources
                     collect (source-range-to-destination-ranges source destinations)))
        #'< :key #'range-start))

(defun try2 (input)
  (loop for slot in (cons nil *slots*)
        for source = (seeds input) then (sources-to-destinations source (slot-value input slot))
        finally (return source)))
