
(cl:in-package #:advent-of-code)

(defparameter *test*
  '("47|53"
    "97|13"
    "97|61"
    "97|47"
    "75|29"
    "61|13"
    "75|53"
    "29|13"
    "97|29"
    "53|29"
    "61|53"
    "97|53"
    "61|29"
    "47|13"
    "75|47"
    "97|75"
    "47|61"
    "75|61"
    "47|29"
    "75|13"
    "53|13"
    ""
    "75,47,61,53,29"
    "97,61,53,29,13"
    "75,29,13"
    "75,97,47,61,53"
    "61,13,29"
    "97,13,75,29,47"))

(defparameter *rules* '())

(defparameter *pages* '())

(defun rules (lines)
  (flet ((rulep (line)
           (find #\| line))
         (parse-rule (line)
           (map 'list
                #'parse-integer
                (nth-value 1 (cl-ppcre:scan-to-strings "(\\d+)\\|(\\d+)" line)))))
    (map 'list
         #'parse-rule
         (remove-if (complement #'rulep) lines))))

(defun pages (lines)
  (flet ((pagesp (line)
           (find #\, line))
         (parse-pages (line)
           (map 'list
                #'parse-integer
                (split #\, line))))
    (map 'list
         #'parse-pages
         (remove-if (complement #'pagesp) lines))))

(defun load-day-05-2024 ()
  (with-input-file-lines (lines :day-number 5 :year-number 2024)
    (setf *rules* (rules lines)
          *pages* (pages lines))
    (values *rules* *pages*)))



(defun predecessor (rule)
  (first rule))

(defun successor (rule)
  (second rule))

(defun predecessors (page)
  "Return a list of pages that must be ordered before PAGE."
  (loop for rule in *rules*
        when (= page (successor rule))
        collect (predecessor rule)))

(defun successors (page)
  "Return a list of pages that must be ordered after PAGE."
  (loop for rule in *rules*
        when (= page (predecessor rule))
        collect (successor rule)))

(defun invalid-predecessor-p (page predecessors)
  "True if any successor of PAGE can be found in PREDECESSORS."
  (loop for s in (successors page)
        when (find s predecessors :test #'=)
        do (return t)))

(defun invalid-successor-p (page successors)
  "True if any predecessor of PAGE can be found in SUCCESSORS."
  (loop for p in (predecessors page)
        when (find p successors :test #'=)
        do (return t)))

(defun correct-order-p (successors &optional page predecessors)
  (cond
    ((null page) ; initial call
     (correct-order-p (rest successors) (first successors)))
    ((null successors) ; final call
     (not (invalid-predecessor-p page predecessors)))
    (t
     (and (not (invalid-predecessor-p page predecessors))
          (not (invalid-successor-p page successors))
          (correct-order-p (rest successors) (first successors) (cons page predecessors))))))

(defun sum-middle-numbers (list)
  (flet ((middle-number (pages)
           (nth (floor (length pages) 2) pages)))
    (reduce #'+ (map 'list #'middle-number list))))

(defun correct-ones (list)
  (loop for pages in list
        when (correct-order-p pages)
        collect pages))

(defun compute-part1 ()
  (sum-middle-numbers (correct-ones *pages*)))


(defun incorrect-ones (list)
  (loop for pages in list
        unless (correct-order-p pages)
        collect pages))

(defun beforep (p s)
  (or (find s (successors p) :test #'=)
      (find p (predecessors s) :test #'=)))

(defun sort-pages (pages)
  (sort pages #'beforep))

(defun compute-part2 ()
  (sum-middle-numbers (map 'list #'sort-pages (incorrect-ones *pages*))))
