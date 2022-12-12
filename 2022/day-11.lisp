
(cl:defpackage #:aoc-2022-day-11
  (:use #:common-lisp
        #:advent-of-code)
  (:export #:what-is-the-level-of-monkey-business-after-20-rounds?
           #:what-is-the-level-of-monkey-business-after-n-rounds?)
  (:documentation
   "Solutions for the Advent of Code 2022 day  puzzles found at
https://adventofcode.com/2022/day/11

Part 1 is solved by
  what-is-the-level-of-monkey-business-after-20-rounds?
and part 2 by
  what-is-the-level-of-monkey-business-after-n-rounds?

Both look for a file named day-11 in the *INPUT-DIRECTORY*.

Depends on split-sequence and cl-ppcre.

See also"))

(cl:in-package #:aoc-2022-day-11)

;;; Sample input
;;;
;;; Monkey 0:
;;;   Starting items: 79, 98
;;;   Operation: new = old * 19
;;;   Test: divisible by 23
;;;     If true: throw to monkey 2
;;;     If false: throw to monkey 3

(defun read-monkey-id (stream)
  (cl-ppcre:register-groups-bind ((#'parse-integer id))
      ("Monkey (\\d+):" (read-line stream))
    id))

(defun read-starting-items (stream)
  (let ((line (read-line stream)))
    (map 'vector #'parse-integer
         (split #\, (subseq line (1+ (position #\: line :test #'char=)))))))

(defun read-operation (stream)
  (destructuring-bind (var1 operator var2)
      (split #\Space (second (split #\= (read-line stream))))
    (list (cond
            ((string= "*" operator) #'*)
            ((string= "+" operator) #'+)
            (t (error "Unknown operator ~A in the expression ~A ~A ~A."
                      operator var1 operator var2)))
          (unless (every #'alpha-char-p var1) (parse-integer var1))
          (unless (every #'alpha-char-p var2) (parse-integer var2)))))

(defun read-test (stream)
  (list (cl-ppcre:register-groups-bind ((#'parse-integer n))
            ("Test: divisible by (\\d+)" (read-line stream))
          n)
        (cl-ppcre:register-groups-bind ((#'parse-integer id))
            ("  If true: throw to monkey (\\d+)" (read-line stream))
          id)
        (cl-ppcre:register-groups-bind ((#'parse-integer id))
            ("  If false: throw to monkey (\\d+)" (read-line stream))
          id)))

(defclass monkey ()
  ((id :reader monkey-id
       :initarg :id
       :type integer
       :initform (error "The :id initarg is required."))
   (activity :accessor activity-level
             :type (integer 0 *)
             :initform 0)
   (items :reader monkey-items
          :initarg :items
          :type vector
          :initform (error "The :items initarg is required."))
   (operation :reader monkey-operation
              :initarg :operation
              :type list
              :initform (error "The :operation initarg is required."))
   (test :reader monkey-test
         :initarg :test
         :type list
         :initform (error "The :test initarg is required."))
   (divisor :accessor monkey-divisor
            :type integer
            :initform 1
            :allocation :class)))

(defmethod print-object ((monkey monkey) stream)
  (print-unreadable-object (monkey stream :type t)
    (with-slots (id items operation test) monkey
      (format stream "~A operation: ~A test: ~A items: ~A" id operation test items))))

(defun read-monkey (stream)
  (make-instance 'monkey
                 :id (read-monkey-id stream)
                 :items (read-starting-items stream)
                 :operation (read-operation stream)
                 :test (read-test stream)))

(defun count-items (monkeys)
  (reduce #'+ (map 'list #'monkey-items monkeys) :key #'length))

(defun compute-divisor (monkeys)
  (reduce #'* (map 'list #'monkey-test monkeys) :key #'first))

(defun read-monkeys (stream)
  (let ((monkeys (coerce (loop for monkey = (read-monkey stream)
                               for separator = (read-line stream nil nil)
                               collect monkey
                               while separator)
                         'vector)))
    (loop with count = (count-items monkeys)
          for monkey across monkeys
          do (with-slots (items) monkey
               (setf items (adjust-array (make-array (length items)
                                                     :element-type 'integer
                                                     :fill-pointer (length items)
                                                     :initial-contents items)
                                         count))))
    (setf (monkey-divisor (aref monkeys 0)) (compute-divisor monkeys))
    monkeys))

(defun inspect-item1 (monkey item)
  (with-slots (operation) monkey
    (floor (funcall (first operation)
                    (or (second operation) item)
                    (or (third operation) item))
           3)))

(defun choose-target (monkey item)
  (with-slots (test) monkey
    (if (zerop (mod item (first test)))
        (second test)
        (third test))))

(defun find-target (id monkeys)
  (aref monkeys id))

(defun throw-item (item to)
  (with-slots (items) to
    (vector-push item items)))

(defun play-20-rounds (monkeys)
  (loop repeat 20
        do (loop for monkey across monkeys
                 do (with-slots (activity items) monkey
                      (incf activity (length items))
                      (loop for item across items
                            for worry-level = (inspect-item1 monkey item)
                            for id = (choose-target monkey worry-level)
                            do (throw-item worry-level (find-target id monkeys)))
                      (setf (fill-pointer items) 0)))))

(defun monkey-business-level (monkeys)
  (let ((sorted (sort monkeys #'> :key #'activity-level)))
    (* (activity-level (aref sorted 0))
       (activity-level (aref sorted 1)))))

(defun what-is-the-level-of-monkey-business-after-20-rounds? ()
  (with-open-input (stream "day-11")
    (let ((monkeys (read-monkeys stream)))
      (play-20-rounds monkeys)
      (monkey-business-level monkeys))))


(defun inspect-item2 (monkey item)
  (with-slots (operation) monkey
    (funcall (first operation)
             item
             (or (third operation) item))))

(defun play-n-rounds (n monkeys)
  (loop repeat n
        do (loop for monkey across monkeys
                 do (with-slots (activity items divisor) monkey
                      (incf activity (length items))
                      (loop for item across items
                            for worry-level = (inspect-item2 monkey (mod item divisor))
                            for id = (choose-target monkey worry-level)
                            do (throw-item worry-level (find-target id monkeys)))
                      (setf (fill-pointer items) 0)))))


(defun what-is-the-level-of-monkey-business-after-n-rounds? (&optional (n 10000))
  (with-open-input (stream "day-11")
    (let ((monkeys (read-monkeys stream)))
      (play-n-rounds n monkeys)
      (values (monkey-business-level monkeys)))))
