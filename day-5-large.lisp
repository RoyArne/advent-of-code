
(cl:in-package #:advent-of-code)


(declaim (type fixnum *maximum-stack-height*))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *maximum-stack-height* 1000000))

(deftype crate-count () `(and fixnum (integer 0 #.*maximum-stack-height*)))

(declaim (type fixnum *maximum-crate-index*))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *maximum-crate-index* (1- *maximum-stack-height*)))

(deftype crate-index () `(and fixnum (integer 0 #.*maximum-crate-index*)))

(defstruct (stack (:conc-name nil))
  (height 0 :type crate-index)
  (crates (make-array *maximum-stack-height* :element-type 'character) :type simple-string))



(declaim (type fixnum *maximum-stack-count*))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *maximum-stack-count* 9))

(deftype stack-count () `(and fixnum (integer 1 #.*maximum-stack-count*)))

(declaim (type fixnum *maximum-stack-index*))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *maximum-stack-index* 9))

(deftype stack-index () `(and fixnum (integer 0 #.*maximum-stack-index*)))

(defun make-stacks (n)
  (declare (type stack-count n)
           (optimize (speed 3) (safety 0)))
  (make-array n
              :element-type 'stack
              :initial-contents (loop repeat n collect (make-stack))))

(deftype stacks () `(and simple-vector (vector stack)))

(defun populate-stacks (stacks lines)
  (declare (type stacks stacks)
           (type list lines)
           (optimize (speed 3) (safety 0)))
  (loop for level in (reverse lines)
        do (loop for c in level
                 for s across stacks
                 do (unless (char= #\Space c)
                      (setf (schar (crates s) (height s)) c)
                      (incf (height s))))
        finally (return stacks)))

(defun read-stack-lines (stream)
  (loop for line = (read-line stream nil nil)
        while (find #\[ line :test #'char=)
        ;; Using 1- line length here because the data set has an extra space
        ;; character at the end of each line.
        collect (loop for i from 1 below (1- (length line)) by 4
                      collect (char line i))))
          
(defun read-instruction-lines (stream)
  (loop for line = (read-line stream nil nil)
        while line
        collect (cl-ppcre:register-groups-bind ((#'parse-integer n from to))
	            ("move (\\d+) from (\\d+) to (\\d+)" line)
                  ;; 1- so they can be used as array indices.
                  (list n (1- from) (1- to)))))

(defun read-stacks-and-instructions ()
  (with-open-input (stream "aoc_2022_day05_large_input.txt")
    (let ((stacks (populate-stacks (make-stacks *maximum-stack-count*)
                                   (read-stack-lines stream))))
      (read-line stream)
      (values stacks
              (read-instruction-lines stream)))))

(defun print-top-crates (stacks)
  (declare (type stacks stacks))
  (loop for stack across stacks
        do (princ (aref (crates stack) (1- (height stack))))))

(defun rearrange-1-by-1 ()
  (declare (optimize (speed 3) (safety 0)))
  (multiple-value-bind (stacks instructions)
      (read-stacks-and-instructions)
    (declare (type stacks stacks)
             (type list instructions))
    (loop for instruction in instructions
          do (destructuring-bind (n from to)
                 instruction
               (declare (type crate-count n)
                        (type stack-index from to))
               (loop repeat n
                     for i = (1- (height (aref stacks from))) then (1- i)
                     for j = (height (aref stacks to)) then (1+ j)
                     do (setf (aref (crates (aref stacks to)) j)
                              (aref (crates (aref stacks from)) i)))
               (decf (height (aref stacks from)) n)
               (incf (height (aref stacks to)) n)))
    (print-top-crates stacks)))
