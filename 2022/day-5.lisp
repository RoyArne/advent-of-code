
(cl:in-package #:advent-of-code)

(defun read-stacks (stream)
  (loop for line = (read-line stream nil nil)
        while (find #\[ line :test #'char=)
        collect (loop for i from 1 below (length line) by 4
                      collect (char line i))))

(defun make-empty-stacks (line)
  (map 'vector
       #'(lambda (c)
           (declare (ignore c))
           (make-array 256 :fill-pointer 0))
       line))

(defun make-stack-vectors (lines)
  (loop with stacks = (make-empty-stacks (first lines))
        for level in (reverse lines)
        do (loop for c in level
                 for stack across stacks
                 unless (char= #\Space c)
                 do (vector-push c stack))
        finally (return stacks)))
                          
(defun print-top-crates (stacks)
  (loop for stack across stacks
        do (princ (aref stack (1- (length stack))))))
          
(defun read-instructions (stream)
  (loop for line = (read-line stream nil nil)
        while line
        collect (cl-ppcre:register-groups-bind ((#'parse-integer n from to))
	            ("move (\\d+) from (\\d+) to (\\d+)" line)
                  ;; 1- so they can be used as array indices.
                  (list n (1- from) (1- to)))))

(defun read-stacks-and-instructions ()
  (with-open-input (stream "day-5")
    (let ((stacks (make-stack-vectors (read-stacks stream))))
      (read-line stream)
      (values stacks
              (read-instructions stream)))))

(defun move-crates-1-by-1 (n from to)
  (loop for i from 1 upto n
        do (vector-push (vector-pop from) to)))

(defun execute-instructions (stacks instructions move-function)
  (loop for instruction in instructions
        do (destructuring-bind (n from to)
               instruction
             (funcall move-function n (aref stacks from) (aref stacks to)))))

(defun rearrange-crates ()
  "After the rearrangement procedure completes, what crate ends up on top of
each stack?"
  (multiple-value-bind (stacks instructions)
      (read-stacks-and-instructions)
    (execute-instructions stacks instructions #'move-crates-1-by-1)
    (print-top-crates stacks)))

(defun move-multiple-crates (n from to)
  (let ((start (length to)))
    (incf (fill-pointer to) n)
    (setf (subseq to start) (subseq from (- (length from) n)))
    (decf (fill-pointer from) n)))

(defun rearrange-crates-without-reordering ()
  "After the rearrangement procedure completes, what crate ends up on top of
each stack?"
  (multiple-value-bind (stacks instructions)
      (read-stacks-and-instructions)
    (execute-instructions stacks instructions #'move-multiple-crates)
    (print-top-crates stacks)))
    
