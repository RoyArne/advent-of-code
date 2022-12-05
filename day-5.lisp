
(cl:in-package #:advent-of-code)

(defun read-stacks (stream)
  (reverse (loop for line = (read-line stream nil nil)
                 while (find #\[ line :test #'char=)
                 collect (loop for i from 1 below (length line) by 4
                               collect (char line i)))))

(defun make-empty-stacks (line)
  (map 'vector
       #'(lambda (c)
           (declare (ignore c))
           (make-array 256 :fill-pointer 0))
       line))

(defun make-stack-vectors (lines)
  (loop with stacks = (make-empty-stacks (first lines))
        for level in lines
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

(defun read-puzzle-data-for-part-1 ()
  (with-open-input (stream "day-5")
    (let ((stacks (make-stack-vectors (read-stacks stream))))
      (read-line stream)
      (values stacks
              (read-instructions stream)))))

(defun move-crates (n from to)
  (loop for i from 1 upto n
        do (vector-push (vector-pop from) to)))

(defun execute-instructions (stacks instructions)
  (loop for instruction in instructions
        do (destructuring-bind (n from to)
               instruction
             (move-crates n (aref stacks from) (aref stacks to)))))

(defun rearrange-crates ()
  "After the rearrangement procedure completes, what crate ends up on top of
each stack?"
  (multiple-value-bind (stacks instructions)
      (read-puzzle-data-for-part-1)
    (execute-instructions stacks instructions)
    (print-top-crates stacks)))

(defun execute-instructions-without-reordering (stacks instructions)
  (loop for instruction in instructions
        do (destructuring-bind (n from to)
               instruction
             (let ((to-stack (aref stacks to)))
               (move-crates n (aref stacks from) to-stack)
               (let ((start (- (length to-stack) n)))
                 (setf (subseq to-stack start) (reverse (subseq to-stack start))))))))
  
(defun rearrange-crates-without-reordering ()
  "After the rearrangement procedure completes, what crate ends up on top of
each stack?"
  (multiple-value-bind (stacks instructions)
      (read-puzzle-data-for-part-1)
    (execute-instructions-without-reordering stacks instructions)
    (print-top-crates stacks)))
    
