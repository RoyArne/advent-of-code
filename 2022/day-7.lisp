
(cl:defpackage #:aoc-2022-day-7
  (:use #:common-lisp
        #:advent-of-code)
  (:export #:total-size-of-directories-with-a-total-size-of-at-most-100000
           #:size-of-smallest-directory-to-free)
  (:documentation
   "Solutions for the Advent of Code 2022 day 7 puzzles found at
https://adventofcode.com/2022/day/7

Part 1 is solved by
  total-size-of-directories-with-a-total-size-of-at-most-100000
and part 2 by
  size-of-smallest-directory-to-free.

Both look for a file named day-7 in the *INPUT-DIRECTORY*.

Requires the split-sequence library: \(ql:quickload \"split-sequence\"\)

See also
https://www.reddit.com/r/adventofcode/comments/zesk40/2022_day_7_solutions/"))

(cl:in-package #:aoc-2022-day-7)

(defun make-directory ()
  (make-hash-table :test #'equal))

(defun add-listing (directory name value)
  (setf (gethash name directory)
        (if (integerp value)
            value
            (make-directory))))

(defun make-root ()
  (let ((root (make-directory)))
    (add-listing root "/" 'dir)
    root))

(defun read-command (stream)
  "Read a command from STREAM.
Returns a list \(name <arguments>\) or NIL at end of file."
  (handler-case
      (ecase (read-char stream)
        (#\$ (let ((command (split-sequence:split-sequence #\Space (read-line stream)
                                                           :remove-empty-subseqs t)))
               (cond
                 ((string= "ls" (first command))
                  (list 'ls))
                 ((string= "cd" (first command))
                  (if (string= ".." (second command))
                      (list 'pop)
                      (list 'push (second command))))
                 (t
                  (error "Unknown command ~A." command))))))
    (end-of-file () nil)))

(defun read-listing (stream directory)
  (loop for c = (peek-char nil stream nil)
        while (and c (char/= #\$ c))
        do (let ((line (split-sequence:split-sequence #\Space (read-line stream))))
             (add-listing directory (second line)
                          (if (string= "dir" (first line))
                              'dir
                              (parse-integer (first line)))))))

(defun read-terminal-output (stream)
  (loop with root = (make-root)
        with path = (list root)
        for command = (read-command stream)
        while command
        do (ecase (first command)
             (ls (read-listing stream (first path)))
             (pop (pop path))
             (push (push (gethash (second command) (first path)) path)))
        finally (return root)))


(defun size-of (dir max)
  "Return two values; the total size of DIR and sum of the total sizes of the
directories whose total size is less than or equal to MAX."
  (declare (type hash-table dir)
           (type fixnum max))
  (loop with dir-total = 0
        with subdir<=max-total = 0
        for file being the hash-values in dir
        do (incf dir-total
                 (etypecase file
                   (integer file)
                   (hash-table (multiple-value-bind (size total<=max)
                                   (size-of file max)
                                 (incf subdir<=max-total total<=max)
                                 size))))
        finally (return (values dir-total
                                (+ subdir<=max-total
                                   (if (<= dir-total max)
                                       dir-total
                                       0))))))

(defun total-size-of-directories-with-a-total-size-of-at-most-100000 ()
  "Find all of the directories with a total size of at most 100000. What is
the sum of the total sizes of those directories?"
  (with-open-input (stream "day-7")
    (nth-value 1 (size-of (read-terminal-output stream) 100000))))


(defun find-smallest-directory-matching (dir target-size)
  "Return two values; the total size of DIR and the size of the smallest
 directory that is greater than or equal to TARGET-SIZE."
  (declare (type hash-table dir)
           (type fixnum target-size))
  (loop with dir-total = 0
        with best-match = most-positive-fixnum
        for file being the hash-values in dir
        do (incf dir-total
                 (etypecase file
                   (integer file)
                   (hash-table (multiple-value-bind (size match)
                                   (find-smallest-directory-matching file target-size)
                                 (setf best-match (min best-match match))
                                 size))))
        finally (return (values dir-total
                                (if (>= best-match dir-total target-size)
                                    dir-total
                                    best-match)))))

(defun size-of-smallest-directory-to-free ()
  "Find the smallest directory that, if deleted, would free up enough space on
the filesystem to run the update. What is the total size of that directory?"
  (with-open-input (stream "day-7")
    (let* ((root (read-terminal-output stream))
           (target-size (+ (- (size-of root 0) 70000000) 30000000)))
      (nth-value 1 (find-smallest-directory-matching root target-size)))))
