
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
  (when (peek-char nil stream nil)
    (ecase (read-char stream)
      (#\$ (ecase (read stream)
             (cd (list 'cd (read-line stream nil nil)))
             (ls (list 'ls)))))))

(defun read-listing (stream directory)
  "Read a directory listing from stream."
  (loop for c = (peek-char nil stream nil)
        while c
        until (char= #\$ c)
        do (let ((value (read stream)))
             (add-listing directory (read-line stream) value))))

(defun read-terminal-output (stream)
  (let ((*package* (find-package "AOC-2022-DAY-7"))
        (*read-eval* nil)
        (root (make-root)))
    (loop with path = (list root)
          for command = (read-command stream)
          while command
          do (ecase (first command)
               (ls (read-listing stream (first path)))
               (cd (if (string= ".." (second command))
                       (pop path)
                       (push (gethash (second command) (first path)) path)))))
    root))

(defun size-of (item max)
  (etypecase item
    (integer
     (values item 0))
    (hash-table
     (loop with dir-total = 0
           with subdir-total = 0
           for file being the hash-values in item using (hash-key name)
           do (multiple-value-bind (file-size running-total)
                  (size-of file max)
                (incf dir-total file-size)
                (incf subdir-total running-total))
           finally (return (values dir-total
                                   (+ subdir-total
                                      (if (<= dir-total max)
                                          dir-total
                                          0))))))))

(defun size-of (dir max)
  (loop with dir-total = 0
        with subdir-total = 0
        for file being the hash-values in dir using (hash-key name)
        do (etypecase file
             (integer
              (incf dir-total file))
             (hash-table
              (multiple-value-bind (file-size running-total)
                  (size-of file max)
                (incf dir-total file-size)
                (incf subdir-total running-total))))
        finally (return (values dir-total
                                (+ subdir-total
                                   (if (<= dir-total max)
                                       dir-total
                                       0))))))

(defun total-size-of-directories-with-a-total-size-of-at-most-100000 ()
  (with-open-input (stream "day-7")
    (nth-value 1 (size-of (read-terminal-output stream) 100000))))


(defun find-smallest-directory-matching (dir target-size)
  (loop with dir-total = 0
        with best-match = most-positive-fixnum
        for file being the hash-values in dir using (hash-key name)
        do (etypecase file
             (integer
              (incf dir-total file))
             (hash-table
              (multiple-value-bind (size match)
                  (find-smallest-directory-matching file target-size)
                (incf dir-total size)
                (setf best-match (min best-match match)))))
        finally (return (values dir-total
                                (if (>= best-match dir-total target-size)
                                    dir-total
                                    best-match)))))

(defun size-of-smallest-directory-to-free ()
  (with-open-input (stream "day-7")
    (let* ((root (read-terminal-output stream))
           (target-size (+ (- (size-of root 0) 70000000) 30000000)))
      (nth-value 1 (find-smallest-directory-matching root target-size)))))
