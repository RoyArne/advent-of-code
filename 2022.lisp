
(cl:defpackage #:advent-of-code
  (:use #:common-lisp)
  (:nicknames #:aoc))

(cl:in-package #:advent-of-code)

(defparameter *input-directory* (merge-pathnames "lisp/advent-of-code/input/"
                                                 (user-homedir-pathname)))

(defun input-pathname (filename)
  (merge-pathnames filename *input-directory*))

(defmacro with-open-input ((stream filename)  &body body)
  `(with-open-file (,stream
                    (input-pathname ,filename)
                    :direction :input
                    :element-type 'character
                    :external-format :utf-8)
     ,@body))

(defun read-file-lines (stream)
  (loop for line = (read-line stream nil nil)
        while line
        collect line))

(defmacro with-file-lines ((lines filename) &body body)
  (let ((stream (gensym)))
    `(with-open-input (,stream ,filename)
       (let ((,lines (read-file-lines ,stream)))
         ,@body))))

(defun trim-whitespace (line)
  (string-trim '(#\Tab #\Space #\Return #\Newline) line))
