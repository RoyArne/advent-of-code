
(cl:in-package #:advent-of-code)

(defparameter *input-directory*
  (merge-pathnames "lisp/advent-of-code/input/" (user-homedir-pathname))
  "Full path to the directory containing the input files.")

(defun input-pathname (filename)
  "Return the path to FILENAME in the *INPUT-DIRECTORY*."
  (merge-pathnames filename *input-directory*))

(defmacro with-open-input ((stream filename)  &body body)
  "Evaluates BODY with STREAM bound to an open file named FILENAME in the
*INPUT-DIRECTORY*."
  `(with-open-file (,stream
                    (input-pathname ,filename)
                    :direction :input
                    :element-type 'character
                    :external-format :utf-8)
     ,@body))

(defun read-file-lines (stream &key parse-line)
  "Read text lines from STREAM until end of file.
Returns the lines as a list of strings."
  (loop for line = (read-line stream nil)
        while line
        collect (if parse-line
                    (funcall parse-line line)
                    line)))

(defmacro with-file-lines ((lines filename &key parse-line) &body body)
  "Evaluates BODY with LINES bound to a list of text lines (strings) read from
FILENAME in the *INPUT-DIRECTORY*."
  (let ((stream (gensym)))
    `(with-open-input (,stream ,filename)
       (let ((,lines (read-file-lines ,stream :parse-line ,parse-line)))
         ,@body))))

(defun trim-whitespace (line)
  "Return LINE with the whitespace characters Tab, Space, Return, and, Newline,
removed from both ends."
  (string-trim '(#\Tab #\Space #\Return #\Newline) line))

(defun split (char line)
  (mapcar #'trim-whitespace
          (split-sequence:split-sequence char line
                                         :remove-empty-subseqs t
                                         :test #'char=)))
