
(cl:in-package #:advent-of-code)

(defun current-timestamp ()
  (local-time:universal-to-timestamp (get-universal-time)))

(deftype year-number ()
  "An integer >= 2015, Advent of Codes first year."
  `(integer 2015 *))

(deftype day-number ()
  "An integer in the range 1 to 31."
  `(integer 1 31))

(defun make-day-number-name (&optional day-number)
  (declare (type (or null day-number) day-number))
  (format nil "~2,'0D"
          (or day-number
              (local-time:timestamp-day (current-timestamp)))))

(defun make-year-number-name (&optional year-number)
  (declare (type (or null year-number) year-number))
  (format nil "~D"
          (or year-number
              (local-time:timestamp-year (current-timestamp)))))

(defun input-directory-pathname (&optional year-number)
  (declare (type (or null year-number) year-number))
  (truename (asdf:system-relative-pathname (asdf:find-system "advent-of-code")
                                           (make-year-number-name year-number))))

(defun input-file-name (&optional day-number)
  (declare (type (or null day-number) day-number))
  (concatenate 'string "input-" (make-day-number-name day-number) ".lisp"))

(defun input-file-pathname (&optional day-number year-number)
  (declare (type (or null day-number) day-number)
           (type (or null year-number) year-number))
  (truename (merge-pathnames (input-file-name day-number)
                             (input-directory-pathname year-number))))

(defun read-stream-lines (stream &key parse-line)
  "Read text lines from STREAM until end of file.
Returns the lines as a list of strings unless PARSE-LINE is provided.

If PARSE-LINE is provided then it must be a function of one parameter. It is
applied to each line in order, and a list of the results is returned."
  (loop for line = (read-line stream nil)
        while line
        collect (if parse-line
                    (funcall parse-line line)
                    line)))

(defun read-input-file-lines (&key day-number year-number parse-line)
  (with-open-file (stream (input-file-pathname day-number year-number))
    (read-stream-lines stream :parse-line parse-line)))
    
(defmacro with-input-file-lines ((lines &key day-number year-number parse-line) &body body)
  `(let ((,lines (read-input-file-lines :day-number ,day-number
                                        :year-number ,year-number
                                        :parse-line ,parse-line)))
     ,@body))


(defparameter *year* "2022")

(defparameter *directory*
  (asdf:system-relative-pathname (asdf:find-system "advent-of-code") *year*))

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

(defmacro with-stream-lines ((lines stream &key parse-line) &body body)
  "Evaluates BODY with LINES bound to a list of text lines (strings) read from
STREAM."
  `(let ((,lines (read-file-lines ,stream :parse-line ,parse-line)))
     ,@body))

(defmacro with-file-lines ((lines filename &key parse-line) &body body)
  "Evaluates BODY with LINES bound to a list of text lines (strings) read from
FILENAME in the *INPUT-DIRECTORY*."
  (let ((stream (gensym)))
    `(with-open-input (,stream ,filename)
       (with-stream-lines (,lines ,stream :parse-line ,parse-line)
         ,@body))))

(defun trim-whitespace (line)
  "Return LINE with the whitespace characters Tab, Space, Return, and, Newline,
removed from both ends."
  (string-trim '(#\Tab #\Space #\Return #\Newline) line))

(defun split (char line)
  "Split LINE at each CHAR, removing empty subsequences."
  (declare (type character char)
           (type string line))
  (mapcar #'trim-whitespace
          (split-sequence:split-sequence char line
                                         :remove-empty-subseqs t
                                         :test #'char=)))

(defun parse-numbers (list)
  (map 'list #'parse-integer list))
