
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

(defmacro with-input-file-grid ((grid &key day-number year-number parse-line) &body body)
  "Return a 2d array of characters from the input file. Assumes that every
line is equally long. Access characters with \(aref grid column row\)."
  (let ((column (gensym))
        (row (gensym))
        (char (gensym))
        (line (gensym))
        (lines (gensym)))
    `(with-input-file-lines (,lines
                             :day-number ,day-number :year-number ,year-number
                             :parse-line ,parse-line)
       (let ((,grid (make-array (list (length (first ,lines))  (length ,lines))
                                :element-type 'character)))
         (loop for ,line in ,lines
               for ,row = 0 then (1+ ,row)
               do (loop for ,char across ,line
                        for ,column = 0 then (1+ ,column)
                        do (setf (aref ,grid ,column ,row) ,char)))
         ,@body))))

(defun row-dimension (grid)
  (array-dimension grid 1))

(defun row (position)
  (imagpart position))

(defun column-dimension (grid)
  (array-dimension grid 0))

(defun column (position)
  (realpart position))

(defun in-grid-p (grid position)
  (array-in-bounds-p grid (column position) (row position)))

(defun grid (grid position)
  (aref grid (column position) (row position)))

(defun (setf grid) (character grid position)
  (setf (aref grid (column position) (row position)) character))


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

(defun list-numbers (string)
  (map 'list #'parse-integer (split #\Space string)))

(defun flatten (l)
  (when l
    (if (atom l)
        (list l)
        (loop for a in l appending (flatten a)))))

(defun duplicates (list &optional (test #'eql))
  (when list
    (if (member (car list) (cdr list) :test test)
        (cons (car list) (duplicates (cdr list)))
        (duplicates (cdr list)))))
