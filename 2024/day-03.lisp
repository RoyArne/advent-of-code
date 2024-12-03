
(cl:in-package #:advent-of-code)

(defun read-digits (stream)
  (let ((string (coerce (loop for exponent from 0 upto 2
                              for sc = (peek-char nil stream nil)
                              while (and sc (digit-char-p sc))
                              collect (read-char stream))
                        'string)))
    (when (and (zerop (length string))
               (peek-char nil stream nil nil))
      (read-char stream))
    string))

(defun read-token (match stream)
  (etypecase match
    (string (loop for mc across match
                  for sc = (peek-char nil stream nil nil)
                  do (if (and sc (char= sc mc))
                         (read-char stream)
                         (progn
                           (when sc (read-char stream))
                           (return nil)))
                  finally (return t)))
    (integer (let ((digits (read-digits stream)))
               (when (< 0 (length digits) 4)
                 (parse-integer digits))))))

(defun match (expression stream)
  (let ((match (loop for elt in expression
                     for token = (read-token elt stream)
                     while token
                     collect token)))
    (when (= (length match) (length expression))
      match)))

(defun read-muls (stream)
  (loop with match-expression = '("mul" "(" 1 "," 1 ")")
        for match = (match match-expression stream)
        when match collect (* (third match) (fifth match))
        while (peek-char nil stream nil nil)))


(defun sum-muls ()
  (with-open-file (stream (input-file-pathname 3 2024))
    (reduce #'+ (read-muls stream))))


(defun dont-state (stream)
  (loop for next = (peek-char nil stream nil nil) while next
        do (case next
             (#\d (when (match '("do()") stream)
                    (return nil)))
             (otherwise (read-char stream)))))

(defun mul-state (stream)
  (loop with numbers = nil
        for next = (peek-char nil stream nil nil) while next
        do (case next
             (#\m (let ((match (match '("mul" "(" 1 "," 1 ")") stream)))
                    (when match
                      (push (* (third match) (fifth match)) numbers))))
             (#\d (when (match '("don't()") stream)
                    (dont-state stream)))
             (otherwise (read-char stream)))
        finally (return (reduce #'+ numbers))))
              
(defun sum-muls-but-not-all ()
  (with-open-file (stream (input-file-pathname 3 2024))
    (mul-state stream)))
