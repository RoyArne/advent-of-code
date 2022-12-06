
(cl:in-package #:advent-of-code)

(defun match-index (string start end)
  (loop for i from end downto start
        do (loop for j from (1- i) downto start
                 when (char= (char string i) (char string j))
                 do (return-from match-index i))))

(defun index-of-first-n-distinct-chars (string n)
  (loop with i = 0
        with j = (1- n)
        for match = (match-index string i (+ i j))
        do (if match
               (incf i)
               (return i))))
  
(defun first-start-of-packet-marker ()
  (with-open-input (stream "day-6")
    (+ 4 (index-of-first-n-distinct-chars (read-line stream nil nil) 4))))

(defun first-start-of-message-marker ()
  (with-open-input (stream "day-6")
    (+ 14 (index-of-first-n-distinct-chars (read-line stream nil nil) 14))))
