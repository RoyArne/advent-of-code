
(cl:defpackage #:aoc-2022-day-13
  (:use #:common-lisp
        #:advent-of-code)
  (:export #:sum-indices-of-packets-in-right-order
           #:decoder-key)
  (:documentation
   "Solutions for the Advent of Code 2022 day 13 puzzles found at
https://adventofcode.com/2022/day/13

Part 1 is solved by
  sum-indices-of-packets-in-right-order
and part 2 by
  decoder-key.

Both look for a file named day-13 in the *INPUT-DIRECTORY*.

See also"))

(cl:in-package #:aoc-2022-day-13)

(defun parse-element (line start)
  (loop for i from start below (length line)
        do (case (char line i)
             (#\] (return (values (parse-integer line :start start :end i) i)))
             (#\, (return (values (parse-integer line :start start :end i) i)))
             (#\[ (multiple-value-bind (list end)
                      (parse-list line i)
                    (return (values list end)))))))

(defun parse-list (line start)
  (if (char= #\[ (char line start))
      (let ((i (1+ start)))
        (values (loop until (char= #\] (char line i))
                      collect (multiple-value-bind (elt end)
                                  (parse-element line i)
                                (setf i end)
                                ;;(format t "~A " elt)
                                elt)
                      when (char= #\, (char line i))
                      do (incf i))
                (1+ i)))))

(defun parse-packet-pairs (stream)
  (loop for left = (read-line stream)
        for right = (read-line stream)
        collect (list (parse-list left 0)
                      (parse-list right 0))
        until (not (read-line stream nil nil))))

(defun compare (elt1 elt2)
  (etypecase elt1
    (integer (etypecase elt2
               (integer (- elt1 elt2))
               (list (compare (list elt1) elt2))))
    (list (etypecase elt2
            (integer (compare elt1 (list elt2)))
            (list (loop with length1 = (length elt1)
                        with length2 = (length elt2)
                        for left in elt1
                        for right in elt2
                        for test = (compare left right)
                        when (minusp test) do (return -1)
                        when (plusp test) do (return 1)
                        finally (return (- length1 length2))))))))

(defun in-right-order-p (packet)
  (minusp (compare (first packet) (second packet))))

(defun indices-of-packets-in-right-order (packets)
  (loop for i = 1 then (1+ i)
        for packet in packets
        when (in-right-order-p packet)
	collect i))

(defun sum-indices-of-packets-in-right-order ()
  (with-open-input (stream "day-13")
    (reduce #'+ (indices-of-packets-in-right-order (parse-packet-pairs stream)))))

(defun parse-packet-list (stream)
  (loop collect (parse-list (read-line stream) 0)
        collect (parse-list (read-line stream) 0)
        while (read-line stream nil)))

(defun decoder-key ()
  (with-open-input (stream "day-13")
    (let* ((divider1 '((2)))
           (divider2 '((6)))
           (packets (sort (cons divider1
                                (cons divider2
                                      (parse-packet-list stream)))
                          #'(lambda (packet1 packet2)
                              (minusp (compare packet1 packet2))))))
      (* (1+ (position divider1 packets))
         (1+ (position divider2 packets))))))
