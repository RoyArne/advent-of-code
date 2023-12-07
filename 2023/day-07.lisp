
(cl:in-package #:advent-of-code)

(defparameter *sample-07*
  '("32T3K 765"
    "T55J5 684"
    "KK677 28"
    "KTJJT 220"
    "QQQJA 483"))

(defparameter *input-07*
  (read-input-file-lines :day-number 7 :year-number 2023))

(defun card-value (card)
  (position card "23456789TJQKA":test #'char=))

(defparameter *five-of-a-kind* 7)
(defparameter *four-of-a-kind* 6)
(defparameter *full-house* 5)
(defparameter *three-of-a-kind* 4)
(defparameter *two-pairs* 3)
(defparameter *one-pair* 2)
(defparameter *highest-card* 1)

(defun compute-hand-type (cards)
  (loop with type = *five-of-a-kind*
        with sorted-cards = (sort (copy-seq cards) #'<)
        for start from 0 below (length sorted-cards)
        for count = (count (aref sorted-cards start) sorted-cards :start start)
        do (case count
             (5 (return *five-of-a-kind*))
             (4 (return *four-of-a-kind*))
             (3 (case type
                  (#.*five-of-a-kind* (setf type *three-of-a-kind*))
                  (#.*one-pair* (return *full-house*))
                  (#.*highest-card* (return *three-of-a-kind*)))
              (incf start 2))
             (2 (case type
                  (#.*five-of-a-kind* (setf type *one-pair*))
                  (#.*highest-card* (setf type *one-pair*))
                  (#.*three-of-a-kind* (return *full-house*))
                  (#.*one-pair* (return *two-pairs*)))
              (incf start))
             (1 (case type
                  (#.*three-of-a-kind* (return *three-of-a-kind*))
                  (#.*five-of-a-kind* (setf type *highest-card*)))))
        finally (return type)))

(defun list-cards-type-and-bid (line)
  (let ((list (split #\space line)))
    (let ((cards (map 'vector #'card-value (first list))))
      (list cards (compute-hand-type cards) (parse-integer (second list))))))

(defun cards (hand)
  (first hand))

(defun hand-type (hand)
  (second hand))

(defun bid (hand)
  (third hand))

(defun parse-input-07 (lines)
  (map 'list #'list-cards-type-and-bid lines))
        
(defun strength< (hand1 hand2)
  (or (< (hand-type hand1) (hand-type hand2))
      (and (= (hand-type hand1) (hand-type hand2))
           (loop for card1 across (cards hand1)
                 for card2 across (cards hand2)
                 when (< card1 card2)
                 do (return t)
                 when (> card1 card2)
                 do (return nil)))))

(defun total-winnings-part-1 (input)
  (loop for hand in (sort (parse-input-07 input) #'strength<)
        for rank = 1 then (1+ rank)
        summing (* rank (bid hand))))
  
(defun card-value-with-joker (card)
  (position card "J23456789TQKA":test #'char=))

(defun count-jokers (cards)
  (count (card-value-with-joker #\J) cards :test #'=))

(defun maximize-hand-type (cards)
  (let ((initial-type (compute-hand-type cards))
        (joker-count (count-jokers cards)))
    (if (zerop joker-count)
        initial-type
        (case initial-type
          (#.*five-of-a-kind* initial-type)
          (#.*four-of-a-kind* *five-of-a-kind*) ; there may be ONE or FOUR jokers
          (#.*full-house* *five-of-a-kind*) ; there may be TWO or THREE jokers
          (#.*three-of-a-kind* (case joker-count 
                                 (1 *four-of-a-kind*) ; ONE cannot be part of the three.
                                 (2 *five-of-a-kind*) ; the TWO cannot be part of the three
                                 (3 *four-of-a-kind*))) ; the THREE must be jokers
          (#.*two-pairs*  (case joker-count
                            (1 *full-house*) ; ONE cannot be part of either of the two
                            (2 *four-of-a-kind*))) ; two pairs, so one of them must be jokers.
          (#.*one-pair* (case joker-count
                          ;; 3 jokers would be three of a kind or full house above.
                          ;; 4 jokers would be four of a kind above.
                          ;; 5 jokers would be five of a kind above.
                          (1 *three-of-a-kind*) ; ONE cannot be part of the pair
                          (2 *three-of-a-kind*))) ; the pair must pe TWO jokers
          (#.*highest-card* *one-pair*)))))

(defun list-cards-type-and-bid-part-2 (line)
  (let ((list (split #\space line)))
    (let ((cards (map 'vector #'card-value-with-joker (first list))))
      (list cards (maximize-hand-type cards) (parse-integer (second list))))))

(defun parse-input-07-part-2 (lines)
  (map 'list #'list-cards-type-and-bid-part-2 lines))

(defun total-winnings-part-2 (input)
  (loop for hand in (sort (parse-input-07-part-2 input) #'strength<)
        for rank = 1 then (1+ rank)
        summing (* rank (bid hand))))
