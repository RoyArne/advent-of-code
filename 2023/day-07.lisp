
(cl:in-package #:advent-of-code)

(defparameter *sample-07*
  '("32T3K 765"
    "T55J5 684"
    "KK677 28"
    "KTJJT 220"
    "QQQJA 483"))

(defparameter *input-07*
  (read-input-file-lines :day-number 7 :year-number 2023))

(defparameter *card-values-without-joker* "23456789TJQKA"
  "The position of the card is its value.")

(defparameter *card-values-with-joker* "J23456789TQKA"
  "The position of the card is its value.")

(defparameter *joker* #\J
  "Character value of the joker card.")

(defun card-value (character card-values)
  "Return CHARACTERs position in CARD-VALUES."
  (position character card-values :test #'char=))

(defun cards-by-value (string card-values)
  "Return a vector of card values formed by applying CARD-VALUE to each
character in STRING in order."
  (map 'vector
       #'(lambda (character)
           (card-value character card-values))
       string))

(defparameter *five-of-a-kind* 7)
(defparameter *four-of-a-kind* 6)
(defparameter *full-house* 5)
(defparameter *three-of-a-kind* 4)
(defparameter *two-pairs* 3)
(defparameter *one-pair* 2)
(defparameter *highest-card* 1)

(defun count-jokers (cards card-values joker)
  (count (card-value joker card-values) cards :test #'=))

(defun compute-hand-type-with-joker (initial-type cards card-values joker)
  (let ((joker-count (count-jokers cards card-values joker)))
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

(defun compute-hand-type-without-joker (cards)
  (loop with sorted-values = (sort (copy-seq cards) #'<)
        with type = *five-of-a-kind*
        for start from 0 below (length sorted-values)
        for count = (count (aref sorted-values start) sorted-values :start start)
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
                  (#.*five-of-a-kind* (setf type *highest-card*))
                  (#.*three-of-a-kind* (return *three-of-a-kind*)))))
        finally (return type)))

(defun compute-hand-type (cards card-values joker)
  "Return a hand type for the CARDS vector of card values:

*five-of-a-kind*—five equal cards,
*four-of-a-kind*—four equal cards,
*full-house*—three equal cards and a pair,
*three-of-a-kind*—three equal cards,
*two-pairs*—two pairs,
*one-pair*—one pair, and
*highest-card*—five different cards."
  (let ((hand-type (compute-hand-type-without-joker cards)))
    (if joker
        (compute-hand-type-with-joker hand-type cards card-values joker)
        hand-type)))

(defun hand (line card-values joker)
  "Return a HAND structure by parsing the LINE string.

The LINE string must be in the format \"<C><C><C><C><C> <bid>\". 
  <C> is a character found in CARD-VALUES, and
  <bid> is a string of decimal characters.

CARD-VALUES is a string where the position of each character \(<C> above\)
determines its value."
  (let* ((initial-list (split #\Space line))
         (cards (cards-by-value (first initial-list) card-values)))
    (list cards
          (compute-hand-type cards card-values joker)
          (parse-integer (second initial-list)))))

(defstruct (hand (:type list))
  (cards)
  (type)
  (bid))

(defun parse-input-07 (lines card-values joker)
  "Return a list of HAND structures."
  (loop for line in lines
        collect (hand line card-values joker)))
        
(defun hand-strength< (hand1 hand2)
  "True if HAND1 is weaker than HAND2:

HAND1 is stronger than HAND2 if its HAND-TYPE is greater.  

If their HAND-TYPE is the same then card values are compared in order, and the
hand with the greater card value that is found first is the stronger hand."
  (or (< (hand-type hand1) (hand-type hand2))
      (and (= (hand-type hand1) (hand-type hand2))
           (loop for card1 across (cards hand1)
                 for card2 across (cards hand2)
                 when (< card1 card2)
                 do (return t)
                 when (> card1 card2)
                 do (return nil)))))

(defun total-winnings (lines card-values &optional joker)
  (loop for hand in (sort (parse-input-07 lines card-values joker) #'hand-strength<)
        for rank = 1 then (1+ rank)
        summing (* rank (hand-bid hand))))
