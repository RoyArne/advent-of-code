
(cl:in-package #:asdf-user)

(defsystem "advent-of-code"
  :name "Advent of Code"
  :version "0.0.1"
  :depends-on ("cl-ppcre" "split-sequence")
  :serial t
  :components
  ((:file "advent-of-code")
   (:file "utilities")))
