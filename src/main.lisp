(defpackage yahtzee
  (:use :cl))
(in-package :yahtzee)

(defconstant +dice-sides+ 6 "The number of sides for the dice to be rolled")
(defparameter *dice* `((1 . nil) (2 . nil) (3 . nil) (4 . nil) (5 . nil) (6 . nil)))

(setf *random-state* (make-random-state t))


(defun get-random-value (mod-val)
  "return a random value between 1 and the mod-val"
  (1+ (mod (random 777) mod-val)))


(defun draw-dice (val)
  "Draw the current dice to the screen"
  (cond
    ((= val 1) (format t "~a~&" val))
    ((= val 2) (format t "~a~&" val))
    ((= val 3) (format t "~a~&" val))
    ((= val 4) (format t "~a~&" val))
    ((= val 5) (format t "~a~&" val))
    ((= val 6) (format t "~a~&" val))))

(defun roll-dice (dice)
  "roll the dice taken from the dice parameter"
  (let ((ds dice))
    (setf (cdr ds) (get-random-value +dice-sides+))))


(defun roll-select-dice (&rest args)
  "Roll the selected dice that the user wishes to change"
  (dolist (temp args) (setf (cdr (assoc temp *dice*)) (get-random-value +dice-sides+))))


(format t "~a~&" (roll-select-dice 1 2 3 4 5 6))
