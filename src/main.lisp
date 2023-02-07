(defpackage yahtzee
  (:use :cl))
(in-package :yahtzee)

(defconstant +dice-sides+ 6 "The number of sides for the dice to be rolled")
(defparameter *dice* `((1 . nil) (2 . nil) (3 . nil) (4 . nil) (5 . nil) (6 . nil)))
(defparameter *score* `((Aces . 0)
                        (Twos . 0)
                        (Threes . 0)
                        (Fours . 0)
                        (Sixes . 0)
                        (Three-of-a-kind . 0)
                        (Four-of-a-kind . 0)
                        (Full-House . 0)
                        (Small-Straight . 0)
                        (Large-Straight . 0)
                        (Yahtzee . 0)
                        (Chance . 0)))

(defvar *players* () "Store each of the players")
(defvar *number-players* 0)

(setf *random-state* (make-random-state t))


;;;  Dice functions

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

(defun print-dice (dice)
  "Print a dice value"
  (format t " Dice ~a: ~a~&" (car dice) (cdr dice)))

(format t "~a~&" (roll-select-dice 1 2 3 4 5 6))

(defun print-all-dice ()
  "Print all of the dice"
  (format t "Current Dice:~&")
  (mapcar #'print-dice *dice*))

(print-all-dice)


;;; Player functions

(defun create-player ()
  "Create a new player for the game"
  (copy-tree *score*))

(defun add-player ()
  "Add a new player"
  (setf *number-players* (1+ *number-players*))
  (setf *players* (cons (cons *number-players* (create-player)) *players*)))

;; Creating a players first move
(defun player-turn (player)
  "Get a player and begin their turn"
  (let ((p (assoc player *players*))
        (vals (loop :for i :from 1 :below (+ 1 +dice-sides+) :collect i)))
    (progn
      (print-all-dice)
      (apply #'roll-select-dice vals)
      (print-all-dice))))
;; Need to collect user input in a turn
    

(player-turn 1)
(print *players*)
(add-player)

(print ())
(setf *players* ())
(setf *number-players* 0)
