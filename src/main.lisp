(ql:quickload :cl-ppcre)
(defpackage yahtzee
  (:use :cl :cl-ppcre))
(in-package :yahtzee)

(defconstant +dice-sides+ 6 "The number of sides for the dice to be rolled")
(defparameter *dice* `((1 . nil) (2 . nil) (3 . nil) (4 . nil) (5 . nil)))
(defparameter *score* `(("ACES" . 0)
                        ("TWO" . 0)
                        ("THREES" . 0)
                        ("FOURS" . 0)
                        ("SIXES" . 0)
                        ("THREE-OF-A-KIND" . 0)
                        ("FOUR-OF-A-KIND" . 0)
                        ("FULL-HOUSE" . 0)
                        ("SMALL-STRAIGHT" . 0)
                        ("LARGE-STRAIGHT" . 0)
                        ("YAHTZEE" . 0)
                        ("CHANCE" . 0)))

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
;; Breakdown of logic,
;; need to initialize rolls
;; ask if they want to claim on of the game conditions
;; either update their score or roll again
;; ask if the want to claim one of the game conditions or roll again
;; roll again, force to claim one of the conditinos or pass
(defun player-turn (player)
  "Get a player and begin their turn"
  (let ((p (assoc player *players*))
        (vals (loop :for i :from 1 :below (+ 1 +dice-sides+) :collect i)))
    (progn
      (print-all-dice)
      (apply #'roll-select-dice vals)
      (print-all-dice))))

(defun pprint-list (list-player)
  "Print a list of the yahtzee outputs"
  (if list-player
      (progn
        (format t "~a: ~a ~&" (car (car list-player)) (cdr (car list-player)))
        (pprint-list (cdr list-player)))))

(defun pprint-player (player)
  "Pretty print the players current score"
  (let ((score (cdr player)))
    (pprint-list score)))

(defun game-condition-update (condition player dice)
  "Select the condition to be updated with the dice score"
  (let* ((value (string-upcase condition))
         (val-update (assoc value (car player))))
    (if val-update
        (setf (assoc value (car player)) (sum dice))
        (print "Did not deal with incorrect inputs yet, tehe"))))


;; Need to collect user input in a turn
(defun user-loop (player)
  "Ask which dice to roll or to claim a game condition"
  (format t "Enter dice you wish to roll again or the game condition you wish to play~&")
  (pprint-player player)
  (let* ((input (read-line)) (text (cl-ppcre:split " " input :with-registers-p t)))
    (print text)))


(player-turn 1)
(print *players*)
(add-player)

(user-loop (assoc 1 *players*))
(setf *players* ())
(setf *number-players* 0)
