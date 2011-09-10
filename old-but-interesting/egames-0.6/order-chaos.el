;;;
;;; Copyright (C) 1992 Eric M. Ludlam
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, you can either send email to this
;;; program's author (see below) or write to:
;;;
;;;              The Free Software Foundation, Inc.
;;;              675 Mass Ave.
;;;              Cambridge, MA 02139, USA. 
;;;
;;; Please send bug reports, etc. to zappo@gnu.ai.mit.edu.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                      ;;;
;;;                          TYRANT MODE GAME                            ;;;
;;;                                                                      ;;;
;;;  This program contains a program designed for use with               ;;;
;;; tyrant-mode.  This program may be used under the following           ;;;
;;; software conditions.                                                 ;;;
;;;                                                                      ;;;
;;;  By itself with no tyrant support.  All extraneous keys will mess    ;;;
;;;  up the information in a given buffer.                               ;;;
;;;                                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'order-chaos)

(defvar order-chaos-map nil
  "Keymap used in playing 4 by 4")

(if order-chaos-map
    ()
  (setq order-chaos-map (make-sparse-keymap))
  (define-key order-chaos-map "" 'order-chaos-move)  
  (define-key order-chaos-map "f" 'order-chaos-move)
  (define-key order-chaos-map "" 'order-chaos-move)
  (define-key order-chaos-map "b" 'order-chaos-move)
  (define-key order-chaos-map "" 'order-chaos-move)
  (define-key order-chaos-map "p" 'order-chaos-move)
  (define-key order-chaos-map "" 'order-chaos-move)
  (define-key order-chaos-map "n" 'order-chaos-move)
  (define-key order-chaos-map "x" 'order-chaos-place-piece)
  (define-key order-chaos-map "o" 'order-chaos-place-piece)
  (define-key order-chaos-map "q" 'order-chaos-quit)
)

(defconst order-chaos-piece1 [ " /XX\\ " " \\XX/ "]
  "String as piece 1")

(defconst order-chaos-piece2 [ " /--\\ " " \\__/ " ]
  "String as piece 2")

(defconst order-chaos-piece-win [ " /\\/\\ " " \\/\\/ " ]
  "String as piece 2")

(defun order-chaos ()
  "Mode for playing order and chaos.
Rules:  ORDER must get 5 in a row of either X or O pieces.
        CHAOS must prevent order from doing that.
      Either player may place an X or O peice as they wish.
   C-f, C-b, C-p, C-n : movement
   x                  : X piece
   o                  : O piece"

  (interactive)
  (switch-to-buffer (get-buffer-create "Order and ChAoS"))
  (setq mode-name "O&C")
  (setq major-mode 'order-chaos)
  (delete-region (point-min)(point-max))
  (insert "+---------------------------------------------------------------------+
|  ----       +------+------+------+------+------+------+     ----\\   |
| /    \\      |      |      |      |      |      |      |    /        |
| \\____/      |      |      |      |      |      |      |    |        |
| ----        +------+------+------+------+------+------+    \\----/   |
| |   \\       |      |      |      |      |      |      |             |
| |---/       |      |      |      |      |      |      |    |    |   |
| |  \\        +------+------+------+------+------+------+    |----|   |
| ----        |      |      |      |      |      |      |    |    |   |
| |   \\       |      |      |      |      |      |      |             |
| |   |       +------+------+------+------+------+------+      /-\\    |
| |___/       |      |      |      |      |      |      |     /   \\   |
|  ____       |      |      |      |      |      |      |     |---|   |
| |           +------+------+------+------+------+------+     |   |   |
| |---        |      |      |      |      |      |      |      ---    |
| |____       |      |      |      |      |      |      |     /   \\   |
| ----        +------+------+------+------+------+------+     \\___/   |
| |   \\       |      |      |      |      |      |      |     .--     |
| |---/       |      |      |      |      |      |      |     |__.    |
| |  \\        +------+------+------+------+------+------+      __|    |
+---------------------------------------------------------------------+")
  (delete-other-windows (selected-window))
  (use-local-map order-chaos-map)
  (make-local-variable 'tyrant-turn)
  (setq tyrant-turn 1)
  (make-local-variable '4x)
  (setq 4x 0)
  (make-local-variable '4y)
  (setq 4y 0)
  (make-local-variable 'moves)
  (setq moves 36)
  (make-local-variable 'talk-tyrant-brief-help)
  (setq talk-tyrant-brief-help
	"O&C: [C-fbnp] Move [x] Place an X piece [o] place an O piece.")
  (setq tyrant-player1-hook 
	'(lambda ()
	   (message "You are ORDER.")))
  (setq tyrant-player2-hook
	'(lambda ()
	   (setq talk-tyrant-enabled-console nil)
	   (message "You are CHAOS."))) ;player2 goes 2nd
  )

(defun order-chaos-win (move-piece)
  "return t if that is a winning move"
  (cond
   ((>= (+ 1 (order-chaos-check-dir 0 1 move-piece)
	   (order-chaos-check-dir 0 -1 move-piece)) 5)
    (order-chaos-check-dir 0 1 move-piece t) 
    (order-chaos-check-dir 0 -1 move-piece t)
    (order-chaos-put 4x 4y 3) t)
   ((>= (+ 1 (order-chaos-check-dir 1 0 move-piece) 
	   (order-chaos-check-dir -1 0 move-piece)) 5)
    (order-chaos-check-dir 1 0 move-piece t) 
    (order-chaos-check-dir -1 0 move-piece t)
    (order-chaos-put 4x 4y 3) t)
   ((>= (+ 1 (order-chaos-check-dir 1 1 move-piece) 
	   (order-chaos-check-dir -1 -1 move-piece)) 5)
    (order-chaos-check-dir 1 1 move-piece t) 
    (order-chaos-check-dir -1 -1 move-piece t)
    (order-chaos-put 4x 4y 3) t)
   ((>= (+ 1 (order-chaos-check-dir -1 1 move-piece) 
	   (order-chaos-check-dir 1 -1 move-piece)) 5)
    (order-chaos-check-dir -1 1 move-piece t) 
    (order-chaos-check-dir 1 -1 move-piece t)
    (order-chaos-put 4x 4y 3) t)
   (t nil)))

(defun order-chaos-check-dir (dx dy &optional count place)
  "if count: returns t if supported by a side
else returns # of peices of player counts in that direction"
  (let ((x (+ 4x dx)) (y (+ 4y dy)) (number 0))
    (while (and (and (and (>= x 0) (<= x 5))  ;; positionally ok
		     (and (>= y 0) (<= y 5)))
		(or  (and (not count) (order-chaos-owned x y))
		     (and count (eq (order-chaos-owned x y) count))))
      (setq number (+ number 1))
      (if place (order-chaos-put x y 3))
      (setq x (+ x dx))
      (setq y (+ y dy)))
    (if count
	number
      (or (= x -1) (= x 8) (= y -1) (= y 8)))))

(defun order-chaos-owned (x y)
  "returns nil if empty, 1 if player 1 peice, and 2 if player 2 peice"
  (save-excursion
    (goto-char (+ (order-chaos-xy2index x y) 2))
    (cond
     ((= (following-char) ?\ )
      nil)
     ((= (following-char) ?X)
      1)
     ((= (following-char) ?-)
      2)
     (t nil))))

(defun order-chaos-move ()
  "Move the cursor to a new position"
  (interactive)
  (cond
   ((or (= last-input-char ?\C-f)
	(= last-input-char ?f))
    (if (< 4x 5)
	(setq 4x (+ 4x 1))
      (error "Can't go farther right!")))
   ((or (= last-input-char ?\C-b)
	(= last-input-char ?b))
    (if (> 4x 0)
	(setq 4x (- 4x 1))
      (error "Can't go farther left!")))
   ((or (= last-input-char ?\C-p)
	(= last-input-char ?p))
    (if (> 4y 0)
	(setq 4y (- 4y 1))
      (error "Can't go farther up!")))
   ((or (= last-input-char ?\C-n)
	(= last-input-char ?n))
    (if (< 4y 5)
	(setq 4y (+ 4y 1))
      (error "Can't go farther down!"))))
  (order-chaos-place-cursor))

(defun order-chaos-place-cursor ()
  "Place the cursor on the correct spot on the board..."

  (goto-char (+ 1 (order-chaos-xy2index 4x 4y))))

(defun order-chaos-place-piece ()
  "Actuall place the piece on the board"

  (interactive)
  (if (= moves 0)
      (error "Game is over man!"))
  (if (= last-input-char ?x)
      (order-chaos-put 4x 4y 1)
    (order-chaos-put 4x 4y 2))
  (sit-for 0)
  (if (order-chaos-win (if (= last-input-char ?x) 1 2))
      (progn
	(if (and (boundp 'talk-tyrannical-mode) talk-tyrannical-mode)
	    (talk-usurp-tyrant "ORDER WINS!")
	  (message "ORDER WINS!")))
    (setq moves (1- moves))
    (if (= moves 0)
	(progn
	  (if (and (boundp 'talk-tyrannical-mode) talk-tyrannical-mode)
	      (talk-usurp-tyrant "CHAOS WINS!")
	    (message "CHAOS WINS!")))
      (order-chaos-swap-turns))))

(defun order-chaos-put (x y player)
  "Place a piece made of char at position x and y"
  
  (if (and (order-chaos-owned x y) 
	   (not (= player 3)))
      (error "Piece already there!"))
  (cond
   ((equal player 0) (setq str1 "      ") (setq str2 "      "))
   ((equal player 1) 
    (setq str1 (aref order-chaos-piece1 0))
    (setq str2 (aref order-chaos-piece1 1)))
   ((equal player 2) 
    (setq str1 (aref order-chaos-piece2 0))
    (setq str2 (aref order-chaos-piece2 1)))
   ((equal player 3) 
    (setq str1 (aref order-chaos-piece-win 0))
    (setq str2 (aref order-chaos-piece-win 1))))

  (save-excursion
    (goto-char (order-chaos-xy2index x y)) (delete-char 6) (insert str1)
    (goto-char ( + (order-chaos-xy2index x y) 72)) (delete-char 6) 
    (insert str2))
)

(defun order-chaos-xy2index (x y)
  "change x y position to absolute buffer address"

  (+ (+ (+ 15 (* x 7)) (+ (* y 216) 1)) 144))

(defun order-chaos-swap-turns ()
  "Swap turns in connect 4"

  (cond
   ((= tyrant-turn 1) (setq tyrant-turn 2))
   ((= tyrant-turn 2) (setq tyrant-turn 1)))
  ;; tyrant mode support:
  ;; flip turns accross net each time.
  (if (boundp 'talk-tyrant-enabled-console)
      (setq talk-tyrant-enabled-console (not talk-tyrant-enabled-console)))
  (message "Player %s's move." (if (= tyrant-turn 1) "ORDER" "CHAOS")))
