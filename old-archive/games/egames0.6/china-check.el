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
;;;  Under tyrant-mode for TALK.  To run this way, use "talk"            ;;;
;;;  and once a connection is established, use the game playing          ;;;
;;;  function "talk-initiate-special-function" bound to C-c g            ;;;
;;;  to start.  Must be installed on all systems.                        ;;;
;;;                                                                      ;;;
;;;  Under tyrant-ai.  To run, load the library "tyrn-ai" and then use   ;;;
;;;  the function "tyrant-play-computer" and choose this game.  An AI    ;;;
;;;  function must be installed on the system for this to work.          ;;;
;;;                                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'china-check)

(defvar china-check-map nil
  "Keymap used in playing chineese checkers")

(if china-check-map
    ()
  (setq china-check-map (make-sparse-keymap))
  (define-key china-check-map "" 'china-check-move)
  (define-key china-check-map "f" 'china-check-move)
  (define-key china-check-map "" 'china-check-move)
  (define-key china-check-map "b" 'china-check-move)
  (define-key china-check-map "" 'china-check-move)
  (define-key china-check-map "p" 'china-check-move)
  (define-key china-check-map "" 'china-check-move)
  (define-key china-check-map "n" 'china-check-move)
  (define-key china-check-map "y" 'china-check-move)
  (define-key china-check-map "u" 'china-check-move)
  (define-key china-check-map "g" 'china-check-move)
  (define-key china-check-map "j" 'china-check-move)
  (define-key china-check-map "b" 'china-check-move)
  (define-key china-check-map "n" 'china-check-move)
  (define-key china-check-map " " 'china-check-grab-piece)
  (define-key china-check-map "h" 'china-check-grab-piece)
  (define-key china-check-map "q" 'china-check-quit)
)

(defconst china-check-piece1 "O"
  "String as piece 1")

(defconst china-check-piece2 "X"
  "String as piece 2")

(defun china-check ()
  "Mode for playing chinese checkers"
  (interactive)
  (switch-to-buffer (get-buffer-create "Chinese Checkers"))
  (setq mode-name "C-C")
  (setq major-mode 'china-check)
  (delete-region (point-min)(point-max))
  (insert "+------- Chinese Checkers! -------+
|              / \\                |
| Move Score  / O \\ Directionals  |
|            / O O \\     Y U      |
|           / O O O \\   G H J     |
|          / O O O O \\   B N      |
|_________/ O O O O O \\___________|
|\\ . . . . . . . . . . . . . . . /|
| \\ . . . . . . . . . . . . . . / |
|  \\ . . . . . . . . . . . . . /  |
|   \\ . . . . . . . . . . . . /   |
|    > . . . . . . . . . . . <    |
|   / . . . . . . . . . . . . \\   |
|  / . . . . . . . . . . . . . \\  |
| / . . . . . . . . . . . . . . \\ |
|/ . . . . . . . . . . . . . . . \\|
|---------\\ X X X X X /-----------|
|          \\ X X X X /            |
|           \\ X X X / Move Score  |
|            \\ X X /              |
|             \\ X /               |
|              \\ /                |
+---------------------------------+")
  (delete-other-windows (selected-window))
  (use-local-map china-check-map)
  (make-local-variable 'tyrant-turn)
  (setq tyrant-turn 1)
  (make-local-variable '4x)
  (setq 4x 13)
  (make-local-variable '4y)
  (setq 4y 0)
  (make-local-variable '1x)
  (setq 1x 13)
  (make-local-variable '1y)
  (setq 1y 0)
  (make-local-variable '2x)
  (setq 2x 13)
  (make-local-variable '2y)
  (setq 2y 18)
  (make-local-variable 'p1-score)
  (setq p1-score 0)
  (make-local-variable 'p2-score)
  (setq p2-score 0)
  (make-local-variable 'china-grab)
  (setq china-grab nil)
  (make-local-variable 'talk-tyrant-brief-help)
  (setq talk-tyrant-brief-help
"ChinaCheck: Directional move [SPC] Grab, Directionals move piece [H] done.")
  (china-check-place-cursor)
  (setq tyrant-player1-hook
	'(lambda ()
	   (message "You are player 1")))
  (setq tyrant-player2-hook
	'(lambda ()
	   (setq talk-tyrant-enabled-console nil)
	   (message "Your are player 2"))) ;player2 goes 2nd
 )

(defun china-check-grab-piece ()
  "Mark the peice as nabbed for future reference"
  (interactive)
  (if china-grab
      (if (listp china-grab)
	  (progn
	    (china-check-put (nth 0 china-grab) (nth 1 china-grab) t t)
	    (china-check-put (nth 0 china-grab) (nth 1 china-grab))
	    (setq china-grab nil))
	(china-check-put 4x 4y t t)
	(china-check-put 4x 4y)
	(setq china-grab nil)
	(china-check-swap-turns))
    (if (not (= (china-check-owned 4x 4y) tyrant-turn))
	(error "You don't have a piece in that location!")
      (setq china-grab (list 4x 4y))
      (china-check-put 4x 4y t))))
      
(defun china-check-move ()
  "Move the cursor to a new position"
  (interactive)
  (let ((dx 0) (dy 0))
    (cond
     ((or (= last-input-char ?\C-f)
	  (= last-input-char ?f)
	  (= last-input-char ?j))
      (if (china-check-owned (+ 4x 2) 4y)
	  (progn (setq dx 2) (setq dy 0))
	(error "Can't go farther right!")))
     ((or (= last-input-char ?\C-b)
	  (= last-input-char ?g))
      (if (china-check-owned (- 4x 2) 4y)
	  (progn (setq dx -2) (setq dy 0))
	(error "Can't go farther left!")))
     ((or (= last-input-char ?\C-p)
	  (= last-input-char ?p)
	  (= last-input-char ?y))
      (if (china-check-owned (1- 4x) (1- 4y))
	  (progn (setq dy -1) (setq dx -1))
	(error "Can't go farther up!")))
     ((= last-input-char ?u)
      (if (china-check-owned (1+ 4x) (1- 4y))
	  (progn (setq dy -1) (setq dx +1))
	(error "Cant go farther up!")))
     ((or (= last-input-char ?\C-n)
	  (= last-input-char ?n))
      (if (china-check-owned (1+ 4x) (1+ 4y))
	  (progn (setq dy +1) (setq dx +1))
	(error "Can't go farther down!")))
     ((= last-input-char ?b)
      (if (china-check-owned (1- 4x) (1+ 4y))
	  (progn (setq dy +1) (setq dx -1))
	(error "Can't go farther down!"))))
    (if (not china-grab)
	(progn
	  (setq 4x (+ 4x dx))
	  (setq 4y (+ 4y dy)))
      (if (= (china-check-owned (+ 4x dx) (+ 4y dy)) 0)
	  (if (not (listp china-grab))
	      (error "You may only jump now...")
	    (china-check-put 4x 4y t t)
	    (setq 4x (+ 4x dx))
	    (setq 4y (+ 4y dy))
	    (china-check-put 4x 4y)
	    (setq china-grab nil)
	    (if (= tyrant-turn 1)
		(setq p1-score (+ p1-score dy))
	      (setq p2-score (- p2-score dy)))
	    (china-check-swap-turns))
	(if (eq (china-check-owned (+ 4x dx dx) (+ 4y dy dy)) 0)
	    (progn
	      (china-check-put 4x 4y t t)
	      (setq 4x (+ 4x dx dx))
	      (setq 4y (+ 4y dy dy))
	      (setq china-grab t)
	      (china-check-put 4x 4y t)
	      (if (= tyrant-turn 1)
		  (setq p1-score (+ p1-score (* dy 2)))
		(setq p2-score (- p2-score (* dy 2)))))
	  (error "Can't move your stone that way!")))))
  (china-check-place-cursor))

(defun china-check-owned (x y)
  "returns nil if empty, 1 if player 1 peice, and 2 if player 2 peice"
  (save-excursion
    (goto-char (+ (china-check-xy2index x y) 1))
    (cond
     ((= (following-char) ?.)
      0)
     ((= (following-char) (string-to-char china-check-piece1))
      1)
     ((= (following-char) (string-to-char china-check-piece2))
      2)
     (t nil))))

(defun china-check-place-cursor ()
  "Place the cursor on the correct spot on the board..."

  (goto-char (+ 1 (china-check-xy2index 4x 4y))))

(defun china-check-put (x y &optional marked off)
  "Based on variable \"turn\" place peice there."

  (save-excursion
    (goto-char (+ 1 (china-check-xy2index x y)))
    (if marked 
	(progn (forward-char -1)
	       (delete-char 3))
      (delete-char 1))
    (insert (if (equal off t)
		(if marked " . " ".")
	      (if (= tyrant-turn 1)
		  (if marked (concat ">" china-check-piece1 "<")
		    china-check-piece1)
		(if marked (concat ">" china-check-piece2 "<")
		  china-check-piece2))))))

(defun china-check-xy2index (x y)
  "change x y position to absolute buffer address"

  (+ (+ (+ 6 x) (+ (* y 36) 1)) 68))

(defun china-check-win-string ()
  "generates a string declairing who won."
  (china-check-swap-turns)
  (if (and (boundp 'talk-tyrannical-mode) talk-tyrannical-mode)  
      (tyrant-format (if (> p1-score p2-score)
			 "%n wins!!!!"
		       "%N wins!!!"))
    (message "Player %s wins!" (if (> p1-score p2-score) 1 2))))

(defun china-check-swap-turns ()
  "Swap turns in connect 4"

  (cond
   ((= tyrant-turn 1) 
    (setq tyrant-turn 2)
    (setq 1x 4x) (setq 1y 4y)
    (setq 4x 2x) (setq 4y 2y))
   ((= tyrant-turn 2) 
    (setq tyrant-turn 1)
    (setq 2x 4x) (setq 2y 4y)
    (setq 4x 1x) (setq 4y 1y)))
  (if (or (= p1-score 194) (= p2-score 194))
      (progn
	(if (and (boundp 'talk-tyrannical-mode) talk-tyrannical-mode)
	    (talk-usurp-tyrant (china-check-win-string))
	  (message (china-check-win-string)))))
  (goto-char 113)
  (delete-char 4)
  (insert (format "%4d" p1-score))
  (goto-char 709)
  (delete-char 4)
  (insert (format "%4d" p2-score))
  ;; tyrant mode support:
  ;; flip turns accross net each time.
  (if (boundp 'talk-tyrant-enabled-console)
      (progn
	(message (tyrant-format "It is now %P's turn."))
	(setq talk-tyrant-enabled-console (not talk-tyrant-enabled-console)))
    (message "Player %d's move." tyrant-turn))
  (china-check-place-cursor))
