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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'reversi)

(defvar reversi-map nil
  "Keymap used in playing reversi")

(if reversi-map
    ()
  (setq reversi-map (make-sparse-keymap))
  (define-key reversi-map "" 'reversi-move)  
  (define-key reversi-map "f" 'reversi-move)
  (define-key reversi-map "" 'reversi-move)
  (define-key reversi-map "b" 'reversi-move)
  (define-key reversi-map "" 'reversi-move)
  (define-key reversi-map "p" 'reversi-move)
  (define-key reversi-map "" 'reversi-move)
  (define-key reversi-map "n" 'reversi-move)
  (define-key reversi-map "g" 'reversi-pass)
  (define-key reversi-map " " 'reversi-place-piece)
  (define-key reversi-map "q" 'reversi-quit)
)

(defconst reversi-piece1 "##"
  "String as piece 1")

(defconst reversi-piece2 "::"
  "String as piece 2")

(defconst reversi-buff "REVERSI")
(defconst reversi-stat "Reversi Stats")

(defun reversi ()
  "Mode for playing reversi
Rules:  You may only move to an empty square in which an oponants peice is 
        between current square and another one of your peices.
        Taking this square takes the peices of the oponant between your
        pieces.  Winner has the most peices.
Playing:
        C-fbnp moves point
        SPC    takes square"

  (interactive)
  (switch-to-buffer (get-buffer-create reversi-buff))
  (setq mode-name "Reversi")
  (setq major-mode 'reversi)
  (delete-region (point-min)(point-max))
  (insert "+----+----+----+----+----+----+----+----+
|    |    |    |    |    |    |    |    |
+----+----+----+----+----+----+----+----+
|    |    |    |    |    |    |    |    |
+----+----+----+----+----+----+----+----+
|    |    |    |    |    |    |    |    |
+----+----+----+----+----+----+----+----+
|    |    |    | ## | :: |    |    |    |
+----+----+----+----+----+----+----+----+
|    |    |    | :: | ## |    |    |    |
+----+----+----+----+----+----+----+----+
|    |    |    |    |    |    |    |    |
+----+----+----+----+----+----+----+----+
|    |    |    |    |    |    |    |    |
+----+----+----+----+----+----+----+----+
|    |    |    |    |    |    |    |    |
+----+----+----+----+----+----+----+----+")
  (delete-other-windows (selected-window))
  (use-local-map reversi-map)
  (make-local-variable 'tyrant-turn)
  (setq tyrant-turn 1)
  (make-local-variable 'rx)
  (setq rx 0)
  (make-local-variable 'ry)
  (setq ry 0)
  (make-local-variable 'p1-score)
  (setq p1-score 2)
  (make-local-variable 'p2-score)
  (setq p2-score 2)
  (make-local-variable 'reversi-delta)
  (make-local-variable 'talk-tyrant-brief-help)
  (setq talk-tyrant-brief-help
"REVERSI: [C-f,f] Forward [C-b,b] Back [C-p,p] Previous [C-n,n] Next [SPC] GO")
  (make-local-variable 'talk-tyrant-quit-string)
  (fset talk-tyrant-quit-string 'reversi-win-string)
  (split-window (selected-window) 50 t)
  (other-window 1)
  (switch-to-buffer (get-buffer-create reversi-stat))
  (delete-region (point-min) (point-max))
  (insert "Player 1 score:
  2

Player 2 score:
  2

Last move:
  - -

Delta:
  - -


")
  (other-window 1)
  (setq tyrant-player1-hook 
	'(lambda ()
	   (message "You are player 1")
	   (save-excursion
	     (switch-to-buffer reversi-stat)
	     (make-local-variable 'talk-tag)
	     (setq talk-tag t)
	     (goto-line 1)
	     (delete-region (point) (save-excursion (end-of-line) (point)))
	     (insert (tyrant-format "%n score:"))
	     (goto-line 4)
	     (delete-region (point) (save-excursion (end-of-line) (point)))
	     (insert (tyrant-format "%N score:")))))
  (setq tyrant-player2-hook
	'(lambda ()
	   (setq talk-tyrant-enabled-console nil)
	   (message "Your are player 2")
	   (save-excursion
	     (switch-to-buffer reversi-stat)
	     (make-local-variable 'talk-tag)
	     (setq talk-tag t)
	     (goto-line 1)
	     (delete-region (point) (save-excursion (end-of-line) (point)))
	     (insert (tyrant-format "%n score:"))
	     (goto-line 4)
	     (delete-region (point) (save-excursion (end-of-line) (point)))
	     (insert (tyrant-format "%N score:")))))
  )

(defun reversi-place-piece ()
  "Put a piece onto the board."

  (interactive)
  (if (not (= (reversi-owned rx ry) 0))
      (error "Player %s is already there!" (reversi-owned rx ry))
    (if (reversi-legal)
	(progn
	  (setq reversi-delta 0)
	  (reversi-put rx ry)
	  (if (= tyrant-turn 1)
	      (setq p1-score (1+ p1-score))
	    (setq p2-score (1+ p2-score)))
	  (if (reversi-check-dir 0  1) (reversi-check-dir 0  1 t))
	  (if (reversi-check-dir 0 -1) (reversi-check-dir 0 -1 t))
	  (if (reversi-check-dir 1  0) (reversi-check-dir 1  0 t))
	  (if (reversi-check-dir -1 0) (reversi-check-dir -1 0 t))
	  (if (reversi-check-dir 1  1) (reversi-check-dir 1  1 t))
	  (if (reversi-check-dir -1 -1) (reversi-check-dir -1 -1 t))
	  (if (reversi-check-dir 1 -1) (reversi-check-dir 1  -1 t))
	  (if (reversi-check-dir -1 1) (reversi-check-dir -1 1 t))
	  (if (reversi-win)
	      (if (and (boundp 'talk-tyrannical-mode) talk-tyrannical-mode)
		  (talk-usurp-tyrant (reversi-win-string))
		(message (reversi-win-string)))
	    (reversi-swap-turns)))
      (error "Not a legal move."))))

(defun reversi-win-string ()
  "generates a string declairing who won."
  (reversi-swap-turns)
  (if (and (boundp 'talk-tyrannical-mode) talk-tyrannical-mode)  
      (tyrant-format (if (> p1-score p2-score)
			 "%n wins!!!!"
		       "%N wins!!!"))
    (message "Player %s wins!" (if (> p1-score p2-score) 1 2))))

(defun reversi-win ()
  "return t if that is a winning move"
  (or (= (+ p1-score p2-score) 64)
      (= p1-score 0)
      (= p2-score 0))
  )

(defun reversi-legal ()
  "return if the location is a legal move."
  (let ((x rx) (y ry))
    (or (reversi-check-dir 0  1) (reversi-check-dir 0 -1)
	(reversi-check-dir 1  0) (reversi-check-dir -1 0)
	(reversi-check-dir 1  1) (reversi-check-dir -1 -1)
	(reversi-check-dir 1  -1) (reversi-check-dir -1 1))))

(defun reversi-check-dir (dx dy &optional place)
  "returns t if it is a legal direction.  Legal being when the
neighboring pieces are the opponant, and the last peice is yours.  If
place then snarf them for your own."

  (let ((x (+ rx dx)) (y (+ ry dy)) (number 0) (flag nil))
    (while (and (and (and (>= x 0) (<= x 7))  ;; positionally ok
		     (and (>= y 0) (<= y 7)))
		(or  (= (if (= tyrant-turn 1) 2 1)
			(reversi-owned x y)))) ;not me first
      (if place 
	  (progn
	    (reversi-put x y)
	    (setq reversi-delta (1+ reversi-delta))
	    (if (= tyrant-turn 1)
		(progn
		  (setq p1-score (1+ p1-score))
		  (setq p2-score (1- p2-score)))
	      (setq p2-score (1+ p2-score))
	      (setq p1-score (1- p1-score)))))
      (setq number (1+ number))
      (setq x (+ x dx))
      (setq y (+ y dy)))
    (if (and (< 0 number) (= tyrant-turn (reversi-owned x y))) (setq flag t))
    flag))

(defun reversi-owned (x y)
  "returns nil if empty, 1 if player 1 peice, and 2 if player 2 peice"
  (if (or (< x 0) (> x 7) (< y 0) (> y 7))
      0
    (save-excursion
      (goto-char (+ (reversi-xy2index x y) 1))
      (cond
       ((= (following-char) ?\ )
	0)
       ((= (following-char) (string-to-char reversi-piece1))
	1)
       ((= (following-char) (string-to-char reversi-piece2))
	2)
       (t nil)))))

(defun reversi-move ()
  "Move the cursor to a new position"
  (interactive)
  (cond
   ((or (= last-input-char ?\C-f)
	(= last-input-char ?f))
    (if (< rx 7)
	(setq rx (+ rx 1))
      (error "Can't go farther right!")))
   ((or (= last-input-char ?\C-b)
	(= last-input-char ?b))
    (if (> rx 0)
	(setq rx (- rx 1))
      (error "Can't go farther left!")))
   ((or (= last-input-char ?\C-p)
	(= last-input-char ?p))
    (if (> ry 0)
	(setq ry (- ry 1))
      (error "Can't go farther up!")))
   ((or (= last-input-char ?\C-n)
	(= last-input-char ?n))
    (if (< ry 7)
	(setq ry (+ ry 1))
      (error "Can't go farther down!"))))
  (reversi-place-cursor))

(defun reversi-place-cursor ()
  "Place the cursor on the correct spot on the board..."

  (goto-char (+ 1 (reversi-xy2index rx ry))))

(defun reversi-put (x y &optional off)
  "Based on variable \"turn\" place peice there."

  (save-excursion
    (goto-char (+ 1 (reversi-xy2index x y)))
    (delete-char 2)
    (insert (if (equal off t)
		"  "
	      (if (equal off 1)
		  reversi-piece-win
		(if (= tyrant-turn 1)
		    reversi-piece1
		  reversi-piece2))))))
  
(defun reversi-xy2index (x y)
  "change x y position to absolute buffer address"

  (+ (+ (+ 1 (* x 5)) (+ (* y 84) 1)) 42))

(defun reversi-pass ()
  "Pass the buck"
  (interactive)
  (setq reversi-delta 0)
  (reversi-swap-turns))

(defun reversi-swap-turns ()
  "Swap turns in reversi"

  (save-excursion
    (let ((s1 (format "  %d blocks" p1-score))
	  (s2 (format "  %d blocks" p2-score))
	  (s3 (if (and (boundp 'talk-tyrannical-mode) talk-tyrannical-mode)
		  (tyrant-format " %:1P moved to %d, %d" rx ry)
		(format " Player %d moved to %d, %d" tyrant-turn rx ry)))
	  (s4 (if (and (boundp 'talk-tyrannical-mode) talk-tyrannical-mode)
		  (tyrant-format " %:1P steals %d blocks" reversi-delta)
		(format " Player %d steals %d blocks" tyrant-turn 
			reversi-delta))))
      (set-buffer reversi-stat)
      (goto-line 2)
      (delete-region (point) (save-excursion (end-of-line) (point)))
      (insert s1)
      (goto-line 5)
      (delete-region (point) (save-excursion (end-of-line) (point)))
      (insert s2)
      (goto-line 8)
      (delete-region (point) (save-excursion (end-of-line) (point)))
      (insert s3)
      (goto-line 11)
      (delete-region (point) (save-excursion (end-of-line) (point)))
      (insert s4)))
  (cond
   ((= tyrant-turn 1) (setq tyrant-turn 2))
   ((= tyrant-turn 2) (setq tyrant-turn 1)))
  ;; tyrant mode support:
  ;; flip turns accross net each time.
  (if (boundp 'talk-tyrant-enabled-console)
      (progn
	(message (tyrant-format "It is now %P's turn."))
	(setq talk-tyrant-enabled-console (not talk-tyrant-enabled-console)))
    (message "Player %d's move." tyrant-turn)))
