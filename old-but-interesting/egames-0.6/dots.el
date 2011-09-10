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

(provide 'dots)

(defvar dots-map nil
  "Keymap used in playing connect 4")

(if dots-map
    ()
  (setq dots-map (make-sparse-keymap))
  (define-key dots-map "" 'dots-move)  
  (define-key dots-map "f" 'dots-move)
  (define-key dots-map "" 'dots-move)
  (define-key dots-map "b" 'dots-move)
  (define-key dots-map "" 'dots-move)
  (define-key dots-map "p" 'dots-move)
  (define-key dots-map "" 'dots-move)
  (define-key dots-map "n" 'dots-move)
  (define-key dots-map " " 'dots-place-piece)
  (define-key dots-map "q" 'dots-quit)
)

(defvar dots-buffer-name "DOTS!"
  "Buffer name format string for dots!")

(defun dots ()
  "Mode for playing a game an awful lot like connect 4 with 4
directional gravity.  dots is played by placing pieces on the board
to get 4 in a row.  Legal moves are any that can attach via a straight
line to any border."

  (interactive)
  (switch-to-buffer (get-buffer-create dots-buffer-name))
  (setq mode-name "DoTs")
  (setq major-mode 'dots)
  (delete-region (point-min)(point-max))
  (insert "+-------------------------------------------------------------------+
| +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   + |
|                                                                   |
| +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   + |
|                                                                   |
| +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   + |
|                                                                   |
| +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   + |
|                                                                   |
| +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   + |
|                                                                   |
| +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   + |
|                                                                   |
| +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   + |
|                                                                   |
| +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   + |
|                                                                   |
| +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   + |
|                                                                   |
| +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   +   + |
+-------------------------------------------------------------------+")
  (delete-other-windows (selected-window))
  (use-local-map dots-map)
  (make-local-variable 'tyrant-turn)
  (setq tyrant-turn 1)
  (make-local-variable 'Dx)
  (setq Dx 0)
  (make-local-variable 'Dy)
  (setq Dy 0)
  (make-local-variable 'p1-mark)
  (setq p1-mark " 1 ")
  (make-local-variable 'p2-mark)
  (setq p2-mark " 2 ")
  (make-local-variable 'p1-name)
  (setq p1-name "Player1")
  (make-local-variable 'p2-name)
  (setq p2-name "Player2")
  (make-local-variable 'moves-left)
  (setq moves-left 313)
  (make-local-variable 'dots-score-1)
  (setq dots-score-1 0)
  (make-local-variable 'dots-score-2)
  (setq dots-score-2 0)
  (setq mode-line-buffer-identification 
	(list "Emacs" ": %5b"
	      (format " %s [%2d] %s [%2d]" 
		      p1-name dots-score-1
		      p2-name dots-score-2)))
  (set-buffer-modified-p (buffer-modified-p))
  
  ;; tyrant stuff here!
  (make-local-variable 'talk-tyrant-brief-help)
  (setq talk-tyrant-brief-help "DOTS: C-[fbnp] movement [SPC] go")
  (make-local-variable 'talk-tyrant-quit-string)
  (fset talk-tyrant-quit-string 'dots-win-string)

  (setq tyrant-player1-hook 
	'(lambda ()
	   (dots-tyrant-onechar)
	   (message "You are player 1")))
  (setq tyrant-player2-hook
	'(lambda ()
	   (setq talk-tyrant-enabled-console nil)
	   (dots-tyrant-onechar)
	   (message "Your are player 2"))) ;player2 goes 2nd
  )

(defun dots-tyrant-onechar ()
  "Look at the first char of the names in tyrant mode and select them.
If both people have same char 1, then use numbers."
  (let ((c1 (tyrant-format "%3u"))
	(c2 (tyrant-format "%3U")))
    (if (not (string= c1 c2))
	(progn
	  (setq p1-mark c1)
	  (setq p2-mark c2)
	  (setq p1-name (tyrant-format "%u"))
	  (setq p2-name (tyrant-format "%U"))))
    (setq mode-line-buffer-identification 
	  (list "Emacs" ": %5b"
		(format " %s [%2d] %s [%2d]" 
			p1-name dots-score-1
			p2-name dots-score-2)))))
	
(defun dots-place-piece ()
  "Put a piece onto the board."

  (interactive)
  (if (dots-owned Dx Dy)
      (error "Piece already there!" (dots-owned Dx Dy))
    (dots-put Dx Dy)
    (setq moves-left (- moves-left 1))
    (if (dots-gain-box)
	(progn
	  (if (and (boundp 'talk-tyrannical-mode) talk-tyrannical-mode)
	      (message (tyrant-format "It's still %p's turn."))
	    (message "It is STILL player %d's turn" tyrant-turn))
	  (if (equal moves-left 0)
	      (if (and (boundp 'talk-tyrannical-mode) talk-tyrannical-mode)  
		  (talk-usurp-tyrant (dots-win-string))
		(message (dots-win-string)))))
      (dots-swap-turns)))

  (setq mode-line-buffer-identification 
	(list "Emacs" ": %5b"
	      (format " %s [%2d] %s [%2d]" 
		      p1-name dots-score-1
		      p2-name dots-score-2)))
  (set-buffer-modified-p (buffer-modified-p)))

(defun dots-win-string ()
  "create a string declairing who wins based onthe score."
  
  (if (and (boundp 'talk-tyrannical-mode) talk-tyrannical-mode)  
      (tyrant-format (if (= dots-score-1 dots-score-2)
			 "Nobody wins"
		       (if (< dots-score-1 dots-score-2)
			   "%U wins!!!!!!!!!"
			 "%u wins!!!!!!")))
    (format (if (= dots-score-1 dots-score-2)
		"Nobody wins"
	      (if (< dots-score-1 dots-score-2)
		  "Player 1 wins!!!!!!!!!"
		"Player 2 wins!!!!!!")))))

(defun dots-turn-char ()
  "return character to place based on turn"
  (if (= tyrant-turn 1)
      p1-mark p2-mark))

(defun dots-gain-box ()
  "Check the move to x y to see if you win a box or not."
   (let ((flag 0))
      (save-excursion
      (if (equal (% Dy 2) 0)
	  (progn
	    (if (and (dots-owned Dx (- Dy 1))
		     (dots-owned Dx (- Dy 2))
		     (dots-owned (+ Dx 1) (- Dy 1)))
		(progn
		  (setq flag (+ flag 1))
		  (goto-char (dots-xy2index Dx (- Dy 1)))
		  (forward-char 2)
		  (delete-char 3)
		  (insert (dots-turn-char))))
	    (if (and (dots-owned Dx (+ Dy 1))
		     (dots-owned Dx (+ Dy 2))
		     (dots-owned (+ Dx 1) (+ Dy 1)))
		(progn
		  (setq flag (+ flag 1))
		  (goto-char (dots-xy2index Dx (+ Dy 1)))
		  (forward-char 2)
		  (delete-char 3)
		  (insert (dots-turn-char)))))
	(if (and (dots-owned (- Dx 1) (- Dy 1))
		 (dots-owned (- Dx 1) (+ Dy 1))
		 (dots-owned (- Dx 1) Dy))
	    (progn
	      (setq flag (+ flag 1))
	      (goto-char (dots-xy2index Dx Dy))
	      (forward-char -2)
	      (delete-char 3)
	      (insert (dots-turn-char))))
	(if (and (dots-owned Dx (+ Dy 1))
		 (dots-owned Dx (- Dy 1))
		 (dots-owned (+ Dx 1) Dy))
	    (progn
	      (setq flag (+ flag 1))
	      (goto-char (dots-xy2index Dx Dy))
	      (forward-char 2)
	      (delete-char 3)
	      (insert (dots-turn-char)))))
      (if (equal tyrant-turn 1)
	  (setq dots-score-1 (+ dots-score-1 flag))
	(setq dots-score-2 (+ dots-score-2 flag)))
      (if (equal flag 0)
	  nil
	flag))))

(defun dots-owned (x y)
  "returns nil if empty, 1 if player 1 peice, and 2 if player 2 peice"
  (if (or (< x 0) (or (and (equal (% y 2) 0) (> x 15))
		      (> x 16))
	  (< y 0) (> y 18))
      nil
    (save-excursion
      (goto-char (+ (dots-xy2index x y) 1))
      (cond
       ((= (following-char) ?\ )
	nil)
       (t t)))))

(defun dots-move ()
  "Move the cursor to a new position"
  (interactive)
  (cond
   ((or (= last-input-char ?\C-f)
	(= last-input-char ?f))
    (if (< Dx 15)
	(setq Dx (+ Dx 1))
      (if (equal (% Dy 2) 1)
	  (if (< Dx 16)
	      (setq Dx (+ Dx 1))
	    (error "Can't go farther right!"))
	(error "Can't go farther right!"))))
   ((or (= last-input-char ?\C-b)
	(= last-input-char ?b))
    (if (> Dx 0)
	  (setq Dx (- Dx 1))
      (error "Can't go farther left!")))
   ((or (= last-input-char ?\C-p)
	(= last-input-char ?p))
    (if (> Dy 0)
	(progn
	  (setq Dy (- Dy 1))
	  (if (equal Dx 16) (setq Dx 15)))
      (error "Can't go farther up!")))
   ((or (= last-input-char ?\C-n)
	(= last-input-char ?n))
    (if (< Dy 18)
	(progn
	  (setq Dy (+ Dy 1))
	  (if (equal Dx 16) (setq Dx 15)))
      (error "Can't go farther down!"))))
  (dots-place-cursor))

(defun dots-place-cursor ()
  "Place the cursor on the correct spot on the board..."

  (goto-char (+ 1 (dots-xy2index Dx Dy))))

(defun dots-put (x y &optional off)
  "Based on variable \"tyrant-turn\" place peice there."

  (save-excursion
    (goto-char (+ 1 (dots-xy2index x y)))
    (if (equal (% Dy 2) 0)
	(progn
	  (forward-char -1)
	  (delete-char 3)
	  (insert "---"))
      (delete-char 1)
      (insert "|"))))
  
(defun dots-xy2index (x y)
  "change x y position to absolute buffer address"

  (+ 70 (* 70 y) (* x 4) (if (equal (% y 2) 0) 4 2)))

(defun dots-swap-turns ()
  "Swap turns in connect 4"

  (cond
   ((= tyrant-turn 1) (setq tyrant-turn 2))
   ((= tyrant-turn 2) (setq tyrant-turn 1)))
  ;; tyrant mode support:
  ;; flip turns accross net each time.
  (if (boundp 'talk-tyrant-enabled-console)
      (progn
	(message (tyrant-format "It is now %p's turn"))
	(setq talk-tyrant-enabled-console (not talk-tyrant-enabled-console)))
    (message "Player %d's move." tyrant-turn)))
