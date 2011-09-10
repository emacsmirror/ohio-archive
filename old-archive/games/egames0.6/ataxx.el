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
(provide 'ataxx)

(defvar ataxx-map nil
  "Keymap used in playing 4 by 4")

(if ataxx-map
    ()
  (setq ataxx-map (make-sparse-keymap))
  (define-key ataxx-map "" 'ataxx-move)  
  (define-key ataxx-map "f" 'ataxx-move)
  (define-key ataxx-map "" 'ataxx-move)
  (define-key ataxx-map "b" 'ataxx-move)
  (define-key ataxx-map "" 'ataxx-move)
  (define-key ataxx-map "p" 'ataxx-move)
  (define-key ataxx-map "" 'ataxx-move)
  (define-key ataxx-map "n" 'ataxx-move)
  (define-key ataxx-map " " 'ataxx-place-piece)
  (define-key ataxx-map "q" 'ataxx-quit)
)

(defconst ataxx-piece1 "##"
  "String as piece 1")

(defconst ataxx-piece2 "::"
  "String as piece 2")

(defconst ataxx-stat "Ataxx statistics"
  "Buffer name of statistics window.")

(defun ataxx ()
  "Mode for playing ataxx.
Rules:  A person may either \"drool\" adjacently, and create a new peice.
          or \"jump\" up to 2 places away and move a peices.  When you move
          adjacent to an opponants peice, you take it over.  Take the most 
          peices.
Playing:
        C-fbnp keys move cursor.
        SPC    Grab a piece
          SPC  Let go, drool, or jump that piece to point."
  (interactive)
  (switch-to-buffer (get-buffer-create "Ataxx"))
  (setq mode-name "Ataxx")
  (setq major-mode 'ataxx)
  (delete-region (point-min)(point-max))
  (insert "+----+----+----+----+----+----+----+
| ## |    |    |    |    |    | :: |
+----+----+----+----+----+----+----+
|    |    |    |    |    |    |    |
+----+----+----+----+----+----+----+
|    |    |    |    |    |    |    |
+----+----+----+----+----+----+----+
|    |    |    |    |    |    |    |
+----+----+----+----+----+----+----+
|    |    |    |    |    |    |    |
+----+----+----+----+----+----+----+
|    |    |    |    |    |    |    |
+----+----+----+----+----+----+----+
| :: |    |    |    |    |    | ## |
+----+----+----+----+----+----+----+")
  (delete-other-windows (selected-window))
  (use-local-map ataxx-map)
  (make-local-variable 'tyrant-turn)
  (setq tyrant-turn 1)
  (make-local-variable '4x)
  (setq 4x 0)
  (make-local-variable '4y)
  (setq 4y 0)
  (make-local-variable 'p1-score)
  (setq p1-score 2)
  (make-local-variable 'p2-score)
  (setq p2-score 2)
  (make-local-variable 'grabbed)
  (setq grabbed nil)
  (make-local-variable 'talk-tyrant-brief-help)
  (setq talk-tyrant-brief-help
"ATAXX: [C-f] Forward [C-b] Back [C-p] Previous [C-n] Next [SPC] Grab/Drop")
  (split-window (selected-window) 50 t)
  (make-local-variable 'talk-tyrant-quit-string)
  (fset talk-tyrant-quit-string 'ataxx-win-string)
  (other-window 1)
  (switch-to-buffer (get-buffer-create ataxx-stat))
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
	     (switch-to-buffer ataxx-stat)
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
	   (message "Your are player 2") ;player2 goes 2nd
	   (save-excursion
	     (switch-to-buffer ataxx-stat)
	     (make-local-variable 'talk-tag)
	     (setq talk-tag t)
	     (goto-line 1)
	     (delete-region (point) (save-excursion (end-of-line) (point)))
	     (insert (tyrant-format "%n score:"))
	     (goto-line 4)
	     (delete-region (point) (save-excursion (end-of-line) (point)))
	     (insert (tyrant-format "%N score:")))))
  )

(defun ataxx-place-piece ()
  "Put a piece onto the board."

  (interactive)
  (if grabbed
      (let ((xd (- 4x (car grabbed)))
	    (yd (- 4y (nth 1 grabbed)))
	    (newscore 0))
	(if (and (= xd 0) (= yd 0))
	    (progn
	      (ataxx-put (nth 0 grabbed) (nth 1 grabbed) t t) ;clear
	      (ataxx-put (nth 0 grabbed) (nth 1 grabbed)) ;reset
	      (setq grabbed nil)
	      )	      
	  (if (ataxx-owned 4x 4y)
	      (error "You can't go on top of someone elses peice!")
	    (if (and (<= xd 1) (>= xd -1) (<= yd 1) (>= yd -1))
		;; the we may drool to a neighboring square
		(progn
		  (ataxx-put (nth 0 grabbed) (nth 1 grabbed) t t) ;clear
		  (ataxx-put (nth 0 grabbed) (nth 1 grabbed)) ;reset
		  (ataxx-put 4x 4y)		;new
		  (setq newscore (ataxx-snarf-pieces 4x 4y))
		  (let* ((name (if (and (boundp 'talk-tyrannical-mode)
					talk-tyrannical-mode)
				   (tyrant-format "%:1P")
				 (format "Player %d" tyrant-turn)))
			 (s1 (format " %s drools!\n  %d, %d ==> %d, %d" name
				     (nth 0 grabbed) (nth 1 grabbed) 4x 4y))
			 (s2 (format " %s gains %d stones" name (1+ newscore))))
		    (save-excursion
		      (set-buffer ataxx-stat)
		      (goto-line 8)
		      (delete-region (point)
				     (save-excursion (forward-line 1)
						     (end-of-line) (point)))
		      (insert s1)
		      (goto-line 12)
		      (delete-region (point) (save-excursion 
					       (end-of-line) (point)))
		      (insert s2)))
		  (if (= tyrant-turn 1)
		      (progn
			(setq p1-score (+ p1-score 1 newscore))
			(setq p2-score (- p2-score newscore)))
		    (setq p2-score (+ p2-score 1 newscore))
		    (setq p1-score (- p1-score newscore))))
	      (if (and (<= xd 2) (>= xd -2) (<= yd 2) (>= yd -2)) ;a jump
		  (progn
		    (ataxx-put (nth 0 grabbed) (nth 1 grabbed) t t) ;clear
		    (ataxx-put 4x 4y)		;new
		    (setq newscore (ataxx-snarf-pieces 4x 4y))
		    (let* ((name (if (and (boundp 'talk-tyrannical-mode)
					  talk-tyrannical-mode)
				     (tyrant-format "%:1P")
				   (format "Player %d" tyrant-turn)))
			   (s1 (format " %s jumps!\n  %d, %d ==> %d, %d" name
				       (nth 0 grabbed) (nth 1 grabbed) 4x 4y))
			   (s2 (format " %s gains %d stones" name newscore)))
		      (save-excursion
			(set-buffer ataxx-stat)
			(goto-line 8)
			(delete-region (point)
				       (save-excursion (forward-line 1)
						       (end-of-line) (point)))
			(insert s1)
			(goto-line 12)
			(delete-region (point) (save-excursion 
						 (end-of-line) (point)))
			(insert s2)))
		    (if (= tyrant-turn 1)
			(progn
			  (setq p1-score (+ p1-score newscore))
			  (setq p2-score (- p2-score newscore)))
		      (setq p2-score (+ p2-score newscore))
		      (setq p1-score (- p1-score newscore))))
		(error "That is neither a jump nor drool!")))
	    (setq grabbed nil)
	    (ataxx-swap-turns))))
    (if (not (eq (ataxx-owned 4x 4y) tyrant-turn))
	(error "That isn't one of your pieces silly!")
      (ataxx-put 4x 4y nil t)
      (setq grabbed (list 4x 4y))
      (let ((s1 (format "Grabbing (%d,%d)\n" 4x 4y)))
	(save-excursion
	  (set-buffer ataxx-stat)
	  (goto-line 8)
	  (delete-region (point) (save-excursion (forward-line 1)
						 (end-of-line) (point)))
	  (insert s1)
	  (goto-line 12)
	  (delete-region (point) (save-excursion (end-of-line) (point)))))))
  (if (ataxx-win)
      (if (and (boundp 'talk-tyrannical-mode) talk-tyrannical-mode)  
	  (talk-usurp-tyrant (ataxx-win-string))
	(message (ataxx-win-string)))))
(defun ataxx-snarf-pieces (x y)
  "Pretend to move to xy, then change all adjacent owned peices to yourself!"
  (let ((dx -1) (dy -1) (got 0))
    (while (< dx 2)
      (setq dy -1)
      (while (< dy 2)
	(if (and (ataxx-owned (+ dx x) (+ dy y))
		 (not (= (ataxx-owned (+ dx x) (+ dy y)) tyrant-turn)))
	    (progn
	      (setq got (1+ got))
	      (ataxx-put (+ dx x) (+ dy y))))
	(setq dy (1+ dy)))
      (setq dx (1+ dx)))
    got))
(defun ataxx-win-string ()
  "return a string declairing the winner"
  (ataxx-swap-turns)
  (if (and (boundp 'talk-tyrannical-mode) talk-tyrannical-mode)  
      (tyrant-format (if (> p1-score p2-score)
			 "%n wins!!!!"
		       "%N wins!!!"))
    (message "Player %s wins!" (if (> p1-score p2-score) 1 2))))
(defun ataxx-win ()
  "return t if that is a winning move"
  (or (= p1-score 0) (= p2-score 0) (= (+ p1-score p2-score) 49))
)
(defun ataxx-owned (x y)
  "returns nil if empty, 1 if player 1 peice, and 2 if player 2 peice"
  (save-excursion
    (goto-char (+ (ataxx-xy2index x y) 1))
    (cond
     ((= (following-char) ?\ )
      nil)
     ((= (following-char) (string-to-char ataxx-piece1))
      1)
     ((= (following-char) (string-to-char ataxx-piece2))
      2)
     (t nil))))

(defun ataxx-move ()
  "Move the cursor to a new position"
  (interactive)
  (cond
   ((or (= last-input-char ?\C-f)
	(= last-input-char ?f))
    (if (< 4x 6)
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
    (if (< 4y 6)
	(setq 4y (+ 4y 1))
      (error "Can't go farther down!"))))
  (ataxx-place-cursor))

(defun ataxx-place-cursor ()
  "Place the cursor on the correct spot on the board..."

  (goto-char (+ 1 (ataxx-xy2index 4x 4y))))

(defun ataxx-put (x y &optional off mark)
  "Based on variable \"turn\" place peice there."

  (save-excursion
    (let* ((coff (if mark 0 1))
	   (deln (if mark 4 2))
	   (str (if off "  " 
		  (if (= tyrant-turn 1) ataxx-piece1 ataxx-piece2)))
	   (pstr (if mark (if off 
			      (concat " " str " ")
			    (concat ">" str "<"))
		   str)))
      (goto-char (+ coff (ataxx-xy2index x y)))
      (delete-char deln)
      (insert pstr))))
  
(defun ataxx-xy2index (x y)
  "change x y position to absolute buffer address"

  (+ (+ (+ 1 (* x 5)) (+ (* y 74) 1)) 37))

(defun ataxx-swap-turns ()
  "Swap turns in connect 4"

  (cond
   ((= tyrant-turn 1) (setq tyrant-turn 2))
   ((= tyrant-turn 2) (setq tyrant-turn 1)))
  (save-excursion
    (let ((s1 (format "  %d blocks" p1-score))
	  (s2 (format "  %d blocks" p2-score)))
      (set-buffer ataxx-stat)
      (goto-line 2)
      (delete-region (point) (save-excursion (end-of-line) (point)))
      (insert s1)
      (goto-line 5)
      (delete-region (point) (save-excursion (end-of-line) (point)))
      (insert s2)))
  ;; tyrant mode support:
  ;; flip turns accross net each time.
  (if (boundp 'talk-tyrant-enabled-console)
      (progn
	(message (tyrant-format "It is now %P's turn."))
	(setq talk-tyrant-enabled-console (not talk-tyrant-enabled-console)))
    (message "Player %d's move." tyrant-turn)))
