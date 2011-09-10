;;; -*- Mode: Emacs-Lisp -*-

;;; File:		sokoban.el
;;; Description:	sokoban game: Push packets into goal positions
;;;				      throughout 50 game screens.
;;; Author:		Boaz Ben-Zvi <boaz@lcs.mit.edu>
;;; Idea taken from:    X11 version by Kevin Solie <kevins@ms.uky.edu>
;;; Last Modified:	Jan. 9, 1992
;;; Version:		1.3

;; Copyright (C) 1992 Boaz Ben-Zvi

;;   This program is free software; you can redistribute it and/or modify
;;   it under the terms of the GNU General Public License as published by
;;   the Free Software Foundation; either version 1, or (at your option)
;;   any later version.

;;   This program is distributed in the hope that it will be useful,
;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;   GNU General Public License for more details.

;;   You should have received a copy of the GNU General Public License
;;   along with this program; if not, write to the Free Software
;;   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;----------------------------------------------------------------------

;; HOW TO PLAY:
;;
;;    Start the game and move the player [] using control-N,P,B,F keys or
;; (arrow keys or) h,j,k,l (like vi). Use the player to push packets  ><
;; into goal positions  ..  .  For example, starting from the left:
;;
;;  ###############   ###############   ###############   ###############
;;  ##[]   ##  ..##   ##     ##  ..##   ##     ##  ..##   ##     ##  )(##
;;  ##     ##    ##   ##     ##    ##   ##     ##    ##   ##     ##  []##
;;  ##  ><       ##   ##[]><       ##   ##       []><##   ##           ##
;;  ##     ##    ##   ##     ##    ##   ##     ##    ##   ##     ##    ##
;;  ###############   ###############   ###############   ###############

;; HOW TO INSTALL:
;;
;;    The game comes with 50 screen files (all named "screen.<number>").
;; You need to put them all in some directory and set the value of
;; sokoban-screen-file-dir  to the correct directory.
;;;----------------------------------------------------------------------
;;;
;;;  History:
;;;    Version 1.0 -- Dec. 30, 1991
;;;    Version 1.1 -- Jan. 6, 1992 : Variable size squares, restart game,
;;;				     report moves/pushes, bugs fixed.
;;;    Version 1.2 -- Jan. 7, 1992 : mode hook added, bugs fixed.
;;;    Version 1.3 -- Jan. 9, 1992 : style improved.

;;;
;;;  USER CONFIGURABLE VARIABLES
;;;
(defvar sokoban-screen-file-dir "/u/boaz/sok/screens"
    "*Directory where the screen files reside.")
(defvar sokoban-save-file "~/.sokoban" "*File to save current game")
(defvar sokoban-score-file "~/.sokoban.score" "*File to save current score")

;; Shape of game squares. 
;;   You can change them to have a different number of characters per square
;; (e.g., 1 character), only make sure that they all are of the same size !
;;   Be careful if changing to other characters: the game file uses different
;; char-codes which are replaced at game load time (with replace-string).
(defvar sokoban-wall-square "##" "*Pattern for wall square")
(defvar sokoban-gold-square "><" "*Pattern for gold packet square")
(defvar sokoban-goal-square ".." "*Pattern for goal square")
(defvar sokoban-player-square "[]" "*Pattern for player square")
(defvar sokoban-gold-on-goal-square ")(" "*Pattern for gold on goal square")
(defvar sokoban-player-on-goal-square "{}" "*Pattern: Player on goal square")
(defvar sokoban-empty-square "  " "*Pattern for an empty square")

(defvar sokoban-size (length sokoban-empty-square)
    "Size of each square (in characters)")

(defvar sokoban-beep-error t "*Iff non-nil, sound a beep on a move error")

;;;
;;;  INTERNAL VARIABLES
;;;
(defvar sokoban-mode-hook nil "Called after sokoban-mode is set")

(defvar sokoban-level 1 "Current level (in range 1..sokoban-max-level)")
(defvar sokoban-initial-level 0 "Initial level")
(defvar sokoban-gold-num 0 "Total number of gold packets in current game")
(defvar sokoban-goals 0 "Number of golds currently on goal squares. \
A game is finished when this equals sokoban-gold-num")

(defvar sokoban-buffer "*sokoban*")
(defvar sokoban-help-buffer "*sokoban-help*")
(defvar sokoban-undo-stack nil "Stack of previous moves to facilitate UNDO")
(defvar sokoban-moves 0 "Number of moves in current game")
(defvar sokoban-pushes 0 "Number of pushes in current game")

(defconst sokoban-file-header-regexp 
    "Level: [0-9]+[ \t]+Packets: [0-9]+[ \t]+Goals: [0-9]+[ \t]*$"
    "Regexp describing header (first line) of game file")
(defconst sokoban-score-file-regexp "Level: [0-9]+[ \t]*"
    "Regexp describing format of score file")

(defconst sokoban-mode-map nil)
(defconst sokoban-max-level 50 "Maximal screen number available")
(defvar sokoban-player-pos nil "Position of player (should be == (point))")

;;;
;;;  SOKOBAN FUNCTIONS
;;;

(defun sokoban (level)
    "Play sokoban. Type ? for help. Optional argument specifies level
between 1 and sokoban-max-level, if zero then start from a saved game."
    (interactive "P")
    (cond ((null level)
	   (sokoban-load-game (sokoban-screen-file (sokoban-get-score))))
	  ((eq level 0) (sokoban-load-game sokoban-save-file))
	  ((numberp level)
	   (sokoban-load-game (sokoban-screen-file level)))
	  (t (error "Bad argument given: %s" level))))

(defun sokoban-screen-file (num)
    "Return name of screen file for level NUM."
    (if (or (> num sokoban-max-level) (< num 1))
	    (error "Level %d does not exist!  Pick one between 1-%d." 
		   num sokoban-max-level))
    (concat sokoban-screen-file-dir "/screen." (int-to-string num)))

(defun sokoban-init-map ()
    "Initialize sokoban-mode-map."
    (setq sokoban-mode-map (make-keymap))
    (suppress-keymap sokoban-mode-map 'no-digits)
    (aset sokoban-mode-map   2 'sokoban-move-left) ;; 
    (aset sokoban-mode-map   6 'sokoban-move-right) ;; 
    (aset sokoban-mode-map  16 'sokoban-move-up) ;; 
    (aset sokoban-mode-map  14 'sokoban-move-down) ;; 
    (aset sokoban-mode-map 104 'sokoban-move-left) ;; h
    (aset sokoban-mode-map 108 'sokoban-move-right) ;; l
    (aset sokoban-mode-map 107 'sokoban-move-up) ;; k
    (aset sokoban-mode-map 106 'sokoban-move-down) ;; j
    (aset sokoban-mode-map 101 'sokoban-exit) ;; e
    (aset sokoban-mode-map  63 'sokoban-help) ;; ?
    (aset sokoban-mode-map 109 'sokoban-report-moves) ;; m
    (aset sokoban-mode-map 113 'sokoban-quit) ;; q
    (aset sokoban-mode-map 114 'sokoban-restart) ;; r
    (aset sokoban-mode-map 115 'sokoban-save-game) ;; s
    (aset sokoban-mode-map 117 'sokoban-undo-move) ;; u
    )

(defun sokoban-mode ()
    "Mode for playing SOKOBAN. Press ? for help."
    (kill-all-local-variables)
    (if (null sokoban-mode-map) (sokoban-init-map))
    (use-local-map sokoban-mode-map)
    (setq mode-name "sokoban")
    (setq major-mode 'sokoban-mode)
    (setq buffer-read-only t)
    (sokoban-set-mode-line)
    (run-hooks 'sokoban-mode-hook)
    )

(defun sokoban-set-mode-line()
    "Update and redisplay the sokoban mode line."
    (setq mode-line-format 
	  (concat " * S O K O B A N *   " "Level: " 
		  (int-to-string sokoban-level)  "  Packets: " 
		  (int-to-string sokoban-gold-num) "  Goals: "
		  (int-to-string sokoban-goals) "   (type ? for help)"))
    ;;; force redisplay of mode-line
    (save-excursion (set-buffer (other-buffer)))
    (set-buffer-modified-p (buffer-modified-p))
    (sit-for 0))

;;;
;;; Load game file and initiate board
;;;

(defun sokoban-replace-string (from to)
    "Replace string FROM with TO in rest of current buffer."
    (save-excursion
	(while (search-forward from nil t)
	    (replace-match to))))

(defun sokoban-load-game (filename)
    "Loads a game saved in FILENAME. Sets parameters from the header."
    (let ((buffer-read-only nil))  ; allow updates to buffer
	(if (file-readable-p (expand-file-name filename)) nil
	    (error "Can not read file %s" filename))
	(switch-to-buffer (get-buffer-create sokoban-buffer))
	(erase-buffer)
	(insert-file-contents (expand-file-name filename))
	;; ----- READ HEADER
	(goto-char 1)
	(cond ((not (looking-at sokoban-file-header-regexp))
	       (kill-buffer (current-buffer))
	       (error "File %s: Bad format!" filename)))
	(re-search-forward "[0-9]+")
	(setq sokoban-level 
	      (string-to-int (buffer-substring (match-beginning 0) (point))))
	(if (= sokoban-initial-level 0) 
		(setq sokoban-initial-level sokoban-level)) ;; first time only
	(re-search-forward "[0-9]+")
	(setq sokoban-gold-num 
	      (string-to-int (buffer-substring (match-beginning 0) (point))))
	(re-search-forward "[0-9]+")
	(setq sokoban-goals
	      (string-to-int (buffer-substring (match-beginning 0) (point))))
	(re-search-forward "\n")
	(delete-region 1 (point))
	;; ----  CONVERT FILE FORMAT TO GAME FORMAT ( must be (= 1 (point)) ! )
	(sokoban-replace-string "#" sokoban-wall-square)
	(sokoban-replace-string " " sokoban-empty-square)
	(sokoban-replace-string "$" sokoban-gold-square)
	(sokoban-replace-string "@" sokoban-player-square)
	(sokoban-replace-string "*" sokoban-gold-on-goal-square)
	(sokoban-replace-string "+" sokoban-player-on-goal-square)
	(sokoban-replace-string "." sokoban-goal-square)
	;;  ( must be here:  (= 1 (point)) ! )
	(cond ((re-search-forward (regexp-quote sokoban-player-square) nil t)
	       (setq sokoban-player-pos (1- (point))))
	      ((re-search-forward (regexp-quote sokoban-player-on-goal-square)
				  nil t)
	       (setq sokoban-player-pos (1- (point))))
	      (t
	       (kill-buffer (current-buffer))
	       (error "Did not find player in game in file %s" filename)))
	(goto-char sokoban-player-pos)
	(sokoban-mode)
	(setq sokoban-undo-stack '())  ;; reset undo stack
	(setq sokoban-moves 0)
	(setq sokoban-pushes 0)
	(message "Sokoban: Loaded game screen number %d" sokoban-level)
	))

;;;
;;; All the "move" commands bellow assume that the game board is correct !!!
;;; (i.e. they do not check for possible errors)
;;;

(defun sokoban-move-left()
    "Move sokoban player one square to the left (if possible)."
    (interactive)
    (goto-char sokoban-player-pos) ;; in case cursor not on player
    (let ((next-point (- sokoban-player-pos sokoban-size))
	  (push-to-point (- sokoban-player-pos (* 2 sokoban-size))))
	(if (> 1 push-to-point) (error "") ;; only on badly formatted boards
	    (sokoban-move next-point push-to-point))))

(defun sokoban-move-right()
    "Move sokoban player one square to the right (if possible)."
    (interactive)
    (goto-char sokoban-player-pos) ;; in case cursor not on player
    (let ((next-point (+ sokoban-player-pos sokoban-size))
	  (push-to-point (+ sokoban-player-pos (* 2 sokoban-size))))
	(if (> push-to-point (point-max)) (error "") ; on badly formatted board
	    (sokoban-move next-point push-to-point))))

(defun sokoban-move-up()
    "Move sokoban player one square up (if possible)."
    (interactive)
    (goto-char sokoban-player-pos) ;; in case cursor not on player
    (sokoban-move-vertical -1))

(defun sokoban-move-down()
    "Move sokoban player one square down (if possible)."
    (interactive)
    (goto-char sokoban-player-pos) ;; in case cursor not on player
    (sokoban-move-vertical 1))

(defun sokoban-move-vertical (direction)
    "Do move-up for DIRECTION value of -1, down for 1."
    (let ((temporary-goal-column (current-column))
	  next-point push-to-point)
	(save-excursion
	    (forward-line direction)
	    (move-to-column temporary-goal-column)
	    ;; bellow -- error on badly formatted boards
	    (if (< (current-column) temporary-goal-column) (error ""))
	    (setq next-point (point))
	    (forward-line direction)
	    (move-to-column temporary-goal-column)
	    ;; bellow -- error when push-to-line is too short
	    (setq push-to-point ;; 
		  (if (= (current-column) temporary-goal-column) (point) 
		      next-point)))  ;; cause an error !!
	(if (= next-point push-to-point)  ;; last/first line
		(if sokoban-beep-error (error "Can not move through walls!")
		    (message "Can not move through walls!")))
	(sokoban-move next-point push-to-point)
	))

(defun sokoban-set-square (square what goal)
    "Set SQUARE to WHAT, check for the case when SQUARE is GOAL."
    (let ((buffer-read-only nil))
	(save-excursion
	    (goto-char (- square (1- sokoban-size)))
	    (delete-char sokoban-size)
	    (cond ((eq what 'player)
		   (if goal (insert sokoban-player-on-goal-square)
		       (insert sokoban-player-square)))
		  ((eq what 'gold)
		   (if goal (insert sokoban-gold-on-goal-square)
		       (insert sokoban-gold-square)))
		  ((eq what 'empty)
		   (if goal (insert sokoban-goal-square)
		       (insert sokoban-empty-square)))
		  (t (error "sokoban-set-square received unknown symbol")))
	    )))

(defun sokoban-get-square (point)
    "Returns the string in the square pointed to by POINT."
    (buffer-substring (- point (1- sokoban-size)) (1+ point)))

(defun sokoban-move (next-point push-to-point)
    "Perform a sokoban move.
Use three points: sokoban-player-pos, the neighboring NEXT-POINT and
its following neighbor PUSH-TO-POINT to perform a sokoban move, regardless
of the move direction."
    (let ((player (sokoban-get-square sokoban-player-pos))
	  (next (sokoban-get-square next-point))
	  (push-to (sokoban-get-square push-to-point))
	  player-goal next-goal push-to-goal ;; are these GOAL squares ??
	  (pushed nil) (can-move nil) (err ""))
	(setq player-goal (string-equal sokoban-player-on-goal-square player))
	(setq next-goal (or (string-equal sokoban-goal-square next)
			    (string-equal sokoban-gold-on-goal-square next)))
	(setq push-to-goal (string-equal sokoban-goal-square push-to))
	(cond ((or (string-equal next sokoban-empty-square)  ; no need to push
		   (string-equal next sokoban-goal-square))
	       (setq can-move t))
	      ((and
		(or (string-equal next sokoban-gold-square) ;; gold to push
		    (string-equal next sokoban-gold-on-goal-square))
		(or (string-equal push-to sokoban-empty-square) ;; is clear?
		    (string-equal push-to sokoban-goal-square)))
	       (setq can-move t pushed t))
	      (t ))
	(cond (can-move
	       (sokoban-set-square sokoban-player-pos 'empty player-goal)
	       (sokoban-set-square next-point 'player next-goal)
	       (cond (pushed 
		      (sokoban-set-square push-to-point 'gold push-to-goal)
		       ;;; --- UPDATE GOALS COUNT (if needed)
		      (if next-goal (setq sokoban-goals (1- sokoban-goals)))
		      (if push-to-goal (setq sokoban-goals 
					     (1+ sokoban-goals)))
		      (sokoban-set-mode-line))) ;;; update mode line
		;;; ----  LOAD NEXT SCREEN (if it is time)
	       (if (= sokoban-goals sokoban-gold-num)
		       (if (not (= sokoban-level sokoban-max-level))
			       (sokoban-load-game (sokoban-screen-file 
						   (1+ sokoban-level)))
			   (kill-buffer (current-buffer))
			   (error "Sokoban game is over."))
		    ;;; else ----  PUSH INTO UNDO STACK
		   (setq sokoban-undo-stack
			 (cons (list (cons sokoban-player-pos player-goal)
				     (cons next-point next-goal)
				     (if (not pushed) nil 
					 (cons push-to-point push-to-goal)))
			       sokoban-undo-stack))
		   (setq sokoban-moves (1+ sokoban-moves))
		   (if pushed (setq sokoban-pushes (1+ sokoban-pushes)))
		   (goto-char next-point)
		   (setq sokoban-player-pos next-point)))
	      (t (if (string-equal next sokoban-wall-square) 
			 (setq err "Can not move through walls!")
		     (if (string-equal push-to sokoban-wall-square)
			     (setq err "Can not push through walls!")
			 (setq err "Can not push more than one packet!")))
		 (if sokoban-beep-error (error err) (message err))))
	))

(defun sokoban-undo-move (num)
    "Undo last move. With positive NUM undo last NUM moves."
    (interactive "p")
    (while (< 0 num)
	(if (null sokoban-undo-stack) (error "Undo stack is empty !"))
	(let* ((last-move (car sokoban-undo-stack))
	       ;; Each of the following three is a pair: < square (= point)
	       ;; . goal (= t iff goal square) >
	       (player (car last-move))
	       (next (car (cdr last-move)))
	       (pushed (car (cdr (cdr last-move)))))
	    (sokoban-set-square (car player) 'player (cdr player))
	    (if (not pushed) 
		    (sokoban-set-square (car next) 'empty (cdr next))
		(sokoban-set-square (car next) 'gold (cdr next))
		(sokoban-set-square (car pushed) 'empty (cdr pushed))
		(cond ((and (cdr pushed) (not (cdr next)))
		       (setq sokoban-goals (1- sokoban-goals))
		       (sokoban-set-mode-line))
		      ((and (not (cdr pushed)) (cdr next))
		       (setq sokoban-goals (1+ sokoban-goals))
		       (sokoban-set-mode-line))
		      ))
	    (goto-char (car player))
	    (setq sokoban-player-pos (car player))
	    (setq sokoban-undo-stack (cdr sokoban-undo-stack))
	    (setq sokoban-moves (1- sokoban-moves))
	    (if pushed (setq sokoban-pushes (1- sokoban-pushes))))
	(setq num (1- num))))

(defun sokoban-help ()
    "Describe the SOKOBAN game."
    (interactive)
    (switch-to-buffer (get-buffer-create sokoban-help-buffer))
    (with-output-to-temp-buffer sokoban-help-buffer
	(insert "SOKOBAN:\n"
		"  The problem in this game is to push packets (each looks "
		"like  " sokoban-gold-square " ) into\n"
		"the goal positions (each looks like  "
		sokoban-goal-square " ) using the player "
		"( " sokoban-player-square " ).\n"
		"  A player can only push one packet at a time. "
		"Neither player nor packet can \ngo through walls ( "
		sokoban-wall-square " ), and both change form "
		"a little when positioned on a\ngoal position (to  "
		sokoban-player-on-goal-square "  and  "
		sokoban-gold-on-goal-square " ).\n"
		"  To move the player use the Control-B,F,N,P (or arrow) keys "
		"or  h,j,k,l \n( vi style ). Other useful keys:\n\n"
		"\tq: quit (current level is kept in sokoban-score-file)\n"
		"\ts: save current game (in sokoban-save-file)\n"
		"\te: exit game \n"
		"\tu: undo last move (with argument: undo several moves)\n"
		"\tr: restart current level\n"
		"\tm: report number of moves and pushes so far\n"
		"\nSTARTING THE GAME: \n"
		"  Calling \"M-x sokoban\" without an argument starts "
		"the game at your current \n"
		"level (taken from the score file). With a numerical "
		"argument: start at that\n"
		"argument level (between 1 and "
		(int-to-string sokoban-max-level) "). With 0 (zero): "
		"start from the save file. \n")
	)
    
    )

(defun sokoban-get-score ()
    "Gets level from local score file."
    (let ((full-name (expand-file-name sokoban-score-file))
	  (temp-buff (make-temp-name "sokoban"))
	  temp)
	(if (not (file-exists-p full-name)) 1 ; start at level 1
	    (if (not (file-readable-p full-name)) 
		    (error "Can not read score file: %s" sokoban-score-file))
	    (set-buffer (get-buffer-create temp-buff))
	    (insert-file-contents full-name)
	    (goto-char 1)
	    (if (not (looking-at sokoban-score-file-regexp))
		    (error "Score file %s: Bad format!" sokoban-score-file))
	    (re-search-forward "[0-9]+")
	    (setq temp (string-to-int (buffer-substring 
				       (match-beginning 0) (match-end 0))))
	    (kill-buffer temp-buff)
	    temp)
	))

(defun sokoban-exit ()
    "Exit game, save nothing."
    (interactive)
    (if (y-or-n-p "Really exit? ")
	    (kill-buffer (current-buffer)))
    (message ""))

(defun sokoban-quit ()
    "Quit game, keeping current level in score file."
    (interactive)
    (let ((full-name (expand-file-name sokoban-score-file))
	  (continue t)
	  (question "Want to quit? "))
	(if (= sokoban-initial-level sokoban-level) ;; dont save
		(setq continue nil)
	    (if (file-writable-p full-name) nil  ;; trouble writing ?
		(setq continue nil)
		(setq question 
		      (concat (format "Can not write score file: %s . So: "
				      sokoban-score-file) question))))
	(cond ((y-or-n-p question)
	       (message "") ;; erase question
	       (cond (continue
		      (find-file full-name)
		      (erase-buffer)
		      (insert "Level: " (int-to-string sokoban-level) " \n")
		      (save-buffer 0)
		      (kill-buffer (current-buffer))
		      (set-buffer sokoban-buffer)))
	       (kill-buffer (current-buffer)))
	      (t (message "Quiting aborted.")))))

(defun sokoban-save-game ()
    "Save current game as is (without undo information)."
    (interactive)
    (let ((full-name (expand-file-name sokoban-save-file))
	  start)
	(if (file-writable-p full-name) nil  ;; trouble writing ?
	    (error "Save file %s: can not write!" sokoban-save-file))
	(find-file full-name)
	(delete-region (point-min) (point-max))
	(insert-buffer sokoban-buffer)
	(goto-char 1)
	(sokoban-replace-string sokoban-wall-square "#")
	(sokoban-replace-string sokoban-empty-square " ")
	(sokoban-replace-string sokoban-gold-square "$")
	(sokoban-replace-string sokoban-player-square "@")
	(sokoban-replace-string sokoban-gold-on-goal-square "*")
	(sokoban-replace-string sokoban-player-on-goal-square "+")
	(sokoban-replace-string sokoban-goal-square ".")
	(insert "Level: " (int-to-string sokoban-level)
		" Packets: " (int-to-string sokoban-gold-num)
		" Goals: " (int-to-string sokoban-goals) " \n")
	(save-buffer 0)
	(kill-buffer (current-buffer))
	(set-buffer sokoban-buffer)
	(message "Current game saved.")))

(defun sokoban-report-moves ()
    "Report number of moves  and pushes so far in this game."
    (interactive)
    (message "So far:  %d moves and  %d pushes" sokoban-moves sokoban-pushes))

(defun sokoban-restart ()
    "Restart game at current level."
    (interactive)
    (if (y-or-n-p "Restart this game? ")
	    (sokoban-load-game (sokoban-screen-file sokoban-level))
	(message "")))

