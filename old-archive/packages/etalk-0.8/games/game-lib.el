;;; Short library of game support for tyrant mode, X displays
;;; and other common code
;;;
;;; Author: Eric M. Ludlam <zappo@gnu.ai.mit.edu>
;;; Version: 0.1
;;; Keywords: games, extensions
;;;
;;; Copyright (C) 1994 Free Software Foundation
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

;;; Comentary:
;;;   This source was written to supply a cleaner and more functional
;;;   approach to writing games for emacs talk.  These routines supply
;;;   a single spot for providing user-supplied parameters that can
;;;   effect all games, specifically, colors.


;;;   This package doesn't require tyrant-mode because all calls are
;;;   checked for existence before running.

;; this variable is duplicated in etalk-tyrn.  this doesn't matter,
;; but provides better compiler messages
(defvar tyrant-turn nil
  "Variable which will always be used in each game as the turn variable.")

(defvar game-lib-replace t
  "In most cases, we wish to replace characters for games, rarely,
we need a little hook in so we may insert (end of lines, etc)")

(defvar game-lib-use-colors (and  (= (string-to-int emacs-version) 19)
				  window-system)
  "Set this to nil if you never wish to use colors, otherwise, always
use colors when you have windows")

(defvar game-lib-player1-face-color "red"
  "Color of player 2s face in window environ.")

(defvar game-lib-player2-face-color "yellow"
  "Color of player 2s face in window environ.")

(defvar game-lib-invert-foreground-color "black")

;;; If we have a window system, load in cool colors to use on the game board
(if (and game-lib-use-colors  (= (string-to-int emacs-version) 19) 
	 window-system)
    (progn
      (copy-face 'default 'game-lib-player1-face)
      (set-face-foreground 'game-lib-player1-face game-lib-player1-face-color)
      
      (copy-face 'default 'game-lib-player2-face)
      (set-face-foreground 'game-lib-player2-face game-lib-player2-face-color)

      (copy-face 'game-lib-player1-face 'game-lib-player1-face-R)
      (invert-face 'game-lib-player1-face-R)
      (set-face-foreground 'game-lib-player1-face-R 
			   game-lib-invert-foreground-color)

      (copy-face 'game-lib-player2-face 'game-lib-player2-face-R)
      (invert-face 'game-lib-player2-face-R)
      (set-face-foreground 'game-lib-player2-face-R 
			   game-lib-invert-foreground-color)
      ))  

(defun game-lib-add-mouse-support (keymap) 
  "Adds mouse support to a game.  It puts a call to
game-lib-handle-mouse into KEYMAP entries for FUNCTION which will be
used to handle the mouse events.  This must be done this way so
mouse-related actions can be sent over the network.  The event [mouse]
can't be transferred as a character.

  FUNCTION should receive the list (p e m) where P is the
position in the buffer, and e is the event type (mouse-1, mouse-2,
mouse-movement) and m is a list of modifiers (down, click, drag, etc)
Your average game will probably skip many of these types.
"
  (if (= (string-to-int emacs-version) 19)
      (progn
	(define-key keymap [down-mouse-1] 'game-lib-handle-mouse)
	(define-key keymap [down-mouse-2] 'game-lib-handle-mouse)
	(define-key keymap [down-mouse-3] 'game-lib-handle-mouse)
	(define-key keymap [mouse-1] 'game-lib-handle-mouse)
	(define-key keymap [mouse-2] 'game-lib-handle-mouse)
	(define-key keymap [mouse-3] 'game-lib-handle-mouse)
	(define-key keymap [drag-mouse-1] 'game-lib-handle-mouse)
	(define-key keymap [drag-mouse-2] 'game-lib-handle-mouse)
	(define-key keymap [drag-mouse-3] 'game-lib-handle-mouse)
	(define-key keymap [double-down-mouse-1] 'game-lib-handle-mouse)
	(define-key keymap [double-down-mouse-2] 'game-lib-handle-mouse)
	(define-key keymap [double-down-mouse-3] 'game-lib-handle-mouse)
	(define-key keymap [double-mouse-1] 'game-lib-handle-mouse)
	(define-key keymap [double-mouse-2] 'game-lib-handle-mouse)
	(define-key keymap [double-mouse-3] 'game-lib-handle-mouse)))
)

(defun game-lib-handle-mouse (event)
  "Handle a mouse event by gleening the important information from the
event"
  (interactive "e")
  (game-lib-do-mouse-thing event)
  (track-mouse
    (while (progn
	     (setq event (read-event))
	     (or (mouse-movement-p event)
		 (eq (car-safe event) 'switch-frame)))
      (game-lib-do-mouse-thing event))
    )
  (game-lib-do-mouse-thing event)
  )

(defun game-lib-do-mouse-thing (event)
  "Do the mouse thing with event.  The handler handles drag type
things."
  (let ((pos (event-start event)))
    (if tyrant-mouse-function
	(eval (list tyrant-mouse-function
		    (posn-point pos)
		    '(event-basic-type event)
		    '(event-modifiers event)))
      )))

(defun game-lib-clear-buffer (&optional buffer)
  "Take the current buffer and delete everything in it.  This also
will delete any overlays existing in the buffer."
  (save-excursion
    (if (bufferp buffer) (set-buffer buffer))
    (delete-region (point-min) (point-max))
    (if (fboundp 'overlay-lists)
	(let ((ol (car (overlay-lists))))
	  ;; delete overlays sitting here already
	  (while ol
	    (delete-overlay (car ol))
	    (setq ol (cdr ol))))
      )))

(defun game-lib-insert-string (pnt string color)
  "Take STRING and place into buffer at PNT, replaceing what was
there, and making sure that face color is COLOR (being a symbol
containing a face)"

  (let ((rpnt (point))
	(ol (if (fboundp 'overlays-at)
		(overlays-at pnt)
	      nil))
	(end (+ pnt (length string)))
	(no nil))
    ;; delete overlays sitting here already
    (while ol
      (if (and (= (overlay-start (car ol)) pnt)
	       (= (overlay-end (car ol)) end)
	       (overlay-get (car ol) 'game-lib))
	  (delete-overlay (car ol)))
      (setq ol (cdr ol)))
    (goto-char pnt)
    (if game-lib-replace		;only delete if turned on
	(if (> (length string) (- (point-max) pnt))
	    (delete-region pnt (point-max))
	  (delete-char (length string))))
    (insert string)
    (if (and color (fboundp 'make-overlay) (fboundp 'overlay-put) 
	     window-system)
	(progn
	  (setq no (make-overlay pnt end))
	  (overlay-put no 'face color)
	  (overlay-put no 'game-lib t)))
    ;; when not replacing, we are expecting to be at the end of this
    ;; new string
    (if game-lib-replace
	(goto-char rpnt))))

(defun game-lib-win (who etalk-message solo-message)
  "Declare WHO winner handling tyrant problems.  If tyranted, use the
ETALK-MESSAGE, otherwise, use SOLO-MESSAGE, where the etalk message
contains tyrant-format controls, and solo-message may contain one %d
which will be filled in by WHO.  Just before printing the message,
tyrant-turn is set to WHO, thus allowing use of %P etc in tyrant
messages even when it isn't tyrant-turn's turn"

  (let ((tyrant-turn who))
    (if (and (boundp 'etalk-tyrannical-mode) etalk-tyrannical-mode)
	(etalk-usurp-tyrant (tyrant-format etalk-message))
      (message solo-message tyrant-turn))))

(defun game-lib-swap-turns (etalk-message solo-message)
  "Swap turns handling tyrant problems.  If tyranted, use the
ETALK-MESSAGE, otherwise use SOLO-MESSAGE, where the etalk messasge
contains tyrant-format controls, and solo-message may contain one %d
which will be give the current turn number."

 ;; tyrant-turn should always be defined for every game as the
  (cond
   ((= tyrant-turn 1) (setq tyrant-turn 2))
   ((= tyrant-turn 2) (setq tyrant-turn 1)))
  ;; tyrant mode support:
  ;; flip turns accross net each time.
  (if (and (boundp 'etalk-tyrant-enabled-console)
	   (boundp 'etalk-tyrannical-mode)
	   etalk-tyrannical-mode)
      (progn
	(message (tyrant-format etalk-message))
	(setq etalk-tyrant-enabled-console (not etalk-tyrant-enabled-console)))
    (message solo-message tyrant-turn)))


(defun game-lib-quit (kill)
  "Covers over icky tyrant mode stuff needed to quit a game.  If KILL,
then kill the buffer when done."

  (if (and (boundp 'etalk-tyrannical-mode) etalk-tyrannical-mode)  
      (etalk-usurp-tyrant)
    (kill-buffer (current-buffer))))

;;; end lisp

(provide 'game-lib)