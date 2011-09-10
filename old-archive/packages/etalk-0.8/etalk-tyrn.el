;;; tyrant mode support under etalk
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
;;; Purpose:
;;;   This file contains the etalk-tyrant code for initializing tyrant
;;; mode within a talk process...

(defvar etalk-tyrant-local-buildsequence nil
  "Used to build a keysequence to send to remote.")

(defvar etalk-tyrant-remote-buildsequence nil
  "Used to build a keysequence to interpret from remote.")

(defvar etalk-tyrant-enabled-console nil 
  "Variable for minor-mode alist, defining when the tyrant console is
enabled.  This provides default value.")

(defvar tyrant-player1-hook nil
  "Hook used when starting up player 1's player space.  This should
only be set by games who believe that they will be used under tyrant mode.")

(defvar tyrant-player2-hook nil
  "Hook used when starting up player 2's player space.  This should
only be set by games who believe that they will be used under tyrant mode.")

(defvar tyrant-mouse-function nil
  "Function used to evaluate mouse actions locally in a buffer.")

;; add mode etalk-tyrannical-mode to mode alist
(or (assq 'etalk-tyrannical-mode minor-mode-alist)
    (setq minor-mode-alist 
	  (cons '(etalk-tyrannical-mode " Tyrant") minor-mode-alist)))

;; add mode etalk-tyrant-enabled-console to mode alist
(or (assq 'etalk-tyrant-enabled-console minor-mode-alist)
    (setq minor-mode-alist 
	  (cons '(etalk-tyrant-enabled-console " T-Con") minor-mode-alist)))

(defvar etalk-tyrant-map nil
  "Keymap used when taking over keyboard to interact with functions.")

(defvar etalk-tyrant-c-map nil
  "Tyrant key sequences after a c-c")

(if etalk-tyrant-c-map
    ()
  (setq etalk-tyrant-c-map (make-sparse-keymap))
  (define-key etalk-tyrant-c-map "\C-c" 'etalk-usurp-tyrant-keyed)
  (define-key etalk-tyrant-c-map "m" 'tyrant-send-minibuffer-message))

(if etalk-tyrant-map
    ()
  (setq etalk-tyrant-map (make-keymap))
  (suppress-keymap etalk-tyrant-map)
  (if etalk-18-p
      (fillarray etalk-tyrant-map 'etalk-tyrant-fork-keypress)
    (fillarray (nth 1 etalk-tyrant-map) 'etalk-tyrant-fork-keypress))

  (define-key etalk-tyrant-map "\C-c"  etalk-tyrant-c-map)
  (define-key etalk-tyrant-map "\e"    nil)
  (define-key etalk-tyrant-map "\C-h"  'etalk-tyrant-help)
  (define-key etalk-tyrant-map "\C-l"  nil)
  (define-key etalk-tyrant-map "\C-x"  nil)
  ;; the following binding is to prevent accidental process deletion
  ;; upon deletion of a tyranted buffer.
  (define-key etalk-tyrant-map "\C-xk" 'etalk-usurp-tyrant-keyed)
  (define-key etalk-tyrant-map "\C-z"  nil)

  (if (not etalk-18-p)
      (progn
	;; Here, rebind all the arrows and the like to produce things
	;; which we want to see (keys like "^M" instead of [return]
	;;
	;; Hopefully, a key pres to C-m wont go to [return] brining us
	;; back to c-m
	(define-key etalk-tyrant-map [return] "\C-m")
	(define-key etalk-tyrant-map [right]  "\C-f")
	(define-key etalk-tyrant-map [left]   "\C-b")
	(define-key etalk-tyrant-map [up]     "\C-p")
	(define-key etalk-tyrant-map [down]   "\C-n")

	;; Here, lets make sure we can support the mouse in tyrant
	;; mode, thus, grab all mouse type events, and let the
	;; tyrant-mouse manager handle this stuff.
	(define-key etalk-tyrant-map [down-mouse-1] 'etalk-tyrant-handle-mouse)
	(define-key etalk-tyrant-map [down-mouse-2] 'etalk-tyrant-handle-mouse)
	(define-key etalk-tyrant-map [down-mouse-3] 'etalk-tyrant-handle-mouse)
	(define-key etalk-tyrant-map [mouse-1] 'etalk-tyrant-handle-mouse)
	(define-key etalk-tyrant-map [mouse-2] 'etalk-tyrant-handle-mouse)
	(define-key etalk-tyrant-map [mouse-3] 'etalk-tyrant-handle-mouse)
	(define-key etalk-tyrant-map [drag-mouse-1] 'etalk-tyrant-handle-mouse)
	(define-key etalk-tyrant-map [drag-mouse-2] 'etalk-tyrant-handle-mouse)
	(define-key etalk-tyrant-map [drag-mouse-3] 'etalk-tyrant-handle-mouse)
	(define-key etalk-tyrant-map [double-down-mouse-1] 'etalk-tyrant-handle-mouse)
	(define-key etalk-tyrant-map [double-down-mouse-2] 'etalk-tyrant-handle-mouse)
	(define-key etalk-tyrant-map [double-down-mouse-3] 'etalk-tyrant-handle-mouse)
	(define-key etalk-tyrant-map [double-mouse-1] 'etalk-tyrant-handle-mouse)
	(define-key etalk-tyrant-map [double-mouse-2] 'etalk-tyrant-handle-mouse)
	(define-key etalk-tyrant-map [double-mouse-3] 'etalk-tyrant-handle-mouse)
	)
    )
  )

;; The following list of variables describes what should be local variables
(defvar tyrant-point nil
  "Point where a tyrant function has left the cursor")

(defvar etalk-borrowed-keymap nil
  "Keymap tyrant mode has taken over.")

(defvar tyrant-call-interpreter nil
  "Function which can be set to evaluate message type 1 (game data).
Allows games to play as master/slave where initiator knows some piece
of global/random data like cards of word-thing pieces.")

(defvar etalk-tyrannical-usurped nil
  "Flag so asynchonous function calls know what is going on when a
tyranted game is ending.")

(defvar etalk-tyrannical-mode nil
  "Minor Mode variable")

(defvar etalk-tyrant-enabled-console nil
  "For games with TURNS, this flag is set by game to control playing.
When this is NIL, key presses are not interpretted, otherwise play can
proceede as normal.")

(defvar etalk-tyrant-brief-help nil
  "C-h will print a tyrant help string unless this is set in the game,
then it prints a brief help message based on the game rules.")

(defvar etalk-tyrant-quit-string nil
  "When a program is quit, and this has a value, then print something
like 'Eric wins!' before the hit return message.")

(defvar tyrant-not-turn-message nil
  "If set locally, the specified string is used instead of 'it's not
your turn' as per tyrant mode default.")

(defvar tyrant-player-index nil
  "Flag indicating who you are (player 1 means initiator, player 2
means you agreed to a game request")

(defvar tyrant-turn nil
  "Contains local version of who's turn it is during a tyrant
controlled game.  This allows tyrant-format to easily pring strings
based on turn so the game need not figure it out.")

(defvar tyrant-dont-restore-position nil
  "Set locally inside tyrant key press to allow games to hook upward
in removing where the cursor should be.")

;; functions

(defun etalk-tyrannical-mode ()
  "Minor mode taking over keypresses of innocent modes to allert
remote processes of what is going on.  (current-buffer) must be the
newly created mode."

  ;; not interactive!

  ;; set a mark to keep track of cursor
  (if (not (markerp 'tyrant-point)) 
      (progn 
	(make-local-variable 'tyrant-point)
	(setq tyrant-point (make-marker))))
  (set-marker tyrant-point (point-max))

  ;; mark this buffer as used by etalk
  (make-local-variable 'etalk-tag)
  (setq etalk-tag t)

  ;; get map generated by local mode
  (make-local-variable 'etalk-borrowed-keymap)
  (setq etalk-borrowed-keymap (current-local-map))

  ;; take over the buffer
  (use-local-map etalk-tyrant-map)

  ;; take over the etalk filter
  (make-local-variable 'etalk-filter-message)
  (setq etalk-filter-message nil)
  (make-local-variable 'etalk-filter-message-type)
  (setq etalk-filter-message-type 0)

  (if etalk-tyrant-imprisoned-process
      (progn
	(set-process-filter etalk-tyrant-imprisoned-process
			    'etalk-tyrant-filter-proc)
	(set-process-buffer etalk-tyrant-imprisoned-process
			    (current-buffer))))

  ;; flag for some procedures when the game is usurped.
  (make-local-variable 'etalk-tyrannical-usurped)
  (setq etalk-tyrannical-usurped nil)

  ;; use this variable to determine if you are a supressed mode.
  (make-local-variable 'etalk-tyrannical-mode)
  (setq etalk-tyrannical-mode t)

  ;; ALWAYS START WITH CONSOLE ENABLED
  ;; The etalk-filter-proc always sets the answeree to nil
  ;; in this case.  If you write something with no distinct "turns"
  ;; then you must set this variable back in your program.
  (make-local-variable 'etalk-tyrant-enabled-console)
  (setq etalk-tyrant-enabled-console t)
  (run-hooks 'etalk-tyrannical-hook)
  )

(defun tyrant-format (formatstring &rest args)
  "Format a string using with some key %'s to fix things, then process
as normal.  Valid %s are:
  %u : player one's username.
  %U : player two's username.
  %n : player one's preferred name.
  %N : player two's preferred name.
  %p : choose player based on local variable tyrant-turn.
  %P : choose player based on local variable tyrant-turn and use pref name
"
  (let ((extensions (list (list ?u (if (equal tyrant-player-index 1)
				       etalk-announce-as
				     etalk-tyrant-imprisoned-user))
			  (list ?U (if (equal tyrant-player-index 2)
				       etalk-announce-as
				     etalk-tyrant-imprisoned-user))
			  (list ?n (if (equal tyrant-player-index 1)
				       etalk-preferred-name
				     etalk-tyrant-imprisoned-preferred-name))
			  (list ?N (if (equal tyrant-player-index 2)
				       etalk-preferred-name
				     etalk-tyrant-imprisoned-preferred-name))
			  (list ?p (if tyrant-turn
				       (if (equal tyrant-turn 1)
					   (if (equal tyrant-player-index 1)
					       etalk-announce-as
					     etalk-tyrant-imprisoned-user)
					 (if (equal tyrant-player-index 2)
					     etalk-announce-as
					   etalk-tyrant-imprisoned-user))
				     "name"))
			  (list ?P (if tyrant-turn
				       (if (equal tyrant-turn 1)
					   (if (equal tyrant-player-index 1)
					       etalk-preferred-name
					     etalk-tyrant-imprisoned-preferred-name)
					 (if (equal tyrant-player-index 2)
					     etalk-preferred-name
					   etalk-tyrant-imprisoned-preferred-name))
				     "name")))))
    (eval (cons 'Sformat (cons (quote extensions) (cons formatstring args))))))

(defun tyrant-player1 ()
  "Book keeping proc to set some things up for tyrant mode"
  (make-local-variable 'tyrant-player-index)
  (setq tyrant-player-index 1))

(defun tyrant-player2 ()
  "Book keeping proc to set some things up for tyrant mode"
  (make-local-variable 'tyrant-player-index)
  (setq tyrant-player-index 2))

(defun etalk-tyrant-help ()
  "Universal help function for all tyrant mode games.  Depends on
having certain variables defined by the game itself."

  (interactive)
  (if etalk-tyrant-imprisoned-process
      (process-send-string etalk-tyrant-imprisoned-process ""))
  (if (equal last-command 'etalk-tyrant-help)
      (describe-mode)
    (if (and (boundp 'etalk-tyrant-brief-help) etalk-tyrant-brief-help)
	(message etalk-tyrant-brief-help)
      (message 
"Tyranted game: `C-c m' send message, `C-c C-c' quit `C-h' for describe-mode."
     ))))

(defun etalk-usurp-tyrant (&optional exit-message)
  "Restore the buffer the way it was before.  Includes restoring the
filter and the buffer keymap and some other things."

  (interactive)
  (use-local-map etalk-borrowed-keymap)
  (setq etalk-tyrannical-usurped t)
  ;; reset buffer incase of incomming message between filter/buffer change
  (if etalk-tyrant-imprisoned-process
      (progn
	(set-process-buffer etalk-tyrant-imprisoned-process
			    etalk-tyrant-return-buffer)
	(set-process-filter etalk-tyrant-imprisoned-process
			    'etalk-tcp-filter)))
  ;; ok  The buffer is restored.  Now we hope that the innocent mode
  ;; can remove itself and regenerate the etalk screen
  (setq etalk-tyrannical-mode nil)
  ;; Ok now pause meaningfully on the window.  process actions happen
  ;; as usuall, and will be seen after hitting return.
  (if exit-message
      (read-string (format "%s : Hit return to continue." exit-message))
    (read-string "Hit return to continue.")))

(defun etalk-usurp-tyrant-keyed (&optional nosend)
  "When you choose this, it usurps current tyrant process, _and_ sends
the usurp command to the remote machine."
  
  (interactive)
  (if (and (not nosend) etalk-tyrant-imprisoned-process)
      (process-send-string etalk-tyrant-imprisoned-process "\C-c\C-a"))
  (let ((qmess (if (boundp 'etalk-tyrant-quit-string) 
		   (if (fboundp 'etalk-tyrant-quit-string)
		       (funcall 'etalk-tyrant-quit-string)
		     (if (stringp etalk-tyrant-quit-string)
			 etalk-tyrant-quit-string
		       nil)))))
    (etalk-usurp-tyrant qmess)
    (if (equal tyrant-opponent-type 'etalk)
	(etalk-setup-windows))))

(defun tyrant-send-message (msg)
  "Send a message to a remote process of same type"

  (if etalk-tyrant-imprisoned-process
      (process-send-string etalk-tyrant-imprisoned-process 
			   (concat "\C-[" msg "\n"))))

(defun tyrant-send-minibuffer-message (msg)
  "Send a message to remote tyranted process to make message in minibuffer."
  
  (interactive "sMessage: ")
  (if etalk-tyrant-imprisoned-process
      (process-send-string etalk-tyrant-imprisoned-process
			   (concat "" etalk-announce-as ": " msg "\n"))))

(defun etalk-tyrant-handle-mouse (event)
  "Handle mouse events by sending that information as text to other
tyrant process."
  (interactive "e")
  (if (not etalk-tyrant-enabled-console)
      (if (and (boundp 'tyrant-not-turn-message) tyrant-not-turn-message)
	  (error tyrant-not-turn-message)
	(error "It isn't your turn!")))
  (etalk-tyrant-do-mouse-thing event)
  (track-mouse
    (while (progn
	     (setq event (read-event))
	     (or (mouse-movement-p event)
		 (eq (car-safe event) 'switch-frame)))
      (etalk-tyrant-do-mouse-thing event))
    )
  (etalk-tyrant-do-mouse-thing event)
  )

(defun etalk-tyrant-do-mouse-thing (event)
  "Do the mouse thing with event.  The handler handles drag type
things."
  (let ((pos (event-start event)))
    (if etalk-tyrant-imprisoned-process
	(process-send-string etalk-tyrant-imprisoned-process 
			     (concat "\C-]" 
				     ;; we can use %S because this
				     ;; function will never be called by
				     ;; 18 because we use using mousey
				     ;; things.  The message, however,
				     ;; will be interpretable
				     (format "%d %S %S" 
					     (posn-point pos)
					     (event-basic-type event)
					     (event-modifiers event))
				     "\n")))
    (if tyrant-mouse-function
	(eval (list tyrant-mouse-function
		    (posn-point pos)
		    '(event-basic-type event)
		    '(event-modifiers event)))
      )
    
    ;; check for exit condition
    (if (and etalk-tyrannical-usurped (equal tyrant-opponent-type 'etalk))
	(etalk-setup-windows)
      (if (not (equal tyrant-opponent-type 'etalk)) ;we are playing a program!
	  (if (not etalk-tyrant-enabled-console) ;it is thier turn
	      (progn
		(sit-for 0)
		;; this is fset in tyrn-ai
		(tyrant-opponent-type)))))
    ))
  
(defun etalk-tyrant-interpolate-keypress (the-key send)
  "Interpolates a keypress based on one new character in THE-KEY.
Returns t if it is used, and nil if it is not.  If SEND is t, then
send sequence to remote"

  ;; buildsequence is null when not given as argument.
  (let* ((sequence (concat etalk-tyrant-local-buildsequence 
			   (char-to-string the-key)))
	 (ttfk-sym (lookup-key etalk-borrowed-keymap sequence)))
    (if ttfk-sym
	(progn 
	  (cond
	   ;; if ttfk-symbol is a keymap, then read the next char untill done.
	   ((keymapp ttfk-sym)
	    (setq etalk-tyrant-local-buildsequence ttfk-sym))
	   ;; if symbol is a function, then get function and send sequence
	   ;; to remote, but only if we are not called from a filter
	   ((fboundp ttfk-sym)
	    (if (and send (equal tyrant-opponent-type 'etalk))
		(process-send-string etalk-tyrant-imprisoned-process sequence))
	    (setq etalk-tyrant-local-buildsequence nil)
	    (if (not send)
		(setq last-input-char the-key))
	    (funcall ttfk-sym))
	   ;; just in case something weird happens
	   (t
	    (setq etalk-tyrant-local-buildsequence nil)
	    (etalk-tyrant-help)))
	  t)
      nil)
    ))
 
(defun etalk-tyrant-fork-keypress ()
  "This function finds the command needed by a keypress, sends the
info to the remote machine, and runs that function on the local
machine."

  (interactive)
  (if (not etalk-tyrant-enabled-console)
      (if (and (boundp 'tyrant-not-turn-message) tyrant-not-turn-message)
	  (error tyrant-not-turn-message)
	(error "It isn't your turn!")))

  ;; interpret it, and do help if we fail
  (if (not (etalk-tyrant-interpolate-keypress last-input-char t))
      (etalk-tyrant-help))      

  ;; check for exit condition
  (if (and etalk-tyrannical-usurped (equal tyrant-opponent-type 'etalk))
      (etalk-setup-windows)
    (if (not (equal tyrant-opponent-type 'etalk)) ;we are playing a program!
	(if (not etalk-tyrant-enabled-console) ;it is thier turn
	    (progn
	      (sit-for 0)
	      ;; this is fset in tyrn-ai
	      (tyrant-opponent-type))))) )

(defun etalk-tyrant-filter-proc (process output)
  "This function filters keypresses sent over the etalk connection, and
runs procedures based on those sequences."

  (let ((tf-oldbuffer (window-buffer (selected-window)))
	(tf-pbuffer (process-buffer process)))

    (save-excursion
      (set-buffer tf-pbuffer)
      (move-marker tyrant-point (point))) ;backup point mover when
					;nothing is happening.
  (let ((usurped nil)
	(tyrant-dont-restore-position nil)
	(tf-strlen (length output))
	(tf-cnt 0)
	(tf-tchar))
    (while (and ( < tf-cnt tf-strlen) (not usurped))
      (setq tf-tchar (string-to-char (substring output tf-cnt (+ 1 tf-cnt))))
      (setq tf-cnt (+ 1 tf-cnt))
      (if (equal tf-tchar 3)
	  (save-excursion
	    (set-buffer tf-pbuffer)
	    (setq etalk-filter-message "")
	    (setq etalk-filter-message-type 0))
	(if (equal tf-tchar 29)
	    (save-excursion
	      (set-buffer tf-pbuffer)
	      (setq etalk-filter-message "")
	      (setq etalk-filter-message-type 2))
	  (if (equal tf-tchar 27)
	      (save-excursion
		(set-buffer tf-pbuffer)
		(setq etalk-filter-message "")
		(setq etalk-filter-message-type 1))
	    ;; if there is a message, _and_ if that message has a type
	    ;; then continue on, else, wait for message to have a type.
	    (if (save-excursion (and (set-buffer tf-pbuffer)
				     etalk-filter-message
				     etalk-filter-message-type))
		(save-excursion
		  (set-buffer tf-pbuffer)
		  (if (and (equal etalk-filter-message "")
			   (equal tf-tchar 1))
		      (progn
			(setq etalk-filter-message nil)
			(setq usurped t))
		    (progn
		      ;; when we hit the CR ending of message text.
		      (if (equal tf-tchar 10)
			  (progn
			    (cond
			     ((equal etalk-filter-message-type 0)
			      ;; dont display if in minibuffer!!!!!!!
			      (setq etalk-filter-message-type nil)
			      ;; set type to nil _first_ in case of
			      ;; interuption due to sleeps in tyranted code
			      (if (equal (selected-window)
					 (minibuffer-window))
				  ()
				(message etalk-filter-message))
			      (setq etalk-filter-message nil))
			     ((equal etalk-filter-message-type 1)
			      (save-excursion
				;; set type to nil _first_ in case of
				;; interuption due to sleeps in tyranted code
				(setq etalk-filter-message-type nil)
				(funcall tyrant-call-interpreter
					 etalk-filter-message)
				(move-marker tyrant-point (point))
				(setq etalk-filter-message nil)))
			     ((equal etalk-filter-message-type 2)
			      ;; parse the message for mouse commands
			      (let ((omd (match-data)))
				(if (string-match
				     "\\([0-9]+\\) \\([-a-z0-9]+\\)"
				     etalk-filter-message)
				    (let ((pos (string-to-int
						(substring
						 etalk-filter-message
						 (match-beginning 1)
						 (match-end 1))))
					  (evnt (read
						 (substring
						  etalk-filter-message
						  (match-beginning 2)
						  (match-end 2))))
					  (mod (read
						(substring
						 etalk-filter-message
						 (match-end 2)))))
				      ;; Now that it is parsed, lets
				      ;; do some mouse things.
				      (if tyrant-mouse-function
					  (eval (list tyrant-mouse-function
						      pos
						      'evnt
						      'mod)))))
				(move-marker tyrant-point (point))
				(save-match-data omd)))
				))
			(setq etalk-filter-message 
			      (concat etalk-filter-message
				      (char-to-string tf-tchar)))
			(setq tyrant-dont-restore-position nil)))))
	      (if (equal tf-tchar ?\C-h)
		  (message "Remote is reading help!!!")
		(save-excursion
		  (set-buffer tf-pbuffer)
		  (etalk-tyrant-interpolate-keypress tf-tchar nil)
		  (move-marker tyrant-point (point)))))))))
    (if tyrant-dont-restore-position (set-buffer tf-oldbuffer))
    (if (save-excursion 
	  (set-buffer tf-pbuffer)
	  etalk-tyrannical-usurped)
	(etalk-setup-windows)
      ;; else
      (if (not (equal (selected-window) (minibuffer-window)))
	  (etalk-move-realpoint tf-pbuffer 
			       (save-excursion
				 (set-buffer tf-pbuffer)
				 tyrant-point))))
    (if (save-excursion
	  (set-buffer tf-pbuffer)
	  usurped)
	(progn
	  (etalk-usurp-tyrant-keyed t)	;keyed with NOSEND set to print quit
	  (etalk-setup-windows))))))

;;; end lisp
(provide 'etalk-tyrn)