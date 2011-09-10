;;;
;;; FILE
;;;	follow.el	V0.1	Alpha
;;;
;;;	Copyright (C) 1995 by Anders Lindgren.
;;;
;;; DISTRIBUTION
;;;	follow-mode is free software; you can redistribute it and/or modify
;;;	it under the terms of the GNU General Public License as published 
;;;	by the Free Software Foundation; either version 2, or (at your 
;;;	option) any later version.
;;;
;;;	GNU Emacs is distributed in the hope that it will be useful,
;;;	but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;	GNU General Public License for more details.
;;;
;;;	You should have received a copy of the GNU General Public
;;;	License along with GNU Emacs; see the file COPYING.  If not,
;;;	write to the Free Software Foundation, 675 Mass Ave, Cambridge,
;;;	MA 02139, USA.
;;;
;;; AUTHOR
;;;	Anders Lindgren, andersl@csd.uu.se
;;;
;;; DISCRIPTION
;;;	`Follow' makes all the windows displaying the same buffer 
;;;	act like one window. The contents of the windows (displaying the 
;;;	same buffer) are always adjecant and you can use your normal
;;;	commands to move between windows.
;;;
;;;	`Follow' comes to its prime when used on a big graphical screen
;;;	wide enough for two columns. 
;;;
;;;	`follow-mode' is a minor mode and can be used together with, to my
;;;	knowledge, all major and minor modes.
;;;
;;;	Only windows visible in the same frame follow eachother, giving you 
;;;	full freedom to look at two different positions in the file
;;;	at the same time.
;;;
;;;	Some special commands has been developed to make life even easier:
;;;
;;;		follow-scroll-up	C-c C-v
;;;			Places the first window below the last and
;;;			redisplays. This is useful when walking through
;;;			a file, say, two pages at the time.
;;;		follow-scroll-down	C-c v
;;;			Like `follow-scroll-up', but in the 
;;;			other direction.
;;;		follow-recenter		C-c C-l
;;;			Places the point in the middle window and 
;;;			recenters it.
;;;
;;;	The next functions is not needed in `follow-mode' (i hope :-) )
;;;	but can be useful to arrange the windows around the selected
;;;	window without `gluing' them together:
;;;		follow-redraw		<not bound>
;;;
;;;	`follow' will only operate on the selected window only.
;;;	(When a possible disaligned window is selected the
;;;	`followers' are automagically aligned.)
;;;
;;;	`follow' mode can be turned off.
;;;
;;; USAGE
;;;	Place the following line in your startupfile (e.g. ~/.emacs):
;;;		(autoload 'follow-mode "follow" nil t)
;;;
;;;	To activate type:  M-x follow-mode
;;;	and press return.
;;;
;;;	When defining keys specific to follow-mode, please use
;;;	`follow-define-key' as the binding is undone when `follow' mode is
;;;	deactivated.
;;;
;;;	For example:
;;;	(setq follow-mode-hook '(lambda ()
;;;	            (follow-define-key \"\\C-ca\" 'your-favorite-function)
;;;		    (follow-define-key \"\\C-cb\" 'another-function)
;;;		    ))
;;;
;;; HISTORY
;;;	25-May-95 ALi * File created. The minor mode code copied from my
;;;			my sas-c-emacs package.
;;;	26-May-95 ALi * It works!
;;;	27-May-95 ALi * Avoids hitting the head in the roof.
;;;			follow-scroll-up, -scroll-down, and -recenter.
;;;
;;; ABNORMALITIES
;;;	- When dragging the scrollbar of a window other than the selected,
;;;	  the scrollbar immediately returns to its original position.
;;;	- When instering text at the end of a window, it is recentered.
;;;
;;; LCD Archive Entry:
;;; follow-0.1|Anders Lindgren|andersl@csd.uu.se|
;;; Make several windows act like one. (Wide screen => Twice the workspace)|
;;; 27-May-1995|0.1|~/misc/follow-0.1.el.gz|
;;;

(defvar follow-mode nil
  "Variable indicating if follow-mode is active.")

;;; Internal variables

(defvar follow-original-keys nil
  "Storeage for replaced keys.")

(defvar follow-internal-force-redisplay nil
  "True when `follow' should redisplay the windows.")


(defun follow-mode (arg)
  "Minor mode making all windows showing the same buffer glued together.
The contents of the windows (displaying the same buffer) are always adjecant
and you can use your normal commands to move between windows.

`Follow' mode comes to its prime when windows are displayed side by
side (\\[split-window-horizontally]).

Only windows in the same frame follow eachother.

When follow-mode is switched on, the hook `follow-hook' is called."
  (interactive "P")
  (make-local-variable 'follow-mode)
  (make-local-variable 'follow-original-keys)
  (make-local-variable 'post-command-hook)
  (let ((follow-mode-orig follow-mode))
    (setq follow-mode
	  (if (null arg) (not follow-mode)
	    (> (prefix-numeric-value arg) 0)))
    (or (assq 'follow-mode minor-mode-alist)
	(setq minor-mode-alist
	      (cons '(follow-mode " Follow") minor-mode-alist)))
    (cond ((and follow-mode (not follow-mode-orig))
	   ;; turning on follow-mode
	   (setq follow-original-keys '())
	   (follow-define-key "\C-c\C-l"     'follow-recenter)
	   (follow-define-key "\C-c\C-v"     'follow-scroll-up)
	   (follow-define-key "\C-cv"        'follow-scroll-down)
	   (add-hook 'post-command-hook 'follow-post-command-hook)
	   (run-hooks 'follow-mode-hook))
	  ((and (not follow-mode) follow-mode-orig)
	   ;; turning off follow-mode
	   (setq post-command-hook 
		 (delq 'follow-post-command-hook post-command-hook))
	   (follow-undef-keys)))))

;;
;; Key definition functions. (Keys defined is unbound when
;; `follow' mode is exited. This makes it possible to, for example,
;; rebind C-v to `follow-scroll-up'.)
;;

(defun follow-define-key (key fnk)
  "Make a keybinding which can be undone."
  (setq follow-original-keys (cons (cons key (local-key-binding key)) 
				  follow-original-keys))
  (local-set-key key fnk))

(defun follow-undef-keys ()
  "Undefine the keys bound by `follow-mode'
and restore the bindings to their original values"
  (while follow-original-keys
    (let ((fnk (cdr (car follow-original-keys)))
	  (key (car (car follow-original-keys))))
      (if (numberp fnk)
	  (local-unset-key key)
	(local-set-key key fnk)))
    (setq follow-original-keys (cdr follow-original-keys))))

;;
;; User functions usable when in follow-mode.
;;

(defun follow-scroll-up ()
  "Scroll text of `following' windows up above all windows.
Works like `scroll-up' when not in `follow' mode."
  (interactive)
  (if follow-mode
      (let* ((followers (follow-all-followers))
	     (end (window-end (car (reverse followers)))))
	(if (eq end (point-max))
	    (signal 'end-of-buffer nil)
	  (select-window (car followers))
	  (goto-char end)
	  (forward-line (- next-screen-context-lines))
	  (set-window-start (car followers) (point))))
    (scroll-up)))


(defun follow-scroll-down ()
  "Scroll text of all `following' windows down below all windows.
Works like `scroll-down' when not in `follow' mode."
  (interactive)
  (if follow-mode
      (let* ((followers (follow-all-followers))
	     (win (car (reverse followers)))
	     (start (window-start (car followers))))
	(if (eq start (point-min))
	    (signal 'beginning-of-buffer nil)
	  (select-window win)
	  (goto-char start)
	  (forward-line (- (- (window-height win) 
			      1 
			      next-screen-context-lines)))
	  (set-window-start win (point))
	  (goto-char start)
	  (forward-line (- next-screen-context-lines 1))
	  (setq follow-internal-force-redisplay t)))
    (scroll-down)))


(defun follow-recenter ()
  "Recenter the middle window around the point,
and rearrange all other windows around the middle window."
  (interactive)
  (let* ((dest (point))
	 (followers (follow-all-followers))
	 (win (nth (/ (- (length followers) 1) 2) followers)))
    (select-window win)
    (goto-char dest)
    (recenter)
    (setq follow-internal-force-redisplay t)))


(defun follow-redraw ()
  "Arrange windows displaying the same buffer in successor order.
This function can be called even if the buffer is not in `follow-mode'.

Hopefully, there should be no reason to call this function when in
`follow' mode since the windows should always be aligned."
  (interactive)
  (sit-for 0)
  (follow-redisplay))

;;;
;;; The redisplay routines:
;;;


(defun follow-windows-aligned-p ()
  "Returns `t' whenever the follower windows are alinged"
  (let ((followers (follow-all-followers))
	(res t))
    (while (and res (cdr followers))
      ;; At least two followers left
      (if (not (eq (window-end (car followers)) 
		   (window-start (car (cdr followers)))))
	  (setq res nil))
      (setq followers (cdr followers)))
    res))


(defun follow-all-followers ()
  "Get all windows displaying the same buffer as the selected window."
  (let* ((top (window-at 0 0))
	 (win top)
	 (done)
	 (followers))
    (while (not done)
      (if (equal (window-buffer win) (current-buffer))
	  (setq followers (cons win followers)))
      (setq win (next-window win))
      (if (equal win top)
	  (setq done t)))
    (reverse followers)))


(defun follow-split-followers (followers)
  "Split the followers into the sets: predecessors and successors.
Returns (pred . succ). `pred' and `succ' are ordered starting 
from the selected window."
  (let ((pred '()))
    (while (not (equal (car followers) (selected-window)))
      (setq pred (cons (car followers) pred))
      (setq followers (cdr followers)))
    (cons pred (cdr followers))))

  
(defun follow-redisplay ()
  "Redisplay all the followers around the selected window.
Should the point be too close to the roof we redisplay everything
from the top."
  (let* ((followers (follow-all-followers))
	 (all (follow-split-followers followers))
	 (pred (car all))
	 (succ (cdr all))
	 (dest (point)))
    (if (follow-hit-roof-p pred)
	; We hit the roof, redisplay everything from the top.
	(progn
	  (select-window (car followers))
	  (goto-char (point-min))
	  (set-window-start (car followers) (point-min))
	  (follow-downward (cdr followers))
	  (follow-goto-char dest))
      ; We managed to stay clear of the roof, redraw with the current
      ; window as center.
      (follow-downward succ)
      (follow-upward pred))))


(defun follow-downward (followers)
  "Redisplay all the the windows below the current"
  (save-excursion
    (let ((origwin (selected-window))
	  (start (+ (count-lines (point-min) (window-start)) 
		    (window-height)))
	  (newstart))
      (while followers
	(select-window (car followers))
	(setq newstart (+ start (- (window-height) 1)))
	(goto-line start)
	(set-window-start (selected-window) (point))
	(setq start newstart)
	(setq followers (cdr followers)))
      (select-window origwin))))


(defun follow-upward (followers)
  "Redisplay all the the windows above the current
Note: all `followers' are predecessors."
  (save-excursion
    (let ((origwin (selected-window))
	  (start (+ (count-lines (point-min) (window-start)) 1)))
      (while followers
	(select-window (car followers))
	(setq start (- start (- (window-height) 1)))
	(goto-line start)
	(set-window-start (selected-window) (point))
	(setq followers (cdr followers)))
      (select-window origwin))))


(defun follow-hit-roof-p (followers)
  "True if we will hit the roof while drawing the windows above us."
  (let ((start (+ (count-lines (point-min) (window-start)) 1)))
    (while followers
      (setq start (- start (- (window-height (car followers)) 1)))
      (setq followers (cdr followers)))
    (< start 1)))

;;
;; The magic little box. This function is called after every command.
;;
;; If the point has left the window, it is moved to another window, if
;; possible. The origial window is restored to its original position.
;;
;; If the point can't be moved to any window, or if the windows in some
;; way has been unaligned, all windows (displaying the current buffer) are
;; redrawn.
;;
;; Not perfect: The pos-visible-in-window-p acts on old window limits.
;; If, however, we should execute it after `sit-for' we would redraw the
;; window, which isn't what we would like to do since we can't detect that
;; the point has left the window and wa can't restore it to it's original
;; position. However, it seems to work...
;;

(defun follow-post-command-hook ()
  "Repositions point and windows when in follow mode."
  (if (and (not follow-internal-force-redisplay)
	   (not (pos-visible-in-window-p (point))))
    (let ((dest (point))
	  (win (selected-window))
	  (start (window-start (selected-window))))
      (follow-goto-char dest)
      ;; If a new window has been selected, force the old window back 
      ;; to it's original position.
      (if (not (eq win (selected-window)))
	  (set-window-start win start nil))))
  (sit-for 0)	;; Redisplay
  (if (or follow-internal-force-redisplay
	  (not (follow-windows-aligned-p)))
      (follow-redisplay))
  (setq follow-internal-force-redisplay nil))


;;
;; If we end up outside the window, try to pick the best new candidate.
;;

(defun follow-goto-char (pos)
  "Set the point in the window, or in a follower."
  (if (pos-visible-in-window-p pos)
      (goto-char pos)
    (let* ((followers (follow-all-followers))
	   (all followers)
	   (done))
      ; Make a window, displaying the current point position, active.
      (while (and followers (not done))
	(if (pos-visible-in-window-p pos (car followers))
	    (progn
	      (select-window (car followers))
	      (goto-char pos)
	      (setq done t))
	  (setq followers (cdr followers))))
      (if (not done)
	  ; No one is displaying the point. Select a new one.
	  ; This could later lead to a full redisplay.
	  (progn
	    (if (eq pos (point-max)) 
		(select-window (car (reverse all)))
	      (if (eq pos (point-min)) 
		  (select-window (car all))))
	    (goto-char pos))))))
