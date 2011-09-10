;;; winhist.el --- window configuration history

;; Copyright 1997 Bob Glickstein.      <http://www.zanshin.com/~bobg/>

;; Author: Bob Glickstein <bobg@zanshin.com>
;; Maintainer: Bob Glickstein <bobg@zanshin.com>
;; Version: 3.0

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, send e-mail to
;; this program's maintainer or write to the Free Software Foundation,
;; Inc., 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Plug:

;; Check out my book, "Writing GNU Emacs Extensions," from O'Reilly
;; and Associates.  <http://www.ora.com/catalog/gnuext/>

;;; Commentary:

;; `winhist-mode' is a global mode that records every change to the
;; window configuration in a "ring" (like the yank ring or the mark
;; ring).  It is then possible to step backward and forward through
;; the history of window configurations.  I find this to be valuable
;; for restoring mental context when working on a complex project.

;; This works imperfectly because of vagaries in the way buffers and
;; windows are connected and manipulated by various packages.  In
;; particular, sometimes several consecutive window configurations in
;; the history may seem to be identical.  But it's better than
;; nothing, and I also like it better than some packages that require
;; you to remember to save and possibly name a window configuration
;; before restoring it.

;; To use this package, add
;;  (require 'winhist)
;;  (winhist-mode 1)
;; to your .emacs.  You should then bind keysequences to the commands
;; `winhist-backward' and `winhist-forward'.  I prefer:
;;  (global-set-key "\M-B" 'winhist-backward)
;;  (global-set-key "\M-F" 'winhist-forward)

;;; Code:

(require 'ring)

(defvar winhist-timer nil)
(defvar winhist-delay .125
  "*Seconds after Emacs becomes idle before recording window configuration.")
(defvar winhist-frames nil
  "Alist of the form ((frame . (ring . index)) ...).")

(defvar winhist-ring-size 100
  "*How many window configurations to automatically remember.")
(defvar winhist-mode nil)
(defvar winhist-traversing nil)
(defvar winhist-changed nil)

;; This function goes in window-configuration-change-hook.
(defsubst winhist-notice-change ()
  (setq winhist-changed (selected-frame)))

;; This function goes in post-command-hook.
(defun winhist-maybe-record-configuration ()
  (if (and winhist-changed
	   winhist-mode
	   (not winhist-traversing)
	   (not (window-minibuffer-p (selected-window))))
      (let ((entry (assq winhist-changed winhist-frames)))
	(unless entry
	  (setq entry (cons winhist-changed
			    (cons (make-ring winhist-ring-size)
				  0))
		winhist-frames (cons entry winhist-frames)))
	(ring-insert (car (cdr entry))
		     (current-window-configuration))
	(setcdr (cdr entry) 0)))
  (setq winhist-changed nil)
  (setq winhist-traversing nil))

(defun winhist-traverse (&optional n)
  "Move backward through the window configuration history by N entries."
  (interactive "p")
  (let ((entry (assq (selected-frame) winhist-frames))
	ring index conf)
    (unless entry
      (error "No window history for this frame."))
    (setq ring (car (cdr entry))
	  index (+ n (cdr (cdr entry)))
	  conf (ring-ref ring index)
	  winhist-traversing t)
    (setcdr (cdr entry) index)
    (when conf
      (set-window-configuration conf)
      (message "Window history: %d" index))))

(defalias 'winhist-backward 'winhist-traverse)

(defsubst winhist-forward (&optional n)
  "Move forward through the window configuration history by N entries."
  (interactive "p")
  (winhist-traverse (- n)))

(defun winhist-mode (&optional arg)
  "Toggle recording of window configuration changes.
With optional ARG, turn winhist-mode on iff positive, off otherwise.
The number of window configurations that are memorized is controlled
by `winhist-ring-size'.
To traverse the window configuration history, use \\[winhist-backward]
and \\[winhist-forward]."
  (interactive "P")
  (setq winhist-mode
	(if (null arg)
	    (not winhist-mode)
	  (> (prefix-numeric-value arg) 0)))
  (when winhist-timer
    (cancel-timer winhist-timer)
    (setq winhist-timer nil))
  (if winhist-mode
      (if (boundp 'window-configuration-change-hook)
	  (progn
	    (add-hook 'window-configuration-change-hook 'winhist-notice-change)
	    (setq winhist-timer
		  (run-with-idle-timer winhist-delay t
				       'winhist-maybe-record-configuration)))
	(progn
	  (setq winhist-mode nil)
	  (error "This version of Emacs does not have `window-configuration-change-hook'")))
    (setq winhist-frames nil)
    (remove-hook 'window-configuration-change-hook 'winhist-notice-change)))

(provide 'winhist)

;;; winhist.el ends here
