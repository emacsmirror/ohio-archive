;;; finger information browsing program.
;;;
;;; Copyright (C) 1992 Eric M. Ludlam
;;;
;;; Author: Eric M. Ludlam <zappo@gnu.ai.mit.edu>
;;; Version: 0.8
;;; Keywords: talk, tools
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
;;; Please send bug reports, etc. to zappo@gnu.ai.mit.edu
;;;

;;; Commentary:
;;;   This package provides a simpled dired like interface to a finger
;;;   information list recieved from a remote/local machine.  It uses
;;;   several calls to the etalk code for minibuffer access, and
;;;   finger header parsing abilities, making this package much
;;;   smaller than it could otherwise prove to be.
;;;
;;;   From this list, a user is able to single out a single user
;;;   currently logged in, and "do" things to them, wether it be mail,
;;;   or opening a talk connection.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Finger mode for emacs, included with etalk.
;;;   O Can parse multiple finger headers through use of etalk
;;;     minibuffer functions.
;;;   O Send pre-filled mail message to user on a line.
;;;   O Get extra finger information on user on a line.
;;;   O Start etalk to a person on a given line.
;;;   O Does not work with VMS very well.
;;;

(require 'etalk-mini)

(autoload 'etalk "etalk"
  "Talk to remote user via emacs buffers." t)

(defvar finger-version "Finger 0.8B"
  "The current running version of finger.")

(defvar finger-mode-map nil
  "Keymap used in finger-mode")
(if finger-mode-map
    ()
  (setq finger-mode-map (make-sparse-keymap))
  (define-key finger-mode-map " "   'finger-next-line)
  (define-key finger-mode-map "f"   'finger-finger-name)
  (define-key finger-mode-map "g"   'finger-re-finger)
  (define-key finger-mode-map "h"   'describe-mode)
  (define-key finger-mode-map "m"   'finger-mail)
  (define-key finger-mode-map "n"   'finger-next-line)
  (define-key finger-mode-map "p"   'finger-prev-line)
  (define-key finger-mode-map "q"   'finger-quit)
  (define-key finger-mode-map "r"   'isearch-backward)
  (define-key finger-mode-map "s"   'isearch-forward)
  (define-key finger-mode-map "t"   'finger-talk)
  (define-key finger-mode-map "x"   'finger-quit)
  (define-key finger-mode-map "\177" 'finger-prev-line)
  (define-key finger-mode-map [down] 'finger-next-line)
  (define-key finger-mode-map [up]   'finger-prev-line)
)

(defvar finger-inhibit-startup-message nil
  "Don't display stuff when starting finger.")

(defvar finger-buffer-name "@%m"
  "The buffer name used in finger mode.")

(defvar finger-clear-before-finger t
  "Non-nil means delete the finger buffer before conducting a finger
of someone.")

(defvar finger-tty-start nil
  "Defines locally where the tty column starts")
(defvar finger-tty-none nil
  "Defines a flag which is t when there is no tty column found (VMS)")
(defvar finger-name-start nil
  "Defines the column where the username starts.")
(defvar finger-node nil
  "Defines locally the node the current finger buffer is looking at.")

;;; ###autoload
(defun finger (node)
  "Major mode for looking at finger information."

  (interactive (cons 
		(unwind-protect
		    (save-window-excursion
		      (finger-blurb-buffer)
		      (etalk-read-host "Finger where: "))
		  (if (get-buffer "Your Rights with finger.el")
		      (kill-buffer "Your Rights with finger.el")))
		     '()))
  (let ((tmp nil))
    (if (string-match "\\(@\\)" node)
	(setq node (substring node (match-end 1) (length node))))
    (if (equal node "")
	(setq node (system-name)))
    (if (setq tmp (finger-make-list node))
	(progn
	  (set-buffer tmp)
	  (finger-mode node)
	  (switch-to-buffer tmp)))))

(defun finger-mode (node)
  "Defines a major mode for processing finger information.
\\<finger-mode-map>
  n, C-n, SPC Next line.
  p, C-p, DEL Previous line.
  \\[finger-finger-name]           Get more finger information on user.
  \\[finger-re-finger]           Update finger list.
  \\[finger-mail]           Send mail to user in pre-filled mail buffer.
  \\[finger-talk]           Talk to user, with filled in minibuffer query.
  \\[finger-quit]           Delete this annoying buffer.
  \\[isearch-forward]           Search this list.
  \\[isearch-backward]           Search this list backwards.

"
  (interactive (cons (etalk-read-host "Finger where: ") '()))
  (use-local-map finger-mode-map)
  (setq mode-name "Fingered")
  (setq major-mode 'finger-mode)
  (setq mode-line-buffer-identification (list finger-version ": %17b"))
  (make-local-variable 'finger-node)
  (setq finger-node node)
  (run-hooks 'finger-mode-hooks))

(defun finger-re-finger ()
  "Update the current finger list."
  
  ;; for some reason, when this procedure is used among all these
  ;; other procedures, it crashes emacs with signal 4.  It didn't work
  ;; within a cut-up version of this file.

  (interactive)
  (finger-make-list finger-node))

(defun finger-quit ()
  "Quit the current finger buffer."

  (interactive)
  (if (equal major-mode 'finger-mode)
      (kill-buffer (current-buffer))))

(defun finger-talk ()
  "Get the username and tty from the command line and call talk."
  
  (interactive)
  (let ((name (finger-get-address-line))
	(tty (finger-get-tty-line)))
    (etalk (etalk-read-username (concat (finger-get-address-line) " "
					(finger-get-tty-line))))))

(defun finger-mail ()
  "send mail to the person the cursor is on."

  (interactive)
  (let ((to-field (finger-get-address-line)))
    (mail)
    (beginning-of-buffer)
    (end-of-line)
    (insert to-field)
    (next-line 1)
    (end-of-line)))

(defun finger-finger-name ()
  "finger person the cursor is on"

  (interactive)
  (finger-user (finger-get-address-line)))

(defun finger-get-address-line ()
  "get an address from a line in a finger buffer"
  
  (format "%s@%s" (finger-get-name-line) finger-node))

(defun finger-get-name-line ()
  "Get the username from the current line"

  (let ((ans
	 (save-excursion
	   (beginning-of-line)
	   (if (bobp) (error "No username on first line of buffer!"))
	   (forward-char finger-name-start)
	   (buffer-substring (point) (+ (point) 9)))))
    (if (string-match "\\([A-Za-z0-9]+\\)" ans)
	(substring ans (match-beginning 1) (match-end 1))
      ans)))

(defun finger-get-tty-line ()
  "Get the username from the current line"

  (let ((ans
	 (save-excursion
	   (beginning-of-line)
	   (forward-char finger-tty-start)
	   (buffer-substring (point) (+ (point) 9)))))
    (if (string-match "\\([A-Za-z0-9]+\\)" ans)
	(setq ans (substring ans (match-beginning 1) (match-end 1))))
    (if finger-tty-none
	(setq ans (concat "tty" ans)))
    ans))

(defun finger-next-line ()
  "Move finger cursor to next line."
  (interactive)
  (next-line 1)
  (beginning-of-line)
  (search-forward-regexp " [^ ]")
  (forward-char -1))

(defun finger-prev-line ()
  "Move finger cursor to prev line."
  (interactive)
  (previous-line 1)
  (beginning-of-line)
  (search-forward-regexp " [^ ]")
  (forward-char -1))

(defun finger-user (user)
  "Run finger on people with neat interactive prompt."

  (interactive (cons (etalk-read-username) '()))

  (if (string-match "\\( \\)" user)
      (setq user (substring user 0 (match-beginning 1))))

  (if (string-match "\\(@\\)" user)
      (let ((remote (substring user (match-end 1) (length user))))
	(if (equal remote (system-name))
	    (setq user (substring user 0 (match-beginning 1))))))
    
  (message "Looking for %s." user)

  (let ((bstring (format "*finger* {%s}" user)))

    (get-buffer-create bstring)
    
    (set-buffer (get-buffer bstring))
    (if finger-clear-before-finger
	(delete-region (point-min) (point-max)))

    (goto-char (length (buffer-string)))
    (call-process "finger" nil bstring nil "-m" user)
    (goto-char 1)
    (display-buffer bstring)))

(defun finger-make-list (node)
  "Generate a finger list of users logged on to a specific node."

  (interactive "sNode: ")

  (message "Checking %s for finger information." node)
  (let ((bstring (etalk-format finger-buffer-name "" node "")))
    (get-buffer-create bstring)
    (set-buffer (get-buffer bstring))
    (setq buffer-read-only nil)
    (if finger-clear-before-finger
	(delete-region (point-min) (point-max)))
    (if (equal node "")
	(call-process "finger" nil bstring nil)
      (call-process "finger" nil bstring nil (format "@%s" node)))
    
    (let* ((numlist (etalk-parse-finger-header node))
	   (namestart (car numlist))
	   (ttystart (nth 1 numlist))
	   (ttynone (nth 2 numlist)))
      (make-local-variable 'finger-tty-start)
      (setq finger-tty-start ttystart)
      (make-local-variable 'finger-tty-none)
      (setq finger-tty-none ttynone)
      (make-local-variable 'finger-name-start)
      (setq finger-name-start namestart))
    (finger-prev-line)
    (setq buffer-read-only t)
    (get-buffer bstring)))

(defun finger-blurb-buffer ()
  "You have the right to remain silent, anything you say can and will
be typecast against you."

  (or finger-inhibit-startup-message
      (save-excursion
	(setq finger-inhibit-startup-message t)
	(get-buffer-create "Your Rights with finger.el")
	(set-buffer (get-buffer "Your Rights with finger.el"))
	(delete-region (point-min) (point-max))
	(insert "
     Thanks for using Emacs Finger! [" finger-version "]

     This program is free under the GNU general public license.
     See the GNU COPYING file for more details.

     Please report Bugs/Problems/Suggestions to: Eric Ludlam via
                                                 zappo@gnu.ai.mit.edu")
	(display-buffer "Your Rights with finger.el"))))

;;; end of lisp
(provide 'finger)
