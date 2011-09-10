;Date: 1 Mar 89 04:01:21 GMT
;From: wolfgang@mgm.mit.edu  (Wolfgang Rupprecht)
;Subject: Re: view mode, mode line, too many [[[]]]
;To: bug-gnu-emacs@prep.ai.mit.edu
;
;In article <TALE.89Feb24184230@pawl22.pawl.rpi.edu> tale@pawl.rpi.edu writes:
;>  Recursive editing levels are important and useful features of Emacs, but
;>they can seem like malfunctions to the user who does not understand them.
;
;Infact, they can even seem like bugs to users that do.  
;
;In an attempt to stamp out needless recursive edits I submit more-mode.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;									     ;;
;;	File:     more.el						     ;;
;;	Author:   Wolfgang Rupprecht					     ;;
;;	Created:  Thu Nov 19 20:01:59 EST 1987				     ;;
;;	Contents: just like more(1). Doesn't do a recursive edit.	     ;;
;;									     ;;
;;	Copyright (c) 1987 Wolfgang Rupprecht.				     ;;
;;	All rights reserved.						     ;;
;;									     ;;
;;	$Log$								     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar more-mode-map nil "Keymap for more mode.")
(defvar more-mode-hook nil "Functions to call when entering more mode. ")

(if more-mode-map
    nil
  (setq more-mode-map (make-keymap))
  (suppress-keymap more-mode-map)  
  (define-key more-mode-map " " 'scroll-up)
  (define-key more-mode-map "b" 'scroll-down)
  (define-key more-mode-map "q" 'more-quit)
  (define-key more-mode-map "\177" 'scroll-down))

(defun more-buffer (buffer)
  "Run More on a buffer."
  (interactive "bbuffer to More: ")
  (switch-to-buffer buffer)
  (more-mode))

(defun more-file (file)
  "Run More on a file."
  (interactive "fFile to More: ")
  (find-file file)
  (more-mode))

(defun more-mode ()
  "Set current buffer to more mode.
The more-mode key bindings are:
\\{more-mode-map}"
  (interactive)
  (make-local-variable 'old-major-mode)
  (setq old-major-mode major-mode)
  (setq major-mode 'more-mode)
  (setq mode-name "More")
  (make-local-variable 'old-buffer-read-only)
  (setq old-buffer-read-only buffer-read-only)
  (setq buffer-read-only t)
  (use-local-map more-mode-map)
  (if more-mode-hook
      (run-hooks more-mode-hook)))

(defun more-quit ()
  "Exit more mode."
  (interactive)
  (if (eq major-mode 'more-mode)
      (progn
	(setq buffer-read-only old-buffer-read-only)
	(funcall old-major-mode)
	(set-buffer-modified-p (buffer-modified-p)))
    (error "This buffer is NOT in more mode!")))

(defun more-nop ()
  "Do nut'n. Just gripe."
  (interactive)
  (ding))

(defun dired-more-file ()
  "In dired, 'more' the file named on this line."
  (interactive)
  (if (save-excursion
	(beginning-of-line)
	(looking-at "  d"))
      (dired (dired-get-filename))
    (more-file (dired-get-filename))))
;Wolfgang Rupprecht	ARPA:  wolfgang@mgm.mit.edu (IP 18.82.0.114)
;TEL: (617) 267-4365	UUCP:  mit-eddie!mgm.mit.edu!wolfgang

