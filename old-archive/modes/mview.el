;;; mview.el --- Minor view mode

;; Copyright (C) Mike Williams 1993

;; Author: Mike Williams <mikew@gopher.dosli.govt.nz>
;; Created: 3-November-93
;; Version: $Revision: 1.3 $

;; LCD Archive Entry:
;; mview|Mike Williams|mikew@gopher.dosli.govt.nz|
;; Minor view mode|
;; 03-Nov-1993|1.3|~/modes/mview.el.Z|

;; This file is not (yet) part of GNU Emacs, but is made available under
;; the same conditions.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary ============================================================
;;
;; The default emacs view-mode uses recursive edit to preserve major mode
;; and keybindings while viewing a file.  Recursive edit is (IMHO) a pain
;; in the posterior.  This view-mode replacement uses a temporary local
;; keymap, avoiding the need for recursive edits.
;;
;; This is an adaption of more-mode.el, by Wolfgang Rupprecht.  The main
;; difference is that mview-mode is a minor mode, not a major one.

;;--- Installation --------------------------------------------------------

;; (autoload 'mview-file   "mview" nil t)
;; (autoload 'mview-buffer "mview" nil t)
;; (autoload 'mview-mode   "mview" nil t)

;; To replace view-mode by mview-mode:
;; (fset 'view-file   'mview-file)
;; (fset 'view-buffer 'mview-buffer)

;;; Code ==================================================================

(provide 'mview)

;;=== Variables ===========================================================

(defvar mview-mode nil "Non-nil if mview-mode is enabled.")
(make-variable-buffer-local 'mview-mode)

(or (assq 'mview-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(mview-mode " MView") minor-mode-alist)))

(defvar mview-mode-hook nil "Functions to call when entering mview-mode. ")

(defvar mview-auto-kill-buffer nil 
  "If non-nil, don't kill the buffer when quitting mview-mode.")
(make-variable-buffer-local 'mview-auto-kill-buffer)

(defvar mview-buffer-read-only nil 
  "Saved copy of buffer-read-only.")
(make-variable-buffer-local 'mview-buffer-read-only)

(defvar mview-old-local-map nil 
  "Saved copy of local keymap.")
(make-variable-buffer-local 'mview-old-local-map)

;;=== Keymap ==============================================================

(defvar mview-mode-map nil "Keymap for mview-mode.")

(if mview-mode-map
    nil
  (setq mview-mode-map (make-keymap))
  (suppress-keymap mview-mode-map)
  ;; movement
  (define-key mview-mode-map 	"<" 	'beginning-of-buffer) 
  (define-key mview-mode-map 	">" 	'end-of-buffer) 
  (define-key mview-mode-map 	"n" 	'next-line)
  (define-key mview-mode-map 	"p" 	'previous-line)
  (define-key mview-mode-map 	" " 	'scroll-up)
  (define-key mview-mode-map 	"\C-d" 	'scroll-up)
  (define-key mview-mode-map 	"\177" 	'scroll-down) 
  (define-key mview-mode-map 	"b" 	'scroll-down)
  (define-key mview-mode-map 	"\r" 	'mview-scroll-up-line) 
  (define-key mview-mode-map 	"k" 	'mview-scroll-down-line) 
  (define-key mview-mode-map 	"g" 	'goto-line)
  (define-key mview-mode-map 	"=" 	'what-line)
  ;; prefix arguments
  (define-key mview-mode-map 	"-" 	'negative-argument)
  (define-key mview-mode-map 	"0"  	'digit-argument)
  (define-key mview-mode-map 	"1"  	'digit-argument)
  (define-key mview-mode-map 	"2" 	'digit-argument)
  (define-key mview-mode-map 	"3" 	'digit-argument)
  (define-key mview-mode-map 	"4" 	'digit-argument)
  (define-key mview-mode-map 	"5" 	'digit-argument)
  (define-key mview-mode-map 	"6" 	'digit-argument)
  (define-key mview-mode-map 	"7" 	'digit-argument)
  (define-key mview-mode-map 	"8" 	'digit-argument)
  (define-key mview-mode-map 	"9" 	'digit-argument)
  ;; searching
  (define-key mview-mode-map 	"s" 	'isearch-forward)
  (define-key mview-mode-map 	"/" 	'isearch-forward)
  (define-key mview-mode-map 	"\\" 	'isearch-backward)
  ;; mark etc.
  (define-key mview-mode-map 	"." 	'set-mark-command)
  (define-key mview-mode-map 	"x" 	'exchange-point-and-mark)
  (define-key mview-mode-map 	"'" 	'mview-pop-mark)
  (define-key mview-mode-map 	"@" 	'mview-pop-mark)
  ;; exiting
  (define-key mview-mode-map 	"z" 	'bury-buffer)
  (define-key mview-mode-map 	"q" 	'mview-quit)
  (define-key mview-mode-map 	"Q" 	'mview-quit-retain)
  (define-key mview-mode-map 	"r" 	'mview-quit-retain)
  )

;;=== Functions ===========================================================

;;;###autoload
(defun mview-mode (&optional ARG AUTO-KILL)
  "Minor mode for viewing text without editing it.
Letters do not insert themselves.  Instead these commands are provided.
Most commands take prefix arguments.  Commands dealing with lines
default to \"scroll size\" lines (initially size of window).
Search commands default to a repeat count of one.
Space, C-d	scroll forward pages.
DEL, b		scroll backward pages.
CR		scroll forward lines.
k		scroll backward lines.
n		move forward lines.
p		move backward lines.
<		move to beginning of buffer.
>		move to end of buffer.
C-u and Digits	provide prefix arguments.  `-' denotes negative argument.
=		prints the current line number.
g		goto line.
s, /		incremental search forward.
\\		incremental search backward.
C-@ or .	set the mark.
x		exchanges point and mark.
@ or '		return to mark and pops mark ring.
		  Mark ring is pushed at start of every
		  successful search and when jump to line to occurs.
		  The mark is set on jump to buffer start or end.
z		bury the buffer
q		exit mview-mode
Q, r		exit mview-mode, but do not kill the buffer

Entry to this mode runs the normal hook `mview-mode-hook'.

Set current buffer to mview mode.
Optional argument AUTO-KILL means auto-kill this buffer after quitting 
mview-mode.
The mview-mode key bindings are:
\\{mview-mode-map}"
  (interactive "P") 
  (setq mview-mode
	(if (null ARG)
	    (not mview-mode)
	  (> (prefix-numeric-value ARG) 0)))
  (cond 
   (mview-mode
    (setq mview-auto-kill-buffer AUTO-KILL
	  mview-old-local-map (current-local-map)
	  mview-buffer-read-only buffer-read-only
	  buffer-read-only t)
    (use-local-map mview-mode-map)
    (run-hooks 'mview-mode-hook))
   (t
    (setq buffer-read-only mview-buffer-read-only)
    (use-local-map mview-old-local-map)))
  (force-mode-line-update))

;; force-mode-line-update is not standard in emacs 18
(or (fboundp 'force-mode-line-update)
    (defun force-mode-line-update ()
      (set-buffer-modified-p (buffer-modified-p))))

;;;###autoload
(defun mview-buffer (buffer)
  "View a buffer."
  (interactive "bMView buffer: ")
  (switch-to-buffer buffer)
  (mview-mode 1))

;;;###autoload
(defun mview-file (file)
  "View a file."
  (interactive "fMView file: ")
  (let ((retain (get-file-buffer file)))
    (find-file file)
    (if (eq (get major-mode 'mode-class) 'special)
	nil				; do nuthin if special buffer
      (mview-mode 1 (not retain)))))

(defun mview-pop-mark ()
  "Return to previous mark."
  (interactive)
  (let ((mark-active t))
    (goto-char (or (mark) (point-min)))
    (pop-mark)))

(defun mview-scroll-up-line (ARG)
  (interactive "p")
  (scroll-up ARG))
(defun mview-scroll-down-line (ARG)
  (interactive "p")
  (scroll-down ARG))
  
(defun mview-quit (&optional RETAIN)
  "Exit mview-mode.  
The buffer will be killed if mview-auto-kill-buffer is non-nil, unless
an optional RETAIN argument is given."
  (interactive "P")
  (mview-mode -1)
  (or (not mview-auto-kill-buffer) RETAIN
      (kill-buffer (current-buffer))))

(defun mview-quit-all ()
  "Quit all buffers that are in mview-mode."
  (interactive)
  (let ((buffers (buffer-list)))
    (while buffers
      (save-excursion 
	(set-buffer (car buffers))
	(if mview-mode (mview-quit)))
      (setq buffers (cdr buffers)))))

(defun mview-quit-retain ()
  "Return buffer to it's original mode."
  (interactive)
  (mview-quit 'retain))

;; mview.el ends here
