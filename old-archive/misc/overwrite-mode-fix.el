;; Make backward deletion work properly in overwrite mode.
;; Copyright (C) 1991, 1992 Free Software Foundation, Inc.
;; Copyright (C) 1992 Joe Wells
;;
;; Parts of this are directly derived from part of GNU Emacs.  Thus, the
;; GNU Emacs copying conditions are included.  My portion is distributed
;; under the same copying conditions as GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; Created by: Joe Wells, jbw@maverick.uswest.com
;; Created on: Sun Jul 28 19:04:29 1991
;; Last modified by: Joe Wells, jbw@csd.bu.edu
;; Last modified on: Wed Oct  7 13:03:16 1992
;; Filename: overwrite-mode-fix.el
;; Purpose: make backward deletion work properly in overwrite mode

;; LCD Archive Entry:
;; overwrite-mode-fix|Joe Wells|jbw@cs.bu.edu|
;; Make backward deletion work properly in overwrite mode.|
;; 1992-10-07||~/misc/overwrite-mode-fix.el.Z|

;;(global-set-key "\M-o" 'overwrite-mode) ; interferes with terminal fn keys

(or (fboundp 'original-backward-delete-char-untabify)
    (fset 'original-backward-delete-char-untabify
	  (symbol-function 'backward-delete-char-untabify)))

(defun backward-delete-char-untabify (arg &optional killp)
  "Delete (or erase) characters backward, changing tabs into spaces.
Delete ARG chars, and kill (save in kill ring) if KILLP is non-nil.
Interactively, ARG is the prefix arg (default 1) and KILLP is t if prefix
arg is was specified.  If overwrite-mode is non-nil, erases characters
instead of deleting."
  (interactive "*p\nP")
  (if overwrite-mode
      (backward-erase-char-untabify arg killp)
    (original-backward-delete-char-untabify arg killp)))

(or (fboundp 'original-backward-delete-char)
    (fset 'original-backward-delete-char
	  (symbol-function 'backward-delete-char)))
;; Equivalent to:
;; (fset 'original-backward-delete-char 'delete-backward-char)

(or (fboundp 'original-delete-backward-char)
    (fset 'original-delete-backward-char
	  (symbol-function 'delete-backward-char)))

(defun delete-backward-char (arg &optional killp)
  "Delete (or erase) the previous ARG characters (following, with neg. ARG).
Optional second arg KILLFLAG non-nil means kill instead (save in kill
ring).  Interactively, ARG is the prefix arg, and KILLFLAG is set if ARG
was explicitly specified.  If overwrite-mode is non-nil, erases characters
instead of deleting."
  (interactive "p\nP")
  (if overwrite-mode
      (backward-erase-char arg killp)
    (original-delete-backward-char arg killp)))

(defun backward-erase-char (arg &optional killp)
  "Erase the previous ARG characters (following, with neg. ARG).
Optional second arg KILLFLAG non-nil means kill instead (save in kill
ring).  Interactively, ARG is the prefix arg, and KILLFLAG is set if ARG
was explicitly specified."
  (interactive "*p\nP")
  (let* ((del-fun (if killp 'kill-region 'delete-region))
	 (last-command last-command)
	 opoint diff)
    (while (> arg 0)
      (cond ((memq (preceding-char) '(?\n ?\t ? ))
	     (backward-char 1)
	     (if killp
		 (copy-region-as-kill (1+ (point)) (point)))
	     (setq arg (1- arg)))
	    (t
	     (setq opoint (point))
	     (skip-chars-backward "^ \t\n")
	     (if (> (- opoint (point)) arg)
		 (goto-char (- opoint arg)))
	     (setq diff (- opoint (point))
		   arg (- arg diff))
	     (funcall del-fun opoint (point))
	     (or (eolp)
		 (save-excursion
		   (insert-char ?  diff)))))
      (setq last-command 'kill-region))))

(defun backward-erase-char-untabify (arg &optional killp)
  "Erase characters backward, changing tabs into spaces.
Erase ARG chars, and kill (save in kill ring) if KILLP is non-nil.
Interactively, ARG is the prefix arg (default 1) and KILLP is t if prefix
arg is was specified."
  (interactive "*p\nP")
  (let* ((del-fun (if killp 'kill-region 'delete-region))
	 (last-command last-command)
	 opoint original-col target-col)
    (while (> arg 0)
      (cond ((bolp)
	     (backward-char 1)
	     (if killp
		 (copy-region-as-kill (1+ (point)) (point)))
	     (setq arg (1- arg)))
	    (t
	     (setq opoint (point)
		   original-col (current-column)
		   target-col (max 0 (- original-col arg))
		   arg (- arg (- original-col target-col)))
	     (move-to-column-force target-col)
	     (funcall del-fun opoint (point))
	     (or (eolp)
		 (save-excursion
		   (indent-to (max target-col original-col))))))
      (setq last-command 'kill-region))))

(autoload 'move-to-column-force "picture")

(provide 'overwrite-mode-fix)
