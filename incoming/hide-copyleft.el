;;; hide-copyleft.el --- hide obnoxious copyright prologs

;; Copyright (C) 1997 Sun Microsystems.

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc. 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:

;; Written by Jamie Zawinski <jwz@netscape.com>, 19-jan-91.
;; Minor fixes by Martin Buchholz, 14-may-97.
;; Licenses added, minor mode by Reini Urban, 2000-01-14 and 2000-02-26.
;;
;; I sometimes find it tiresome to have fifteen lines of copyright notice at
;; the beginning of each file.  Meta-< does not take you to the beginning of
;; the code, it takes you a windowfull or two away, which can be tedious on
;; slow terminal lines.
;;
;; I know what the copyright notice says; so this code makes all but the first
;; line of it be invisible, by using Emacs's selective-display feature.  The
;; text is still present and unmodified, but it is invisible.
;;
;; Elide the copyright notice with "Meta-X hide-copyleft-region".  Make it
;; visible again with "Control-U Meta-X hide-copyleft-region".  Or, if you're
;; sure you're not gonna get sued, you can do something like this in your
;; .emacs file:
;;
;; (autoload 'hide-copyleft-region   "hide-copyleft" nil t)
;; (autoload 'unhide-copyleft-region "hide-copyleft" nil t)
;; (mapcar '(lambda (hook) (add-hook hook 'hide-copyleft-region))
;;	   '(emacs-lisp-mode-hook lisp-mode-hook
;;	     cperl-mode-hook perl-mode-hook c-mode-hook
;;           autolisp-mode-hook))
;; (add-hook 'emacs-lisp-mode-hook 'hide-copyleft-region)
;; (add-hook 'c-mode-hook 'hide-copyleft-region)
;; (global-set-key "\C-ch" 'hide-copyleft-mode)
;;
;; This code (obviously) has quite specific knowledge of the wording of the 
;; various copyrights I've run across.  Let me know if you find one on which
;; it fails.

;; Todo: 
;;   go the into the © mode, say show © in the statusline, only iff a 
;;   message was hidden.

(defgroup hide-copyleft nil
  "Hide copyright prologs."
  :group 'matching)

(defvar hide-copyleft-mode nil)

(defcustom copylefts-to-hide
  ;; There are some extra backslashes in these strings to prevent this code
  ;; from matching the definition of this list as the copyright notice!
  '(;; GNU
    ("free software\; you can redistribute it" .
     "notice must be\ preserved on all")
    ("free software\; you can redistribute it" .
     "copy of the GNU General Public License.*\n?.*\n?.*\n?.*\n?.*\\(02139,\\|02111-1307\\)")
    ("distributed in the hope that it will be useful\," .
     "notice must be\ preserved on all")
    ("free software\; you can redistribute it" .
     "General Public License for more details\\.")
    ;; X11
    ("Permission to use\, copy, modify," .
     "WITH THE USE OR PERFORMANCE")
    ("Permission to use\, copy, modify," .
     "without express or implied warranty")
    ;; Motif
    ("Copyright.*OPEN\ SOFTWARE FOUNDATION" .
     "X Window System is a trademark of the")
    ("THIS SOFTWARE\ IS FURNISHED UNDER A LICENSE" .
     "X Window System is a trademark of the")
    ;; UPenn
    ("Permission to use\, copy, and distribute" .
     " provided \"as is\" without")
    ;; Evans & Sutherland, Solbourne.
    ("Copyright 19[0-9][0-9] by " .
     "OR PERFORMANCE OF THIS SOFTWARE\\.")
    ;; TI Explorer
    ("RESTRICTED RIGHTS LEGEND" . "All rights reserved\\.\\(\n;;; ?$\\)?")
;;    ("^%%BeginDocumentation" . "^%%EndDocumentation")
    ;; Berkeley
    ("Redistribution and use in source and binary forms\," . 
     "without specific prior written permission")
    (" THIS SOFTWARE IS PROVIDED" . " SUCH DAMAGE")
    ("Permission to use\, copy\, modify and distribute " . 
     "\"as is\" without express or implied warranty")
    ("Copyright \\(©\\|(c)\\) [1-2][089][0-9][0-9] by " . 
     "\"as is\" without express\ or implied warranty")
    ("Copyright [1-2][089][0-9][0-9] " . 
     " WARRANTIES OF NONINFRINGEMENT")
   )
  "An alist of pairs of regexps which delimit copyright notices to hide.
The first one found is hidden, so order is significant."
  :group 'hide-copyleft
  :type '(repeat (cons (regexp :tag "Top line")
		       (regexp :tag "Bottom line"))))

;;;###autoload
(defun hide-copyleft-mode (&optional arg)
  "Toggle Hide Copyleft minor mode.
With numerical arg, turn Hide Copyleft minor mode on iff arg is positive.
Toggles between \\[hide-copyleft-region] and  \\[unhide-copyleft-region].

Keybinding:  \\[hide-copyleft-mode]."
  (interactive "P")
  (setq hide-copyleft-mode 
	(or (and arg (> (prefix-numeric-value arg) 0))
	    (and (null arg) (null hide-copyleft-mode))))
  (if (null hide-copyleft-mode)
      (unhide-copyleft-region)
    (hide-copyleft-region)))

;; For init-file hooks
;;;###autoload
(defun hide-copyleft-region (&optional arg)
  "Make the legal drivel at the front of this file invisible.  Unhide it again
with C-u \\[hide-copyleft-region]."
  (interactive "P")
  (if arg
      (unhide-copyleft-region)
    (save-excursion
     (save-restriction
      (if selective-display (error "selective-display is already on."))
      (catch 'Abort
	(let ((mod-p (buffer-modified-p))
	      (buffer-read-only nil)
	      (rest copylefts-to-hide)
	      pair start end)
	  (widen)
	  (goto-char (point-min))
	  (while (and rest (not pair))
	    (save-excursion
	      (and (re-search-forward (car (car rest)) nil t)
		   (setq start (point))
		   (re-search-forward (cdr (car rest)) nil t)
		   (setq end (point)
			 pair (car rest))))
	    (setq rest (cdr rest)))
	  (setq x pair)
	  (or pair
	      (if (interactive-p)
		  (error "Couldn't find a CopyLeft to hide.")
		(setq hide-copyleft-mode nil)
		;; Todo: toggle check status-bar character
		;; uncheck minor-mode hide-copyleft-mode
		(throw 'Abort nil)))
	  (goto-char end)
	  (forward-line 1)
	  ;; If the last line of the notice closes a C comment, don't
	  ;; hide that line (to avoid confusion...)
	  (if (save-excursion (forward-char -3) (looking-at "\\*/"))
	      (forward-line -1))
	  (setq end (point))
	  (goto-char start)
	  (forward-line 1)
	  (while (< (point) end)
	    (delete-char -1)
	    (insert "\^M")
	    (forward-line 1))
	  (setq selective-display t)
	  (setq hide-copyleft-mode t)
	  (set-buffer-modified-p mod-p)))))))

;;;###autoload
(defun unhide-copyleft-region ()
  "If the legal nonsense at the top of this file is elided, make it visible again."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((mod-p (buffer-modified-p))
	    (buffer-read-only nil)
	    end)
	(or (search-forward "\^M" nil t) (error "No CopyLeft hidden here."))
	(end-of-line)
	(setq end (point))
	(beginning-of-line)
	(while (search-forward "\^M" end t)
	  (delete-char -1)
	  (insert "\^J"))
	(set-buffer-modified-p mod-p)
	(setq hide-copyleft-mode nil)
	(setq selective-display nil)))))

(eval-when '(load eval)
  (if (string-match "XEmacs\\|Lucid" emacs-version)
    ;; when can this char "©" be used?
    (add-minor-mode 'hide-copyleft-mode "©")
    ;; or this: (add-minor-mode 'hide-copyleft-mode "(c)")
  )
)

(provide 'hide-copyleft)

;;; hide-copyleft.el ends here
