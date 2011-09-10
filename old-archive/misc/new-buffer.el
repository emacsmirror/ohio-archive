;; @(#) new-buffer.el -- Better buffer-menu
;; @(#) $Keywords: buffer menu $
;; $KnownCompatibility: 19.28 $

;; Copyright (C) 1995 Michael Ernst
;; Author:       mernst@theory.lcs.mit.edu (Michael Ernst)
;; Maintainer:   mernst@theory.lcs.mit.edu (Michael Ernst)
;; Created:      May 15 1995
;;
;; To get information on this program use ident(1)
;; Look at the code with folding.el, tinyfold.el, tinybm.el

;; LCD Archive Entry:
;; new-buffer|Michael Ernst|mernst@theory.lcs.mit.edu|
;; Better buffer-menu|
;; 11-Aug-1995|1.1|~/misc/new-buffer.el.Z|


;;{{{ Install

;; Intallation:
;; ....................................................... &t-install ...
;; Put this file on your Emacs-Lisp load path, add following into your
;; ~/.emacs startup file
;;
;; (require 'new-buffer)
;;
;; (setq buffer-menu-replacement-alist
;;       '(
;; 	("/mnt/aapo/usr43/users/PM2/jaalto"	.	"~")
;; 	("/users/jaalto"	.	"~")
;; 	;; ;; `list-buffers' doesn't respect `directory-abbrev-alist'.
;; 	;; ("/usr/spool/ftp" . "~ftp")
;; 	))

;;}}}
;;{{{ Documentation

;; ..................................................... &t-commentary ...

;; From: mernst@theory.lcs.mit.edu (Michael Ernst)
;; Subject: Re: avoiding full paths in buffer-menu
;; Date: 15 May 95
;;
;; > Is there a way to make buffer-menu use '~' instead of the full
;; > path-name to my home directory?
;;
;; Below is something I call new-buffer-menu which does that and more.  To use
;; it, put
;;
;;   (define-key ctl-x-map "\C-b" 'new-buffer-menu)	; was list-buffers
;;
;; in your .emacs.  You might also want to set buffer-menu-replacement-alist,
;; which permits additional substitutions besides ~ for your home directory:

;;}}}
;;{{{ history

;; CHANGE HISTORY
;; ........................................................ &t-history ...
;; [jari] = jari.aalto@ntc.nokia.com
;;
;; Aug  11       1995    [jari]
;; - I packaged Michael's code and added one hook function
;;   that I needed for hiliting some filenames. [see example in tinymatch.el]
;; - I also looked if Michael had archived this file but I couldn't
;;   find it, so I him and asked to send it to ohio archive, where
;;   everyone could enjoy this great code. Isn't the gnu.emacs.help
;;   group facinating when you can pick code like this :-)
;; - Thanks again Michael!
;; - In case you can't reach Michael, I can offer support for this package too.
;;
;; - Wan't colors to this package ? There is ready example in 
;;   my package, located in ohio: ~/misc/tinymatch.el.Z

;;}}}

;;{{{ setup

;;; ....................................................... &configure ...

(defvar new-buffer-load-hook nil
  "*hook run when file is loaded.")


(defvar buffer-menu-replacement-alist nil
  "*Variable to do substitutions:
\( \(STR . REPLACE_WITH\) .. \)")


;; This is nice place for hilighting
(defvar buffer-menu-hook nil
  "*Run after buffer content is ready.")

(defvar buffer-menu-replacement-alist nil
  "Association list of directories and their abbreviations
for the buffer-menu.")

;; Made this a separate variable to avoid the mapconcat per invocation.
(defvar Buffer-menu-kill-regexp
  (mapconcat
   (function regexp-quote)
   '("*Buffer List*"
     ;; I think I don't need these
     ;; "*ispell*"
     ;; "*ispell choices*"
     )
   "\\|"))


(define-key Buffer-menu-mode-map "V" 'Buffer-menu-plusify-dwim)
(define-key Buffer-menu-mode-map "c" 'Buffer-menu-bdiff)
(define-key Buffer-menu-mode-map "R" 'Buffer-menu-revert)

;;}}}


;;; ########################################################### &Funcs ###


;;; ----------------------------------------------------------------------
;;; Buffer-menu
;;;
(defun set-some-buffers-unmodified (name-regexp major-modes)
  "Clears the modification flag of buffers whose names match NAME-REGEXP
or whose major mode is a member of MAJOR-MODES.  Either or both of the
arguments may be nil.
Also sets dired buffer modification flags according to `dired-pending-marks-p',
if that function is defined."
  (let ((blist (buffer-list)))
    (while blist
      (save-excursion
	(set-buffer (car blist))
	(setq blist (cdr blist))
	;; Don't do the work unless the buffer is marked modified.
	(if (buffer-modified-p)
	    (progn
	      (if (or (and name-regexp (string-match name-regexp
						     (buffer-name)))
		      (memq major-mode major-modes))
		  (set-buffer-modified-p nil))
	      ;; This special-casing is sort of cheating.  But hey, it works.
	      ;; It's OK for this to be in the progn because adding a mark
	      ;; will always flag the buffer modified; that is, even though
	      ;; this code only changes the modification flag from t to
	      ;; nil, it does the right thing.
	      (if (and (eq major-mode 'dired-mode)
		       (fboundp 'dired-pending-marks-p))
		  (set-buffer-modified-p (dired-pending-marks-p)))))))))


;;; ----------------------------------------------------------------------
;;;
(defun new-buffer-menu (arg)
  "Make a menu of buffers so you can save, delete or select them.
User's home directory will be replaced with a ~ in the file listing.
With argument, show only buffers that are visiting files.
Type ? after invocation to get help on commands available.
Type q immediately to make the buffer menu go away."
  (interactive "P")

  ;; Are there any cases when I wouldn't want a * buffer to be set unmodified?
  ;; Yes, the *mail* buffer.  I should deal with that.
  (set-some-buffers-unmodified
   "^\\*[^m]\\|^\\*.[^a]\\|^RMAIL-summary"
   ;; (mapconcat (function regexp-quote)
   ;;	     '("*Messages*" "*compilation*"
   ;; 	       "*Shell Command Output*")
   ;; 	     "\\|")
   nil
   ;; '(ange-ftp-shell-mode
   ;;   webster-mode)
   )
  (list-buffers arg)
  (pop-to-buffer "*Buffer List*")

  (let ((buffer-read-only nil))
    ;; Original idea from Edward Nieters <nieters@crd.ge.com>.
    ;; In Emacs 19, I should use directory-abbrev-alist here.  (No, that's
    ;; already done for us.)
    (let ((repl-alist (cons (cons (getenv "HOME") "~")
			    buffer-menu-replacement-alist))
	  from to)
      (while repl-alist
	(setq from (concat "\\([\t ]\\)" (car (car repl-alist)))
	      to (concat "\\1" (cdr (car repl-alist)))
	      repl-alist (cdr repl-alist))
	(goto-char (point-min))
	(while (re-search-forward from nil t)
	  (replace-match to))))

    ;; Delete "*" modified mark from the "*Buffer List*" buffer.
    ;; Set-some-buffers-unmodified does no good since (list-buffers) modifies
    ;; this buffer.
    ;;   (goto-char (point-min))
    ;;   (replace-string "*  *Buffer List*" "   *Buffer List*")
    ;; On second thought, don't show the "*Buffer List*" buffer at all.
    (Buffer-menu-kill-some-lines Buffer-menu-kill-regexp))

  (goto-char (point-min))
  (forward-line 2)

  (run-hooks 'buffer-menu-hook)

  (set-buffer-modified-p nil)
  (message
   "Commands: d, s, x; 1, 2, m, u, q; delete; ~;  ? for help.")
  )


;;; ----------------------------------------------------------------------
;;;
(defun Buffer-menu-kill-some-lines (regexp)
  "Delete lines in the buffer menu whose buffer name matches REGEXP."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward (concat "^..  \\(" regexp "\\)") nil t)
      (progn
	(beginning-of-line)
	;; was (delete-region (point)
	;;      (save-excursion (end-of-line) (1+ (point))))
	;; This works even if the buffer entry appears at end of list
	(delete-region (point) (progn (end-of-line) (point)))
	(delete-backward-char 1)))
    ))


(provide 'new-buffer)
(run-hooks 'new-buffer-load-hook)
;;; ................... end of file ......................................
