;;; diff-mode.el --- A mode for viewing/editing context diffs

;; Copyright (C) 1998-1999  Stefan Monnier <monnier@cs.yale.edu>

;; Author: Stefan Monnier <monnier@cs.yale.edu>
;; Keywords: patch diff
;; Version: v1_8
;; Revision: diff-mode.el,v 1.11 1999/10/09 23:38:29 monnier Exp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Provides support for font-lock patterns, outline-regexps, navigation
;; commands, editing and various conversions as well as jumping
;; to the corresponding source file.

;; History:

;; inspired by Pavel Machek's patch-mode.el (<pavel@atrey.karlin.mff.cuni.cz>)
;; some efforts were spent to have it somewhat compatible with XEmacs'
;; diff-mode as well as with compilation-minor-mode

;; to use it, simply add to your .emacs the following lines:
;; 
;; (autoload 'diff-mode "diff-mode" "Diff major mode" t)
;; (add-to-list 'auto-mode-alist '("\\.\\(diffs?\\|patch\\|rej\\)\\'" . diff-mode))

;; Bugs:

;; - reverse doesn't work with normal diffs.
;; - (nitpick) the mark is not always quite right in diff-goto-source.

;; Todo:

;; - improve narrowed-view support.
;; - improve diff-find-file-name.
;; - improve the `compile' support.

;;; Code:

(eval-when-compile (require 'cl))


(defgroup diff-mode ()
  "Major-mode for viewing/editing diffs"
  :group 'tools
  :group 'diff)

(defcustom diff-jump-to-old-file-flag nil
  "*Non-nil means `diff-goto-source' jumps to the old file.
Else, it jumps to the new file."
  :group 'diff-mode
  :type '(boolean))

(defcustom diff-update-on-the-fly-flag t
  "*Non-nil means hunk headers are kept up-to-date on-the-fly.
When editing a diff file, the line numbers in the hunk headers
need to be kept consistent with the actual diff.  This can
either be done on the fly (but this sometimes interacts poorly with the
undo mechanism) or whenever the file is written (can be slow
when editing big diffs)."
  :group 'diff-mode
  :type '(boolean))

(defvar diff-mode-hook nil
  "Run after setting up the `diff-mode' major mode.")

(defvar diff-outline-regexp
  "\\([*+][*+][*+] [^0-9]\\|@@ ...\\|\\*\\*\\* [0-9].\\|--- [0-9]..\\)")

;;;; 
;;;; keymap, menu, ...
;;;; 

(defmacro diff-defmap (var bindings doc)
  `(defvar ,var
     (let ((m (make-keymap)))
       (dolist (b ,bindings)
	 (define-key m (car b) (cdr b)))
       m)
     ,doc))

(diff-defmap diff-mode-shared-map
  '(;; from Pavel Machek's patch-mode
    ("n" . diff-next-hunk)
    ("N" . diff-next-file)
    ("p" . diff-prev-hunk)
    ("P" . diff-prev-file)
    ("k" . diff-kill-hunk)
    ("K" . diff-kill-file)
    ;; from compilation-minor-mode
    ("}" . diff-next-file)
    ("{" . diff-prev-file)
    ("\C-m" . diff-goto-source)
    ;; from XEmacs' diff-mode
    ("W" . widen)
    ;;("." . diff-goto-source)		;display-buffer
    ;;("f" . diff-goto-source)		;find-file
    ("o" . diff-goto-source)		;other-window
    ;;("w" . diff-goto-source)		;other-frame
    ;;("N" . diff-narrow)
    ;;("h" . diff-show-header)
    ;;("j" . diff-show-difference)	;jump to Nth diff
    ;;("q" . diff-quit)
    (" " . scroll-up)
    ("\177" . scroll-down)
    ;; our very own bindings
    ("A" . diff-ediff-patch)
    ("r" . diff-restrict-view)
    ("R" . diff-reverse-direction)
    ("U" . diff-context->unified)
    ("C" . diff-unified->context))
  "Keymap for read-only `diff-mode'. Only active in read-only mode.")

(diff-defmap diff-mode-map
  `(("\e" . ,diff-mode-shared-map)
    ;; from compilation-minor-mode
    ("\C-c\C-c" . diff-goto-source))
  "Keymap for `diff-mode'.  See also `diff-mode-shared-map'.")

(easy-menu-define diff-mode-menu diff-mode-map
  "Menu for `diff-mode'."
  '("Diff"
    ["Jump to Source"		diff-goto-source	t]
    ["Apply with Ediff"		diff-ediff-patch	t]
    ["-----" nil nil]
    ["Reverse direction"	diff-reverse-direction	t]
    ["Context -> Unified"	diff-context->unified	t]
    ["Unified -> Context"	diff-unified->context	t]
    ;;["Fixup Headers"		diff-fixup-modifs	(not buffer-read-only)]
    ))


;;;; 
;;;; font-lock support
;;;; 

(defface diff-file-header-face
  '((((class color) (background light))
     (:background "grey70" :bold t))
    (t (:bold t)))
  "diff-mode face used to highlight file header lines."
  :group 'diff-mode)
(defvar diff-file-header-face 'diff-file-header-face)

(defface diff-index-face
  '((((class color) (background light))
     (:background "grey70" :bold t))
    (t (:bold t)))
  "diff-mode face used to highlight index header lines."
  :group 'diff-mode)
(defvar diff-index-face 'diff-index-face)

(defface diff-hunk-header-face
  '((((class color) (background light))
     (:background "grey85"))
    (t (:bold t)))
  "diff-mode face used to highlight hunk header lines."
  :group 'diff-mode)
(defvar diff-hunk-header-face 'diff-hunk-header-face)

(defface diff-removed-face
  '((t ()))
  "diff-mode face used to highlight removed lines."
  :group 'diff-mode)
(defvar diff-removed-face 'diff-removed-face)

(defface diff-added-face
  '((t ()))
  "diff-mode face used to highlight added lines."
  :group 'diff-mode)
(defvar diff-added-face 'diff-added-face)

(defface diff-changed-face
  '((t ()))
  "diff-mode face used to highlight changed lines."
  :group 'diff-mode)
(defvar diff-changed-face 'diff-changed-face)

(defvar diff-font-lock-keywords
  '(("^@@ .+ @@$" . diff-hunk-header-face) ;unified
    ("^--- .+ ----$" . diff-hunk-header-face) ;context
    ("^\\*\\*\\*.+\\*\\*\\*\n" . diff-hunk-header-face) ;context
    ("^\\(---\\|\\+\\+\\+\\|\\*\\*\\*\\) .*\n" . diff-file-header-face)
    ("^[0-9,]+[acd][0-9,]+$" . diff-hunk-header-face)
    ("^!.*\n" . diff-changed-face)	;context
    ("^[+>].*\n" . diff-added-face)
    ("^[-<].*\n" . diff-removed-face)
    ("^Index: .*\n" . diff-index-face)
    ("^[^-=+*!<>].*\n" . font-lock-comment-face)))

(defconst diff-font-lock-defaults
  '(diff-font-lock-keywords t nil nil nil))

;;;;
;;;; Compile support
;;;;

(defvar diff-file-regexp-alist
  '(("Index: \\(.+\\)" 1)))

(defvar diff-error-regexp-alist
  '(("@@ -\\([0-9]+\\),[0-9]+ \\+\\([0-9]+\\),[0-9]+ @@" nil 2)
    ("--- \\([0-9]+\\),[0-9]+ ----" nil 1)
    ("\\([0-9]+\\)\\(,[0-9]+\\)?[adc]\\([0-9]+\\)" nil 3)))

;;;; 
;;;; Movement
;;;; 

(defconst diff-hunk-header-re "^\\(@@ .+ @@\\|\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\n\\*\\*\\* .+ \\*\\*\\*\\*\\|[0-9]+\\(,[0-9]+\\)?[acd][0-9]+\\(,[0-9]+\\)?\\)$")
(defconst diff-file-header-re (concat "^\\(--- .+\n\\+\\+\\+\\|\\*\\*\\* .+\n---\\|[^-+!<>0-9@* ]\\).+\n" (substring diff-hunk-header-re 1)))
(defvar diff-narrowed-to nil)

(defun diff-end-of-hunk (&optional style)
  (if (looking-at diff-hunk-header-re) (goto-char (match-end 0)))
  (re-search-forward (case style
		       (unified "^[^-+ \\]")
		       (context "^\\([^-+! \\][ \t]\\|--- .+ ----\\)")
		       (normal "^\\([<>\\][ \t]\\|---\\)")
		       (t "^[^-+!<> \\]"))
		     nil 'move)
  (beginning-of-line))

(defun diff-beginning-of-hunk ()
  (beginning-of-line)
  (unless (looking-at diff-hunk-header-re)
    (forward-line 1)
    (condition-case ()
	(re-search-backward diff-hunk-header-re)
      (error (error "Can't find the beginning of the hunk")))))

(defun diff-beginning-of-file ()
  (beginning-of-line)
  (unless (looking-at diff-file-header-re)
    (forward-line 2)
    (condition-case ()
	(re-search-backward diff-file-header-re)
      (error (error "Can't find the beginning of the file")))))

(defun diff-end-of-file ()
  (re-search-forward "^[-+!<>0-9@* \\]" nil t)
  (re-search-forward "^[^-+!<>0-9@* \\]" nil 'move)
  (beginning-of-line))

(defun diff-next-hunk (&optional count)
  "Move to next (COUNT'th) hunk."
  (interactive "p")
  (unless count (setq count 1))
  (if (< count 0) (diff-prev-hunk (- count))
    (when (looking-at diff-hunk-header-re) (incf count))
    (condition-case ()
	(re-search-forward diff-hunk-header-re nil nil count)
      (error (error "Can't find next hunk")))
    (goto-char (match-beginning 0))))

(defun diff-prev-hunk (&optional count)
  "Move to previous (COUNT'th) hunk."
  (interactive "p")
  (unless count (setq count 1))
  (if (< count 0) (diff-next-hunk (- count))
    (condition-case ()
	(re-search-backward diff-hunk-header-re nil nil count)
      (error (error "Can't find previous hunk")))))

(defun diff-next-file (&optional count)
  "Move to next (COUNT'th) file header."
  (interactive "p")
  (unless count (setq count 1))
  (if (< count 0) (diff-prev-file (- count))
    (when (looking-at diff-file-header-re) (incf count))
    (condition-case ()
	(re-search-forward diff-file-header-re nil nil count)
      (error (error "Can't find next file")))
    (goto-char (match-beginning 0))))

(defun diff-prev-file (&optional count)
  "Move to (COUNT'th) previous file header."
  (interactive "p")
  (unless count (setq count 1))
  (if (< count 0) (diff-next-file (- count))
    (condition-case ()
	(re-search-backward diff-file-header-re nil nil count)
      (error (error "Can't find previous file")))))

(defun diff-restrict-view (&optional arg)
  "Restrict the view to the current hunk.
If the prefix ARG is given, restrict the view to the current file instead."
  (interactive "P")
  (save-excursion
    (if arg (diff-beginning-of-file) (diff-beginning-of-hunk))
    (narrow-to-region (point)
		      (progn (if arg (diff-end-of-file) (diff-end-of-hunk))
			     (point)))
    (set (make-local-variable 'diff-narrowed-to) (if arg 'file 'hunk))))


(defun diff-kill-hunk ()
  "Kill current hunk."
  (interactive)
  (diff-beginning-of-hunk)
  (let ((start (point))
	(firsthunk (save-excursion
		     (ignore-errors
		       (diff-beginning-of-file) (diff-next-hunk) (point))))
	(nexthunk  (save-excursion
		     (ignore-errors
		       (diff-next-hunk) (point))))
	(nextfile (save-excursion
		    (ignore-errors
		      (diff-next-file) (point)))))
    (if (and firsthunk (= firsthunk start)
	     (or (null nexthunk)
		 (and nextfile (> nexthunk nextfile))))
	;; we're the only hunk for this file, so kill the file
	(diff-kill-file)
      (diff-end-of-hunk)
      (kill-region start (point)))))

(defun diff-kill-file ()
  "Kill current file's hunks."
  (interactive)
  (diff-beginning-of-file)
  (let* ((start (point))
	 (prevhunk (save-excursion
		     (ignore-errors
		       (diff-prev-hunk) (point))))
	 (index (save-excursion
		  (re-search-backward "^Index: " prevhunk t))))
    (when index (setq start index))
    (diff-end-of-file)
    (kill-region start (point))))

;;;;
;;;; jump to other buffers
;;;;

(defun diff-filename-drop-dir (file)
  (when (string-match "/" file) (substring file (match-end 0))))

(defun diff-find-file-name (&optional old)
  "Return the file corresponding to the current patch.
Non-nil OLD means that we want the old file."
  (save-excursion
    (unless (looking-at diff-file-header-re)
      (or (ignore-errors (diff-beginning-of-file))
	  (re-search-forward diff-file-header-re nil t)))
    (let* ((limit (save-excursion
		   (condition-case ()
		       (progn (diff-prev-hunk) (point))
		     (error (point-min)))))
	   (fs (append
		(when (looking-at "[-*][-*][-*] \\(\\S-+\\)\\s-.*\n[-+][-+][-+] \\(\\S-+\\)\\s-.*$")
		  (list (if old (match-string 1) (match-string 2))
			(if old (match-string 2) (match-string 1))))
		(progn (forward-line 1) nil)
		(when (save-excursion
			(re-search-backward "^Index: \\(.+\\)" limit t))
		  (list (match-string 1)))
		(when (re-search-backward "^diff \\(-\\S-+ +\\)*\\(\\S-+\\)\\( +\\(\\S-+\\)\\)?" nil t)
		  (list (if old (match-string 2) (match-string 4))
			(if old (match-string 4) (match-string 2))))))
	   (fs (delq nil fs))
	   (file
	    ;; look for each file in turn.  If none found, try again but
	    ;; ignoring the first level of directory, ...
	    (do* ((files fs (delq nil (mapcar 'diff-filename-drop-dir files)))
		  (file nil nil))
		((or (null files)
		     (setq file (do* ((files files (cdr files))
				      (file (car files) (car files)))
				    ((or (null file) (file-exists-p file))
				     file))))
		 file))))
      (or
       file
       (and (string-match "\\.rej\\'" (or buffer-file-name ""))
	    (let ((file (substring buffer-file-name 0 (match-beginning 0))))
	      (when (file-exists-p file) file)))
       ;; FIXME: use a more informative prompt
       (let ((file (read-file-name "File: " nil (first fs) nil (first fs))))
	 ;; FIXME: remember for the next invocation
	 file)))))

(defun diff-goto-source (&optional other-file)
  "Jump to the corresponding source line.
`diff-jump-to-old-file-flag' (or its opposite if the OTHER-FILE prefix arg
is give) determines whether to jump to the old or the new file.
If the prefix arg is bigger than 8 (for example with \\[universal-argument] \\[universal-argument])
  then `diff-jump-to-old-file-flag' is also set, for the next invokations."
  (interactive "P")
  (save-excursion
    (let ((old (if (not other-file) diff-jump-to-old-file-flag
		 (not diff-jump-to-old-file-flag))))
      (when (> (prefix-numeric-value other-file) 8)
	(setq diff-jump-to-old-file-flag old))
      (diff-beginning-of-hunk)
      (let* ((loc (if (not (looking-at "[-@*\n ]*\\([0-9,]+\\)\\([ acd+]+\\([0-9,]+\\)\\)?"))
		      (error "Can't find the hunk header")
		    (if old (match-string 1)
		      (if (match-end 3) (match-string 3)
			(unless (re-search-forward "^--- \\([0-9,]+\\)" nil t)
			  (error "Can't find the hunk separator"))
			(match-string 1)))))
	     (lines (if (string-match "^\\([0-9]*\\),\\([0-9]*\\)" loc)
			(cons (string-to-number (match-string 1 loc))
			      (string-to-number (match-string 2 loc)))
		      (cons (string-to-number loc) nil)))
	     (file (diff-find-file-name old)))
	(unless file (error "Can't find the file"))
	(pop-to-buffer (find-file-noselect file))
	(let* ((line (car lines))
	       (span (if (or (null (cdr lines)) (< (cdr lines) 0)) 0
		       (if (< (cdr lines) line) (cdr lines)
			 (- (cdr lines) line)))))
	  (ignore-errors
	    (goto-line line)
	    (forward-line span)
	    (push-mark (point) t t)
	    (goto-line line)))))))


(defun diff-ediff-patch ()
  "Call `ediff-patch-file' on the current buffer."
  (interactive)
  (condition-case err
      (ediff-patch-file (current-buffer))
    (wrong-number-of-arguments (ediff-patch-file))))

;;;; 
;;;; Conversion functions
;;;; 

;;(defvar diff-inhibit-after-change nil
;;  "Non-nil means inhibit `diff-mode's after-change functions.")

(defun diff-unified->context (start end)
  "Convert unified diffs to context diffs.
START and END are either taken from the region (if a prefix arg is given) or
else cover the whole bufer."
  (interactive (if current-prefix-arg
		   (list (mark) (point))
		 (list (point-min) (point-max))))
  (unless (markerp end) (setq end (copy-marker end)))
  (let (;;(diff-inhibit-after-change t)
	(inhibit-read-only t))
    (save-excursion
      (goto-char start)
      (while (and (re-search-forward "^\\(\\(---\\) .+\n\\(\\+\\+\\+\\) .+\\|@@ -\\([0-9]+\\),\\([0-9]+\\) \\+\\([0-9]+\\),\\([0-9]+\\) @@\\)$" nil t)
		  (< (point) end))
	(combine-after-change-calls
	  (if (match-beginning 2)
	      ;;we matched a file header
	      (progn
		;; use reverse order to make sure the indices are kept valid
		(replace-match "---" t t nil 3)
		(replace-match "***" t t nil 2))
	    ;; we matched a hunk header
	    (let ((line1 (match-string 4))
		  (lines1 (match-string 5))
		  (line2 (match-string 6))
		  (lines2 (match-string 7)))
	      (replace-match
	       (concat "***************\n*** " line1 ","
		       (number-to-string (+ (string-to-number line1)
					    (string-to-number lines1)
					    -1)) " ****"))
	      (forward-line 1)
	      (save-restriction
		(narrow-to-region (point)
				  (progn (diff-end-of-hunk 'unified) (point)))
		(let ((hunk (buffer-string)))
		  (goto-char (point-min))
		  (if (not (save-excursion (re-search-forward "^-" nil t)))
		      (delete-region (point) (point-max))
		    (goto-char (point-max))
		    (let ((modif nil) last-pt)
		      (while (progn (setq last-pt (point))
				    (= (forward-line -1) 0))
			(case (char-after)
			  (?  (insert " ") (setq modif nil) (backward-char 1))
			  (?+ (delete-region (point) last-pt) (setq modif t))
			  (?- (if (not modif)
				  (progn (forward-char 1)
					 (insert " "))
				(delete-char 1)
				(insert "! "))
			      (backward-char 2))
			  (?\\ (when (save-excursion (forward-line -1)
						     (= (char-after) ?+))
				 (delete-region (point) last-pt) (setq modif t)))
			  (t (setq modif nil))))))
		  (goto-char (point-max))
		  (save-excursion
		    (insert "--- " line2 ","
			    (number-to-string (+ (string-to-number line2)
						 (string-to-number lines2)
						 -1)) " ----\n" hunk))
		  ;;(goto-char (point-min))
		  (forward-line 1)
		  (if (not (save-excursion (re-search-forward "^+" nil t)))
		      (delete-region (point) (point-max))
		    (let ((modif nil) (delete nil))
		      (while (not (eobp))
			(case (char-after)
			  (?  (insert " ") (setq modif nil) (backward-char 1))
			  (?- (setq delete t) (setq modif t))
			  (?+ (if (not modif)
				  (progn (forward-char 1)
					 (insert " "))
				(delete-char 1)
				(insert "! "))
			      (backward-char 2))
			  (?\\ (when (save-excursion (forward-line 1)
						     (not (eobp)))
				 (setq delete t) (setq modif t)))
			  (t (setq modif nil)))
			(let ((last-pt (point)))
			  (forward-line 1)
			  (when delete
			    (delete-region last-pt (point))
			    (setq delete nil)))))))))))))))

(defun diff-context->unified (start end)
  "Convert context diffs to unified diffs.
START and END are either taken from the region (if a prefix arg is given) or
else cover the whole bufer."
  (interactive (if current-prefix-arg
		   (list (mark) (point))
		 (list (point-min) (point-max))))
  (unless (markerp end) (setq end (copy-marker end)))
  (let (;;(diff-inhibit-after-change t)
	(inhibit-read-only t))
    (save-excursion
      (goto-char start)
      (while (and (re-search-forward "^\\(\\(\\*\\*\\*\\) .+\n\\(---\\) .+\\|\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\n\\*\\*\\* \\([0-9]+\\),\\(-?[0-9]+\\) \\*\\*\\*\\*\\)$" nil t)
		  (< (point) end))
	(combine-after-change-calls
	  (if (match-beginning 2)
	      ;; we matched a file header
	      (progn
		;; use reverse order to make sure the indices are kept valid
		(replace-match "+++" t t nil 3)
		(replace-match "---" t t nil 2))
	    ;; we matched a hunk header
	    (let ((line1s (match-string 4))
		  (line1e (match-string 5))
		  (pt1 (match-beginning 0)))
	      (replace-match "")
	      (unless (re-search-forward
		       "^--- \\([0-9]+\\),\\(-?[0-9]+\\) ----$" nil t)
		(error "Can't find matching `--- n1,n2 ----' line"))
	      (let ((line2s (match-string 1))
		    (line2e (match-string 2))
		    (pt2 (progn
			   (delete-region (progn (beginning-of-line) (point))
					  (progn (forward-line 1) (point)))
			   (point-marker))))
		(goto-char pt1)
		(forward-line 1)
		(while (< (point) pt2)
		  (case (char-after)
		    ((?! ?-) (delete-char 2) (insert "-") (forward-line 1))
		    (?\ 		;merge with the other half of the chunk
		     (let* ((endline2
			     (save-excursion
			       (goto-char pt2) (forward-line 1) (point)))
			    (c (char-after pt2)))
		       (case c
			 ((?! ?+)
			  (insert "+"
				  (prog1 (buffer-substring (+ pt2 2) endline2)
				    (delete-region pt2 endline2))))
			 (?\ 		;FIXME: check consistency
			  (delete-region pt2 endline2)
			  (delete-char 1)
			  (forward-line 1))
			 (?\\ (forward-line 1))
			 (t (delete-char 1) (forward-line 1)))))
		    (t (forward-line 1))))
		(while (looking-at "[+! ] ")
		  (if (/= (char-after) ?!) (forward-char 1)
		    (delete-char 1) (insert "+"))
		  (delete-char 1) (forward-line 1))
		(save-excursion
		  (goto-char pt1)
		  (insert "@@ -" line1s ","
			  (number-to-string (- (string-to-number line1e)
					       (string-to-number line1s)
					       -1))
			  " +" line2s ","
			  (number-to-string (- (string-to-number line2e)
					       (string-to-number line2s)
					       -1)) " @@"))))))))))

(defun diff-reverse-direction (start end)
  "Reverse the direction of the diffs.
START and END are either taken from the region (if a prefix arg is given) or
else cover the whole bufer."
  (interactive (if current-prefix-arg
		   (list (mark) (point))
		 (list (point-min) (point-max))))
  (unless (markerp end) (setq end (copy-marker end)))
  (let (;;(diff-inhibit-after-change t)
	(inhibit-read-only t))
    (save-excursion
      (goto-char start)
      (while (and (re-search-forward "^\\(\\([-*][-*][-*] \\)\\(.+\\)\n\\([-+][-+][-+] \\)\\(.+\\)\\|\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\\*\n\\*\\*\\* \\(.+\\) \\*\\*\\*\\*\\|@@ -\\(.+\\) \\+\\(.+\\) @@\\)$" nil t)
		  (< (point) end))
	(combine-after-change-calls
	  (cond
	   ;; a file header
	   ((match-beginning 2) (replace-match "\\2\\5\n\\4\\3" nil))
	   ;; a context-diff hunk header
	   ((match-beginning 6)
	    (let ((pt-lines1 (match-beginning 6))
		  (lines1 (match-string 6)))
	      (replace-match "" nil nil nil 6)
	      (forward-line 1)
	      (let ((half1s (point)))
		(while (looking-at "[-! \\][ \t]")
		  (when (= (char-after) ?-) (delete-char 1) (insert "+"))
		  (forward-line 1))
		(let ((half1 (buffer-substring half1s (point))))
		  (delete-region half1s (point))
		  (unless (looking-at "^--- \\([0-9]+,-?[0-9]+\\) ----$")
		    (insert half1)
		    (error "Can't find matching `--- n1,n2 ----' line"))
		  (let ((str1 (match-string 1)))
		    (replace-match lines1 nil nil nil 1)
		    (forward-line 1)
		    (let ((half2s (point)))
		      (while (looking-at "[!+ \\][ \t]")
			(when (= (char-after) ?+) (delete-char 1) (insert "-"))
			(forward-line 1))
		      (let ((half2 (buffer-substring half2s (point))))
			(delete-region half2s (point))
			(insert half1)
			(goto-char half1s)
			(insert half2)))
		    (goto-char pt-lines1)
		    (insert str1))))))
	   ;; a unified-diff hunk header
	   ((match-beginning 7)
	    (replace-match "@@ -\\8 +\\7 @@" nil)
	    (forward-line 1)
	    (let ((c (char-after)) first last)
	      (while (case (setq c (char-after))
		       (?- (setq first (or first (point)))
			   (delete-char 1) (insert "+") t)
		       (?+ (setq last (or last (point)))
			   (delete-char 1) (insert "-") t)
		       (?\\ t)
		       (t (when (and first last (< first last))
			    (let ((str (buffer-substring first last)))
			      (save-excursion (delete-region first last))
			      (insert str)))
			  (setq first nil last nil)
			  (equal ?\  c)))
		(forward-line 1))))))))))

(defun diff-fixup-modifs (start end)
  "Fixup the hunk headers (in case the buffer was modified).
START and END are either taken from the region (if a prefix arg is given) or
else cover the whole bufer."
  (interactive (if current-prefix-arg
		   (list (mark) (point))
		 (list (point-min) (point-max))))
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char end) (diff-end-of-hunk)
      (let ((plus 0) (minus 0) (space 0) (bang 0))
	(while (and (= (forward-line -1) 0) (<= start (point)))
	  (if (not (looking-at "\\(@@ .+ @@\\|[-*][-*][-*] .+ [-*][-*][-*][-*]\\)$"))
	      (case (char-after)
		(?\  (incf space))
		(?+ (incf plus))
		(?- (incf minus))
		(?! (incf bang))
		(?\\ nil)
		(t  (setq space 0 plus 0 minus 0 bang 0)))
	    (cond
	     ((looking-at "@@ -[0-9]+,\\([0-9]*\\) \\+[0-9]+,\\([0-9]*\\) @@$")
	      (let* ((old1 (match-string 1))
		     (old2 (match-string 2))
		     (new1 (number-to-string (+ space minus)))
		     (new2 (number-to-string (+ space plus))))
		(unless (string= new2 old2) (replace-match new2 t t nil 2))
		(unless (string= new1 old1) (replace-match new1 t t nil 1))))
	     ((looking-at "--- \\([0-9]+\\),\\([0-9]*\\) ----$")
	      (when (> (+ space bang plus) 0)
		(let* ((old1 (match-string 1))
		       (old2 (match-string 2))
		       (new (number-to-string
			     (+ space bang plus -1 (string-to-number old1)))))
		  (unless (string= new old2) (replace-match new t t nil 2)))))
	     ((looking-at "\\*\\*\\* \\([0-9]+\\),\\(-?[0-9]*\\) \\*\\*\\*\\*$")
	      (when (> (+ space bang minus) 0)
		(let* ((old (match-string 1))
		       (new (format
			     (concat "%0" (number-to-string (length old)) "d")
			     (+ space bang minus -1 (string-to-number old)))))
		  (unless (string= new old) (replace-match new t t nil 2))))))
	    (setq space 0 plus 0 minus 0 bang 0)))))))

;;;; 
;;;; Hooks
;;;; 

(defun diff-write-contents-hooks ()
  "Fixup hunk headers if necessary."
  (if (buffer-modified-p) (diff-fixup-modifs (point-min) (point-max)))
  nil)

;; XEmacs doesn't seem to have this feature
(defvar undo-in-progress nil)

;; It turns out that making changes in the buffer from within an
;; *-change-function is asking for trouble, whereas making them
;; from a post-command-hook doesn't pose much problems
(defvar diff-unhandled-changes nil)
(defun diff-after-change-function (beg end len)
  "Remember to fixup the hunk header.
See `after-change-functions' for the meaning of BEG, END and LEN."
  (when (and (not undo-in-progress) (not inhibit-read-only))
    (if diff-unhandled-changes
	(setq diff-unhandled-changes
	      (cons (min beg (car diff-unhandled-changes))
		    (max beg (cdr diff-unhandled-changes))))
      (setq diff-unhandled-changes (cons beg end)))))

(defun diff-post-command-hook ()
  "Fixup hunk headers if necessary."
  (when (consp diff-unhandled-changes)
    (ignore-errors
      (save-excursion
	(goto-char (car diff-unhandled-changes)) (diff-beginning-of-hunk)
	(diff-fixup-modifs (point) (cdr diff-unhandled-changes))))
    (setq diff-unhandled-changes nil)))

;;;###autoload
(defadvice vc-backend-diff (around diff-mode-vc activate)
  ;; BEWARE!! when this is autoloaded, CL might not be available
  (condition-case ()
      (with-current-buffer "*vc-diff*" (setq buffer-read-only nil))
    (error nil))
  ad-do-it
  (condition-case ()
      (with-current-buffer "*vc-diff*"
	(if (memq major-mode '(fundamental-mode diff-mode)) (diff-mode)))
    (error nil)))


;;;; 
;;;; The main function
;;;; 

;;(autoload 'diff-mode "diff-mode" "Major mode for viewing context diffs." t)
;;;###autoload
(defun diff-mode ()
  "Major mode for viewing context diffs.
Supports unified and context diffs as well as (to a lesser extent) normal diffs.
When the buffer is read-only, the ESC prefix is not necessary.
This mode runs `diff-mode-hook'.
\\{diff-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'diff-mode)
  (setq mode-name "Diff")
  (use-local-map diff-mode-map)
  (set (make-local-variable 'font-lock-defaults) diff-font-lock-defaults)
  (set (make-local-variable 'outline-regexp) diff-outline-regexp)
  ;; compile support
  (set (make-local-variable 'compilation-file-regexp-alist)
       diff-file-regexp-alist)
  (set (make-local-variable 'compilation-error-regexp-alist)
       diff-error-regexp-alist)
  (when (string-match "\\.rej\\'" (or buffer-file-name ""))
    (set (make-local-variable 'compilation-current-file)
	 (substring buffer-file-name 0 (match-beginning 0))))
  (compilation-shell-minor-mode 1)
  ;; 
  (setq buffer-read-only t)
  (if (not diff-update-on-the-fly-flag)
      (add-hook 'write-contents-hooks 'diff-write-contents-hooks)
    (make-local-variable 'diff-unhandled-changes)
    (make-local-hook 'after-change-functions)
    (add-hook 'after-change-functions 'diff-after-change-function nil t)
    (make-local-hook 'post-command-hook)
    (add-hook 'post-command-hook 'diff-post-command-hook nil t))
  ;; Neat trick from Dave Love to add more bindings in read-only mode:
  (add-to-list (make-local-variable 'minor-mode-map-alist)
	       (cons 'buffer-read-only diff-mode-shared-map))
  ;;
  (run-hooks 'diff-mode-hook))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.\\(diffs?\\|patch\\|rej\\)\\'" . diff-mode))

;; provide the package
(provide 'diff-mode)

;;; Change Log:
;; diff-mode.el,v
;; Revision 1.11  1999/10/09 23:38:29  monnier
;; (diff-mode-load-hook): dropped.
;; (auto-mode-alist): also catch *.diffs.
;; (diff-find-file-name, diff-mode):  add smarts to find the right file
;;     for *.rej files (that lack any file name indication).
;;
;; Revision 1.10  1999/09/30 15:32:11  monnier
;; added support for "\ No newline at end of file".
;;
;; Revision 1.9  1999/09/15 00:01:13  monnier
;; - added basic `compile' support.
;; - have diff-kill-hunk call diff-kill-file if it's the only hunk.
;; - diff-kill-file now tries to kill the leading garbage as well.
;;
;; Revision 1.8  1999/09/13 21:10:09  monnier
;; - don't use CL in the autoloaded code
;; - accept diffs using -T
;;
;; Revision 1.7  1999/09/05 20:53:03  monnier
;; interface to ediff-patch
;;
;; Revision 1.6  1999/09/01 20:55:13  monnier
;; (ediff=patch-file):  add bindings to call ediff-patch.
;; (diff-find-file-name):  taken out of diff-goto-source.
;; (diff-unified->context, diff-context->unified, diff-reverse-direction,
;;  diff-fixup-modifs):  only use the region if a prefix arg is given.
;;
;; Revision 1.5  1999/08/31 19:18:52  monnier
;; (diff-beginning-of-file, diff-prev-file):  fixed wrong parenthesis.
;;
;; Revision 1.4  1999/08/31 13:01:44  monnier
;; use `combine-after-change-calls' to minimize the slowdown of font-lock.
;;

;;; diff-mode.el ends here
