;;; inform-mode.el --- Inform mode for GNU Emacs

;; Copyright (c) 1994 by Gareth Rees

;; Author: Gareth Rees <Gareth.Rees@cl.cam.ac.uk>
;; Version 0.2 (December 6th, 1994)
;; Keywords: languages

;; LCD Archive Entry:
;; inform-mode|Gareth Rees|gdr11@cl.cam.ac.uk|
;; Major mode for editing Inform programs|
;; 06-Dec-1994|Version 0.2|~/modes/inform-mode.el.Z|

;; inform-mode is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; inform-mode is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;;; Commentary:

;; Inform is a compiler for adventure games by Graham Nelson, available
;; by anonymous FTP from /ftp.gmd.de:/if-archive/infocom/compilers/inform

;; This file implements a major mode for editing Inform programs which
;; supports the formatting of comments and of quoted strings, dealing
;; correctly with the backslashes that Inform requires to appear before
;; newlines.  It does not at present understand Inform syntax or indent
;; program lines correctly.

;; Put this file somewhere on your load-path, and the following code
;; in your .emacs file:

;; (autoload 'inform-mode "inform-mode" "Inform editing mode." t)
;; (setq auto-mode-alist (cons '("\\.inf\\'".inform-mode) auto-mode-alist)))

(defconst inform-mode-version "version 0.2")

;;; Code:


(defvar inform-fill-prefix "[ \t]*"
  "*Regexp to match text at start of line that constitutes indentation
in a quoted piece of text in an Inform program. Used by the function
`inform-fill-paragraph' to determine the indentation for quoted strings.")


(defvar inform-comment-prefix "[ \t]*! "
  "*Regexp to match text at start of line that constitutes indentation
in a comment in an Inform program. Used by the function 
`inform-fill-paragraph' to determine the indentation for comments.")


(defvar inform-start-of-string 
  "[A-Za-z_][A-Za-z_,: ]* \""
  "*Regexp to match starts of statements (but excluding initial white
space described by `inform-fill-prefix') whose subsequent lines should 
be indented by an additional tab when auto-filling.")


(defvar inform-startup-message t
  "*Non-nil displays a startup message when Inform mode is first called.")


(defvar inform-tab-width 4
  "*Width of tab stops in Inform mode.")


(defvar inform-fill-column 75
  "*Width of program text in Inform mode.")


(defvar inform-mode-map nil
  "Keymap for Inform mode.")


(if inform-mode-map
    ()
  (setq inform-mode-map (make-sparse-keymap))
  (define-key inform-mode-map "\t" 'tab-to-tab-stop)
  (define-key inform-mode-map "\eq" 'inform-fill-paragraph))


(defvar inform-mode-abbrev-table nil
  "Abbrev table used while in Inform mode.")
(define-abbrev-table 'inform-mode-abbrev-table ())


(defun inform-mode ()
  "Major mode for editing Inform programs.

Turns on auto-fill mode so that long lines are split at word
boundaries and the portion after the split is given the same
indentation as the previous line.

KEY DEFINITIONS:

\\{inform-mode-map}
VARIABLES:

 inform-startup-message
    Set to nil to inhibit message first time Inform mode is
    used.
 inform-fill-prefix
    Regexp to match text at start of line that constitutes
    indentation in a quoted piece of text in an Inform
    program. Used by the function `inform-fill-paragraph' to
    determine the indentation for quoted strings.  (default
    \"[ \\t]+\")
 inform-start-of-string
    Regexp to match starts of statements (but excluding
    initial white space described by `inform-fill-prefix')
    whose subsequent lines should be indented by an
    additional tab when auto-filling.  (by default matches
    print, description, cant_go, etc followed by space and
    double-quote)
 inform-comment-prefix
    Regexp to match text at start of line that constitutes
    indentation in a comment in an Inform program. Used by
    the function `inform-fill-paragraph' to determine the
    indentation for comments.  (default \"[ \\t]*! \")
 inform-tab-width
    Width of tab stops in Inform mode.  (default 4)
 inform-fill-column
    Width of program text in Inform mode. (default 75)

FUNCTIONS:

 inform-fill-paragraph
    Fills paragraph containing point, using the start of the
    line containing point to decide whether paragraph is a
    comment (if the start of the line matches
    `inform-comment-prefix') or a quoted string (if the
    start of the line matches `inform-fill-prefix').  Deals
    appropriately with trailing backslashes in the latter
    case.  To fill a quoted string, point must be between
    the quotes.
 inform-indent-line
    Indent current line so that it has the same indentation
    as the previous line.

Turning on Inform mode calls the value of the variable
`inform-mode-hook' with no args, if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (if inform-startup-message
      (message "Emacs Inform mode %s by Gareth Rees." inform-mode-version))
  (setq inform-startup-message nil)
  (use-local-map inform-mode-map)
  (set-syntax-table text-mode-syntax-table)
  (setq local-abbrev-table inform-mode-abbrev-table)
  (setq mode-name "Inform")
  (setq major-mode 'inform-mode)
  (setq fill-column inform-fill-column)
  (setq tab-width inform-tab-width)
  (setq tab-stop-list (inform-tab-list tab-width tab-width fill-column))
  (auto-fill-mode 1)
  (setq indent-line-function 'inform-indent-line)
  (run-hooks 'inform-mode-hook))


(defun inform-tab-list (l m n)
  (if (> l n) nil (cons l (inform-tab-list (+ l m) m n))))


(defun inform-fill-paragraph ()
  "Fill quoted string or comment containing point.
Uses the start of the line containing point to decide
whether paragraph is a comment (if the start of the line
matches `inform-comment-prefix') or a quoted string (if the
start of the line matches `inform-fill-prefix').  Deals
appropriately with trailing backslashes in the latter case.
To fill a quoted string, point must be between the quotes."
  (interactive)
  (let ((before (point))
	(fill-prefix fill-prefix))
    (beginning-of-line)
    (cond 

     ;; fill comments like ordinary paragraphs
     ((looking-at inform-comment-prefix)
      (setq fill-prefix (buffer-substring (match-beginning 0) 
					  (match-end 0)))
      (goto-char before)
      (fill-paragraph nil))

     ;; look for indentation appropriate to strings
     ((looking-at inform-fill-prefix)
      (setq fill-prefix (buffer-substring (match-beginning 0) 
					  (match-end 0)))
      (goto-char before)
      (save-restriction

	;; find the start and end of the string containing the point
	(let ((end (progn (search-forward "\"") (point)))
	      (beg (progn (search-backward "\"" nil nil 2) 
			  (beginning-of-line) (point))))
	  (narrow-to-region beg end)

	  ;; remove trailing backslashes
	  (goto-char (point-min))
	  (while (re-search-forward " \\\\$" (point-max) t)
	    (replace-match "" t t))

	  ;; fill the paragraph (but leave enough room to put the 
	  ;; backslashes back when we're done)
	  (setq fill-column (- fill-column 2))
	  (fill-region (point-min) (point-max))
	  (setq fill-column (+ fill-column 2))

	  ;; fill-region has added a newline at the end
	  (goto-char (point-max))
	  (backward-delete-char 1)

	  ;; now put the backslashes back
	  (goto-char (point-min))
	  (while (and (not (eobp)) 
		      (re-search-forward "$" (- (point-max) 1) t))
	    (replace-match " \\" t t)
	    (forward-line 1)))))

     ;; failed to match either kind of indentation
     (t (message "Not an Inform paragraph.")))))


(defun inform-indent-line ()
  "Indent current line of Inform program.
This function just uses the indentation of the previous line
if that matches `inform-comment-prefix' or `inform-fill-prefix'."
  (interactive)
  (let ((before (point))
	(indent-prefix))

    ;; delete the old indentation
    (beginning-of-line)
    (or (looking-at inform-comment-prefix)
	(looking-at inform-fill-prefix)
	(looking-at ""))
    (delete-region (match-beginning 0) (match-end 0))

    ;; work out the indentation of the previous line, looking for print
    ;; followed by double quotes (if found, add an extra tab to the
    ;; indent)
    (forward-line -1)
    (if (looking-at (concat inform-fill-prefix inform-start-of-string))
	(progn (looking-at inform-fill-prefix)
	       (setq indent-prefix 
		     (concat (buffer-substring (match-beginning 0)
					       (match-end 0)) "\t")))
      (or (looking-at inform-comment-prefix)
	  (looking-at inform-fill-prefix)
	  (looking-at ""))
      (setq indent-prefix (buffer-substring (match-beginning 0) 
					    (match-end 0))))

    ;; insert it
    (forward-line 1)
    (insert indent-prefix)))


;;; inform-mode.el ends here
