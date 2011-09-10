;; insert-random-quote.el --- Insert a random quote from a file.
;; Copyright (C) 1992 Joe Wells

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; Created by: Joe Wells, jbw@bigbird.bu.edu
;; Created on: Sat Oct  3 23:26:08 1992
;; Last modified by: Joe Wells, jbw@csd.bu.edu
;; Last modified on: Thu Mar  4 17:22:52 1993
;; Filename: insert-random-quote.el
;; Purpose: insert a random quote from a file
;; Change log: 
;; 
;; Sun Feb 21 01:26:00 1993  Joe Wells  (jbw at csd.bu.edu)
;; 
;; 	* Added quoting and changing preexisting double quotes to single
;; 	quotes.
;; 

;; LCD Archive Entry:
;; insert-random-quote|Joe Wells|jbw@cs.bu.edu|
;; Insert a random quote from a file.|
;; February 21, 1993||functions/insert-random-quote.el.Z|

(defvar random-quote-file (expand-file-name "~/.quotes")
  "*Pathname of a file containing quotes terminated by \"\\n%\\n\".
Any text past the last terminator is ignored.")

(defvar random-quote-regexp nil
  "A cache of the last regexp specified to insert-random-quote.
It does no good to set this variable.")

(defun insert-random-quote ()
  "Inserts a randomly selected quote from random-quote-file.
If invoked twice in a row by keystroke, deletes the previously selected
random quote before inserting the new one.  Indents the inserted quote by
two columns."
  (interactive)
  (let* ((quote-buf (let ((inhibit-local-variables nil))
		     (find-file-noselect random-quote-file)))
	 (repeat-flag (equal last-command 'insert-random-quote))
	 rnd
	 (num-quotes 0)
	 beg end
	 insert-point)
    (cond ((not repeat-flag)
	   (random t)
	   (beginning-of-line 1)
	   (setq beg (point))
	   (end-of-line 1)
	   (setq end (point))
	   (setq random-quote-regexp
		 (if (eq beg end)
		     nil
		   (buffer-substring beg end)))
	   (delete-region beg end)))
    (save-excursion
      (set-buffer quote-buf)
      (bury-buffer quote-buf)
      (goto-char (point-min))
      (while (search-forward "\n%\n" nil t)
	(setq num-quotes (1+ num-quotes)))
      (while (progn
	       (setq rnd (random (not repeat-flag)))
	       (if (< rnd 0)
		   (setq rnd (- (1+ rnd))))
	       (setq rnd (% rnd num-quotes))
	       (goto-char (point-min))
	       (or (= rnd 0)
		   (search-forward "\n%\n" nil nil rnd))
	       (setq beg (point))
	       (search-forward "\n%\n")
	       (setq end (1+ (match-beginning 0)))
	       (goto-char beg)
	       (and random-quote-regexp
		    (not (re-search-forward random-quote-regexp end t))))))
    (cond (repeat-flag
	   (delete-region (point) (mark))
	   (undo-boundary))
	  (t
	   (push-mark)))
    (save-restriction
      (narrow-to-region (point) (point))
      (insert-buffer-substring quote-buf beg end)
      (save-restriction
	(goto-char (point-max))
	(if (re-search-backward "^[ \t]+-- " nil t)
	    (beginning-of-line 1))
	(narrow-to-region (point-min) (point))
	(goto-char (point-min))
	(while (search-forward "\"" nil t)
	  (delete-char -1)
	  (insert (if (looking-at "\\w") "`" "'")))
	(goto-char (point-min))
	(skip-chars-forward " \t\n")
	(insert "\"")
	(goto-char (point-max))
	(skip-chars-backward " \t\n")
	(insert "\""))
      (goto-char (point-max)))
    (indent-rigidly (mark) (point) 2)))

(provide 'insert-random-quote)