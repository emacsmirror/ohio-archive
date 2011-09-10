;; Insert a random quote from a file.
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
;; Last modified by: Dave Brennan
;; Last modified on: Thu Mar  4 11:20:29 1993
;; Filename: insert-random-quote.el
;; Purpose: insert a random quote from a file

;; LCD Archive Entry:
;; insert-random-quote|Joe Wells|jbw@cs.bu.edu|
;; Insert a random quote from a file|
;; 1992-10-07||~/functions/insert-random-quote.el.Z|

(defvar random-quote-file (expand-file-name "~/.quotes")
  "*Pathname of a file containing quotes terminated by \"\\n%%\\n\".
Any text past the last terminator is ignored.")

(defun insert-random-quote ()
  "Inserts a randomly selected quote from random-quote-file.
If invoked twice in a row by keystroke, deletes the previously selected
random quote before inserting the new one.  Indents the inserted quote by
two columns."
  (interactive)
  (let ((quote-buf (let ((inhibit-local-variables nil))
		     (find-file-noselect random-quote-file)))
	(rnd (random (not (equal last-command 'insert-random-quote))))
	(num-quotes 0)
	beg end insert-point)
    (if (< rnd 0)
	(setq rnd (- (1+ rnd))))
    (save-excursion
      (set-buffer quote-buf)
      (bury-buffer quote-buf)
      (goto-char (point-min))
      (while (search-forward "\n%%\n" nil t)
	(setq num-quotes (1+ num-quotes)))
      (setq rnd (% rnd num-quotes))
      (goto-char (point-min))
      (or (= rnd 0)
	  (search-forward "\n%%\n" nil nil rnd))
      (setq beg (point))
      (search-forward "\n%%\n")
      (setq end (1+ (match-beginning 0))))
    (cond ((equal last-command 'insert-random-quote)
	   (delete-region (point) (mark))
	   (undo-boundary))
	  (t
	   (push-mark)))
    (insert-buffer-substring quote-buf beg end)
    (indent-rigidly (mark) (point) 2)))
