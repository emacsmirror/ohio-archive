;;; flow-fill.el --- interprete RFC2646 "flowed" text
;; Copyright (C) 2000 Free Software Foundation, Inc.

;; Author: Simon Josefsson <jas@pdc.kth.se>
;; Keywords: mail

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
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Commentary:

;; This implement decoding of RFC2646 formatted text, including the
;; quoted-depth wins rules.

;; Theory of operation: search for lines ending with SPC, save quote
;; length of line, remove SPC and concatenate line with the following
;; line if quote length of following line matches current line.

;; When no further concatenations are possible, we've found a
;; paragraph and we let `fill-region' fill the long line into several
;; lines with the quote prefix as `fill-prefix'.

;; Todo: encoding

;; History:

;; 2000-02-17  posted on ding mailing list
;; 2000-02-19  use `point-at-{b,e}ol' in XEmacs
;; 2000-03-11  no compile warnings for point-at-bol stuff
;; 2000-03-26  commited to gnus cvs

;;; Code:

(eval-and-compile
  (fset 'fill-flowed-point-at-bol
	(if (fboundp 'point-at-bol)
	    'point-at-bol
	  'line-beginning-position))
  
  (fset 'fill-flowed-point-at-eol
	(if (fboundp 'point-at-eol)
	    'point-at-eol
	  'line-end-position)))

(defun fill-flowed (&optional buffer)
  (save-excursion
    (set-buffer (or (current-buffer) buffer))
    (goto-char (point-min))
    (while (re-search-forward " $" nil t)
      (when (save-excursion
	      (beginning-of-line)
	      (looking-at "^\\(>*\\)\\( ?\\)"))
	(let ((quote (match-string 1)))
	  (if (string= quote "")
	      (setq quote nil))
	  (when (and quote (string= (match-string 2) ""))
	    (save-excursion
	      ;; insert SP after quote for pleasant reading of quoted lines
	      (beginning-of-line)
	      (when (> (skip-chars-forward ">") 0)
		(insert " "))))
	  (while (and (save-excursion
			(backward-char 3)
			(looking-at "[^-][^-] "))
		      (save-excursion
			(unless (eobp)
			  (forward-char 1)
			  (if quote
			      (looking-at (format "^\\(%s\\)\\([^>]\\)" quote))
			    (looking-at "^ ?")))))
	    (save-excursion
	      (replace-match (if (string= (match-string 2) " ")
				 "" "\\2")))
	    (backward-delete-char -1)
	    (end-of-line))
	  (let ((fill-prefix (when quote (concat quote " "))))
	    (fill-region (fill-flowed-point-at-bol)
			 (fill-flowed-point-at-eol)
			 'left 'nosqueeze)))))))

(provide 'flow-fill)

;;; flow-fill.el ends here
