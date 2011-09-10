;;; longlines.el --- automatically wrap long lines

;; Copyright (C) 2000 by Free Software Foundation, Inc.

;; Author: Kai Grossjohann <Kai.Grossjohann@CS.Uni-Dortmund.DE>
;; Keywords: convenience

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Some text editors save text files with long lines, and they
;; automatically break these lines at whitespace, without actually
;; inserting any newline characters.  When doing `M-q' in Emacs, you
;; are inserting newline characters.  This file provides a file format
;; which automatically fills the long lines when reading a file and
;; unfills the lines when saving the file.

;; There is also a mode function, so you can do stuff like the
;; following:
;;
;; (add-to-list 'auto-mode-alist '("\\.ll\\'" . longlines-mode))
;;
;; This means that all `.ll' files are assumed to have long lines
;; which will be decoded on reading an encoded on writing.

;;; Code:

(defvar longlines-mode nil
  "Whether the current buffer is in longlines mode.
Automatically local to every buffer.")
(make-variable-buffer-local 'longlines-mode)

(defconst longlines-version "$Id$"
  "Version information.")

(defun longlines-wrap (start end)
  "Wrap long lines in this buffer.
Buffer is assumed to contain long lines.  Every line is wrapped."
  (let (pos)
    (use-hard-newlines 1 'never)
    (save-excursion
      (goto-char start)
      (while (search-forward "\n" end t)
        (set-hard-newline-properties (match-beginning 0) (match-end 0)))
      (goto-char start)
      (setq pos (point))
      (while (and (zerop (forward-line 1))
                  (<= (point) end))
        (fill-region-as-paragraph pos (point))
        (setq pos (point)))
      (setq longlines-mode t)
      pos)))

(defun longlines-unwrap (start end buffer)
  "Unwrap long lines in this buffer.
Buffer is assumed to contain short lines and soft and hard newlines.
Replaces all soft newlines with spaces."
  (save-excursion
    (goto-char start)
    (while (re-search-forward "\n[ \t]*" end t)
      (unless (get-text-property (match-beginning 0) 'hard)
        (replace-match " ")))
    (max end (point))))

(defun longlines-mode (&optional arg)
  "Turn on longlines mode.
This automatically wraps lines."
  (interactive)
  (when (and arg (not (> arg 0)))
    (error "Cannot turn off longlines mode"))
  (add-to-list 'buffer-file-format 'longlines)
  (unless longlines-mode
    (longlines-wrap (point-min) (point-max))))

(add-to-list 'format-alist
             (list 'longlines           ;name
                   "Automatically wrap long lines." ;doc
                   nil                  ;regexp
                   'longlines-wrap      ;decode
                   'longlines-unwrap    ;encode
                   t                    ;modify
                   'longlines-mode))    ;mode function
        


;;; longlines.el ends here