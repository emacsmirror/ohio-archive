;;; caesar.el --- Caesar cipher routines (including rot13)
;;
;; Copyright (C) 1994 Christopher J. Madsen
;;
;; Author: Christopher J. Madsen <ac608@yfn.ysu.edu>
;; Created: 20 Oct 1994
;; Version: 1.4 (19-Jan-1996)
;; Keywords: news
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; LCD Archive Entry:
;; caesar|Christopher J. Madsen|ac608@yfn.ysu.edu|
;; Caesar cipher routines (including rot13)|
;; 19-Jan-1996|1.4|~/misc/caesar.el.gz|

;;; Commentary:
;;
;; I needed a routine to do rot13 on a string, and was suprised to
;; find that there wasn't one.  Each package that does rot13 has its
;; own custom function.
;;
;; So, here's a package that provides basic rot13 (or any other
;; Caesar rotation) functions.  This package is compatible with the
;; caesar-region in rnews.el (22 Apr 87).
;;
;; The main functions are documented individually:
;;   caesar-buffer
;;   caesar-rot-region
;;   caesar-string
;; The names are pretty much self-explanatory.
;; (I couldn't use caesar-region because rnews.el already has one.)

;;; Installation:
;;
;; Put caesar.el somewhere on your load path and byte compile it.
;; Unless you want to call the functions interactively, that's all.
;;
;; If you do want to use caesar-buffer or caesar-rot-region
;; interactively, put the following in your .emacs file:
;;     (autoload 'caesar-buffer     "caesar" nil t)
;;     (autoload 'caesar-rot-region "caesar" nil t)

(provide 'caesar)

;;====================================================================
;;; Code:

(defun caesar-build-table (&optional n)
  "Build table for Caesar rotation by N, default 13.
Stores the table in `caesar-translate-table'.
N may be an integer or a raw prefix argument."
  (setq n (if n
              (mod (prefix-numeric-value n) 26) ;canonicalize N
            13))                        ;Set default value for N
  (if (or (not (boundp 'caesar-translate-table))
          (/= (aref caesar-translate-table ?a) (+ ?a n)))
      (let ((i 0) (lower "abcdefghijklmnopqrstuvwxyz") upper)
        (message "Building caesar-translate-table...")
        (setq caesar-translate-table (make-vector 256 0))
        (while (< i 256)
          (aset caesar-translate-table i i)
          (setq i (1+ i)))
        (setq lower (concat lower lower) upper (upcase lower) i 0)
        (while (< i 26)
          (aset caesar-translate-table (+ ?a i) (aref lower (+ i n)))
          (aset caesar-translate-table (+ ?A i) (aref upper (+ i n)))
          (setq i (1+ i)))
        (message "Building caesar-translate-table... done"))))

(defun caesar-string (str &optional n)
  "Perform Caesar rotation of STRING by N, default 13.
Modify the string in place and return it.
N may be an integer or a raw prefix argument."
  (caesar-build-table n)
  (let ((i 0)
        (len (length str)))
    (while (< i len)
      (aset str i (aref caesar-translate-table (aref str i)))
      (setq i (1+ i))))
  str)

(defun caesar-buffer (&optional n buffer)
  "Caesar rotation of buffer by N, default 13, for decrypting netnews.
This removes any restrictions (narrowing) before rotating.

Interactively, use a prefix arg to specify the rotation.
From a Lisp program, optional second arg BUFFER specifies the buffer."
  (interactive "*P")
  (save-excursion
    (and buffer
         (set-buffer buffer))
    (widen)
    (let ((str (caesar-string (buffer-substring (point-min) (point-max)) n)))
      (erase-buffer)
      (insert str))))

(defun caesar-rot-region (beg end &optional n)
  "Caesar rotation of region by N, default 13, for decrypting netnews.
Interactively, use a prefix arg to specify the rotation.
From a Lisp program, takes 3 arguments, BEGIN, END, and (optionally) N."
  (interactive "*rP")
  (let ((str (caesar-string (buffer-substring beg end) n)))
    (goto-char beg)
    (delete-region beg end)
    (insert str)))

;;; caesar.el ends here
