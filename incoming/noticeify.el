;;; noticeify.el -- update a boilerplate copyright message in the buffer

;; Copyright (C) 1999-2000 Neil W. Van Dyke

;; Author:   Neil W. Van Dyke <nwv@acm.org>
;; Created:  07-Jul-1999
;; Version:  1.0
;; Keywords: copyright, boilerplate
;; X-URL:    http://www.media.mit.edu/~nwv/noticeify/
;; X-RCS:    $Id: noticeify.el,v 1.10 2000/08/18 05:35:50 nwv Exp $ GMT

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.
;;
;; This software is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;; This script updates a block of boilerplate comment text at the top of the
;; source code file in the current buffer.  By default, the text is inserted
;; from the file named `NOTICE-TEXT' in the same directory as the file.
;; `noticeify' makes a half-hearted attempt to use appropriate commenting
;; characters, based on the current major mode, so you can use insert the same
;; `NOTICE-TEXT' file into files written in any of several programming
;; languages.  The boilerplate text will be inserted between two cookies lines
;; `NOTICE' and `NOTICE-END', which result in something like the following.
;;
;;     /*
;;      * NOTICE()
;;      * 
;;      * Copyright (C) 1969 Foo Corp.
;;      *
;;      * Blah blah blah...
;;      *
;;      * NOTICE-END
;;      */
;;
;; You can also specify on a per-file basis whether to use boilerplate text
;; file other than `NOTICE-TEXT' by inserting a relative pathname or filename
;; between the parentheses in the `NOTICE()' cookie.  This is useful when you
;; have code files in multiple subdirectories that all use the same
;; `NOTICE-TEXT' file from a parent directory, in which case the would have
;; `NOTICE(..)' cookies.  It's also useful when a few of your files require
;; different boilerplate text, such as if they are not covered under the same
;; copyright license, in which case you might have cookies such as
;; `NOTICE(NOTICE-TEXT-FOR-BAZ-LIB)'.
;;
;; Note that `noticeify' is a quick&dirty thing I hacked up to add copyright
;; notices to a bunch of files in different languages a while ago.  This is not
;; the best way to do things (e.g., it should really use the syntax table for
;; the major mode, and it should also have a batch mode).

;;; Change Log:

;; [Version 1.0, 18-Aug-2000, nwv@acm.org] Initial release.

;;; Code:

(defvar noticeify-filename "NOTICE-TEXT"
  "*The base name of the file that contains boilerplate text for `noticeify'.")

(defun noticeify ()
  (interactive)
  (let ((dir        nil)
        (first-line nil)
        (fname      nil)
        (last-line  nil)
        (prefix     nil)
        (text-begin nil)
        (text-end   nil))

    ;; If a notice already exists, get its source directory/filename and
    ;; remember where the old text is.  If no notice exists, text-begin and
    ;; text-end will remain set to nil.
    (goto-char (point-min))
    (if (re-search-forward "NOTICE([ ]*\\([^\n)]*\\)[ ]*)[^\n]*\n" 300 t)
        (progn
          (setq text-begin (match-end 0))
          (setq dir (buffer-substring (match-beginning 1) (match-end 1)))
          (unless (re-search-forward
                   "^[^\n]*NOTICE-END[^\n]*\n?" nil t)
            (error "You have no \"NOTICE-END\" cookie!"))
          (setq text-end (match-beginning 0))))
      
    ;; Pick a line prefix to use.
    (setq first-line nil)
    (setq last-line  nil)
    (setq prefix
          (cond 
           ((memq major-mode '(c-mode c++-mode java-mode jde-mode))
            (progn (setq first-line "/*\n")
                   (setq last-line  " */\n")
                   " * "))
           ((memq major-mode '(emacs-lisp-mode lisp-mode scheme-mode))
            ";; ")
           ((memq major-mode
                  '(makefile-mode perl-mode sh-mode shell-script-mode))
            "# ")
           (t "")))
      
    ;; If there is existing text, delete it; otherwise insert new cookies.
    ;; Set text-begin to where new text should be inserted.
    (if text-begin
        (progn
          (goto-char text-begin)
          (delete-region text-begin text-end))
      (goto-char (point-min))
      (if (and (eq major-mode 'shell-script-mode)
               (looking-at "#!"))
          (forward-line 1))
      (if (not (bolp))
          (newline))
      (if first-line
          (insert first-line))
      (insert prefix "NOTICE()\n")
      (setq text-begin (point))
      (insert prefix "NOTICE-END\n")
      (if last-line
          (insert last-line))
      (newline))

    ;; Try to find the boilerplate file.
    (setq fname (cond ((or (not dir)
                           (string= dir ""))
                       (expand-file-name noticeify-filename))
                      ((file-directory-p dir)
                       (expand-file-name noticeify-filename dir))
                      (t
                       dir)))
    (unless (file-exists-p fname)
      (error "Noticeify file \"%s\" does not exist!" fname))
    (unless (file-readable-p fname)
      (error "Noticeify file \"%s\" is not readable!" fname))
      
    ;; Insert the text and format it.
    (goto-char text-begin)
    (goto-char (+ text-begin (nth 1 (insert-file-contents fname))))
    (if (not (bolp))
        (newline))
    (setq text-end (point))
    (goto-char text-begin)
    (save-restriction
      (narrow-to-region text-begin text-end)
      (goto-char (point-min))
      (while (not (eobp))
        (beginning-of-line)
        (insert prefix)
        (forward-line 1)))
      
    ;; Leave point at top of buffer.
    (goto-char (point-min))))

;; noticeify.el ends here
