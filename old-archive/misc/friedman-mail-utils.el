;;; friedman-mail-utils.el -- random mail frobnication utilities

;;; Copyright (C) 1992, 1993 Noah S. Friedman

;;; Maintainer: Noah Friedman <friedman@prep.ai.mit.edu>
;;; $Id: friedman-mail-utils.el,v 1.2 1993/10/28 09:31:02 friedman Exp $
;;; LCD Archive Entry:
;;; friedman-mail-utils|Noah Friedman|friedman@gnu.ai.mit.edu|
;;; random mail frobnication utilities|
;;; 28-Oct-1993|1.2|~/misc/friedman-mail-utils.el.gz|

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, you can either send email to this
;;; program's author (see below) or write to: The Free Software Foundation,
;;; Inc.; 675 Massachusetts Avenue; Cambridge, MA 02139, USA.

;;; These functions are meant to be primitives with which to build other
;;; header-parsing routines.  I have written an example of their use in
;;; mail-reorder-headers.el (available separately).
;;;
;;; These functions use mail-header-separator to determine the end of the
;;; mail header list in a given buffer.  This string is by default set to
;;; "--text follows this line--" but you can change it to something else
;;; temporarily via a (let ...) and hack incoming mail messages as well (in
;;; that case, mail-header-separator should be temporarily set to "").
;;;
;;; Inspiration for this package came from a couple of mail-parsing
;;; functions written by Mike Williams.


;; Require sendmail now because mail-position-on-field is redefined.
(require 'sendmail)

;; This replaces the mail-position-on-field function in sendmail.el (this
;; version is more robust).
;;;###autoload
(defun mail-position-on-field (field &optional soft)
  (let (end
	(case-fold-search t))
    (goto-char (point-min))
    (re-search-forward
     (concat "^" (regexp-quote mail-header-separator) "$"))
    (setq end (match-beginning 0))
    (goto-char (point-min))
    (if (re-search-forward (concat "^" field ":") end t)
	(progn
	  (re-search-forward "^[^ \t]" nil 'move)
	  (beginning-of-line)
	  (skip-chars-backward "\n")
	  t)
      (or soft
	  (progn (goto-char end)
		 (insert field ": \n")
		 (skip-chars-backward "\n")))
      nil)))

;;;###autoload
(defun mail-header-separator-position ()
"Return character offset in current buffer of the beginning of the
mail-header-separator line, or nil if there isn't one."
  (save-excursion
    (goto-char (point-min))
    (and (re-search-forward (concat "^" (regexp-quote mail-header-separator) "$") nil t)
         (match-beginning 0))))

;;;###autoload
(defun mail-append-header-contents (HEADER CONTENTS)
"If HEADER exists, append CONTENTS to any existing contents for that
header, otherwise create new header and append contents to it."
  (save-excursion
    (mail-position-on-field HEADER)
    (insert CONTENTS)))

;;;###autoload
(defun mail-current-header ()
  "Return as a string the name of the mail header in which point is
currently sitting, either in the header itself or in header contents.
If point is not in a mail header (e.g. in the body of the message) the
return value is nil."
  (save-excursion
    (let ((mail-header-separator-position (mail-header-separator-position)))
      (and mail-header-separator-position
           (< (point) (mail-header-separator-position))
           (progn
             (end-of-line)
             (re-search-backward "^[^:\n]*:" nil t))
           (buffer-substring (match-beginning 0) (1- (match-end 0)))))))

;;;###autoload
(defun mail-get-beginning-of-header-line-position (&optional header)
  "Return the offset into the current buffer of the beginning of the
line containing header HEADER, or current header if none is specified.  See
function  mail-current-header .  Return value is nil if header doesn't exist
or point isn't in a mail header."
  (or header (setq header (mail-current-header)))
  (save-excursion
    (save-restriction
      (widen)
      (and (mail-header-separator-position)
           (mail-position-on-field header 'soft)
           (progn
             (re-search-backward (concat "^" (regexp-quote header) ":"))
             (beginning-of-line)
             (point))))))

;;;###autoload
(defun mail-get-header-contents (HEADER)
  "Return a list containing contents of any headers named HEADER.
If no occurrences of HEADER exist in the current mail buffer, return nil."
  (save-excursion
    (save-restriction
      (let (contents-list
            end
            beg)
        (while (mail-position-on-field HEADER 'soft)
          (setq end (point)
                beg (progn
                      (re-search-backward (concat HEADER ": "))
                      (goto-char (match-end 0)))
                contents-list (cons (buffer-substring beg end) contents-list))
          (narrow-to-region end (point-max)))
        (nreverse contents-list)))))

;;;###autoload
(defun mail-get-header-names (&optional uniquep)
  "Return a list of all existing mail headers, nil if none.  If optional
argument UNIQUEP is non-nil, only list each header name once, even if it
appears more than once in the mail headers."
  (let ((case-fold-search t)
        (mail-headers-end (mail-header-separator-position))
        mail-header-list
        this-mail-header)
    (and mail-headers-end
         (save-excursion
           (goto-char (point-min))
           (while (re-search-forward "^[^:\n]*:" mail-headers-end t)
             (setq this-mail-header (buffer-substring (match-beginning 0)
                                                      (1- (match-end 0))))
             (if (not (and uniquep (fmu-member this-mail-header mail-header-list)))
                 (setq mail-header-list
                       (cons this-mail-header mail-header-list))))
           (nreverse mail-header-list)))))

;;;###autoload
(defun mail-put-header (HEADER CONTENTS)
  "Add the specified header to the current mail message, with the given
CONTENTS.
If the header already exists in the message, place this header and contents
one below it, on a new line.  (use  mail-put-unique-header  if you want to
overwrite pre-existing headers and their contents)."
  (if (mail-position-on-field HEADER 'soft)
      (save-excursion
        (save-restriction
          (widen)
          (let (mail-header-end
                (case-fold-search t))
            (goto-char (point-min))
            (re-search-forward (concat "^" (regexp-quote mail-header-separator) "$"))
            (setq mail-header-end (match-end 0))

            (while (mail-position-on-field HEADER 'soft)
              (narrow-to-region (1+ (point)) mail-header-end))
            (insert HEADER ": " CONTENTS "\n"))))
    ;; use mail-put-unique-header, which will put the header at the end of
    ;; the header list.  This is where we really want it since no previous
    ;; header of the same name exists anyway.
    (mail-put-unique-header HEADER CONTENTS)))

;;;###autoload
(defun mail-put-unique-header (HEADER CONTENTS &optional REPLACE)
  "Add the specified HEADER to the current mail message, with the given
CONTENTS.
If the header already exists, the contents are left unchanged, unless optional
argument REPLACE is non-nil."
  (save-excursion
    (let ((header-exists (mail-position-on-field HEADER)))
      ;; Delete old contents if REPLACE is set
      (if (and header-exists REPLACE)
          (let ((end (point))
                (beg (progn
                       (re-search-backward (concat HEADER ": "))
                       (goto-char (match-end 0)))))
            (delete-region beg end)))
      ;; Add new contents if REPLACE is set, or this is a new header.
      (if (or (not header-exists) REPLACE)
          (progn (insert CONTENTS) CONTENTS)))))

;;;###autoload
(defun mail-remove-header (HEADER &optional all)
  "Remove first instance of HEADER (and contents) from the current
mail message.  If optional second argument ALL is non-nil, all such
instances are removed."
  (save-excursion
    (let (beg
          end
          (doit t))
      (while (and (mail-position-on-field HEADER 'soft) doit)
        (setq end (point)
              beg (progn
                    (re-search-backward (concat HEADER ": "))
                    (goto-char (match-beginning 0)))
              doit all)
        (delete-region beg (1+ end))))))


;;; Some useful primitives not supplied in the emacs 18 distribution.

(defun fmu-delete (elt list)
  "Like delq, but uses `equal' rather than `eq' for comparison."
  (if (equal elt (car list))
      (fmu-delete elt (cdr list))
    (let ((tail list))
      (while (cdr tail)
        (if (equal elt (car (cdr tail)))
            (setcdr tail (cdr (cdr tail)))
          (setq tail (cdr tail)))))
    list))

(defun fmu-member (elt list)
  "Like memq, but uses `equal' rather than `eq' for comparison."
  (while (and list (not (equal elt (car list))))
    (setq list (cdr list)))
  list)


;; This really doesn't belong here.

;;;###autoload
(defvar mail-folder-directory "~/"
        "*Default directory where mail saved via FCC headers should go.")

;;;###autoload
(defun mail-fcc (&optional file)
  "Add a new FCC field, with file name completion."
  (interactive)
  (or file
      (setq file (read-file-name "Folder carbon copy: " mail-folder-directory)))
  (mail-put-unique-header "Fcc" file 'force-replace))

(define-key mail-mode-map "\C-c\C-f\C-f" 'mail-fcc)


;; Put provide here so that package won't be considered provided if an
;; interruption occurs before loading is complete.
(provide 'friedman-mail-utils)

;; End of friedman-mail-utils.el
