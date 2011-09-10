;;; mqr-replace.el --- query replace in multiple buffers
;;
;; Copyright (C) 1994 Christian Moen
;;
;; Author: Christian Moen <christim@ifi.uio.no>
;; Created: 2 Nov 1994
;; Version: 1.0
;; Keywords: query replace, multiple buffers
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
;;
;; LCD Archive Entry:
;; mqr-replace|Christian Moen|christim@ifi.uio.no|
;; Query replace in multiple buffers|
;; 02-Nov-1994|1.0|~/misc/mqr-replace.el.Z|
;;

;;; Purpose:
;;
;; This package provides an easy way to query replace strings in
;; multiple buffers.  The ability to automatically pick buffer
;; contexts according to the current buffer is also included.  New
;; buffer contexts are easily added.

;;; Installation:
;;
;; To use this package, be sure to have mqr-replace.el in your emacs
;; lisp load path.  If you put mpr-replace.el in a directory called
;; ~/elisp, add the following form to your ~/.emacs:
;;
;; (setq load-path (append (cons (expand-file-name "~/elisp") load-path)))
;;
;; You need to add these forms to as well:
;;
;; (require 'mqr-replace)
;; (define-key global-map "\C-ca" 'mqr-auto-replace)
;; (define-key global-map "\C-cq" 'mqr-query-replace)
;;
;; Alternatively:
;;
;; (autoload 'mqr-auto-replace  "mqr-replace" "Auto multi query replace." t)
;; (autoload 'mqr-query-replace "mqr-replace" "Query buffers query replace." t)
;; (define-key global-map "\C-ca" 'mqr-auto-replace)
;; (define-key global-map "\C-cq" 'mqr-query-replace)
;;
;; Bind the keys to whatever suits you best.
;;

;;; History:
;;
;; - 1.0:
;;   Initial release.
;;

;;; Todo:
;;
;; - Add more buffer contexts
;;

;;; Feedback:
;;
;; Please send me bug reports, bug fixes, suggestions, extensions,
;; comments, etc.  Thanks.
;;

;;; Code:

(defvar mqr-from-prompt "Multi query replace"
  "String to prompt when querying for from-string.")

(defvar mqr-buffer-prompt "Multi query replace buffer"
  "String to prompt when querying for buffes.")

(defvar mqr-query-buffers-first t
  "*If non-nil, query buffers before from- and to-string.")

(defvar mqr-beginning-of-buffer t
  "If non-nil, move point to beginning of buffer before searching.
Don't change this value unless you really know what you're doing.")

(defvar mqr-auto-buffer-list
  '(
    "\\.cc$\\|\\.hh$\\|\\.h$"
    "\\.C$\\|\\.H$\\|\\.h$"
    "\\.c$\\|\\.h$"
    "\\.l$"
    "\\.scm$"
    "\\.el$"
    "\\.tcl$"
    "\\.sim$"
    "\\.pl$"
    )
  "*List of buffer-name regexps to match when building buffer
list to be used by `mqr-auto-replace'.  The name of the current
buffer is matched agains each element in `mqr-auto-buffer-list'.
If a match is found, query replace in all buffers matched by the
same regexp.")

(defun mqr-query-from ()
  "Query and return from-string."
  (read-from-minibuffer	(format "%s: " mqr-from-prompt) nil nil nil))

(defun mqr-query-to (from)
  "Query and return to-string.

Arguments (from)"
  (read-from-minibuffer	(format "%s %s with: " mqr-from-prompt from)
			nil nil nil))

(defun mqr-replace-buffers (from to buffers)
  "Query replace strings matched by FROM with TO in BUFFERS.

Arguments: (from to buffers)"
  (while buffers
    (switch-to-buffer (car buffers))
    (save-excursion
      (if mqr-beginning-of-buffer
	  (goto-char (point-min)))
      (query-replace from to))
    (setq buffers (cdr buffers))))

(defun mqr-make-bufferlist (name-regexp)
  "Return a list of buffer-names matched by NAME-REGEXP.

Arguments: (name-regexp)"
  (let ((match-buffers (list))
	(buffers (mapcar (function buffer-name) (buffer-list))))
    (while buffers
      (if (string-match name-regexp (car buffers))
	  (setq match-buffers (append (cons (car buffers) match-buffers))))
      (setq buffers (cdr buffers)))
    match-buffers))

(defun mqr-auto-replace ()
  "Query replace strings in automatically chosen buffers.

Use the name of the current buffer and the information in
`mqr-auto-buffer-list' to make a list of buffers to query
replace in.  See `mqr-auto-buffer-list' for more information."
  (interactive)
  (let ((abufs mqr-auto-buffer-list))
    (while abufs
      (if ((lambda ()
	     (let ((cfs case-fold-search)
		   match)
	       (setq case-fold-search nil
		     match (string-match (car abufs) (buffer-name))
		     case-fold-search cfs)
	       match)))
	  (let* ((buffers (mqr-make-bufferlist (car abufs)))
		 (from (mqr-query-from))
		 (to (mqr-query-to from)))
	    (mqr-replace-buffers from to buffers)))
      (setq abufs (cdr abufs)))))

(defun mqr-query-buffers ()
  "Query and return a list of buffers to be query replaced."
  (let ((buffer (read-buffer (concat mqr-buffer-prompt ": ") (buffer-name) t))
	buffers)
    (while buffer
      (setq buffers (cons buffer buffers)
	    buffer (read-buffer (concat mqr-buffer-prompt " ("
					(mapconcat (function (lambda (b) b))
						   (reverse buffers)
						   ", ")
					") :") nil t)))
    (reverse buffers)))

(defun mqr-query-replace ()
  "Query replace strings in queried buffers.

Query user for buffers to query replace in.  Press RET at the
prompt to stop adding buffers.  Depending on the value of
`mqr-query-buffers-first', query buffers first."
  (interactive)
  (if mqr-query-buffers-first
      (let* ((buffers (mqr-query-buffers))
	     (from (mqr-query-from))
	     (to (mqr-query-to from)))
	(mqr-replace-buffers from to buffers))
    (let* ((buffers (mqr-query-buffers))
	   (from (mqr-query-from))
	   (to (mqr-query-to from)))
      (mqr-replace-buffers from to buffers))))

(provide 'mqr-replace)

;;; mqr-replace.el ends her
