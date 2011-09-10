;; message-x.el -- customizable completion in message headers
;; Copyright (C) 1998 Kai Groﬂjohann

;; $Id: message-x.el,v 1.8 2000/04/05 18:59:14 grossjoh Exp $

;; Author: Kai Grossjohann <grossjohann@ls6.informatik.uni-dortmund.de>
;; Keywords: news, mail, compose, completion

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; The most recent version of this can always be fetched from the
;; following FTP site:
;; ls6-ftp.cs.uni-dortmund.de:/pub/src/emacs

;; Installation:
;;
;; You must be using Gnus 5 or higher for this to work.  Installation
;; is simple: just put this file somewhere in your load-path, run M-x
;; byte-compile-file RET, and put the following line in your .gnus file:
;;
;;      (require 'message-x)
;;
;; Customization is possible through the two variables
;; message-x-body-function and message-x-completion-alist, which see.
;;
;; Purpose:
;;
;; This assigns a context-sensitive function to the TAB key in message
;; mode of Gnus.  When in a header line, this performs completion
;; based on which header we're in (for example, newsgroup name
;; completion makes sense in the Newsgroups header whereas mail alias
;; expansion makes sense in the To and Cc headers).  When in the
;; message body, this executes a different function, by default it is
;; indent-relative.

;;; Setup Code:

(defconst message-x-version "$Id: message-x.el,v 1.8 2000/04/05 18:59:14 grossjoh Exp $"
  "Version of message-x.")

(require 'message)

;;; User Customizable Variables:

(defgroup message-x nil
  "Customizable completion in message headers.")

(defcustom message-x-body-function 'indent-relative
  "message-x-tab executes this if point is in the body of a message."
  :type '(function)
  :group 'message-x)

(defcustom message-x-completion-alist
  '(("to" . message-x-complete-name)
    ("cc" . message-x-complete-name)
    ("bcc" . message-x-complete-name)
    ("resent-to" . message-x-complete-name)
    ("resent-cc" . message-x-complete-name)
    ("resent-bcc" . message-x-complete-name)
    ("gcc" . message-expand-group)
    ("newsgroups" . message-expand-group)
    ("followup-to" . message-expand-group))
  "Table telling which completion function message-x-tab should invoke.
Lookup in the table is done with `equal' comparison of the header."
  :type '(repeat (cons string function))
  :group 'message-x)

(defcustom message-x-before-completion-functions nil
  "`message-x-tab' runs these functions before doing the actual
completion.  The functions are called with one argument, a string
specifying the current header name in lower case or nil, which
specifies the message body.  Also see `message-x-after-completion-hook'."
  :type 'hook
  :group 'message-x)

(defcustom message-x-after-completion-functions nil
  "`message-x-tab' runs these functions after doing the actual
completion.  The functions are called with one argument, a string
specifying the current header name in lower case or nil, which
specifies the message body.  Also see `message-x-before-completion-hook'."
  :type 'hook
  :group 'message-x)

;;; Code:

(defun message-x-in-header-p ()
  "Returns t iff point is in the header section."
  (save-excursion
    (let ((p (point)))
      (beginning-of-buffer)
      (and (re-search-forward (concat "^"
				      (regexp-quote mail-header-separator)
				      "$"))
	   (progn (beginning-of-line) t)
	   (< p (point))))))
	  

(defun message-x-which-header ()
  "Returns the header we're currently in.  Returns nil if not in a header.
Example: returns \"to\" if we're in the \"to\" header right now."
  (and (message-x-in-header-p)
       (save-excursion
	 (beginning-of-line)
	 (while (looking-at "^[ \t]+") (forward-line -1))
	 (looking-at "\\([^:]+\\):")
	 (downcase (buffer-substring-no-properties (match-beginning 1)
						   (match-end 1))))))

(defun message-x-complete-name ()
  "Does name completion in recipient headers."
  (interactive)
  (if (fboundp 'bbdb-complete-name)
      (bbdb-complete-name)
    (expand-abbrev)))

(defun message-x-tab ()
  "Does completion based on header currently in or executes a default
function in the body."
  (interactive)
  (let ((header (assoc (message-x-which-header)
                       message-x-completion-alist)))
    (run-hook-with-args 'message-x-before-completion-functions header)
    (funcall (if header (cdr header) message-x-body-function))
    (run-hook-with-args 'message-x-after-completion-functions header)))

(define-key message-mode-map "\t" 'message-x-tab)

(provide 'message-x)
;; message-x ends here
