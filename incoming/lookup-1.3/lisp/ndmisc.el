;;; ndmisc.el --- miscellaneous pseudo-agents
;; Copyright (C) 1999 Lookup Development Team <lookup@ring.gr.jp>

;; Author: Keisuke Nishida <kei@psn.net>
;; Version: $Id: ndmisc.el,v 1.2 1999/05/23 17:27:23 knishida Exp $

;; This file is part of Lookup.

;; Lookup is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; Lookup is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Lookup; if not, write to the Free Software Foundation,
;; Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Code:

(require 'lookup)

(defconst ndmisc-version "0.3")


;;;
;:: Internal variables
;;;

(defconst ndmisc-agent (lookup-make-agent 'ndmisc nil nil))

(put 'ndmisc ':arranges '(lookup-arrange-default-headings))
(put 'ndmisc ':adjusts '(lookup-adjust-goto-min))

;;;
;:: URL dictionary
;;;

(defvar ndmisc-url-dictionary nil)

(defun lookup-make-url-reference (url heading)
  (unless ndmisc-url-dictionary
    (setq ndmisc-url-dictionary
	  (lookup-new-dictionary ndmisc-agent nil "URL"))
    (lookup-dictionary-init ndmisc-url-dictionary))
  (let ((entry (lookup-make-entry ndmisc-url-dictionary url heading)))
    (lookup-entry-set-jump entry 'ndmisc-url-jump)
    entry))

(defun ndmisc-url-jump (entry)
  (browse-url (lookup-entry-code entry)))


;;;
;:: Interface functions
;;;

(put 'ndmisc 'content 'ndmisc-dictionary-content)
(defun ndmisc-dictionary-content (dictionary entry)
  (cond
   ((eq dictionary ndmisc-url-dictionary)
    (let ((heading (lookup-entry-heading entry))
	  (url (lookup-entry-code entry))
	  (keys (with-current-buffer (lookup-entry-buffer)
		  (substitute-command-keys "\\[lookup-entry-open-other]"))))
      (if (eq heading url) (setq heading "See also:"))
      (format "%s\n\n  %s\n\nType `%s' to go to this site.\n"
	      heading url keys)))))

(put 'ndmisc 'open 'ndmisc-dictionary-open)
(defun ndmisc-dictionary-open (dictionary entry)
  (cond
   ((eq dictionary ndmisc-url-dictionary) (ndmisc-url-jump entry))))

(provide 'ndmisc)

;;; ndmisc.el ends here
