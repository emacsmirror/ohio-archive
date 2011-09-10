;;; ndcookie.el --- Lookup from cookie file
;; Copyright (C) 1999 Lookup Development Team <lookup@ring.gr.jp>

;; Author: Keisuke Nishida <kei@psn.net>
;; Version: $Id: ndcookie.el,v 1.2 1999/05/23 17:27:22 knishida Exp $

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
(require 'cookie1)

(defconst ndcookie-version "1.0")

(put 'ndcookie ':methods lookup-word-search-methods)

(put 'ndcookie 'setup 'ndcookie-setup)
(defun ndcookie-setup (agent)
  (let* ((file (expand-file-name (lookup-agent-location agent) data-directory))
	 (name (file-name-sans-extension (file-name-nondirectory file))))
    (list (lookup-new-dictionary agent file name))))

(put 'ndcookie 'clear 'ndcookie-clear)
(defun ndcookie-clear (agent) nil)

(put 'ndcookie 'search 'ndcookie-dictionary-search)
(defun ndcookie-dictionary-search (dictionary query)
  (let* ((cookie (cookie (lookup-dictionary-code dictionary) "" ""))
	 (heading (substring cookie 0 (string-match "\n" cookie))))
    (list (lookup-make-entry dictionary cookie heading))))

(put 'ndcookie 'content 'ndcookie-dictionary-content)
(defun ndcookie-dictionary-content (dictionary entry)
  (lookup-entry-code entry))

(provide 'ndcookie)

;;; ndcookie.el ends here
