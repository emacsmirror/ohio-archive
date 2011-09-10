;;; vera.el --- supplement package for "V.E.R.A."
;; Copyright (C) 1999 Lookup Development Team <lookup@ring.gr.jp>

;; Author: Keisuke Nishida <kei@psn.net>
;; Target: ndic 1.0, ndict 1.0
;; Format: 1.1
;; Version: 1.0

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software Foundation,
;; Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Code:

(require 'lookup-package)

(setq lookup-package-agent-options
      '((:title . "Virtual Entity of Relevant Acronyms")))

(setq lookup-package-dictionary-options-alist
      '(("vera" . ((:title . "V.E.R.A")
		   (:arranges . (vera-arrange-references
				 lookup-arrange-default-headings))))))

(defun vera-arrange-references (entry)
  (when (search-forward "(" nil t)
    (let ((dictionary (lookup-entry-dictionary entry)) heading reference)
      (while (looking-at "[, ]*\\([^,)]+\\)")
	(setq heading (match-string 1))
	(setq reference (lookup-make-reference dictionary heading heading))
	(lookup-reference-make-dynamic reference 'lookup-dynamic-code-search)
	(lookup-set-link (match-beginning 1) (match-end 1) reference)
	(goto-char (match-end 0))))))

;;; vera.el ends here
