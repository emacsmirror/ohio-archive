;;; oxford.el --- supplement package for "Oxford Dictionary/Thesaurus"
;; Copyright (C) 1999 Lookup Development Team <lookup@ring.gr.jp>

;; Author: Keisuke Nishida <kei@psn.net>
;; Target: ndtp 1.0, ndeb 1.0
;; Format: 1.1
;; Version: 0.2

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

(defvar oxford-dictionary-arranges
  '(oxford-arrange-structure
    lookup-arrange-references
    lookup-arrange-gaijis
    lookup-arrange-default-headings
    lookup-arrange-fill-lines))

(defvar oxford-dictionary-options
  (list (cons ':title "Oxford Dictionary")
	(cons ':stemmer 'stem-english)
	(cons ':arranges oxford-dictionary-arranges)))

(defun oxford-arrange-structure (entry)
  (while (re-search-forward "\\( \\([nv]\\|adj\\)\\.\\)\\|\\[[0-9]\\]" nil t)
    (save-excursion
      (goto-char (match-beginning 0))
      (newline))))

;;;
;;; Package Options
;;;

(setq lookup-package-agent-options
      '((:title . "Oxford Dictionary/Thesaurus")
	(:enable . ("OXFORD"))
	(:coding . iso-8859-1)
	(:stop-code . "0x1f090000")))

(setq lookup-package-dictionary-options-alist
      (list (cons (cond ((eq lookup-package-agent 'ndtp) "OXFORD/OXFORD")
			((eq lookup-package-agent 'ndeb) "OXFORD"))
		  oxford-dictionary-options)))

;;; oxford.el ends here
