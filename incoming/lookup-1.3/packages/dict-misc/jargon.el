;;; jargon.el --- supplement file for "The Jargon File"
;; Copyright (C) 1999 Lookup Development Team <lookup@ring.gr.jp>

;; Author: Keisuke Nishida <kei@psn.net>
;; Target: ndic 1.0, ndict 1.0
;; Format: 1.1
;; Version: $Id: jargon.el,v 1.2 1999/05/23 17:27:28 knishida Exp $

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

(defconst jargon-reference-pattern
  '("{\\([^}]+\\)}" 1 (lookup-oneline-string (match-string 1))
    lookup-dynamic-code-search))

(defconst jargon-arrange-functions
  '(lookup-arrange-references
    jargon-arrange-headings
    lookup-arrange-fill-paragraphs))

(defconst jargon-adjust-functions
  '(lookup-adjust-check-references
    lookup-adjust-goto-min))

(setq lookup-package-dictionary-options
      (list (cons ':title "Jargon File")
	    (cons ':reference-pattern jargon-reference-pattern)
	    (cons ':arranges jargon-arrange-functions)
	    (cons ':adjusts jargon-adjust-functions)))

(defun jargon-arrange-headings (entry)
  (if (looking-at "[^/\n]+")
      (lookup-make-region-heading (match-beginning 0) (match-end 0) 1)))

;;; jargon.el ends here
