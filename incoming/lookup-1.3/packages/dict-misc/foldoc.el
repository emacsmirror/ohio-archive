;;; foldoc.el --- supplement file for "FOLDOC"
;; Copyright (C) 1999 Lookup Development Team <lookup@ring.gr.jp>

;; Author: Keisuke Nishida <kei@psn.net>
;; Target: ndic 1.0, ndict 1.0
;; Format: 1.1
;; Version: $Id: foldoc.el,v 1.2 1999/05/23 17:27:28 knishida Exp $

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

(defconst foldoc-arrange-functions
  '(foldoc-fix-by-black-list
    foldoc-arrange-references
    lookup-arrange-default-headings
    lookup-arrange-fill-paragraphs))

(defconst foldoc-adjust-functions
  '(lookup-adjust-check-references
    lookup-adjust-goto-min))

(setq lookup-package-dictionary-options
      (list (cons ':title "FOLDOC")
	    (cons ':arranges foldoc-arrange-functions)
	    (cons ':adjusts foldoc-adjust-functions)))

(defun foldoc-fix-by-black-list (entry)
  (let ((heading (lookup-entry-heading entry)))
    (cond
     ((string= heading "control flow")
      (search-forward "structures}") (replace-match "structure}")))))

(defun foldoc-arrange-references (entry)
  (let ((dictionary (lookup-entry-dictionary entry))
	start end heading url reference)
    (while (re-search-forward "{[^}]+}" nil t)
      (setq start (1+ (match-beginning 0)) end (1- (match-end 0)))
      (setq heading (buffer-substring-no-properties start end))
      (while (string-match "\n *" heading)
	(setq heading (replace-match " " t t heading)))
      (if (string-match " *(\\([a-z]+://[^)]*\\))" heading)
	  (setq url (match-string 1 heading)
		heading (if (eq (match-beginning 0) 0) url
			  (substring heading 0 (match-beginning 0)))
		reference (lookup-make-url-reference url heading))
	(setq reference (lookup-make-reference dictionary heading heading))
	(lookup-reference-make-dynamic reference 'lookup-dynamic-code-search))
      (lookup-set-link start end reference)
      (delete-region end (1+ end))
      (delete-region (1- start) start))))

;;; foldoc.el ends here
