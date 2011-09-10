;;; colloc.el --- supplement package for 『新編 英和活用大辞典』
;; Copyright (C) 1999 Lookup Development Team <lookup@ring.gr.jp>

;; Author: Keisuke Nishida <kei@psn.net>
;; Target: ndtp 1.0, ndeb 1.0
;; Format: 1.1
;; Version: 0.1

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

(defconst chujiten-arranges
  '(colloc-arrange-first
    lookup-arrange-gaijis
    lookup-arrange-references
    lookup-arrange-default-headings
    lookup-arrange-fill-lines))

(defvar colloc-dictionary-options
  (list (cons ':title "英和活用大辞典")
	(cons ':arranges chujiten-arranges)))

(defun colloc-arrange-first (entry)
  (if (re-search-forward " <reference>→<gaiji=zb12e>.*" nil t)
      (delete-region (match-beginning 0) (match-end 0)))
  (when (looking-at "【.*】")
    (forward-line)
    (if (re-search-forward "^【.*】" nil t)
	(delete-region (match-beginning 0) (point-max)))
    (goto-char (point-min)))
  (if (re-search-forward " <reference>→<gaiji=zb12d>.*" nil t)
      (let ((string (match-string 0)))
	(goto-char (point-min))
	(end-of-line)
	(insert string)))
  (while (re-search-forward " <reference>→<gaiji=zb12d>.*" nil t)
    (delete-region (match-beginning 0) (match-end 0))))

(setq lookup-package-agent-options
      '((:title . "新編 英和活用大辞典")))

(setq lookup-package-dictionary-options-alist
      (list (cons (cond ((eq lookup-package-agent 'ndtp) "COLLOC/COLLOC")
			((eq lookup-package-agent 'ndeb) "COLLOC"))
		  colloc-dictionary-options)))

;;; colloc.el ends here
