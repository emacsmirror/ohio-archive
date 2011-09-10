;;; genius.el --- supplement package for 『ジーニアス英和・和英辞典』
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

(defconst genius-arranges
  '(genius-arrange-heading
    lookup-arrange-gaijis
    lookup-arrange-references
    lookup-arrange-squeezed-references
    lookup-arrange-default-headings
    lookup-arrange-fill-lines))

(defvar genius-dictionary-options
  (list (cons ':arranges genius-arranges)))

(defun genius-arrange-heading (entry)
  (when (re-search-forward "\\`.+\\(/\\|<gaiji:ha235>\\).+\\(/\\|<gaiji:ha235>\\)." nil t)
;  (when (re-search-forward "\\`.+/.+/." nil t)
    (backward-char 1) 
    (insert-string "\n")))

(setq lookup-package-agent-options
      '((:title . "ジーニアス英和・和英辞典")))

(setq lookup-package-dictionary-options-alist
      (list (cons (cond ((eq lookup-package-agent 'ndtp) "GENIUS/GENIUS")
			((eq lookup-package-agent 'ndeb) "GENIUS"))
		  genius-dictionary-options)))

;;; genius.el ends here
