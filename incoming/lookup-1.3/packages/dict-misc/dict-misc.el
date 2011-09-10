;;; dict-misc.el --- supplement package for "dict-misc"
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
      '((title . "misc dictionaries")))

(setq lookup-package-dictionary-options-alist
      (list (cons "jargon" (lookup-package-load "jargon"))
	    (cons "foldoc" (lookup-package-load "foldoc"))
	    (cons "elements" '((title . "The Elements")))
	    (cons "easton" '((title . "Bible Dictionary")))
	    (cons "hitchcock" '((title . "Bible Names")))))

;;; dict-misc.el ends here
