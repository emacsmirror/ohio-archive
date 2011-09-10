;;; rfc1983.el --- supplement package for "RFC1983"

;; Copyright (C) 1999 

;; Author: KOREEDA Kazuyoshi <k_koreed@d2.dion.ne.jp>
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
      '((:title . "RFC1983")))

(setq lookup-package-dictionary-options-alist
         '(("rfc1983" . ((:title . "RFC1983")
			 (:arranges 
			  . (rfc1983-arrange-references
                                 lookup-arrange-default-headings))))))

(defun rfc1983-arrange-references (entry)
  (when (or (re-search-forward "See:" nil t)
	    (re-search-forward "See[ \n]+also:" nil t))
	(while (looking-at "[, \n]*\\([^,]+\\)")
	  (save-match-data
	    (setq heading (lookup-oneline-string (upcase (match-string 1))))
	    (if(string-match "\\.? *\\(\\[.*\\]\\)? *$" heading)
		(setq heading (replace-match "" nil t heading))))
	  (setq reference (lookup-make-reference dictionary heading heading))
	  (lookup-reference-make-dynamic reference 'lookup-dynamic-code-search)
	  (lookup-set-link (match-beginning 1) (match-end 1) reference)
	  (goto-char (match-end 0)))))

;;; rfc1983.el ends here





