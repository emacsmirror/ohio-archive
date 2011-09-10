;;; ndsrd.el --- search agent for 小学館『ランダムハウス英語辞典』
;; Copyright (C) 1999 Lookup Development Team <lookup@ring.gr.jp>

;; Author: Keisuke Nishida <kei@psn.net>
;; Version: $Id: ndsrd.el,v 1.4 1999/09/08 16:48:00 yoshimi Exp $

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

;;; Commentary:

;; 利用するには、~/.emacs で次のように設定して下さい。
;; 
;; (setq lookup-search-agents
;;       '((ndsrd "/usr/local/dict/srd"
;;                :fmt "/usr/local/dict/srd/csrd.fmt"
;;                :gai "/usr/local/dict/srd/csrd.gai")))

;;; Code:

(require 'lookup)

(defconst ndsrd-version "0.3")

;;;
;:: Customizable variables
;;;

(defgroup ndsrd nil
  "Lookup csrd interface."
  :group 'lookup-agents)

(defcustom ndsrd-program-name "csrd"
  "*csrd のコマンド名。"
  :type 'string
  :group 'ndsrd)

(defcustom ndsrd-process-coding-system lookup-process-coding-system
  "*Coding system for csrd process."
  :type 'symbol
  :group 'ndsrd)


;;;
;:: types
;;;

(put 'ndsrd ':methods '(exact prefix suffix substring))

(put 'ndsrd ':arranges
     '(lookup-arrange-default-headings
       lookup-arrange-fill-lines))

(put 'ndsrd ':adjusts
     '(lookup-adjust-goto-min))


;;;
;:: Interface functions
;;;

(put 'ndsrd 'setup 'ndsrd-setup)
(defun ndsrd-setup (agent)
  (let* ((directory (lookup-agent-location agent))
	 (format (lookup-agent-option agent ':fmt))
	 (gaiji (lookup-agent-option agent ':gai))
	 opts dictionary)
    (if gaiji (setq opts (cons (concat "-g" gaiji) opts)))
    (if format (setq opts (cons (concat "-f" format) opts)))
    (setq opts (cons (concat "-d" directory) opts))
    (list (lookup-new-dictionary agent opts "srd" "ランダムハウス英語辞典"))))

(put 'ndsrd 'clear 'ndsrd-clear)
(defun ndsrd-clear (agent) nil)

(put 'ndsrd 'search 'ndsrd-dictionary-search)
(defun ndsrd-dictionary-search (dictionary query)
  (with-temp-buffer
    (lookup-with-coding-system ndsrd-process-coding-system
      (apply 'call-process ndsrd-program-name nil t nil
	     (append (lookup-dictionary-code dictionary)
		     (list (lookup-query-string query)))))
    (goto-char (point-min))
    (let (start content heading entries)
      (while (re-search-forward "^□ \\([^ \n]*\\)" nil 0)
	(when start
	  (setq content (buffer-substring start (match-beginning 0)))
	  (setq entries (cons (lookup-make-entry dictionary content heading)
			      entries)))
	(setq start (match-beginning 0) heading (match-string 1)))
      (when start
	(setq content (buffer-substring start (point)))
	(setq entries (cons (lookup-make-entry dictionary content heading)
			    entries)))
      (nreverse entries))))

(put 'ndsrd 'content 'ndsrd-dictionary-content)
(defun ndsrd-dictionary-content (dictionary entry)
  (lookup-entry-code entry))

(provide 'ndsrd)

;;; ndsrd.el ends here
