;;; ndkks.el --- Lookup KAKASI interface
;; Copyright (C) 1999 Lookup Development Team <lookup@ring.gr.jp>

;; Author: Keisuke Nishida <kei@psn.net>
;; Version: $Id: ndkks.el,v 1.2 1999/05/23 17:27:23 knishida Exp $

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

(defconst ndkks-version "1.0")

;;;
;:: Customizable variables
;;;

(defgroup ndkks nil
  "Lookup KAKASI interface."
  :group 'lookup-agents)

(defcustom ndkks-program-name "kakasi"
  "*KAKASI のプログラム名。"
  :type 'string
  :group 'ndkks)

(defcustom ndkks-dictionary-title "漢字かな辞典"
  "*ndkks 辞書のタイトル。"
  :type 'string
  :group 'ndkks)

(defcustom ndkks-content-format
  '(t "\n" ("-JH") "\n" "【振り仮名】" ("-JH" "-f" "-p") "\n")
  "*ndkks 辞書が出力するエントリ内容のフォーマット。
リストの各要素として、文字列、文字列のリスト、及び `t' を指定できる。
文字列の場合、それがそのまま挿入される。
リストの場合、それを引数として KAKASI が呼び出され、その結果が挿入される。
`t' の場合、検索語が挿入される。"
  :type '(repeat (radio :tag "出力内容"
			(const :tag "検索語" t)
			(string :tag "文字列")
			(repeat :tag "KAKASI 呼び出し"
				(string :tag "option"))))
  :group 'ndkks)

(defcustom ndkks-process-coding-system lookup-kakasi-coding-system
  "*Coding system for KAKASI process."
  :type 'symbol
  :group 'ndkks)


;;;
;:: Internal variables
;;;

(if (string-match "^19" emacs-version) 
    (defconst ndkks-valid-charsets (list lc-jp))
  (defconst ndkks-valid-charsets '(japanese-jisx0208)))

;;;
;:: types
;;;

;; ndkks agent:
;;
;;   (ndkks)

(put 'ndkks ':methods '(exact))

(put 'ndkks ':arranges
     '(lookup-arrange-default-headings
       lookup-arrange-fill-lines))

(put 'ndkks ':adjusts 
     '(lookup-adjust-goto-min))

;; ndkks dictionary:
;;
;; CODE  - none
;; NAME  - same as `ndkks-program-name'

;; ndkks entry:
;;
;; CODE    - input string
;; HEADING - same as CODE above


;;;
;:: Interface functions
;;;

(put 'ndkks 'setup 'ndkks-setup)
(defun ndkks-setup (agent)
  (unless (featurep 'mule)
    (error "ndkks requires `mule' feauture."))
  (call-process ndkks-program-name nil 0)	; check KAKASI exists
  (list (lookup-new-dictionary agent nil ndkks-program-name
			       ndkks-dictionary-title)))

(put 'ndkks 'clear 'ndkks-clear)
(defun ndkks-clear (agent)
  (ndkks-process-kill))

(put 'ndkks 'search 'ndkks-dictionary-search)
(defun ndkks-dictionary-search (dictionary query)
  (let ((string (lookup-query-string query)))
    (and
     ;; xxx: 漢字が含まれているか調べたいのだが、どうやればいいんだろう?
     ;; xxx: とりあえず、適当にチェック。
     (let ((charsets (find-charset-string string)))
       (catch 'return
	 (while charsets
	   (if (memq (car charsets) ndkks-valid-charsets)
	       (throw 'return t)
	     (setq charsets (cdr charsets))))))
     (string-match "[^あ-んア-ンーＡ-Ｚａ-ｚ]" string)
     (list (lookup-make-entry dictionary string string)))))

(put 'ndkks 'content 'ndkks-dictionary-content)
(defun ndkks-dictionary-content (dictionary entry)
  (let ((string (lookup-entry-code entry)))
    (mapconcat (lambda (element)
		 (cond ((eq element t) string)
		       ((stringp element) element)
		       ((listp element) (ndkks-process-require element string))
		       (t (error "Invalid format element: %S" element))))
	       ndkks-content-format "")))


;;;
;:: KAKASI process
;;;

(defvar ndkks-process-alist nil)

(defun ndkks-get-process (args)
  (let ((process (lookup-assoc-ref ndkks-process-alist args)))
    (unless (and process (eq (process-status process) 'run))
      (if process (kill-process process))
      (let ((buffer (lookup-open-process-buffer " *ndkks*")))
	(setq process (apply 'start-process "ndkks" buffer
			     ndkks-program-name args))
	(process-kill-without-query process)
	;; 起動後、少し時間を置かないと、最初の検索がうまくいかない。
	(sleep-for 0.1)
	(let ((coding ndkks-process-coding-system))
	  (when coding
	    (set-process-coding-system process coding coding)))
	(setq ndkks-process-alist
	      (lookup-assoc-set ndkks-process-alist args process))))
    process))

(defun ndkks-process-require (args string)
  (lookup-process-require (ndkks-get-process args) (concat string "\n") "\n"))

(defun ndkks-process-kill ()
  (while ndkks-process-alist
    (lookup-process-kill (cdar ndkks-process-alist))
    (setq ndkks-process-alist (cdr ndkks-process-alist))))

(provide 'ndkks)

;;; ndkks.el ends here
