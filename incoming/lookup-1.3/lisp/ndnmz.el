;;; ndnmz.el --- Lookup Namazu interface
;; Copyright (C) 1999 Lookup Development Team <lookup@ring.gr.jp>

;; Author: TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>
;; Version: $Id: ndnmz.el,v 1.2 1999/09/29 08:04:10 tsuchiya Exp $

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

(require 'eword-decode)
(require 'lookup-vars)

(defgroup ndnmz nil
  "Lookup Namazu interface."
  :group 'lookup-agents)

(defcustom ndnmz-program-name "namazu"
  "*検索プログラム(namazu)の名前"
  :type 'string
  :group 'ndnmz)

(defcustom ndnmz-program-options '("-aS")
  "*検索プログラム(namazu)のオプションのリスト"
  :type 'list
  :group 'ndnmz)

(defcustom ndnmz-subject-limit 3000
  "*メールの表題を探す上限"
  :type 'integer
  :group 'ndnmz)

(defcustom ndnmz-mail-headers '("From" "To" "Cc" "Reply-To" "Subject" "Date")
  "*メールの本文と共に表示するヘッダのリスト"
  :type 'list
  :group 'ndnmz)

(defcustom ndnmz-process-coding-system lookup-process-coding-system
  "*Condig system used for Namazu process."
  :type 'symbol
  :group 'ndnmz)

(defcustom ndnmz-default-file-coding-system
  (if (>= emacs-major-version 20) 'undecided *autoconv*)
  "*Condig system used for Namazu files."
  :type 'symbol
  :group 'ndnmz)

(defvar ndnmz-backend-coding-system
  (if (>= emacs-major-version 20) 'raw-text *noconv*)
  "Coding system used in file backends of Namazu.")

(defvar ndnmz-temp-buffer nil "エージェントの作業用バッファ")

(defun ndnmz-agent-recursive-p (agent)
  "サブディレクトリに含まれる索引ファイルも検索するかどうか調べる関数"
  (or (lookup-agent-option agent 'recursive) t))

(defun ndnmz-search-subdirectories (directory)
  "指定されたディレクトリ以下で索引ファイルを含むディレクトリを全て返す関数"
  (delq nil (apply 'nconc
		   (mapcar (lambda (file)
			     (cond
			      ((file-directory-p file)
			       (ndnmz-search-subdirectories file))
			      ((string= "NMZ.i" (file-name-nondirectory file))
			       (list (directory-file-name (file-name-directory file))))))
			   (directory-files directory t "[^\\.]")))))

(put 'ndnmz 'setup 'ndnmz-agent-setup)
(defun ndnmz-agent-setup (agent)
  "エージェントを作成する関数"
  (let ((directory (directory-file-name
		    (expand-file-name (lookup-agent-location agent)))))
    (mapcar (lambda (dir)
	      (lookup-make-dictionary agent dir (file-name-nondirectory dir) (file-name-nondirectory dir)))
	    (if (ndnmz-agent-recursive-p agent)
		(ndnmz-search-subdirectories directory)
	      (list directory)))))

(put 'ndnmz 'clear 'ndnmz-agent-clear)
(defun ndnmz-agent-clear (agent)
  "エージェントを終了する関数"
  (when (and (bufferp ndnmz-temp-buffer)
	     (buffer-name ndnmz-temp-buffer))
    (kill-buffer ndnmz-temp-buffer)
    (setq ndnmz-temp-buffer nil)))
  
(defun ndnmz-generate-temp-buffer ()
  "作業用バッファを生成する関数"
  (unless (buffer-live-p ndnmz-temp-buffer)
    (setq ndnmz-temp-buffer (generate-new-buffer " *ndnmz*")
	  lookup-buffer-list (cons ndnmz-temp-buffer lookup-buffer-list))
    (buffer-disable-undo ndnmz-temp-buffer))
  ndnmz-temp-buffer)

(put 'ndnmz-with-temp-buffer 'lisp-indent-function 0)
(defmacro ndnmz-with-temp-buffer (&rest body)
  "一時バッファをカレントバッファとして body を実行し、その後一時バッファを掃除するマクロ"
  (` (let ((original-buffer (current-buffer)))
       (unwind-protect
	   (progn
	     (set-buffer (ndnmz-generate-temp-buffer))
	     (unwind-protect
		 (progn (,@ body))
	       (delete-region (point-min) (point-max))))
	 (set-buffer original-buffer)))))

(defsubst ndnmz-dictionary-type (dictionary)
  "検索対象のファイルの性質を指定するオプションを調べる関数"
  (lookup-dictionary-option dictionary ':type t))

(defsubst ndnmz-dictionary-coding-system (dictionary)
  "検索対象のファイルの漢字コードを指定するオプションを調べる関数"
  (or (lookup-dictionary-option dictionary ':coding-system t)
      ndnmz-default-file-coding-system))

(defun ndnmz-header-value (header)
  (let ((point (point))
	(data (match-data))
	(case-fold-search t))
    (unwind-protect
	(progn
	  (goto-char (point-min))
	  (if (search-forward (concat "\n" header ": ") nil t)
	      (eword-decode-string (buffer-substring (point) (point-at-eol)))))
      (goto-char point)
      (set-match-data data))))

(defun ndnmz-get-mail-subject (file)
  "指定されたファイルから Subject を取り出す関数"
  (ndnmz-with-temp-buffer
    (lookup-with-coding-system ndnmz-backend-coding-system
      (insert-file-contents file nil 0 ndnmz-subject-limit))
    (or (ndnmz-header-value "subject") "(none)")))

(put 'ndnmz 'search 'ndnmz-search-dictionary)
(defun ndnmz-search-dictionary (dictionary query)
  "指定された辞書を検索する関数"
  (let (files headings entries)
    (ndnmz-with-temp-buffer
      (lookup-with-coding-system ndnmz-process-coding-system
	(apply 'call-process
	       ndnmz-program-name nil t nil
	       (append ndnmz-program-options
		       (list (lookup-query-string query)
			     (lookup-dictionary-code dictionary)))))
      (goto-char (point-min))
      (while (not (eobp))
	(setq files (cons (buffer-substring (point) (point-at-eol)) files))
	(forward-line 1)))
    (setq headings (mapcar (if (eq (ndnmz-dictionary-type dictionary) 'mail)
			       'ndnmz-get-mail-subject
			     'file-name-nondirectory)
			   files))
    (while files
      (setq entries (cons (lookup-make-entry dictionary (car files) (car headings)) entries)
	    files (cdr files)
	    headings (cdr headings)))
    entries))

(put 'ndnmz 'content 'ndnmz-dictionary-content)
(defun ndnmz-dictionary-content (dictionary entry)
  (ndnmz-with-temp-buffer
    (lookup-with-coding-system (ndnmz-dictionary-coding-system dictionary)
      (insert-file-contents (lookup-entry-code entry)))
    (buffer-substring (point-min) (point-max))))

(defun ndnmz-arrange-default (entry)
  (let ((type (ndnmz-dictionary-type (lookup-entry-dictionary entry)))
	(case-fold-search t))
    (cond
     ((eq type 'plain) (lookup-arrange-default-headings entry))
     ((eq type 'mail) (ndnmz-arrange-mail entry))
     ;; auto detect
     ((looking-at "From \\|[a-z-]+: ") (ndnmz-arrange-mail entry))
     (t (lookup-arrange-default-headings entry)))))

(defun ndnmz-arrange-mail (entry)
  (save-restriction
    (narrow-to-region (goto-char (point-min))
		      (progn (re-search-forward "^$" nil t) (point)))
    (apply 'insert
	   (delq nil (prog1 (let (v)
			      (mapcar (lambda (s)
					(if (setq v (ndnmz-header-value s))
					    (format "%s: %s\n" s v)))
				      ndnmz-mail-headers))
		       (delete-region (point-min) (point-max)))))))

(put 'ndnmz ':type 'auto)
(put 'ndnmz ':methods '(exact text))
(put 'ndnmz ':arranges '(ndnmz-arrange-default))
(put 'ndnmz ':adjusts '(lookup-adjust-goto-min))

(provide 'ndnmz)
