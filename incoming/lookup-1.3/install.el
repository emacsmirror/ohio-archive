;;; install.el --- Lookup installer
;; Copyright (C) 1999 NISHIDA Keisuke <knishida@ring.aist.go.jp>

;; $Id: install.el,v 1.6 2000/02/10 00:54:31 knishida Exp $

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Code:

(if (string< emacs-version "19.29")
    (setq command-line-args-left (cdr command-line-args-left)))

(defconst install-elisp-files
  '("evi.el" "evi-mule.el" "lookup.el" "lookup-utils.el" "lookup-types.el"
    "lookup-vse.el" "lookup-package.el" "lookup-kanji.el"
    "lookup-entry.el" "lookup-content.el" "lookup-select.el"
    "ndic.el" "ndeb.el" "ndtp.el" "ndict.el" "ndkks.el" "ndspell.el"
    "ndcookie.el" "ndmisc.el" "ndsrd.el" "sdicf.el" "stem-english.el"
    "lookup-vars.el"))

(defconst install-info-files
  '("lookup.info" "lookup-guide.info"))

(defvar install-lisp-directory nil)
(defvar install-info-directory nil)
(defvar install-lookup-version "unknown")

(defun install-check-directory (directory)
  (and (not (file-exists-p directory))
       (y-or-n-p (format "Directory %s is not exist.  Creat it? " directory))
       (make-directory directory t))
  (if (not (file-directory-p directory))
      (error "%s is not directory" directory))
  (directory-file-name (expand-file-name directory)))

(or (fboundp 'with-current-buffer)
    (defmacro with-current-buffer (buffer &rest body)
      (` (save-current-buffer
	   (set-buffer (, buffer))
	   (,@ body)))))

(or (fboundp 'save-current-buffer)
    (defmacro save-current-buffer (&rest body)
      (` (let ((evi-orig-buffer (current-buffer)))
           (unwind-protect
               (progn (,@ body))
             (set-buffer evi-orig-buffer))))))


;; get the version number of lookup
(with-current-buffer (get-buffer-create " *work lookup-version*")
  (erase-buffer)
  (setq install-lookup-version
	(condition-case nil
	    (progn
	      (insert-file-contents "VERSION")
	      (goto-char (point-min))
	      (buffer-substring (point-min) (progn (end-of-line) (point))))
	  (error "unknown")))
  (kill-buffer (current-buffer)))

;; message

(if noninteractive nil
  (switch-to-buffer (generate-new-buffer "*Lookup Installer*"))
  (insert "Lookup インストーラ\n")
  (insert "===================\n\n")
  (insert "Lookup のインストールを始めます。")
  (insert "途中で中断するには C-g を押して下さい。\n\n"))

;; directory

(if noninteractive nil
  (insert "ディレクトリの決定\n")
  (insert "------------------\n\n")
  (insert "elisp ファイルのディレクトリを入力して下さい:\n"))
(let ((default "~/emacs/lisp/lookup/"))
  (setq install-lisp-directory
	(install-check-directory
	 (if noninteractive
	     (or (car command-line-args-left) default)
	   (read-file-name "Lisp directory: " default default)))))
(if noninteractive nil
  (insert "    -> " install-lisp-directory "\n\n") (sit-for 0))

(if noninteractive nil
  (insert "info ファイルのディレクトリを入力して下さい:\n"))
(let ((default "~/emacs/info/"))
  (setq install-info-directory
	(install-check-directory
	 (if noninteractive
	     (or (car (cdr command-line-args-left)) default)
	   (read-file-name "Info directory: " default default)))))
(if noninteractive nil
  (insert "    -> " install-info-directory "\n\n") (sit-for 0))

;; lookup-vars.el

(if noninteractive nil
  (insert "インストールの実行\n")
  (insert "------------------\n\n")
  (insert "lookup-vars.el.in から lookup-vars.el を生成中...") (sit-for 0))
(with-current-buffer (find-file-noselect "lisp/lookup-vars.el.in" t)
  (if (search-forward "@VERSION@")
      (replace-match install-lookup-version))
  (if (search-forward "@pkgemacsdir@")	;needs to be fixed to the correct one
      (replace-match install-lisp-directory))
  (write-file "lookup-vars.el")
  (kill-buffer (current-buffer))
  )
(message "Copied lookup-vars.el.in to lookup-vars.el")
(if (not noninteractive) (insert "done\n"))

;; compile

(if noninteractive nil
  (insert "elisp ファイルのコンパイル中...") (sit-for 0))
(let* ((default-directory (expand-file-name "lisp/"))
       (lookup-compile-directory default-directory)
       (command-line-args-left (if (string< emacs-version "19.29")
				   (cons nil install-elisp-files)
				 install-elisp-files)))
  (load (expand-file-name "lookup-compile.el")))
(if (not noninteractive) (insert "done\n"))

;; install

(if noninteractive nil
  (insert "elisp ファイルのインストール中...") (sit-for 0))
(mapcar (lambda (file)
	  (copy-file (expand-file-name file "lisp/")
		     (expand-file-name file install-lisp-directory) t)
	  (message "Installed %s to %s" file install-lisp-directory)
	  (setq file (byte-compile-dest-file file))
	  (copy-file (expand-file-name file "lisp/")
		     (expand-file-name file install-lisp-directory) t)
	  (message "Installed %s to %s" file install-lisp-directory))
	install-elisp-files)
(if (not noninteractive) (insert "done\n"))

(if noninteractive nil
  (insert "info ファイルのフォーマット中...") (sit-for 0))
(mapcar (lambda (info)
	  (if (file-readable-p (expand-file-name info "texi/"))
	      t
	    ;; the pre-formatted .info file does not exist; format it here
	    (let ((texi (concat (file-name-sans-extension info) ".texi")))
	      (save-current-buffer
	       (let ((buf (find-file-noselect 
			   (expand-file-name texi "texi/") t)))
		 (set-buffer buf)
		 (condition-case nil
		     (texinfo-format-buffer t) ; t for nosplit
		   (error ; one more try with no @direntry
					;(kill-buffer (current-buffer))
		    (set-buffer buf)
		    (goto-char (point-min))
		    (message "Format failed. Trying no @direntry")
		    (when (re-search-forward "@direntry" nil t)
			  (setq beg (match-beginning 0))
			  (when (re-search-forward "@end direntry" nil t)
				(delete-region beg (match-end 0))))
		    (goto-char (point-min))
		    (when (re-search-forward "@dircategory" nil t)
			  (setq end (progn(end-of-line)(point)))
			  (delete-region (match-beginning 0) end))
		    (when (re-search-forward "@detailmenu" nil t)
			  (beginning-of-line)
			  (insert "@c")
			  (when (re-search-forward "@end detailmenu" nil t)
				(beginning-of-line)
				(insert "@c")))
		    (goto-char (point-min))
		    (while (re-search-forward  
			    "@\\(email\\|url\\)\\{\\([^\\}]+\\)\\}" nil t)
		      (replace-match 
		       (buffer-substring (match-beginning 2) (match-end 2))))
		    (texinfo-format-buffer t)))
		 (save-buffer 0)
		 (kill-buffer (current-buffer))
		 (kill-buffer buf))))))
	install-info-files)

(if (not noninteractive) (insert "done\n"))

(if noninteractive nil
  (insert "info ファイルのインストール中...") (sit-for 0))
(mapcar (lambda (info)
	  (mapcar (lambda (file)
		    (copy-file (expand-file-name file "texi/")
			       (expand-file-name file install-info-directory)
			       t)
		    (message "Installed %s to %s" file install-info-directory))
		  (directory-files "texi/" nil info)))
	install-info-files)
(if (not noninteractive) (insert "done\n"))

;; clean

(if noninteractive nil
  (insert "一時ファイルの削除中...") (sit-for 0))
(let ((default-directory (expand-file-name "lisp/")))
  (delete-file "lookup-vars.el")
  (mapcar (lambda (file)
	    (delete-file (byte-compile-dest-file (expand-file-name file))))
	  install-elisp-files))
(let ((default-directory (expand-file-name "texi/")))
   (mapcar (lambda (file)
	(delete-file (expand-file-name file)))
	install-info-files))
	
(message "Removed lookup-vars.el, *.elc, *.info")
(if (not noninteractive) (insert "done\n"))

;; initialize

(if noninteractive nil
  (insert "Lookup の初期化中...")
  (sit-for 0)
  (lookup-initialize)
  (insert "done\n")
  (kill-buffer (current-buffer)))

;; congratulate

(if noninteractive
    (message "\nSee etc/SETUP for the setup after installation")
  (switch-to-buffer (generate-new-buffer "*Congratulations!*"))
  (insert "Lookup セットアップガイド
=========================

Lookup のインストールが完了しました。必要に応じて、以下の設定を行なって
下さい。

load-path の設定
----------------

次のようにして load-path の設定を行なって下さい。これはインストールした
プログラムファイルを読み込むために必要なものです。パスが通っていることが
わかっているなら、なくても構いません。

  (setq load-path (cons \"" install-lisp-directory "\" load-path))

info ディレクトリの設定
-----------------------

次のようにして Info-default-directory-list の設定を行なって下さい。これ
はインストールした info ファイルを読み込むために必要なものです。パスが
通っていることがわかっているなら、なくても構いません。

  (setq Info-default-directory-list
        (cons \"" install-info-directory "\" Info-default-directory-list))

dir ファイルの設定
------------------

このインストーラは info ファイルの一覧ファイル(dir ファイル)への書き出し
を自動では行ないません。必要に応じて、次の各行を dir ファイルに追加して
下さい。

* Lookup: (lookup).             Lookup, a Search Interface.
* Lookup Guide: (lookup-guide). The Lookup Startup Guide.

以上でインストールは終了です。ここで C-c C-c とタイプするとスタートアッ
プガイドが開かれますので、まずはそちらからご欄下さい。

-- EOF --
")
  (goto-char (point-min))
  (set-buffer-modified-p nil)
  (view-mode)
  (local-set-key "\C-c\C-c" 'install-open-info))

(defun install-open-info ()
  (interactive)
  (require 'info)
  (Info-find-node (expand-file-name "lookup-guide" install-info-directory)
		  "Top"))

;;; install.el ends here
