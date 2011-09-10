;;; lookup-content.el --- lookup-content-mode
;; Copyright (C) 1999 Lookup Development Team <lookup@ring.gr.jp>

;; Author: Keisuke Nishida <kei@psn.net>
;; Version: $Id: lookup-content.el,v 1.5 2000/04/02 02:52:20 knishida Exp $

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
(require 'lookup-vse)

;;;
;:: Internal variables
;;;

;; バッファが表示しているエントリを保持する。
(defvar lookup-content-current-entry nil)

;; モードライン行に表示する情報。
(defvar lookup-content-line-heading nil)

(make-variable-buffer-local 'lookup-content-current-entry)
(make-variable-buffer-local 'lookup-content-line-heading)

;;;;;;;;;;;;;;;;;;;;
;: Construct Buffer
;;;;;;;;;;;;;;;;;;;;

;; Content バッファにはエントリの内容が出力される。
;; 関数 `lookup-content-display' により、これを行なう。

(defun lookup-content-display (entry)
  ;; Content バッファを生成し、ENTRY の内容を表示する。
  ;; 実際の挿入処理を行なうのは `lookup-vse-insert-content'。
  (with-current-buffer (lookup-open-buffer (lookup-content-buffer))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (if (lookup-reference-p entry)
	  (insert "(no contents)")
	(lookup-vse-insert-content entry)))
    (lookup-content-mode)
    (setq lookup-content-current-entry entry)
    (setq lookup-content-line-heading (lookup-entry-heading entry))
    (lookup-display-buffer (current-buffer))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;:  Lookup Content mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar lookup-content-mode-map nil
  "*Keymap for Lookup Content mode.")

(unless lookup-content-mode-map
  (setq lookup-content-mode-map (make-sparse-keymap))
  (define-key lookup-content-mode-map " " 'scroll-up)
  (define-key lookup-content-mode-map "\C-?" 'scroll-down)
  (define-key lookup-content-mode-map [delete] 'scroll-down)
  (define-key lookup-content-mode-map "<" 'beginning-of-buffer)
  (define-key lookup-content-mode-map ">" 'end-of-buffer)
  (define-key lookup-content-mode-map "\C-i" 'lookup-content-next-link)
  (define-key lookup-content-mode-map [(shift tab)] 'lookup-content-previous-link)
  (define-key lookup-content-mode-map "\C-m" 'lookup-content-follow-link)
  (define-key lookup-content-mode-map "t" 'lookup-content-toggle-format)
  (define-key lookup-content-mode-map "w" 'lookup-content-cite-region)
  (define-key lookup-content-mode-map "h" 'lookup-content-entry-window)
  (define-key lookup-content-mode-map "f" 'lookup-entry-search-pattern)
  (define-key lookup-content-mode-map "g" 'lookup-content-update)
  (define-key lookup-content-mode-map "q" 'lookup-content-leave)
  (define-key lookup-content-mode-map "?" 'lookup-content-help)
  (if (featurep 'xemacs)
      (define-key lookup-content-mode-map 'button2 'lookup-content-mouse-follow)
    (define-key lookup-content-mode-map [mouse-2] 'lookup-content-mouse-follow))
  )

(defconst lookup-content-mode-help
  "Lookup Content モード:

`SPC' - ページを進める          `<'   - バッファの最初へ
`DEL' - ページを戻る            `>'   - バッファの最後へ

`TAB' - 次のリンクへ            `RET' - リンクを辿る

`t'   - 整形処理をトグルする    `w'   - リージョンを引用
`h'   - Entry バッファに移動    `g'   - バッファを更新する
`q'   - バッファを抜ける        `?'   - ヘルプを表示")

(defvar lookup-content-mode-hook nil)

(defun lookup-content-mode ()
  "Lookup Content モード。

\\{lookup-content-mode-map}
モードに入るときに `lookup-content-mode-hook' が呼ばれる。"
  (interactive)
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq major-mode 'lookup-content-mode)
  (setq mode-name "Content")
  (setq mode-line-buffer-identification
	'("Lookup:%b {" lookup-content-line-heading "}"))
  (setq buffer-read-only t)
  (use-local-map lookup-content-mode-map)
  (run-hooks 'lookup-content-mode-hook))

;;;
;:: Interactive commands
;;;

(defun lookup-content-next-link ()
  "次のリンクに移動する。"
  (interactive)
  (if (lookup-goto-next-link)
      (message (lookup-entry-id (lookup-get-link (point))))
    (if (lookup-get-link (point))
	(error "No more link in this buffer")
      (goto-char (point-min))
      (if (lookup-goto-next-link)
	  (message (lookup-entry-id (lookup-get-link (point))))
	(error "No link in this buffer")))))

(defun lookup-content-previous-link ()
  "前のリンクに移動する。"
  (interactive)
  (if (lookup-goto-previous-link)
      (message (lookup-entry-id (lookup-get-link (point))))
    (if (lookup-get-link (point))
	(error "No more link in this buffer")
      (goto-char (point-min))
      (if (lookup-goto-previous-link)
	  (message (lookup-entry-id (lookup-get-link (point))))
	(error "No link in this buffer")))))

(defun lookup-content-follow-link ()
  "ポイント位置のリンクを参照する。"
  (interactive)
  (let ((entry (lookup-get-link (point))))
    (if entry
	(let* ((module (lookup-session-module lookup-current-session))
	       (heading (lookup-entry-heading lookup-content-current-entry))
	       (query (lookup-make-query 'reference heading))
	       (entries (if (not (lookup-reference-p entry))
			    (list entry)
			  (lookup-reference-refer entry)
			  (lookup-reference-entries entry))))
	  (if entries
	      (lookup-display-entries module query entries)
	    (error "This link is torn off")))
      (error "No link here"))))

(defun lookup-content-mouse-follow (event)
  "マウスでクリックしたリンクを参照する。"
  (interactive "e")
  (mouse-set-point event)
  (lookup-content-follow-link))

(defun lookup-content-toggle-format ()
  "本文の整形処理をトグルする。"
  (interactive)
  (setq lookup-enable-format (not lookup-enable-format))
  (lookup-content-display lookup-content-current-entry))

(defun lookup-content-cite-region (start end)
  "リージョンの内容をキルリングに保存する。
その際、変数 `lookup-cite-header' または辞書オプション `cite-header'
により引用時のヘッダを、変数 `lookup-cite-prefix' または辞書オプション
`cite-prefix' により引用時のプレフィクスを指定することが出来る。"
  (interactive "r")
  (let* ((dictionary (lookup-entry-dictionary lookup-content-current-entry))
	 (header (or (lookup-dictionary-option dictionary ':cite-header t)
		     lookup-cite-header))
	 (prefix (or (lookup-dictionary-option dictionary ':cite-prefix t)
		     lookup-cite-prefix))
	 (contents (buffer-substring-no-properties start end)))
    (when prefix
      (with-temp-buffer
	(insert contents)
	(goto-char (point-min))
	(while (not (eobp))
	  (insert prefix)
	  (forward-line))
	(setq contents (buffer-string))))
    (when header
      (let ((title (lookup-dictionary-title dictionary)))
	(while (string-match "%T" header)
	  (setq header (replace-match title t t header))))
      (setq contents (concat header contents)))
    (kill-new contents)
    (if (featurep 'xemacs)
	(zmacs-deactivate-region)
      (deactivate-mark))
    (when (interactive-p)
      (if (pos-visible-in-window-p (mark) (selected-window))
	  (let ((inhibit-quit t))
	    (save-excursion (goto-char (mark)) (sit-for 1)))
	(let ((len (min (abs (- end start)) 40)))
	  (if (= (point) start)
	      (message "Saved text until \"%s\""
		       (buffer-substring (- end len) end))
	    (message "Saved text from \"%s\""
		     (buffer-substring start (+ start len)))))))))

(defun lookup-content-entry-window ()
  "Entry バッファに移動する。"
  (interactive)
  (select-window (get-buffer-window (lookup-entry-buffer))))

(defun lookup-content-update ()
  "キャッシュを用いずに本文を読み直す。"
  (interactive)
  (let ((lookup-force-update t))
    (lookup-content-display lookup-content-current-entry)))

(defun lookup-content-leave ()
  "Content バッファを抜ける。"
  (interactive)
  (lookup-hide-buffer (current-buffer))
  (lookup-entry-display-content))

(defun lookup-content-help ()
  "Content モードの簡易ヘルプを表示する。"
  (interactive)
  (with-current-buffer (lookup-open-buffer (lookup-help-buffer))
    (help-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert lookup-content-mode-help))
    (lookup-display-help (current-buffer))))

;;;
;:: Useful functions
;;;

(defun lookup-content-collect-references ()
  ;; Content バッファのリファレンスを全て集めてリストにして返す。
  (with-current-buffer (lookup-content-buffer)
    (let (entries)
      (lookup-map-over-property
       (point-min) (point-max) 'lookup-reference
       (lambda (start end entry)
	 (setq entries 
	       (if (and (lookup-reference-p entry)
			(not (lookup-reference-dynamic-p entry)))
		   (nconc (reverse (lookup-reference-entries entry)) entries)
		 (cons entry entries)))))
      (nreverse entries))))

(provide 'lookup-content)

;;; lookup-content.el ends here
