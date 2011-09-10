;;; lookup-select.el --- lookup-select-mode
;; Copyright (C) 1999 Lookup Development Team <lookup@ring.gr.jp>

;; Author: Keisuke Nishida <kei@psn.net>
;; Version: $Id: lookup-select.el,v 1.3 1999/05/23 17:27:21 knishida Exp $

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

;;;;;;;;;;;;;;;;;;;;
;: Construct Buffer
;;;;;;;;;;;;;;;;;;;;

(defun lookup-select-display (session)
  (with-current-buffer (lookup-open-buffer (lookup-select-buffer))
    (lookup-select-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "Type `m' to select, `u' to unselect, `?' for help.\n\n")
      (lookup-table-insert
       "%c %-12t %-20t %s\n"
       (append '((?% "Identfier" "Title" "Method")
		 (?- "---------" "-----" "------"))
	       (mapcar (lambda (dic)
			 (list (if (lookup-dictionary-selected-p dic) ?* ? )
			       (lookup-dictionary-id dic)
			       (lookup-dictionary-title dic)
			       (mapconcat 'lookup-method-key
					  (lookup-dictionary-methods dic) "")))
		       (lookup-module-dictionaries
			(lookup-session-module session)))))
      (lookup-select-goto-first))
    (lookup-pop-to-buffer (current-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;:  Lookup Select mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar lookup-select-mode-map nil
  "*Keymap for Lookup Select mode.")

(unless lookup-select-mode-map
  (setq lookup-select-mode-map (make-sparse-keymap))
  (define-key lookup-select-mode-map " " 'lookup-select-next-line)
  (define-key lookup-select-mode-map "n" 'lookup-select-next-line)
  (define-key lookup-select-mode-map "p" 'lookup-select-previous-line)
  (define-key lookup-select-mode-map "\en" 'lookup-history-next)
  (define-key lookup-select-mode-map "\ep" 'lookup-history-previous)
  (define-key lookup-select-mode-map "\ef" 'lookup-module-forward)
  (define-key lookup-select-mode-map "\eb" 'lookup-module-backward)
  (define-key lookup-select-mode-map "m" 'lookup-select-do-select)
  (define-key lookup-select-mode-map "u" 'lookup-select-do-unselect)
  (define-key lookup-select-mode-map "a" 'lookup-select-do-select-all)
  (define-key lookup-select-mode-map "\C-m" 'lookup-select-do-select-only)
  (define-key lookup-select-mode-map "d" 'lookup-select-mark-disable)
  (define-key lookup-select-mode-map "x" 'lookup-select-do-execute)
;  (define-key lookup-select-mode-map "i" 'lookup-select-info)
  (define-key lookup-select-mode-map "M" 'lookup-select-menu)
  (define-key lookup-select-mode-map "f" 'lookup-select-search-pattern)
  (define-key lookup-select-mode-map "o" 'delete-other-windows)
  (define-key lookup-select-mode-map "/" 'lookup-select-text-search)
  (define-key lookup-select-mode-map "g" 'lookup-select-update)
  (define-key lookup-select-mode-map "q" 'lookup-suspend)
  (define-key lookup-select-mode-map "Q" 'lookup-exit)
  (define-key lookup-select-mode-map "R" 'lookup-restart)
  (define-key lookup-select-mode-map "?" 'lookup-select-help))

(defconst lookup-select-mode-help
  "Lookup Select モード:

`n'(ext)    - 次の辞書へ        `p'(revios) - 前の辞書へ

`m'(ark)    - 辞書を選択        `u'(nmark)  - 辞書を非選択
`a'(ll)     - 全ての辞書を選択  `RET'       - その辞書だけを選択
`d'(isable) - 辞書を無効化   (e)`x'(ecute)  - 無効化を実行

`f'(ind)    - 検索を実行        `M'(enu)    - 辞書のメニューを表示
`o'(pen)    - 画面を最大化      `/'         - その辞書から全文検索

`q'    - バッファを抜ける       `g'    - モジュールを初期化し直す
`Q'    - Lookup を終了する      `R'    - Lookup を再起動する")

(defvar lookup-select-mode-hook nil)

(defun lookup-select-mode ()
  (interactive)
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq major-mode 'lookup-select-mode)
  (setq mode-name "Select")
  (setq mode-line-buffer-identification '("Lookup:%12b"))
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (use-local-map lookup-select-mode-map)
  (run-hooks 'lookup-select-mode-hook))

;;;
;:: Interactive commands
;;;

(defun lookup-select-next-line ()
  "次の行に進む。"
  (interactive)
  (if (eobp) (ding) (forward-line)))

(defun lookup-select-previous-line ()
  "前の行に戻る。"
  (interactive)
  (if (bobp) (ding) (forward-line -1)))

(defun lookup-select-do-select ()
  "ポイント行の辞書を選択する。"
  (interactive)
  (lookup-select-set-selected t))

(defun lookup-select-do-unselect ()
  "ポイント行の辞書を非選択にする。"
  (interactive)
  (lookup-select-set-selected nil))

(defun lookup-select-toggle-selected ()
  "ポイント行の辞書の選択状態をトグルする。"
  (interactive)
  (let ((dict (lookup-select-point-dictionary)))
    (lookup-select-set-selected
     (not (lookup-dictionary-selected-p dict)))))

(defun lookup-select-do-select-all ()
  "全ての辞書を選択する。"
  (interactive)
  (save-excursion
    (lookup-select-goto-first)
    (while (not (eobp))
      (lookup-select-set-selected t))))

(defun lookup-select-do-select-only ()
  "ポイント行の辞書のみを選択する。"
  (interactive)
  (if (not (lookup-select-point-dictionary))
      (error "No dictionary on current line")
    (save-excursion
      (lookup-select-goto-first)
      (while (not (eobp))
	(lookup-select-set-selected nil)))
    (lookup-select-set-selected t t)))

(defun lookup-select-mark-disable ()
  "ポイント行の辞書に無効化のマークを付ける。"
  (interactive)
  (lookup-select-mark ?D t))

(defun lookup-select-do-execute ()
  "無効化を実行する。"
  (interactive)
  (save-excursion
    (lookup-select-goto-first)
    (let* ((inhibit-read-only t)
	   (module (lookup-session-module lookup-current-session))
	   (dicts (lookup-module-dictionaries module)))
      (while (re-search-forward "^D" nil t)
	(setq dicts (delq (lookup-select-point-dictionary) dicts))
	(kill-region (progn (beginning-of-line) (point))
		     (progn (forward-line) (point))))
      (lookup-module-set-dictionaries module dicts))))

(defun lookup-select-menu ()
  "辞書がメニューに対応している場合、それを参照する。"
  (interactive)
  (let* ((dict (lookup-select-point-dictionary))
	 (entries (lookup-vse-get-menu dict)))
    (if entries
	(let* ((module (lookup-session-module lookup-current-session))
	       (title (lookup-dictionary-title dict))
	       (query (lookup-make-query 'reference title)))
	  (lookup-display-entries module query entries))
      (error "This dictionary has no menu"))))

(defun lookup-select-search-pattern (pattern)
  "選択された辞書から検索を行なう。"
  (interactive (list (lookup-read-string "Look up" nil 'lookup-input-history)))
  (lookup-search-pattern (lookup-session-module lookup-last-session) pattern))

(defun lookup-select-text-search (string &optional force)
  "ポイント行の辞書から全文検索を行なう。"
  (interactive
   (list (let ((dictionary (lookup-select-point-dictionary)))
	   (if (memq 'text (lookup-dictionary-methods dictionary))
	       (lookup-read-string "Look up" nil 'lookup-input-history)
	     (error "This dictionary does not support text search")))
	 current-prefix-arg))
  (let ((module (lookup-session-module lookup-current-session))
	(dictionary (lookup-select-point-dictionary))
	(query (lookup-make-query 'text string)))
    (message "searcing...")
    (lookup-display-entries module query
			    (lookup-vse-search-query dictionary query))
    (message "searcing...done")))

(defun lookup-select-update ()
  "現在の検索モジュールを初期化し直す。
ただし設定ファイルを変更した場合には、変更を反映するには
\\[lookup-restart] を用いる必要がある。"
  (interactive)
  (let ((module (lookup-session-module lookup-current-session)))
    (message "Updating %s..." (lookup-module-name module))
    (lookup-module-clear module)
    (lookup-module-init module)
    (lookup-select-dictionary module)
    (message "Updating %s...done" (lookup-module-name module))))

(defun lookup-select-help ()
  "Select モードの簡易ヘルプを表示する。"
  (interactive)
  (with-current-buffer (lookup-open-buffer (lookup-help-buffer))
    (help-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert lookup-select-mode-help))
    (lookup-display-help (current-buffer))))

;;;
;:: Internal functions
;;;

(defun lookup-select-goto-first ()
  (goto-char (point-min))
  (forward-line 4))

(defun lookup-select-point-dictionary ()
  (save-excursion
    (beginning-of-line)
    (forward-char 2)
    (if (looking-at "[^ ]+") (lookup-get-dictionary (match-string 0)))))

(defun lookup-select-set-selected (value &optional dont-move)
  (let ((dict (lookup-select-point-dictionary)))
    (when dict
      (lookup-dictionary-set-selected dict value)
      (lookup-select-mark (if value ?* ? ) (not dont-move)))))

(defun lookup-select-mark (mark &optional down-after)
  (save-excursion
    (let ((inhibit-read-only t))
      (beginning-of-line)
      (delete-char 1)
      (insert-char mark 1)))
  (if down-after (forward-line)))

(provide 'lookup-select)

;;; lookup-select.el ends here
