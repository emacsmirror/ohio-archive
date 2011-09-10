;;; lookup-entry.el --- lookup-entry-mode
;; Copyright (C) 1999 Lookup Development Team <lookup@ring.gr.jp>

;; Author: Keisuke Nishida <kei@psn.net>
;; Version: $Id: lookup-entry.el,v 1.5 2000/01/05 07:37:22 tsuchiya Exp $

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
(require 'lookup-content)

;;;
;:: Internal variables
;;;

(defvar lookup-entry-line-pattern "")
(defvar lookup-entry-line-number "0")

(make-variable-buffer-local 'lookup-entry-line-pattern)
(make-variable-buffer-local 'lookup-entry-line-number)

(defvar lookup-entry-insert-format "%-19s ")

;;;;;;;;;;;;;;;;;;;;
;: Construct Buffer
;;;;;;;;;;;;;;;;;;;;

(defun lookup-entry-display (session)
  (with-current-buffer (lookup-open-buffer (lookup-entry-buffer))
    (let ((query (lookup-session-query session))
	  (entries (lookup-session-entries session))
	  (excursion (lookup-session-excursion session)))
      ;; insert entries
      (let ((inhibit-read-only t))
	(lookup-entry-mode)
	(erase-buffer)
	(lookup-foreach 'lookup-entry-insert entries))
      ;; set mode line
      (setq lookup-entry-line-pattern (lookup-query-string query))
      (setq lookup-entry-line-number (number-to-string (length entries)))
      ;; display buffer
      (if excursion
	  (lookup-entry-set-excursion excursion)
	(lookup-pop-to-buffer (current-buffer))
	(goto-char (point-min))
	(lookup-entry-goto-link)
	(if lookup-dynamic-display (sit-for 0))
	(lookup-entry-display-content)
	(if lookup-dynamic-display (sit-for 0))))))

(defun lookup-entry-append (session entries)
  (with-current-buffer (lookup-entry-buffer)
    (goto-char (prog1 (point)
		 (let ((inhibit-read-only t))
		   (goto-char (point-max))
		   (lookup-foreach 'lookup-entry-insert entries))))
    (let ((entries (append (lookup-session-entries session) entries)))
      (lookup-session-set-entries session entries))
    (setq lookup-entry-line-number
	  (number-to-string (+ (string-to-number lookup-entry-line-number)
			       (length entries))))
    (if lookup-dynamic-display (sit-for 0))))

(defun lookup-entry-expand-reference (reference)
  (let ((entries (lookup-reference-entries reference)))
    (when entries
      ;; replace buffer
      (let ((start (progn (beginning-of-line) (point)))
	    (inhibit-read-only t))
	(delete-region start (progn (forward-line) (point)))
	(lookup-foreach 'lookup-entry-insert entries)
	(goto-char start)
	(lookup-entry-goto-link))
      ;; replace cache
      (let ((list (lookup-session-entries lookup-current-session)))
	(if (eq reference (car list))
	    (setq list (append entries (cdr list)))
	  (while (not (eq reference (cadr list))) (setq list (cdr list)))
	  (when list
	    (setcdr list (append entries (cddr list))))))
      (setq lookup-entry-line-number
	    (number-to-string (+ (string-to-number lookup-entry-line-number)
				 (1- (length entries))))))))

(defun lookup-entry-insert (entry)
  ;; エントリ行をバッファに挿入し、リンクをセットする。
  (let ((dictionary (lookup-entry-dictionary entry))
	(prefix (lookup-entry-prefix entry))
	(heading (lookup-entry-heading entry))
	start end)
    (insert (format lookup-entry-insert-format (lookup-dictionary-title dictionary)))
    ;; property や extent を次の行にまたがないように先に newline しておく。
    (newline)
    (backward-char)
    (setq start (point)
	  end (progn (if prefix (insert prefix)) (insert heading) (point)))
    (lookup-entry-set-link start end entry)
    (lookup-map-over-property start end
			      'lookup-gaiji 'lookup-gaiji-glyph-paste)
    (forward-char)))

;; Excursion:

(defun lookup-entry-excursion ()
  (let ((entry (get-buffer (lookup-entry-buffer)))
	(content (get-buffer (lookup-content-buffer))))
    (when entry
      (cons (with-current-buffer entry
	      (cons (point) (let ((window (get-buffer-window entry)))
			      (if window (window-start window)))))
	    (when (and content (with-current-buffer entry
				 (lookup-entry-current-line-entry)))
	      (with-current-buffer content
		(cons (point) (let ((window (get-buffer-window content)))
				(if window (window-start window))))))))))

(defun lookup-entry-set-excursion (excursion)
  (let ((entry-point (caar excursion)) (entry-start (cdar excursion))
	(content (cdr excursion)))
    (lookup-pop-to-buffer (lookup-entry-buffer))
    (goto-char entry-point)
    (if entry-start
	(set-window-start (selected-window) entry-start))
    (lookup-entry-display-content)
    (when content
      (lookup-with-buffer-and-window (lookup-content-buffer)
	(goto-char (car content))
	(if (cdr content)
	    (set-window-start (selected-window) (cdr content)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;:  Lookup Entry mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar lookup-entry-mode-map nil
  "*Keymap for Lookup Entry mode.")

(unless lookup-entry-mode-map
  (setq lookup-entry-mode-map (make-sparse-keymap))
  (define-key lookup-entry-mode-map " " 'lookup-entry-next-page)
  (define-key lookup-entry-mode-map "\C-?" 'lookup-entry-previous-page)
  (define-key lookup-entry-mode-map [delete] 'lookup-entry-previous-page)
  (define-key lookup-entry-mode-map [backspace] 'lookup-entry-previous-page)
  (define-key lookup-entry-mode-map "\C-m" 'lookup-entry-scroll-up-content)
  (define-key lookup-entry-mode-map "<" 'lookup-entry-beginning-of-content)
  (define-key lookup-entry-mode-map ">" 'lookup-entry-end-of-content)
  (define-key lookup-entry-mode-map "n" 'lookup-entry-next-entry)
  (define-key lookup-entry-mode-map "p" 'lookup-entry-previous-entry)
  (define-key lookup-entry-mode-map "\en" 'lookup-history-next)
  (define-key lookup-entry-mode-map "\ep" 'lookup-history-previous)
  (define-key lookup-entry-mode-map "\ef" 'lookup-module-forward)
  (define-key lookup-entry-mode-map "\eb" 'lookup-module-backward)
  (define-key lookup-entry-mode-map "i" 'lookup-entry-info)
  (define-key lookup-entry-mode-map "o" 'lookup-entry-open)
  (define-key lookup-entry-mode-map "O" 'lookup-entry-open-other)
;  (define-key lookup-entry-mode-map "e" 'lookup-entry-edit)
  (define-key lookup-entry-mode-map "v" 'lookup-entry-overview-mode)
  (define-key lookup-entry-mode-map "t" 'lookup-entry-toggle-format)
  (define-key lookup-entry-mode-map "s" 'lookup-entry-isearch-content)
  (define-key lookup-entry-mode-map "w" 'lookup-entry-cite-content)
  (define-key lookup-entry-mode-map "f" 'lookup-entry-search-pattern)
;  (define-key lookup-entry-mode-map "I" 'lookup-entry-show-index)
  (define-key lookup-entry-mode-map "M" 'lookup-entry-show-menu)
  (define-key lookup-entry-mode-map "L" 'lookup-entry-list-references)
;  (define-key lookup-entry-mode-map "H" 'lookup-entry-list-history)
  (define-key lookup-entry-mode-map "S" 'lookup-entry-select-dictionary)
  (define-key lookup-entry-mode-map "r" 'lookup-entry-start-window)
  (define-key lookup-entry-mode-map "h" 'lookup-entry-content-window)
  (define-key lookup-entry-mode-map "g" 'lookup-entry-update)
  (define-key lookup-entry-mode-map "q" 'lookup-suspend)
  (define-key lookup-entry-mode-map "Q" 'lookup-exit)
  (define-key lookup-entry-mode-map "R" 'lookup-restart)
  (define-key lookup-entry-mode-map "?" 'lookup-entry-help)
  (if (featurep 'xemacs)
      (define-key lookup-entry-mode-map 'button2 'lookup-entry-mouse-follow)
    (define-key lookup-entry-mode-map [mouse-2] 'lookup-entry-mouse-follow))
  )

(defconst lookup-entry-mode-help
  "Lookup Entry モード:

`n'(ext)     - 次のエントリへ   `M-n' - 検索履歴を次へ
`p'(revious) - 前のエントリへ   `M-p' - 検索履歴を前へ

`v'(iew)   - Overview モード  	`o'(pen)  - 画面を最大化
`s'(earch) - isearch-forward  	`i'(nfo)  - エントリの情報表示
`t'(oggle) - 整形処理をトグル 	`w'(rite) - 内容をキルリングに保存
`f'(ind)   - 検索語を入力

`M'(enu)   - 辞書のメニュー表示
`L'(inks)  - リファレンスを一覧 `S'(elect) - 辞書選択モード

`r'   - 検索開始バッファに移動  `h'   - Content バッファに移動
`q'   - バッファを抜ける        `g'   - 検索をやり直す
`Q'   - Lookup を終了する       `R'   - Lookup を再起動する")

(defvar lookup-entry-overview-mode nil)

(make-variable-buffer-local 'lookup-entry-overview-mode)
(or (assq 'lookup-entry-overview-mode minor-mode-alist)
    (setq minor-mode-alist (cons '(lookup-entry-overview-mode " Overview")
				 minor-mode-alist)))

(defvar lookup-entry-overview-mode-map nil
  "*Keymap for Lookup Entry Overview mode.")

(unless lookup-entry-overview-mode-map
  (setq lookup-entry-overview-mode-map (make-sparse-keymap))
  (define-key lookup-entry-overview-mode-map "n" 'next-line)
  (define-key lookup-entry-overview-mode-map "p" 'previous-line))

(or (assq 'lookup-entry-overview-mode minor-mode-map-alist)
    (setq minor-mode-map-alist (cons (cons 'lookup-entry-overview-mode
					   lookup-entry-overview-mode-map)
				     minor-mode-map-alist)))

(defun lookup-entry-mode ()
  "Lookup Entry モード。
これは検索の結果、見付かったエントリを一覧するためのモード。


モードに入るときに `lookup-entry-mode-hook' が呼ばれる。"
  (interactive)
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq major-mode 'lookup-entry-mode)
  (setq mode-name "Entry")
  (setq mode-line-buffer-identification
	'("Lookup:%b {" lookup-entry-line-pattern "} ["
	  lookup-entry-line-number "]"))
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (use-local-map lookup-entry-mode-map)
  (if (and (featurep 'xemacs) (featurep 'scrollbar))
      (set-specifier scrollbar-height (cons (current-buffer) 0)))
  (run-hooks 'lookup-entry-mode-hook))

(defun lookup-entry-overview-mode (&optional arg)
  "Overview モードに入る。
Cotent バッファがクローズされ、`n' と `p' が単にポイント移動だけを
行なうようになる。"
  (interactive)
  (setq lookup-entry-overview-mode (if arg (> (prefix-numeric-value arg) 0)
				     (not lookup-entry-overview-mode)))
  (if lookup-entry-overview-mode
      (lookup-hide-buffer (lookup-content-buffer))
    (lookup-display-buffer (lookup-content-buffer)))
  (force-mode-line-update))

;;;
;:: Interactive commands
;;;

(defun lookup-entry-display-content ()
  "ポイント行のエントリの本文を表示する。
Overview モードになっている場合にはそれを解除し、Content バッファを
オープンする。エントリがリファレンスの場合には、それを参照する。"
  (interactive)
  (lookup-entry-goto-link)
  (let ((entry (lookup-entry-current-line-entry)))
    (when (lookup-reference-p entry)
      (lookup-reference-refer entry)
      (lookup-entry-expand-reference entry)
      ;; dynamic reference を参照した場合、バッファが書き換わることがある。
      (setq entry (lookup-entry-current-line-entry)))
    (when entry
      (lookup-content-display entry)
      (lookup-entry-overview-mode 0))))

(defun lookup-entry-mouse-follow (event)
  "マウスでクリックしたエントリの本文を表示する。"
  (interactive "e")
  (mouse-set-point event)
  (lookup-entry-display-content))

(defun lookup-entry-next-page ()
  "エントリ本文の表示を一ページ進める。
バッファの終わりまで達したら、次のエントリに移動する。"
  (interactive)
  (cond
   ((not (lookup-entry-current-line-entry)) nil)
   ((not (lookup-entry-content-visible-p)) (lookup-entry-display-content))
   ((lookup-with-buffer-and-window (lookup-content-buffer)
      (not (pos-visible-in-window-p (point-max) (selected-window))))
    (lookup-entry-scroll-up-content))
   (t (lookup-entry-next-entry))))

(defun lookup-entry-previous-page ()
  "エントリ本文の表示を一ページ戻す。
バッファの始めまで達したら、前のエントリに移動する。"
  (interactive)
  (cond
   ((not (lookup-entry-current-line-entry)) (lookup-entry-previous-entry))
   ((not (lookup-entry-content-visible-p)) (lookup-entry-display-content))
   ((lookup-with-buffer-and-window (lookup-content-buffer)
      (not (pos-visible-in-window-p (point-min) (selected-window))))
    (lookup-entry-scroll-down-content))
   (t (lookup-entry-previous-entry))))

(defun lookup-entry-scroll-up-content (&optional arg)
  "エントリ本文をプレフィスクの行数だけスクロール・アップする。"
  (interactive "p")
  (if (lookup-entry-content-visible-p)
      (lookup-with-buffer-and-window (lookup-content-buffer)
	(scroll-up arg))
    (lookup-entry-display-content)))

(defun lookup-entry-scroll-down-content (&optional arg)
  "エントリ本文をプレフィスクの行数だけスクロール・ダウンする。"
  (interactive "p")
  (if (lookup-entry-content-visible-p)
      (lookup-with-buffer-and-window (lookup-content-buffer)
	(scroll-down arg))
    (lookup-entry-display-content)))

(defun lookup-entry-beginning-of-content ()
  "エントリ本文の表示を先頭まで戻す。"
  (interactive)
  (if (lookup-entry-content-visible-p)
      (lookup-with-buffer-and-window (lookup-content-buffer)
	(goto-char (point-min)))
    (lookup-entry-display-content)))

(defun lookup-entry-end-of-content ()
  "エントリ本文の表示を末尾まで進める。"
  (interactive)
  (if (lookup-entry-content-visible-p)
      (lookup-with-buffer-and-window (lookup-content-buffer)
	(goto-char (point-max))
	(recenter -2))
    (lookup-entry-display-content)))

(defun lookup-entry-next-entry (&optional arg)
  "次のエントリを表示する。プレフィクスの数だけ進む。"
  (interactive "p")
  (if (eobp)
      (progn (message "End of buffer") (ding))
    (forward-line arg)
    (lookup-entry-goto-link)
    (or (pos-visible-in-window-p (save-excursion (forward-line) (point)))
	(recenter -2))
    (lookup-entry-display-content)))

(defun lookup-entry-previous-entry (&optional arg)
  "前のエントリを表示する。プレフィクスの数だけ戻る。"
  (interactive "p")
  (beginning-of-line)
  (if (bobp)
      (progn (message "Beginning of buffer") (ding))
    (forward-line (- (or arg 1)))
    (lookup-entry-goto-link)
    (or (pos-visible-in-window-p (save-excursion (forward-line -1) (point)))
	(recenter 1))
    (lookup-entry-display-content)))

(defun lookup-entry-info ()
  "エントリの情報を出力する。"
  (interactive)
  (let ((entry (lookup-entry-current-line-entry)))
    (with-current-buffer (lookup-open-buffer "*Entry Information*")
      (help-mode)
      (let ((inhibit-read-only t))
	(erase-buffer)
	(insert (format "Entry information for `%s':\n"
			(lookup-entry-heading entry)))
	(newline)
	(insert (format "Dictionary: %s\n" (lookup-dictionary-id
					    (lookup-entry-dictionary entry))))
	(insert (format "Heading:    %s\n" (lookup-entry-heading entry)))
	(insert (format "Code:       %s\n" (lookup-entry-code entry)))
	(goto-char (point-min))
	(forward-line 2))
      (lookup-display-buffer (current-buffer)))))

(defun lookup-entry-open ()
  "エントリ本文を画面一杯に表示する。"
  (interactive)
  (delete-other-windows)
  (lookup-entry-display-content))

(defun lookup-entry-open-other ()
  "エントリ本文を別プログラムで表示する。"
  (interactive)
  (unless (lookup-vse-open-entry (lookup-entry-current-line-entry))
    (error "This entry doesn't have a open command")))

(defun lookup-entry-toggle-format ()
  "エントリ本文の整形処理をトグルする。"
  (interactive)
  (with-current-buffer (lookup-content-buffer)
    (lookup-content-toggle-format)))

(defun lookup-entry-isearch-content (&optional rexexp-p)
  "Content バッファで isearch-forward を実行する。"
  (interactive "P")
  (lookup-with-buffer-and-window (lookup-content-buffer)
    (isearch-forward rexexp-p)))

(defconst lookup-entry-default-policies
  '((asis . ((gaiji . glyph)))
    (plain . ((gaiji . alternate)))))

(defun lookup-entry-cite-content ()
  "エントリ本文をキルリングに保存する。
See also `lookup-content-cite-region'."
  (interactive)
  (unless (lookup-entry-content-visible-p)
    (lookup-entry-display-content))
  (with-current-buffer (lookup-content-buffer)
    (lookup-content-cite-region (point-max) (point-min)))
  (when (interactive-p)
    (message "Saved text for `%s'"
	     (lookup-entry-heading (lookup-entry-current-line-entry)))))

(defun lookup-entry-search-pattern (pattern)
  "ミニバッファから検索式を入力して検索する。"
  (interactive
   (list (lookup-read-string "Look up" nil 'lookup-input-history
			     (lookup-query-string
			      (lookup-session-query lookup-last-session)))))
  (lookup-search-pattern (lookup-session-module lookup-last-session) pattern))

(defun lookup-entry-show-menu ()
  "辞書がメニューに対応している場合、それを参照する。"
  (interactive)
  (let* ((dict (lookup-entry-dictionary (lookup-entry-current-line-entry)))
	 (entries (lookup-vse-get-menu dict)))
    (if entries
	(let* ((module (lookup-session-module lookup-current-session))
	       (title (lookup-dictionary-title dict))
	       (query (lookup-make-query 'reference title)))
	  (lookup-display-entries module query entries))
      (error "This dictionary has no menu"))))

(defun lookup-entry-list-references ()
  "エントリ本文に含まれるリファレンスを一覧する。"
  (interactive)
  (unless (lookup-entry-content-visible-p)
    (lookup-entry-display-content))
  (let ((entries (lookup-content-collect-references)))
    (if entries
	(let* ((module (lookup-session-module lookup-current-session))
	       (entry (lookup-entry-current-line-entry))
	       (heading (lookup-entry-heading entry))
	       (query (lookup-make-query 'reference heading)))
	  (lookup-display-entries module query entries))
      (error "No valid reference in current content"))))

(defun lookup-entry-select-dictionary ()
  "辞書選択バッファに移動。"
  (interactive)
  (lookup-select-dictionary (lookup-session-module lookup-current-session)))

(defun lookup-entry-start-window ()
  "検索を開始したウィンドウに移動する。"
  (interactive)
  (if (window-live-p lookup-start-window)
      (select-window lookup-start-window)
    (lookup-hide-buffer (current-buffer))
    (delete-other-windows)
    (let ((lookup-open-function 'lookup-other-window))
      (lookup))))

(defun lookup-entry-content-window ()
  "Content バッファに移動する。"
  (interactive)
  (unless (lookup-entry-content-visible-p)
    (lookup-entry-display-content))
  (select-window (get-buffer-window (lookup-content-buffer))))

(defun lookup-entry-update (&optional module)
  "今回と同じ検索語で、エントリを再検索する。"
  (interactive (list (if current-prefix-arg (lookup-input-module))))
  (let ((query (lookup-session-query lookup-last-session)))
    (if (eq (lookup-query-method query) 'reference)
	(error "This is reference session")
      (let ((lookup-force-update t))
	(setq module (or module (lookup-session-module lookup-last-session)))
	(lookup-search-query module query)))))

(defun lookup-entry-update-content ()
  "エントリ本文を再表示する。整形処理も全てやり直される。"
  (interactive)
  (let ((lookup-force-update t))
    (lookup-entry-display-content)))

(defun lookup-entry-help ()
  "Entry モードの簡易ヘルプを表示する。"
  (interactive)
  (with-current-buffer (lookup-open-buffer (lookup-help-buffer))
    (help-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert lookup-entry-mode-help))
    (lookup-display-help (current-buffer))))

;;;
;:: Internal functions
;;;

(defun lookup-entry-set-link (start end entry)
  ;; バッファの START から END までのリージョンを ENTRY へのリンクにする。
  (add-text-properties start end (list 'mouse-face 'highlight
				       'lookup-entry entry)))

(defun lookup-entry-goto-link ()
  ;; ポイント行のリンク位置に移動する。
  (let ((p (progn (beginning-of-line) (point))))
    (if (setq p (next-single-property-change p 'lookup-entry))
	(goto-char p))))

(defun lookup-entry-current-line-entry ()
  ;; ポイント行の entry を返す。
  (save-excursion
    (end-of-line)
    (get-text-property (1- (point)) 'lookup-entry)))

(defun lookup-entry-content-visible-p ()
  ;; content が表示されていれば `t'。
  (and (get-buffer-window (lookup-content-buffer))
       (eq (lookup-entry-current-line-entry)
	   (with-current-buffer (lookup-content-buffer)
	     lookup-content-current-entry))))

(provide 'lookup-entry)

;;; lookup-entry.el ends here
