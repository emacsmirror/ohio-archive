;;; lookup.el --- Search interface to electronic dictionaries
;; Copyright (C) 1999 Lookup Development Team <lookup@ring.gr.jp>

;; Author: Keisuke Nishida <kei@psn.net>
;; Version: $Id: lookup.el,v 1.7 1999/08/09 07:27:13 tsuchiya Exp $

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

(require 'evi)
(require 'lookup-vars)
(require 'lookup-utils)
(require 'lookup-types)


;;;;;;;;;;;;;;;;;;;;
;: Top-level
;;;;;;;;;;;;;;;;;;;;

;;;
;:: General commands
;;;

;;;###autoload
(defun lookup-version (arg)
  "Display the version string of Lookup.
With prefix argument, insert string at point."
  (interactive "P")
  (let ((version (concat "Lookup " lookup-version)))
    (if arg (insert version) (message version))))

;;;###autoload
(defun lookup (&optional module)
  "Lookup を起動し、設定した辞書のリストを表示する。
既に起動している場合は、最後に検索したときの状態を取り戻す。"
  (interactive (list (if current-prefix-arg (lookup-input-module))))
  (setq module (or module (if lookup-last-session
			      (lookup-session-module lookup-last-session)
			    (lookup-default-module))))
  (let ((history (lookup-module-history module)))
    (if (= (lookup-history-length history) 0)
	(lookup-select-dictionary module)
      (lookup-session-display (lookup-history-ref history)))))

;;;###autoload
(defun lookup-setup ()
  "Lookup の全てのセットアップを行なう。
初期化ファイルの読み込み、エージェントの立ち上げ、辞書の初期化等を実行する。
それ以外は何もしない。"
  (interactive)
  (prog1 (lookup-foreach 'lookup-module-setup (lookup-module-list))
    (lookup-dictionary-alist t))) ; 変数 lookup-dictionary-alist の値を更新しておく

(defun lookup-suspend ()
  "Lookup の各ウィンドウを一時的に閉じる。
変数 `lookup-save-configuration' が non-nil に設定されていた場合、
画面の状態を検索開始時のものに戻す。その後 `\\[lookup]' すると Lookup の
ウィンドウが回復する。"
  (interactive)
  (if (lookup-exclusive-frame-p)
      (delete-frame)
    (lookup-foreach 'lookup-hide-buffer lookup-buffer-list)
    (when (and lookup-save-configuration lookup-window-configuration)
      (set-window-configuration lookup-window-configuration)
      (setq lookup-window-configuration nil))))

(defun lookup-exit (&optional arg)
  "Lookup を終了する。プレフィクスを付けると強制終了。"
  (interactive "P")
  (when (and (or (not (interactive-p))
		 (y-or-n-p "Are you sure to exit Lookup? "))
	     lookup-last-session)
    (if arg
	(progn (setq lookup-last-session nil)
	       (message "OK, you can exit Emacs"))
      (lookup-suspend)
      (lookup-foreach 'lookup-agent-clear lookup-agent-list)
      (lookup-foreach 'lookup-module-clear lookup-module-list)
      (lookup-foreach 'kill-buffer lookup-buffer-list)
      (setq lookup-agent-list nil)
      (setq lookup-agent-alist nil)
      (setq lookup-module-list nil)
      (setq lookup-module-alist nil)
      (setq lookup-dictionary-alist nil)
      (setq lookup-buffer-list nil)
      (setq lookup-default-module nil)
      (setq lookup-current-session nil)
      (setq lookup-last-session nil)
      (when (featurep 'lookup-vse)
	(lookup-entries-cache-clear)
	(lookup-contents-cache-clear)))))

(defun lookup-restart ()
  "Lookup を再起動する。"
  (interactive)
  (if (and (interactive-p)
	   (not (yes-or-no-p "Are you sure to restart Lookup? ")))
      (message nil)
    (lookup-exit)
    (lookup-initialize)
    (lookup)))

(defun lookup-debug ()
  "デバッグモードに入る。"
  (interactive)
  (setq lookup-debug-mode (not lookup-debug-mode))
  (setq debug-on-error lookup-debug-mode)
  (message (if lookup-debug-mode
	       "Lookup debug enabled"
	     "Lookup debug disabled")))

;;;
;:: Search commands
;;;

(defun lookup-pattern-input ()
  (let ((module (if current-prefix-arg (lookup-input-module)))
	(pattern (lookup-input-pattern)))
    (list pattern module)))

;;;###autoload
(defun lookup-pattern (pattern &optional module)
  "Search for the PATTERN."
  (interactive (lookup-pattern-input))
  (lookup-search-pattern (or module (lookup-default-module)) pattern))

;;;###autoload
(defun lookup-pattern-full-screen (pattern &optional module)
  "Search for the PATTERN in full screen."
  (interactive (lookup-pattern-input))
  (let ((lookup-open-function 'lookup-full-screen))
    (lookup-pattern pattern module)))

;;;###autoload
(defun lookup-pattern-other-frame (pattern &optional module)
  "Search for the PATTERN in another frame."
  (interactive (lookup-pattern-input))
  (let ((lookup-open-function 'lookup-other-frame))
    (lookup-pattern pattern module)))

(defun lookup-word-input ()
  (let ((module (if current-prefix-arg (lookup-input-module)))
	(word (lookup-current-word)))
    (list word module)))

;;;###autoload
(defun lookup-word (word &optional module)
  "Search for the word near the cursor."
  (interactive (lookup-word-input))
  (let ((lookup-search-method 'default))
    (lookup-pattern word module)))

;;;###autoload
(defun lookup-word-full-screen (word &optional module)
  "Search for the word near the cursor in full screen."
  (interactive (lookup-word-input))
  (let ((lookup-open-function 'lookup-full-screen))
    (lookup-word word module)))

;;;###autoload
(defun lookup-word-other-frame (word &optional module)
  "Search for the word near the cursor in another frame."
  (interactive (lookup-word-input))
  (let ((lookup-open-function 'lookup-other-frame))
    (lookup-word word module)))

(defun lookup-region-input ()
  (let ((module (if current-prefix-arg (lookup-input-module)))
	(start (mark)) (end (point)) tmp)
    (if (> start end) (setq tmp start start end end tmp))
    (list start end module)))

;;;###autoload
(defun lookup-region (start end &optional module)
  "Search for the region."
  (interactive (lookup-region-input))
  (lookup-word (buffer-substring-no-properties start end) module))

;;;###autoload
(defun lookup-region-full-screen (start end &optional module)
  "Search for the region in full screen."
  (interactive (lookup-region-input))
  (let ((lookup-open-function 'lookup-full-screen))
    (lookup-region start end module)))

;;;###autoload
(defun lookup-region-other-frame (start end &optional module)
  "Search for the region in another frame."
  (interactive (lookup-region-input))
  (let ((lookup-open-function 'lookup-other-frame))
    (lookup-region start end module)))

;;;###autoload
(defun lookup-selection (click)
  "Search for the mouse's selection."
  (interactive "e")
  (lookup-word (current-kill 0 t)))

;;;###autoload
(defun lookup-selection-full-screen (click)
  "Search for the mouse's selection in full screen."
  (interactive "e")
  (let ((lookup-open-function 'lookup-full-screen))
    (lookup-selection click)))

;;;###autoload
(defun lookup-selection-other-frame (click)
  "Search for the mouse's selection in another frame."
  (interactive "e")
  (let ((lookup-open-function 'lookup-other-frame))
    (lookup-selection click)))

;;;###autoload
(defun lookup-secondary (click)
  "Search for the mouse's secondary selection."
  (interactive "e")
  (call-interactively 'mouse-drag-secondary)
  (let ((start (overlay-start mouse-secondary-overlay))
	(end (overlay-end mouse-secondary-overlay)))
    (unless (eq start end)
      (with-current-buffer (window-buffer (posn-window (event-start click)))
	(unwind-protect
	    (lookup-word (buffer-substring-no-properties start end))
	  (delete-overlay mouse-secondary-overlay))))))

;;;###autoload
(defun lookup-secondary-full-screen (click)
  "Search for the mouse's secondary selection in full screen."
  (interactive "e")
  (let ((lookup-open-function 'lookup-full-screen))
    (lookup-secondary click)))

;;;###autoload
(defun lookup-secondary-other-frame (click)
  "Search for the mouse's secondary selection in another frame."
  (interactive "e")
  (let ((lookup-open-function 'lookup-other-frame))
    (lookup-secondary click)))


;;;;;;;;;;;;;;;;;;;;
;: User-visible
;;;;;;;;;;;;;;;;;;;;

;;;
;:: Get object
;;;

(defun lookup-get-module (name)
  "Return the search module named NAME."
  (lookup-assoc-ref (lookup-module-alist) name))

(defun lookup-get-agent (id)
  "Return the search agent had ID."
  (lookup-assoc-ref (lookup-agent-alist) id))

(defun lookup-get-dictionary (id)
  "Return the dictionary had ID."
  (lookup-assoc-ref (lookup-dictionary-alist) id))

;;;
;:: Search functions
;;;

(defvar lookup-input-history nil
  "Lookup の検索パターン入力の履歴。")

(defvar lookup-input-module-history nil
  "Lookup の検索モジュール入力の履歴。")

(defun lookup-input-pattern ()
  "プロンプトを出し、検索パターンの入力を促す。
デフォルトとしてカーソル位置の単語を取り出す。入力された文字列を返す。"
  (let ((default (lookup-current-word)))
    (if (string-equal default "") (setq default nil))
    (lookup-read-string "Look up" nil 'lookup-input-history default t)))

(defun lookup-input-module ()
  "プロンプトを出し、検索モジュールの入力を促す。"
  (lookup-get-module (completing-read "Search module: " (lookup-module-alist)
				      nil t nil lookup-input-module-history)))

(defun lookup-search-pattern (module pattern)
  "検索式 PATTERN を検索し、結果を表示する。"
  (if (> (length pattern) 80)
      (error "Too long search pattern"))
  (if (string-match "\n" pattern)
      (error "Search pattern should be one line"))
  (setq lookup-search-pattern pattern)
  (let ((query (if lookup-search-method
		   (lookup-make-query lookup-search-method pattern)
		 (lookup-parse-pattern pattern))))
    (if (string= (lookup-query-string query) "")
	(error "Specify a search string"))
    (if (or (not (eq (lookup-query-method query) 'text))
	    (eq lookup-search-method 'text)
	    (y-or-n-p "This may cost vast time.  Do you realy search text? "))
	(lookup-search-query module query))))

;;;
;:: Setup functions
;;;

(defun lookup-set-agent-option (id tag value)
  "検索エージェントのオプションをセットする。
ID はエージェントID。TAG はオプションのタグ。VALUE はセットする値。
この関数は `lookup-agent-options-alist' の値を書き換える。"
  (let ((options (lookup-assoc-ref lookup-agent-options-alist id)))
    (setq lookup-agent-options-alist
	  (lookup-assoc-set lookup-agent-options-alist id
			    (lookup-assq-set options tag value)))))

(defun lookup-set-dictionary-option (id tag value)
  "辞書のオプションをセットする。
ID は辞書ID。TAG はオプションのタグ。VALUE はセットする値。
この関数は `lookup-dictionary-options-alist' の値を書き換える。"
  (let ((options (lookup-assoc-ref lookup-dictionary-options-alist id)))
    (setq lookup-dictionary-options-alist
	  (lookup-assoc-set lookup-dictionary-options-alist id
			    (lookup-assq-set options tag value)))))

;;;
;:: Global commands
;;;

;;;###autoload
(defun lookup-module-forward (&optional arg)
  "モジュールを前に切り替える。"
  (interactive "p")
  (let* ((module (if lookup-current-session
		     (lookup-session-module lookup-current-session)
		   (lookup-default-module)))
	 (len (length (lookup-module-list)))
	 (pos (- len (length (memq module lookup-module-list)))))
    (setq pos (% (+ pos (or arg 1)) len))
    (if (< pos 0) (setq pos (+ pos len)))
    (setq module (nth pos lookup-module-list))
    (if (window-live-p lookup-main-window)
	(lookup module))
    (setq lookup-default-module module)
    (message (lookup-module-name module))))

;;;###autoload
(defun lookup-module-backward (&optional arg)
  "モジュールを後に切り替える。"
  (interactive "p")
  (lookup-module-forward (- (or arg 1))))

(defun lookup-history-next (&optional arg)
  "履歴を次に進む。プレフィスクの数だけ進む。"
  (interactive "p")
  (let* ((history (lookup-module-history
		   (lookup-session-module lookup-last-session)))
	 (session (lookup-history-ref history (or arg 1))))
    (cond
     ((eq session 'no-object) (message "No session in history") (ding))
     ((eq session 'first) (message "This is first session") (ding))
     ((eq session 'last) (message "This is last session") (ding))
     (t (lookup-session-display session)
	(message "%d" (lookup-history-position history))))))

(defun lookup-history-previous (&optional arg)
  "履歴を前に戻る。プレフィスクの数だけ戻る。"
  (interactive "p")
  (lookup-history-next (- (or arg 1))))


;;;;;;;;;;;;;;;;;;;;
;: Session Interface
;;;;;;;;;;;;;;;;;;;;

;; Description:
;; 
;; Lookup で "セッション" とは、検索などが行なわれる一回の単位のことを
;; 言う。コマンドの実行、検索式の入力、検索の実行、結果の表示、そして
;; ユーザがそれを見終ってバッファを閉じるまでが一回のセッションである。
;; 
;; セッションは履歴に残されるので、一つのデータタイプを持つ。lookup-types.el
;; の Session の項を参照。現在、セッションには次の種類がある。下記を参照。
;; 
;; 選択セッション - 辞書一覧を表示、選択するセッション。
;; 検索セッション - 検索を行ない結果を表示するセッション。
;; 
;; セッションに入るには、利用するモジュールとセッションのタイプを指定して
;; `lookup-start-session' を実行する。BODY の実行が完了すると、そのセッショ
;; ンを表示してユーザの入力を待つ。BODY の中で `lookup-open-session' を実行
;; することで、処理の途中でセッションを表示することも出来る。
;; 
;; 変数 `lookup-current-session' により、処理中のセッション・オブジェクトを
;; 得ることが出来る。`lookup-last-session' により、最後に実行されたセッション
;; を得ることが出来る。`lookup-last-session' の値は、セッションが表示された
;; 時点で `lookup-current-session' と同じ値にセットされる。

(defmacro lookup-start-session (module type &rest body)
  (` (unwind-protect
	 (let ((lookup-current-session (lookup-make-session (, module) (, type))))
	   (lookup-module-setup (, module))
	   (lookup-dictionary-alist t) ; 変数 lookup-dictionary-alist の値を更新しておく
	   (,@ body)
	   (unless (eq lookup-last-session lookup-current-session)
	     (lookup-open-session)))
       ;; セッションの途中でエラーが発生したときは最後のセッションに戻す。
       (setq lookup-current-session lookup-last-session))))

(defun lookup-open-session ()
  (lookup-session-display lookup-current-session)
  (lookup-history-push (lookup-module-history
			(lookup-session-module lookup-current-session))
		       lookup-current-session))

;;;
;:: Select session
;;;

(put 'lookup-select-session 'display 'lookup-select-display)

(defun lookup-select-dictionary (module)
  (lookup-start-session module 'lookup-select-session))

;;;
;:: Search session
;;;

(put 'lookup-search-session 'display 'lookup-entry-display)
(put 'lookup-search-session 'excursion 'lookup-entry-excursion)

(defun lookup-search-query (module query)
  (let* ((lookup-proceeding-message
	  (format "Looking up `%s'" lookup-search-pattern))
	 (lookup-dynamic-display t)
	 (lookup-search-found nil)
	 (input-method (lookup-query-method query))
	 (method input-method)
	 valid-dictionary entries)
    (lookup-proceeding-message nil)
    (lookup-start-session module 'lookup-search-session
      (lookup-foreach
       (lambda (dictionary)
	 (when (lookup-dictionary-selected-p dictionary)
	   (let ((methods (lookup-dictionary-methods dictionary)))
	     (when (eq input-method 'default)
	       (setq method (lookup-dictionary-default-method dictionary))
	       (lookup-query-set-method query method))
	     (when (memq method methods)
	       (setq valid-dictionary t)
	       (lookup-proceeding-message
		(format "by %s..." (lookup-dictionary-title dictionary)))
	       (setq entries (lookup-vse-search-query dictionary query))
	       (when (and (not entries) (eq input-method 'default)
			  (memq 'stemming methods))
		 (lookup-query-set-method query 'stemming)
		 (setq entries (lookup-vse-search-query dictionary query)))
	       (when (and (not entries) (eq input-method 'default)
			  (memq 'expansion methods))
		 (lookup-query-set-method query 'expansion)
		 (setq entries (lookup-vse-search-query dictionary query)))
	       (when entries
		 (if lookup-search-found
		     (lookup-entry-append lookup-current-session entries)
		   (setq lookup-search-found t)
		   (lookup-session-set-query lookup-current-session query)
		   (lookup-session-set-entries lookup-current-session entries)
		   (lookup-open-session)))))))
       (lookup-module-dictionaries module))
      (cond ((not valid-dictionary)
	     (error "No valid dictionary for method: %S" method))
	    ((not lookup-search-found)
	     (error "No entry for `%s'" lookup-search-pattern))))
    (lookup-proceeding-message t)))

(defun lookup-display-entries (module query entries)
  (lookup-start-session module 'lookup-search-session
    (lookup-session-set-query lookup-current-session query)
    (lookup-session-set-entries lookup-current-session entries)))


;;;;;;;;;;;;;;;;;;;;
;: Buffer Functions
;;;;;;;;;;;;;;;;;;;;

;;;
;:: Lookup buffers
;;;

(defconst lookup-entry-buffer " *Entry*")
(defconst lookup-content-buffer " *Content*")
(defconst lookup-select-buffer " *Dictionary List*")
(defconst lookup-help-buffer "*Lookup Help*")

(defsubst lookup-entry-buffer () lookup-entry-buffer)
(defsubst lookup-content-buffer () lookup-content-buffer)
(defsubst lookup-select-buffer () lookup-select-buffer)
(defsubst lookup-help-buffer () lookup-help-buffer)

(defsubst lookup-temp-buffer ()
  (generate-new-buffer " *Lookup temp buffer*"))

(defsubst lookup-open-process-buffer (name)
  (if lookup-debug-mode (generate-new-buffer name)))

;;;
;:: Buffer utils
;;;

(defvar lookup-start-window nil)
(defvar lookup-main-window nil)
(defvar lookup-sub-window nil)

(defun lookup-open-buffer (name)
  ;; バッファ NAME を生成して返す。
  ;; 生成したバッファは変数 `lookup-buffer-list' に登録される。
  (let ((buffer (get-buffer-create name)))
    (if (not (memq buffer lookup-buffer-list))
	(setq lookup-buffer-list (cons buffer lookup-buffer-list)))
    buffer))

(defun lookup-hide-buffer (buffer)
  ;; バッファ BUFFER を隠す。バッファがウィンドウを持つ場合、それも閉じる。
  (let ((window (get-buffer-window buffer)))
    (when window
      (if (eq window lookup-main-window) (setq lookup-main-window nil))
      (if (eq window lookup-sub-window) (setq lookup-sub-window nil))
      (if (> (count-windows) 1)
	  (delete-window window)
	(switch-to-buffer (other-buffer)))))
  (if (window-live-p lookup-main-window)
      (select-window lookup-main-window))
  (bury-buffer buffer))

(defun lookup-pop-to-buffer (buffer)
  (if (and (window-live-p lookup-main-window)
	   (if (fboundp 'frame-visible-p)
	       (frame-visible-p (window-frame lookup-main-window))))
      (progn
	(set-window-buffer (select-window lookup-main-window) buffer)
	(raise-frame (window-frame lookup-main-window)))
    (setq lookup-start-window (selected-window))
    (if lookup-save-configuration
	(setq lookup-window-configuration (current-window-configuration)))
    (funcall lookup-open-function buffer)
    (setq lookup-main-window (get-buffer-window buffer t)))
  (if (window-live-p lookup-sub-window)
      (delete-window lookup-sub-window))
  buffer)

(defun lookup-display-buffer (buffer)
  (if (window-live-p lookup-sub-window)
      (set-window-buffer lookup-sub-window buffer)
    (when (or (eq (count-windows) 1) (eq (next-window) lookup-start-window))
      (let ((height (if (integerp lookup-window-height)
			lookup-window-height
		      (round (* (window-height) lookup-window-height)))))
	(split-window lookup-main-window (1+ height))))
    (let ((pop-up-frames nil))
      (display-buffer buffer))
    (setq lookup-sub-window (get-buffer-window buffer)))
  buffer)

(defun lookup-display-help (buffer)
  (with-current-buffer buffer (goto-char (point-min)))
  (if (window-live-p lookup-start-window)
      (set-window-buffer lookup-start-window buffer)
    (display-buffer buffer)))

(defun lookup-full-screen (buffer)
  (delete-other-windows)
  (switch-to-buffer buffer))

(defun lookup-other-window (buffer)
  (let ((pop-up-windows t)
	(pop-up-frames nil))
    (pop-to-buffer buffer)))

(defun lookup-other-frame (buffer)
  (let ((pop-up-frames t)
	(default-frame-alist (cons '(name . "Lookup") lookup-frame-alist)))
    (pop-to-buffer buffer)))

(defun lookup-exclusive-frame-p ()
  ;; Lookup の専用フレームを用いている場合 t を返す。
  (string= (frame-parameter (selected-frame) 'name) "Lookup"))


;;;
;:: Internal functions
;;;

(defconst lookup-obarray (make-vector 1511 nil))

(defsubst lookup-intern-string (string)
  (symbol-name (intern string lookup-obarray)))

(defun lookup-proceeding-message (work)
  (when lookup-proceeding-message
    (let ((message (concat lookup-proceeding-message "...")))
      (cond
       ((not work) (message "%s" message))
       ((eq work t) (message "%s" (concat message "done")))
       (t (message "%s" (concat message " (" work ")")))))))

(eval-when-compile
  (defvar lookup-logo-file (if (featurep 'xemacs)
                               "lookup-logo.xpm"
                             "lookup-logo.xbm"))
  (defmacro logo-image ()
    (with-temp-buffer
      (cond ((featurep 'xemacs)
	     (insert-file-contents lookup-logo-file))
	    ((locate-library "bitmap")
	     (require 'bitmap)
	     (bitmap-insert-xbm-file lookup-logo-file)
	     (put-text-property (point-min) (point-max)
				'face 'lookup-splash-face))
	    (t ))
      (buffer-substring (point-min) (point-max)))))

(defconst lookup-logo-image (logo-image))

(defun lookup-splash ()
  (interactive)
  (when (and lookup-enable-splash window-system
	     (or lookup-use-bitmap (featurep 'xemacs)))
    (let ((buffer (get-buffer-create "*Lookup Logo*")))
      (set-buffer buffer)
      (erase-buffer)
      (if (featurep 'xemacs)
	  (let ((logo-glyph (make-glyph (vector 'xpm :data lookup-logo-image)))
		logo-ext)
	    (indent-to (startup-center-spaces logo-glyph))
	    (setq logo-ext (make-extent (point) (point)))
	    (set-extent-end-glyph logo-ext logo-glyph))
	(center-region (point-min)(point-max))
	(insert lookup-logo-image)
	(goto-char (1- (point-max)))
	(indent-rigidly (point-min) (point-max)
			(max 0 (/ (- (window-width) (current-column)) 2))))
      (goto-char (point-max))
      (let((pt (point))
           (line (count-lines (point-min)(point-max))))
        (insert (format "\nLookup %s\n\n" lookup-version))
        (insert "Copyright (C) 2000 Lookup Development Team")
        (insert "\n<lookup@ring.gr.jp>\n")
        (center-region pt (point-max))
        (goto-char (point-min))
        (insert-char ?\n 
		     (max 1 (+ -5 (/ (- (window-height) line) 2)))))
      (switch-to-buffer buffer)
      (sit-for 3)
      (kill-buffer buffer))))


;;;
;:: Lookup Initialize
;;;

(defun lookup-initialize ()
  (load lookup-init-file t)
  (run-hooks 'lookup-load-hook)
  (lookup-splash)
  (lookup-init-gaiji-functions))

(defun lookup-agent-list ()
  (or lookup-agent-list
      (setq lookup-agent-list
	    (mapcar 'lookup-new-agent lookup-search-agents))))

(defun lookup-agent-alist ()
  (or lookup-agent-alist
      (setq lookup-agent-alist (mapcar (lambda (agent)
					 (cons (lookup-agent-id agent) agent))
				       (lookup-agent-list)))))

(defun lookup-module-list ()
  (or lookup-module-list
      (setq lookup-module-list
	    (mapcar 'lookup-new-module (or lookup-search-modules
					   '(("%EVERY" "")))))))

(defun lookup-module-alist ()
  (or lookup-module-alist
      (setq lookup-module-alist (mapcar (lambda (mod)
					  (cons (lookup-module-name mod) mod))
					(lookup-module-list)))))

(defun lookup-dictionary-alist (&optional reset)
  (or (and (not reset) lookup-dictionary-alist)
      (setq lookup-dictionary-alist
	    (mapcar (lambda (dict) (cons (lookup-dictionary-id dict) dict))
		    (apply 'append (mapcar 'lookup-agent-dictionaries
					   (lookup-agent-list)))))))

(defun lookup-default-module ()
  (or lookup-default-module
      (setq lookup-default-module (car (lookup-module-list)))))

;;;
;:: Provide Lookup
;;;

(autoload 'lookup-vse-get-menu "lookup-vse")
(autoload 'lookup-vse-search-query "lookup-vse")
(autoload 'lookup-select-display "lookup-select")
(autoload 'lookup-entry-display "lookup-entry")
(autoload 'lookup-use-package "lookup-package")
(autoload 'lookup-make-url-reference "ndmisc")
(autoload 'stem-english "stem-english")
(add-hook 'kill-emacs-hook 'lookup-exit)

(provide 'lookup)

(unless lookup-byte-compile
  (lookup-initialize))

;;; lookup.el ends here
