;;;
;;; mew-petname.el by Junichiro Kita (喜多淳一郎) <kita@sec.rd.nttdata.co.jp>
;;;

;; [How to use]
;;     (add-hook 'mew-summary-mode-hook
;;               (function
;;                (lambda ()
;;                  (define-key mew-summary-mode-map "\M-p"
;;                    'mew-summary-petname-save-new-petname))))
;;     (autoload 'mew-summary-petname-save-new-petname "mew-petname" nil t)

(if (fboundp 'mew-toggle-kanji)
    nil
  (cond
   ((boundp 'WNN)
    (fset 'mew-toggle-kanji 'toggle-egg-mode))
   ((boundp 'CANNA)
    (fset 'mew-toggle-kanji 'canna-toggle-japanese-mode))
   ((eq system-type 'windows-nt)
    (fset 'mew-toggle-kanji 'win32-ime-toggle))
   (t          ; 他のは知りません
    (fset 'mew-toggle-kanji '(lambda () nil)))))

(defun mew-summary-petname-save-new-petname ()
  (interactive)
  (cond
   ((eobp)
    (message "No message"))
   ((not (or (mew-summary-message-number) (mew-syntax-number)))
    (message "No message"))
   (t
    (let (msg from petname oldpetname replace)
      ;; とにかく save-excursion
      (save-excursion
	(if (mew-syntax-number)
	    (re-search-backward mew-summary-message-regex nil t nil))
	;; メッセージを表示させる
	(mew-summary-display t)
	(setq msg (mew-summary-message-number))
	(set-buffer (or (mew-cache-hit (cons (buffer-name) msg))
			(mew-buffer-message)))
	;; From:
	(setq from (mew-header-parse-address "From:"))
	;; Petname が登録されていたら
	(and (setq oldpetname (cdr (mew-assoc-case-equal
				    from mew-petname-alist 0)))
	     (setq replace (y-or-n-p 
			    (format "Petname \"%s\" already exists. Replace?"
				    oldpetname))))
	(if (and oldpetname (not replace))
	    nil
	  (let (minibuffer-setup-hook)
	    ;; 普通 petname は漢字でしょ
	    (add-hook 'minibuffer-setup-hook 'mew-toggle-kanji)
	    (setq petname (read-from-minibuffer
			   (format "Petname for \"%s\": " from))))
	  (cond
	   ((and 
	     (y-or-n-p (format "Petname %s for \"%s\", correct and save?"
			       petname from))
	     ;; mew-petname-alist に登録
	     (setq mew-petname-alist
		   (cons (cons from petname) mew-petname-alist))
	     ;; mew-petname-file にセーブ
	     (let ((buffer (find-file mew-petname-file)))
	       (set-buffer buffer)
	       (if (not replace)
		   (insert (format "%s\t\"%s\"\n" from petname))
		 (perform-replace oldpetname petname nil nil nil))
	       (save-buffer)
	       (kill-buffer buffer)))))))))))
