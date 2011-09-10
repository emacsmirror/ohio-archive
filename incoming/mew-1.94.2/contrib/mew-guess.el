;;; mew-guess.el --- Guess header and template file in draft for Mew

;; Author:  OBATA Noboru <obata@nippon-control-system.co.jp>
;; Created: Mar 22, 1999
;; Revised: Aug 31, 1999

;;; Commentary:

;; Shun-ichi GOTO <gotoh@taiyo.co.jp> さんに感謝します。

;; このパッケージは、ドラフトモードにおいて、既存のヘッダの内容から、他
;; のヘッダの内容を推測して書き換える機能を提供します。例えば、To: ヘッ
;; ダから From: を変更したり、Config: を挿入することができます。
;;
;; また、もうひとつの機能として、文頭にテキストファイル (テンプレート) 
;; を挿入することができます。挿入するファイル名も、ヘッダから推測させる
;; ことができます。
;;
;; インストールの方法。
;;
;;  - このファイルを emacs が見付けられる場所に置きます。
;;
;;  - .emacs に次の記述を追加します。(define-key ...) はキーバインドの例
;;  です。お好みに合わせて変えて下さい。
;;
;;    (add-hook 'mew-init-hook
;;      (lambda ()
;;        (require 'mew-guess)
;;        (define-key mew-draft-header-map "\C-c\C-v" 'mew-guess-template)
;;        (define-key mew-draft-header-map "\C-c\C-d" 'mew-guess-header)
;;        (define-key mew-draft-body-map "\C-c\C-v" 'mew-guess-template)
;;        (define-key mew-draft-body-map "\C-c\C-d" 'mew-guess-header)))
;;
;; 設定の例。
;;
;;  - ヘッダ推測の例。
;;
;;    (setq mew-guess-header-alist
;;          '(
;;            ("From:"
;;             ;; From: の推測のルール
;;             ("To:"                   
;;              ;; To: の内容が、"sorry@" にマッチしたら、From: の内容を 
;;              ;; "小幡 昇 <obata@nippon-control-system.co.jp>" に書き
;;              ;; 換えます。
;;              ("sorry@" "小幡 昇 <obata@nippon-control-system.co.jp>")
;;              ;; 同様に、To: の内容が "@linux\\.or\\.jp" にマッチした
;;              ;; ら、From: を "OBATA Noboru <obata@hh.iij4u.or.jp>" に
;;              ;; 書き換えます。
;;              ("@linux\\.or\\.jp" "OBATA Noboru <obata@hh.iij4u.or.jp>")
;;              )
;;             ("Config:"
;;              ;; Config: の内容によって書き換えたい場合。
;;              ("office" "OBATA Noboru <obata@nippon-control-system.co.jp>"))
;;             ;; 次の特別な記法によって、デフォルト値を指定します。
;;             (t "OBATA Noboru <obata@nippon-control-system.co.jp>"))
;;            ))
;;
;;  置換後の文字列として、値として文字列を持つ変数名や、文字列を返す関数
;;  名やラムダ式を記述できます。例えば、次のように書けば Config: に
;;  mew-config-imget の値を設定できます。
;;
;;    (setq mew-guess-header-alist
;;          '(
;;            ("Config:"
;;             (t mew-config-imget)
;;             )
;;            ))
;;
;;  - テンプレート推測の例。
;;
;;    (setq mew-guess-template-alist
;;          '(("To:"
;;             ;; To: の内容が "foo" にマッチしたら、文頭にファイル 
;;             ;; "~/.ff-foo" を挿入します。
;;             ("foo" "~/.ff-foo")
;;             ;; 同様。
;;             ("bar" "~/.ff-bar")
;;             )
;;            ))
;;
;;  - キーワード置換の例。上の方法では、推測のルールだけファイルを作らな
;;  くてはいけないので、面倒です。テンプレートファイルに |>keyword<| の
;;  書式でキーワードを埋め込み、そのキーワードの置換を指定することができ
;;  ます。
;;
;;    (setq mew-guess-template-alist
;;          '(("To:"
;;             ;; To: の内容が "foo" にマッチしたら、文頭にファイル 
;;             ;; "~/.ff-foo" を挿入します。そのとき、ファイル内のキーワー
;;             ;; ド |>me<| を、"ふー" (ダブルクォーテーションはなし) に
;;             ;; 置き換えます。
;;             ("foo" "~/.ff-foo" ("me" . "ふー"))
;;             ;; 同様。
;;             ("bar" "~/.ff-foo" ("me" . "ばー"))
;;             )
;;            ;; デフォルト
;;            (t "~/.ff-foo")
;;            ))
;;
;;  その際、キーワード置換の既定値を、次のように記述することができます。
;;  置換後の文字列として、変数名、関数名、ラムダ式を記述できます。
;;
;;    (setq mew-draft-replace-alist
;;          '(("me" . "小幡")
;;            ("email" . mew-mail-address)
;;            ("time" . (lambda () (current-time-string)))
;;            ;; ("time" . current-time-string) も OK
;;            ))

;;; Code:


(defvar mew-guess-query-when-replaced nil
  "*If non-nil, make query to accept result of replacement.")

;; Guess

(defun mew-guess-by-alist (alist)
  (let (name header sublist key val ent ret)
    (while (and alist (not ret))
      (setq name (car (car alist)))
      (setq sublist (cdr (car alist)))
      (cond
       ((eq name t)
	(setq ret sublist))
       ;;((eq name nil)
       ;;(setq ret sublist))
       (t
	(setq header (mew-header-get-value name))
	(if header
	    (while (and sublist (not ret))
	      (setq key (car (car sublist)))
	      (setq val (cdr (car sublist)))
	      (if (and (stringp key) (string-match key header))
		  (cond
		   ((stringp (car val))
		    (setq ent
                          (mew-refile-guess-by-alist2 key header (car val))))
		   ((or (functionp (car val))
			(symbolp (car val)))
		    (setq ent (car val)))
		   ((listp (car val))
		    (setq ent (mew-guess-by-alist val)))))
              (if ent (setq ret val))
              (setq sublist (cdr sublist))))))
      (setq alist (cdr alist)))
    ret))

;; Header

(defvar mew-guess-header-alist nil
  "*Alist to guess header contents.
The syntax is:

    (HEADER-GUS (HEADER-CND (KEY VALUE)... )... )...

HEADER-GUS is the target header which you want to guess and modify.

HEADER-CND and KEY specify the condition to guess. If regexp KEY matches
to contents of HEADER-CND, contents are replaced with string VALUE.")

(defun mew-guess-header ()
  "Guess and modify header according to \"mew-guess-header-alist\"."
  (interactive)
  (let ((alist mew-guess-header-alist)
	header-gus sublist header-cnd glist undo changed)
    (save-excursion
      (mew-header-goto-end)
      (setq undo (buffer-substring 1 (point)))
      (while alist
	(setq header-gus (car (car alist)))
	(setq glist (mew-guess-by-alist (cdr (car alist))))
	(if glist
	    (mew-header-replace-value header-gus (car glist)))
	(setq alist (cdr alist)))
      ;; compare with original
      (mew-header-goto-end)
      (setq changed (not (string= undo (buffer-substring 1 (point)))))
      (if (not changed)
	  (if (interactive-p)
	      (message "Nothing changed")) ; nothing done
	;; something changed! query if need
	(mew-highlight-header)
        (mew-draft-header-keymap)
	(if (or (not mew-guess-query-when-replaced)
		(y-or-n-p "Headers are changed. Accept this? "))
	    (message "Some headers are changed") ; accepted
	  ;; restore original
	  (kill-region 1 (point))
	  (insert undo)
	  (mew-header-goto-end)
	  (mew-highlight-header)
	  (mew-draft-header-keymap)
	  (message "Changes are canceled"))))))


(defun mew-header-replace-value (field value)
  "Replace header contents."
  (interactive)
  (let ((newvalue (cond
                   ((stringp value) value)
                   ((symbolp value)
		    (cond 
		     ((eq value 'delete) nil) ; delete this line
		     ((fboundp value) 
		      (funcall value))	; use function result
		     ((and (boundp value)
			   (stringp (symbol-value value)))
		      (symbol-value value)) ; use value of variable
		     (t nil)))
                   ((functionp value) (funcall value))
                   (t nil)))
	orgvalue)
    (if (not (and (stringp field)
		  (or (null newvalue) (stringp newvalue))))
	(error "Invalid field pair in mew-header-replace-alist")
      (setq orgvalue (mew-header-get-value field))
      (if (and orgvalue
	       newvalue
	       (string= (downcase newvalue) (downcase orgvalue)))
	  ()				; same ... don't replace
	(if orgvalue
	    (mew-header-delete-lines (list field))
	  (mew-header-goto-end))
	(if newvalue
	    (insert field " " newvalue "\n"))))))

;; Template

(defvar mew-guess-template-alist nil
  "*Alist to guess template file.
The basic syntax is:

    (HEADER (KEY TEMPLATE)... )...

If regexp KEY matches to contents of HEADER, file TEMPLATE is guessed
and guess is finished. Note that there is no dot (.) between KEY and
TEMPLATE.

You can specify alists for keyword replacement like:

    (HEADER (KEY TEMPLATE (REPLACE-FROM . REPLACE-TO)... )... )...

Alists in this form take precedence over \"mew-draft-replace-alist\".

For example:

    (setq mew-guess-template-alist
          '((\"To:\"
             (\"mew-dist@mew.org\" \"~/.ff-mew-dist\"
              (\"hello\" . \"Mew friends,\")))
             (\"foo@hoge.hoge\" \"~/.ff-other\")
	    (t \"~/.ff-default\")
            ))

There is exceptional form as you can see in the example:

    (t TEMPLATE [(REPLACE-FROM . REPLACE-TO)...])

You can specify default TEMPLATE in this form, putting it on the last.")

(defun mew-guess-template ()
  "Insert template file on the top of the draft message."
  (interactive)
  (let* ((glist (mew-guess-by-alist mew-guess-template-alist))
         (file (car glist)) (kwlist (cdr glist)) deleted efile)
    (if file
        (progn
          (setq efile (expand-file-name file))
          (if (not (file-exists-p efile))
              (message "No template file %s" efile)
            (progn
              (forward-char
               (mew-draft-insert-file-and-replace
                efile 'top (list kwlist mew-draft-replace-alist)))))))))

;; Misc

(defvar mew-draft-replace-alist nil
  "*Alist for keyword replacement in draft.
Keywords \"|>keyword<|\" in the template file and signature file (not
yet) are replaced with it's associated value. It is possible to specify
string, function, variable and lambda expression as the associated
value, which is evaluated when replacement occurs.

You can replace keyword to the string flushed on the right. For example:

    (setq mew-draft-replace-alist
          '((\"name\" . \"HOGE Hoge\")
            (\"email\" . \"foo@hoge.hoge\")
            (\"time\" .
             (lambda () (format (format \"%%%ds\" fill-column)
                                (current-time-string))))
            ))")

(defun mew-draft-insert-file-and-replace (file pos &optional alists)
  "Insert file and replace keyword.
Insert file FILE on position POS (possible values are top, bottom and
here), and replace keyword according to ALISTS (list of alist)."
  (interactive)
  (cond
   ((eq pos 'top)
    (goto-char (mew-header-end))
    (forward-line))
   ((eq pos 'bottom)
    (if (null (mew-attach-p))
        (goto-char (point-max))
      (goto-char (mew-attach-begin))
      (forward-line -1)
      (end-of-line)
      (insert "\n"))))
  (let (bytes)
    (save-restriction
      (narrow-to-region
       (point) (+ (point) (car (cdr (insert-file-contents file)))))
      (while alists
        (mew-draft-replace-by-alist (car alists))
        (setq alists (cdr alists)))
      (mew-fib-delete-frame)
      (setq bytes (- (point-max) (point-min))))))

(defun mew-draft-replace-by-alist (alist)
  "Fill |>item<| by alist."
  (interactive)
  (save-excursion
    (let (begin end str)
      (goto-char (point-min))
      (while (re-search-forward "|>\\([^<]+\\)<|" nil t)
	(setq begin (match-beginning 1)
	      end (match-end 1)
	      str (buffer-substring begin end))
	(delete-region begin end)
	(backward-char 2)
	(insert (let ((obj (cdr (assoc (downcase str) alist))))
                  (cond
		   ((null obj) str)
		   ((stringp obj) obj)
		   ((functionp obj) (funcall obj))
		   ((symbolp obj)
		    (if (fboundp obj) (funcall obj)
		      (if (and (boundp obj)
			       (stringp (symbol-value obj)))
			  (symbol-value obj))))
                   (t str))))))))

(provide 'mew-guess)

;;; mew-guess.el ends here
