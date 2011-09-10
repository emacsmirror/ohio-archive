;; -*- Mode: Emacs-Lisp -*-
;;  $Id: mew-summary-hl.el,v 1.2.2.1 1999/10/20 11:20:55 kazu Exp $
;;
;;                                "Hideyuki SHIRAI" <shirai@rdmg.mgcs.mei.co.jp>
;;
;;;; Mew Summary buffer を font-lock を使って色付けする
;;
;;;; 使い方: ~/.emacs に書いてね
;;
;;; これがあると素敵 (というか無いと遅くて使い物にならない)
;; (cond
;;  ((locate-library "lazy-shot")	;; for XEmacs
;;   (require 'font-lock)
;;   (add-hook 'font-lock-mode-hook 'turn-on-lazy-shot)
;;   (setq lazy-shot-verbose nil)
;;   (setq lazy-shot-stealth-verbose nil))
;;  ((locate-library "lazy-lock")	;; for Emacs
;;   (require 'font-lock)
;;   (setq font-lock-support-mode 'lazy-lock-mode)))
;;
;;; 内部で window-system とかの判定はしていません。
;; (if (and (or window-system (string-match "XEmacs" emacs-version))
;; 	    (locate-library "mew-summary-hl"))
;;     (eval-after-load "mew" '(require 'mew-summary-hl)))
;;
;;;; 使用上の注意
;;; XEmacs は良くわからないので、変だったら教えて下さい。(_ _)
;;

(eval-when-compile (require 'mew))
(defconst mew-summary-hl-version "mew-summary-hl 0.10")

;; default で対象としている ~/.im/Config の 'From' は以下の通り
;;; imget.Form=%+4n %m%d %h:%E %+2KK %-24A %S || %b
;;; Form=%+4n %m%d/%y %+3KK %-24A %S || %b
;; PC によっては %+2n や %2Kk というのもあるので 'K' と '24' で決めうち
(defvar mew-summary-hl-start-regex "^ *\\([1-9][0-9]* [^K]+K\\) ")
;;                                                   ~
;; ここの space を忘れないでね。これで、mark があるかないか判定しています。
(defvar mew-summary-hl-from-width 24)
(defvar	mew-summary-hl-ml " *\\([\[(][^])\n\r]*[\])]\\)")
(defvar	mew-summary-hl-subject-regex1 " *\\(.*\\) +\\(\|\|[^\n\r]*\\)")
(defvar mew-summary-hl-subject-regex2 " *\\([^\n\r]*\\)")
;; ↑ 最後が '|' で終るとダメだけどご愛敬

;;; Form=%+5n %m%d %-14A %S || %b
;; という IM の default だったら、こんな感じ？
;; (setq mew-summary-hl-start-regex "^ *\\([1-9][0-9]* [^/]*[0-9]+/[0-9]+\\) ")
;; (setq mew-summary-hl-from-width 14)
;; あとは自分の環境に合わせて下さい。(_ _)

;; face の書体と色はお好みで変えよう。
;; この設定だと http://www.netlaputa.ne.jp/~hshirai/Image/summary1.png 
;; のようになります。
(defvar mew-sumamry-hl-face-list '("num" "from" "to" "ml" "subject" "body"))

(defvar mew-summary-hl-face-num-type 'italic)
(defvar mew-summary-hl-face-num-color "Maroon")

(defvar mew-summary-hl-face-from-type 'bold)
(defvar mew-summary-hl-face-from-color "Purple")

(defvar mew-summary-hl-face-to-type 'bold-italic)
(defvar mew-summary-hl-face-to-color "DarkOrange3")

(defvar mew-summary-hl-face-ml-type 'italic)
(defvar mew-summary-hl-face-ml-color "DarkGreen")

(defvar mew-summary-hl-face-subject-type 'bold)
(defvar mew-summary-hl-face-subject-color "DarkGreen")

(defvar mew-summary-hl-face-body-type 'italic)
(defvar mew-summary-hl-face-body-color "Grey50")

;; MUE/MHC などの色づけ用関数を定義する
(defvar mew-summary-hl-external-function nil)

;; hook の追加
(add-hook 'mew-summary-mode-hook 'mew-summary-hl-enable)
(add-hook 'mew-virtual-mode-hook 'mew-summary-hl-enable)
(add-hook 'mew-summary-inc-sentinel-hook 'mew-summary-hl-block)
(add-hook 'mew-summary-scan-sentinel-hook 'mew-summary-hl-block)

(if (locate-library "mew-refile-view")
    ;; とりあえず。
    (add-hook 'mew-refile-view-mode-hook (lambda () (font-lock-mode 0))))

;; 自前でやるから nil にする
(setq mew-use-highlight-mark nil)

(defun mew-summary-hl-enable ()
  (make-local-variable 'font-lock-fontify-buffer-function)
  (make-local-variable 'font-lock-fontify-region-function)
  (setq font-lock-fontify-buffer-function 'mew-summary-hl-buffer)
  (setq font-lock-fontify-region-function 'mew-summary-hl-region)
  (font-lock-mode 1))

(cond
 ((fboundp 'font-lock-fontify-block)
  (fset 'mew-summary-hl-block (symbol-function 'font-lock-fontify-block)))
 (t
  ;; いい加減だけど XEmacs 用
  (defun mew-summary-hl-block ()
    (font-lock-mode 1))))

(defun mew-summary-hl-buffer ()
  "Mew summary buffer highlight with font-lock-mode."
  (interactive)
  (mew-summary-hl-region (point-min) (point-max)))

(defun mew-summary-hl-region (beg end &optional loudly)
  "Mew summary region highlight with font-lock-mode."
  (interactive "r")
  (if (or (eq major-mode 'mew-summary-mode)
	  (eq major-mode 'mew-virtual-mode))
      (mew-elet
       (goto-char beg)
       (beginning-of-line)
       (setq beg (point))
       (remove-text-properties beg end '(face nil))
       (while (< (point) end)
	 (cond
	  ;; 普通の行
	  ((looking-at mew-summary-hl-start-regex)
	   (put-text-property (match-beginning 1) (match-end 1)
			      'face 'mew-summary-hl-face-num)
	   (goto-char (match-end 0))
	   (if (looking-at "To:")
	       ;; 自分のメール
	       (put-text-property (point)
				  (progn (move-to-column
					  (+ (current-column)
					     mew-summary-hl-from-width))
					 (point))
				  'face 'mew-summary-hl-face-to)
	     ;; 他の人のメール
	     (put-text-property (point)
				(progn (move-to-column
					(+ (current-column)
					   mew-summary-hl-from-width))
				       (point))
				'face 'mew-summary-hl-face-from))
	   (if (not (looking-at mew-summary-hl-ml))
	       ()
	     ;; [mew-dist 0123] や (pgp-users 1234) があった
	     (put-text-property (match-beginning 1) (match-end 1)
				'face 'mew-summary-hl-face-ml)
	     (goto-char (match-end 0)))
	   (if (not (looking-at mew-summary-hl-subject-regex1))
	       (if (looking-at mew-summary-hl-subject-regex2)
		   (put-text-property (match-beginning 1) (match-end 1)
				      'face 'mew-summary-hl-face-subject))
	     ;; || のあとの body があった
	     (put-text-property (match-beginning 1) (match-end 1)
				'face 'mew-summary-hl-face-subject)
	     (put-text-property (match-beginning 2) (match-end 2)
				'face 'mew-summary-hl-face-body)))
	  ;; mark が付いている行
	  ((looking-at (concat mew-summary-message-regex "\\([^ ]\\)"))
	   (let (face)
	     (setq face (cdr (assoc (string-to-char (mew-match 2))
				    mew-highlight-mark-keywords)))
	     (if face
		 (put-text-property (point) (progn (end-of-line) (point))
				    'face face)
	       ;; multipart part 2
	       (put-text-property (point) (progn (end-of-line) (point))
				  'face 'mew-summary-hl-face-num))))
	  ;; その他の行(普通の人は multi part を展開した行)
	  (t
	   (or (and mew-summary-hl-external-function
		    ;; MUE の subject 行や MHC の Virtual folder の色づけをする
		    ;; 色づけ対象行でなかったら 'nil' を返してもらう
		    (funcall mew-summary-hl-external-function))
	       (put-text-property (point) (progn (end-of-line) (point))
				  'face 'mew-summary-hl-face-num))))
	 (forward-line)))))

(defun mew-summary-hl-setup ()
  (let ((flist mew-sumamry-hl-face-list)
	fname type color)
    (mapcar
     '(lambda (face)
	(setq type (intern-soft
		    (concat "mew-summary-hl-face-" face "-type")))
	(setq color (intern-soft
		     (concat "mew-summary-hl-face-" face "-color")))
	(setq fname (intern (concat "mew-summary-hl-face-" face)))
	(copy-face (symbol-value type) fname)
	(set-face-foreground fname (symbol-value color)))
     flist)))

;; load したときに face を作ってしまう。
(mew-summary-hl-setup)

(provide 'mew-summary-hl)

;; ends here.
