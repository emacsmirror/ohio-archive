;; -*- Mode: Emacs-Lisp -*-
;;
;; mew-cite-color.el --- "Colorful highligh message body with Mew Message mode."
;;     $Id: mew-cite-color.el,v 1.5 1999/08/27 11:13:48 kazu Exp $
;;
;;                         "Hideyuki SHIRAI" <shirai@rdmg.mgcs.mei.co.jp>
;;                                            Created: <03/10/1998>
;;                                Revised: Time-stamp: <08/27/1999 19:38 shirai>
;;             
;; Special Thanks,
;;  Shigeki MORIMOTO-san, and his 'message-cite-color.el'.
;;
;;  *** Sorry, this file includes a few japanese characters. ***
;;
;; Usage: Put your ~/.emacs.
;; (eval-after-load "mew" '(require 'mew-cite-color))
;;

(eval-when-compile (require 'mew))

(defconst mew-cite-color-version "mew-cite-color.el 1.10")

;; Variables
(defvar mew-cite-color-face-list-source
  '("ForestGreen" "MediumBlue" "Magenta" "DarkOrange2" "purple")
  "*Color list for cite highlight.")
(defvar mew-cite-color-face-type 'default
  "*Face type select, which 'default 'bold 'italic 'bold-italic.")

(defvar mew-cite-color-comment-face-color "Grey50"
  "*Color for comment highlight.")
(defvar mew-cite-color-comment-face-type 'default
  "*Face type select for comment, which 'default 'bold 'italic 'bold-italic.")

(defvar mew-cite-color-max-line 500
  "*Maximum line count enable highlite.")

(defvar mew-cite-color-prefix-regexp
  "^\\([ \t　]*\\(\\(\\(\[》≫＞>|]+[ \t　]?\\)\\|\\([-.a-zA-Z0-9'@_ー　＿-龠]*[＞>]+[ \t　]?\\)\\)[ \t　]*\\)+\\)"
  "*Regexp matching the longest possible citation prefix on a line.")
(defvar mew-cite-color-non-prefix-regexp
  "^.*[<＜《≪]"
  "*NON highlite citation prefix regexp.")
(defvar mew-cite-color-non-prefix-length 20
  "*NON highlite citation prefix length.")
(defvar mew-cite-color-non-body-regexp
  "\\(^.*>>>>>\\)\\|\\(>+.*[ \t]*in article\\)\\|\\(>+[ \t]*in message\\)\\|\\(^[ \t　]*|[ \t　]+/\\)\\|\\(^.*の刻に$\\)\\|\\(^.*曰く$\\)"
  "*NON highlite regexp on a line.")
(defvar mew-cite-color-comment-regexp
  "^[ \t]*[;#＃%]+"
  "*Regexp matching the comment prefix on a line.")

(defvar mew-cite-color-end-separator
  "\\(^-- \n\\)\\|\\(^begin [0-9][0-9][0-9] \\)\\|\\(^#!/bin/\\)"
  "*If exist this regexp in buffer, non highlight after this regexp.")

(defvar mew-cite-color-use-draft-mode t
  "*If nil, do not use mew-cite-color in draft buffer.")

;; default setting (officious)
(setq mew-use-highlight-body nil)
(defadvice mew-highlight-body (after mew-cite-color activate)
  (mew-cite-color))

;; internal variables
(defvar mew-cite-color-faces nil)
(defvar mew-cite-color-comment-face nil)
(defvar mew-cite-color-face-list nil)
(defvar mew-cite-color-face-alist nil)

;; for Emacs 19.28
(or (fboundp 'facep)
    ;; Introduced in Emacs 19.29.
    (defun facep (x)
      "Return t if X is a face name or an internal face vector."
      (and (or (and (fboundp 'internal-facep) (internal-facep x))
              (and 
               (symbolp x) 
               (assq x (and (boundp 'global-face-data) global-face-data))))
          t)))

(defun mew-cite-color ()
  (if (and (or window-system mew-xemacs-p)
	   (or (not (eq major-mode 'mew-draft-mode))
	       (and mew-cite-color-use-draft-mode (eq major-mode 'mew-draft-mode))))
      (save-excursion
	(save-restriction
	  (widen)
	  (mew-cite-color-reset-cite-face)
	  (let ((buffer-read-only nil)
		(line 1))
	    (if (mew-header-p)
		(progn
		  (goto-char (mew-header-end))
		  (forward-line)
		  (narrow-to-region (point)
				    (or (mew-attach-begin) (point-max)))))
	    (if (not (eq major-mode 'mew-draft-mode))
		()
	      (mew-overlay-delete-region (point-min) (point-max))
	      (mew-highlight-url))
	    (goto-char (point-min))
	    (while (and (not (eobp))
			(< line mew-cite-color-max-line)
			(not (looking-at mew-cite-color-end-separator)))
	      (if (looking-at mew-cite-color-prefix-regexp)
		  (let ((cite (buffer-substring (match-beginning 0)
						(match-end 0))))
		    (if (string-match "[ \t　]+$" cite)
			(setq cite (substring cite 0 (match-beginning 0))))
		    (if (or (> (length cite) mew-cite-color-non-prefix-length)
			    (string-match mew-cite-color-non-prefix-regexp cite)
			    (looking-at mew-cite-color-non-body-regexp))
			()
		      (mew-overlay-put (mew-overlay-make (point)
							 (progn (end-of-line)(point)))
				       'face (mew-cite-color-prefix-cite-face cite))))
		(if (looking-at mew-cite-color-comment-regexp)
		    (mew-overlay-put (mew-overlay-make (point)
						       (progn (end-of-line)(point)))
				     'face 'mew-cite-color-comment-face)))
	      (forward-line 1)
	      (setq line (1+ line)))
	    )))))

(defun mew-cite-color-reset-cite-face ()
  (setq mew-cite-color-face-list mew-cite-color-face-list-source)
  (setq mew-cite-color-face-alist nil))

(defun mew-cite-color-prefix-cite-face (cite)
  (let (color)
    (if (not (setq color (cdr (assoc cite mew-cite-color-face-alist))))
	(progn
	  (setq color (car mew-cite-color-face-list))
	  (setq mew-cite-color-face-list
		(or (cdr mew-cite-color-face-list)
		    mew-cite-color-face-list-source))
	  (setq mew-cite-color-face-alist
		(append mew-cite-color-face-alist
			(list (append (list cite) color))))))
    (cdr (assoc color mew-cite-color-faces))))

;; setup at loadtime.
(defun mew-cite-color-setup ()
  (interactive)
  (let ((mew-cite-color-face-list mew-cite-color-face-list-source))
    (if (or (facep 'mew-cite-color-comment-face)
	    (and (fboundp 'find-face)
		 (find-face 'mew-cite-color-comment-face)))
	()
      (make-face 'mew-cite-color-comment-face)
      (copy-face mew-cite-color-comment-face-type
		 'mew-cite-color-comment-face)
      (set-face-foreground 'mew-cite-color-comment-face mew-cite-color-comment-face-color))
    (setq mew-cite-color-faces
	  (mapcar '(lambda (color)
		     (cons color
			   (intern (concat "mew-cite-color-face-" color))))
		  mew-cite-color-face-list))
    (mapcar '(lambda (face-list)
	       (if (or (facep (cdr face-list))
		       (and (fboundp 'find-face)
			    (find-face (cdr face-list))))
		   ()
		 (make-face (cdr face-list))
		 (copy-face mew-cite-color-face-type (cdr face-list))
		 (set-face-foreground (cdr face-list) (car face-list))))
	    mew-cite-color-faces)))

(mew-cite-color-setup)

(provide 'mew-cite-color)
;; end here.
