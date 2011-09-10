;;; lookup-kanji.el --- Lookup Kanji-Kana Converter
;; Copyright (C) 1999 Lookup Development Team <lookup@ring.gr.jp>

;; Author: TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>
;; Version: $Id: lookup-kanji.el,v 1.4 1999/07/28 01:38:55 tsuchiya Exp $

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
(require 'lookup-utils)
(require 'lookup-vars)

(defcustom lookup-kanji-command "kakasi"
  "*�����Ҥ餬���Ѵ��ץ����(KAKASI)��̾��"
  :type 'string
  :group 'lookup-kanji)

(defcustom lookup-kanji-option '("-JH" "-p")
  "*�����Ҥ餬���Ѵ��ץ����(KAKASI)�Υ��ץ����"
  :type 'list
  :group 'lookup-kanji)

(defcustom lookup-kanji-coding-system lookup-kakasi-coding-system
  "*�����Ҥ餬���Ѵ��ץ����(KAKASI)�θƤӽФ����Ѥ���ʸ��������"
  :type 'symbol
  :group 'lookup-kanji)

(defvar lookup-kanji-process nil
  "�����Ҥ餬���Ѵ��ץ����Υץ���(�����ѿ�)")

(defvar lookup-kanji-buffer nil
  "�����Ҥ餬���Ѵ��ץ����Υץ������Ϣ�Ť���줿�Хåե�(�����ѿ�)")

(defun lookup-kanji-generate-buffer ()
  "����ѥХåե�����������ؿ�"
  (unless (and (bufferp lookup-kanji-buffer)
	       (buffer-name lookup-kanji-buffer))
    (setq lookup-kanji-buffer (generate-new-buffer " *lookup-kanji*")
	  lookup-buffer-list (cons lookup-kanji-buffer lookup-buffer-list))
    (buffer-disable-undo lookup-kanji-buffer))
  lookup-kanji-buffer)

(defun lookup-kanji-start-process ()
  "�����Ҥ餬���Ѵ��ץ�����ư����ؿ�"
  (unless (and (processp lookup-kanji-process)
	       (eq (process-status lookup-kanji-process) 'run))
    (lookup-with-coding-system lookup-kanji-coding-system
      (setq lookup-kanji-process
	    (apply 'start-process
		   "lookup-kanji"
		   (lookup-kanji-generate-buffer)
		   lookup-kanji-command
		   lookup-kanji-option)))
    (sit-for 0.1)
    (process-kill-without-query lookup-kanji-process))
  lookup-kanji-process)

(defun lookup-kanji-send-string (str)
  "STR ��ץ�����ɸ�����Ϥ��Ϥ��ơ����η�̤��֤��ؿ�"
  (lookup-process-require (lookup-kanji-start-process) (concat str "\n") "\n")
  (save-excursion
    (set-buffer lookup-kanji-buffer)
    (goto-char (point-min))
    (forward-line 1)
    (prog1 (buffer-substring (point) (progn (end-of-line) (point)))
      (delete-region (point-min) (point-max)))))

(defun lookup-kanji-analysis-output (str)
  "KAKASI �ν��Ϥ˴ޤޤ�Ƥ����̤���Ϥ��ƥꥹ�Ȥ�Ÿ������"
  (if (string-match "{\\([^}]+\\)}" str)
      (let ((x (substring str 0 (match-beginning 0)))
	    (y (substring str (match-end 0))))
	(apply 'nconc 
	       (mapcar 'lookup-kanji-analysis-output
		       (mapcar (lambda (s) (concat x s y))
			       (lookup-split-string (substring str (match-beginning 1) (match-end 1)) "|")))))
    (list str)))

(defun lookup-kanji-get-readings (str)
  "STR ������Ҥ餬���Ѵ���������줿��̤Υꥹ�Ȥ��֤��ؿ�"
  (lookup-kanji-analysis-output (lookup-kanji-send-string str)))

(defun lookup-kanji-filter (str entries)
  "STR �����Ф��˴ޤޤ�Ƥ���褦�ʥ���ȥ�Τߤ���Ф��ե��륿�ؿ�"
  (let ((regex (mapconcat 'char-to-string (string-to-char-list string) ".?.?.?")))
    (delq nil (mapcar (lambda (entry)
			(if (string-match regex (lookup-entry-heading entry)) entry))
		      entries))))

(provide 'lookup-kanji)
;;; lookup-kanji.el ends here
