;;; ndeb.el --- Lookup eblook interface
;; Copyright (C) 1999 Lookup Development Team <lookup@ring.gr.jp>

;; Author: Keisuke Nishida <kei@psn.net>
;; Version: $Id: ndeb.el,v 1.8 2000/02/13 00:38:12 knishida Exp $

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

(defconst ndeb-version "1.2")

;;;
;:: Customizable variables
;;;

(defgroup ndeb nil
  "Lookup eblook interface."
  :group 'lookup-agents)

(defcustom ndeb-program-name "eblook"
  "*Program name of eblook."
  :type 'string
  :group 'ndeb)

(defcustom ndeb-prompt-string "eblook> "
  "*Prompt string of eblook."
  :type 'string
  :group 'ndeb)

(defcustom ndeb-process-coding-system lookup-process-coding-system
  "*Coding system for eblook process."
  :type 'symbol
  :group 'ndeb)


;;;
;:: Internal varialbes
;;;

(defvar ndeb-current-agent nil)
(defvar ndeb-current-dictionary nil)
(defvar ndeb-current-process nil)

(defconst ndeb-method-table
  '((exact . "exact") (prefix . "word") (suffix . "endword")))

;;;
;:: types
;;;

;; ndeb agent:
;;
;;   (ndeb DIRECTORY :appendix APPENDIX)
;;
;; DIRECTORY - dictionary directory
;; APPENDIX  - appendix directory
;;
;; [property]
;; ndeb-process - eblook process related with agent
;; ndeb-dict    - last used dictionary
;; ndeb-method  - last used value of search-method
;; ndeb-stop    - last used value of stop-code

(defun ndeb-agent-directory (agent)
  (let ((dir (lookup-agent-location agent)))
    (if dir (expand-file-name dir)
      (error "You should specify a dictionary directory"))))

(defun ndeb-agent-appendix (agent)
  (let ((dir (lookup-agent-option agent ':appendix)))
    (if dir (expand-file-name dir))))

(defun ndeb-agent-coding (agent)
  (or (lookup-agent-option agent ':coding)
      ndeb-process-coding-system))

(put 'ndeb ':methods '(exact prefix suffix))
(put 'ndeb ':gaiji-regexp "<gaiji=\\([^>]*\\)>")
(put 'ndeb ':reference-pattern '("<reference>\\(→?\\(\\([^<]\\|<gaiji=[^>]*>\\)+\\)\\)</reference=\\([^>]*\\)>" 1 2 4))

(put 'ndeb ':headings '(lookup-arrange-gaijis))

(put 'ndeb ':arranges
     '(lookup-arrange-references
       lookup-arrange-gaijis
       lookup-arrange-squeezed-references
       lookup-arrange-default-headings
       lookup-arrange-fill-lines))

(put 'ndeb ':adjusts
     '(lookup-adjust-show-gaijis
       lookup-adjust-check-references
       lookup-adjust-goto-min))

;; ndeb dictionary:
;;
;; CODE - same as NAME below
;; NAME - given by eblook `list' command
;; 
;; [option]
;; :coding    - process coding system
;; :stop-code - stop-code used by eblook

(defun ndeb-new-dictionary (name title)
  (lookup-new-dictionary ndeb-current-agent name name title))

(defun ndeb-dictionary-coding (dictionary)
  (or (lookup-dictionary-option dictionary ':coding t)
      ndeb-process-coding-system))

(defun ndeb-dictionary-stopcode (dictionary)
  (lookup-dictionary-option dictionary ':stop-code t))

;; ndeb entry:
;;
;; CODE    - entry specific code (e.g. "2c00:340") by eblook `search' command
;; HEADING - given by eblook `search' command

(defun ndeb-make-entry (code heading)
  (lookup-make-entry ndeb-current-dictionary code heading))

;;;
;:: macros
;;;

(put 'ndeb-with-agent 'lisp-indent-function 1)
(defmacro ndeb-with-agent (agent &rest body)
  (` (let ((ndeb-current-agent (, agent))
	   (ndeb-current-process (ndeb-agent-process (, agent))))
       (,@ body))))

(put 'ndeb-with-dictionary 'lisp-indent-function 1)
(defmacro ndeb-with-dictionary (dictionary &rest body)
  (` (ndeb-with-agent (lookup-dictionary-agent (, dictionary))
       (let ((ndeb-current-dictionary (, dictionary)))
	 (unless (eq (, dictionary)
		     (lookup-agent-get-property ndeb-current-agent 'ndeb-dict))
	   ;; 必要なときだけ辞書を select する。
	   ;; 外部プロセスとやりとりするよりこの方が高速だろうし、
	   ;; デバッグのときバッファがごちゃごちゃするのはうざったい。
	   (ndeb-process-require
	    (concat "select " (lookup-dictionary-code (, dictionary))))
	   (lookup-agent-put-property ndeb-current-agent 'ndeb-dict
				      (, dictionary))
	   ;; 辞書毎に文字コードを設定する。
	   (let ((code (ndeb-dictionary-coding (, dictionary))))
	     (when code
	       (set-process-coding-system ndeb-current-process code code))))
	 (,@ body)))))

(defun ndeb-agent-process (agent)
  (let ((process (lookup-agent-get-property agent 'ndeb-process)))
    (unless (and process (eq (process-status process) 'run))
      (if process (lookup-process-kill process))
      (setq process (ndeb-process-open (ndeb-agent-directory agent)
				       (ndeb-agent-appendix agent)))
      ;; 最初に辞書一覧を得るのに文字コードの設定が必要。
      (let ((coding (ndeb-agent-coding agent)))
	(when coding
	  (set-process-coding-system process coding coding)))
      ;; コマンドの実行毎に行なう必要のある処理。
      (let ((ndeb-current-process process))
	(if lookup-max-hits (ndeb-require-set "max-hits" lookup-max-hits))
	(if lookup-max-text (ndeb-require-set "max-text" lookup-max-text)))
      (lookup-agent-put-property agent 'ndeb-process process)
      (lookup-agent-put-property agent 'ndeb-dict nil)
      (lookup-agent-put-property agent 'ndeb-method nil)
      (lookup-agent-put-property agent 'ndeb-stop nil))
    process))

(defun ndeb-agent-kill-process (agent)
  (let ((process (lookup-agent-get-property agent 'ndeb-process)))
    (when process
      (if (eq (process-status process) 'run)
	  (process-send-string process "quit\n"))
      (lookup-process-kill process)
      (lookup-agent-put-property agent 'ndeb-process nil))))


;;;
;:: Interface functions
;;;

(put 'ndeb 'setup 'ndeb-setup)
(defun ndeb-setup (agent)
  (ndeb-with-agent agent
    (ndeb-process-require "list"
      (lambda (process)
	(let (name title dicts)
	  (while (re-search-forward "^[^.]+\\. \\([^\t]+\\)\t\\(.*\\)" nil t)
	    (setq name (match-string 1) title (match-string 2))
	    (setq dicts (cons (ndeb-new-dictionary name title) dicts)))
	  (nreverse dicts))))))

(put 'ndeb 'clear 'ndeb-clear)
(defalias 'ndeb-clear 'ndeb-agent-kill-process)

(put 'ndeb 'menu 'ndeb-dictionary-menu)
(defun ndeb-dictionary-menu (dictionary)
  (ndeb-with-dictionary dictionary
    (ndeb-process-require "menu"
      (lambda (process)
	(let ((case-fold-search nil) code heading entries)
	  (while (re-search-forward ">\\(.*\\)</[^=]*=\\([^>]*\\)>" nil t)
	    (setq heading (match-string 1) code (match-string 2))
	    (setq entries (cons (ndeb-make-entry code heading) entries)))
	  (nreverse entries))))))

(put 'ndeb 'search 'ndeb-dictionary-search)
(defun ndeb-dictionary-search (dictionary query)
  (ndeb-with-dictionary dictionary
    (let ((method (lookup-query-method query))
	  (last (lookup-agent-get-property ndeb-current-agent 'ndeb-method)))
      (unless (eq method last)
	;; 必要のあるときだけ search-method を設定する。ndeb-dict に同じ。
	(ndeb-require-set "search-method"
			  (lookup-assq-ref ndeb-method-table method))
	(lookup-agent-put-property ndeb-current-agent 'ndeb-method method)))
    (ndeb-process-require (format "search \"%s\"" (lookup-query-string query))
      (lambda (process)
	(let (code heading last-code last-heading entries)
	  (while (re-search-forward "^[^.]+\\. \\([^\t]+\\)\t\\(.*\\)" nil t)
	    (setq code (match-string 1) heading (match-string 2))
	    ;; 同じエントリが連続していないかチェックする。
	    ;; これがけっこうあるんだ・・
	    (when (or (not (string= code last-code))
		      (not (string= heading last-heading)))
	      (setq entries (cons (ndeb-make-entry code heading) entries))
	      (setq last-code code last-heading heading)))
	  (nreverse entries))))))

(put 'ndeb 'content 'ndeb-dictionary-content)
(defun ndeb-dictionary-content (dictionary entry)
  (ndeb-with-dictionary dictionary
    (let ((stop (ndeb-dictionary-stopcode dictionary))
	  (last (lookup-agent-get-property ndeb-current-agent 'ndeb-stop)))
      (unless (eq stop last)
	;; 必要のあるときだけ stop-code を設定する。ndeb-dict に同じ。
	(ndeb-require-set "stop-code" stop)
	(lookup-agent-put-property ndeb-current-agent 'ndeb-stop stop)))
    (ndeb-process-require (concat "content " (lookup-entry-code entry)))))

(put 'ndeb 'gaiji 'ndeb-dictionary-gaiji)
(defun ndeb-dictionary-gaiji (dictionary code)
  (ndeb-with-dictionary dictionary
    (let ((xbm (ndeb-process-require (concat "font " code))))
      (if (string-match "default_width" xbm)
	  xbm))))


;;;
;:: Internal functions
;;;

(defun ndeb-require-set (var value)
  (if value
      (ndeb-process-require (format "set %s \"%s\"" var value))
    (ndeb-process-require (format "unset %s" var))))

;;;
;:: eblook process
;;;

(defun ndeb-process-open (directory appendix)
  (let* ((args (cons "-q" (cons directory (if appendix (list appendix)))))
	 (buffer (lookup-open-process-buffer (concat " *ndeb+" directory "*")))
	 (process (apply 'start-process "ndeb" buffer ndeb-program-name args)))
    (process-kill-without-query process)
    (accept-process-output process)
    (with-current-buffer (or buffer (lookup-temp-buffer))
      (save-excursion
	(goto-char (point-min))
	(if (search-forward "Warning: invalid book directory" nil t)
	    (error "Invalid dictionary directory: %s" directory))
	(goto-char (point-min))
	(if (search-forward "Warning: invalid appendix directory" nil t)
	    (error "Invalid appendix directory: %s" appendix)))
      (if (not (eq buffer (current-buffer)))
	  (kill-buffer (current-buffer))))
    process))

(put 'ndeb-process-require 'lisp-indent-function 1)
(defun ndeb-process-require (command &optional filter)
  (lookup-process-require ndeb-current-process (concat command "\n")
			  (concat "^" ndeb-prompt-string) filter))

(provide 'ndeb)

;;; ndeb.el ends here
