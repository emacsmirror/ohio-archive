;;; MH folder access using NNTP for GNU Emacs
;; Copyright (C) 1988, 1989 Fujitsu Laboratories LTD.
;; Copyright (C) 1988, 1989, 1990 Masanobu UMEDA
;; $Header: mhspool.el,v 1.5 90/03/23 13:25:23 umerin Locked $

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

(provide 'mhspool)
(require 'nntp)

;; This package enables you to read mail or articles in MH folders, or
;; articles saved by GNUS. In any case, the file names of mail or
;; articles must consist of only numeric letters.

;; Before using this package, you have to create a server specific
;; startup file according to the directory which you want to read. For
;; example, if you want to read mail under the directory named
;; `~/Mail', the file must be a file named `.newsrc-:Mail'. (There is
;; no way to specify hierarchical directory now.) In this case, the
;; name of the NNTP server passed to GNUS must be `:Mail'.

(defvar mhspool-list-directory-switches '("-R")
  "*Switches for nntp-request-list to pass to `ls' for gettting file lists.
One entry should appear on one line. You may need to add `-1' option.")



(defconst mhspool-version "MHSPOOL 1.5"
  "Version numbers of this version of MHSPOOL.")

(defvar mhspool-spool-directory "~/Mail"
  "Private mail directory.")

(defvar mhspool-current-directory nil
  "Current news group directory.")

;;;
;;; Replacement of Extended Command for retrieving many headers.
;;;

(defun mhspool-retrieve-headers (sequence)
  "Return list of article headers specified by SEQUENCE of article id.
The format of list is
 `([NUMBER SUBJECT FROM XREF LINES DATE MESSAGE-ID REFERENCES] ...)'.
Reader macros for the vector are defined as `nntp-header-FIELD'.
Writer macros for the vector are defined as `nntp-set-header-FIELD'.
News group must be selected before calling me."
  (save-excursion
    (set-buffer nntp-server-buffer)
    ;;(erase-buffer)
    (let ((file nil)
	  (number (length sequence))
	  (count 0)
	  (headers nil)			;Result list.
	  (article 0)
	  (subject nil)
	  (message-id nil)
	  (from nil)
	  (xref nil)
	  (lines 0)
	  (date nil)
	  (references nil))
      (while sequence
	;;(nntp-send-strings-to-server "HEAD" (car sequence))
	(setq article (car sequence))
	(setq file
	      (concat mhspool-current-directory (prin1-to-string article)))
	(if (and (file-exists-p file)
		 (not (file-directory-p file)))
	    (progn
	      (erase-buffer)
	      (insert-file-contents file)
	      ;; Make message body invisible.
	      (goto-char (point-min))
	      (search-forward "\n\n" nil 'move)
	      (narrow-to-region (point-min) (point))
	      ;; Fold continuation lines.
	      (goto-char (point-min))
	      (while (re-search-forward "\\(\r?\n[ \t]+\\)+" nil t)
		(replace-match " " t t))
	      ;; Make it possible to search for `\nFIELD'.
	      (goto-char (point-min))
	      (insert "\n")
	      ;; Extract From:
	      (goto-char (point-min))
	      (if (search-forward "\nFrom: " nil t)
		  (setq from (buffer-substring
			      (point)
			      (save-excursion (end-of-line) (point))))
		(setq from "(Unknown User)"))
	      ;; Extract Subject:
	      (goto-char (point-min))
	      (if (search-forward "\nSubject: " nil t)
		  (setq subject (buffer-substring
				 (point)
				 (save-excursion (end-of-line) (point))))
		(setq subject "(None)"))
	      ;; Extract Message-ID:
	      (goto-char (point-min))
	      (if (search-forward "\nMessage-ID: " nil t)
		  (setq message-id (buffer-substring
				    (point)
				    (save-excursion (end-of-line) (point))))
		(setq message-id nil))
	      ;; Extract Date:
	      (goto-char (point-min))
	      (if (search-forward "\nDate: " nil t)
		  (setq date (buffer-substring
			      (point)
			      (save-excursion (end-of-line) (point))))
		(setq date nil))
	      ;; Extract Lines:
	      (goto-char (point-min))
	      (if (search-forward "\nLines: " nil t)
		  (setq lines (string-to-int
			       (buffer-substring
				(point)
				(save-excursion (end-of-line) (point)))))
		(setq lines 0))
	      ;; Extract Xref:
	      (goto-char (point-min))
	      (if (search-forward "\nXref: " nil t)
		  (setq xref (buffer-substring
			      (point)
			      (save-excursion (end-of-line) (point))))
		(setq xref nil))
	      ;; Extract References:
	      ;; If no References: field, use In-Reply-To: field instead.
	      ;; Suggested by tanaka@flab.fujitsu.co.jp (Hiroshi TANAKA).
	      (goto-char (point-min))
	      (if (or (search-forward "\nReferences: " nil t)
		      (search-forward "\nIn-Reply-To: " nil t))
		  (setq references (buffer-substring
				    (point)
				    (save-excursion (end-of-line) (point))))
		(setq references nil))
	      (setq headers
		    (cons (vector article subject from
				  xref lines date
				  message-id references) headers))
	      ))
	(setq sequence (cdr sequence))
	(setq count (1+ count))
	(and (numberp nntp-large-newsgroup)
	     (> number nntp-large-newsgroup)
	     (zerop (% count 20))
	     (message "MHSPOOL: %d%% of headers received."
		      (/ (* count 100) number)))
	)
      (and (numberp nntp-large-newsgroup)
	   (> number nntp-large-newsgroup)
	   (message "MHSPOOL: 100%% of headers received."))
      (nreverse headers)
      )))


;;;
;;; Replacement of NNTP Raw Interface.
;;;

(defun mhspool-open-server (host &optional service)
  "Open news server on HOST.
If HOST is nil, use value of environment variable `NNTPSERVER'.
If optional argument SERVICE is non-nil, open by the service name."
  (let ((host (or host (getenv "NNTPSERVER")))
	(status nil))
    ;; Get directory name from HOST name.
    (if (string-match ":\\(.+\\)$" host)
	(progn
	  (setq mhspool-spool-directory
		(file-name-as-directory
		 (expand-file-name
		  (substring host (match-beginning 1) (match-end 1))
		  (expand-file-name "~/" nil))))
	  (setq host (system-name)))
      (setq mhspool-spool-directory nil))
    (setq nntp-status-string "")
    (cond ((and (stringp host)
		(stringp mhspool-spool-directory)
		(file-directory-p mhspool-spool-directory)
		(string-equal host (system-name)))
	   (setq status (mhspool-open-server-internal host service)))
	  ((string-equal host (system-name))
	   (setq nntp-status-string
		 (format "No such directory: %s.  Goodbye."
			 mhspool-spool-directory)))
	  ((null host)
	   (setq nntp-status-string "NNTP server is not specified."))
	  (t
	   (setq nntp-status-string
		 (format "MHSPOOL: cannot talk to %s." host)))
	  )
    status
    ))

(defun mhspool-close-server ()
  "Close news server."
  (mhspool-close-server-internal))

(fset 'mhspool-request-quit (symbol-function 'mhspool-close-server))

(defun mhspool-server-opened ()
  "Return server process status, T or NIL.
If the stream is opened, return T, otherwise return NIL."
  (and nntp-server-buffer
       (get-buffer nntp-server-buffer)))

(defun mhspool-status-message ()
  "Return server status response as string."
  nntp-status-string
  )

(defun mhspool-request-article (id)
  "Select article by message ID (or number)."
  (let ((file (concat mhspool-current-directory (prin1-to-string id))))
    (if (and (stringp file)
	     (file-exists-p file)
	     (not (file-directory-p file)))
	(save-excursion
	  (mhspool-find-file file)))
    ))

(defun mhspool-request-body (id)
  "Select article body by message ID (or number)."
  (if (mhspool-request-article id)
      (save-excursion
	(set-buffer nntp-server-buffer)
	(goto-char (point-min))
	(if (search-forward "\n\n" nil t)
	    (delete-region (point-min) (point)))
	t
	)
    ))

(defun mhspool-request-head (id)
  "Select article head by message ID (or number)."
  (if (mhspool-request-article id)
      (save-excursion
	(set-buffer nntp-server-buffer)
	(goto-char (point-min))
	(if (search-forward "\n\n" nil t)
	    (delete-region (1- (point)) (point-max)))
	t
	)
    ))

(defun mhspool-request-stat (id)
  "Select article by message ID (or number)."
  (error "MHSPOOL: STAT is not implemented."))

(defun mhspool-request-group (group)
  "Select news GROUP."
  (cond ((file-directory-p
	  (mhspool-article-pathname group))
	 ;; Mail/NEWS.GROUP/N
	 (setq mhspool-current-directory
	       (mhspool-article-pathname group)))
	((file-directory-p
	  (mhspool-article-pathname
	   (mhspool-replace-chars-in-string group ?. ?/)))
	 ;; Mail/NEWS/GROUP/N
	 (setq mhspool-current-directory
	       (mhspool-article-pathname
		(mhspool-replace-chars-in-string group ?. ?/))))
	))

(defun mhspool-request-list ()
  "List valid newsgoups."
  (save-excursion
    (let* ((newsgroup nil)
	   (articles nil)
	   (directory (file-name-as-directory
		       (expand-file-name mhspool-spool-directory nil)))
	   (folder-regexp (concat "^" (regexp-quote directory) "\\(.+\\):$"))
	   (buffer (get-buffer-create " *GNUS file listing*")))
      (set-buffer nntp-server-buffer)
      (erase-buffer)
      (set-buffer buffer)
      (erase-buffer)
      (apply 'call-process
	     "ls" nil t nil
	     (append mhspool-list-directory-switches (list directory)))
      (goto-char (point-min))
      (while (re-search-forward folder-regexp nil t)
	(setq newsgroup
	      (mhspool-replace-chars-in-string
	       (buffer-substring (match-beginning 1) (match-end 1)) ?/ ?.))
	(setq articles nil)
	(forward-line 1)		;(beginning-of-line)
	;; Thank nobu@flab.fujitsu.junet for his bug fixes.
	(while (and (not (eobp))
		    (not (looking-at "^$")))
	  (if (looking-at "^[0-9]+$")
	      (setq articles
		    (cons (string-to-int
			   (buffer-substring
			    (match-beginning 0) (match-end 0)))
			  articles)))
	  (forward-line 1))
	(if articles
	    (princ (format "%s %d %d n\n" newsgroup
			   (apply (function max) articles)
			   (apply (function min) articles))
		   nntp-server-buffer))
	)
      (kill-buffer buffer)
      (set-buffer nntp-server-buffer)
      (buffer-size)
      )))

(defun mhspool-request-last ()
  "Set current article pointer to the previous article
in the current news group."
  (error "MHSPOOL: LAST is not implemented."))

(defun mhspool-request-next ()
  "Advance current article pointer."
  (error "MHSPOOL: NEXT is not implemented."))

(defun mhspool-request-post ()
  "Post a new news in current buffer."
  (setq nntp-status-string "MHSPOOL: what do you mean post?")
  nil
  )


;;;
;;; Replacement of Low-Level Interface to NNTP Server.
;;; 

(defun mhspool-open-server-internal (host &optional service)
  "Open connection to news server on HOST by SERVICE (default is nntp)."
  (save-excursion
    (if (not (string-equal host (system-name)))
	(error "MHSPOOL: cannot talk to %s." host))
    ;; Initialize communication buffer.
    (setq nntp-server-buffer (get-buffer-create " *nntpd*"))
    (set-buffer nntp-server-buffer)
    (buffer-flush-undo (current-buffer))
    (erase-buffer)
    (kill-all-local-variables)
    (setq case-fold-search t)		;Should ignore case.
    (setq nntp-server-process nil)
    (setq nntp-server-name host)
    ;; It is possible to change kanji-fileio-code in this hook.
    (run-hooks 'nntp-server-hook)
    t
    ))

(defun mhspool-close-server-internal ()
  "Close connection to news server."
  (if nntp-server-buffer
      (kill-buffer nntp-server-buffer))
  (setq nntp-server-buffer nil)
  (setq nntp-server-process nil))

(defun mhspool-find-file (file)
  "Insert FILE in server buffer safely."
  (set-buffer nntp-server-buffer)
  (erase-buffer)
  (condition-case ()
      (progn
	(insert-file-contents file)
	(goto-char (point-min))
	;; If there is no body, `^L' appears at end of file. Special
	;; hack for MH folder.
	(and (search-forward "\n\n" nil t)
	     (string-equal (buffer-substring (point) (point-max)) "\^L")
	     (delete-char 1))
	t
	)
    (file-error nil)
    ))

(defun mhspool-article-pathname (group)
  "Make pathname for GROUP."
  (concat (file-name-as-directory mhspool-spool-directory) group "/"))

(defun mhspool-replace-chars-in-string (string from to)
  "Replace characters in STRING from FROM to TO."
  (let ((string (substring string 0))	;Copy string.
	(len (length string))
	(idx 0))
    ;; Replace all occurence of FROM with TO.
    (while (< idx len)
      (if (= (aref string idx) from)
	  (aset string idx to))
      (setq idx (1+ idx)))
    string
    ))
