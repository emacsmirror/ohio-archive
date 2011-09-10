;From: ange@hplb.hpl.hp.com (Andy Norman)
;Newsgroups: gnu.emacs,comp.emacs
;Subject: Re: ange-tags: simple support for multiple tag tables
;Message-ID: <ANGE.89Aug9115317@anorman.hpl.hp.com>
;Date: 9 Aug 89 15:53:17 GMT
;References: <ANGE.89Aug1194214@anorman.hpl.hp.com>
;Organization: Hewlett-Packard Laboratories, Bristol, UK.
;Lines: 149
;
;I've extended ange-tags to include the functionality of tags-search and
;tags-query-replace (with help from Ramana Rao). I've also taken the
;opportunity to simplify it a bit.
;
;Enjoy...
;
;--------------------------------------------------------------------------------
; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         ange-tags.el
; RCS:          $Header: ange-tags.el,v 1.11 89/08/07 12:19:51 ange Exp $
; Description:  simple support for multiple tags files
; Author:       Andy Norman, Kraken
; Created:      Tue Aug  1 14:28:03 1989
; Modified:     Mon Aug  7 12:17:17 1989 (Ange) ange@hplb.hpl.hp.com
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Revisions:
;
; Mon Aug  7 11:07:16 1989 (Ange) ange@hplb.hpl.hp.com
;  Added ange-visit-tags-table, ange-tags-search and ange-tags-query-replace
;  based on code by Ramana Rao (rao@arisia.xerox.com).
;  Isolated visiting tags tables to ange-visit-tags-table.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; put the following in your .emacs
;;
;(autoload 'ange-find-tag "ange-tags" "" t)
;(autoload 'ange-tags-loop-continue "ange-tags" "" t)
;(autoload 'ange-find-tag-other-window "ange-tags" "" t)
;
;(global-set-key "\M-." 'ange-find-tag)
;(global-set-key "\M-," 'ange-tags-loop-continue)
;(global-set-key "\C-x4." 'ange-find-tag-other-window)

(provide 'ange-tags)

(autoload 'find-tag-tag "tags" "" t)	; we need this later

(defvar ange-tags-alist nil
  "Alist of id vs buffername/filename pattern vs tag table.
Each element looks like (ID REGEXP TAGTABLE)")

(defvar ange-last-tag nil
  "Tag found by the last ange-find-tag.")

(defvar ange-tags-file-name nil
  "File name of tag table.")


(defun ange-re-assoc (key alist)
  "Similar to assoc, but uses string-match rather than equal."
  (cond ((null alist)
	 nil)
	((string-match (nth 1 (car alist)) key)
	 (car alist))
	(t (ange-re-assoc key (cdr alist)))))


(defun ange-visit-tags-table ()
  "Finds the tag table associated with the current buffer. If there is no tag table
then call visit-tags-table to set one. Returns a string identifying the tag table.

To see if there is a tags table associated with the current buffer, a key is
generated by using either the buffer's visited filename, or its name. Each entry
in ange-tags-alist is compared to this key until a match succeeds or the end of
the alist is reached. If a match succeeded then the tags table named in the
matching entry is used, otherwise the default tags table in tags-file-name
is used instead.

See the documentation for ange-tags-alist and tags-file-name for more details."
  (let* ((key (or (buffer-file-name) (buffer-name)))
	 (entry (ange-re-assoc key ange-tags-alist)))
    (if entry
	(progn
	  (setq ange-tags-file-name (substitute-in-file-name (nth 2 entry)))
	  (visit-tags-table ange-tags-file-name)
	  (concat (car entry) " "))
      (or tags-file-name (call-interactively 'visit-tags-table))
      (setq ange-tags-file-name tags-file-name)
      "")))


(defun ange-find-tag-tag (prompt)
  "Similar to find-tag-tag except that it uses the tag table associated with the
current buffer."
  (find-tag-tag (format prompt (ange-visit-tags-table))))


(defun ange-find-tag (tagname &optional next other-window)
  "Similar to find-tag except that the tag table associated with the current
buffer is used. See the documentation for find-tag and ange-tags-alist for
more details."
  (interactive (if current-prefix-arg
		   '(nil t)
		 (ange-find-tag-tag "Find %stag: ")))
  (let ((tags-file-name ange-tags-file-name)
	(last-tag ange-last-tag))
    (find-tag tagname next other-window)
    (setq ange-last-tag last-tag)))


(defun ange-tags-loop-continue (&optional first-time)
  "Similar to tags-loop-continue except that it remembers the name of the
tag file used on the last tag operation. See the documentaton for 
tags-loop-continue for more details."
  (interactive)
  (let ((tags-file-name ange-tags-file-name)
	(last-tag ange-last-tag))
    (tags-loop-continue first-time)))


(defun ange-find-tag-other-window (tagname &optional next)
  "Similar to find-tag-other-window except that the tag table associated with 
the current buffer is used."
  (interactive (if current-prefix-arg
		   '(nil t)
		 (ange-find-tag-tag "Find %stag other window: ")))
  (ange-find-tag tagname next t))


(defun ange-tags-search (regexp)
  "Similiar to tags-search except that tag table associated with the current
buffer is used."
  (interactive 
    (list
      (read-string (concat (ange-visit-tags-table) "Tags search (regexp): "))))
  (let ((tags-file-name ange-tags-file-name)
	(last-tag ange-last-tag))
    (tags-search regexp)))


(defun ange-tags-query-replace (from to &optional delimited)
  "Similiar to tags-query-replace except that tag table associated with the 
current buffer is used."
  (interactive 
    (let ((id (ange-visit-tags-table)))
      (list
       (read-string (format "%sTags query replace (regexp): " id ))
       (read-string (format "%sTags query replace by: " id)))))
  (let ((tags-file-name ange-tags-file-name)
	(last-tag ange-last-tag))
    (tags-query-replace from to delimited)))
;--
;					-- ange --
;
;					ange@hplb.hpl.hp.com
