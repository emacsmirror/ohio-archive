;; mew-addrbook.el --- Completion magic for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Mar 22, 1999
;; Revised: Aug 30, 1999

;;; Code:

(defconst mew-addrbook-version "mew-addrbook.el version 0.02")

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Address book
;;;

(defvar mew-addrbook-mode-map nil)

(if mew-addrbook-mode-map
    ()
  (setq mew-addrbook-mode-map (make-sparse-keymap))
  (mew-set-keymap-parent mew-addrbook-mode-map text-mode-map)
  (define-key mew-addrbook-mode-map "\C-c\C-c" 'mew-addrbook-register)
  (define-key mew-addrbook-mode-map "\C-c\C-q" 'mew-addrbook-kill))

(defvar mew-addrbook-mode-alias "Alias")
(defvar mew-addrbook-mode-personalinfo "Personal Info")

(defvar mew-addrbook-strip-domainpart t
  "*If *non-nil*, a shortname is created by stripping its domain part.")

;;

(defvar mew-addrbook-alist nil
  "(key addr) or (key (addr1, addr2) nickname name)")
(defvar mew-alias-auto-alist nil
  "(key addr)")
(defvar mew-alias-auto-file-name ".mew-alias")

;;

(defun mew-addrbook-setup ()
  (if (and mew-alias-auto-file-name (null mew-alias-auto-alist))
      ;; make auto-alist only at the initialization time
      ;; not at update time (auto-alist have not been saved yet)
      (setq mew-alias-auto-alist (mew-lisp-load mew-alias-auto-file-name)))
  (setq mew-addrbook-alist (mew-addrbook-make-alist))
  (if (listp mew-addrbook-alist) ;; including nil
      ;; mew-alias-auto-alist is used independently so must use copy-alist
      (setq mew-addrbook-alist
	    (nconc mew-addrbook-alist (copy-alist mew-alias-auto-alist)))
    ;; addrbook does not exist. Backward compatibility only
    (setq mew-addrbook-alist (nconc (mew-alias-make-alist)
				    (copy-alist mew-alias-auto-alist)
				    (mew-petname-make-alist))))
  (setq mew-addrbook-alist (mew-uniq-alist mew-addrbook-alist))
  (add-hook 'kill-emacs-hook (function mew-addrbook-clean-up)))

(defun mew-addrbook-clean-up ()
  (remove-hook 'kill-emacs-hook (function mew-addrbook-clean-up))
  (mew-lisp-save mew-alias-auto-file-name mew-alias-auto-alist))

;;

(defmacro mew-addrbook-func (key)
  (` (cdr (assoc (, key) mew-addrbook-switch))))

;;

(defmacro mew-alias-get (key)
  (` (mew-addrbook-alias-get (, key) mew-addrbook-alist)))

(defmacro mew-alias-next (key)
  (` (mew-addrbook-alias-next (, key) mew-addrbook-alist)))

(fset 'mew-addrbook-alias-hit (symbol-function 'assoc))

(defun mew-addrbook-alias-get (key alist)
  (let ((addrs (mew-addrbook-alias-get1 key alist 0)))
    (cond
     ((stringp addrs) addrs)
     ((listp addrs)
      (mapconcat (function identity) (nreverse addrs) ", "))
     (t key))))

(defun mew-addrbook-alias-get1 (key alist n)
  "Expand KEY to addresses according ALIST.
If addresses is a list, that follows one-of convention and
return the first member of the list.
If addresses is a string, expands it recursively."
  (let* ((crnt (nth 1 (mew-addrbook-alias-hit key alist)))
	 (keys (and (stringp crnt)
		    (mapcar (function mew-chop) (mew-split crnt ?,))))
	 ret tmp)
    (cond
     ((> n mew-expand-max-depth) key)
     ((null crnt) key)
     ((listp crnt) (car crnt))
     (t
      (while keys
	(setq tmp (mew-addrbook-alias-get1 (car keys) alist (1+ n)))
	(if (listp tmp)
	    (setq ret (nconc tmp ret))
	  (setq ret (cons tmp ret)))
	(setq keys (cdr keys)))
      ret))))

(defun mew-addrbook-alias-next (key alist)
  (let* ((addrs (nth 1 (mew-assoc-member key alist 1))))
    (if (and addrs (listp addrs))
	(mew-get-next addrs key))))

(defun mew-addrbook-alias-add (addr)
  (if (and (stringp addr) (string-match "@" addr))
      (let* ((user (mew-addrstr-extract-user addr))
	     (match-auto (assoc user mew-alias-auto-alist))
	     (match-adbk (assoc user mew-addrbook-alist)))
	(cond
	 (match-auto
	  (cond
	   ((equal addr (nth 1 match-auto))
	    ;; move the entry to the top for the recent-used-first.
	    (setq mew-alias-auto-alist
		  (cons match-auto (delete match-auto mew-alias-auto-alist))))
	   (mew-addrbook-override-by-newone
	    ;; override match-auto by (user addr)
	    (setq mew-addrbook-alist
		  (cons (list user addr)
			(delete match-auto mew-addrbook-alist)))
	    (setq mew-alias-auto-alist
		  (cons (list user addr)
			(delete match-auto mew-alias-auto-alist))))
	   (t 
	    ;; the old entry remains
	    )))
	 (match-adbk
	  ;; do nothing
	  )
	 (t
	  (setq mew-addrbook-alist (cons (list user addr) mew-addrbook-alist))
	  (setq mew-alias-auto-alist
		(cons (list user addr) mew-alias-auto-alist)))))))

(defun mew-addrbook-alias-delete (addr)
  (if (and (stringp addr) (string-match "@" addr))
      (let* ((user (mew-addrstr-extract-user addr))
	     (ent (assoc user mew-alias-auto-alist)))
	(if (and ent (equal (cdr ent) addr))
	    (progn
	      (setq mew-addrbook-alist (delete ent mew-addrbook-alist))
	      (setq mew-alias-auto-alist (delete ent mew-alias-auto-alist)))
	  ))))

;;

(defun mew-addrbook-shortname-get (addr)
  (nth 0 (mew-assoc-member-case-equal addr mew-addrbook-alist 1)))

(defun mew-addrbook-nickname-get (addr)
  (nth 2 (mew-assoc-member-case-equal addr mew-addrbook-alist 1)))

(defun mew-addrbook-name-get (addr)
  (nth 3 (mew-assoc-member-case-equal addr mew-addrbook-alist 1)))
;;

(defun mew-addrbook-insert-file (file cregex &optional unquote)
  (if (not (stringp file))
      ()
    (let* ((case-fold-search t)
	   (pars (mew-split file ?,)) ;; parents
	   (files pars) ;; included
	   par chr path beg qchar ret)
      ;; include parents files
      (while pars
	(setq par (car pars))
	(setq pars (cdr pars))
	(if (not (file-readable-p par))
	    ()
	  (setq ret t)
	  (insert-file-contents par)
	  (setq path (file-name-directory par))
	  ;; include children files
	  (while (re-search-forward "^<[ \t]*\\([^ \t\n]+\\).*$" nil t)
	    (setq chr (expand-file-name (mew-match 1) path))
	    (delete-region (match-beginning 0) (match-end 0))
	    (if (and (file-readable-p chr) (not (member chr files)))
		(progn
		  (insert-file-contents chr)
		  (setq files (cons chr files)))))
	  (goto-char (point-max))))
      ;; remove commets
      (goto-char (point-min))
      (while (re-search-forward cregex nil t)
	(delete-region (match-beginning 0) (match-end 0)))
      ;; concat continuation lines
      (goto-char (point-min))
      (while (re-search-forward "\\\\\n" nil t)
	(delete-region (match-beginning 0) (match-end 0)))
      ;; concat separated lines by comma
      (goto-char (point-min))
      (while (re-search-forward ",$" nil t)
	(end-of-line)
	(forward-char 1)
	(delete-backward-char 1))
      ;; replace ", " to "\0" inside/ouside quote.
      (goto-char (point-min))
      (while (re-search-forward ",[ \t]+" nil t)
	(replace-match ",\0" nil t))
      ;; unquote, replace white spaces to "\0".
      (goto-char (point-min))
      (if unquote
	  (catch 'quote
	    (while (re-search-forward "[\"']" nil t)
	      (setq qchar (char-before (point)))
	      ;; (point) is for backward compatibility
	      (backward-delete-char 1) ;; delete quote
	      (setq beg (point))
	      (if (not (re-search-forward (char-to-string qchar) nil t))
		  (throw 'quote nil) ;; error
		(backward-delete-char 1) ;; delete quote
		(save-restriction
		  (narrow-to-region beg (point))
		  (goto-char (point-min))
		  (while (re-search-forward "[ \t]+" nil t)
		    (replace-match "\0" nil t))
		  (goto-char (point-max))))))) ;; just in case
      ;; remove optional white spaces
      (goto-char (point-min))
      (while (re-search-forward "[ \t]+" nil t)
	(replace-match " " nil t))
      ret)))

(defun mew-addrbook-strsafe (var)
  (if (or (string-equal "" var) (string-equal "*" var))
      nil
    (mew-replace-character var 0 32)))

(defun mew-addrbook-make-alist ()
  (save-excursion
    (let (alias colon addrs nick name alist)
      (mew-set-buffer-tmp)
      (if (not (mew-addrbook-insert-file
		mew-addrbook-file mew-addrbook-comment-regex 'unquote))
	  'addrbook-does-not-exist
	(goto-char (point-min))
	(while (re-search-forward "^ ?\\([^ \n:]+\\) ?\\(:?\\) ?\\([^ \n]+\\)" nil t)
	  (setq alias (mew-addrbook-strsafe (mew-match 1)))
	  (setq colon (mew-match 2))
	  (setq addrs (mew-addrbook-strsafe (mew-match 3)))
	  (if (equal colon ":")
	      (setq alist (cons (list alias addrs) alist))
	    (and addrs (setq addrs (mapcar (function mew-chop) 
					   (mew-split addrs ?,))))
	    (if (looking-at " ?\\([^ \n]*\\) ?\\([^ \n]*\\)")
		(progn
		  (setq nick (mew-addrbook-strsafe (mew-match 1)))
		  (setq name (mew-addrbook-strsafe (mew-match 2))))
	      (setq nick nil)
	      (setq name nil))
	    (setq alist (cons (list alias addrs nick name) alist))))
	(nreverse alist)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Addrbook mode
;;;

(defun mew-summary-addrbook-add (&optional personalinfo)
  "Adding the value of From: in Message mode to Addrbook. When
executed with '\\[universal-argument]', it will add personal information.  Otherwise,
it will add an alias."
  (interactive "P")
  (mew-summary-display nil)
  (let ((buf (mew-buffer-message))
	from shortname addrs name)
    (save-excursion
      (set-buffer buf)
      (if (mew-header-p)
	  ()
	(setq buf (mew-current-get 'cache))
	(if (null buf)
	    (message "No address to be registered")
	  (set-buffer buf)))
      (setq from (mew-header-get-value mew-from:))
      (if (null from)
	  (message "No address to be registered")
	(setq addrs (mew-addrstr-parse-address from))
 	(if (mew-is-my-address (mew-get-my-address-regex-list) addrs)
 	    (if personalinfo
 		(setq addrs (car (mew-header-parse-address-list
 				  (list mew-to:))))
	      (setq addrs
		    (mapconcat (function identity)
			       (mew-header-parse-address-list
				(list mew-to: mew-cc:))
			       ",")))
 	  (if (string-match "\\(.*\\)<.*>" from)
 	      (progn
 		(setq name (mew-match 1 from))
 		(setq name (mew-chop name)))))
 	(if (not addrs)
 	    (message "No address to be registered")
 	  (if mew-addrbook-strip-domainpart
 	      (setq shortname (mew-addrstr-extract-user addrs))
 	    (setq shortname addrs))
 	  (mew-addrbook-prepare-template personalinfo shortname addrs nil name))
	))))

(defun mew-addrbook-prepare-template (personalinfop shortname addrs &optional nickname name)
  (delete-other-windows)
  (switch-to-buffer (get-buffer-create mew-buffer-addrbook))
  (mew-erase-buffer)
  (insert "#If you want to register this entry, type "
	  (substitute-command-keys
	   "'\\<mew-addrbook-mode-map>\\[mew-addrbook-register]'.\n")
	  "#If you want to NOT register this entry, type "
	  (substitute-command-keys
	   "'\\<mew-addrbook-mode-map>\\[mew-addrbook-kill]'.\n"))
  (mew-addrbook-insert-template "Shortname" shortname)
  (mew-addrbook-insert-template "Addresses" addrs)
  (cond
   (personalinfop
    (mew-addrbook-insert-template "Nickname" nickname)
    (mew-addrbook-insert-template "Name" name)
    (mew-addrbook-mode mew-addrbook-mode-personalinfo))
   (t
    (mew-addrbook-mode mew-addrbook-mode-alias)))
  (mew-addrbook-insert-template "Comments" nil)
  (goto-char (point-min))
  (search-forward ": " nil t))

(defun mew-addrbook-insert-template (key val)
  (mew-elet
   (let ((beg (point)))
     (insert key ": ")
     (put-text-property beg (point) 'read-only t)
     (mew-rear-nonsticky beg (point))
     (and val (insert val))
     (insert "\n"))))

(defun mew-addrbook-mode (mname)
  "\\<mew-addrbook-mode-map>
Mew Addrbook mode:: major mode to register Addrbook.
The keys that are defined for this mode are:

\\[mew-addrbook-register]	Register information in Addrbook mode to Addrbook.
\\[mew-addrbook-kill]	Kill Addrbook mode.
"
  (interactive)
  (setq major-mode 'mew-addrbook-mode)
  (setq mode-name mname)
  (setq mode-line-buffer-identification mew-mode-line-id)
  (use-local-map mew-addrbook-mode-map)
  (run-hooks 'mew-addrbook-mode-hook)
  (setq buffer-undo-list nil))

(defun mew-addrbook-register ()
  "Register information in Addrbook mode to Addrbook."
  (interactive)
  (let ((shortname (mew-header-get-value "Shortname:"))
	(addrs     (mew-header-get-value "Addresses:"))
	(nickname  (mew-header-get-value "Nickname:"))
	(name      (mew-header-get-value "Name:"))
	(comments  (mew-header-get-value "Comments:"))
	(mode mode-name)
	buf addrsl errmsg not-uniq)
     (cond
      ((equal mode mew-addrbook-mode-alias)
       (cond
	((and (null shortname) (null addrs))
	 (setq errmsg "Must fill both Shortname and Addresses."))
	((null shortname)
	 (setq errmsg "Must fill Shortname."))
	((null addrs)
	 (setq errmsg "Must fill Addresses."))))
      (t
       (cond
	((null addrs)
	 (setq errmsg "Must fill Addresses."))
	((and (null shortname) (null nickname) (null name))
	 (setq errmsg "Must fill Shortname or Nickname or Name."))
	((and name (string-match "^\"[^\"]*[^\000-\177]" name))
	 (setq errmsg "Remove quote around non-ASCII Name.")))))
     (if errmsg
	 (message errmsg)
       (save-excursion
	 (setq buf (find-file-noselect mew-addrbook-file))
	 (set-buffer buf)
	 (goto-char (point-min))
	 (if (and shortname
		  (re-search-forward 
		   (concat "^" (regexp-quote shortname) "[ \t]*:?[ \t]+") nil t))
	     (setq not-uniq t))
	 (if not-uniq
	     () ;; see later
	   ;; All errors are checked.
	   (goto-char (point-max))
	   (if (not (bolp)) (insert "\n"))
	   (cond
	    ((equal mode mew-addrbook-mode-alias)
	     (setq mew-addrbook-alist
		   (cons (list shortname addrs) mew-addrbook-alist))
	     (insert shortname ":\t" addrs))
	    (t
	     (setq addrsl (mew-addrstr-parse-address-list addrs))
	     (setq mew-addrbook-alist
		   (cons (list shortname addrsl nickname name) mew-addrbook-alist))
	     (if (null shortname) (setq shortname "*"))
	     (if (and nickname (string-match "^[^\" \t]+[ \t]+.*$" nickname))
		 (setq nickname (concat "\"" nickname "\"")))
	     (if (and name (string-match "^[^\" \t]+[ \t]+.*$" name))
		 (setq name (concat "\"" name "\"")))
	     (if name
		 (insert shortname "\t" addrs "\t" (or nickname "*") "\t" name)
	       (if nickname
		   (insert shortname "\t" addrs "\t" nickname)
		 (insert shortname "\t" addrs)))))
	   (if comments
	       (insert "\t#" comments "\n")
	     (insert "\n"))
	   (save-buffer)))
       ;; Addrbook buffer
       (kill-buffer buf)
       (if not-uniq
	   (message "Shortname is already used. Change Shortname.")
	 (mew-addrbook-kill 'no-msg)
	 (message "Registered to Addrbook.")))))

(defun mew-addrbook-kill (&optional no-msg)
  "Kill Addrbook mode."
  (interactive "P")
  (kill-buffer (current-buffer))
  (or no-msg (message "Not registered.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Old aliases and petnames
;;;

(defun mew-alias-make-alist ()
  "Make alias alist with association of (alias . expantion).
Currently, only 'user: user@domain' syntax is supported."
  (save-excursion
    (let (alias expn alist)
      (mew-set-buffer-tmp)
      (mew-addrbook-insert-file mew-alias-file mew-alias-comment-regex)
      (goto-char (point-min))
      (while (re-search-forward "^ ?\\([^ \n:]+\\) ?: ?\\(.*\\)$" nil t)
	(setq alias (mew-addrbook-strsafe (mew-match 1)))
	(setq expn (mew-addrbook-strsafe (mew-match 2)))
	;; append for first assoc comes first
	(setq alist (cons (list alias expn) alist)))
      (nreverse alist))))

(defun mew-petname-make-alist ()
  (if (and mew-petname-file (file-readable-p mew-petname-file))
      (save-excursion
	(let (ret)
	  (mew-set-buffer-tmp)
	  (insert-file-contents mew-petname-file)
	  (goto-char (point-min))
	  (while (not (eobp))
	    (if (looking-at "^\\([^ \t]+\\)[ \t]+\"?\\([^\"\n]+\\)\"?$")
		(setq ret (cons (list nil (list (mew-match 1)) (mew-match 2) nil) ret)))
	    (forward-line))
	  ret))))

(provide 'mew-addrbook)

;;; Copyright Notice:

;; Copyright (C) 1999 Mew developing team.
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. Neither the name of the team nor the names of its contributors
;;    may be used to endorse or promote products derived from this software
;;    without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE TEAM AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE TEAM OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; mew-addrbook.el ends here
