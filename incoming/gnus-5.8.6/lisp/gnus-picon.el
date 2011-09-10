;;; gnus-picon.el --- displaying pretty icons in Gnus
;; Copyright (C) 1996,97,98,99 Free Software Foundation, Inc.

;; Author: Wes Hardaker <hardaker@ece.ucdavis.edu>
;; Keywords: news xpm annotation glyph faces

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'gnus)
;; (require 'xpm)
(require 'annotations)
(require 'custom)
(require 'gnus-art)
(require 'gnus-win)

;;; User variables:

(defgroup picons nil
  "Show pictures of people, domains, and newsgroups (XEmacs).
For this to work, you must switch on the `gnus-treat-display-picons'
variable."
  :group 'gnus-visual)

(defcustom gnus-picons-display-where 'picons
  "Where to display the group and article icons.
Valid values are `article' and `picons'."
  :type '(choice symbol string)
  :group 'picons)

(defcustom gnus-picons-has-modeline-p t
  "*Whether the picons window should have a modeline.
This is only useful if `gnus-picons-display-where' is `picons'."
  :type 'boolean
  :group 'picons)

(defcustom gnus-picons-database "/usr/local/faces"
  "*Defines the location of the faces database.
For information on obtaining this database of pretty pictures, please
see http://www.cs.indiana.edu/picons/ftp/index.html"
  :type 'directory
  :group 'picons)

(defcustom gnus-picons-news-directories '("news")
  "*List of directories to search for newsgroups faces."
  :type '(repeat string)
  :group 'picons)
(define-obsolete-variable-alias 'gnus-picons-news-directory
  'gnus-picons-news-directories)

(defcustom gnus-picons-user-directories '("local" "users" "usenix" "misc")
  "*List of directories to search for user faces."
  :type '(repeat string)
  :group 'picons)

(defcustom gnus-picons-domain-directories '("domains")
  "*List of directories to search for domain faces.
Some people may want to add \"unknown\" to this list."
  :type '(repeat string)
  :group 'picons)

(defcustom gnus-picons-refresh-before-display nil
  "*If non-nil, display the article buffer before computing the picons."
  :type 'boolean
  :group 'picons)

(defcustom gnus-picons-group-excluded-groups nil
  "*If this regexp matches the group name, group picons will be disabled."
  :type 'regexp
  :group 'picons)

(defcustom gnus-picons-display-as-address t
  "*If t display textual email addresses along with pictures."
  :type 'boolean
  :group 'picons)

(defcustom gnus-picons-file-suffixes
  (when (featurep 'x)
    (let ((types (list "xbm")))
      (when (featurep 'gif)
	(push "gif" types))
      (when (featurep 'xpm)
	(push "xpm" types))
      types))
  "*List of suffixes on picon file names to try."
  :type '(repeat string)
  :group 'picons)

(defcustom gnus-picons-display-article-move-p nil
  "*Whether to move point to first empty line when displaying picons.
This has only an effect if `gnus-picons-display-where' has value `article'."
  :type 'boolean
  :group 'picons)

(defcustom gnus-picons-clear-cache-on-shutdown t
  "*Whether to clear the picons cache when exiting gnus.
Gnus caches every picons it finds while it is running.  This saves
some time in the search process but eats some memory.  If this
variable is set to nil, Gnus will never clear the cache itself; you
will have to manually call `gnus-picons-clear-cache' to clear it.
Otherwise the cache will be cleared every time you exit Gnus."
  :type 'boolean
  :group 'picons)

(defcustom gnus-picons-piconsearch-url nil
  "*The url to query for picons.  Setting this to nil will disable it.
The only publicly available address currently known is
http://www.cs.indiana.edu:800/piconsearch.  If you know of any other,
please tell me so that we can list it."
  :type '(choice (const :tag "Disable" :value nil)
		 (const :tag "www.cs.indiana.edu"
			:value "http://www.cs.indiana.edu:800/piconsearch")
		 (string))
  :group 'picons)

(defface gnus-picons-xbm-face '((t (:foreground "black" :background "white")))
  "Face to show xbm picons in."
  :group 'picons)

(defface gnus-picons-face '((t (:foreground "black" :background "white")))
  "Face to show picons in."
  :group 'picons)

(defcustom gnus-picons-setup-hook nil
  "Hook run in Picons buffers."
  :group 'picons
  :type 'hook)

;;; Internal variables:

(defvar gnus-picons-setup-p nil)
(defvar gnus-picons-processes-alist nil
  "Picons processes currently running and their environment.")
(defvar gnus-picons-glyph-alist nil
  "Picons glyphs cache.
List of pairs (KEY . GLYPH) where KEY is either a filename or an URL.")
(defvar gnus-picons-url-alist nil
  "Picons file names cache.
List of pairs (KEY . NAME) where KEY is (USER HOST DBS) and NAME is an URL.")

(defvar gnus-picons-jobs-alist nil
  "List of jobs that still need be done.
This is a list of (SYM-ANN TAG ARGS...) where SYM-ANN three annotations list,
TAG is one of `picon' or `search' indicating that the job should query a
picon or do a search for picons file names, and ARGS is some additionnal
arguments necessary for the job.")

(defvar gnus-picons-job-already-running nil
  "Lock to ensure only one stream of http requests is running.")

;;; Functions:

(defun gnus-picons-remove-all ()
  "Removes all picons from the Gnus display(s)."
  (interactive)
  (map-extents (function (lambda (ext unused) (delete-annotation ext) nil))
	       nil nil nil nil nil 'gnus-picon)
  (setq gnus-picons-jobs-alist '())
  ;; notify running job that it may have been preempted
  (if (and (listp gnus-picons-job-already-running)
	   gnus-picons-job-already-running)
      (setq gnus-picons-job-already-running t)))

(defun gnus-get-buffer-name (variable)
  "Returns the buffer name associated with the contents of a variable."
  (let ((buf (gnus-get-buffer-create
	      (gnus-window-to-buffer-helper
	       (cdr (assq variable gnus-window-to-buffer))))))
    (and buf
	 (buffer-name buf))))

(defun gnus-picons-buffer-name ()
  (cond ((or (stringp gnus-picons-display-where)
	     (bufferp gnus-picons-display-where))
	 gnus-picons-display-where)
	((eq gnus-picons-display-where 'picons)
	 (if gnus-single-article-buffer
	     "*Picons*"
	   (concat "*Picons " gnus-newsgroup-name "*")))
	(t
	 (gnus-get-buffer-name gnus-picons-display-where))))

(defun gnus-picons-kill-buffer ()
  (let ((buf (get-buffer (gnus-picons-buffer-name))))
    (when (and (buffer-live-p buf)
	       (string-match "Picons" (buffer-name buf)))
      (kill-buffer buf))))

(defun gnus-picons-setup-buffer ()
  (let ((name (gnus-picons-buffer-name)))
    (save-excursion
      (if (and (get-buffer name)
	       (with-current-buffer name
		 gnus-picons-setup-p))
	  (set-buffer name)
	(set-buffer (gnus-get-buffer-create name))
	(buffer-disable-undo)
	(setq buffer-read-only t)
	(run-hooks 'gnus-picons-setup-hook)
	(set (make-local-variable 'gnus-picons-setup-p) t)
	(add-hook 'gnus-summary-prepare-exit-hook 'gnus-picons-kill-buffer))
      (current-buffer))))

(defun gnus-picons-set-buffer ()
  (set-buffer (gnus-picons-setup-buffer))
  (goto-char (point-min))
  (if (and (eq gnus-picons-display-where 'article)
	   gnus-picons-display-article-move-p)
      (if (search-forward "\n\n" nil t)
	  (forward-line -1)
	(goto-char (point-max)))
    (setq buffer-read-only t)
    (unless gnus-picons-has-modeline-p
      (set-specifier has-modeline-p
		     (list (list (current-buffer)
				 (cons nil gnus-picons-has-modeline-p)))))))

(defun gnus-picons-prepare-for-annotations ()
  "Prepare picons buffer for putting annotations."
  ;; let drawing catch up
  (when gnus-picons-refresh-before-display
    (sit-for 0))
  (gnus-picons-set-buffer)
  (gnus-picons-remove-all))

(defun gnus-picons-make-annotation (&rest args)
  (let ((annot (apply 'make-annotation args)))
    (set-extent-property annot 'gnus-picon t)
    (set-extent-property annot 'duplicable t)
    annot))

(defun gnus-article-display-picons ()
  "Display faces for an author and her domain in gnus-picons-display-where."
  (interactive)
  (let (from at-idx)
    (when (and (featurep 'xpm)
	       (or (not (fboundp 'device-type)) (equal (device-type) 'x))
	       (setq from (mail-fetch-field "from"))
	       (setq from (downcase (or (cadr (mail-extract-address-components
					       from))
					"")))
	       (or (setq at-idx (string-match "@" from))
		   (setq at-idx (length from))))
      (save-excursion
	(let ((username (downcase (substring from 0 at-idx)))
	      (addrs (if (eq at-idx (length from))
			 (if gnus-local-domain
			     (message-tokenize-header gnus-local-domain "."))
		       (message-tokenize-header (substring from (1+ at-idx))
						"."))))
	  (gnus-picons-prepare-for-annotations)
	  (gnus-group-display-picons)
	  (unless gnus-picons-display-article-move-p
	    (let ((buffer-read-only nil)
		  (case-fold-search t))
	      (when (re-search-forward "^From *: *" nil t)
		(when (search-forward from (gnus-point-at-eol) t)
		  (gnus-put-text-property
		   (match-beginning 0) (match-end 0)
		   'invisible t)))))
	  (if (null gnus-picons-piconsearch-url)
	      (progn
		(gnus-picons-display-pairs (gnus-picons-lookup-pairs
					    addrs
					    gnus-picons-domain-directories)
					   gnus-picons-display-as-address
					   "." t)
		(if (and gnus-picons-display-as-address addrs)
		    (gnus-picons-make-annotation
		     [string :data "@"] nil 'text nil nil nil t))
		(gnus-picons-display-picon-or-name
		 (gnus-picons-lookup-user username addrs)
		 username t))
	    (push (list 'gnus-article-annotations 'search username addrs
			gnus-picons-domain-directories t (point-marker))
		  gnus-picons-jobs-alist)
	    (gnus-picons-next-job)))))))

(defun gnus-group-display-picons ()
  "Display icons for the group in the `gnus-picons-display-where' buffer."
  (interactive)
  (when (and (featurep 'xpm)
	     (or (not (fboundp 'device-type)) (equal (device-type) 'x))
	     (or (null gnus-picons-group-excluded-groups)
		 (not (string-match gnus-picons-group-excluded-groups
				    gnus-newsgroup-name))))
    (let* ((newsgroups (mail-fetch-field "newsgroups"))
	   (groups
	    (if (or gnus-picons-display-article-move-p
		    (not newsgroups))
		(list (gnus-group-real-name gnus-newsgroup-name))
	      (split-string newsgroups ",")))
	   group)
      (save-excursion
	(gnus-picons-prepare-for-annotations)
	(while (setq group (pop groups))
	  (unless gnus-picons-display-article-move-p
	    (let ((buffer-read-only nil)
		  (case-fold-search t))
	      (goto-char (point-min))
	      (if (and (re-search-forward "^Newsgroups *: *" nil t)
		       (search-forward group (gnus-point-at-eol) t))
		  (gnus-put-text-property
		   (match-beginning 0) (match-end 0)
		   'invisible t)
		(let ((article-goto-body-goes-to-point-min-p nil))
		  (article-goto-body))
		(unless (bobp)
		  (backward-char 1)))))
	  (if (null gnus-picons-piconsearch-url)
	      (gnus-picons-display-pairs
	       (gnus-picons-lookup-pairs
		(reverse (split-string group "\\."))
		gnus-picons-news-directories)
	       t ".")
	    (push (list 'gnus-group-annotations 'search nil
			(split-string group "\\.")
			(if (listp gnus-picons-news-directories)
			    gnus-picons-news-directories
			  (list gnus-picons-news-directories))
			nil (point-marker))
		  gnus-picons-jobs-alist)
	    (gnus-picons-next-job))

	  (add-hook 'gnus-summary-exit-hook 'gnus-picons-remove-all))))))

(defun gnus-picons-lookup-internal (addrs dir)
  (setq dir (expand-file-name dir gnus-picons-database))
  (gnus-picons-try-face (dolist (part (reverse addrs) dir)
			  (setq dir (expand-file-name part dir)))))

(defun gnus-picons-lookup (addrs dirs)
  "Lookup the picon for ADDRS in databases DIRS.
Returns the picon filename or NIL if none found."
  (let (result)
    (while (and dirs (null result))
      (setq result (gnus-picons-lookup-internal addrs (pop dirs))))
    result))

(defun gnus-picons-lookup-user-internal (user domains)
  (let ((dirs gnus-picons-user-directories)
	domains-tmp dir picon)
    (while (and dirs (null picon))
      (setq domains-tmp domains
	    dir (pop dirs))
      (while (and domains-tmp
		  (null (setq picon (gnus-picons-lookup-internal
				     (cons user domains-tmp) dir))))
	(pop domains-tmp))
      ;; Also make a try in MISC subdir
      (unless picon
	(setq picon (gnus-picons-lookup-internal (list user "MISC") dir))))
    picon))

(defun gnus-picons-lookup-user (user domains)
  "Lookup the picon for USER at DOMAINS.
USER is a string containing a name.
DOMAINS is a list of strings from the fully qualified domain name."
  (or (gnus-picons-lookup-user-internal user domains)
      (gnus-picons-lookup-user-internal "unknown" domains)))

(defun gnus-picons-lookup-pairs (domains directories)
  "Lookup picons for DOMAINS and all its parents in DIRECTORIES.
Returns a list of PAIRS whose CAR is the picon filename or NIL if
none, and whose CDR is the corresponding element of DOMAINS."
  (let (picons)
    (setq directories (if (listp directories)
			  directories
			(list directories)))
    (while domains
      (push (list (gnus-picons-lookup (cons "unknown" domains) directories)
		  (pop domains))
	    picons))
    picons))

(defun gnus-picons-display-picon-or-name (picon name &optional right-p)
  (cond (picon (gnus-picons-display-glyph picon name right-p))
	(gnus-picons-display-as-address (list (gnus-picons-make-annotation
					       (vector 'string :data name)
					       nil 'text
					       nil nil nil right-p)))))

(defun gnus-picons-display-pairs (pairs &optional bar-p dot-p right-p)
  "Display picons in list PAIRS."
  (let ((domain-p (and gnus-picons-display-as-address dot-p))
	pair picons)
    (when (and bar-p domain-p right-p
	       gnus-picons-display-article-move-p)
      (setq picons (gnus-picons-display-glyph
		    (let ((gnus-picons-file-suffixes '("xbm")))
		      (gnus-picons-try-face
		       gnus-xmas-glyph-directory "bar."))
		    nil right-p)))
    (while (setq pair (pop pairs))
      (setq picons (nconc picons
			  (gnus-picons-display-picon-or-name
			   (car pair) (cadr pair) right-p)
			  (if (and domain-p pairs)
			      (list (gnus-picons-make-annotation
				     (vector 'string :data dot-p)
				     nil 'text nil nil nil right-p))))))
    picons))

(defun gnus-picons-try-face (dir &optional filebase)
  (let* ((dir (file-name-as-directory dir))
	 (filebase (or filebase "face."))
	 (key (concat dir filebase))
	 (glyph (cdr (assoc key gnus-picons-glyph-alist)))
	 (suffixes gnus-picons-file-suffixes)
	 f suf)
    (while (setq suf (pop suffixes))
      (when (file-exists-p (setq f (expand-file-name
				    (concat filebase suf)
				    dir)))
	(setq suffixes nil
	      glyph (make-glyph f))
	(if (equal suf "xbm")
	    (set-glyph-face glyph 'gnus-picons-xbm-face)
	  (set-glyph-face glyph 'gnus-picons-face))
	(push (cons key glyph) gnus-picons-glyph-alist)))
    glyph))

(defun gnus-picons-display-glyph (glyph &optional part rightp)
  (set-glyph-baseline glyph 70)
  (let ((new (gnus-picons-make-annotation
	      glyph (point) 'text nil nil nil rightp)))
    (when (and part gnus-picons-display-as-address)
      (set-annotation-data
       new (cons new (make-glyph (vector 'string :data part))))
      (set-annotation-action new 'gnus-picons-action-toggle))
    (nconc
     (list new)
     (if (and (eq major-mode 'gnus-article-mode)
	      (not gnus-picons-display-as-address)
	      (not part))
	 (list (gnus-picons-make-annotation [string :data " "] (point)
					    'text nil nil nil rightp))))))

(defun gnus-picons-action-toggle (data)
  "Toggle annotation."
  (interactive "e")
  (let* ((annot (car data))
	 (glyph (annotation-glyph annot)))
    (set-annotation-glyph annot (cdr data))
    (set-annotation-data annot (cons annot glyph))))

(defun gnus-picons-clear-cache ()
  "Clear the picons cache."
  (interactive)
  (setq gnus-picons-glyph-alist nil
	gnus-picons-url-alist nil))

(gnus-add-shutdown 'gnus-picons-close 'gnus)

(defun gnus-picons-close ()
  "Shut down the picons."
  (if gnus-picons-clear-cache-on-shutdown
      (gnus-picons-clear-cache)))

;;; Query a remote DB.  This requires some stuff from w3 !

(eval-and-compile
  (ignore-errors
    (require 'url)
    (require 'w3-forms)))

(defun gnus-picons-url-retrieve (url fn arg)
  (let ((old-asynch (default-value 'url-be-asynchronous))
	(url-working-buffer (generate-new-buffer " *picons*"))
	(url-package-name "Gnus")
	(url-package-version gnus-version-number)
	url-request-method)
    (setq-default url-be-asynchronous t)
    (save-excursion
      (set-buffer url-working-buffer)
      (setq url-be-asynchronous t
	    url-current-callback-data arg
	    url-current-callback-func fn)
      (url-retrieve url t))
    (setq-default url-be-asynchronous old-asynch)))

(defun gnus-picons-make-glyph (type)
  "Make a TYPE glyph using current buffer as data.  Handles xbm nicely."
  (cond ((null type) nil)
	((eq type 'xbm) (let ((fname (make-temp-name "/tmp/picon")))
			  (write-region (point-min) (point-max) fname
					nil 'quiet)
			  (prog1 (make-glyph (vector 'xbm :file fname))
			    (delete-file fname))))
	(t (make-glyph (vector type :data (buffer-string))))))

;;; Parsing of piconsearch result page.

;; Assumes:
;; 1 - each value field has the form: "<strong>key</strong> = <kbd>value</kbd>"
;; 2 - a "<p>" separates the keywords from the results
;; 3 - every results begins by the path within the database at the beginning
;;     of the line in raw text.
;; 3b - and the href following it is the preferred image type.

;; if 1 or 2 is not met, it will probably cause an error.  The other
;; will go undetected

(defun gnus-picons-parse-value (name)
  (goto-char (point-min))
  (if (re-search-forward (concat "<strong>"
				 (regexp-quote name)
				 "</strong> *= *<kbd> *\\([^ <][^<]*\\) *</kbd>")
			 nil t)
      (buffer-substring (match-beginning 1) (match-end 1))))

(defun gnus-picons-parse-filenames ()
  ;; returns an alist of ((USER ADDRS DB) . URL)
  (let ((case-fold-search t)
	(user (gnus-picons-parse-value "user"))
	(host (gnus-picons-parse-value "host"))
	(dbs (message-tokenize-header (gnus-picons-parse-value "db") " "))
	start-re cur-db cur-host cur-user types res)
    ;; now point will be somewhere in the header.  Find beginning of
    ;; entries
    (when (and user host dbs)
      (setq start-re
	    (concat
	     ;; dbs
	     "^\\(" (mapconcat 'regexp-quote dbs "\\|") "\\)/"
	     ;; host
	     "\\(\\(" (mapconcat 'regexp-quote
				 (message-tokenize-header host ".") "/\\|")
	     "/\\|MISC/\\)*\\)"
	     ;; user
	     "\\(" (regexp-quote user) "\\|unknown\\)/"
	     "face\\."))
      (re-search-forward "<p>[ \t\n]*")
      (while (re-search-forward start-re nil t)
	(setq cur-db (buffer-substring (match-beginning 1) (match-end 1))
	      cur-host (buffer-substring (match-beginning 2) (match-end 2))
	      cur-user (buffer-substring (match-beginning 4) (match-end 4))
	      cur-host (nreverse (message-tokenize-header cur-host "/")))
	;; XXX - KLUDGE: there is a blank picon in news/MISC/unknown
	(unless (and (string-equal cur-db "news")
		     (string-equal cur-user "unknown")
		     (equal cur-host '("MISC")))
	  ;; ok now we have found an entry (USER HOST DB), find the
	  ;; corresponding picon URL
	  (save-restriction
	    ;; restrict region to this entry
	    (narrow-to-region (point) (search-forward "<br>"))
	    (goto-char (point-min))
	    (setq types gnus-picons-file-suffixes)
	    (while (and types
			(not (re-search-forward
			      (concat "<a[ \t\n]+href=\"\\([^\"]*\\."
				      (regexp-quote (car types)) "\\)\"")
			      nil t)))
	      (pop types))
	    (push (cons (list cur-user cur-host cur-db)
			(buffer-substring (match-beginning 1) (match-end 1)))
		  res))))
      (nreverse res))))

;;; picon network display functions :

(defun gnus-picons-network-display-internal (sym-ann glyph part right-p marker)
  (let ((buf (marker-buffer marker))
	(pos (marker-position marker)))
    (if (and buf pos)
	(save-excursion
	  (set-buffer buf)
	  (goto-char pos)
	  (gnus-picons-display-picon-or-name glyph part right-p))))
  (gnus-picons-next-job-internal))

(defun gnus-picons-network-display-callback (url part sym-ann right-p marker)
  (let ((glyph (gnus-picons-make-glyph (cdr (assoc url-current-mime-type
						   w3-image-mappings)))))
    (kill-buffer (current-buffer))
    (push (cons url glyph) gnus-picons-glyph-alist)
    ;; only do the job if it has not been preempted.
    (if (equal gnus-picons-job-already-running
	       (list sym-ann 'picon url part right-p marker))
	(gnus-picons-network-display-internal
	 sym-ann glyph part right-p marker)
      (gnus-picons-next-job-internal))))

(defun gnus-picons-network-display (url part sym-ann right-p marker)
  (let ((cache (assoc url gnus-picons-glyph-alist)))
    (if (or cache (null url))
	(gnus-picons-network-display-internal
	 sym-ann (cdr cache) part right-p marker)
      (gnus-picons-url-retrieve url 'gnus-picons-network-display-callback
				(list url part sym-ann right-p marker)))))

;;; search job functions

(defun gnus-picons-display-bar-p ()
  (if (eq gnus-picons-display-where 'article)
      gnus-picons-display-article-move-p
    gnus-picons-display-as-address))

(defun gnus-picons-network-search-internal (user addrs dbs sym-ann right-p
						 marker &optional fnames)
  (let (curkey dom pfx url dbs-tmp cache new-jobs)
    ;; First do the domain search
    (dolist (part (if right-p
		      (reverse addrs)
		    addrs))
      (setq pfx (nconc (list part) pfx)
	    dom (cond ((and dom right-p) (concat part "." dom))
		      (dom (concat dom "." part))
		      (t part))
	    curkey (list "unknown" dom dbs))
      (when (null (setq cache (assoc curkey gnus-picons-url-alist)))
	;; This one is not yet in the cache, create a new entry
	;; Search for an entry
	(setq dbs-tmp dbs
	      url nil)
	(while (and dbs-tmp (null url))
	  (setq url (or (cdr (assoc (list "unknown" pfx (car dbs-tmp)) fnames))
			(and (eq dom part)
			     ;; This is the first component.  Try the
			     ;; catch-all MISC component
			     (cdr (assoc (list "unknown"
					       '("MISC")
					       (car dbs-tmp))
					 fnames)))))
	  (pop dbs-tmp))
	(push (setq cache (cons curkey url)) gnus-picons-url-alist))
      ;; Put this glyph in the job list
      (if (and (not (eq dom part)) gnus-picons-display-as-address)
	  (push (list sym-ann "." right-p marker) new-jobs))
      (push (list sym-ann 'picon (cdr cache) part right-p marker) new-jobs))
    ;; next, the user search
    (when user
      (setq curkey (list user dom gnus-picons-user-directories))
      (if (null (setq cache (assoc curkey gnus-picons-url-alist)))
	  (let ((users (list user "unknown"))
		dirs usr domains-tmp dir picon)
	    (while (and users (null picon))
	      (setq dirs gnus-picons-user-directories
		    usr (pop users))
	      (while (and dirs (null picon))
		(setq domains-tmp addrs
		      dir (pop dirs))
		(while (and domains-tmp
			    (null (setq picon (assoc (list usr domains-tmp dir)
						     fnames))))
		  (pop domains-tmp))
		(unless picon
		  (setq picon (assoc (list usr '("MISC") dir) fnames)))))
	    (push (setq cache (cons curkey (cdr picon)))
		  gnus-picons-url-alist)))
      (if (and gnus-picons-display-as-address new-jobs)
	  (push (list sym-ann "@" right-p marker) new-jobs))
      (push (list sym-ann 'picon (cdr cache) user right-p marker) new-jobs))
    (if (and (gnus-picons-display-bar-p) (not right-p))
	(push (list sym-ann 'bar right-p marker) new-jobs))
    ;; only put the jobs in the queue if this job has not been preempted.
    (if (equal gnus-picons-job-already-running
	       (list sym-ann 'search user addrs dbs right-p marker))
	(setq gnus-picons-jobs-alist
	      (nconc (if (and (gnus-picons-display-bar-p) right-p)
			 (list (list sym-ann 'bar right-p marker)))
		     (nreverse new-jobs)
		     gnus-picons-jobs-alist)))
    (gnus-picons-next-job-internal)))

(defun gnus-picons-network-search-callback (user addrs dbs sym-ann right-p
						 marker)
  (gnus-picons-network-search-internal
   user addrs dbs sym-ann right-p marker
   (prog1
       (gnus-picons-parse-filenames)
     (kill-buffer (current-buffer)))))

;; Initiate a query on the picon database
(defun gnus-picons-network-search (user addrs dbs sym-ann right-p marker)
  (let* ((host (mapconcat 'identity addrs "."))
	 (key (list (or user "unknown") host (if user
						 gnus-picons-user-directories
					       dbs)))
	 (cache (assoc key gnus-picons-url-alist)))
    (if (null cache)
	(gnus-picons-url-retrieve
	 (concat gnus-picons-piconsearch-url
		 "?user=" (w3-form-encode-xwfu (or user "unknown"))
		 "&host=" (w3-form-encode-xwfu host)
		 "&db=" (mapconcat 'w3-form-encode-xwfu
				   (if user
				       (append dbs
					       gnus-picons-user-directories)
				     dbs)
				   "+"))
	 'gnus-picons-network-search-callback
	 (list user addrs dbs sym-ann right-p marker))
      (gnus-picons-network-search-internal
       user addrs dbs sym-ann right-p marker))))

;;; Main jobs dispatcher function

(defun gnus-picons-next-job-internal ()
  (when (setq gnus-picons-job-already-running (pop gnus-picons-jobs-alist))
    (let* ((job gnus-picons-job-already-running)
	   (sym-ann (pop job))
	   (tag (pop job)))
      (when tag
	(cond
	 ((stringp tag);; (SYM-ANN "..." RIGHT-P MARKER)
	  (gnus-picons-network-display-internal
	   sym-ann nil tag (pop job) (pop job)))
	 ((eq 'bar tag);; (SYM-ANN 'bar RIGHT-P MARKER)
	  (gnus-picons-network-display-internal
	   sym-ann
	   (let ((gnus-picons-file-suffixes '("xbm")))
	     (gnus-picons-try-face
	      gnus-xmas-glyph-directory "bar."))
	   nil (pop job) (pop job)))
	 ((eq 'search tag);; (SYM-ANN 'search USER ADDRS DBS RIGHT-P MARKER)
	  (gnus-picons-network-search
	   (pop job) (pop job) (pop job) sym-ann (pop job) (pop job)))
	 ((eq 'picon tag);; (SYM-ANN 'picon URL PART RIGHT-P MARKER)
	  (gnus-picons-network-display
	   (pop job) (pop job) sym-ann (pop job) (pop job)))
	 (t
	  (setq gnus-picons-job-already-running nil)
	  (error "Unknown picon job tag %s" tag)))))))

(defun gnus-picons-next-job ()
  "Start processing the job queue if it is not in progress."
  (unless gnus-picons-job-already-running
    (gnus-picons-next-job-internal)))

(provide 'gnus-picon)

;;; gnus-picon.el ends here
