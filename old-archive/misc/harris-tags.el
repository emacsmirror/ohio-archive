; From: Ken Laprade <laprade@trantor.harris-atd.com>
; Subject: Update to harris-tags.el
; Date: Thu, 25 Mar 93 08:38:07 EST
; 
; Here is the most recent version of `harris-tags.el'.  The version currently
; in the elisp-archive is out of date.
; --------------------
;; -*- Mode: Emacs-Lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File:	harris-tags.el
;; RCS:		$Id: harris-tags.el,v 1.9 1993/03/25 13:28:58 laprade Exp $
;; Description:	Tags with multiple active files, etags or ctags, completion...
;; Author:	Ken Laprade <laprade@trantor.harris-atd.com>
;; Modified:	Tue Jul 30 16:01:34 1991
;; Created:	Thu Aug 30 17:53:38 1990 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Tags facility with lots of extensions:
;;	- multiple active tags files
;;	- either etags or ctags -x format tags
;;	- hook for a 'tag-trail' or 'location' marker
;;	- tags completion both in find-tag prompt and normal buffers
;;	- uninteresting tags-search files stay at bottom of buffer list

;; Copyright (C) 1991 Ken Laprade <laprade@trantor.harris-atd.com>
;; Copyright (C) 1987 Donald Becker
;; Copyright (C) 1985, 1986, 1988 Free Software Foundation, Inc.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; this file, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;; This file contains, in part, code originally distributed with GNU Emacs.

;; This file also contains code or ideas from these sources (all
;; found in the elisp archive on tut.cis.ohio-state.edu):
;; tag-comp.el by Henry Kautz <allegra.UUCP!kautz%eddie.mit.edu.uucp@bbn.com>
;;	Completion for specifying tag for find-tag.
;; ange-tags.el by Andy Norman <ange@hplb.hpl.hp.com>
;;	The idea of using buffer-local tags files, but I used
;;	buffer-local variables instead.
;; tag-trail.el by Duke Briscoe <briscoe@cs.yale.edu>
;;	Keeping tags-search'ed files at the end of the buffer
;;	list unless something is found.  Also, mark-trail
;;	could be used instead of my push-bmark.
;; location.el by Lynn Slater <lrs@indetech.com>
;;      Same idea as bmark.el but doesn't use markers.

;; LCD Archive Entry:
;; harris-tags|Ken Laprade|laprade@trantor.harris-atd.com|
;; Tags with multiple active files, etags or ctags, completion...|
;; 25-Mar-1993|1.9|~/misc/harris-tags.el.Z|

;; --- Usage:
;; This file is a direct replacement for tags.el supplied with GNU emacs
;; version 18.55.  Several features have been added.
;;
;; Multiple active tags files:
;;   tags-file-name is now a buffer-local variable which is set whenever a
;;   file is visited by a tag function.  This means that when you do a
;;   find-tag, the tag will be looked up in the table associated with the
;;   current buffer.  If there is no tag file associated with it, the
;;   default value is used.  The default value is what is set by
;;   visit-tags-table (unless inhibit-setting-tags-file-name is set).  A
;;   new command, visit-tags-table-locally is an easy way to set the tag
;;   table associated with the current buffer.
;; Automatic tags file detection:
;;   If the current buffer when find-tag is invoked does not have a
;;   tags-file-name, all tags tables already loaded are checked for entries
;;   from that file.  If the current buffer's file is mentioned in a tags
;;   table, that table is then associated with the buffer.  This feature is
;;   enabled with allow-associated-tags-tables.  load-tags-table may be used
;;   to read in a file as a tag table.
;; Dual tags formats:
;;   Two tags file formats are now supported: the traditional emacs tags
;;   file format produced by 'etags' as well as the format produced by
;;   'ctags -x'.  The correct format is automatically determined when the
;;   tags file is read in.
;; Tags completion:
;;   The new functions completing-find-tag and completing-find-tag-other-window
;;   will do tag name completion in the minibuffer for a find-tag.  For
;;   large tags files, the initial build of the completion table may take
;;   some time.  Therefore, the completing-find-tag functions will act as
;;   regular find-tag's until the tag completion table is built.  It may be
;;   built explicitly with build-tag-completion-table or implicitly with
;;   complete-tag.  Also, if allow-build-tag-completion-table is non-nil,
;;   the tag completion table will be built the first time completing-find-tag
;;   is run.  complete-tag may be used in any buffer to complete the symbol
;;   preceding point from the tags table.
;; Hook for a 'tag-trail' or 'location' marker:
;;   mark-location-form may be set to a function that will remember the
;;   point and buffer when a find-tag or tags-loop-continue is run.  This
;;   makes it easy to backtrack through a series of find-tag or tags-search
;;   commands.  My bmark.el package is ideal for this.  tag-trail.el or
;;   location.el will work as well.
;; Uninteresting tags-search files stay at bottom of buffer list:
;;   Now files scanned by tags-search or tags-query-replace that do not
;;   contain matches remain at the bottom of the buffer list.
;; Tags file may be symbolic links:
;;   If a tags file is a symbolic link, the link is resolved so files
;;   referenced in the file will be relative to the directory of the tags
;;   file.

;; --- Installation:
;; Put these in default.el or ~/.emacs:
;(autoload 'visit-tags-table "harris-tags" "Set default tag table (not loaded)." t)
;(autoload 'visit-tags-table-locally "harris-tags" "Set local tag table (not loaded)." t)
;(autoload 'load-tags-table "harris-tags" "Read in FILE to use as a tag table (not loaded)." t)
;(autoload 'find-tag "harris-tags" "Find tag (not loaded)." t)
;(autoload 'find-tag-other-window "harris-tags" "Find tag (not loaded)." t)
;(autoload 'tags-loop-continue "harris-tags" "Continue last tag command (not loaded)." t)
;(autoload 'tags-search "harris-tags" "Search files listed in tag table (not loaded)." t)
;(autoload 'tags-query-replace "harris-tags" "Query-replace-regexp files listed in tag table (not loaded)." t)
;(autoload 'list-tags "harris-tags" "Display list of tags in file (not loaded)." t)
;(autoload 'tags-apropos "harris-tags" "Display list of tags matching regexp (not loaded)." t)
;(autoload 'build-tag-completion-table "harris-tags" "Explicitly create tag-completion-table (not loaded)." t)
;(autoload 'completing-find-tag "harris-tags" "Find tag with completion (not loaded)." t)
;(autoload 'completing-find-tag-other-window "harris-tags" "Find tag with completion (not loaded)." t)
;(autoload 'complete-tag "harris-tags" "Complete tag (not loaded)." t)
;(autoload 'find-tag-default "harris-tags" "(not loaded)" t)
;;
;; And this (or something similar) if you have the package to support it (bmark.el):
;(setq mark-location-form '(push-bmark))
;;
;; Rebind these if you want to:
;(global-set-key "\M-." 'completing-find-tag)
;(global-set-key "\C-x4." 'completing-find-tag-other-window)
;(global-set-key "\M-\e\t" 'complete-tag)


(defvar tag-table-files nil
  "List of file names covered by current tag table.
nil means it has not been computed yet; do (tag-table-files) to compute it.

This is buffer local on each tags table.")
(make-variable-buffer-local 'tag-table-files)

(defvar last-tag nil
  "Tag found by the last find-tag.

This is buffer local on each tags table.")
(make-variable-buffer-local 'last-tag)

(defvar tags-file-name nil
"*File name of tag table.
To switch to a new tag table, setting this variable is sufficient.
Use either the `etags' or `ctags' program to make a tag table file
in the format of your choice.

This is buffer local, so you can have multiple tags files.  To set
the default value, use visit-tags-table.  To set the local value,
use visit-tags-table-locally.")
(make-variable-buffer-local 'tags-file-name)

(defvar traditional-tags-table nil
  "t if tag table was produced with etags.  nil for ctags.

This is buffer local on each tags table.")
(make-variable-buffer-local 'traditional-tags-table)

(defvar tag-completion-table nil
  "The completion table used by completing-find-tag.

This is buffer local on each tags table.")
(make-variable-buffer-local 'tag-completion-table)

(defvar allow-build-tag-completion-table nil
  "*If non-nil, tag-completion-table will be built whenever a
completing-find-tag is done.  If nil, build-tag-completion-table
or complete-tag must be invoked explicitly.")

(defvar mark-location-form nil
  "*Form to eval to record point and buffer before doing something
that changes buffers such as find-tag, tags-search, or next-error.
This is a nice choice: '(push-bmark).")

(defvar allow-associated-tags-tables nil
  "*non-nil allows tags functions to search loaded tags tables for an
associated tags table for buffers with no value of tags-file-name.")

(defvar inhibit-setting-tags-file-name 'global
  "*non-nil prevents visit-tags-table from setting the default value of
tags-file-name.  This will cause find-tag to prompt for a tags table when
invoked from a buffer that has no local tags-file-name and is not
mentioned in any existing tags tables.  If set to 'global, the local value
of tags-file-name will be set in the buffer visit-tags-table is called in.")

(defvar auto-accept-tags-file-names '("TAGS" "tags")
  "*A list of filenames that will be searched for in the current directory
when visit-tags-table-buffer is called.  If one is found, it is used and
the user is not queried.  If nil, the user will always be queried.")


(defun chase-symlinks (file)
  "Return the filename that FILENAME references, following all symbolic links."
  (let (temp)
    (while (setq temp (file-symlink-p file))
      (setq file
	    (if (file-name-absolute-p temp)
		temp
	      (concat (file-name-directory file) temp)))))
  file)

(defun visit-tags-table (file)
  "Tell tags commands to use tag table file FILE.
FILE should be the name of a file created with either the `etags' or `ctags'
program.  A directory name is ok too; it means file `tags' (or `TAGS') in
that directory.  If inhibit-setting-tags-file-name is non-nil, just the
FILE is returned; the default value of tags-file-name is not set.

Any buffer-local value of tags-file-name for the current buffer cleared."
  (interactive (list (let ((default (if (file-exists-p (concat default-directory "TAGS")) "TAGS" "tags")))
		       (read-file-name (format "Visit tags table: (default %s) " default)
				       default-directory
				       (concat default-directory default)
				       t))))
  (if (file-directory-p file)
      (setq file (file-name-as-directory file)
	    file (concat file (if (file-exists-p (concat file "TAGS")) "TAGS" "tags"))))
  (setq file (expand-file-name (chase-symlinks file)))
  (if (eq inhibit-setting-tags-file-name 'global)
      (message "Local tags table now %s" (setq tags-file-name file))
    (or inhibit-setting-tags-file-name
	(progn (message "Default tags table now %s"file)
	       (set-default 'tags-file-name file)
	       (kill-local-variable 'tags-file-name))))
  file)

(defun visit-tags-table-locally (file)
  "Tell tags commands to use tag table file FILE, only for this buffer.
FILE should be the name of a file created with either the `etags' or `ctags'
program.  A directory name is ok too; it means file `tags' (or `TAGS') in
that directory."
  (interactive (list (let ((default (if (file-exists-p (concat default-directory "TAGS")) "TAGS" "tags")))
		       (read-file-name (format "Local tags table: (default %s) " default)
				       default-directory
				       (concat default-directory default)
				       t))))
  (if (file-directory-p file)
      (setq file (file-name-as-directory file)
	    file (concat file (if (file-exists-p (concat file "TAGS")) "TAGS" "tags"))))
  (message "Local tags table now %s" (setq file (expand-file-name (chase-symlinks file))))
  (setq tags-file-name file))

(defun load-tags-table (file)
  "Read in FILE to use as a tag table.
FILE should be the name of a file created with either the `etags' or `ctags'
program.  A directory name is ok too; it means file `tags' (or `TAGS') in
that directory.  This command does not set tags-file-name."
  (interactive (list (let ((default (if (file-exists-p (concat default-directory "TAGS")) "TAGS" "tags")))
		       (read-file-name (format "Load tags table: (default %s) " default)
				       default-directory
				       (concat default-directory default)
				       t))))
  (if (file-directory-p file)
      (setq file (file-name-as-directory file)
	    file (concat file (if (file-exists-p (concat file "TAGS")) "TAGS" "tags"))))
  (setq file (expand-file-name (chase-symlinks file)))
  (save-excursion
    (set-buffer (find-file-noselect file))
    (setq tags-file-name file)
    (visit-tags-table-buffer)))

(defun auto-tags-file-name ()
  "Return the name of an existing file matching one of
auto-accept-tags-file-names in the current directory."
  (let* ((names auto-accept-tags-file-names)
	(name (car names)))
    (while (and name
		(not (file-exists-p (concat default-directory name))))
      (setq names (cdr names)
	    name (car names)))
    (if name (expand-file-name (chase-symlinks (concat default-directory name))))))

(defun associated-tags-table (&optional buffer)
  "Return the filename of a tags table buffer that references the file
associated with BUFFER."
  (let ((buflist (buffer-list))
	tagbuf
	found
	(file (buffer-file-name buffer)))
    (if file
	(progn
	  (save-excursion
	    (while (and (not found) (setq tagbuf (car buflist)))
	      (if (assoc 'traditional-tags-table (buffer-local-variables tagbuf))
		  (progn (set-buffer tagbuf)
			 (if (in-list file (tag-table-files))
			     (setq found (buffer-file-name)))))
	      (setq buflist (cdr buflist))))
	  (if found
	      (setq tags-file-name found))))))

(defun visit-tags-table-buffer ()
  "Select the buffer containing the current tag table.  This is a file
whose name is in the variable tags-file-name.  If tags-file-name is nil and
auto-accept-tags-file-names contains the name of an existing file in the
current directory, then it is returned.  If this fails, but
allow-associated-tags-tables is non-nil, all current tags tables will be
searched for references to the current file.  If one is found,
tags-file-name will be set accordingly.  Otherwise, visit-tags-table is
called interactively to get a tags-file-name."
  (let ((filename
	 (or tags-file-name
	     (auto-tags-file-name)
	     (and allow-associated-tags-tables (associated-tags-table (current-buffer)))
	     (call-interactively 'visit-tags-table))))
    (set-buffer (or (get-file-buffer filename)
		    (if (file-exists-p filename)
			(find-file-noselect filename)
		      (error "No tags table: %s" filename))))
    (or (verify-visited-file-modtime (current-buffer))
	(cond ((yes-or-no-p "Tags file has changed, read new contents? ")
	       (revert-buffer t t)
	       (setq tag-table-files nil
		     tag-completion-table nil))))
    (setq tags-file-name (buffer-file-name))
    (setq traditional-tags-table (eq (char-after 1) ?\^L))))

(defun file-of-tag ()
  "Return the file name of the file whose tags point is within.
Assumes the tag table is the current buffer.
File name returned is relative to tag table file's directory."
  (let ((opoint (point))
	prev size)
    (save-excursion
     (goto-char (point-min))
     (while (< (point) opoint)
       (forward-line 1)
       (end-of-line)
       (skip-chars-backward "^,\n")
       (setq prev (point))
       (setq size (read (current-buffer)))
       (goto-char prev)
       (forward-line 1)
       (forward-char size))
     (goto-char (1- prev))
     (buffer-substring (point)
		       (progn (beginning-of-line) (point))))))

(defun tag-table-files ()
  "Return a list of files in the current tag table.
File names returned are absolute."
  (save-excursion
   (visit-tags-table-buffer)
   (or tag-table-files
       (let (files)
	(goto-char (point-min))
	(if traditional-tags-table
	    (while (not (eobp))
	      (forward-line 1)
	      (end-of-line)
	      (skip-chars-backward "^,\n")
	      (setq prev (point))
	      (setq size (read (current-buffer)))
	      (goto-char prev)
	      (setq files (cons (expand-file-name
				 (buffer-substring (1- (point))
						   (save-excursion
						     (beginning-of-line)
						     (point)))
				 (file-name-directory tags-file-name))
				files))
	      (forward-line 1)
	      (forward-char size))
	  ;; For ctags files this will be slow.
	  (while (re-search-forward "^[^ ]+ +[0-9]+ +\\([^ \t\n]+\\).*$" nil t)
	    (setq file (expand-file-name
			(buffer-substring (match-beginning 1) (match-end 1))
			(file-name-directory tags-file-name)))
	    (or (in-list file files)
		(setq files (cons file files)))))
	(setq tag-table-files (nreverse files))))))

(defun in-list (entry lst)
  (let ((not-found t))
    (while (and not-found lst)
      (if (equal entry (car lst))
	  (setq not-found nil))
      (setq lst (cdr lst)))
    (not not-found)))

;; Return a default tag to search for, based on the text at point.
(defun find-tag-default ()
  (save-excursion
    (while (looking-at "\\sw\\|\\s_")
      (forward-char 1))
    (if (re-search-backward "\\sw\\|\\s_" nil t)
	(progn (forward-char 1)
	       (buffer-substring (point)
				 (progn (forward-sexp -1)
					(while (looking-at "\\s'")
					  (forward-char 1))
					(point))))
      nil)))

(defun find-tag-tag (string)
  (let* ((default (find-tag-default))
	 (spec (read-string
		(if default
		    (format "%s(default %s) " string default)
		  string))))
    (list (if (equal spec "")
	      default
	    spec))))

(defun find-tag (tagname &optional next other-window)
  "Find tag (in current tag table) whose name contains TAGNAME.
 Selects the buffer that the tag is contained in
and puts point at its definition.
 If TAGNAME is a null string, the expression in the buffer
around or before point is used as the tag name.
 Ctags style tags are always anchored to the beginning of a word.
Etags style tags are not.
 If second arg NEXT is non-nil (interactively, with prefix arg),
searches for the next tag in the tag table
that matches the tagname used in the previous find-tag.

See documentation of variable tags-file-name."
  (interactive (if current-prefix-arg
		   '(nil t)
		 (find-tag-tag "Find tag: ")))
  (let (buffer file linebeg startpos startline tags-file traditional-tags)
    (save-excursion
      (visit-tags-table-buffer)
      (setq tags-file (buffer-file-name)
	    traditional-tags traditional-tags-table)
      (if next (setq tagname last-tag)
	(goto-char (point-min))
	(setq last-tag tagname))
      (if traditional-tags
	  (progn
	    (while (progn
		     (if (not (search-forward tagname nil t))
			 (error "No %sentries containing %s"
				(if next "more " "") tagname))
		     (not (looking-at "[^\n\177]*\177"))))
	    (search-forward "\177")
	    (setq file (expand-file-name (file-of-tag)
					 (file-name-directory tags-file-name)))
	    (setq linebeg
		  (buffer-substring (1- (point))
				    (save-excursion (beginning-of-line) (point))))
	    (search-forward ",")
	    (setq startpos (read (current-buffer))))
	;; Becker's code for ctags style tag files:
	(or next
	    ;; Use the fastest search first...
	    (binary-search tagname))
	(if (null (re-search-forward
		   (concat "^"
			   (regexp-quote tagname)
			   "[^ ]* +\\([0-9]+\\) +\\([^ \t\n]+\\)[ \t]+\\(.*\\)$")
		   nil t))
	    (error (if next "No more tags for %s." "Failed tag search for %s.")
		   tagname))
	(setq startline (string-to-int 
			 (buffer-substring (match-beginning 1) (match-end 1))))
	(setq file	(buffer-substring (match-beginning 2) (match-end 2)))
	(setq linebeg   (buffer-substring (match-beginning 3) (match-end 3)))
	(setq file      (if (string-match ":" file)
			    (concat "/" file)
			  (expand-file-name file (file-name-directory tags-file-name))))))
    (eval mark-location-form)
    (if other-window
	(find-file-other-window file)
      (find-file file))
    (setq tags-file-name tags-file)
    (widen)
    (push-mark)
    (let ((offset (if traditional-tags 1000 20))
	  found
	  (pat (concat (if traditional-tags "^" "^[ \t]*")
		       (regexp-quote linebeg))))
      (or traditional-tags
	  (setq startpos (if (null startline)
			     (point-min)
			   (goto-char 1)
			   (forward-line (1- startline))
			   (point))))
      (while (and (not found)
		  (progn
		    (goto-char (- startpos offset))
		    (not (bobp))))
	(setq found
	      (re-search-forward pat (+ startpos offset) t))
	(setq offset (* 3 offset)))
      (or found
	  (if traditional-tags
	      (or (re-search-forward pat nil t)
		  (error "%s not found in %s" pat file))
	    (re-search-forward pat))))
    (beginning-of-line))
  (set-tags-loop-form '(find-tag nil t))
  ;; Return t in case used as the tags-loop-form.
  t)

(defun find-tag-other-window (tagname &optional next)
  "Find tag (in current tag table) whose name contains TAGNAME.
 Selects the buffer that the tag is contained in in another window
and puts point at its definition.
 If TAGNAME is a null string, the expression in the buffer
around or before point is used as the tag name.
 Ctags style tags are always anchored to the beginning of a word.
Etags style tags are not.
 If second arg NEXT is non-nil (interactively, with prefix arg),
searches for the next tag in the tag table
 that matches the tagname used in the previous find-tag.

See documentation of variable tags-file-name."
  (interactive (if current-prefix-arg
		   '(nil t)
		   (find-tag-tag "Find tag other window: ")))
  (find-tag tagname next t))

(defvar next-file-list nil
  "List of files for next-file to process.

This is buffer local on each tags table.")
(make-variable-buffer-local 'next-file-list)

(defun next-file (&optional initialize)
  "Select next file among files in current tag table.
Non-nil argument (prefix arg, if interactive)
initializes to the beginning of the list of files in the tag table."
  (interactive "P")
  (let (tags-file
	next-file)
    (save-excursion
      (visit-tags-table-buffer)
      (if initialize
	  (setq next-file-list
		(tag-table-files)))
      (or next-file-list
	  (error "All files processed."))
      (setq tags-file (buffer-file-name)
	    next-file (car next-file-list)
	    next-file-list (cdr next-file-list)))
    (set-buffer (find-file-noselect next-file))
    (setq tags-file-name tags-file)))

(defvar tags-loop-form nil
  "Form for tags-loop-continue to eval to process one file.
If it returns nil, it is through with one file; move on to next.

This is buffer local on each tags table.")
(make-variable-buffer-local 'tags-loop-form)

(defun get-tags-loop-form ()
  (save-excursion
    (visit-tags-table-buffer)
    tags-loop-form))
		    
(defun set-tags-loop-form (form)
  (save-excursion
    (visit-tags-table-buffer)
    (setq tags-loop-form form)))

(defun tags-loop-continue (&optional first-time)
  "Continue last \\[tags-search] or \\[tags-query-replace] command.
Used noninteractively with non-nil argument
to begin such a command.  See variable tags-loop-form."
  (interactive)
  (let ((form  (get-tags-loop-form)))
    (or (eq (car form) 'find-tag)	; find-tag does it itself.
	(eval mark-location-form))
    (if first-time
	(progn (next-file t)
	       (goto-char (point-min))))
    (while (not (eval form))
      (next-file)
      (message "Scanning file %s..." buffer-file-name)
      (goto-char (point-min)))
    (message "")
    (switch-to-buffer (current-buffer))))

(defun tags-search (regexp)
  "Search through all files listed in tag table for match for REGEXP.
Stops when a match is found.
To continue searching for next match, use command \\[tags-loop-continue].

See documentation of variable tags-file-name."
  (interactive "sTags search (regexp): ")
  (if (and (equal regexp "")
	   (eq (car (get-tags-loop-form)) 're-search-forward))
      (tags-loop-continue nil)
    (set-tags-loop-form (list 're-search-forward regexp nil t))
    (tags-loop-continue t)))

(defun tags-query-replace (from to &optional delimited)
  "Query-replace-regexp FROM with TO through all files listed in tag table.
Third arg DELIMITED (prefix arg) means replace only word-delimited matches.
If you exit (C-G or ESC), you can resume the query-replace
with the command \\[tags-loop-continue].

See documentation of variable tags-file-name."
  (interactive "sTags query replace (regexp): \nsTags query replace %s by: \nP")
  (set-tags-loop-form
   (list 'and (list 'save-excursion
		    (list 're-search-forward from nil t))
	 (list 'progn
	       '(switch-to-buffer (current-buffer))
	       (list 'not (list 'perform-replace from to t t 
				(not (null delimited)))))))
  (tags-loop-continue t))

(defun list-tags (string)
  "Display list of tags in file FILE."
  (interactive "sList tags (in file): ")
  (if (equal string "") (setq string (buffer-file-name)))
  (setq string (file-name-nondirectory string))
  (with-output-to-temp-buffer "*Tags List*"
    (princ "Tags in file ")
    (princ string)
    (terpri)
    (save-excursion
      (visit-tags-table-buffer)
      (goto-char 1)
      (if traditional-tags-table
	  (progn
	    (re-search-forward (concat "\f\n.*" string ","))
	    (forward-line 1)
	    (while (not (looking-at "\f"))
	      (princ (buffer-substring (point)
				       (progn (skip-chars-forward "^\177")
					      (point))))
	      (terpri)
	      (forward-line 1)))
	;; ctags style:
	(while (re-search-forward
		   (concat "^\\([^ ]+\\) +[0-9]+ +[^ \t\n]*"
			   (regexp-quote string)
			   ".*$")
		   nil t)
	  (princ (buffer-substring (match-beginning 1) (match-end 1)))
	  (terpri))))))

(defun tags-apropos (string)
  "Display list of all tags in tag table REGEXP matches."
  (interactive "sTag apropos (regexp): ")
  (with-output-to-temp-buffer "*Tags List*"
    (princ "Tags matching regexp ")
    (prin1 string)
    (terpri)
    (save-excursion
     (visit-tags-table-buffer)
     (goto-char 1)
     (or traditional-tags-table
	 (setq string (concat "^[^ \t]*" string)))
     (while (re-search-forward string nil t)
       (beginning-of-line)
       (if traditional-tags-table
	   (princ (buffer-substring (point)
				    (progn (skip-chars-forward "^\177")
					   (point))))
	 (re-search-forward "^[^ \t]+ +\\([0-9]+\\) +\\([^ \t\n]+\\)[ \t]+\\(.*\\)$")
	 (princ (buffer-substring (match-beginning 3) (match-end 3))))
       (terpri)
       (forward-line 1)))))

(defun binary-search (tagname &optional casefold)
  "Move to the line beginning with TAGNAME assuming the lines are sorted.
If no line starts with TAGNAME, it moves the point to the position
in the buffer the line would have been in.
  If the optional argument CASEFOLD is set, the file is assumed to ordered
with the character case folded and strings are downcased before comparison."
  (interactive (find-tag-tag nil "Move to tag: "))
  (let ((range-min (point-min))
	(range-max (point-max))
	(str-len (length tagname))
	(bol nil) (eol nil)
	(tagname (if casefold (downcase tagname) tagname))
	current-tag)
    (while (progn
	     (goto-char (/ (+ range-min range-max) 2))
	     (beginning-of-line)
	     (setq current-tag (buffer-substring (point) (+ (point) str-len)))
	     (not (equal range-min range-max)))
      (if (string-lessp (if casefold (downcase current-tag) current-tag) tagname)
	  (setq range-min (progn (forward-line) (point)))
	(setq range-max (progn (beginning-of-line) (point)))))))


;;; Based on tag-comp.el:

(defun current-tag-completion-table ()
  "Return the current tag completion table."
  (if (or tags-file-name
	  allow-build-tag-completion-table)
      (save-excursion
	(and (eq (current-buffer) (window-buffer (minibuffer-window)))
	     (boundp 'find-tag-buffer)
	     (set-buffer find-tag-buffer))
	(visit-tags-table-buffer)
	(or tag-completion-table
	    (and allow-build-tag-completion-table
		 (build-tag-completion-table))))))

(defun build-tag-completion-table ()
  "Create tag-completion-table.  Returns table."
  (interactive)
  (save-excursion
    (visit-tags-table-buffer)
    (message "Building tags completion table...")
    (goto-char (point-min))
    (setq tag-completion-table nil)
    (let ((tag-regexp (if traditional-tags-table
			  "\\([^\012 \t(),]+\\)[ \t(),]*\177"
			"^\\([^ ]+\\) +"))
	  table)
      (while (re-search-forward tag-regexp nil t)
	(setq table 
	      (cons (list (buffer-substring (match-beginning 1)
					    (match-end 1))) table)))
      (setq tag-completion-table table))
    (message "Building tags completion table...done")
    tag-completion-table))

(defun completing-find-tag-tag (string)
  (let* ((default (find-tag-default))
	 (table (current-tag-completion-table))
	 ;; This is for complete-tag calling current-tag-completion-table
	 ;; so it will use the right tags table if invoked in the minibuffer:
	 (find-tag-buffer (current-buffer))
	 (str (if table
		  (concat "Completing " (downcase string))
		string))
	 (prompt (if default
		     (format "%s(default %s) " str default)
		   str))
	 (spec (if table
		   (completing-read prompt table nil nil nil)
		 (read-string prompt))))
    (list (if (equal spec "")
	      default
	    spec))))

(defun completing-find-tag (tagname &optional next other-window)
  "Find tag (in current tag table) whose name contains TAGNAME.
 Selects the buffer that the tag is contained in
and puts point at its definition.
 If TAGNAME is a null string, the expression in the buffer
around or before point is used as the tag name.
 Ctags style tags are always anchored to the beginning of a word.
Etags style tags are not.
 If second arg NEXT is non-nil (interactively, with prefix arg),
searches for the next tag in the tag table
that matches the tagname used in the previous find-tag.
 Tag completion can be done if tag-completion-table has been
computed (see build-tag-completion-table).

See documentation of variable tags-file-name."
  (interactive (if current-prefix-arg
		   '(nil t)
		 (completing-find-tag-tag "Find tag: ")))
  (find-tag tagname next other-window))

(defun completing-find-tag-other-window (tagname &optional next)
  "Find tag (in current tag table) whose name contains TAGNAME.
 Selects the buffer that the tag is contained in in another window
and puts point at its definition.
 If TAGNAME is a null string, the expression in the buffer
around or before point is used as the tag name.
 Ctags style tags are always anchored to the beginning of a word.
Etags style tags are not.
 If second arg NEXT is non-nil (interactively, with prefix arg),
searches for the next tag in the tag table
that matches the tagname used in the previous find-tag.
 Tag completion can be done if tag-completion-table has been
computed (see build-tag-completion-table).

See documentation of variable tags-file-name."
  (interactive (if current-prefix-arg
		   '(nil t)
		 (completing-find-tag-tag "Find tag other window: ")))
  (find-tag tagname next t))

(defun complete-tag ()
  "Perform tag completion on symbol preceding point.
That symbol is compared against the entries in the tag table
and any additional characters determined by what is there
are inserted."
  (interactive)
  (let* ((allow-build-tag-completion-table t)
	 (enable-recursive-minibuffers t)
	 (tag-completion-table (current-tag-completion-table))
	 (end (point))
	 (beg (save-excursion
		(backward-sexp 1)
		(point)))
	 (pattern (buffer-substring beg end))
	 (completion (try-completion pattern tag-completion-table)))
    (cond ((eq completion t))
	  ((null completion)
	   (message "Can't find completion for \"%s\"" pattern)
	   (ding))
	  ((not (string= pattern completion))
	   (delete-region beg end)
	   (insert completion))
	  (t
	   (message "Making completion list...")
	   (let ((list (all-completions pattern tag-completion-table)))
	     (with-output-to-temp-buffer "*Help*"
	       (display-completion-list list)))
	   (message "Making completion list...%s" "done")))))

(provide 'harris-tags)
(provide 'tags)

(run-hooks 'tags-load-hook)

---
Ken Laprade			INTERNET: laprade@trantor.harris-atd.com
Harris Corporation 		Usenet:  ...!uunet!x102a!trantor!laprade
PO Box 37, MS 16/1912		Voice: (407)727-4433
Melbourne, FL 32902		FAX: (407)729-3363
