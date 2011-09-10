;From ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!think!snorkelwacker!apple!sun-barr!newstop!sun!rberlin Mon Mar  5 09:20:37 1990
;Article 1247 of gnu.emacs:
;Path: ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!think!snorkelwacker!apple!sun-barr!newstop!sun!rberlin
;From rberlin@birdland.sun.com (Rich Berlin)
;Newsgroups: gnu.emacs
;Subject: Re: tags -- vi vs. emacs
;Message-ID: <RBERLIN.90Mar1172151@birdland.sun.com>
;Date: 2 Mar 90 01:21:51 GMT
;References: <1990Feb24.015620.27231@Solbourne.COM>
;Sender: news@sun.Eng.Sun.COM
;Distribution: na
;Organization: Sun Microsystems
;Lines: 103
;In-reply-to: dworkin@salgado.Solbourne.COM's message of 24 Feb 90 01:56:20 GMT
;
;Try this.

;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; find-vi-tag.el --- find-tag function using vi tags file.
;; Author          : Rich Berlin
;; Created On      : Wed Aug  9 11:11:37 1989
;; Last Modified By: SILL D E
;; Last Modified On: Fri Jul 20 15:01:39 1990
;; Last Modified By: Rich Berlin
;; Last Modified On: Thu Sep 28 09:55:20 1989
;; Update Count    : 5
;; Status          : Unknown, Use with caution!
;; 
;; Contributions:
;; Darren Austin added default prompting mechanism
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Customized tags table functions which work on vi-style (ctags) tags
;; file.
;;
;; Richard Berlin
;; 1 June 1988
;;
(defvar vi-tag-table-files nil)
(defvar vi-tags-file-name nil)

(defun visit-vi-tags-table (file)
  "Tell tags commands to use tag table file FILE.
FILE should be the name of a file created with the `ctags' program.
A directory name is ok too; it means file tags in that directory."
  (interactive (list (read-file-name "Visit tags table: (default tags) "
				     default-directory
				     (concat default-directory "tags")
				     t)))
  (setq file (expand-file-name file))
  (if (file-directory-p file)
      (setq file (concat file "tags")))
  (setq vi-tag-table-files nil
	vi-tags-file-name file))

(defun visit-vi-tags-table-buffer ()
  "Select the buffer containing the current tag table.
This is a file whose name is in the variable tags-file-name."
  (or vi-tags-file-name
      (call-interactively 'visit-vi-tags-table))
  (set-buffer (or (get-file-buffer vi-tags-file-name)
		  (progn
		    (setq vi-tag-table-files nil)
		    (find-file-noselect vi-tags-file-name))))
  (or (verify-visited-file-modtime (get-file-buffer vi-tags-file-name))
      (cond ((yes-or-no-p "Tags file has changed, read new contents? ")
	     (revert-buffer t t)
	     (setq vi-tag-table-files nil)))))

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

(defun find-vi-tag (name)
  (interactive (if current-prefix-arg
		   '(nil t)
		 (find-tag-tag "Find VI tag: ")))
  (let ((tag-regexp (format "^%s" name))
	filename
	search-regexp
	line-number
	(case-fold-search nil))
    (save-excursion
      (visit-vi-tags-table-buffer)
      (goto-char 1)
      (re-search-forward tag-regexp)
      (re-search-forward "[ \t]\\([^ \t]+\\)[ \t]")
      (setq filename (buffer-substring (match-beginning 1) (match-end 1)))
      (goto-char (match-end 0))
      (re-search-forward "\\([0-9]+\\|/[^/]+/\\)")
      (setq search-regexp (buffer-substring (1+ (match-beginning 0)) (1- (match-end 0))))
      (goto-char (match-beginning 0))
      (if (looking-at "[^/]")
	(setq line-number (read (current-buffer)))))
    (find-file (concat (file-name-directory vi-tags-file-name) filename))
    (if line-number
	(goto-line line-number)
      (re-search-forward search-regexp))))
