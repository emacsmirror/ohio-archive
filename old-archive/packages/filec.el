;From ark1!uakari.primate.wisc.edu!samsung!uunet!talos!kjones Fri Mar 23 15:52:52 1990
;Article 1609 of comp.emacs:
;Path: ark1!uakari.primate.wisc.edu!samsung!uunet!talos!kjones
;>From elves@magic-tree.keebler.com (The Keebler Elves)
;Newsgroups: comp.emacs
;Subject: filec.el (improved)
;Summary: filename completion for normally non-completing types of
;         minibuffer input.
;Message-ID: <1990Mar21.160311.23399@talos.pm.com>
;Date: 21 Mar 90 16:03:11 GMT
;Sender: kjones@talos.pm.com (Kyle Jones)
;Reply-To: kyle@cs.odu.edu
;Lines: 266
;
;Two new variables: completion-use-environment set non-nil support expansion of
;references to enviromental variables, and completion-slashify-directories set
;non-nil causes the automatic appending of a slash to unambiguously completed
;directory names.  There is a tiny bug fix to minibuffer-completion-message.
;
;Installation instructions are in the comments at the top of the file.
;
;Scream at the peacocks,
;
;kyle jones   <kjones@talos.pm.com>   ...!uunet!talos!kjones
;------------------------------------------------------------
;;; Filename completion in the minibuffer
;;; Copyright (C) 1990 Kyle E. Jones
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; A copy of the GNU General Public License can be obtained from this
;;; program's author (send electronic mail to kyle@cs.odu.edu) or from
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.
;;;
;;; Send bug reports to kyle@cs.odu.edu.

;; This package provides filename completion for the normally
;; non-completing types of minibuffer input.  The central function is
;; minibuffer-complete-file.  This function should be bound to a key in
;; minibuffer-local-map that you want to use to invoke filename
;; completion.  The installtion instructions below assumes your choice
;; will be TAB; change the define-key call to suit your own tastes.
;;
;; To use this package, put it in a file called "filec.el" in a Lisp
;; directory that Emacs knows about, and byte-compile it.
;;
;; At this point you can either:
;;
;;  1. Put the lines:
;;       (require 'filec)
;;       (define-key minibuffer-local-map "\t" 'minibuffer-complete-file)
;;     in your .emacs file.
;;
;;  2. Put the lines:
;;       (autoload 'minibuffer-complete-file "filec" nil t)
;;       (define-key minibuffer-local-map "\t" 'minibuffer-complete-file)
;;     in your .emacs file.

(provide 'filec)

(defvar completion-auto-correct nil
  "*Non-nil means that minibuffer-complete-file should aggressively erase
the trailing part of a word that caused completion to fail, and retry
the completion with the resulting word.")

(defvar completion-use-environment nil
  "*Non-nil value means that minibuffer-complete-file should expand
references to environmental variables.  Such references should appear as
$var, where var is an environmental variable.  To get a $ interpreted
normally in a filename when this variable is set non-nil, you must type $$.")

(defvar completion-slashify-directories nil
  "*Non-nil value means that minibuffer-complete-file should automatically
append a slash to directory names that complete unambiguously.")

(defun minibuffer-complete-file ()
  "Interpret the word under or before the cursor as a filename and complete
it as far as possible."
  (interactive)
  (let ((opoint (point)) beg end unexpanded-word word completion c-list
	directory file-regexp)
    ;; find the beginning and end of the word we're trying to complete
    (if (or (eobp) (memq (following-char) '(?\t ?\n ?\ )))
	(progn
	  (skip-chars-backward " \t\n")   
	  (and (not (eobp)) (forward-char))
	  (setq end (point)))
      (skip-chars-forward "^ \t\n")
      (setq end (point)))
    (skip-chars-backward "^ \t\n")
    (setq beg (point))
    (goto-char opoint)
    ;; copy the word into a string
    (setq word (buffer-substring beg end))
    ;; expand environmental variables if the user requested it.
    (and completion-use-environment
	 (not (eq word (setq word (substitute-in-file-name word))))
	 (progn
	   (delete-region beg end)
	   (insert word)
	   (setq end (+ beg (length word)))))
    (setq unexpanded-word word)
    ;; expand the filename fully so we can compare to the full pathname.
    ;; expand-file-name "resolves" . and .., so we have to shield them.
    (if (and (string-match "\\(^\\|/\\)?\.\.?$" word)
	     (file-directory-p word))
	(setq word
	      (concat (expand-file-name
		       (or (file-name-directory word)
			   default-directory))
		      (file-name-nondirectory word)))
      (while (not (eq word (setq word (expand-file-name word))))))
    ;; extract the directory information from the word
    (setq directory (file-name-directory word))
    ;; extract the file part of the word and convert it to a regular
    ;; expression that matches itself and any other string prefixed by
    ;; it.
    (setq file-regexp
	  (concat "^" (regexp-quote (file-name-nondirectory word))))
    ;; Generate a completion list consisting of the filenames in the
    ;; specified directory (see above), taking into account
    ;; completion-ignored-extensions.
    (setq c-list (directory-files directory t file-regexp)
	  c-list (or (delete-matching-strings
		      (concat (mapconcat 'regexp-quote
					 completion-ignored-extensions
					 "\\|")
			      "$")
		      c-list)
		     c-list)
	  c-list (mapcar 'list c-list))
    ;; Try the word against the completion list.
    (and c-list (setq completion (try-completion word c-list)))
    ;; If completion is nil, figure out what prefix of the word would prefix
    ;; something in the completion list... but only if the user is interested.
    (if (and (null completion) completion-auto-correct)
	(let ((c-list (mapcar 'list (directory-files directory t nil)))
	      (i -1))
	  (while (null (setq completion
			     (try-completion (substring word 0 i) c-list)))
	    (setq i (1- i)))
	  (setq completion (substring word 0 i))))
    ;; If completion is t, we had a perfect match already.
    (if (eq completion t)
	(cond ((cdr c-list)
	       (minibuffer-completion-message "[Complete, but not unique]"))
	      ((and completion-slashify-directories
		    (file-directory-p word)
		    (/= ?/ (substring word -1 (length word))))
	       (goto-char end)
	       (insert "/"))
	      (t
	       (minibuffer-completion-message "[Sole completion]")))
      ;; Compute the difference in length between the completion and the
      ;; word.  A negative difference means no match and the magnitude
      ;; indicates the number of chars that need to be shaved off the end
      ;; before a match will occur.  A positive difference means a match
      ;; occurred and the magnitude specifies the number of new chars that
      ;; can be appended to the word as a completion.
      ;;
      ;; Because of file name expansion, the magnitude of a negative
      ;; difference can be greater than the length of the unexpanded word.
      ;; Therefore the floor value is limited by negative length of the word.
      ;;
      ;; `completion' can be nil here, but the code works anyway because
      ;; (length nil) still equals 0!
      (setq diff (max (- beg end) (- (length completion) (length word))))
      (cond
       ;; We have some completion chars.  Insert them.
       ((> diff 0)
	(goto-char end)
	(insert (substring completion (- diff)))
	(if (and completion-slashify-directories
		 (null (cdr c-list))
		 (file-directory-p completion))
	    (insert "/")))
       ;; The word prefixed more than one string, but we can't complete
       ;; any further.  Either give help or say "Ambiguous".
       ((zerop diff)
	(if (assoc word c-list)
	    (minibuffer-completion-message "[Complete, but not unique]")
	  (if (not completion-auto-help)
	      (minibuffer-completion-message "[Ambiguous]")
	    (minibuffer-show-completions
	     (sort
	      (directory-files
	       directory nil
	       (concat "^" (regexp-quote (file-name-nondirectory word))))
	      'string-lessp)))))
       ;; The word didn't prefix anything... if completion-auto-correct is
       ;; non-nil strip the offending characters and try again.
       (completion-auto-correct
	(goto-char end)
	(delete-char diff)
	(minibuffer-complete-file))
       ;; completion utterly failed, tell the user so.
       (t
	(minibuffer-completion-message "[No match]"))))))

(defun minibuffer-completion-message (string &optional seconds)
  "Briefly display STRING to the right of the current minibuffer input.
Optional second arg SECONDS specifies how long to keep the message visible;
the default is 2 seconds.

A keypress causes the immediate erasure of the STRING, and return of control
to the calling program."
  (let (omax (inhibit-quit t))
    (save-excursion
      (goto-char (point-max))
      (setq omax (point))
      (insert " " string))
    (sit-for (or seconds 2))
    (delete-region omax (point-max))))

(defun minibuffer-show-completions (list)
  "Display LIST in a multi-column listing in the \" *Completions*\" buffer.
LIST should be a list of strings."
  (save-excursion
    (let (longest rows columns list-length q i)
      (set-buffer (get-buffer-create " *Completions*"))
      (erase-buffer)
      (insert "Possible completions are:\n")
      (setq q list
	    list-length 0
	    longest 0)
      (while q
	(setq longest (max longest (length (car q)))
	      list-length (1+ list-length)
	      q (cdr q)))
      ;; provide for separation between columns
      (setq longest (+ 3 longest))
      (setq columns (/ (- (screen-width) 2) longest)
	    rows (/ list-length columns)
	    rows
	    (+ (if (zerop (% list-length columns)) 0 1)
	       rows))
      (setq i columns
	    tab-stop-list nil)
      (while (not (zerop i))
	(setq tab-stop-list (cons (* longest i) tab-stop-list)
	      i (1- i)))
      (setq q list
	    i 0)
      (while q
	(insert (car q))
	(setq i (1+ i)
	      q (cdr q))
	(if (zerop (% i columns))
	    (insert "\n")
	  (tab-to-tab-stop)))
      (goto-char (point-min))
      (display-buffer " *Completions*"))))

(defun delete-matching-strings (regexp list &optional destructively)
  "Delete strings matching REGEXP from LIST.
Optional third arg non-nil means to destructively alter LIST, instead of
working on a copy.

The new version of the list, minus the deleted strings, is returned."
  (or destructively (setq list (copy-sequence list)))
  (let ((curr list) (prev nil))
    (while curr
      (if (not (string-match regexp (car curr)))
	  (setq prev curr
		curr (cdr curr))
	(if (null prev)
	    (setq list (cdr list)
		  curr list)
	  (setcdr prev (cdr curr))
	  (setq curr (cdr curr)))))
    list ))
