;; jargon-mode.el - Major mode for reading the Jargon File
;;
;; Version 0.9
;;
;; Copyright (C) 1985, 1986 Free Software Foundation, Inc.
;; Copyright (C) 1993 Sascha Wildner <swildner@channelz.GUN.de>,
;;
;; Authors:
;;
;;	Sascha Wildner <swildner@channelz.GUN.de>
;;	Jonathan Stigelman <Stig@netcom.com>
;;
;; Other contributors:
;;
;;	Dong-Ping Deng <deng@bunny.rhic.bnl.gov>
;;	Erik C. Ostrom <eostrom@nic.gac.edu>
;;	Timo Rinne <tri@cirion.fi>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; The GNU General Public License is available by anonymous ftp from
;; prep.ai.mit.edu in pub/gnu/COPYING.  Alternately, you can write to
;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139,
;; USA.
;;
;;
;; To install, copy jargon-mode.el into your lisp directory, byte-compile it
;; (M-x byte-compile-file) and put the following lines into your .emacs file:
;;
;; (autoload 'jargon-mode "jargon-mode"
;;           "Major mode for browsing the Jargon File." t)
;; (setq auto-mode-alist (cons
;;                        '("[jJ]argon.*\\.\\(txt\\|ascii\\)" . jargon-mode)
;;                        auto-mode-alist))
;;
;;
;; --- HISTORY ---
;; 19 Feb 93
;;		Created jargon.el
;; 30 Mar 93
;;		Changed name to jargon-mode.el
;;		Added installation information
;; 31 Mar 93
;;		jargon-next-entry now goes ahead when at preceding colon
;;		Rebound '<' to jargon-first-entry and '>' to jargon-last-entry
;;		Added jargon-beginning-of-entry
;;		Added jargon-next-reference and jargon-prev-reference
;;		Added default to jargon-chase-reference
;;		Added prefix argument handling where it makes sense
;;		Buffer is now made read-only
;;		Bound scroll-up and scroll-down to <SPC> and <DEL>
;;		Fixed a bug in jargon-chase-reference (remove TABs and NLs)
;; 06 Apr 93
;;		Bound describe-mode to '?'
;; 11 Apr 93
;;		Fixed a bug in jargon-chase-reference (sort out double refs)
;; 14 Apr 93
;;		Added jargon-output-to-cut-file
;; 16 Apr 93
;;		jargon-chase-reference now catches "" input
;;		Added jargon-quit
;; 17 Apr 93
;;		Rebound '?' to jargon-mini-help
;;		A longer help can now be obtained with describe-mode
;;		jargon-chase-reference now catches pseudo-entries
;;		jargon-mode now runs jargon-mode-hook as the last step
;; 22 Apr 93
;;		case-fold-search is nil now only when necessary
;; 26 Apr 93
;;		Wrote a better documentation string for jargon-mode
;; 21 May 93
;;		jargon-prev-entry at the first entry now moves to the top
;;		jargon-next-entry at the last entry now moves to the bottom
;; 22 May 93
;;		jargon-chase-reference now finds the first reference in a file
;; 27 May 93
;;		Added jargon-chase-reference-blind and
;;		jargon-trace-back-references
;; 31 May 93
;;		Added jargon-prev-keyword and jargon-next-keyword
;;
;; May/June 93	Jonathan Stigelman <Stig@netcom.com>
;;		
;;		changed the keybindings around to correspond more to info
;;              mode and view mode.
;;
;;		added narrowwing functionality...  Causes ugliness elsewhere
;;		because the code could stand to be more modular.
;;
;;              tried to reduce some of the redundant code...
;;              
;;              added highlighting support for emacs 19 (get my hilit19.el)
;;		
;;		eliminated redundant creation of very long completion list
;;		for jargon-find-entry
;;
;;		added jargon-dfs-cut-to-buffer (Depth First Search).
;;		It's kinda slow...because of missing modularity.
;;
;; LCD Archive Entry:
;; jargon-mode.el|Sascha Wildner|swildner@channelz.GUN.de|
;; Major mode for reading the Jargon File.|
;; 28-June-93|0.9|~/modes/jargon-mode.el.Z|

(defvar jargon-mode-map nil
  "Local keymap for jargon mode buffers.")

(defvar jargon-entry-regexp "^:[^:]+:"
  "Regular expression for recognizing entries.")

(defvar jargon-reference-regexp "{[^{}]+}"
  "Regular expression for recognizing references.")

(defvar jargon-cut-file (expand-file-name "~/jargon.cut")
  "File where exerpts of the jargon file are placed")

(defvar jargon-reference-backtrace nil
  "List to trace back reference jumps.")

(defvar jargon-narrow-mode t
  "Buffer is narrowwed to just the entry that you're reading")

(defvar jargon-highlight t
  "Buffer is narrowwed to just the entry that you're reading")

(if jargon-mode-map
    nil
  (setq jargon-mode-map (make-keymap))
  (suppress-keymap jargon-mode-map)

  ; these mimic less(1) (or view-less.el)
  (define-key jargon-mode-map " " 'scroll-up)
  (define-key jargon-mode-map "b" 'scroll-down)
  (define-key jargon-mode-map "g" 'jargon-first-entry)
  (define-key jargon-mode-map "G" 'jargon-last-entry)
  (define-key jargon-mode-map "/" 'isearch-forward)
  (define-key jargon-mode-map "?" 'isearch-backward)

  ; these mimic info mode
  (define-key jargon-mode-map "n" 'jargon-next-entry)
  (define-key jargon-mode-map "p" 'jargon-prev-entry)
  (define-key jargon-mode-map "q" 'jargon-quit)
  (define-key jargon-mode-map "h" 'jargon-mini-help)
  (define-key jargon-mode-map "s" 'jargon-find-entry)
  (define-key jargon-mode-map "f" 'jargon-chase-reference-query)
  (define-key jargon-mode-map "l" 'jargon-trace-back-references)
  (define-key jargon-mode-map "u" 'jargon-trace-back-references)
  (define-key jargon-mode-map "\177" 'jargon-trace-back-references)
  (define-key jargon-mode-map "L" 'jargon-clear-trace-back)

  ; synonyms and jargon-specific bindings
  (define-key jargon-mode-map "a" 'jargon-beginning-of-entry)

  (define-key jargon-mode-map "D" 'jargon-dfs-to-buffer)
  (define-key jargon-mode-map "N" 'jargon-narrow-toggle)
  (define-key jargon-mode-map "w" 'widen)

  (define-key jargon-mode-map "c" 'jargon-chase-reference)
  (define-key jargon-mode-map "\r" 'jargon-chase-reference)

  (define-key jargon-mode-map "\M-\t" 'jargon-prev-reference)
  (define-key jargon-mode-map "\t" 'jargon-next-reference)

  (define-key jargon-mode-map "[" 'jargon-prev-keyword)
  (define-key jargon-mode-map "]" 'jargon-next-keyword)

  (define-key jargon-mode-map "o" 'jargon-output-to-cut-file)
  )

(defun jargon-beginning-of-entry ()
  "Go to the beginning of the current entry."
  (interactive)
  (beginning-of-line)
  (if (not (looking-at jargon-entry-regexp))
      (re-search-backward jargon-entry-regexp (point-min) 'noerr)))

(defun jargon-chase-reference-query ()
  (interactive)
  (jargon-chase-reference 'ask))

(defun jargon-slurp-ref (&optional where)
  "get the reference under point"
  (let (default i (opoint (point)))
    (if where (goto-char where))
    (setq default (cond ((looking-at jargon-reference-regexp)
			 (buffer-substring (1+ (match-beginning 0))
					   (1- (match-end 0))))
			((and (save-excursion
				(search-backward "{" nil t)
				(looking-at jargon-reference-regexp))
			      (> (match-end 0) (point)))
			 (buffer-substring (1+ (match-beginning 0))
					   (1- (match-end 0))))
			(t "")))
    (goto-char opoint)
    (while (setq i (string-match "[ \n\t]+" default i))
      (setq default (concat (substring default 0 i) " "
			    (substring default (match-end 0))))
      (setq i (1+ i)))
    default))

(defun jargon-read-ref-complete ()
  (let ((completion-ignore-case nil)
	jargon-narrow-mode jargon-highlight
	completions str end default entry)
    (save-restriction
      (widen)
      (save-excursion
	 (jargon-next-entry)
	 (setq end (point))
	 (jargon-prev-entry)
	 (while (re-search-forward jargon-reference-regexp end t)
	   (setq str (jargon-slurp-ref (match-beginning 0)))
	   (or (member str completions)
	       (setq completions (cons (cons str nil) completions))))))
    (if (not completions)
	(error "No references in this entry")
      (setq default (cond ((eq (length completions) 1)
			   (car (car completions)))
			  (t (jargon-slurp-ref))))
      (while (null entry)
	(setq entry (completing-read "Chase reference: " completions
				     nil t default))
	(if (string= entry "")
	    (setq entry nil)))
      entry)))

(defun jargon-chase-reference (&optional ask)
  "Follow a reference from the current entry."
  (interactive)
  (let ((entry (if ask
		   (jargon-read-ref-complete)
		 (jargon-slurp-ref))))
    (setq jargon-reference-backtrace
	  (cons (point) jargon-reference-backtrace))
    (jargon-find-entry entry)))

(defun jargon-clear-trace-back ()
  (interactive)
  (setq jargon-reference-backtrace nil)
  (message "Reference backtrace cleared."))

(defvar jargon-find-completions nil
  "List of completions for searches")

(defun jargon-build-completions ()
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (while (re-search-forward jargon-entry-regexp nil t)
	(setq jargon-find-completions
	      (cons (cons (buffer-substring
			   (1+ (match-beginning 0))
			   (1- (match-end 0))) nil)
		    jargon-find-completions)))))
  jargon-find-completions)

(defun jargon-find-entry (str)
  "Find a certain entry."
  (interactive
   (let ((completion-ignore-case nil))
     (or jargon-find-completions
	 (jargon-build-completions)
	 (error "No entries in this buffer"))
     (list (completing-read "Find entry: " jargon-find-completions nil t))))
  (widen)
  (let ((case-fold-search nil)
	(opoint (point)))
    (goto-char (point-min))
    (goto-char (if (re-search-forward (concat "^:" (regexp-quote str)
					      ":") nil t)
		   (match-beginning 0)
		 opoint)))
  (jargon-narrow))

(defun jargon-narrow-toggle ()
  "toggle jargon-narrow-mode"
  (interactive)
  (and jargon-narrow-mode (progn (widen) (recenter)))
  (if (setq jargon-narrow-mode (not jargon-narrow-mode))
      (jargon-narrow)))

(defun jargon-narrow ()
  "narrow jargon buffer to just this word's definition"
  (if (or jargon-highlight jargon-narrow-mode)
      (let (p1 p2)
	(save-excursion
	  (forward-char)
	  (re-search-forward "^\\(= . =\\|:[^:]+:\\)" nil t)
	  (beginning-of-line)
	  (setq p2 (point))
	  (forward-char)
	  (re-search-backward jargon-entry-regexp nil t)
	  (setq p1 (point)))
	(and jargon-narrow-mode (narrow-to-region p1 p2))
	(if (and jargon-highlight (featurep 'hilit19))
	    (hilit-rehighlight-region p1 p2 'quietly)))))

(defun jargon-first-entry ()
  "Go to the first entry."
  (interactive)
  (widen)
  (goto-char (point-min))
  (jargon-next-entry))

(defun jargon-last-entry ()
  "Go to the last entry."
  (interactive)
  (widen)
  (goto-char (point-max))
  (jargon-prev-entry))

(defun jargon-mini-help ()
  "Display some commands in the minibuffer."
  (interactive)
  (message
   "[n]ext, [p]revious, [TAB] next ref, [RET] follow ref, [u]p to last ref, [q]uit"))

(defun jargon-next-entry (&optional arg)
  "Go to the next entry.  With arg, do it arg times."
  (interactive "p")
  (widen)
  (or arg (setq arg 1))
  (while (> arg 0)
    (forward-char)
    (if (re-search-forward jargon-entry-regexp (point-max) 0)
	(beginning-of-line))
    (setq arg (1- arg)))
  (jargon-narrow))

(defun jargon-next-keyword (&optional arg)
  "Move to the next reference or entry.  With arg, do it arg times."
  (interactive "p")
  (or arg (setq arg 1))
  (while (> arg 0)
    (condition-case nil
	(jargon-next-reference 1)
      (error (jargon-next-entry 1)))
    (setq arg (1- arg))))

(defun jargon-next-reference (&optional arg)
  "Move to the next reference in this entry.  With arg, do it arg times."
  (interactive "p")
  (or arg (setq arg 1))
  (let ((end (save-excursion (forward-char)
			     (re-search-forward jargon-entry-regexp
						(point-max) 'noerr)
			     (beginning-of-line)
			     (point))))
    (while (> arg 0)
      (forward-char)
      (if (re-search-forward jargon-reference-regexp end t)
	  (goto-char (match-beginning 0))
	(backward-char)
	(error "No more references in this entry"))
      (setq arg (1- arg)))))

(defun jargon-prev-entry (&optional arg)
  "Go to the previous entry.  With arg, do it arg times."
  (interactive "p")
  (widen)
  (or arg (setq arg 1))
  (while (> arg 0)
    (if (re-search-backward jargon-entry-regexp (point-min) 0)
	(beginning-of-line))
    (setq arg (1- arg)))
  (jargon-narrow))

(defun jargon-prev-keyword (&optional arg)
  "Move to the previous reference or entry.  With arg, do it arg times."
  (interactive "p")
  (or arg (setq arg 1))
  (while (> arg 0)
    (condition-case nil
	(jargon-prev-reference 1)
      (error (if (not (looking-at jargon-entry-regexp))
		 (jargon-prev-entry 1)
	       (widen)
	       (if (bobp)
		   nil
		 (backward-char 2)
		 (jargon-narrow)
		 (setq arg (1+ arg))))))
    (setq arg (1- arg))))

(defun jargon-prev-reference (&optional arg)
  "Move to the previous reference in this entry.  With arg, do it arg times."
  (interactive "p")
  (or arg (setq arg 1))
  (let ((beg (save-excursion (re-search-backward jargon-entry-regexp
						 (point-min) 'noerr)
			     (beginning-of-line)
			     (point))))
    (while (> arg 0)
      (if (re-search-backward jargon-reference-regexp beg t)
	  (goto-char (match-beginning 0))
	(error "No previous references in this entry"))
      (setq arg (1- arg)))))

(defun jargon-read-cutfile-name (prompt)
  (read-file-name
   (concat prompt " (default " (file-name-nondirectory jargon-cut-file) ") ")
   (file-name-directory jargon-cut-file)
   jargon-cut-file))

(defun jargon-output-to-cut-file (file-name)
  "Append the current entry to a cut file."
  (interactive (list (jargon-read-cutfile-name "Append entry to cut file:")))
  (save-excursion
    (jargon-beginning-of-entry)
    (setq beg (point))
    (jargon-next-entry)
    (setq end (point))
    (append-to-file beg end file-name)))

(defun jargon-cut-to-buffer (buf-name)
  "Append the current entry to a cut file."
  (interactive "BCut to buffer: ")
  (save-excursion
    (jargon-beginning-of-entry)
    (setq beg (point))
    (jargon-next-entry)
    (setq end (point))
    (append-to-buffer buf-name beg end)))

(defun jargon-dfs-to-buffer (buffer depth)
  "Do a depth-first elaboration of the current keyword and dump this to buffer"
  (interactive "BRecursive cut-to-buffer: \nnDepth: ")
  (let ((jargon-dfs-visited nil)
	(jargon-dfs-buffer buffer)
	(jargon-dfs-maxdepth depth)
	jargon-narrow-mode jargon-highlight)
    (jargon-dfs-dump 0))
  (jargon-narrow))

(defun jargon-dfs-dump (level)
  (jargon-beginning-of-entry)
  (if (memq (point) jargon-dfs-visited)
      nil
    (setq jargon-dfs-visited (cons (point) jargon-dfs-visited))
    (message (buffer-substring (point) (save-excursion (forward-char)
						       (search-forward ":"))))
    (jargon-cut-to-buffer jargon-dfs-buffer)
    (let (done)
      (while (and (< level jargon-dfs-maxdepth) (not done))
	(condition-case nil
	    (progn (jargon-next-reference)
		   (save-excursion
		     (jargon-chase-reference)
		     (jargon-dfs-dump (1+ level))))
	  (error (setq done t)))))))

(defun jargon-trace-back-references ()
  "Trace back reference jumps."
  (interactive)
  (widen)
  (if (null jargon-reference-backtrace)
      (error "No further backtrace.")
    (goto-char (car jargon-reference-backtrace))
    (jargon-narrow)
    (setq jargon-reference-backtrace 
	  (cdr jargon-reference-backtrace))))

(defun jargon-quit ()
  "Quit reading the Jargon File and select another buffer."
  (interactive)
  (switch-to-buffer (prog1 (other-buffer (current-buffer))
		      (bury-buffer (current-buffer)))))

(defun jargon-mode ()
  "Major mode for reading the Jargon File.

The Jargon File is a huge collection of hacker slang, humor, and folklore which
is currently maintained by Eric S. Raymond <esr@snark.thyrsus.com>.  It's
available for anonymous ftp on prep.ai.mit.edu in the directory pub/gnu.

The following commands are available:

  \\[jargon-mini-help]	Display quick reference in minibuffer.

  \\[jargon-next-entry]	Go to the next entry.
  \\[jargon-prev-entry]	Go to the previous entry.
  \\[jargon-find-entry]	Go to a specific entry.
  \\[jargon-beginning-of-entry]	Go to the beginning of current entry.

  \\[jargon-chase-reference-query]	Chase a reference.
  \\[jargon-chase-reference]	Chase a reference w/o confirmation.
  \\[jargon-next-reference]	Go to the next reference
  \\[jargon-prev-reference]	Go to the previous reference.
  \\[jargon-trace-back-references]	Trace back reference jumps.
  \\[jargon-clear-trace-back]	Clear the reference jump backtrace.

  \\[jargon-narrow-toggle]	Toggle jargon-narrow-mode
  \\[widen]			Temporarily widen the buffer

  \\[jargon-next-keyword]	Next keyword (entry or reference).
  \\[jargon-prev-keyword]	Previous keyword.
  \\[jargon-first-entry]	Go to the first entry.
  \\[jargon-last-entry]	Go to the last entry.

  \\[jargon-output-to-cut-file]	Append the current entry to a cut file.
  \\[jargon-quit]	Quit reading the Jargon File.

The following variables influence the behavior of jargon-mode:

jargon-entry-regexp
	The regular expression used for recognizing entries.

jargon-reference-regexp
	The regular expression used for recognizing references.


Turning on jargon-mode calls the value of the variable jargon-mode-hook with
no args, if that value is non-nil."
  (interactive)
  (setq major-mode 'jargon-mode)
  (setq mode-name "Jargon")
  (setq buffer-read-only t)
  (use-local-map jargon-mode-map)
  (run-hooks 'jargon-mode-hook))
