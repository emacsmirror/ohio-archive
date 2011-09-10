;;; S mode for GNU Emacs	Mon Apr 24 17:23:18 CDT 1989
;;; Copyright (C) 1989		Doug Bates and Ed Kademan
;;; 				bates@stat.wisc.edu
;;;				kademan@stat.wisc.edu
;;; This program is free software;  you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option) any
;;; later version.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License along
;;; with this program; if not, write to the Free Software Foundation, Inc.,
;;; 675 Mass Ave, Cambridge, MA 02139, USA.
;;; 
;;; This package includes an S mode for editing S data and functions, and
;;; an inferior S mode so that you can run S in a buffer.  It is built on
;;; top of comint (the general command interpreter mode written by Olin
;;; Shivers), and so comint.el (or comint.elc) should be either loaded or
;;; in your load path when you invoke it.  You might want to put something
;;; like the following in your .emacs file:
;;; 	(autoload 'S "~/elisp/S" "" t)
;;; where "~/elisp/S.el" is the path name of this file.  That way, all you
;;; will have to do to get S running is to type "M-x S" from within emacs.
;;;
;;; Aside from the general features offered by comint such as command
;;; history editing and job control, inferior S mode allows you to dump and
;;; load S objects into and from external files, and to display help on
;;; functions.  It also provides name completion while you do these.  For
;;; more detailed information see the documentation strings for S,
;;; inferior-S-mode, S-mode, and comint-mode.  There are also many
;;; variables and hooks available for customizing (see below).
;;;
;;; Bugs:
;;; 	Inferior S mode doesn't do a very good job of offering defaults
;;; when it prompts for names and it is often not wise to accept them even
;;; when they look right.
;;; 	S mode for editing S source is basically just fundamental mode with
;;; a few extra key bindings.  It could be a lot fancier.

(require 'comint)
(provide 'S)

;;; Inferior S mode
;;;======================================================================
;;;

(defvar inferior-S-program "S"
  "*Program name for invoking an inferior S.")

(defvar explicit-S-args nil
  "*String of arguments passed to the S process on startup if the name of
the S program is `S'.")

(defvar inferior-S-prompt "^\\(\\+\\|[^>]*>\\) *"
  "*The regular expression inferior S mode uses for recognizing prompts")

(defvar S-scratch-file nil
  "*The name of the scratch source file that receives dumped objects.")

(defvar S-scratch-directory "/tmp"
  "*The directory inferior S puts the scratch source files into.")

(defvar S-source-modes '(S-mode)
  "*A list of modes used to determine if a buffer contains S source code.
If a file is loaded into a buffer that is in one of these major modes, it
is considered an S source file.  The function S-load-file uses this to
determine defaults.")

(defvar inferior-S-load-command "source(\"%s\")\n"
  "*Format-string for building the S command to load a file.
This format string should use %s to substitute a file name
and should result in an S expression that will command the inferior S
to load that file.")

(defvar inferior-S-dump-command "dump(\"%s\",file=\"%s\")\n"
  "*Format-string for building the S command to dump an object into a file.
This format string should use %s to substitute an object and a file name.")

(defvar inferior-S-help-command "help(\"%s\")\n"
  "*Format-string for building the S command to ask for help on an object.
This format string should use %s to substitute an object name.")

(defvar inferior-S-search-list-command "attach()\n"
  "*S command that prints out the search list---the directory paths that S
uses when it searches for objects")

(defvar S-directory nil
  "The directory S is running from.  Set by the S function and not by the
user.")

(defvar S-prev-load-dir/file nil
  "This symbol saves the (directory . file) pair used in the last
S-load-file command.  Used for determining the default in the next one.")

(defvar inferior-S-mode-map nil)
(if inferior-S-mode-map
    nil
  (setq inferior-S-mode-map (full-copy-sparse-keymap comint-mode-map))
  (define-key inferior-S-mode-map "\C-cl" 'S-load-file)
  (define-key inferior-S-mode-map "\C-cd" 'S-dump-object-into-scratch)
  (define-key inferior-S-mode-map "\C-ch" 'S-display-help-on-object))

(defvar inferior-S-mode-hook '()
  "Hook for customizing inferior S mode")

(defun S ()
  "Run an inferior S process, input and output via buffer *S*.
If there is a process already running in *S*, just switch to that buffer.
Takes the program name from the variable inferior-S-program.
The S program name is used to make a symbol name such as `explicit-S-args'.
If that symbol is a variable its value is used as a string of arguments
when invoking S.
\(Type \\[describe-mode] in the process buffer for a list of commands.)"
  (interactive)
  (if (not (comint-check-proc "*S*"))
      (let* ((symbol-string
	      (concat "explicit-" inferior-S-program "-args"))
	     (switches-symbol (intern-soft symbol-string))
	     (switches
	      (if (and switches-symbol (boundp switches-symbol))
		  (symbol-value switches-symbol)))
	     (tbuffer (generate-new-buffer "getting S-directory"))
	     sprocess)
	(set-buffer
	 (if switches
	     (make-comint "S" inferior-S-program nil switches)
	   (make-comint "S" inferior-S-program nil)))
	(inferior-S-mode)
	(setq sprocess (get-process "S"))
	;; Make sure S has started.
	(while (progn
		 (accept-process-output sprocess)
		 (goto-char (point-max))
		 (beginning-of-line)
		 (not (looking-at inferior-S-prompt))))
	(goto-char (point-max))
	;; Now get S-directory.
	(command-to-S "!pwd\n" tbuffer)
	(set-buffer tbuffer)
	(setq S-directory (buffer-substring (point-min) (- (point-max) 1)))
	(kill-buffer tbuffer)))
  (switch-to-buffer "*S*"))

(defun inferior-S-mode () 
  "Major mode for interacting with an inferior S process.  
Runs an S interactive job as a subprocess of Emacs, with I/O through an
Emacs buffer.  Variable inferior-S-program controls which S
is run.

\\{inferior-S-mode-map}

Do not type \\[S-dump-object-into-scratch] or \\[S-display-help-on-object]
when you are in the middle of delivering a multi-line command to S and S is
prompting you with its secondary prompt. It's ok to do this if S hasn't
seen the initial lines yet---that is, if you ended those lines with
something other than a \"send input\" command (usually bound to RETURN).

Customization: Entry to this mode runs the hooks on comint-mode-hook and
inferior-S-mode-hook (in that order).

You can send text to the inferior S process from other buffers containing
S source.
    switch-to-S switches the current buffer to the S process buffer.
    S-eval-buffer sends the current buffer to the S process.
    S-eval-region sends the current region to the S process.

    S-eval-buffer-and-go, and S-eval-region-and-go
	switch to the S process buffer after sending their text.

Commands:
Return after the end of the process' output sends the text from the 
    end of process to point.
Return before the end of the process' output copies the sexp ending at point
    to the end of the process' output, and sends it.
Delete converts tabs to spaces as it moves back.
C-M-q does Tab on each line starting within following expression.
Paragraphs are separated only by blank lines.  Crosshatches start comments.
If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it."
  (interactive)
  (comint-mode)
  (setq comint-prompt-regexp inferior-S-prompt)
  (setq major-mode 'inferior-S-mode)
  (setq mode-name "Inferior S")
  (setq mode-line-process '(": %s"))
  (use-local-map inferior-S-mode-map)
  (run-hooks 'inferior-S-mode-hook))

(defun S-dump-object-into-scratch (object)
  "Dump the S object into a file (and buffer) for editing."
  (interactive (find-S-object "Object to edit: "))
  (let* ((filename (concat S-scratch-directory
			   "/"
			   (or S-scratch-file (make-temp-name "scr."))))
	 (complete-dump-command (format inferior-S-dump-command
					object filename))
	 old-scratch-buffer)
    (command-to-S complete-dump-command)
    (if (setq old-scratch-buffer (get-file-buffer filename))
	(kill-buffer old-scratch-buffer)) ;make sure we start fresh
    (find-file-other-window filename)
    (S-mode)
    (setq S-prev-load-dir/file
	  (cons (file-name-directory filename)
		(file-name-nondirectory filename)))))

(defun find-S-object (p-string)
  (let* ((default (find-S-object-default))
	 (prompt-string (if default
			    (format "%s(default %s) " p-string default)
			  p-string))
	 (S-object-list (get-S-object-list (get-S-search-list)))
	 (spec (completing-read prompt-string S-object-list)))
    (list (or spec default))))

(defun find-S-object-default ()
  (save-excursion
    (while (looking-at "\\sw\\|\\s.")
      (forward-char 1))
    (if (re-search-backward "\\sw\\|\\s." nil t)
	(progn (forward-char 1)
	       (buffer-substring (point)
				 (progn (forward-sexp -1)
					(while (looking-at "\\s'")
					  (forward-char 1))
					(point))))
      nil)))

(defun get-S-search-list ()
  "Get the list of directories that S searches when it looks for objects."
  (let ((tbuffer (generate-new-buffer "search-list"))
	S-search-list
	dir)
    (buffer-flush-undo tbuffer)
    (set-buffer tbuffer)
    (command-to-S inferior-S-search-list-command tbuffer)
    (goto-char (point-max))
    (while (re-search-backward "\"\\([^\"]*\\)\"" nil t)
      (setq dir (buffer-substring (match-beginning 1) (match-end 1)))
      (setq dir
	    (cond ((string-match "^\\." dir)
		   (concat S-directory (substring dir 1)))
		  ((string-match "^[^/]" dir)
		   (concat S-directory "/" dir))
		  (t
		   dir)))
      (setq S-search-list (cons dir S-search-list)))
    (kill-buffer tbuffer)
    (symbol-value 'S-search-list)))

(defun get-S-object-list (s-list)
  "Return the alist of current S object names (suitable for use with
completing-read).  S-LIST is the search list of directories for S."
  (if (null s-list)
      nil
    (append (mapcar (function list)(directory-files (car s-list)))
	    (get-S-object-list (cdr s-list)))))

(defun command-to-S (com &optional buf)
  "Send the S process a COMMAND and delete the output from the S process
buffer.  If an optional second argument BUFFER exists save the output
there. (BUFFER cannot be the S process buffer.)"
  (let* ((cbuffer (current-buffer))
	 (sprocess (get-process "S"))
	 (sbuffer (process-buffer sprocess))
	 place-holder
	 last)
    (set-buffer sbuffer)
    (setq place-holder (point-marker))
    (kill-region (process-mark sprocess) (point-max))
    (setq last (point-max))
    (process-send-string sprocess com)
    (while (progn
	     (accept-process-output sprocess)
	     (goto-char (point-max))
	     (beginning-of-line)
	     (or (= (point-max) last)
		 (not (looking-at inferior-S-prompt)))))
    (if buf
	(append-to-buffer buf last (point)))
    (delete-region last (point-max))
    (yank)				;possible command in process
    (goto-char (marker-position place-holder))
    (set-buffer cbuffer)))

(defun S-display-help-on-object (object)
  "Display the help page for OBJECT in the *Help* buffer."
  (interactive (find-S-object "Help on: "))
  (let (tbuffer)
    (pop-to-buffer "*Help*")
    (setq tbuffer (current-buffer))
    (delete-region (point-min) (point-max))
    (command-to-S (format inferior-S-help-command object) tbuffer)
    (goto-char (point-min))))

(defun S-load-file (filename)
  "Load an S source file into an inferior S process."
  (interactive (comint-get-source "Load S file: "
				  S-prev-load-dir/file
				  S-source-modes
				  nil))
  (comint-check-source filename)
  (setq S-prev-load-dir/file
	(cons (file-name-directory filename)
	      (file-name-nondirectory filename)))
  (process-send-string "S" (format inferior-S-load-command
				   filename))
  (switch-to-S t))

;;; S mode
;;;======================================================================
;;;

(defvar S-mode-map nil)
(if S-mode-map
    nil
  (setq S-mode-map (make-sparse-keymap))
  (define-key S-mode-map "\C-cr"    'S-eval-region)
  (define-key S-mode-map "\C-c\C-r" 'S-eval-region-and-go)
  (define-key S-mode-map "\C-cb"    'S-eval-buffer)
  (define-key S-mode-map "\C-c\C-b" 'S-eval-buffer-and-go)
  (define-key S-mode-map "\C-cz"    'switch-to-S)
  (define-key S-mode-map "\C-cl"    'S-load-file))

(defvar S-mode-hook '()
  "Hook for customizing S mode.")

(defun S-mode ()
  "Major mode for editing S source.

\\{S-mode-map}

Customization: Entry to this mode runs the hooks in S-mode-hook.

You can send text to the inferior S process from other buffers containing
S source.
    switch-to-S switches the current buffer to the S process buffer.
    S-eval-buffer sends the current buffer to the S process.
    S-eval-region sends the current region to the S process.

    S-eval-buffer-and-go, and S-eval-region-and-go
	switch to the S process buffer after sending their text."
  (interactive)
  (setq major-mode 'S-mode)
  (setq mode-name "S")
  (use-local-map S-mode-map)
  (run-hooks 'S-mode-hook))

(defun S-eval-region (start end)
  "Send the current region to the inferior S process."
  (interactive "r")
  (process-send-region "S" start end)
  (process-send-string "S" "\n"))

(defun S-eval-region-and-go (start end)
  "Send the current region to the inferior S and switch to the process
buffer."
  (interactive "r")
  (S-eval-region start end)
  (switch-to-S t))

(defun S-eval-buffer ()
  "Send the current buffer to the inferior S process."
  (interactive)
  (S-eval-region (point-min) (point-max)))

(defun S-eval-buffer-and-go ()
  "Send the current buffer to the inferior S and switch to the process
buffer."
  (interactive)
  (S-eval-buffer)
  (switch-to-S t))

(defun switch-to-S (eob-p)
  "Switch to the inferior S process buffer.
With argument, positions cursor at end of buffer."
  (interactive "P")
  (cond ((comint-check-proc "*S*")
	 (pop-to-buffer "*S*")
	 (cond (eob-p
		(push-mark)
		(goto-char (point-max)))))
	(t
	 (message "No inferior S process")
	 (ding))))
