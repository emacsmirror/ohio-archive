;----Le laboratoire de robotique de l'Institut de recherche d'Hydro-Quebec-----
; 
; Nom     : (global-replace-lines)
;	    (global-replace <string> <replacement>)
;	    (global-grep-and-replace
;   	    	    	    <string> <replacement> <files> <grep-command>)
; Fonction: Search and replace strings in multiple files.
; Fichiers: global-replace.el
; Notes   : This version is only for Emacs 19.
; 
; Cree    : 20 avril 90 --------- Martin Boyer <mboyer@ireq-robot.uucp>
; Modifie :  7 mars 94 --------4- Martin Boyer <mboyer@ireq-robot.hydro.qc.ca>
;           Copyright (C) 1985, 86, 87, 93 Free Software Foundation, Inc.
;           Copyright (c) 1990, 1994 Martin Boyer and Hydro-Quebec
; 
; Historique: 
; 
;  7 mars 94 --------4- Martin Boyer <mboyer@ireq-robot.hydro.qc.ca>
; 	Updated the documentation after initial comments from the net and the
; 	FSF.  This is version 1.1.
; 
;  1 mars 94 --------3- Martin Boyer <mboyer@ireq-robot.hydro.qc.ca>
; 	Version 1.0: Prepared for distribution through LCD.
; 
; 23 novembre 93 ----2- Martin Boyer <mboyer@ireq-robot.hydro.qc.ca>
; 	Ported to version 19.
; 	No longer requires a modified version of compile.el.
;------------------------------------------------------------------------------

;;; COPYRIGHT NOTICE
;;;
;; Copyright (C) 1985, 86, 87, 93 Free Software Foundation, Inc.
;; Copyright (C) 1990, 1994 Martin Boyer and Hydro-Quebec.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; author of this program <mboyer@ireq-robot.hydro.qc.ca> or to the
;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; Send bug reports to the author, <mboyer@ireq-robot.hydro.qc.ca>.


;;; DESCRIPTION AND USAGE
;;;
;; This program can be used to search for and replace strings in
;; multiple files.  The idea is to find, in a set of files, all the
;; lines that contain a string to be changed.  Then, one can edit
;; the result of this search, and the program will take care of the
;; tedious task of splicing the changes back into the original files.
;;
;; The search operation can be done separately, before calling any
;; command in this file.  The output of the search has be be in a
;; special format and, indeed, there is an emacs command that does
;; that for you; see the description of the grep command (C-h f grep
;; [RET]).
;;
;; Once you are satisfied with the list of lines to be changed, you
;; can edit it in any way you like, under the following conditions:
;;
;;  	1. The file names and line numbers are not edited.
;;  	2. Lines can be deleted altogether, in which case
;;	   the program will not attempt to put them back in
;;	   the original files.
;;  	3. A single line can be replaced by multiple lines if carriage
;;	   returns (^M) are used to delimit the end of a line.  In
;;	   other words, the program replaces all '\r' by '\n' just
;;	   before putting the edited lines back in place.
;;
;; The last step is to call global-replace-lines.  This command will
;; prompt you for every line in the "grep" buffer -- even those that
;; you didn't edit, asking if you want to carry to the original file
;; the change you made on that line.  Since a large number of files is
;; involved, the command will alsl ask you if you want to dispose of a
;; buffer after all processing has been completed on that buffer.
;;
;; Eventually, the command will also present a preformatted ChangeLog
;; entry, saving you some time in keeping the ChangeLog file up to
;; date.  But I haven't had time to code this yet.
;;
;; If you prefer, there are two commands that can call grep for you,
;; and then proceed to carry out the changes.  global-grep-and-replace
;; will prompt you for original and replacement strings, and for a
;; list of files.  It will then call grep, allow you to inspect and
;; modify the result, and then will preform the actual replacements.
;; global-replace is similar, but will skip the inspection stage.
;;
;; You may wish to examine the documentation of the query.el library.
;; But if you don't, here's the short story: whenever you are prompted
;; and a set of possible answers appear between [brackets], the first
;; is always the default.
;;
;;
;; This program differs from tags-query-replace for the following
;; reasons:
;; 
;; 1. tags-query-replace requires a TAGS file, and not all files can
;;    be processed by etags.
;; 
;; 2. Shell wildcards are more flexible that TAGS files to specify
;;    which files to process.  Anyhow, some variants of grep can take
;;    a list a files from a file, so the TAGS functionality can be
;;    duplicated.
;; 
;; 3. Mainly, global-replace is more flexible in the way the
;;    replacements are done; the list of lines to be changed, along
;;    with the name of the file where they appear, is directly
;;    available in an emacs buffer.  From then on, the user can elect
;;    not to change lines from certain files, or to process certain
;;    files differently, etc.
;; 
;; 4. Finally, I find it faster and easier to use, since I specify all
;;    the replacements in one operation (editing the result from
;;    grep), and then tell emacs to do the tedious job of actually
;;    putting the changes back in the files.


;;; INSTALLATION
;;;
;; Put this file somewhere in your load-path and byte-compile it.
;; Retrieve and install query.el in the same fashion.
;; Put the following lines in your .emacs:
;;
;; (autoload 'global-replace-lines "global-replace"
;;           "Put back grepped lines" t)
;; (autoload 'global-replace "global-replace"
;;           "query-replace across files" t)
;; (autoload 'global-grep-and-replace "global-replace"
;;           "grep and query-replace across files" t)


;; LCD Archive Entry:
;; global-replace|Martin Boyer|mboyer@ireq-robot.hydro.qc.ca|
;; Search and replace strings in multiple files|
;; 07-Mar-1994|1.1|~/packages/global-replace.el.Z|


;;; BUGS
;;;
;; next-error-descriptor" and "show-next-error" are taken almost
;; verbatim from the compile.el library from emacs 19.19.  I consider
;; this is a misfeature; "paralleling" code is a waste of resources
;; and is harder to maintain.  I believe the solution would be to
;; incorporate the functionality of those two functions in compile.el,
;; so that other packages can use them.  I just don't have time to do
;; it right now.  I have heard the FSF is considering it.



(require 'compile)
(require 'query)
(require 'cl-19 "cl")

(defconst global-replace-version "1.1"
  "The version number of the \"global-replace\" library.")

;;; This should be provided by the compile.el library.
;;; As is it, it's almost a copy of (next-error).
(defun next-error-descriptor (&optional argp)
  "Return the next error message descriptor, or nil if there are no more errors.
Each error descriptor is a cons.  Its car is a marker pointing to an error
message, and its cdr is a marker pointing to the text of the line the message
is about.

The optional argument specifies how many error messages to move;
negative means move back to previous error messages.
Just C-u as an argument means reparse the error message buffer
and start at the first error.

next-error-descriptor normally applies to the most recent compilation started,
but as long as you are in the middle of parsing errors from one compilation
output buffer, you stay with that compilation output buffer.

Use next-error-descriptor in a compilation output buffer to switch to
processing errors from that compilation."

  (setq compilation-last-buffer (compilation-find-buffer))
  (compile-reinitialize-errors argp nil
			       ;; We want to pass a number here only if
			       ;; we got a numeric prefix arg, not just C-u.
			       (and (not (consp argp))
				    (if (< (prefix-numeric-value argp) 1)
					0
				      (1- (prefix-numeric-value argp)))))
  ;; Make ARGP nil if the prefix arg was just C-u,
  ;; since that means to reparse the errors, which the
  ;; compile-reinitialize-errors call just did.
  ;; Now we are only interested in a numeric prefix arg.
  (if (consp argp)
      (setq argp nil))
  (let (next-errors next-error)
    (catch 'no-next-error
      (save-excursion
	(set-buffer compilation-last-buffer)
	;; compilation-error-list points to the "current" error.
	(setq next-errors 
	      (if (> (prefix-numeric-value argp) 0)
		  (nthcdr (1- (prefix-numeric-value argp))
			  compilation-error-list)
		;; Zero or negative arg; we need to move back in the list.
		(let ((n (1- (prefix-numeric-value argp)))
		      (i 0)
		      (e compilation-old-error-list))
		  ;; See how many cdrs away the current error is from the start
		  (while (not (eq e compilation-error-list))
		    (setq i (1+ i)
			  e (cdr e)))
		  (if (> (- n) i)
		      (error "Moved back past first error")
		    (nthcdr (+ i n) compilation-old-error-list))))
	      next-error (car next-errors))
	(while
	    (progn
	      (if (null next-error)
		  (progn
		    (if argp (if (> (prefix-numeric-value argp) 0)
				 (error "Moved past last error")
			       (error "Moved back past first error")))
		    (compilation-forget-errors)
		    (throw 'no-next-error nil))
		(setq compilation-error-list (cdr next-errors))
		(if (null (cdr next-error))
		    ;; This error is boring.  Go to the next.
		    t
		  (or (markerp (cdr next-error))
		      ;; This error has a filename/lineno pair.
		      ;; Find the file and turn it into a marker.
		      (let* ((fileinfo (car (cdr next-error)))
			     (buffer (compilation-find-file (cdr fileinfo)
							    (car fileinfo)
							    (car next-error))))
			(if (null buffer)
			    ;; We can't find this error's file.
			    ;; Remove all errors in the same file.
			    (progn
			      (setq next-errors compilation-old-error-list)
			      (while next-errors
				(and (consp (cdr (car next-errors)))
				     (equal (car (cdr (car next-errors)))
					    fileinfo)
				     (progn
				       (set-marker (car (car next-errors)) nil)
				       (setcdr (car next-errors) nil)))
				(setq next-errors (cdr next-errors)))
			      ;; Look for the next error.
			      t)
			  ;; We found the file.  Get a marker for this error.
			  ;; compilation-old-error-list is a buffer-local
			  ;; variable, so we must be careful to extract its value
			  ;; before switching to the source file buffer.
			  (let ((errors compilation-old-error-list)
				(last-line (cdr (cdr next-error))))
			    (set-buffer buffer)
			    (save-excursion
			      (save-restriction
				(widen)
				(goto-line last-line)
				(beginning-of-line)
				(setcdr next-error (point-marker))
				;; Make all the other error messages referring
				;; to the same file have markers into the buffer.
				(while errors
				  (and (consp (cdr (car errors)))
				       (equal (car (cdr (car errors))) fileinfo)
				       (let ((this (cdr (cdr (car errors))))
					     (lines (- (cdr (cdr (car errors)))
						       last-line)))
					 (if (eq selective-display t)
					     (if (< lines 0)
						 (re-search-backward "[\n\C-m]"
								     nil 'end
								     (- lines))
					       (re-search-forward "[\n\C-m]"
								  nil 'end
								  lines))
					   (forward-line lines))
					 (setq last-line this)
					 (setcdr (car errors) (point-marker))))
				  (setq errors (cdr errors)))))))))
		  ;; If we didn't get a marker for this error,
		  ;; go on to the next one.
		  (not (markerp (cdr next-error))))))
	  (setq next-errors compilation-error-list
		next-error (car next-errors)))))

    (when next-error
      ;; Skip over multiple error messages for the same source location,
      ;; so the next C-x ` won't go to an error in the same place.
      (while (and compilation-error-list
		  (equal (cdr (car compilation-error-list)) (cdr next-error)))
	(setq compilation-error-list (cdr compilation-error-list)))

      ;; We now have a marker for the position of the error.
      next-error)))

;;; This should be provided by the compile library.
;;; Note that this is simply copied from the end of the next-error command.
(defun show-next-error (descriptor)
  "Position point at the given error DESCRIPTOR and show the error message in
the compilation buffer."
  (switch-to-buffer (marker-buffer (cdr descriptor)))
  (goto-char (cdr descriptor))
  ;; If narrowing got in the way of
  ;; going to the right place, widen.
  (or (= (point) (marker-position (cdr descriptor)))
      (progn
	(widen)
	(goto-char (cdr descriptor))))
  ;; Show compilation buffer in other window, scrolled to this error.
  (let* ((pop-up-windows t)
	 (w (display-buffer (marker-buffer (car descriptor)))))
    (set-window-point w (car descriptor))
    (set-window-start w (car descriptor))))



(defun global-replace-lines (&optional rescan)
  "Splice back to their original files the (possibly edited) output of grep.
This is a type of query-replace in which the original lines (in the
files) are replaced by those currently in the grep-output buffer.  You
will be asked to replace (y), don't replace (n), or quit (q) the
procedure.  When all changes are done in a file, you will be asked to
either save and kill the buffer, simply save it, or leave it as is.
In the replacement text, all ^M are changed to newlines, to allow a
single line to be replaced by multiple lines."
  (interactive "P")
  (let ((continue 'start)
	(loops -1)
	next-error
	(count 0)
	action
	buf b e
	next-buf
	replacement-line)

    (while continue
      (incf loops)
      (setq next-error (next-error-descriptor rescan))
      (setq rescan nil)			;We are not just starting anymore
      (if (or (eq continue 'exit) (null next-error))
	  (setq continue nil)
	(setq next-buf (marker-buffer (cdr next-error))))
      (if (null next-error)
	  (setq next-buf nil))
      (setq action
	    (if (or (eq continue 'start) (eq next-buf buf))
		"no"
	      (switch-to-buffer buf)
	      (query-string "Done with this buffer, dispose?"
			    '("kill (after saving)" "save" "no"))))
      (cond ((string= action "save")
	     (set-buffer buf)
	     (save-buffer nil))
	    ((string= action "kill (after saving)")
	     (set-buffer buf)
	     (save-buffer nil)
	     (kill-buffer buf))
	    ((string= action "no")
	     ;; Do nothing
	     )
	    )

      (when continue
	(setq continue t)		;We are not just starting anymore
	(show-next-error next-error)
	(setq buf (current-buffer))	;Where the grep hit occurred
	(setq action (query-character "Replace?" "ynq"))
	(cond ((eq action ?y)
	       (incf count)
	       (beginning-of-line) (setq b (point))
	       (end-of-line) (setq e (point))
	       (delete-region b e)
	       ;; Go to the error message
	       (set-buffer (marker-buffer (car next-error)))
	       (goto-char (car next-error))
	       (end-of-line) (setq e (point))
	       (beginning-of-line)
	       (skip-chars-forward "^:" e) (forward-char)
	       (skip-chars-forward "^:" e) (forward-char)
	       (setq b (point))
	       (narrow-to-region b e)
	       (replace-string "\r" "\n")
	       (widen)
	       (setq replacement-line (buffer-substring b e))
	       (set-buffer buf)
	       (insert replacement-line))
	      ((eq action ?n))
	      ((eq action ?q)
	       (setq continue 'exit))
	      )
	)
      )
	  
    (if (interactive-p)
	(message "%d replacements (of %d) done." count loops)
      count)
    )
  )


(defun global-replace (string replacement &optional rescan)
  "From a grep output buffer, query replace STRING by REPLACEMENT.
Prefix argument or optional RESCAN forces rescanning of the *compilation*
buffer.  See also global-replace-lines for a more flexible approach."
  (interactive "sGlobal replace: \nsGlobal replace %s by: \nP")
  (set-buffer (compilation-find-buffer))
  (if (not (string-match "grep" mode-name))
      (error "The last compilation was not a grep!"))
  (goto-char (point-min))
  (setq buffer-read-only nil)
  (replace-string string replacement)
  (if (interactive-p)
      (progn
	(setq prefix-arg rescan)
	(call-interactively 'global-replace-lines))
    (global-replace-lines rescan)))


(defun global-grep-and-replace (string replacement files grep-command)
  "Query replace STRING by REPLACEMENT in FILES, using GREP-COMMAND to find STRING.
global-replace-lines is used to perform the actual replacement.
Before that, however, the user is given a chance to edit the grep output."
  (interactive
   (let* ((s (read-string "Global replace: "))
	  (r (read-string (concat "Global replace " s " by: ")))
	  (f (read-string (concat "Searching for " s " in files: ")))
	  (l (read-string "grep command: " "grep -n")))
     (list s r f l)))
  (grep (concat grep-command " " string " " files))
  (let* ((status 'run)
	 (compilation-buffer (compilation-find-buffer))
	 (compilation-process (get-buffer-process compilation-buffer))
	 action)
    (while (eq status 'run)
      (message "running...") (sit-for 0)
      (sleep-for 1)
      (message "") (sit-for 0)
      (sleep-for 1)
      (setq status (process-status compilation-process)))
    (if (not (eq status 'exit))
	(error "Grep process exited abnormally"))
    (setq action
      (query-string "Do you want to"
		    '("replace" "edit search" "quit")))
    (cond ((string= action "replace")
	   (message "On with the replace!")
	   (sit-for 0)
	   (set-buffer compilation-buffer)
	   (goto-char (point-min))
	   (setq buffer-read-only nil)
	   (replace-string string replacement))
	  ((string= action "edit search")
	   (message "Entering recursive edit, exit with C-M-c, abort with C-]")
	   (recursive-edit))
	  ((string= action "quit")
	   (error "Aborted."))
	  )
    )
  (global-replace-lines))
