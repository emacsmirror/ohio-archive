;Return-Path: <@relay.cs.net:davis@scr.slb.com>
;Date: Mon, 24 Apr 89 11:33 EST
;From: Paul Davis <davis@scr.slb.com>
;Subject: generic completion functions/function expansion
;To: INFO-GNU-EMACS@prep.ai.mit.edu
;X-Vms-To: prep.ai.mit.edu::info-gnu-emacs
;
;
;
;elisp part 3: a generic completion function (complete-token)why isn't
;this in Emacs already ? Also, some stuff to do expansions that show
;a docstring if the token is already complete (complete-or-expand-token).
;
;I use this in a `hyper-C' mode: I can get help on all the Unix calls,
;and all the standard library functions by just typing part of the
;name, completing and then trying to complete again: this pops up the
;declaration next to the function, and a short docstring in the minibuffer.
;I bind complete-or-expand-token to M-TAB in C-mode, and various others too.
;
;It may be useless ... especially without the docstring file - that
;comes later.
;
;Paul
;
;-- cut here ---
;; Generic completion/expansion functions
;; Paul Davis <davis%scr.slb.com@relay.cs.net>, May 1988

;; Inspired by DEC's Language Sensitive Editor and
;; Emacs' lisp completion


(defun complete-token (completion-list)
  "Complete token before point using COMPLETION-LIST. Inserts as many
characters as possible, and then if multiple completions, display them
in a *Completions* buffer."
    (let* ((end (point))
	 (beg (save-excursion
		(backward-sexp 1)
		(point)))
	 (pattern (buffer-substring beg end))
	 (completion (try-completion pattern completion-list)))
    (cond ((eq completion t)
	   (expand-token pattern completion-list))
	  ((null completion)
	   (message "Can't find completion for \"%s\"" pattern)
	   (ding))
	  ((not (string= pattern completion))
	   (delete-region beg end)
	   (insert completion))
	  (t
	   (message "Making completion list...")
	   (let ((list (all-completions pattern completion-list)))
	     (with-output-to-temp-buffer "*Completions*"
	       (display-completion-list list))
	   (message "Making completion list...%s" "done"))))))

(defun complete-or-expand-token (completion-list)
  "Complete token before point using COMPLETION-LIST, or if complete
expand to show generic declaration, and display synopsis in the echo area"
    (let* ((end (point))
	 (beg (save-excursion
		(backward-sexp 1)
		(point)))
	 (pattern (buffer-substring beg end))
	 (completion (try-completion pattern completion-list)))
    (cond ((eq completion t)
	   (expand-token pattern completion-list))
	  ((null completion)
	   (message "Can't find completion for \"%s\"" pattern)
	   (ding))
	  ((not (string= pattern completion))
	   (delete-region beg end)
	   (insert completion))
	  (t
	   (message "Making completion list...")
	   (let ((list (all-completions pattern completion-list)))
	     (with-output-to-temp-buffer "*Completions*"
	       (display-completion-list list))
	   (message "Making completion list...%s" "done"))))))

(defun expand-token  (completion completion-list)
  "\
Expand COMPLETION using COMPLETION-LIST to (momentarily) show generic 
declaration, and display synopsis in echo area. COMPLETION must be 
complete, and COMPLETION-LIST is a list of elements of the form

    \(\"completion\" . \(\"declaration\" . \"synopsis\"\)\)\n"
   (cond
   ((null completion-list)
    (message "no expansion found for %s" completion))
   ((string-equal completion (car (car completion-list)))
    (momentary-string-display 
     (concat " " (car (cdr (car completion-list)))) (point) ?\ 
     (cdr (cdr (car completion-list)))))
   (t
    (expand-token completion (cdr completion-list)))))

(provide 'completion)

