Date: 5 Dec 87 01:00:46 GMT
From: Conor Rafferty <goose!conor@LABREA.STANFORD.EDU>
Organization: Integrated Circuits Lab. Stanford University
Subject: shell-util.el

Due to brain leakage, I posted a shell utility package to
comp.windows.x instead of comp.emacs. Here's a duplicate of the description;
the lisp code can be found over there.


A new version of gnumacs just came up for me with a minimum of bother.
To express my gratitude :) I have collected a few little hacks I use
to make interaction with subshells more convenient. Particularly subshells
containing dbx. The first hack, called shell-repeat, takes an expression like
    print pt[nd[0:2]->pt]->cord
and turns it into
    print pt[nd[0]->pt]->cord; print pt[nd[1]->pt]->cord; print pt[nd[2]->pt]->cord

Vi lovers have said it's almost enough to convert them :-)

The second hack is called just-like-csh and does what you would expect.
!!	repeats the previous command
!a	repeats the previous command starting with an a
!?abc	repeats the previous command containing abc

just-like-csh relies on shell and telnet windows copying user input
lines into a buffer called *history*. That requires a minor change
in both shell.el and telnet.el.

It has already been pointed out that just-like-csh is a misnomer,
since it doesn't try to get !$ or !-2. Call it sorta-kinda-like-csh if
you like :)

---
             conor rafferty    The command
  conor@sierra.stanford.edu       1,$s/^\([^,]*\), *\(.*\)/\2 \1/
decwrl!glacier!conor@sierra    although hard to read, does the job.
                           --- Brian W. Kernighan "Advanced Editing on Unix"
A new version of gnumacs just came up for me with a minimum of bother.
To express my gratitude :) I have collected a few little hacks I use
to make interaction with subshells more convenient. Particularly subshells
containing dbx. The first hack, called shell-repeat, takes an expression like
    print pt[nd[0:2]->pt]->cord
and turns it into
    print pt[nd[0]->pt]->cord; print pt[nd[1]->pt]->cord; print pt[nd[2]->pt]->cord

Vi lovers have said it's almost enough to convert them :-)

The second hack is called just-like-csh and does what you would expect.
!!	repeats the previous command
!a	repeats the previous command starting with an a
!?abc	repeats the previous command containing abc

just-like-csh relies on shell and telnet windows copying user input
lines into a buffer called *history*. That requires a minor change
in both shell.el and telnet.el.

------------------------------Changes to shell.el------------------------------
*** /u2/conor/bin/shell.el	Thu Dec  3 22:30:14 1987
--- /u2/conor/src/emacs/lisp/shell.el	Fri Aug 14 14:55:08 1987
***************
*** 216,228 ****
  	  (goto-char last-input-start)
  	  (shell-set-directory))
        (error (funcall shell-set-directory-error-hook)))
!   (let ((process (get-buffer-process (current-buffer))) command-copy) ;csr{
      (process-send-region process last-input-start last-input-end)
!     (set-marker (process-mark process) (point))
!     (setq command-copy (buffer-substring last-input-start last-input-end))
!     (set-buffer (get-buffer-create "*history*"))
!     (end-of-buffer)
!     (insert command-copy)))		;csr}
  ;;;  If this code changes (shell-send-input and shell-set-directory),
  ;;;  the customization tutorial in
  ;;;  info/customizing-tutorial must also change, since it explains this
--- 216,225 ----
  	  (goto-char last-input-start)
  	  (shell-set-directory))
        (error (funcall shell-set-directory-error-hook)))
!   (let ((process (get-buffer-process (current-buffer))))
      (process-send-region process last-input-start last-input-end)
!     (set-marker (process-mark process) (point))))
! 
  ;;;  If this code changes (shell-send-input and shell-set-directory),
  ;;;  the customization tutorial in
  ;;;  info/customizing-tutorial must also change, since it explains this


------------------------------Changes to telnet.el------------------------------
*** /u2/conor/bin/telnet.el	Thu Dec  3 22:31:00 1987
--- /u2/conor/src/emacs/lisp/telnet.el	Tue Sep  1 10:28:14 1987
***************
*** 24,31 ****
  ;;to eval current expression.  Also to try to send escape keys correctly.
  ;;essentially we'll want the rubout-handler off.
  
- (provide 'telnet)
- 
  (defvar telnet-new-line "\r")
  (defvar telnet-mode-map nil)
  (defvar telnet-prompt-pattern "^[^#$%>]*[#$%>] *")
--- 24,29 ----
***************
*** 97,106 ****
  		 (t (setq telnet-count (1+ telnet-count)))))))
  
  (defun telnet-filter (proc string)
-   (let ((move-point-p))			;csr to avoid underfoot movement
    (save-excursion
      (set-buffer (process-buffer proc))
-     (if (eq (point) (point-max)) (setq move-point-p t))	;csr
      (goto-char (point-max))
      (let ((now (point)))
        (insert string)
--- 95,102 ----
***************
*** 112,122 ****
  	     (marker-position last-input-start)
  	     telnet-remote-echoes)
  	(delete-region last-input-start last-input-end)))
!   (if (and				;csr
! 	move-point-p			;csr
! 	(eq (process-buffer proc)
! 	    (current-buffer)))
!       (goto-char (point-max)))))
  
  (defun delete-char-or-send-eof (arg killp)
    "At end of buffer, send eof to subshell.  Otherwise delete character."
--- 108,116 ----
  	     (marker-position last-input-start)
  	     telnet-remote-echoes)
  	(delete-region last-input-start last-input-end)))
!   (if (eq (process-buffer proc)
! 	  (current-buffer))
!       (goto-char (point-max))))
  
  (defun delete-char-or-send-eof (arg killp)
    "At end of buffer, send eof to subshell.  Otherwise delete character."
***************
*** 150,164 ****
        (move-marker last-input-end (point))))
    (save-excursion
      (goto-char last-input-start)
!     (shell-set-directory)
!     (let ((process (get-buffer-process (current-buffer))) copy-command) ;csr{
        (send-region process last-input-start last-input-end)
        (if (not copied) (send-string process telnet-new-line))
!       (set-marker (process-mark process) (point))
!       (setq copy-command (buffer-substring last-input-start last-input-end))
!       (set-buffer (get-buffer-create "*history*"))
!       (goto-char (point-max))
!       (insert copy-command "\n"))))) ;csr}
  
  (defun telnet (arg)
    "Open a network login connection to host named HOST (a string).
--- 144,153 ----
        (move-marker last-input-end (point))))
    (save-excursion
      (goto-char last-input-start)
!     (let ((process (get-buffer-process (current-buffer))))
        (send-region process last-input-start last-input-end)
        (if (not copied) (send-string process telnet-new-line))
!       (set-marker (process-mark process) (point))))))
  
  (defun telnet (arg)
    "Open a network login connection to host named HOST (a string).

------------------------------shell-util.el------------------------------
; load this explicitly
(require 'shell)
(require 'telnet)
(require 'blisp)



;----------
;Extra history stuff
;----------

(defvar last-shell nil "shell buffer to copy input to")
(defvar history-map nil "map inside history buffer")

;define the history buffer
(save-excursion
  (set-buffer (get-buffer-create "*history*"))
  (if (not history-map) (setq history-map (make-sparse-keymap)))
  (define-key history-map "\C-m" 'copy-history-to-shell)
  (use-local-map history-map)
)

;how you go back to shells
(defun copy-history-to-shell ()
  (interactive)
  (let ((line (buffer-line)))
    (or last-shell (error "no last shell defined?!"))
    (switch-to-window last-shell)
    (goto-char (point-max))
    (insert line)))

;define how you get there from shells
(define-key shell-mode-map "\e!" 'switch-to-history)
(define-key shell-mode-map "!" 'just-like-csh)
(define-key telnet-mode-map "\e!" 'switch-to-history)
(define-key telnet-mode-map "!" 'just-like-csh)

(defun switch-to-history ()
  (interactive)
  (setq last-shell (current-buffer))
  (switch-to-buffer "*history*"))


(defun just-like-csh ()
  (interactive)
  (setq last-shell (current-buffer))
  (let ((s (read-string "Repeat: ")))
    (set-buffer "*history*")
    (save-excursion
      (goto-char (point-max))
      (cond
	((or (zerop (length s)) (equal (aref s 0) ?!))
	 (re-search-backward "^."))
	((equal (aref s 0) ??)
	 (search-backward (substring s 1)))
	(t
	  (re-search-backward (concat "^" (regexp-quote s)))))
      (copy-history-to-shell))))


;----------
;random
;----------
(define-key shell-mode-map "\C-x9" 'shell-repeat)
(define-key telnet-mode-map "\C-x9" 'shell-repeat)

(defun shell-repeat ()
"Expands arithmetic loops. ``p pt[nd[0:2]->pt]'' becomes
``p pt[nd[0]->pt]->cord;p pt[nd[1]->pt]->cord;p pt[nd[2]->pt]->cord;''
This makes unprogrammable debuggers like dbx a little more tolerable."
  (interactive)
  (let (start-user end-user line count lo hi lsp last-insert)
    (if (not (eobp))
	(message "Only works at end of buffer")
      (setq start-user (process-mark (get-buffer-process (current-buffer))))
      (goto-char start-user)
      ;Someday, add , in the expansion
      (re-search-forward "\\([0-9]+\\):\\([0-9]+\\)")
      (setq lo (buffer-substring (match-beginning 1) (match-end 1)))
      (setq hi (buffer-substring (match-beginning 2) (match-end 2)))
      (setq lo (string-to-int lo))
      (setq hi (string-to-int hi))
      (if (< hi lo) (error "first number must be less than second"))
      (end-of-line)
      (setq end-user (point))
      (setq line (buffer-substring start-user end-user))
      (kill-region start-user end-user)

      ;Get exciting screen action because replace-regexp calls push-mark
      (setq count lo)
      (setq lsp start-user)
      (while (<= count hi)
	(goto-char (point-max));
	(setq last-insert (point))

	; Hack to avoid sending > 200 chars per line to send-region
	(if (> (- last-insert lsp) 200) (progn (insert "\n") (setq lsp last-insert)))
	(insert line)
	(goto-char last-insert)
	(replace-regexp "[0-9]+:[0-9]+" (int-to-string count))
	(goto-char (point-max))
	(insert ";")
	(setq count (+ count 1)))
      (set-buffer "*history*")
      (end-of-buffer)
      (insert line "\n"))))

------------------------------blisp.el------------------------------
; some basic lisp functions
; I repeat again and again.
(provide 'blisp)

(defun lose-buffer (&optional bufname)
  "Do what bury-buffer only promises to do"
  (interactive)
  (if (not bufname)
      (setq bufname (current-buffer)))
  (bury-buffer bufname)
  (switch-to-buffer (other-buffer (current-buffer))))

(defun switch-to-window (name)
  "select the window of the named BUFFER. If none, get one."
  (let* ((bbb (if (bufferp name) name (get-buffer name)))
	 (www (get-buffer-window bbb)))
    (if www
	(progn
	  (select-window www)
	  (set-buffer bbb))	  ; looks but isn't redundant
      (switch-to-buffer bbb))))

		  
(defun buffer-line ()
  (save-excursion
    (buffer-substring (progn (beginning-of-line) (point))
		      (progn (end-of-line) (point)))))
