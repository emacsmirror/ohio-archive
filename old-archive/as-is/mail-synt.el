;Return-Path: <think!compass!worley@eddie.mit.edu>
;Date: Thu, 1 Jun 89 17:28:55 EDT
;From: compass!worley@eddie.mit.edu (Dale Worley)
;To: info-gnu-emacs@eddie.mit.edu
;Subject: Modify syntax entry for % in mail-mode
;
;In mail-mode, the syntax of "%" is "word", unlike "!" and "@", which
;have syntax "punctuation".  This makes is hard to step through the
;components of an address with M-f, etc.  The following code creates a
;mail-mode-syntax-table which defines "%" to be "punctuation".

(defvar mail-mode-syntax-table nil
  "Syntax table used while in mail mode.")

(if (null mail-mode-syntax-table)
    (progn
     (setq mail-mode-syntax-table (copy-syntax-table text-mode-syntax-table))
     (modify-syntax-entry ?% ". " mail-mode-syntax-table)))

(defun mail-mode ()
  "Major mode for editing mail to be sent.
Like Text Mode but with these additional commands:
C-c C-s  mail-send (send the message)    C-c C-c  mail-send-and-exit
C-c C-f  move to a header field (and create it if there isn't):
	 C-c C-f C-t  move to To:	C-c C-f C-s  move to Subj:
	 C-c C-f C-b  move to BCC:	C-c C-f C-c  move to CC:
C-c C-w  mail-signature (insert ~/.signature at end).
C-c C-y  mail-yank-original (insert current message, in Rmail).
C-c C-q  mail-fill-yanked-message (fill what was yanked)."
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'mail-reply-buffer)
  (setq mail-reply-buffer nil)
  (set-syntax-table mail-mode-syntax-table)
  (use-local-map mail-mode-map)
  (setq local-abbrev-table text-mode-abbrev-table)
  (setq major-mode 'mail-mode)
  (setq mode-name "Mail")
  (setq buffer-offer-save t)
  (make-local-variable 'paragraph-separate)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^" mail-header-separator
				"$\\|^[ \t]*[-_][-_][-_]+$\\|"
				paragraph-start))
  (setq paragraph-separate (concat "^" mail-header-separator
				   "$\\|^[ \t]*[-_][-_][-_]+$\\|"
				   paragraph-separate))
  (run-hooks 'text-mode-hook 'mail-mode-hook))


;Dale Worley, Compass, Inc.                      worley@compass.com
;"The United States has entered an anti-intellectual phase in its
;history, perhaps most clearly seen in our virtually thought-free
;political life." -- David Baltimore

