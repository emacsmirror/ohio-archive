;Date: Sat, 7 Jan 89 16:24:25 EST
;To: bpa!cbmvax!vu-vlsi!mpx1!mpx2!erik@rutgers.edu
;From: MAP@lcs.mit.edu
;Cc: info-gnu-emacs@prep.ai.mit.edu
;Subject: nice addition to dired
;
;I thought your added dired command was a nice idea.  Here's a slightly
;improved version that doesn't lose pending deletions if there are any.

;;; dired-exit hook stuff... 1/6/89 erik@mpx2.UUCP
;; 7-Jan-89  MAP@LCS.MIT.Edu do pending deletions before punting
(setq dired-mode-hook 'set-my-dired-keys-hook)

(defun set-my-dired-keys-hook ()
  "My favorite dired vars."
  (local-set-key "q" 'dired-delete-and-exit))

;;; command to quit a dired session...
(defun dired-delete-and-exit ()
  "Quit editing this directory."
  (interactive)
  (dired-do-deletions)
  (kill-buffer (current-buffer)))

