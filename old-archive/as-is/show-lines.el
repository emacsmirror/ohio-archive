;Date: Mon 26 Dec 88 18:05:26-MST
;From: "Nelson H.F. Beebe" <Beebe@science.utah.edu>
;Subject: Displaying line numbers in a buffer
;To: info-gnu-emacs@prep.ai.mit.edu
;Cc: Beebe@science.utah.edu
;
;In response to a recent request on this newsgroup, here is
;some code I wrote to add (and remove) line numbers in a
;buffer; it is public domain, so do what you like with it:

(defun show-line-numbers () 
  "Show line numbers in a copy of the current buffer."
  (interactive)
  (let ((n 0))
    (goto-char (point-min))
    (while (< (point) (point-max))
      (setq n (1+ n))
      (insert (format "%5d:\t" n))
      (forward-line 1)))
  (toggle-read-only)
  )

(defun remove-line-numbers ()
  "Remove line numbers installed by show-line-numbers."
  (interactive)
  (toggle-read-only)
  (save-excursion
    (goto-char (point-min))      
    ;; use loop rather than replace-regexp so M-x undo can undo it
    (while (re-search-forward "^[ 0-9][ 0-9][ 0-9][ 0-9][ 0-9]:\t" 
			      (point-max) t)
      (replace-match "" t t))))
;-------
