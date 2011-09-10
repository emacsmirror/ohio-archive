;To: unix-emacs@bbn.com
;Date: 11 Feb 89 00:48:31 GMT
;From: sor.inria.fr!shapiro@eddie.mit.edu
;Subject: Useful addition to mail-mode
;
;Below you will find a useful addition to mail sending (mail-mode,
;actually mailalias.el) which I offer to the community.  The result is
;demonstrated in the header of this message.  Typing C-c C-x (as in
;"eXpand") expands the mail aliases in the To, CC, and Bcc fields of
;the mail header, and you see the result.
;
;I hereby waive all my copyright rights, which I give  over to the Free
;Software Foundation.
;
;There are probably more efficient implementations, but this one was
;very easy, and it works.
;
;						Marc Shapiro
;
;INRIA, B.P. 105, 78153 Le Chesnay Cedex, France.  Tel.: +33 (1) 39-63-53-25
;e-mail: shapiro@sor.inria.fr or: ...!mcvax!inria!shapiro
;
;
;---- Here comes file ``~emacs/lisp/mailalias-local.el'' --------------------------
;;; shapiro@sor.inria.fr 3-feb-89

(defun mail-show-expanded-aliases (arg)
  "Expand mail aliases in header.  With arg, re-read .mailrc first."
  (interactive "P")
  (when arg
      (mail-re-read-mailrc))
  (let ((case-fold-search t)
	(end)
	(beg)
	(end1))
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search nil))
	(re-search-forward
	 (concat "^" (regexp-quote mail-header-separator) "\n")))
      (backward-char 1)
      (beginning-of-line)
      (setq end (point-marker))
      (if mail-aliases
	  (expand-mail-aliases (point-min) end))
      (goto-char (point-min))
      (save-restriction
	(while (re-search-forward "^\\(to\\|cc\\|bcc\\):" end t)
	  (narrow-to-region (point)
			    (progn
			      (re-search-forward "^[^ \t]" end 'move)
			      (point)))
	  (goto-char (point-min))
	  (replace-regexp "[ \t]*,[ \t\n]*" ",\n\t")
	  (goto-char (point-min))
	  (widen))))))	  

(defun mail-re-read-mailrc ()
  "Re-read .mailrc for alias expansions"
  (interactive)
  (setq mail-aliases t))

(provide 'mailalias-local)
(define-key mail-mode-map "\C-c\C-x" 'mail-show-expanded-aliases)
(define-key mail-mode-map "\C-c." 'mail-re-read-mailrc)

;---- End of file ``~emacs/lisp/mailalias-local.el'' ------------------------------

