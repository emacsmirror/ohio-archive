;To: unix-emacs@bbn.com
;Date: 15 Feb 89 01:39:30 GMT
;From: Ashwin Ram <Ram-Ashwin@yale.ARPA>
;Subject: Re: Rmail modifications
;
;In article <817@wpi.wpi.edu>, john@wpi.wpi.edu (John F Stoffel) writes:
;> I'm havinbg a problem modifying RMAIL to use the '>' symbol when I'm
;> replying to a message and yanking it in to serve as reference.
;
;I think mail-yank-original ought to have a hook (say, mail-yank-hook) that
;would allow the user to do stuff like this.  Anyway, I use a variable to
;indicate the yank prefix (usually "   " by default, but you would setq it to
;"> " or whatever).  In the code below, I have also added mail-yank-hook for
;other customizations you might want to do.
;
;-----------------------------------------------------------------------------
;; Ashwin Ram, 2/14/89.

(defvar mail-yank-prefix "> "
   "String to prefix every line in a yanked message.")

(defun mail-yank-original (arg)
  "Insert the message being replied to, if any (in rmail).
Puts point before the text and mark after.
Prefix each line with mail-yank-prefix.
Just \\[universal-argument] as argument means don't indent
and don't delete any header fields."
  (interactive "P")
  (if mail-reply-buffer
      (let ((start (point)))
	(delete-windows-on mail-reply-buffer)
	(insert-buffer mail-reply-buffer)
	(if (consp arg)
	    nil
	  (mail-yank-clear-headers start (mark)))
	(exchange-point-and-mark)
        (delete-region (point) ; Remove trailing blank lines.
                       (progn (re-search-backward "[^ \^I\^L\n]")
                              (end-of-line)
                              (point)))
        (mail-prefix-region (mark) (point)) ; Highlight original message.
        (insert ?\n)
        (run-hooks 'mail-yank-hook))))

(defun mail-yank-clear-headers (start end)
   (save-excursion
      (goto-char start)
      (if (search-forward "\n\n" end t)
          (delete-region start (point)))))

(defun mail-prefix-region (beg end)
   "Prefix region with mail-yank-prefix."
   (interactive "r")
   (save-excursion
      (save-restriction
         (narrow-to-region beg end)
         (goto-char (point-min))
         (while (re-search-forward "^" (point-max) t)
            (replace-match mail-yank-prefix t t)))))
;-----------------------------------------------------------------------------
;
;Hope this helps.
;
;-- Ashwin.
;
;