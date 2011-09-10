;To: unix-emacs@bbn.com
;Date: 12 Apr 89 08:03:29 GMT
;From: David C Lawrence <csd4.milw.wisc.edu!leah!rpi!rpi.edu!tale@bbn.com>
;Sender: arpa-unix-emacs-request@bbn.com
;Subject: More on mail-yank.el
;Reply-To: tale@pawl.rpi.edu
;Source-Info:  From (or Sender) name not authenticated.
;
;When mailing or posting, I was tired of having to position myself at
;the end of a prefix for a quoted region, type C-x . and then fill the
;paragraph.  Most of my problem was just taking the time to move to the
;end of the proper fill-prefix in the beginning and later forgetting to
;cancel it.  It was an annoyance.
;
;However, it was mechanical.  My actions were always the same
;(including forgetting to cancel the prefix) so it was a good thing to
;let Emacs do by itself and let (no pun intended) it remember to pop the
;fill-prefix back for me.  Taking a function which Bruce Israel wrote
;to automatically find the fill-prefix for a paragraph before filling,
;I modified it to go with the rest of mail-yank.el.  The following is
;in my .emacs:
;
;  (setq mail-mode-hook '(lambda ()
;                          (require 'mail-yank)
;                          (local-set-key "\eq" 'fill-quoted-paragraph))
;        news-reply-mode-hook mail-mode-hook)
;
;If no fill-prefix is set when it is invoked, fill-quoted-paragraph
;attempts to deduce the fill-prefix, first from the mail-cited-line-re
;and, if that fails, from the beginning of the paragraph to the first
;alphanumeric character.  If what it decides is the fill-prefix does
;not indeed prefix every line in the paragraph, it does fill-paragraph
;with a nil fill-prefix.  It does have a side-effect of removing any
;tabs from the paragraph; I consider it a feature (I hate hard C-i's)
;but others might consider it a bug.
;
;I find it a useful addition to mail-yank and mail-yank is a useful
;modification to mail and news-reply modes.  Since the mail-yank.el
;file is short, it is included in full here.
;
;Hope someone finds this useful ...
;--- Cut  (and at the bottom) ---
;;; Copyright (C) 1989 by Free Software Foundation, Ashwin Ram, Martin
;;; Neitzel.  The standard GNU General Public License applies.  Even if
;;; it is called something different this week...

;;; History
;;; 11-Apr-89  Emacs Maint Acct (emacs@pawl.rpi.edu)
;;;	Put in fill-quoted-paragraph to automatically find
;;;	mail-cited-line-re.  If no mail-cited-line-re is found,
;;;     fill-quoted-paragraph attempts to make a semi-intelligent
;;;     guess about what the fill-prefix is.
;;;	
;;;  5-Apr-89  Emacs Maint Acct (emacs@pawl.rpi.edu)
;;;	Added mail-quote-string for a little more flexibility among the
;;;	teeming masses.  In particular, my boss likes to use "} "
;;;	rather than "> " so this lets him have it easily.
;;;	

; Make sure that we re-define the original mail-yank-original
(provide 'mail-yank)
(require 'sendmail)

(defvar mail-quote-string "> "
  "*String to be inserted before quoted regions of mail.  When mail-yank is 
called interactively, this string is appended to the author's shortname or 
initials provided at the prompt.")

(defvar mail-cited-line-re "[^ ]*[]>})<] ?"
  "*Regexp that tells how the beginning of an already cited line in a
mail or news article looks like.  Used in mail-cite-region to prevent
nestings of already marked citations.  This regexp is only used at the
beginning of a line, so it needn't begin with a '^'.")

(defun mail-yank-original (arg)
  "Insert the message being replied to, if any (in rmail).
Puts point before the text and mark after.
Just \\[universal-argument] as argument means don't delete any header fields."
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
        (insert ?\n)
	(call-interactively 'mail-cite-region)
        (run-hooks 'mail-yank-hook))))

(defun mail-cite-region (beg end authorname)
  "Prefix region with a citation mark, prevententing nested citations."
  (interactive "r\nsShort name or initials of Author? ")
  (setq authorname (concat authorname mail-quote-string))
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char beg)
      (while (not (eobp))
	(if (and (not (looking-at mail-cited-line-re))
                 (not (looking-at "^$")))
            (insert authorname))
	(forward-line 1)))))

;; based on Bruce Israel's "fill-paragraph-properly"
(defun fill-quoted-paragraph (arg)
  "Fill paragraph containing or following point, automatically finding
the mail-cited-line-re and using it as the prefix.  If the mail-cited=
line-re is not in the first line of the paragraph, it makes a guess at
what the fill-prefix for the paragraph should be by looking at the first
line and taking anything up to the first alphanumeric character.

Prefix arg means justify both sides of paragraph as well.

This function just does fill-paragraph if a) fill-prefix is set, or
b) what it deduces to be the paragraph prefix (based on the first line)
does not precede each line in the region."
  (interactive "P")
  (save-excursion
    (forward-paragraph)
    (or (bolp) (newline 1))
    (let ((end (point)) st (fill-prefix fill-prefix))
      (backward-paragraph)
      (if (looking-at "\n") (forward-char 1))
      (setq st (point))
      (if fill-prefix nil
	(untabify st end) ;; die, scurvy tabs!
        ;; untabify might have made the paragraph longer character-wise,
        ;; make sure end reflects the correct location of eop.
        (forward-paragraph) (setq end (point))
        (goto-char st)
        (if (looking-at mail-cited-line-re)
            (setq fill-prefix (buffer-substring
                               st (progn (re-search-forward mail-cited-line-re)
                                         (point))))
          ;; this regexp is is convenient because paragraphs quoted by simple
          ;; indentation must still yield to us  <evil laugh>
          (while (looking-at "[^a-zA-Z0-9]") (forward-char 1))
          (setq fill-prefix (buffer-substring st (point))))
        (next-line 1) (beginning-of-line)
        (while (and (< (point) end) (not (string-equal fill-prefix "")))
          ;; if what we decided was the fill-prefix does not precede all
          ;; of the lines in the paragraph, we probably goofed.  Cancel it.
          (if (looking-at (regexp-quote fill-prefix)) () (setq fill-prefix ""))
          (next-line 1) (beginning-of-line)))
      (fill-region-as-paragraph st end arg))))
;-- "You have unpacked the archive." :-) --
;--
;      tale@rpitsmts.bitnet, tale%mts@itsgw.rpi.edu, tale@pawl.rpi.edu

