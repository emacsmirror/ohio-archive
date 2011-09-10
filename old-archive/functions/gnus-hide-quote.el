;From: jwz@lucid.com (Jamie Zawinski)
;Subject: new gnus-hide-quote.el
;Date: 29 Jan 91 23:18:00 GMT
;Organization: Lucid, Inc., Menlo Park, CA
;
;Tim Lambert posted a version of this back in december; I've improved it to
;automagically cope with citation styles other than quoted-lines-begin-with-">".
;I've been using this for a couple of days, and haven't run across any messages
;that it doesn't Do the Right Thing for, even SuperCited ones (but even if it
;did guess wrong, you can still give it a prefix argument and it will prompt 
;you for the citation prefix.)
;
;Let me know if you stumble across any messages for which it picks an incorrect
;citation string.
;					-- Jamie <jwz@lucid.com>

;;; -*- Mode:Emacs-Lisp -*-

;;; Don't you hate it when an article has pages of quoted text that you've
;;; already read?  Load this file and you can just type 'h' to get rid of
;;; it and 'H' to bring it back.
;;;
;;; If someone uses something other than ">" to mark the quoted text, it
;;; notices that; it can even cope with the kind of stuff that SuperCite
;;; inserts, and simple indentation (though it tries that as a last resort).
;;; If it can't figure out what the attribution string is, it prompts for it.
;;; 'C-uh' will make it prompt anyway, with it's guess as a default.  If a
;;; SuperCited article has multiple sections like
;;;
;;;       FOO> some text, some text
;;;       FOO> blah blah blah
;;;       oh yeah?
;;;       BAR> some text, some text
;;;       BAR> blah blah blah
;;;
;;; you can generally make both sections go away just by typing 'h' twice.
;;; Also, if two blocks of text to be elided are seperated only by blank
;;; lines, the blank lines are hidden as well.
;;;
;;;  14-dec-90	Tim Lambert <lambert@spectrum.cs.unsw.oz.au>
;;;		Created.
;;;  27-jan-91	Jamie Zawinski <jwz@lucid.com>
;;;		Made it automatic.

(require 'gnus)

(defun gnus-hide-quote-internal (prefix)
  (let ((search-pattern (concat "\n+" prefix))
	(looking-at-pattern (concat "^" prefix)))
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(and (search-forward "\n\n" nil t)
	     (forward-char -1))
	(while (re-search-forward search-pattern nil t)
	  ;; go forward one line, so that exactly one line of each
	  ;; elided block is visible, to give a little bit of context.
	  (forward-line 1)
	  (while (looking-at looking-at-pattern)
	    (delete-char -1)
	    (insert "\^M")
	    (forward-line 1)))))))

(defconst gnus-possible-quote-prefixes
    '("^[^ \t\n\(A-Z#%;]"	;; first, search for ">", "}", etc.
      "^[ \t]+[^ \t\n\(A-Z#%;]"	;; then that with leading whitespace.
				;; these don't use #%; because of shar files
      				;; and postscript and lisp code...
      "^[ \t]*[A-Z]+[]}>[{<-]"  ;; then, SuperCite: "FOO> ", "  Yow>", etc.
      "^[ \t]+"			;; then, simple indentation.
      )
  "Regexps to search for to identify quoted-text attributions.
These regexps should match the initial subsequence of the line that is the
attribution prefix.  They are ordered; regexps which are less ambiguous and 
less likely to produce mismatches should come first.  The entire buffer will 
be searched for two or more consecutive lines which match the first element 
of this list, then the second, and so on.  The initial subsequence of the 
two lines which first match is returned.")

(defun gnus-identify-quote-prefix ()
  "Figure out what the current message uses for attribution.  See the
documentation for gnus-possible-quote-prefixes."
  (save-excursion
    (goto-char (point-min))
    (search-forward "\n\n" nil t)
    (let ((match nil)
	  (start (point))
	  (rest gnus-possible-quote-prefixes))
      (while (and rest (not match))
	(goto-char start)
	(let ((regexp (car rest)))
	  (while (not (or match (eobp)))
	    (if (re-search-forward regexp nil 0)
		(progn
		  (beginning-of-line)
		  (let ((prefix (buffer-substring (point) (match-end 0))))
		    (forward-line 1)
		    (if (looking-at (regexp-quote prefix))
			(setq match prefix)
			(forward-line -1)))))
	    (forward-line 1)))
	(setq rest (cdr rest)))
      match)))

(defun gnus-Subject-hide-quote (&optional prefix-string)
  "Hide quotations in current article."
  (interactive (list
		 (let* ((default (gnus-eval-in-buffer-window gnus-Article-buffer
				   (gnus-identify-quote-prefix)))
			(string (if (or current-prefix-arg (not default))
				    (read-from-minibuffer
				      (concat
					"String that starts quotation lines"
					(if default
					    (concat " \(default \"" default "\"\)"))
					": "))
				    default)))
		   (if (string= "" string)
		       (or default (error "You tell me, buckaroo."))
		       string))))
  (if (string= prefix-string "") (error "empty string"))
  (message "Hiding text beginning with \"%s\"..." prefix-string)
  (gnus-eval-in-buffer-window gnus-Article-buffer
    (save-excursion
      (goto-char (point-min))
      (let ((buffer-read-only nil)
	    (quote-regexp (concat "\n*" (regexp-quote prefix-string))))
	(gnus-hide-quote-internal quote-regexp)
	(set-buffer-modified-p nil))
      (setq selective-display t)))
  (message "Hiding text beginning with \"%s\"... done." prefix-string))
      
(defun gnus-Subject-show-quote ()
  "Show quotations in curent article."
  (interactive)
  (gnus-eval-in-buffer-window gnus-Article-buffer
    (save-excursion
      (goto-char (point-min))
      (let ((buffer-read-only nil))
	(while (search-forward "\C-m" nil t)
	  (delete-char -1)
	  (insert "\n")))
      (set-buffer-modified-p nil))))

(define-key gnus-Subject-mode-map "h" 'gnus-Subject-hide-quote)
(define-key gnus-Subject-mode-map "H" 'gnus-Subject-show-quote)

