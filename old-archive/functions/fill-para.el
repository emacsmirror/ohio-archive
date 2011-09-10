;From utkcs2!emory!samsung!uunet!mcsun!ukc!edcastle!aipna!rjc Thu Jun 21 11:36:02 EDT 1990
;Article 3027 of gnu.emacs:
;Path: utkcs2!emory!samsung!uunet!mcsun!ukc!edcastle!aipna!rjc
;>From: rjc@uk.ac.ed.cstr (Richard Caley)
;Newsgroups: gnu.emacs
;Subject: syntax tables (was: regexp question: search for words)
;Message-ID: <RJC.90Jun20040929@brodie.uk.ac.ed.cstr>
;Date: 20 Jun 90 04:09:29 GMT
;References: <3017@isaak.isa.de> <1990Jun19.143251.28944@talos.pm.com>
;Sender: news@aipna.ed.ac.uk
;Organization: Center for Speech Technology Research
;Lines: 108
;In-reply-to: kjones@talos.pm.com's message of 19 Jun 90 14:32:51 GMT
;
;
;To go off at something of a tangent...
;
;I've been thinking for a while that emacs underuses the ysntax tables.
;The specific example which brought it to mind was my wanting to use
;M-q to format a paragraph of text which was written in a strange mode
;where word boundries are indicated by `|` ( don't ask ). I declared
;`|' as a space and ` ' as a word character which got forward/backward
;word working, but M-q still broke lines at spaces.
;
;Anyway, enough motivation, now onto the soapbox--
;
;	Any elisp code which looks for a space should look for a
;	character with syntax class ` ' rather than for a literal
;	space. By extension, the same holds for looking for
;	alphanumerics. 
;
;Discuss; use both sides of the paper.
;
;-----------	
;
;Appendix A: Code for filling based on syntax class.
;
;This seems to work, but I have not leant on it very hard. Certainly it
;is almost always in my emacs and M-q still works fine in text mode
;etc. 

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;                                                                  ;;
 ;; A version of fill-region-as-paragraph which uses some syntax     ;;
 ;; information. It should be upward compatable with the standard    ;;
 ;; one. We define this here so that M-q can be used to justify      ;;
 ;; transcription files.                                             ;;
 ;;                                                                  ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fill-region-as-paragraph (from to &optional justify-flag)
  "Fill region as one paragraph: break lines to fit fill-column.
Prefix arg means justify too.
From program, pass args FROM, TO and JUSTIFY-FLAG."
  (interactive "r\nP")
  (save-restriction
    (narrow-to-region from to)
    (goto-char (point-min))
    (skip-chars-forward "\n")
    (narrow-to-region (point) (point-max))
    (setq from (point))
    (let ((fpre (and fill-prefix (not (equal fill-prefix ""))
		     (regexp-quote fill-prefix))))
      ;; Delete the fill prefix from every line except the first.
      ;; The first line may not even have a fill prefix.
      (and fpre
	   (progn
	     (if (>= (length fill-prefix) fill-column)
		 (error "fill-prefix too long for specified width"))
	     (goto-char (point-min))
	     (forward-line 1)
	     (while (not (eobp))
	       (if (looking-at fpre)
		   (delete-region (point) (match-end 0)))
	       (forward-line 1))
	     (goto-char (point-min))
	     (and (looking-at fpre) (forward-char (length fill-prefix)))
	     (setq from (point)))))
    ;; from is now before the text to fill,
    ;; but after any fill prefix on the first line.

    ;; Make sure sentences ending at end of line get an extra space.
    (goto-char from)
    (while (re-search-forward "[.?!][])""']*$" nil t)
      (insert ? ))
    ;; The change all newlines to spaces.
    (subst-char-in-region from (point-max) ?\n ?\ )
    ;; Flush excess spaces, except in the paragraph indentation.
    (goto-char from)
    (skip-chars-forward " \t")
    (while (re-search-forward "   *" nil t)
      (delete-region
       (+ (match-beginning 0)
	  (if (save-excursion
	       (skip-chars-backward " ])\"'")
	       (memq (preceding-char) '(?. ?? ?!)))
	      2 1))
       (match-end 0)))
    (goto-char (point-max))
    (delete-horizontal-space)
    (insert "  ")
    (goto-char (point-min))
    (let ((prefixcol 0))
      (while (not (eobp))
	(move-to-column (1+ fill-column))
	(if (eobp)
	    nil
	    (re-search-backward "\\s \\|\\s(\\|\\s)\\|\\s$" )
	  (if (if (zerop prefixcol) (bolp) (>= prefixcol (current-column)))
	      (re-search-forward "\\s \\|\\s(\\|\\s)\\|\\s$" )
	    (forward-char 1)))
	(delete-horizontal-space)
	(insert ?\n)
	(and (not (eobp)) fill-prefix (not (equal fill-prefix ""))
	     (progn
	       (insert fill-prefix)
	       (setq prefixcol (current-column))))
	(and justify-flag (not (eobp))
	     (progn
	       (forward-line -1)
	       (justify-current-line)
	       (forward-line 1)))))))


