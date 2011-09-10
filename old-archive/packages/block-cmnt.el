;From ark1!uakari!zaphod.mps.ohio-state.edu!uwm.edu!ogicse!decwrl!shelby!portia!stergios Mon Feb 26 13:16:30 1990
;Article 1214 of gnu.emacs:
;Path: ark1!uakari!zaphod.mps.ohio-state.edu!uwm.edu!ogicse!decwrl!shelby!portia!stergios
;>From stergios@portia.portia.stanford.edu (Stergios)
;Newsgroups: gnu.emacs
;Subject: Re: What's the best way to handle these comments in GNU emacs
;Message-ID: <STERGIOS.90Feb23193045@portia.portia.stanford.edu>
;Date: 24 Feb 90 03:30:45 GMT
;References: <1990Feb21.055602.14270@csis.dit.csiro.au>
;Sender: Stergios <stergios@portia.stanford.edu>
;Reply-To: stergios@jessica.stanford.edu
;Organization: Stanford University
;Lines: 163
;In-reply-to: stuarth@csis.dit.csiro.au's message of 21 Feb 90 05:56:02 GMT
;
;
;
;# Occasionally I use comment "boxes" like this:
;
;# /*------------------------------------------------------------*/
;# /* This is a comment in a box.                                */
;# /*------------------------------------------------------------*/
;
;# Is there a neat way to insert this kind of comment?
;
;# 					Stuart Hungerford
;# 					stuart@csis.dit.csiro.au
;
;Well, you asked for it, you got it.  Be careful with this.  Its not
;pretty, hey I'm no elisp guru, but I use it anyway.

; com.el, origional code snarfed from a gosling translation bye dsm
; and modified by stergios to find the comment block rather than
; having to set point and mark by hand.
; suggested use:
;		M-x comment-region
;		M-x uncomment-region
;		M-x recomment-block
; Stergios Marinopoulos. copy, steal, sell this code. do want you want
; with it - just dont blame me. 
;

(defun recomment-block ()
  "Fills paragarph for the block-styled comment point is in.  Point must
be in a block comment, it cannot not be on the 1st or last line.
My definition of a comment block looks like this:
/*********************************************************/
/* You mean you actually comment code?			 */
/*							 */
/*							 */
/*********************************************************/
You can change the regexps for different looking comment lines.
"
  (interactive)
  ; locate comment region point is in, then call uncomment it, then fill it.
  (save-excursion
    (end-of-line)
    (re-search-backward "/\\*[*]*\\*/\n")
    (push-mark)
    (next-line 1)
    (re-search-forward "/\\*[*]*\\*/\n")
    (uncomment-region)
    (backward-word 1)
    (fill-paragraph 'nil)
    (set-fill-column 63)
    
    ; locate region filled paragraph is in, and wrap it in comments.
    (backward-paragraph 1)
    (next-line 1)
    (push-mark)
    (forward-paragraph 1)
    (comment-region)
    )
  )

(defun comment-region ()
"Wrap block style comments around region."
  (interactive)
  (narrow-to-region (point) (mark))
  (goto-char (point-min))
  
  ; clear out blank lines
  (while (and (eolp) (not (eobp)))
    (delete-char 1))

  ; Now count the length of the longest line
  (setq longest 0)
  (while (not (eobp))
    (end-of-line)
    (delete-horizontal-space)
    (if (> (setq len (current-column)) longest)
	(setq longest len))
    (forward-char))

  (if (< longest 65)
      (setq longest 65))


  ; Clear out blank lines at the end of region
  (goto-char (point-max))
  (while (and (not (bobp)) (= (preceding-char) 10))
    (delete-char -1))
  (insert "\n")

  ; Now insert the top of the comment
  (goto-char (point-min))
  (insert "/*")
  (setq len 1)
  (while (<= len longest)
    (insert "*")
    (setq len (+ len 1)))
  (insert "*/\n")

  ; Insert sides of comment
  (while (not (eobp))
    (insert "/* ")
    (end-of-line)
    (while (< (current-column) (1+ longest))
      (insert " "))
    (insert " */")
    (forward-char))

  ; Now insert last line of comment
  (goto-char (point-max))
  (insert "/*")
  (setq len 1)
  (while (<= len longest)
    (insert "*")
    (setq len (1+ len)))
  (insert "*/\n")

  ; Count the number of lines in the comment
  (setq longest (count-lines (point-min) (point-max)))
  (beginning-of-buffer)
  (set-mark (point))
  (goto-char (point-max))
  (widen)

  (while (looking-at "[ \n]")
    (forward-char))
  (setq len (current-column))
  (exchange-point-and-mark)
  (set-mark (point))
  (while (> longest 0)
    (setq longest (- longest 1))
    (setq loop len)
    (while (> loop 0)
      (insert " ")
      (setq loop (- loop 1)))
    (next-line 1)
    (beginning-of-line))
  )

(defun uncomment-region ()
"Strip out block style comments in region"
  (interactive)
  (save-excursion
    (narrow-to-region (point) (mark))

    ; remove spaces
    (beginning-of-buffer)
    (replace-regexp "^[ ]*" "")
    
    ; remove comment top and bottom
    (beginning-of-buffer)
    (replace-regexp "/\\*[*]*\\*/\n" "")  
    
    ; remove left side
    (beginning-of-buffer)
    (replace-regexp "^/\\* " "")
    
    ; remove right side
    (beginning-of-buffer)
    (replace-regexp "\\*/$" "")
    
    (widen)
    ) 
  )


