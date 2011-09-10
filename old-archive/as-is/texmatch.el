;;;Date: Tue, 23 Feb 88 18:10:25 EST
;;;From: "Mark W. Eichin" <eichin@ATHENA.MIT.EDU>
;;;Subject: Closing LaTeX blocks

;;;Well, I always just use a function called tex-match-begin, so I NEVER
;;;type \end anything... I just hit M-E and it puts in the Correct \end
;;;statement.

;;;There is a validate-tex-buffer (?) function in tex-mode, but I have
;;;never had it work...
;;;				Mark Eichin
;;;			<eichin@athena.mit.edu>
;;;		SIPB Member & Project Athena ``Watchmaker'' 

;;;;; my special TeX (and LaTeX) mode hacks
(defun TeX-looping-search1 (regval repval)
  "Find the regval. If it is repval, loop again..."
	   
  (re-search-backward regval 0 nil)	; error if failed...
  (while 
      (not (looking-at repval))
    (search1 regval repval)
    (re-search-backward regval 0 nil)
    )
  )
(defun TeX-match-begin ()
  "Make a \\end that matches a \\begin somewhere up above."
  (interactive)
  (save-excursion
    (TeX-looping-search1 latm-reg latm-beg)
    (re-search-forward latm-getarg)
    (setq arg (buffer-substring (match-beginning 0) (match-end 0)))
    )
  (insert (concat "\n\\end" arg))
  )
(define-key TeX-mode-map "\eE" 'TeX-match-begin)
