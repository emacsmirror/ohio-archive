;From ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!uwm.edu!ux1.cso.uiuc.edu!ux1.cso.uiuc.edu!m.cs.uiuc.edu!reingold Fri May 18 20:05:27 EDT 1990
;Article 1961 of comp.emacs:
;Path: ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!uwm.edu!ux1.cso.uiuc.edu!ux1.cso.uiuc.edu!m.cs.uiuc.edu!reingold
;>From: reingold@m.cs.uiuc.edu
;Newsgroups: comp.emacs
;Subject: Webster.elFix
;Message-ID: <4300072@m.cs.uiuc.edu>
;Date: 17 May 90 18:09:00 GMT
;Lines: 13
;Nf-ID: #N:m.cs.uiuc.edu:4300072:000:435
;Nf-From: m.cs.uiuc.edu!reingold    May 17 13:09:00 1990
;
;
;The function webster-current-word in webster.el is slightly brain damaged:
;when on the first character of a word, it returns the PREVIOUS word!  It's
;also poorly written.  Here is a correct version.

(defun webster-current-word ()
   "Word cursor is over, as a string."
   (save-excursion
      (let (beg)
        (re-search-backward "\\<")
        (setq beg (point))
        (forward-word 1)
        (buffer-substring beg (point)))))


