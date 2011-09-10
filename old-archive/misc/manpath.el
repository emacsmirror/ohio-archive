;From ark1!nems!mimsy!haven!purdue!tut.cis.ohio-state.edu!zaphod.mps.ohio-state.edu!usc!apple!sun-barr!newstop!sun!rberlin Wed Dec  6 12:18:31 1989
;Article 809 of gnu.emacs:
;Path: ark1!nems!mimsy!haven!purdue!tut.cis.ohio-state.edu!zaphod.mps.ohio-state.edu!usc!apple!sun-barr!newstop!sun!rberlin
;>From rberlin@birdland.sun.com (Rich Berlin)
;Newsgroups: gnu.emacs
;Subject: Re: MANPATH
;Message-ID: <RBERLIN.89Dec1180023@birdland.sun.com>
;Date: 2 Dec 89 02:00:23 GMT
;References: <8911212118.AA29799@sn1987a.compass.com>
;Sender: news@sun.Eng.Sun.COM
;Distribution: gnu
;Organization: Sun Microsystems
;Lines: 49
;
;This ought to fill the bill.
;
;-- Rich

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; manpath.el --- find /cat? directories using MANPATH environment variable.
;; Author          : Rich Berlin
;; Created On      : Thu Sep  1 16:52:15 1988
;; Last Modified By: SILL D E
;; Last Modified On: Tue Jul 24 10:20:07 1990
;; Update Count    : 6
;; Status          : In daily use, probably OK.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is a handy way to keep up-to-date with the MANPATH in your .cshrc.
;; I use this by loading this file, and then doing
;; (setq manual-formatted-dirlist (search-manpath-for-cat-directories))
;;
;; Of some interest is csh-path-to-list, which you may want to use
;; for other purposes.

(defun search-manpath-for-cat-directories ()
  "Return a list of all 'cat' directories in the current path specified\n\
by the 'MANPATH' environment variable."
  (search-path-for-files-matching
   (csh-path-to-list (getenv "MANPATH")) "cat.+"))

(defun csh-path-to-list (string)
  (if (null string)
      (setq string "/usr/man"))
  (let ((path nil) index)
    (while (setq index (string-match ":" string))
      (setq path (append path (list (substring string 0 index))))
      (setq string (substring string (1+ index))))
    (setq path (append path (list string)))))


(defun search-path-for-files-matching (path regexp)
  (if (null path)
      (list regexp)
    (let (curr-dir directory-list)
      (setq regexp (format "^%s$" regexp))  ; So files MATCH regexp, instead
      (while path			    ; of just CONTAINING regexp.
	(setq curr-dir (car path))
	(setq path (cdr path))
	(if (file-exists-p curr-dir)
	    (setq directory-list (append directory-list
					 (directory-files curr-dir t regexp)))))
      directory-list)))


