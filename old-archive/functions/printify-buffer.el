; Path: utkcs2!emory!swrinde!ucsd!helios.ee.lbl.gov!pasteur!tully.Berkeley.EDU!mcgrath
; >From: mcgrath@tully.Berkeley.EDU (Roland McGrath)
; Newsgroups: comp.emacs
; Subject: Re: searching for "non-printables"
; Date: 23 Jul 90 19:43:31 GMT
; References: <929@meaddata.mead.UUCP> <MCGRATH.90Jul20171843@homer.Berkeley.EDU>
; 	<Jul.22.09.46.19.1990.21055@athos.rutgers.edu>
; Organization: Hackers Anonymous International, Ltd., Inc. (Applications
; 	welcome)
; X-Local-Date: 23 Jul 90 12:43:31 PDT
; In-reply-to: shapiro@athos.rutgers.edu's message of 22 Jul 90 13:46:21 GMT
; 
;    I don't think mcgrath answered the question, at least not my question,
;    which is
;       How can you pass through a file checking for the existence of any
;    control characters that might screw up printing? 
;       and    does anyone have an .el file which will convert all such
;    characters to some printable representation (for example, so they will
;    print the way they appear on the screen)
; 		   Joel Shapiro
; 
; I answered the question asked in the posting I was responding to.
; 
; Since I don't know what characters will screw up printing for you, I cannot
; necessarily answer your question to your satisfaction.  However, if it is
; assumed that all non-printing characters other than TAB, LF, SPC, RET, and FF are problematical, I can offer this:
; 
; A regular expression for searching for such characters is (in a Lisp string):
; "[\^@-\^h\^k\^n-\^_\177-\377]".  The following function will turn such
; characters into their printing representations:
; 
(defun printify-buffer ()
  "Turn nonprinting characters in the current buffer 
into their printable representations."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let (c)
      (while (re-search-forward "[\^@-\^h\^k\^n-\^_\177-\377]" nil t)
	(setq c (char-after (point)))
	(delete-char 1)
	(insert (if (and (>= c ?\^@) (<= c ?\^_))
		    (format "^%c" (- c ?@)))
		(format "\\%03o" c))))))
