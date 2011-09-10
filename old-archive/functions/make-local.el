;From: gnb@bby.oz (Gregory N. Bond)
;Newsgroups: comp.emacs
;Subject: a make-multiple-local-variables function - revert to GNU c style
;Message-ID: <GNB.89Aug21154541@baby.bby.oz>
;Date: 21 Aug 89 05:45:41 GMT
;Distribution: comp
;Organization: Burdett, Buckeridge and Young Ltd.
;Lines: 42
;
;A quick hack I put together to ease working on GNU C programs.  
;
;We use K&R standard indenting locally, so our EMACS is set up for K&R
;indentation variables.  This is most annoying when working on GNU
;software which uses a different set of style parameters.  I defined a
;function to make the style variables local to a buffer, and then set
;them to the gnu defaults.  It is based on another function for doing
;multiple make-local-variable calls.
;
;I'm an elisp novice, so there may be better ways of doing this...
;
;Share and enjoy!
;
;Greg.
;
;---------8<-------------8<------------
;
; Make a stack of variables buffer-local and give them the specified values
;
(defun make-multiple-local-variables (alist)
  "Given a list ((var value) (var value) ...) make each var a buffer-local 
variable with the value specified."
  (mapcar '(lambda (var) 
	     (make-local-variable (car var)) (set (car var) (car (cdr var))))
	  alist))

; A function to reset the C style parameters back to standard 
; GNU C cstyle FOR THIS BUFFER ONLY
;
(defun c-gnu-style ()
  "Reset the C mode style variables to the standard GNU defaults
for this buffer."
  (interactive)
  (make-multiple-local-variables
	  '((c-indent-level 2) (c-continued-statement-offset 2)
	    (c-brace-offset 0) (c-brace-imaginary-offset 0)
	    (c-argdecl-indent 5) (c-label-offset -2))))

;--
;Gregory Bond, Burdett Buckeridge & Young Ltd, Melbourne, Australia
;Internet: gnb@melba.bby.oz.au    non-MX: gnb%melba.bby.oz@uunet.uu.net
;Uucp: {uunet,pyramid,ubc-cs,ukc,mcvax,prlb2,nttlab...}!munnari!melba.bby.oz!gnb
