;From arpa-unix-emacs-request@ALEXANDER.BBN.COM Wed Jun 24 04:43:44 1987
;Received: from alexander by ALEXANDER.BBN.COM id aa01099; 24 Jun 87 1:34 EDT
;Received: from [128.89.0.122] by ALEXANDER.BBN.COM id aa01095;
;          24 Jun 87 1:34 EDT
;Received: from g.bbn.com by BBN.COM id aa29238; 24 Jun 87 1:33 EDT
;Received: from UCBVAX.BERKELEY.EDU by G.BBN.COM; Wed 24 Jun 87 01:33:28-EDT
;Received: by ucbvax.Berkeley.EDU (5.57/1.26)
;	id AA19953; Tue, 23 Jun 87 21:53:09 PDT
;Received: from USENET by ucbvax.Berkeley.EDU with netnews
;	for arpa-unix-emacs@bbn.com (unix-emacs@bbn.com)
;	(contact usenet@ucbvax.Berkeley.EDU if you have questions)
;Date: 23 Jun 87 15:57:08 GMT
;From: btl.CSNET!kautz.allegra@EDDIE.MIT.EDU
;Subject: bib mode
;Message-Id: <8706232333.AA17306@EDDIE.MIT.EDU>
;Sender: unix-emacs-request@BBN.COM
;To: unix-emacs@BBN.COM
;
;
;Here is some nifty code to help maintain databases compatible with
;refer and lookbib.  Enjoy!
;
;---- Henry Kautz
;:uucp:	allegra!kautz		
;:csnet:	kautz%allegra@btl.csnet
;:arpa:	kautz%allegra%btl.csnet@csnet-relay.arpa
;:office phone: (201) 582-2815	
;
;; Bib-Mode
;;   GNU Emacs code to help maintain databases compatible with Unix
;;   refer and lookbib.  The file bib-file should be set to your 
;;   bibliography file.  Keys are automagically inserted as you type,
;;   and appropriate keys are presented for various kinds of entries.
;;     Copyright 1987 Henry A. Kautz


(setq bib-file "~/abstracts/abstr1987.bib")

(defun addbib ()
   "Set up editor to add to bibliography file 
specified by global variable bib-file.  
Keys automagically inserted.  To ignore a key,
just hit return a second time."
   (interactive)
   (find-file bib-file)
   (end-of-buffer)
   (text-mode)
   (abbrev-mode 1)
   (bib-mode 1)
   )
   
(setq bib-mode-on nil)

(defun bib-mode (setting)
   "Toggle bib mode.  If called from program, SETTING is non-nil to turn on."
   (interactive (list (not bib-mode-on)))
   (setq bib-mode-on setting)
   (if bib-mode-on
      (local-set-key "\C-M" 'return-key-bib)
      (local-unset-key "\C-M")
      )
   )


(setq bib-assoc '(
		   (" *$" . "%A ")
		   ("%A ." . "%A ")
		   ("%A $" . "%T ")
		   ("%T " . "%D ")
		   ("%D " . "%J ")
		   ("%J ." . "%V ")
		   ("%V " . "%N ")
		   ("%N " . "%P ")
		   ("%P " . "%K ")
		   ("%K " . "%X ")
		   ("%X " . "")
		   ("%J $" . "%B ")
		   ("%B ." . "%E ")
		   ("%E ." . "%E ")
		   ("%E $" . "%I ")
		   ("%I " . "%C ")
		   ("%C " . "%P ")
		   ("%B $" . "%R ")
		   ("%R " . "%I ")
		   ))
		   
;; Format of various kinds of references is as follows:
;
; journal papers:                    a* t d j v n p k x
; articles in books & proceedings:   a* t d b e* i c p k x 
; tech reports:                      a* t d r i c k x
; books:                             a* t d i c k x


(defun bib-find-key (slots)
   (cond
      ((null slots)
	 (if (bobp)
	    ""
	    (progn (previous-line 1) (bib-find-key bib-assoc))))
      ((looking-at (car (car slots)))
	 (cdr (car slots)))
      (t (bib-find-key (cdr slots)))
      ))


(defun return-key-bib ()
   "Magic when user hits return, used by bib-mode"
   (interactive)
   (if (eolp)
      (let (new-key beg-current)
	 (beginning-of-line)
	 (setq beg-current (point))
	 (setq new-key (bib-find-key bib-assoc))
	 (goto-char beg-current)
	 (if (looking-at "%. $")
	    (kill-line nil)
	    (progn (end-of-line nil) (newline))
	    )
	 (insert-string new-key)
	 )
      (newline)
      )
   )

