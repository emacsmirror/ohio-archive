;From dtix!mimsy!haven!purdue!bu-cs!bloom-beacon!athena.mit.edu!jtkohl Tue Jul 18 06:26:21 PDT 1989
;Article 12 of gnu.emacs.gnus:
;Path: ark1!dtix!mimsy!haven!purdue!bu-cs!bloom-beacon!athena.mit.edu!jtkohl
;>From: jtkohl@athena.mit.edu (John T Kohl)
;Newsgroups: gnu.emacs.gnus
;Subject: re: how to pretty print out saved messages?
;Message-ID: <12739@bloom-beacon.MIT.EDU>
;Date: 17 Jul 89 17:19:33 GMT
;References: <55098@tut.cis.ohio-state.edu>
;Sender: daemon@bloom-beacon.MIT.EDU
;Reply-To: jtkohl@athena.mit.edu (John T Kohl)
;Organization: Massachusetts Institute of Technology
;Lines: 35
;Keywords: Babyl Text convert
;
;In article <55098@tut.cis.ohio-state.edu> amra@cis.ohio-state.edu (Nasir K Amra) writes:
;
;	   Basically, I have a lot of messages saved from various news
;   groups in babyl format (?) via gnus's "o" command. How can I pretty
;   print the message files such that for example each message in a file
;   gets printed on a separate page?
;
;Here's some ancient e-lisp code I wrote to do this to RMAIL files (which
;are Babyl format).  Share and enjoy.
;
;To use, do a find-file to get the file in Text (or Fundamental) mode,
;run the function, then save to some other file and print.
;
;---------- cut here -------------
(defun convert-rmail-buffer-to-printable ()
  "Convert an rmail buffer visited in TEXT mode into a file suitable for
printing on a line printer.  Removes headers which have been filtered, and
leaves a formfeed before each message."
  (interactive)
  (message "Converting to printable file...")
  (search-forward "*** EOOH ***\n")
  (delete-region (point-min) (point))
  (let ((mesg-counter 1))
    (while (search-forward "\^_\^L" nil t)
      (if (zerop (% (setq mesg-counter (1+ mesg-counter)) 10))
	  (message "Converting to printable file...%d" mesg-counter))
      (delete-region (- (point) 2) (1- (point)))
      (let ((point-save (point)))
	(search-forward "*** EOOH ***\n")
	(delete-region point-save (point))))
    (delete-region (1- (point-max)) (point-max))
    (message "Converting to printable file...done")))
;John Kohl <jtkohl@ATHENA.MIT.EDU> or <jtkohl@Kolvir.Brookline.MA.US>
;Digital Equipment Corporation/Project Athena
;(The above opinions are MINE.  Don't put my words in somebody else's mouth!)


