;;;Date: 12 Sep 87 13:15:10 GMT
;;;From: allegra.UUCP!kautz@EDDIE.MIT.EDU
;;;Subject: better insert file function version 2

;;;A couple of days ago I posted a "better insert file function".  I just
;;;determined (blush) that it didn't always work as well as it might for
;;;pathnames which include "~" (namely /usr/name would not match against
;;;~).  So this version fixes that difficulty.

(defun insert-file-name-tail (name) 
 "Prompts for a file or directory name and inserts that name after point.
 Does not include unnecessary directory path prefixes.
 The name may be non-existent.  Useful in Shell mode."  
   (interactive "FInsert file name: ")
   (if (string-match (concat "^" (expand-file-name default-directory))
	  (expand-file-name name))
      (setq name (substring (expand-file-name name) (match-end 0))))
   (insert name))

;;;---- Henry Kautz
;;;:uucp:	allegra!kautz		
;;;:csnet:	kautz@allegra.att.com
;;;:arpa:	kautz%allegra%btl.csnet@csnet-relay.arpa
;;;:mail:  AT&T Bell Laboratories	
;;;	Room 3C-402A
;;;	600 Mountain Ave.	
;;;	Murray Hill, N.J. 07974	
;;;:office phone: (201) 582-2815	
