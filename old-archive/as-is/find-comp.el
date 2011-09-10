;To: unix-emacs@bbn.com
;Date: 8 Nov 88 07:09:09 GMT
;From: Joel Spolsky <spolsky-joel@yale.ARPA>
;Subject: Automatically uncompress files when finding them
;
;Version: GNU-Emacs 18.52. Probably works on earlier versions.
;
;This is a little function that replaces find-file. If the file to be
;found is compressed (ends with .Z) this function will uncompress it
;before finding it. I don't know if this has been done before, anyway,
;here it is:
;
(defun find-file-with-uncompress (filename) 
  "Find a file. If it is compressed, uncompress it first. Assumes that
   any file ending in .Z is compressed. The argument should include a
   .Z to uncompress a file."
  (interactive "FFind file: ")
  (progn
    (if (equal (substring filename -2) ".Z")
	(progn
	  (let* ((buffer-read-only nil)  ; code stolen from dired.el
		 (from-file filename)
		 (to-file (substring from-file 0 -2)))
	    (message "Uncompressing %s..." from-file)
	    (call-process "uncompress" nil nil nil from-file)
	    (message "Uncompressing %s... done" from-file)
	    (switch-to-buffer (find-file-noselect to-file))))
      (switch-to-buffer (find-file-noselect filename)))))

;You might bind this to ^X^F (in your .emacs for example) using:
;
;	(global-set-key "\C-x\C-f" 'find-file-with-uncompress)
;
;
;I really don't know too much about emacs-lisp, so if any of you real
;hackers have any comments, I would be happy to hear them.
;
;+----------------+---------------------------------------------------+
;|  Joel Spolsky  | bitnet: spolsky@yalecs     uucp: ...!yale!spolsky |
;|                | arpa:   spolsky@yale.edu   voicenet: 203-436-1483 |
;+----------------+---------------------------------------------------+
;                                               #include <disclaimer.h>

