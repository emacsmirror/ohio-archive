;From ark1!uakari.primate.wisc.edu!aplcen!uunet!mcsun!ukc!mucs!graham@r3.cs.man.ac.uk Sun Dec 17 22:39:01 EST 1989
;Article 1077 of comp.emacs:
;Path: ark1!uakari.primate.wisc.edu!aplcen!uunet!mcsun!ukc!mucs!graham@r3.cs.man.ac.uk
;From: graham@r3.cs.man.ac.uk (Graham Gough)
;Newsgroups: comp.emacs
;Subject: Re: Automatic uncompress of .Z files?
;Message-ID: <408@m1.cs.man.ac.uk>
;Date: 15 Dec 89 15:35:11 GMT
;Sender: news@cs.man.ac.uk
;Organization: University of Manchester, UK
;Lines: 91
;
;>From article <7365@cs.utexas.edu>, by ctp@cs.utexas.edu (Clyde T. Poole):
;> Sometime awhile back, I believe I saw a posting including some emacs
;> lisp code to implement the automatic uncompress of .Z files (files
;> compressed with the unix compress utility).  I would appreciate it if
;> someone would provide me with the code or give me pointers to where I
;> can find it.
;> 
;
;This  isn't the one that was posted then and isn't  the  most  elegant
;piece  of  e-lisp  I've  ever  written,  but it's served me well for a
;couple of years. I hope it does what is required,
;
;Graham Gough

;From ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!uwm.edu!cs.utexas.edu!rutgers!mit-eddie!bloom-beacon!eru!luth!sunic!mcsun!ukc!mucs!graham@r3.cs.man.ac.uk Mon Jan  8 10:02:12 1990
;Article 1151 of comp.emacs:
;Path: ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!uwm.edu!cs.utexas.edu!rutgers!mit-eddie!bloom-beacon!eru!luth!sunic!mcsun!ukc!mucs!graham@r3.cs.man.ac.uk
;From graham@r3.cs.man.ac.uk
;Newsgroups: comp.emacs
;Subject: zcat.el : Repost
;Message-ID: <552@m1.cs.man.ac.uk>
;Date: 4 Jan 90 15:38:44 GMT
;Sender: news@cs.man.ac.uk
;Organization: University of Manchester, UK
;Lines: 86
;
;This  is  a  repost  prompted  by  RMS's message re copyright, since I
;inadvertently  omitted the copyright notice from my previous posting. 
;It also fixes a bug by replacing a call to shell-command-on-region  by
;one to call-process-region.
;
;Graham Gough
;
;----------------------------------------------------------
;Graham D. Gough, Department of Computer Science,
;University of Manchester, Oxford Road, Manchester, M13 9PL, U.K.
;Tel: (+44) 61-275 6277
;JANET: graham@uk.ac.man.cs   USENET: ..ukc!man.cs.r3!graham
;
;------------------------cut here ------------------------
;;
;; zcat.el
;;
;; Copyright (C) 1989 Graham D. Gough
;;
;; This file is not  part of GNU Emacs, however, GNU copyleft applies
;;
;; Visiting compressed files.
;;
;; Graham Gough (graham@uk.ac.man.cs.ux) 13/5/87
;; 
;; To use just load via .emacs, everything else is automatic
;;

(or (assoc "\\.Z$" auto-mode-alist)
    (setq auto-mode-alist (append  auto-mode-alist '(("\\.Z$" . zcat-buffer)))))

(defvar delete-compressed-files t "*Non-nil means delete the compressed version 
 of a file when a buffer is saved. Only has effect if original file visited
 was compressed.")

(defun delete-compressed-file ()
  "   Deletes (on confirmation) compressed version of file associated with
   current buffer"
  (interactive)
  (let ((fname (concat (buffer-file-name) ".Z"))
	ret)
    (if  (file-exists-p fname)
	(progn
	  (setq answer (yes-or-no-p (concat "Delete compressed file (" fname ")? ")))
	  (if answer 
	      (condition-case ()
		  (delete-file fname)
		(error
		 (message "Can't delete compressed file.")
		 (setq ret t))))))
    nil))

(defun zcat-buffer ()
  "   Uncompresses contents of buffer, respecting read-only status. Changes
   buffer-name, visited-file-name and mode appropriately. If buffer is saved, 
   compressed file is (optionally) deleted.
   Bug: Doesn't find an existing uncompressed buffer, creates a new one"
  (interactive)
  (let ((buf-stat buffer-read-only)	     ; remember read-only status
	(new-buf-name (substring (buffer-name) 0 -2)) ; get new buffer name
	(new-buf-fname (substring (buffer-file-name) 0 -2))) ; and file name
    (setq buffer-read-only nil)
    (message "Uncompressing %s .." (buffer-name))
    (call-process-region  (point-min)  (point-max)  "zcat" t t)
    (message "Done")
    (setq buffer-read-only buf-stat)	     ; reinstate original
					     ; buffer-read-only status
    (rename-buffer
     (let ((newbname new-buf-name)
	   (counter 1))
       (while (get-buffer newbname)
	 (setq newbname
	       (concat new-buf-name "<" (prin1-to-string counter) ">"))
	 (setq counter (1+ counter)))
       newbname))			     ;  generate appropriate name.
    (set-visited-file-name new-buf-fname)
    (set-auto-mode)
    (if (not delete-compressed-files)
	nil
      (make-variable-buffer-local 'write-file-hooks)
      (setq write-file-hooks
	    (append write-file-hooks '(delete-compressed-file))))
    (set-buffer-modified-p nil)		     ; this means that auto-saves
					     ; and saves only take place
					     ; if buffer is really modified
    (goto-char (point-min))))


