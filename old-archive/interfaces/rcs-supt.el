;From ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!tut.cis.ohio-state.edu!LINUS.MITRE.ORG!guttman Mon Mar  5 09:12:10 1990
;Article 785 of gnu.emacs.bug:
;Path: ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!tut.cis.ohio-state.edu!LINUS.MITRE.ORG!guttman
;>From guttman@LINUS.MITRE.ORG
;Newsgroups: gnu.emacs.bug
;Subject: Re: RCS mode for gnu emacs ?
;Message-ID: <9003021409.AA04545@darjeeling.mitre.org>
;Date: 2 Mar 90 14:09:36 GMT
;References: <9003011616.AA23343@life.ai.mit.edu>
;Sender: daemon@tut.cis.ohio-state.edu
;Distribution: gnu
;Organization: GNUs Not Usenet
;Lines: 64
;
;I use the enclosed.  It's not super sophisticated, but it works, and you can
;improve it if you need more.
;
;	Josh
;
;~~~~~~~~~~~~~~~~

;; file el/rcs-support.el
;;
;;; Provide support by checking out files when needed, either with a lock or
;;; for read-only, and checking them back in, if necessary before emacs exits.

(require 'shell)

(defvar *rcs-file-read-only* nil)

(defun rcs-find-file-noselect (name &optional path)
  "Check out and visit current revision of NAME in current default directory.  
Search PATH and PATH/RCS for RCS file named NAME or NAME,v."
  (let ((rcs-name (if (string-match ",v$" name)
		      name
		    (concat name ",v")))
	(rcs-path (if (string-match "RCS/$" path)
		      path
		    (concat path "RCS/"))))    
    (cond ((and (file-exists-p name)
		(file-writable-p name)))
	  ((file-exists-p (concat path rcs-name))
	   (call-co (concat path rcs-name) default-directory))
	  ((file-exists-p (concat rcs-path rcs-name))
	   (call-co (concat rcs-path rcs-name) default-directory))
	  (t (error "RCS file not found %s" (concat path name))))
    (find-file-noselect name)))

(defun call-co (rcs-filename working-dir)
  (switch-to-buffer-other-window (get-buffer-create "*rcs-notes*"))
  (setq default-directory working-dir)
  (if  *rcs-file-read-only*
      (call-process "co" nil t t rcs-filename)
    (call-process "co" nil t t "-l" rcs-filename))
  (insert "\n~~~\n")
  (other-window 1))

(defun rcs-find-file (name path read-only)
  "Check out and select current revision of NAME in current default directory.
Prefix arg means don't lock; file will be read only.
Search PATH and PATH/RCS for RCS file named NAME or NAME,v."
  (interactive "sGet RCS-controlled file named: 
DIn RCS directory for: 
P")		 
  (let ((*rcs-file-read-only* read-only))
    (switch-to-buffer
     (rcs-find-file-noselect name (expand-file-name path)))))

(defun rcs-check-in (buff)
  "Check in RCS file in current-buffer, killing the buffer.  From program, supply BUFF."
  (interactive (list (current-buffer)))
  (let ((filename (buffer-file-name buff)))
    (switch-to-buffer-other-window
     (make-shell "rcs-notes" "ci" nil filename))
    (kill-buffer buff)))


    


