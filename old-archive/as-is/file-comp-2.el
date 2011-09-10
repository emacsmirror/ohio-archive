;To: unix-emacs@bbn.com
;Date: 24 Apr 89 14:40:00 GMT
;From: Paul Davis <scr.slb.COM!davis@eddie.mit.edu>
;Sender: arpa-unix-emacs-request@bbn.com
;Subject: filename completion for anywhere in Emacs
;Source-Info:  From (or Sender) name not authenticated.
;
;
;
;Here is the first of many bits of lisp code I'll mail in the next few
;days (I'm leaving this job, and may as well get stuff out for
;posterity, if not for function: Datronics Inc. here I come ... )
;
;This one provides the function shell-filename-complete, which I
;globally bind to M-`, and which will expand the filename before point
;anywhere in Emacs. Its so named because I use it mostly in shell-mode.
;It does have one problem: unlike minibufer completions, which always
;throw away the *Completions* buffer after use, this one leaves it
;around. I don't know how to fix this, but you're welcome to try. Let
;me know if you succeed.
;
;Have fun. After April 28th, I'm reachable at:
;
;	bartonb%duvm.bitnet@cunyvm.cuny.edu
;
;Paul
;                             Paul Davis at Schlumberger Cambridge Research
;                                <davis%scrsu1%sdr.slb.com@relay.cs.net>
;
;                              "to shatter tradition makes us feel free ..."
;---- cut here -----
;; filename completion for anywhere in Emacs
;; Paul Davis <davis%scr.slb.com@relay.cs.net> July 1988

;; perhaps better to do this at the source level, but its
;; quick enough and a fair bit more obvious whats going on.

(defvar not-filename-regexp "\\(^\\|[] ^<>\"'`?$%{}|&*()#!@^\;\t\n]\\)"
  "grouped regexp specifying characters considered to be excluded 
from filenames. Based on csh special characters, coupled with a
brief consideration of C and Lisp syntax.")

(defun shell-filename-complete ()
  (interactive)
  (let* ((filename (expand-file-name (grab-filename)))
	(partial-name (file-name-nondirectory filename)))
    (if (null (setq directory (file-name-directory filename)))
	(error "no such directory"))
    (if (not (null
	      (setq completion-list
		    (mapcar 'list
			    (file-name-all-completions 
			     partial-name directory)))))
	(progn
	  (setq completion (try-completion partial-name completion-list))
	  (cond ((eq completion t))
		;; probably always grabbed by try-completion but throw it in anyway...
		((null completion)
		 (message "No such file or directory")
		 (ding))
		((not (string= partial-name completion))
		 (delete-region
		  (save-excursion 
		    (re-search-backward partial-name 
					(save-excursion
					  (beginning-of-line)(point))) (point))
		  (point))
		 (insert completion))
		(t
		 (message "Making completion list...")
		 (let ((list (all-completions partial-name completion-list)))
		   (with-output-to-temp-buffer "*Help*"
		     (display-completion-list list))
		   (message "Making completion list... done")))))
      (progn
	(message "No such file or directory")
	(ding)))))
  

(defun grab-filename ()
  "Gets the filename preceeding point. We have to assume 
something about characters not legal in filenames, because Un*x
only disallows / and NULL. This is determined by not-filename-regexp,
which is a regexp specifying a set of characters NOT legal in
filenames. It might be nice to add things to mode hooks to set
this for different modes, but the default is a guess at one
that should be reasonably general."
  (buffer-substring 
   (save-excursion
     (re-search-backward not-filename-regexp (point-min) t)
     (if (bolp)
	 (point)
       (1+ (point))))
   (point)))

