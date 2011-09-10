; From: nickel@cs.tu-berlin.de (Juergen Nickelsen)
; Subject: Dumping a private copy of Emacs (was Re: model for .emacs file)
; Date: 16 Jan 93 00:44:26 GMT
; Organization: STONE Project, Technical University of Berlin, Germany
; 
; In article <GISLI.93Jan13225133@liapunov.eecs.umich.edu>
; gisli@liapunov.eecs.umich.edu (Gisli Ottarsson) writes:
; 
; > How about dumping your private copy of emacs once all your
; > customization has stablized?  Works fine for me.
; 
; This posting made me try this again; I had done that previously and
; fallen into some holes; one of these was bug no 2 (see below).
; However, the function dump-emacs lacks some features that may be
; useful for the user; so I have written an interactive dump function.
; 
; My .emacs file now looks like this:
; 
;     (defvar privately-dumped-emacs nil
;       "If non-nil, the running Emacs is a privately dumped copy.
;     This variable is set by dump-my-emacs to a string containing the
;     date of the dump and the userid and hostname of the person who did
;     the dump.")
; 
;     (if (not privately-dumped-emacs)
; 	(progn
;     
; 	  ;; load stuff
;           (load "ange-ftp")
;           ;; etc.
; 	  
;         ))
;     ;; actions, even to be done in a privately dumped Emacs.
;     (if display-time-process
; 	(delete-process display-time-process))
;     (display-time)
;     ;; etc.
; 
; Since I load some expensive packages on startup (tree-dired, ange-ftp,
; completion), this affected my Emacs' startup speed by a factor of
; five.
; 
; Two bugs still remain: 
; 
;   - It seems to be impossible to set the variable
;     privately-dumped-emacs from the function dump-my-emacs for the
;     dumped Emacs without affecting the Emacs it has been dumped from.
; 
;   - If the dumping Emacs used an X display, the dumped Emacs will not
;     run on an alpha terminal. On the other hand, if the dumping Emacs
;     ran on an alpha terminal, the dumped *will* be able to use an X
;     display.
;       
; ----------------------------------------------------------------------
; 

;; interactive Emacs dump function
;; nickel@cs.tu-berlin.de  Jan 16 1993

;; LCD Archive Entry:
;; dump-my-emacs|Juergen Nickelsen|nickel@cs.tu-berlin.de|
;; Interactive Emacs dump function.|
;; 1993-01-16||~/misc/dump-my-emacs.el.Z|

(defun dump-my-emacs (filename)
  "Dump current Emacs to file FILENAME.
command-line-processed is set to nil, the variable
privately-dumped-emacs is set to a descriptive string. This variable
can be used in the .emacs startup file to determine if some libraries
have yet to be loaded.

If the variable emacs-symbol-file exists and contains a string,
its contents is used as the name of the symbol-file for dumping Emacs.
Otherwise <exec-directory>/../src/emacs-<emacs-version>
and <exec-directory>/../src/xemacs are tried.

The dumped Emacs has the same buffer configuration and contents as
the one it has been dumped from. To have a clean Emacs for
general-purpose use, dump a newly started Emacs that contains only
an empty *scratch* buffer."
  (interactive "FDump Emacs to file: ")
  (setq filename (expand-file-name filename))
  (setq command-line-processed nil)
  (setq privately-dumped-emacs
	(format "Emacs dumped at %s by %s@%s"
		(current-time-string)
		(user-login-name)
		(system-name)))
  (let ((symbol-file (or (if (and (boundp 'emacs-symbol-file)
				  (stringp emacs-symbol-file))
			     emacs-symbol-file)
			 (let ((version-emacs (concat exec-directory
						      "../src/emacs-"
						      emacs-version)))
			   (if (file-exists-p version-emacs)
			       version-emacs))
			 (let ((xemacs (concat exec-directory
					       "../src/xemacs")))
			   (if (file-exists-p xemacs)
			       xemacs)))))
    (if (not symbol-file)
	(error "No symbol file found for dumping Emacs")
      (garbage-collect)
      (message "Dumping Emacs...")
      (message (if (dump-emacs filename symbol-file)
		   ""
		 "Dumping Emacs...done")))))
