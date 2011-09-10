;From ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!mips!bridge2!jarthur!spectre.ccsf.caltech.edu!news Sat Mar  3 17:27:34 EST 1990
;Article 1530 of comp.emacs:
;Xref: ark1 gnu.emacs:1259 comp.emacs:1530
;Path: ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!mips!bridge2!jarthur!spectre.ccsf.caltech.edu!news
;>From: johns@macondo.ccsf.caltech.edu (John Salmon)
;Newsgroups: gnu.emacs,comp.emacs
;Subject: Re: Better Directory Tracking Engine for GNU emacs
;Message-ID: <1990Mar3.011743.11263@spectre.ccsf.caltech.edu>
;Date: 3 Mar 90 01:17:43 GMT
;References: <WARSAW.90Mar1192018@rtg.cme.nist.gov>
;Sender: news@spectre.ccsf.caltech.edu
;Reply-To: johns@macondo.ccsf.caltech.edu (John Salmon)
;Followup-To: gnu.emacs
;Organization: Caltech Concurrent Supercomputing Facility
;Lines: 91
;In-Reply-To: warsaw@cme.nist.gov (Barry A. Warsaw)
;
;In article <WARSAW.90Mar1192018@rtg.cme.nist.gov>, warsaw@cme (Barry A. Warsaw) writes:
;>
;>And the like. Someone mentioned an interesting idea where the pwd
;>command might be used to resync the tracking engine, but I opted for
;>an explicit resync command (tk-resync) instead.
;>
;etc.
;>
;>NAME: Barry A. Warsaw                USMAIL: National Institute of Standards
;>TELE: (301) 975-3460                         and Technology (formerly NBS)
;>UUCP: {...}!uunet!cme-durer!warsaw           Rm. B-124, Bldg. 220
;>ARPA: warsaw@cme.nist.gov                    Gaithersburg, MD 20899
;>
;
;Here is a set of elisp functions that partially automate
;the process of resync-ing a process buffer.  They send a
;pwd command to the running process, and then cd to the
;answer.  I have tried to put in sanity checks, but
;it still isn't impossible to completely confuse these functions.
;Thus, the fwd-reset-filter function is provided.
;
;I also have some code for following a user's 'cdpath', but
;it is not nearly so self-contained.  I can post diffs to
;cmushell.el, if there's interest.
;
;John Salmon
;johns@macondo.ccsf.caltech.edu
;johns@caltech.bitnet
;
;------------------- cut here, filename: fixwd.el  ---------------------
;; Author: John Salmon
;; Copyright 1989, John Salmon
;; Licensing: This software is made available under the terms
;; of the GNU EMACS GENERAL PUBLIC LICENSE (11 Feb 1989 version)
;;
;; An attempt to send 'pwd' to a process running in a buffer
;; shell, and then use the output
;; to fix up the current working directory.
;; As it stands, this should work as well with shell.el, comint.el
;; or any of their clients.  It would be prettier if it were
;; merged into comint.el, with an interface using a
;; local variables like:
;; (defvar comint-pwd-command "pwd")
;; Another necessary improvement is some kind of timeout,
;; if the process does not respond to the 'pwd' command.
;;
;; Usage:
;; I have the following in my .emacs (this file is called fixwd.el)
;; (autoload 'fwd "fixwd"
;;	  "Use pwd to repair a process buffer's notion of current-directory"
;;	  t)
;; (setq cmushell-load-hook
;;       '((lambda () 
;;	   (define-key cmushell-mode-map "\C-cf" 'fwd))))
;;
;;
(provide 'fwd)

(defvar pwd-old-filter nil "The previous filter, in place before the fixwd.")

(defun pwd-filter (proc str) 
  "STR should be the output from pwd.  We peel off the terminal
newline, and hand it to the elisp function cd.  Thus, we go to
where the PROC really is.  In addition, we return filtering
for PROC to whatever was doing it before.  See also fwd."
  (set-process-filter proc pwd-old-filter)
  (let ((to (substring str 0 (string-match "\n" str))))
    (if (file-directory-p to)
	(cd to)
      (message "Strange reply from pwd: %s" to))))

(defun fwd ()
  "Fix up the current directory, by using (process-send-string \"pwd\")
to enquire directly, and then cd'ing to the answer.  Only valid in
a buffer with an active process."
  (interactive)
  (make-local-variable 'pwd-old-filter)
  (let* ((proc (get-buffer-process (current-buffer))))
    (if	(processp proc)
	(progn
	  (set-variable 'pwd-old-filter (process-filter proc))
	  (set-process-filter proc 'pwd-filter)
	  (process-send-string proc "pwd\n"))
      (error "Can't fix working directory if there is no process."))))

(defun fwd-reset-filter() "If fwd got very confused, this may be necessary."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (if (processp proc)
	(set-process-filter proc nil) ;; pwd-old-filter may be wrong!
      (error "Can't reset the process filter if there's no process."))))


