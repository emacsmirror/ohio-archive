;From ark1!uakari.primate.wisc.edu!aplcen!uunet!cme!durer!warsaw Sat Mar  3 17:26:04 EST 1990
;Article 1528 of comp.emacs:
;Xref: ark1 gnu.emacs:1246 comp.emacs:1528
;Path: ark1!uakari.primate.wisc.edu!aplcen!uunet!cme!durer!warsaw
;From: warsaw@cme.nist.gov (Barry A. Warsaw)
;Newsgroups: gnu.emacs,comp.emacs
;Subject: Better Directory Tracking Engine for GNU emacs
;Message-ID: <WARSAW.90Mar1192018@rtg.cme.nist.gov>
;Date: 2 Mar 90 00:20:18 GMT
;Sender: news@cme.nist.gov
;Organization: National Institute of Standards and Technology
;Lines: 422
;
;
;I've been out of touch from these newsgroups for a while, but I just
;recently had someone ask me about the state of my replacement for
;shell-set-directory.  I've also seen some discussion of this while I
;was catching up the other day.  So I decided to post the latest
;version of my directory tracker.  Its undergone a major rewrite since
;my last posting but the basic mechanism is the same.  Now, however,
;there is a general tracking engine and front ends for shell-mode
;(shell-set-directory) and cmushell-mode (shell-directory-tracker).
;BTW, my latest interest in this was prompted by my move from
;shell-mode to cmushell-mode, which still had a somewhat faulty
;tracker.
;
;Caveats:
;1) I've only used it with /bin/csh on a Sun3 as the underlying shell
;process. Your mileage may differ.
;
;2) Its still not perfect. It seems to be quite tricky to track
;commands such as:
;
;	% cd `my-favorite-path`
;	% pushd $SHELL_VAR
;	% !cd
;	% alias frob 'cd ~/blah' ; frob
;
;And the like. Someone mentioned an interesting idea where the pwd
;command might be used to resync the tracking engine, but I opted for
;an explicit resync command (tk-resync) instead.
;
;I did include a mechanism whereby the tracker could be automatically
;or explicitly turned on and off.  This is useful if you go into
;something like ftp, you don't want tracking while you're cd'ing around
;in another environment.
;
;Enjoy, comments are of course welcome, and if you come up with ways to
;cleanly do some of #2 above, please share it with me.
;
;-Barry
;
;NAME: Barry A. Warsaw                USMAIL: National Institute of Standards
;TELE: (301) 975-3460                         and Technology (formerly NBS)
;UUCP: {...}!uunet!cme-durer!warsaw           Rm. B-124, Bldg. 220
;ARPA: warsaw@cme.nist.gov                    Gaithersburg, MD 20899
;
;===== cut here ==================================================
;; baw-tracker.el
;;
;; a better tracker of directory navigation

;; Does a better, but still not perfect job of tracking csh builtin
;; commands that modify the directory and directory stack.  Correctly
;; tracks commands `cd', `pushd', `popd' and `dirs' with arguments.
;; Correctly expands paths containing environment variables, `~', `.'
;; and `..'.  Still can't track paths which contain shell variables,
;; execs, etc.  Currently only tested with /bin/csh, on Suns.
;; Compatible with both *shell* mode and *cmushell* mode.

;; Builtins recognized:
;;
;; cd [path]
;; pushd [+n | path]
;; popd [+n]
;; dirs [-l]
;;
;; Other commands recognized:
;;
;; ftp  (this shuts off auto tracking, waits for "quit")
;;
;; path can be either absolute or relative, n must be > 0.

;; Interactive commands available to you:
;; tk-on     : explicitly turn on directory tracking
;; tk-off    : explicitly turn off directory tracking
;; tk-resync : explicitly resync tracking engine

;; To use cons this to your shell-mode-hook:
;; (load "baw-tracker")

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY.  No author or distributor accepts
;; responsibility to anyone for the consequences of using it or for
;; whether it serves any particular purpose or works at all, unless
;; s/he says so in writing.

;; This software was written as part of the author's official duty as
;; an employee of the United States Government and is thus in the
;; public domain.  You are free to use this software as you wish, but
;; WITHOUT ANY WARRANTY WHATSOEVER.  It would be nice, though if when
;; you use this code, you give due credit to the author.

;; ======================================================================
;; Author:
;;
;; NAME: Barry A. Warsaw           USMAIL: National Institute of Standards
;; TELE: (301) 975-3460                      and Technology (formerly NBS)
;; UUCP: {...}!uunet!cme-durer!warsaw      Rm. B-124, Bldg. 220
;; ARPA: warsaw@cme.nist.gov               Gaithersburg, MD 20899

;; ======================================================================
;; Modification history:
;;
;; posted  :  1-Mar-1990 baw (comp.emacs, gnu.emacs)
;; modified:  1-Mar-1990 baw (turn tracking on and off with ftp/quit)
;;                           (explicit resync)
;; modified: 26-Feb-1990 baw (fixed front end for *shell* mode)
;; modified: 22-Jan-1990 baw (for compatibility w/ cmushell)
;;                           (fix some algorithms, factor code)
;; modified: 16-Nov-1989 baw (buffer local variables)
;; posted  : 14-Sep-1989 baw (comp.emacs, gnu.emacs)
;; modified: 14-Sep-1989 baw (cleaned up for posting)
;; modified: 11-Sep-1989 baw (fixed regexps)
;; created :  8-Sep-1989 baw

;; ======================================================================
;; Wish list:
;;
;; 1) Would like to be able to glob directory better to find the actual
;;    directory cd'd to in the case of shell variables, execs, etc.
;;
;; 2) Really would to be able to query the shell process for the current
;;    working directory.
;;
;; 3) Perhaps finagle "pwd" command for resyncing directory tracker when
;;    it gets off track.  This is a little more difficult since tracker
;;    has to watch for input that comes after its done processing the
;;    current line.  I'm currently opting for a manual resync of the
;;    the tracking engine via tk-resync


(defvar tk-track-p t
  "*Boolean flag which indicates whether tracking should or should not
be done. This is used to turn off tracking when entering ftp mode and
turning it back on when exiting ftp mode. Non-nil means to track
directories, nil means don't track directories.")

(defvar tk-eos-regexp "\\s *\\([\n;]\\|$\\)"
  "*Regular expression signifying the end of a shell builtin command,
correctly locating either a newline terminated statement, or a \";\"
delimited compound statement.")

(defvar tk-cd-regexp "cd"
  "*Regular expression signifying builtin `cd' command.")

(defvar tk-popd-regexp "popd"
  "*Regular expression signifying builtin `popd' command.")

(defvar tk-pushd-regexp "pushd"
  "*Regular expression signifying builtin `pushd' command.")

(defvar tk-dirs-regexp "dirs"
  "*Regular expression signifying builtin `dirs' command.")

(defvar tk-ftp-regexp "ftp"
  "*Regular expression signifying builtin `ftp' commands.")

(defvar tk-tracking-error-hook 'ignore
  "*Function called with no arguments when tracking in either
shell-mode or cmushell-mode results in an error.")

(defvar tk-start-tracking-regexp "quit"
  "*Regular expression which tells tracker to start tracking once
turned off.")

(setq shell-set-directory-error-hook tk-tracking-error-hook)
(setq tk-directory-stack nil)
(make-variable-buffer-local 'default-directory)
(make-variable-buffer-local 'tk-directory-stack)


(defun tk-parse-statement (statement regexp)
  "Parse STATEMENT to see if it contains the builtin command signified
by REGEXP.  Returns nil if the statement is not the signified builtin
command, otherwise returns a list specifying the arguments passed to
command, in the form: (N (ARG1 ARG2 ...)) where N is the number of
arguments."
  (let ((n 0)
	(args nil)
	arg
	(command (string-match (concat "^\\("
				       regexp
				       "\\)\\(\\s +\\|$\\)")
			       statement))
	(nextargi (match-end 0))
	)
    (if (not command) nil
      (while (string-match "\\S +" statement nextargi)
	(setq arg (substring statement (match-beginning 0) (match-end 0)))
	(setq args (append args (list arg)))
	(setq n (1+ n))
	(string-match "\\S +\\s +" statement nextargi)
	(setq nextargi (match-end 0))
	)
      (list n args)
      )
    ))


(defun tk-wholenum-arg-p (string)
  "Predicate which tests whether STRING is a whole number (i.e. an
integer greater than zero).  It returns the whole number if STRING
does not contain any non-numeric charcters, and is greater than zero,
otherwise returns nil. Also note that since this is an argument to a
builtin command, the first character of the string *must* be a plus
sign."
  (cond
   ((not (string-match "^\\+[1-9]+[0-9]*$" string))
    nil)
   ((> (string-to-int string) 0)
    (string-to-int string))
   (t nil)
   ))


(defun tk-listify-compound-command (command)
  "Convert COMMAND, which may be a compound statement (i.e. cd; ls)
into a list of simple statements. Only separators I know of are
\";\"."
  (let ((statements nil)
	(nextstatementi 0))
    (while (< nextstatementi (length command))
      (string-match tk-eos-regexp command nextstatementi)
      (setq statements
	    (append statements (list (substring command
						nextstatementi
						(match-beginning 0)))))
      (setq nextstatementi (1+ (match-end 0)))
      )
    statements
    ))
    

(defun tk-on ()
  "Turn on directory tracker."
  (interactive)
  (message "Turning on directory tracker.")
  (setq tk-track-p t)
  )


(defun tk-off ()
  "Turn off directory tracker."
  (interactive)
  (message "Turning off directory tracker.")
  (setq tk-track-p nil)
  )


(defun tk-resync (dir)
  "Resync directory tracking engine."
  (interactive "DResync to: ")
  (cd dir)
  )


(defun tk-tracking-engine (statement)
  "Parse the STATEMENT for one of the directory navigating builtin
commands and modify the directory stack accordingly.  STATEMENT should
be a single statement, not a compound statement."
  (let ((dir0 default-directory)
	args
	numeric)

    ;; test for each builtin command
    (cond
     ((setq args (tk-parse-statement statement tk-start-tracking-regexp))
      ;; we're looking at a command that restarts tracking, set by
      ;; various tracked commands
      (tk-on)
      )

     ((not tk-track-p))
     ;; is the tracking flag turned off?

     ((setq args (tk-parse-statement statement tk-ftp-regexp))
      ;; we're looking at an ftp command which shouldn't track
      (setq tk-start-tracking-regexp "quit")
      (tk-off)
      )

     ((setq args (tk-parse-statement statement tk-cd-regexp))
      ;; we're looking at a cd command
      (cond
       ((= (car args) 0)
	;; looking at a no arg'd cd command, means cd to $HOME
	(cd (expand-file-name (substitute-in-file-name "$HOME"))))
       ((> (car args) 1)
	(error "cd: Too many arguments."))
       ((= (car args) 1)
	(cd (expand-file-name
	     (substitute-in-file-name (car (car (cdr args)))))))
       (t
	(error "How did you get a negative number of arguments?"))
       ))

     ((setq args (tk-parse-statement statement tk-popd-regexp))
      ;; we're looking at a popd command
      (cond
       ((= (car args) 0)
	;; looking at a no arg'd popd, pop "top" directory from stack
	(cd (or (car tk-directory-stack)
		(error "popd: Directory stack empty.")))
	(setq tk-directory-stack (cdr tk-directory-stack)))
       ((> (car args) 1)
	(error "popd: Too many arguments."))
       ((< (car args) 0)
	(error "How did you get a negative number of arguments?"))
       ((not (setq numeric (tk-wholenum-arg-p (car (car (cdr args))))))
	(error "popd: Invalid argument: %s" (car (car (cdr args)))))
       ;; check to be sure there *is* an nth dir on the stack
       ((not (nth (1- numeric) tk-directory-stack))
	(error "popd: Directory stack not that deep."))
       ;; first pecial case when only two dirs are on the stack
       ((not (cdr tk-directory-stack))
	(setq tk-directory-stack nil))
       ;; second special case when popping the first dir on stack
       ((= numeric 1)
	(let ((tcdr (nthcdr numeric tk-directory-stack)))
	  (setcar tk-directory-stack (car tcdr))
	  (setcdr tk-directory-stack (cdr tcdr))
	  ))
       (t
	(setcdr (nthcdr (- numeric 2) tk-directory-stack)
		(nthcdr numeric tk-directory-stack)))
       ))

     ((setq args (tk-parse-statement statement tk-pushd-regexp))
      ;; we're looking at a pushd command
      (cond
       ((= (car args) 0)
	;; looking at a no arg'd pushd, exchange top two directories
	(cd (or (car tk-directory-stack)
		(error "pushd: No other directory.")))
	(setq tk-directory-stack (append (list dir0)
					 (cdr tk-directory-stack))))
       ;; looking at a numeric argument
       ((setq numeric (tk-wholenum-arg-p (car (car (cdr args)))))
	(cd (or (nth (1- numeric) tk-directory-stack)
		(error "pushd: Directory stack not that deep.")))
	(while (< 0 numeric)
	  (setq tk-directory-stack (append tk-directory-stack (list dir0))
		dir0 (car tk-directory-stack)
		tk-directory-stack (cdr tk-directory-stack)
		numeric (1- numeric)
		))
	)
       (t
	;; must be looking at a directory pathname
	(cd (expand-file-name
	     (substitute-in-file-name (car (car (cdr args))))))
	(setq tk-directory-stack (append (list dir0) tk-directory-stack)))
       ))

     ((setq args (tk-parse-statement statement tk-dirs-regexp))
      ;; we're looking at a dirs command
      (cond
       ((= (car args) 0)
	;; looking at a no arg'd dirs command
	(let ((dirs ""))
	  (mapcar
	   (function
	    (lambda (dir)
	      (and (string-match (concat "^" (substitute-in-file-name "$HOME"))
				 dir)
		   (setq dir (concat "~" (substring dir (match-end 0))))
		   )
	      (setq dirs (concat dirs (if (string-match "^~/$" dir)
					  "~"
					(directory-file-name dir))
				 " "))
	      )) ;; function
	   (append (list default-directory) tk-directory-stack)) ;; mapcar
	  (message "%s" dirs)
	  ))
       ((< (car args) 0)
	(error "How did you get a negative number of arguments?"))
       ((> (car args) 1)
	(error "dirs: Too many arguments."))
       ;; -l option is only one I know of for dirs
       ((string-match "^-l$" (car (car (cdr args))))
	(let ((dirs ""))
	  (mapcar
	   (function
	    (lambda (dir)
	      (setq dirs (concat dirs (directory-file-name dir) " "))
	      ))
	   (append (list default-directory) tk-directory-stack))
	  (message "%s" dirs)))
       (t
	(error "Usage: dirs [ -l ]."))
       ))
     )
    ))


(defun shell-set-directory ()
  "Better directory navigation tracker for shell-mode."
  (let* ((commandline (buffer-substring (point)
					(save-excursion
					  (end-of-line)
					  (point))))
	 (statements (tk-listify-compound-command commandline)))
    (while statements
      (tk-tracking-engine (car statements))
      (setq statements (cdr statements))
      )
    ))


(defun shell-directory-tracker (commandline)
  "Better directory navigation tracker for cmushell-mode."
  (let ((statements (tk-listify-compound-command commandline)))
    (while statements
      ;; note that we need to encase the call to the tracking engine
      ;; in a condition case to trap any errors that are signaled in
      ;; tk-tracking-engine.  Shell-mode automatically does this and we
      ;; want the two to be compatible. Ripped this from shell.el
      (condition-case ()
	  (tk-tracking-engine (car statements))
	(error (funcall tk-tracking-error-hook)))
      (setq statements (cdr statements))
      )
    ))
