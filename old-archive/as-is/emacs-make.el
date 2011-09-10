;To: unix-emacs@BBN.COM
;Date: 12 Aug 88 23:09:00 GMT
;From: Lloyd Zusman <fxgrp!ljz@AMES.ARC.NASA.GOV>
;Subject: Making within elisp.  I DID IT!
;Reply-To: Lloyd Zusman <ljz%fx.com@ames.arc.nasa.gov>
;Organization: Master Byte Software, Los Gatos, California
;
;About a week ago I posted a question here about using 'make' with
;Elisp code.  Since then, I have figured out how to run a make inside
;an emacs buffer with the make productions being just about any Elisp
;commands you like.  My code is enclosed below.  But first, let me give
;you some background.  In my original posting, I asked about how to do
;two things:
;
;1)  Compile my Elisp code outside of emacs without the overhead of
;    invoking emacs for every single compilation.
;
;2)  Somehow run a make inside of emacs.
;
;I got several helpful replies addressing the first question, mostly
;telling me about "byte-recompile-directory" and "batch-byte-compile",
;the latter I hadn't known about before.
;
;I was sent some Elisp code which performs a make-like procedure
;on Xscheme code.  I haven't had a chance to dig through this yet.
;
;And I also got some suggestions about using 'make' outside of emacs to
;build a series of "byte-compile-file" commands, one for each *.el file
;that needs to be recompiled.  This list would then be passed into
;emacs as the final step in the make, thereby only incurring the
;overhead of invoking emacs once.
;
;Thank you very much to all of you kind and generous people who took
;the time to send me your suggestions and your code.  Please forgive me
;if I didn't get to personally reply to all of you who sent me things.
;Your help has been invaluable to me.
;
;After thinking about all of the stuff you folks sent me, I suddenly
;got a flash of insight:
;
;What if I were to use 'make -n' to figure out what files need to be
;remade?  It would output a list of productions that I could capture
;in a buffer.  Since the '-n' flag inhibits 'make' from doing any
;execution, the productions could just as well be Elisp functions.
;I would then run this buffer through 'eval-current-buffer' and the
;job is done.
;
;I tried this and it works!
;
;My code follows.  The function is called 'emacs-make'.  Look at the
;comments and function descriptions for more details on how it works.
;As an added bonus, I also enclosed some functions to replace
;'call-process' and 'call-process-region' (my replacements are called
;'run-process' and 'run-process-region').  These behave exactly the
;same as their counterparts, but they return an indication to the
;caller as to what the process's exit code is: if the exit code is 0,
;these return 't'; otherwise they return 'nil' ... in addition, the
;actual numeric value of the exit code is put into a variable called
;'runproc-exit-code'.
;
;I am not very adept at Elisp, so please forgive any function misuse
;or ugly style.  I welcome any suggestions as to how to improve this.
;
;Enjoy:
;
;------------------------------- cut here --------------------------------
;;; -*- Emacs-Lisp -*-

;;; Emacs make -- Lloyd Zusman 8/12/88
;;;
;;;	Performs a make inside of an emacs buffer.  The make productions
;;;	are expected to be emacs-lisp functions residing in a file called
;;;	'Emakefile'.  For example, here's an Emakefile which will perform
;;;	'byte-compile-file' on any .el files that need it and then load
;;;	them.
;;;	
;;;		.SUFFIXES:	.el .elc
;;;
;;;		.el.elc:
;;;			(and
;;;			    (byte-compile-file "$<")
;;;			    (load-file "$@"))
;;;
;;;		all:	foo.elc bar.elc baz.elc
;;;
;;;	This is done by running 'make -f Emakefile -n' and capturing the
;;;	output in a buffer.  Then, 'eval-current-buffer' is run on that
;;;	buffer.
;;;
;;;	In addition, I wrote a replacement for 'call-process' which makes
;;;	the program's exit code available to the caller.

(defvar emacs-make-program "make"
  "*Name of make program.")

(defvar emacs-makefile-name "Emakefile"
  "*Name of emacs make file.")

(defvar runproc-name "run-process"
  "*Name of the process created by run-process.")

(defvar runproc-process nil
  "Process created by the run-process command, or nil if none exists now.
Note that the process may have been \"deleted\" and still
be the value of this variable.")

(defvar runproc-exit-code 0
  "*Exit code from run-process.")

;; NOTE!!!! You may have to tweak the value of 'runproc-buffer-size'
;;          (below) for your system.  If you use a value that is too
;;	    large, the 'run-process' calls will hang indefinitely.  You
;;	    should set this to be as large as possible for best performance.
;;	    I suspect that the size should be the same as the number
;;	    of characters your system will buffer in a pipe.
(defvar runproc-buffer-size 2048
  "*Buffer size used in run-process text piping.
You may have to readjust this for your system.
If it's too large, the 'send-process-string' call
will block forever.")

(defvar runproc-display nil
  "*Flag controlling whether the run-process filter echos.")

(defvar runproc-buffer nil
  "*The buffer used by run-process.")

(defun emacs-make ()
  "Performs a make inside of emacs."
  (interactive)
;; Create a buffer, and then run 'make -f Emakefile -n' through 'run-process'
;; to capture the output, which is assumed to be emacs lisp functions.
;; Then, evaluate the buffer.  Encapsulate the functions output through
;; 'make' inside of an '(and ...)' function to ensure that the make will
;; stop once one of the functions fails.
  (let ((makebuffer (get-buffer-create "*emacs-make*")) (save-flag))
    (save-excursion
      (set-buffer makebuffer)
      (erase-buffer)
      (insert "(and\n")
      (if (run-process
		 emacs-make-program nil t nil "-f" emacs-makefile-name "-n")
	  (progn
	    (insert ")\n")
	    (eval-current-buffer))
	(message "%s failed: exit code %d"
		 emacs-make-program runproc-exit-code)))))

(defun run-process (program &optional infile outbuf display &rest args)
  "This function is called with the same arguments as 'call-process'.
The only difference is that the process's exit code is made available
to the caller in the variable 'runproc-exit-code'.  If the process
terminates with a non-zero exit code, this function returns nil;
otherwise, it returns t."
;; If outbuf is 0, just run it through 'call-process', since we can't
;; capture the exit code anyway.
  (if (and (not (null outbuf)) (eq outbuf 0))
      (progn
	(setq runproc-exit-code nil)
	(apply 'call-process program infile outbuf display args))
;; Set the 'input' variable to a string containing the standard input as
;; read from the file.  Then, pass this input and the other arguments to
;; 'run-process-string'.
    (let ((input nil))
      (if (not (null infile))
	  (save-excursion
	    (goto-char (point-max))
	    (insert-file-contents infile)
	    (setq input (buffer-substring (point) (point-max)))
	    (delete-region (point) (point-max))))
      (apply 'run-process-string program input outbuf display args))))

(defun run-process-region
  (start end program &optional delete outbuf display &rest args)
  "This function is called with the same arguments as 'call-process-region'.
The only difference is that the process's exit code is made available
to the caller in the variable 'runproc-exit-code'.  If the process
terminates with a non-zero exit code, this function returns nil;
otherwise, it returns t."
  (setq runproc-exit-code 0)
;; If outbuf is 0, just run it through 'call-process', since we can't
;; capture the exit code anyway.
  (if (and (not (null outbuf)) (eq outbuf 0))
      (progn
	(setq runproc-exit-code nil)
	(apply 'call-process-region
	       start end program delete outbuf display args))
;; Set the 'input' variable to a string containing the standard input as
;; contained in the specified region.  Optionally delete the region, depending
;; on the setting of the 'delete' variable.  Then, pass this input and the
;; other arguments to 'run-process-string'.
    (let ((input nil))
      (and
       (not (null start))
       (not (null end))
       (setq input (buffer-substring start end)))
      (if delete
	  (delete-region start end))
      (apply 'run-process-string program input outbuf display args))))

(defun run-process-string
  (program &optional string outbuf display &rest args)
  "This function runs PROGRAM with its standard input coming from STRING.
The rest of its arguments are the same as their counterparts in
'call-process'."
;; Initialize the exit code and set the output buffer as does 'call-process'.
  (setq runproc-exit-code nil)
  (let ((len) (string-size) (input string) (background nil))
    (if (eq outbuf t)
	(setq outbuf (current-buffer)))
;; If a process is already running, ask the user if it should be killed.
;; If not, terminate now; otherwise, kill it and continue.
    (if runproc-process
	(if (or (not (eq (process-status runproc-process) 'run))
		(yes-or-no-p
		   "A 'run-process' process is running; kill it? "))
	    (condition-case ()
		(progn
		  (interrupt-process runproc-process)
		  (sit-for 1)
		  (delete-process runproc-process))
	      (error nil))
	  (error "Cannot have two 'run-process' processes")))
;; Start the process.
    (setq runproc-process
	  (apply 'start-process runproc-name nil program args))
;; If the output is to be captured, register a filter function.
    (if (not (null outbuf))
	(progn
	  (setq runproc-display display)
	  (setq runproc-buffer outbuf)
	  (set-process-filter runproc-process 'runproc-filter)))
;; If there is input, pass it to the process.  The input must be fed
;; in blocks of size no greater than 'runproc-buffer-size'.  The value
;; of this variable might be system-dependent, so you might have to
;; tweak it (where it is defined above) to get this to work.  If the
;; value of this variable is too large, this call will block indefinitely.
    (if input
	(while (> (setq len (length input)) 0)
	  (progn
	    (setq string-size (min len runproc-buffer-size))
	    (process-send-string runproc-process
				 (substring input 0 string-size))
	    (accept-process-output)
	    (setq input (substring input string-size)))))
;; Send eof to the process, even if it didn't ask for input.
    (process-send-eof runproc-process)
    (accept-process-output)
;; Wait for the process to terminate.  Once it does, capture its exit code
;; and return to the caller.
    (while (not (equal (process-status runproc-process) 'exit))
      (progn
	(sleep-for 1)
	(accept-process-output)))
    (eq 0 (setq runproc-exit-code (process-exit-status runproc-process)))))

(defun runproc-filter (proc string)
  "Filter function for the 'run-process' calls."
;; Do nothing unless a buffer is defined.
  (if (not (null runproc-buffer))
      (progn
	(set-buffer runproc-buffer)
	(insert string)
;; If the 'display' flag was set in the 'run-process' call, refresh the screen
;; after the characters have been inserted.
	(if runproc-display
	    (sit-for 0)))))

;---------------------------- end of enclosed code ----------------------------
;
;Thanks again to all of yo for your help and suggestions.
;
;--
;  Lloyd Zusman                        uucp:  ...!ames!fxgrp!ljz
;  Master Byte Software	          Internet:  unreliable, but try these:
;  Los Gatos, California                      ljz%fx.com@ames.arc.nasa.gov
;  "We take things well in hand."             fxgrp!ljz@ames.arc.nasa.gov
