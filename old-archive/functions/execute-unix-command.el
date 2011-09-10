; Path: dg-rtp!uunet!uunet!cis.ohio-state.edu!tut.cis.ohio-state.edu!unreplyable!garbage
; From: darrylo@HPNMXX.SR.HP.COM (Darryl Okahata)
; Newsgroups: gnu.emacs.help,comp.emacs
; Subject: Re: process exit status
; Date: 4 Jun 91 17:34:10 GMT
; References: <WALT.91Jun4105619@arrisun3.utarl.edu>
; Organization: Gatewayed from the GNU Project mailing list help-gnu-emacs@prep.ai.mit.edu
; 
; In message <WALT.91Jun4105619@arrisun3.utarl.edu> you write:
; 
; > Is there a way to obtain the exit status besides resorting to
; > 'start-process' and using a process-sentinal?  The function I'm
; > writing shouldn't require such hair, but I *must* have the exit
; > status.
; 
;      I wrote the following function eons ago.  It will execute a unix
; command, wait for it to finish, and return the exit status.  Note,
; however, that this is not the world's best written function; if I had to
; re-write it, I'd do it slightly differently.  Also note that the
; function documentation says that a SIGINT will be sent if ^G is pressed.
; This will only work on those machines that do NOT support SIGIO.  I wrote
; this function when HP-UX did not support SIGIO; it does now, and
; pressing ^G to interrupt the subprocess no longer works.  Pressing ^G
; will interrupt Emacs, but the subprocess will continue to grind away in
; the background (this can be easily fixed using `unwind-protect', but I
; don't have the time to modify and test it).
; 
;      -- Darryl Okahata
; 	Internet: darrylo@sr.hp.com
; 
; DISCLAIMER: this message is the author's personal opinion and does not
; constitute the support, opinion or policy of Hewlett-Packard or of the
; little green men that have been following him all day.
; 
; ============================================================================
;; LCD Archive Entry:
;; execute-unix-command|Darryl Okahata|darrylo@sr.hp.com
;; |Execute a command, wait for finish, and return exit status
;; |91-06-04||~/functions/execute-unix-command.el.Z

(defun execute-unix-command (buffer program &rest args)
  "Execute a unix command, wait for it for finish, and return the exit status.
This function creates a process, using BUFFER for output, and waits
for the process to exit.  The name of the program to run is given by
PROGRAM, and any remaining arguments, ARGS, are passed as command line
arguments to the program.  All arguments are strings.

Note that there are no provisions for giving input to the program.  If
you must give input to the program, create a file containing the input
for the process and pass the name of this file to a shell script that
starts up the program with standard input redirected to the given
file.

If BUFFER is nil, any output from the program will be throw away.

If a ^G is pressed while the program is running, the program will be
killed via a SIGINT, and this function will return nil.  All other
keypresses will be flushed.

Upon normal completion of the program, the numeric exit status of the
program will be returned.  Note that this function returns either nil
or a numeric value.
"
  (let (prg (process nil) status (return-status nil))
    (catch 'exit-process
      (progn
	(setq prg (list 'start-process "unix-process" buffer program))
	(if args
	  (if (listp args)
	    (setq prg (append prg args))
	    (setq prg (append prg (list args)))
	  )
	)
	(setq process (eval prg))
	(if (processp process)
	  (progn
	    (process-send-eof process)
	    (while (or (equal (setq status (process-status process))
			      'run)
		       (equal status 'stop))
	      (sit-for 1)
	      (if (input-pending-p)
		(if (equal (read-char) ?\^G)
		  (progn
		    (interrupt-process process t)
		    (throw 'exit-process nil)
		  )
		)
	      )
	    )
	    (setq return-status (process-exit-status process))
	  )
	)
      )
    )
    return-status
  )
)
