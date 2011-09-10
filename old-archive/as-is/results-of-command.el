;;;From: rberlin%birdland@Sun.COM (Rich Berlin)
;;;Subject: Re: A find-file Hack
;;;Date: 25 Oct 88 20:59:18 GMT

;;;By the way, I've had other occasions when I wanted the output of a command,
;;;so I wrote a function which, rather than applying 'message to the buffer
;;;contents, returns them as a string.  It's easy to then pass the string to
;;;message:

(defun results-of-command (command &rest args)
  "Execute the given COMMAND with &rest ARGS, and return the resulting\n\
output as a string."
  (let (foo
	(temp-buffer-show-hook
	 '(lambda (buffer)
	    (set-buffer buffer)
	    (setq foo (buffer-substring (point-min) (1- (point-max))))
	    (kill-buffer buffer))))
    (save-excursion
      (with-output-to-temp-buffer "*results of command*"
	(apply 'call-process command nil standard-output nil args))
    foo)))

;;;-- Rich
