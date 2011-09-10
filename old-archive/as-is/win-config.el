;To: unix-emacs@bbn.com
;Date: 28 Oct 88 00:14:38 GMT
;From: Dave Emme <oliveb!amdahl!daveemme@ames.arc.nasa.gov>
;Subject: Re: Saving window configurations
;
;In article <41546@yale-celray.yale.UUCP> Ram-Ashwin@cs.yale.edu (Ashwin Ram) writes:
; >Is there a way to save the current window configuration (as returned by
; >(current-window-configuration)) in a file, such that loading that file would
; >restore that window configuration?  I know I can save it in a variable and
; >later use set-window-configuration to restore it in the same session, but I'd
; >like to be able to restore a particular window configuration in later
; >sessions too.
;
;I realize that the following isn't what was asked for (I'd like that
;ability, too!), but here is some code I've put together to support saving
;and restoring of Gnu Emacs window configurations *within* a session.  You can
;push and pop configurations, or give them names and restore by name (in any
;order). 
;
;- Dave
;
;------------------------------ Cut Here ------------------------------

(defvar window-configuration-stack nil
  "The stack used by functions `push-window-configuration' and
`pop-window-configuration'.")

(defun push-window-configuration ()
  "Push the current window configuration onto the configuration stack.
It can be retrieved later using `pop-window-configuration'."
  (interactive)
  (setq window-configuration-stack
	(cons (current-window-configuration) window-configuration-stack)))

(defun pop-window-configuration ()
  "Pop the top window configuration off of the stack and adjust windows
accordingly.
Window configurations are saved on the stack using 
`push-window-configuration'."
  (interactive)
  (if window-configuration-stack
      (progn (set-window-configuration (car window-configuration-stack))
	     (setq window-configuration-stack (cdr window-configuration-stack)))

    (error "The window configuration stack is empty.")))


(defvar named-window-configurations nil
  "List of named window configurations used by commands
`save-window-configuration' and `restore-window-configuration'.")

(defun save-window-configuration (name)
  "Save the current window configuration as NAME.
It can be later retrieved using `restore-window-configuration'."
  (interactive "sName for window configuration: ")
  (let ((x (assoc name named-window-configurations)))
    (if (or (null x)
	    (if (y-or-n-p "Name is currently used; reassign? ")
		(progn
		  (setq named-window-configurations
			(delq x named-window-configurations))
		  t)))
	(setq named-window-configurations
	      (cons (cons name (current-window-configuration))
		    named-window-configurations)))
    (message "")))


(defun restore-window-configuration ()
  "Restore (make current) the window configuration NAME, which was
saved using `save-window-configuration'."
  (interactive)
  (set-window-configuration
   (cdr (assoc (completing-read "Window configuration name: "
				named-window-configurations
				nil 'force)
	       named-window-configurations))))
;-- 
;------------------------------------------------------------------------------
;	(Insert disclaimer of your choice here)
;
;Dave Emme  (M/S 316)                    daveemme@uts.amdahl.com
;Amdahl Corporation            {ames, decwrl, sun, uunet}!amdahl!daveemme
;1250 E. Arques Ave.
;Sunnyvale, CA 94088;