; From: roland@ai.mit.edu (Roland McGrath)
; Subject: Enable yourself, cretin!
; Date: 26 Dec 90 22:13:11 GMT
; Organization: Hackers Anonymous International, Ltd., Inc. (Applications
; 	welcome)
; 
; Recent discussion about disabled commands prompted me to write this little
; thingie for all you Emacs studs out there:
; 
;; Enable yourself, cretin.
(setq disabled-command-hook 'enable-me)

(defun enable-me (&rest args)
  "Called when a disabled command is executed.
Enable it and reexecute it."
  (put this-command 'disabled nil)
  (message "You typed %s.  %s was disabled.  It ain't no more."
	   (key-description (this-command-keys)) this-command)
  (sit-for 0)
  (call-interactively this-command))
