;To: unix-emacs@bbn.com
;Date: 16 Dec 88 18:36:35 GMT
;From: John Robinson <jr@bbn.com>
;Subject: Re: Suggestion: An su mode for Emacs???
;
;In article <8812161518.AA20708@uk.ac.lon.rfhsm.ux>, and@ux (Andy Holyer) writes:
;>Dear Net-things,
;>	I've just thought of a possible extra command for emacs: I
;>often find, right in the middle of an emacs session, that I need to do
;>something in SuperUser mode. Is it possible to have a command
;>(su-emacs, say) which asks for the su password, and from then on acts
;>as if it was run by root?
;
;Um, why not switch to your shell buffer and type the 'su -c ...' there?
;
;I decided the function you want should be easy enough to write.  Well,
;it wsa a tad difficult because of having to handle process i/o, but
;here's one that seems to work with limited testing, though it may not
;be optimally speedy.  I liked it enough to bind it to M-#.  It started
;out as the code for shell-command, but little of that is left.  It
;does not have the option of shell-command of inserting the output in
;the curent buffer; that's an exercise for the reader :-).
;
(defun su-command (password command)
  "Prompt for root password and a command, then do the latter as root."
  (interactive "sRoot password: \nsCommand: ")
  (let ((buffer (get-buffer-create "*Shell Command Output*"))
        proc)
    (if (save-excursion
	  (set-buffer buffer)
	  (erase-buffer)
	  (setq proc (start-process "su-emacs" buffer "/bin/su"
				    "-c" command))
	  (goto-char (point-min))
	  (while (not (looking-at "Password:"))
	    (accept-process-output proc)
	    (goto-char (point-min)))
	  (erase-buffer)
	  (send-string proc (concat password "\n"))
	  (while (not (looking-at "\n"))
	    (accept-process-output proc)
	    (goto-char (point-min)))
	  (delete-char 1)
	  (while (not (equal (process-status proc) 'exit))
	    (accept-process-output))
	  (> (buffer-size) 0))
	(set-window-start (display-buffer buffer) 1)
      (message "(Command completed with no output)"))))

;I agree that nothing in what you have proposed is worse than having an
;su at all.  And that already is no less secure than the root password.
;--
;/jr
;jr@bbn.com or bbn!jr

