;Return-Path: <info-gnu-emacs-request@prep.ai.mit.edu>
;Date: 1 May 89 21:51:11 GMT
;From: littlei!omepd!merlyn@uunet.uu.net  (Randal L. Schwartz @ Stonehenge)
;Organization: Stonehenge; netaccess via BiiN, Hillsboro, Oregon, USA
;Subject: see-chars for GNU
;Sender: info-gnu-emacs-request@prep.ai.mit.edu
;To: info-gnu-emacs@prep.ai.mit.edu
;
;Ever wonder what characters a function key sends out, or one of those
;other "unknown" keys like "scroll right"?  Well, even if you don't
;have one of those problems today, tuck this little goody away, 'cause
;it'll help you figure that out when the time comes.
;
;`see-chars' accepts *any* characters (including a C-g) until a
;three-second timeout has passed.  It then tosses up a human-readable
;display of the characters entered.
;
;/=Randal L. Schwartz, Stonehenge Consulting Services (503)777-0095===\
;{ on contract to BiiN, Hillsboro, Oregon, USA, until 30 May 1989     }
;{ <merlyn@intelob.intel.com> ...!uunet!tektronix!biin!merlyn         }
;{ or try <merlyn@agora.hf.intel.com> after 30 May 1989               }
;\=Cute quote: "Welcome to Oregon... home of the California Raisins!"=/
;
;================================================== snip snip ==========
;;; original by merlyn -- LastEditDate = "Mon Apr 10 15:45:46 1989"

(defun see-chars ()
  "Displays characters typed, terminated by a 3-second timeout."
  (interactive)
  (let ((chars "")
	(inhibit-quit t))
    (message "Enter characters, terminated by 3-second timeout...")
    (while (not (sit-for 3))
      (setq chars (concat chars (list (read-char)))
	    quit-flag nil))		; quit-flag maybe set by C-g
    (message "Characters entered: %s" (key-description chars))))

