;To: unix-emacs@bbn.com
;Date: 17 Feb 89 23:04:12 GMT
;From: "Randal L. Schwartz @ Stonehenge" <ogccse!littlei!omepd!merlyn@husc6.harvard.edu>
;Subject: Re: Copy From Above?
;
;In article <MGH.89Feb17085945@intelob.biin.com>, mgh@intelob (Mike Heavin) writes:
;| how about the following:
;| 
;| (defun copy-char-above-cursor ()
;| "Copy the character immediately above the cursor onto the current line."
;|   (interactive)
;|   (point-to-register 1)
;|   (setq temp-column (current-column))
;|   (previous-line 1)
;|   (beginning-of-line)
;|   (forward-char temp-column)
;|   (setq temp-start-region (dot-marker))
;|   (forward-char 1)
;|   (setq temp-end-region (dot-marker))
;|   (copy-to-register 0 temp-start-region temp-end-region)
;|   (register-to-point 1)
;|   (insert-register 0 1)
;| )
;| 
;
;Which is probably vaguely equivalent to (without all the smoke and mirrors):

(defun copy-char-above-cursor ()
  "Copy the character immediately above the cursor onto the current line."
  (interactive)
  (insert (save-excursion
	    (move-to-column (prog1 (current-column)
			      (forward-line -1)))
	    (char-after (point)))))

;[warning... untested code... compile at your own risk]
;
;"Variables?  What are variables?"
;-- 
;Randal L. Schwartz, Stonehenge Consulting Services (503)777-0095
;on contract to BiiN (for now :-), Hillsboro, Oregon, USA.
;ARPA: <@intel-iwarp.arpa:merlyn@intelob> (fastest!)
;MX-Internet: <merlyn@intelob.intel.com> UUCP: ...[!uunet]!tektronix!biin!merlyn
;Standard disclaimer: I *am* my employer!
;Cute quote: "Welcome to Oregon... home of the California Raisins!"
