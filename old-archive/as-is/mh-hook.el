;To: unix-emacs@bbn.com
;Date: 15 May 89 20:15:30 GMT
;From: David C Lawrence <csd4.milw.wisc.edu!leah!rpi!rpi.edu!tale@bbn.com>
;Sender: arpa-unix-emacs-request@bbn.com
;Subject: Re: mh-inc-folder-hook
;Reply-To: tale@pawl.rpi.edu
;References: <TALE.89May11183939@imagine.pawl.rpi.edu>ale
;Source-Info:  From (or Sender) name not authenticated.
;
;In <TALE.89May11183939@imagine.pawl.rpi.edu> I gave a hook to clobber
;the " Mail" string when new mail is incorporated by mh-e.  I
;overlooked the fact that " Mail" might not be part of the
;display-time-string because you are either a) don't have any mail or
;b) have mail but Emacs hasn't told you yet.  Mark D. Baushke
;<mdb@kosciusko.ESD.3Com.COM> pointed this out to me and he asked that
;I post the revised version as a bug-fix/enhancement.

(setq mh-inc-folder-hook 
      '(lambda ()
         (if (and (boundp 'display-time-process)
                  (eq 'run (process-status display-time-process))) 
             (let ((real-match-data (match-data)))
               (unwind-protect
                   (setq display-time-string
			 (let ((loc
                                (string-match " Mail" display-time-string)))
			   (if loc
			       (concat 
				(substring display-time-string 0 loc)
				(substring display-time-string (match-end 0)))
			     display-time-string)))
		 (store-match-data real-match-data))
               (set-buffer-modified-p (buffer-modified-p))))))


A couple people have said that protecting the match-data like that was
a good idea so I have this little function now:

(defun save-match-data (&rest forms)
  "Save the match-data while &rest FORMS do some miscellaneous matching.
This is similar in principle to save-excursion and save-restriction."
  (let ((original-match-data (match-data)))
    (unwind-protect
        (while forms
          (eval (car forms))
          (setq forms (cdr forms)))
      (store-match-data original-match-data))))
(put 'save-match-data 'lisp-indent-hook 0)

If you decide to keep that around as being handy, then the hook shortens a
little to this: 

(setq mh-inc-folder-hook 
      '(lambda ()
         (if (and (boundp 'display-time-process)
                  (eq 'run (process-status display-time-process)))
             (save-match-data
               (setq display-time-string
                     (let ((loc (string-match " Mail" display-time-string)))
                       (if loc
                           (concat 
                            (substring display-time-string 0 loc)
                            (substring display-time-string (match-end 0)))
                         display-time-string)))
               (set-buffer-modified-p (buffer-modified-p))))))

;Ashwin informs me that as of 18.52 the hook for inc-folder was not
;there, so this won't work for anyone running 18.52 or earlier.  You
;have to either code in the hook or redefine mh-inc-folder to do the
;stripping by itself.
;
;Dave
;
;--
; (setq mail '("tale@pawl.rpi.edu" "tale@itsgw.rpi.edu" "tale@rpitsmts.bitnet"))
; (error "UUCP not spoken here.  Long bang paths unlikely to get mail replies.")

