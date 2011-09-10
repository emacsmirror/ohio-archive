;From ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!uwm.edu!cs.utexas.edu!mailrus!accuvax.nwu.edu!krulwich Sat May 12 23:40:50 EDT 1990
;Article 1917 of comp.emacs:
;Path: ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!uwm.edu!cs.utexas.edu!mailrus!accuvax.nwu.edu!krulwich
;From: krulwich@ils.nwu.edu (Bruce Krulwich)
;Newsgroups: comp.emacs
;Subject: PINGing other machines
;Message-ID: <7477@accuvax.nwu.edu>
;Date: 9 May 90 17:26:32 GMT
;Sender: news@accuvax.nwu.edu
;Reply-To: krulwich@ils.nwu.edu (Bruce Krulwich)
;Organization: Institute for the Learning Sciences, Northwestern University, Evanston, IL 60201
;Lines: 42
;
;The GNU emacs-lisp code below can be used to check if another machine is up
;and if the network connection to it is up.  It does this by spawning a
;background job and notifying you as soon as it gets an answer.  I've found
;it useful in an environment where the network is sometimes flakey and
;machines that I have to connect to sometimes go down.
;
;Send any comments and/or suggestions to krulwich@ils.nwu.edu.


;;;  --------------------------------------------------------------------------

;;;  PING -- Do a background ping of another machine
;;;  Bruce Krulwich
;;;  Institute for the Learning Sciences
;;;  3/4/90

(defun ping (host)
  (interactive "sHost to ping (in background): ")
  (if (get-process "ping")
      (error "Previous ping hasn't finished yet")
      (let ((proc
	     (start-process "ping" nil "ping" host) ))
	(set-process-filter proc 'ping-filter)
	(set-process-sentinel proc 'ignore)
	)))

;;; The INSERT is instead of MESSAGE so that user keystrokes won't erase it
;;; as soon as it's displayed.
(defun ping-filter (proc msg)
  (save-excursion
    (set-buffer (window-buffer (minibuffer-window)))
    (insert (substring msg 0 (1- (length msg))))
    ))

;;;  --------------------------------------------------------------------------


;Enjoy,
;
;Bruce

 


