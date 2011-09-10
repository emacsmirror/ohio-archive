;;; -*- Mode: LISP; Syntax: Common-lisp; Package: ENLIB; Base: 10 -*-

;Author: Eyvind Ness (eyvind) 
;Date:   Sunday, May 3 1992 18:54 GMT
;File:   /tmp/alfa:>eyvind>rpc-hm>rpc-server.lisp (ftp)a15398

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SERVER SIDE:

(net:define-server
  :rpc-hm-protocol		
    (:medium :byte-stream
     ;; :address client-address
     :error-disposition :debugger		; OR :IGNORE WHEN PROPERLY IMPLEMENTED
     :host client-host
     :process-name "RPC HM Server"
     :stream
     (communication-channel
       :ascii-translation nil			; USE THE SYMBOLICS CHARACTER SET
       :accept-p t
       :direction :bidirectional))
  
   (let
     ((comm-outcome
	(process:with-timeout
	  (*rpc-hm-server-timeout*)
	  (let (step-1 step-2)
	    (setq
	      step-1
	      (with-errors-and-io-trapped (EVAL (READ-FROM-STRING (READ communication-channel)))))
	    ;; REGARDLESS OF EVAL OUTCOME, TRY TO RETURN THE
	    ;; OUTPUT FROM STEP-1 TO THE CALLER:
	    (setq
	      step-2
	      (with-errors-and-io-trapped (prin1 (second step-1) communication-channel)))
	    ;; CHECK FOR POSSIBLE PRINTING PROBLEMS:
	    (cond ((not (first step-2))
		   ;; THERE IS NO POINT IN TRYING TO DECODE PROBLEMS
		   ;; HERE. IF THIS GO WRONG, WE ARE IN SERIOUS TROUBLE:
		   (scl:ignore-errors
		     (clear-output communication-channel)
		     (prin1
		       (format
			 nil
			 "Unable to transfer results - see *RPC-HM-SERVER-INFO* on ~A"
			 (machine-instance)))
		     communication-channel))
		  (t (second step-1)))))))
    
     (setq
       *rpc-hm-server-info*
       (or
	 comm-outcome
	 (format
	   nil
	   "RPC HM Server timed out or cancelled a request from ~A"
	   client-host)))))


;;; -------------------------------------------------------------------
;;; Hello, Emacs - here are some hints for you (in case this file is in
;;; the wrong (non-lisp) mode):
;;;
;;; Local Variables:
;;; write-file-hooks: (update-std-header update-sccs-string clpu-make-export-list)
;;; comment-start: "; "
;;; comment-end: ""
;;; comment-start-skip: ";+ *"
;;; End:
;;; -------------------------------------------------------------------
