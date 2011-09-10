;;; -*- Mode: LISP; Syntax: Common-lisp; Package: ENLIB; Base: 10 -*-

;Author: Eyvind Ness (eyvind) 
;Date:   Sunday, May 3 1992 18:38 GMT
;File:   /tmp/remulus:eyvind.rpc-hm;rpc-server.lisp (ftp)a15398

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SERVER SIDE:

(defun rpc-server-function ()
  "The Home Made Remote Procedure Call Server Function"
  (OR
    (ticl:IGNORE-ERRORS
      ;; IGNORE ALL UNKNOWN COMMUNICATION PROBLEMS.
      (with-open-stream
	(stream
	  (net:listen-for-connection-on-medium
	    :tcp
	    "RPC-HM"
	    :timeout nil
	    :timeout-after-open nil))
	(let
	  ((comm-outcome
	     (ticl:with-timeout
	       ((* *rpc-hm-server-timeout* 60) nil)
	       (let (step-1 step-2)
		 (setq
		   step-1
		   (with-errors-and-io-trapped (eval (READ-FROM-STRING (read stream)))))
		 ;; REGARDLESS OF EVAL OUTCOME, TRY TO RETURN THE
		 ;; OUTPUT FROM STEP-1 TO THE CALLER:
		 (setq
		   step-2
		   (with-errors-and-io-trapped (prin1 (second step-1) stream)))
		 ;; CHECK FOR POSSIBLE PRINTING PROBLEMS:
		 (cond ((not (first step-2))
			;; THERE IS NO POINT IN TRYING TO DECODE PROBLEMS
			;; HERE. IF THIS GO WRONG, WE ARE IN SERIOUS TROUBLE:
			(ticl:ignore-errors
			  ;; (ticl:send stream :clear-output)
			  (clear-output stream)
			  (prin1
			    (format
			      nil
			      "Unable to transfer results - see *RPC-HM-SERVER-INFO* on ~A"
			      (machine-instance))
			    stream)))
		       (t (second step-1)))))))
	  (setq
	    *rpc-hm-server-info*
	    (or
	      comm-outcome
	      (format
		nil
		"RPC HM Server timed out or cancelled a request from ~A"
		(ticl:send stream :foreign-host)))))))
    (SETQ
      *rpc-hm-server-info*
      (FORMAT
	nil
	"RPC HM Server died after detecting uncorrectable TCP-IP problems."))))

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
