;;; -*- Mode: LISP; Syntax: Common-lisp; Package: ENLIB; Base: 10 -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CLIENT SIDE:

(export '(rpc-basic rpc))

(net:define-protocol :rpc-hm-protocol (:rpc-hm :tcp)
  (:invoke (communication-channel)
    (with-open-stream
      (stream (neti:get-connection-for-service communication-channel))
      (prin1
	(format nil
		"(enlib:with-errors-and-io-trapped ~S)"
		(car
		  ;; ONLY 1 ARG (THE FORM) XFERRED:
		  (neti:service-access-path-args communication-channel)))
	stream)
      (finish-output stream)
	     
      (prog1
	(read-from-string (read stream))
	(CLOSE stream)))))

;;;
;;;
;;; Basic RPC interface:
;;;

(defun rpc-basic (host-name
		  a-form
		  &optional
		  (timeout *rpc-hm-client-timeout*)
		  (max-no-of-retries *rpc-hm-client-max-no-of-retries*))
  (block outer
    (let ((retry-no 0)
	  comm-outcome)
      (loop
	(setq
	  comm-outcome
	  (process:with-timeout
	    (timeout)
	    ;; THE BODY OF THIS MACRO MUST NOT RETURN NIL, OR ELSE IT
	    ;; WOULD BE IMPOSSIBLE TO DISTINGUISH THE TIMEOUT CASE
	    ;; FROM A SUCCESSFUL CALL.
	    
	    ;; THERE IS NO POINT IN TRYING TO DECODE COMMUNICATION
	    ;; PROBLEMS.
	    
	    (or
	      (scl:ignore-errors
		;; Returns NIL as first val when errors.
		(LET ((ans (net:invoke-service-on-host :rpc-hm (si:parse-host host-name) a-form)))
		  (LIST (FIRST ans) (READ-FROM-STRING (SECOND ans)) (THIRD ans))))
	      (list
		nil
		(format
		  nil
		  "General communication problem. Check args (~A ~A) for validity and spelling."
		  host-name a-form)))))
	
	(when comm-outcome
	  (return-from outer comm-outcome))
	(when (>= retry-no max-no-of-retries)
	  (return-from
	    outer
	    (list
	      nil
	      (format
		nil
		"Host ~A not responding"
		host-name))))
	(setq retry-no (1+ retry-no))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CLIENT FUNCTION FOR CONVENIENT SERVICE INVOCATION IN INTERACTIVE
;;; APPLICATIONS:

(defun rpc (host-name a-form)
  (let ((result
	  (rpc-basic host-name a-form)))
    (cond
      ((first result)
       (second result))
      (t
       (progn
	 (cerror
	   "Retry call using a different form"
	   (format
	     nil
	     "Evaluation failed on remote computer ~A:~%~3t~A"
	     (string-upcase host-name)
	     (second result)))
	 (rpc
	   host-name
	   (scl:accept
	     'sys:form
	     :prompt "Supply a new form to use instead"
	     :default a-form)))))))
