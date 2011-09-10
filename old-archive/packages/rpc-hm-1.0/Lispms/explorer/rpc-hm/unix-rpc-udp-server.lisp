;;; -*- Mode:LISP; Package:ENLIB; Syntax:COMMON-LISP; Base:10 -*-

;Author: Eyvind Ness (eyvind) 
;Date:   Thursday, May 7 1992 12:19 GMT
;File:   /tmp/remulus:eyvind.rpc-unix;general-lisp-server.lisp (ftp)a15398


;;; The server part of a UDP based RPC service called RPC HM. This code
;;; is for the Texas Instruments Explorer Lisp machines.

(export
 '(
   *guls-incoming-request*
   *spawning-rpcsvc*
   *srpc-dispatcher-conditions*
   *srpc-input-arg*
   general-udp-lisp-server
   general-udp-lisp-server-with-timeout
   restart-spawning-rpc-dispatcher
   set-up-udp-rpc-server
   spawning-rpc-dispatcher
   spawning-rpc-udp-program-number
   ))


(lisp:provide 'general-lisp-server-for-rpc)

;;; If the basic system networking software isn't loaded, then load it:
(lisp:require 'ip)
(lisp:require 'rpc)

;;; 
;;; If, for some reason, the RPC UDP Port Mapper isn't running, eval
;;; the following expression:
;;;    (rpc:start-port-map-server :udp)
;;; Use Peek (select-p) to check whether it is running or not.
;;;
;;; To debug turn on RPC message tracing:
;;; (setq rpc:*rpc-msg-trace-p* t)
;;; or, more detailed: (setq rpc:*rpc-buffer-trace-p* t)

;;;
;;; Use the cache option for getting foreign port map addresses:

(setq rpc:*pmap-getport-cache-p* t)

;;; This way we might save one extra call to the remote computer.


;;; Here is a simple-minded macro for easy server configuration:

(DEFMACRO set-up-udp-rpc-server
    (progno &optional &key
	    (versno 1)
	    (procid 1)
	    (server-function ''general-udp-lisp-server)
	    (name (symbol-name (gentemp "LISTENER-")))
	    (receive-whostate "Awaiting Call"))
  "To set up an RPC server for some application do the following:

* Pick a program number in the interval #x20000000 - #x3FFFFFFF
  This number uniquely identifies your RPC server.
  On Unix computers you should consult the file /etc/rpc to find
  an unused number.
* Select version numbers and procedure numbers.  Normally 1 is used
  for both.
* Use this macro to set up a dedicated server process for your
  application.  You may also do this step yourself by calling
  REGISTERRPC manually.
* You are done!  Your client RPC process is supposed to send an
  ascii-string containing the application specific code to be 
  executed in the server process.  The server then returns the 
  result of the evaluation back to the client as an ascii-string.
  Your own protocol can easily be built on top of this by using 
  other call format conventions for your application server.

By using the macro WITH-ERRORS-AND-IO-TRAPPED you may also employ your
own error-handling conventions in place of the default one.

Here is a conceptual picture of this scheme of RPC serving as 
offered through this interface:

RPC application      General RPC             Application
server processes     server function         functions

[program number]                          [corresponding function]
[program number]                          [corresponding function]
[program number]         \\     /          [corresponding function]
[program number]          \\   /           [corresponding function]
[program number]           \\ /            [corresponding function]
[program number]       -----X-----        [corresponding function]
[program number]           / \\            [corresponding function]
[program number]          /   \\           [corresponding function]
[program number]         /     \\          [corresponding function]
[program number]                          [corresponding function]
[program number]                          [corresponding function]

See also Appendix B in the Networking Reference Manual."
  
  `(ticl:registerrpc
    ,progno
    ,versno
    ,procid
    ,server-function
    :xdr-ascii-string
    :xdr-ascii-string
    :name ,name
    :receive-whostate ,receive-whostate))


(defvar *guls-incoming-request* 'no-requests-received)

(DEFUN general-udp-lisp-server (string-arg)
  "This function is the default server for handling and dispatching 
incoming requests. Here we only care about the actual results returned
from careful-eval, whether ok or not. Applications which need their 
own error handling should use careful-eval themselves."
  (SECOND 
   (WITH-ERRORS-AND-IO-TRAPPED
       (EVAL
	(READ-FROM-STRING
	 (SETQ *guls-incoming-request* string-arg))))))



(DEFUN general-udp-lisp-server-with-timeout (string-arg)
  "Same as general-udp-lisp-server, but if the execution of FORM takes
more time than the default wait interval for a typical RPC client (~ 5
times 5 seconds), the execution is aborted, and a dummy error message is
returned instead."
  (ticl:with-timeout
      ((* (1- (* 5 5)) 60)
       (prin1-to-string
	(FORMAT nil "The evaluation of ~S timed out." string-arg)))
    (general-udp-lisp-server string-arg)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; More advanced server with its own dispatcher function which doesn't
;;; block other calls when serving a request.

(defvar *srpc-dispatcher-conditions*
  nil 
  "List of conditions catched while inside SPAWNING-RPC-DISPATCHER")

(defvar *srpc-input-arg* ""
  "Last request received")

(defun spawning-rpc-dispatcher (svc-request stream)
  (let ((argument nil))
    (flet ((get-arg-or-return ()
              (when 
	       (null (ticl:send stream 
			   :getargs 
			   :xdr-ascii-string (ticl:locf argument)))
	       (ticl:send stream :svcerr-decode)
	       (return-from spawning-rpc-dispatcher)))
	   (condition-handler (condition)
	       (push condition *srpc-dispatcher-conditions*)
	       (ticl:send stream :svcerr-systemerr)
	       (return-from spawning-rpc-dispatcher)))

       (ticl:condition-bind ((error #'condition-handler))
	 (case (rpc:svc-req-procedure svc-request)
	       (0			
		;; the null procedure convention
		(ticl:send stream :sendreply :xdr-void nil))
	       (1
		;; the real thing
		(get-arg-or-return)
		(setq *srpc-input-arg* argument)
		(ticl:process-run-function
		 "RPC HM UDP Server"
		 #'(lambda (rpcstream rpcarg)
		     (ticl:condition-case (c)
			 (ticl:send rpcstream :sendreply :xdr-ascii-string
			       (funcall
				#'general-udp-lisp-server-with-timeout
				rpcarg))
		       (error
			(push
			 (format
			 nil
			 "Request handler function gave up:~s" c)
			 *srpc-dispatcher-conditions*))))
		 stream argument))
	       (otherwise
		(ticl:send stream :svcerr-noproc)))))))

(defvar *spawning-rpcsvc* nil
  "Instance of type RPC:SERVER for the spawning RPC server process")

(defconstant spawning-rpc-udp-program-number #x20000ffe)

(defun restart-spawning-rpc-dispatcher ()
  (setq
   *spawning-rpcsvc*   
   (progn
     (rpc:kill-server-process 'srpc-server)
     (rpc:make-server-process
      spawning-rpc-udp-program-number 1
      :dispatcher #'spawning-rpc-dispatcher
      :name "Multi-tasking LISP Listener with timeout"
      :run-reason 'start-now
      :server-id 'srpc-server))))
