;;; -*- Mode: LISP; Syntax: Common-lisp; Package: ENLIB; Base: 10 -*-

;Author: Eyvind Ness (eyvind) 
;Date:   Thursday, May 7 1992 12:55 GMT
;File:   /tmp/remulus:eyvind.rpc-hm;rpc-server-and-client.lisp (ftp)a15398


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 10801: check agaist /etc/services on Unix machines.

(net:define-logical-contact-name "RPC-HM" '((:tcp 10801)))
(net:add-server-for-medium :tcp "RPC-HM"
     '(ticl:process-run-function
       "RPC HM TCP Server"
       #'enlib:rpc-server-function))
(net:define-service :rpc-hm () ()
     "This is the Home Made Remote Procedure Call Service")

;;; While we're at it - Kick off the RPC/UDP-based RPC HM server, too:
(restart-spawning-rpc-dispatcher)
