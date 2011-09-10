;;; -*- Mode: LISP; Syntax: Common-lisp; Package: ENLIB; Base: 10 -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BOTH SIDES:
;;;
;;; 10801: AN INVENTED NUMBER>1024 WHICH IS HOPEFULLY NOT ALREADY IN USE.
;;; Check /etc/services and the Namespace Def for current site.

(tcp:add-tcp-port-for-protocol :rpc-hm-protocol 10801)
