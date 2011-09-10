;;; -*- Mode: LISP; Syntax: Common-lisp; Package: ENLIB; Base: 10 -*-

;Author: Eyvind Ness (eyvind) 
;Date:   Sunday, May 3 1992 18:38 GMT
;File:   /tmp/remulus:eyvind.rpc-hm;rpc-server.lisp (ftp)a15398


(DEFPARAMETER
  *tcp-connect-timeout*
  1200
  "
For compataiblity with Symbolics; the symbol is really in 
the TCP package.")


(DEFPARAMETER
  *rpc-hm-client-timeout*
  (round
    (/ *tcp-connect-timeout* 80))
  "
Default no of seconds before the client gives up hope
for an answer from a server. The idea is to avoid 
explicit debugger invocations from within TCP, which
seems to call the function DBG:DEBUGGER-TOP-LEVEL at
inappropriate times.

 - 1/80 is 1/60 * 3/4 tcp-connect-timeout seconds.")


(DEFPARAMETER
  *rpc-hm-client-max-no-of-retries*
  5
  "
Maximum number of retransmissions of request before
the client gives up.")


(DEFPARAMETER
  *rpc-hm-server-info*
  "Initialized"
  "Transaction outcome of last client request")


(DEFPARAMETER
  *rpc-hm-server-timeout*
  120
  "
Default no of seconds before the server times out the
execution of a request.")
