;;; -*- Mode: LISP; Syntax: Common-lisp; Package: ENLIB; Base: 10 -*-

;Author: Eyvind Ness (eyvind) 
;Date:   Sunday, May 3 1992 18:38 GMT
;File:   /tmp/remulus:eyvind.rpc-hm;rpc-server.lisp (ftp)a15398


(defun accept (presentation-type &key prompt default)
  "Texas doesn't even have an ACCEPT function - here's a simple substitute:"
  (declare (ignore presentation-type default))
  (ticl:prompt-and-read :read "~A" prompt))
