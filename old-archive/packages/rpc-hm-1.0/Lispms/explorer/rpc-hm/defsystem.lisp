;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-

;Author: Eyvind Ness (eyvind) 
;Date:   Thursday, May 7 1992 12:37 GMT
;File:   /tmp/remulus:eyvind.rpc-hm;defsystem.lisp (ftp)a15398

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; System declaration for the Remote Procedure Call Facility.
;;;

(defsystem RPC-HM
  (:name "Home Made Remote Procedure Call Programmer Interface")
  (:short-name "RPC-HM")
  (:pathname-default "rpc-hm-host:rpc-hm-dir;")
  ;; (:package ENLIB)
  (:default-output-directory "rpc-hm-host:rpc-hm-dir.binaries;")
  (:patchable nil)
  (:initial-status :released)
  (:serial t)
  (:module zero "rpc-hm-host:general-library;defpackage")
  (:module one ("rpc-texas-aux" "rpc-globals"))
  (:module two "rpc-hm-host:general-library;background-eval")
  (:module three ("rpc-server" "rpc-client"))
  (:module four "unix-rpc-udp-server")
  (:module five "rpc-server-and-client")
  (:compile-load zero)
  (:compile-load one)
  (:compile-load two)
  (:compile-load three)
  (:compile-load four)
  (:compile-load five))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Recompile system with:
;;; (make-system 'rpc-hm :recompile :noconfirm :record)

;;;  Keywords from Symbolics not avail on TI:
;;;
;;;     :required-systems ("ip-tcp")
;;;     :maintain-journals nil
;;;     :before-patches-initializations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
