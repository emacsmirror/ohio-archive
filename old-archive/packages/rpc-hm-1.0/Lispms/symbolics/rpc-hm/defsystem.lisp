;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-

;;;
;;; System declaration for the Remote Procedure Call Facility.
;;;

(defsystem RPC-HM
    (:pretty-name "Home Made Remote Procedure Call Programmer Interface"
     :short-name "RPC-HM"
;;; Consider commenting away this restriction which should be
;;; automatically taken care of by the module declaration below:
     :required-systems ("ip-tcp")
     :default-pathname "rpc-hm-host:rpc-hm-dir;"
     :default-package COMMON-LISP-USER
     :default-destination-pathname
     "rpc-hm-host:rpc-hm-dir;binaries;"
;;; Change to NIL if version control is unwanted:
     :maintain-journals t
     :installation-script
     "installation-script"
     :before-patches-initializations
     nil
     :initial-status :released)

;;; Consider autoloading ip-tcp by uncommenting the module
;;; declaration below:
; (:module basic-networking ip-tcp (:type :system))

  (:serial
;   basic-networking
    "rpc-hm-host:general-library;defpackage"
    "rpc-hm-host:general-library;background-eval"
    "rpc-globals"
    "rpc-server"
    "rpc-server-and-client"
    "rpc-client")
  (:module text-files
   ("rpc-doc"
    "installation-script.lisp")
   (:type :text-data)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
