;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-

;;; Put here the forms to execute to run the CP command Install System.

;; For instance:
(cp:execute-command
  "Add Services to Hosts"
  "all"
  :service "RPC-HM" :medium "TCP" :protocol "RPC-HM-PROTOCOL")
