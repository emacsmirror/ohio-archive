;;; -*- Mode: LISP; Package: USER; Syntax: Common-Lisp -*-

;Author: Eyvind Ness (eyvind) 
;Date:   Sunday, May 3 1992 12:33 GMT
;File:   /usr/ife/copma/online/lisp/enlib/src/defpackage.lisp

#-lispm
(in-package "USER")

(#+genera future-common-lisp:defpackage #-genera defpackage "EN-LIBRARY"
  (:size 256)
  (:nicknames "ENLIB")
  ;; #+explorer
  ;; (:shadowing-import-from "CLEH" "HANDLER-CASE")
  ;; (:shadowing-import cleh:handler-case)
  #-lucid
  (:use "COMMON-LISP")
  #+lucid
  ;; "(:use "LISP" "LUCID-COMMON-LISP") )"
  (:use "LISP") )
