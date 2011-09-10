;;; mew-temacs.el --- Environment of Text Emacs for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Oct 13, 1997
;; Revised: Aug 30, 1999

;;; Code:

(defconst mew-temacs-version "mew-temacs.el version 0.04")

(defvar mew-icon-separate-spec nil)
(defvar mew-icon-blank nil)
(defvar mew-icon-audio nil)
(defvar mew-icon-image nil)
(defvar mew-icon-video nil)
(defvar mew-icon-application/postscript nil)
(defvar mew-icon-application/octet-stream nil)
(defvar mew-icon-message/rfc822 nil)
(defvar mew-icon-message/external-body nil)
(defvar mew-icon-text nil)
(defvar mew-icon-multipart nil)
(defvar mew-icon-unknown nil)

(defmacro mew-summary-toolbar-update () nil)
(defmacro mew-draft-toolbar-update () nil)

(defmacro mew-message-set-end-of-message ()
  '(mew-overlay-put mew-message-overlay
		    'before-string
		    mew-end-of-message-string))

(defmacro mew-message-set-end-of-part ()
  '(mew-overlay-put mew-message-overlay
		    'before-string
		    mew-end-of-part-string))

(defmacro mew-message-set-end-of-nil ()
  '(mew-overlay-put mew-message-overlay 'before-string nil))

(provide 'mew-temacs)

;;; Copyright Notice:

;; Copyright (C) 1997, 1998, 1999 Mew developing team.
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. Neither the name of the team nor the names of its contributors
;;    may be used to endorse or promote products derived from this software
;;    without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE TEAM AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE TEAM OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; mew-temacs.el ends here
