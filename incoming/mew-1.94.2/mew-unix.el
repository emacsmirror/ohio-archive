;;; mew-unix.el -- MIME content type for UNIX

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Dec  4, 1997
;; Revised: Aug 30, 1999

;;; Code:

(defconst mew-unix-version "mew-unix.el version 0.03")

;;
;;
;;

(defvar mew-prog-plain    '(mew-mime-text/plain    () nil))
(defvar mew-prog-html     '(mew-mime-text/html     () nil))
(defvar mew-prog-enriched '(mew-mime-text/enriched () nil))
(defvar mew-prog-text     '(mew-mime-text/plain    () nil))
(defvar mew-prog-audio
  (list mew-prog-shell (list mew-prog-shell-arg "cat - > /dev/audio") nil))
(defvar mew-prog-audio2
  (list mew-prog-shell (list mew-prog-shell-arg "cat < /dev/audio") nil))
(defvar mew-prog-gif (if (and window-system mew-xemacs-p
			      (valid-image-instantiator-format-p 'gif))
			 '(mew-mime-image/gif () nil)
		       '("xv" ("-geometry" "+0+0") t)))
(defvar mew-prog-tiff (if (and window-system mew-xemacs-p
			       (valid-image-instantiator-format-p 'tiff));;xxx
			  '(mew-mime-image/tiff () nil)
			'("xv" ("-geometry" "+0+0") t)))
(defvar mew-prog-jpeg (if (and window-system mew-xemacs-p
			      (valid-image-instantiator-format-p 'jpeg))
			  '(mew-mime-image/jpeg () nil)
			'("xv" ("-geometry" "+0+0") t)))
(defvar mew-prog-xwd '("xv" ("-geometry" "+0+0") t))
(defvar mew-prog-xbm (if (and window-system mew-xemacs-p
			      (valid-image-instantiator-format-p 'xbm))
			 '(mew-mime-image/xbm () nil)
		       '("xv" ("-geometry" "+0+0") t)))
(defvar mew-prog-xpm (if (and window-system mew-xemacs-p
			      (valid-image-instantiator-format-p 'xpm))
			 '(mew-mime-image/xpm () nil)
		       '("xv" ("-geometry" "+0+0") t)))
(defvar mew-prog-png (if (and window-system mew-xemacs-p
			      (valid-image-instantiator-format-p 'png))
			 '(mew-mime-image/png () nil)
		       '("xv" ("-geometry" "+0+0") t)))
(defvar mew-prog-bmp '("xv" ("-geometry" "+0+0") t))
(defvar mew-prog-image '("xv" ("-geometry" "+0+0") t))
(defvar mew-prog-mpeg '("mpeg_play" () t))
(defvar mew-prog-rfc822 '(mew-mime-message/rfc822 () nil))
(defvar mew-prog-external-body '(mew-mime-external-body () nil))
(defvar mew-prog-delivery-status '(mew-mime-text/plain () nil))
(defvar mew-prog-postscript '("ghostview" ("-geometry" "+0+0") t))
(defvar mew-prog-pdf '("acroread" ("-geometry" "+0+0") t))
(defvar mew-prog-pgp-keys '(mew-mime-pgp-keys () nil))
(defvar mew-prog-octet-stream '(mew-mime-application/octet-stream () nil))

(provide 'mew-unix)

;;; Copyright Notice:

;; Copyright (C) 1996, 1997, 1998, 1999 Mew developing team.
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

;;; mew-unix.el ends here
