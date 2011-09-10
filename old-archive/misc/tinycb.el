;; @(#) tinycb.el -- (C)irculate (b)uffers easily.

;; Copyright (C) 1996 Jari Aalto

;; @(#) Author: Jari Aalto <jari.aalto@ntc.nokia.com>
;; @(#) Maintainer: Jari Aalto <jari.aalto@ntc.nokia.com>
;; @(#) Keywords: tools
;; @(#) $KnownCompatibility: 18.57 - 19.30+ $

;; This file is not part of GNU Emacs.

;; LCD Archive Entry:
;; tinycb|Jari Aalto|jari.aalto@ntc.nokia.com|
;; Circulate buffers easily in current window. Back, fwd, skipping unwanted.|
;; 09-May-1996|1.3|~/misc/tinycb.el.gz|

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;  Add following statement to your ~/.emacs
;;
;;	(require 'tinycb)
;;  or
;;	(autoload 'ticb-previous-buffer "tinycb" t t)
;;	(autoload 'ticb-next-buffer     "tinycb" t t)
;;
;;  If you don't want default bindings, clear the installation with
;;  following. This must be prior the 'require command.
;;
;;	(setq ticb-load-hook nil)
;;
;;  To circulate buffers forward/backward easily, this file
;;  installs following X keybindings
;;
;;	C-,	-- previous buffer
;;	C-.	-- Next buffer
;;
;;  You should also configure ticb-ignore-regex to your taste.

;;; Change Log:

;;; Code:


(defvar tick-version
  "$Id: tinycb.el,v 1.3 1996/05/09 07:35:19 jaalto Exp $"
  "Full RCS Version id string.")


(defvar ticb-load-hook '(ticb-install)
  "*Hook run when file has been loaded.")


(defvar ticb-ignore-regexp
  (concat
   "^ "                 ;hidden buffers
   "\\|completion\\|summary"
   "\\|buffer list\\|help\\|ispell\\|abbrev"
   "\\|temp\\|tmp\\|vc\\|compile-log\\|occur"
   )
  "*Buffers to ignore when changing to another.")


;;; ----------------------------------------------------------------------
;;;
(defun ticb-next (list)
  "Switch to next buffer in list, skipping unwanted ones.
See variable ticb-ignore-regexp."
  (let* ((re  ticb-ignore-regexp)
         buffer go
         )
    (while (and list (null go))
      (setq buffer (car list))
      (if (string-match re (buffer-name buffer))                ;skip over
          (setq list (cdr list))
        (setq go buffer)))
    (if go   (switch-to-buffer go))
    ))


;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun ticb-previous-buffer ()
  "Switch to previous buffer in current window."
  (interactive)
  (ticb-next (reverse (buffer-list))))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun ticb-next-buffer ()
  "Switch to the other buffer (2nd in list-buffer) in current window."
  (interactive)
  (bury-buffer (current-buffer))
  (ticb-next (buffer-list)))


;;; ----------------------------------------------------------------------
;;;
(defun ticb-install ()
  "Define global keys Ctrl-,  and Ctrl-, to circulate buffers."

  ;;  This breaks in 18.57 -nw, so just ignore the errors,
  ;;  user must define his own preference keys.
  ;;
  (condition-case nil
      (progn
	(global-set-key [?\C-,] 'ticb-previous-buffer)
	(global-set-key [?\C-.] 'ticb-next-buffer)
	)
    (error nil))
  )

(provide 'ticb)
(run-hooks 'ticb-load-hook)

;;; tinycb.el ends here
