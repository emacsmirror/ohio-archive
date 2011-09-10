;; @(#) tinyappend.el -- A simple text gathering to buffer utility.

;; @(#) $Id: tinyappend.el,v 1.5 1995/02/27 14:17:52 jaalto Release_3 $
;; @(#) $Keywords: text, collection, documenting $
;; $KnownCompatibility: 18.57 , 19.28 $

;; Copyright (C) 1994,1995 Jari Aalto
;; Author:       Jari Aalto <jaalto@tre.tele.nokia.fi>
;; Maintainer:   Jari Aalto <jaalto@tre.tele.nokia.fi>
;; Created:      March 20th 1994
;; Version:      $Revision: 1.5 $
;; state:        $State: Release_3 $
;;
;; To get information on this program use ident(1) or do M-x tia-version
;;
;; <another developer, put your contact mail here>

;; LCD Archive Entry:
;; tinyappend|Jari Aalto|jaalto@tre.tele.nokia.fi|
;; Gather text cumulatively to *append* buffer with ease|
;; 20-March-1994|1.5|~/misc/tinyappend.el.Z|

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs.  If you did not, write to the Free Software Foundation,
;; Inc., 675 Mass Ave., Cambridge, MA 02139, USA.

;;; Installation:

;; To install tinyappend, put this file on you Emacs-Lisp
;; load path, and put the following in your .emacs:
;;
;;     ;; Normal load
;;     (require 'tinyappend)
;;     (tia-install-keys "g")        ;add this if you want keybindings
;;
;;     ;;  Or use Autoload
;;     (autoload 'tia-end "tinyappend" "" t)
;;     (global-set-key "\C-c="
;;                     '(lambda ()
;;		          (interactive "r") (tia-autoload) (tia-end beg end)))
;;     (setq tia-autoload-hook '(lambda () (tia-install-keys "g")))


;;; Commentary:

;; RESERVERD prefix FOR FUNCTIONS IN THIS FILE
;; - To avoid collisions to other modules I use "tia-" in front of
;;   every function & variable. It stands for '(ti)ny (a)ppend
;; - variable names contain letters '-v-'.
;;
;; PREFACE
;; ========================================
;; - There is nothing fancy, I just wrote this for my own purpose so
;;   that I could gather text from buffers with easy. Later on I could
;;   then peek on that buffer, arrange text etc...
;;
;; - C-x a is handy when appending data to buffer, but it's
;;   annoying that you have to give "buffer name" all the time
;;   This one adds to buffer "*append*" automatically, creating one if
;;   it doesn't exist.
;;
;; CURRENT KEY BINDINGS
;; ========================================
;;    	C-c =   Append to the end
;;    	C-c +	Append to the beginning
;;  	C-c _	underscore, Kill (empty) *append* buffer
;;
;; - My keyboard (HP-UX /X term) just happens to access
;;   '=' without shift, so I chose it. If your kbd accesses '+' without
;;   shift you'd propably like to use it instead for appending to the end.
;; - To install these keys see installation above.
;;


;; HISTORY OF CHANGES
;; ========================================
;; Feb 27	1995	[jaalto]	19.28	v1.5	Release_3
;; - Corrected LCD entry, now supports Autoload.
;;
;; Nov	26	1994	[jaalto]	19.28	v1.3	Release_2
;; - File body rewritten. Naming conventions adjusted.
;;   No functional changes
;; - Right GPL licence changed, documentation polished.
;; - Clearer installation
;;
;; March 20	1994 	[jaalo]		18.57	v1.1	Release_1
;; - First release



;; To do list:
;; ========================================
;; <empty>


(provide 'tinyappend)

;;; Code:
;;; ..... Keybingings ...........................................&bind ...
;;; - I's better to have these behind a function, so that
;;;   user can call them within hooks, if the binding gets lost
;;;   for some reason.

;; install keys only if user wants them
(defun tia-set-keys (key-func)
  "Installs keys."
  (funcall key-func  "\C-c=" 'tia-end)
  (funcall key-func "\C-c+" 'tia-beg)
  (funcall key-func "\C-c_" 'tia-kill)
  )

(defun tia-install-keys (env)
  "Installs 'local or 'global keybindings to ENV"
  (interactive "slocal global both [lgb]: ")
  (cond
   ((string-equal env "l")
    (tia-set-keys 'local-set-key))
   ((string-equal env "g")
    (tia-set-keys 'global-set-key))
   ((string-equal env "b")
    (tia-set-keys 'local-set-key)
    (tia-set-keys 'global-set-key))
   ))



;;; ........................................................ &autoload ...

(defvar tia-v-autoloaded nil
  "Changed to t when autoloaded")

(defvar tia-autoload-hook nil
  "*Run only once, when autoloaded")

;;    You can hook up the installation of keys.
(defun tia-autoload ()
  "Will run tia-autoload-hook when autoloaded."
  (interactive)
  (d! "in autoload")
  (if tia-v-autoloaded nil		;already loaded
    (d! "load")
    (setq tia-v-autoloaded t)
    (run-hooks 'tia-autoload-hook)))




;;; ..... user configurable ................................... &conf ...

(defvar tia-buffer "*append*"
  "Buffer to use for text gathering.")


;;; ..... version notice ................................... &version ...

(defconst tia-version
  "$Revision: 1.5 $"
  "Latest version number.")

(defconst tia-version-id
  "$Id: tinyappend.el,v 1.5 1995/02/27 14:17:52 jaalto Release_3 $"
  "Latest modification time and version number.")

(defconst tia-version-doc
  "tinyappend.el -- A simple text gathering to buffer utility.

First created: March 20th 1994
Author       : Jari Aalto <jaalto@tre.tele.nokia.fi
Maintainer   : Jari Aalto <jaalto@tre.tele.nokia.fi

Suggestions and enchancements welcome!"
  "Brief version and contact info")

;;; ----------------------------------------------------------------------
(defun tia-version ()
  "tinyappend.el information."
  (interactive)
  (let* ((ob (current-buffer))
         (bp (get-buffer-create "*version*"))
         )
      (set-buffer bp)                   ;for insert
      (erase-buffer)
      (switch-to-buffer-other-window bp)
      (insert
       tia-version-doc
       "\n\ncurrent version:\n" tia-version-id)
      (pop-to-buffer  ob)
    ))




;;; ########################################################### &funcs ###

;;; ----------------------------------------------------------------------
;;;
(defun tia-append-to-buffer (beg end &optional arg )
  "Stores region to tia-buffer, default is to the end.
prefix arguments:
  0 = kills buffer    1 = adds to the beginning"
  (let (junk
	(bn tia-buffer)
	(bp))
    (save-excursion
      ;;  The case struct remains more clear, when these are not within it.
      (if (not (eq 0 arg)) (copy-region-as-kill beg end))
      (set-buffer (get-buffer-create bn))
      (point-max)

      (cond
       ;; According to prefix
       ((eq arg 0)			; yank to the beginning
	(kill-buffer bn)
	(message (concat bn " buffer killed")))
       ((eq arg 1)			; yank to the beginning
	(point-min) (yank) (message "*appended BEG*"))
       (t
	(yank) (message "*appended*")))
)))

;;; ----------------------------------------------------------------------
;;;
(defun tia-end (beg end)
  "Stores region to the END of tia-buffer  buffer."
  (interactive "r" ) ; pass point marks as arguments
  (tia-append-to-buffer beg end nil)
)

(defun tia-beg (beg end)
  "Stores region to the BEGINNING of tia-buffer  buffer."
  (interactive "r" ) ; pass point marks as arguments
  (tia-append-to-buffer beg end 1)
)

(defun tia-kill (beg end)
  "Kills tia-buffer  buffer."
  (interactive "r" )
  (tia-append-to-buffer beg end 0)
)


;;; ................ end of tinyappend.el ................................
