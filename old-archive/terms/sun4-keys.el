;; LCD Archive Entry:
;; sun4-keys|Ethan Bradford|ethanb@u.washington.edu|
;; Names and definitions for all the named keys on a Sun4 keyboard|
;; 8-Jul-1993|v1.0|~/terms/sun4-keys.el.Z|

;; Copyright (C) 1993 Free Software Foundation, Inc.
;;   See end for conditions.

;; For emacs-19 and later only.

;; Usage:   Just load this file to get the key definitions.
;; Here is the code I have in our default.el to do this:
;;  (setq term (getenv "TERM"))
;;  (setq keyboard-type
;;        (cond ((string= (substring term 0 3) "sun") "Sun")
;;  	    (t nil)))
;;
;;  (if (string= keyboard-type "Sun")
;;      (load-library "sun-keys"))



;; Name everything according to what appears on its keytop, except choose
;;  standard name for redo, prior, and next.  Name "kp-5" center.
;; The only shifted key defined is S-find because that is used in Openwindows.
;; Commented out lines are part of the standard definitions.
(define-key function-key-map [f11] [stop]) ; L1
(define-key function-key-map [f12] [redo]) ; L2 (Again)
(define-key function-key-map [f13] [props]) ; L3
(define-key function-key-map [f14] [undo]) ; L4
(define-key function-key-map [f15] [front]) ; L5 (Not captured by Emacs)
(define-key function-key-map [f16] [copy]) ; L6
(define-key function-key-map [f17] [open]) ; L7 (Not captured by Emacs)
(define-key function-key-map [f18] [paste]) ; L8
(define-key function-key-map [f19] [find]) ; L9
(define-key function-key-map [S-f19] [S-find]) ; L9
(define-key function-key-map [f20] [cut]) ; L10
(define-key function-key-map [f21] [pause]) ; R1
(define-key function-key-map [f22] [prsc]) ; R2
(define-key function-key-map [f23] [scrolllockbreak]) ; R3
(define-key function-key-map [f24] [equals]) ; R4
(define-key function-key-map [f25] [slash]) ; R5
(define-key function-key-map [f26] [times]) ; R6
(define-key function-key-map [f27] [home]) ; R7
; (define-key function-key-map [f28] [up]) ; R8
(define-key function-key-map [f29] [prior]) ; R9 (PgUp)
;(define-key function-key-map [f30] [left]) ; R10
(define-key function-key-map [f31] [center]) ; R11
;(define-key function-key-map [f32] [right]) ; R12
(define-key function-key-map [f33] [end]) ; R13
;(define-key function-key-map [f34] [down]) ; R14
(define-key function-key-map [f35] [next]) ; R15 (PgDn)


(define-key global-map [stop] 'keyboard-quit)
;(define-key global-map [redo] 'repeat-complex-command)
(define-key global-map [help] 'help-for-help)
(define-key global-map [props] 'edit-options)
;(define-key global-map [undo] 'undo)
(define-key global-map [copy] 'kill-ring-save)
(global-set-key [paste] 'yank)
(global-set-key [cut] 'kill-region)
(define-key global-map [find] 'isearch-forward)
(define-key global-map [S-find] 'isearch-backward)

;(global-set-key [pause])
(global-set-key [prsc] 'print-buffer)
(global-set-key [scrolllockbreak] 'keyboard-quit)
; (global-set-key [numlock] ) 
(define-key global-map [center] 'recenter)
(global-set-key [equals] "=")
(global-set-key [slash] "/")
(global-set-key [times] "*")
(global-set-key [kp-subtract] "-")
(global-set-key [kp-add] "+")
(global-set-key [kp-enter] 'newline)


;; This file is not part of GNU Emacs (yet).

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
 
