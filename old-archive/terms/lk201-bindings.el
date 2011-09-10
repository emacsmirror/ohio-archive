;;; File:  lk201-bindings.el, v 1.2 (FSF emacs 19 version)
;;;
;;;
;;;
;;; Copyright (C) 1993 Jeff Morgenthaler

;; LCD Archive Entry:
;; lk201-bindings.el|Jeff Morgenthaler|jpmorgen@wisp4.physics.wisc.edu|
;; Sets up bindings for DEC function keys in emacs 19.|
;; 11-Nov-1993|1.2|~/terms/lk201-bindings.el.Z|

;;; GNU Emacs is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY.  No author or distributor accepts
;;; RESPONSIBILITY TO anyone for the consequences of using it or for
;;; whether it serves any particular purpose or works at all, unless 
;;; he says so in writing.  Refer to the GNU Emacs General Public
;;; License for full details.
;;;
;;;  Send bug reports and suggestions for improvement to Jeff Morgenthaler
;;;  (jpmorgen@wisp4.physics.wisc.edu).
;;;
;;; Everyone is granted permission to copy, modify and redistribute
;;; GNU Emacs, but only under the conditions described in the GNU
;;; Emacs General Public License.  A copy of this license is supposed
;;; to have been given to you along with GNU Emacs so you can know
;;; your rights and responsibilities.  It should be in a file named
;;; COPYING.  Among other things, the Copyright notice and this notice
;;; must be preserved on all copies.
;;;

;; This file sets up key bindings on DEC and DEC clone terminals.  The
;; keyboard is assumed to be vt200 type, though since it is a subset,
;; the vt100 keyboard is defined too.  

;; To use this, put this code on the emacs load path somewhere (say in
;; site-lisp) and put the line (require 'lk201-bindings) in your
;; .emacs file (or better yet in you site's default.el file).

(provide 'lk201-bindings)

;; Politely define some basic elisp functions for the labeled function
;; keys and keypad keys.  These are based on my experience using the
;; lk201 keyboard without a meta key.

;; The labeled keys should be self evident.

(if (not (global-key-binding [prior]))
    (global-set-key [prior] 'scroll-down))
(if (not (global-key-binding [next]))
    (global-set-key [next] 'scroll-up))
(if (not (global-key-binding [select]))
    (global-set-key [select] 'set-mark-command))
(if (not (global-key-binding [menu]))
    (global-set-key [menu] 'execute-extended-command))
;; isearch should be done with C-s.
(if (not (global-key-binding [find]))
    (global-set-key [find] 'search-forward))
(if (not (global-key-binding [help]))
    (global-set-key [help] 'help-for-help))
;; Check to make sure that user hasn't defined [insert] to something else.
;; Note:  if they like overwite-mode, they will need a term setup-hook.
(if (eq (global-key-binding [insert]) 'overwrite-mode)
    (global-set-key [insert] 'yank))
(if (not (global-key-binding [remove]))
    (global-set-key [remove] 'kill-region))
;; RMS refuses to name the insert and remove keys of dumb terminals
;; "insert" and "remove" (see term/lk201.el) so here are some
;; duplicate bindings.  Furthermore, insertchar and deletechar are
;; bound in loaddefs.el to functions more appropriate for IBM/sun
;; keyboards.
;;(if (eq (global-key-binding [insertchar]) 'overwrite-mode)
    (global-set-key [insertchar] 'yank)
(if (eq (global-key-binding [deletechar]) 'delete-char)
    (global-set-key [deletechar] 'kill-region))

;; I fill text so often, I like it to be a one button operation.  I
;; chose this sequence of function keys since f11 is the escape key
;; f12 is really close to f11-q ( = M-q = fill paragraph).  The rest
;; of the keys just go outward from there.  

(if (not (global-key-binding [f10]))
    (global-set-key [f10] 'rmail))
;; Define f11 this way since programming language modes remap M-q
;; NOTE: this is how "Find" should be defined for C-s, but the
;; keyboard macro thing is buggy in isearch-mode.
(if (not (global-key-binding [f12]))
    (define-key global-map [f12] [?\M-q]))
(if (not (global-key-binding [f13]))
    (global-set-key [f13] 'fill-region))
(if (not (global-key-binding [f14]))
    (global-set-key [f14] 'set-fill-prefix))

;; The keypad keys are defined to mimick the labled group of keys on
;; the lk201 keyboard, since I touch type them at this point.  The
;; rest of the bindings are reminiscent of EDT bindings, though I
;; don't use them much (otherwise I would probably make kp-f1 into a
;; GOLD prefix kind of thing).  The main thing I do use the kepad for
;; is cursor motion, ESPECIALLY forward/backward-word (4/6).  If you
;; like the EDT bindings, use "edt-emulation-on."  

;; Find the biggest key on the keypad and assign it to M-x.
(if (not (global-key-binding [kp-0]))
    (global-set-key [kp-0] 'execute-extended-command))
;; Help is nearby.
(if (not (global-key-binding [kp-decimal]))
    (global-set-key [kp-decimal] 'help-for-help))
(if (not (global-key-binding [kp-enter]))
    (global-set-key [kp-enter] 'open-line))
;; The lk201 arrow keys left, down, and right are all in a line.
(if (not (global-key-binding [kp-1]))
    (global-set-key [kp-1] 'backward-char))
(if (not (global-key-binding [kp-2]))
    (global-set-key [kp-2] 'next-line))
(if (not (global-key-binding [kp-3]))
    (global-set-key [kp-3] 'forward-char))
(if (not (global-key-binding [kp-4]))
    (global-set-key [kp-4] 'backward-word))
(if (not (global-key-binding [kp-5]))
    (global-set-key [kp-5] 'previous-line))
(if (not (global-key-binding [kp-6]))
    (global-set-key [kp-6] 'forward-word))
(if (not (global-key-binding [kp-separator]))
    (global-set-key [kp-separator] 'delete-char))
(if (not (global-key-binding [kp-7]))
    (global-set-key [kp-7] 'set-mark-command))
(if (not (global-key-binding [kp-8]))
    (global-set-key [kp-8] 'scroll-down))
(if (not (global-key-binding [kp-9]))
    (global-set-key [kp-9] 'scroll-up))
(if (not (global-key-binding [kp-subtract]))
    (global-set-key [kp-subtract] 'kill-word))
(if (not (global-key-binding [kp-f1]))
    (global-set-key [kp-f1] 'search-forward))
(if (not (global-key-binding [kp-f2]))
    (global-set-key [kp-f2] 'yank))
(if (not (global-key-binding [kp-f3]))
    (global-set-key [kp-f3] 'kill-region))
(if (not (global-key-binding [kp-f4]))
    (global-set-key [kp-f4] 'kill-line))

