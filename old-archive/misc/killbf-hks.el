;From ark1!uakari.primate.wisc.edu!uwm.edu!cs.utexas.edu!tut.cis.ohio-state.edu!GINGER.BERKELEY.EDU!mcgrath%paris.Berkeley.EDU Wed Dec  6 12:15:37 1989
;Article 437 of gnu.emacs.bug:
;Path: ark1!uakari.primate.wisc.edu!uwm.edu!cs.utexas.edu!tut.cis.ohio-state.edu!GINGER.BERKELEY.EDU!mcgrath%paris.Berkeley.EDU
;>From mcgrath%paris.Berkeley.EDU@GINGER.BERKELEY.EDU (Roland McGrath)
;Newsgroups: gnu.emacs.bug
;Subject: kill-buffer-hooks.el
;Message-ID: <8911262330.AA02393@paris.Berkeley.EDU>
;Date: 26 Nov 89 23:30:37 GMT
;Sender: daemon@tut.cis.ohio-state.edu
;Distribution: gnu
;Organization: GNUs Not Usenet
;Lines: 47
;
;This version won't beep at you if your kill-buffer hook kill the buffer (take
;care to do (let ((kill-buffer-hooks nil)) ...) to avoid recursion here).

;;; Run hook functions when killing buffers.
;;;
;;; Copyright (C) 1989 Roland McGrath
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; A copy of the GNU General Public License can be obtained from this
;;; program's author (send electronic mail to roland@ai.mit.edu) or from
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.
;;;
;;; Send bug reports to roland@ai.mit.edu.

;; To use this, just load it and set kill-buffer-hooks as desired.
;; You cannot autoload this file.

(defvar kill-buffer-hooks nil "\
*Hooks to run when a buffer is killed.
May be a function or a list of functions.  If it is a list,
each element is called in order.  Functions are called with
no arguments, with the buffer to be killed current.")

;; Rename the kill-buffer subr to basic-kill-buffer.
(let ((func (symbol-function 'kill-buffer)))
  (and (subrp func)
       (fset 'basic-kill-buffer func)))

(defun kill-buffer (buffer)
  (interactive "bKill buffer: ")
  (set-buffer buffer)
  (run-hooks 'kill-buffer-hooks)
  (setq buffer (get-buffer buffer))
  (if buffer
      (basic-kill-buffer buffer)))

(provide 'kill-buffer-hooks)


