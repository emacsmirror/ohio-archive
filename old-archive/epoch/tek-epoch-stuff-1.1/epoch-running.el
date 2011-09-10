;*****************************************************************************
;
; Filename:	epoch-running.el
;
; Description:	Sets a flag to indicate if user is running Epoch or Emacs.
;
; Author:	Someone on the net, whose name I forgot.
;
; This version by:	Ken Wood, <kwood@austek.oz.au>
; Organisation:		Austek Microsystems Pty Ltd, Australia.
;
;		See the INSTALL file that comes with this package for
;		installation details.
;
;*****************************************************************************

; $Id: epoch-running.el,v 1.3 1991/10/23 02:15:10 kwood Exp $

(provide 'epoch-running)

;; epoch-version is an epoch-defined variable whose value is not nil.
;; By looking at it you can tell if you are running epoch.  But, on
;; the other hand, it doesnt exist in a regular gnuemacs, so defvar-ing
;; it to nil here guarantees it will be nil in gnuemacs and non-nil
;; in epoch
(defvar epoch::version nil)     ;; in epoch this will not set!

;; running-epoch is a better name for the variable that tells us is we are
;; in epoch or not
(setq running-epoch epoch::version)
