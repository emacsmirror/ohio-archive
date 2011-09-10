; Path: dg-rtp!rock.concert.net!mcnc!gatech!ncar!elroy.jpl.nasa.gov!swrinde!zaphod.mps.ohio-state.edu!cis.ohio-state.edu!tut.cis.ohio-state.edu!unreplyable!garbage
; From: djm@ENG.UMD.EDU (David J. MacKenzie)
; Newsgroups: gnu.emacs.sources
; Subject: running emacs as vip
; Date: 30 May 91 12:38:36 GMT
; Organization: Source only  Discussion and requests in gnu.emacs.help.
; 
; I decided to see what would be necessary to make emacs run as a vi
; emulator from the command line, so I could type
; bash$ vip file1 file2.c file3
; Here's what I came up with.
; 
; In .bashrc:
; alias vip='emacs -f vip-default'
; 
; In .emacs:
;;; Use text mode by default instead of fundamental mode.
(setq default-major-mode 'text-mode)

(defvar vip-mode-default nil
  "*If non-nil, text mode buffers start up in vip-mode.")
(defun vip-default (&optional arg)
  "Toggle making vip-mode a default minor mode for text-mode.
With arg, turn it on iff arg is positive."
  (interactive "P")  (setq vip-mode-default
	(if arg
	    (> (prefix-numeric-value arg) 0)
	  (not vip-mode-default))))

;;; Set auto-fill minor mode in all text modes.
(setq text-mode-hook
      '(lambda ()
	 (auto-fill-mode 1)
	 (if vip-mode-default (vip-mode))))
(setq c-mode-hook
      '(lambda ()
	 (if vip-mode-default (vip-mode))))
(setq lisp-mode-hook
      '(lambda ()
	 (if vip-mode-default (vip-mode))))
