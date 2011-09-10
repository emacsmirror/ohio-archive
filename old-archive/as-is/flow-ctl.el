;To: unix-emacs@bbn.com
;Date: 25 Apr 89 07:13:22 GMT
;From: jeffrey templon <mailrus!iuvax!silver!templon@bbn.com>
;Sender: arpa-unix-emacs-request@bbn.com
;Subject: Re: GNU Emacs Edt-emulation-mode
;Reply-To: jeffrey templon <templon%silver.uucp@CHIPS.BBN.COM>
;References: <460@dmsadel.dms.oz>
;Organization: Indiana University BACS, Bloomington
;Source-Info:  From (or Sender) name not authenticated.
;
;
;I have had exactly the same problem running GNU 18.52 on a VAX 8650 running
;VAX VMS and another 8650 running Ultrix.  I sent a bug report to the
;bug report people and they couldn't reproduce it.  Glad to see I am not
;losing my mind.
;
;	Are you using flow control and the key-translate-table deal?
;I wondered if perhaps that was the problem, that the translation somehow
;didn't cover the GOLD key combos and thus they got ignored.  Unfortunately
;I am not a lisper so I can't tell.  Here is my flow controller in case someone
;wants to test this guess.  It gets run at emacs startup time.
;
;					Jeff
;
;P.S. Please cc: any discussion this promotes directly to me as I will
;not be able (probably ) to read it before it expires!!!  Thanks -jt


;;; IUCF Emacs site-specific initialization file
;;; This file cobbled together by JAT to solve flow control problem.
;;; Note that control-q and control-s are remapped into control-\
;;; and control-` (control-^ on non-VT terminals.)
;;; 
;;;
 
(setq inhibit-startup-message t)
(setq delete-auto-save-files t)

;;; Make a keyboard translate table and initialize it to the identity.
(setq flow-control-keyboard-translate-table (make-string 128 0))
(let ((i 0))
  (while (< i 128)
    (aset flow-control-keyboard-translate-table i i)
    (setq i (1+ i))))
;;; Now, map C-^ to C-s and C-\ to C-q (on vt100s C-^ is C-`).
(aset flow-control-keyboard-translate-table ?\^^ ?\^s)
(aset flow-control-keyboard-translate-table ?\^\\ ?\^q)
 
(defun flow-on ()
"Handle C-s/C-q flow control by mapping C-^ to C-s and C-\ to C-q.
Also tell Emacs to use CBREAK mode and interpret C-s and C-q as flow
control commands.  Meta-keys are ignored, and C-g discards buffered output
(possibly causing incorrect screen updating)."
  (interactive)
  (setq keyboard-translate-table flow-control-keyboard-translate-table)
  (set-input-mode nil t))
 
(defun flow-off ()
"Turn off flow control handling.  See the function flow-on."
  (interactive)
  (setq keyboard-translate-table nil)
  (set-input-mode t nil))
 
(defun prefix-region (point mark string)
  "Prefix the region between POINT and MARK with STRING."
  (interactive "*r\nsPrefix: ")
  (save-excursion
    (save-restriction
      (narrow-to-region point mark)
      (goto-char point)
      (replace-regexp "^" string))))

(flow-on)

