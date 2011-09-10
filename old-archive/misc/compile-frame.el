;; compile-frame.el: Support for FSF GNU Emacs 19 Compilation Buffer Frames
;; version: 1.3  Apr 15 '94
;;
;; Copyright (C) 1993 1994 Cengiz Alaettinoglu <ca@cs.umd.edu>
;;
;; LCD Archive Entry:
;; compile-frame|Cengiz Alaettinoglu|ca@cs.umd.edu|
;; Compilation Buffer Frames.|
;; 15-Apr-1994|1.3|~/misc/compile-frame.el|
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You didn't receive a copy of the GNU General Public License along
;; with this program; so, write to the Free Software Foundation, Inc.,
;; 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;;----------------------------------------------------------------------------
;;
;; DESCRIPTION:
;;   With this package, compilation buffer uses a separate frame for itself.
;;
;; INSTALLATION:
;;
;;   Add this to your .emacs:
;;
;;     (and window-system (require 'compile-frame))
;;
;;   and put this elisp file somewhere on your load-path and byte-compile it.
;;   byte-compilation is not optional (unless defadvice's below are changed).
;;
;; CUSTOMIZATION:
;;
;;   Hook variable: compilation-frame-selected-hook
;;      is called when the compilation-frame is selected during
;;      compile and next-error commands.
;;
;;   E.g. Add this to your .emacs to auto-raise compilation frame:
;;
;;     (add-hook 'compilation-frame-selected-hook
;;               '(lambda () (raise-frame compilation-frame-id)))
;;
;;   Variable: compilation-frame-alist
;;     Alist of frame parameters used for creating the COMPILATION frame.
;;
;;   For more customization, see end of file
;;
;; CREDITS:
;; I would like to thank Marc Girod, Greg Ullmann, Bob Sloan, 
;; Christian Lynbech and Kevin Broadey for various suggestions, bug fixes
;; and alpha-testing.
;; 
;;----------------------------------------------------------------------------

(require 'advice)
(setq ad-activate-on-definition t)
(ad-start-advice)

(defvar cf-first-pop-p nil "")

; cf-setup sets pop-up-frames and pop-up-frame-function so that
; compilation commands use their own window
(defmacro cf-setup (fn-name ad-name frame-id i-frame-id frame-alist frame-selected-hook)
  (` (defadvice (, fn-name)
       (around (, ad-name) preact)
       "Creates a frame for compilation buffer."

       (let ((selected-fid (selected-frame)))
	 (select-frame (if (and (, frame-id) (frame-live-p (, frame-id)))
			   (, frame-id)
			 (setq (, frame-id) (new-frame (, frame-alist)))))
	 (run-hooks (quote (, frame-selected-hook)))
	 (select-frame selected-fid)
	 )
       (let ((pop-up-frames t)
	     (cf-first-pop-p t)
	     (pop-up-frame-function     ; more complicated than needs to
	      '(lambda () (if cf-first-pop-p  ; but works for AUC-TeX
			      (progn
				(setq cf-first-pop-p nil)
				(, frame-id))
			    (, i-frame-id)))))
	 ad-do-it
	 )
       )
     )
  )

; i-frame refers to the frame where the compile command is invoked
; cf-setup-get-i-frame steal its value
(defmacro cf-setup-get-i-frame 
  (fn-name ad-name frame-id i-frame-id frame-alist frame-selected-hook)
  (` (defadvice (, fn-name)
       (before (, ad-name) preact)
       "Get the frame id where the compile is invoked."
       (setq (, i-frame-id) (selected-frame))
       )))

; On command like next-error, cf-setup-switch-i-frame switches to
; the i-frame if the current frame is the compilation frame
(defmacro cf-setup-switch-i-frame 
  (fn-name ad-name frame-id i-frame-id frame-alist frame-selected-hook)
  (` (defadvice (, fn-name)
       (before (, ad-name) preact)
       "Switch to the frame that invoked compile."
       (if (eq (selected-frame) (, frame-id))
	   (select-frame (if (and (, i-frame-id) (frame-live-p (, i-frame-id)))
			(, i-frame-id)
		      (setq (, i-frame-id) (new-frame))))))))

; Many people suggested the compilation buffer should scroll as the messages
; are printed. cf-setup-follow-messages does this
(defmacro cf-setup-follow-messages
  (fn-name ad-name frame-id i-frame-id frame-alist frame-selected-hook)
  (` (defadvice (, fn-name)
       (after (, ad-name) preact)
       "Continuously scroll compilation buffer to display end of messages."
       (let ((selected-fid (selected-frame)))
	 (select-frame (, frame-id)) 
	 (goto-char (point-max)) 
	 (let ((current-proc (get-buffer-process (current-buffer))))
	   (and current-proc
		(set-marker (process-mark current-proc) (point))))
	 (select-frame selected-fid)
	 )
       )))

;;;;; Compilation Frame Setup (You can customize these)

(defvar compilation-frame-alist
  '((name . "COMPILATION")     
    (height . 14) (width . 80) 
    (menu-bar-lines . 0))
  "*Alist of frame parameters used for creating the COMPILATION frame.")

(defvar compilation-frame-selected-hook 
  '((lambda () (raise-frame compilation-frame-id)))
 "Hook called when the compilation-frame is selected 
during compile and next-error commands.")

(defvar compilation-frame-id nil 
  "*Frame id of the COMPILATION frame.")

(defvar compilation-i-frame-id nil 
  "*Frame id where compilation is started.")

;;; Hack compile command
(cf-setup compile compile-frame 
	  compilation-frame-id compilation-i-frame-id
	  compilation-frame-alist compilation-frame-selected-hook)
(cf-setup-get-i-frame compile compile-frame 
	  compilation-frame-id compilation-i-frame-id
	  compilation-frame-alist compilation-frame-selected-hook)
(cf-setup-follow-messages compile compile-frame 
	  compilation-frame-id compilation-i-frame-id
	  compilation-frame-alist compilation-frame-selected-hook)


;;; Hack grep command
(cf-setup grep compile-frame 
	  compilation-frame-id compilation-i-frame-id
	  compilation-frame-alist compilation-frame-selected-hook)
(cf-setup-get-i-frame grep compile-frame 
	  compilation-frame-id compilation-i-frame-id
	  compilation-frame-alist compilation-frame-selected-hook)
(cf-setup-follow-messages grep compile-frame 
	  compilation-frame-id compilation-i-frame-id
	  compilation-frame-alist compilation-frame-selected-hook)

;;; For users of Kevin Rodgers "igrep" package (get it from the Ohio State
;;; elisp archive).  Only need to advise "igrep" because "egrep" and "fgrep"
;;; work by calling "igrep".

(cf-setup igrep compile-frame 
	  compilation-frame-id compilation-i-frame-id
	  compilation-frame-alist compilation-frame-selected-hook)
(cf-setup-get-i-frame igrep compile-frame 
	  compilation-frame-id compilation-i-frame-id
	  compilation-frame-alist compilation-frame-selected-hook)
(cf-setup-follow-messages igrep compile-frame 
	  compilation-frame-id compilation-i-frame-id
	  compilation-frame-alist compilation-frame-selected-hook)


;;; Hack next-error command
(cf-setup next-error compile-frame 
	  compilation-frame-id compilation-i-frame-id
	  compilation-frame-alist compilation-frame-selected-hook)
(cf-setup-switch-i-frame next-error compile-frame 
	  compilation-frame-id compilation-i-frame-id
	  compilation-frame-alist compilation-frame-selected-hook)

;; For AUC-TeX users
(cf-setup TeX-run-TeX compile-frame                              
         compilation-frame-id compilation-i-frame-id             
         compilation-frame-alist compilation-frame-selected-hook)
(cf-setup-get-i-frame TeX-run-TeX compile-frame                  
         compilation-frame-id compilation-i-frame-id             
         compilation-frame-alist compilation-frame-selected-hook)
(cf-setup-follow-messages TeX-run-TeX compile-frame              
         compilation-frame-id compilation-i-frame-id             
         compilation-frame-alist compilation-frame-selected-hook)

(cf-setup TeX-run-LaTeX compile-frame                              
         compilation-frame-id compilation-i-frame-id             
         compilation-frame-alist compilation-frame-selected-hook)
(cf-setup-get-i-frame TeX-run-LaTeX compile-frame                  
         compilation-frame-id compilation-i-frame-id             
         compilation-frame-alist compilation-frame-selected-hook)
(cf-setup-follow-messages TeX-run-LaTeX compile-frame              
         compilation-frame-id compilation-i-frame-id             
         compilation-frame-alist compilation-frame-selected-hook)

(cf-setup TeX-run-interactive compile-frame                              
         compilation-frame-id compilation-i-frame-id             
         compilation-frame-alist compilation-frame-selected-hook)
(cf-setup-get-i-frame TeX-run-interactive compile-frame                  
         compilation-frame-id compilation-i-frame-id             
         compilation-frame-alist compilation-frame-selected-hook)
(cf-setup-follow-messages TeX-run-interactive compile-frame              
         compilation-frame-id compilation-i-frame-id             
         compilation-frame-alist compilation-frame-selected-hook)

(cf-setup TeX-run-BibTeX compile-frame                              
         compilation-frame-id compilation-i-frame-id             
         compilation-frame-alist compilation-frame-selected-hook)
(cf-setup-get-i-frame TeX-run-BibTeX compile-frame                  
         compilation-frame-id compilation-i-frame-id             
         compilation-frame-alist compilation-frame-selected-hook)
(cf-setup-follow-messages TeX-run-BibTeX compile-frame              
         compilation-frame-id compilation-i-frame-id             
         compilation-frame-alist compilation-frame-selected-hook)



;(cf-setup TeX-command compile-frame                              
;         compilation-frame-id compilation-i-frame-id             
;         compilation-frame-alist compilation-frame-selected-hook)
;(cf-setup-get-i-frame TeX-command compile-frame                  
;         compilation-frame-id compilation-i-frame-id             
;         compilation-frame-alist compilation-frame-selected-hook)
;(cf-setup-follow-messages TeX-command compile-frame              
;         compilation-frame-id compilation-i-frame-id             
;         compilation-frame-alist compilation-frame-selected-hook)

(cf-setup TeX-help-error compile-frame 
	  compilation-frame-id compilation-i-frame-id
	  compilation-frame-alist compilation-frame-selected-hook)
(cf-setup-switch-i-frame TeX-help-error compile-frame 
	  compilation-frame-id compilation-i-frame-id
	  compilation-frame-alist compilation-frame-selected-hook)

(cf-setup TeX-parse-TeX compile-frame 
	  compilation-frame-id compilation-i-frame-id
	  compilation-frame-alist compilation-frame-selected-hook)
(cf-setup-switch-i-frame TeX-parse-TeX compile-frame 
	  compilation-frame-id compilation-i-frame-id
	  compilation-frame-alist compilation-frame-selected-hook)


;; For foo users
;;(cf-setup foo ...)

(provide 'compile-frame)
