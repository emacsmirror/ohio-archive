;;; jde-make.el -- Integrated Development Environment for Java.
;; $Revision: 1.6 $ 

;; Author: Paul Kinnucan <paulk@mathworks.com>
;; Maintainer: Paul Kinnucan
;; Keywords: java, tools

;; Copyright (C) 1997, 1998 Paul Kinnucan.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(require 'compile)

(defcustom jde-make-program "make"
  "*Specifies name of make program."
 :group 'jde-project
 :type 'string)

(defcustom jde-make-args ""
  "*Specifies arguments to be passed to make program."
  :group 'jde-project
  :type 'string)


(defvar jde-interactive-make-args ""
"String of compiler arguments entered in the minibuffer.")

(defcustom jde-read-make-args nil
"*Specify whether to prompt for additional make arguments.
If this variable is non-nil, and if `jde-build-use-make' is non nil
the jde-build command prompts you to enter additional make
arguments in the minibuffer. These arguments are appended to those 
specified by customization variables. The JDE maintains a history 
list of arguments entered in the minibuffer."
  :group 'jde-project
  :type 'boolean
)


(defun jde-make-make-command (more-args)
  "Constructs the java compile command as: jde-compiler + options + buffer file name."
  (concat jde-make-program " " jde-make-args
	  (if (not (string= more-args ""))
	      (concat " " more-args))
	  " "))


;;;###autoload
(defun jde-make ()
  "Run the JDE make program."
  (interactive)
  (if jde-read-make-args
      (setq jde-interactive-make-args
	      (read-from-minibuffer 
	       "Make args: "
	       jde-interactive-make-args
	       nil nil
	       '(jde-interactive-make-arg-history . 1))))

  (let ((make-command
	 (jde-make-make-command 
	  jde-interactive-make-args)))
    ;; Force save-some-buffers to use the minibuffer
    ;; to query user about whether to save modified buffers.
    ;; Otherwise, when user invokes jde-make from
    ;; menu, save-some-buffers tries to popup a menu
    ;; which seems not to be supported--at least on
    ;; the PC.
    (if (and (eq system-type 'windows-nt)
	     (not jde-xemacsp))	
	(let ((temp last-nonmenu-event))
	  ;; The next line makes emacs think that jde-make
	  ;; was invoked from the minibuffer, even when it
	  ;; is actually invoked from the menu-bar.
	  (setq last-nonmenu-event t)
	  (save-some-buffers (not compilation-ask-about-save) nil)
	  (setq last-nonmenu-event temp))
      (save-some-buffers (not compilation-ask-about-save) nil))
    (compile-internal make-command "No more errors")))

; (defun jde-make (args)
;   "Run the JDE make program."
;   (interactive
;    (list (if (string= jde-make-args "")
; 	     (read-from-minibuffer (concat jde-make-program " ")
; 				   (nth 0 minibuffer-history))
; 	   jde-make-args)))

;   (compile 
;    (concat
;     jde-make-program
;     " "
;     (jde-run-make-arg-string
;      (jde-run-parse-args args)))))

(provide 'jde-make)

;; $Log: jde-make.el,v $
;; Revision 1.6  1999/04/27 16:44:49  paulk
;; Updated to allow interactive entry of make arguments. Thanks to Yarek J. Kowalik <jgk@klg.com> for providing this enhancement.
;;
;; Revision 1.5  1999/01/17 00:43:57  paulk
;; Removed two line feeds at the end of make command as they appeared to
;; confuse GNU make for NT.
;;
;; Revision 1.4  1998/11/27 09:38:23  paulk
;; Changed to use compile mode as suggested by Robert Grace <rmg2768@draper.com>.
;;
;; Revision 1.3  1998/05/29 01:46:39  paulk
;; Added dummy function for jde-make-mode to facilitate autoloading.
;;
;; Revision 1.2  1998/05/27 06:04:52  paulk
;; Added autoload comments.
;;
;; Revision 1.1  1998/03/27 04:44:36  kinnucan
;; Initial revision
;;

;; End of jde-make.el