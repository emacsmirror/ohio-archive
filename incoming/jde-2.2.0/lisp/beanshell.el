;;; beanshell.el
;; $Revision: 1.13 $ 

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


(defcustom bsh-startup-timeout 3
  "*Length of time the JDE waits for the Beanshell to startup.
Increase the value of this variable if you get Lisp errors
on BeanShell startup on Unix."
  :group 'bsh
  :type 'integer)

(defun bsh()
"*Starts BeanShell, a Java interpreter developed by Pat Niemeyer."
  (interactive)
  (bsh-internal t))

(defun bsh-internal (&optional display-buffer) 
  (let ((bsh-buffer-name "*bsh*"))
    (if (not (comint-check-proc bsh-buffer-name))
	(let* ((bsh-buffer (get-buffer-create bsh-buffer-name))
	       (jde-java-directory
		(concat
		 (jde-find-jde-data-directory)
		 "java/"))
	       (vm (if (eq system-type 'windows-nt)
		       jde-run-java-vm-w
		     jde-run-java-vm))
	       (vm-args
		(list
		 "-classpath"
                 (jde-convert-cygwin-path
                  (concat
		   jde-java-directory "bsh-commands" jde-classpath-separator
                   jde-java-directory "lib/jde.jar" jde-classpath-separator
                   jde-java-directory "lib/bsh.jar" jde-classpath-separator
                   (if jde-global-classpath
                       (jde-run-build-classpath-arg jde-global-classpath)
                     (getenv "CLASSPATH"))) jde-classpath-separator)
		 "bsh.Interpreter")))
	  (save-excursion
	    (set-buffer bsh-buffer)
	    (erase-buffer)
	    (comint-mode)
	    (setq comint-prompt-regexp "bsh % "))
	 (save-w32-show-window
	   ;; (message "%s" (nth 1 vm-args))
	   (message "%s" "Starting the BeanShell. Please wait...")
	   (comint-exec bsh-buffer "bsh" vm nil vm-args))
	 (process-kill-without-query (get-buffer-process bsh-buffer))
	 (if display-buffer
	      (pop-to-buffer bsh-buffer-name)))
      (when display-buffer
	  (message "The Java interpreter is already running.")
	  (pop-to-buffer bsh-buffer-name)))))


(setq bsh-tq-reply nil)

(defun bsh-eval-filter (process result)
  (let ((end-of-result (string-match ".*bsh % " result)))
    (if end-of-result
	(setq bsh-tq-reply (concat bsh-tq-reply (substring result 0 end-of-result)))
      (setq bsh-tq-reply (concat bsh-tq-reply result))
      (accept-process-output process 5 5))))

(defun bsh-eval (expr &optional eval-return)
  "Uses the BeanShell Java interpreter to evaluate a Java statement.
If the interpreter is not already running, this function starts
the interpreter. This function returns any text output by the
Java interpreter's standard out or standard error pipes.
If the optional argument eval-return is non-nil, this function
returns the result of evaluating the Java output as a Lisp
expression."
  (let* ((bsh-process
	  (if (get-process "bsh")
	      (get-process "bsh")
	    (let (proc)
	      (bsh-internal)
	      (setq proc (get-process "bsh"))
	      (if (eq system-type 'windows-nt)
		  (accept-process-output proc)
		(while (accept-process-output proc bsh-startup-timeout 0)))
	      proc)))
	 (comint-filter (if bsh-process (process-filter bsh-process))))
    (when bsh-process
      (setq bsh-tq-reply nil)
      (set-process-filter bsh-process 'bsh-eval-filter)
      ;; (message "Evaluating: %s" expr)
      (process-send-string bsh-process (concat expr "\n"))
      (if (not (accept-process-output bsh-process 100 100))
	  (message "No reply from BeanShell"))
      (set-process-filter bsh-process comint-filter)
      ;; (if eval-return (message "Evaluating reply: %s" bsh-tq-reply))
      (if eval-return
	  (eval (read bsh-tq-reply))
	bsh-tq-reply))))

(defun bsh-eval-r(java-statement) 
  "Convenience function for evaluating Java statements
that return Lisp expressions as output. This function 
invokes bsh-eval with the evaluate-return option set to
t."
  (bsh-eval java-statement t))



(provide 'beanshell);

;; $Log: beanshell.el,v $
;; Revision 1.13  2000/02/16 04:39:28  paulk
;; Implemented Cygwin/XEmacs compatiblity fix provided by Fred Hart
;; <cfhart@Z-TEL.com> in bsh-internal.
;;
;; Revision 1.12  2000/02/02 05:51:00  paulk
;; Expanded doc string.
;;
;; Revision 1.11  2000/01/28 04:28:00  paulk
;; Fixed startup timing bug that cause commands that use the beanshell to
;; failt the first time on Unix systems.
;;
;; Revision 1.10  2000/01/15 08:00:03  paulk
;; Corrected typo.
;;
;; Revision 1.9  1999/11/01 03:13:07  paulk
;; No change.
;;
;; Revision 1.8  1999/09/17 06:55:26  paulk
;; Set comint-prompt-regexp to the beanshell prompt.
;; Fixed bug where Emacs was querying user whether to kill the beanshell
;; buffer on exit from Emacs.
;;
;; Revision 1.7  1999/01/15 22:18:41  paulk
;; Added Andy Piper's NT/XEmacs compatibility changes.
;;
;; Revision 1.6  1998/12/13 22:10:04  paulk
;; Add check for chunked traffic between Emacs and the BeanShell.
;;
;; Revision 1.5  1998/12/09 00:59:43  paulk
;; Added a startup message for beanshell.
;;
;; Revision 1.4  1998/11/27 10:07:57  paulk
;; Use CLASSPATH environment variable if jde-global-classpath is nil.
;;
;; Revision 1.3  1998/11/22 23:14:28  paulk
;; Fixed path separator bug.
;;
;; Revision 1.2  1998/11/22 18:11:56  paulk
;; Changed path to use jde.jar.
;;
;; Revision 1.1  1998/10/22 00:07:56  paulk
;; Initial revision
;;


;; End of beanshell.el