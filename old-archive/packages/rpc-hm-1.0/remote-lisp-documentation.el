;Author: Eyvind Ness (eyvind) 
;Date:   Thursday, May 7 1992 07:29 GMT
;File:   /usr/local/gnu/emacs/elisp/site-extensions/remote-lisp-documentation.el

;;;
;;; This is a GNU Emacs interface to on-line documentation of Lisp
;;; functions on a remote documentation server host using a general
;;; RPC-based protocol. Convenient for use inside inferior-lisp-mode to
;;; have easy, single-keystroke access to the Common Lisp documentation
;;; of variables and functions just as on Lispms with C-D and C-A.

;;;     Copyright (C) 1991, 1992 Eyvind Ness.
;;;
;;;     Permission to use, copy, modify, and distribute this software and its
;;;     documentation for non-commercial purposes and without fee is hereby
;;;     granted, provided that the above copyright notice appear in all copies
;;;     and that both the copyright notice and this permission notice appear in
;;;     supporting documentation. OECD Halden Reactor Project makes no
;;;     representations about the suitability of this software for any purpose.
;;;     It is provided "as is" without express or implied warranty.
;;;
;;;     OECD Halden Reactor Project disclaims all warranties with regard to this
;;;     software, including all implied warranties of merchantability and
;;;     fitness, and in no event shall OECD Halden Reactor Project be liable for
;;;     any special, indirect or consequential damages or any damages whatsoever
;;;     resulting from loss of use, data or profits, whether in an action of
;;;     contract, negligence or other tortious action, arising out of or in
;;;     connection with the use or performance of this software.
;;;
;;;
;;;     Eyvind Ness
;;;     Research Scientist
;;;     Control Room Systems Division
;;;     OECD Halden Reactor Project
;;;     Norway
;;;
;;;     Internet Email: eyvind@hrp.no
;;;     Voice: +47 9 183100
;;;     Fax: +47 9 187109
;;;     Surface mail: P.O. Box 173, N-1751 Halden, Norway

(require 'rpc-hm)
(provide 'remote-lisp-documentation)

	
(defun rld-print-help-return-message ()
  ;; simplified, bug-free (?) version of `print-help-return-message'
  (message
   (substitute-command-keys
    (if (one-window-p t)
	(if pop-up-windows
	    "Type \\[delete-other-windows] to remove help window."
	    "Type \\[switch-to-buffer] RET to remove help window.")
	"Type \\[switch-to-buffer-other-window] RET to restore old contents of help window."))))

(defun rld-function-called-at-point ()
  (condition-case ()
      (save-excursion
	(save-restriction
	  (narrow-to-region (max (point-min) (- (point) 1000)) (point-max))
	  (backward-up-list 1)
	  (forward-char 1)
	  (let (obj)
	    (setq obj (read (current-buffer)))
	    (and (symbolp obj) obj))))
    (error nil)))


(defun rld-variable-at-point ()
  (condition-case ()
      (save-excursion
	(forward-sexp -1)
	(skip-chars-forward "'")
	(let ((obj (read (current-buffer))))
	  (and (symbolp obj) obj)))
    (error nil)))


(defun rld-describe-function (function &optional doc-host no-display)
  "Display the documentation of FUNCTION [supplied by DOC-HOST].
If optional third arg NO-DISPLAY is non-nil, the doc is not displayed in
a separate buffer."
  
  (interactive
   (let ((fn (rld-function-called-at-point))
	 (enable-recursive-minibuffers t)	     
	 val)
     (setq
      val
      (completing-read
       (if fn (format "Describe function (default %s): " fn)
	 "Describe function: ")
       obarray 'fboundp nil))
     (list (if (equal val "") fn (intern val)))))
  
  (let ((symname (symbol-name function))
	(retval nil))
    (or doc-host
	(setq doc-host (rpc-hm-get-current-host)))
    (prog1
	(setq 
	 retval
	 (rpc-hm-internal
	  doc-host
	  (concat
	   "(format nil \"Function ~A:~A ~:A~\%~\%~8T~A\" "
	   ;; ~% has to be \-ed, to avoid elisp interference.
	   "(package-name (symbol-package '" symname "))"
	   "(symbol-name '" symname ")"
	   "(if (fboundp '" symname ")" "(arglist '" symname ")"
	   "\"[Not a Function]\")"
	   "(or (documentation '" symname " 'function)"
	   "\"[Not documented]\" ))"
	   ) 'invoke-reader ':any))
      (and (not noninteractive) (not no-display)
	   (save-excursion
	     (set-buffer (get-buffer-create "*Documentation Output*"))
	     (goto-char (point-min))
	     (insert retval
		     (format
		      "\n\n%s%s%s%s.\n\n\n"
		      ";;; End of documentation for "
		      (upcase symname)
		      " provided by "
		      (upcase
		       (if (stringp doc-host) doc-host
			 (prin1-to-string doc-host)))))
	     (goto-char (point-min))
	     (display-buffer "*Documentation Output*")
	     (rld-print-help-return-message))))))

  
(defun rld-describe-variable (var &optional doc-host no-display)
  "Display the documentation of VAR [supplied by DOC-HOST].
If optional third arg NO-DISPLAY is non-nil, the doc is not displayed in
a separate buffer."

  (interactive 
   (let ((v (rld-variable-at-point))
	 (enable-recursive-minibuffers t)
	 val)
     (setq
      val
      (completing-read
       (if v (format "Describe variable (default %s): " v)
	 "Describe variable: ")
       obarray 'boundp nil))
     (list (if (equal val "") v (intern val)))))
  
  (let ((symname (symbol-name var))
	(retval nil))
    (or doc-host
	(setq doc-host (rpc-hm-get-current-host)))
    (prog1
	(setq 
	 retval
	 (rpc-hm-internal
	  doc-host
	  (concat
	   "(format nil \"Variable ~A:~A [~A]~\%~\%~8T~A\" "
	   "(package-name (symbol-package '" symname "))"
	   "(symbol-name '" symname ")"
	   "(if (boundp '" symname ") 'bound 'unbound)"
	   "(or (documentation '" symname " 'variable)"
	   "\"[Not documented]\" ))"
	   ) 'invoke-reader ':any))
      (and (not noninteractive) (not no-display)
	   (save-excursion
	     (set-buffer (get-buffer-create "*Documentation Output*"))
	     (goto-char (point-min))
	     (insert retval
		     (format
		      "\n\n%s%s%s%s.\n\n\n"
		      ";;; End of documentation for "
		      (upcase symname)
		      " provided by "
		      (upcase
		       (if (stringp doc-host) doc-host
			 (prin1-to-string doc-host)))))
	     (goto-char (point-min))
	     (display-buffer "*Documentation Output*")
	     (rld-print-help-return-message))))))
