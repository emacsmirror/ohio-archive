;Author: Eyvind Ness (eyvind) 
;Date:   Wednesday, May 6 1992 12:33 GMT
;File:   /usr/local/gnu/emacs/elisp/site-extensions/ilisp-rpc-hm-mods.el

;;;
;;; Mods to have ILISP functionality with rpc-hm.

(require 'ilisp)
(provide 'ilisp-rpc-hm-mods)

;;;   (WITH-FTP-FILE-ACCESS-ENV (:HOST-NAME "snapp"
;;;                                         :USER
;;;                                         "eyvind"
;;;                                         :PW
;;;                                         "xxx")
;;;     (CAREFUL-EVAL
;;;      (COMPILE-FILE
;;;       (PARSE-NAMESTRING
;;;        "/usr/local/gnu/emacs/elisp/ilisp-4.11/clisp.lisp"
;;;        "snapp")
;;;       :OUTPUT-FILE
;;;       (PARSE-NAMESTRING
;;;        "/usr/local/gnu/emacs/elisp/ilisp-4.11/clisp.xld"
;;;        "snapp")
;;;       :LOAD
;;;       T)))
;;;
;;;   << While compiling ILISP-DOCUMENTATION >>
;;;    (EQ (ELT TYPE 0) #\() should probably use EQL instead.
;;;   ; Loading snapp: /usr/local/gnu/emacs/elisp/ilisp-4.11/clisp.xld into package USER
;;;   (T #FS::UNIX-UCB-PATHNAME "snapp: /usr/local/gnu/emacs/elisp/ilisp-4.11/clisp.xld")
;;;
;;;
;;;   (insert (ilisp-value 'ilisp-complete-command))
;;;
;;;   ex.
;;;
;;;   (ILISP:ilisp-matching-symbols "with-" "user" t nil nil)
;;;    => (("with-open-file-case") ("with-open-file-search") ("with-timeout") ("with-stack-list") ("with-input-editing") ("with-lisp-mode") ("with-common-lisp-on") ("with-zetalisp-on") ("with-open-stream-case") ("with-stack-list*") ("with-self-variables-bound") ("with-lock") ("with-open-file-retry") ("with-open-stream") ("with-output-to-string") ("with-open-file") ("with-input-from-string") ("with-slots") ("with-added-methods") ("with-accessors") ("with-ftp-file-access-env") ("with-errors-and-output-trapped"))


;;;
;;; Need to redefine ilisp-send. The redef is backward compatible with
;;; original 4.11 def.

(defun ilisp-send (string &optional message status and-go handler)
  "Send STRING to the ILISP buffer, print MESSAGE set STATUS and
return the result if AND-GO is NIL, otherwise switch to ilisp if
and-go is T and show message and results.  If AND-GO is 'dispatch,
then the command will be executed without waiting for results.  If
AND-GO is 'call, then a call will be generated. If this is the first
time an ilisp command has been executed, the lisp will also be
initialized from the files in ilisp-load-inits.  If there is an error,
comint-errorp will be T and it will be handled by HANDLER."
  
  (if (and (boundp 'ilisp-use-rpc-hm-instead) ilisp-use-rpc-hm-instead)
      (let ((result
	     (progn
	       (rpc-hm-internal
		(rpc-hm-get-current-host) string
		;; e.g. "(ILISP:ilisp-matching-symbols \"with-\" \"user\" t nil nil)"
		nil ':any)
	       (rpc-hm-reparse-ans))))
	(concat
	 (substring (rpc-hm-read-from-string (car (cdr (cdr result)))) 1)
	 "\n" (car (cdr result))))
      
      (ilisp-init t)
      (let ((process (ilisp-process))
	    (dispatch (eq and-go 'dispatch)))
	(if message
	    (message "%s" (if dispatch
			      (concat "Started " message)
			      message)))
	;; No completion table
	(setq ilisp-original nil)
	(if (memq and-go '(t call))
	    (progn (comint-send process string nil nil status message handler)
		   (if (eq and-go 'call)
		       (call-defun-lisp nil)
		       (switch-to-lisp t t))
		   nil)
	    (let* ((save (ilisp-value 'ilisp-save-command t))
		   (result
		    (comint-send 
		     process
		     (if save (format save string) string)
		     ;; Interrupt without waiting
		     t (if (not dispatch) 'wait) status message handler)))
	      (if save 
		  (comint-send
		   process
		   (ilisp-value 'ilisp-restore-command t)
		   t nil 'restore "Restore" t t))
	      (if (not dispatch)
		  (progn
		    (while (not (cdr result))
		      (sit-for 0)
		      (accept-process-output))
		    (comint-remove-whitespace (car result)))))))))

;;;
;;; Unfortunately, the prevailing assumption in ILISP is that there must
;;; be an Emacs subprocess associated with everything, so this need
;;; redefinition, too. It backward compatible with the original 4-11
;;; definition.

(defun ilisp-buffer ()
  "Return the current ILISP buffer."
  (if (memq major-mode ilisp-modes)
      (current-buffer)
      (let ((buffer 
	     (if ilisp-buffer (get-buffer ilisp-buffer))))
	(or (and (boundp 'ilisp-use-rpc-hm-instead)
		 ilisp-use-rpc-hm-instead
		 (current-buffer))
	    buffer
	    (error "You must start an inferior LISP with run-ilisp.")))))
