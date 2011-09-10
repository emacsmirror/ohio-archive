;;;Date: 7 Aug 87 17:35:01 GMT
;;;From: "Alan Stebbens, aks@hub.ucsb.edu" <HUB.UCSB.EDU!aks@EDDIE.MIT.EDU>
;;;Subject: auto-indent minor mode

;;;It always seemed to me that auto-indention should be a minor mode,
;;;instead of a major mode, as it is with indented-text mode.  So, I
;;;wrote the following little piece of Emacs-lisp to provide
;;;auto-indention as a minor mode to the current major mode.  This is
;;;particularly useful with outline-mode.  It sets the TAB key to
;;;indent-relative, and links indent-relative-maybe to the
;;;indent-line-function. 

;;;To use: (auto-indent-mode ARG) will set, clear, or toggle the
;;;auto-indent-flag if ARG is nonzero, zero, or omitted (null).  As a
;;;matter of course, I added the following line to my text-mode-init func
;;;(which is invoked via text-mode-hook):

;;;       (auto-indent-mode 1)

;;;If there are improvements which can be made on this, please mail
;;;them to me so I can learn from it.

;;;============================== auto-indent.el ==============================
;; auto-indent.el -- Set up minor mode to auto-indent relatively.
;; Alan K. Stebbens  7 Aug 87
;;
(if (boundp 'auto-indent-flag)
    nil
  (defvar auto-indent-flag nil "\
If set, indicates that auto-indent mode is active.  This variable is 
automatically set by invoking \\[auto-indent-mode].")
  (setq minor-mode-alist (cons '(auto-indent-flag " Indent") 
			       minor-mode-alist))
  (make-variable-buffer-local 'auto-indent-flag)
  (set-default 'auto-indent-flag nil))

;; auto-indent-mode 

(defun auto-indent-mode (arg) "\
Enable, disable, or toggle auto-indent mode if ARG is nonzero, zero, or
omitted.  Auto-indent mode works by invoking indent-relative for TAB,
and using indent-relative-may as the indent-line-function for auto-fill,
and NEWLINE."  
  (interactive "P")
  (if (setq auto-indent-flag (if (or (zerop (prefix-numeric-value arg))
				     (and (null arg) auto-indent-flag))
				 nil 
			       t))
      (progn
	(if (not (boundp 'auto-indent-old-tab-key))
	    (make-local-vars 'auto-indent-old-tab-key (key-binding "\t")
			     'auto-indent-old-indent-func indent-line-function
			     'auto-indent-flag t))
	(local-set-key "\t" 'indent-relative)
	(setq indent-line-function 'indent-relative-maybe))
    (if (boundp 'auto-indent-old-tab-key)
	(progn
	  (local-set-key "\t" auto-indent-old-tab-key)
	  (setq indent-line-function auto-indent-old-indent-func)
	  (makunbound auto-indent-old-tab-key)
	  (makunbound auto-indent-old-indent-func))))
  (set-buffer-modified-p (buffer-modified-p)))
