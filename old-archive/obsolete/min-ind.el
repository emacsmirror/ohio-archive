;;; @ min-ind.el - minor mode to auto-indent relatively.
;;;
;;; $Id: min-ind.el,v 5.5 1993/07/27 22:50:46 amanda Exp $
;;;
;;; LCD Archive Entry:
;;; min-ind|Per Abrahamsen|abraham@iesd.auc.dk|
;;; Minor mode version of indentated-text-mode|
;;; 27-Jul-1993|5.5|~/modes/min-ind.el.Z|

(provide 'min-ind)
(require 'min-bind)

;;; @@ Author
;;;
;;; Created by Alan K. Stebbens  7 Aug 87
;;;
;;; Added minor maps and ugly code by,
;;; Per Abrahamsen at University of Aalborg, Denmark 12 sep 87.
;;; Some cleanup 17 Feb 93.
;;;
;;; This file is hopefully to small to be affected by any form of copyright.

;;; @@ The Mode Line

(defvar auto-indent-mode nil
  "True, iff we are in auto indent mode.")

  (make-variable-buffer-local 'auto-indent-mode)

(or (assq 'auto-indent-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(auto-indent-mode " Indent") minor-mode-alist)))

;;; @@ Minor Map

(defvar auto-indent-keymap nil
  "Keymap used for auto-indent-mode commands.")

(if auto-indent-keymap
    ()
  (setq auto-indent-keymap (make-sparse-keymap)) 
  (define-key auto-indent-keymap "\t" 'indent-relative)
  (define-key auto-indent-keymap "\n" 'newline-and-indent))

;;; @@ The Mode

(defun auto-indent-mode (&optional arg)
  "Toggle auto indent mode.
With prefix arg, turn auto indent mode on iff arg is positive.

Auto indent mode works by invoking indent-relative for TAB,
and using indent-relative-maybe as the indent-line-function for auto-fill,
and LFD."  
  (interactive "P")
  (if (or (and (null arg) auto-indent-mode)
	  (<= (prefix-numeric-value arg) 0))
      ;; Turn it off
      (if auto-indent-mode
	  (minor-unbind 'auto-indent-mode))
    ;;Turn it on
    (if auto-indent-mode
	()
      (minor-add-to-keymap 'auto-indent-mode auto-indent-keymap)
      (minor-set-variable 'auto-indent-mode
			  'indent-line-function 'indent-relative-maybe)
      (minor-set-variable 'auto-indent-mode 'auto-indent-mode t)))
  ;; Update mode line
  (set-buffer-modified-p (buffer-modified-p)))

;;; @@ Emacs

(run-hooks 'after-min-ind-hook)

;;; Local Variables:
;;; mode: emacs-lisp
;;; mode: outline-minor
;;; outline-regexp: ";;; @+\\|(......"
;;; End:
