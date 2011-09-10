;;; First things first: fix brain-damaged XON/XOFF

(set-input-mode t nil)

;;; Enable eval-ing of an emacs-lisp expression.

(put 'eval-expression 'disabled nil)

;;; Tell emacs where to look for customization files.

(setq load-path (cons (expand-file-name "/usr/u/cap/.elisp") load-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Extend default extensions to use my own set. It's faster when the
;;; ones I use are at the beginning of the alist.  Force a file with .lsp 
;;; extension into clisp-mode.

(setq auto-mode-alist (append
		       '(("\\.txt$" . nroff-mode)
			 ("\\.bib$" . nroff-mode)
			 ("\\.ltr$" . nroff-mode)
			 ("\\.ref$" . refer-mode))
		       auto-mode-alist))

;;; Set up the autoinclude file hack.

(load "autoinclude" nil t)
(setq auto-include-alist (append
			  '(("\\.lsp$" . "header.lsp"))
			  auto-include-alist))
(setq auto-include-directory "/usr/u/cap/.auto/")

;;; Set up autoloading of refer.el

(defun refer-mode ()
  (load "refer")
  (refer-mode))

;;; Change the lisp-mode-hook to load the inferior lisp process stuff.
;;; If you want your default lisp to start on another machine, use
;;; (start-lisp "hostname") instead.

(setq lisp-mode-hook '(lambda ()
			(require 'clisp)
			(start-lisp)))

;;; Set the mode hooks for other common modes.

(setq text-mode-hook '(lambda ()
			(auto-fill-mode 1)))
(setq nroff-mode-hook '(lambda ()
			 (electric-nroff-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Key bindings.

;;; Global replace
(global-set-key "r" 'replace-string)
(global-set-key "" 'query-replace)

;;; Change other window commands.
(global-set-key "n" 'select-next-window)
(global-set-key "p" 'select-previous-window)
(global-set-key "d" 'delete-window)
(global-set-key "v" 'find-alternate-file)
(global-set-key "" 'find-file-other-window)
(global-set-key "t" 'line-to-top)

;;; Add kill-some-buffers.
(global-set-key "" 'kill-some-buffers)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Here are the next and previous window functions.

(defun select-next-window ()
  (interactive)
  (other-window 1))

(defun select-previous-window ()
  (interactive)
  (other-window -1))

;;; Brings current line to top of window.

(defun line-to-top ()
  (interactive)
  (recenter 0))
