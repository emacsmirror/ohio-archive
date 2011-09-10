;;;; Russell Ritchie, <russell@uk.ac.strath.hci>. Wed Nov 9 16:45:40 1988

;;;; Set up the default mode specific menus to load on demand.
;;;; Warning: this file is intended for Lusers, and simply 
;;;; appends it's actions to the mode-hooks it affects.

(provide 'def-menus)
(require 'utilities)			; Just in case...

(defvar menu-init-quiet nil
  "*If non-nil the menu software will not announce it's arrival.")

(defun add-to-hook (hook action)
  "Return HOOK with ACTION added. Incredibly simplistic."
  (append (and (boundp hook) (symbol-value hook)) (list action)))

(setq term-setup-hook
      (function
       (lambda ()
	 (cond
	  ((eq window-system 'x)
	   (require 'x-menus)
	   (or menu-init-quiet
	       (message "Press the %s mouse button in the text region for X menus."
			(cdr (assoc (string-to-int x-menu-mouse-binding)
				    x-button-help-alist)))))
	  ((getenv "IN_EMACSTOOL")	; Peck says this defines Emacstool env.
	   (require 'sun-menus)
	   (sun-init)
	   (or menu-init-quiet (sun-menu-banner)))
	  (t (require 'term-menus)	; Assume simple terminal
	     (term-menu-init)
	     (or menu-init-quiet
		 (message
		  (substitute-command-keys
		   "Type \\[emacs-term-menu] for menus."))))))))

(setq text-mode-hook
      (add-to-hook
       'text-mode-hook
       (function (lambda ()
		   (require 'text-menus)
		   (set-mode-menu 'text-menu)))))

(setq TeX-mode-hook
      (add-to-hook
       'TeX-mode-hook
       (function (lambda ()
		   (require 'TeX-menus)
		   (set-mode-menu 'TeX-menu)))))

(setq LaTeX-mode-hook
      (add-to-hook
       'LaTeX-mode-hook
       (function (lambda ()
		   ;; TeX-mode-hook is called first so LaTeX-menu is loaded.
		   (set-mode-menu 'LaTeX-menu)))))

(setq BibTeX-mode-hook
      (add-to-hook
       'BibTeX-mode-hook
       (function (lambda ()
		   (require 'BibTeX-menus)
		   (set-mode-menu 'BibTeX-menu)))))

(setq lisp-mode-hook
      (add-to-hook
       'lisp-mode-hook
       (function (lambda ()
		   (require 'lisp-fns) 
		   (require 'lisp-menus)
		   (if (eq major-mode 'lisp-mode) ; Inferior lisps inherit
		       (set-mode-menu 'lisp-menu)))))) ; from shell-mode.

(setq emacs-lisp-mode-hook
      (add-to-hook
       'emacs-lisp-mode-hook
       (function (lambda ()
		  (require 'elisp-menus)
		  (set-mode-menu 'emacs-lisp-menu)))))

(setq prolog-mode-hook
      (add-to-hook
       'prolog-mode-hook
       (function (lambda ()
		   (require 'Prolog-menus)
		   (if (eq major-mode 'prolog-mode)
		       (set-mode-menu 'prolog-menu))))))

(setq shell-mode-hook
      (add-to-hook
       'shell-mode-hook
       (function (lambda ()
		   (require 'shell-menus)
		   (set-mode-menu 'shell-menu)))))

(setq dired-mode-hook
      (add-to-hook
       'dired-mode-hook
       (function (lambda ()
		   (require 'dired-menus)
		   (set-mode-menu 'dired-menu)
		   (cond ((eq window-system 'x)
			  (require 'dired-x)
			  (local-set-mouse
			   'text x-button-middle 'x-mouse-inspect-file)
			  (local-set-mouse
			   'text x-button-s-middle 'x-mouse-inspect-file-other-window))
			 ((getenv "IN_EMACSTOOL")
			  (require 'dired-sun)
			  (define-mouse current-local-mousemap
			    '(left middle text) 'mouse-inspect-file)
			  (define-mouse current-local-mousemap
			    '(left shift middle text) 'mouse-inspect-file-other-window)))))))

(setq rmail-mode-hook
      (add-to-hook
       'rmail-mode-hook
       (function (lambda ()
		   (require 'rmail-menus)
		   (set-mode-menu 'rmail-menu)))))

(setq mail-mode-hook
      (add-to-hook
       'mail-mode-hook
       (function (lambda ()
		   (require 'mail-menus)
		   (set-mode-menu 'mail-menu)))))
