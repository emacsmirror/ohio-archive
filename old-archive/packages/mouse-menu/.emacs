;;; An example personal GNUemacs initialisation file.
;;; Last modified: Thu Mar 2 12:18:59 1989.

;;; BEWARE!! This file does some strange things and you probably won't like it.
;;; Consider it solely as an example of setting menus and mouse bindings.

;;; Set up some default things and load the libraries I always use.

(autoload 'typeout-buffer	"typeout" 	"Typeout buffer stuff." t)
(setq-default typeout-command-asychronous t) ; I don't want to wait.
(setq-default typeout-kill-buffers t)	; I want typeout buffers to die.

(require 'shell)			; I use this a *lot*.
(require 'utilities)			; I require my new extended stuff.

(setq inhibit-default-init t)		; This file assumes it knows better.

;; Set up various mode hooks...

(setq term-setup-hook
      (function
       (lambda ()
	 (cond
	  ((eq window-system 'x)
	   ;; Standard V18 X setup.
	   (require 'x-fns)
	   (require 'x-menus)
	   (require 'x-plus)
 	   ;; Swap the modeline bindings of Control-right and Control-left.
	   ;; Control-left now `lowers' Emacs windows.
	   ;; Control-right now `raises' Emacs windows.
	   (global-set-mouse
	    'modeline x-button-c-right 'x-mouse-delete-other-windows)
	   (global-set-mouse
	    'modeline x-button-c-left 'x-mouse-delete-window)
	   (if (x-color-p)
	       (progn
		 (x-set-cursor-color "DarkOrchid")
		 (x-set-foreground-color "Navy Blue")
		 (x-set-background-color "Lavender")
		 (x-set-mouse-color "Blue Violet")))
	   (x-set-baud 1000000))	; Make redisplay *really* fast.
	  ((getenv "IN_EMACSTOOL")	; Peck says this defines Emacstool env.
	   (require 'sun-menus)
	   (require 'sun-plus)
	   (if (let* ((edges (window-edges (selected-window)))
		      (x (/ (- (car (cdr (cdr edges))) (car edges)) 2))
		      (y (/ (- (car (cdr (cdr (cdr edges)))) (car (cdr edges)))
			    2)))
		 (sun-mouse-yes-or-no-p
		  "Should the mouse be used for all Y/N confirmation requests?"
		  (selected-window) x y))
	       (fset 'y-or-n-p 'sun-mouse-y-or-n-p))
	   (sun-init)
	   (sun-menu-banner))
	  (t (setq term-menu-buffer-switch t) ; Electric Switch Buffer.
	     (require 'term-menus)	; Assume simple terminal
	     (term-menu-init))))))

(setq NeWS-setup-hook
      '(lambda ()
	 (defun set-mode-menu (ignored) ; Fix this someday...
	   "Setting of mode menus is unsupported under NeWS.")
	 (NeWS-set-font "Screen" 14)
	 (NeWS-set-dimensions 80 34)
	 (NeWS-set-origin 178 85)))

(setq text-mode-hook
      (function (lambda ()
		  (auto-fill-mode 1)	; Turn on auto-fill when for text and
		  (abbrev-mode 1)	; Turn on abbreviation expansion.
		  (setq case-fold-search nil)))) ; Make search case-sensitive.

(setq TeX-mode-hook
      (function (lambda ()
		  (make-local-variable 'TeX-directory)
		  (setq TeX-directory default-directory)
		  (setq TeX-dvi-print-command "lpw")
		  (require 'TeX-menus)
		  (set-mode-menu 'TeX-menu)
		  (cond ((getenv "NEWSSERVER") ; NeWS or X/NeWS
			 (setq TeX-screen-print-command "dvinews"))
			((eq window-system 'x) ; X 
			 (setq TeX-screen-print-command "xdvi"))
			((getenv "IN_EMACSTOOL") ; Emacstool
			 (setq TeX-screen-print-command "dvipage"))
			(t nil)))))

(setq LaTeX-mode-hook
      (function (lambda ()
		  (local-set-key "\C-cc" 'LaTeX-cite)
		  (local-set-key "\C-ci" 'LaTeX-index)
		  (local-set-key "\C-cl" 'LaTeX-label)
		  (local-set-key "\C-cr" 'LaTeX-reference)
		  (local-set-key "\C-cs" 'LaTeX-section)
		  (local-set-key "\C-ct" 'LaTeX-typestyle)
		  (local-set-key "\C-cv" 'LaTeX-verbatim)
		  (local-set-key "\C-c\C-a" 'TeX-spell-all-TeX-buffers)
		  (local-set-key "\C-c\C-x" 'TeX-BibTeX-buffer)
		  (local-set-key "\e\C-q" 'query-replace-regexp)
		  ;; TeX-mode-hook is called first so LaTeX-menu is loaded
		  (set-mode-menu 'LaTeX-menu))))

(setq BibTeX-mode-hook
      (function (lambda ()
		  (require 'BibTeX-menus)
		  (set-mode-menu 'BibTeX-menu))))

(setq electric-buffer-menu-mode-hook
      (function (lambda ()
		  (local-set-key "\C-x" (make-sparse-keymap))
		  (local-set-key "\C-c\C-m" 'x-multiple-map-flush-mouse-queue)
		  (local-set-key "\C-x\C-m" 'x-multiple-map-flush-mouse-queue)
		  (local-set-key "\C-x\C-@" 'x-multiple-map-flush-mouse-queue)
		  (require 'ebuff-menus)
		  (local-set-mouse 'text x-button-left 'x-mouse-ebuff-show)
		  (local-set-mouse 'text x-button-middle 'x-mouse-ebuff-select)
		  (set-mode-menu 'electric-buffer-menu))))

(setq prolog-mode-hook
      (function (lambda ()
		  (local-set-key "\e\C-i" 'shell-expand-file-name) ; M-TAB
		  (require 'Prolog-menus)
		  (if (eq major-mode 'prolog-mode) ; Inferior prolog inherits 
		      (set-mode-menu 'prolog-menu)) ; menu from shell-mode.
		  (local-set-key "\C-c\C-p" 'lpr-buffer))))

(setq shell-mode-hook
      (function (lambda ()
		  (autoload 'shell-expand-file-name "shell-expand-file-name")
		  (local-set-key "\C-i" 'shell-expand-file-name) ; TAB
		  (require 'shell-menus)
		  (set-mode-menu 'shell-menu))))

(setq dired-mode-hook
      (function (lambda ()
		  (require 'dired-menus)
		  (set-mode-menu 'dired-menu)
		  (cond ((eq window-system 'x) ; X
			 (require 'dired-x)
			 (local-set-mouse
			  'text x-button-middle 'x-mouse-inspect-file)
			 (local-set-mouse
			  'text x-button-s-middle
			  'x-mouse-inspect-file-other-window)) 
			((getenv "IN_EMACSTOOL") ; Emacstool
			 (require 'dired-sun)
			 (define-mouse current-local-mousemap
			   '(left middle text) 'mouse-inspect-file)
			 (define-mouse current-local-mousemap
			   '(left shift middle text)
			   'mouse-inspect-file-other-window))
			(t nil)))))

(setq rmail-mode-hook
      (function (lambda ()
		  (require 'rmail-menus)
		  (set-mode-menu 'rmail-menu))))

(setq rmail-summary-mode-hook
      (function (lambda ()
		  (require 'rmail-menus)
		  (set-mode-menu 'rmailsum-menu)
		  (require 'x-rmailsum-fns)
		  (local-set-mouse
		   'text x-button-left 'x-mouse-rmailsum-goto-msg)
 		  (local-set-mouse
 		   'text x-button-middle 'x-mouse-rmailsum-exit))))

(setq mail-mode-hook
      (function (lambda ()
		  (require 'mail-menus)
		  (set-mode-menu 'mail-menu))))

(setq lisp-mode-hook
      (function (lambda ()
		  (abbrev-mode 1)	; Turn on abbrev-mode.
		  (require 'lisp-fns)	; Load my lisp stuff if necessary...
		  (setq case-fold-search nil) ; & search case-sensitive.
		  (require 'lisp-menus)
		  (if (eq major-mode 'lisp-mode) ; Inferior Lisp inherits its 
		      (set-mode-menu 'lisp-menu)) ; menu from shell-mode.
		  (if (eq major-mode 'lisp-mode)
		      (progn
			(local-set-key "\e\C-i" 'shell-expand-file-name) 
			(local-set-key "\C-c\C-p" 'lpr-buffer))))))

(setq emacs-lisp-mode-hook
      (function (lambda ()
		  (require 'elisp-menus)
		  (local-set-key "\C-c\C-p" 'lpr-buffer)
		  (set-mode-menu 'emacs-lisp-menu))))

;; The remaining hooks are specific to Allegro Common Lisp, but could
;; possibly be made to work with other Common Lisps with a little work.
;; I'm not certain what parts of Franz Inc's Emacs interface are
;; proprietary: the majority of this stuff won't work without their
;; underlying code (and a few small patches from me). If you've got
;; their stuff already mail me and I'll send you the patches.

(setq fi:emacs-lisp-mode-hook emacs-lisp-mode-hook)

(setq clman:mode-hook
      (function (lambda ()
		  (if (and (eq window-system 'x)
			   (eq window-system-version 11))
		      (progn
			(require 'x-cl-fns)
			(local-set-mouse
			 'text x-button-middle 'x-mouse-ignore)
			(local-set-mouse
			 'text x-button-middle-up 'x-mouse-clman))))))

(setq fi:common-lisp-mode-hook
      (function (lambda ()
		  (local-set-key "\C-hl" 'fi:clman)
		  (local-set-key "\C-hL" 'view-lossage)
		  (setq case-fold-search nil)
		  (require 'cl-indent)
		  (make-variable-buffer-local 'lisp-indent-hook) ; Only for CL.
		  (setq lisp-indent-hook 'common-lisp-indent-hook)
		  (put 'if 'common-lisp-indent-hook 2) ; Emacs-if indenting.
		  (put 'until 'common-lisp-indent-hook 1)
		  (put 'while 'common-lisp-indent-hook 1)
		  (put 'defmethod 'common-lisp-indent-hook 2) ; PCL is wierd.
		  (put 'with-slots 'common-lisp-indent-hook 2)
		  (put 'with-accessors 'common-lisp-indent-hook 2)
		  (put 'with-graphics-batching 'common-lisp-indent-hook 0)
		  (if (and (eq window-system 'x) (eq window-system-version 11))
		      (progn
			(require 'x-cl-fns)
			(local-set-mouse
			 'text x-button-c-m-s-left 'x-mouse-ignore)
			(local-set-mouse
			 'text x-button-c-m-s-left-up 'x-mouse-describe)
			(local-set-mouse
			 'text x-button-c-m-s-middle 'x-mouse-ignore)
			(local-set-mouse
			 'text x-button-c-m-s-middle-up 'x-mouse-arglist)))
		  (local-set-key "\C-c\C-p" 'lpr-buffer)
		  (require 'cl-menus)
		  (set-mode-menu 'cl-menu)
		  (setq
		   kill-emacs-hook	; Only if we've loaded Franz stuff.
		   (function
		    (lambda ()
		      (fi:remove-all-temporary-lisp-transaction-files)))))))

(setq fi:inferior-common-lisp-mode-hook
      (function (lambda ()
		  (local-set-key "\C-hl" 'fi:clman)
		  (local-set-key "\C-hL" 'view-lossage)
		  (setq mode-line-process (list ": %s"))
		  (require 'infcl-menus)
		  (set-mode-menu 'inferior-cl-menu))))

