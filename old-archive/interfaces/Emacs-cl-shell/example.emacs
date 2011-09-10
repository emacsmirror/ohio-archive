;;;; This file contains example expressions that could be placed in
;;;; your personal emacs startup (.emacs) file.  It is divided into
;;;; sections corresponding to the files in the distibution.  The
;;;; lines marked ";***" should be customized for your system.


;;; Make sure emacs knows where to find the emacs-lisp code:
(setq load-path (cons "/usr/local/emacs/extensions" load-path)) ;***


;;;; -----------------------------------------------------------------
;;;; cl-shell.el 
;;;; (also: cl-lucid.el, cl-clos.el, cl-pcl.el, cl-flavors.el,
;;;; cl-obvius.el, shell-history.el, source-file-extensions.lisp).

;;; To run Common Lisp as a sub-shell inside emacs, type "M-x run-cl".
;;; To get a list of key bindings, type "C-h f cl-shell-mode", or see
;;; the file cl-shell.doc

;;; Command for running a Common Lisp sub-shell.
(setq *cl-program* "/usr/local/bin/lucid-pcl.5-89")  ;***
(autoload 'run-cl "cl-shell" "" t)

;;; Bind C-M-l globally to goto *lisp* buffer, or run lisp if it isn't
;;; running.
(global-set-key "\C-\M-l" 'cl-goto-lisp-buffer)
(autoload 'cl-goto-lisp-buffer "cl-shell" "" t)

;;; Some cl-shell parameters:
(setq *cl-echo-commands* t)		;echo defuns in lisp....
(setq *cl-pop-up* t)			;see cl-shell.el

;;; Run another lisp, with a different prompt, and send some
;;; initialization commands to it.
(defun run-development-lisp ()
  (interactive)
  (require "cl-shell")
  (let ((*cl-replacement-prompt*  "Lucid-4.0> "))  ;***
    (run-cl "/usr/local/lucid-4.0"))     	;load Beta version of Lucid4.0
  (cl-send-string "(load \"/usr/local/lucid-4.0/patches\")\n"))  ;***

;;; Stuff for running OBVIUS (Object-Based Vision and Image
;;; Undertanding System).
(setq *obvius-program* "/usr/local/bin/obvius")  ;***
(autoload 'run-obvius  "cl-obvius" "" t)


;;;; -----------------------------------------------------------------
;;;; completion.el  (also always-complete.el, advise.el).

;;; Completion code from TMC.  Default binding is M-<return>.
(global-set-key "\M-\r" 'complete)
(autoload 'complete "always-complete" "" t)

;;; If you don't want the minibuffer reminders, use this line instead
;;; of the one above.
; (autoload 'complete "always-complete" "" t)


;;;; -----------------------------------------------------------------
;;;; misc-extensions.el

;;; Note: you must have set your load-path (see first command in this
;;; file) for this to work!
(load "misc-extensions")

;;; Set indentation for Common Lisp
(setq lisp-indent-hook 'common-lisp-indent-hook)

;;; Some useful functions which delete excess whitespace
(global-set-key "\C-x " 'delete-forward-whitespace) 
(rebind-keys-which-call 'just-one-space 'my-just-one-space) ;typically M-spc

;;; When a close-paren is typed on top of an existing paren, blink the
;;; match, but don't insert a new paren
(setq blink-paren-hook
      '(lambda ()
	 (if (and (not (eobp))
		  (char-equal (char-after (point)) last-input-char))
	     (delete-char 1))		;UGLY - this sets modification flag!
	 (blink-matching-open)))

;;; C-M-s repositions window with point or defun at top
(global-set-key "\M-\C-s" 'reposition-point-at-top)
(define-key lisp-mode-map "\M-\C-s" 'reposition-defun-at-top)
(define-key emacs-lisp-mode-map "\M-\C-s" 'reposition-defun-at-top)

;; Grep for symbol nearest mouse in *.lisp files.
(define-key lisp-mode-map "\M-\C-g" 'cl-grep-for-symbol)

;;; Switch bindings so that standard carriage return indents the new line:
(define-key lisp-mode-map "\C-m" 'newline-and-indent)
(define-key lisp-mode-map "\n" 'newline)     ;this is control-<cr> on Suns
(define-key emacs-lisp-mode-map "\C-m" 'newline-and-indent)
(define-key emacs-lisp-mode-map "\n" 'newline)

;;; The next 4 expressions fix comment paragraph filling in lisp:
(rebind-keys-which-call 'fill-paragraph 'lisp-fill-paragraph lisp-mode-map)
(rebind-keys-which-call 'fill-paragraph 'lisp-fill-paragraph emacs-lisp-mode-map)
(setq lisp-mode-hook
      '(lambda () 
	(message "Running lisp-mode-hook.")
	(setq paragraph-start
	 (concat "^[ \t]*[^ ; \t]\\|^[ \t]*$\\|" (or paragraph-start "")))
	(setq paragraph-separate paragraph-start)
	(setq buffer-file-name
	      (and buffer-file-name (expand-symlinks buffer-file-name)))))
(setq emacs-lisp-mode-hook
      '(lambda () 
	(message "Running lisp-mode-hook.")
	(setq paragraph-start
	 (concat "^[ \t]*[^ ; \t]\\|^[ \t]*$\\|" (or paragraph-start "")))
	(setq paragraph-separate paragraph-start)))

;;; Bind C-middle-mouse to copy the sexpr under the mouse to the point.
;;; This amazingly useful!
(if (and (eq window-system 'x) (= window-system-version 11))
    (progn
      (require 'x-mouse)    ; load this from the standard Emacs distribution
   ;;; RAW:
      (define-key mouse-map x-button-left       'x-mouse-set-point)
      (define-key mouse-map x-button-left-up    'x-cut-text)          ;drag left
      (define-key mouse-map x-button-middle     'x-paste-text)
      (define-key mouse-map x-button-right      'x-cut-text)
   ;;; C: deleting text (these are parallel to raw bindings)
      (define-key mouse-map x-button-c-left     'x-mouse-set-point)
      (define-key mouse-map x-button-c-left-up  'x-cut-and-wipe-text) ;drag C-left
      (define-key mouse-map x-button-c-middle   'x-copy-sexp)
      (define-key mouse-map x-button-c-right    'x-cut-and-wipe-text)
   ;;; M: sexp manipulation
      (define-key mouse-map x-button-m-left     'x-move-sexp)
      (define-key mouse-map x-button-m-middle   'x-replace-sexp)      
      (define-key mouse-map x-button-m-right    'x-zap-sexp)
   ;;; S: scrolling
      (define-key mouse-map x-button-s-left     'x-line-to-top)
      (define-key mouse-map x-button-s-middle   'x-line-to-middle)
      (define-key mouse-map x-button-s-right    'x-line-to-bottom)
   ;;; M-S: window operations
      (define-key mouse-map x-button-m-s-left   'x-mouse-keep-one-window)
      (define-key mouse-map x-button-m-s-right  'x-mouse-select-and-split)
   ;;; C-S: menus
      (define-key mouse-map x-button-c-s-left   'x-buffer-menu)
      (define-key mouse-map x-button-c-s-middle 'x-help)))
