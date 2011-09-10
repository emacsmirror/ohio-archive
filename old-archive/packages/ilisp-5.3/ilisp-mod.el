;;; -*-Emacs-Lisp-*-
;;; $Id: ilisp-mod.el,v 1.13 1993/06/29 06:13:12 ivan Rel $
;;;%Header
;;; Copyright (C) 1990, 1991, 1992, 1993 Chris McConnell, ccm@cs.cmu.edu.
;;;
;;; Send mail to ilisp-bug@darwin.bu.edu if you have problems.
;;;
;;; Send mail to ilisp-request@darwin.bu.edu if you want to be on the
;;; ilisp mailing list.
;;;
;;;


;;;
;;; ILISP mode top level definitions.
;;; 


;;;%ilisp-mode

;;;
(defun ilisp-set-doc (function string)
  "Set the documentation of the symbol FUNCTION to STRING."
  (let* ((old-function (symbol-function function)))
    (if (consp old-function)  ; old-style v18 compiled-function objects
	;; I did not use rplacd so that I can replace read-only objects
	(let ((old-doc (cdr (cdr old-function))))
	  (fset function
		(nconc (list (car old-function)
			     (car (cdr old-function))
			     string)
		       (if (or (stringp (car old-doc)) 
			       (numberp (car old-doc)))
			   (cdr old-doc)
			 old-doc))))
      ;; else, new-style compiled-code objects
      (let ((code-as-list (append old-function nil)))
	(if (nthcdr 4 code-as-list)
	    (setcar (nthcdr 4 code-as-list) string)
	  (setcdr (nthcdr 3 code-as-list) (cons string nil)))
	(fset function (apply 'make-byte-code code-as-list))))))


;;;
(defun ilisp-mode ()
  (interactive)
  (run-ilisp))
(ilisp-set-doc 'ilisp-mode ilisp-documentation)
(ilisp-set-doc 'lisp-mode ilisp-documentation)

;;;%%ILISP
(defun lisp-command-args (string)
  "Break up STRING into (command args ...)."
  (let ((len (length string))
	(position 0)
	(arg 0)
	(args nil))
    (while (< position len)
      (if (eq (aref string position) ?\ )
	  (setq args (cons (substring string arg position)  args)
		arg (1+ position)))
      (setq position (1+ position)))
    (setq args (reverse (cons (substring string arg position)  args)))
    args))



;;;
(defun ilisp (name setup)
  "Run an inferior LISP process NAME, input and output via buffer *name*.
If there is a process already running in *name*, just switch to that buffer.
Takes the program name from the variable ilisp-program.
\(Type \\[describe-mode] in the process buffer for a list of commands.)"
  (set-buffer ilisp-buffer)
  (if (not (comint-check-proc ilisp-buffer))
      (let* ((dialect (car ilisp-dialect))
	     (program ilisp-program)
	     (args (lisp-command-args program))
	     ;; Use pipes so that strings can be long
	     (process-connection-type nil)
	     (names (format "%s" name))
	     start)
	(apply 'make-comint name (car args) nil (cdr args))
	(comint-setup-ipc)
	;; Because comint-mode kills all buffer-local variables in
	;; fsf-19 we have to re-call the setup here.
	(funcall setup name)
	(setq major-mode 'ilisp-mode
	      mode-name "ILISP")
	(rplaca (car comint-send-queue) 
		(function (lambda ()
			    (run-hooks 'ilisp-init-hook))))
	(setq ilisp-initialized (lisp-del ilisp-buffer ilisp-initialized))
	(if (not (lisp-memk names ilisp-buffers 'car))
	    (setq ilisp-buffers (cons (list names) ilisp-buffers)))
	(lisp-pop-to-buffer ilisp-buffer)
	(setq start (window-start (selected-window))
	      ilisp-program program)
	(goto-char (point-max))
	(insert (format "Starting %s ...\n" ilisp-program))
	(set-marker (process-mark (ilisp-process)) (point))
	(funcall comint-update-status 'start)
	(if ilisp-motd
	    (progn (lisp-display-output (format ilisp-motd ilisp-version))
		   (set-window-start (selected-window) start)))
	(if (not ilisp-prefix-match) (require 'completer)))
      (lisp-pop-to-buffer ilisp-buffer))
  (use-local-map ilisp-use-map)
  ;; This is necessary to get mode documentation to come out right
  (set-default 'ilisp-use-map ilisp-use-map))


;;;%Manual
(autoload 'fi:clman         "fi/clman" 
	  "Look up SYMBOL in the online manual with completion." t)
(autoload 'fi:clman-apropos "fi/clman" 
	  "Do an apropos search in online manual for STRING." t)

;;;%Bridges
(autoload 'install-bridge "bridge" "Install process bridge." t)

;;;%Modes
(set-default 'auto-mode-alist
	     (append '(("\\.cl$" . lisp-mode) ("\\.lisp$" . lisp-mode))
		     auto-mode-alist))
(setq completion-ignored-extensions 
      (append '(".68fasl" ".sfasl" ".ifasl" ".pfasl" 
		".68fasl4" ".sfasl4" ".ifasl4" ".pfasl4" 
		".sbin")
	      completion-ignored-extensions))

(provide 'ilisp-mod)