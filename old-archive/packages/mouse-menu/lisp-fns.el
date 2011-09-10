;;;; Russell Ritchie, Scottish HCI Centre, <russell@uk.ac.strath.hci>. Tue Nov  8 13:02:17 1988
;;;; Various utilities and modifications specific to Lisp. Incorporates 'lisp-eval' also.

(require 'shell)
(require 'utilities)
(provide 'lisp-fns)

(defvar inferior-lisp-program "lisp"
  "*The program to invoke in the Lisp buffer. 
A buffer-local variable, in order to allow different dialects 
of Lisp to be edited and run simultaneously. Defaults to \"lisp\".")

(defvar inferior-lisp-buffer nil
  "The buffer the inferior Lisp process is running in.
Do NOT set this variable, it is bound automatically after determination of which program to run.")

(defun run-lisp ()
  "The same as the distribution run-lisp, but uses inferior-lisp-program for the name of the Lisp buffer.
Binds the buffer-local variable  inferior-lisp-buffer  to the name of the Lisp buffer."
  (interactive)
  (make-shell (file-name-nondirectory inferior-lisp-program)
	      (if (string-match "~" inferior-lisp-program)
		  ;; User has specified a ~-relative program, so...
		  (expand-file-name inferior-lisp-program)
		inferior-lisp-program))
  (setq inferior-lisp-buffer (concat "*" (file-name-nondirectory inferior-lisp-program) "*"))
  (switch-to-buffer inferior-lisp-buffer)
  (inferior-lisp-mode))

(defun inferior-lisp-call (func-and-args &optional go-afterwards)
  "Pass FUNC-AND-ARGS to inferior-lisp process and do a (shell-send-input).
Create LISP if necessary. 
If optional 2nd arg. GO-AFTERWARDS is t pop to LISP buffer after
 (shell-send-input) to see what is happening."
  (if (or (string= func-and-args "") (white-spacep func-and-args))
      (error "Empty or white-space function call passed to inferior-lisp-call.")
    (if (> (length func-and-args) 255)
	(error "Inferior-lisp-call can only send s-expressions of less than 256 characters.")
      (if go-afterwards 
	  (progn
	    (run-lisp)			; Create LISP (if none) & goto end.
	    (process-send-string (get-buffer-process (current-buffer)) func-and-args)
	    (shell-send-input))
	(save-excursion
	  (run-lisp)
	  (process-send-string (get-buffer-process (current-buffer)) func-and-args)
	  (shell-send-input))))))

(defun allegro-lisp-mode ()
  "Major mode for editing Allegro Common Lisp code."
  (interactive)
  (lisp-mode)
  (setq mode-name "Allegro Common Lisp")
  (make-local-variable 'inferior-lisp-program)
  (setq inferior-lisp-program "cl")
  (make-local-variable 'inferior-lisp-load-command)
  (setq inferior-lisp-load-command
	"(progn (load \"%s\" :verbose nil :print t) (values))\n")
  (make-local-variable 'inferior-lisp-buffer) ; Set by #'run-lisp, when the buffer is actually created.
  (run-hooks 'allegro-lisp-mode-hook))

(defun franz-lisp-mode ()
  "Major mode for editing Franz Lisp code."
  (interactive)
  (lisp-mode)
  (setq mode-name "Franz Lisp")
  (make-local-variable 'inferior-lisp-program)
  (setq inferior-lisp-program "lisp")
  (make-local-variable 'inferior-lisp-load-command)
  (setq inferior-lisp-load-command "(load \"%s\")\n")
  (make-local-variable 'inferior-lisp-buffer) ; Set by #'run-lisp, when the buffer is actually created.
  (run-hooks 'franz-lisp-mode-hook))

(defun kyoto-lisp-mode ()
  "Major mode for editing Kyoto Common Lisp code."
  (interactive)
  (lisp-mode)
  (setq mode-name "Kyoto Common Lisp")
  (make-local-variable 'inferior-lisp-program)
  (setq inferior-lisp-program "kcl")
  (make-local-variable 'inferior-lisp-load-command)
  (setq inferior-lisp-load-command
	"(progn (load \"%s\" :verbose nil :print t) (values))\n") 
  (make-local-variable 'inferior-lisp-buffer) ; Set by #'run-lisp, when the buffer is actually created.
  (run-hooks 'kyoto-lisp-mode-hook))

(defun start-allegro-lisp ()
  "
Called when a file ending in \".cl\" is visited. Starts
an inferior lisp process (using (run-lisp))
in another window and displays that and the file, leaving the
cursor at the top of the file buffer."
  (interactive)
  (allegro-lisp-mode)			; Turn on lisp-mode in the ".cl" file.
  (let ((this (current-buffer))
	(pop-up-windows t))
    (run-lisp)				; Start up a lisp process.
    (pop-to-buffer this)))

(defun start-franz-lisp ()
  "
Called when a file ending in \".l\" is visited. Starts an
inferior lisp process (using (run-lisp inferior-lisp-program))
in another window and displays that and the file, leaving 
the cursor at the top of the file buffer."
  (interactive)
  (franz-lisp-mode)			; Turn on franz-mode in the ".l" file.
  (let ((this (current-buffer))
	(pop-up-windows t))
    (run-lisp)				; Start up a lisp process.
    (pop-to-buffer this)))

(defun start-kyoto-lisp ()
  "
Called when a file ending in \".lsp\" is visited. Starts an
inferior lisp process (using (run-lisp inferior-lisp-program))
in another window and displays that and the file, leaving 
the cursor at the top of the file buffer."
  (interactive)
  (kyoto-lisp-mode)			; Turn on kyoto-mode in the ".lsp" file.
  (let ((this (current-buffer))
	(pop-up-windows t))
    (run-lisp)				; Start up a lisp process.
    (pop-to-buffer this)))

(or (fboundp 'common-lisp-mode) (fset 'common-lisp-mode 'kyoto-lisp-mode)) ; Default Common LISP
(or (fboundp 'start-common-lisp) (fset 'start-common-lisp 'start-kyoto-lisp)) ; is KCL.

(defun lisp-send-region (beg end &optional display-flag)
  "Send the current region to the Lisp process.
With argument, force redisplay and scrolling of the buffer 
the inferior Lisp process is running in (note that there may
be more than 1 inferior Lisp process). 
Variable `inferior-lisp-load-command' controls formatting of
the `load' form that is set to the Lisp process."
  (interactive "r\nP")
  (message "Sending region...")
  (save-excursion
    (let* ((inferior-lisp-program
	    (file-name-nondirectory inferior-lisp-program))
	   (filename
	    (if (and inferior-lisp-buffer ; We've started one...
		     (get-process inferior-lisp-program)) ; It's still going.
		(format "/tmp/emlisp%d"
			(process-id (get-process inferior-lisp-program)))
	      (let ((mesg (format "%starting %s process..."
				  (if inferior-lisp-buffer "Res" "S")
				  inferior-lisp-program)))
		(message mesg)		       
		(save-excursion (run-lisp))
		(sit-for 1)		; Give Lisp time to start up....
		(message (concat mesg "done"))
		(format "/tmp/emlisp%d"
			(process-id (get-process inferior-lisp-program)))))))
      (write-region beg end filename nil 'nomessage)
      (process-send-string inferior-lisp-program
			   (format inferior-lisp-load-command filename))
      (if display-flag
	  (let* ((process (get-process inferior-lisp-program))
		 (buffer (process-buffer process))
		 (w (or (get-buffer-window buffer) (display-buffer buffer)))
		 (height (window-height w))
		 (end))
	    (save-excursion
	      (set-buffer buffer)
	      (setq end (point-max))
	      (while (progn
		       (accept-process-output process)
		       (goto-char (point-max))
		       (beginning-of-line)
		       (or (= (point-max) end)
			   (not (looking-at inferior-lisp-prompt)))))
	      (setq end (point-max))
	      (vertical-motion (- 4 height))
	      (set-window-start w (point)))
	    (set-window-point w end)))
      (message "Sent region!"))))

(defun lisp-send-region-and-go (beg end &optional ignored)
  "Send to current region and go to Lisp process."
  (interactive "r\nP")
  (lisp-send-region beg end nil)
  (switch-to-buffer-other-window inferior-lisp-buffer)
  (goto-char (point-max)))

(defun lisp-send-buffer (display-flag)
  "Send the current buffer to the Lisp process, if prefix arg non-nil redisplay too.

Bound to \\<lisp-mode-map>\\[lisp-send-buffer]"
  (interactive "P")
  (message (concat "Sending contents of " (buffer-name) "..."))
  (save-excursion
    (mark-whole-buffer)
    (lisp-send-region (point) (mark) display-flag))
  (message (concat "Sent " (buffer-name) "!")))

(defun lisp-send-buffer-and-go (&optional ignored)
  "Send the current buffer to the Lisp process and go to the Lisp process.

Bound to \\<lisp-mode-map>\\[lisp-send-buffer-and-go]"
  (interactive)
  (lisp-send-buffer nil)
  (switch-to-buffer-other-window inferior-lisp-buffer)
  (goto-char (point-max)))

(defun lisp-send-defun (display-flag)
  "Send the current defun to the Lisp process made by M-x run-lisp.
With argument, force redisplay and scrolling of the Lisp buffer.
Variable `inferior-lisp-load-command' controls formatting of
the `load' form that is set to the Lisp process."
  (interactive "P")
  (save-excursion
   (mark-defun)
   (lisp-send-region (point) (mark) display-flag)))

(defun lisp-send-defun-and-go ()
  "Send the current defun to the inferior Lisp, and switch to Lisp buffer."
  (interactive)
  (lisp-send-defun nil)
  (switch-to-buffer-other-window inferior-lisp-buffer)
  (goto-char (point-max)))

(define-key lisp-mode-map "\C-c\C-d" 'lisp-send-defun-and-go)
(define-key lisp-mode-map "\C-cd" 'lisp-send-defun)
(define-key lisp-mode-map "\C-c\C-r" 'lisp-send-region-and-go)
(define-key lisp-mode-map "\C-cr" 'lisp-send-region)
(define-key lisp-mode-map "\C-c\C-b" 'lisp-send-buffer-and-go)
(define-key lisp-mode-map "\C-cb" 'lisp-send-buffer)

;;; Inferior Lisp Eval region. Author: Russell A. Ritchie, Scottish HCI Centre.

;;; Allow evaluation of region AS IF IT WAS TYPED to an inferior lisp. 
;;; To solve buffering problems, lisp-send-defun and friends create a temp file
;;; and tell the inferior lisp to load this. For Franz LISP (possibly others)
;;; this does not have the same effect for arbitrary s-expressions as typing
;;; them to the top-level, since only the value of the load is returned (and
;;; hence printed).

(defun lisp-eval-send (beg end show)
  "Make a string from BEG to END and do a show-output-from-shell after sending 
it to the inferior lisp if SHOW is non-nil."
  (lisp-eval-call (buffer-substring beg end) show))

(defun lisp-eval-call (string show)
  (if (or (string= string "") (white-spacep string))
      (error "Region is empty or white space only.")
    (if (> (length string) 255)
	(error "Lisp-eval cannot send more than 255 characters without blocking irretrievably.")
      (save-excursion
	(set-buffer inferior-lisp-buffer)
	(set-mark (point-max))
	(process-send-string (get-buffer-process (current-buffer)) string)
	(shell-send-input)
	(if show (show-output-from-shell))))))

(defun lisp-eval-send-region (arg beg end)
  "Evaluate current region in the inferior lisp buffer as if it were typed.
If prefix arg is greater than 1 then show output from shell afterwards.

Bound to \\[lisp-eval-send-region] by default.
Trying to send regions larger than 255 characters will block irretrievably."
  (interactive "p\nr")
  (lisp-eval-send beg end (> arg 1)))

(defun lisp-eval-send-region-and-go (arg beg end)
  "Evaluate current region in the inferior lisp buffer as if it were typed.
If prefix arg is greater than 1 then show output from shell afterwards.
After evaluating region, go to inferior lisp buffer.

Bound to \\<lisp-mode-map>\\[lisp-eval-send-region-and-go] by default.
Trying to send regions larger than 255 characters will block irretrievably."
  (interactive "p\nr")
  (lisp-eval-send beg end (> arg 1))
  (pop-to-buffer inferior-lisp-buffer))

(define-key lisp-mode-map "\C-c\C-e" 	'lisp-eval-send-region-and-go)
(define-key lisp-mode-map "\C-ce" 	'lisp-eval-send-region)

