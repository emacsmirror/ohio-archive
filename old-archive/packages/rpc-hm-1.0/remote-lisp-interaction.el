;Author: Eyvind Ness (eyvind) 
;Date:   Thursday, May 7 1992 08:42 GMT
;File:   /usr/local/gnu/emacs/elisp/site-extensions/remote-lisp-interaction.el

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
(require 'gobble-whitespace)
(condition-case c
    (progn
      (require 'completer)
      (require 'ilisp)
      (require 'ilisp-rpc-hm-mods))
  (error
   (message "Non-fatal error: Ignoring problems loading ILISP (%s)" c)))
(require 'remote-lisp-documentation)
(provide 'remote-lisp-interaction)


(defvar rlm-with-transparent-signals nil
  "If non-nil signals from rpc-hm will not be trapped by the functions
in this package, but relayed, if that makes sense.
Using a lexical binding is the recommended way to temporarily rebind
this var to a specific value.")


(defvar ilisp-use-rpc-hm-instead nil
  "*Set to non-nil when using rpc-hm as a transport for ILISP.")
(make-variable-buffer-local 'ilisp-use-rpc-hm-instead)


(defun remote-lisp-mode-commands (map)
  (define-key map "\C-hf" 'rld-describe-function)
  (define-key map "\C-hv" 'rld-describe-variable)
  (define-key map "\e\C-m" 'rpc-hm-next-host)
  (define-key map "\e\C-x" 'rlm-lisp-send-defun)
  ;; inherit a few nice things from ilisp:
  (define-key map "\e\C-i" 'complete-lisp)
  )
    

(defun rlm-lisp-send-defun (&optional insertp)
  ;; Evaluates expression surrounding point using
  ;; rpc-hm-internal.
  (interactive)
  (let (result)
    (save-excursion
      ;; I'm not quite happy with mark-defun:
      ;; (mark-defun)
      (push-mark (point) 'no-message-please)
      (end-of-defun)
      (push-mark (point) 'no-message-please)
      (forward-sexp -1)
      (or noninteractive (message "RPC... "))
      (setq result
	    (rpc-hm-internal
	     (rpc-hm-get-current-host)
	     (buffer-substring (region-beginning) (region-end))
	     nil ':any)))
    (prog1
	result
      (cond (insertp
	     (insert (format "\n%s\n" result))
	     (or noninteractive (message "RPC... done.")))
	    (t
	     (or noninteractive (message "%s" result)))))))


(defun rlm-eval-last-sexp (arg)
  "Evaluate sexp before point; print value in minibuffer.
With argument, print output into current buffer."
  (interactive "P")
  (or noninteractive (message "RPC... "))
  (condition-case c
      (let ((result
	     (rpc-hm-internal
	      (rpc-hm-get-current-host)
	      (buffer-substring
	       (let ((stab (syntax-table)))
		 (unwind-protect
		      (save-excursion
			(set-syntax-table rpc-hm-lisp-mode-syntax-table)
			(forward-sexp -1)
			(point))
		   (set-syntax-table stab)))
	       (point))
	      nil
	      ':any)))
	(prog1 
	    result
	  (cond (arg
		 (let ((i 2) (all-ret-vals (rpc-hm-reparse-ans)))
		   (or (bolp) (insert "\n"))
		   (while (< i 3)
		     ;; the output streams are now concatenated, so there is
		     ;; really no need to loop any longer.
		     (insert (rpc-hm-read-from-string (elt all-ret-vals i)))
		     (setq i (1+ i)))
		   (or (bolp) (insert "\n"))
		   (insert (elt all-ret-vals 1) "\n"))
		 (or noninteractive (message "RPC... done.")))
		(t
		 (or noninteractive (message "%s" result))))))
    (rpc-hm-network-condition
     (if rlm-with-transparent-signals
	 (signal (car c) (cdr c))
	 (let ((ctype (car c))
	       (simple-errmess
		(and (= (length (cdr c)) 1)
		     (stringp (car (cdr c)))
		     (car (cdr c))))
	       (cdata (format "%s" (cdr c))))
	   (if (and (not noninteractive)
		    (or (> (length cdata) (screen-width))
			(string-match "\n" cdata)))
	       (progn
		 (with-output-to-temp-buffer "*RPC HM Errors*"
		   (save-excursion
		     (set-buffer standard-output)
		     (insert (get ctype 'error-message) ":\n\n")
		     (insert (or simple-errmess cdata)))
		   (rld-print-help-return-message)))
	       (progn 
		 (message "%s" (or simple-errmess cdata)))))))))


(defun rlm-eval-print-last-sexp ()
  "Same as rlm-eval-last-sexp, but also inserts the returned values
into the current buffer."
  (interactive)
  (rlm-eval-last-sexp t))


(defun rlm-eval-current-buffer (arg)
  "Sends the top-level forms in the current buffer to remote host using
rpc-hm-internal. With prefix ARG, insert vals into the current buffer as
you go."
  (interactive "P")
  (let ((rlm-with-transparent-signals t))
    ;; We don't want to go on evaluating forms if a condition is raised,
    ;; so make sure signals from rpc-hm are transparent during the body
    ;; of this LET.
    (save-excursion
      (goto-char (point-min))
      (gw-skip-blank-lines-and-comments)
      (while (not (eobp))
;;;	(message "%s" (rlm-lisp-send-defun)) (end-of-defun)
	(forward-sexp 1) (message "%s" (rlm-eval-last-sexp arg))
	(gw-skip-blank-lines-and-comments)))))


;;;
;;; Some utils for interactive mode (very much like Common-Lisp mode).

(defvar lispm-mode-map () "")

(if lispm-mode-map
    ()
  (setq lispm-mode-map (make-sparse-keymap))
  (lisp-mode-commands lispm-mode-map)
  (remote-lisp-mode-commands lispm-mode-map)
  (define-key lispm-mode-map "\C-j" 'rlm-eval-print-last-sexp))


(defvar lispm-mode-hook nil
  "If non-nil a function to be called when entering lispm-mode.")


(defun lispm-mode (&optional host)
  "Major mode for interacting with a remote Lisp process.
Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{lispm-mode-map}
Note that `run-lisp' may be used either to start an inferior Lisp job
or to switch back to an existing one."

  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'global-mode-string)
  (make-local-variable 'ilisp-complete-command)
  (setq ilisp-complete-command 
	"(ILISP:ilisp-matching-symbols \"%s\" \"%s\" %s %s %s)")
  (make-local-variable 'ilisp-symbol-delimiters)
  (setq ilisp-symbol-delimiters "^ \t\n\('\"#.\)<>")
  (setq ilisp-use-rpc-hm-instead t)
  (use-local-map lispm-mode-map)
  (lisp-mode-variables t)
  (setq major-mode 'lispm-mode)
  (setq mode-name "Lispm Mode")
  (set-syntax-table rpc-hm-lisp-mode-syntax-table)
  (rpc-hm-set-current-host host)
  (rpc-hm-update-mode-line-if-convenient)
  (run-hooks 'lispm-mode-hook))


(defun rlm-run-remote-lisp (host)
  "Switch to an interactive buffer virtually connected to a remote lisp
interpreter on HOST, a remote lisp machine."
  (interactive
   (list
    (intern
     (let ((completion-ignore-case t))
       (completing-read
	"Remote host: "
	(mapcar
	 (function (lambda (el) (list (symbol-name (car el)))))
	 rpc-hm-host-db)
	(function identity) 'must-exist
	(prin1-to-string (rpc-hm-get-current-host)))))))
  (switch-to-buffer
   (generate-new-buffer "*remote-lisp-interaction*"))
  (lispm-mode host)
  (or rpc-hm-startup-message-displayed-p
      (rpc-hm-display-startup-message)))
