;;; scheme-inspector.el -- View and navigate among Scheme values under Emacs
;;; Michael D. Ernst <mernst@research.microsoft.com>
;;; Process code and Emacs 18 mouse handling enhanced by Roger Crew.

;; This mode lets you pretty-print the values of Scheme expressions and
;; navigate among them using keystrokes or mouse clicks.  An arbitrary
;; number of Scheme values may be displayed at a time.  It is somewhat
;; reminiscent of the Inspector that came with the Lisp Machine and other
;; Lisp systems and is very useful for examining structure values.

;; To invoke, type M-x scheme-inspector RET.
;; For more details, see the documentation for scheme-inspector-mode.

;; Install this file in your load-path and add the following to your .emacs:
;;   (autoload 'scheme-inspector "scheme-inspector" "Scheme inspector" t)
;; Perhaps also do this:
;;   (autoload 'si-jump "scheme-inspector" "Scheme inspector" t)
;; and, in your xscheme-start-hook:
;;   (define-key scheme-interaction-mode-map [mouse-1] 'si-jump)

;; Currently I add an expression to the history when it is obscured from
;; the buffer rather than when it is displayed.  Is this the right thing?
;; Note also that the entries killed by si-last are gone forever; I don't
;; see how else to do this, though, so it seems like the right thing.
;; It would be nicer if there was a full history list which could be
;; traversed in both directions.

;; This package works under Emacs 18 and 19.  Currently it supports only
;; MIT Scheme; extensions to other Schemes would be welcomed.

;; LCD Archive Entry:
;; scheme-inspector|Michael Ernst|mernst@research.microsoft.com|
;; View and navigate among Scheme values under Emacs|
;; 24-Mar-1994|1.2|~/interfaces/scheme-inspector.el.Z|


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Scheme inspector mode
;;;

(defvar si-history '())

(defvar scheme-inspector-mode-map (make-sparse-keymap))

(defun scheme-inspector ()
  "View Scheme values, especially structures.
See documentation for `scheme-inspector-mode'."
  (interactive)
  (pop-to-buffer "*Scheme-inspector*")
  (scheme-inspector-mode))

(defun scheme-inspector-mode ()
  "Major mode for viewing and traversing Scheme values, especially structures.

This mode lets you pretty-print the values of Scheme expressions (usually
structures) and navigate among them using keystrokes or mouse clicks.  An
arbitrary number of Scheme values may be displayed at a time.

Type e or v to display the value of an expression or variable.
Type #, h, or n to display the value associated with a hash number.
Type SPC or mouse-1 to display the value of the structure under point.

The above arguments replace the current entry by default.  With a prefix
argument or capital letter, they insert a new entry at buffer end instead.
\(mouse-2 is the \"capital\" of mouse-1.\)
Type l or p to display the most recently obscured entry.
Type d or mouse-3 to delete an entry from the buffer.

Ordinary movement commands work.
Use TAB to move to the next entry and Shift-TAG to move to the previous entry.

Type q to bury the buffer.
Type ? or C-h m to see this help.

Turning on Scheme inspector mode calls the value of the variable
`scheme-inspector-mode-hook', if that value is non-nil."
  (interactive)
  (if (not (eq major-mode 'scheme-inspector-mode))
      (progn
	(message "Welcome to Scheme inspector mode.  Type ? or C-h m for help.")
	(kill-all-local-variables)
	(make-variable-buffer-local 'si-history)
	(setq si-history nil)
	(setq buffer-read-only t)
	(use-local-map scheme-inspector-mode-map)
	(setq mode-name "Scheme inspector")
	(setq major-mode 'scheme-inspector-mode)
	;; (setq local-abbrev-table scheme-inspector-mode-abbrev-table)
	;; (set-syntax-table scheme-inspector-mode-syntax-table)
	(run-hooks 'scheme-inspector-mode-hook))))

(define-key scheme-inspector-mode-map "#" 'si-unhash)
(define-key scheme-inspector-mode-map "h" 'si-unhash)
(define-key scheme-inspector-mode-map "n" 'si-unhash)
(define-key scheme-inspector-mode-map "e" 'si-expression)
(define-key scheme-inspector-mode-map "v" 'si-expression)

(define-key scheme-inspector-mode-map "H" 'si-add-unhash)
(define-key scheme-inspector-mode-map "N" 'si-add-unhash)
(define-key scheme-inspector-mode-map "E" 'si-add-expression)
(define-key scheme-inspector-mode-map "V" 'si-add-expression)

(define-key scheme-inspector-mode-map " " 'si-jump)

(define-key scheme-inspector-mode-map "\C-i" 'si-next-entry)
(if (string-match "^19" emacs-version)
    (define-key scheme-inspector-mode-map [S-tab] 'si-previous-entry))
(define-key scheme-inspector-mode-map "d" 'si-delete)

(define-key scheme-inspector-mode-map "l" 'si-last)
(define-key scheme-inspector-mode-map "p" 'si-last)

(define-key scheme-inspector-mode-map "q" 'bury-buffer)
(define-key scheme-inspector-mode-map "?" 'describe-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Process munging
;;;

(defvar si-process-output)		; quiet the byte-compiler

;; (require 'xscheme-proc)
(if (not (fboundp 'si-orig-xscheme-process-filter-output))
    (if (not (fboundp 'xscheme-process-filter-output))
	(error "You must load xscheme.el before scheme-inspector.el")
      (fset 'si-orig-xscheme-process-filter-output
	    (symbol-function 'xscheme-process-filter-output))))

(defun si-xscheme-process-filter-output (&rest args)
  (setq si-process-output (append si-process-output args)))

;; Returns a string, the pp description.
(defun si-value (expression)
  (let* ((si-process-output '())	; list of strings
	 result				; single string
	 (si-done nil)
	 )
    (fset 'xscheme-process-filter-output 'si-xscheme-process-filter-output)

    ;; When Scheme writes ESC E exp ESC, Emacs evaluates expression exp.
    ;; A similar thing could probably be done by having
    ;; si-xscheme-process-filter-output set si-done when it sees
    ;; ";No value" as an argument, and omitting that string from the output.
    (setq xscheme-silent-value t)
    (xscheme-send-string (format "(begin (pp %s)(write-string \"\eE(setq si-done t)\e\"))"
				 expression))
    (unwind-protect
	(while (not si-done)
	  (accept-process-output (get-process xscheme-process-name)))
      (fset 'xscheme-process-filter-output 'si-orig-xscheme-process-filter-output))
    (setq result (apply (function concat) si-process-output))
    (if (string-match "\C-G" result)
	(error "Scheme error in %s: %s"
	       expression (substring result 0 (match-beginning 0)))
      result)))
;; (progn (setq foo (si-value "#@15")) (describe-variable 'foo))
;; (progn (setq foo (si-value "#@62")) (describe-variable 'foo))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internals for displaying entries
;;;

;; The space between entries belongs to the previous entry.
;; Beware:  the end of one entry is the beginning of the next!  `si-end'
;; belongs to the next entry, so always call it after `si-beginning'.

;; Go to the beginning of the current entry.
(defun si-beginning ()
  (forward-line 2)
  (if (search-backward ":\n\n" nil t)
      (beginning-of-line 1)
    (goto-char (point-min)))
  (point))

;; Go to the end of the current entry.
(defun si-end ()
  (forward-line -1)
  (if (not (search-forward "\n\n\n" nil t))
      (goto-char (point-max)))
  (point))

;; Insert a formatted value at a particular buffer location.
(defun si-insert (expression value begin end)
  (let ((buffer-read-only nil))
    (delete-region begin end)
    (goto-char begin)
    ;; VALUE begins with a newline.
    (insert expression ":\n" value "\n\n\n")
    (si-tighten-region begin (point))
    (si-make-region-visible begin (- (point) 3))
    (goto-char begin)))

;; Get rid of extraneous vertical whitespace.  Leave point at end of region.
(defun si-tighten-region (begin end)
  (save-restriction
    (narrow-to-region begin end)
    (goto-char begin)
    (while (re-search-forward "(\\(define-global-macro\\|lambda\\|if\\)\n"
			      nil t)
      (delete-indentation))
    (goto-char (point-max))))

(defun si-current-exp-to-history ()
  (si-beginning)
  (if (looking-at "\\(.*\\):$")
      (setq si-history (cons (si-match-string 1) si-history))))

(defun si-replace (expression value &optional add)
  "Replace the current entry with EXPRESSION and VALUE.
With optional argument ADD, insert at the end of the buffer instead."
  (or (si-goto expression value)
      (if add
	  (si-insert expression value (point-max) (point-max))
	(progn
	  (si-current-exp-to-history)
	  (si-insert expression value (si-beginning) (si-end))))))

(defun si-location (expression)
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward (concat "\\(\\`\\|\n\n\n\\)"
				   (regexp-quote expression)
				   ":$")
			   nil t)
	(match-end 1))))

;; Return t if successful.
(defun si-goto (expression value)
  (let ((loc (si-location expression)))
    (if loc
	(progn
	  (goto-char loc)
	  ;; The value might have changed since it was last displayed.
	  ;; (si-make-region-visible loc (- (si-end) 3))
	  ;; (goto-char loc)
	  (si-insert expression value (si-beginning) (si-end))
	  t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous
;;;

(defun si-next-entry (arg)
  "Go to the beginning of the next entry.
With a prefix arg, go forward that many entries."
  (interactive "p")
  (si-beginning)
  (while (< arg 0)
    (if (bobp)
	(goto-char (point-max))
      (forward-line -1))
    (si-beginning)
    (setq arg (1+ arg)))
  (while (> arg 0)
    (si-end)
    (if (eobp)
	(goto-char (point-min)))
    (setq arg (1- arg)))
  (let ((begin (point)))
    ;; Don't check for end of buffer because there are three newlines at the
    ;; end of the buffer.
    (re-search-forward "\n\n\n")
    (si-make-region-visible begin (match-beginning 0))
    (goto-char begin)))

(defun si-previous-entry (arg)
  "Go to the beginning of the previous entry.
With a prefix arg, go backward that many entries."
  (interactive "p")
  (si-next-entry (- arg)))

(defun si-quit ()
  "Bury the current buffer."
  (bury-buffer))

(defun si-last (arg)
  "View the Scheme value previously viewed.
With a prefix arg, go back that many values, discarding intervening values."
  (interactive "p")
  (let ((pared-history (nthcdr (1- arg) si-history)))
    (if (null pared-history)
	(error "No previous Scheme value to edit.")
      (setq si-history pared-history)))
  (let ((last (car si-history)))
    (si-expression last)
    ;; Get rid of two expressions, the restored one and the clobbered one.
    (setq si-history (cdr (cdr si-history)))))

(defun si-delete ()
  "Remove the entry under point."
  (interactive)
  (let ((buffer-read-only nil))
    (si-current-exp-to-history)
    (delete-region (si-beginning) (si-end))
    (if (and (eobp) (not (bobp)))
	(si-next-entry 0))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User interface for viewing values
;;;

(defun si-unhash (hash-number &optional add)
  "Display the Scheme value associated with HASH-NUMBER (a number or string).
With prefix argument, add a new entry instead of replacing the current one."
  (interactive "nHash number: \nP")
  (si-expression (format "#@%s" hash-number) add))

(defun si-expression (expression &optional add)
  "Display EXPRESSION and its pretty-printed Scheme value for inspecting.
With prefix argument, add a new entry instead of replacing the current one."
  (interactive "sExpression: \nP")
  (let ((value (si-value (if (string-match "^'?#@" expression)
			     (let ((hn (substring expression (match-end 0))))
			       (format "(or (object-unhash %s) \";Invalid hash number %s\")"
				       hn hn))
			   expression))))
    (scheme-inspector)
    (si-replace expression value add)))

(defvar si-structure-regexp "#\\[[-:A-Za-z0-9]+ \\([0-9]*\\)")

(defun si-jump (&optional add)
  "View the Scheme structure under point.
With prefix argument, add a new entry instead of replacing the current one."
  (interactive "P")
  (let* ((here (point))
	 (hash (or (save-excursion
		     ;; if we are on a number, use it
		     (skip-chars-backward "0-9")
		     (and (looking-at "[0-9]+")
			  (si-match-string 0)))
		   (save-excursion
		     (if (looking-at "#") (forward-char 1))
		     (if (looking-at "\\[") (forward-char 1))
		     (if (and (re-search-backward "#\\[\\|\\]" nil t)
			      (string-equal "#[" (si-match-string 0))
			      (looking-at si-structure-regexp)
			      (progn (forward-sexp 1) (>= (point) here)))
			 (si-match-string 1)))
		   ;; If we're after a #[], the above finds it instead of
		   ;; what we're actually in.
		   (save-excursion
		     (if (and (re-search-forward "#\\[\\|\\]" nil t)
			      (string-equal "]" (si-match-string 0))
			      (progn (backward-sexp 1) (backward-char 1)
				     (<= (point) here))
			      (looking-at si-structure-regexp))
			 (si-match-string 1))))))

    ;; If we are in scheme interaction mode when the eval occurs, lest
    ;; extraneous CR's will be inserted.
    (scheme-inspector)

    (if hash
	(si-unhash hash add)
      (error "No structure at point."))))


;;;
;;; Versions with default add behavior
;;;

(defun si-add-unhash (hash-number &optional replace)
  "Display the Scheme value associated with HASH-NUMBER (a number or string).
With prefix argument, replace the current entry instead of adding a new one."
  (interactive "nHash number: \nP")
  (si-unhash hash-number (not replace)))

(defun si-add-expression (expression &optional replace)
  "Display EXPRESSION and its pretty-printed Scheme value for inspecting.
With prefix argument, replace the current entry instead of adding a new one."
  (interactive "sExpression: \nP")
  (si-expression expression (not replace)))

(defun si-add-jump (&optional replace)
  "View the Scheme structure under point.
With prefix argument, replace the current entry instead of adding a new one."
  (interactive "P")
  (si-jump (not replace)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mouse support
;;;

(if (eq window-system 'x)
    (if (string-match "^19" emacs-version)
	;; Emacs 19
	(progn
	  ;; Yuck.  Is there a better way to deal with mouse-1?  I need to
	  ;; shadow both of these bindings (as of Emacs 19.15).
	  (define-key scheme-inspector-mode-map [down-mouse-1] 'mouse-set-point)
	  (define-key scheme-inspector-mode-map [mouse-1] 'si-jump)
	  (define-key scheme-inspector-mode-map [mouse-2] 'si-mouse-add-jump)
	  (define-key scheme-inspector-mode-map [mouse-3] 'si-mouse-delete)

	  (define-key scheme-interaction-mode-map [mouse-1] 'si-jump)

	  (defun si-mouse-jump (click)
	    "View the Scheme structure clicked upon."
	    (interactive "e")
	    (goto-char (posn-point (event-start click)))
	    (si-jump nil))

	  (defun si-mouse-add-jump (click)
	    "View the Scheme structure clicked upon, adding a new entry."
	    (interactive "e")
	    (goto-char (posn-point (event-start click)))
	    (si-jump t))

	  (defun si-mouse-delete (click)
	    "Remove the entry clicked upon."
	    (interactive "e")
	    (goto-char (posn-point (event-start click)))
	    (si-delete))
	  )

      ;; Emacs 18
      (progn
	(defvar mouse-map)		; quiet the byte-compiler
	(defvar x-button-left)		; quiet the byte-compiler
	(defvar x-button-middle)	; quiet the byte-compiler
	(defvar x-button-right)		; quiet the byte-compiler

	(defvar si-orig-x-mouse-left   (lookup-key mouse-map x-button-left))
	(defvar si-orig-x-mouse-middle (lookup-key mouse-map x-button-middle))
	(defvar si-orig-x-mouse-right  (lookup-key mouse-map x-button-right))

	(defun si-scheme-window-p (arg)
	  (save-window-excursion
	    (save-excursion
	      (x-mouse-set-point arg)
	      (memq major-mode '(scheme-inspector-mode scheme-interaction-mode)))))

	(define-key mouse-map x-button-left
	  (function (lambda (arg)
		      (if (not (si-scheme-window-p arg))
			  (funcall si-orig-x-mouse-left arg)
			(x-mouse-set-point arg)
			(si-jump)))))
	(define-key mouse-map x-button-middle
	  (function (lambda (arg)
		      (if (not (si-scheme-window-p arg))
			  (funcall si-orig-x-mouse-middle arg)
			(x-mouse-set-point arg)
			(si-jump t)))))
	(define-key mouse-map x-button-right
	  (function (lambda (arg)
		      (if (not (si-scheme-window-p arg))
			  (funcall si-orig-x-mouse-right arg)
			(x-mouse-set-point arg)
			(si-delete))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utilities
;;;

;;; Lifted from util-mde.el
(defun si-match-string (n &optional source)
  "Return the string matched by parentheses number N.  If there is a
SOURCE string, return the substring of that string; else, return
substring of the current buffer."
  (cond
   ((stringp source)
    (substring source (match-beginning n) (match-end n)))
   (t (buffer-substring (match-beginning n) (match-end n)))))

;;; This assumes that the entire region can fit onscreen, not necessarily a
;;; true assumption; no serious ill comes if it can't, however.
(defun si-make-region-visible (begin end)
  (cond ((save-excursion
	   (move-to-window-line 0)
	   (> (point) begin))
	 (goto-char begin)
	 (recenter 0))
	((save-excursion
	   (move-to-window-line -1)
	   (end-of-line)
	   (< (point) end))
	 (goto-char end)
	 (recenter -1))))

