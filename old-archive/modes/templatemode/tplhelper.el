;;; tplhelper.el -- Helper functions for template-mode.
;;; Copyright (C) 1987 Mark A. Ardis.

(provide 'tplhelper)

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-blank-line ()
  "Returns t if current line contains only whitespace.
    Otherwise, returns nil."
					; Local Variables
  (let (result)
					; Body
    (save-excursion
      (beginning-of-line)
      (if (eolp)
	  (setq result t)
	; else
	(progn
	  (re-search-forward tpl-pattern-whitespace (point-max) t)
	  (if (eolp)
	      (setq result t)
	    (setq result nil)
	    ) ; if
	  ) ; progn
	) ; if
      ) ; save
    ; return
    result
    ) ; let
  ) ; defun tpl-blank-line

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-build-template-list ()
  "Build template-list, using current major mode."
					; Local Variables
  (let (mode-entry template-list)
					; Body
    (setq tpl-local-template-list
	  (list (tpl-mode-templates
		 (tpl-mode-match 'generic tpl-global-template-list))))
    ; Use loaded templates if available
    (setq template-list
	  (tpl-mode-templates
	   (tpl-mode-match major-mode tpl-global-template-list)))
    (if template-list
	(setq tpl-local-template-list
	      (cons template-list tpl-local-template-list))
      ; else
      (progn
	(setq mode-entry (tpl-mode-match major-mode tpl-auto-template-alist))
	(if mode-entry
	    (progn
	      (load-tpl-library (tpl-mode-file mode-entry) major-mode)
	      ) ; progn
	  ; else
	  (message "No templates found for this mode.")
	  ) ; if mode-entry
	) ; progn
      ) ; if template-list
    (if tpl-rebuild-all-templates-template
	(tpl-make-all-templates-template)
      ) ; if
    ) ; let
  ) ; defun tpl-build-template-list

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-delete-placeholders-in-region (start stop)
  "Delete all placeholders in region between START and STOP."
					; Local Variables
  (let (stop-marker)
					; Body
    (setq stop-marker (make-marker))
    (set-marker stop-marker stop)
    (goto-char start)
    (while (re-search-forward tpl-pattern-placeholder
			      (marker-position stop-marker) t)
      (re-search-backward tpl-pattern-placeholder)
      (delete-placeholder)
      ) ; while
    (set-marker stop-marker nil)
    ) ; let
  ) ; defun tpl-delete-placeholders-in-region

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-expand-lexical-type (name stop)
  "Expand the lexical placeholder NAME at point.  Replaces all instances
    of identical placeholders before STOP with the same value.
    Checks for match with lexical description."
					; Local Variables
  (let (save-hook)
					; Body
    (if (boundp 'sym-check-validity-hook)
	(setq save-hook sym-check-validity-hook)
      (setq save-hook nil)
      ) ; if
    (setq sym-check-validity-hook 'tpl-lexical-check)
    (setq tpl-lexical-pattern (tpl-find-value-of-template name))
    (if tpl-lexical-pattern
	(tpl-expand-text-type stop)
      (error "Cannot find template.")
      ) ; if
    (setq sym-check-validity-hook save-hook)
    ) ; let
  ) ; defun tpl-expand-lexical-type

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-expand-placeholder (stop)
  "Expand the placeholder at point.  Replace identical occurrences
    of text placeholders before STOP with the same value."
					; Local Variables
  (let (placeholder template-name start placeholder-name)
					; Body
    (setq start (point))
					; Process placeholder
    (setq placeholder (tpl-scan-placeholder))
    (setq template-name (tpl-token-name placeholder))
    (setq placeholder-name (tpl-token-value placeholder))
    (cond
     ((equal template-name "text")
      (tpl-expand-text-type stop)
      ) ; (equal template-name "text")
     ((equal template-name "textenter")
      (tpl-expand-textenter-type stop)
      ) ; (equal template-name "textenter")
     ((equal template-name "textlong")
      (tpl-expand-textlong-type placeholder-name)
      ) ; (equal template-name "textlong")
     ((equal template-name tpl-destination-symbol)
      (progn
	(re-search-forward tpl-pattern-placeholder)
	(ding)
	(message "Cannot expand destination placeholder.")
	) ; progn
      ) ; (equal template-name "textlong")
     (t
      (if (equal tpl-lexical-type
		 (tpl-find-type-of-template template-name))
	  (tpl-expand-lexical-type template-name stop)
	; else
	(progn
	  (re-search-forward tpl-pattern-placeholder)
	  (delete-region start (point))
	  (tpl-insert-template template-name)
	  ) ; progn
	) ; if
      ) ; t
     ) ; cond
    ) ; let
  ) ; defun tpl-expand-placeholder

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-expand-text-type (stop)
  "Expand the text placeholder at point.  Replace identical placeholders
    before STOP with the same value.  Return that value."
					; Local Variables
  (let (start stop-marker placeholder-string sym-input)
					; Body
    (setq start (point))
    (if stop
	(progn
	  (setq stop-marker (make-marker))
	  (set-marker stop-marker stop)
	  ) ; progn
      ) ; if stop
    (re-search-forward tpl-pattern-placeholder)
    (setq placeholder-string (buffer-substring start (point)))
    (goto-char start)
    (setq sym-input (sym-read-string
		     (concat "Replace " placeholder-string " with what? ")
		     placeholder-string))
    (if (= (length sym-input) 0)
	(re-search-forward placeholder-string)
      ; else
      (if stop
	  (progn
	    (setq start (point))
					; Replace all identical placeholders
	    (while (re-search-forward placeholder-string
				      (marker-position stop-marker) t)
	      (re-search-backward placeholder-string)
	      (insert-before-markers sym-input)
	      (delete-char (length placeholder-string))
	      ) ; while (re-search-forward...)
	    (goto-char start)
	    ) ; progn
	) ; if stop
      ) ; if (= (length sym-input) 0)
    ; return
    sym-input
    ) ; let
  ) ; defun tpl-expand-text-type

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-expand-textenter-type (stop)
  "Expand the text placeholder at point.  Replace identical placeholders
    before STOP with the same value.  Enter that value in the symbol
    table."
					; Local Variables
  (let (value)
					; Body
    (setq value (tpl-expand-text-type stop))
    (sym-enter-id value)
    ) ; let
  ) ; defun tpl-expand-textenter-type


;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-expand-textlong-type (name)
  "Expand the textlong placeholder at point called NAME."
					; Local Variables
  (let (start display-string save-buffer new-string start-column)
					; Body
					; Highlight placeholder
    (setq start (point))
    (re-search-forward tpl-pattern-placeholder)
    (delete-region start (point))
    (setq display-string (concat tpl-display-begin name tpl-display-end))
    (insert-before-markers display-string)
    (backward-char (length display-string))
					; Save current location
    (setq start (point))
					; Prepare buffer
    (save-window-excursion
      (setq save-buffer (buffer-name))
      (switch-to-buffer-other-window tpl-textlong-buffer)
      (erase-buffer)
      (shrink-window 5)
					; Wait for return from recursive edit
      (message (substitute-command-keys
		"Type replacement and exit with \\[exit-recursive-edit]"))
      (recursive-edit)
					; Get new value and insert
      (setq new-string (buffer-substring (point-min) (point-max)))
      (set-buffer save-buffer)
      (delete-windows-on tpl-textlong-buffer)
      ) ; save-window-excursion
    (bury-buffer tpl-textlong-buffer)
					; Return to proper location
    (goto-char start)
    (delete-char (length display-string))
    (setq start-column (current-column))
    (setq start (point))
    (insert-before-markers new-string)
    (indent-rigidly start (point) start-column)
    ) ; let
  ) ; defun tpl-expand-textlong-type

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-find-end-of-group ()
  "Find the end of a group defined for query-replace-groups."
					; Local Variables
  (let ()
					; Body
    (if tpl-form-placeholder-name-from-context
	(tpl-make-placeholder-name)
      ) ; if tpl-form-placeholder-name-from-context
    (if tpl-include-prefix-in-groups
	(beginning-of-line nil)
      ) ; if tpl-include-prefix-in-groups
    (set-mark (point))
    (end-of-line nil)
    (re-search-forward tpl-end-group nil "not-t")
    (if tpl-verify-end-of-group
	(progn
	  (message
	   (concat "Position point AFTER end of group and exit ("
		   (substitute-command-keys "\\[exit-recursive-edit]")
		   ")."))
	  (unwind-protect
	      (recursive-edit)
	    ) ; unwind-protect
	  ) ; progn
      ) ; if tpl-verify-end-of-group
    (end-of-line 0)
    ) ; let
  ) ; defun tpl-find-end-of-group

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-find-expansion-destination (start stop)
  "Delete special destination placeholder between START and STOP
    and set destination marker if a destination needs to be found."
					; Local Variables
  (let (stop-marker)
					; Body
    (goto-char start)
    (setq stop-marker (make-marker))
    (set-marker stop-marker stop)
    (while (re-search-forward tpl-destination-placeholder stop stop)
	(progn
	  (re-search-backward tpl-pattern-placeholder)
	  (delete-placeholder)
	  (if tpl-destination-needed
	      (progn
		(set-marker tpl-destination-marker (point))
		(setq tpl-destination-needed nil)
		) ; progn
	    ) ; if tpl-destination-needed
	  ) ; progn
      ) ; while (re-search-forward tpl-destination-placeholder stop stop)
    (goto-char (marker-position stop-marker))
    (set-marker stop-marker nil)
    ) ; let
  ) ; defun tpl-find-expansion-destination

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-find-next-group ()
  "Find the end of a group defined for query-replace-groups.
    Do not interact with user."
					; Local Variables
  (let ()
					; Body
    (end-of-line nil)
    (re-search-forward tpl-end-group nil "not-t")
    (end-of-line 0)
    ) ; let
  ) ; defun tpl-find-next-group

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-find-template-file (file)
  "Find FILE using the 'tpl-load-path value."
					; Local Variables
  (let (tpl-name compiled-name dir-list looking)
					; Body
    (setq tpl-name (concat file ".tpl"))
    (setq compiled-name (concat file "tpl.elc"))
    (setq name nil)
    (setq looking t)
					; First try compiled versions
    (setq dir-list tpl-load-path)
    (while (and looking dir-list)
      (setq name (concat (car dir-list) "/" compiled-name))
      (setq dir-list (cdr dir-list))
      (if (file-readable-p name)
	  (setq looking nil)
	) ; if
      ) ; while
					; Second, try uncompiled
    (setq dir-list tpl-load-path)
    (while (and looking dir-list)
      (setq name (concat (car dir-list) "/" tpl-name))
      (setq dir-list (cdr dir-list))
      (if (file-readable-p name)
	  (setq looking nil)
	) ; if
      ) ; while
					; Last, try literal name
    (setq dir-list tpl-load-path)
    (while (and looking dir-list)
      (setq name (concat (car dir-list) "/" file))
      (setq dir-list (cdr dir-list))
      (if (file-readable-p name)
	  (setq looking nil)
	) ; if
      ) ; while
    ; return
    name
    ) ; let
  ) ; defun tpl-find-template-file

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-find-template (tpl-name)
  "Find template TPL_NAME and return template or nil (if not found)."
					; Local Variables
  (let (found file-list template-file template-list template template-name)
					; Body
    (setq found nil)
    (setq file-list tpl-local-template-list)
    (while (and file-list (not found))
      (setq template-file (car file-list))
      (setq file-list (cdr file-list))
      (setq template-list (nth 1 template-file))
      (while (and template-list (not found))
	(setq template (car template-list))
	(setq template-list (cdr template-list))
	(setq template-name (tpl-token-name template))
	(if (equal template-name tpl-name)
	    (setq found template)
	  ) ; if (equal template-name tpl-name)
	) ; while (and template-list (not found))
      ) ; while (and file-list (not found))
					; return
    found
    ) ; let
  ) ; defun tpl-find-template

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-find-type-of-template (name)
  "Find template NAME and return its type or nil (if not found)."
					; Local Variables
  (let (template result)
					; Body
    (setq template (tpl-find-template name))
    (if template
	(setq result (tpl-token-type template))
      (setq result nil)
      ) ; if
					; return
    result
    ) ; let
  ) ; defun tpl-find-type-of-template

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-find-value-of-template (name)
  "Find template NAME and return its value or nil (if not found)."
					; Local Variables
  (let (template result)
					; Body
    (setq template (tpl-find-template name))
    (if template
	(setq result (tpl-token-value template))
      (setq result nil)
      ) ; if
					; return
    result
    ) ; let
  ) ; defun tpl-find-value-of-template

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-find-wrappers (tpl-name)
  "Find the beginning and ending part of TPL-NAME that encloses a
    destination placeholder."
					; Local Variables
  (let (msg template midpoint result)
					; Body
    (setq msg nil)
    (setq template (tpl-find-template tpl-name))
    (save-excursion
      (set-buffer tpl-work-buffer)
      (erase-buffer)
      (if template
	  (progn
	    (tpl-unscan template)
	    (goto-char (point-min))
	    (if (re-search-forward tpl-destination-placeholder
				   (point-max) t)
		(progn
		  (delete-region (match-beginning 0) (match-end 0))
		  (setq midpoint (point))
		  ) ; progn
	      ; else
	      (progn
		(setq msg "Template does not contain a destination placeholder.")
		) ; progn
	      ) ; if
	    ) ; progn
	; else
	(progn
	  (setq msg "Cannot find template.")
	  ) ; progn
	) ; if template
      (if (not msg)
	  (setq result (list (buffer-substring 1 midpoint)
			     (buffer-substring midpoint (point-max))
			     (current-column)))
	) ; if
      ) ; save-excursion
    (bury-buffer tpl-work-buffer)
    (if msg
	(error msg)
      ) ; if
					; return
    result
    ) ; let
  ) ; defun tpl-find-wrappers

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-generate (tpl-name)
  "Insert and expand the template TPL-NAME at point."
					; Local Variables
  (let (start stop)
					; Body
    ; Insert and expand template
    (setq start (point))
    (insert-before-markers tpl-begin-placeholder tpl-name tpl-end-placeholder)
    (goto-char start)
    (setq tpl-destination-needed t)
    (message "Looking for template...")
    (tpl-expand-placeholder nil)
    (setq stop (point))
    (if (not tpl-destination-needed)
	(progn
	  (goto-char (marker-position tpl-destination-marker))
	  (set-marker tpl-destination-marker nil)
	  ) ; progn
      ; else
      (progn
	(setq tpl-destination-needed nil)
	(goto-char start)
	(if (re-search-forward tpl-pattern-placeholder stop stop)
	    (re-search-backward tpl-pattern-placeholder)
	  ) ; if
	) ; progn
      ) ; if (not tpl-destination-needed)
    (message "%s" "Done.")
    ) ; let
  ) ; defun tpl-generate

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-get-placeholder-name ()
  "Prompt for a placeholder name.  If none supplied, use temporary
    name and regenerate another unique name.  Return the name."
					; Local Variables
  (let (name)
					; Body
    (if tpl-query-flag
	(progn
	  (setq name (read-string
		      (concat "Template name? ("
			      tpl-next-placeholder-name ") ")))
	  ) ; progn
      ; else
      (setq name "")
      ) ; if tpl-query-flag
    (if (equal name "")
	(progn
	  (setq name tpl-next-placeholder-name)
	  (tpl-increment-next-placeholder-name)
	  ) ; progn
      ) ; if (equal name "")
					; return
    name
    ) ; let
  ) ; tpl-get-placeholder-name

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-increment-next-placeholder-name ()
  "Increment unique name for temporary placeholders."
					; Local Variables
  (let ()
					; Body
    (setq tpl-next-placeholder-number
	  (1+ tpl-next-placeholder-number))
    (setq tpl-next-placeholder-name
	  (concat tpl-temporary-placeholder-name
		  tpl-next-placeholder-number))
    ) ; let
  ) ; defun tpl-increment-next-placeholder-name

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-initialize-modes ()
  "Create initial Alist of major modes and their associated template files.
    Calls 'template-mode-load-hook' if it is defined."
					; Local Variables
  (let ()
					; Body
    (or (assq 'template-mode minor-mode-alist)
	(setq minor-mode-alist
	      (cons '(template-mode " Template") minor-mode-alist)))
    (setq tpl-auto-template-alist
	  (list
	   (tpl-make-mode-entry 'awk-mode "awk")
	   (tpl-make-mode-entry 'bib-mode "bib")
	   (tpl-make-mode-entry 'c-mode "c")
	   (tpl-make-mode-entry 'emacs-lisp-mode "elisp")
	   (tpl-make-mode-entry 'generic "generic")
	   (tpl-make-mode-entry 'LaTeX-mode "latex")
					; Should have another set of templates
					;   for Lisp
	   (tpl-make-mode-entry 'lisp-mode "elisp")
	   (tpl-make-mode-entry 'pascal-mode "pascal")
	   (tpl-make-mode-entry 'scribe-mode "scribe")
	   (tpl-make-mode-entry 'texinfo-mode "texinfo")
					; Should have another set of templates
					;    for TeX
	   (tpl-make-mode-entry 'plain-TeX-mode "latex")
		))
    (setq tpl-local-template-list nil)
    (get-buffer-create tpl-menu-buffer)
    (get-buffer-create tpl-textlong-buffer)
    (get-buffer-create tpl-work-buffer)
    (bury-buffer tpl-menu-buffer)
    (bury-buffer tpl-textlong-buffer)
    (bury-buffer tpl-work-buffer)
    (tpl-initialize-scan)
    (load-tpl-library "generic" 'generic)
    (and (boundp 'template-mode-load-hook)
	 template-mode-load-hook
	 (funcall template-mode-load-hook))
    ) ; let
  ) ; defun tpl-initialize-modes

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-insert-function (template)
  "Insert a template at point using the function type TEMPLATE."
					; Local Variables
  (let (start stop-marker result save-depth)
					; Body
    (setq start (point))
    (setq stop-marker (make-marker))
    (insert (tpl-token-value template))
    (set-marker stop-marker (point))
					; Temporarily expand placeholders
					;    without asking
    (setq save-depth tpl-ask-expansion-depth)
    (setq tpl-ask-expansion-depth 10)
    (expand-placeholders-in-region start (point))
    (setq tpl-ask-expansion-depth save-depth)
					; Evaluate result
    (goto-char start)
    (save-excursion
      (setq result (eval (read (current-buffer))))
      ) ; save-excursion
					; Remove placeholder and insert result
    (delete-region start (marker-position stop-marker))
    (set-marker stop-marker nil)
    (insert result)
    ) ; let
  ) ; defun tpl-insert-function

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-insert-repetition (template)
  "Insert at point instances of the repetition type TEMPLATE."
					; Local Variables
  (let (start template-name column)
					; Body
    (setq start (point))
    (setq column (current-column))
    (setq template-name (tpl-token-name template))
					; Insert first instance
    (tpl-unscan template)
    (re-search-backward tpl-pattern-placeholder)
    (delete-region start (point))
    (tpl-expand-placeholder nil)
					; Insert more instances
    (while (tpl-y-or-n-p (concat "More instances of " template-name "? "))
      (tpl-unscan template column)
      (cond
       ((> tpl-ask-expansion-depth 0)
	(progn
	  (re-search-backward tpl-pattern-placeholder)
	  (tpl-expand-placeholder nil)
	  ) ; progn
	) ; (> tpl-ask-expansion-depth 0)
       ) ; cond
      ) ; while (tpl-y-or-n-p...)
    ) ; let
  ) ; defun tpl-insert-repetition

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-insert-selection (template)
  "Insert a template at point using the selection type TEMPLATE."
					; Local Variables
  (let (save-buffer start stop size choice choice-template choice-list
		    display-string)
					; Body
					; Highlight placeholder
    (setq display-string (concat
			  tpl-display-begin
			  (tpl-token-name template)
			  tpl-display-end))
    (insert-before-markers display-string)
    (backward-char (length display-string))
					; Prepare menu buffer
    (save-window-excursion
      (setq save-buffer (buffer-name))
      (switch-to-buffer-other-window tpl-menu-buffer)
      (erase-buffer)
					; Build the menu
      (tpl-unscan template)
					; Size the window
      (goto-char (point-max))
      (setq stop (point))
      (goto-char (point-min))
      (setq start (point))
      (setq size (1+ (count-lines start stop)))
      (setq size (max size window-min-height))
      (if (< size (window-height))
	  (shrink-window (- (window-height) size))
	) ; if
					; Allow user to view and select
      (setq choice (menu-mode))
      (set-buffer save-buffer)
      (delete-windows-on tpl-menu-buffer)
      ) ; save-window-excursion
    (bury-buffer tpl-menu-buffer)
    (delete-char (length display-string))
					; Insert choice as template or string
    (if choice
	(progn
	  (setq choice-list (tpl-parse-choice choice))
	  (setq choice-template (nth 1 choice-list))
	  (if choice-template
	      (tpl-insert-template choice-template)
	    ; else
	    (insert-before-markers (nth 0 choice-list))
	    ) ; choice-template
	  ) ; progn
      ; else insert placeholder
      (progn
	(setq display-string (concat tpl-begin-placeholder
				     (tpl-token-name template)
				     tpl-end-placeholder))
	(insert-before-markers display-string)
	(backward-char (length display-string))
	(error "Quit.")
	) ; progn
      ) ; if choice
    ) ; let
  ) ; defun tpl-insert-selection

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-insert-string-from-buffer (tpl-name display-string &optional buffer)
  "Insert a template at point using the string type TPL-NAME, temporarily
   represented by DISPLAY-STRING.  Optional third argument BUFFER is the
   buffer to search."
					; Local Variables
  (let (start string)
					; Body
    (if (not buffer)
	(setq buffer
	      (read-buffer "tpl-insert-string: Template buffer? "
			   tpl-new-template-buffer t))
      ) ; if
    (save-window-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (if (re-search-forward (concat tpl-begin-template-definition
				     " " tpl-name " ")
			     (point-max) t)
	  (progn
	    (re-search-forward tpl-begin-template-body)
	    (beginning-of-line 2)
	    (setq start (point))
	    (re-search-forward tpl-end-template-body)
	    (end-of-line 0)
	    (setq string (buffer-substring start (point)))
	    ) ; progn
	; else
	(error "Could not find template in %s" buffer)
	) ; if
      ) ; save-window-excursion
    (delete-char (length display-string))
    (insert-before-markers string)
    ) ; let
  ) ; defun tpl-insert-string-from-buffer

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-insert-template (tpl-name)
  "Insert the template TPL-NAME at point."
					; Local Variables
  (let (display-string template start template-type looking)
					; Body
					; Display selected template
    (setq display-string (concat tpl-display-begin tpl-name tpl-display-end))
    (insert-before-markers display-string)
    (backward-char (length display-string))
    (setq looking t)
    (while looking
					; Find template.
      (setq template (tpl-find-template tpl-name))
      (if template
	  (progn
	    (setq looking nil)
					; Insert template
	    (delete-char (length display-string))
	    (setq start (point))
	    (setq template-type (tpl-token-type template))
	    (cond
	     ((equal template-type tpl-sequence-type)
	      (progn
		(tpl-unscan template)
		(tpl-find-expansion-destination start (point))
		(cond
		 ((< tpl-ask-expansion-depth 0)
		  (tpl-delete-placeholders-in-region start (point))
		  ) ; (< tpl-ask-expansion-depth 0)
		 ((> tpl-ask-expansion-depth 0)
		  (progn
		    (expand-placeholders-in-region start (point))
		    ) ; progn
		  ) ; (> tpl-ask-expansion-depth 0)
		 ) ; cond
		) ; progn
	      ) ; (equal template-type tpl-sequence-type)
	     ((equal template-type tpl-selection-type)
	      (progn
		(tpl-insert-selection template)
		) ; progn
	      ) ; (equal template-type tpl-selection-type)
	     ((equal template-type tpl-repetition-type)
	      (progn
		(tpl-insert-repetition template)
		) ; progn
	      ) ; (equal template-type tpl-repetition-type)
	     ((equal template-type tpl-function-type)
	      (progn
		(tpl-insert-function template)
		) ; progn
	      ) ; (equal template-type tpl-function-type)
	     ((equal template-type tpl-string-type)
	      (progn
		(tpl-unscan template)
		) ; progn
	      ) ; (equal template-type tpl-string-type)
	     ) ; cond
	    ) ; progn
					; Else report failure
	(progn
	  (if (y-or-n-p "Cannot find template---look in a buffer? ")
	      (progn
		(setq looking nil)
		(tpl-insert-string-from-buffer tpl-name display-string)
		) ; progn
	    ; else
	    (if (y-or-n-p "Cannot find template---load a template file? ")
		(progn
		  (save-some-buffers)
		  (load-tpl-file)
		  ) ; progn
	      ; else
	      (progn
		(setq looking nil)
		(error "Gave up looking for template.")
		) ; progn
	      ) ; if (y-or-n-p ...load...)
	    ) ; if (y-or-n-p ...look...)
	  ) ; progn
	) ; if template
      ) ; while looking
    ) ; let
  ) ; defun tpl-insert-template

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-lexical-check (input)
  "Check INPUT for validity against lexical definition."
					; Local Variables
  (let (result)
					; Body
    (if (and (string-match tpl-lexical-pattern input)
	     (equal (match-beginning 0) 0)
	     (equal (match-end 0) (length input)))
	(setq result t)
      (setq result nil)
      ) ; if
    (if (not result)
	(progn
	  (ding)
	  (message (concat "String does not match pattern: "
			   tpl-lexical-pattern))
	  ) ; progn
      ) ; if
					; return
    result
    ) ; let
  ) ; defun tpl-lexical-check

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-make-all-templates-template ()
  "Make a template consisting of a selection of all templates.
    Replace existing version if present."
					; Local Variables
  (let (name template-tree template-file template-list file-name name-list
	     new-template-list)
					; Body
    (message "Rebuilding list of all templates...")
					; Build name-list
    (setq template-list tpl-local-template-list)
    (setq new-template-list nil)
    (setq name-list nil)
    (while template-list
      (setq template-file (car template-list))
      (setq template-list (cdr template-list))
      (setq file-name (nth 0 template-file))
					; Remove existing version if present
      (if (not (string-equal file-name tpl-all-templates-file))
	  (progn
	    (setq new-template-list
		  (append new-template-list (list template-file)))
	    (setq name-list
		  (append name-list (nth 2 template-file)))
	    ) ; progn
	) ; if
      ) ; while template-list
					; Build template
    (save-window-excursion
      (set-buffer tpl-work-buffer)
      (erase-buffer)
      (while name-list
	(setq name (car name-list))
	(setq name-list (cdr name-list))
	(insert (car name) ":")
	(newline)
	) ; while name-list
      (shell-command-on-region (point-min) (point-max) "sort -u" t)
					; Insert preface
      (goto-char (point-min))
      (insert tpl-begin-template-definition " "
	      tpl-all-templates-name " "
	      tpl-selection-type)
      (newline)
      (beginning-of-line 0)
      (delete-char 1)			; Remove regular exression anchor
      (end-of-line)
      (newline)
      (insert tpl-begin-template-body)
      (beginning-of-line)
      (delete-char 1)			; Remove regular exression anchor
					; Insert suffix
      (goto-char (point-max))
      (insert tpl-end-template-body)
      (beginning-of-line)
      (delete-char 1)
      (end-of-line)
      (newline)
					; Create template
      (goto-char (point-min))
      (setq template-tree (tpl-scan-template))
      ) ; save-window-excursion
    (bury-buffer tpl-work-buffer)
					; Rebuild template-list
    (setq tpl-local-template-list
	  (append (list (list tpl-all-templates-file
			      (list template-tree) nil))
		  new-template-list))
    (setq tpl-all-templates-template-invalid nil)
    (message "Rebuilding list of all templates...Done.")
    ) ; let
  ) ; defun tpl-make-all-templates-template

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-make-completion-list ()
  "Create a completion list of template names for prompting."
					; Local Variables
  (let (name completion-list file-list template-file name-list)
					; Body
    ; Build completion list
    (setq completion-list nil)
    (setq file-list tpl-local-template-list)
    (while file-list
      (setq template-file (car file-list))
      (setq file-list (cdr file-list))
      (setq name-list (nth 2 template-file))
      (setq completion-list (append completion-list name-list))
      ) ; while file-list
					; return
    completion-list
    ) ; let
  ) ; defun tpl-make-completion-list

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-make-keymap ()
  "Make keymap for template-mode."
					; Local Variables
  (let ()
					; Body
    (setq tpl-saved-map (current-local-map))
    (if (not template-mode-map)
	(progn
	  (setq template-mode-map tpl-saved-map)
	  (define-key
	    template-mode-map "\^c\^t\t" 'expand-symbol)
	  (define-key
	    template-mode-map "\^c\^ta" 'add-symbol)
	  (define-key
	    template-mode-map "\^c\^te" 'expand-placeholder)
	  (define-key
	    template-mode-map "\^c\^tg" 'query-replace-groups)
	  (define-key
	    template-mode-map "\^c\^tl" 'query-replace-lines)
	  (define-key
	    template-mode-map "\^c\^tr" 'replace-line-with-placeholder)
	  (define-key
	    template-mode-map "\^c\^tt" 'generate-template)
	  (define-key
	    template-mode-map "\^c\^tu" 'unwrap-template-around-point)
	  (define-key
	    template-mode-map "\^c\^tw" 'wrap-template-around-word)
	  (define-key
	    template-mode-map "\^c\^tW" 'wrap-template-around-line)
	  (define-key
	    template-mode-map "\^c\^t\^e" 'expand-placeholders-in-region)
	  (define-key
	    template-mode-map "\^c\^t\^h" 'describe-template-mode)
	  (define-key
	    template-mode-map "\^c\^t\^k" 'delete-placeholder)
	  (define-key
	    template-mode-map "\^c\^t\^n" 'next-placeholder)
	  (define-key
	    template-mode-map "\^c\^t\^p" 'previous-placeholder)
	  (define-key
	    template-mode-map "\^c\^t\^r" 'replace-region-with-placeholder)
	  (define-key
	    template-mode-map "\^c\^t\^u" 'rewrap-template-around-point)
	  (define-key
	    template-mode-map "\^c\^t\^w" 'wrap-template-around-region)
	  (define-key
	    template-mode-map "\^c\^t?" 'generate-any-template)
	  ) ; progn
      ) ; if
    (use-local-map template-mode-map)
    ) ; let
  ) ; defun tpl-make-keymap

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-make-mode-entry (name file)
  "Constructor for mode entries from NAME FILE."
					; Local Variables
  (let ()
					; Body
    (list (list 'name name) (list 'file file))
    ) ; let
  ) ; defun tpl-make-mode-entry

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-make-placeholder-name ()
  "Create a name for a new template by searching for the first symbol
    after point."
					; Local Variables
  (let ()
					; Body
    (save-excursion
      (if (re-search-forward tpl-pattern-symbol nil t)
	  (progn
	    (setq tpl-formed-placeholder-name
		  (buffer-substring (match-beginning 0) (match-end 0)))
	    ) ; progn
	; else
	(progn
	  (setq tpl-formed-placeholder-name tpl-next-placeholder-name)
	  (tpl-increment-next-placeholder-name)
	  ) ; progn
	) ; if
      ) ; save-excursion
    ) ; let
  ) ; defun tpl-make-placeholder-name

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-make-template-entry (name templates)
  "Constructor for mode entries from NAME TEMPLATES."
					; Local Variables
  (let ()
					; Body
    (list (list 'name name) (list 'templates templates))
    ) ; let
  ) ; defun tpl-make-template-entry

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-make-template-list (file &optional buffer)
  "Create a template list from the templates in FILE.
    Optional second argument non-nil means use a buffer, not a file."
					; Local Variables
  (let (template-list template-tree template-name
		      name-list msg table root-name)
					; Body
    (setq msg (concat "Loading templates in " file ": "))
    (save-window-excursion
      (setq table (syntax-table))
      (set-buffer tpl-work-buffer)
      (erase-buffer)
      (if buffer
	  (insert-buffer file)
	; else
	(insert-file file)
	) ;if buffer
      (set-syntax-table table)
      (goto-char (point-min))
      (setq name-list nil)
      (while (re-search-forward
	      tpl-begin-template-definition (point-max) t)
	(beginning-of-line)
	(setq template-tree (tpl-scan-template))
	(setq template-list (append template-list (list template-tree)))
	(setq template-name (tpl-token-name template-tree))
	(message (concat msg template-name "..."))
	(if (not (equal tpl-lexical-type
			(tpl-token-type template-tree)))
	    (setq name-list
		  (append name-list (list (list template-name))))
	  ) ; if
	) ; while (re-search-forward...)
      (setq template-list
	    (list (tpl-root-of-file-name (file-name-nondirectory file))
		  template-list name-list))
      ) ; save-window-excursion
    (bury-buffer tpl-work-buffer)
    (message (concat msg "Done."))
					; return
    template-list
    ) ; let
  ) ; defun tpl-make-template-list

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-mode-file (mode-entry)
  "Selector for file field of MODE-ENTRY."
					; Local Variables
  (let ()
					; Body
    (car (cdr (assq 'file mode-entry)))
    ) ; let
  ) ; defun tpl-mode-file

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-mode-match  (mode-nm list)
  "Find mode-entry that matches MODE-NM in LIST."
					; Local Variables
  (let (entry)
					; Body
    (while list
      (setq entry (car list))
      (setq list (cdr list))
      (if (equal (tpl-mode-name entry) mode-nm)
	  (setq list nil)
	; else
	(setq entry nil)
	) ; if
      ) ; while
					; return
    entry
    ) ; let
  ) ; defun tpl-mode-match

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-mode-name (mode-entry)
  "Selector for name field of MODE-ENTRY."
					; Local Variables
  (let ()
					; Body
    (car (cdr (assq 'name mode-entry)))
    ) ; let
  ) ; defun tpl-mode-name

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-mode-templates (mode-entry)
  "Selector for templates field of MODE-ENTRY."
					; Local Variables
  (let ()
					; Body
    (car (cdr (assq 'templates mode-entry)))
    ) ; let
  ) ; defun tpl-mode-templates

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-parse-choice (line)
  "Break menu LINE into component parts: (string template) or (string nil)."
					; Local Variables
  (let (string-part template-part end-string end-template)
					; Body
					; Line = 
					; "abc" is string "abc"
					; "abc:" is template "abc"
					; "abc:def" is template "def"
					; ";" begins comment area
    (setq end-string (string-match tpl-pattern-symbol line))
    (setq string-part (substring line 0 (match-end 0)))
    (setq line (substring line (match-end 0)))
    (setq end-string (string-match "^\\(\\s \\)*:\\(\\s \\)*" line))
    (if end-string
	(progn
	  (setq line (substring line (match-end 0)))
	  (setq end-string (string-match
			    (concat "^" tpl-pattern-symbol) line))
	  (if end-string
	      (setq template-part (substring line 0 (match-end 0)))
	    ; else
	    (setq template-part string-part)
	    ) ; if end-template
	  ) ; progn
      ; else
      (progn
	(setq template-part nil)
	) ; progn
      ) ; if end-string
    (list string-part template-part)
    ) ; let
  ) ; defun tpl-parse-choice

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-rebuild-global-template-list (name templates)
  "Rebuild global template list, changing major mode NAME to
    include TEMPLATES."
					; Local Variables
  (let (mode-list mode-item entry result)
					; Body
    (setq result nil)
    (setq entry nil)
    (setq mode-list tpl-global-template-list)
    (while (and mode-list (not entry))
      (setq mode-item (car mode-list))
      (setq mode-list (cdr mode-list))
      (if (string-equal (tpl-mode-name mode-item) name)
	  (setq entry mode-item)
	; else
	(setq result (append result (list mode-item)))
	) ; if (equal (tpl-mode-name mode-item) name)
      ) ; while mode-list
    (if (not entry)
	(progn
	  (setq tpl-global-template-list
		(append result
			(list (tpl-make-template-entry name templates))))
	  (message "Added templates for %s." name)
	  ) ; progn
      ; else
      (if (or (not (tpl-mode-templates mode-item))
	      (y-or-n-p "Replace existing templates for this mode? "))
	  (progn
	    (setq result
		  (append result (list (tpl-make-template-entry name
								templates))))
	    (setq result (append result mode-list))
	    (setq tpl-global-template-list result)
	    (message "Added templates for %s." name)
	    ) ; progn
	) ; if
      ) ; if
    ) ; let
  ) ; defun tpl-rebuild-global-template-list

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-replace-group (from to)
  "Replace current region with a temporary placeholder.
    Arguments FROM and TO are ignored.  (They are only needed
    for compatibility with other replacement functions.)"
					; Local Variables
  (let (name)
					; Body
    (if tpl-get-placeholder-name-in-context
	(setq name nil)
      ; else
      (progn
	(setq name tpl-next-placeholder-name)
	(tpl-increment-next-placeholder-name)
	) ; progn
      ) ; if tpl-get-placeholder-name-in-context
    (replace-region-with-placeholder (mark) (point) name
				     "new.tpl" nil)
    ) ; let
  ) ; defun tpl-replace-group

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-replace-line (from to)
  "Replace current line with a temporary placeholder.
    Arguments FROM and TO are ignored.  (They are only needed
    for compatibility with other replacement functions.)"
					; Local Variables
  (let (name)
					; Body
    (if tpl-get-placeholder-name-in-context
	(setq name nil)
      ; else
      (progn
	(setq name tpl-next-placeholder-name)
	(tpl-increment-next-placeholder-name)
	) ; progn
      ) ; if tpl-get-placeholder-name-in-context
    (replace-line-with-placeholder 1 name "new.tpl" nil)
    ) ; let
  ) ; defun tpl-replace-line

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-root-of-file-name (file)
  "Find the root of FILE as a template file name."
					; Local Variables
  (let (result)
					; Body
    (cond
     ((and (> (length file) 7)
	   (equal (substring file -7) "tpl.elc"))
      (setq result (substring file 0 -7))
      )
     ((and (> (length file) 6)
	   (equal (substring file -6) "tpl.el"))
      (setq result (substring file 0 -6))
      )
     ((and (> (length file) 4)
	   (equal (substring file -4) ".tpl"))
      (setq result (substring file 0 -4))
      )
     (t
      (setq result file)
      )
     ) ; cond
					; return
    result
    ) ; let
  ) ; defun tpl-root-of-file-name

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-undo-keymap ()
  "Undo keymap for template-mode."
					; Local Variables
  (let ()
					; Body
    (use-local-map tpl-saved-map)
    ) ; let
  ) ; defun tpl-undo-keymap

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-unwrap-template (template &optional arg)
  "Find the enclosing TEMPLATE around point and replace it with
    whatever is matching the destination placeholder.
    Optional second argument non-nil causes mark to be placed
    at the beginning of the resulting region."
					; Local Variables
  (let (origin wrapper-pair wrapper-begin wrapper-end indent-amount
	       prefix another-wrapper-end start match-start
	       match-stop-marker)
					; Body
    (setq origin (point))
    (setq match-stop-marker (make-marker))
    (setq wrapper-pair (tpl-find-wrappers template))
    (setq wrapper-begin (nth 0 wrapper-pair))
    (setq wrapper-end (nth 1 wrapper-pair))
    (setq indent-amount (nth 2 wrapper-pair))
    (if (search-backward wrapper-begin (point-min) t)
	(progn
	  (setq start (point))
	  (search-forward wrapper-begin)
	  (delete-region start (point))
	  (setq match-start (point))
					; Get prefix of line for another try
					;   at matching ending part.
	  (beginning-of-line nil)
	  (setq prefix (buffer-substring (point) match-start))
	  (goto-char match-start)
	  (setq another-wrapper-end (concat (substring wrapper-end 0 1)
					    prefix
					    (substring wrapper-end 1)))
	  ) ; progn
      ; else
      (error "Enclosing template not found.")
      ) ; if
    (if (search-forward wrapper-end (point-max) t)
	(progn
	  (setq start (point))
	  (search-backward wrapper-end (point-min) t)
	  (delete-region (point) start)
	  (set-marker match-stop-marker (point))
	  ) ; progn
      ; else
					; This is a hack to fix indented
					;   matches.
      (if (search-forward another-wrapper-end (point-max) t)
	  (progn
	    (setq start (point))
	    (search-backward another-wrapper-end (point-min) t)
	    (delete-region (point) start)
	    (set-marker match-stop-marker (point))
	    (goto-char match-start)
	    (delete-backward-char (length prefix))
	    (setq match-start (- match-start (length prefix)))
	    ) ; progn
	; else
	(progn
	  (goto-char origin)
	  (error "End of enclosing template not found.")
	  ) ; progn
	) ; if ...another...
      ) ; if
    (goto-char match-start)
    (forward-line 1)
    (if (< (point) (marker-position match-stop-marker))
	(indent-rigidly (point) (marker-position match-stop-marker)
			(- 0 indent-amount))
      ) ; if
    (goto-char (marker-position match-stop-marker))
    (set-marker match-stop-marker nil)
    (if arg
	(push-mark match-start)
      ) ; if arg
    ) ; let
  ) ; defun tpl-unwrap-template

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-wrap-template (start stop template)
  "Replace the region between START and STOP with TEMPLATE,
    reinserting the replaced region at the destination placeholder.
    The region is indented rigidly at its insertion column."
					; Local Variables
  (let (save-expand-option region start-column orig-column)
					; Body
    (setq save-expand-option tpl-ask-expansion-depth)
    (setq tpl-ask-expansion-depth 0)
    (setq region (buffer-substring start stop))
    (delete-region start stop)
    (goto-char start)
    (setq orig-column (current-column))
    (unwind-protect			; Protect against nonexistent template
	(tpl-generate template)
      (setq start (point))
      (setq start-column (current-column))
      (insert region)
      (indent-rigidly start (point) (- start-column orig-column))
      (setq tpl-ask-expansion-depth save-expand-option)
      ) ; unwind-protect
    (message "Done.")
    ) ; let
  ) ; defun tpl-wrap-template

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-y-or-n-p (msg)
  "Display MSG and await positive ('y') or negative ('n') response.
    Differs from 'y-or-n-p' in that it leaves the cursor in the active
    window, rather than moving to the mode-line."
					; Local Variables
  (let (answered prompt reply result)
					; Body
    (setq answered nil)
    (setq prompt (concat msg "(y or n) "))
    (while (not answered)
      (message prompt)
      (setq reply (read-char))
      (cond
       ((char-equal reply ?y)
	(setq answered t)
	(setq result t)
	) ; (char-equal reply ?y)
       ((char-equal reply ? )
	(setq answered t)
	(setq result t)
	) ; (char-equal reply ? )
       ((char-equal reply ?n)
	(setq answered t)
	(setq result nil)
	) ; (char-equal reply ?n)
       ((char-equal reply ?\177)
	(setq answered t)
	(setq result nil)
	) ; (char-equal reply ?\177)
       (t
	(ding)
	(setq prompt (concat "Please respond 'y' or 'n'.  "
			     msg "(y or n) "))
	) ; t
       ) ; cond
      ) ; while (not answered)
					; return
    result
    ) ; let
  ) ; defun tpl-y-or-n-p

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;;; end of tplhelper.el
