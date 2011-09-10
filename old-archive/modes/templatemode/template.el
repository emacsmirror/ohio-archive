;;; template.el -- generate and manipulate templates
;;; Copyright (C) 1987 Mark A. Ardis.

(require 'tplvars)
(require 'menu)
(require 'symbol)
(require 'tplhelper)
(require 'tplparse)
(require 'tplreplace)
(require 'tplscan)

(provide 'template)

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; All global variables are in "tplvars".
;;; All non-interactive helper functions are in "tplhelper" or "tplscan".

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun template-mode ()
  "Toggle template-mode, a minor mode for manipulation of text via templates.
    Calls 'template-mode-hook' if it is defined."
  (interactive)
					; Local Variables
  (let (file-name)
					; Body
    (setq template-mode (not template-mode))
    (set-buffer-modified-p (buffer-modified-p))
    (if template-mode
	(progn
	  (setq file-name (buffer-name))
	  (setq sym-completion-buffer (concat "id-" file-name))
	  (if tpl-save-identifier-file
	      (find-file-noselect sym-completion-buffer)
	    ; else
	    (get-buffer-create sym-completion-buffer)
	    ) ; if
	  (bury-buffer sym-completion-buffer)
	  (tpl-initialize-scan)
	  (tpl-build-template-list)
	  (tpl-make-keymap)
	  (and (boundp 'template-mode-hook)
	       template-mode-hook
	       (funcall template-mode-hook))
	  ) ; progn
      ; else
      (progn
	(setq tpl-local-template-list nil)
	(tpl-undo-keymap)
	) ; progn
      ) ; if
    ) ; let
  ) ; defun template-mode

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun compile-templates (template-file)
  "Compile the templates in TEMPLATE-FILE into a Lisp structure."
  (interactive	"Fcompile-templates: Template file? ")
					; Local Variables
  (let (file-list file-name file
		  found root-name object-file)
					; Body
    (setq root-name (file-name-nondirectory template-file))
    (if (and (> (length root-name) 4)
	     (equal (substring root-name -4) ".tpl"))
	(setq root-name (substring root-name 0 -4))
      ) ; if
    (setq object-file (concat (file-name-directory template-file)
			      root-name "tpl.el"))
    (setq file-list tpl-local-template-list)
    (setq found nil)
    (while (and file-list (not found))
      (setq file (car file-list))
      (setq file-list (cdr file-list))
      (setq file-name (nth 0 file))
      (if (equal file-name root-name)
	  (setq found t)
	) ; if (equal file-name root-name)
      ) ; while file-list
    (if found
	(progn
	  (save-window-excursion
	    (set-buffer tpl-work-buffer)
	    (erase-buffer)
	    (message "Compiling templates into a lisp form...")
	    (insert "(setq template-list '")
	    (insert (prin1-to-string file))
	    (insert ")")
	    (newline)
	    (write-region (point-min) (point-max) object-file)
	    (byte-compile-file object-file)
	    (delete-file object-file)
	    ) ; save-window-excursion
	  (bury-buffer tpl-work-buffer)
	  ) ; progn
      ; else
      (progn
	(error "Cannot find " template-file)
	) ; progn
      ) ; if found
					; return
    object-file
    ) ; let
  ) ; defun compile-templates

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun delete-placeholder ()
  "Delete the placeholder at point."
  (interactive)
					; Local Variables
  (let (start)
					; Body
    (if (looking-at tpl-pattern-placeholder)
	(progn
	  (setq start (point))
	  (re-search-forward tpl-pattern-placeholder)
	  (delete-region start (point))
	  ) ; progn
      ; else
      (error "No placholder here!")
      ) ; if (looking-at tpl-pattern-placeholder)
    ) ; let
  ) ; defun delete-placeholder

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun describe-template-mode ()
  "Describe template-mode and its keybindings."
  (interactive)
					; Local Variables
  (let (orig-buffer)
					; Body
    (setq orig-buffer (buffer-name))
    (pop-to-buffer (get-buffer-create "*Help*"))
    (erase-buffer)
    (insert "Template-mode is a minor mode for manipulating regions of text\n")
    (insert "called `templates'.  Templates have some of the attributes of\n")
    (insert "productions in a context-free grammar.  They also have some of\n")
    (insert "the attributes of rectangular pictures.  ")
    (insert "For more information try:\n\n")
    (insert "   C-h b  (describe-bindings)  Shows all of the new bindings.\n")
    (insert "   C-h i  (info)  A user manual is available via `info'.\n")
    (goto-char (point-min))
    (pop-to-buffer orig-buffer)
    ) ; let
  ) ; defun describe-template-mode

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun expand-placeholder ()
  "Expand the placeholder at point---interactive version."
  (interactive)
					; Local Variables
  (let (start stop)
					; Body
    (if (looking-at tpl-pattern-placeholder)
	(progn
	  (setq start (point))
	  (setq tpl-destination-needed t)
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
	  ) ; progn
      ; else
      (error "expand-placeholder: No placeholder at point!")
      ) ; if
    ) ; let
  ) ; defun expand-placeholder

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun expand-placeholders-in-region (start stop)
  "Expand each placeholder in the region between START and STOP."
  (interactive "r")
					; Local Variables
  (let (stop-marker save)
					; Body
    (goto-char start)
    (setq stop-marker (make-marker))
    (set-marker stop-marker stop)
					; (The check for out-of-bounds is
					;   needed for a placeholder at
					;   the end of the region.)
    (while (and (< (point) (marker-position stop-marker))
		(re-search-forward
		 tpl-pattern-placeholder (marker-position stop-marker) t))
      (re-search-backward tpl-pattern-placeholder)
      (if (looking-at tpl-begin-optional)
	  (if (or (equal t tpl-keep-optional-placeholders)
		  (and tpl-keep-optional-placeholders
		       (tpl-y-or-n-p "Keep optional placeholder? ")))
	      (progn
		(delete-char (length tpl-begin-optional))
		(insert-before-markers tpl-begin-placeholder)
		(re-search-backward tpl-begin-placeholder)
		(if (or (< tpl-expansion-depth tpl-ask-expansion-depth)
			(tpl-y-or-n-p "Expand? "))
		    (progn
		      (setq tpl-expansion-depth (1+ tpl-expansion-depth))
		      (unwind-protect
			  (tpl-expand-placeholder (marker-position stop-marker))
			(setq tpl-expansion-depth (1- tpl-expansion-depth))
			) ; unwind-protect
		      ) ; progn
		  ; else
		  (progn
		    (re-search-forward tpl-pattern-placeholder)
		    ) ; progn
		  ) ; if (tpl-y-or-n-p "Expand? ")
		) ; progn
	    ; else
	    (progn
	      (setq save (point))
	      (re-search-forward tpl-pattern-placeholder)
	      (delete-region save (point))
	      (if (tpl-blank-line)
		  (delete-indentation)
		) ; if
	      ) ; progn
	    ) ; if (tpl-y-or-n-p "Keep optional placeholder? ")
	; else
	(if (or (< tpl-expansion-depth tpl-ask-expansion-depth)
		(tpl-y-or-n-p "Expand? "))
	    (progn
	      (setq tpl-expansion-depth (1+ tpl-expansion-depth))
	      (unwind-protect
		  (tpl-expand-placeholder (marker-position stop-marker))
		(setq tpl-expansion-depth (1- tpl-expansion-depth))
		) ; unwind-protect
	      ) ; progn
	  ; else
	  (progn
	    (re-search-forward tpl-pattern-placeholder)
	    ) ; progn
	  ) ; if (tpl-y-or-n-p "Expand? ")
	) ; if (looking-at tpl-begin-optional)
      ) ; while (re-search-forward...)
    (if (< (point) (marker-position stop-marker))
	(goto-char (marker-position stop-marker))
      ) ; if
    (set-marker stop-marker nil)
    ) ; let
  ) ; defun expand-placeholders-in-region

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun generate-any-template ()
  "Generate any template, by using the special all-templates-template."
  (interactive)
					; Local Variables
  (let ()
					; Body
    (if (or tpl-all-templates-template-invalid
	    (not (tpl-find-template tpl-all-templates-name)))
	(if (y-or-n-p "Cannot find all-templates-template.  Rebuild? ")
	    (progn
	      (tpl-make-all-templates-template)
	      (tpl-generate tpl-all-templates-name)
	      ) ; progn
	  ; else
	  (error "Aborted.")
	  ) ; if (y-or-n-p ...)
      ; else
      (tpl-generate tpl-all-templates-name)
      ) ; if
    ) ; let
  ) ; defun generate-any-template

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun generate-template ()
  "Complete template name and call tpl-generate."
  (interactive)
					; Local Variables
  (let (name name-list)
					; Body
					; Build completion list
    (setq name-list (tpl-make-completion-list))
    ; Query for name and generate
    (setq name
	  (completing-read "generate-template: Name of template? "
			   name-list nil t nil))
    (tpl-generate name)
    ) ; let
  ) ; defun generate-template

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun load-tpl-buffer (&optional buffer)
  "Load all of the templates (in the optional BUFFER).
    Defaults to 'tpl-new-template-buffer."
  (interactive)
					; Local Variables
  (let (root-name new-list)
					; Body
    (if (not buffer)
	(setq buffer
	      (read-buffer "load-tpl-buffer: Template buffer? "
			   tpl-new-template-buffer t))
      ) ; if (not buffer)
    (setq new-list (tpl-make-template-list buffer t))
    (setq tpl-local-template-list (append (list new-list) tpl-local-template-list))
    (if tpl-rebuild-all-templates-template
	(tpl-make-all-templates-template)
      ; else
      (setq tpl-all-templates-template-invalid t)
      ) ; if
    (if (interactive-p)
	(if (y-or-n-p "Rebuild global template list with these new templates? ")
	    (progn
	      (setq mode-nm (read-string "Mode name for global template list? "))
	      (tpl-rebuild-global-template-list mode-nm new-list)
	      ) ; progn
	  ) ; if (y-or-n-p ...)
      ) ; if (interactive-p)
    ) ; let
  ) ; defun load-tpl-buffer

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun load-tpl-file (&optional file)
  "Load all of the templates (in the optional FILE).
    Uses file name completion in the current directory, and
    defaults to 'tpl-new-template-buffer."
  (interactive)
					; Local Variables
  (let (root-name new-list)
					; Body
    (if (not file)
	(setq file
	      (expand-file-name (read-file-name
				 (concat "load-tpl-file: Template file? ("
					 tpl-new-template-buffer ") ")
				 nil tpl-new-template-buffer)))
      ) ; if (not file)
    (setq root-name (tpl-root-of-file-name (file-name-nondirectory file)))
    (setq new-list (tpl-make-template-list file))
    (setq tpl-local-template-list (append (list new-list) tpl-local-template-list))
    (if tpl-rebuild-all-templates-template
	(tpl-make-all-templates-template)
      ; else
      (setq tpl-all-templates-template-invalid t)
      ) ; if
    (if (interactive-p)
	(if (y-or-n-p "Rebuild global template list with these new templates? ")
	    (progn
	      (setq mode-nm (read-string "Mode name for global template list? "))
	      (tpl-rebuild-global-template-list mode-nm new-list)
	      ) ; progn
	  ) ; if (y-or-n-p ...)
      ) ; if (interactive-p)
    ) ; let
  ) ; defun load-tpl-file

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun load-tpl-library (file &optional mode-nm)
  "Find FILE and add all of the templates in it to the template list.
    (Uses the 'tpl-load-path value to find the file.)  Optional second
    argument MODE-NM is used to rebuild the global template list."
  (interactive "sload-tpl-library: File name? ")
					; Local Variables
  (let (found head-template-list tail-template-list template-file file-name
	      root-name template-list new-list)
					; Body
    (setq root-name (tpl-root-of-file-name (file-name-nondirectory file)))
					; Look for file in existing list
    (setq found nil)
    (setq head-template-list nil)
    (setq tail-template-list tpl-local-template-list)
    (while (and tail-template-list (not found))
      (setq template-file (car tail-template-list))
      (setq tail-template-list (cdr tail-template-list))
      (setq file-name (nth 0 template-file))
      (if (equal file-name root-name)
	  (setq found t)
	; else
	(setq head-template-list
	      (append head-template-list (list template-file)))
	) ; if (equal file-name file)
      ) ; while (and tail-template-list (not found))
					; If found, query about replacing
    (if (and found
	     (not (y-or-n-p "File already loaded.  Replace? ")))
	(error "File already loaded.  Aborted.")
      ) ; if
    (setq tpl-local-template-list (append head-template-list tail-template-list))
					; Find template file
    (setq file (tpl-find-template-file root-name))
    (if (and (> (length file) 4)
	     (equal (substring file -4) ".elc"))
	(progn
	  (load file)
	  (setq new-list template-list)
	  ) ; progn
      ; else
      (progn
	(setq new-list (tpl-make-template-list file))
	) ; progn
      ) ; if compiled
    (setq tpl-local-template-list
	  (append (list new-list)
		  tpl-local-template-list))
    (if tpl-rebuild-all-templates-template
	(tpl-make-all-templates-template)
      ; else
      (setq tpl-all-templates-template-invalid t)
      ) ; if
    (if (interactive-p)
	(if (y-or-n-p "Rebuild global template list with these new templates? ")
	    (progn
	      (setq mode-nm
		    (read-minibuffer "Mode name for global template list? "))
	      (tpl-rebuild-global-template-list mode-nm new-list)
	      ) ; progn
	  ) ; if
      ; else
      (tpl-rebuild-global-template-list mode-nm new-list)
      ) ; if
    ) ; let
  ) ; defun load-tpl-library

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun next-placeholder ()
  "Search forward for the next placeholder."
  (interactive)
					; Local Variables
  (let (count)
					; Body
    (if (looking-at tpl-pattern-placeholder)
	(setq count 2)
      ; else
      (setq count 1)
      ) ; if (looking-at tpl-pattern-placeholder)
    (re-search-forward tpl-pattern-placeholder (point-max) nil count)
    (re-search-backward tpl-pattern-placeholder)
    ) ; let
  ) ; defun next-placeholder

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun previous-placeholder ()
  "Search backward for the previous placeholder."
  (interactive)
					; Local Variables
  (let ()
					; Body
    (re-search-backward tpl-pattern-placeholder)
    ) ; let
  ) ; defun previous-placeholder

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun query-replace-groups (start stop)
  "Replace some lines after point, beginning with a line that
    matches START, and ending before a line that matches STOP, with
    temporary placeholders.  As each match is found, the user
    must type a character saying what to do with it.  For directions,
    type \\[help-command] at that time."
  (interactive "squery-replace-groups starting with: \nsquery-replace-groups starting with %s ending with: ")
					; Local Variables
  (let ()
					; Body
    (setq tpl-end-group (concat "^" stop))
    (perform-replace-tpl
     (concat "^" start)
     "placeholder" t nil nil
     're-search-forward 'tpl-find-end-of-group 'tpl-replace-group
     'tpl-find-next-group)
    ) ; let
  ) ; defun query-replace-groups

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun query-replace-lines (string)
  "Replace some lines after point matching leading STRING with
    temporary placeholders.  As each match is found, the user
    must type a character saying what to do with it.  For directions,
    type \\[help-command] at that time."
  (interactive "squery-replace-lines with leading pattern: ")
					; Local Variables
  (let ()
					; Body
    (perform-replace-tpl
     (concat "^" string)
     "placeholder" t nil nil
     're-search-forward 'beginning-of-line 'tpl-replace-line)
    ) ; let
  ) ; defun query-replace-lines

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun region-to-tpl (start stop &optional name file parse)
  "Create a template with the text between START and STOP.
    Optionally, call it NAME and put the template in FILE.
    Optional fifth argument PARSE specifies whether the template
    should be parsed: if t parse, if nil do not parse, if neither
    t nor nil ask about parsing."
  (interactive "r")
					; Local Variables
  (let (string column text-file template table parse name-place
	       suggestion begin-body)
					; Body
    (if (not file)
	(setq file
	      (expand-file-name (read-file-name
				 (concat "Template file? ("
					 tpl-new-template-buffer ") ")
				 nil tpl-new-template-buffer)))
      ) ; if (not file)
    (if (and parse (not (equal parse t)))
	(setq parse (y-or-n-p "region-to-tpl: Parse the template? "))
      ) ; if
    (setq string (buffer-substring start stop))
    (goto-char start)
    (setq column (current-column))
    (goto-char stop)
    (setq text-file (buffer-file-name))
    (if (not (equal file text-file))
	(progn
	  (find-file-other-window file)
	  (goto-char (point-min))
	  (open-line 1)
	  (insert tpl-begin-template-definition " ")
	  (setq name-place (point))
	  (insert " ")
	  (if parse
	      (insert tpl-sequence-type)
	    (insert tpl-string-type)
	    ) ; if parse
	  (beginning-of-line nil)
	  (delete-char 1)		; Remove regexp anchor
	  (setq name-place (1- name-place))
	  (end-of-line nil)
	  (newline)
	  (insert tpl-begin-template-body)
	  (newline)
	  (beginning-of-line 0)
	  (delete-char 1)		; Remove regexp anchor
	  (beginning-of-line 2)
					; Insert body of template
	  (setq begin-body (point))
	  (insert string)
	  (newline)
					; Fix indentation
	  (indent-rigidly begin-body (point) (- 0 column))
	  (insert tpl-end-template-body)
	  (newline)
	  (beginning-of-line 0)
	  (delete-char 1)		; Remove regexp anchor
	  (goto-char name-place)
	  (if (not name)
	      (if tpl-get-placeholder-name-in-context
		  (progn
		    (if tpl-form-placeholder-name-from-context
			(setq suggestion tpl-formed-placeholder-name)
		      ; else
		      (progn
			(setq suggestion tpl-next-placeholder-name)
			(tpl-increment-next-placeholder-name)
			) ; progn
		      ) ; if tpl-form-placeholder-name-from-context
		    (insert suggestion)
		    (if tpl-query-flag
			(progn
			  (search-backward suggestion)
			  (setq name
				(sym-read-string "Placeholder name? "
						 suggestion))
			  (if (equal (length name) 0)
			      (progn
				(setq name suggestion)
				) ; progn
			    ) ; if
			  ) ; progn
		      ; else
		      (setq name suggestion)
		      ) ; if tpl-query-flag
		    ) ; progn
		; else
		(progn
		  (setq name (tpl-get-placeholder-name))
		  (insert name)
		  ) ; progn
		) ; if tpl-get-placeholder-name-in-context
	    ; else
	    (insert name)
	    ) ; if (not-name)
	  ) ; progn
      ; else
      (error "Cannot reuse this file for templates!")
      ) ; if (not (equal file text-file))
					; return
    name
    ) ; let
  ) ; defun region-to-tpl

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun replace-line-with-placeholder (count &optional name file parse)
  "Replace the line containing point with a placeholder.
    Prefix argument COUNT gives number of lines
    (ending with current line).  Optional second argument NAME is used
    for placeholder name.  Optional third argument FILE is used for
    file to store template.  Optional fourth argument PARSE
    specifies whether template should be parsed.  (See 'region-to-tpl
    for interpretation.)"
  (interactive "p")
					; Local Variables
  (let (start)
					; Body
    (if (interactive-p)
	(progn
	  (setq parse "ask")
	  (setq file "new.tpl")
	  ) ; progn
      ) ; if
    (setq count (1- count))
    (forward-line (* count -1))
    (setq start (point))
    (forward-line count)
    (end-of-line nil)
    (replace-region-with-placeholder start (point) name file parse)
    ) ; let
  ) ; defun replace-line-with-placeholder

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun replace-region-with-placeholder (start stop &optional name file parse)
  "Replace the region between START and STOP with a placeholder.
    Optionally, call it NAME and put the template in FILE.
    Optional fifth argument PARSE specifies whether template
    should be parsed.  (See 'region-to-tpl for interpretation.)"
  (interactive "r")
					; Local Variables
  (let (start-marker stop-marker)
					; Body
    (if (interactive-p)
	(progn
	  (setq parse "ask")
	  (setq file "new.tpl")
	  ) ; progn
      ) ; if
    (if (> (- stop start) 0)
	(progn
	  (save-window-excursion
	    (setq start-marker (make-marker))
	    (set-marker start-marker start)
	    (setq stop-marker (make-marker))
	    (set-marker stop-marker stop)
	    (setq name (region-to-tpl start stop name file parse))
	    (if tpl-auto-save-new-templates
		(save-buffer)
	      ) ; if tpl-auto-save-new-templates
	    ) ; save-window-excursion
	  (delete-region (marker-position start-marker)
			 (marker-position stop-marker))
	  (unwind-protect
	      (if tpl-auto-load-new-templates
		  (load-tpl-buffer (buffer-name (get-file-buffer file)))
		) ; if
	    (goto-char (marker-position start-marker))
	    (set-marker start-marker nil)
	    (set-marker stop-marker nil)
	    (insert-before-markers
	     tpl-begin-placeholder name tpl-end-placeholder))
	  ) ; progn
      ; else
      (insert-before-markers
       tpl-begin-placeholder name tpl-end-placeholder)
      ) ; if (> (- stop start) 0)
    ) ; let
  ) ; defun replace-region-with-placeholder

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun rewrap-template-around-point ()
  "Unwrap FIRST template around point and wrap with SECOND.
    Template names are prompted for with completing-read.
    A side effect of this function is to push-mark at the beginning of the
    enclosed region."
  (interactive)
					; Local Variables
  (let (name-list first second)
					; Body
					; Build completion list
    (setq name-list (tpl-make-completion-list))
					; Query for name to unwrap
    (setq first
	  (completing-read "rewrap-template: Name of enclosing template? "
			   name-list nil t nil))
					; Query for name to wrap
    (setq second
	  (completing-read "rewrap-template: Name of new template? "
			   name-list nil t nil))
    (tpl-unwrap-template first t)
    (tpl-wrap-template (mark) (point) second)
    ) ; let
  ) ; defun rewrap-template-around-point

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-recompile-templates (template-dir)
  "Compile a standard list of templates in TEMPLATE-DIR."
  (interactive "Dtpl-recompile-templates: Template directory? ")
					; Local Variables
  (let (prefix)
					; Body
    (autoload 'awk-mode "awk")
    (autoload 'bib-mode "bib")
    (autoload 'pascal-mode "pascal")
    (setq prefix template-dir)
    (if (not (equal (substring prefix -1) "/"))
	(setq prefix (concat prefix "/"))
      ) ; if
    (if (not template-mode)
	(template-mode)
      ) ; if
    (compile-templates (concat prefix "generic.tpl"))
    (awk-mode)
    (template-mode)
    (compile-templates (concat prefix "awk.tpl"))
    (bib-mode)
    (template-mode)
    (compile-templates (concat prefix "bib.tpl"))
    (c-mode)
    (template-mode)
    (compile-templates (concat prefix "c.tpl"))
    (emacs-lisp-mode)
    (template-mode)
    (compile-templates (concat prefix "elisp.tpl"))
    (latex-mode)
    (template-mode)
    (compile-templates (concat prefix "latex.tpl"))
    (pascal-mode)
    (template-mode)
    (compile-templates (concat prefix "pascal.tpl"))
    (scribe-mode)
    (template-mode)
    (compile-templates (concat prefix "scribe.tpl"))
    (texinfo-mode)
    (template-mode)
    (compile-templates (concat prefix "texinfo.tpl"))
  ) ; let
) ; defun tpl-recompile-templates

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun unwrap-template-around-point (arg)
  "Complete template name and call tpl-unwrap-template.  Prefix argument
    ARG non-nil causes the mark to be set at the beginning of the resulting
    region."
  (interactive "P")
					; Local Variables
  (let (name name-list)
					; Body
					; Build completion list
    (setq name-list (tpl-make-completion-list))
					; Query for name and unwrap
    (setq name
	  (completing-read "unwrap-template-around-point: Name of template? "
			   name-list nil t nil))
    (tpl-unwrap-template name arg)
    ) ; let
  ) ; defun unwrap-template-around-point

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun wrap-template-around-line (count)
  "Replace COUNT lines including point with TEMPLATE,
    reinserting the replaced line at the destination placeholder.
    Prefix argument indicates number of lines to wrap.
    Second argument, TEMPLATE, is read with completing-read."
  (interactive "p")
					; Local Variables
  (let (start)
					; Body
    (setq count (1- count))
    (forward-line (* count -1))
    (beginning-of-line nil)
    (setq start (point))
    (forward-line count)
    (end-of-line nil)
    (wrap-template-around-region start (point))
    ) ; let
  ) ; defun wrap-template-around-line

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun wrap-template-around-region (start stop)
  "Replace the region between START and STOP with TEMPLATE,
    reinserting the replaced region at the destination placeholder.
    The region is indented rigidly at its insertion column.
    The third argument, TEMPLATE, is read with completing-read."
  (interactive "r")
					; Local Variables
  (let (name-list template)
					; Body
    (setq name-list (tpl-make-completion-list))
    (setq template (completing-read "wrap-template: Name of template? "
				    name-list nil t nil))
    (tpl-wrap-template start stop template)
    ) ; let
  ) ; defun wrap-template-around-region

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun wrap-template-around-word (count)
  "Replace COUNT words before point with TEMPLATE,
    reinserting the replaced word at the destination placeholder.
    Prefix argument indicates number of words to wrap.
    Second argument, TEMPLATE, is read with completing-read."
  (interactive "p")
					; Local Variables
  (let (start)
					; Body
    (forward-word (* count -1))
    (setq start (point))
    (forward-word count)
    (wrap-template-around-region start (point))
    ) ; let
  ) ; defun wrap-template-around-word

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(tpl-initialize-modes)

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;;; end of template.el
