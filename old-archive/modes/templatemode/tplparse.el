;;; tplparse.el -- Parsing routines for template package
;;; Copyright (C) 1987 Mark A. Ardis.

(require 'tplvars)
(require 'tplhelper)

(provide 'tplparse)

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; All global variables are in "tplvars"

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun looking-at-tpl ()
  "t if text after point matches specified template."
  (interactive)
					; Local Variables
  (let (name-list tpl-name)
					; Body
    (setq name-list (tpl-make-completion-list))
    (setq tpl-name (completing-read "looking-at-tpl: Template name? "
				    name-list nil t nil))
    (tpl-looking-at tpl-name)
  ) ; let
) ; defun looking-at-tpl

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun query-replace-tpl ()
  "Replace some instances of a template with corresponding instances
   of another."
  (interactive)
					; Local Variables
  (let (name-list from to)
					; Body
    (setq name-list (tpl-make-completion-list))
    (setq from (completing-read "query-replace-tpl: From? "
				    name-list nil t nil))
    (setq to (completing-read (concat "query-replace-tpl: From " from " To? ")
				    name-list nil t nil))
    (tpl-query-replace from to)
  ) ; let
) ; defun query-replace-tpl

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun replace-tpl ()
  "Replace an instance of a template with a corresponding instance
   of another template."
  (interactive)
					; Local Variables
  (let (name-list from to)
					; Body
    (setq name-list (tpl-make-completion-list))
    (setq from (completing-read "replace-tpl: From? "
				    name-list nil t nil))
    (setq to (completing-read (concat "replace-tpl: From " from " To? ")
				    name-list nil t nil))
    (while (tpl-search-forward from (point-max) t)
      (exchange-point-and-mark)
      (tpl-replace from to)
      ) ; while
  ) ; let
) ; defun replace-tpl

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun search-forward-tpl ()
  "Search forward from point for a template."
  (interactive)
					; Local Variables
  (let (name-list tpl-name)
					; Body
    (setq name-list (tpl-make-completion-list))
    (setq tpl-name (completing-read "search-forward-tpl: Name of template? "
				    name-list nil t nil))
    (tpl-search-forward tpl-name)
  ) ; let
) ; defun search-forward-tpl

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-delete-leading-whitespace (text-list)
  "Remove leading whitespace tokens from TEXT-LIST and return remaining list."
					; Local Variables
  (let ()
					; Body
    (while (and text-list (equal tpl-whitespace-type
				 (tpl-token-name (car text-list))))
      (setq text-list (cdr text-list))
      ) ; while
    ; return
    text-list
    ) ; let
  ) ; defun tpl-delete-leading-whitespace

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-fix-match (tree old new)
  "Adjust indentation in TREE from OLD to NEW."
					; Local Variables
  (let (result token-list token)
					; Body
    (if (not new)
	(setq new old)
      ) ; if
    (setq result nil)
    (setq token-list (tpl-token-value tree))
    (while token-list
      (setq token (car token-list))
      (setq token-list (cdr token-list))
      ;(debug nil "token" token)
      (if (and (equal tpl-indentation-type (tpl-token-name token))
	       (/= tpl-comment-level (tpl-token-value token)))
	  (setq token (tpl-make-token (tpl-token-type token)
				      (tpl-token-name token)
				      (+ (- new old) (tpl-token-value token))))
	) ; if
      (setq result (append result (list token)))
      ) ; while token-list
    ; return
    result
    ) ; let
  ) ; defun tpl-fix-match

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-get-match (placeholder tree indent)
  "Find match for PLACEHOLDER in TREE.  Adjust matched value with INDENT."
					; Local Variables
  (let (name match token token-type current-indent)
					; Body
    (setq name (tpl-token-name (tpl-parse-placeholder (tpl-token-value placeholder))))
    (setq match nil)
    (while (and tree (not match))
      (setq token (car tree))
      (setq tree (cdr tree))
      (setq token-type (tpl-token-type token))
      ;(debug nil "token-type" token-type)
      (if (equal tpl-terminal-type token-type)
	  (if (equal tpl-indentation-type (tpl-token-name token))
	      (setq current-indent (tpl-token-value token))
	    ) ; if (equal tpl-indentation-type (tpl-token-name token))
	; else
	(if (equal name
		   (tpl-token-name
		    (tpl-parse-placeholder (tpl-token-name token))))
	    (setq match (tpl-fix-match token indent current-indent))
	  ) ; if (equal name...)
	) ; if (equal tpl-terminal-type token-type)
      ) ; while (and tree (not match))
    ; return
    match
    ) ; let
  ) ; defun tpl-get-match

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-get-placeholder-end (placeholder tpl-name &optional occurrence)
  "Prompt user for end of PLACEHOLDER in TPL-NAME.
   Optional third argument OCCURRENCE specifies which
   occurrence of placeholder to find."
					; Local Variables
  (let (template msg return stop size)
					; Body
    (if (not occurrence)
	(setq occurrence 1)
      ) ; if
					; Get value before changing buffer
    (setq template (tpl-find-template tpl-name))
    (save-window-excursion
      (delete-other-windows)
      (pop-to-buffer (get-buffer-create "*Template*"))
      (erase-buffer)
      (tpl-unscan template)
					; Size the window
      (setq stop (point-max))
      (goto-char (point-min))
      (setq size (1+ (count-lines (point) stop)))
      (setq size (max size window-min-height))
      (if (< size (window-height))
	  (shrink-window (- (window-height) size))
	) ; if
					; Find the placeholder
      (search-forward placeholder (point-max) t occurrence)
      (other-window 1)
      (setq msg (concat "In \"" tpl-name "\" looking for end of \""
			placeholder "\""))
      (setq return (tpl-get-position (point) (point-max) msg))
      ) ; save-window-excursion
    (bury-buffer "*Template*")
    return
  ) ; let
) ; defun tpl-get-placeholder-end

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-get-position (start stop msg &optional start-pos narrow)
  "Prompt user for a location between START and STOP with MSG.
   Optional fourth argument START-POS may be used for initial
   placement of point.  Fifth argument NARROW, if non-nil,
   narrows the region."
					; Local Variables
  (let (looking was-modifiable)
					; Body
					; Check for valid region
    (if (< stop start)
	(error "tpl-get-position: Invalid region specification.")
      ) ; if
					; Save current status
    (if (not start-pos)
	(setq start-pos start)
      ) ; if
    (save-restriction
      (if narrow
	  (narrow-to-region start stop)
	) ; if
      (setq was-modifiable (not buffer-read-only))
      (if was-modifiable
	  (toggle-read-only)
	) ; if was-modifiable
      (setq orig-buffer (current-buffer))
					; Loop until acceptable answer
      (setq looking t)
      (while looking
	(goto-char start-pos)
	(message msg)
					; Wait for user selection
	(recursive-edit)
					; Check validity
	(if (or (not (equal orig-buffer (current-buffer)))
		(< (point) start)
		(> (point) stop))
	    (progn
	      (ding)
	      (message "Selected position out of bounds.")
	      (sit-for 2)
	      (pop-to-buffer orig-buffer)
	      (goto-char start-pos)
	      ) ; progn
	  ; else
	  (setq looking nil)
	  ) ; if
	) ; while looking
					; Restore original status
      (if was-modifiable
	  (toggle-read-only)
	) ; if was-modifiable
      (if narrow
	  (widen)
	) ; if narrow
      ) ; save-restriction
    (point)				; return
  ) ; let
) ; defun tpl-get-position

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-leading-text (template)
  "Return literal text string at start of TEMPLATE (a name)."
					; Local Variables
  (let (body start stop result)
					; Body
    (setq body (tpl-find-template template))
    (if (not body)
	(error "Cannot find template.")
      ) ; if (not body)
    (get-buffer-create "*Work*")
    (save-window-excursion
      (set-buffer "*Work*")
      (erase-buffer)
      (tpl-unscan body)
      (goto-char (point-min))
      (setq start (point))
      (end-of-line nil)
      (setq stop (point))
      (goto-char start)
      (if (re-search-forward tpl-begin-placeholder stop start)
	  (re-search-backward tpl-begin-placeholder)
	) ; if
      (setq result (buffer-substring start (point)))
      ) ; save-window-excursion
    ; return
    result
    ) ; let
  ) ; defun tpl-leading-text

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-line-to-token (tree)
  "Convert TREE from line-format to token-format."
					; Local Variables
  (let (line-list line token result type name)
					; Body
    (setq result nil)
    (setq type (tpl-token-type tree))
    (setq name (tpl-token-name tree))
    (setq line-list (tpl-token-value tree))
    (while line-list
      (setq line (car line-list))
      (setq line-list (cdr line-list))
      (setq result
	    (append result
		    (list (tpl-make-token tpl-terminal-type
					  tpl-indentation-type
					  (tpl-line-indent line)))))
      (setq result (append result (tpl-line-tokens line)))
      (if line-list
	  (setq result (append result (list tpl-newline-token)))
	) ; if line-list
      ) ; while line-list
    (setq result (tpl-make-token type name result))
    ; return
    result
    ) ; let
  ) ; defun tpl-line-to-token

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-looking-at (name)
  "t if text after point matches template NAME"
					; Local Variables
  (let (result)
					; Body
    (setq result (tpl-match-template name))
    (if result
	t
      nil
      ) ; if
    ) ; let
  ) ; defun tpl-looking-at

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-match-function-template (template)
  "Match TEMPLATE and return t or nil."
					; Local Variables
  (let ()
					; Body
    (error "tpl-match-function-type: Cannot match function-type templates.")
    ) ; let
  ) ; defun tpl-match-function-template

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-match-lexical-template (template)
  "Match TEMPLATE and return t or nil."
					; Local Variables
  (let ()
					; Body
    (looking-at (tpl-token-value template))
    ) ; let
  ) ; defun tpl-match-lexical-template

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-match-line (pattern text)
  "Attempt to match the line described by PATTERN with TEXT. Return t or nil."
					; Local Variables
  (let (pattern-list text-list next-pattern result success)
					; Body
    (if (and text
	     (= (tpl-line-indent pattern) (tpl-line-indent text)))
	(progn
	  (setq success t)
	  (setq pattern-list (tpl-line-tokens pattern))
	  (setq text-list (tpl-line-tokens text))
	  (while (and pattern-list success text-list)
	    (setq next-pattern (car pattern-list))
	    (setq pattern-list (cdr pattern-list))
	    (setq result (tpl-match-token next-pattern text-list))
	    (if result
		(setq text-list (cdr result))
	      ; else
	      (setq success nil)
	      ) ; if result
	    ) ; while pattern-list
	  ) ; progn
      ; else
      (setq success nil)
      ) ; if (= (tpl-line-indent pattern) (tpl-line-indent text))
    ; return
    success
    ) ; let
  ) ; defun tpl-match-line

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-match-pattern (pattern-list scanner-patterns)
  "Attempt to match each line in PATTERN-LIST with text after point.
    Return a list of matches.  Second argument SCANNER-PATTERNS
    specifies what type of lexical patterns to use when scanning."
					; Local Variables
  (let (success tree this-pattern next-pattern this-match first-text next-text
		start-region start-col
		this-indent next-indent)
					; Body
    (setq success t)
    (setq tree nil)
					; Initialize scanner
    (setq start-region (point))
    (setq start-col (current-column))
    (setq this-indent 0)
					; Get first "next text line"
    (back-to-indentation)
    (setq next-text (tpl-scan-line start-col scanner-patterns))
    (setq this-indent (tpl-line-indent next-text))
    (if (not (eobp))
	(forward-char)
      ) ; if
					; For each line in pattern
    (while (and pattern-list success)
      ;(debug nil "top of pattern loop")
					; Get next pattern line
      (setq this-pattern (car pattern-list))
      (setq pattern-list (cdr pattern-list))
      (if pattern-list
	  (setq next-pattern (car pattern-list))
	; else
	(setq next-pattern nil)
	) ; if pattern-list
      (setq this-match nil)
					; Get first text line
      (setq first-text next-text)
					; Try to match lines
      (if (tpl-match-line this-pattern first-text)
	  (progn
	    (setq this-match (list first-text))
	    (if next-pattern
		(progn
		  (setq next-indent (tpl-line-indent next-pattern))
					; Get next text line
		  (back-to-indentation)
		  (setq next-text (tpl-scan-line start-col scanner-patterns))
		  (setq this-indent (tpl-line-indent next-text))
		  (if (not (eobp))
		      (forward-char)
		    ) ; if
					; Append until next match
		  (while (and (not (eobp))
			      (or (> this-indent next-indent)
				  (equal (tpl-line-tokens next-text) nil)))
		    ;(debug nil "appending in middle...")
		    (setq this-match (append this-match (list next-text)))
					; Get next text line
		    (back-to-indentation)
		    (setq next-text (tpl-scan-line start-col scanner-patterns))
		    (setq this-indent (tpl-line-indent next-text))
		    (if (not (eobp))
			(forward-char)
		      ) ; if
		    ) ; while
		  ) ; progn
	      ; else
					; Append until no more indentation
	      (progn
		(while (and (not (eobp))
			    (or (> this-indent 0)
				(equal (tpl-line-tokens next-text) nil)))
		  ;(debug nil "appending at end...")
		  (setq this-match (append this-match (list next-text)))
					; Get next text line
		  (back-to-indentation)
		  (setq this-col (current-column))
		  (setq next-text (tpl-scan-line start-col scanner-patterns))
		  (setq this-indent (tpl-line-indent next-text))
		  (if (not (eobp))
		      (forward-char)
		    ) ; if
		  ) ; while
		(if (> this-indent 0)
		    (setq this-match (append this-match (list next-text)))
		  (forward-line -1)
		  ) ; if
		) ; progn
	      ) ; if next-pattern
	    (setq tree (append tree (list (list this-pattern this-match))))
	    ) ; progn
	; else
	(setq success nil)
	) ; if (tpl-match-line this-pattern first-text)
      ) ; while pattern-list
    ; Set point and mark
    (if success
	(progn
	  (setq success tree)
	  (set-mark start-region)
	  (if (eobp)
	      (end-of-line)
	    ; else
	    (end-of-line 0)
	    ) ; if
	  ) ; progn
      ; else
      (goto-char start-region)
      ) ; if success
    ; return
    success
    ) ; let
  ) ; defun tpl-match-pattern

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-match-repetition-template (template)
  "Match TEMPLATE and return t or nil."
					; Local Variables
  (let ()
					; Body
    (error
     "tpl-match-repetition-template: Cannot match repetition-type template.")
    ) ; let
  ) ; defun tpl-match-repetition-template

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-match-selection-template (template)
  "Match TEMPLATE and return tree or nil."
					; Local Variables
  (let (result selection-list selection)
					; Body
    (setq result nil)
    (setq selection-list (tpl-token-value template))
    (while (and selection-list (not result))
      (setq selection (car selection-list))
      (setq selection-list (cdr selection-list))
      (setq selection (tpl-token-value (car (tpl-line-tokens selection))))
      (setq result (tpl-match-template selection))
      ) ; while selection-list
    ; return
    result
    ) ; let
  ) ; defun tpl-match-selection-template

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-match-sequence-template (template)
  "Match TEMPLATE and return tree or nil."
					; Local Variables
  (let (pattern-list result)
					; Body
    (setq pattern-list (tpl-token-value template))
    (setq result (tpl-match-pattern pattern-list lex-patterns))
    (if result
	(setq result (tpl-make-token
		      tpl-sequence-type (tpl-token-name template) result))
      ) ; if result
    ; return
    result
    ) ; let
  ) ; defun tpl-match-sequence-template

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-match-string-template (template)
  "Match TEMPLATE and return tree or nil."
					; Local Variables
  (let (pattern-list result)
					; Body
    (setq pattern-list (tpl-token-value template))
    (setq result (tpl-match-pattern pattern-list string-patterns))
    (if result
	(setq result (tpl-make-token
		      tpl-sequence-type (tpl-token-name template) result))
      ) ; if result
    ; return
    result
    ) ; let
  ) ; defun tpl-match-string-template

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-match-template (name)
  "Match template NAME and return tree or nil."
					; Local Variables
  (let (template template-type result)
					; Body
    (setq template (tpl-find-template name))
    (setq template-type (tpl-token-type template))
    (cond
     ((equal template-type tpl-function-type)
      (setq result (tpl-match-function-template template))
      ) ; (equal template-type tpl-function-type)
     ((equal template-type tpl-lexical-type)
      (setq result (tpl-match-lexical-template template))
      ) ; (equal template-type tpl-lexical-type)
     ((equal template-type tpl-repetition-type)
      (setq result (tpl-match-repetition-template template))
      ) ; (equal template-type tpl-repetition-type)
     ((equal template-type tpl-selection-type)
      (setq result (tpl-match-selection-template template))
      ) ; (equal template-type tpl-selection-type)
     ((equal template-type tpl-sequence-type)
      (setq result (tpl-match-sequence-template template))
      ) ; (equal template-type tpl-sequence-type)
     ((equal template-type tpl-string-type)
      (setq result (tpl-match-string-template template))
      ) ; (equal template-type tpl-string-type)
     ) ; cond
    ; return
    result
    ) ; let
  ) ; defun tpl-match-template

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-match-token (token text-list)
  "Attempt to match TOKEN with tokens in TEXT-LIST.  Return the
    list (t remainder-of-TEXT-LIST) or nil."
					; Local Variables
  (let (type success)
					; Body
    (setq text-list (tpl-delete-leading-whitespace text-list))
    (setq type (tpl-token-name token))
    (cond
     ((or (equal type tpl-other-type)
	  (equal type tpl-punctuation-type)
	  (equal type tpl-string-type))
      (progn
	(if text-list
	    (progn
	      (setq success (equal (tpl-token-value token)
				   (tpl-token-value (car text-list))))
	      (setq text-list (cdr text-list))
	      ) ; progn
	  ; else
	  (setq success nil)
	  ) ; if text-list
	) ; progn
      ) ; (or (equal type tpl-other-type)...)
     ((equal type tpl-word-type)
      (progn
	(if text-list
	    (progn
	      (setq success (equal (upcase (tpl-token-value token))
				   (upcase (tpl-token-value (car text-list)))))
	      (setq text-list (cdr text-list))
	      ) ; progn
	  ; else
	  (setq success nil)
	  ) ; if text-list
	) ; progn
      ) ; (equal type tpl-word-type)
     ((equal type tpl-whitespace-type)
      (progn
	(if (and text-list
		 (equal tpl-whitespace-type (tpl-token-name (car text-list))))
	    (setq text-list (cdr text-list))
	  ) ; if
	(setq success t)
	) ; progn
      ) ; (equal type tpl-whitespace-type)
     ((or (equal type tpl-placeholder-type)
	  (equal type tpl-optional-type))
      (progn
	(setq text-list nil)
	(setq success t)
	) ; progn
      ) ; (equal type tpl-placeholder-type)
     ) ; cond
    (if success
	(setq success (cons t text-list))
      ) ; if success
    ; return
    success
    ) ; let
  ) ; defun tpl-match-token

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-parse-function (template)
  "Try to parse text at point as an instance of function-type TEMPLATE.
   Return a parse tree or nil."
					; Local Variables
  (let ()
					; Body
    (error "tpl-parse-function: Cannot parse function-type templates.")
  ) ; let
) ; defun tpl-parse-function

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-parse-instance (tpl-name)
  "Try to parse text at point as an instance of TPL-NAME.
   Return a parse tree or nil."
					; Local Variables
  (let ()
					; Body
    (setq template (tpl-find-template tpl-name))
    (setq template-type (tpl-token-type template))
    (cond
      ((equal template-type tpl-function-type)
	(setq result (tpl-parse-function template))
      ) ; (equal template-type tpl-function-type)
      ((equal template-type tpl-lexical-type)
	(setq result (tpl-parse-lexical template))
      ) ; (equal template-type tpl-lexical-type)
      ((equal template-type tpl-repetition-type)
	(setq result (tpl-parse-repetition template))
      ) ; (equal template-type tpl-repetition-type)
      ((equal template-type tpl-selection-type)
	(setq result (tpl-parse-selection template))
      ) ; (equal template-type tpl-selection-type)
      ((equal template-type tpl-sequence-type)
	(setq result (tpl-parse-sequence template))
      ) ; (equal template-type tpl-sequence-type)
      ((equal template-type tpl-string-type)
	(setq result (tpl-parse-string template))
      ) ; (equal template-type tpl-string-type)
    ) ; cond
    result				; return
  ) ; let
) ; defun tpl-parse-instance

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-parse-lexical (template)
  "Try to parse text at point as an instance of lexical-type TEMPLATE.
   Return a parse tree or nil."
					; Local Variables
  (let ()
					; Body
    (error "tpl-parse-lexical: Cannot parse lexical-type templates.")
  ) ; let
) ; defun tpl-parse-lexical

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-parse-pattern (pattern tpl-name start-col scanner-patterns)
  "Try to parse text at point as an instance of PATTERN within
   template TPL-NAME.  START-COL specifies the starting column of
   the template.  SCANNER-PATTERNS specifies which lexical patterns
   to use when scanning.  Return a token or nil."
					; Local Variables
  (let (type result start stop this-col indent-level)
					; Body
    (setq type (tpl-token-name pattern))
    (cond
      ((equal type tpl-indentation-type)
	(progn
	  (setq result pattern)
	  ) ; progn
      ) ; (equal type tpl-indentation-type)
      ((equal type tpl-newline-type)
	(progn
	  (setq result pattern)
	  ) ; progn
      ) ; (equal type tpl-newline-type)
      ((equal type tpl-other-type)
	(progn
	  (tpl-skip-over-whitespace)
	  (if (looking-at (tpl-token-value pattern))
	      (setq result (tpl-scan-token scanner-patterns))
	    (setq result nil)
	    ) ; if
	  ) ; progn
      ) ; (equal type tpl-other-type)
      ((equal type tpl-placeholder-type)
	(progn
	  (tpl-skip-over-whitespace)
	  (setq start (point))
	  (setq stop (tpl-get-placeholder-end (tpl-token-value pattern)
					      tpl-name))
	  (setq result nil)
	  (goto-char start)
	  (while (< (point) stop)
	    (if (eolp)
					; This code duplicates some of
					;   "tpl-scan-line"
		(progn
		  (setq result
			(append result (list tpl-newline-token)))
		  (forward-line 1)
		  (back-to-indentation)
		  (setq this-col (current-column))
		  (cond
		   ((>= this-col comment-column)
		    (progn
		      (setq indent-level tpl-comment-level)
		      ) ; progn
		    ) ; comment
		   ((<= this-col start-col)
		    (progn
		      (setq indent-level 0)
		      ) ; progn
		    ) ; too small
		   (t
		    (progn
		      (setq indent-level (- this-col start-col))
		      ) ; progn
		    ) ; t
		   ) ; cond
		  (setq result
			(append result (list (tpl-make-token
					      tpl-terminal-type
					      tpl-indentation-type
					      indent-level))))
		  ) ; progn
	      ; else
	      (progn
		(setq result
		      (append result (list (tpl-scan-token scanner-patterns))))
		) ; progn
	      ) ; if
	    ) ; while
	  (setq result (tpl-make-token tpl-placeholder-type
				   (tpl-token-value pattern)
				   result))
	  ) ; progn
      ) ; (equal type tpl-placeholder-type)
      ((equal type tpl-punctuation-type)
	(progn
	  (tpl-skip-over-whitespace)
	  (if (looking-at (tpl-token-value pattern))
	      (setq result (tpl-scan-token scanner-patterns))
	    (setq result nil)
	    ) ; if
	  ) ; progn
      ) ; (equal type tpl-punctuation-type)
      ((equal type tpl-string-type)
	(progn
	  (tpl-skip-over-whitespace)
	  (if (looking-at (tpl-token-value pattern))
	      (setq result (tpl-scan-token scanner-patterns))
	    (setq result nil)
	    ) ; if
	  ) ; progn
      ) ; (equal type tpl-string-type)
      ((equal type tpl-whitespace-type)
	(progn
	  (setq result pattern)
	  ) ; progn
      ) ; (equal type tpl-whitespace-type)
      ((equal type tpl-word-type)
	(progn
	  (tpl-skip-over-whitespace)
	  (if (looking-at (tpl-token-value pattern))
	      (setq result (tpl-scan-token scanner-patterns))
	    (setq result nil)
	    ) ; if
	  ) ; progn
      ) ; (equal type tpl-word-type)
    ) ; cond
    result				; return
  ) ; let
) ; defun tpl-parse-pattern

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-parse-placeholder (string)
  "Parse STRING as a placeholder and return token."
					; Local Variables
  (let (token)
					; Body
    (get-buffer-create "*Work*")
    (save-window-excursion
      (set-buffer "*Work*")
      (erase-buffer)
      (insert string)
      (beginning-of-line)
      (setq token (tpl-scan-placeholder))
      ) ; save-window-excursion
    ; return
    token
    ) ; let
  ) ; defun tpl-parse-placeholder

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-parse-repetition (template)
  "Try to parse text at point as an instance of repetition-type TEMPLATE.
   Return a parse tree or nil."
					; Local Variables
  (let ()
					; Body
    (error "tpl-parse-repetition: Cannot parse repetition-type templates.")
  ) ; let
) ; defun tpl-parse-repetition

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-parse-selection (template)
  "Try to parse text at point as an instance of selection-type TEMPLATE.
   Return a parse tree or nil."
					; Local Variables
  (let ()
					; Body
    (error "tpl-parse-selection: Cannot parse selection-type templates.")
  ) ; let
) ; defun tpl-parse-selection

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-parse-sequence (template)
  "Try to parse text at point as an instance of sequence-type TEMPLATE.
   Return a parse tree or nil."
					; Local Variables
  (let (tpl-name pattern-list pattern result success match start-col)
					; Body
    (setq tpl-name (tpl-token-name template))
    (setq pattern-list (tpl-token-value (tpl-line-to-token template)))
    (setq start-col (current-column))
    (setq result nil)
    (setq success t)
    (while (and success pattern-list)
      (setq pattern (car pattern-list))
      (setq pattern-list (cdr pattern-list))
      (setq match (tpl-parse-pattern pattern tpl-name start-col lex-patterns))
      (if match
	  (setq result (append result (list match)))
	; else
	(setq success nil)
	) ; if match
      ) ; while
    (if success
	result				; return
      ; else
      nil				; return
      ) ; if success
  ) ; let
) ; defun tpl-parse-sequence

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-parse-string (template)
  "Try to parse text at point as an instance of string-type TEMPLATE.
   Return a parse tree or nil."
					; Local Variables
  (let (tpl-name pattern-list pattern result success match start-col)
					; Body
    (setq tpl-name (tpl-token-name template))
    (setq pattern-list (tpl-token-value (tpl-line-to-token template)))
    (setq start-col (current-column))
    (setq result nil)
    (setq success t)
    (while (and success pattern-list)
      (setq pattern (car pattern-list))
      (setq pattern-list (cdr pattern-list))
      (setq match (tpl-parse-pattern
		   pattern tpl-name start-col string-patterns))
      (if match
	  (setq result (append result (list match)))
	; else
	(setq success nil)
	) ; if match
      ) ; while
    (if success
	result				; return
      ; else
      nil				; return
      ) ; if success
  ) ; let
) ; defun tpl-parse-string

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-query-replace (from to)
  "Replace some instances after point matching FROM template with
    corresponding instances of TO.  As each match is found, the user
    must type a character saying what to do with it.  For directions,
    type \\[help-command] at that time."
					; Local Variables
  (let ()
					; Body
    (perform-replace-tpl from to t nil nil
			 'tpl-search-forward
			 'exchange-point-and-mark 'tpl-replace)
    ) ; let
  ) ; defun tpl-query-replace

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-replace (from to)
  "Replace the instance of template FROM with a corresponding instance
    of template TO."
					; Local Variables
  (let (token-tree new start)
					; Body
    (setq start (point))
    (message (concat "replace-tpl: Trying to match \"" from "\"..."))
    (setq token-tree (tpl-parse-instance from))
    ;(debug nil "token-tree" token-tree)
    (message (concat "replace-tpl: Trying to construct \"" to "\"..."))
    (setq new (tpl-token-to-line (tpl-replace-placeholders to token-tree)))
    ;(debug nil "new tree" new)
    (delete-region start (point))
    (setq start (point))
    (tpl-unscan new)
    (set-mark start)
    (message "replace-tpl: Done.")
    ) ; let
  ) ; defun tpl-replace

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-replace-placeholders (name tree)
  "Replace placeholders in template NAME using values from TREE."
					; Local Variables
  (let (result template token-list token token-type current-indent match)
					; Body
    (setq result nil)
    (setq template (tpl-find-template name))
    (if (not (or
	      (equal tpl-sequence-type (tpl-token-type template))
	      (equal tpl-string-type (tpl-token-type template))))
	(error (concat "tpl-replace-placeholders: "
		       "Target template must be SEQUENCE or STRING type"))
      ) ; if
    (setq token-list (tpl-token-value (tpl-line-to-token template)))
    (while token-list
      (setq token (car token-list))
      (setq token-list (cdr token-list))
      (setq token-type (tpl-token-name token))
      ;(debug nil "token-type" token-type)
      (if (or (equal tpl-placeholder-type token-type)
	      (equal tpl-optional-type token-type))
	  (progn
	    (setq match (tpl-get-match token tree current-indent))
	    (if match
		(setq result (append result match))
	      ; else
	      (setq result (append result (list token)))
	      ) ; if match
	    ) ; progn
	; else
	(progn
	  (if (equal tpl-indentation-type token-type)
	      (setq current-indent (tpl-token-value token))
	    ) ; if (equal tpl-indentation-type token-type)
	  (setq result (append result (list token)))
	  ) ; progn
	) ; if (equal tpl-placeholder-type token-type)
      ) ; while token-list
    (setq result (tpl-make-token t t result))
    ; return
    result
    ) ; let
  ) ; defun tpl-replace-placeholders

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-search-forward (template &optional bound forgiving count)
  "Search forward from point for TEMPLATE (a name).
    An optional second argument bounds the search; it is a buffer
    position.  The match found must not extend beyond that position.
    Optional third argument, if t, means if fail just return nil
    (no error).  If not nil and not t, move to limit of search and
    return nil.  Optional fourth argument is repeat count."
					; Local Variables
  (let (leading found occur gaveup start trial)
					; Body
    (setq start (point))
    (if (not bound)
	(setq bound (point-max))
      )
    (if (not count)
	(setq count 1)
      )
    (setq occur 0)
    (setq leading (tpl-leading-text template))
    (if leading
	(progn
	  (setq found nil)
	  (setq gaveup nil)
	  (while (and (not found) (not gaveup))
	    (if (search-forward leading bound t)
		(progn
		  (search-backward leading)
		  (setq trial (point))
		  (setq found (tpl-looking-at template))
		  (if (and found
			   (<= (point) bound))
		      (progn
			(setq occur (1+ occur))
			(if (< occur count)
			    (setq found nil)
			  )
			) ; progn
		    ; else
		    (if found
			(setq gaveup t)	; Out of bounds---no more
		      ; else
		      (progn		; Failed this time---try again
			(goto-char trial)
			(forward-line 1) 
			) ; progn
		      ) ; if found
		    ) ; if (and found...)
		  ) ; progn
	      ; else
	      (setq gaveup t)
	      ) ; if (search-forward...)
	    ) ; while
	  ) ; progn
      ; else
      (error "Cannot search for templates that start with a placeholder.")
      ) ; if leading
    (if (or gaveup (not found))
	(if (not forgiving)
	    (progn
	      (goto-char bound)
	      (error "Could not find template.")
	      ) ; progn
	  ; else
	  (if (eq forgiving t)
	      (progn
		(goto-char start)
		) ; progn
	    ; else
	    (progn
	      (goto-char bound)
	      ) ; progn
	    ) ; if (eq forgiving t)
	  ) ; if (not forgiving)
      ) ; if (not found)
    (if gaveup
	(setq found nil)
      ) ; if gaveup
    ; return
    found
    ) ; let
  ) ; defun tpl-search-forward

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-skip-over-whitespace ()
  "Advance point past newlines and whitespace."
					; Local Variables
  (let (moving)
					; Body
    (setq moving t)
    (while (and moving (not (eobp)))
      (setq moving nil)
      (if (eolp)
	  (progn
	    (setq moving t)
	    (forward-line 1)
	    ) ; progn
	) ; if
      (if (looking-at tpl-pattern-whitespace)
	  (progn
	    (setq moving t)
	    (re-search-forward tpl-pattern-whitespace)
	    ) ; progn
	) ; if
      ) ; while
  ) ; let
) ; defun tpl-skip-over-whitespace

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun tpl-token-to-line (tree)
  "Convert TREE from token-format to line-format."
					; Local Variables
  (let (result line token-list token type name token-type save-indent)
					; Body
    (setq result nil)
    (setq line nil)
    (setq type (tpl-token-type tree))
    (setq name (tpl-token-name tree))
    (setq token-list (tpl-token-value tree))
    (while token-list
      (setq token (car token-list))
      (setq token-list (cdr token-list))
      (setq token-type (tpl-token-name token))
      (cond
       ((equal token-type tpl-indentation-type)
	(progn
	  (setq save-indent (tpl-token-value token))
	  ) ; progn
	) ; tpl-indentation-type
       ((equal token-type tpl-newline-type)
	(progn
	  (setq result (append result (list (tpl-make-line save-indent line))))
	  (setq line nil)
	  ) ; progn
	) ; tpl-newline-type
       (t
	(progn
	  (setq line (append line (list token)))
	  ) ; progn
	) ; t
       ) ; cond
      ) ; while token-list
    (setq result (append result (list (tpl-make-line save-indent line))))
    (setq result (tpl-make-token type name result))
    ; return
    result
    ) ; let
  ) ; defun tpl-token-to-line

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;;; end of tplparse.el
