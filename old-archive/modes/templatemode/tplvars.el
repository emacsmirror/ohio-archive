;;; tplvars.el -- Variables for template-mode.
;;; Copyright (C) 1987 Mark A. Ardis.

(provide 'tplvars)

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; User Options

(defvar tpl-ask-expansion-depth 1
  "*Depth of recursive placeholder expansions at which to start asking
    whether to expand.  Possible values are:
      -1  delete all placeholders
	0  don't expand or delete placeholders---leave them alone
	1  ask about expansion at level 1
	n  ask about expansion at level n of expanding
      Defaults to 1, which means always ask."
) ; tpl-ask-expansion-depth

(defvar tpl-auto-load-new-templates nil
  "*If non-nil load new templates after creating them with
    'replace-wtih-placeholder.  Otherwise, loading must be done
    by invoking 'load-tpl-file.  Defaults to nil."
) ; tpl-auto-load-new-templates

(defvar tpl-auto-save-new-templates nil
  "*If non-nil save new templates after creating them with
    'replace-wtih-placeholder.  Otherwise, saving must be done
    by invoking 'save-buffer.  Defaults to nil."
) ; tpl-auto-save-new-templates

(defvar tpl-auto-template-alist nil
  "*Global Alist of major modes and their associated template files.
    Initialized by 'tpl-initialize-modes'."
) ; tpl-auto-template-alist

(defvar tpl-begin-placeholder "<"
  "*Regular expression for beginning of placeholder."
) ; tpl-begin-placeholder

(defvar tpl-begin-template-body "^:begin"
  "*Regular expression for beginning of template body."
) ; tpl-begin-template-body
(make-variable-buffer-local 'tpl-begin-template-body)
(setq-default tpl-begin-template-body "^:begin")

(defvar tpl-begin-template-definition "^Template"
  "*Regular expression for beginning of template definition."
) ; tpl-begin-template-definition
(make-variable-buffer-local 'tpl-begin-template-definition)
(setq-default tpl-begin-template-definition "^Template")

(defvar tpl-destination-symbol "POINT"
  "*Special symbol used as placeholder as location for point
    after expanding a template."
) ; tpl-destination-symbol

(defvar tpl-display-begin ">>"
  "*Delimiter marking beginning of a selected placeholder."
) ; tpl-display-begin

(defvar tpl-display-end "<<"
  "*Delimiter marking end of a selected placeholder."
) ; tpl-display-end

(defvar tpl-end-placeholder ">"
  "*Regular expression for end of placeholder."
) ; tpl-end-placeholder

(defvar tpl-end-template-body "^:end"
  "*Regular expression for end of template body."
) ; tpl-end-template-body
(make-variable-buffer-local 'tpl-end-template-body)
(setq-default tpl-end-template-body "^:end")

(defvar tpl-fill-while-unscanning nil
  "*If non-nil, use whatever fill mode is in effect while unscanning
    (inserting) templates.  Defaults to nil, which ensures that template
    formats are not disturbed by context."
) ; tpl-fill-while-unscanning

(defvar tpl-form-placeholder-name-from-context nil
  "*Option to generate placeholder names by looking for the first symbol
    after point.  Defaults to nil, which means use temporary names instead."
) ; tpl-form-placeholder-name-from-context

(defvar tpl-function-type "Function"
  "*Name of function-type template type."
) ; tpl-function-type

(defvar tpl-get-placeholder-name-in-context t
  "*If non-nil allow the user to type in the placeholder name within
    the context of the template definition.  Otherwise, use temporary
    names.  Defaults to t."
) ; tpl-get-placeholder-name-in-context

(defvar tpl-include-prefix-in-groups t
  "*Option to include the prefix string (used to find the beginning of
    a group) in the group."
) ; tpl-include-prefix-in-groups

(defvar tpl-indentation-size 2
  "*Size of indentation units in columns."
) ; tpl-indentation-size
(make-variable-buffer-local 'tpl-indentation-size)
(setq-default tpl-indentation-size 2)

(defvar tpl-keep-optional-placeholders "ask"
  "*Option to determine processing of optional placeholders in template-mode.
    If t, then always keep them.  If nil, then always delete them.  If neither
    t nor nil, then always ask."
) ; tpl-keep-optional-placeholders

(defvar tpl-lexical-type "Lexical"
  "*Name of lexical-type template type."
) ; tpl-lexical-type

(defvar tpl-literal-whitespace nil
  "*If non-nil leave leading whitespace in templates as-is.
    Otherwise, calculate relative indentation units (see
    'tpl-indentation-size' variable).  Defaults to nil."
) ; tpl-literal-whitespace

(defvar tpl-load-path (list nil "/faculty/ardis/Gnu/Template/Templates")
  "*List of directories to search for template files to load.
    Each element is a string (directory name) or nil (try default directory).
    Use 'template-mode-load-hook to change this value."
) ; tpl-load-path

(defvar tpl-new-template-buffer "new.tpl"
  "*Buffer containing new templates."
) ; tpl-new-template-buffer

(defvar tpl-next-placeholder-number 1
  "*Counter used to generate unique temporary placeholder names."
) ; tpl-next-placeholder-number

(defvar tpl-pattern-optional "#"
  "*Regular expression for all optional placeholders."
) ; tpl-pattern-optional

(defvar tpl-pattern-other "."
  "*Regular expression for all other tokens."
) ; tpl-pattern-other

(defvar tpl-pattern-placeholder nil
  "*Regular expression for placeholder."
) ; tpl-pattern-placeholder

(defvar tpl-pattern-punctuation "\\s.+"
  "*Regular expression for at least one punctuation character."
) ; tpl-pattern-punctuation

(defvar tpl-pattern-string ".*"
  "*Regular expression for any string."
) ; tpl-pattern-string

(defvar tpl-pattern-symbol "\\(\\sw\\|\\s_\\)+"
  "*Regular expression for at least one symbol character."
) ; tpl-pattern-symbol

(defvar tpl-pattern-whitespace "[ 	]+"
  "*Regular expression for at least one whitespace character."
) ; tpl-pattern-whitespace

(defvar tpl-pattern-word "\\sw+"
  "*Regular expression for at least one word character."
) ; tpl-pattern-word

(defvar tpl-rebuild-all-templates-template nil
  "*If non-nil rebuild the list of all templates after invoking
    template-mode and after loading new templates.  Otherwise, do not
    (improves performance of starting up).  Defaults to nil."
) ; tpl-rebuild-all-templates-template

(defvar tpl-repetition-type "Repetition"
  "*Name of repetition-type template type."
) ; tpl-repetition-type

(defvar tpl-save-identifier-file nil
  "*Option to save identifier table in a separate file.  Defaults
    to nil, which means save only in a buffer."
) ; tpl-save-identifier-file

(defvar tpl-selection-type "Selection"
  "*Name of selection-type template type."
) ; tpl-selection-type

(defvar tpl-sep-placeholder ":"
  "*Regular expression for placeholder body separator."
) ; tpl-sep-placeholder

(defvar tpl-sequence-type "Sequence"
  "*Name of sequence-type template type."
) ; tpl-sequence-type

(defvar tpl-string-type "String"
  "*Name of string-type template type."
) ; tpl-string-type

(defvar tpl-temporary-placeholder-name "TEMP"
  "*Root of temporary placeholder names."
) ; tpl-temporary-placeholder-name

(defvar tpl-verify-end-of-group nil
  "*Option to verify (by positioning point) the end of each group
    of lines in 'query-replace-groups."
) ; tpl-verify-end-of-group

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; Global Variables

(defvar lex-patterns nil
  "A list of regular expressions to be used in recognizing tokens."
) ; lex-patterns

(defvar string-patterns nil
  "A list of string patterns to be used in recognizing tokens."
) ; string-patterns

(defvar template-mode nil
  "Minor mode symbol."
) ; template-mode
(make-variable-buffer-local 'template-mode)

(defvar template-mode-map nil
  "Keymap for template-mode."
) ; template-mode-map

(defvar tpl-all-templates-file "ALLTEMPLATES"
  "Name of dummy template file for all-templates-template."
) ; tpl-all-templates-name

(defvar tpl-all-templates-name "ALLTEMPLATES"
  "Name of special template that is a selection of all other templates."
) ; tpl-all-templates-name

(defvar tpl-all-templates-template-invalid t
  "Flag to indicate validity of all-templates-template."
) ; tpl-all-templates-template-invalid

(defvar tpl-begin-optional nil
  "Regular expression for beginning of optional placeholder."
) ; tpl-begin-optional

(defvar tpl-buffer
  "Current template buffer."
) ; tpl-buffer

(defvar tpl-comment-level 100
  "Special value indicating alignment on comment column."
) ; tpl-comment-level

(defvar tpl-destination-marker (make-marker)
  "Location (a marker) to leave point after expanding placeholder."
) ; tpl-destination-marker
(make-variable-buffer-local 'tpl-destination-marker)
(setq-default tpl-destination-marker (make-marker))

(defvar tpl-destination-needed nil
  "Boolean flag used to signal whether a destination for point (after
    expanding a placeholder) has been found yet."
) ; tpl-destination-needed

(defvar tpl-destination-placeholder nil
  "Special placeholder used to place point after expanding a template."
) ; tpl-destination-placeholder

(defvar tpl-end-group nil
  "Global variable to hold pattern for end of group."
) ; tpl-end-group

(defvar tpl-expansion-depth 1
  "Current depth of recursive calls to expand placeholders.
    Compared to tpl-ask-expansion-depth."
) ; tpl-expansion-depth
(make-variable-buffer-local 'tpl-expansion-depth)
(setq-default tpl-expansion-depth 1)

(defvar tpl-formed-placeholder-name nil
  "Value formed by searching for next symbol after point."
) ; tpl-formed-placeholder-name

(defvar tpl-global-template-list nil
  "Global Alist of major modes and their associated templates."
) ; tpl-global-template-list

(defvar tpl-indentation-type 'indentation
  "Type of indentation terminals."
) ; tpl-indentation-type

(defvar tpl-local-template-list nil
  "List of all templates and their tree values."
) ; tpl-local-template-list
(make-variable-buffer-local 'tpl-local-template-list)

(defvar tpl-menu-buffer "Menu"
  "Buffer used for making selections of templates."
) ; tpl-menu-buffer

(defvar tpl-newline-token nil
  "Token indicating presence of a newline."
) ; tpl-newline-token

(defvar tpl-newline-type 'newline
  "Type of newline terminals."
) ; tpl-newline-type

(defvar tpl-next-placeholder-name (concat tpl-temporary-placeholder-name
					tpl-next-placeholder-number)
  "Next unique name for temporary placeholder."
) ; tpl-next-placeholder-number

(defvar tpl-optional-type 'optional
  "Type of optional placeholders."
) ; tpl-optional-type

(defvar tpl-other-type 'other
  "Type of other terminals."
) ; tpl-other-type

(defvar tpl-placeholder-type 'placeholder
  "Type of all placeholders."
) ; tpl-placeholder-type

(defvar tpl-punctuation-type 'punctuation
  "Type of punctuation terminals."
) ; tpl-punctuation-type

(defvar tpl-query-flag t
  "If non-nil query about placeholder names."
) ; tpl-query-flag

(defvar tpl-saved-map nil
  "Local keymap to restore when turning off template-mode."
) ; tpl-saved-map
(make-variable-buffer-local 'tpl-saved-map)

(defvar tpl-terminal-type 'terminal
  "Type of all literal strings."
) ; tpl-terminal-type

(defvar tpl-textlong-buffer "Textlong"
  "Buffer used for creating textlong values."
) ; tpl-textlong-buffer

(defvar tpl-whitespace-type 'whitespace
  "Type of whitespace terminals."
) ; tpl-whitespace-type

(defvar tpl-word-type 'word
  "Type of word terminals."
) ; tpl-word-type

(defvar tpl-work-buffer "Work"
  "Buffer used for constructing temporary objects."
) ; tpl-work-buffer

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;;; end of tplvars.el
