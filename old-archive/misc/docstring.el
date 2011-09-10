;;; docstring.el
;;; Michael D. Ernst <mernst@theory.lcs.mit.edu>, August 1991

;; LCD Archive Entry:
;; docstring|Michael D. Ernst|mernst@theory.lcs.mit.edu|
;; Keep documentation consistent with Lisp code by substituting doc strings.|
;; 02-20-1993|1.2|~/misc/docstring.el.Z|


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Overview
;;;

;;; This code makes it easy to keep the documentation of Emacs Lisp code
;;; consistent with the code's documentation strings and key bindings, and
;;; to arrange that functions and variables are properly indexed and
;;; formatted.

;;; It works by replacing special placeholders in your documentation, which
;;; are surrounded by <<<triple brockets>>>, by text such as documentation
;;; strings, key bindings, table preambles and postambles, and the results
;;; of evaluating arbitrary Emacs Lisp expressions.  It can produce output
;;; for use in texinfo documents or in ASCII documentation files; set
;;; variable  ds-tex-output-p  to nil to get ASCII output.

;;; It is worth repeating the GNU Emacs Lisp Manual's advice about the
;;; difference between a manual and documentation strings:
;;;     A collection of documentation strings is not sufficient as a manual
;;;     because a good manual is not organized in that fashion; it is
;;;     organized in terms of topics of discussion.

;;; However, no manual is complete without the documentation of the
;;; package's commands and variables, which complements the rest of the
;;; manual; docstring-substitute makes such tables easier to create and
;;; maintain.  It has been my experience that writing documentation strings
;;; to be intelligible even in a manual is a good exercise which results in
;;; better documentation strings and better manuals.

;;; To run this, your code must be loaded (its keymaps and commands must be
;;; defined).


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commands
;;;

;;; The top-level command is `docstring-substitute', which replaces the
;;; placeholder forms after point in the current buffer with the
;;; appropriate text:

;; <<<key:  keystroke>>>
;; <<<command:  command>>>
;;   Either of these is replaced by "key (command) description", where `key'
;;   is the printed representation of the keystroke, `command' is the name of
;;   the command, and `description' is the documentation string for
;;   command.  In `description', formal parameters are set in slanted type,
;;   and function and global variable names in typewriter font; functions
;;   and global variables are placed in the document's function and varible
;;   indices, respectively.
;; <<<variable:  variablename>>>
;;   This is replaced by "variablename documentation".

;;; The above are appropriate for use in a table, and so must be surrounded
;;; by a table preamble and postamble:

;; <<<table:start>>> or <<<table:begin>>>
;;   Preamble for a table of characters or M-x commands.
;;   Appropriate for a table of commands or keys.
;; <<<table:cstart>>> or <<<table:cbegin>>>
;;   Preamble for a table of variable or function names.
;; <<<table:end>>>
;;   Postamble for the above two types of table.
;; <<<table:fstart>>> or <<<table:fbegin>>>
;;   Preamble for a table of function names; table items are automatically
;;   inserted into the document's function index.
;; <<<table:fend>>>
;;   Postamble for a function table.

;;; There are shortcuts for the above when a table only contains commands,
;;; only keys, or only variables.  Any number of items may follow the
;;; colon, but there should be no newlines between the <<< and >>>.

;; <<<commandtable:  command, command, command>>>
;; <<<keytable:  key key key>>>
;; <<<variabletable:  var, var>>>

;;; Key bindings (which appear in place of <<<command: >>> or <<<key: >>>)
;;; are looked up in the current local keymap.  To change it, use:

;; <<<map:  form>>>
;;   This is removed from the buffer and replaced by the empty string; it
;;   is executed for side effect.  The local keymap is set to the result of
;;   evaluating form (which is probably a variable name).  The original
;;   local keymap is replaced when the call to `docstring-substitute'
;;   returns.

;;; Finally, there is a way to insert arbitrary text in the documentation.

;; <<<value:  form>>>
;;   This is replaced by the result of evaluating form.  This is a good way
;;   to insert a version number, today's date, etc.  You can also use it
;;   for side effect, as in the following:
;;       <<<value: (progn (require 'database) (load-database) "")>>>



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Example
;;;

;;; The text

;; The usual save-file and write-file keystrokes are rebound in all
;; database modes.
;; 
;; <<<map:  database-view-mode-map>>>
;; <<<commandtable:  db-save-database, db-write-database-file>>>

;;; becomes, after docstring-substitute is run,

;; The usual save-file and write-file keystrokes are rebound in all
;; database modes.
;; 
;; @table @kbd
;; @item C-x C-s
;; @findex db-save-database
;; (@code{db-save-database})  Save the database to disk in the default save file.
;; Any changes to the current record are processed first.
;; The default save file is the file it was last saved to or read from.
;; 
;; @item C-x C-w
;; @findex db-write-database-file
;; (@code{db-write-database-file})  Save the database to disk in file @var{filename}, which becomes the default save file.
;; Any changes to the current record are processed first.
;; @end table


;;; I typically edit an unsubstituted documentation file (named, for
;;; instance, database.texi-unsub); when I want to create a new texinfo
;;; file, I execute `dostring-substitute' in its buffer and save the
;;; resulting buffer into the file database.texi.  I keep that
;;; database.texi write-protected so that I don't accidentally edit it, but
;;; override the protection when I'm saving a substituted version.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Stylistic concerns
;;;

;;; The documentation strings should be written in a stylized format to
;;; make recognition of formal parameters, functions, and global variables
;;; easier, and to prevent misrecognition of common words such as "insert"
;;; or "list".  This style appears to be a GNU standard for Emacs Lisp code.
;;;  * capitalize formal parameter names, that is, argument names
;;;  * suround function names (in documentation) with single quotes `'
;;;  * surround global variable names with extra whitespace
;;; The self-evaluating constants nil and t will be recognized even
;;; if not surrounded by extra whitespace.

;;; Here are the definitions of the functions in the example above.
;; (defun db-save-database ()
;;   "Save the database to disk in the default save file.
;; Any changes to the current record are processed first.
;; The default save file is the file it was last saved to or read from."
;;   ... )
;; (defun db-write-database-file (filename)
;;   "Save the database to disk in file FILENAME, which becomes the default save file.
;; Any changes to the current record are processed first."
;;   ... )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; To do
;;;

;;; Add lists of variables and functions not to index, even if they're
;;; marked up; for instance, `equal'.

;;; Have option to put the original <<<...>>> in a comment instead of
;;; completely replacing it, so it's easy to see what happened.

;;; Make push-mark in perform-replace stop saying "Mark set" all the time.

;;; Make this more like substitute-command-keys.  I don't think I can use
;;; it directly, though.  Could make the syntax more like it (with tripled
;;; backslash or doubled [{<), if I felt like it.  I don't just now.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Docstring-substitute
;;;


(defvar ds-texinfo-output-p t
  "Nil for ASCII substitution, t for texinfo.")


;; Maybe complain if didn't read the whole string in read-from-string.
(defun docstring-substitute ()
  "Substitute variable or function documentation into a buffer from point forward."
  (interactive)
  (let ((old-map (current-local-map))
	command form obj-index obj docstring-errors)
    (unwind-protect
	(while (re-search-forward "<<<\\([a-z]+\\):[ \t]*\\(.*\\)>>>" nil t)
	  (setq command (ds-match-string 1)
		form (ds-match-string 2)
		obj-index (and (not (string-equal form ""))
			       (read-from-string form))
		obj (car obj-index))
	  (cond ((string-equal command "map")
		 (let ((eval-obj (eval obj)))
		   (if (keymapp eval-obj)
		       (progn
			 (replace-match "")
			 (use-local-map eval-obj)
			 (if (looking-at "^$")
			     (delete-char 1)))
		     (message "`%s' (%s) isn't a keymap." obj eval-obj))))
		((string-equal command "table")
		 (replace-match (if ds-texinfo-output-p
				    (cond ((or (eq obj 'start) (eq obj 'begin))
					   "@table @kbd")
					  ((or (eq obj 'cstart) (eq obj 'cbegin))
					   "@table @code")
					  ((or (eq obj 'fstart) (eq obj 'fbegin))
					   "@ftable @code")
					  ((eq obj 'end)
					   "@end table")
					  ((eq obj 'fend)
					   "@end ftable")
					  (t
					   ;; no change
					   (message "Bad `table' operative %s." obj)
					   (ds-match-string 0)))
				  "")))
		((string-equal command "command")
		 ;; this will err if there's trouble 
		 (if (and (fboundp obj) (symbol-function obj))
		     (progn
		       (replace-match "")
		       (ds-insert-keys-command (where-is-internal
					          obj (current-local-map))
					       obj))
		   (message "`%s' (%s) isn't a command." form obj)))
		((string-equal command "key")
		 (let* ((key (car (read-from-string (concat "\"" form "\""))))
			(defn (key-binding key)))
		   (if (or (null defn) (integerp defn))
		       (message "%s is undefined" (key-description key))
		     (progn
		       (replace-match "")
		       (ds-insert-keys-command (list key) defn)))))
		((string-equal command "variable")
		 (let* ((var (car (read-from-string form))))
		   (if (boundp var)
		       (let ((doc-prop (documentation-property
					var 'variable-documentation)))
			 (if (and doc-prop (not (string-equal doc-prop ""))
				  (char-equal ?* (aref doc-prop 0)))
			     (setq doc-prop (substring doc-prop 1)))
			 ;; This appears to assume texinfo output
			 (replace-match (format "@item %s\n@vindex %s\n%s\n"
						var
						var
						(or (ds-massage-documentation
						     doc-prop)
						    "Not documented."))
					t t)
			 (if (looking-at "\n<<<table:end>>>")
			     (delete-backward-char 1)))
		     (message "%s is undefined." var))))
		((string-equal command "commandtable")
		 ;; ftable is not appropriate here because the table items
		 ;; are still keys.  I do add a function index entry for
		 ;; the command.
		 (narrow-to-region (match-beginning 0) (match-end 0))
		 (replace-match (concat
				 "<<<table:start>>>\n<<<command: "
				 form
				 ">>>\n<<<table:end>>>"))
		 (goto-char (point-min))
		 (ds-replace-regexp-quietly ",[ \t]+" ">>>\n\n<<<command: ")
		 (goto-char (point-min))
		 (widen))
		((string-equal command "keytable")
		 (narrow-to-region (match-beginning 0) (match-end 0))
		 ;; PROBLEM!  \ getting lost here.  Can't use replace-match.
		 (goto-char (point-min))
		 (ds-replace-regexp-quietly "<<<keytable:[ \t]*" "<<<table:start ")
		 ;; extra newline between table start and first key.  Oh well.
		 (goto-char (point-min))
		 (ds-replace-regexp-quietly " " ">>>\n\n<<<key:")
		 (goto-char (point-max))
		 (insert "\n<<<table:end>>>")
		 (goto-char (point-min))
		 (widen))
		((string-equal command "variabletable")
		 (narrow-to-region (match-beginning 0) (match-end 0))
		 (replace-match (concat
				 "<<<table:cstart>>>\n<<<variable: "
				 form
				 ">>>\n<<<table:end>>>"))
		 (goto-char (point-min))
		 (ds-replace-regexp-quietly ",[ \t]*" ">>>\n\n<<<variable: ")
		 (goto-char (point-min))
		 (widen))
		((string-equal command "value")
		 (let ((match (match-data))
		       (value (eval (car (read-from-string form)))))
		   (store-match-data match)
		   (replace-match (format "%s" value))))
		(t
		 (message "Unrecognized command `%s' found with form `%s'."
			  command form))
		))
      (progn
	(use-local-map old-map)
	(if (and buffer-file-name (not (string-match "-sub$" buffer-file-name)))
	    (setq buffer-file-name (concat buffer-file-name "-sub")))))))

(defun ds-insert-keys-command (keys command)
  (if ds-texinfo-output-p
      (progn
	(insert "@item ")
	(if keys
	    (insert (mapconcat (function ds-key-description)
			       keys
			       "\n@itemx ")
		    (format "\n@findex %s" command)
		    (format "\n(@code{%s})  " command))
	  (insert (format "%s%s\n" (if (commandp command) "M-x " "") command)
		  (format "@findex %s\n" command)))
	(insert (if (documentation command)
		    (ds-massage-documentation (documentation command)
					      (ds-command-arguments command))
		  "Not documented.")))
    (progn
      (if keys
	  (insert (mapconcat (function ds-key-description)
			     keys
			     "\n")
		  (format "\t(%s)" command))
	(insert (format "M-x %s" command)))
      (insert "\n")
      (insert (or (documentation command)
		  "not documented")))))

(defun ds-command-arguments (command)
  (let ((sym-func (symbol-function command)))
    ;; Deal with symbols fset to other symbols.
    (while (symbolp sym-func)
      (setq sym-func (symbol-function sym-func)))
    (delq '&rest (delq '&optional (car (cdr (if (eq (car sym-func) 'macro)
						(cdr sym-func)
					      sym-func)))))))

(defun ds-key-description (keys)
  "Like `key-description', but changes \"ESC char\" into \"M-char\"."
  (ds-string-substitute-substring-general-case "M-" "ESC " (key-description keys)))

;; DOC is a string; we want to downcase and index the function and variable
;; names that appear in it.  ARGUMENTS is a list of arguments, if this is a
;; command's documentation.
(defun ds-massage-documentation (doc &optional arguments)
  ;; I used to seem to need a save-excursion, for some odd reason; no more.
  (let ((old-match-data (match-data)))
    (save-window-excursion
      (set-buffer (get-buffer-create " mapsymbol-temp-buffer"))
      (emacs-lisp-mode)
      (erase-buffer)
      (insert doc)
      (let ((case-fold-search nil))
	;; Command arguments.
	(let (arg upcased-arg-regexp replacement)
	  (while arguments
	    (setq arg (symbol-name (car arguments))
		  upcased-arg-regexp (concat "\\<" (upcase arg) "\\(th\\)?\\>")
		  replacement (concat "@var{" arg "}\\1")
		  arguments (cdr arguments))
	    (goto-char (point-min))
	    (while (re-search-forward upcased-arg-regexp nil t)
	      (replace-match replacement t))))
	;; Functions.
	(goto-char (point-min))
	(let (function-name function-symbol)
	  (while (re-search-forward "`\\(\\sw\\|\\s_\\)+\\('\\)" nil t)
	    (setq function-name (ds-match-string 1)
		  function-symbol (intern-soft function-name))
	    (if (fboundp function-symbol)
		(progn
		  (replace-match "@code{\\1}")
		  (save-excursion
		    (beginning-of-line)
		    (insert "@findex " (symbol-name function-symbol) "\n"))))))
	;; Global variables.
	(goto-char (point-min))
	(let (variable-name variable-symbol)
	  ;; Find double-whitespace, symbol, punctuation-or-double-whitespace.
	  ;; Recall we are using the Emacs Lisp syntax table.
	  (while (re-search-forward (concat "\\(  \\|\n\\|\\s(\\)"
					    "\\(\\sw\\|\\s_\\)+"
					    "\\(  \\|\n\\|\\s.\\|\\s'\\|\\s)\\)")
				    nil t)
	    (setq variable-name (ds-match-string 2)
		  variable-symbol (intern-soft variable-name))

	    ;; Test that the symbol has documentation (ie, is defvar'ed)
	    ;; rather than just that it has been interned at some point.
	    (if (documentation-property variable-symbol 'variable-documentation)
		(progn
		  (replace-match "\\1@code{\\2}\\3")
		  (save-excursion
		    (beginning-of-line)
		    (insert "@vindex " (symbol-name variable-symbol) "\n"))))))
	;; Special constants.
	(goto-char (point-min))
	(while (re-search-forward "\\<\\(nil\\|t\\)\\>" nil t)
	  ;; Don't need to save match data because it will change only if
	  ;; the looking-back-at test is successful.
	  (if (not (ds-looking-back-at "n't"))
	      (replace-match "@code{\\1}")))
	 ;; Make sure indexing commands start at the beginning of the line.
	(goto-char (point-min))
	(if (looking-at "@")
	    (insert "\n"))
	(store-match-data old-match-data)
	(buffer-string)))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utility functions
;;;

(defun ds-replace-regexp-quietly (regexp to-string &optional delimited)
  "Like `replace-regexp', but doesn't message \"Done\" afterward."
  (perform-replace regexp to-string nil t delimited))

(defun ds-match-string (n &optional source)
  "Returns the string matched by parentheses number N.  If there is a
SOURCE string, returns the substring of that string; else, returns
substring of the current buffer."
  (cond
   ((stringp source)
    (substring source (match-beginning n) (match-end n)))
   (t (buffer-substring (match-beginning n) (match-end n)))))

(defun ds-looking-back-at (PAT)
  "t when text before point matches regular expression PAT."
  (save-excursion
    (save-restriction
      (narrow-to-region (point-min) (point))
      (re-search-backward (concat "\\(" PAT "\\)\\'") (point-min) t))))

(defun ds-string-substitute-substring-general-case (new old-regexp string)
  "Calls `ds-string-replace-regexp-2'.  Beware special meaning of \\!."
  (ds-string-replace-regexp-2 string old-regexp new))

;; Dies a horrible death if passed a very long string, which is why we use
;; string-replace-regexp-2 instead.
(defun ds-string-substitute-substring-general-case-1 (new old-regexp string)
  (if (string-match old-regexp string)
      (concat (substring string 0 (match-beginning 0))
	      new
	      (ds-string-substitute-substring-general-case
	       new old-regexp (substring string (match-end 0))))
    string))

;; If much replacement is going to happen, this is more efficient.
;; Original version from gaynor@brushfire.rutgers.edu (Silver).
(defun ds-string-replace-regexp-2 (string regexp replacement)
  "Return the string resulting by replacing all of STRING's instances of REGEXP
with REPLACEMENT."
  (save-excursion
    (set-buffer (get-buffer-create " *Temporary*"))
    (erase-buffer)
    (buffer-flush-undo (current-buffer))
    (save-excursion (insert string))
    (while (re-search-forward regexp nil t)
      (replace-match replacement))
    (buffer-string)
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Unused
;;;

(defun ds-mapsymbol (function &optional string)
  "Applies FUNCTION to each symbol in STRING if non-nil; otherwise, in buffer.
Each symbol is replaced by the function call result, which must be a string!
If STRING is non-nil, the result is returned."
  (save-window-excursion
    (if (not string)
	(progn
	  (set-buffer (get-buffer-create " mapsymbol-temp-buffer"))
	  (erase-buffer)
	  (insert string)))
    ;; The save-excursion isn't necessary if STRING was specified, but I
    ;; don't want to special-case this.
    (save-excursion
       (goto-char (point-min))
       (while (re-search-forward "\\(\\sw\\|\\s_\\)+")
	 (replace-match (funcall function (ds-match-string 0)))))
    (if string
	(buffer-string))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Testing
;;;

;; Some of these usages are (intentionally) incorrect.  It's for testing.
(defvar ds-test-defvar 3
  "Variable affecting `ds-test-defun', which takes args FOO and BAR but not BAZ.")
(defun ds-test-defun (foo &optional bar)
  "Function taking FOO and BAR but not BAZ and  affected  by  ds-test-defvar."
  ds-test-defvar)
