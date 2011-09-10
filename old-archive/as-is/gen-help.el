;; GNU Emacs help/database facility
;; Paul Davis <davis%scr.slb.com@relay.cs.net>, May & July 1988

;; this file is subject to the conditions of the GNU Emacs general
;; public license.

;; Not an interface to dbm, and not much use with BIG databases, but
;; for on-line (non-Emacs) help, and quick database queries, this e-lisp code seems quite
;; adequate.  Again, brought about due to an inferiority complex over
;; Unipress, although this does regexp's, and will handle any number
;; of keys. It does lose a bit in speed however, since Emacs has to
;; read each database visited into a buffer first (!!).

(require 'elib)

;;----------------------------------------------------------
;; `global variables' - use set-default to set your default
;; help path 
;; 
;;   eg; (set-default 'help-default-path "/a /b/c /d/e/f")
;;
;;-----------------------------------------------------------

(defvar help-last-file nil
  "File last visited by \[help]")
(defvar help-last-start 0
  "Start of line where last match occured in this buffer")
(defvar help-last-regexp nil
  "previous regexp passed to help in this buffer")
(defvar help-default-path nil
  "*default file used by help")
(defvar help-regexp-prefix "^\\(\\S +.*"
  "*regexp used by help to filter key lines from a help file. This
should always incude an open alternative specifier \\(")
(defvar help-keyline-identifier-regexp "^\\S "
  "*Regexp used for locating a key line in a help file")
(defvar help-buffer-to-add-to nil
  "Holds name of file from which help-add-item
is called")
(defvar help-case-fold-search nil
  "*If non-nil, help/db commands ignore case when searching")
(defvar help-modeline-prefix-string " GNU-DBASE Database:[%b]%3 current target: "
  "Used for modeline format in help-mode, suffixed by help-last-regexp")

;; --------------------------------------------------------------------
;; local variables - this is so that each buffer (and each mode ?) can
;; have their own search path and their own previous items searched for.
;; We don't really want to have to make require-final-newline global
;; so its recast here as a local variable. We need it on when adding
;; new entries to the database files.
;; --------------------------------------------------------------------

(make-local-variable 'help-last-regexp)
(make-local-variable 'help-last-start)
(make-local-variable 'help-default-path)
(make-local-variable 'require-final-newline)

;; ------------------------------------------------
;; help - the function
;; ------------------------------------------------

(defun help (key path &optional repeat reverse)

  "Display text on KEY from a file in PATH. KEY is a regexp
combined with help-regexp-prefix to filter out key lines from text.

Optional third argument non-nil specifies repeated search for KEY from
current position in PATH.  Optional fourth argument if non-nil
specifies reverse direction for search.

The format of FILE is simple: keys are on a line which begins with a
non-space character, text is on lines beginning with a whitespace
character. An entry runs from the start of its key line to the start
of the next key line. The file *must* have newlines both *before* the
first and *after* the last entry, if these are not to be lost.

This function is really designed for building other, more specific
help and db functions , and not for calling via M-x."
  
  (interactive "sKey (regexp): \nsFile (search path): ")
  (setq path (expand-path-names (resolve-string path)))
  (if repeat
      (setq path (pop-this-file path help-last-file)))           ;; rotate current file to front of path
  (cond ((null path)                                             ;; empty search path
	 (message "no further matching entries for \"%s\"" key)  ;; tell us with words
	 (ding))			 			 ;; tell us with sound
	((setq entry (help-entry key (car path) repeat reverse)) ;; search for it
	 (show-help entry key (car path)))                       ;; found it - now display it
	(t (help key (cadr path)))))                             ;; otherwise, recurse down the search path

;; -------------------------------------------------------
;; second order functions (not to be called interactively)
;; -------------------------------------------------------

(defun show-help (entry key file)
  "Do some formatting of a help file entry."
  (if (string-match "\n" entry)
      (progn
	(setq key-list (substring entry 0 (match-beginning 0)))
	(setq entry (substring entry (match-end 0))))
    (setq key-list nil))
  (if (and key-list (string-match "{" key-list))
      (progn
	(setq primary-key (substring key-list 0 (match-beginning 0)))
	(setq key-list (substring key-list (match-end 0) (1- (length
							      key-list)))))
    (setq primary-key (eval key-list))
    (setq key-list nil))
  (with-output-to-temp-buffer "*DBASE*"
    (pop-to-buffer "*DBASE*")
    (help-mode)
    (insert "  " primary-key "\n\n")
    (insert entry)
    (if key-list
	(insert "\n\n Cross references: " key-list))
    (setq mode-line-format
	(list (concat "Emacs-DBASE database:[" (file-name-nondirectory
						file) "]   current target: " key)))))
  
(defun help-entry (help-regexp file repeat reverse)
  "Return a strng containing an entry with a key matching
HELP-REGEXP found in FILE. Third and fourth arguments are as for \[help].
Returns NIL if no match is found."
  (save-window-excursion
    (pop-to-buffer (find-file-noselect file))
    (help-locate-search-start repeat reverse)
    (if help-case-fold-search
	(setq case-fold-search t))
    (if (help-search)
	(progn
	  (beginning-of-line)
	  (setq help-last-file (buffer-file-name))
	  (setq help-last-regexp help-regexp)
	  (setq help-last-start (point))
	  (buffer-to-next-key))
      nil)))

(defun help-search ()
  "Encapsulation of search commands for help-mode"
  (if reverse
      (re-search-backward (concat help-regexp-prefix help-regexp
				  "\\|" help-regexp "\\)") (point-min) t)
    (re-search-forward (concat help-regexp-prefix help-regexp
			       "\\|" help-regexp "\\)") (point-max) t)))

(defun help-locate-search-start (repeat reverse)
  "Set up position of point for search"
  (widen)
  (if  repeat
      (progn
	(goto-char help-last-start)
	(if reverse
	    (beginning-of-line 0)
	  (beginning-of-line 2)))
    (if reverse
	(goto-char (point-max))
      (goto-char (point-min)))))
  
(defun buffer-to-next-key ()
  "Return current buffer from point to the start of the next key line or
EOB if there are no futher key lines in the buffer"
  (setq start (save-excursion (beginning-of-line) (point)))
  (save-excursion
    (beginning-of-line 2)
    (if (null
	 (re-search-forward help-keyline-identifier-regexp (point-max) t))
	(goto-char (point-max)))
    (beginning-of-line 1)
    (buffer-substring start (point))))

;; ---------------------------
;; interactive "key" commands
;; ---------------------------

(defun db (key)
  "Master help function"
  (interactive "sKey (regexp): ")
  (help key help-default-path))

(defun help-next-match ()
  "Display the next entry in the current help file having a key
matching the current target"
  (interactive)
  (help help-last-regexp help-default-path 1 nil))

(defun help-previous-match ()
  "Display the previous entry in the current help file having a key
matching the current target"
  (interactive)
  (help help-last-regexp help-default-path 1 t))

(defun help-this-file (item)
  (interactive "sItem (regexp): ")
  (help item (buffer-file-name)))

;; -------------------------------------
;; adding new entries to a database file
;; -------------------------------------

(defun help-edit-this-item ()
  "Edit the current help/db item being viewed."
  (interactive)
  (pop-to-buffer (file-name-nondirectory help-last-file))
  (beginning-of-line 1)
  (narrow-to-region (point)
		    (save-excursion 
		      (forward-char 1)
		      (if (null (re-search-forward
				 help-keyline-identifier-regexp (point-max) t))
			  (goto-char (point-max))
			(beginning-of-line))
			(point))))

(defun help-add-item (key xref)
  "Add a new entry to the file from which the current entry came.
This function simply sets up a buffer for the user to add the entry -
the actual save is done by \[help-save-item]."
  (interactive "sStore under keys: \nsCross-reference by: ")
  (setq help-buffer-to-add-to (file-name-nondirectory help-last-file))
  (with-output-to-temp-buffer "help-item"
    (set-buffer "help-item")
    (indented-text-mode)
    (local-set-key "\C-x\C-s" 'help-save-item)
    (local-set-key "\C-x\C-q" 'help-quit-add)
    (message "C-x C-s to add the entry, C-x C-q to quit")
    (insert (concat key " {" xref "}\n")))
  (pop-to-buffer "help-item")
  (goto-char (point-min))
  (forward-line 2)
  (narrow-to-region (point) (point-max)))

(defun help-save-item ()
  "append the current buffer (assumed to have been created using
\[help-add-item]) to that of the help file from which this was
created, and save the buffer, ensuring the prescence of a final
newline."
  (interactive)
  (indent-region (point-min) (point-max) 4)
  (widen)
  (save-window-excursion
    (pop-to-buffer help-buffer-to-add-to)
    (widen)
    (goto-char (point-max)))
  (append-to-buffer help-buffer-to-add-to (point-min) (point-max))
  (save-window-excursion
    (pop-to-buffer help-buffer-to-add-to)
    (setq require-final-newline t)
    (save-buffer 1)
    (bury-buffer))
  (kill-buffer "help-item"))

(defun help-quit-add ()
  "Exit from an item creating session by killing the temporary
buffer in which its going on."
  (interactive)
  (kill-buffer "help-item"))

(defun help-add-key (key)
  (interactive "sKey to add: ")
  (save-window-excursion
    (pop-to-buffer (file-name-nondirectory help-last-file))
    (beginning-of-line)
    (cond ((re-search-forward "}" (save-excursion (end-of-line) (point)) t)
	   (backward-char 1)
	   (insert " " key))
	  (t
	   (insert "{" key "}")))))

(defun help-set-default-path (path)
  "Set help-default-path to FILE so that future
calls to help use FILE instead of current default."
  (interactive (list (read-string "Help search path: " help-default-path)))
  (setq help-default-path path))

(defun help-mode ()
  "Mode for Emacs help/database mode

\\{help-mode-map}."
  (interactive)
  (kill-all-local-variables)
  (use-local-map help-mode-map)
  (define-abbrev-table 'text-mode-abbrev-table ())
  (set-syntax-table text-mode-syntax-table)
  (setq major-mode 'help-mode)
  (setq mode-name "HELP")
  (run-hooks 'help-mode-hook))
  
(defvar help-mode-map (make-sparse-keymap)
  "*Keymap for help mode")

(define-key help-mode-map "\e[B" 'help-next-match)       ;; ANSI arrow key DOWN
(define-key help-mode-map "\e[A" 'help-previous-match)   ;; ANSI arrow key UP
(define-key help-mode-map "f" 'db)
(define-key help-mode-map "n" 'help-next-match)
(define-key help-mode-map "p" 'help-previous-match)
(define-key help-mode-map "c" 'help-this-file)
(define-key help-mode-map "a" 'help-add-item)
(define-key help-mode-map "s" 'help-set-default-path)
(define-key help-mode-map "k" 'help-add-key)
(define-key help-mode-map "e" 'help-edit-this-item)

;; ----------------------------------------------------------
;; this one's for doing Norton Guide type pop-up info
;; on something in a buffer. Needs a good hyperkey to be
;; bound to and mode-specific setting of the help search path
;; ----------------------------------------------------------

(defun help-on-thing ()
  "Provide help on thing before point, using current buffer's
help-default-path"
  (interactive)
  (if (eobp) (open-line 1))
  (let* ((char (char-after (point)))
         (syntax (char-syntax char)))
    (cond
     ((eq syntax ?\ )
      (backward-sexp 1)
      (set-mark (point))
      (forward-sexp 1))
     ((eq syntax ?w)			; word.
      (forward-word 1)
      (set-mark (point))
      (forward-word -1))
     ((eq syntax ?\( )			; open paren.
      (mark-sexp 1))
     ((eq syntax ?\) )			; close paren.
      (forward-char 1)
      (mark-sexp -1)
      (exchange-point-and-mark))
     ((eolp)				; mark line if at end.
      (set-mark (1+ (point)))
      (beginning-of-line 1))
     (t					; mark character
      (set-mark (1+ (point)))))
    (help (buffer-substring (region-beginning) 
			    (region-end)) help-default-path)))

;; BibTeX/rolodex database


(defun bibtex-db (bibtex-database key)
  (interactive "Fbibtex database: \nskey: ")
  (help key bibtex-database)
  (pop-to-buffer "*DBASE*")
  (goto-char (point-min))
  (replace-regexp "^\\|}$" "")
  (setq old-regexp help-regexp-prefix
	 help-xref-string "Cite code: "
	 help-default-path bibtex-database
	 show-help-hook '(lambda ()
			   (goto-char (point-min))
			   (replace-regexp "^\\|}$" ""))))

(provide 'generic-help)


