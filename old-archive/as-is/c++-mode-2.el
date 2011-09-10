;; DAIMS C++ mode by Bruce Eckel   Version 1.0 12/8/87
;; automatically invoked on any file with a suffix of ".cxx" or ".hxx"
;;
;; Bruce Eckel: eckel@beluga.ocean.washington.edu
;;              uw-beaver!beluga!eckel
;;              bitnet: eckel%namu.ocean.washington.edu@UWAVM
;;
;; You must have the following in your .emacs file for this to work automatically:
;;(defvar global-class-name nil "Name of current C++ class.")
;;(autoload 'C++-mode "C++-mode.el" nil t)
;;(setq find-file-hooks '(what-to-do-with-files))
;;(defun what-to-do-with-files () nil
;;  (cond
;;   ((string-equal (substring  buffer-file-name -2) ".C")
;;    (C++-mode))
;;   ((string-equal (substring  buffer-file-name -4) ".hxx")
;;    (C++-mode))
;;   ;; other conditionals...
;;  ))
;;
;; (The ".hxx" is for compatibility with Advantage C++ on the PC.  I
;; would have used ".cxx" for c++ source code but the CC script
;; wouldn't allow it and I didn't want to change it).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;     I have created a simple syntax for documentation which is
;; designed to be expandable.  The syntax contains markers for
;; function/class names, and general-purpose markers to enclose and
;; name blocks of descriptive text.  You can then use any number of
;; Unix tools to find, display and "database" this text.  I intend to
;; create some of my own.
;; 
;;     The trick was to find some sequence of characters which would
;; never occur in normal code.  I chose -*++ for function/class "brief
;; descriptions" because the "-*" looked good in a comment header
;; block set off with "**"'s.  The "++", of course, sets off C++.  I
;; also used "++" for the begin- and end-markers for the descriptive
;; text.  In addition, I added open- and close-parentheses to indicate
;; start and stop of descriptive text.  The result is a rather
;; cumbersome but workable: (*++ and ++*).  Again, I chose the
;; sequence of operators because I don't think they will ever appear
;; in normal code.  The only reason I say it's "workable" is because
;; I've made C++-mode do the work for you.
;; 
;; See the documentation for C++-mode for more details
;; 
;; 
;; 
;;     A slick idea might be this:  read in the C++ header file and
;; automatically create all the comment headers and function
;; definition beginnings.  This would prevent duplication of effort
;; and typing errors.  Also, one could create a lisp database with the
;; function declarations so you could just select the one you're
;; defining and it would stick it in for you, sort of like file-name
;; completion. This could also allow you to scan the existing function
;; definitions and pop to the one you want, like tags only more
;; useful (like electric buffer mode). 
;; 
;; 
;;     I am indebted to Brian McElhinney for the original ideas on
;; comment-header documentation.
;; 
;; These modes provide an easy and accurate method of writing C++ code
;;
;;	* C++-mode options (indent-level, etc) set automatically,
;;	auto-fill turned on, fill-prefix and fill-column set.
;;	* Lisp functions provided for C++ mode that,
;;	using your name and the current date:
;;		* Add a new routine with the standard block
;;		comment partially initialized (bound to C-x N).
;;		You are first prompted for the routine name.
;;		* Add a function modification log entry (C-x M).
;;	* C++-mode paragraphs ignore use fill-prefix (but filling does not).
;;	* C++-mode binds M-) go to the start of the next function,
;;	and M-( to go to the start of the last (or current) function.
;;	(The later is the same as M-C-a, but was added for consistancy)
;;	* Both modes automatically remove the standard disclaimer from
;;	view if it is present (via narrow-to-region -- it is still there).
;;	Use M-x widen to view all of the file.  C-x w is rebound to
;;	widen back to the file without the disclaimer showing.  Next-error 
;;	is also rebound to go to the proper line in the file with the
;;	disclaimer not showing.
;;
;; The standard language functions (such as M-;) are still available.

;;  Function descriptions should say: what class they're in and have a
;;  brief description after the function name and parentheses (class,
;;  of course, is determined by the ::).  A place should be allowed
;;  for detailed description, but no isolated place for a brief
;;  description.  Brief function descriptions should be encouraged to
;;  be on only one line, since that's what will be picked out if you
;;  grep for "-*".  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modified from Stallman's original C-mode for C++ mode by Bruce
;; Eckel for the DAIMS project:  Most of the normal C-mode stuff was
;; left alone.  Some functions were added, in particular: 
;; create-C++-class-description, insert-C++-function-description etc.
;; which creates a standard documentation form which allows later
;; searching to create a class information database.  A simple grep
;; for "*-" will generate all classes, functions and plain functions
;; along with their one-line "brief" descriptions.  If AWK is as
;; powerful as I've heard, it might be used to find the "detailed"
;; descriptions delimited by (++ and ++)
;; DATE    Changes
;; 11/30/87 query replace c with C++

;;TODO
;;-- What about inline functions in .hxx files? How will you get
;;documentation on those?  You'll certainly want some indication those
;;functions exist!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C++ code editing commands for Emacs
;; Copyright (C) 1985, 1986, 1987 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

(defvar re-commenting nil
  "*set to true if you're re-commenting existing code -- this prevents
any insertion of actual code")

(defconst comment-routine-start "-*++ "
  "*Comment string to start standard  function summary.
The value of this variable is local to each C buffer.")
(defconst comment-block-start "** "
  "*Comment string to use to start each line of a block comment.
The value of this variable is local to each C buffer.")
(defconst history-description-start "(*++ history: "
  "*Comment string to start the history section of a class or function.")
(defconst detailed-description-start "(*++ detailed: "
  "*Comment string to start a detailed description of the C++ function.
This is the equivalent of the full man page")
(defconst description-end "++*)"
  "*Comment string to end C++ descriptions")

;; The following should probably be cleaned up and integrated more
;; neatly into the code...
(defun C++-mode-options ()
  (setq C++-argdecl-indent 0)
  (setq C++-continued-statement-offset 4)
  (auto-fill-mode 1)
  (make-local-variable 'fill-prefix)
  (setq fill-prefix comment-block-start)
  (make-local-variable 'fill-column)
  (setq fill-column 78)
  (make-local-variable 'comment-routine-start)
  (make-local-variable 'comment-block-start)
  (make-local-variable 'comment-end)
  (setq comment-end "*/")
  (make-local-variable 'newline-suffix)
)

(setq C++-mode-hook 'C++-mode-options)

(defvar C++-mode-abbrev-table nil
  "Abbrev table in use in C++-mode buffers.")
(define-abbrev-table 'C++-mode-abbrev-table ())

(defvar C++-mode-map ()
  "Keymap used in C++mode.")
(if C++-mode-map
    ()
  (setq C++-mode-map (make-sparse-keymap))
  (define-key C++-mode-map "{" 'electric-C++-brace)
  (define-key C++-mode-map "}" 'electric-C++-brace)
  (define-key C++-mode-map ";" 'electric-C++-semi)
  (define-key C++-mode-map ":" 'electric-C++-terminator)
  (define-key C++-mode-map "\e\C-h" 'mark-C++-function)
  (define-key C++-mode-map "\e\C-q" 'indent-C++-exp)
  (define-key C++-mode-map "\e[" 'backward-C++-definition)
  (define-key C++-mode-map "\e]" 'forward-C++-definition)
  (define-key C++-mode-map "\177" 'backward-delete-char-untabify)
  (define-key C++-mode-map "\t" 'C++-indent-command)
  (define-key C++-mode-map "\eh" 'mark-C++-expression)
  (define-key C++-mode-map "\e(" 'backward-to-start-of-defun)
  (define-key C++-mode-map "\e)" 'forward-to-start-of-defun)
  (define-key C++-mode-map "\C-c\C-c" 'exit-recursive-edit)
  (define-key C++-mode-map "\C-xC" 'create-C++-class-description)
  (define-key C++-mode-map "\C-xF" 'insert-C++-function-description)
  (define-key C++-mode-map "\C-xM" 'insert-C++-modification-entry)
  (define-key C++-mode-map "\C-xN" 'change-C++-class-name)
  (define-key C++-mode-map "\C-xP" 'insert-C++-plain-function-description)
  (define-key C++-mode-map "\C-xD" 'add-to-C++-detailed-entry )
  (define-key C++-mode-map "\e:" 'indent-for-C++-one-line-comment)
)

(defvar C++-mode-syntax-table nil
  "Syntax table in use in C++-mode buffers.")

(if C++-mode-syntax-table
    ()
  (setq C++-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\\ "\\" C++-mode-syntax-table)
;; Stallman says "//"-style comments won't work
;;  (modify-syntax-entry ?/ ". 124" C++-mode-syntax-table) ;; '2' added BTE
  (modify-syntax-entry ?/ ". 14" C++-mode-syntax-table) 
  (modify-syntax-entry ?* ". 23" C++-mode-syntax-table)
  (modify-syntax-entry ?+ "." C++-mode-syntax-table)
  (modify-syntax-entry ?- "." C++-mode-syntax-table)
  (modify-syntax-entry ?= "." C++-mode-syntax-table)
  (modify-syntax-entry ?% "." C++-mode-syntax-table)
  (modify-syntax-entry ?< "." C++-mode-syntax-table)
  (modify-syntax-entry ?> "." C++-mode-syntax-table)
  (modify-syntax-entry ?& "." C++-mode-syntax-table)
  (modify-syntax-entry ?| "." C++-mode-syntax-table)
  (modify-syntax-entry ?\' "\"" C++-mode-syntax-table))

(defconst C++-indent-level 4
  "*Indentation of C++statements with respect to containing block.")
(defconst C++-brace-imaginary-offset 0
  "*Imagined indentation of a C++open brace that actually follows a statement.")
(defconst C++-brace-offset 0
  "*Extra indentation for braces, compared with other text in same context.")
(defconst C++-argdecl-indent 5
  "*Indentation level of declarations of C++function arguments.")
(defconst C++-label-offset -2
  "*Offset of C++ label lines and case statements relative to usual indentation.")
(defconst C++-continued-statement-offset 2
  "*Extra indent for lines not starting new statements.")
(defconst C++-continued-brace-offset 0
  "*Extra indent for substatements that start with open-braces.
This is in addition to C++-continued-statement-offset.")

(defconst C++-auto-newline nil
  "*Non-nil means automatically newline before and after braces,
and after colons and semicolons, inserted in C++ code.")

(defconst C++-tab-always-indent t
  "*Non-nil means TAB in C++ mode should always reindent the current line,
regardless of where in the line point is when the TAB command is used.")

(defun C++-mode ()
  "Major mode for editing C++ code.
Expression and list commands understand all C++ brackets.
Tab indents for C++ code.
Comments are delimited with /* ... */.
Paragraphs are separated by blank lines only.
Delete converts tabs to spaces as it moves back.
Comment block editing uses recursive edits where tab and newline are
re-defined to make them more useful for documentation
\\{C++-mode-map}

    C++-mode is an extension of C-mode intended to support C++
programming; in particular the creation of information databases
about C++ classes.  This is a big problem when you're trying to
create re-usable code.  You can't re-use it if you don't know it
exists. 

    The commenting is supposed to work like this:  When you create
a function or class (C-X F or C-X C) C++-mode will ask you what the
name is, then initialize the comment header block with the name and
the appropriate markers.  Following the name, there is a colon: you
are supposed to put a very brief, one line description of the
function or class.  Down below, there is a '(*++ detailed:'.  This
is for a more detailed description of the class or function.  It
can be any length and is terminated by the closing '++*)' which has
already been inserted.  If you think of other descriptive text
blocks which would be useful (for instance, '(*++ inherited:' ) you
can easily change C++-mode to insert these for you.  Please mail me
any changes you make so I can potentially incorporate them with the
main distribution.

    When you're creating C++ class functions, C++-mode will
automatically insert the class name for you; if one hasn't been
defined you will be asked.  That class name is global; it can be
changed by defining a new class or with the C-X N key sequence.

    Brief description searching examples: for classes,
fgrep -e '-* class' *.hxx
    for functions,
fgrep -e '-*' *.cxx

    For an example AWK script to extract brief descriptions and
detailed descriptions, see: cdoc.awk

Variables controlling indentation style:
 C++-tab-always-indent
    Non-nil means TAB in C++ mode should always reindent the current line,
    regardless of where in the line point is when the TAB command is used.
 C++-auto-newline
    Non-nil means automatically newline before and after braces,
    and after colons and semicolons, inserted in C++ code.
 C++-indent-level
    Indentation of C++ statements within surrounding block.
    The surrounding block's indentation is the indentation
    of the line on which the open-brace appears.
 C++-continued-statement-offset
    Extra indentation given to a substatement, such as the
    then-clause of an if or body of a while.
 C++-continued-brace-offset
    Extra indentation given to a brace that starts a substatement.
    This is in addition to C++-continued-statement-offset.
 C++-brace-offset
    Extra indentation for line if it starts with an open brace.
 C++-brace-imaginary-offset
    An open brace following other text is treated as if it were
    this far to the right of the start of its line.
 C++-argdecl-indent
    Indentation level of declarations of C++ function arguments.
 C++-label-offset
    Extra indentation for line that is a label, or case or default.

Settings for K&R and BSD indentation styles are
  C++-indent-level                5    8
  C++-continued-statement-offset  5    8
  C++-brace-offset               -5   -8
  C++-argdecl-indent              0    8
  C++-label-offset               -5   -8

Turning on C++ mode calls the value of the variable C++-mode-hook with no args,
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map C++-mode-map)
  (setq major-mode 'C++-mode)
  (setq mode-name "C++")
  (setq local-abbrev-table C++-mode-abbrev-table)
  (set-syntax-table C++-mode-syntax-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'C++-indent-line)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start "/* ")
  (make-local-variable 'comment-end)
  (setq comment-end " */")
  (make-local-variable 'comment-column)
  (setq comment-column 32)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "/\\*+ *")
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'C++-comment-indent)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (run-hooks 'C++-mode-hook))

;; This is used by indent-for-comment
;; to decide how much to indent a comment in C++ code
;; based on its context.
(defun C++-comment-indent ()
  (if (looking-at "^/\\*")
      0				;Existing comment at bol stays there.
    (save-excursion
      (skip-chars-backward " \t")
      (max (1+ (current-column))	;Else indent at comment column
	   comment-column))))	; except leave at least one space.

(defun electric-C++-brace (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (let (insertpos)
    (if (and (not arg)
	     (eolp)
	     (or (save-excursion
		   (skip-chars-backward " \t")
		   (bolp))
		 (if C++-auto-newline (progn (C++-indent-line) (newline) t) nil)))
	(progn
	  (insert last-command-char)
	  (C++-indent-line)
	  (if C++-auto-newline
	      (progn
		(setq insertpos (1- (point)))
		(newline)
		(C++-indent-line)))
	  (save-excursion
	    (if insertpos (goto-char (1+ insertpos)))
	    (delete-char -1))))
    (if insertpos
	(save-excursion
	  (goto-char insertpos)
	  (self-insert-command (prefix-numeric-value arg)))
      (self-insert-command (prefix-numeric-value arg)))))

(defun electric-C++-semi (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (if C++-auto-newline
      (electric-C++-terminator arg)
    (self-insert-command (prefix-numeric-value arg))))

(defun electric-C++-terminator (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (let (insertpos (end (point)))
    (if (and (not arg) (eolp)
	     (not (save-excursion
		    (beginning-of-line)
		    (skip-chars-forward " \t")
		    (or (= (following-char) ?#)
			;; Colon is special only after a label, or case ....
			;; So quickly rule out most other uses of colon
			;; and do no indentation for them.
			(and (eq last-command-char ?:)
			     (not (looking-at "case"))
			     (save-excursion
			       (forward-word 2)
			       (<= (point) end)))
			(progn
			  (beginning-of-defun)
			  (let ((pps (parse-partial-sexp (point) end)))
			    (or (nth 3 pps) (nth 4 pps) (nth 5 pps))))))))
	(progn
	  (insert last-command-char)
	  (C++-indent-line)
	  (and C++-auto-newline
	       (not (C++-inside-parens-p))
	       (progn
		 (setq insertpos (1- (point)))
		 (newline)
		 (C++-indent-line)))
	  (save-excursion
	    (if insertpos (goto-char (1+ insertpos)))
	    (delete-char -1))))
    (if insertpos
	(save-excursion
	  (goto-char insertpos)
	  (self-insert-command (prefix-numeric-value arg)))
      (self-insert-command (prefix-numeric-value arg)))))

(defun C++-inside-parens-p ()
  (condition-case ()
      (save-excursion
	(save-restriction
	  (narrow-to-region (point)
			    (progn (beginning-of-defun) (point)))
	  (goto-char (point-max))
	  (= (char-after (or (scan-lists (point) -1 1) (point-min))) ?\()))
    (error nil)))

(defun C++-indent-command (&optional whole-exp)
  (interactive "P")
  "Indent current line as C++ code, or in some cases insert a tab character.
If C++-tab-always-indent is non-nil (the default), always indent current line.
Otherwise, indent the current line only if point is at the left margin
or in the line's indentation; otherwise insert a tab.

A numeric argument, regardless of its value,
means indent rigidly all the lines of the expression starting after point
so that this line becomes properly indented.
The relative indentation among the lines of the expression are preserved."
  (if whole-exp
      ;; If arg, always indent this line as C
      ;; and shift remaining lines of expression the same amount.
      (let ((shift-amt (C++-indent-line))
	    beg end)
	(save-excursion
	  (if C++-tab-always-indent
	      (beginning-of-line))
	  (setq beg (point))
	  (forward-sexp 1)
	  (setq end (point))
	  (goto-char beg)
	  (forward-line 1)
	  (setq beg (point)))
	(if (> end beg)
	    (indent-code-rigidly beg end shift-amt "#")))
    (if (and (not C++-tab-always-indent)
	     (save-excursion
	       (skip-chars-backward " \t")
	       (not (bolp))))
	(insert-tab)
      (C++-indent-line))))

(defun C++-indent-line ()
  "Indent current line as C++ code.
Return the amount the indentation changed by."
  (let ((indent (calculate-C++-indent nil))
	beg shift-amt
	(case-fold-search nil)
	(pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (cond ((eq indent nil)
	   (setq indent (current-indentation)))
	  ((eq indent t)
	   (setq indent (calculate-C++-indent-within-comment)))
	  ((looking-at "[ \t]*#")
	   (setq indent 0))
	  (t
	   (skip-chars-forward " \t")
	   (if (listp indent) (setq indent (car indent)))
	   (cond ((or (looking-at "case\\b")
		      (and (looking-at "[A-Za-z]")
			   (save-excursion
			     (forward-sexp 1)
			     (looking-at ":"))))
		  (setq indent (max 1 (+ indent C++-label-offset))))
		 ((and (looking-at "else\\b")
		       (not (looking-at "else\\s_")))
		  (setq indent (save-excursion
				 (C++-backward-to-start-of-if)
				 (current-indentation))))
		 ((= (following-char) ?})
		  (setq indent (- indent C++-indent-level)))
		 ((= (following-char) ?{)
		  (setq indent (+ indent C++-brace-offset))))))
    (skip-chars-forward " \t")
    (setq shift-amt (- indent (current-column)))
    (if (zerop shift-amt)
	(if (> (- (point-max) pos) (point))
	    (goto-char (- (point-max) pos)))
      (delete-region beg (point))
      (indent-to indent)
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos))))
    shift-amt))

(defun calculate-C++-indent (&optional parse-start)
  "Return appropriate indentation for current line as C++ code.
In usual case returns an integer: the column to indent to.
Returns nil if line starts inside a string, t if in a comment."
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
	  (case-fold-search nil)
	  state
	  containing-sexp)
      (if parse-start
	  (goto-char parse-start)
	(beginning-of-defun))
      (while (< (point) indent-point)
	(setq parse-start (point))
	(setq state (parse-partial-sexp (point) indent-point 0))
	(setq containing-sexp (car (cdr state))))
      (cond ((or (nth 3 state) (nth 4 state))
	     ;; return nil or t if should not change this line
	     (nth 4 state))
	    ((null containing-sexp)
	     ;; Line is at top level.  May be data or function definition,
	     ;; or may be function argument declaration.
	     ;; Indent like the previous top level line
	     ;; unless that ends in a closeparen without semicolon,
	     ;; in which case this line is the first argument decl.
	     (goto-char indent-point)
	     (skip-chars-forward " \t")
	     (if (= (following-char) ?{)
		 0   ; Unless it starts a function body
	       (C++-backward-to-noncomment (or parse-start (point-min)))
	       ;; Look at previous line that's at column 0
	       ;; to determine whether we are in top-level decls
	       ;; or function's arg decls.  Set basic-indent accordinglu.
	       (let ((basic-indent
		      (save-excursion
			(re-search-backward "^[^ \^L\t\n#]" nil 'move)
			(if (and (looking-at "\\sw\\|\\s_")
				 (looking-at ".*(")
				 (progn
				   (goto-char (1- (match-end 0)))
				   (forward-sexp 1)
				   (and (< (point) indent-point)
					(not (memq (following-char)
						   '(?\, ?\;))))))
			    C++-argdecl-indent 0))))
		 ;; Now add a little if this is a continuation line.
		 (+ basic-indent (if (or (bobp)
					 (memq (preceding-char) '(?\) ?\; ?\})))
				     0 C++-continued-statement-offset)))))
	    ((/= (char-after containing-sexp) ?{)
	     ;; line is expression, not statement:
	     ;; indent to just after the surrounding open.
	     (goto-char (1+ containing-sexp))
	     (current-column))
	    (t
	     ;; Statement level.  Is it a continuation or a new statement?
	     ;; Find previous non-comment character.
	     (goto-char indent-point)
	     (C++-backward-to-noncomment containing-sexp)
	     ;; Back up over label lines, since they don't
	     ;; affect whether our line is a continuation.
	     (while (or (eq (preceding-char) ?\,)
			(and (eq (preceding-char) ?:)
			     (or (eq (char-after (- (point) 2)) ?\')
				 (memq (char-syntax (char-after (- (point) 2)))
				       '(?w ?_)))))
	       (if (eq (preceding-char) ?\,)
		   (C++-backward-to-start-of-continued-exp containing-sexp))
	       (beginning-of-line)
	       (C++-backward-to-noncomment containing-sexp))
	     ;; Now we get the answer.
	     (if (not (memq (preceding-char) '(nil ?\, ?\; ?\} ?\{)))
		 ;; This line is continuation of preceding line's statement;
		 ;; indent  C++-continued-statement-offset  more than the
		 ;; previous line of the statement.
		 (progn
		   (C++-backward-to-start-of-continued-exp containing-sexp)
		   (+ C++-continued-statement-offset (current-column)
		      (if (save-excursion (goto-char indent-point)
					  (skip-chars-forward " \t")
					  (eq (following-char) ?{))
			  C++-continued-brace-offset 0)))
	       ;; This line starts a new statement.
	       ;; Position following last unclosed open.
	       (goto-char containing-sexp)
	       ;; Is line first statement after an open-brace?
	       (or
		 ;; If no, find that first statement and indent like it.
		 (save-excursion
		   (forward-char 1)
		   (let ((colon-line-end 0))
		     (while (progn (skip-chars-forward " \t\n")
				   (looking-at "#\\|/\\*\\|case[ \t\n].*:\\|[a-zA-Z0-9_$]*:"))
		       ;; Skip over comments and labels following openbrace.
		       (cond ((= (following-char) ?\#)
			      (forward-line 1))
			     ((= (following-char) ?\/)
			      (forward-char 2)
			      (search-forward "*/" nil 'move))
			     ;; case or label:
			     (t
			      (save-excursion (end-of-line)
					      (setq colon-line-end (point)))
			      (search-forward ":"))))
		     ;; The first following code counts
		     ;; if it is before the line we want to indent.
		     (and (< (point) indent-point)
			  (if (> colon-line-end (point))
			      (- (current-indentation) C++-label-offset)
			    (current-column)))))
		 ;; If no previous statement,
		 ;; indent it relative to line brace is on.
		 ;; For open brace in column zero, don't let statement
		 ;; start there too.  If C++-indent-level is zero,
		 ;; use C++-brace-offset + C++-continued-statement-offset instead.
		 ;; For open-braces not the first thing in a line,
		 ;; add in C++-brace-imaginary-offset.
		 (+ (if (and (bolp) (zerop C++-indent-level))
			(+ C++-brace-offset C++-continued-statement-offset)
		      C++-indent-level)
		    ;; Move back over whitespace before the openbrace.
		    ;; If openbrace is not first nonwhite thing on the line,
		    ;; add the C++-brace-imaginary-offset.
		    (progn (skip-chars-backward " \t")
			   (if (bolp) 0 C++-brace-imaginary-offset))
		    ;; If the openbrace is preceded by a parenthesized exp,
		    ;; move to the beginning of that;
		    ;; possibly a different line
		    (progn
		      (if (eq (preceding-char) ?\))
			  (forward-sexp -1))
		      ;; Get initial indentation of the line we are on.
		      (current-indentation))))))))))

(defun calculate-C++-indent-within-comment ()
  "Return the indentation amount for line, assuming that
the current line is to be regarded as part of a block comment."
  (let (end star-start)
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward " \t")
      (setq star-start (= (following-char) ?\*))
      (skip-chars-backward " \t\n")
      (setq end (point))
      (beginning-of-line)
      (skip-chars-forward " \t")
      (and (re-search-forward "/\\*[ \t]*" end t)
	   star-start
	   (goto-char (1+ (match-beginning 0))))
      (current-column))))


(defun C++-backward-to-noncomment (lim)
  (let (opoint stop)
    (while (not stop)
      (skip-chars-backward " \t\n\f" lim)
      (setq opoint (point))
      (if (and (>= (point) (+ 2 lim))
	       (save-excursion
		 (forward-char -2)
		 (looking-at "\\*/")))
	  (search-backward "/*" lim 'move)
	(beginning-of-line)
	(skip-chars-forward " \t")
	(if (looking-at "#")
	    (setq stop (<= (point) lim))
	  (setq stop t)
	  (goto-char opoint))))))   

(defun C++-backward-to-start-of-continued-exp (lim)
  (if (= (preceding-char) ?\))
      (forward-sexp -1))
  (beginning-of-line)
  (if (<= (point) lim)
      (goto-char (1+ lim)))
  (skip-chars-forward " \t"))

(defun C++-backward-to-start-of-if (&optional limit)
  "Move to the start of the last ``unbalanced'' if."
  (or limit (setq limit (save-excursion (beginning-of-defun) (point))))
  (let ((if-level 1)
	(case-fold-search nil))
    (while (not (zerop if-level))
      (backward-sexp 1)
      (cond ((looking-at "else\\b")
	     (setq if-level (1+ if-level)))
	    ((looking-at "if\\b")
	     (setq if-level (1- if-level)))
	    ((< (point) limit)
	     (setq if-level 0)
	     (goto-char limit))))))


(defun mark-C++-function ()
  "Put mark at end of C++ function, point at beginning."
  (interactive)
  (push-mark (point))
  (end-of-defun)
  (push-mark (point))
  (beginning-of-defun)
  (backward-paragraph))

(defun indent-C++-exp ()
  "Indent each line of the C++ grouping following point."
  (interactive)
  (let ((indent-stack (list nil))
	(contain-stack (list (point)))
	(case-fold-search nil)
	restart outer-loop-done inner-loop-done state ostate
	this-indent last-sexp
	at-else at-brace
	(opoint (point))
	(next-depth 0))
    (save-excursion
      (forward-sexp 1))
    (save-excursion
      (setq outer-loop-done nil)
      (while (and (not (eobp)) (not outer-loop-done))
	(setq last-depth next-depth)
	;; Compute how depth changes over this line
	;; plus enough other lines to get to one that
	;; does not end inside a comment or string.
	;; Meanwhile, do appropriate indentation on comment lines.
	(setq innerloop-done nil)
	(while (and (not innerloop-done)
		    (not (and (eobp) (setq outer-loop-done t))))
	  (setq ostate state)
	  (setq state (parse-partial-sexp (point) (progn (end-of-line) (point))
					  nil nil state))
	  (setq next-depth (car state))
	  (if (and (car (cdr (cdr state)))
		   (>= (car (cdr (cdr state))) 0))
	      (setq last-sexp (car (cdr (cdr state)))))
	  (if (or (nth 4 ostate))
	      (C++-indent-line))
	  (if (or (nth 3 state))
	      (forward-line 1)
	    (setq innerloop-done t)))
	(if (<= next-depth 0)
	    (setq outer-loop-done t))
	(if outer-loop-done
	    nil
	  (if (/= last-depth next-depth)
	      (setq last-sexp nil))
	  (while (> last-depth next-depth)
	    (setq indent-stack (cdr indent-stack)
		  contain-stack (cdr contain-stack)
		  last-depth (1- last-depth)))
	  (while (< last-depth next-depth)
	    (setq indent-stack (cons nil indent-stack)
		  contain-stack (cons nil contain-stack)
		  last-depth (1+ last-depth)))
	  (if (null (car contain-stack))
	      (setcar contain-stack (or (car (cdr state))
					(save-excursion (forward-sexp -1)
							(point)))))
	  (forward-line 1)
	  (skip-chars-forward " \t")
	  (if (eolp)
	      nil
	    (if (and (car indent-stack)
		     (>= (car indent-stack) 0))
		;; Line is on an existing nesting level.
		;; Lines inside parens are handled specially.
		(if (/= (char-after (car contain-stack)) ?{)
		    (setq this-indent (car indent-stack))
		  ;; Line is at statement level.
		  ;; Is it a new statement?  Is it an else?
		  ;; Find last non-comment character before this line
		  (save-excursion
		    (setq at-else (looking-at "else\\W"))
		    (setq at-brace (= (following-char) ?{))
		    (C++-backward-to-noncomment opoint)
		    (if (not (memq (preceding-char) '(nil ?\, ?\; ?} ?: ?{)))
			;; Preceding line did not end in comma or semi;
			;; indent this line  C++-continued-statement-offset
			;; more than previous.
			(progn
			  (C++-backward-to-start-of-continued-exp (car contain-stack))
			  (setq this-indent
				(+ C++-continued-statement-offset (current-column)
				   (if at-brace C++-continued-brace-offset 0))))
		      ;; Preceding line ended in comma or semi;
		      ;; use the standard indent for this level.
		      (if at-else
			  (progn (C++-backward-to-start-of-if opoint)
				 (setq this-indent (current-indentation)))
			(setq this-indent (car indent-stack))))))
	      ;; Just started a new nesting level.
	      ;; Compute the standard indent for this level.
	      (let ((val (calculate-C++-indent
			   (if (car indent-stack)
			       (- (car indent-stack))))))
		(setcar indent-stack
			(setq this-indent val))))
	    ;; Adjust line indentation according to its contents
	    (if (or (looking-at "case[ \t]")
		    (and (looking-at "[A-Za-z]")
			 (save-excursion
			   (forward-sexp 1)
			   (looking-at ":"))))
		(setq this-indent (max 1 (+ this-indent C++-label-offset))))
	    (if (= (following-char) ?})
		(setq this-indent (- this-indent C++-indent-level)))
	    (if (= (following-char) ?{)
		(setq this-indent (+ this-indent C++-brace-offset)))
	    ;; Put chosen indentation into effect.
	    (or (= (current-column) this-indent)
		(= (following-char) ?\#)
		(progn
		  (delete-region (point) (progn (beginning-of-line) (point)))
		  (indent-to this-indent)))
	    ;; Indent any comment following the text.
	    (or (looking-at comment-start-skip)
		(if (re-search-forward comment-start-skip (save-excursion (end-of-line) (point)) t)
		    (progn (indent-for-comment) (beginning-of-line)))))))))
; (message "Indenting C++ expression...done")
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Major Added stuff which is different from C-mode
;; C++-mode options

(defun create-C++-class-description (name)
  "Insert a  C++ class description.
   (comment-start and comment-routine-start used)."
  (interactive "sCreate class named: ")
  (conditionally-insert-formfeed)
  (insert comment-start "\n")
  (insert comment-routine-start)
  (insert "class " name ": \n")
  (save-excursion
    (comment-line "")
    (comment-line history-description-start)
    (modification-line)
    (insert "Creation date\n")
    (comment-line description-end)
    (comment-line "")
    (comment-line detailed-description-start)
    (comment-line description-end)
    (insert comment-end "\n\n")
    (if (not re-commenting) (insert "class " name " {\n"))
    )
  (search-backward ": ")
  (end-of-line)
  (setq global-class-name name)	;; for other functions to use
  )

(defun insert-C++-function-description (name)
  "Insert a  C++ function description.
 (comment-start and comment-routine-start used)."
  (interactive "sInsert function named: ")
  (conditionally-insert-formfeed)
  (insert comment-start "\n")
  (insert comment-routine-start)
  (if (not global-class-name)
      (setq global-class-name (read-from-minibuffer "class name: ")))
  (insert global-class-name "::" name "(): \n")
  (save-excursion
    (comment-line "")
    (comment-line history-description-start)
    (modification-line)
    (insert "Creation date\n")
    (comment-line description-end)
    (comment-line "")
    (comment-line detailed-description-start)
    (comment-line description-end)
    (insert comment-end "\n\n")
    (if (not re-commenting) (insert "void " global-class-name "::" name "()\n{\n"))
    (C++-indent-command)
    )
  (search-backward "(): ")
  (end-of-line))

(defun insert-C++-plain-function-description (name)
  "Insert a  C++ function description without associating it with a class.
Friend functions and ordinary C functions use this.
   (comment-start and comment-routine-start used)."
  (interactive "sInsert plain function named: ")
  (conditionally-insert-formfeed)
  (insert comment-start "\n")
  (insert comment-routine-start)
  (insert name "(): \n")
  (save-excursion
    (comment-line "")
    (comment-line history-description-start)
    (modification-line)
    (insert "Creation date\n")
    (comment-line description-end)
    (comment-line "")
    (comment-line detailed-description-start)
    (comment-line description-end)
    (insert comment-end "\n\n")
    (if (not re-commenting) (insert "void " name "()\n{\n"))
    (C++-indent-command)
    )
  (search-backward "(): ")
  (end-of-line))

(defun change-C++-class-name (name)
  "change the global class name for C++ mode.  This name is 
automatically inserted when a new function is defined"
  (interactive "sNew global class name: ")
  (setq global-class-name name)
  )

(defun insert-C++-modification-entry ()
  "Insert  format modification entry"
  (interactive)
  (save-excursion
    (search-backward comment-routine-start nil t)
    (search-forward history-description-start nil t)
    (search-forward description-end nil t)
    (beginning-of-line)
    (open-line 1)
    (modification-line)
    (C++-comment-block-edit)
    )
  )

(defun add-to-C++-detailed-entry ()
  "Go to end of 'detailed' entry in a recursive edit, so stuff can be added"
  (interactive)
  (save-excursion
    (search-backward comment-routine-start nil t)
    (search-forward detailed-description-start nil t)
    (search-forward description-end nil t)
    (end-of-line 0)			;;back one line, and to the end
    (C++-comment-block-edit)
    )
  )

(defun C++-comment-block-edit () 
  "tiny mode for editing comment blocks"
  (interactive)
  (message (substitute-command-keys
	    "type \\[exit-recursive-edit] to leave recursive edit"))
  (let ( (old-newline (key-binding "\r"))
	 (old-tab (key-binding "\t"))
	 )
    (local-set-key "\r" 'C++-comment-header-newline)
    (local-set-key "\t" 'C++-comment-header-tab)
    (recursive-edit)
    (local-set-key "\r" old-newline)
    (local-set-key "\t" old-tab)
    )
)
    

(defun modification-line () nil
  (insert comment-block-start)
  (insert "\t")
  (insert
   (substring (current-time-string) 8 10) ; Day
   (substring (current-time-string) 3 8)  ; Month
   (substring (current-time-string) -2)   ; Year
   "\t" (user-full-name) "\t")
)

(defun forward-to-start-of-defun (arg)
  "Like beginning-of-defun, except default to forward"
  (interactive "p")
  (or arg (setq arg 1))
  (beginning-of-defun (* arg -1)))

(defun backward-to-start-of-defun (arg)
  "Complement to forward-to-start-of-defun (same as beginning-of-defun)"
  (interactive "p")
  (or arg (setq arg 1))
  (beginning-of-defun arg))

;; Paragraphs without fill-prefixii
(defun forward-paragraph-no-fill-prefix (&optional arg)
  "Run forward-paragraph and ignore fill-prefix"
  (interactive "p")
  (let ((fill-prefix nil))
    (forward-paragraph arg)))

(defun backward-paragraph-no-fill-prefix (&optional arg)
  "A simple version of forward-paragraph that ignores fill-prefix"
  (interactive "p")
  (let ((fill-prefix nil))
    (backward-paragraph arg)))

(defun mark-C++-expression ()
  "Put point at beginning of this C++ expression, mark at end."
  (interactive)
  (forward-paragraph-no-fill-prefix 1)
  (set-mark (point))
  (backward-paragraph-no-fill-prefix 1))

(defun comment-line (text) nil
  (insert comment-block-start text "\n"))

(defun conditionally-insert-formfeed () 
  "Put a formfeed in if this isn't the first definition in the file"
  (interactive)
  (if (save-excursion
	(search-backward comment-routine-start nil t))
      (insert "\n")))

(defun line-number ()
  "Return the line number of the current position in the buffer"
  (interactive)
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (1+ (count-lines 1 (point))))))

(defun C++-comment-header-newline ()
  "Newline used in comment header modification"
  (interactive)
  (newline)
  (insert comment-block-start))

(defun C++-comment-header-tab ()
  "Tab used in comment header modification"
  (interactive)
  (insert "\t"))

(defun forward-C++-definition () 
  "move forward to next C++ definition (created with C++-mode)"
  (interactive)
  (if (search-forward comment-routine-start nil t 2)
      (progn (recenter 0)
	     (beginning-of-line 1))
    (message "last C++-mode definition in file")))


(defun backward-C++-definition ()
  "move backward to previous C++ definition (created with C++-mode)"
  (interactive)
  (if (search-backward comment-routine-start nil t)
      (progn (recenter 0)
	     (beginning-of-line 1))
    (message "first C++-mode definition in file")))

;; Stallman says "//"-style comments won't work
;;(defun indent-for-C++-one-line-comment ()
;;  "move forward to comment area and insert the one-line comment
;;characters"
;;  (interactive)
;;  (let ((comment-start "// ")
;;	(comment-end "")
;;	(comment-start-skip "// *"))
;;    (indent-for-comment)))


