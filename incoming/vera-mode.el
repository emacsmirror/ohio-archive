;;; vera-mode.el --- major mode for editing Vera files.

;; Copyright (C) 1999 Reto Zimmermann, Synopsys Inc.

;; Author:      Reto Zimmermann  <reto@gnu.org>
;; Maintainer:  Reto Zimmermann  <reto@gnu.org>
;; Version:     2.1
;; Keywords:    languages vera
;; WWW:         http://www.emacs.org/hdl/vera-mode.html

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This package provides a simple Emacs major mode for editing Vera code.
;; It includes the following features:

;;   - Syntax highlighting
;;   - Indentation
;;   - Word/keyword completion
;;   - Block commenting
;;   - Works under GNU Emacs and XEmacs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Documentation

;; See comment string of function `vera-mode' or type `C-c C-h' in Emacs.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Installation

;; Put `vera-mode.el' into the `site-lisp' directory of your Emacs installation
;; or into an arbitrary directory that is added to the load path by the
;; following line in your Emacs start-up file (`.emacs'):

;;   (setq load-path (cons (expand-file-name "<directory-name>") load-path))

;; If you already have the compiled `vera-mode.elc' file, put it in the same
;; directory.  Otherwise, byte-compile the source file:
;;   Emacs:  M-x byte-compile-file  ->  vera-mode.el
;;   Unix:   emacs -batch -q -no-site-file -f batch-byte-compile vera-mode.el

;; Add the following lines to the `site-start.el' file in the `site-lisp'
;; directory of your Emacs installation or to your Emacs start-up file
;; (`.emacs'):

;;   (autoload 'vera-mode "vera-mode" "Vera Mode" t)
;;   (setq auto-mode-alist (cons '("\\.vr[hi]?\\'" . vera-mode) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

;; XEmacs handling
(defconst vera-xemacs (string-match "XEmacs" emacs-version)
  "Non-nil if XEmacs is used.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup vera nil
  "Customizations for Vera Mode."
  :prefix "vera-"
  :group 'languages)

(defcustom vera-basic-offset 2
  "*Amount of basic offset used for indentation."
  :type 'integer
  :group 'vera)

(defcustom vera-underscore-is-part-of-word nil
  "*Non-nil means consider the underscore character `_' as part of word.
An identifier containing underscores is then treated as a single word in
select and move operations.  All parts of an identifier separated by underscore
are treated as single words otherwise."
  :type 'boolean
  :group 'vera)


(defconst vera-version "2.1"
  "Vera Mode version number.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key bindings

(defvar vera-mode-map ()
  "Keymap for Vera Mode.")

(setq vera-mode-map (make-sparse-keymap))
;; backspace/delete key bindings
(define-key vera-mode-map [backspace] 'backward-delete-char-untabify)
(unless (boundp 'delete-key-deletes-forward) ; XEmacs variable
  (define-key vera-mode-map [delete]       'delete-char)
  (define-key vera-mode-map [(meta delete)] 'kill-word))
;; standard key bindings
(define-key vera-mode-map "\M-e"     'vera-forward-statement)
(define-key vera-mode-map "\M-a"     'vera-backward-statement)
(define-key vera-mode-map "\M-\C-e"  'vera-forward-same-indent)
(define-key vera-mode-map "\M-\C-a"  'vera-backward-same-indent)
;; mode specific key bindings
(define-key vera-mode-map "\C-c\t"   'vera-indent-line)
(define-key vera-mode-map "\M-\C-\\" 'vera-indent-region)
(define-key vera-mode-map "\C-c\C-c" 'vera-comment-uncomment-region)
(define-key vera-mode-map "\C-c\C-f" 'vera-fontify-buffer)
(define-key vera-mode-map "\C-c\C-h" 'vera-doc-mode)
(define-key vera-mode-map "\C-c\C-v" 'vera-version)
(define-key vera-mode-map "\M-\t"    'tab-to-tab-stop)
;; electric key bindings
(define-key vera-mode-map "\t"       'vera-electric-tab)
(define-key vera-mode-map "\r"       'vera-electric-return)
(define-key vera-mode-map " "        'vera-electric-space)
(define-key vera-mode-map "{"        'vera-electric-opening-brace)
(define-key vera-mode-map "}"        'vera-electric-closing-brace)
(define-key vera-mode-map "#"        'vera-electric-pound)
(define-key vera-mode-map "*"        'vera-electric-star)
(define-key vera-mode-map "/"        'vera-electric-slash)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu

(defvar vera-mode-menu-list
  '("Vera"
     ["(Un)Comment Out Region"	vera-comment-uncomment-region (mark)]
     "--"
     ["Move Forward Statement"	vera-forward-statement t]
     ["Move Backward Statement"	vera-backward-statement t]
     ["Move Forward Same Indent" vera-forward-same-indent t]
     ["Move Backward Same Indent" vera-backward-same-indent t]
     "--"
     ["Indent Line"		vera-indent-line t]
     ["Indent Region"		vera-indent-region (mark)]
     ["Indent Buffer"		vera-indent-buffer t]
     "--"
     ["Fontify Buffer"		vera-fontify-buffer t]
     "--"
     ["Documentation"		vera-doc-mode :keys "C-c C-h"]
     ["Version"			vera-version t]
     ["Bug Report..."		vera-submit-bug-report t]
     "--"
     ["Customize..."		vera-customize t]
     )
  "Vera Mode menu.")

(require 'easymenu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax table

(defvar vera-mode-syntax-table nil
  "Syntax table used in `vera-mode' buffers.")

(setq vera-mode-syntax-table (make-syntax-table))
;; punctuation
(modify-syntax-entry ?\# "."    vera-mode-syntax-table)
(modify-syntax-entry ?\$ "."    vera-mode-syntax-table)
(modify-syntax-entry ?\% "."    vera-mode-syntax-table)
(modify-syntax-entry ?\& "."    vera-mode-syntax-table)
(modify-syntax-entry ?\' "."    vera-mode-syntax-table)
(modify-syntax-entry ?\* "."    vera-mode-syntax-table)
(modify-syntax-entry ?\- "."    vera-mode-syntax-table)
(modify-syntax-entry ?\+ "."    vera-mode-syntax-table)
(modify-syntax-entry ?\. "."    vera-mode-syntax-table)
(modify-syntax-entry ?\/ "."    vera-mode-syntax-table)
(modify-syntax-entry ?\: "."    vera-mode-syntax-table)
(modify-syntax-entry ?\; "."    vera-mode-syntax-table)
(modify-syntax-entry ?\< "."    vera-mode-syntax-table)
(modify-syntax-entry ?\= "."    vera-mode-syntax-table)
(modify-syntax-entry ?\> "."    vera-mode-syntax-table)
(modify-syntax-entry ?\\ "."    vera-mode-syntax-table)
(modify-syntax-entry ?\| "."    vera-mode-syntax-table)
;; string
(modify-syntax-entry ?\" "\""   vera-mode-syntax-table)
;; underscore
(when vera-underscore-is-part-of-word
  (modify-syntax-entry ?\_ "w"    vera-mode-syntax-table))
;; escape
(modify-syntax-entry ?\\ "\\"   vera-mode-syntax-table)
;; parentheses to match
(modify-syntax-entry ?\( "()"   vera-mode-syntax-table)
(modify-syntax-entry ?\) ")("   vera-mode-syntax-table)
(modify-syntax-entry ?\[ "(]"   vera-mode-syntax-table)
(modify-syntax-entry ?\] ")["   vera-mode-syntax-table)
(modify-syntax-entry ?\{ "(}"   vera-mode-syntax-table)
(modify-syntax-entry ?\} "){"   vera-mode-syntax-table)
;; comment
(if (memq '8-bit c-emacs-features)
    (modify-syntax-entry ?\/ ". 1456" vera-mode-syntax-table) ; XEmacs
  (modify-syntax-entry ?\/ ". 124b" vera-mode-syntax-table)) ; Emacs
(modify-syntax-entry ?\* ". 23" vera-mode-syntax-table)
;; newline and CR
(modify-syntax-entry ?\n "> b"    vera-mode-syntax-table)
(modify-syntax-entry ?\^M "> b"   vera-mode-syntax-table)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode definition

;;;###autoload
(defun vera-mode ()
  "Major mode for editing Vera code.

Usage:
------

- INDENTATION:  Typing `\\[vera-electric-tab]' at the beginning of a line indents the line.
  The amount of indentation is specified by variable `vera-basic-offset'.
  Indentation can be done for an entire region \(`\\[vera-indent-region]') or buffer (menu).

- WORD/COMMAND COMPLETION:  Typing `\\[vera-electric-tab]' after a (not completed) word looks
  for a word in the buffer or a Vera keyword that starts alike, inserts it
  and adjusts case.  Re-typing `\\[vera-electric-tab]' toggles through alternative word
  completions.

  Typing `\\[vera-electric-tab]' after a non-word character inserts a tabulator stop (if
  not at the beginning of a line).  `\\[tab-to-tab-stop]' always inserts a tabulator stop.

- COMMENTS:  `\\[vera-comment-uncomment-region]' comments out a region if not commented out, and
  uncomments a region if already commented out.

- HIGHLIGHTING (fontification):  Vera keywords, predefined types and constants,
  function names, declaration names, directives, as well as comments and
  strings are highlighted using different colors.


Maintenance:
------------

To submit a bug report, use the corresponding menu entry within Vera Mode.
Add a description of the problem and include a reproducible test case.

Feel free to send questions and enhancement requests to <reto@gnu.org>.

Official distribution is at <http://www.emacs.org/hdl/vera-mode.html>.


                                                  The Vera Mode Maintainer
                                               Reto Zimmermann <reto@gnu.org>

Key bindings:
-------------

\\{vera-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'vera-mode)
  (setq mode-name "Vera")
  ;; set maps and tables
  (use-local-map vera-mode-map)
  (set-syntax-table vera-mode-syntax-table)
  ;; set local variables
  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-column) 40)
  (set (make-local-variable 'comment-start-skip) "// *")
  (set (make-local-variable 'comment-indent-function) 'c-comment-indent)
  (set (make-local-variable 'end-comment-column) 79)
  (set (make-local-variable 'paragraph-start) "^$")
  (set (make-local-variable 'paragraph-separate) paragraph-start)
  (set (make-local-variable 'require-final-newline) t)
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'indent-line-function) 'vera-indent-line)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  ;; initialize font locking
  (require 'font-lock)
  (set (make-local-variable 'font-lock-defaults)
       '(vera-font-lock-keywords nil nil ((?\_ . "w"))))
  (turn-on-font-lock)
  ;; add menu
  (easy-menu-add vera-mode-menu-list) ; for XEmacs
  (easy-menu-define vera-mode-menu vera-mode-map
		    "Menu keymap for Vera Mode." vera-mode-menu-list)
  (run-hooks 'menu-bar-update-hook)
  ;; miscellaneous
  (message "Vera Mode %s.  Type C-c C-h for documentation." vera-version)
  ;; run hooks
  (run-hooks 'vera-mode-hook))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Vera definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keywords

(defconst vera-keywords
  '(
    "all" "any" "assoc_index" "assoc_size" "async"
    "bad_state" "bad_trans" "begin" "big_endian" "bind"
    "bit_normal" "bit_reverse" "break" "breakpoint"
    "case" "casex" "casez" "class" "constraint" "continue"
    "coverage" "coverage_block" "coverage_def" "coverage_depth"
    "coverage_goal" "coverage_option" "coverage_val"
    "default" "depth" "dist"
    "else" "end" "enum" "exhaustive" "export" "extends" "extern"
    "for" "fork" "function"
    "if" "illegal_self_transition" "illegal_state" "illegal_transition"
    "in" "interface" "invisible"
    "join"
    "little_endian" "local"
    "m_bad_state" "m_bad_trans" "m_state" "m_trans"
    "negedge" "new" "newcov" "non_rand" "none" "not" "null"
    "or" "ordered" "packed" "port"
    "posedge" "prod" "prodget" "prodset" "program" "protected" "public"
    "rand" "randc" "randcase" "randseq" "repeat" "return"
    "shadow" "soft" "state" "static" "super"
    "task" "terminate" "this" "trans" "typedef"
    "unpacked"
    "var" "vca" "vector" "verilog_node" "verilog_task"
    "vhdl_node" "vhdl_task" "virtual" "visible" "void"
    "while" "with"
    )
  "List of Vera keywords.")

(defconst vera-types
  '(
    "integer" "bit" "reg" "string" "bind_var" "event"
    "inout" "input" "output"
    "ASYNC" "CLOCK"
    "NDRIVE" "NHOLD" "NRX" "NRZ" "NR0" "NR1" "NSAMPLE"
    "PDRIVE" "PHOLD" "PRX" "PRZ" "PR0" "PR1" "PSAMPLE"
    )
  "List of Vera predefined types.")

(defconst vera-q-values
  '(
    "gnr" "grx" "grz" "gr0" "gr1"
    "nr" "rx" "rz" "r0" "r1"
    "snr" "srx" "srz" "sr0" "sr1"
    )
  "List of Vera predefined VCA q_values.")

(defconst vera-functions
  '(
    ;; system functions and tasks
    "alloc" "call_func" "call_task" "cast_assign" "close_conn"
    "delay" "error" "error_mode" "exit"
    "fclose" "fflush" "flag" "fopen" "fprintf" "freadb" "freadh" "freadstr"
    "get_bind" "get_bind_id" "get_conn_err" "get_cycle" "get_plus_arg"
    "get_systime" "get_time"
    "mailbox_get" "mailbox_put" "make_client" "make_server"
    "printf" "rand48" "random" "region_enter" "region_exit" "rewind"
    "semaphore_get" "semaphore_put" "sprintf" "sscanf" "stop" "sync"
    "timeout" "trace" "trigger"
    "unit_delay" "up_connections" "urand48" "urandom"
    "vsv_call_func" "vsv_call_task" "vsv_close_conn" "vsv_get_conn_err"
    "vsv_make_client" "vsv_make_server" "vsv_up_connections"
    "vsv_wait_for_done" "vsv_wait_for_input"
    "wait_child" "wait_var"
    ;; string class methods
    "atobin" "atohex" "atoi" "atooct" "backref" "bintostr"
    "get_status" "get_status_msg" "getc" "itoa" "len" "match"
    "postmatch" "prematch" "putc" "search" "substr" "thismatch"
    )
  "List of Vera predefined system functions, tasks and class methods.")

(defconst vera-constants
  '(
    "stderr" "stdin" "stdout"
    "ALL" "ANY" "BAD_STATE" "BAD_TRANS"
    "CHECK" "CHGEDGE" "CLEAR" "CROSS" "CROSS_TRANS" "DEBUG" "DELETE"
    "EC_ARRAYX" "EC_CODE_END" "EC_CONFLICT" "EC_EXPECT" "EC_FULLEXPECT"
    "EC_MBXTMOUT" "EC_NEXPECT" "EC_RETURN" "EC_RGNTMOUT" "EC_SCONFLICT"
    "EC_SEMTMOUT" "EC_SEXPECT" "EC_SFULLEXPECT" "EC_SNEXTPECT" "EC_USERSET"
    "EQ" "EVENT"
    "FIRST" "GE" "GOAL" "GT" "HAND_SHAKE" "HI" "HIGH" "HNUM"
    "LE" "LIC_EXIT" "LIC_PRERR" "LIC_PRWARN" "LIC_WAIT" "LO" "LOAD" "LOW" "LT"
    "MAILBOX" "NAME" "NEGEDGE" "NEXT" "NO_OVERLAP" "NO_WAIT" "NUM" "NUM_BIN"
    "OFF" "ON" "ONE_BLAST" "ONE_SHOT" "ORDER" "POSEDGE" "PROGRAM"
    "RAWIN" "REGION" "REPORT"
    "SAVE" "SEMAPHORE" "SET" "SILENT" "STATE" "STR" "STR_ERR_OUT_OF_RANGE"
    "STR_ERR_REGEXP_SYNTAX" "SUM"
    "TRANS" "VERBOSE" "WAIT"
    )
  "List of Vera predefined constants.")

;; `regexp-opt' undefined (`xemacs-devel' not installed)
(unless (fboundp 'regexp-opt)
  (defun regexp-opt (strings &optional paren)
    (let ((open (if paren "\\(" "")) (close (if paren "\\)" "")))
      (concat open (mapconcat 'regexp-quote strings "\\|") close))))

(defconst vera-keywords-regexp
  (concat "\\<\\(" (regexp-opt vera-keywords) "\\)\\>")
  "Regexp for Vera keywords.")

(defconst vera-types-regexp
  (concat "\\<\\(" (regexp-opt vera-types) "\\)\\>")
  "Regexp for Vera predefined types.")

(defconst vera-q-values-regexp
  (concat "\\<\\(" (regexp-opt vera-q-values) "\\)\\>")
  "Regexp for Vera predefined VCA q_values.")

(defconst vera-functions-regexp
  (concat "\\<\\(" (regexp-opt vera-functions) "\\)\\>")
  "Regexp for Vera predefined system functions, tasks and class methods.")

(defconst vera-constants-regexp
  (concat "\\<\\(" (regexp-opt vera-constants) "\\)\\>")
  "Regexp for Vera predefined constants.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Font locking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; XEmacs compatibility
(when vera-xemacs
  (require 'font-lock)
  (copy-face 'font-lock-reference-face 'font-lock-constant-face)
  (copy-face 'font-lock-preprocessor-face 'font-lock-builtin-face))

(defun vera-font-lock-match-item (limit)
  "Match, and move over, any declaration item after point. Adapted from
`font-lock-match-c-style-declaration-item-and-skip-to-next'."
  (condition-case nil
      (save-restriction
	(narrow-to-region (point-min) limit)
	;; match item
	(when (looking-at "\\s-*\\(\\w+\\)")
	  (save-match-data
	    (goto-char (match-end 1))
	    ;; move to next item
	    (if (looking-at "\\(\\s-*\\(\\[[^]]*\\]\\s-*\\)?,\\)")
		(goto-char (match-end 1))
	      (end-of-line) t))))
    (error t)))

(defvar vera-font-lock-keywords
  (list
   ;; highlight keywords
   (list vera-keywords-regexp 1 'font-lock-keyword-face)
   ;; highlight types
   (list vera-types-regexp 1 'font-lock-type-face)
   ;; highlight constants
   (list vera-constants-regexp 1 'font-lock-constant-face)
   ;; highlight q_values
   (list vera-q-values-regexp 1 'font-lock-constant-face)
   ;; highlight predefined functions, tasks and methods
   (list vera-functions-regexp 1 'vera-font-lock-function-face)
   ;; highlight functions
   '("\\<\\(\\w+\\)\\s-*(" 1 font-lock-function-name-face)
   ;; highlight various declaration names
   '("^\\s-*\\(port\\|program\\|task\\)\\s-+\\(\\w+\\)\\>"
     2 font-lock-function-name-face)
   '("^\\s-*bind\\s-+\\(\\w+\\)\\s-+\\(\\w+\\)\\>"
     (1 font-lock-function-name-face) (2 font-lock-function-name-face))
   ;; highlight interface declaration names
   '("^\\s-*\\(class\\|interface\\)\\s-+\\(\\w+\\)\\>"
     2 vera-font-lock-interface-face)
   ;; highlight variable name definitions
   (list (concat "^\\s-*" vera-types-regexp "\\s-*\\(\\[[^]]+\\]\\s-+\\)?")
	 '(vera-font-lock-match-item nil nil (1 font-lock-variable-name-face)))
   ;; highlight numbers
   '("\\([0-9]*'[bdoh][0-9a-fA-FxXzZ_]+\\)" 1 vera-font-lock-number-face)
   ;; highlight filenames in #include directives
   '("^#\\s-*include\\s-*\\(<[^>\"\n]*>?\\)"
     1 font-lock-string-face)
   ;; highlight directives and directive names
   '("^#\\s-*\\(\\w+\\)\\>[ \t!]*\\(\\w+\\)?"
     (1 font-lock-builtin-face) (2 font-lock-variable-name-face nil t))
   ;; highlight `@', `$' and `#'
   '("\\([@$#]\\)" 1 font-lock-keyword-face)
   ;; highlight @ and # definitions
;   '("#\\(-?\\w+\\)" 1 vera-font-lock-number-face)
   '("@\\s-*\\(\\w*\\)\\(\\s-*,\\s-*\\(\\w+\\)\\)?\\>[^.]"
     (1 vera-font-lock-number-face) (3 vera-font-lock-number-face nil t))
   ;; highlight interface signal name
   '("\\(\\w+\\)\\.\\w+" 1 vera-font-lock-interface-face)
   )
  "Regular expressions to highlight in Vera Mode.")

(defvar vera-font-lock-number-face 'vera-font-lock-number-face
  "Face name to use for @ definitions.")

(defvar vera-font-lock-function-face 'vera-font-lock-function-face
  "Face name to use for predefined functions and tasks.")

(defvar vera-font-lock-interface-face 'vera-font-lock-interface-face
  "Face name to use for interface names.")

(defface vera-font-lock-number-face
  '((((class color) (background light)) (:foreground "Gold4"))
    (((class color) (background dark)) (:foreground "BurlyWood1"))
;   '((((class color) (background light)) (:foreground "SaddleBrown"))
;     (((class color) (background dark)) (:foreground "BurlyWood"))
    (t (:italic t :bold t)))
  "Font lock mode face used to highlight @ definitions."
  :group 'font-lock-highlighting-faces)

(defface vera-font-lock-function-face
  '((((class color) (background light)) (:foreground "DarkCyan"))
    (((class color) (background dark)) (:foreground "Orchid1"))
    (t (:italic t :bold t)))
  "Font lock mode face used to highlight predefined functions and tasks."
  :group 'font-lock-highlighting-faces)

(defface vera-font-lock-interface-face
  '((((class color) (background light)) (:foreground "Grey40"))
    (((class color) (background dark)) (:foreground "Grey80"))
    (t (:italic t :bold t)))
  "Font lock mode face used to highlight interface names."
  :group 'font-lock-highlighting-faces)

(defun vera-fontify-buffer ()
  "Fontify buffer."
  (interactive)
  (font-lock-fontify-buffer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Indentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar vera-echo-syntactic-information-p nil
  "If non-nil, syntactic info is echoed when the line is indented.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; offset functions

(defconst vera-offsets-alist
  '((comment        . vera-lineup-C-comments)
    (string         . -1000)
    (directive      . -1000)
    (block-open     . 0)
    (block-intro    . +)
    (block-close    . 0)
    (arglist-intro  . +)
    (arglist-cont   . +)
    (arglist-cont-nonempty . 0)
    (arglist-close  . 0)
    (statement      . 0)
    (statement-cont . +)
    (substatement   . +)
    (else-clause    . 0))
  "Association list of syntactic element symbols and indentation offsets.
Adapted from `c-offsets-alist'.")

(defun vera-evaluate-offset (offset langelem symbol)
  "Offset can be a number, a function, a variable, a list, or one of
the symbols + or -."
  (cond
   ((eq offset '+)         (setq offset vera-basic-offset))
   ((eq offset '-)         (setq offset (- vera-basic-offset)))
   ((eq offset '++)        (setq offset (* 2 vera-basic-offset)))
   ((eq offset '--)        (setq offset (* 2 (- vera-basic-offset))))
   ((eq offset '*)         (setq offset (/ vera-basic-offset 2)))
   ((eq offset '/)         (setq offset (/ (- vera-basic-offset) 2)))
   ((functionp offset)     (setq offset (funcall offset langelem)))
   ((listp offset)
    (setq offset
	  (let (done)
	    (while (and (not done) offset)
	      (setq done (vera-evaluate-offset (car offset) langelem symbol)
		    offset (cdr offset)))
	    (if (not done)
		0
	      done))))
   ((not (numberp offset)) (setq offset (symbol-value offset))))
  offset)

(defun vera-get-offset (langelem)
  "Get offset from LANGELEM which is a cons cell of the form:
\(SYMBOL . RELPOS).  The symbol is matched against
vera-offsets-alist and the offset found there is either returned,
or added to the indentation at RELPOS.  If RELPOS is nil, then
the offset is simply returned."
  (let* ((symbol (car langelem))
	 (relpos (cdr langelem))
	 (match  (assq symbol vera-offsets-alist))
	 (offset (cdr-safe match)))
    (if (not match)
	(setq offset 0
	      relpos 0)
      (setq offset (vera-evaluate-offset offset langelem symbol)))
    (+ (if (and relpos
		(< relpos (save-excursion (beginning-of-line) (point))))
	   (save-excursion
	     (goto-char relpos)
	     (current-column))
	 0)
       (vera-evaluate-offset offset langelem symbol))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; help functions

(defsubst vera-point (position)
  "Returns the value of point at certain commonly referenced POSITIONs.
POSITION can be one of the following symbols:
  bol  -- beginning of line
  eol  -- end of line
  boi  -- back to indentation
  ionl -- indentation of next line
  iopl -- indentation of previous line
  bonl -- beginning of next line
  bopl -- beginning of previous line
This function does not modify point or mark."
  (save-excursion
    (cond
     ((eq position 'bol)  (beginning-of-line))
     ((eq position 'eol)  (end-of-line))
     ((eq position 'boi)  (back-to-indentation))
     ((eq position 'bonl) (forward-line 1))
     ((eq position 'bopl) (forward-line -1))
     ((eq position 'iopl) (forward-line -1) (back-to-indentation))
     ((eq position 'ionl) (forward-line 1) (back-to-indentation))
     (t (error "Unknown buffer position requested: %s" position)))
    (point)))

(defun vera-in-literal (&optional lim)
  "Determine if point is in a Vera literal."
  (save-excursion
    (let ((state (parse-partial-sexp (or lim (point-min)) (point))))
      (cond
       ((nth 3 state) 'string)
       ((nth 4 state) 'comment)
       (t nil)))))

(defun vera-in-comment-p ()
  "Determine if point is in a Vera comment."
  (save-excursion
    (re-search-backward "\\(/\\*\\)\\|\\(\\*/\\)" nil t)
    (match-string 1)))

(defun vera-skip-forward-literal ()
  "Skip forward literal and return t if within one."
  (let ((state (save-excursion (parse-partial-sexp (point-min) (point)))))
    (cond
     ((nth 3 state) (search-forward "\"") t) ; inside string
     ((nth 7 state) (forward-line 1) t)	     ; inside // comment
     ((nth 4 state) (search-forward "*/") t) ; inside /* */ comment
     (t nil))))

(defun vera-skip-backward-literal ()
  "Skip backward literal and return t if within one."
  (let ((state (save-excursion (parse-partial-sexp (point-min) (point)))))
    (cond
     ((nth 3 state) (search-backward "\"") t) ; inside string
     ((nth 7 state) (search-backward "//") t) ; inside // comment
     ((nth 4 state) (search-backward "/*") t) ; inside /* */ comment
     (t nil))))

(defsubst vera-re-search-forward (regexp &optional bound noerror)
  "Like `re-search-forward', but skips over matches in literals."
  (store-match-data '(nil nil))
  (while (and (re-search-forward regexp bound noerror)
	      (vera-skip-forward-literal)
	      (progn (store-match-data '(nil nil))
		     (if bound (< (point) bound) t))))
  (match-end 0))

(defsubst vera-re-search-backward (regexp &optional bound noerror)
  "Like `re-search-backward', but skips over matches in literals."
  (store-match-data '(nil nil))
  (while (and (re-search-backward regexp bound noerror)
	      (vera-skip-backward-literal)
	      (progn (store-match-data '(nil nil))
		     (if bound (> (point) bound) t))))
  (match-end 0))

(defun vera-forward-syntactic-ws (&optional lim skip-directive)
  "Forward skip of syntactic whitespace."
  (save-restriction
    (let* ((lim (or lim (point-max)))
	   (here lim)
	   (hugenum (point-max)))
      (narrow-to-region lim (point))
      (while (/= here (point))
	(setq here (point))
	(forward-comment hugenum)
	(when (and skip-directive (looking-at "^\\s-*#"))
	  (end-of-line))))))

(defun vera-backward-syntactic-ws (&optional lim skip-directive)
  "Backward skip over syntactic whitespace."
  (save-restriction
    (let* ((lim (or lim (point-min)))
	   (here lim)
	   (hugenum (- (point-max))))
      (when (< lim (point))
	(narrow-to-region lim (point))
	(while (/= here (point))
	  (setq here (point))
	  (forward-comment hugenum)
	  (when (and skip-directive
		     (save-excursion (back-to-indentation)
				     (= (following-char) ?\#)))
	    (beginning-of-line)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; comment indentation functions

(defsubst vera-langelem-col (langelem &optional preserve-point)
  "Convenience routine to return the column of langelem's relpos.
Leaves point at the relpos unless preserve-point is non-nil."
  (let ((here (point)))
    (goto-char (cdr langelem))
    (prog1 (current-column)
      (if preserve-point
	  (goto-char here)))))

(defun vera-lineup-C-comments (langelem)
  "Line up C block comment continuation lines.
Nicked from `c-lineup-C-comments'."
  (save-excursion
    (let ((here (point))
	  (stars (progn (back-to-indentation)
			(skip-chars-forward "*")))
	  (langelem-col (vera-langelem-col langelem)))
      (back-to-indentation)
      (if (not (re-search-forward "/\\([*]+\\)" (vera-point 'eol) t))
	  (progn
	    (if (not (looking-at "[*]+"))
		(progn
		  ;; we now have to figure out where this comment begins.
		  (goto-char here)
		  (back-to-indentation)
		  (if (looking-at "[*]+/")
		      (progn (goto-char (match-end 0))
			     (forward-comment -1))
		    (goto-char (cdr langelem))
		    (back-to-indentation))))
	    (- (current-column) langelem-col))
	(if (zerop stars)
	    (progn
	      (skip-chars-forward " \t")
	      (- (current-column) langelem-col))
	  ;; how many stars on comment opening line?  if greater than
	  ;; on current line, align left.  if less than or equal,
	  ;; align right.  this should also pick up Javadoc style
	  ;; comments.
	  (if (> (length (match-string 1)) stars)
	      (progn
		(back-to-indentation)
		(- (current-column) -1 langelem-col))
	    (- (current-column) stars langelem-col)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; move functions

(defconst vera-beg-block-re "{\\|\\<\\(begin\\|fork\\)\\>")

(defconst vera-end-block-re "}\\|\\<\\(end\\|join\\(\\s-+\\(all\\|any\\|none\\)\\)?\\)\\>")

(defconst vera-beg-substatement-re "\\<\\(else\\|for\\|if\\|repeat\\|while\\)\\>")

(defun vera-corresponding-begin ()
  "Find corresponding block begin if cursor is at a block end."
  (while (and (vera-re-search-backward
	       (concat "\\(" vera-end-block-re "\\)\\|" vera-beg-block-re)
	       nil t)
	      (match-string 1))
    (vera-corresponding-begin))
  (vera-beginning-of-substatement))

(defun vera-corresponding-if ()
  "Find corresponding `if' if cursor is at `else'."
  (while (and (vera-re-search-backward "}\\|\\<\\(if\\|else\\)\\>" nil t)
	      (not (equal (match-string 0) "if")))
    (if (equal (match-string 0) "else")
	(vera-corresponding-if)
      (forward-char)
      (backward-sexp))))

(defun vera-beginning-of-statement ()
  "Go to beginning of current statement."
  (let (pos)
    (while
	(progn
	  ;; search for end of previous statement
	  (while
	      (and (vera-re-search-backward
		    (concat "[);]\\|" vera-beg-block-re
			    "\\|" vera-end-block-re) nil t)
		   (equal (match-string 0) ")"))
	    (forward-char)
	    (backward-sexp))
	  (setq pos (match-beginning 0))
	  ;; go back to beginning of current statement
	  (goto-char (or (match-end 0) 0))
	  (vera-forward-syntactic-ws nil t)
	  (when (looking-at "(")
	    (forward-sexp)
	    (vera-forward-syntactic-ws nil t))
	  ;; if "else" found, go to "if" and search again
	  (when (looking-at "\\<else\\>")
	    (vera-corresponding-if)
	    (setq pos (point))
	    t))
      ;; if search is repeated, go to beginning of last search
      (goto-char pos))))

(defun vera-beginning-of-substatement ()
  "Go to beginning of current substatement."
  (let ((lim (point))
	pos)
  ;; go to beginning of statement
    (vera-beginning-of-statement)
    (setq pos (point))
    ;; go forward all substatement opening statements until at LIM
    (while (and (< (point) lim)
		(vera-re-search-forward vera-beg-substatement-re lim t))
      (setq pos (match-beginning 0)))
    (vera-forward-syntactic-ws nil t)
    (when (looking-at "(")
      (forward-sexp)
      (vera-forward-syntactic-ws nil t))
    (when (< (point) lim)
      (setq pos (point)))
    (goto-char pos)))

(defun vera-forward-statement ()
  "Move forward one statement."
  (interactive)
  (while (and (vera-re-search-forward
	       (concat "[(;]\\|" vera-beg-block-re "\\|" vera-end-block-re)
	       nil t)
	      (equal (match-string 0) "("))
    (backward-char)
    (forward-sexp))
  (vera-beginning-of-substatement))

(defun vera-backward-statement ()
  "Move backward one statement."
  (interactive)
  (vera-backward-syntactic-ws nil t)
  (unless (= (preceding-char) ?\))
    (backward-char))
  (vera-beginning-of-substatement))

(defun vera-forward-same-indent ()
  "Move forward to next line with same indent."
  (interactive)
  (let ((pos (point))
	(indent (current-indentation)))
    (beginning-of-line 2)
    (while (and (not (eobp))
		(or (looking-at "^\\s-*$")
		    (> (current-indentation) indent)))
      (beginning-of-line 2))
    (if (= (current-indentation) indent)
	(back-to-indentation)
      (message "No following line with same indent found in this block")
      (goto-char pos))))

(defun vera-backward-same-indent ()
  "Move backward to previous line with same indent."
  (interactive)
  (let ((pos (point))
	(indent (current-indentation)))
    (beginning-of-line -0)
    (while (and (not (bobp))
		(or (looking-at "^\\s-*$")
		    (> (current-indentation) indent)))
      (beginning-of-line -0))
    (if (= (current-indentation) indent)
	(back-to-indentation)
      (message "No preceding line with same indent found in this block")
      (goto-char pos))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syntax analysis

(defmacro vera-add-syntax (symbol &optional relpos)
  "A simple macro to append the syntax in symbol to the syntax list.
try to increase performance by using this macro."
  `(setq syntax (cons (cons ,symbol ,(or relpos 0)) syntax)))

(defun vera-guess-basic-syntax ()
  "Determine syntactic context of current line of code."
  (save-excursion
    (beginning-of-line)
    (let (syntax state placeholder pos)
      ;; determine syntax state
      (setq state (parse-partial-sexp (point-min) (point)))
      (cond
       ;; CASE 1: in a comment?
       ((nth 4 state)
	;; skip empty lines
	(while (and (zerop (forward-line -1))
		    (looking-at "^\\s-*$")))
	(vera-add-syntax 'comment (vera-point 'boi)))
       ;; CASE 2: in a string?
       ((nth 3 state)
	(vera-add-syntax 'string))
       ;; CASE 3: at a directive?
       ((save-excursion (back-to-indentation) (= (following-char) ?\#))
	(vera-add-syntax 'directive (point)))
       ;; CASE 4: after an opening parenthesis (argument list continuation)?
       ((= (char-after (nth 1 state)) ?\()
	(goto-char (1+ (nth 1 state)))
	;; is there code after the opening parenthesis on the same line?
	(if (looking-at "\\s-*$")
	    (vera-add-syntax 'arglist-cont (vera-point 'boi))
	  (vera-add-syntax 'arglist-cont-nonempty (point))))
       ;; CASE 5: at a block closing?
       ((save-excursion (back-to-indentation) (looking-at vera-end-block-re))
	;; look for the corresponding begin
	(vera-corresponding-begin)
	(vera-add-syntax 'block-close (vera-point 'boi)))
       ;; CASE 6: at a block intro (the first line after a block opening)?
       ((and (save-excursion
	       (vera-backward-syntactic-ws nil t)
	       ;; previous line ends with a block opening?
	       (or (/= (skip-chars-backward "{") 0) (backward-word 1))
	       (when (looking-at vera-beg-block-re)
		 ;; go to beginning of substatement
		 (vera-beginning-of-substatement)
		 (setq placeholder (point))))
	     ;; not if "fork" is followed by "{"
	     (save-excursion
	       (not (and (progn (back-to-indentation) (looking-at "{"))
			 (progn (goto-char placeholder)
				(looking-at "\\<fork\\>"))))))
	(goto-char placeholder)
	(vera-add-syntax 'block-intro (vera-point 'boi)))
       ;; CASE 7: at the beginning of an else clause?
       ((save-excursion (back-to-indentation) (looking-at "\\<else\\>"))
	;; find corresponding if
	(vera-corresponding-if)
	(vera-add-syntax 'else-clause (vera-point 'boi)))
       ;; CASE 8: at the beginning of a statement?
       ;; is the previous command completed?
       ((or (save-excursion
	      (vera-backward-syntactic-ws nil t)
	      (setq placeholder (point))
	      ;; at the beginning of the buffer?
	      (or (bobp)
		  ;; previous line ends with a semicolon or
		  ;; is a block opening or closing?
		  (when (or (/= (skip-chars-backward "{};") 0)
			    (progn (back-to-indentation)
				   (looking-at (concat vera-beg-block-re "\\|"
						       vera-end-block-re))))
		    ;; if at a block closing, go to beginning
		    (when (looking-at vera-end-block-re)
		      (vera-corresponding-begin))
		    ;; go to beginning of the statement
		    (vera-beginning-of-statement)
		    (setq placeholder (point)))
		  ;; at a directive?
		  (when (progn (back-to-indentation) (looking-at "#"))
		    ;; go to previous statement
		    (vera-beginning-of-statement)
		    (setq placeholder (point)))))
	    ;; at a block opening?
	    (when (save-excursion (back-to-indentation)
				  (looking-at vera-beg-block-re))
	      ;; go to beginning of the substatement
	      (vera-beginning-of-substatement)
	      (setq placeholder (point))))
	(goto-char placeholder)
	(vera-add-syntax 'statement (vera-point 'boi)))
       ;; CASE 9: at the beginning of a substatement?
       ;; is this line preceeded by a substatement opening statement?
       ((save-excursion (vera-backward-syntactic-ws nil t)
			(when (= (preceding-char) ?\)) (backward-sexp))
			(backward-word 1)
			(setq placeholder (point))
			(looking-at vera-beg-substatement-re))
	(goto-char placeholder)
	(vera-add-syntax 'substatement (vera-point 'boi)))
       ;; CASE 10: it must be a statement continuation!
       (t
	;; go to beginning of statement
	(vera-beginning-of-substatement)
	(vera-add-syntax 'statement-cont (vera-point 'boi))))
      syntax)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; indentation functions

(defun vera-indent-line ()
  "Indent the current line as Vera code. Optional SYNTAX is the
syntactic information for the current line. Returns the amount of
indentation change (in columns)."
  (interactive)
  (let* ((syntax (vera-guess-basic-syntax))
	 (pos (- (point-max) (point)))
	 (indent (apply '+ (mapcar 'vera-get-offset syntax)))
	 (shift-amt  (- (current-indentation) indent)))
    (when vera-echo-syntactic-information-p
      (message "syntax: %s, indent= %d" syntax indent))
    (unless (zerop shift-amt)
      (beginning-of-line)
      (delete-region (point) (vera-point 'boi))
      (indent-to indent))
    (if (< (point) (vera-point 'boi))
	(back-to-indentation)
      ;; If initial point was within line's indentation, position after
      ;; the indentation.  Else stay at same point in text.
      (when (> (- (point-max) pos) (point))
	(goto-char (- (point-max) pos))))
    shift-amt))

(defun vera-indent-buffer ()
  "Indent whole buffer as Vera code.
Calls `indent-region' for whole buffer."
  (interactive)
  (message "Indenting buffer...")
  (indent-region (point-min) (point-max) nil)
  (message "Indenting buffer...done"))

(defun vera-indent-region (start end column)
  "Indent region as Vera code."
  (interactive "r\nP")
  (message "Indenting region...")
  (indent-region start end column)
  (message "Indenting region...done"))

(defsubst vera-indent-block-closing ()
  "If previous word is a block closing or `else', indent line again."
  (when (= (char-syntax (preceding-char)) ?w)
    (save-excursion
      (backward-word 1)
      (when (and (not (vera-in-literal))
		 (looking-at (concat vera-end-block-re "\\|\\<else\\>")))
	(vera-indent-line)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; electrifications

(defun vera-electric-tab (&optional prefix-arg)
  "If preceeding character is part of a word or a paren then hippie-expand,
else if right of non whitespace on line then tab-to-tab-stop,
else if last command was a tab or return then dedent one step or if a comment
toggle between normal indent and inline comment indent,
else indent `correctly'."
  (interactive "*P")
  (cond ((memq (char-syntax (preceding-char)) '(?w ?_))
	 (let ((case-fold-search t)
	       (case-replace nil))
	   (vera-expand-abbrev prefix-arg)))
	((> (current-column) (current-indentation))
	 (tab-to-tab-stop))
	((and (or (eq last-command 'vera-electric-tab)
		  (eq last-command 'vera-electric-return))
	      (/= 0 (current-indentation)))
	 (backward-delete-char-untabify vera-basic-offset nil))
	(t (vera-indent-line)))
  (setq this-command 'vera-electric-tab))

(defun vera-electric-return ()
  "Insert newline and indent.  Indent current line if it is a block closing."
  (interactive)
  (vera-indent-block-closing)
  (newline-and-indent))

(defun vera-electric-space (arg)
  "Insert a space.  Indent current line if it is a block closing."
  (interactive "*P")
  (unless arg
    (vera-indent-block-closing))
  (self-insert-command (prefix-numeric-value arg)))

(defun vera-electric-opening-brace (arg)
  "Outdent opening brace."
  (interactive "*P")
  (self-insert-command (prefix-numeric-value arg))
  (unless arg
    (vera-indent-line)))

(defun vera-electric-closing-brace (arg)
  "Outdent closing brace."
  (interactive "*P")
  (self-insert-command (prefix-numeric-value arg))
  (unless arg
    (vera-indent-line)))

(defun vera-electric-pound (arg)
  "Insert `#' and indent as directive it first character of line."
  (interactive "*P")
  (self-insert-command (prefix-numeric-value arg))
  (unless arg
    (save-excursion
      (backward-char)
      (skip-syntax-backward "-")
      (when (bolp)
	(delete-horizontal-space)))))

(defun vera-electric-star (arg)
  "Insert a star character.  Nicked from `c-electric-star'."
  (interactive "*P")
  (self-insert-command (prefix-numeric-value arg))
  (if (and (not arg)
	   (memq (vera-in-literal) '(comment))
	   (eq (char-before) ?*)
	   (save-excursion
	     (forward-char -1)
	     (skip-chars-backward "*")
	     (if (eq (char-before) ?/)
		 (forward-char -1))
	     (skip-chars-backward " \t")
	     (bolp)))
      (vera-indent-line)))

(defun vera-electric-slash (arg)
  "Insert a slash character.  Nicked from `c-electric-slash'."
  (interactive "*P")
  (let* ((ch (char-before))
	 (indentp (and (not arg)
		       (eq last-command-char ?/)
		       (or (and (eq ch ?/)
				(not (vera-in-literal)))
			   (and (eq ch ?*)
				(vera-in-literal))))))
    (self-insert-command (prefix-numeric-value arg))
    (when indentp
      (vera-indent-line))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hippie expand customization (for expansion of Vera commands)

(defvar vera-abbrev-list
  (append (list nil) vera-keywords
	  (list nil) vera-types
	  (list nil) vera-functions
	  (list nil) vera-constants)
  "Predefined abbreviations for Vera.")

(defvar vera-expand-upper-case nil)

(eval-when-compile (require 'hippie-exp))

(defun vera-try-expand-abbrev (old)
  "Try expanding abbreviations from `vera-abbrev-list'."
  (unless old
    (he-init-string (he-dabbrev-beg) (point))
    (setq he-expand-list
	  (let ((abbrev-list vera-abbrev-list)
		(sel-abbrev-list '()))
	    (while abbrev-list
	      (when (or (not (stringp (car abbrev-list)))
			(string-match
			 (concat "^" he-search-string) (car abbrev-list)))
		(setq sel-abbrev-list
		      (cons (car abbrev-list) sel-abbrev-list)))
	      (setq abbrev-list (cdr abbrev-list)))
	    (nreverse sel-abbrev-list))))
  (while (and he-expand-list
	      (or (not (stringp (car he-expand-list)))
		  (he-string-member (car he-expand-list) he-tried-table t)))
;		  (equal (car he-expand-list) he-search-string)))
    (unless (stringp (car he-expand-list))
      (setq vera-expand-upper-case (car he-expand-list)))
    (setq he-expand-list (cdr he-expand-list)))
  (if (null he-expand-list)
      (progn (when old (he-reset-string))
	     nil)
    (he-substitute-string
     (if vera-expand-upper-case
	 (upcase (car he-expand-list))
       (car he-expand-list))
     t)
    (setq he-expand-list (cdr he-expand-list))
    t))

;; function for expanding abbrevs and dabbrevs
(defun vera-expand-abbrev (arg))
(fset 'vera-expand-abbrev (make-hippie-expand-function
			       '(try-expand-dabbrev
				 try-expand-dabbrev-all-buffers
				 vera-try-expand-abbrev)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comments

(defun vera-comment-uncomment-region (beg end &optional arg)
  "Comment region if not commented, uncomment region if already commented."
  (interactive "r\nP")
  (goto-char beg)
  (if (looking-at (regexp-quote comment-start))
      (comment-region beg end -1)
    (comment-region beg end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Help functions

(defun vera-customize ()
  "Call the customize function with `vera' as argument."
  (interactive)
  (customize-browse 'vera))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other

;; remove ".vr" from `completion-ignored-extensions'
(setq completion-ignored-extensions
      (delete ".vr" completion-ignored-extensions))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bug reports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst vera-mode-help-address "Vera Mode Maintainer <reto@gnu.org>"
  "Address for Vera Mode bug reports.")

;; get reporter-submit-bug-report when byte-compiling
(eval-when-compile
  (require 'reporter))

(defun vera-submit-bug-report ()
  "Submit via mail a bug report on Vera Mode."
  (interactive)
  ;; load in reporter
  (and
   (y-or-n-p "Do you want to submit a report on Vera Mode? ")
   (require 'reporter)
   (reporter-submit-bug-report
    vera-mode-help-address
    (concat "Vera Mode " vera-version)
    (list
     ;; report all important variables
     'vera-basic-offset
     'vera-underscore-is-part-of-word
     )
    nil nil
    "Dear Vera Mode maintainer,")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Documentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vera-version ()
  "Echo the current version of Vera Mode in the minibuffer."
  (interactive)
  (message "Using Vera Mode version %s" vera-version))

(defun vera-doc-mode ()
  "Display Vera Mode documentation in *Help* buffer."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ mode-name)
    (princ " mode:\n")
    (princ (documentation 'vera-mode))
    (unless vera-xemacs
      (help-setup-xref (list #'vera-doc-mode) (interactive-p)))
    (save-excursion
      (set-buffer standard-output)
      (help-mode))
    (print-help-return-message)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'vera-mode)

;;; vera-mode.el ends here
