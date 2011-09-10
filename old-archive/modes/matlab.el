;;; matlab.el --- major mode for MATLAB dot-m files
;;
;; Author: Matt Wette <mwette@alumni.caltech.edu>
;; Maintainer: Matt Wette <mwette@alumni.caltech.edu>
;; Created: 04 Jan 91
;; Version: 1.09.0
;; Keywords: matlab, octave
;;
;; LCD Archive Entry:
;; matlab|Matt Wette|mwette@alumni.caltech.edu|
;; Major mode for MATLAB dot-m files|
;; 07-Mar-1997|1.09.0|~/modes/matlab.el.gz|
;;
;; Copyright (C) 1991-1997 Matthew R. Wette
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;;; Commentary:
;;
;; This major mode for GNU emacs provides support for editing MATLAB dot-m
;; files.  It automatically indents for block structures, line continuations
;; (e.g., ...), and comments.  The usual paren matching support is included.
;; Filling and auto-fill works for comment lines.
;;
;; You may wish to add something like the following to your ~/.emacs file:
;;   (autoload 'matlab-mode "matlab" "Enter Matlab mode." t)
;;   (setq auto-mode-alist (cons '("\\.m$" . matlab-mode) auto-mode-alist))
;;   (defun my-matlab-mode-hook ()
;;     (setq matlab-function-indent t)	; if you want function bodies indented
;;     (setq fill-column 76)		; where auto-fill should wrap
;;     (turn-on-auto-fill))
;;   (setq matlab-mode-hook 'my-matlab-mode-hook)
;; Put this file in the emacs load-path so emacs can find it (check the manual).
;; To get font-lock try adding
;;     (font-lock-mode 1)
;; To get hilit19 support try adding
;;     (matlab-mode-hilit)
;;
;; This version now assumes that you put whitespace immediately after `%' in
;; your comments.  The fact that MATLAB uses single-quote for strings and
;; tranpose makes parsing strings almost impossible.  I have implemented some
;; heuristics to make matlab-mode work with strings better, but ya never know.
;;
;; Indent places the beginning of the entire comment.  If placing a new 
;; comment on the line use fill-prefix from previous line.  If comment
;; already exists keep its existing justification.
;;

;;; Code:

;;(setq debug-on-error t)


;;; user-changable variables ==================================================

;; Variables which the user can change
(defvar matlab-indent-level 2
  "*The indentation in matlab-mode.")

(defvar matlab-cont-level 4
  "*Continuation indent.")

(defvar matlab-fill-code t
  "*If true, auto-fill-mode causes code lines to be automatically continued.")

(defvar matlab-comment-column 40
  "*The goal comment column in matlab-mode buffers.")

(defvar matlab-comment-line-s "% "
  "*String to start comment on line by itself.")

(defvar matlab-comment-on-line-s "% "
  "*String to start comment on line with code.")

(defvar matlab-comment-region-s "% $$$ "
  "*String inserted by \\[matlab-comment-region] at start of each line in \
region.")

(defvar matlab-indent-function nil
  "*If t, indent body of function.")

(defvar matlab-vers-on-startup t
  "*If non-nil, shows the version number on startup.")

(defvar matlab-shell-save-and-go-history '("()")
  "Keeps track of paramters passed to the matlab shell.")

;;; matlab-mode variables =====================================================

;; syntax table
(defvar matlab-mode-syntax-table nil
  "the syntax table used in matlab-mode buffers")

(if matlab-mode-syntax-table
    ()
  ;; changes by Mats Bengtsson
  (setq matlab-mode-syntax-table (make-syntax-table (standard-syntax-table)))
  (modify-syntax-entry ?_  "_" matlab-mode-syntax-table)
  (modify-syntax-entry ?%  "<" matlab-mode-syntax-table)
  (modify-syntax-entry ?\\ "." matlab-mode-syntax-table)
  (modify-syntax-entry ?\t ">" matlab-mode-syntax-table)
  (modify-syntax-entry ?\n ">" matlab-mode-syntax-table)
  (modify-syntax-entry ?+  "." matlab-mode-syntax-table)
  (modify-syntax-entry ?-  "." matlab-mode-syntax-table)
  (modify-syntax-entry ?*  "." matlab-mode-syntax-table)
  (modify-syntax-entry ?'  "." matlab-mode-syntax-table)
  (modify-syntax-entry ?/  "." matlab-mode-syntax-table)
  (modify-syntax-entry ?=  "." matlab-mode-syntax-table)
  (modify-syntax-entry ?<  "." matlab-mode-syntax-table)
  (modify-syntax-entry ?>  "." matlab-mode-syntax-table)
  (modify-syntax-entry ?&  "." matlab-mode-syntax-table)
  (modify-syntax-entry ?|  "." matlab-mode-syntax-table)
  (set-syntax-table matlab-mode-syntax-table))

;; abbrev table
(defvar matlab-mode-abbrev-table nil
  "the abbrev table used in matlab-mode buffers")

(define-abbrev-table 'matlab-mode-abbrev-table ())

;; mode map
(defvar matlab-mode-map ()
  "the keymap used in matlab-mode")

(if matlab-mode-map
    ()
  (setq matlab-mode-map (make-sparse-keymap))
  (define-key matlab-mode-map "\r" 'matlab-return)
  (define-key matlab-mode-map "\C-j" 'matlab-linefeed)
  (define-key matlab-mode-map "\t" 'matlab-indent-line)
  (define-key matlab-mode-map "\M-;" 'matlab-comment)
  (define-key matlab-mode-map "\C-c;" 'matlab-comment-region)
  (define-key matlab-mode-map "\C-c\r" 'matlab-comment-return)
  (define-key matlab-mode-map "\C-cf" 'matlab-fill-comment-line)
  (define-key matlab-mode-map "\C-cj" 'matlab-justify-line)
  (define-key matlab-mode-map "\C-cq" 'matlab-fill-region)
  (define-key matlab-mode-map "\C-cs" 'matlab-shell-save-and-go)
  (define-key matlab-mode-map "\C-ct" 'matlab-show-line-info)
  (define-key matlab-mode-map "\M-\r" 'newline)
  (substitute-key-definition 'forward-sexp 'matlab-forward-sexp
			     matlab-mode-map) ; global-map)
  (substitute-key-definition 'backward-sexp 'matlab-backward-sexp
			     matlab-mode-map) ; global-map)
  (substitute-key-definition 'comment-region 'matlab-comment-region
			     matlab-mode-map) ; global-map) ;torkel
  )

;; font-lock keywords
(defvar matlab-font-lock-keywords
  (list
   ;; String quote chars are also used as transpose, but only if directly
   ;; after a non-deliminating character.  To quote a quote, put two
   ;; in a row, thus we need an anchored first quote. (Eric Ludlam)
   '("\\([ \t(),;]\\|^\\)\\('\\([^\n']\\|''\\)*'\\)"
     2 font-lock-string-face)
   ;; Comments must occur after the string, that way we can check to see
   ;; if the comment start char has occurred inside our string. (EL)
   ;; (match-beginning 1) doesn't work w/ xemacs -- use 0 instead
   '("\\(%[^%\n]*\\)"
     1 (if (eq (get-text-property (match-beginning 0) 'face)
               font-lock-string-face) nil
         font-lock-comment-face) keep)
   '("\\(^\\|[;,]\\)[ \t]*\\(global\\|for\\|while\\|if\\|elseif\\|else\\|end\
\\|return\\|switch\\|case\\|otherwise\\)\\b"
     2 font-lock-keyword-face)
    ;; handle graphics cool stuff
    '("\\<\\(\\figure\\|axes\\|line\\|surface\\|patch\\|text\\|light\\|\
image\\)\\>"
      1 font-lock-type-face)
   )
   "Expressions to hightlight in Matlab mode.")

;; Eric Ludlam's gaudy font-lock keywords
(defvar matlab-gaudy-font-lock-keywords
  (append
   matlab-font-lock-keywords
   (list
   ;; defining a function, assigned variables, and function name
   '("^\\(function\\)\\s-+\\([^=]+\\)\\s-*=\\s-*\\(\\sw+\\)\\s-*(" 
     (1 font-lock-keyword-face) (2 font-lock-variable-name-face)
     (3 font-lock-function-name-face))
   '("^\\(function\\)\\s-+\\(\\sw+\\)\\s-*(" 
     (1 font-lock-keyword-face) (2 font-lock-function-name-face))
   ;; Anchor on the function keyword, hightlight params
   '("^function\\([^=]+=\\)?\\s-*\\(\\sw+\\)+\\s-*("
     ("\\s-*\\(\\sw+\\)\\s-*[,|)]" nil nil 
      (1 font-lock-variable-name-face)))
    ;; I like variables for FOR loops
    '("\\<\\(for\\)\\s-+\\(\\sw+\\)\\s-*=\\s-*\\([^\n,]+\\)"
      (1 font-lock-keyword-face)
      (2 font-lock-variable-name-face) 
      (3 font-lock-reference-face))
    ;; Items after a switch statements are cool
    '("\\<\\(case\\|switch\\)\\s-+\\(.*\\)" 
      (1 font-lock-keyword-face) (2 font-lock-reference-face))
   ;; How about a few matlab constants such as pi, infinity, and sqrt(-1)?
   ;; The ^>> is in case I use this in an interactive mode someday
   '("\\<\\(pi\\|inf\\|Inf\\|NaN\\|nan\\|ans\\|i\\|j\\|^>>\\)\\>"
     1 font-lock-reference-face)
   ;; Define these as variables since this is about as close
   ;; as matlab gets to variables
   '("\\(set\\|get\\)\\s-*(\\s-*\\(\\w+\\)\\s-*\\(,\\|)\\)" 
     2 font-lock-variable-name-face)
   ))
  "Expressions to hightlight in Matlab mode.")


(defvar matlab-really-guady-font-lock-keywords
  (append
   matlab-gaudy-font-lock-keywords
   (list
    ;; Since it's a math language, how bout dem symbols?
    '("\\([<>~]=?\\|\\.[*^']\\|==\\|\\<xor\\>\\|[-!^&|*+\\/~:]\\)"
      1 font-lock-type-face)
    ;; How about the special help comments
    ;;'("function[^\n]+" 
    ;;  ("^%\\([^\n]+\\)\n" nil nil (1 font-lock-reference-face t)))
    ;; continuation elipses.  This will underline all valid elipses
    ;; according to matlab usage.
    '("[^.]\\(\\.\\.\\.\\)\\(\\s-*%\\|\n\\)" 1 'underline)
    ;; How about debugging statements?
    '("\\<\\(db\\sw+\\)\\>" 1 'bold)
    ;; Correct setting of the syntax table and other variables 
    ;; will automatically handle this 
    ;; '("%\\s-+.*" 0 font-lock-comment-face t)
    ))
  "Expressions to hightlight in Matlab mode.")

(defvar matlab-shell-font-lock-keywords
  (list
   ;; How about Errors?
   '("^\\(Error in\\|Syntax error in\\)\\s-+==>\\s-+\\(.+\\)$"
     (1 font-lock-comment-face) (2 font-lock-string-face))
   ;; and line numbers
   '("^\\(On line [0-9]+\\)" 1 font-lock-comment-face)
   ;; User beep things
   '("\\(\\?\\?\\?[^\n]+\\)" 1 font-lock-comment-face)
   ;; Useful user commands, but not useful programming constructs
   '("\\<\\(demo\\|whatsnew\\|info\\|subscribe\\|help\\|doc\\|lookfor\\|what\
\\|whos?\\|cd\\|clear\\|load\\|save\\)\\>"
     1 font-lock-keyword-face)
   ;; Various notices
   '(" M A T L A B (R) " 0 'underline)
   '("All Rights Reserved" 0 'italic)
   '("\\((c)\\s-+Copyright[^\n]+\\)" 1 font-lock-comment-face)
   '("\\(Version\\)\\s-+\\([^\n]+\\)"
     (1 font-lock-function-name-face) (2 font-lock-variable-name-face))
   )
  "Additional keywords used by matlab when reporting errors in interactive\
 mode.")


;; hilit19 patterns
(defvar matlab-hilit19-patterns
  '(
    ("\\(^\\|[^%]\\)\\(%[ \t].*\\|%\\)$" 2 comment)
    ("\\(^\\|[;,]\\)[ \t]*\\(\
function\\|global\\|for\\|while\\|if\\|elseif\\|else\\|end\\|return\
\\|switch\\|case\\|otherwise\\)\\b" 2 keyword)))


;;; matlab-mode entry point ==================================================

(defun matlab-mode ()
  "Matlab-mode is a major mode for editing MATLAB dot-m files.
This is version 1.09.0, dated 07Mar97.  This version
    +   will run matlab-mode-hook if it is non-nil
    +   supports font-lock and hilit19
    +   requires a whitespace (space, tab or newline) after % in comment

Special Key Bindings:
\\{matlab-mode-map}
Variables:
  matlab-indent-level		Level to indent blocks.
  matlab-comment-column         Goal column for on-line comments.
  fill-column			Column used in auto-fill.
  matlab-comment-line-s	        Sring to start comment line.
  matlab-comment-region-s	String to put comment lines in region.
  matlab-vers-on-startup	If t, show version on start-up.
  matlab-indent-function	If t, indents body of MATLAB functions.
  matlab-hilit19-patterns	Patterns for hilit19
  matlab-font-lock-keywords	Keywords for font-lock
  matlab-return-function	Function-variable to customize RET handling

Commands:
  matlab-mode                   Enter MATLAB major mode.
  matlab-return                 RET with post indenting.
  matlab-linefeed               RET with pre and post indent.
  matlab-comment-return		RET for next-line comment.
  matlab-indent-line            Indent line for structure.
  matlab-comment                Add comment to current line.
  matlab-comment-indent         Compute indent for comment.
  matlab-comment-region		Comment (with arg, uncomment) region.
  matlab-fill-region		Fill region (usually comments).
  matlab-justify-line		Delete space on end and justify.
  matlab-mode-hilit             Turn on hilit19.

To add automatic support put something like the following in your .emacs file:
  \(autoload 'matlab-mode \"matlab\" \"Enter Matlab mode.\" t\)
  \(setq auto-mode-alist \(cons '\(\"\\\\.m$\" . matlab-mode\) \
auto-mode-alist\)\)
  \(defun my-matlab-mode-hook \(\)
    \(setq fill-column 76\)
    \(font-lock-mode 1\)    
    \(turn-on-auto-fill\)\)
  \(setq matlab-mode-hook 'my-matlab-mode-hook\)"
  (interactive)
  (kill-all-local-variables)
  (use-local-map matlab-mode-map)
  (setq major-mode 'matlab-mode)
  (setq mode-name "Matlab")
  (setq local-abbrev-table matlab-mode-abbrev-table)
  (set-syntax-table matlab-mode-syntax-table)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'matlab-indent-line)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "%\\s-+")
  (make-local-variable 'comment-column)
  (setq comment-column 'matlab-comment-column)
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'matlab-comment-indent)
  (make-local-variable 'fill-column)
  (setq fill-column default-fill-column)
  (make-local-variable 'auto-fill-function)
  (setq auto-fill-function 'matlab-auto-fill)
  (make-local-variable 'fill-prefix)
  ;; give each file it's own paramter history
  (make-local-variable 'matlab-shell-save-and-go-history)
  ;;(make-local-variable 'font-lock-keywords)
  ;;(setq font-lock-keywords matlab-font-lock-keywords)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '((matlab-font-lock-keywords
			      matlab-gaudy-font-lock-keywords
			      matlab-really-guady-font-lock-keywords
			      )
			     t ; do not do string/comment highlighting
			     t ; keywords are case insensitive.
			     ;; This puts _ as a word constituant,
			     ;; simplifying our keywords significantly
			     ((?_ . "w"))))
  (if window-system (matlab-frame-init))
  (run-hooks 'matlab-mode-hook)
  (make-local-variable 'auto-fill-function) ; ?? move up some?
  (setq auto-fill-function 'matlab-auto-fill)
  (matlab-reset-vars)
  (if matlab-vers-on-startup (matlab-show-version)))


;;; regexps for MATLAB language ===============================================

;; "-pre" means "partial regular expression"

(defvar matlab-block-beg-pre-if "function\\|for\\|while\\|if\\|switch")
(defvar matlab-block-beg-pre-no-if "for\\|while\\|if\\|switch")

(defvar matlab-block-beg-pre 
  (if matlab-indent-function
      matlab-block-beg-pre-if matlab-block-beg-pre-no-if)
  "partial regular expression to recognize matlab block-begin keywords")

(defconst matlab-block-mid-pre 
  "elseif\\|else"
  "partial regular expression to recognize matlab mid-block keywords")

(defconst matlab-block-end-pre
  "end"
  "partial regular expression to recognize matlab block-end keywords")
  
(defconst matlab-other-pre
  "function\\|return"
  "partial regular express to recognize matlab non-block keywords")

(defconst matlab-endless-blocks
  "case\\|otherwise"
  "These keywords initialize new blocks, like a for or if, but don't have
thier own explicit ends (As such, are endless).  A new case or otherwise
will and a previous endless block, and and end will end this block, plus
any outside normal blocks.")

(defvar matlab-block-re
  (concat "\\(^\\|[;,]\\)[ \t]*\\("
	  matlab-block-beg-pre "\\|"
	  matlab-block-mid-pre "\\|"
	  matlab-block-end-pre "\\|"
	  matlab-endless-blocks "\\)\\b")
  "regular expression for keywords which begin matlab blocks")

(defvar matlab-block-scan-re
  (concat "\\<\\("
	  matlab-block-beg-pre "\\|"
	  matlab-block-end-pre "\\)\\b")
  "Expression used to scan over matching pairs of begin/ends")

(defconst matlab-block-beg-re
  (concat "\\(" matlab-block-beg-pre "\\)"))

(defconst matlab-block-mid-re
  (concat "\\(" matlab-block-mid-pre "\\)"))

(defconst matlab-block-end-re
  (concat "\\(" matlab-block-end-pre "\\)"))

(defconst matlab-endless-blocks-re
  (concat "\\(" matlab-endless-blocks "\\)"))

(defconst matlab-cline-start-skip "[ \t]*%[ \t]*"
  "*The regular expression for skipping comment start.")

(defun matlab-reset-vars ()
  (setq matlab-block-beg-pre 
	(if matlab-indent-function
	    matlab-block-beg-pre-if matlab-block-beg-pre-no-if))
  (setq matlab-block-re
	(concat "\\(^\\|[;,]\\)[ \t]*\\("
		matlab-block-beg-pre "\\|"
		matlab-block-mid-pre "\\|"
		matlab-block-end-pre "\\|"
		matlab-endless-blocks "\\)\\b")))


;;; utilities =================================================================

(defun matlab-show-version ()
  "Show the version number in the minibuffer."
  (interactive)
  (message "matlab-mode, version 1.09.0 dated 07Mar97"))

(defun matlab-find-prev-line ()
  (if (= -1 (forward-line -1)) nil
    (if (matlab-ltype-empty) (matlab-find-prev-line) t)))

(defun matlab-prev-line ()
  "Go to the previous line of code.  Return nil if not found."
  (interactive)
  (let ((old-point (point)))
    (if (matlab-find-prev-line) t (goto-char old-point) nil)))

(defun matlab-backward-sexp (&optional autoend)
  "Go from the current position backwards one balenced set of expressions
which can be quite complex in matlab.  If optional AUTOEND, then
pretend we are at an end."
  (interactive)
  (if (and (not autoend)
	   (save-excursion (backward-word 1) 
			   (or (not (looking-at matlab-block-end-re))
			       (matlab-cursor-in-string-or-comment))))
      ;; Go backwards one simple expression
      (forward-sexp -1)
    ;; otherwise go backwards recursively across balanced expressions
    ;; backup over our end
    (if (not autoend) (forward-word -1))
    (let ((done nil))
      (while (not done)
	(re-search-backward matlab-block-scan-re nil t)
	(if (looking-at matlab-block-end-re)
	    (if (matlab-cursor-in-string-or-comment)
		nil
	      ;; we must skip the expression and keep searching
	      (forward-word 1)
	      (matlab-backward-sexp))
	  (if (not (matlab-cursor-in-string-or-comment))
	      (setq done t)))))
    ))

(defun matlab-forward-sexp ()
  "Go from the current position forward one balenced set of expressions
which can be quite complex in matlab."
  (interactive)
  ;; skip over preceeding whitespace
  (skip-chars-forward "[ \t\n]+")
  (if (or (not (looking-at (concat "\\(\\s-\\|;\\|\\.\\)*\\(" 
				   matlab-block-beg-pre
				   "\\)\\>")))
	  (and (matlab-cursor-in-string-or-comment)
	       (not (looking-at "\\(\\s-\\|\\.\\)*$"))))
      ;; Go forwards one simple expression
      (forward-sexp 1)
    ;; otherwise go forwards recursively across balanced expressions
    (forward-word 1)
    (let ((done nil))
      (while (not done)
	(re-search-forward matlab-block-scan-re nil t)
	(goto-char (match-beginning 0))
	(if (looking-at matlab-block-beg-pre)
	    (if (matlab-cursor-in-string-or-comment)
		(forward-word 1)
	      ;; we must skip the expression and keep searching
	      (matlab-forward-sexp))
	  (forward-word 1)
	  (if (not (matlab-cursor-in-string-or-comment))
	      (setq done t)))))
    ))

;;; line types and attributes =================================================

(defun matlab-ltype-empty ()		; blank line
  "Returns t if current line is empty."
  (save-excursion
    (beginning-of-line)
    (looking-at "^[ \t]*$")))

(defun matlab-ltype-comm ()		; comment line
  "Returns t if current line is a MATLAB comment line."
  (save-excursion
    (beginning-of-line)
    (looking-at "[ \t]*%[ \t].*$")))

(defun matlab-ltype-code ()		; line of code
  "Return t if current line is a MATLAB code line."
  (and (not (matlab-ltype-empty)) (not (matlab-ltype-comm))))

(defun matlab-lattr-comm ()		; line has comment
  "Returns t if current line contains a comment."
  (save-excursion
    (beginning-of-line)
    (looking-at "\\(^\\|.*[^%]\\)%[ \t]")))

(defun matlab-lattr-cont ()		; line has continuation
  "Returns t if current line ends in ... and optional comment."
  (save-excursion
    (beginning-of-line)
    (re-search-forward "[^; \t.][ \t]*\\.\\.+[ \t]*\\(%.*\\)?$"
		       (position-at-eol) t)))

(defun matlab-cursor-in-string-or-comment ()
  "Returns t if the cursor is in a valid matlab comment or string"
  (save-restriction
    (narrow-to-region (save-excursion (beginning-of-line) (point))
		      (save-excursion (end-of-line) (point)))
    (let ((c (point)) (l 0) (rv nil))
      (or (save-excursion
	    ;; In a string if odd # quotes before point on line
	    (while (re-search-backward "'" nil t)
	      (setq l (1+ l)))
	    (= 1 (% l 2)))
	  (save-excursion
	    ;; In comment, if there is a % before us, and it is not in
	    ;; a string.  We already have a quote count, so use
	    ;; that to see if any % are outside of a string
	    (while (re-search-backward "%\\|'" nil t)
	      (if (looking-at "'")
		  (setq l (1- l))
		(if (= 1 (% l 2))
		    nil
		  (beginning-of-line)
		  (setq rv t))))
	    rv)
	  ))))

;;; indent functions ==========================================================

(defun matlab-indent-line ()
  "Indent a line in matlab-mode."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (delete-horizontal-space)
    (indent-to (matlab-calc-indent))
    ;; If line contains a comment, format it.
    (if () (if (matlab-lattr-comm) (matlab-comment))))
  (skip-chars-forward " \t%"))

(defun matlab-calc-indent ()
  "Return the appropriate indentation for this line as an int."
  (interactive)
  (let ((indent 0))
    (save-excursion
      (if (matlab-prev-line)
	  (setq indent (+ (current-indentation) (matlab-add-to-next)))))
    (setq indent (+ indent (matlab-add-from-prev)))
    indent))

(defun matlab-add-to-next ()
  (car (cdr (matlab-calc-deltas))))

(defun matlab-add-from-prev ()
  (car (matlab-calc-deltas)))

(defun matlab-calc-deltas ()
  "This routine returns the list (add-from-prev add-to-next)."
  (let ((add-from-prev 0) (add-to-next 0) eol)
    (if (matlab-ltype-comm) (list 0 0) 
      (save-excursion
	(setq eol (position-at-eol))
	;; indentation for control structures
	(beginning-of-line)
	(while (re-search-forward matlab-block-re eol t)
	  (save-excursion
	    (goto-char (match-beginning 2))
	    (if (looking-at matlab-block-beg-re)
		(setq add-to-next (+ add-to-next matlab-indent-level))
	      (if (> add-to-next 0)
		  (setq add-to-next (- add-to-next matlab-indent-level))
		(setq add-from-prev (- add-from-prev matlab-indent-level)))
	      (if (looking-at matlab-endless-blocks-re)
		  ;; With the introduction of switch statements, our current
		  ;; indentation is no-longer indicitive of the last opened
		  ;; block statement.  We must use the specialized forward/
		  ;; backward sexp to navigate over intervening blocks of
		  ;; code to learn our true indentation level.
		  (save-excursion
		    (let ((p (point)))
		      (setq add-to-next (+ add-to-next matlab-indent-level))
		      ;; Ok, the fun is over, now for some unpleasant scanning
		      (matlab-backward-sexp t)
		      (if (and 
			   (re-search-forward matlab-endless-blocks-re nil t)
			   (< p (point)))
			  (setq add-from-prev 
				(+ add-from-prev matlab-indent-level))))))
	      (if (looking-at matlab-block-end-re)
		  (save-excursion
		    (forward-word 1)
		    (matlab-backward-sexp)
		    (if (looking-at "switch")
			(setq add-from-prev 
			      (- add-from-prev matlab-indent-level)))))
	      (if (looking-at matlab-block-mid-re)
		  (setq add-to-next (+ add-to-next matlab-indent-level))))))
	;; indentation for matrix expressions
	(beginning-of-line)
	(while (re-search-forward "[][]" eol t)
	  (save-excursion
	    (goto-char (match-beginning 0))
	    (if (looking-at "\\[")
		(setq add-to-next (+ add-to-next matlab-indent-level))
	      (setq add-to-next (- add-to-next matlab-indent-level)))))
	;; continuation lines
	(if (matlab-lattr-cont)
	    (save-excursion
	      (if (= 0 (forward-line -1))
		  (if (matlab-lattr-cont)
		      ()
		    (setq add-to-next (+ add-to-next matlab-cont-level)))
		(setq add-to-next (+ add-to-next matlab-cont-level))))
	  (save-excursion
	    (if (= 0 (forward-line -1))
		(if (matlab-ltype-comm) ()
		  (if (matlab-lattr-cont)
		      (setq add-to-next (- add-to-next matlab-cont-level)))))))
	)
      (list add-from-prev add-to-next))))


;;; the return key ============================================================

(defvar matlab-return-function 'matlab-indent-end-before-ret
  "Function to handle return key.  Must be one of
    'matlab-plain-ret
    'matlab-indent-after-ret
    'matlab-indent-end-before-ret
    'matlab-indent-before-ret")

(defun matlab-return ()
  "Handle carriage return in matlab-mode."
  (interactive)
  (funcall matlab-return-function))

(defun matlab-plain-ret ()
  "Vanila new line."
  (interactive)
  (newline))
  
(defun matlab-indent-after-ret ()
  "Indent after new line."
  (interactive)
  (newline)
  (matlab-indent-line))

(defun matlab-indent-end-before-ret ()
  "Indent line if block end, start new line, and indent again."
  (interactive)
  (if (save-excursion
	(beginning-of-line)
	(looking-at "[ \t]*\\(elseif\\|else\\|end\\|case\\|otherwise\\)\\b"))
      (matlab-indent-line))
  (newline)
  (matlab-indent-line))

(defun matlab-indent-before-ret ()
  "Indent line, start new line, and indent again."
  (interactive)
  (matlab-indent-line)
  (newline)
  (matlab-indent-line))

(defun matlab-linefeed ()
  "Handle linefeed in matlab-mode.
Has effect of matlab-return with (not matlab-indent-before-return)."
  (interactive)
  (matlab-indent-line)
  (newline)
  (matlab-indent-line))

(defun matlab-comment-return ()
  "Handle carriage return for matlab comment line."
  (interactive)
  (cond
   ((matlab-ltype-comm)
    (matlab-set-comm-fill-prefix) (newline) (insert fill-prefix)
    (matlab-reset-fill-prefix) (matlab-indent-line))
   ((matlab-lattr-comm)
    (newline) (indent-to matlab-comment-column)
    (insert matlab-comment-on-line-s))
   (t
    (newline) (matlab-comment) (matlab-indent-line))))

(defun matlab-comm-from-prev ()
  "If the previous line is a comment-line then set up a comment on this line."
  (save-excursion
    ;; If the previous line is a comment-line then set the fill prefix from
    ;; the previous line and fill this line.
    (if (and (= 0 (forward-line -1)) (matlab-ltype-comm))
	(progn 
	  (matlab-set-comm-fill-prefix)
	  (forward-line 1) (beginning-of-line)
	  (delete-horizontal-space)
	  (if (looking-at "%") (delete-char 1))
	  (delete-horizontal-space)
	  (insert fill-prefix)
	  (matlab-reset-fill-prefix)))))

(defun matlab-comment ()
  "Add a comment to the current line."
  (interactive)
  (cond
   ((matlab-ltype-empty)		; empty line
    (matlab-comm-from-prev)
    (skip-chars-forward " \t%"))
   ((matlab-ltype-comm)			; comment line
    (matlab-comm-from-prev)
    (skip-chars-forward " \t%"))
   ((matlab-lattr-comm)			; code line w/ comment
    (beginning-of-line)
    (re-search-forward "[^%]%[ \t]")
    (forward-char -2)
    (if (< (current-column) matlab-comment-column)
	(indent-to matlab-comment-column))
    (skip-chars-forward "% \t"))
   (t					; code line w/o comment
    (end-of-line)
    (re-search-backward "[^ \t\n^]" 0 t)
    (forward-char)
    (delete-horizontal-space)
    (if (< (current-column) matlab-comment-column)
	(indent-to matlab-comment-column)
      (insert " "))
    (insert matlab-comment-on-line-s))))

(defun matlab-comment-indent ()
  "Indent a comment line in matlab-mode."
  (matlab-calc-indent))

(defun matlab-comment-region (beg-region end-region arg)
  "Comments every line in the region.
Puts matlab-comment-region-s at the beginning of every line in the region. 
BEG-REGION and END-REGION are args which specify the region boundaries. 
With non-nil ARG, uncomments the region."
  (interactive "*r\nP")
  (let ((end-region-mark (make-marker)) (save-point (point-marker)))
    (set-marker end-region-mark end-region)
    (goto-char beg-region)
    (beginning-of-line)
    (if (not arg)			;comment the region
	(progn (insert matlab-comment-region-s)
	       (while (and  (= (forward-line 1) 0)
			    (< (point) end-region-mark))
		 (insert matlab-comment-region-s)))
      (let ((com (regexp-quote matlab-comment-region-s))) ;uncomment the region
	(if (looking-at com)
	    (delete-region (point) (match-end 0)))
	(while (and  (= (forward-line 1) 0)
		     (< (point) end-region-mark))
	  (if (looking-at com)
	      (delete-region (point) (match-end 0))))))
    (goto-char save-point)
    (set-marker end-region-mark nil)
    (set-marker save-point nil)))


;;; filling ===================================================================

(defun matlab-set-comm-fill-prefix ()
  "Set the fill-prefix for the current (comment) line."
  (interactive)
  (setq fill-prefix
	(save-excursion
	  (beginning-of-line) 
	  (buffer-substring
	   (point)
	   (progn (re-search-forward "[ \t]*%[ \t]+") (point))))))

(defun matlab-set-code-fill-prefix ()
  "Set the fill-prefix for the current code line."
  (setq fill-prefix
	(save-excursion
	  (beginning-of-line) 
	  (buffer-substring
	   (point)
	   (progn (re-search-forward "[ \t]*") (point))))))

(defun matlab-reset-fill-prefix ()
  "Reset the fill-prefix."
  (setq fill-prefix nil))

(defun matlab-auto-fill ()
  "Do filling."
  (interactive)
  (if (> (current-column) fill-column)
      (cond
       ((matlab-ltype-comm)
	(matlab-set-comm-fill-prefix) (do-auto-fill)
	(matlab-reset-fill-prefix))
       ((and 
	 (and (matlab-ltype-code) (not (matlab-lattr-comm))) matlab-fill-code)
	(save-excursion
	  (while (> (current-column) fill-column) (forward-char -1))
	  (re-search-backward "[ \t]+") (delete-horizontal-space)
	  (insert " ...\n") (matlab-indent-line))))))

(defun matlab-join-comment-lines ()
  "Join current comment line to previous, deleting space and comment mark."
  (interactive)
  (beginning-of-line)
  (forward-char -1) (delete-char 1)	; delete newline
  (delete-horizontal-space)
  (delete-char 1)			; delete "%"
  (delete-horizontal-space)
  (insert " "))

(defun matlab-wrap-line () nil)

(defun matlab-fill-region (beg-region end-region &optional justify-flag)
  "Fill the region. Non-nil arg means justify commment lines as well."
  (interactive "*r\nP")
  (let ((end-reg-mk (make-marker)))
    (set-marker end-reg-mk end-region)
    (goto-char beg-region)
    (beginning-of-line)
    (while (< (save-excursion (forward-line 1) (point)) end-reg-mk)
      (if (save-excursion (= (forward-line 1) 0))
	  (progn 
	    (cond
	     ((matlab-ltype-comm)
	      (while (matlab-fill-comment-line))
	      (if justify-flag (justify-comment-line))))
	    (forward-line 1))))))

(defun matlab-fill-comment-line ()
  "Fill the current comment line."
  (interactive)
  (let ((prev-indent-col 0))
    (beginning-of-line)
    (re-search-forward matlab-cline-start-skip)
    (setq prev-indent-col (current-column))
    ;;(matlab-set-comm-fill-prefix)
    (if (/= (forward-line 1) 0)
	()
      (beginning-of-line)
      (re-search-forward matlab-cline-start-skip)
      (if (/= prev-indent-col (current-column))
	  (progn (forward-line -1) ())
	(matlab-join-comment-lines)
	(if (matlab-wrap-line)
	    (save-excursion
	      (forward-line 1)
	      (beginning-of-line)
	      (insert matlab-comment-line-s)
	      t))))))

(defun matlab-justify-line ()
  "Delete space on end of line and justify."
  (interactive)
  (save-excursion
    (end-of-line)
    (delete-horizontal-space)
    (justify-current-line)))


;;; V19 stuff =================================================================

(defun matlab-mode-hilit ()
  "Set up hilit19 support for matlab-mode."
  (interactive)
  (cond (window-system
	 (setq hilit-mode-enable-list  '(not text-mode)
	       hilit-background-mode   'light
	       hilit-inhibit-hooks     nil
	       hilit-inhibit-rebinding nil)
	 (require 'hilit19)
	 (hilit-set-mode-patterns 'matlab-mode matlab-hilit19-patterns))))

(defvar matlab-mode-menu-keymap nil
  "Keymap used in matlab mode to provide a menu.")

(defun matlab-frame-init ()
  (interactive)
  ;;(modify-frame-parameters (selected-frame) '((menu-bar-lines . 2)))
  ;; make a menu keymap
  (if matlab-mode-menu-keymap
      nil
    (setq matlab-mode-menu-keymap (make-sparse-keymap "Matlab"))
    (define-key matlab-mode-map [menu-bar matlab]
      (cons "Matlab" matlab-mode-menu-keymap))
    (define-key matlab-mode-map [menu-bar matlab saveandgo] 
      '("Save and Go" . matlab-shell-save-and-go))
    (define-key matlab-mode-map [menu-bar matlab version] 
      '("Version" . matlab-show-version))
    ))

;;; matlab shell from Eric Ludlam <eludlam@mathworks.com> =====================

(defvar matlab-shell-mode-map ()
  "Keymap used in matlab-shell-mode.")

(defvar matlab-shell-font-lock-keywords-1
  (append matlab-font-lock-keywords matlab-shell-font-lock-keywords)
  "Keyword symbol used for fontlock mode.")

(defvar matlab-shell-font-lock-keywords-2
  (append matlab-shell-font-lock-keywords-1 matlab-gaudy-font-lock-keywords)
  "Keyword symbol used for gaudy font-lock symbols.")

(defvar matlab-shell-font-lock-keywords-3
  (append matlab-shell-font-lock-keywords-2 
	  matlab-really-guady-font-lock-keywords)
  "Keyword symbol used for really guady font-lock symbols.")

(defvar matlab-shell-buffer-name "Matlab"
  "Name used to create matlab shell mode buffers.  This name will have
*'s surrounding it.")

(defun matlab-shell ()
  (interactive)
  (require 'comint)
  ;; Build keymap here in case someone never uses comint mode
  (if matlab-shell-mode-map
      ()
    (setq matlab-shell-mode-map (nconc (make-sparse-keymap) comint-mode-map))
    (define-key matlab-shell-mode-map "\C-ce" 'matlab-shell-last-error)
    (define-key matlab-shell-mode-map [menu-bar completion]
      (copy-keymap (lookup-key comint-mode-map [menu-bar completion])))
    (define-key matlab-shell-mode-map [menu-bar matshell] 
      (cons "Matlab" (make-sparse-keymap "Matlab")))
    (define-key matlab-shell-mode-map [menu-bar matshell exit]
      '("Exit" . matlab-shell-exit))
    (define-key matlab-shell-mode-map [menu-bar matshell close]
      '("Close Figures" . matlab-shell-close-figures))
    (define-key matlab-shell-mode-map [menu-bar matshell delfig]
      '("Close Current Figure" . matlab-shell-close-current-figure))
    (define-key matlab-shell-mode-map [menu-bar matshell demos]
      '("Demos" . matlab-shell-demos))
    (define-key matlab-shell-mode-map [menu-bar matshell lasterr]
      '("Goto last error" . matlab-shell-last-error))
    )
  (switch-to-buffer (make-comint matlab-shell-buffer-name "matlab"))
  (matlab-shell-mode))

(defun matlab-shell-mode ()
  "Run matlab as a subprocess, and add nifty colorizer.
\\<matlab-shell-mode-map>
\\[matlab-shell-last-error] - find location of last matlab runtime error
"
  (comint-mode)
  (setq major-mode 'matlab-shell-mode
	mode-name "M-Shell"
	comint-prompt-regexp "^>>"
	comint-delimiter-argument-list [ 59 ] ; semi colon
	comint-dynamic-complete-functions '(comint-replace-by-expanded-history)
	comint-process-echoes t
	)
  (make-local-variable 'comment-start)
  (setq comment-start "%")
  (use-local-map matlab-shell-mode-map)
  (set-syntax-table matlab-mode-syntax-table)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '((matlab-shell-font-lock-keywords-1
			      matlab-shell-font-lock-keywords-2
			      matlab-shell-font-lock-keywords-3)
			     t t ((?_ . "w"))))
  (run-hooks 'matlab-shell-mode-hooks)
  )

(defun matlab-shell-save-and-go ()
  "Save the current matlab M file, and then run it in an available matlab
shell mode buffer."
  (interactive)
  (if (not (eq major-mode 'matlab-mode))
      (error "Save and go is only useful in a matlab buffer!"))
  (let ((fn-name (file-name-sans-extension
		  (file-name-nondirectory (buffer-file-name))))
	(msbn (concat "*" matlab-shell-buffer-name "*"))
	(pos nil)
	(param ""))
    (save-buffer)
    ;; Do we need parameters?
    (if (save-excursion
	  (goto-char (point-min))
	  (end-of-line)
	  (forward-sexp -1)
	  (looking-at "([a-zA-Z]"))
	(setq param (read-string "Paramters: "
				 (car matlab-shell-save-and-go-history)
				 'matlab-shell-save-and-go-history)))
    ;; No buffer?  Make it!
    (if (not (get-buffer msbn)) (matlab-shell))
    ;; Ok, now fun the function in the matlab shell
    (if (get-buffer-window msbn)
	(select-window (get-buffer-window msbn))
      (switch-to-buffer (concat "*" matlab-shell-buffer-name "*")))
    (comint-send-string (get-buffer-process (current-buffer))
			(concat fn-name " " param "\n"))))

(defun matlab-shell-last-error ()
  "Scan the current matlab interactive buffer and find the last
matlab error, and go there.  To reference old errors, put the currsor just
after the error text."
  (interactive)
  (let (eb el)
    (save-excursion
      (if (not (re-search-backward 
		(concat "\\(Error\\|Syntax error\\) in ==> "
			"\\([-.a-zA-Z_0-9/]+\\).*\nOn line "
			"\\([0-9]+\\) ") nil t))
	  (error "No errors found!"))
      (setq eb (buffer-substring-no-properties
		(match-beginning 2) (match-end 2))
	    el (buffer-substring-no-properties
		(match-beginning 3) (match-end 3))))
    (find-file-other-window eb)
    (goto-line (string-to-int el))))


(defun matlab-shell-demos ()
  "Close any open figures"
  (interactive)
  (comint-send-string (get-buffer-process (current-buffer)) "demo\n"))
(defun matlab-shell-close-figures ()
  "Close any open figures"
  (interactive)
  (comint-send-string (get-buffer-process (current-buffer)) "close all\n"))
(defun matlab-shell-close-current-figure ()
  "Close any open figures"
  (interactive)
  (comint-send-string (get-buffer-process (current-buffer)) "delete(gcf)\n"))
(defun matlab-shell-exit ()
  "Close any open figures"
  (interactive)
  (comint-send-string (get-buffer-process (current-buffer)) "exit\n"))

;;; matlab-mode debugging =====================================================

(defun matlab-show-line-info ()
  "Display type and attributes of current line.  Used in debugging."
  (interactive)
  (let ((msg "line-info:") (deltas (matlab-calc-deltas)))
    (cond
     ((matlab-ltype-empty)
      (setq msg (concat msg " empty")))
     ((matlab-ltype-comm)
      (setq msg (concat msg " comment")))
     (t
      (setq msg (concat msg " code"))))
    (setq msg (concat msg " add-from-prev="
		      (int-to-string (car deltas))))
    (setq msg (concat msg " add-to-next="
		      (int-to-string (car (cdr deltas)))))
    (setq msg (concat msg " indent="
		      (int-to-string (matlab-calc-indent))))
    (if (matlab-lattr-cont)
	(setq msg (concat msg " w/cont")))
    (if (matlab-lattr-comm)
	(setq msg (concat msg " w/comm")))
    (message msg)))

(defun matlab-clear-vars ()
  (interactive)
  (makunbound 'matlab-indent-level)
  (makunbound 'matlab-cont-level)
  (makunbound 'matlab-comment-line-s)
  (makunbound 'matlab-comment-on-line-s)
  (makunbound 'matlab-comment-region-s)
  (makunbound 'matlab-indent-function)
  (makunbound 'matlab-matlab-mode-syntax-table)
  (makunbound 'matlab-matlab-mode-abbrev-table)
  (makunbound 'matlab-matlab-mode-map)
  (makunbound 'matlab-matlab-block-beg-pre)
  (makunbound 'matlab-matlab-block-mid-pre)
  (makunbound 'matlab-matlab-block-end-pre)
  (makunbound 'matlab-matlab-other-pre)
  (makunbound 'matlab-matlab-block-re)
  (makunbound 'matlab-matlab-block-beg-re)
  (makunbound 'matlab-matlab-block-end-re)
  (makunbound 'matlab-cline-start-skip)
  (makunbound 'matlab-matlab-font-lock-keywords))


;;; stuff which belongs elsewhere =============================================

(defun position-at-eol ()		; return point for end-of-line
  (interactive)
  (save-excursion
    (end-of-line)
    (point)))

(defun justify-comment-line ()
  "Add spaces to comment line point is in, so it ends at fill-column."
  (interactive)
  (save-excursion
    (save-restriction
      (let (ncols beg)
	(beginning-of-line)
	(forward-char (length fill-prefix))
	(skip-chars-forward " \t")
	(setq beg (point))
	(end-of-line)
	(narrow-to-region beg (point))
	(goto-char beg)
	(while (re-search-forward "   *" nil t)
	  (delete-region
	   (+ (match-beginning 0)
	      (if (save-excursion
		    (skip-chars-backward " ])\"'")
		    (memq (preceding-char) '(?. ?? ?!)))
		  2 1))
	   (match-end 0)))
	(goto-char beg)
	(while (re-search-forward "[.?!][])""']*\n" nil t)
	  (forward-char -1)
	  (insert " "))
	(goto-char (point-max))
	(setq ncols (- fill-column (current-column)))
	(if (search-backward " " nil t)
	    (while (> ncols 0)
	      (let ((nmove (+ 3 (% (random) 3))))
		(while (> nmove 0)
		  (or (search-backward " " nil t)
		      (progn
			(goto-char (point-max))
			(search-backward " ")))
		  (skip-chars-backward " ")
		  (setq nmove (1- nmove))))
	      (insert " ")
	      (skip-chars-backward " ")
	      (setq ncols (1- ncols))))))))


(provide 'matlab)


;;; Change log
;;
;; 07Mar97 by Matt Wette <mwette@alumni.caltech.edu>
;;	Fixed a few xemacs problems.  Released as 1.09.0.
;;
;; 03Mar97 by Eric Ludlam <eludlam@mathworks.com>
;;      Added expressions to handle blocks which are not terminated with
;;            the 'end' command
;;      Added `matlab-shell-save-and-go' function to automatically run
;;            a function after saving it.
;;      Bug fixes to `matlab-forward-sexp'
;;      Improved font lock interface to take advantage of the user
;;            variable `font-lock-use-maximal-decoration'
;;
;; 24Feb97 by Eric Ludlam <eludlam@mathworks.com>
;;      Added more font locking, plus font locking of matlab-shell
;;      Added `matlab-backward-sexp',`matlab-cursor-in-string-or-comment'
;;      Added ability to indent switch/case/case/otherwise/end blocks
;;            as per manual specifications for matlab v5.0
;;      Added command for matlab-shell to goto the last reported error
;;      Modified matlab-shell to use comint features instead of hand
;;            crafted workarounds of the defaults
;;
;; 07Dec96 by Matt Wette <mwette@alumni.caltech.edu>
;;	incorporated many fixes from Mats Bengtsson <matsb@s3.kth.se>;
;;	font-lock comment/string fixes, Eric Ludlam <eludlam@mathworks.com>;
;;	added support for switch construct;
;;
;; 01Aug96 by Matt Wette <mwette@alumni.caltech.edu> 
;;	fixed to jive w/ emacs lib conventions: changed name of file from
;;	matlab-mode.el to matlab.el (14 char limit); released as 1.08.0
;;
;; 28Apr96 by Matt Wette <mwette@alumni.caltech.edu>
;;	comments lines w/ just % are now hilighted; syntax table: "-2" changed
;;	to " 2"; released 1.07.6
;; 
;; 30Jan96 by Matt Wette <mwette@alumni.caltech.edu>
;;	fixed problem w/ emacs-19.30 filling and auto-fill problem thanks to
;;	Mats BengTsson <matsb@s3.kth.se>; started implementation of matlab-
;;	shell, based on comint and shell-mode; released 1.07.5
;;
;; 25Jan96 by Matt Wette <mwette@alumni.caltech.edu>
;;	added "global" to font-lock, hilit keywords; fixed indenting of 2nd
;;	line if first ends in ...; filling is broken for FSF19.30 (works for
;;	FSF19.28); torkel fixes to matlab-reset-vars; fixed indent bug
;;	reported by Trevor Cooper;
;;
;; 20Jan96 by Matt Wette <mwette@alumni.caltech.edu>
;;	cleaned up commenting; added preliminary matlab-shell mode, rel 1.07.4
;;
;; 19Jan96 by Matt Wette <mwette@alumni.caltech.edu>
;;	commented out debug-on-error; got hilit to work for sam
;;
;; 18Jan96 by Matt Wette <mwette@alumni.caltech.edu>
;;	fixed problem int matlab-prev-line which caused fatal matlab-mode;
;;	crash fixed problem with indenting when keywords in comments; still
;;	haven't cleaned up comment formatting ...
;;
;; 21Jul95 by Matt Wette <mwette@alumni.caltech.edu>
;;	fixes by Bjorn Torkelsson <torkel@cs.umu.se>: replaced lattr-comment
;;	w/ lattr-comm to fix inconsistency; added function to font-lock 
;;	keywords, added function name to font-lock-function-name-face.  He had
;;	also added funtion as a block begin keyword.  This should be an option
;;	since it will cause the body of a function to be indented.  Worked on
;;	filling.  More work on filling.  fixed many bugs reported by Rob
;;	Cunningham.  Pulled cadr.
;;
;; 13Jul95 by Matt Wette <mwette@mr-ed.jpl.nasa.gov>
;; 	changed indenting for continuation lines in calc-deltas to use
;;	cont-level; changed syntab-table;  changed the way the return key is
;;	mapped; released version 1.07.1
;;
;; 08Jul95 by Matt Wette <mwette@mr-ed.jpl.nasa.gov>
;;	This is a fairly major rewrite of the indenting functions to fix long-
;;	standing problems arising from keywords and percents in strings.  We
;;	may have to add more heuristics later but this may work better.
;;	Changed comment region string.  Released version 1.07.0.
;;
;; 10Oct94 by Matt Wette <mwette@csi.jpl.nasa.gov>
;;	changed auto-fill-mode to auto-fill-function; changed
;;	comment-indent- to comment-indent-function; fixed percents in strings
;;	being interpreted as comments, but a % for comment should not be
;;	followed by [disx%]
;;
;; 23Nov93 by Matt Wette <mwette@csi.jpl.nasa.gov>
;;	added Lucid emacs, GNU emacs font-lock and lhilit support; repaired
;;	mtlb-block-{beg,end}-kw (Thanks to Dave Mellinger <dkm1@cornell.edu>)
;;	removed string delim entry from matlab-mode-syntax-table (MATLAB lang
;;	sucks here -- why not use " for strings?).   Released vers 1.06.0
;;
;; 10Aug93 by Matt Wette <mwette@csi.jpl.nasa.gov>
;;	added matlab-indent-end-before-return; indent may be fixed now
;;	still not working for emacs 19
;;
;; 02Aug93 by Matt Wette <mwette@csi.jpl.nasa.gov>
;;	fixed error in mtlb-calc-indent;  bumped version to 1.05.1; added
;;	mtlb-prev-line;  bumped version to 1.05.3; added mtlb-calc-blok-indent
;;
;; 01Aug93 by Matt Wette <mwette@csi.jpl.nasa.gov>
;;	Fixed bug which treated form as block-begin keyword.  Reworked 
;;	mtlb-calc-indent -- seems to work better w/ redundant cont lines now.
;; 	Bumbed version to 1.05.
;;
;; 13Jun93 by Matt Wette <mwette@csi.jpl.nasa.gov>
;;	Changed `linea' to `lattr', `linet' to `ltype', fixed
;;	Bumped version number from 1.03bb to 1.04.
;;
;; 02May91 by Matt Wette, mwette@csi.jpl.nasa.gov
;;	Added matlab-auto-fill for auto-fill-hook so that this mode doesn't
;;	try to fill matlab code, just comments.
;;
;; 22Apr91 by Matt Wette, mwette@csi.jpl.nasa.gov
;;	Changed "mtlb-ltype-cont" to "mtlb-lattr-cont", "mtlb-ltype-comment-on-
;;	line" to "mtlb-lattr-comment" and "mtlb-ltype-unbal-mexp" to "mtlb-
;;	lattr-unbal-mext" to emphasize that these are line attributes and not
;;	line types.  Modified "matlab-line-type" to reflect the change ini
;;	logic.
;;
;; 18Apr91 by Matt Wette, mwette@csi.jpl.nasa.gov
;;      Modified matlab-comment-return so that when hit on a line with a
;;	comment at the end it will go to the comment column.  To get the
;;	comment indented with the code, just hit TAB.
;;
;; 17Apr91 by Matt Wette, mwette@csi.jpl.nasa.gov
;;	Received critique from gray@scr.slb.com.  Changed ml- to mtlb- due to 
;;	possible conflict with mlsupport.el routines.  Added matlab-comment
;;	-line-s and -on-line-s.  Fixed bug in matlab-comment (set-fill-prefix).
;;	matlab-comment-return now works if called on a non-comment line.
;;
;; 04Mar91 by Matt Wette, mwette@csi.jpl.nasa.gov
;;	Added const matlab-indent-before-return.  Released Version 1.02.
;;
;; 02Feb91 by Matt Wette, mwette@csi.jpl.nasa.gov
;;	Changed names of ml-*-line to ml-ltype-*.  Cleaned up a lot. Added
;;	ml-format-comment-line, fixed ml-format-region.  Changed added "-s"
;;	on end of matlab-comment-region string.  Justify needs to be cleaned
;;	up.
;;
;; Fri Feb  1 09:03:09 1991; gray@scr.slb.com
;;      Add function matlab-comment-region, which inserts the string
;;      contained in the variable matlab-comment-region at the start
;;      of every line in the region.  With an argument the region is
;;      uncommented.  [Straight copy from fortran.el]
;;
;; 25Jan91 by Matt Wette, mwette@csi.jpl.nasa.gov
;;	Got indentation of matrix expression to work, I think.  Also,
;;	added tabs to comment start regular-expression.
;;
;; 14Jan91 by Matt Wette, mwette@csi.jpl.nasa.gov
;;	Added functions (ml-unbal-matexp ml-matexp-indent) for matrix
;;	expressions.
;;
;; 07Jan91 by Matt Wette, mwette@csi.jpl.nasa.gov
;;      Many changes.  Seems to work reasonably well.  Still would like
;;      to add some support for filling in comments and handle continued
;;      matrix expressions.  Released as Version 1.0.
;;
;; 04Jan91 by Matt Wette, mwette@csi.jpl.nasa.gov
;;      Created.  Used eiffel.el as a guide.
;;

;;; matlab.el ends here
