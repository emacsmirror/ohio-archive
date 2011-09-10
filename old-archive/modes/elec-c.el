;;; elec-c.el --- Gosmacs-like electric C code editing mode

;; (C) Copyright 1987, 1993 Mark Davies

;; Author: Mark Davies <mark@comp.vuw.ac.nz>
;; Created: Dec 1985
;; Version: 2.0
;; Last Modified: Sat Jul 10 19:41:10 1993 NZST
;; Maintainer: Mark Davies <mark@comp.vuw.ac.nz>
;; Keywords: c

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; LCD Archive Entry:
;; elec-c|Mark Davies|mark@comp.vuw.ac.nz|
;; Gosmacs-like electric C code editing mode|
;; 10-Jul-1993|version 2.0|~/modes/elec-c.el.Z|

;;; Commentary:

;; Originally written in Dec 1985 inspired by the Gosling emacs electric C
;; mode, then revised in Jun 1987.
;; Updated for emacs 19 and enhanced in Jun 1993.
;; Thanks to Andrew Vignaux <ajv@datamark.co.nz> for the code to
;; incorporate the current line into the condition for a new if/while/do
;; construct.

;; I invoke this from my .emacs file with the following incantation
;;   (add-hook 'c-mode-hook 'elec-c-mode)
;;   (add-hook 'elec-c-mode-hook 'turn-on-auto-fill)
;;   (autoload 'elec-c-mode "elec-c" "High powered C editing mode." t)

;;; Code:

(defvar elec-c-mode-abbrev-table nil
  "Abbrev table in use in elec-C-mode buffers.")

(defvar elec-c-mode-map nil
  "Keymap used in elec C mode.")
(if elec-c-mode-map
    ()
  (setq elec-c-mode-map (make-sparse-keymap))
  (define-key elec-c-mode-map "{" 'elec-c-left-brace)
  (define-key elec-c-mode-map "}" 'electric-c-brace)
  (define-key elec-c-mode-map "(" 'elec-c-opening-brac)
  (define-key elec-c-mode-map "[" 'elec-c-opening-brac)
  (define-key elec-c-mode-map "#" 'electric-c-sharp-sign)
  (define-key elec-c-mode-map ";" 'elec-c-semi)
  (define-key elec-c-mode-map ":" 'electric-c-terminator)
  (define-key elec-c-mode-map "\e\C-h" 'mark-c-function)
  (define-key elec-c-mode-map "\e\C-q" 'indent-c-exp)
  (define-key elec-c-mode-map "\ea" 'c-beginning-of-statement)
  (define-key elec-c-mode-map "\ee" 'c-end-of-statement)
  (define-key elec-c-mode-map "\eq" 'c-fill-paragraph)
  (define-key elec-c-mode-map "\C-c\C-n" 'c-forward-conditional)
  (define-key elec-c-mode-map "\C-c\C-p" 'c-backward-conditional)
  (define-key elec-c-mode-map "\C-c\C-u" 'c-up-conditional)
  (define-key elec-c-mode-map "\177" 'backward-delete-char-untabify)
  (define-key elec-c-mode-map "\C-c\C-c" 'elec-c-close-block)
  (define-key elec-c-mode-map "\C-cv" 'elec-c-toggle-verbatim)
  (define-key elec-c-mode-map "\e{" 'elec-c-remove-braces)
  (define-key elec-c-mode-map "\C-j" 'elec-c-linefeed)
  (define-key elec-c-mode-map "\t" 'c-indent-command))

(modify-syntax-entry ?# "w" c-mode-syntax-table) ;so abbrevs work

;; this should really be a user preference option
;(modify-syntax-entry ?_ "w" c-mode-syntax-table)

(defvar elec-c-verbatim nil
  "Should abbrevs be expanded explicitly?")
(defvar elec-c-brace-on-same-line t
  "Should braces follow after if's for's etc or be on separate line.")

(defconst comment-edged nil
  "*Use comments with an edge.
   eg.      /*
             * ...
             */")

(defconst elec-c-style-alist
  '(("GNU"
     (elec-c-brace-on-same-line . nil))
    ("K&R"
     (elec-c-brace-on-same-line . t))
    ("BSD"
     (elec-c-brace-on-same-line . t))))

;;
;; Add elec-c settings to c-style-alist
(setq c-style-alist
      (mapcar '(lambda (taglist)
		 (let ((tag (car taglist)))
		   (cons tag
			 (append
			  (cdr taglist)
			  (cdr (assoc tag elec-c-style-alist))))))
	      c-style-alist))


(defun elec-c-mode ()
  "High powered C editing mode.
Elec C mode provides expansion of the C control constructs:
   if, else, while, for, do, and switch.
The user types the keyword immediately followed by a space, which causes
the construct to be expanded, and the user is positioned where she is most
likely to want to be.
eg. when the user types a space following \"if\" the following appears in
the buffer:
            if () {
            }

and the cursor is between the parentheses.  The user can then type some
boolean expression within the parens.  Having done that, typing \\[elec-c-linefeed]
places you, appropriately indented on a new line between the braces.

Various characters in C almost always come in pairs: {}, (), [].
When the user types the first, she gets the second as well, with optional
special formatting done on {}.  You can always quote (with \\[quoted-insert]) the left
\"paren\" to avoid the expansion.

#de, and #in are defined as abbreviations for #define and #include 
respectively.

With auto-fill-mode on, three types of automatic formatting of comments are
possible. The default is of the form
                            /* ...   ... */
                            /* ...     ... */
If comment-multi-line is set non-nil you get comments of the form
                            /* ...   ...
                               ...     ... */
If additionally comment-edged is set non-nil you get comments of the form
                            /*
                             * ...    ...
                             */

Expression and list commands understand all C brackets.
Tab indents for C code.
Paragraphs are separated by blank lines only.
Delete converts tabs to spaces as it moves back.
\\{elec-c-mode-map}
Variables controlling indentation style:
 c-auto-newline
    Non-nil means automatically newline before and after braces,
       and after colons and semicolons, inserted in C code.
    with this on colons and semicolons want to go to the end of the line.
 c-indent-level
    Indentation of C statements within surrounding block.
    The surrounding block's indentation is the indentation of the line
    on which the open-brace appears.
 c-continued-statement-offset
    Extra indentation given to a substatement, such as the then-clause
    of an if or body of a while
 c-continued-brace-offset
    Extra indentation given to a brace that starts a substatement.
    This is in addition to c-continued-statement-offset.
 c-brace-offset
    Extra indentation for a line if it starts with an open brace.
 c-brace-imaginary-offset
    An open brace following other text is treated as if it were this far
    to the right of the start of its line.
 c-argdecl-indent
    Indentation level of declarations of C function arguments.
 c-label-offset
    Extra indentation for line that is a label, or case or default.

Turning on elec C mode calls the value of the variable elec-c-mode-hook
with no args, if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map elec-c-mode-map)
  (setq major-mode 'elec-c-mode)
  (setq mode-name "elec C")
  (if (not elec-c-mode-abbrev-table)
      (let ((prev-a-c abbrevs-changed))
	(define-abbrev-table 'elec-c-mode-abbrev-table '(
        	("main" "main" elec-c-keyword-main 0)
		("argc" "argc" elec-c-keyword-argc 0)
		("if" "if" elec-c-keyword-if-while 0)
		("elsif" "else if" elec-c-keyword-if-while 0)
		("switch" "switch" elec-c-keyword-if-while 0)
		("while" "while" elec-c-keyword-if-while 0)
		("else" "else" elec-c-keyword-else 0)
		("for" "for" elec-c-keyword-for 0)
		("do" "do" elec-c-keyword-do 0)
		("#d" "#define" nil 0)
		("#de" "#define" nil 0)
		("#e" "#endif" nil 0)
		("#i" "#ifdef" nil 0)
		("#in" "#include" nil 0)))
	(setq abbrevs-changed prev-a-c)))
  (setq local-abbrev-table elec-c-mode-abbrev-table)
  (abbrev-mode 1)
  (set-syntax-table c-mode-syntax-table)
  (make-local-variable 'elec-c-verbatim)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'c-indent-line)
  (make-local-variable 'indent-region-function)
  (setq indent-region-function 'c-indent-region)
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
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'c-comment-indent)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (run-hooks 'elec-c-mode-hook))

; so lets hope noone writes *LARGE* C files.
(defun elec-c-inside-comment-p ()
  (nth 4 (parse-partial-sexp (point-min) (point))))

(defun elec-c-inside-string-p ()
  (nth 3 (parse-partial-sexp (point-min) (point))))

(defun elec-c-inside-comment-or-string-p ()
  (let ((parse-state (parse-partial-sexp (point-min) (point))))
    (or (nth 4 parse-state) (nth 3 parse-state))))

(defun elec-c-open-block ()
  (interactive)
  (search-forward "{")
  (backward-char 1)
  (forward-sexp 1)
  (backward-char 1)
  (split-line)
  (c-indent-line))

(defun elec-c-close-block ()
  (interactive)
  (while (not (looking-at "{"))
    (backward-up-list 1))
  (forward-sexp 1)
  (save-excursion
    (forward-line -1)
    (delete-blank-lines)
    (beginning-of-line)
    (if (looking-at "[ \t]*$")
	(kill-line 1)))
  (end-of-line)
  (newline)
  (c-indent-line))

(defun elec-c-remove-braces ()
  "remove the surrounding pair of {}'s from the function."
  (interactive)
  (save-excursion
    (while (not (looking-at "{"))
      (backward-up-list 1))
    (let (end)
      (save-excursion			; kill tail
	(forward-sexp 1)
	(delete-char -1)
	(delete-horizontal-space)
	(and (bolp) (eolp)
	     (delete-char 1))
	(setq end (point-marker)))
      (delete-char 1)			; kill head
      (delete-horizontal-space)
      (and (bolp) (eolp)
	   (delete-char 1))
      (while (<= (point) (marker-position end))
	(c-indent-line)
	(forward-line 1)))))

(defun elec-c-linefeed ()
  "Go to end of line, open a new line and indent appropriately."
  (interactive)
  (end-of-line)
  (and (not (elec-c-inside-comment-p))
       (= (preceding-char) ?\))
       (looking-at "\n[ \t]*{")
       (forward-line 1)
       (end-of-line))
  (newline-and-indent))

(defun elec-c-semi (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (if c-auto-newline
      (let ((end (point)))
	(if (not (save-excursion
		   (beginning-of-line)
		   (skip-chars-forward " \t")
		   (or (= (following-char) ?#)
		       (progn
			 (beginning-of-defun)
			 (let ((pps (parse-partial-sexp (point) end)))
			   (or (nth 3 pps) (nth 4 pps) (nth 5 pps)))))))
	    (end-of-line))
	(electric-c-terminator arg))
    (self-insert-command (prefix-numeric-value arg))))

(defun elec-c-left-brace ()
  "if c-auto-newline is on insert matching close brace and format appropriately."
  (interactive)
  (if (or (not c-auto-newline)
	  (c-inside-parens-p)
	  (elec-c-inside-comment-or-string-p))
      (insert ?{)
    (end-of-line)
    (delete-horizontal-space)
    (if (/= (char-syntax (preceding-char)) ? )
	(insert ? ))
    (insert ?{)
    (c-indent-line)
    (insert "\n\n}")
    (c-indent-line)
    (forward-line -1)
    (c-indent-line)))
      
(defun elec-c-opening-brac ()
  "For one of (, [ insert it and its pair, and postion point in the centre"
  (interactive)
  (insert last-command-char)
  (if (not (elec-c-inside-comment-or-string-p))
      (save-excursion
	(cond
	 ((= last-command-char ?\() (insert ?\)))
	 ((= last-command-char ?[) (insert ?]))))))

(defun elec-c-keyword-main ()
  (if (elec-c-inside-comment-or-string-p)
      nil
    (insert-string " ()\n{\n}\n")
    (search-backward ")")
    (setq unread-command-events '(?\C-?))))

(defun elec-c-keyword-argc ()
  (if (save-excursion
	(beginning-of-line)
	(looking-at "\\(int\\)?[ \t]*main[ \t](argc"))
      (progn
	(insert-string ", argv")
	(end-of-line)
	(newline) (c-indent-line)
	(insert-string "int argc;")
	(newline) (c-indent-line)
	(insert-string "char *argv [];")
	(elec-c-open-block)
	(setq unread-command-events '(?\C-?)))))

(defun elec-c-keyword-if-while ()
  (if (elec-c-inside-comment-or-string-p)
      nil
    (let ((empty-condition (eolp)))
      (insert-string " (")
      (or empty-condition
	  (elec-c-incorporate-line))
      (insert-string ")")
      (if elec-c-brace-on-same-line
	  (insert-string " {")
	(insert-string "\n{")
	(c-indent-line))
      (insert-string "\n}")
      (c-indent-line)
      (if empty-condition
	  (search-backward ")")
	(beginning-of-line)
 	(newline)
 	(backward-char 1)
 	(c-indent-line))
      (setq unread-command-events '(?\C-?)))))

(defun elec-c-keyword-else ()
  (if (elec-c-inside-comment-or-string-p)
      nil
    (if elec-c-brace-on-same-line
	(insert-string " {")
      (insert-string "\n{")
      (c-indent-line))
    (insert-string "\n\n}")
    (c-indent-line)
    (forward-line -1)
    (c-indent-line)
    (setq unread-command-events '(?\C-?))))
    
(defun elec-c-keyword-for ()
  (if (elec-c-inside-comment-or-string-p)
      nil
    (insert-string " (;;)")
    (if elec-c-brace-on-same-line
	(insert-string " {")
      (insert-string "\n{")
      (c-indent-line))
    (insert-string "\n}")
    (c-indent-line)
    (search-backward ";;)")
    (setq unread-command-events '(?\C-?))))

(defun elec-c-keyword-do ()
  (if (elec-c-inside-comment-or-string-p)
      nil
    (insert-string " {\n")
    (save-excursion
      (insert-string "\n} while (")
      (if (not (eolp))
	  (elec-c-incorporate-line))
      (insert-string ");")
      (c-indent-line))
    (c-indent-line)
    (setq unread-command-events '(?\C-?))))

(defun elec-c-incorporate-line ()
  ;; Incorporate the remainder of line which starts after (point) into the
  ;; if/while () test condition.
  ;; Strip any trailing ";"
  (delete-horizontal-space)
  (end-of-line)
  (if (= (preceding-char) ?\;)
      (delete-char -1)))

; this is a HACK but I can't think of a better place to do it.

(defun calculate-c-indent-within-comment (&optional after-star)
  "Return the indentation amount for line inside a block comment.
Optional arg AFTER-STAR means, if lines in the comment have a leading star,
return the indentation of the text that would follow this star."
  (let (end star-start)
    (and (eq major-mode 'elec-c-mode)
	 comment-edged
	 (/= last-command-char ?\t)
	 (save-excursion
	   (insert "* ")))
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward " \t")
      (setq star-start (= (following-char) ?\*))
      (skip-chars-backward " \t\n")
      (setq end (point))
      (beginning-of-line)
      (skip-chars-forward " \t")
      (if after-star
	  (and (looking-at "\\*")
	       (re-search-forward "\\*[ \t]*")))
      (and (re-search-forward "/\\*[ \t]*" end t)
	   star-start
	   (not after-star)
	   (goto-char (1+ (match-beginning 0))))
      (if (and (looking-at "[ \t]*$") (= (preceding-char) ?\*))
	  (1+ (current-column))
	(current-column)))))

(defun elec-c-toggle-verbatim (arg)
  "Toggle elec-c verbatim mode.
Doesn't expand keywords unless explicitly
This command toggles that mode (off->on, on->off), 
with an argument, turns it on iff arg is positive, otherwise off."
  (interactive "P")
  (abbrev-mode arg)
  (or (assq 'elec-c-verbatim minor-mode-alist)
      (setq minor-mode-alist (append minor-mode-alist
				     (list '(elec-c-verbatim
					     " Verbatim")))))
  (setq elec-c-verbatim
	(if (null arg) (not elec-c-verbatim)
	  (> (prefix-numeric-value arg) 0))))

;;; elec-c.el ends here
