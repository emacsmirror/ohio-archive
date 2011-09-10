;;; ada-stmt.el - An extension to Ada-mode for inserting statement templates.

;;; Copyright (C) 1994 Markus Heritsch (MH)
;;;               1994 Rolf Ebert
;;;               1993 Daniel Pfeiffer
;;;               1987 Free Software Foundation, Inc.

;;; $Revision: 1.9 $

;;; $Date: 1994/11/11 20:01:20 $

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


;;; Usage:
;;;
;;; put the following statement in your .emacs:
;;; (require 'ada-stmt)
;;;

;;; History:

;; Created May 1987.
;; Original version from V. Bowman as in ada.el of Emacs-18
;; (borrowed heavily from Mick Jordan's Modula-2 package for GNU,
;; as modified by Peter Robinson, Michael Schmidt, and Tom Perrine.)
;;
;; Adapted Sep 1993. Daniel Pfeiffer <pfeiffer@cict.fr> (DP)
;; Introduced statement.el for smaller code and user configurability.
;;
;; Adapted Nov 1993. Rolf Ebert <ebert@enpc.fr> (RE)
;; Moved the skeleton generation into a separate file. The code is
;; essentially written by DP
;; 
;; Adapted Jun 1994. Markus Heritsch
;; <Markus.Heritsch@studbox.uni-stuttgart.de> (MH)
;; a) modified templates for inserting pseudo code or asking user for
;;    real values. 
;;    This only works with the appropriately modified version of skeleton.el
;; b) added menu bar support for templates
;;
;; Adapted Oct 1994. Markus Heritsch
;;    <Markus.Heritsch@studbox.uni-stuttgart.de> (MH) 

;; moved ada-toggle-prompt-pseudo to toggle-promt-pseudo in
;; skeleton.el.  (modified version of the original skeleton.el - comes
;; only with the ada-mode set of files).

;;; Code:

(require 'ada-mode)
(load "skeleton") ;; bug in 19.28 skeleton.el, not provided.
(require 'easymenu)


(defvar ada-template-map nil
  "Keymap used in Ada mode for smart template operations.")

(let ((ada-mp (make-sparse-keymap)))
  (define-key ada-mp "h" 'ada-header)
;  (define-key ada-mp "p" 'ada-toggle-prompt-pseudo)
  (define-key ada-mp "(" 'insert-parentheses)
  (define-key ada-mp "\C-a" 'ada-array)
  (define-key ada-mp "b" 'ada-exception-block)
  (define-key ada-mp "d" 'ada-declare-block)
  (define-key ada-mp "c" 'ada-case)
  (define-key ada-mp "\C-e" 'ada-elsif)
  (define-key ada-mp "e" 'ada-else)
  (define-key ada-mp "\C-k" 'ada-package-spec)
  (define-key ada-mp "k" 'ada-package-body)
  (define-key ada-mp "\C-p" 'ada-procedure-spec)
  (define-key ada-mp "\C-f" 'ada-function-spec)
  (define-key ada-mp "f" 'ada-for-loop)
  (define-key ada-mp "i" 'ada-if)
  (define-key ada-mp "l" 'ada-loop)
  (define-key ada-mp "\C-r" 'ada-record)
  (define-key ada-mp "\C-s" 'ada-subtype)
  (define-key ada-mp "S" 'ada-tabsize)
  (define-key ada-mp "\C-t" 'ada-task-spec)
  (define-key ada-mp "t" 'ada-task-body)
  (define-key ada-mp "\C-y" 'ada-type)
  (define-key ada-mp "\C-v" 'ada-private)
  (define-key ada-mp "u" 'ada-use)
  (define-key ada-mp "\C-u" 'ada-with)
  (define-key ada-mp "\C-w" 'ada-when)
  (define-key ada-mp "w" 'ada-while-loop)
  (define-key ada-mp "\C-x" 'ada-exception)
  (define-key ada-mp "x" 'ada-exit)
  (setq ada-template-map ada-mp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Place the templates into ada mode.  They may be inserted under any key.
;; C-c t and C-c C-t will be the defaults.  If you use templates alot, you
;; may want to consider moving the binding to another key in your .emacs
;; file.  be sure to (require 'ada-etempl) first.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key ada-mode-map "\C-ct" ada-template-map)
(define-key ada-mode-map "\C-c\C-t" ada-template-map)

;;; ---- statement skeletons ------------------------------------------

(define-skeleton ada-array
  "Insert array type definition.  Uses the minibuffer to prompt
for component type and index subtypes."
  "[index subtype[s]]: "
  "array (" str ") of " _ ?\;)



(define-skeleton ada-case
  "Build skeleton case statement, prompting for the selector expression.
Also builds the first when clause."
  "[selector expression]: "
  "case " str " is" \n
  > "when " (read-string "['|'-delimited choice list]: ") " =>" \n
  > _ \n
  < < "end case;")

(define-skeleton ada-when
  "Start a case statement alternative with a when clause."
  "['|'-delimited choice list]: "
  < "when " str " =>" \n
  >)



(define-skeleton ada-declare-block
  "Insert a block with a declare part.
Indent for the first declaration."
  "[block name]: "
  < str & ?: & \n
  > "declare" \n
  > _ \n
  < "begin" \n
  > \n
  < "end " str | (delete-backward-char 1) ?\;)

(define-skeleton ada-exception-block
  "Insert a block with an exception part.
Indent for the first line of code."
  "[block name]: "
  < str & ?: & \n
  < "begin" \n
  > _ \n
  < "exception" \n
  > \n
  < "end " str | (delete-backward-char 1) ?\;)

(define-skeleton ada-exception
  "Insert an indented exception part into a block."
  ()
  < "exception" \n
  >)



(define-skeleton ada-exit
  "Insert an exit statement, prompting for loop name and condition."
  "[name of loop to exit]: "
  "exit " str & ?\  "when "
  (read-string "[exit condition]: ")
  | (delete-backward-char 6) ?\;)


(defun ada-header ()
  "Insert a descriptive header at the top of the file."
  (interactive "*")
  (save-excursion
    (goto-char (point-min))
    (if (fboundp 'make-header)
	(make-header)
      (ada-header-tmpl))))


(define-skeleton ada-header-tmpl
  "Insert a comment block containing the module title, author, etc."
  "[Description]: "
  "--                              -*- Mode: Ada -*-"
  "\n-- Filename        : " (buffer-name)
  "\n-- Description     : " str
  "\n-- Author          : " (user-full-name) 
  "\n-- Created On      : " (current-time-string)
  "\n-- Last Modified By: "
  "\n-- Last Modified On: "
  "\n-- Update Count    : 0"
  "\n-- Status          : Unknown, Use with caution!"
  "\n-- $Locker:  $"
  "\n-- $Log: ada-stmt.el,v $
; Revision 1.9  1994/11/11  20:01:20  re
; corrected RCS keywords in ada-header.
;\n")

(define-skeleton ada-display-comment
  "Inserts three comment lines, making a display comment."
  ()
  "--\n-- " _ "\n--")



(define-skeleton ada-if
  "Insert skeleton if statment, prompting for a boolean-expression."
  "[condition]: "
  "if " str " then" \n
  > _ \n
  < "end if;")

(define-skeleton ada-elsif
  "Add an elsif clause to an if statement, 
prompting for the boolean-expression."
  "[condition]: "
  < "elsif " str " then" \n
  >)

(define-skeleton ada-else
  "Add an else clause inside an if-then-end-if clause."
  ()
  < "else" \n
  >)



(define-skeleton ada-loop
  "Insert a skeleton loop statement.  The exit statement is added by hand."
  "[loop name]: "
  < str & ?: & \n
  > "loop" \n
  > _ \n
  < "end loop " str | (backward-delete-char 1) ?\;)

(define-skeleton ada-for-loop
  "Build a skeleton for-loop statement, prompting for the loop parameters."
  "[loop name]: "
  < str & ?: & \n
  > "for "
  (read-string "[loop variable]: ")
  " in "
  (read-string "[range]: ")
  " loop" \n
  > _ \n
  < "end loop " str | (backward-delete-char 1) ?\;)

(define-skeleton ada-while-loop
  "Insert a skeleton while loop statement."
  "[loop name]: "
  < str & ?: & \n
  > "while "
  (read-string "[entry condition]: ")
  > _ \n
  < "end loop " str | (backward-delete-char 1) ?\;)



(define-skeleton ada-package-spec
  "Insert a skeleton package specification."
  "[package name]: "
  "package " str  " is" \n
  > _ \n
  < "end " str ?\;)

(define-skeleton ada-package-body
  "Insert a skeleton package body --  includes a begin statement."
  "[package name]: "
  "package body " str " is" \n
  > _ \n
;  < "begin" \n
  "end " str ?\;)

(define-skeleton ada-private
  "Undent and start a private section of a package spec. Reindent."
  ()
  < "private" \n
  >)



(define-skeleton ada-function-spec
  "Insert a function specification.  Prompts for name and arguments."
  "[function name]: "
  ; arg is a local variable unused by this command
  (not (setq arg (current-column)))
  "function " str (ada-get-arg-list)
  " return "
  (read-string "[result type]: ")
  (not (indent-to arg))
  > _ \n
  < "begin -- " str \n 
  > "null;" \n
    "return" \n
  < "end " str ?\;
)

(define-skeleton ada-procedure-spec
  "Insert a procedure specification, prompting for its name and arguments."
  "[procedure name]: "
  ; arg is a local variable unused by this command
  (not (setq arg (current-column)))
  "procedure " str (ada-get-arg-list) " is\n"
  (not (indent-to arg))
  > _ \n
  < "begin -- " str \n
  > "null;" \n
  < "end " str ?\; \n
)


(define-skeleton ada-subprogram-body
  "Insert frame for subprogram body.
Invoke right after `ada-function-spec' or `ada-procedure-spec'."
  ()
  > _ \n
  < "begin" \n
  > \n
  < "end "
  (save-excursion
    (let ((case-fold-search t))
      ;; Unfortunately, comments are not ignored in this string search.
      (if (re-search-backward "\\<procedure\\>\\|\\<function\\>" nil t)
	  (buffer-substring (progn (forward-sexp 2) (point))
			    (progn (forward-sexp -1) (point)))
	"NAME?")))
  ?\;)

(define-skeleton ada-separate
  "Finish a body stub with `separate'."
  ()
  > "separate;" \n
  <)

;(define-skeleton ada-with
;  "Inserts a with clause, prompting for the list of units depended upon."
;  "[list of units depended upon]: "
;  "with " str ?\;)

;(define-skeleton ada-use
;  "Inserts a use clause, prompting for the list of packages used."
;  "[list of packages used]: "
;  "use " str ?\;)
 

(define-skeleton ada-record
  "Insert a skeleton record type declaration."
  ()
  "record" \n
  > _ \n
  < "end record;")

(define-skeleton ada-subtype
  "Start insertion of a subtype declaration, prompting for the subtype name."
  "[subtype name]: "
  "subtype " str " is " _ ?\;
  (not (message "insert subtype indication.")))

(define-skeleton ada-type
  "Start insertion of a type declaration, prompting for the type name."
  "[type name]: "
  "type " str ?\(
  (read-string "[discriminant specs]: ")
  | (backward-delete-char 1) | ?\)
  " is "
  (not (message "insert type definition.")))

(define-skeleton ada-task-body
  "Insert a task body, prompting for the task name."
  "[task name]: "
  "task body " str " is\n"
  "begin\n"
  > "declare\n" > _ \n
  < "begin\n"
  > "null;\n"
  < "end;\n"
  < "end " str ";" )

(define-skeleton ada-task-spec
  "Insert a task specification, prompting for the task name."
  "[task name]: "
  "task " str (ada-get-arg-list) " is\n"
  > "entry " _ \n
  <"end " str ";" )
  
(defun ada-statement-insert-string (string)
  "Inserts cdr of STRING or prompts with car of STRING for string to insert.
Behaviour depends on the value of ada-stmt-use-prompting"
  (if stmt-use-prompting
      (read-string (car string))
      (insert-string (cdr string))))

(defun ada-get-arg-list ()
  "Read from the user a procedure or function argument list.
Add parens unless arguments absent, and insert into buffer.
Individual arguments are arranged vertically if entered one at a time.
Arguments ending with `;' are presumed single and stacked."
  (insert " (")
  (if stmt-use-prompting
      (let ((ada-arg-indent (current-column))
	    (ada-args (read-string "[arguments]: ")))
	(if (string-equal ada-args "")
	    (backward-delete-char 2)
	  (progn
	    (while (string-match ";$" ada-args)
	      (insert ada-args)
	      (newline)
	      (indent-to ada-arg-indent)
	      (setq ada-args (read-string "next argument: ")))
	    (insert ada-args ")"))))
      (insert "<arg'list>)")))

;; ---- Add Menu 'Statements' in Ada-Mode (MH)

(defun ada-add-statement-menu ()
  "Adds the menu 'Statements' to the menu-bar in Ada-Mode."
  (easy-menu-define t ada-mode-map t
		    '("Statements"
;		      ["Toggle: Prompt/Pseudo Code" toggle-prompt-pseudo t]
		      ["-------" nil nil]
		      ["Header" ada-header t]
		      ["-------" nil nil]
		      ["package Body" ada-package-body t]
		      ["package Spec" ada-package-spec t]
		      ["function Spec" ada-function-spec t]
		      ["procedure Spec" ada-procedure-spec t]
		      ["task Body" ada-task-body t]
		      ["task Spec" ada-task-spec t]
		      ["declare Block" ada-declare-block t]
		      ["exception Block" ada-exception-block t]
		      ["--" nil nil]
		      ["type" ada-type t]
		      ["private" ada-private t]
		      ["subtype" ada-subtype t]
		      ["record" ada-record t]
		      ["array" ada-array t]
		      ["------" nil nil]
		      ["if" ada-if t]
		      ["else" ada-else t]
		      ["elsif" ada-elsif t]
		      ["case" ada-case t]
		      ["-----" nil nil]
		      ["while Loop" ada-while-loop t]
		      ["for Loop" ada-for-loop t]
		      ["loop" ada-loop t]
		      ["---" nil nil]
;		      ["exception" ada-exception t]
;		      ["exit" ada-exit t]
;		      ["when" ada-when t]
		      )))

(add-hook 'ada-mode-hook 'ada-add-statement-menu)

(provide 'ada-stmt)
;;; ada-stmt.el ends here



