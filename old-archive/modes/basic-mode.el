;; basic-mode.el --- A mode for editing Visual Basic programs.

;; Copyright (C) 1996, Fred White <fwhite@world.std.com>

;; Author: Fred White <fwhite@world.std.com>
;; Version: 1.0 (April 18, 1996)
;; Keywords: languages basic

;; LCD Archive Entry:
;; basic-mode|Fred White|fwhite@world.std.com|
;; A mode for editing Visual Basic programs.|
;; 18-Apr-1997|1.0|~/modes/basic-mode.el.gz|

;; This file is NOT part of GNU Emacs but the same permissions apply.
;;
;; GNU Emacs  is free software;  you can redistribute it and/or modify
;; it under the terms of  the GNU General  Public License as published
;; by  the Free Software  Foundation;  either version  2, or (at  your
;; option) any later version.
;;
;; GNU  Emacs is distributed  in the hope that  it will be useful, but
;; WITHOUT    ANY  WARRANTY;  without even the     implied warranty of
;; MERCHANTABILITY or FITNESS FOR A  PARTICULAR PURPOSE.  See the  GNU
;; General Public License for more details.
;;
;; You should have received  a copy of  the GNU General Public License
;; along with GNU Emacs; see  the file COPYING.  If  not, write to the
;; Free Software Foundation, 675  Mass Ave, Cambridge, MA 02139,  USA.
;; This  program  is free  software;  you  can  redistribute it and/or
;; modify it  under  the terms of the  GNU  General Public License  as
;; published by the Free Software  Foundation; either version 2 of the
;; License, or (at your option) any later version.


;; Purpose of this package:
;;  This is a simple mode for editing programs written in The Most
;;  Popular Programming Language In The World.  It features automatic
;;  indentation, font locking, keyword capitalization, and some minor
;;  convenience functions.

;; Installation instructions
;;  Put basic-mode.el somewhere in your path, compile it, and add the
;;  following to your init file:

;;  (autoload 'basic-mode "basic-mode" "Basic mode." t)
;;  (setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\)$" . basic-mode)) auto-mode-alist))

;; Of course, under Windows 3.1, you'll have to name this file
;; something shorter than basic-mode.el



;; Known bugs:
;;  Doesn't know about ":" separated stmts
;;  Doesn't know about single-line IF stmts


;; todo:
;;  fwd/back-compound-statement
;;  completion over OCX methods and properties.
;;  ensure Then at the end of IF statements.
;;  IDE integration
;;  etc.


(provide 'basic-mode)

(defvar basic-xemacs-p (string-match "XEmacs\\|Lucid" (emacs-version)))
(defvar basic-winemacs-p (string-match "Win-Emacs" (emacs-version)))

;; Variables you may want to customize.
(defvar basic-mode-indent 2 "*Default indentation per nesting level")
(defvar basic-fontify-p t "*Whether to fontify Basic buffers.")
(defvar basic-capitalize-keywords-p t "*Whether to capitalize BASIC keywords.")
(defvar basic-wild-files "*.frm *.bas *.cls" "*Wildcard pattern for BASIC source files")
(defvar basic-ide-pathname nil
  "*The full pathname of your Visual Basic exe file, if any.")


(defvar basic-keywords-to-highlight '("Dim" "If" "Then" "Else" "ElseIf" "End If")
  "*A list of keywords to highlight in Basic mode, or T, meaning all keywords")

(defvar basic-defn-templates
  (list "Public Sub ()\nEnd Sub\n\n"
	"Public Function () As Variant\nEnd Function\n\n"
	"Public Property Get ()\nEnd Property\n\n")
  "*List of function templates though which basic-new-sub cycles.")



(defvar basic-mode-syntax-table nil)
(if basic-mode-syntax-table
    ()
  (setq basic-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\' "\<" basic-mode-syntax-table) ; Comment starter
  (modify-syntax-entry ?\n ">" basic-mode-syntax-table)
  (modify-syntax-entry ?\\ "w" basic-mode-syntax-table)  ; backslash is not an escape.
  (modify-syntax-entry ?_ "w" basic-mode-syntax-table))


(defvar basic-mode-map nil)
(if basic-mode-map
    ()
  (setq basic-mode-map (make-sparse-keymap))
  (define-key basic-mode-map "\t" 'basic-indent-line)
  (define-key basic-mode-map "\r" 'basic-newline-and-indent)
  (define-key basic-mode-map "\M-\C-a" 'basic-beginning-of-defun)
  (define-key basic-mode-map "\M-\C-e" 'basic-end-of-defun)
  (define-key basic-mode-map "\M-\C-h" 'basic-mark-defun)
  (define-key basic-mode-map "\M-\C-\\" 'basic-indent-region)
  (define-key basic-mode-map "\M-q" 'basic-fill-or-indent)
  (if basic-xemacs-p
      (progn
	(if basic-winemacs-p
	    (define-key basic-mode-map '(control C) 'basic-start-ide))
	(define-key basic-mode-map "\M-G" 'basic-grep)
	(define-key basic-mode-map '(meta backspace) 'backward-kill-word)
	(define-key basic-mode-map '(control meta /) 'basic-new-sub))))


;; These abbrevs are valid only in a code context.
(defvar basic-mode-abbrev-table nil)

(defvar basic-mode-hook ())


;; Is there a way to case-fold all regexp matches?

(defconst basic-defun-start-regexp
  "^[ \t]*\\([Pp]ublic \\|[Pp]rivate \\|[Ss]tatic \\)*\\([Ss]ub\\|[Ff]unction\\|[Pp]roperty +[GgSsLl]et\\|[Tt]ype\\)[ \t]+\\(\\w+\\)[ \t]*(?")

(defconst basic-defun-end-regexp
  "^[ \t]*[Ee]nd \\([Ss]ub\\|[Ff]unction\\|[Pp]roperty\\|[Tt]ype\\)")


;; Includes the compile-time #if variation.
(defconst basic-if-regexp "^[ \t]*#?[Ii]f")
(defconst basic-else-regexp "^[ \t]*#?[Ee]lse\\([Ii]f\\)?")
(defconst basic-endif-regexp "[ \t]*#?[Ee]nd[ \t]*[Ii]f")

(defconst basic-continuation-regexp "^.*\\_[ \t]*$")
(defconst basic-label-regexp "^[ \t]*[a-zA-Z0-9_]+:$")

(defconst basic-select-regexp "^[ \t]*[Ss]elect[ \t]+[Cc]ase")
(defconst basic-case-regexp "^[ \t]*[Cc]ase")
(defconst basic-select-end-regexp "^[ \t]*[Ee]nd[ \t]+[Ss]elect")

(defconst basic-for-regexp "^[ \t]*[Ff]or")
(defconst basic-next-regexp "^[ \t]*[Nn]ext")

(defconst basic-do-regexp "^[ \t]*[Dd]o")
(defconst basic-loop-regexp "^[ \t]*[Ll]oop")

(defconst basic-while-regexp "^[ \t]*[Ww]hile")
(defconst basic-wend-regexp "^[ \t]*[Ww]end")

(defconst basic-with-regexp "^[ \t]*[Ww]ith")
(defconst basic-end-with-regexp "^[ \t]*[Ee]nd[ \t]+[Ww]ith")

(defconst basic-blank-regexp "^[ \t]*$")
(defconst basic-comment-regexp "^[ \t]*\\s<.*$")


;; This is some approximation of the set of reserved words in Visual Basic.
(defconst basic-all-keywords
  '("Aggregate" "And" "App" "AppActivate" "Application" "Array" "As"
    "Asc" "AscB" "Atn" "Beep" "BeginTrans" "ByVal" "CBool" "CByte" "CCur"
    "CDate" "CDbl" "CInt" "CLng" "CSng" "CStr" "CVErr" "CVar" "Call"
    "Case" "ChDir" "ChDrive" "Character" "Choose" "Chr" "ChrB"
    "ClassModule" "Clipboard" "Close" "Collection" "Column" "Columns"
    "Command" "CommitTrans" "CompactDatabase" "Component" "Components"
    "Const" "Container" "Containers" "Cos" "CreateDatabase" "CreateObject"
    "CurDir" "Currency" "DBEngine" "DDB" "Data" "Database" "Databases"
    "Date" "DateAdd" "DateDiff" "DatePart" "DateSerial" "DateValue" "Day"
    "Debug" "Declare" "Deftype" "DeleteSetting" "Dim" "Dir" "Do" "Domain"
    "Double" "Dynaset" "EOF" "Each" "Else" "End" "Environ" "Erase" "Err"
    "Error" "Exit" "Exp" "FV" "False" "Field" "Fields" "FileAttr"
    "FileCopy" "FileDateTime" "FileLen" "Fix" "Font" "For" "Form"
    "FormTemplate" "Format" "Forms" "FreeFile" "FreeLocks" "Function"
    "Get" "GetAllSettings" "GetAttr" "GetObject" "GetSetting" "GoSub"
    "GoTo" "Group" "Groups" "Hex" "Hour" "IIf" "IMEStatus" "IPmt" "IRR"
    "If" "InStr" "Input" "Int" "Integer" "Is" "IsArray" "IsDate" "IsEmpty"
    "IsError" "IsMissing" "IsNull" "IsNumeric" "IsObject" "Kill" "LBound"
    "LCase" "LOF" "LSet" "LTrim" "Left" "Len" "Let" "Like" "Line" "Load"
    "LoadPicture" "LoadResData" "LoadResPicture" "LoadResString" "Loc"
    "Lock" "Log" "Long" "Loop" "MDIForm" "MIRR" "Me" "MenuItems"
    "MenuLine" "Mid" "Minute" "MkDir" "Month" "MsgBox" "NPV" "NPer" "Name"
    "New" "Next" "Now" "Oct" "On" "Open" "OpenDatabase" "Operator"
    "Option" "PPmt" "PV" "Parameter" "Parameters" "Partition" "Picture"
    "Pmt" "Print" "Printer" "Printers" "Private" "ProjectTemplate"
    "Properties" "Public" "Put" "QBColor" "QueryDef" "QueryDefs" "RGB"
    "RSet" "RTrim" "Randomize" "Rate" "ReDim" "Recordset" "Recordsets"
    "RegisterDatabase" "Relation" "Relations" "Rem" "RepairDatabase"
    "Reset" "Resume" "Return" "Right" "RmDir" "Rnd" "Rollback" "RowBuffer"
    "SLN" "SYD" "SavePicture" "SaveSetting" "Screen" "Second" "Seek"
    "SelBookmarks" "Select" "SelectedComponents" "SendKeys" "Set"
    "SetAttr" "SetDataAccessOption" "SetDefaultWorkspace" "Sgn" "Shell"
    "Sin" "Single" "Snapshot" "Space" "Spc" "Sqr" "Static" "Stop" "Str"
    "StrComp" "StrConv" "String" "Sub" "SubMenu" "Switch" "Tab" "Table"
    "TableDef" "TableDefs" "Tan" "Then" "Time" "TimeSerial" "TimeValue"
    "Timer" "To" "Trim" "True" "Type" "TypeName" "UBound" "UCase" "Unload"
    "Unlock" "Val" "VarType" "Verb" "Weekday" "Wend"
    "While" "Width" "With" "Workspace" "Workspaces" "Write" "Year"))


(defun basic-word-list-regexp (keys)
  (let ((re "\\b\\(")
	(key nil))
    (while keys
      (setq key (car keys)
	    keys (cdr keys))
      (setq re (concat re key (if keys "\\|" ""))))
    (concat re "\\)\\b")))

(defun basic-keywords-to-highlight ()
  (if (eq basic-keywords-to-highlight t)
      basic-all-keywords
    basic-keywords-to-highlight))


(defvar basic-font-lock-keywords
  (list
   ;; Names of functions.
   (list basic-defun-start-regexp 3 'font-lock-function-name-face)

   ;; Statement labels
   (cons basic-label-regexp 'font-lock-keyword-face)

   ;; Case values
   ;; String-valued cases get font-lock-string-face regardless.
   (list "^[ \t]*[Cc]ase[ \t]+\\([^'\n]+\\)" 1 'font-lock-keyword-face t)

   ;; Any keywords you like.
   (cons (basic-word-list-regexp (basic-keywords-to-highlight))
	 'font-lock-keyword-face)))



(defun basic-mode ()
  "A mode for editing Microsoft Visual Basic programs.
Features automatic  indentation, font locking, keyword capitalization, 
and some minor convenience functions.
Commands:
\\{basic-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map basic-mode-map)
  (setq major-mode 'basic-mode)
  (setq mode-name "Basic")
  (set-syntax-table basic-mode-syntax-table)

  (add-hook 'write-file-hooks 'basic-untabify)

  (setq local-abbrev-table basic-mode-abbrev-table)
  (if basic-capitalize-keywords-p
      (progn
	(make-local-variable 'pre-abbrev-expand-hook)
	(add-hook 'pre-abbrev-expand-hook 'basic-pre-abbrev-expand-hook)
	(abbrev-mode 1)))


  (make-local-variable 'comment-start)
  (setq comment-start "' ")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "'+ *")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-end)
  (setq comment-end "")

  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'basic-indent-line)

  (make-local-variable 'font-lock-keywords)
  (setq font-lock-keywords basic-font-lock-keywords)

  (if basic-fontify-p
      (font-lock-mode 1))

  (run-hooks 'basic-mode-hook))


(defun basic-construct-keyword-abbrev-table ()
  (if basic-mode-abbrev-table
      nil
    (let ((words basic-all-keywords)
	  (word nil)
	  (list nil))
      (while words
	(setq word (car words)
	      words (cdr words))
	(setq list (cons (list (downcase word) word) list)))

      (define-abbrev-table 'basic-mode-abbrev-table list))))

(basic-construct-keyword-abbrev-table)


(defun basic-in-code-context-p ()
  (if (fboundp 'buffer-syntactic-context) ; XEmacs function.
      (null (buffer-syntactic-context))
    ;; Attempt to simulate buffer-syntactic-context
    ;; I don't know how reliable this is.
    (let* ((beg (save-excursion
		  (beginning-of-line)
		  (point)))
	   (list
	    (parse-partial-sexp beg (point))))
      (and (null (nth 3 list))		; inside string.
	   (null (nth 4 list))))))	; inside comment

(defun basic-pre-abbrev-expand-hook ()
  ;; Allow our abbrevs only in a code context.
  (setq local-abbrev-table
	(if (basic-in-code-context-p)
	    basic-mode-abbrev-table)))
	 
	

(defun basic-newline-and-indent (&optional count)
  "Insert a newline, updating indentation."
  (interactive)
  (expand-abbrev)
  (basic-indent-line)
  (call-interactively 'newline-and-indent))
  
(defun basic-beginning-of-defun ()
  (interactive)
  (re-search-backward basic-defun-start-regexp))

(defun basic-end-of-defun ()
  (interactive)
  (re-search-forward basic-defun-end-regexp))

(defun basic-mark-defun ()
  (interactive)
  (beginning-of-line)
  (basic-end-of-defun)
  (set-mark (point))
  (basic-beginning-of-defun)
  (if basic-xemacs-p
      (zmacs-activate-region)))

(defun basic-indent-defun ()
  (interactive)
  (save-excursion
    (basic-mark-defun)
    (call-interactively 'basic-indent-region)))


(defun basic-fill-long-comment ()
  "Fills block of comment lines around point."
  ;; Derived from code in ilisp-ext.el.
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((comment-re "^[ \t]*\\s<+[ \t]*"))
      (if (looking-at comment-re)
	  (let ((fill-prefix
		 (buffer-substring
		  (progn (beginning-of-line) (point))
		  (match-end 0))))

	    (while (and (not (bobp))
			(looking-at basic-comment-regexp))
	      (forward-line -1))
	    (if (not (bobp)) (forward-line 1))

	    (let ((start (point)))

	      ;; Make all the line prefixes the same.
	      (while (and (not (eobp))
			  (looking-at comment-re))
		(replace-match fill-prefix)
		(forward-line 1))

	      (if (not (eobp))
		  (beginning-of-line))

	      ;; Fill using fill-prefix
	      (fill-region-as-paragraph start (point))))))))


(defun basic-fill-or-indent ()
  "Fill long comment around point, if any, else indent current definition."
  (interactive)
  (cond ((save-excursion
	   (beginning-of-line)
	   (looking-at basic-comment-regexp))
	 (basic-fill-long-comment))
	(t
	 (basic-indent-defun))))


(defun basic-new-sub ()
  "Insert template for a new subroutine. Repeat to cycle through alternatives."
  (interactive)
  (beginning-of-line)
  (let ((templates (cons basic-blank-regexp
			 basic-defn-templates))
	(tem nil)
	(bound (point)))
    (while templates
      (setq tem (car templates)
	    templates (cdr templates))
      (cond ((looking-at tem)
	     (replace-match (or (car templates)
				""))
	     (setq templates nil))))

    (search-backward "()" bound t)))


(defun basic-untabify ()
  "Do not allow any tabs into the file"
  (if (eq major-mode 'basic-mode)
      (untabify (point-min) (point-max)))
  nil)

(defun basic-default-tag ()
  (if (and (not (bobp))
	   (save-excursion
	     (backward-char 1)
	     (looking-at "\\w")))
      (backward-word 1))
  (let ((s (point))
	(e (save-excursion
	     (forward-word 1)
	     (point))))
    (buffer-substring s e)))

(defun basic-grep (tag)
  "Search BASIC source files in current directory for tag."
  (interactive
   (list (let* ((def (basic-default-tag))
		(tag (read-string
		      (format "Grep for [%s]: " def))))
	   (if (string= tag "") def tag))))

  (grep (format "grep -n %s %s" tag basic-wild-files)))



(defun basic-start-ide ()
  "Start Visual Basic (or your favorite IDE, (after Emacs, of course))
on the project file in the current directory.
Note: it's not a good idea to leave Visual Basic running while you
are editing in emacs, since Visual Basic has no provision for reloading
changed files."
  (interactive)
  (let (file)
    (cond ((not (fboundp 'win-exec))
	   (error "Not available"))
	  ((null basic-ide-pathname)
	   (error "No pathname set for Visual Basic. See basic-ide-pathname"))
	  ((setq file (car (directory-files (pwd) t "\\.vbp")))
	   (iconify-emacs)
	   (win-exec basic-ide-pathname 'win-show-normal file))
	  (t
	   (error "No project file found.")))))



;;; Indentation-related stuff.

(defun basic-indent-region (start end)
  "Perform basic-indent-line on each line in region."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (beginning-of-line)
    (while (and (not (eobp))
		(< (point) end))
      (if (not (looking-at basic-blank-regexp))
	  (basic-indent-line))
      (forward-line 1)))

  (cond ((fboundp 'zmacs-deactivate-region)
	 (zmacs-deactivate-region))
	((fboundp 'deactivate-mark)
	 (deactivate-mark))))



(defun basic-previous-line-of-code ()
  (if (not (bobp))
      (forward-line -1))	; previous-line depends on goal column
  (while (and (not (bobp))
	      (or (looking-at basic-blank-regexp)
		  (looking-at basic-comment-regexp)))
    (forward-line -1)))


(defun basic-find-original-statement ()
  ;; If the current line is a continuation from the previous, move
  ;; back to the original stmt.
  (let ((here (point)))
    (basic-previous-line-of-code)
    (while (and (not (bobp))
		(looking-at basic-continuation-regexp))
      (setq here (point))
      (basic-previous-line-of-code))
    (goto-char here)))

(defun basic-find-matching-stmt (open-regexp close-regexp)
  ;; Searching backwards
  (let ((level 0))
    (while (and (>= level 0) (not (bobp)))
      (basic-previous-line-of-code)
      (basic-find-original-statement)
      (cond ((looking-at close-regexp)
	     (setq level (+ level 1)))
	    ((looking-at open-regexp)
	     (setq level (- level 1)))))))

(defun basic-find-matching-if ()
  (basic-find-matching-stmt basic-if-regexp basic-endif-regexp))

(defun basic-find-matching-select ()
  (basic-find-matching-stmt basic-select-regexp basic-select-end-regexp))

(defun basic-find-matching-for ()
  (basic-find-matching-stmt basic-for-regexp basic-next-regexp))

(defun basic-find-matching-do ()
  (basic-find-matching-stmt basic-do-regexp basic-loop-regexp))

(defun basic-find-matching-while ()
  (basic-find-matching-stmt basic-while-regexp basic-wend-regexp))

(defun basic-find-matching-with ()
  (basic-find-matching-stmt basic-with-regexp basic-end-with-regexp))


(defun basic-calculate-indent ()
  (let ((original-point (point)))
    (save-excursion
      (beginning-of-line)
      ;; Some cases depend only on where we are now.
      (cond ((or (looking-at basic-defun-start-regexp)
		 (looking-at basic-label-regexp)
		 (looking-at basic-defun-end-regexp))
	     0)

	    ;; The outdenting stmts, which simply match their original.
	    ((or (looking-at basic-else-regexp)
		 (looking-at basic-endif-regexp))
	     (basic-find-matching-if)
	     (current-indentation))

	    ;; All the other matching pairs act alike.
	    ((looking-at basic-next-regexp) ; for/next
	     (basic-find-matching-for)
	     (current-indentation))

	    ((looking-at basic-loop-regexp) ; do/loop
	     (basic-find-matching-do)
	     (current-indentation))

	    ((looking-at basic-wend-regexp) ; while/wend
	     (basic-find-matching-while)
	     (current-indentation))

	    ((looking-at basic-with-regexp) ; with/end with
	     (basic-find-matching-with)
	     (current-indentation))

	    ((looking-at basic-select-end-regexp) ; select case/end select
	     (basic-find-matching-select)
	     (current-indentation))

	    ;; A case of a select is somewhat special.
	    ((looking-at basic-case-regexp)
	     (basic-find-matching-select)
	     (+ (current-indentation) basic-mode-indent))

	    (t
	     ;; Other cases which depend on the previous line.
	     (basic-previous-line-of-code)

	     ;; Skip over label lines, which always have 0 indent.
	     (while (looking-at basic-label-regexp)
	       (basic-previous-line-of-code))

	     (cond 
	      ((looking-at basic-continuation-regexp)
	       (basic-find-original-statement)
	       ;; Indent continuation line under matching open paren,
	       ;; or else one word in.
	       (let* ((orig-stmt (point))
		      (matching-open-paren
		       (condition-case ()
			   (save-excursion
			     (goto-char original-point)
			     (beginning-of-line)
			     (backward-up-list 1)
			     ;; Only if point is now w/in cont. block.
			     (if (<= orig-stmt (point))
				 (current-column)))
			 (error nil))))
		 (cond (matching-open-paren
			(1+ matching-open-paren))
		       (t
			;; Else, after first word on original line.
			(back-to-indentation)
			(forward-word 1)
			(while (looking-at "[ \t]")
			  (forward-char 1))
			(current-column)))))
	      (t
	       (basic-find-original-statement)
	       (let ((indent (current-indentation)))
		 ;; All the various +indent regexps.
		 (cond ((looking-at basic-defun-start-regexp)
			(+ indent basic-mode-indent))

		       ((or (looking-at basic-if-regexp)
			    (looking-at basic-else-regexp))
			(+ indent basic-mode-indent))

		       ((or (looking-at basic-select-regexp)
			    (looking-at basic-case-regexp))
			(+ indent basic-mode-indent))
			
		       ((or (looking-at basic-do-regexp)
			    (looking-at basic-for-regexp)
			    (looking-at basic-while-regexp)
			    (looking-at basic-with-regexp))
			(+ indent basic-mode-indent))

		       (t
			;; By default, just copy indent from prev line.
			indent))))))))))

(defun basic-indent-to-column (col)
  (let* ((bol (save-excursion
		(beginning-of-line)
		(point)))
	 (point-in-whitespace
	  (<= (point) (+ bol (current-indentation))))
	 (blank-line-p
	  (save-excursion
	    (beginning-of-line)
	    (looking-at basic-blank-regexp))))

    (cond ((/= col (current-indentation))
	   (save-excursion
	     (beginning-of-line)
	     (back-to-indentation)
	     (delete-region bol (point))
	     (indent-to col))))

    ;; If point was in the whitespace, move back-to-indentation.
    (cond (blank-line-p
	   (end-of-line))
	  (point-in-whitespace
	   (back-to-indentation)))))


(defun basic-indent-line ()
  "Indent current line for BASIC"
  (interactive)
  (basic-indent-to-column (basic-calculate-indent)))
