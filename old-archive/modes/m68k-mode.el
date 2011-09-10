;;; m68k-mode  -  Major mode for editing m680x0 code in emacs.
;;; Copyright (C) 1993 by Espen Skoglund (espensk@stud.cs.uit.no)
;;; Thanks to Chee Seng Chau for his ideas and bug-reports.
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;; INSTALLATION
;;; ============
;;; Put the m68k-mode.el file in your load-path for .el files and optionally
;;; byte compile it. If you don't know the load path of your emacs, try:
;;;   C-h v load-path
;;;
;;; Add these lines to your .emacs if you want to enter m68k-mode automatic
;;; when loading an assembler file.
;;;
;;; ;;; m68k-mode custumization.
;;; (autoload 'm68k-mode "m68k-mode" "Major mode for editing m68k code." nil t)
;;; (setq auto-mode-alist (append (list (cons "\\.s$" 'm68k-mode)
;;;                                     (cons "\\.asm$" 'm68k-mode))
;;;                       auto-mode-alist))
;;;
;;; If you want to customize the m68k mode in your startup file, you
;;; can add these lines to your .emacs file:
;;;
;;; ;; User specifications
;;; (setq m68k-indent-level       8
;;;       m68k-indent-level-2     8
;;;       m68k-auto-indent        nil
;;;       m68k-toggle-completions nil)
;;; USAGE
;;; =====
;;; If you have modified your startup file as described above, emacs
;;; should enter m68k-mode when you load a m68k source into emacs.
;;; If not, you will have to start m68k-mode manually:
;;;    M-x load-library m68k-mode
;;;    M-x m68k-mode
;;; When you have entered m68k-mode, you may get more info by pressing
;;; C-h m. You may also get online help describing various functions by:
;;; C-h d <Name of function you want described>
;;; KNOWN BUGS / BUGREPORTS
;;; =======================
;;; If you find any bugs, or have any suggestions for improvement, you 
;;; may reach me by email at: espensk@stud.cs.uit.no
;;; TODO
;;; ====
;;; * Implement cycle tables for other processors than the M68000.
;;; HISTORY
;;; =======
;;; 25-Feb-94:
;;;   Made the key-bindings in m68k-search-register more general. Added
;;;   the functions m68k-byte-count and m68k-toggle-variables and removed
;;;   m68k-toggle-never-branch.
;;; 26-Nov-93:
;;;   Added m68k-show-completions function, and modified the m68k-
;;;   complete-label function to handle the new m68k-toggle-completions
;;;   user option. Defined all user options with defvar instead of
;;;   defconst. Removed define-key bug. Added m68k-comment-region and
;;;   m68k-uncomment-region functions.
;;; 21-Nov-93:
;;;   First release of m68k-mode.
;;; LCD Archive Entry:
;;; m68k-mode|Espen Skoglund|espensk@stud.cs.uit.no|
;;; Major mode for editing m680x0 code|
;;; 25-Feb-1994|1.2|~/modes/m68k-mode.el.Z|

;;; When the complete.el package by Dave Gillespie is used, PC-first-char
;;; has to be set to t for the label completion to work properly.
(if (boundp 'PC-first-char)
    (setq PC-first-char t))
(defvar m68k-mode-abbrev-table nil
  "Abbrev table in use in m68k-mode buffers.")
(define-abbrev-table 'm68k-mode-abbrev-table ())
(defvar m68k-mode-map ()
  "Keymap used in m68k mode.")
(if m68k-mode-map
    ()
  (setq m68k-mode-map (make-sparse-keymap))
  (define-key m68k-mode-map "\177"     'backward-delete-char-untabify)
  (define-key m68k-mode-map "\t"       'electric-m68k-tab)
  (define-key m68k-mode-map "\e\t"     'm68k-complete-label)
  (define-key m68k-mode-map "\e?"      'm68k-show-completions)
  (define-key m68k-mode-map "\C-c\t"   'm68k-insert-tab)
  (define-key m68k-mode-map "\C-cs"    'm68k-show-cycles)
  (define-key m68k-mode-map "\C-c\C-s" 'm68k-show-cycles-region)
  (define-key m68k-mode-map "\C-c\C-t" 'm68k-toggle-variables)
  (define-key m68k-mode-map "\C-c\C-b" 'm68k-byte-count)
  (define-key m68k-mode-map "\C-c\C-d" 'm68k-label-distance)
  (define-key m68k-mode-map "\C-c\C-v" 'm68k-view-label)
  (define-key m68k-mode-map "\C-cg"    'm68k-goto-label)
  (define-key m68k-mode-map "\C-cc"    'm68k-comment-region)
  (define-key m68k-mode-map "\C-cu"    'm68k-uncomment-region)
  (define-key m68k-mode-map "\e\C-a"   'm68k-beginning-of-defun)
  (define-key m68k-mode-map "\e\C-e"   'm68k-end-of-defun)
  (define-key m68k-mode-map "\C-cr"    'm68k-register-check)
  (define-key m68k-mode-map "\C-c\C-r" 'm68k-register-summary)
  (define-key m68k-mode-map "\C-c\C-n" 'm68k-search-register-forw)
  (define-key m68k-mode-map "\C-c\C-p" 'm68k-search-register-back))
(defvar m68k-mode-syntax-table nil
  "Syntax table in use in m68k-mode buffers.")
(if m68k-mode-syntax-table
    ()
  (setq m68k-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\\ "\\" m68k-mode-syntax-table)
  (modify-syntax-entry ?\; "<" m68k-mode-syntax-table)
  (modify-syntax-entry ?* "<" m68k-mode-syntax-table)
  (modify-syntax-entry ?\n ">" m68k-mode-syntax-table)
  (modify-syntax-entry ?+ "." m68k-mode-syntax-table)
  (modify-syntax-entry ?- "." m68k-mode-syntax-table)
  (modify-syntax-entry ?= "." m68k-mode-syntax-table)
  (modify-syntax-entry ?% "." m68k-mode-syntax-table)
  (modify-syntax-entry ?< "." m68k-mode-syntax-table)
  (modify-syntax-entry ?> "." m68k-mode-syntax-table)
  (modify-syntax-entry ?& "." m68k-mode-syntax-table)
  (modify-syntax-entry ?# "." m68k-mode-syntax-table)
  (modify-syntax-entry ?| "." m68k-mode-syntax-table)
  (modify-syntax-entry ?: "w" m68k-mode-syntax-table)
  (modify-syntax-entry ?_ "w" m68k-mode-syntax-table)
  (modify-syntax-entry ?. "w" m68k-mode-syntax-table)
  (modify-syntax-entry ?\' "\"" m68k-mode-syntax-table))
(defconst m68k-word "a-zA-Z0-9_."
  "Characters used in words (identifiers, instructions, etc.)")
(defconst m68k-label-re "\\(^[a-zA-Z_][a-zA-Z0-9_.]*\\)\\|\\(\\<\\([a-zA-Z_][a-zA-Z0-9_.]*\\):\\)"
  "Regexp to match a label.")
(defconst m68k-local-label-re "\\(^\\.[a-zA-Z_][a-zA-Z0-9_.]*\\|[0-9]+\\$\\)\\|\\(\\<\\(\\.[a-zA-Z_][a-zA-Z0-9_.]*\\|[0-9]+\\$\\):\\)"
  "Regexp to match a local label.")
(defvar m68k-indent-level 8
  "*Default indettationlevel of instructions.")
(defvar m68k-indent-level-2 8
  "*Indentation of source/destination relatively to instruction indent.")
(defvar m68k-auto-indent nil
  "*Non-nil means RET should do a newline-and-indent, while LFD should do only
newline. (For those who don't want to press TAB all the time.)")
(defvar m68k-toggle-completions nil
  "*Non-nil means that \\<m68k-mode-map>\\[m68k-complete-label] should not \
display a completion buffer when
the label couldn't be completed, but instead toggle the possible completions
with repeated \\[m68k-complete-label]'s.")

(defun m68k-mode ()
  "Major mode for editing M68000 code. \\<m68k-mode-map>
Pressing TAB at the beginning of a line indents to the same level as the
instruction of last line, or to a constant value. Pressing TAB just after
an instruction indents source/destination relatively to the instruction,
and pressing TAB after the destination indents to the same level as the
comment on last line or just inserts a tab-character.
Pressing \\[m68k-insert-tab] simply inserts a tab-character in the buffer.
By pressing \\[m68k-complete-label] you can complete the label name you are \
currently typing,
and \\[m68k-show-completions] shows all possible completions at current point.
Delete converts tabs to spaces as it moves back.
Moving in the buffer
* \\[m68k-beginning-of-defun]
  Move to beginning of the current function. A function is considered
  to start at a global label, and end at the next global label.
* \\[m68k-end-of-defun]
  Move to the end of the current function.
* \\[m68k-goto-label]
  Move to a label prompted for in minibuffer (with default options).
* \\[m68k-view-label]
  Look at label prompted for in minibuffer (with default options). The line
  with the label is shown in the minibuffer.
* \\[m68k-search-register-forw]
  Move to next/previous register prompted for in minibuffer. When searching,
  these key-sequences are special:
    C-n  Search forward.
    C-p  Search backward.
    C-y  Yank last search string into minibuffer. (Also C-c C-n and C-c C-p)
    C-q  Quit searching, leaving point at last hit.
    C-g  Abort searching, leaving point at original position.
* \\[m68k-search-register-back]
  Same as \\[m68k-search-register-forw] except that this initially starts \
searching backwards.
Calculating instruction timings
* \\[m68k-show-cycles]
  Show cycle timing of current instruction.
* \\[m68k-show-cycles-region]
  Show total cycle timing of instructions in marked region.
Check register usage
* \\[m68k-register-check]
  You will be prompted for a register, and will be told how many times
  that register is used within the marked region.
* \\[m68k-register-summary]
  A temporary window will show you how many times the different registers
  were accessed in the marked region.
Misc. functions
* \\[m68k-byte-count]
  Count number of bytes within the marked region.
* \\[m68k-label-distance]
  Get distance from label prompted for in minibuffer.
* \\[m68k-toggle-variables]
  Toggle a user-specified variable prompted for in minibuffer.
Variables for customizing indentation style etc. :
  m68k-indent-level        (default 8)
  Default indentation-level of instructions.
  m68k-indent-level-2      (default 8)
  Indentation of source/destination relatively to the instruction indent.
  m68k-auto-indent         (default nil)
  Non-nil means RET should do a newline-and-indent, while LFD should do only
  newline. (For those who don't want to press TAB all the time.)
  m68k-toggle-completions  (default nil)
  Non-nil means that \\[m68k-complete-label] should not display a \
completion buffer when
  the label couldn't be completed, but instead toggle the possible completions
  with repeated \\[m68k-complete-label]'s.
Turning on m68k-mode calls the value of the variable m68k-mode-hook with
no args, if that value is non-nil.
Summary of all local keybindings:
\\{m68k-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map m68k-mode-map)
  (setq major-mode 'm68k-mode)
  (setq mode-name "M68k")
  (setq local-abbrev-table m68k-mode-abbrev-table)
  (set-syntax-table m68k-mode-syntax-table)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'm68k-indent)
  (setq comment-indent-function 'm68k-indent-comment)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (make-local-variable 'case-fold-search)
  (setq case-fold-search nil)
  (make-local-variable 'completion-ignore-case)
  (setq completion-ignore-case nil)
  (if m68k-auto-indent
      (progn (define-key m68k-mode-map "\r" 'newline-and-indent)
	     (define-key m68k-mode-map "\C-j" 'newline)))
  (run-hooks 'm68k-mode-hook))

(defun electric-m68k-tab ()
  (interactive)
  (m68k-indent))
(defun m68k-insert-tab ()
  (interactive)
  (insert "\t"))
(defun m68k-delete-whitespaces ()
  "Delete whitespaces around point."
  (let ((b (progn (skip-chars-backward " \t")
		  (point))))
    (skip-chars-forward " \t")
    (delete-region b (point))))
;;;
;;; Indent functions
;;;
(defun m68k-get-indent (&optional arg)
  "Return indent of instruction at current line, or nil if there was no
instruction. If optional arg is non nil, return indent arg lines ahead."
  (save-excursion
    (if arg 
	(forward-line arg)
      (beginning-of-line))
    ;; Skip label and whitespaces
    (skip-chars-forward (concat m68k-word ".:$"))
    (skip-chars-forward " \t")
    (if (looking-at "[a-zA-Z0-9_.$]+:\\>")
	(progn
	  (skip-chars-forward (concat m68k-word ".:$"))
	  (skip-chars-forward " \t")))
    ;; Return nil if there is no instruction at this line
    (if (or (looking-at "\\*\\|;") (eolp))
	nil
      ;; If not, return indent level
      (current-column))))
(defun m68k-get-comment-indent (&optional arg)
  "Return indent of comment at current line, or nil if there was no comment. 
If optional arg, return indent arg lines ahead."
  (save-excursion
    (if arg 
	(forward-line arg)
      (beginning-of-line))
    (if (re-search-forward "\\*\\|;" (save-excursion (end-of-line) (point)) t)
	(1- (current-column))
      nil)))
(defun m68k-indent ()
  "Indent for m68k code."
  (let ((indent (m68k-get-indent -1)))
    ;; We have to check what we are going to indent.
    ;; First we check for indent of mnemonic field
    (cond ((save-excursion
	     (skip-chars-backward " \t")
	     (or (= (preceding-char) ?:)
		 (progn
		   (skip-chars-backward (concat m68k-word ".$"))
		   (bolp))))
	   ;; If this is a line with a label, and we are at the right position
	   ;; already, just insert TAB
	   (if (and (save-excursion
		      (skip-chars-backward " \t")
		      (not (bolp)))
		    (>= (current-column)
			(save-excursion
			  (beginning-of-line)
			  (skip-chars-forward " \t")
			  (skip-chars-forward (concat m68k-word ":"))
			  (if (>= (current-column) m68k-indent-level)
			      (1+ (current-column))
			    m68k-indent-level))))
	       (insert "\t")
	     ;; If not indent normally
	     (progn
	       (m68k-delete-whitespaces)
	       ;; Maybe we should indent relative to last line
	       (if (not (null indent))
		   (indent-to-column indent 1)
		 (indent-to-column m68k-indent-level 1))
	       )))
	  ;; Indent operand field
	  ((save-excursion
	     (and (not (looking-at "[ \t]*[;*]"))
		  (progn
		    (skip-chars-backward " \t")
		    (skip-chars-backward m68k-word)
		    t)
		  (not (looking-at
			"\\(rt[ers]\\|nop\\|reset\\|trapv\\|illegal\\)\\>"))
		  (progn
		    (skip-chars-backward " \t")
		    (or (= (preceding-char) ?:)
			(progn (skip-chars-backward m68k-word)
			       (bolp))))))
	   (m68k-delete-whitespaces)
	   ;; Indent relatively to instruction indentation
	   (indent-to-column (+ (m68k-get-indent) m68k-indent-level-2) 1))
	  ;; Indent for comments
	  (t (m68k-indent-comment)))
    ))
(defun m68k-indent-comment ()
  "Indent for comment in m68k-mode."
  (let ((indent (m68k-get-comment-indent -1)))
    ;; Check to see if there was a comment on last line and it
    ;; was at a higher column position
    (if (and (not (null indent))
	     (< (current-column) indent))
	;; If so, indent according to last line
	(progn
	  (m68k-delete-whitespaces)
	  (indent-to-column indent 1))
      ;; If not, just insert TAB
      (insert "\t"))))
;;;
;;; Toggle variables
;;;
(defun m68k-toggle-variables ()
  "Toggle a variable prompted for in minibuffer (with completion)."
  (interactive)
  (let ((name (completing-read "Variable: " m68k-toggle-var-alist nil t))
	var)
    (setq var (intern name))
    (set var (not (symbol-value var)))
    (message "%s" (if (symbol-value var)
		      (car (cdr (assoc name m68k-toggle-var-alist)))
		    (car (cdr (cdr (assoc name m68k-toggle-var-alist))))))))
(defconst m68k-toggle-var-alist
  '(("m68k-never-branch" . ("Never branch" "Always branch"))
    ("m68k-byte-count-worst-case" . ("Use worst case" "Calculate runtime"))
    ("m68k-toggle-completions" . ("Toggle completions" "Show completion list"))
    )
  "Variables that can be toggled with \\<m68k-mode-map>\
\\[m68k-toggle-variables].")
;;;
;;; Movement
;;;
(defun m68k-beginning-of-defun ()
  "Move backwards to beginning of function.
A function is considered to start where a global label is and end where the 
next global label is."
  (interactive)
  (re-search-backward m68k-label-re nil 'move)
  (beginning-of-line))
(defun m68k-end-of-defun ()
  "Move forward to end of function.
A function is considered to start where a global label is and end where the
next global label is."
  (interactive)
  (end-of-line)
  (re-search-forward m68k-label-re nil 'move)
  (beginning-of-line))
;;;
;;; Comment/Uncomment region
;;;
(defun m68k-comment-region (b e)
  "Put ;'s at the beginning of lines in the current region.
If there are lines with ;'s in front of or behind region, include these lines 
in region. (This is to make \\<m68k-mode-map>\\[m68k-uncomment-region] to \
behave correctly.)"
  (interactive "r")
  (save-excursion
    (let ((b (progn
	       ;; Check for ;-lines in front of region
	       (goto-char b)
	       (beginning-of-line)
	       (while (save-excursion
			(and (not (bobp))
			     (progn  (forward-line -1)
				     (= (following-char) ?\;))))
		 (forward-line -1))
	       (point)))
	  (e (progn
	       ;; Check for ;-lines behind region
	       (goto-char e)
	       (beginning-of-line)
	       (while (save-excursion
			(and (progn (end-of-line)
				    (not (eobp)))
			     (progn  (forward-line 1)
				     (= (following-char) ?\;))))
		 (forward-line 1))
	       (point))))
      (goto-char b)
      ;; Insert ;'s
      (while (<= (point) e)
	(insert ";")
	(forward-line 1)))))
(defun m68k-uncomment-region ()
  "Remove ;'s at the beginning of all lines surronding point."
  (interactive)
  (save-excursion
    (let ((b (save-excursion
	       ;; Find beginning of commented region
	       (beginning-of-line)
	       (while (save-excursion
			(and (not (bobp))
			     (progn  (forward-line -1)
				     (= (following-char) ?\;))))
		 (forward-line -1))
	       (point)))
	  (e (save-excursion
	       ;; Find the end of commented region
	       (beginning-of-line)
	       (while (save-excursion
			(and (progn (end-of-line)
				    (not (eobp)))
			     (progn  (forward-line 1)
				     (= (following-char) ?\;))))
		 (forward-line 1))
	       (point))))
      (goto-char b)
      ;; Check if this really is a commented region
      (if (not (looking-at "^;"))
	  (error "Not within a commented region.")
	;; Remove ;'s
	(while (<= (point) e)
	  (delete-char 1)
	  (forward-line 1))))))
;;;
;;; Register functions
;;;
(defun m68k-register-check (reg b e)
  "Return number of times register was used in region."
  (interactive "sRegister: \nr")
  ;; Stack pointers = a7
  (if (string-match "SP\\>" reg)
      (setq reg "a7"))
  ;; Check if this is a valid register
  (or (string-match "\\<[aAdD][0-7]\\>" reg)
      (error "Unknown register"))
  (let ((cnt 0) match)
    (save-excursion
      (goto-char b)
      (while (< (point) e)
	(beginning-of-line)
	(setq match (m68k-query-register reg))
	(cond ((or (equal match 'src) (equal match 'dest))
	       (setq cnt (1+ cnt)))
	      ((equal match 'both)
	       (setq cnt (+ 2 cnt))))
	(forward-line 1))
      (if (interactive-p)
	  (cond ((= cnt 0) (message "%s not used" reg))
		((= cnt 1) (message "%s used once" reg))
		(t (message "%s used %d times" reg cnt))))
      cnt)))
(defun m68k-register-summary (b e)
  "Display summary of how many times the registers in region were used."
  (interactive "r")
  (let ((buf (current-buffer))
	(i 0) reg cnt tbuf)
    ;; Make sure we return to same window state after we've finished
    (save-window-excursion
      ;; Create buffer to show summary
      (set-buffer (setq tbuf (get-buffer-create "*Register summary*")))
      (delete-region (point-min) (point-max))
      (insert "Summary of how many times registers were used:\n")
      (display-buffer tbuf t)
      (while (< i 8)
	;; Dataregisters
	(setq reg (concat "d" (int-to-string i)))
	(set-buffer buf)
	(setq cnt (m68k-register-check reg b e))
	(set-buffer tbuf)
	(insert "\n    ")
	(insert reg (cond ((= cnt 0) " - unused")
			  ((= cnt 1) " - used 1 time")
			  (t (concat " - used " (int-to-string cnt)
				     " times"))))
	;; Addressregisters
	(aset reg 0 ?a)
	(set-buffer buf)
	(setq cnt (m68k-register-check reg b e))
	(set-buffer tbuf)
	(indent-to-column (/ (window-width) 2) 1)
	(insert reg (cond ((= cnt 0) " - unused")
			  ((= cnt 1) " - used 1 time")
			  (t (concat " - used " (int-to-string cnt) 
				     " times"))))
	(setq i (1+ i))
	)
      (momentary-string-display "" (point)))))
(defun m68k-query-reg-str (reg str)
  "Return t if register REG is used in string STR."
  (catch 'found
    ;; Stack pointer is really a7
    (if (and (string= "a7" reg) (string-match "\\<.?SP\\>" str))
	(throw 'found t))
    ;; Quick escape if there is no register used in str
    (or (string-match "\\b[dDaA][0-7]\\b" str)
	(throw 'found nil))
    ;; Quick escape is register use is obvious
    (if (string-match (concat "\\b" reg "\\b") str)
	(throw 'found t))
    ;; Check all spans in string
    (let ((gen (copy-sequence reg))
	  (x (- (aref reg 1) ?0)))
      (aset gen 1 ?.)
      (while (string-match (concat gen "-" gen) str)
	;; Check if register is within span
	(if (and (>= x (- (aref str (1+ (match-beginning 0))) ?0))
		 (<= x (- (aref str (1- (match-end 0))) ?0)))
	    (throw 'found t))
	;; Check next span
	(setq str (substring str (match-end 0))))
      ;; Register not found. Return nil
      nil)))
(defun m68k-query-register (reg)
  "Return 'src if register was used in source, 'dest if register was used in
destination, and 'both if register was used in both source and destination. Nil
if register was not used."
  (let (ret)
    ;; Check source
    (if (m68k-query-reg-str reg (m68k-get-src-str))
	(setq ret 'src))
    ;; Check destination
    (if (m68k-query-reg-str reg (m68k-get-dest-str))
	(if (equal ret 'src)
	    (setq ret 'both)
	  (setq ret 'dest)))
    ret))
(defvar m68k-search-map nil
  "Keymap used when searching for registers in m68k-mode.")
(if m68k-search-map
    ()
  (setq m68k-search-map (make-keymap))
  (define-key m68k-search-map "\C-n" 'm68k-search-register-forw)
  (define-key m68k-search-map "\C-p" 'm68k-search-register-back)
  (define-key m68k-search-map "\C-y" 'm68k-search-register)
  (define-key m68k-search-map "\C-g" 'm68k-search-abort)
  (define-key m68k-search-map "d" 'm68k-search-reg-chgreg)
  (define-key m68k-search-map "a" 'm68k-search-reg-chgreg)
  (define-key m68k-search-map "0" 'm68k-search-reg-number)
  (define-key m68k-search-map "1" 'm68k-search-reg-number)
  (define-key m68k-search-map "2" 'm68k-search-reg-number)
  (define-key m68k-search-map "3" 'm68k-search-reg-number)
  (define-key m68k-search-map "4" 'm68k-search-reg-number)
  (define-key m68k-search-map "5" 'm68k-search-reg-number)
  (define-key m68k-search-map "6" 'm68k-search-reg-number)
  (define-key m68k-search-map "7" 'm68k-search-reg-number)
  (substitute-key-definition 'undefined 'm68k-search-quit-and-exe
			     m68k-search-map)
  )
(defvar m68k-search-dir      'forward "Holds the direction of the search.")
(defvar m68k-search-reg      nil      "Holds the search register.")
(defvar m68k-search-old-reg  nil      "Holds the last register searched for.")
(defvar m68k-search-start    nil      "The position when searching started.")
(defun m68k-search-quit-and-exe ()
  (interactive)
  (setq m68k-search-old-reg m68k-search-reg)
  (setq m68k-search-reg nil)
  (use-local-map m68k-mode-map)
  ;; Make sure the whole key-sequence is recorded
  (while (input-pending-p)
    (read-char))
  ;; Execute lost command
  (command-execute (key-binding (this-command-keys)) t))
(defun m68k-search-abort ()
  (interactive)
  (goto-char m68k-search-start)
  (m68k-search-quit-and-exe))
(defun m68k-search-reg-chgreg ()
  (interactive)
  (if (not (stringp m68k-search-reg))
      (setq m68k-search-reg "  "))
  (aset m68k-search-reg 0 last-input-char)
  (m68k-search-register))
(defun m68k-search-reg-number ()
  (interactive)
  (if (not (stringp m68k-search-reg))
      (setq m68k-search-reg "  "))
  (aset m68k-search-reg 1 last-input-char)
  (m68k-search-register))
(defun m68k-search-register-back ()
  "Same as \\[m68k-search-register-forw] , except that this initially searches\
 backward."
  (interactive)
  (m68k-search-register 'backward))
(defun m68k-search-register-forw (&optional arg)
  "Search forward for next occurance of register prompted for in minibuffer.
When searching, these key-sequences are special:
  C-n  Search forward.
  C-p  Search backward.
  C-y  Yank last search string into minibuffer. (Also C-c C-n and C-c C-p)
  C-q  Quit searching, leaving point at last hit.
  C-g  Abort searching, leaving point at original position."
  (interactive)
  (m68k-search-register 'forward))
(defun m68k-search-register (&optional arg)
  "Handles searching for registers both forward, backward and yanking."
  (interactive)
  (if arg (setq m68k-search-dir arg))
  (cond ((eq m68k-search-reg 'inuse)
	 ;; Searching was used on last key-sequence too
	 (setq m68k-search-reg m68k-search-old-reg))
	((not m68k-search-reg)
	 ;; First time searching is used
	 (setq m68k-search-reg 'inuse)
	 (setq m68k-search-start (point))
	 (use-local-map m68k-search-map)))
  (if (and (stringp m68k-search-reg)
	   (string-match "[aAdD][0-7]" m68k-search-reg))
      ;; The searh string is a valid register
      (let ((start-pos (point))
	    where reg-cond)
	(message "Search %s: %s" m68k-search-dir m68k-search-reg)
	(while (null where)
	  (setq reg-cond (m68k-query-register m68k-search-reg))
	  (if (equal m68k-search-dir 'forward)
	      ;; Search forward
	      (setq where (cond ((and (< (point) (m68k-src-pos))
				      (or (equal reg-cond 'both)
					  (equal reg-cond 'src)))
				 ;; Register found in source
				 (m68k-src-pos))
				((and (< (point) (m68k-dest-pos))
				      (or (equal reg-cond 'both)
					  (equal reg-cond 'dest)))
				 ;; Register found in destination
				 (m68k-dest-pos))
				(t
				 (if (= (forward-line 1) 1)
				     ;; Reached end of buffer
				     'nowhere
				   nil))))
	    ;; Search backward
	    (setq where (cond ((and (> (point) (m68k-dest-pos))
				    (or (equal reg-cond 'both)
					(equal reg-cond 'dest)))
			       ;; Register found in destination
			       (m68k-dest-pos))
			      ((and (> (point) (m68k-src-pos))
				    (or (equal reg-cond 'both)
					(equal reg-cond 'src)))
			       ;; Register found in source
			       (m68k-src-pos))
			      (t
			       (if (= (forward-line -1) -1)
				   ;; Reached end of buffer
				   'nowhere
				 (end-of-line)
				 nil))))))
	(if (not (equal where 'nowhere))
	    ;; Register found - move to it
	    (goto-char where)
	  ;; Register not found - go back to start
	  (goto-char start-pos)
	  (message "Search %s: %s (not found)" m68k-search-dir
		   m68k-search-reg)))
    (message "Search %s: %s" m68k-search-dir (if (stringp m68k-search-reg)
						 m68k-search-reg ""))))
	  
;;;
;;; Functions to handle label completions
;;;
(defun m68k-narrow-to-function ()
  "Narrow region to just include the current function."
  (save-excursion
    (narrow-to-region (progn (m68k-end-of-defun) (point))
		      (progn (m68k-beginning-of-defun) (point)))))
(defun string-diff (str1 str2)
  "Returns index of first letter where STR1 and STR2 differs."
  (catch 'done
    (let ((diff 0))
      (while t
	(if (or (> (1+ diff) (length str1))
		(> (1+ diff) (length str2)))
	    (throw 'done diff))
	(or (equal (aref str1 diff) (aref str2 diff))
	    (throw 'done diff))
	(setq diff (1+ diff)))
      )))
(defun m68k-label-match ()
  "Return a string, the last matching label. (Have to be called just after a
search with m68k-label-re or m68k-local-label-re has been done.)"
  (if (null (match-beginning 3))
      (buffer-substring (match-beginning 1) (match-end 1))
    (buffer-substring (match-beginning 3) (match-end 3))))
(defun m68k-build-label-regexp (str &optional start)
  "Return a regexp that matches a label named STR. If optional arg is non-nil
Return a label matching a label starting with STR."
  (if (string-match "\\." str)
      (setq str (concat "\\" str)))
  (if (string-match "\\$\\>" str)
      (setq str (concat (substring str 0 -1) "\\$")))
  (if start
      (concat "\\(^" str "[a-zA-Z0-9_.]*\\$?\\>\\)\\|\\(\\<\\("
	      str "[a-zA-Z0-9_.]*\\$?\\):\\)")
    (concat "\\(^" str "\\>\\)\\|\\(\\<\\(" str "\\):\\)")))
;; Function passed to completing-read, try-completion or
;; all-completions to get completion on STR. If predicate is non-nil,
;; it must be a function to be called for every match to check if this
;; should really be a match. If flag is t, the function returns a list
;; of all possible completions. If it is nil it returns a string, the
;; longest possible completion, or t if STR is an exact match. If flag
;; is 'lambda, the function returns t if STR is an exact match, nil
;; otherwise.
(defun m68k-label-completion (str predicate flag)
  (save-excursion
    (let ((all nil)
	  (ostr str))
      ;; Set buffer to use for searching labels. This should be set
      ;; within functins which use m68k-label-completions
      (set-buffer buffer-to-use)
      ;; We may use narrowing while making completion list. Make sure this
      ;; becomes invisible to the function caller.
      (save-restriction
	;; if using local labels, we have to fix str a little
	(if (string-match "\\<[0-9.]" str)
	    (m68k-narrow-to-function)
	  ;; If we're not using local labels anyway, we have to check
	  ;; the whole buffer.
	  (widen))
	;; Build regular expression for labels
	(if (string= str "")
	    (setq str (m68k-build-label-regexp "[a-zA-Z_]" t))
	  (setq str (m68k-build-label-regexp str t)))
	(goto-char (point-min))
	;; Build a list of all possible completions
	(if (null predicate)
	    ;; If we don't need a predicate. Do a different loop to avoid
	    ;; checking for predicate for each match
	    (while (re-search-forward str nil t)
	      (setq all (cons (m68k-label-match) all)))
	  ;; We have to check the predicate for each match
	  (while (re-search-forward str nil t)
	    (if (funcall predicate (setq match (m68k-label-match)))
		(setq all (cons match all)))))
	;; Now we have built a list of all matches. Give response to caller
	(cond ((or (equal flag 'lambda) (null flag))
	       ;; This was not called by all-completions
	       (if (null all)
		   ;; Return nil if there was no matching label
		   nil
		 ;; Get longest string common in the labels
		 (let* ((elm (cdr all))
			(match (car all))
			(min (length match))
			(exact nil)
			tmp)
		   (if (string= match ostr)
		       ;; Return t if first match was an exact match
		       (setq match t)
		     (while (not (null elm))
		       ;; Find longest common string
		       (if (< (setq tmp (string-diff match (car elm))) min)
			   (progn
			     (setq min tmp)
			     (setq match (substring match 0 min))))
		       ;; Terminate with match=t if this is an exact match
		       (if (string= (car elm) ostr)
			   (progn
			     (setq match t)
			     (setq elm nil))
			 (setq elm (cdr elm)))))
		   ;; If this is a test just for exace match, return nil ot t
		   (if (and (equal flag 'lambda) (not (equal match 't)))
		       nil
		     match))))
	      ;; If flag is t, this was called by all-completions. Return
	      ;; list of all possible completions
	      (flag
	       all))
	))))
	    
(defun m68k-get-default-label ()
  "Return a string containing a label to use as default."
  (let ((str (m68k-get-src-str)))
    (cond ((string-match "^#?\\([.a-zA-Z_][a-zA-Z_0-9]+\\|[0-9]+\\$\\)" str)
	   (substring str (match-beginning 1) (match-end 1)))
	  ((string-match "^#?\\([.a-zA-Z_][a-zA-Z_0-9]+\\|[0-9]+\\$\\)"
			 (setq str (m68k-get-dest-str)))
	   (substring str (match-beginning 1) (match-end 1)))
	  (t
	   ""))))
(defvar m68k-last-label-numb 0
  "The number in list of the last label shown by m68k-complete-label.")
(defvar m68k-last-label-shown nil
  "The str of the last label shown by m68k-complete-label.")
(defvar m68k-last-completions nil
  "The list of all completions done by m68k-complete-label.")
	
(defun m68k-complete-label ()
  "Complete label at current point."
  (interactive)
  (let* ((b (save-excursion (skip-chars-backward m68k-word) (point)))
	 (e (save-excursion (skip-chars-forward m68k-word) (point)))
	 (str (buffer-substring b e))
	 ;; The following variable is used in m68k-label-completion
	 (buffer-to-use (current-buffer))
	 (allcomp (if (and m68k-toggle-completions
			   (string= m68k-last-label-shown str))
		      m68k-last-completions
		    (all-completions str 'm68k-label-completion)))
	 (match (if m68k-toggle-completions
		    "" (try-completion str 'm68k-label-completion))))
    ;; Delete old string
    (delete-region b e)
    ;; Toggle-completions inserts whole labels
    (if m68k-toggle-completions
	(progn
	  ;; Update entry number in list
	  (setq m68k-last-completions allcomp
		m68k-last-label-numb 
		(if (>= m68k-last-label-numb (1- (length allcomp)))
		    0
		  (1+ m68k-last-label-numb)))
	  (setq m68k-last-label-shown (elt allcomp m68k-last-label-numb))
	  ;; Display next match or same string if no match was found
	  (if (not (null allcomp))
	      (insert "" m68k-last-label-shown)
	    (insert "" str)
	    (message "(No match)")))
      ;; The other form of completion does not necessarly do that.
      ;; Insert match if found, or the original string if no match
      (if (or (null match) (equal match 't))
	  (progn (insert "" str)
		 (message "(No match)"))
	(insert "" match))
      ;; Give message about current status of completion
      (cond ((equal match 't)
	     (if (not (null (cdr allcomp)))
		 (message "(Complete but not unique)")
	       (message "(Sole completion)")))
	    ;; Display buffer if the current completion didn't help 
	    ;; on completing the label.
	    ((and (not (null (cdr allcomp))) (= (length str) (length match)))
	     (with-output-to-temp-buffer "*Completions*"
	       (display-completion-list allcomp))
	     ;; Wait for a keypress. Then delete *Completion*  window
	     (momentary-string-display "" (point))
	     (delete-window (get-buffer-window (get-buffer "*Completions*")))
	     )))
    ;; Just in case we used narrowing
    (widen)
    ))
(defun m68k-show-completions ()
  "Show all possible label completions at current point."
  (interactive)
  (let* ((str (buffer-substring
	       (save-excursion (skip-chars-backward m68k-word) (point))
	       (save-excursion (skip-chars-forward m68k-word) (point))))
	 ;; The following variable is used in m68k-label-completion
	 (buffer-to-use (current-buffer))
	 (allcomp (if (and m68k-toggle-completions
			   (string= m68k-last-label-shown str))
		      m68k-last-completions
		    (all-completions str 'm68k-label-completion))))
    ;; Show possible completions in a temporary buffer.
    (with-output-to-temp-buffer "*Completions*"
      (display-completion-list allcomp))
    ;; Wait for a keypress. Then delete *Completion*  window
    (momentary-string-display "" (point))
    (delete-window (get-buffer-window (get-buffer "*Completions*")))))
(defun m68k-goto-label ()
  "Move to label prompted for in mininuffer, with option to use a default label
>from the source or destination of the current instruction if it exist."
  (interactive)
  (let* ((default (m68k-get-default-label))
	 ;; The following variable is used in m68k-label-completion
	 (buffer-to-use (current-buffer))
	 (default (progn (if (m68k-label-completion default nil 'lambda)
			     default 
			   ;; Widen region again if there were no default
			   (progn (widen) ""))))
	 (label (if (not (string= default ""))
		    ;; Do completion with default
		    (completing-read (concat "Label: (default " default ") ")
				     'm68k-label-completion nil t "")
		  ;; There is no default value. Complete without it
		  (completing-read "Label: "
				   'm68k-label-completion nil t ""))))
    ;; If there was no respons on prompt, use default value
    (if (string= label "")
	(setq label default))
    ;; Narrow if we are using local labels
    (if (string-match "\\<[0-9.]" label)
	(m68k-narrow-to-function))
    ;; Goto right place in buffer if label is not an empty string
    (or (string= label "")
	(progn
	  (setq label (m68k-build-label-regexp label))
	  (goto-char (point-min))
	  (re-search-forward (concat "^" label ":?\\>"))
	  (beginning-of-line)))
    ;; Just in case we used narrowing
    (widen)
    ))
(defun m68k-view-label ()
  "Prompt for a label in mininuffer, with option to use a default label from
the source or destination of the current instruction if it exist. Print the
line which the label points at in the minibuffer."
  (interactive)
  (let* ((default (m68k-get-default-label))
	 ;; The following variable is used in m68k-label-completion
	 (buffer-to-use (current-buffer))
	 (default (progn (if (m68k-label-completion default nil 'lambda)
			     default 
			   ;; Widen region again if there were no default
			   (progn (widen) ""))))
	 (label (if (not (string= default ""))
		    ;; Do completion with default with default
		    (completing-read (concat "Label: (default " default ") ")
				     'm68k-label-completion nil t "")
		  ;; There is no default value. Complete without it
		  (completing-read "Label: "
				   'm68k-label-completion nil t ""))))
    ;; If there was no respons on prompt, use default value
    (if (string= label "")
	(setq label default))
    ;; Narrow if we are using local labels
    (if (string-match "\\<[0-9.]" label)
	(m68k-narrow-to-function))
    ;; Get line with label if label is not an empty string
    (or (string= label "")
	(save-excursion
	  (setq label (m68k-build-label-regexp label))
	  (goto-char (point-min))
	  (message "%s" (buffer-substring
			 (progn
			   (re-search-forward (concat "^" label ":?\\>"))
			   (beginning-of-line)
			   (point))
			 (progn
			   (end-of-line)
			   (point))))))
    ;; Just in case we used narrowing
    (widen)
    ))
;;;
;;; Functions operating on source and destination of instruction
;;;
(defun m68k-immediate-size ()
  "Return the size of the immediate number from source of current instruction, 
or 'Unknown if it is not a readable immediate number."
  (save-excursion
    ;; Go to right place on line
    (beginning-of-line)
    (if (looking-at "\\w")
	(skip-chars-forward m68k-word))
    (skip-chars-forward " \t")
    (skip-chars-forward m68k-word)
    (skip-chars-forward " \t")
    ;; Are we able to read this number
    (if (not (looking-at "#$[0-9a-fA-F]+\\|\\#[0-9]+"))
	'Unknown
      (string-to-int (buffer-substring
		      (progn (skip-chars-forward "#$") (point))
		      (progn (skip-chars-forward "0-9a-fA-F") (point)))))))
(defun m68k-get-number-of-regs (str)
  "Return number of registers in string."
  (let ((tot 0)	span reg old)
    (while (string-match "[-0-7]" str)
      (cond ((= (aref str (match-beginning 0)) ?-)
	     ;; Ooops.. This was part of a span
	     (setq span t))
	    (t
	     (setq old reg)
	     ;; Get register number
	     (setq reg (- (aref str (match-beginning 0)) ?0))
	     ;; If this is a span, calculate number of registers in it
	     (if span (progn (setq tot (+ tot (- reg old))
				   span nil))
	       ;; If not, just add 1 to total
	       (setq tot (1+ tot)))))
      ;; Narrow string
      (setq str (substring str (match-end 0))))
    tot))
(defun m68k-get-number (str)
  "Return number in STR (takes both hex and decimal)."
  (if (= (aref str 0) ?#)
      (setq str (substring str 1 (length str))))
  (let ((base (if (/= (aref str 0) ?$)
		  10
		(setq str (substring str 1 (length str)))
		16))
	(num 0) ch)
    ;; Go through all numbers
    (while (and (stringp str)
		(string-match "^[0-9a-fA-F]" str))
      ;; Get current number
      (setq ch (aref str 0)
	    str (substring str 1 (length str))
	    num (+ (* base num)
		   (cond ((and (>= ch ?0) (<= ch ?9)) (- ch ?0))
			 ((and (>= ch ?a) (<= ch ?f)) (+ (- ch ?a) 10))
			 ((and (>= ch ?A) (<= ch ?F)) (+ (- ch ?A) 10))))))
    ;; Return the  number
    num))
;;;
;;; Functions concerning type of instruction word, source or destination
;;;
(defun s= (amode)
  "Return t if source is amode, nil otherwise."
  (eq (m68k-get-src) amode))
(defun d= (amode)
  "Return t if destination is amode, nil otherwise."
  (eq (m68k-get-dest) amode))
;;; Extract posistions
(defun m68k-instr-pos ()
  "Return current instruction position."
  (beginning-of-line)
  ;; Skip label at beginning of line
  (skip-chars-forward (concat m68k-word ":."))
  (skip-chars-forward " \t")
  ;; This may be a label not starting at the beginning of the line if
  ;; it is ending with a  :
  (if (looking-at "\\<[0-9a-zA-Z_.$]+:")
      (progn
	(skip-chars-forward (concat m68k-word ":$"))
	(skip-chars-forward " \t")))
  (point))
(defun m68k-src-pos ()
  "Return source position of current instruction."
  (save-excursion
    (goto-char (m68k-instr-pos))
    (skip-chars-forward m68k-word)
    (skip-chars-forward " \t")
    (point)))
(defun m68k-dest-pos ()
  "Return destination position of current instruction."
  (save-excursion
    (goto-char (m68k-src-pos))
    (skip-chars-forward "^,([\\*;\n")
    (cond ((= (following-char) ?\()
	   (skip-chars-forward "^)"))
	  ((= (following-char) ?\[)
	   (skip-chars-forward "^]")))
    (skip-chars-forward "^,[\\*;\n")
    (if (= (following-char) ?\[)
	(skip-chars-forward "^]"))
    (skip-chars-forward "+), \t")
    (point)))
;;; Extract strings
(defun m68k-get-inst-str ()
  "Return instruction string of current instruction."
  (save-excursion
    (m68k-instr-pos)
    ;; Return instruction string
    (buffer-substring (point) (progn (skip-chars-forward m68k-word)
				     (point)))))
(defun m68k-get-src-str ()
  "Return source field of instruction as a string."
  (save-excursion
    (goto-char (m68k-src-pos))
    (if (or (eolp) (looking-at "[;*]"))
	""
      (buffer-substring (point) (progn (skip-chars-forward "^,(*;\n")
				       (if (= (following-char) ?\()
					   (skip-chars-forward "^)"))
				       (skip-chars-forward "+)")
				       (skip-chars-backward " \t")
				       (point))))))
(defun m68k-get-dest-str ()
  "Return destination field of instruction as a string."
  (save-excursion
    (goto-char (m68k-dest-pos))
    (if (or (eolp) (looking-at "[;*]"))
	""
      (buffer-substring (point) (progn (skip-chars-forward "^ \t\n;*")
				       (point))))))
;;; Extract types
(defun m68k-get-inst ()
  "Return instruction structure of current instruction."
  (let ((instring (m68k-get-inst-str))
	(case-fold-search t)
	(i 0) (inst 'Unknown))
    ;; Check if this is a commented or blank line
    (if (save-excursion
	  (beginning-of-line)
	  (skip-chars-forward " \t")
	  (or (looking-at "[;*]") (eolp) (string= instring "")))
	'None
      ;; Find instruction in cycle table
      (while (and (eq inst 'Unknown) (nth i m68k-cycles))
	(if (string-match (car (nth i m68k-cycles)) instring)
	    (setq inst (nth i m68k-cycles)))
	(setq i (1+ i)))
      ;; Return instruction structure
      inst)))
(defun m68k-get-src ()
  "Return symbol of source."
  (save-excursion
    (goto-char (m68k-src-pos))
    ;; Maybe the instruction need no parameters
    (if (or (looking-at "[;*]") (eolp) (bolp))
	'None
      ;; Find addressing mode for source
      (let ((i 0) (src 'Unknown))
	(while (and (eq src 'Unknown) (nth i m68k-addr-modes))
	  (if (looking-at (symbol-value (nth i m68k-addr-modes)))
	      (setq src (nth i m68k-addr-modes)))
	  (setq i (1+ i)))
	;; Return symbol of addressing mode
	src))))
(defun m68k-get-dest ()
  "Return symbol of destination."
  (save-excursion
    (goto-char (m68k-dest-pos))
    ;; Maybe source was the destination
    (if (or (looking-at "[;*]") (eolp) (bolp))
	'None
      ;; Find addressing mode for dest
      (let ((i 0) (dest 'Unknown))
	(while (and (eq dest 'Unknown) (nth i m68k-addr-modes))
	  (if (looking-at (symbol-value (nth i m68k-addr-modes)))
	      (setq dest (nth i m68k-addr-modes)))
	  (setq i (1+ i)))
	;; Return symbol of addressing mode
	dest))))
;;;
;;; Functions for calculating cycles
;;;
(defun m68k-show-cycles ()
  "Show instruction timing of current instruction."
  (interactive)
  (let ((src (m68k-get-src)) (exe 0)
	(dest (m68k-get-dest))
	(inst (m68k-get-inst)))
    (cond ((eq inst 'Unknown)
	   (message "Unknown instruction"))
	  ((eq inst 'None) 0)
	  (t
	   (setq exe (eval (car (cdr inst))))
	   (if (interactive-p)
	       ;; Only print message when called interactivly
	       (cond ((equal src 'None)
		      (message "%s  - %d cycles" (m68k-get-inst-str) exe))
		     ((equal dest 'None)
		      (message "%s %s  - %d cycles" (m68k-get-inst-str)
			       (cdr (assoc src m68k-addr-mode-str)) exe))
		     (t
		      (message "%s %s,%s  - %d cycles" (m68k-get-inst-str)
			       (cdr (assoc src m68k-addr-mode-str))
			       (cdr (assoc dest m68k-addr-mode-str)) exe))))))
    exe))
(defun m68k-show-cycles-region (b e &optional n)
  "Show total number of cycles in region. Prefix arg n is number of times to
go trough region."
  (interactive "r\np")
  (save-excursion
    (let ((tot 0)
	  (m68k-never-branch t))
      (goto-char b)
      (while (< (point) e)
	(setq tot (+ tot (m68k-show-cycles)))
	(next-line 1))
      (message "Total: %dcycles" (* n tot)))))
(defvar m68k-never-branch nil
  "Non-nil means branching on condition should never be done when calculating
instruction timings. When calculating total number of cycles in a region, this
varible is automatically set to t while calculating.")
;;;
;;; Byte counting
;;;
(defun m68k-byte-count (b e &optional check-short-branch)
  "Find size of region in bytes. If optional third arg is used, stop counting
as soon as the count passes 128 bytes (max size for short branches)."
  (interactive "r")
  (let (instr src dest (size 0))
    (save-excursion
      (goto-char b)
      (while (and (< (point) e)
		  (or (not check-short-branch)
		      (< size 128)))
	(setq instr (m68k-get-inst-str)
	      src (m68k-get-src)
	      dest (m68k-get-dest)
	      size (+ size
		      (catch 'found
			(let ((size-list m68k-inst-sizes))
			  (while (car size-list)
			    (if (string-match (eval (car (car size-list)))
					      instr)
				(throw 'found (eval (cdr (car size-list))))
			      (setq size-list (cdr size-list))))
			  0))))
	(forward-line 1)))
    (if (interactive-p)
	(message "Size: %d bytes" size))
    size))
(defun m68k-label-distance (&optional label)
  "Get distance from label prompted for in minibuffer in bytes."
  (interactive)
  (let* ((startpos (if (bolp) (point)
		     (save-excursion (forward-line 1) (point))))
	 (just-check (stringp label))
	 (default (m68k-get-default-label) )
	 ;; The following variable is used in m68k-label-completion
	 (buffer-to-use (current-buffer))
	 (default (cond (label "")
			((m68k-label-completion default	nil 'lambda) default)
			(t "")))
	 (label (cond (label label)
		      ((not (string= default ""))
		       (completing-read (concat "Label: (default "
						default ") ")
					'm68k-label-completion nil t ""))
		      (t (completing-read "Label: "
					  'm68k-label-completion nil t ""))))
	 dist dir)
    ;; If there was no respons on prompt, use default value
    (if (string= label "")
	(setq label default))
    ;; Narrow if we are using local labels
    (if (string-match "\\<[0-9.]" label)
	(m68k-narrow-to-function))
    ;; Get length to label if label is not an empty string
    (or (string= label "")
	(save-excursion
	  (setq label (m68k-build-label-regexp label))
	  (goto-char (point-min))
	  (if (not (re-search-forward (concat "^" label ":?\\>") nil t))
	      (setq dist 130)
	    (beginning-of-line)
	    (setq dir (if (<= startpos (point))
			  "forward" "backward")
		  dist (m68k-byte-count (min startpos (point))
					(max startpos (point)) just-check))
	    (if (interactive-p)
		(message "Distance: %d bytes %s" dist dir)))))
    (widen) ; Just in case we used narrowing
    dist))  ; Return distance
(defconst m68k-inst-sizes 
  ;; When calculating the instruction sizes, the variables `src' (addressing
  ;; mode for source as a symbol), `dest' (addr. mode for destination) and
  ;; `instr' (instruction mnemonic as a string) may be used.
  '(;; Some instructions have a constant length - 2
    ((concat "[as]bcd\\|add[qx]\\|bkpt\\|cmpm\\|ex[gt]\\|illegal\\nop"
	     "reset\\|rt[ers]\\|swap\\|trapv?\\|unlk") . 2)
    ;; Others have a base length of 4 (and not 2)
    ("move[ms]" . (+ 4 (eval (cdr (assq src m68k-ea-extensions)))
		     (eval (cdr (assq dest m68k-ea-extensions)))))
    ;; Shift instructions have a special case when used on memory
    ("\\(as\\|ls\\|rox?\\)[lr]" .
     (if (eq dest 'None) 
	 (+ 2 (eval (cdr (assq src m68k-ea-extensions)))) 2))
    ;; Most instructions however have a base length of 2
    ((concat "\\(a[nd]d\\|e?ori?\\|cmp\\|sub\\)i?\\|adda\\|bchg\\|b?clr\\|"
	     "bset\\|b?tst\\|chk\\|cmpa\\|\\(div\\|mul\\)[su]\\|jmp\\|jsr\\|"
	     "[lp]ea\\|move[ac]?\\|nbcd\\|negx?\\|not\\|\\<s..\\|suba\\|tas"
	     ;; The following instrucions are '020+
	     "\\|callm") .
     (+ 2 (eval (cdr (assq src m68k-ea-extensions)))
	(eval (cdr (assq dest m68k-ea-extensions)))))
    ;; Size of block declaration is read from buffer
    ("\\<\\(dcb\\|ds\\|blk\\)" .
     (cond ((string-match "\\.[wW]" instr)
	    (* 2 (m68k-get-number (m68k-get-src-str))))
	   ((string-match "\\.[lL]" instr)
	    (* 4 (m68k-get-number (m68k-get-src-str))))
	   (t (m68k-get-number (m68k-get-src-str)))))
    ;; When checking inline data, look for strings or `,'
    ("\\<dc" .
     (let ((sz 0))
       (goto-char (m68k-src-pos))
       (if (not (looking-at "[\"\']"))
	   (setq sz 1))
       (while (re-search-forward "\\(\"[^\"]+\"\\|\'[^\']+\'\\)\\|," 
				 (save-excursion (end-of-line) (point)) t)
	 (setq sz (+ sz (if (match-end 1)
			    (- (match-end 1) (match-beginning 1) 2)
			  (if (looking-at "[ \t]*$\\|[\"\']")
			      0 1)))))
       (cond ((string-match "\\.[wW]" instr) (* sz 2))
	     ((string-match "\\.[lL]" instr) (* sz 4))
	     (t sz))))
    ;; Some instructions have a constant length - 4
    ("\\<db..\\|link\\|movep\\|rtd" . 4)
    ;; Branch instructions are the main problem in calculating instruction
    ;; lengths. They can be 2 or 4 bytes depending on the offset to the
    ;; branch location. To make it easy, we will not enter a recursive 
    ;; calculation that we may not be able to escape from, but just calculate
    ;; the exact length on our first run through the code.
    ("\\<b.." . 
     ;; Check-short-branch is true if program has entered recursion
     (if (or m68k-byte-count-worst-case
	     check-short-branch
	     (> (m68k-label-distance (m68k-get-src-str)) 128))
	 4 2))
    )
  "Table used for calculating instruction length.")
(defconst m68k-ea-extensions
  '((dn . 0) (an . 0) ({an} . 0) ({an}+ . 0)
    (-{an} . 0) (d16{an} . 2) (d8{an,xn} . 2)
    ({xxx}w . 2) ({xxx}l . 4) (d8{PC} . 0)
    (d16{PC,xn} . 2) (<data> . (if (string-match "\\.[lL]" instr) 4 2))
    (register . 0) (Unknown . 0) (None . 0))
  "Size of extension words for effective address calculation.")
(defvar m68k-byte-count-worst-case nil
  "Non-nil means use worst case when calculating branch-instruction-sizes. This
saves a LOT of time, but makes all brances 4 bytes instead of 4 OR 2 bytes.")
;;;
;;; Adressing modes
;;;
(defconst dn        "[dD][0-7]")
(defconst an        "[aA][0-7]")
(defconst register  "\\<\\(SR\\|CCR\\)\\>")
(defconst {an}      "([aA][0-7])\\|(.?SP)")
(defconst {an}+     "(\\([aA][0-7]\\|.?SP\\))\\+")
(defconst -{an}     "-(\\([aA][0-7]\\|.?SP\\))")
(defconst d16{an}   "-?\\$?\\w+([aA][0-7])")
(defconst d8{an,xn} "-?\\$?\\w*([aA][0-7],[aAdD][0-7]\\(\\.[wWlL]\\)?)")
(defconst {xxx}w    "\\$?\\w*\\.[wW]")
(defconst {xxx}l    "\\$?\\w*\\(\\.[lL]\\)?")
(defconst d16{PC}   "-?\\$?\\w*(PC)")
(defconst d8{PC,xn} "-?\\$?\\w*(PC,[aAdD][0-7]\\(\\.[wWlL]\\)?)")
(defconst <data>    "#")
;; Addressing modes for '020+
(defconst {d8,an,xn} (concat "(-?\\$?[0-9a-fA-F][0-9a-fA-f]?[0-9]?,"
			     "[aA][0-7],[aAdD][0-7]\\(.[wWlL]\\)?"
			     "\\(\\*[1248]\\)?)"))
(defconst {bd,an,xn} (concat "(\\(-?\\$?\\w+(\\.[wWlL]\\)?,\\)?"
			     "\\([aA][0-7],\\)?\\([aAdD][0-7]\\(.[wWlL]\\)?"
			     "\\(\\*[1248]\\)?\\)?)"))
(defconst {d8,PC,xn} (concat "(-?\\$?[0-9a-fA-F][0-9a-fA-f]?[0-9]?,"
			     "PC\\(,[aAdD][0-7]\\(.[wWlL]\\)?"
			     "\\(\\*[1248]\\)?\\)?)"))
(defconst {bd,PC,xn} (concat "(\\(-?\\$?\\w+(\\.[wWlL]\\)?,\\)?"
			     "Z?PC\\(,[aAdD][0-7]\\(.[wWlL]\\)?"
			     "\\(\\*[1248]\\)?\\)?)"))
(defconst {{bd,an,xn},od} (concat "(\\[\\(-?\\$?\\w+(\\.[wWlL]\\)?,?\\)?"
				  "\\([aA][0-7],?\\)?"
				  "\\([aAdD][0-7]\\(.[wWlL]\\)?"
				  "\\(\\*[1248]\\)?\\)?\\]"
				  "\\(,-?\\$?\\w+\\)?)"))
(defconst {{bd,an},xn,od} (concat "(\\[\\(-?\\$?\\w+(\\.[wWlL]\\)?,?\\)?"
				  "\\([aA][0-7]\\)?\\]"
				  "\\(,[aAdD][0-7]\\(.[wWlL]\\)?"
				  "\\(\\*[1248]\\)?\\)?\\(,-?\\$?\\w+\\)?)"))
(defconst m68k-addr-mode-str
  '((dn . "dn")
    (an . "an")
    ({an} . "(an)")
    ({an}+ . "(an)+")
    (-{an} . "-(an)")
    (d16{an} . "d16(an)")
    (d8{an,xn} . "d8(an,xn)")
    ({xxx}w . "(xxx).w")
    ({xxx}l . "(xxx).l")
    (d16{PC} . "d16(PC)")
    (d8{PC,an} . "d8(PC,xn)")
    (<data> . "#<data>")
    (register . "reg")
    ;; '020+ addressing modes
    ({d8,an,xn} . "(d8,an,xn)")
    ({bd,an,xn} . "(bd,an,xn)")
    ({d8,PC,xn} . "(d8,PC,xn)")
    ({bd,PC,xn} . "(bd,PC,xn)")
    ({{bd,an,xn},od} . "([bd,an,xn],od)")
    ({{bd,an},xn,od} . "([bd,an],xn,od)"))
  "Alist of strings to use for addressing modes.")
(defconst m68k-addr-modes
  '(dn an {an}+ {an} -{an} d16{an} d8{an,xn}
       {xxx}w d16{PC} d8{PC,xn} <data> register {xxx}l)
  "List of addressing modes.")
;;;
;;; Cycle calculation for the M68000
;;;
(defun ea (amode)
  "Return effective address calculation time of addressing mode."
  (if (string-match "\\.[lL]" (m68k-get-inst-str))
      (car (cdr (cdr (assoc amode m68k-68000-eacalc))))
    (car (cdr (assoc amode m68k-68000-eacalc)))))
(defconst  m68k-68000-eacalc
  '((dn . (0 0)) (an . (0 0)) ({an} . (4 8)) ({an}+ . (4 8))
    (-{an} . (6 10)) (d16{an} . (8 12)) (d8{an,xn} . (10 14))
    ({xxx}w . (8 12)) ({xxx}l . (12 16)) (d8{PC} . (8 12))
    (d16{PC,xn} . (10 14)) (<data> . (4 8)) (register . (0 0))
    (None . (0 0)) (Unknown . (0 0)))
  "Alist of effective address calculation times for the m68000.
Elements in the list are of the form (<addr-mode> . (<bw time> <l time>))")
(defconst m68k-68000-cycles
  '(("movea?\\(\\.[bwl]\\)?\\>"
     (cond ((or (string= "USP" (m68k-get-src-str))
		(string= "USP" (m68k-get-dest-str))) 4)
	   ((string-match "CCR\\|SR" (m68k-get-dest-str)) (+ 12 (ea src)))
	   ((string= "SR" (m68k-get-src-str))
	    (if (or (d= 'dn) (d= 'an)) 6 (+ 8 (ea dest))))
	   ((d= '-{an}) (+ 4 (ea src) (ea '{an})))
	   (t (+ 4 (ea src) (ea dest))))) 
    ("\\(adda\\|\\(add\\|sub\\|and\\|e?or\\)i?\\)\\.l"
     (cond ((or (d= 'an) (d= 'dn))
	    (if (or (s= 'dn) (s= 'an) (s= '<data>))
		(+ 8 (ea src)) (+ 6 (ea src))))
	   (t (+ 12 (ea src) (ea dest)))))
    ("\\(adda\\|\\(add\\|sub\\|and\\|e?or\\)i?\\)\\(\\.[bw]\\)?"
     (cond ((string-match "CCR\\|SR" (m68k-get-dest-str)) 20)	    
	   ((d= 'an) (+ 8 (ea src)))
	   ((d= 'dn) (+ 4 (ea src)))
	   (t (+ 8 (ea src) (ea dest)))))
    ("cmp[ai]?\\.l"
     (cond ((and (s= '<data>) (not (d= 'dn))) (+ 12 (ea dest)))
	   (t (+ 6 (ea src) (ea dest)))))
    ("cmp[ai]?\\(\\.[bw]\\)?"
     (cond ((d= 'an) (+ 6 (ea src)))
	   (t (+ 4 (ea src) (ea dest)))))
    ("divs" (+ 158 (ea src)))
    ("divu" (+ 140 (ea src)))
    ("mul[su]" (+ 70 (ea src)))
    ("\\(add\\|sub\\)q\\.l"
     (cond ((or (d= 'dn) (d= 'an)) 8)
	   (t (+ 12 (ea dest)))))
    ("\\(add\\|sub\\)q\\(\\.[bw]\\)?"
     (cond ((d= 'dn) 4)
	   (t (+ 8 (ea dest)))))
    ("moveq\\(\\.l\\)?" 4)
    ("\\(as\\|ls\\|rox?\\)[lr]\\.l"
     (if (equal (m68k-immediate-size) 'Unknown) 24
       (+ 8 (* 2 (m68k-immediate-size)))))
    ("\\(as\\|ls\\|rox?\\)[lr]\\(\\.[bw]\\)?"
     (cond ((d= 'Unknown) (+ 8 (ea src)))
	   (t (if (equal (m68k-immediate-size) 'Unknown) 22
		(+ 6 (* 2 (m68k-immediate-size)))))))
    ("\\<\\(clr\\|negx?\\|not\\)\\.l"
     (cond ((or (s= 'dn) (s= 'an)) 6)
	   (t (+ 12 (ea src)))))
    ("\\<\\(clr\\|negx?\\|not\\)\\(\\.[bw]\\)?"
     (cond ((or (s= 'dn) (s= 'an)) 4)
	   (t (+ 8 (ea src)))))
    ("\\<tst\\(\\.[bwl]\\)?"
     (+ 4 (ea src)))
    ("tas\\(\\.b\\)?"
     (cond ((or (s= 'dn) (s= 'an)) 4)
	   (t (+ 10 (ea src)))))
    ("nbcd\\(\\.b\\)?"
     (cond ((or (s= 'dn) (s= 'an)) 6)
	   (t (+ 8 (ea src)))))
    ("\\<s..\\>"
     (cond ((or (s= 'dn) (s= 'an)) 6)
	   (t (+ 8 (ea src)))))
    ("\\<db..?\\>"
     (if m68k-never-branch 14 10))
    ("bsr" 18)
    ("bra" 10)
    ("bset\\|bchg" (+ 8 (ea dest) (ea src)))
    ("bclr"
     (if (or (d= 'dn) (d= 'an))
	 (+ 10 (ea src))
       (+ 8 (ea dest) (ea src))))
    ("btst"
     (if (or (d= 'dn) (d= 'an))
	 (+ 6 (ea src))
       (+ 4 (ea src) (ea dest))))
    ("\\<b..\\.[lw]"
     (if m68k-never-branch 12 10))
    ("\\<b.."
     (if m68k-never-branch 8 10))
    ("addx\\.l\\|subx\\.l"
     (if (s= 'dn) 8 30))
    ("\\(add\\|sub\\)x\\(\\.[bw]\\)?"
     (if (s= 'dn) 4 18))
    ("[as]bcd" (if (s= 'dn) 6 18))
    ("cmpm\\.l" 20)
    ("cmpm\\(\\.[bw]\\)?" 12)
    ("jmp"
     (cond ((s= '{an}) 8)
	   ((or (s= 'd16{an}) (s= '{xxx}w) (s= 'd8{PC})) 10)
	   ((s= '{xxx}l) 12)
	   (t 14)))
    ("jsr"
     (cond ((s= '{an}) 16)
	   ((or (s= 'd16{an}) (s= '{xxx}w) (s= 'd8{PC})) 18)
	   ((s= '{xxx}l) 20)
	   (t 22)))
    ("lea"
     (cond ((s= '{an}) 4)
	   ((or (s= 'd16{an}) (s= '{xxx}w) (s= 'd8{PC})) 8)
	   (t 12)))
    ("pea"
     (cond ((s= '{an}) 12)
	   ((or (s= 'd16{an}) (s= '{xxx}w) (s= 'd8{PC})) 16)
	   (t 20)))
    ("ext\\|nop\\|swap\\|trapv\\|stop" 4)
    ("exg" 6)
    ("rts" 16)
    ("rt[er]" 20)
    ("link" 16)
    ("unlk" 12)
    ("reset" 132)
    ("movem"
     (if (or (s= 'dn) (s= 'an))
	 ;; Moving R->M
	 (+ (cond ((or (d= '{an}) (d= '-{an})) 8)
		  ((or (d= 'd16{an}) (d= '{xxx}w)) 12)
		  ((d= '{xxx}l) 16)
		  (t 14))
	    (if (string-match "\\.l" (m68k-get-inst-str))
		(* 8 (m68k-get-number-of-regs (m68k-get-src-str)))
	      (* 4 (m68k-get-number-of-regs (m68k-get-src-str)))))
       ;; Moving M->R
       (+ (cond ((or (s= '{an}) (s= '{an}+)) 12)
		((or (s= 'd16{an}) (s= '{xxx}w) (s= 'd16{PC})) 16)
		((s= '{xxx}l) 20)
		(t 18))
	  (if (string-match "\\.l" (m68k-get-inst-str))
	      (* 8 (m68k-get-number-of-regs (m68k-get-dest-str)))
	    (* 4 (m68k-get-number-of-regs (m68k-get-dest-str)))))))
    ("chk" (+ 10 (ea src)))
    ("movep\\.l" 24)
    ("movep\\(\\.w\\)?" 16)
    )
  "Cycle tables for the M68000 processor. These tables always use the worst
cases in instructions including condition codes. It also uses the worst
case when there is a bitshift and the number of bits to shift is unknown.")
(defconst m68k-cycles m68k-68000-cycles)
;;; m68k-mode.el ends here

