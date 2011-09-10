;; TeX mode commands.
;; Copyright (C) 1985, 1986 Free Software Foundation, Inc.
;;

; Some suggestions for your .emacs file:
; ======================================
; ;; Arrange for tex mode to autoload from cmutex.el, instead of the standard
; ;; package, tex-mode.el.
; (autoload 'tex-mode "cmutex" "Major mode for editing files of TeX or LaTeX."
; 	    t)
;
; ;; Tell ispell to run latex source through delatex before spell checking:
; (setq latex-mode-hook '((lambda () (setq ispell-filter-hook "delatex")
;                                    (setq ispell-filter-hook-args nil))))
;
; ;; Arrange for tex-view-file to use dvi2tty when I'm displaying on a
; ;; terminal.
; (if (not (eq window-system 'x))
;     (setq tex-view-dvi-file-command "dvi2tty %s | cat -s"))
;
; ;; If cmutex.el or comint.el reside in a non-standard directory, you
; ;; must tell emacs where to look for them. This may not be necessary.
; (setq load-path (cons (expand-file-name "/afs/user/shivers/lib/emacs")
; 		      load-path))

;; See notes below to if you intend to use the tex-region command.
;;
;; CHANGE LOG:
;; ============================================================================
;; Rewritten following contributions by William F. Schelter
;; and Dick King (king@kestrel).
;; Modified August 1986 by Stephen Gildea <mit-erl!gildea> and
;; Michael Prange <mit-erl!prange> to add LaTeX support and enhance
;; TeX-region.
;; Added TeX-directory and reorganized somewhat  gildea 21 Nov 86
;;
;; Process stuff rewritten substantially. TeX processes are now run at
;; toplevel in a comint buffer. They are killed with the KILL signal,
;; so they die without core dumping. Comint mode provides better
;; process interaction than old shell mode. 
;; - suggest you arrange for TeX-directory to be "." so that
;;   \include and \input's work. See notes below.
;; - fixed TeX-region so it doesn't include the line before the header.
;;   I did not fix it so that if you TeX-region the entire buffer,
;;   the trailer won't get included twice (once from the buffer, and once
;;   courtesy of TeX-region). TeX works in the presence of this bug, so
;;   what the hell.
;; - changed TeX-buffer so it directly TeX's the file when appropriate.
;;   Also added TeX-last-processed-file so that TeX-print knows who to print.
;;   This is pretty winning.
;; - TeX-dvi-print-command is now a format string, with a %s where the filename
;;   goes. Now you can have print commands like: "dvi-ps %s | lpr".
;; Olin Shivers (shivers@cs.cmu.edu) 2/89
;;
;; Modified by S. Anderson <anderson@sapir.cog.jhu.edu> to add 
;; extended LaTeX support (including contributions from Ashwin Ram 
;; and others), BibTeX and dvi-file previewer invocations
;; 21 Nov. 1989
;;
;; Anderson, Patel and Ram's stuff merged in to master source at CMU. Small
;; things cleaned up. Shivers 2/90.
;; New features:
;; - LaTeX mode is default, not TeX mode.
;; - Stunning array of commands to insert and close LaTeX commands.
;;   So many commands, the bindings are now politically incorrect.
;; - Commands to invoke dvi previewer and bibtex.
;; - Separate TeX and LaTeX keymaps.
;; - Smarter indent-for-comment command.
;; - TeX-print with a numerical arg allows you to edit the print command.
;;   Handy for specifying a non-default printer, page count, etc.
;; - Smarter validate-region and close-block commands.
;; - Added help menu on ? to latex-insert-begin. Shuffled keybindings
;;   on latex keymap and env codes on latex-insert-begin. See code
;;   for rationale.
;; - Munged the validation commands, but they still need work.
;;   Also swapped around pieces of the names to make for consistent
;;   naming scheme: validate-TeX/latex-region/buffer/paragraph.
;; - Added stuff to allow customisation of the print command.
;; This package is now so featureful it makes my head hurt.
;;
;; Gildea worked it over again 3/90
;; - Cleaned up the doc strings
;; - Deleted TeX-indent-for-comment and changed comment-start-skip.
;;   In 18.51 indent-for-comment changed so TeX mode could use it directly.
;; - Changed paragraph-start to make lines starting with '%' separate
;;   paragraphs.  (This is from 18.54 tex-mode.el.)
;; - Tweaked quote handling.  The characters that " inserts are now variables
;;   so they can be changed for editing other languages. "
;; - Fixed an error in TeX-define-common-keys.
;;
;; Shivers worked it over again 5/90
;; - Lower-cased occurences of TeX and LaTeX in command and variable names.
;;   This was a problem.
;; - Changed tex-directory default to "./" since that's what everyone uses.
;;   This makes tex-region work when the source contains \include and \input's.
;; - zap file name is just "zap", not "#zap".


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

;; Still to do:
;;  Validation needs work!
;;
;;  Make TAB indent correctly for TeX code.  Then we can make linefeed
;;  do something more useful.
;;
;;  Have spell understand TeX instead of assuming the entire world
;;  uses nroff.
;;
;;  The code for finding matching $ needs to be fixed.


(provide 'tex-mode)

;;; HOOKS AND SWITCHES
;;; ===========================================================================
;;; Here's all the variables the user can set to customise the mode.

(defvar tex-directory "./"
  "*Directory in which to run TeX subjob.  Temporary files are created here.")

(defvar tex-dvi-print-command "dvipr %s"
  "*Shell command used by \\[tex-print] to print a .dvi file.")

(defvar tex-dvi-print-command-suffix ""
  "*Additional arguments or command name modification for command string to print a .dvi file.")

(defvar tex-last-processed-file nil
  "The last file that was TeX'ed by \\[tex-region] or \\[tex-buffer].
Used by \\[tex-print].")

(defvar tex-show-queue-command "lpq"
  "*Command string used by \\[tex-show-print-queue] to show the print queue.
Should show the queue that \\[tex-print] put your job on.")

(defvar tex-bibtex-command "bibtex %s"
  "*Command string used by \\[bibtex-buffer] to run BiBtex on a LaTeX'ed file.")

(defvar tex-view-dvi-file-command (if (eq window-system 'x) "texx %s" "texsun %s")
  "*Command to invoke previewer on dvi file.")

(defvar tex-default-mode 'latex-mode
  "*Mode to enter for a new file when it can't be determined whether
the file is plain TeX or LaTeX or what.")

(defvar tex-open-quote "``"
  "String inserted by typing \\[tex-insert-quote] to open a quotation.")

(defvar tex-close-quote "''"
  "String inserted by typing \\[tex-insert-quote] to close a quotation.")

(defvar tex-command nil
  "*The command to run TeX on a file.
The name of the file will be appended to this string, separated by a space.
This is a buffer local variable, defaults to "tex" in plain-tex-mode buffers,
and "latex" in latex-mode buffers. If you want to alter it, do the change
in the plain-tex-mode-hook or the latex-mode-hook.")

(defvar tex-trailer nil
  "*String appended after the end of a region sent to TeX by \\[tex-region].")

(defvar tex-start-of-header nil
  "*String used by \\[tex-region] to delimit the start of the file's header.")

(defvar tex-end-of-header nil
  "*String used by \\[tex-region] to delimit the end of the file's header.")

(defvar tex-zap-file nil
  "*Temporary file name used for text being sent as input to TeX.
Should be a simple file name with no extension or directory specification.")


;;; STARTUP STUFF: MODES & MAPS
;;; ===========================================================================
;;; Here's the code that sets up the modes: initialises the keymaps, buffer
;;; local variables, and so forth.

(defvar tex-mode-syntax-table nil
  "Syntax table used while in TeX mode.")

(defvar tex-mode-map nil "Keymap for TeX mode.")
(defvar latex-mode-map nil "Keymap for LaTeX mode.")

(defun tex-define-common-keys (keymap)
  "Define the keys that we want defined in all TeX-related modes.
These modes are tex-mode, latex-mode, and the tex-shell."
  (define-key keymap "\C-c\C-k" 'tex-kill-job)
  (define-key keymap "\C-c\C-l" 'tex-recenter-output-buffer)
  (define-key keymap "\C-c\C-q" 'tex-show-print-queue)
  (define-key keymap "\C-c\C-p" 'tex-print)
  (define-key keymap "\C-cP" 'tex-view-file))

(if tex-mode-map 
    nil
  (setq tex-mode-map (make-sparse-keymap))
  (tex-define-common-keys tex-mode-map)
  (define-key tex-mode-map "\"" 'tex-insert-quote)
  (define-key tex-mode-map "\n" 'tex-terminate-paragraph)
  (define-key tex-mode-map "\e}" 'up-list)
  (define-key tex-mode-map "\e{" 'tex-insert-braces)
  (define-key tex-mode-map "\C-c\C-r" 'tex-region)
  (define-key tex-mode-map "\C-c\C-b" 'tex-buffer)
  (define-key tex-mode-map "\C-cb" 'bibtex-buffer)
  )

;;; Shuffled keybindings so that
;;; - latex-insert-begin codes are consistent with keybindings to
;;;   equivalent commands. This must be so, or you will *never* remember
;;;   all those bindings!
;;; - When binding t/T to table/tabular or q/Q to quote/quotation,
;;;   consistently bound the small letter to the smaller (shorter)
;;;   command. A simple rule to help you remember.
;;; - latex-close-block is on C-c space. Easy to type.
;;; Olin 2/90

(if latex-mode-map
    nil
  (setq latex-mode-map (make-sparse-keymap))
  (tex-define-common-keys latex-mode-map)
  (define-key latex-mode-map "\C-j" 'tex-terminate-paragraph)
  (define-key latex-mode-map "\e{" 'tex-insert-braces)
  (define-key latex-mode-map "\e}" 'up-list)
  (define-key latex-mode-map "\"" 'tex-insert-quote)
  (define-key latex-mode-map "\C-ca" 'latex-array)
  (define-key latex-mode-map "\C-cA" 'latex-abstract)
  (define-key latex-mode-map "\C-c\C-b" 'tex-buffer)
  (define-key latex-mode-map "\C-cb" 'bibtex-buffer)
  (define-key latex-mode-map "\C-c\C-c" 'latex-cite)
  (define-key latex-mode-map "\C-cc" 'latex-center)
  (define-key latex-mode-map "\C-cC" 'latex-chapter)
  (define-key latex-mode-map "\C-c\C-d" 'latex-document)
  (define-key latex-mode-map "\C-cd" 'latex-description)
  (define-key latex-mode-map "\C-c\C-e" 'latex-equation)
  (define-key latex-mode-map "\C-ce" 'latex-enumerate)
  (define-key latex-mode-map "\C-cE" 'latex-example)
  (define-key latex-mode-map "\C-c\C-f" 'latex-footnote)
  (define-key latex-mode-map "\C-cf" 'latex-figure)
  (define-key latex-mode-map "\C-c\C-i" 'latex-item)    ; ^C-TAB
  (define-key latex-mode-map "\C-ci" 'latex-insert-begin)
  (define-key latex-mode-map "\C-cI" 'latex-itemize)
;;;                                                     ; ^C^K = kill job
  (define-key latex-mode-map "\C-cl"  'latex-label)     ; ^C^L = recenter output buffer
  (define-key latex-mode-map "\C-c\C-m" 'latex-make-environment)
  (define-key latex-mode-map "\C-cm" 'latex-minipage)
  (define-key latex-mode-map "\C-cN" 'latex-eqnarray)
  (define-key latex-mode-map "\C-cp" 'latex-picture)    ; ^C^P = tex-print
;;;							; ^CP = tex-view-file
  (define-key latex-mode-map "\C-cq" 'latex-quote)      ; ^C^Q = show print queue
  (define-key latex-mode-map "\C-cQ" 'latex-quotation)
  (define-key latex-mode-map "\C-c\C-r" 'tex-region)
  (define-key latex-mode-map "\C-cr" 'latex-ref)
; What is this? Olin 2/90
; (define-key latex-mode-map "\C-cR" 'refer-find-entry) ; call refer.el package for bib lookup
  (define-key latex-mode-map "\C-c\C-s" 'latex-section)
  (define-key latex-mode-map "\C-cS" 'latex-sloppypar)
  (define-key latex-mode-map "\C-c\C-t" 'latex-titlepage)
  (define-key latex-mode-map "\C-ct" 'latex-table)
  (define-key latex-mode-map "\C-cT" 'latex-tabular)
  (define-key latex-mode-map "\C-c\C-v" 'latex-verb)
  (define-key latex-mode-map "\C-cv" 'latex-verbatim)
  (define-key latex-mode-map "\C-cV" 'latex-verse)
  (define-key latex-mode-map "\C-c " 'latex-close-block) ;easy to type
  (define-key latex-mode-map "\C-c$" 'latex-mathmode)
  )

(defvar tex-shell-map nil
  "Keymap for the tex shell.  A shell-mode-map with a few additions.")

;;; This would be a lot simpler if we just used a regexp search,
;;; but then it would be too slow.
(defun tex-mode ()
  "Major mode for editing files of input for TeX or LaTeX.
Tries to determine (by looking at the beginning of the file) whether
this file is for plain TeX or LaTeX and calls plain-tex-mode or
latex-mode.  If it cannot be determined (e.g., there are no commands
in the file), the value of tex-default-mode is used."
  (interactive)
  (let (mode slash comment)
    (save-excursion
      (goto-char (point-min))
      (while (and (setq slash (search-forward "\\" nil t))
		  (setq comment (let ((search-end (point)))
				  (save-excursion
				    (beginning-of-line)
				    (search-forward "%" search-end t))))))
      (if (and slash (not comment))
	  (setq mode (if (looking-at "documentstyle")
			 'latex-mode
			 'plain-tex-mode))))
    (if mode (funcall mode)
      (funcall tex-default-mode))))

(defun plain-tex-mode ()
  "Major mode for editing files of input for plain TeX.
Makes $ and } display the characters they match.
Makes \" insert `` when it seems to be the beginning of a quotation,
and '' when it appears to be the end; it inserts \" only after a \\.

Use \\[tex-region] to run TeX on the current region, plus a \"header\"
copied from the top of the file (containing macro definitions, etc.),
running TeX in a separate window.  \\[tex-buffer] does the whole buffer.
\\[tex-print] prints the .dvi file made by either of these.

Use \\[validate-tex-buffer] to check buffer for paragraphs containing
mismatched $'s or braces.

Special commands:
\\{tex-mode-map}

Mode variables:
tex-directory
	Directory in which to create temporary files for TeX jobs
	run by \\[tex-region] or \\[tex-buffer].
tex-dvi-print-command
	Command string used by \\[tex-print] to print a .dvi file.
tex-dvi-print-command-suffix
        Additional argument(s) or modifier to tex-dvi-print-command
tex-show-queue-command
	Command string used by \\[tex-show-print-queue] to show the print
	queue that \\[tex-print] put your job on.

Entering plain-tex mode calls the value of text-mode-hook,
then the value of tex-mode-hook, and then the value
of plain-tex-mode-hook."
  (interactive)
  (tex-common-initialization tex-mode-map)
  (setq mode-name "TeX")
  (setq major-mode 'plain-tex-mode)
  (setq tex-command "tex")
  (setq tex-start-of-header "%**start of header")
  (setq tex-end-of-header "%**end of header")
  (setq tex-trailer "\\bye\n")
  (run-hooks 'text-mode-hook 'tex-mode-hook 'plain-tex-mode-hook))

(defun latex-mode ()
  "Major mode for editing files of input for LaTeX.
Makes $ and } display the characters they match.
Makes \" insert `` when it seems to be the beginning of a quotation,
and '' when it appears to be the end; it inserts \" only after a \\.

Use \\[tex-region] to run LaTeX on the current region, plus the preamble
copied from the top of the file (containing \\documentstyle, etc.),
running LaTeX under a special subshell.  \\[tex-buffer] does the whole buffer.
\\[tex-print] prints the .dvi file made by either of these.

Use \\[validate-latex-buffer] to check buffer for unmatched latex constructs.

Special commands:
\\{latex-mode-map}

Mode variables:
tex-directory
	Directory in which to create temporary files for TeX jobs
	run by \\[tex-region] or \\[tex-buffer].
tex-dvi-print-command
	Command string used by \\[tex-print] to print a .dvi file.
tex-dvi-print-command-suffix
        Additional argument(s) or other modification for tex-dvi-print-command
tex-show-queue-command
	Command string used by \\[tex-show-print-queue] to show the print
	queue that \\[tex-print] put your job on.

Entering LaTeX mode calls the value of text-mode-hook,
then the value of tex-mode-hook, and then the value
of latex-mode-hook."
  (interactive)
  (tex-common-initialization latex-mode-map)
  (setq mode-name "LaTeX")
  (setq major-mode 'latex-mode)
  (setq tex-command "latex")
  (setq tex-start-of-header "\\documentstyle")
  (setq tex-end-of-header "\\begin{document}")
  (setq tex-trailer "\\end{document}\n")
  (run-hooks 'text-mode-hook 'tex-mode-hook 'latex-mode-hook))

(defun tex-common-initialization (keymap)
  (kill-all-local-variables)
  (use-local-map keymap)
  (setq local-abbrev-table text-mode-abbrev-table)
  (if (null tex-mode-syntax-table)
      (progn
	(setq tex-mode-syntax-table (make-syntax-table))
	(set-syntax-table tex-mode-syntax-table)
	(modify-syntax-entry ?\\ ".")
	(modify-syntax-entry ?\f ">")
	(modify-syntax-entry ?\n ">")
	(modify-syntax-entry ?$ "$$")
	(modify-syntax-entry ?% "<")
	(modify-syntax-entry ?\" ".")
	(modify-syntax-entry ?& ".")
	(modify-syntax-entry ?_ ".")
	(modify-syntax-entry ?@ "_")
	(modify-syntax-entry ?~ " ")
	(modify-syntax-entry ?' "w"))
    (set-syntax-table tex-mode-syntax-table))
  (make-local-variable 'paragraph-start)
  (setq paragraph-start "^[ \t]*$\\|^[\f\\\\%]")
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'comment-start)
  (setq comment-start "%")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "\\(\\(^\\|[^\\]\\)\\(\\\\\\\\\\)*\\)\\(%+ *\\)")
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'tex-comment-indent)
  (make-local-variable 'tex-command)
  (make-variable-buffer-local 'tex-dvi-print-command-suffix)
  (make-local-variable 'tex-start-of-header)
  (make-local-variable 'tex-end-of-header)
  (make-local-variable 'tex-trailer))


;;; COMMENT INDENTING AND QUOTES
;;; ===========================================================================

(defun tex-comment-indent ()
  (if (looking-at "%%%")
      (current-column)
    (skip-chars-backward " \t")
    (max (if (bolp) 0 (1+ (current-column)))
	 comment-column)))

(defun tex-insert-quote (arg)
  "Insert the appropriate quote marks for TeX.
Inserts the value of tex-open-quote (normally ``) or tex-close-quote
(normally '') depending on the context.  With prefix argument, always
inserts \" characters."
  (interactive "P")
  (if arg
      (self-insert-command (prefix-numeric-value arg))
    (insert
     (cond ((or (bobp)
		(save-excursion
		  (forward-char -1)
		  (looking-at "\\s(\\|\\s \\|\\s>")))
	    tex-open-quote)
	   ((= (preceding-char) ?\\)
	    ?\")
	   (t
	    tex-close-quote)))))


;;; VALIDATION FUNCTIONS
;;; ===========================================================================
;;; There are two kinds of validation function: TeX and LaTeX.
;;; TeX validation means checking for balanced {}, $ $, and $$ $$ pairs.
;;; LaTeX validation also means checking for balanced environment delimiters,
;;; such as \begin{center} ... \end{center}.
;;; It is probably a mistake to insist on LaTeX validating on paragraph
;;; boundaries, because LaTeX environments can span multiple environments.
;;;
;;; Validation is a mess. See my notes at the end of the file. Olin


;;; Added code to really position cursor at the beginning of the offending
;;; paragraph, not 2 lines above it. Olin 2/90

(defun validate-tex-buffer ()
  "Check current buffer for paragraphs containing mismatched $'s and {}'s.
As each such paragraph is found, a mark is pushed at its beginning,
and the location is displayed for a few seconds."
  (interactive)
  (let ((opoint (point)))
    (goto-char (point-max))
    ;; Does not use save-excursion
    ;; because we do not want to save the mark.
    (unwind-protect
	(while (and (not (input-pending-p)) (not (bobp)))
	  (let* ((end (point))
		 (found-para (search-backward "\n\n" nil 'move)))
	    (or (validate-tex-paragraph (point) end)
		(progn
		  (if found-para (forward-char 2))
		  (push-mark (point))
		  (message "Mismatch found in pararaph starting here")
		  (sit-for 4)
		  (if found-para (backward-char 2))))))
      (goto-char opoint))))

(defun validate-tex-region (start end)
  "Check for mismatched braces or $'s in region.
Returns t if no mismatches.  Returns nil and moves point to suspect
area if a mismatch is found."
 (interactive "r")
  (let ((failure-point (save-excursion
			 (condition-case ()
			     (save-restriction
			       (narrow-to-region start end)
			       (goto-char start)
			       (while (< (point) (point-max))
				 (forward-sexp))
			       nil)
			   (error (point))))))
    (if failure-point
	(progn
	  (goto-char failure-point)
	  (re-search-forward "[\n\t ]*") ; Skip to start of bad sexp.
	  nil)
      t)))

;; AR's additions backed out. They're good for LaTeX, but screw TeX up.
;; But see validate-latex-region below. Olin. 2/90 

(defun validate-tex-paragraph (start end)
  (condition-case ()
      (save-excursion
	(save-restriction
	  (narrow-to-region start end)
;         (goto-char end)       ;; Added, AR 2/22/88.
;         (latex-close-block t) ;; Added, AR 2/22/88.
	  (goto-char start)
	  (while (< (point) (point-max))
	    (forward-sexp))
	  t))
    (error nil)))

;;; Changed to only insert a single newline. Hit it twice. This
;;; double-newline stuff is amazingly irritating when you tend to
;;; do it a lot accidentally. People who are used to using linefeed
;;; to terminate lines of Lisp code do this a lot. Olin 2/90

(defun tex-terminate-paragraph (inhibit-validation)
  "Insert a newline and check for mismatched braces/$'s in previous paragraph.
A prefix arg inhibits the checking."
  (interactive "P")
  (or inhibit-validation
      (save-excursion
	(validate-tex-region
	 (save-excursion
	   (search-backward "\n\n" nil 'move)
	   (point))
	 (point)))
      (message "Paragraph being closed appears to contain a mismatch"))
  (insert "\n"))


;;; LaTeX validation functions.
;;; ===========================================================================
;;; Both functions only report the first tex-validation problem they find,
;;; possibly missing others. This can be losing.

(defun validate-latex-region (start end)
  "TeX validate the region, and also check for balanced \\begin \\end pairs."
  (interactive "r")
  (cond ((not (validate-tex-region start end)) ; validate for TeX syntax
	 (message "Unbalanced identifier.")
	 (sit-for 2)))
  (save-restriction
    (let ((opoint (point)))
      (narrow-to-region start end)
      (goto-char end)			; Added, AR 2/22/88.
      (latex-close-block t)		; Added, AR 2/22/88.
      (goto-char opoint))))

;;; Note that this uses a global TeX validate, not a per-paragraph one.
(defun validate-latex-buffer ()
 "TeX validate the entire buffer, and check for balanced \\begin \\end pairs."
 (interactive)
 (validate-latex-region (point-min) (point-max))
 (message "Buffer validated."))


;;; SOURCE ENTRY COMMANDS
;;; ===========================================================================
;;; (Mostly courtesy Dorab Patel)

(defun tex-insert-braces ()
  "Make a pair of braces and be poised to type inside of them."
  (interactive)
  (insert ?\{)
  (save-excursion
    (insert ?})))

;;;
;;; Most of the environments below are based on versions by Dorab Patel
;;; at UCLA, further adapted by Steve Anderson <anderson@sapir.cog.jhu.edu>
;;;
;;; These functions make an entire LaTeX environment and leave the cursor
;;; poised to type at the appropriate place within it:

(defun latex-mathmode ()
  "Make a pair of dollar-signs and be poised to type inside of them."
  (interactive)
  (insert ?\$)
  (save-excursion
    (insert ?\$)))

(defun latex-make-environment (name)
  "Make a latex NAME environment."
  (interactive "sEnvironment: ")
  (insert "\\begin\{" name "}\n")
  (save-excursion (insert "\n\\end\{" name "}")))

(defun latex-abstract () "Make an \\abstract environment."
  (interactive) (latex-make-environment "abstract"))

(defun latex-array (arg)
  "Make a \\array env; with argument, include \[pos\] slot."
  (interactive "P")
  (insert "\\begin\{array\}")
  (if arg (progn (insert "\[")
		 (save-excursion
		   (insert "\]\{\}\n\n\\end\{array\}")))
      (progn (insert ?{)
	     (save-excursion (insert "\}\n\n\\end\{array\}")))))

(defun latex-center () "Make a \\center environment."
  (interactive) (latex-make-environment "center"))

(defun latex-chapter (&optional arg)
  "Insert a \\chapter or (with argument) \\chapter* command."
  (interactive "P")
  (insert (concat "\\chapter" (if arg "*") "\{"))
  (save-excursion (insert?})))

(defun latex-cite (arg)
  "Insert a \\cite command.
With one \\[universal-argument], insert a \\shortcite command
With two \\[universal-argument]'s, insert a \\nocite command"
  (interactive "p")
  (insert (cond ((eq arg 1) "\\cite\{")
		((eq arg 4) "\\shortcite\{")
		((eq arg 16) "\\nocite\{")
		(t (error "Bad arg to latex-cite"))))
  (save-excursion (insert ?})))

(defun latex-description () "Make a \\description environment."
  (interactive) (latex-make-environment "description"))

(defun latex-document (&optional arg)
  "Set up a LaTeX document; with argument, just make a \\document environment."
  (interactive "P")
  (if arg (latex-make-environment "document")
      (progn
	(insert "\\documentstyle\[\]\{")
	(save-excursion
	  (insert-string "\}\n\n\\begin\{document\}\n\n\\end\{document\}")))))

(defun latex-enumerate () "Make an \\enumerate environment."
  (interactive) (latex-make-environment "enumerate"))

(defun latex-eqnarray (&optional arg)
  "Make a \\eqnarray or (with argument) \\eqnarray* environment."
  (interactive "P")
  (latex-make-environment (concat "eqnarray"
				  (if arg "*"))))

(defun latex-equation () "Make an \\equation environment."
  (interactive) (latex-make-environment "equation"))

(defun latex-example () "Make an \\example environment."
  (interactive) (latex-make-environment "example"))

(defun latex-figure (&optional arg)
  "Make a \\figure environment; with arg, include optional \[loc\] slot."
  (interactive "P")
  (insert "\\begin\{figure\}")
  (if arg (progn (insert ?[)
		 (save-excursion
		   (insert "\]\n\n\\end\{figure\}")))
      (progn (insert "\n")
	     (save-excursion (insert "\n\\end\{figure\}")))))

(defun latex-footnote ()
    "Make a LaTeX footnote."
  (interactive)
  (insert "\\footnote\{")
  (save-excursion (insert ?})))

(defun latex-item ()
  "Insert an \\item command."
  (interactive)
  (insert "\\item "))

(defun latex-itemize () "Make an \\itemize environment."
  (interactive) (latex-make-environment "itemize"))

(defun latex-label ()
  "Insert a \\label command."
  (interactive)
  (insert "\\label\{")
  (save-excursion (insert ?})))

(defun latex-minipage (&optional arg)
  "Make a \\minipage environment; with argument, include \[pos\] slot."
  (interactive "P") 
  (insert "\\begin\{minipage\}")
  (if arg (progn (insert "\[")
		 (save-excursion
		   (insert "\]\{\}\n\n\\end\{minipage\}")))
      (progn (insert ?{)
	     (save-excursion (insert "\}\n\n\\end\{minipage\}")))))

(defun latex-picture () "Make a \\picture environment."
  (interactive) (latex-make-environment "picture"))

(defun latex-quotation () "Make a \\quotation environment."
  (interactive) (latex-make-environment "quotation"))

(defun latex-quote () "Make a \\quote environment."
  (interactive) (latex-make-environment "quote"))

(defun latex-ref ()
  "Insert a \\ref command."
  (interactive)
  (insert "\\ref\{")
  (save-excursion (insert ?})))
  
(defun latex-section (arg)
  "Insert a \\section command or a variation of it.
With no argument, insert a \\section command.
With one \\[universal-argument], insert a \\subsection command.
With two \\[universal-argument]'s, insert a \\subsubsection command.
With three \\[universal-argument]'s, insert a \\paragraph command.
With four \\[universal-argument]'s, insert a \\subparagraph command."
  (interactive "p")
  (insert (cond ((eq arg 1) "\\section\{")
		((eq arg 4) "\\subsection\{")
		((eq arg 16) "\\subsubsection\{")
		((eq arg 64) "\\paragraph\{")
		((eq arg 256) "\\subparagraph\{")
		(t (error "Bad arg to latex-section"))))
  (save-excursion (insert ?})))

(defun latex-sloppypar () "Make a \\sloppypar environment."
  (interactive) (latex-make-environment "sloppypar"))

(defun latex-table (&optional arg)
  "Make a \\table environment; with arg, include \[loc\] slot."
  (interactive "P")
  (insert "\\begin\{table\}")
  (if arg (progn (insert "\[" )
		 (save-excursion
		   (insert "\]\n\n\\end\{table\}")))
      (progn (insert "\n")
	     (save-excursion (insert "\n\\end\{table\}")))))

(defun latex-tabular (&optional arg)
  "Make a \\tabular environment; with arg, include optional \[pos\] slot."
  (interactive "P")
  (insert "\\begin\{tabular\}")
  (if arg (progn (insert "\[")
		 (save-excursion
		   (insert "\]\{\}\n\n\\end\{tabular\}")))
      (progn (insert ?{)
	     (save-excursion (insert "\}\n\n\\end\{tabular\}")))))

(defun latex-titlepage () "Make a \\titlepage environment."
  (interactive) (latex-make-environment "titlepage"))

(defun latex-verb (&optional arg)
  "Insert a \\verb or (with argument) \\verb* command."
  (interactive "P")
  (insert "\\verb" (if arg "*" "")))

(defun latex-verbatim (&optional arg)
  "Make a \\verbatim or \\verbatim* environment."
  (interactive "P")
  (latex-make-environment (concat "verbatim" (if arg "*"))))

(defun latex-verse () "Make a \\verse environment."
  (interactive)
  (latex-make-environment "verse"))

;; The following function (based on an idea originally due to
;; Niall Mansfield <mansfiel%scr.slb.com@relay.cs.net>), as
;; modified by Steve Anderson <anderson@sapir.cog.jhu.edu>
;; inserts just the \begin{environment} statement, leaving it to 
;; be closed by the function latex-close-block below
;;
;; The command codes have been made consistent with the default 
;; latex-mode keybindings. You also now get a helpful 1 line menu if you get
;; confused and type ? at the prompt. Whew. Olin 2/90.

(defun latex-insert-begin (environment)
  "Define a LaTeX environment.
You are prompted for environment single character replies insert an
appropriate \\begin\{environment\} as follows:

a  array          lL list
A  abstract       q  quote
c  center         Q  quotation
d  description    sS sloppypar
e  enumerate      t  table
E  example        T  tabular
f  figure         v  verbatim
iI itemize        ?  <show short menu>

Any other reply just yields \\begin\{\}."
; (interactive "cEnvironment: ")
  (interactive (let* ((cursor-in-echo-area t)
		      (result (progn (message "Environment (? for help):")
				     (read-char))))
		 (while (eq result ??)
		   (message
 "arr Abs cent desc enum Ex fig Item Lis quot Quota Sloppy tabl Tabu verbat")
		   (setq result (read-char)))
		 (list result)))
  (let ((environment-string
	 (cond
	   ((equal environment ?a)"array\}\{")
	   ((equal environment ?A)"abstract")
	   ((equal environment ?c)"center")
	   ((equal environment ?d)"description")
	   ((equal environment ?e)"enumerate")
	   ((equal environment ?E)"example")
	   ((equal environment ?f)"figure")
	   ((equal environment ?i)"itemize") ; hack
	   ((equal environment ?I)"itemize")
	   ((equal environment ?l)"list") ; hack
	   ((equal environment ?L)"list")
	   ((equal environment ?q)"quote")
	   ((equal environment ?Q)"quotation")
	   ((equal environment ?s)"sloppypar") ; hack
	   ((equal environment ?S)"sloppypar")
	   ((equal environment ?T)"tabular\}\{")
	   ((equal environment ?t)"table")
	   ((equal environment ?v)"verbatim")
	   (t ""))))
    (insert "\\begin{" 
	    environment-string 
	    "}\n")
    (if (not (memq environment ; if ?a ?T or unknown
		   '(?A ?c ?d ?e ?E ?f ?i ?I ?l ?L ?q ?Q ?s ?S ?t ?v)))
	(backward-char 2))))

;; Replacement for latex-close-block in lisp/tex-mode.el.
;; Ashwin Ram, 2/22/88.
(defun latex-close-block (&optional validate)
   "Inserts an \\end{...} to match corresponding \\begin{...}.
If optional argument VALIDATE is t, doesn't insert anything, only validates."
   (interactive "*")
   (let ((fail-point (point))
         (nesting '())
         (done nil))         
      (end-of-line)
      (while (not done)
         (if (re-search-backward "\\\\\\(begin\\|end\\){\\([^}\n]*\\)}" (point-min) t)
             (let ((which (buffer-substring (match-beginning 1) (match-end 1)))
                   (text (buffer-substring (match-beginning 2) (match-end 2))))
                (if (equal which "end")
                    (setq nesting (cons text nesting))
                    (if (null nesting)
                        (let ((indentation (current-column)))
                           (goto-char fail-point)
                           (if validate
                               (error "\\begin{%s} is never ended" text)
                               (progn
                                  (beginning-of-line)
                                  (if (not (looking-at "^[ \t]*\n"))
                                      (progn (end-of-line) (insert "\n")))
                                  (indent-to indentation)
                                  (insert "\\end{" text "}\n")))
                           (setq done t))
                        (if (equal text (car nesting))
                            (setq nesting (cdr nesting))
                            (error "\\begin{%s} ended by \\end{%s}" text (car nesting))))))
             (progn
                (goto-char fail-point)
                (if (not validate) (message "No unmatched \\begin{...} to end here"))
                (setq done t))))))




;;; PROCESS STUFF: TEXING, BIBTEXING, PRINTING, VIEWING & DISPLAYING QUEUE
;;; ===========================================================================

;;; The utility functions:

(defun tex-make-process-buffer ()
  (require 'comint)
  (save-excursion
    (set-buffer (get-buffer-create "*tex-shell*"))
    (comint-mode)
    (setq tex-shell-map (full-copy-sparse-keymap comint-mode-map))
    (tex-define-common-keys tex-shell-map)
    (use-local-map tex-shell-map)))

(defun set-buffer-directory (buffer directory)
  "Set BUFFER's default directory to be DIRECTORY."
  (setq directory (file-name-as-directory (expand-file-name directory)))
  (if (not (file-directory-p directory))
      (error "%s is not a directory" directory)
    (save-excursion
      (set-buffer buffer)
      (setq default-directory directory))))

;;; The commands:

;;; It's a kludge that we have to create a special buffer just 
;;; to write out the tex-trailer.  It would nice if there were a
;;; function like write-region that would write literal strings.

;;; Changed a (forward-line -1) that was causing an extra
;;; line to be included in the header to a (beginning-of-line). Olin 1/89

(defun tex-region (beg end)
  "Run TeX on the current region, via a temporary file.
The temporary file is tex-zap-file and it is written in directory
tex-directory, and TeX is run in that directory.

If the buffer has a header, it is written to the temporary file before
the region itself.  The buffer's header is all lines between the
strings defined by tex-start-of-header and tex-end-of-header
inclusive.  The header must start in the first 100 lines.  The value
of tex-trailer is appended to the temporary file after the region.

Process is run in a comint buffer (see comint mode). Additional
commands available are:
	tex-kill-job		tex-recenter-output-buffer
	tex-print		tex-show-print-queue
	tex-view-file"
  (interactive "r")
  (or tex-zap-file (setq tex-zap-file (make-temp-name "zap")))
  (let* ((temp-buffer (get-buffer-create " Tex-Output-Buffer"))
	 (zap-directory
	  (file-name-as-directory (expand-file-name tex-directory)))
	 (tex-out-file (concat tex-zap-file ".tex")) ; Looks nicer
	 (full-tex-out-file (expand-file-name tex-out-file zap-directory)))
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(forward-line 100)
	(let ((search-end (point))
	      (hbeg (point-min)) (hend (point-min)))
	  (goto-char (point-min))
	  ;; Initialize the temp file with either the header or nothing
	  (if (search-forward tex-start-of-header search-end t)
	      (progn
		(beginning-of-line)
		(setq hbeg (point))	;mark beginning of header
		(if (search-forward tex-end-of-header nil t)
		    (progn (forward-line 1)
			   (setq hend (point)))	;mark end of header
		  (setq hbeg (point-min))))) ;no header
	  (write-region (min hbeg beg) hend full-tex-out-file nil nil)
	  (write-region (max beg hend) end full-tex-out-file t nil))
	(let ((local-tex-trailer tex-trailer))
	  (set-buffer temp-buffer)
	  (erase-buffer)
	  ;; make sure trailer isn't hidden by a comment
	  (insert-string "\n")
	  (if local-tex-trailer (insert-string local-tex-trailer))
	  (write-region (point-min) (point-max) full-tex-out-file t nil))))
    (tex-process-file zap-directory tex-out-file)))

;;; Altered so that TeX is run directly on the file, if the buffer
;;; is visiting a file and unmodified. Olin 1/89

(defun tex-buffer ()
  "Run TeX on current buffer.  
If buffer is a file buffer, and modified, first offers to save buffer.
Afterwards, if buffer is a file buffer and unmodified, runs TeX directly on
file.  Otherwise, calls tex-region on whole buffer."
  (interactive)
  (let ((fname (buffer-file-name)))
    ;; Offer to save buffer if a modified file buffer.
    (if (and fname
	     (buffer-modified-p)
	     (y-or-n-p (format "Save buffer %s first? " (buffer-name))))
	(save-buffer))
    (if (and fname (not (buffer-modified-p)))
	;; If unmodified file buffer, then tex it directly.
	(tex-process-file (file-name-directory fname)
			  (file-name-nondirectory fname)) ; looks nicer
	;; otherwise, just tex-region the whole buffer.
	(tex-region (point-min) (point-max)))))

;; New auxiliary function.
(defun tex-process-file (dir fname)
  "Fires up a TeX process, working directory DIR, on file FNAME."
  (if (get-buffer "*tex-shell*")
      (let ((old-proc (get-buffer-process "*tex-shell*")))
	(if (and old-proc (memq (process-status old-proc) '(run stop)))
	    (message "Killing old TeX process %s..." (process-name old-proc))))
      (tex-make-process-buffer))
  (set-buffer-directory "*tex-shell*" dir)
  (tex-recenter-output-buffer 0)
  (save-excursion
    (let ((command tex-command)) ; tex-command is buffer local.
      (set-buffer "*tex-shell*")
      (goto-char (point-max))
      (insert (format "\n--- Running process: %s %s\n--- working directory %s\n"
		      command fname dir))))
  (comint-exec "*tex-shell*" "tex-shell" tex-command nil (list fname))
  (setq tex-last-processed-file (expand-file-name fname dir)))

(defun tex-kill-job ()
  "Kill the currently running TeX job."
  (interactive)
  (kill-process "tex-shell" t)) ; no mercy. no coredump.

(defun tex-recenter-output-buffer (linenum)
  "Redisplay buffer of TeX job output so that most recent output can be seen.
The last line of the buffer is displayed on
line LINE of the window, or centered if LINE is nil."
  (interactive "P")
  (let ((tex-shell (get-buffer "*tex-shell*"))
	(old-buffer (current-buffer)))
    (if (null tex-shell)
	(message "No TeX output buffer")
      (pop-to-buffer tex-shell)
      (bury-buffer tex-shell) ; WHAT IS THE POINT OF THIS LINE??? Olin
      (goto-char (point-max))
      (recenter (if linenum
		    (prefix-numeric-value linenum)
		  (/ (window-height) 2)))
      (pop-to-buffer old-buffer)
      )))

	
(defun tex-print (arg)
  "Print the .dvi file made by \\[tex-region] or \\[tex-buffer].
Runs the shell command defined by tex-dvi-print-command.
With argument, allows the user to edit the command string first.
This is useful for inserting special flags, e.g., to print a range of
pages, route output to non-default printer, print landscape, etc..
See variables tex-dvi-print-command and tex-dvi-print-command-suffix.
Non-nil tex-after-print-hook is called after a successful completion."
  (interactive "P")
  (if (stringp tex-last-processed-file)
      (let ((dir (file-name-directory tex-last-processed-file))
	    (fname (file-name-nondirectory tex-last-processed-file)))
	(if (string-match "\\(^.*\\.\\)[^.]*$" fname) ; Strip off extension
	    (let* ((noext (substring fname 0 (match-end 1))) ; foo.tex ==> foo.
		   (cmd (tex-optionally-mung-command
			 (format tex-dvi-print-command
				 (concat tex-dvi-print-command-suffix
					 "\"" dir noext "dvi\"")))))
	      (shell-command cmd)
	      (run-hooks 'tex-after-print-hook))
	    (error "Can't figure out name of dvi file")))
      (error "No last file processed.")))

(defun bibtex-buffer ()
  "Run BibTeX on the .aux file made by \\[tex-region] or \\[tex-buffer].
Runs the shell command defined by tex-bibtex-command."
  (interactive)
  (require 'background)
  (if (stringp tex-last-processed-file)
      (let ((dir (file-name-directory tex-last-processed-file))
	    (fname (file-name-nondirectory tex-last-processed-file)))
	(if (string-match "\\(^.*\\)\\.[^.]*$" fname) ; Strip off extension
	    (let* ((noext (substring fname 0 (match-end 1))) ; foo.tex ==> foo
		   (cmd (format tex-bibtex-command                   ;;my copy of
				(concat "\"" dir noext "\"" "\n")))) ;;bibtex requires 
	      (message cmd)		                             ;;extra linefeed
	      (background cmd))
	    (error "Can't figure out name of aux file")))
      (error "No last file processed.")))

(defun tex-view-file (&optional arg)
  "Display the .dvi file made by \\[tex-region] or \\[tex-buffer] in a window.
Runs the shell command defined by tex-view-dvi-file-command in the background.
With argument, allows the user to edit the command string first."
  (interactive "P")
  (require 'background)
  (if (stringp tex-last-processed-file)
      (let ((dir (file-name-directory tex-last-processed-file))
	    (fname (file-name-nondirectory tex-last-processed-file)))
	(if (string-match "\\(^.*\\)\\.[^.]*$" fname) ; Strip off extension
	    (let* ((noext (substring fname 0 (match-end 1))) ; foo.tex ==> foo
		   (cmd (tex-optionally-mung-command
			 (format tex-view-dvi-file-command
				 (concat "\"" dir noext "\"")))))
	      (background cmd))
	    (error "Can't figure out name of dvi file")))
      (error "No last file processed.")))
	
(defun tex-show-print-queue (arg)
  "Show the print queue that \\[tex-print] put your job on.
Runs the shell command defined by tex-show-queue-command.
With argument, allows the user to edit the command string first."
  (interactive "P")
  (require 'background)
  (background (tex-optionally-mung-command tex-show-queue-command)))

;;; If the user gave a prefix arg to the interactive command, give him
;;; a chance to edit CMD before returning it.
(defun tex-optionally-mung-command (cmd &optional prompt)
  (if current-prefix-arg (read-string (or prompt ": ") cmd)
      (message cmd)
      cmd))


;;;Notes on validation
;;;============================================================================
;;; Validation is a mess. There are several interacting problems.
;;; - Validation is errorful in both directions: it can miss real problems,
;;;   and report non-problems. For example, TeX validating $\left( X \right.$
;;;   will decide that the open paren is unbalanced, and there isn't much
;;;   you can do to fix this. So validation error reports must be treated
;;;   as suggestions. Further, you shouldn't stop at the first error report,
;;;   because it may be bogus, and you might miss out on reporting a real
;;;   error further in.
;;;
;;; - Validating at paragraph boundaries is debatable. It is a useful
;;;   heuristic, but some TeX delimiters and many LaTeX environments
;;;   span multiple paragraphs. Validating regions a paragraph at a time
;;;   in such cases is very irritating, because each non-error is bogusly
;;;   reported twice: at the open delimiter and at the close delimiter.
;;;   Validating globally, on the other hand, can miss two errors that
;;;   balance each other out -- this is probably very rare, though.
;;;
;;;   Per-paragraph validation has the advantage that you can reset
;;;   after finding a possible error to the next paragraph and keep on
;;;   looking for further errors. Global validation uses forward-sexp,
;;;   and it would be a lot of hair to use it to find more than a
;;;   single (potential) error. What to do? You lose.
;;;
;;; - Some validation functions signal errors by returning nil, some
;;;   by calling ERROR. The interprocedural communications of these
;;;   guys is a mess.
;;;
;;; - TeX support is very important for Emacs. But this issue is not
;;;   trivial. It needs a careful working out by someone reasonably
;;;   competent who will take the time and trouble to do a good
;;;   solution.
;;;       -Olin 2/90
