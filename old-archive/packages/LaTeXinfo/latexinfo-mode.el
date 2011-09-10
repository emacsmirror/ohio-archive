;; Major mode for editing latexinfo files.
;; Copyright (C) 1985, 1988 Free Software Foundation, Inc.

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


(defvar latexinfo-mode-syntax-table nil)

(if latexinfo-mode-syntax-table
    nil
  (setq latexinfo-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\" " " latexinfo-mode-syntax-table)
  (modify-syntax-entry ?@ " " latexinfo-mode-syntax-table)
  (modify-syntax-entry ?\\ "\\" latexinfo-mode-syntax-table)
  (modify-syntax-entry ?\^q "\\" latexinfo-mode-syntax-table)
  (modify-syntax-entry ?\[ "(]" latexinfo-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" latexinfo-mode-syntax-table)
  (modify-syntax-entry ?{ "(}" latexinfo-mode-syntax-table)
  (modify-syntax-entry ?} "){" latexinfo-mode-syntax-table)
  (modify-syntax-entry ?\' "w" latexinfo-mode-syntax-table))

(defvar latexinfo-mode-map nil)

(if latexinfo-mode-map
    nil
  (setq latexinfo-mode-map (make-sparse-keymap))
  (define-key latexinfo-mode-map "\C-c\C-f" 'latexinfo-format-region)
  (define-key latexinfo-mode-map "\C-c\C-s" 'latexinfo-show-structure)
  (define-key latexinfo-mode-map "\e}"          'up-list)
  (define-key latexinfo-mode-map "\e{"          'latexinfo-insert-braces)
  (define-key latexinfo-mode-map "\C-c\C-cv"    'latexinfo-insert-var)
  (define-key latexinfo-mode-map "\C-c\C-cs"    'latexinfo-insert-samp)
  (define-key latexinfo-mode-map "\C-c\C-cn"    'latexinfo-insert-node)
  (define-key latexinfo-mode-map "\C-c\C-ci"    'latexinfo-insert-item)
  (define-key latexinfo-mode-map "\C-c\C-ce"    'latexinfo-insert-end)
  (define-key latexinfo-mode-map "\C-c\C-cd"    'latexinfo-insert-dfn)
  (define-key latexinfo-mode-map "\C-c\C-cc"    'latexinfo-insert-code))

(defun latexinfo-insert-var ()
  "Insert the string \\var{} in a latexinfo buffer."
  (interactive)
  (insert "\\var{}")
  (backward-char))

(defun latexinfo-insert-samp ()
  "Insert the string \\samp{} in a latexinfo buffer."
  (interactive)
  (insert "\\samp{}")
  (backward-char))

(defun latexinfo-insert-node ()
  "Insert the string \\node in a latexinfo buffer, 
along with a comment indicating the arguments to \\node."
  (interactive)
  (insert "\\node     \n\\comment  node-name,  next,  previous,  up")
  (forward-line -1)
  (forward-char 6))

(defun latexinfo-insert-item ()
  "Insert the string \\item in a latexinfo buffer."
  (interactive)
  (insert "\\item")
  (newline))

(defun latexinfo-insert-end ()
  "Insert the string \\end{} in a latexinfo buffer."
  (interactive)
  (insert "\\end{}"))

(defun latexinfo-insert-dfn ()
  "Insert the string \\dfn{} in a latexinfo buffer."
  (interactive)
  (insert "\\dfn{}")
  (backward-char))

(defun latexinfo-insert-code ()
  "Insert the string \\code{} in a latexinfo buffer."
  (interactive)
  (insert "\\code{}")
  (backward-char))

(defun latexinfo-insert-braces ()
  "Make a pair of braces and be poised to type inside of them.
Use \\[up-list] to move forward out of the braces."
  (interactive)
  (insert "{}")
  (backward-char))

(defun latexinfo-mode ()
  "Major mode for editing latexinfo files.

  It has these extra commands:
\\{latexinfo-mode-map}

  These are files that are used as input for LaTeX to make printed manuals
and also to be turned into Info files by \\[latexinfo-format-buffer].
These files must be written in a very restricted and modified version
of TeX input format.

  Editing commands are like text-mode except that the syntax table is
set up so expression commands skip Latexinfo bracket groups.  To see
what the Info version of a region of the Latexinfo file will look like,
use \\[latexinfo-format-region].  This command runs Info on the current region
of the Latexinfo file and formats it properly.

  You can show the structure of a Latexinfo file with \\[latexinfo-show-structure].
This command shows the structure of a Latexinfo file by listing the
lines with the commands for \\node, \\chapter, \\section and the
like.  These lines are displayed in another window called the *Occur*
window.  In that window, you can position the cursor over one of the
lines and use \\[occur-mode-goto-occurrence], to jump to the
corresponding spot in the Latexinfo file.

  In addition, Latexinfo mode provides commands that insert various
frequently used commands into the buffer.  You can use these
commands to save keystrokes.  And you can insert balanced braces with
\\[latexinfo-insert-braces] and later use the command \\[up-list] to
move forward past the closing brace.

Entering Latexinfo mode calls the value of text-mode-hook, and then the
value of latexinfo-mode-hook."
  (interactive)
  (text-mode)
  (setq mode-name "Latexinfo")
  (setq major-mode 'latexinfo-mode)
  (use-local-map latexinfo-mode-map)
  (set-syntax-table latexinfo-mode-syntax-table)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate (concat "^\b\\|^\\\\[a-zA-Z{}]*[ \n]\\|" paragraph-separate))
  (make-local-variable 'paragraph-start)
  (setq paragraph-start    (concat "^\b\\|^\\\\[a-zA-Z{}]*[ \n]\\|" paragraph-start))
  (make-local-variable 'fill-column)
  (setq fill-column 72)
  (make-local-variable 'comment-start)
  (setq comment-start "\\c ")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "\\c +")
  (run-hooks 'text-mode-hook 'latexinfo-mode-hook))

(defvar latexinfo-heading-pattern
  "^\\\\\\(chapter\\|appendix\\|sect\\|sub\\|heading\\|major\\|node\\)"
  "This is a regular expression to match Latexinfo lines that are chapter
or sections headings or like such.")

(defun latexinfo-show-structure () 
  "Show the structure of a Latexinfo file by listing the lines with the
commands for \\node, \\chapter, \\section and the like.  Lines
with structuring commands in them are displayed in another window
called the *Occur* window.  In that window, you can position the
cursor over one of the lines and use \\[occur-mode-goto-occurrence],
to jump to the corresponding spot in the Latexinfo file."
  (interactive)
  (save-excursion 
    (goto-char (point-min))
    (occur latexinfo-heading-pattern))
  (pop-to-buffer "*Occur*")
  (goto-char (point-min))
  (flush-lines "-----"))

