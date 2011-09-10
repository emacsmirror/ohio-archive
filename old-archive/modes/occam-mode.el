;; OCCAM mode for GNU emacs at DIKU
;; by Jesper Larsson Traff, DIKU autumn 1989.
;; Copyright (C) Jesper Larsson Traff and DIKU

;; LCD Archive Entry:
;; occam-mode|Jesper Larsson Traff||
;; OCCAM programing mode.|
;; 89-09||~/modes/occam-mode.el.Z|

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

(defconst occam-indent 2 
  "*OCCAM standard indentation (do not change!)")

(defconst occam-process-keywords
  '("SEQ"                         ; sequential process
    "PAR"                         ; parallel process
    "IF"                          ; conditional process
    "ALT"                         ; alternative (special) process
    "WHILE"                       ; iterative process
    "CASE"                        ; selection process
    "ELSE"                        ; default process in selection
    "VALOF"
    "PROC"
    "PROCESSOR"
    "PLACED"
    "PRI"
    )
  "*OCCAM proccess keywords")

(defconst occam-reserved-words
  '("INT" "INT16""INT32" "INT64"  ; integer declarations
    "REAL" "REAL32" "REAL64"      ; real declarations
    "BYTE"                        ; byte (character) declaration
    "BOOL" "TRUE" "FALSE"         ; boolean declaration and constants
    "CHAN"                        ; channel declaration
    "OF"
    "PROTOCOL"                    ; protocol declaration
    "TIMER"                       ; timer declaration
    "VAL"
    "IS"
    "RESULT"
    "FOR"                         ; replicator keyword
    "AT"
    "SIZE"                        ; size operator
    "FROM"                        ; array selector keyword
    "SKIP" "STOP"                 ; special processes
    )
  "*OCCAM reserved words (will be capitalized)")

(defvar occam-mode-syntax-table nil
  "Syntax table in use in OCCAM mode buffers")
(if occam-mode-syntax-table
    ()
  (setq occam-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?. "w" occam-mode-syntax-table)
  (modify-syntax-entry ?+ "." occam-mode-syntax-table)
  (modify-syntax-entry ?- "." occam-mode-syntax-table)
  (modify-syntax-entry ?* "." occam-mode-syntax-table)
  (modify-syntax-entry ?/ "." occam-mode-syntax-table)
  (modify-syntax-entry ?\: "." occam-mode-syntax-table)
  (modify-syntax-entry ?\? "." occam-mode-syntax-table)
  (modify-syntax-entry ?\! "." occam-mode-syntax-table)
  (modify-syntax-entry ?\r ">" occam-mode-syntax-table)
  (modify-syntax-entry ?- ".12" occam-mode-syntax-table))

(defvar occam-mode-map ()
  "Keymap used in OCCAM mode")
(if occam-mode-map
    ()
  (setq occam-mode-map (make-sparse-keymap))
  (define-key occam-mode-map " " 'uppercase-occam-keyword)
  (define-key occam-mode-map "\r" 'occam-indent-newline)
  (define-key occam-mode-map "\177" 'backward-delete-unindent))

(defun occam-mode ()
  "Major mode for editing OCCAM programs.
TAB and CR automatically indents.
All OCCAM keywords (which are separated into process keywords which force 
indentation and reserved words) are recognized and uppercase'd.

Variables and constants controlling case change:
    occam-indentation :      indentation, default 2
    occam-process-keywords : list of process keywords
    occam-reserved-words :   list of reserved words

The value of the variable occam-mode-hook (must be a function name) is called 
with no arguments prior to entering  OCCAM mode if the value of that variable
is non-nil"
  (interactive)
  (kill-all-local-variables)
  (use-local-map occam-mode-map)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'occam-indent-line)
  (setq mode-name "OCCAM")
  (setq major-mode 'occam-mode)
  (run-hooks 'occam-mode-hook))

(defun occam-indent-line ()
  "Indents current OCCAM line"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((p (point)))
      (indent-to (calculate-occam-indent))
      (while (looking-at "[ \t]")
	(delete-char 1))
      (untabify p (point)))))

(defun calculate-occam-indent ()
  "calculate indentation for current OCCAM line"
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (while (and (looking-at "[ \t]*$") (not (bobp)))
      (beginning-of-line 0))
    (if (looking-at "[ \t]*$")
	0
      (progn
	(skip-chars-forward " \t")
	(let ((col (current-column)))
	  (forward-word 1)
	  (if (occam-keyword occam-process-keywords)
	      (+ col occam-indent)
	    col))))))

(defun uppercase-occam-keyword ()
  "check if last word was an OCCAM keyword"
  (interactive)
  (occam-keyword (append occam-process-keywords occam-reserved-words))
  (insert " "))

(defun occam-indent-newline ()
  "Indent new line to current indentation unless previous line contained 
a process keyword"
  (interactive)
  (save-excursion
    (let ((eol (point)))
      (beginning-of-line)
      (if (looking-at "[ \t]*$")
	  (delete-region (point) eol)
	())))
  (occam-keyword (append occam-process-keywords occam-reserved-words))
  (newline)
  (occam-indent-line)
  (end-of-line))

(defun backward-delete-unindent ()
  "Delete and unindent"
  (interactive)
  (let ((p (point)))
    (skip-chars-backward " \t" (- p (current-column)))
    (if (bolp)
	(progn
	  (goto-char p)
	  (if (bolp)
	      (delete-char -1)
	    (delete-char (- occam-indent))))
      (progn
	(goto-char p)
	(delete-char -1)))))

(defun occam-keyword (keywords)
  "upcase current word and if OCCAM keyword, t if keyword"
  (save-excursion
    (let ((eow (point)))
      (forward-word -1)
      (let ((bow (point)))
	(if (re-search-backward "\s<" (- bow (current-column)) t)
	    nil
	  (if (word-in-list (upcase (buffer-substring bow eow))
			    keywords)
	      (not (upcase-region bow eow))
	    nil))))))

(defun word-in-list (word words)
  "t if word occurs in words, nil otherwise"
  (if (null words)
      nil
    (if (string-equal word (car words))
	t
      (word-in-list word (cdr words)))))


