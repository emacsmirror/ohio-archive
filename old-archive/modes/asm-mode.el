;To: unix-emacs@bbn.com
;Date: 3 Jan 89 18:20:36 GMT
;From: Martin Neitzel <mcvax!unido!infbs!neitzel@uunet.uu.net>
;Subject: asm-mode, incl. nice comment placements.
;
;
;I just hacked up this little asm-mode. ("Oh no!").  Even if you throw
;the asm-mode itself far away, it might be worthwhile to rip off the
;function asm-comment, which handles general eol-comments in a nice
;way.  For example, you could use it for Ada, Icon, and Lisp too.
;A comment request (triggered by, say, ";") will try to figure out the
;appropriate kind of comment based on the current context.  If this
;isn't the kind you want, simply hit ";" again: e.g., a side comment
;will be promoted to a comment on its own line indented like code and
;so on.  I like it, but your mileage may vary...
;
;Uhhhmmm, yes, I don't know how to elisp...
;
;							Martin
;---snip---
;; Asm-mode, and its related commands.
;; Cloned from text-mode, and therefore the usual prelude is inevitable:

;; Copyright (C) 1985, 1989 Free Software Foundation, Inc.

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


(defvar asm-mode-syntax-table nil
  "Syntax table used while in asm mode.")

(defvar asm-mode-abbrev-table nil
  "Abbrev table used while in asm mode.")
(define-abbrev-table 'asm-mode-abbrev-table ())

(if asm-mode-syntax-table
    ()
  (setq asm-mode-syntax-table (make-syntax-table))
  (set-syntax-table asm-mode-syntax-table)
  (modify-syntax-entry ?;	"<   " asm-mode-syntax-table)
  (modify-syntax-entry ?\n	">   " asm-mode-syntax-table))

(defvar asm-mode-map nil "")
(if asm-mode-map
    ()
  (setq asm-mode-map (make-sparse-keymap))
  (define-key asm-mode-map "\t" 'tab-to-tab-stop)
  (define-key asm-mode-map ":" 'asm-colon)
  (define-key asm-mode-map ";" 'asm-comment)
  (define-key asm-mode-map "\C-c\C-c" 'compile)
  (define-key asm-mode-map "\C-c\C-n" 'next-error)
  )


(defun asm-mode ()
  "
Major mode for editing typical assembler code \(gack!\).  Features:

own asm-mode-abbrev-table
\\[asm-colon]\toutdents labels,
\\[asm-comment]\tmakes placement of comments a fun game.

Turning on asm-mode calls the value of the variable asm-mode-hook,
if that value is non-nil.  You probably want to redefine the
variables comment-start and comment-start-skip there.

Special commands:\\{asm-mode-map}
"

  (interactive)
  (kill-all-local-variables)
  (use-local-map asm-mode-map)
  (setq mode-name "Asm")
  (setq major-mode 'asm-mode)
  (setq local-abbrev-table asm-mode-abbrev-table)
  (set-syntax-table asm-mode-syntax-table)
  (make-local-variable 'comment-start)
  (setq comment-start "; ")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-column)
  (setq comment-column 32)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip ";+[ \t]*")
  ; (make-local-variable 'comment-indent-hook)
  ; (setq comment-indent-hook 'c-comment-indent)
  (auto-fill-mode 1)
  (run-hooks 'asm-mode-hook))


(defun asm-colon ()
  "Insert a colon, and delete the indentation iff it follows a label."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (looking-at "[ \t]+\\(\\sw\\|\\s_\\)+$")
	(delete-horizontal-space)))
  (insert ":"))

(defun asm-comment ()
  "Introduce a comment or convert an already existing comment into a
comment of a `bigger' kind.  These are the known comment classes:

	1-- to the right of the code (at the comment-column)
	2-- comment on a own line, indented like code
	3-- at the left-most column.

Suggested usage:  while writing your code, trigger asm-comment
repeatedly until you are satisfied with the kind of comment."

  (interactive)
  ;; on a blank line, do nothing nothing special
  (if (eq (current-column) (current-indentation))
      (insert comment-start)
    ;; There is already something on the line.
    ;; If we NOT already have a comment, indent for a new one.
    (beginning-of-line)
    (if (not (looking-at (concat ".*" (regexp-quote comment-start))))
	(indent-for-comment)
      ;; There is a comment, convert it to a `bigger' class.
      (end-of-line)
      (search-backward comment-start)
      (delete-horizontal-space)
      (if (= (current-column) (current-indentation))
	  () ;; was code level, --> col-1-comment
	;; was right to code
	(newline-and-indent))
      (end-of-line))))
;--
;Martin Neitzel,  Techn. Univ. Braunschweig, W.Germany
;BITNET/EARN:	neitzel@dbsinf6.bitnet	  (mail via bitnet preferred)
;UUCP:		neitzel@infbs.uucp  (unido!infbs!neitzel)

