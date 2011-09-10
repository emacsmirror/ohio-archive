;From: quiroz@cs.rochester.edu (Cesar Quiroz)
;Newsgroups: comp.emacs
;Subject: Re: Looking for an insert at column function
;Message-ID: <1989Jun17.230717.28962@cs.rochester.edu>
;Date: 22 Jul 89 03:07:17 GMT
;Reply-To: quiroz@cs.rochester.edu (Cesar Quiroz)
;Organization: U of Rochester, Dept. of Computer Science, Rochester, NY 14627
;Lines: 345
;Summary: provided here function goto-column
;
;At the end of this posting is a program (generic-code-mode) I use
;whenever dealing with roughly block structured files (like shell
;scripts, awk programs, etc.), for which there is no specific mode
;that knows their syntax.  The General Public License applies, as
;usual.  I haven't done much work on it for a while, but I use it
;frequently.  I don't expect bugs, but I am not happy with the fake
;unix-script-mode.  So I am posting it in `pre-release' condition,
;for criticism and improvement from the user community.
;
;Frank: The function you need can be synthesized from `goto-column'.
;(I think you can separate goto-column from the rest, you may not
;need the bulk of generic-code-mode for this application).  Suppose
;you need to insert a string at column N (remember it is 0-based) _in
;the current line_, you would just go there and insert.  So, if you
;need to navigate by lines and columns, I would use something like
;this (vastly untested, careful):
;
;;;; Bug: if line LINE does not exist, you get to the end of the
;;;; buffer.  Exercise for the reader: using count-lines or its
;;;; friends, first figure out if the buffer needs to be stretched.
;;;; Another exercise:  figure out a reasonable interactive setting.
;(defun go-and-insert (string column &optional line)
;  "Drop STRING at COLUMN of LINE.  Omitting LINE defaults to the current line,
;the line point is on.  LINE counts from 1 at the top of the buffer, COLUMN
;counts from 0 at the left edge.  Point is left at the end of the inserted
;string."
;  ;; you wouldn't want this to be (interactive), now would you?
;  (if line (goto-line line))
;  (goto-column column)
;  (insert string))
;
;The peculiar argument order is justified by the desire to make the
;line number optional, of course.  The function you need requires
;more work (has to drop the string more than once, etc.), but it
;shouldn't be hard to modify go-and-insert to accomplish it.
;
;Cesar
;; Editing generic block structured files
;; Pre-release. 
;; Copyright (C) 1989 Free Software Foundation, Inc.

;; Although this file is not yet part of GNU Emacs, it is
;; distributed in the same spirit.

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

;;; Generic code mode for EMACS.
;;;
;;; Integrated more stuff.  Point-to-tab-stop lets you move point
;;; without adding whitespace.  Goto-column lets you go to a column of
;;; your choice, even when the line is shorter than that.  CQ, 14 sep 88.
;;;
;;; This library lets the user deal with editing inputs that look
;;; block structured and have optional comments.  It cannot be too
;;; bright, as there is little or no indication of the intended
;;; syntax, but the user can indicate his preferences via syntactic
;;; classes, abbrevs and such.  CQ, 30 sep 87.

(provide 'generic-code-mode)
(require 'cl)

(defvar generic-code-mode-hook nil)
(defvar generic-code-mode-syntax-table nil)
(defvar generic-code-mode-abbrev-table nil)
(defvar generic-code-mode-map nil)

(define-abbrev-table 'generic-code-mode-abbrev-table ())

(cond ((null generic-code-mode-map)
       (setq generic-code-mode-map (copy-keymap indented-text-mode-map))
       (define-key generic-code-mode-map "\M-o"    'indent-to-point)
       (define-key generic-code-mode-map "\C-i"    'indent-to-tab-stop)
       (define-key generic-code-mode-map "\M-\C-i" 'tab-to-tab-stop)
       (define-key generic-code-mode-map "\M-i"    'point-to-tab-stop)
       (define-key generic-code-mode-map "\C-x|"   'goto-column)
       ))

(defun generic-code-mode ()
  "Sets generic code mode, essentially indented-text mode
with no fill.  TAB is rebound to run indent-to-tab-stop (q.v.),
which is used to advance or retreat the indentation of the 
current line.  See the function `set-scripts-syntax' for a simple
customization useful when editing shell scripts, awk programs or icon
programs, and the function `unix-script-mode' for a simple way to use
it.  See `right-adjust-line' too. 
\\{generic-code-mode-map}"
  (interactive)
  (indented-text-mode)
  ;; the variables specific to generic code mode are made buffer-local
  ;; in order to preserve the possibility of editing simultaneously
  ;; with slightly different `less-generic' modes based on this one.
  (make-local-variable 'generic-code-mode-map)
  (use-local-map generic-code-mode-map)
  (auto-fill-mode -1)
  (setq major-mode 'generic-code-mode)
  (setq mode-name "Code")
  (make-local-variable 'generic-code-mode-syntax-table)
  (setq generic-code-mode-syntax-table (make-syntax-table))
  (make-local-variable 'generic-code-mode-syntax-table)
  (set-syntax-table generic-code-mode-syntax-table)
  (make-local-variable 'generic-code-mode-abbrev-table)
  (setq local-abbrev-table generic-code-mode-abbrev-table)
  ;;
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-indent-hook)
  ;;
  (run-hooks 'generic-code-mode-hook))

(defun indent-to-tab-stop (n)
  "Advance or retreat the indentation of this line.
A positive argument (interactive default) advances,
a negative argument retreats,
a zero argument aligns with the previous nonblank line.
The number of tab stops actually moved is the absolute value of the
argument.
Just using \C-u as prefix means the same as -1."
  (interactive "p")
  ;; Special case suggested by Neil:  Just giving
  ;; `C-u' means `-', instead of `M- 4'.
  (when (consp current-prefix-arg)      ;Any number of `C-u's
    (setq n -1))
  (let ((here (point-marker))
        (do-not-return nil))
    (back-to-indentation)
    (setq do-not-return (looking-at "[ \t]*$"))
    (multiple-value-bind
        (prev-tab next-tab)
        (find-neighboring-tabs (current-column))
      ;; Depending on sign of n, move forward or backward
      (cond ((> n 0)
             (indent-to-column next-tab)
             (when (> (decf n) 0)
               (indent-to-tab-stop n)))
            ((= n 0)
             (delete-horizontal-space)
             (indent-relative-maybe))
            ((< n 0)
             (delete-horizontal-space)
             (indent-to-column prev-tab)
             (when (< (incf n) 0)
               (indent-to-tab-stop n)))))
    ;; go back to the correct relative position
    (unless do-not-return
      (goto-char (marker-position here)))))

(defun point-to-tab-stop (n)
  "Advance or retreat the current-column to a near tab stop.  Just
move the cursor, don't add nor delete anything.  Compare with
tab-to-tab-stop, that pushes things to the right.
A positive argument (interactive default) advances, 
a negative argument retreats.
The number of tab stops actually moved is the absolute value of the
argument.  Giving it \C-u means `-', not 4."
  (interactive "p")
  ;; Special case suggested by Neil:  Just giving
  ;; `C-u' means `-', instead of `M- 4'.
  (when (consp current-prefix-arg)      ;Any number of `C-u's
    (setq n -1))
  (multiple-value-bind
      (prev-tab next-tab)
      (find-neighboring-tabs (current-column))
    ;; Depending on sign of n, move forward or backward
    (cond ((> n 0)
           (goto-column next-tab)
           (when (> (decf n) 0)
             (point-to-tab-stop n)))
          ((< n 0)
           (goto-column prev-tab)
           (when (< (incf n) 0)
             (point-to-tab-stop n))))))

(defun find-neighboring-tabs (column)
  "Return previous and next tab positions, when at position COLUMN.
The values returned are chosen from tab-stop-list (if not null) or
computed as exact multiples of tab-width.  If COLUMN is outside the
range of tab-stop-list, either 0 or (+ Column Tab-Width) are used as
the missing bounds."
  (cond  ((null tab-stop-list)          ; have to use multiples of tab-width
          (cond ((= (mod column tab-width) 0) ;exact match
                 (values (max 0 (- column tab-width)) (+ column tab-width)))
                (t                      ;in between matches
                 (let* ((prev (floor column tab-width))
                        (next (+ prev tab-width)))
                   (values prev next)))))
         (t                             ;there is a tab-stop-list
          (do* ((tabs                   ;add special terminator
                 (append tab-stop-list (list (+ column tab-width)))
                 (cdr tabs))
                (past 0 this)
                (this (car tabs) (car tabs))
                (next)                  ;used to store the values
                (prev)                  ; to return.
                (done nil))
              (done (values prev next))
            ;; we compare the current column against each tab stop
            ;; (the tab-stop-list being augmented by a catch-all
            ;; terminator) until the current column either matches one
            ;; of the tab stops or is precisely contained between two
            ;; of them.
            (cond ((= column this)
                   ;; column matches a tab-stop=>return surrounding ones.
                   (setf prev past      ;need to protect value of past!
                         next (cadr tabs) ;cadr won't ever fail here
                         done t))
                  ((and (>= column past)
                        (<  column this))
                   ;; column is in between past and this
                   (setf prev past      ;need to protect value of past!
                         next this
                         done t))
                  (t
                   ;; no need to do anything, do* will step for us
                   ))))))

(defun indent-to-point (here)
  "Move indentation of this line to the column of point.
Used to force indentation to a given spot.  When called from a
program, give it a character position.  The line that contains that
position will be indented to the column of that position in it."
  (interactive "d")
  ;; Make sure you are really HERE (in the line containing HERE)
  (goto-char here)
  (let ((indentation (current-column)))
    (beginning-of-line)
    (delete-horizontal-space)
    (indent-to-column indentation)))

;;; Customizations of code mode

(defun unix-script-mode ()
  "This function sets up generic-code-mode with the syntax for scripts
that is provided by set-scripts-syntax.  It is not a real major mode,
so it appears to be generic-code-mode for all intents and purposes."
  (interactive)
  (generic-code-mode)
  (set-scripts-syntax))

(defun set-scripts-syntax ()
  "Establish # as a comment character, etc...  Modifies
generic-code-mode such that it is useful for scripts to
be fed to sh, csh, awk, icont and the like."
  (interactive)
  ;; special commenting conventions
  (modify-syntax-entry ?# "<")
  (modify-syntax-entry ?\n ">")
  (modify-syntax-entry ?\f ">")
  (setq comment-start "#")
  (setq comment-end "")
  (setq comment-start-skip "#+ *")
  (setq comment-indent-hook 'generic-code-hash-comment-indent))

(defun generic-code-hash-comment-indent ()
  "Indent comments that begin with at least one #.
Comments at the beginning of a line or that begin with more than one
hash are left alone.  Move them around with indent-to-tab-stop, if you
must."
  (if (or (bolp) (looking-at "##"))
      (current-column)
    (skip-chars-backward " \t")
    (max (if (bolp) 0 (1+ (current-column)))
         comment-column)))

;;; This routine provides absolute motions in a line, with possible
;;; extension of the line if so requested.  Note that the standard
;;; `move-to-column' stops at line end.  I bind this usually to `C-x |'.

(defun goto-column (n)
  "Go to column N in this line, extend line at end with spaces if
needed.  Interactively, take the prefix argument as a column number or
query in the minibuffer if none given."
  (interactive "p")
  (if (and (interactive-p) (null current-prefix-arg))
      (setq n (call-interactively
               '(lambda (n) (interactive "nColumn to go to? ") n))))
  (if (< n 0)
      (error "Negative column `%d' given to goto-column." n))
  (let* ((line-length (save-excursion (end-of-line)
                                      (current-column)))
        (difference   (- n line-length)))
    (cond ((<= difference 0)
           (move-to-column n))
          (t
           (end-of-line)
           (while (> difference 0)
             (insert " ")
             (setq difference (- difference 1)))))))

(defun right-adjust-line ()
  "Have the current line end in a non-whitespace character aligned to
the rightmost position.  Rightmost here means 'at the fill-column or
at window end'.  Sort of complements any usage of `center-line' and
the usage of `delete-horizontal-space' at the beginning of a line."
  (interactive)
  (beginning-of-line)
  (let ((had-fill-prefix    (and fill-prefix (looking-at fill-prefix)))
        (fill-prefix-length (length fill-prefix)))        
    (if had-fill-prefix
        (delete-char fill-prefix-length))
    ;; trim horizontal space around the remaining text
    (delete-horizontal-space)
    (end-of-line)
    (delete-horizontal-space)
    ;; ASSERTION: Cursor is now at end of line
    (let* ((text-length  (current-column))
           (line-length  (if fill-column
                             fill-column
                           (window-width)))
           (slack        (- line-length text-length)))
      (beginning-of-line)
      (if (> slack 0)
          (indent-to-column slack))
      ;; restore fill-prefix, if removed earlier
      (cond  (had-fill-prefix
              (beginning-of-line)
              (cond ((> slack fill-prefix-length)
                     (delete-char fill-prefix-length)
                     (insert fill-prefix))
                    (t                  ;give up on this
                     (delete-char slack)
                     (insert fill-prefix)))))
      ;; have to put the cursor somewhere...
      (end-of-line))))

;;; end of generic-code-mode.el

;-- 
;                                      Cesar Augusto Quiroz Gonzalez
;                                      Department of Computer Science
;                                      University of Rochester
;                                      Rochester,  NY 14627
