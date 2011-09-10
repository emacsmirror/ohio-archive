;; 1char.el -- commands to fix typos in the previous word with minimal typing.
;; Copyright (C) Bard Bloom, July 1989

;; This file is not yet part of GNU Emacs.

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

;; Summary:
;; This is a bunch of commands to make minor changes in the previous word.
;; It's intended for fast but inaccurate typists, like me.
;; 
;; They only operate on that word (and sometimes the word before).
;; There's nothing you couldn't do by moving back, typing a character
;; or three, and moving forward --- but it does save a bunch of keystrokes.
;; Might improve your typing speed by a word or two per day, even.
;;
;; I put the commands under c-h because they really want to be on 
;; two-key commands you can type without moving your fingers from
;; the keyboard.
;;
;; Positions in the previous word are specified by a single character,
;; called a 1char.
;; If it's a letter, then it refers to the last occurrance of that letter
;; in the word:   pepper  
;;                   ^    p means this p
;;                    ^   e means this e
;; If it's a  digit 1-9, then it refers to that number from the right end of the 
;; word:          pepper
;;                654321
;; And 0 refers to the first character of the word.
;;  
;;
;; Commands:
;;   c-h c-t: transpose the 1char with the previous character
;;   c-h c-d: delete the 1char
;;   c-h c-i: insert a character before the 1char
;;   c-h a:   insert a character after the 1char
;;   c-h c-c: change the 1char to another character
;;   c-h c-b: break the word before the 1char.  "ofthe" and t  --> "of the"
;;   c-h c-m: put the cursor on the 1char.
;;   c-h c-u: undo (without moving point)
;;   c-h c-l: recenter screen so that this paragraph is on top.
;;   c-h c-j: join the previous two words.
;;   c-h c-x: undo

;; Abbreviations:
;; If the variable 1char-expands-abbrevs is set to true
;; then all these commands try to expand the word(s) they 
;; make as abbrevs.  I have a bunch of abbrevs for my common
;; typos --- `fo' is an abbrev for `of' --- so 
;; typing `fothe', c-h c-b t will result in `of the' which is 
;; probably what I intended.

;; Buggigestions to bard@theory.lcs.mit.edu

(require 'cl)

(unless (fboundp 'point-after)
  (defmacro point-after (&rest commands)
  "Returns the value of point after executing the COMMANDS.  Doesn't move
point.  (Expands to (save-excursion COMMANDS (point)))."
  (` (save-excursion
       (,@ commands)
       point))))

(defun recenter-top-para ()
  "Put the top of this paragraph on the top of the screen.  Don't move point."
  (interactive)
  (save-excursion
    (backward-paragraph 1)
    (next-line 1)
    (recenter 0)
    ))

(defvar 1char-expands-abbrevs t
  "If true, then the various 1char functions expand abbrevs everywhere
appropriate.")
(make-variable-buffer-local '1char-expands-abbrevs)


(defun join-words ()
  "Join two previous words. Expands them as an abbrev if 
1char-expands-abbrevs is true. "
  (interactive)
  (save-excursion
    (backward-word 1)
    (delete-horizontal-space)
    (forward-word 1)
    (if 1char-expands-abbrevs (expand-abbrev))))

(defvar 1char-at-end-of-word-internal nil
  "Don't change this.  True inside the about-the-previous-word
 macro, intended to be false elsewhere.")

(defmacro about-the-previous-word (&rest code)
  (let ((p (gensym)))
    (`
     (let (((, p) (point-marker))
           (1char-at-end-of-word-internal t)
           )
       ;; I don't know why save-excursion screws up
       (save-restriction
         (backward-word 1)
         (narrow-to-region (1- (point))
                           (progn (forward-word 1) (point)))
         (,@ code)
         cond
         (1char-expands-abbrevs
          (goto-char (point-max))
          (expand-abbrev))
         )
       (goto-char (, p))))))


(defun 1char-goto (letter)
  "If letter is a digit 1-9, go back (letter - 1) characters.  
0 is (anomalously) the first    character of the word.
If letter is not a digit, go to the nearest previous such thing, bitching if 
there isn't one."
  (interactive "cGoto (1char): ")
  (cond
   (1char-at-end-of-word-internal
    (backward-word 1)
    (forward-word 1))
   ((featurep 'positions)
    ;; we only get here if it's called interactively 
    (stack-save-current-pos)))
  (cond
   ((and (<= letter ?9) (>= letter ?1))
    (backward-char (- letter ?0))
    )
   ((= letter ?0)
    (backward-word 1))
   ((search-backward (char-to-string letter)
                     (point-after (backward-word 1))
                     't))
   (t
    (error "I don't see a `%c' in this word. Do you see a `%c'?" letter letter))))

(defun 1char-break-word (c)
  "Break the previous word just before a 1char-specified position.
See the documentation of 1char-goto for details."
  (interactive "cBreak before (1char): ")
  (about-the-previous-word
   (1char-goto c)
   (insert " ")
   (backward-char 1)
   (if 1char-expands-abbrevs (expand-abbrev))))

(defun 1char-transpose-chars (letter)
  "Transpose the character given by a 1char-specified position 
and the previous character."
  (interactive "cTwiddle (1char): ")
  (about-the-previous-word
   (1char-goto letter)
   (transpose-chars nil)))

(defun 1char-change (from to)
  "Change the character given by a 1char to another character."
  (interactive "cFrom (1char): \ncTo: ")
  (about-the-previous-word
   (1char-goto from)
   (delete-char 1)
   (insert to)))


(defun 1char-delete (c)
  "Delete the character given by a 1char."
  (interactive "cDelete (1char): ")
  (about-the-previous-word
   (1char-goto c)
   (delete-char 1)))

(defun 1char-insert-before (c new)
  "Insert a character just before the 1char C.  NEW is the new
character.  Expands abbrevs according to 1char-expands-abbrevs."
  (interactive "cBefore (1char): \ncChar to insert: ")
  (about-the-previous-word
   (1char-goto c)
   (insert new)))


(defun 1char-insert-after (c new)
  "Insert a character just after the 1char C.  NEW is the new
character.  Expands abbrevs according to 1char-expands-abbrevs."
  (interactive "cBefore (1char): \ncChar to insert: ")
  (about-the-previous-word
   (1char-goto c)
   (forward-char 1)
   (insert new)))


(defun undo-without-moving ()
  "Undo one thing without moving point."
  (interactive)
  (let ((p (point-marker)))
    (undo)
    (goto-char p)))


(global-set-key "\C-h\C-j" 'join-words)
(global-set-key "\C-h\C-x" 'undo)
(global-set-key "\C-h\C-u" 'undo-without-moving)
(global-set-key "\C-h\C-l" 'recenter-top-para)
(global-set-key "\C-h\C-t" '1char-transpose-chars)
(global-set-key "\C-h\C-d" '1char-delete)
(global-set-key "\C-ha"    '1char-insert-after)
(global-set-key "\C-h\C-i" '1char-insert-before)
(global-set-key "\C-h\C-c" '1char-change)
(global-set-key "\C-h\C-b" '1char-break-word)
(global-set-key "\C-h\C-m" '1char-goto)
