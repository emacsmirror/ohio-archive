;; 1char.el -- commands to fix typos in the previous word with minimal typing.
;; Copyright (C) Bard Bloom, July 1989; revised July 1990.

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

;; Changes since 7/89:
;;
;; Commands to undo the last 1char command, and redo it in a different place.
;; Case toggle 1char
;; Delete from point through 1char.
;;

;; Summary:
;; This is a bunch of commands to make minor changes in recently-typed 
;; text.  It's intended for fast but inaccurate typists, like me.
;; 
;; There's nothing you couldn't do by moving back, typing a character
;; or three, and moving forward --- but it does save a bunch of keystrokes.
;; Might improve your typing speed by a word or two per day, even.
;;
;; I put the commands under c-h because they really want to be on 
;; two-key commands you can type without moving your fingers from
;; the keyboard.
;;
;; Positions in the previous word are specified by a short sequence of 
;; characters, called a 1char.
;; If it's a letter, then it refers to the last occurrance of that letter
;; in the word:   pepper  
;;                   ^    p means this p
;;                    ^   e means this e
;;
;; Most other characters behave the same way.  The main exceptions are (now):
;;
;;   
;;   = and c-q: quotes the next character, so that it is taken literally (i.e., acts
;;        like a letter). This is most useful for digits, -, and =.
;;   -: negates the number you are about to type. Negative numbers go forwards rather 
;;        than backwards.  0 is treated as 1, unless 1char guesses that you really 
;;        wanted -1.  
;;   digits: Digits ask you for another character.  `1 c' goes to the first 
;;        previous occurrance of c (just like c itself).  `2 c' goes to the one
;;        before that, and so on.
;;
;;        pepper
;;           ^   1 p
;;          ^    2 p
;;        ^      3 p
;;
;; Lots of these commands take a prefix argument telling how many words back
;; to go.  Furthermore, if the previous word isn't at least 18 (or whatever
;; value 1char-min-distance has) chars long, you get to work on the previous
;; 18 (or whatever) characters.
;;
;; Commands:
;;   c-h c-t: transpose the 1char with the previous character  (P)
;;   c-h c-d: delete the 1char                                 (P)
;;   c-h c-i: insert a character before the 1char              (P)
;;   c-h c-a: insert a character after the 1char               (P)
;;   c-h c-c: change the 1char to another character            (P)
;;   c-h c-b: break the word before the 1char.  "ofthe" and t  --> "of the"
;;   c-h c-m: put the cursor on the 1char.                     (P)
;;   c-h c-u: undo (without moving point)
;;   c-h c-l: recenter screen so that this paragraph is on top.
;;   c-h c-j: join the previous two words.
;;   c-h ~  : toggle the case of the 1char.                    (P)
;;   c-h c-x: undo
;;
;;   c-h c-k c-d: delete text from point to 1char              (P)
;;   c-h c-k c-k: delete forward from point to 1char           (P)
;;                (like c-h c-k c-d but the 1char is interpreted negatively)

;; It's easy to get the wrong occurrance of a letter.  There are commands which undo
;; the last 1char command you typed and redo it one 1char to the left or right.
;; So, if the buffer had
;;
;; eggplant is extremely toxic
;;                       #
;; and you wanted to capitalize the e in `extremely' with c-h ~ 2 e, you'd get
;;
;; eggplant is extrEmely toxic
;;                       #
;;
;; which is wrong.  You could fix it with c-h c-n, getting
;;
;; eggplant is Extremely toxic
;;                       #

;; The commands marked (P) take a prefix argument telling how many words
;; back to go.  
;;
;; abbreviations:
;; if the variable 1char-expands-abbrevs is set to true
;; then all these commands try to expand the word(s) they 
;; make as abbrevs.  i have a bunch of abbrevs for my common
;; typos --- `fo' is an abbrev for `of' --- so 
;; typing `fothe', c-h c-b t will result in `of the' which is 
;; probably what i intended.


;; buggigestions to bard@cs.cornell.edu

(require 'cl)

(unless (fboundp 'f:l)
  (defmacro f:l (x &rest y)
    "(function (lambda X) Y).  Abbreviation taken from some obscure dialect
of Lisp, but I remembered it seven years after I read the manual, so it
can't be all that obscure, can it?"
    (list 'function
          (append (list 'lambda x ) y))))

(unless (fboundp 'point-after)
  (defmacro point-after (&rest commands)
  "returns the value of point after executing the commands.  doesn't move
point.  (expands to (save-excursion commands (point)))."
  (` (save-excursion
       (,@ commands)
       point))))

(defun recenter-top-para ()
  "put the top of this paragraph on the top of the screen.  don't move point."
  (interactive)
  (save-excursion
    (backward-paragraph 1)
    (next-line 1)
    (recenter 0)
    ))

(defvar 1char-expands-abbrevs t
  "if true, then the various 1char functions expand abbrevs everywhere
appropriate.")
(make-variable-buffer-local '1char-expands-abbrevs)


(defun join-words (p)
  "join two previous words. expands them as an abbrev if 
1char-expands-abbrevs is true. "
  (interactive "p")
  (save-excursion
    (backward-word (1- p))
    (backward-word 1)
    (setq 1char-marker (point-marker))
    (setq 1char-undoer (f:l () (goto-char 1char-marker) (insert " ")))
    (setq 1char-last-prefix-arg p)
    (setq 1char-last-called-function (f:l (p oc) (join-words p)))
    (delete-horizontal-space)
    (forward-word 1)
    (if 1char-expands-abbrevs (expand-abbrev))))

(defvar 1char-at-end-of-word-internal nil
  "don't change this.  true inside the about-the-previous-word
 macro, intended to be false elsewhere.")

(defmacro about-the-previous-word (prefix &rest code)
  (let ((p (gensym)))
    (`
     (let (((, p) (point-marker))
           (1char-at-end-of-word-internal t)
           )
       ;; i don't know why save-excursion screws up
       (save-restriction
         (let ((a (point)))
           (backward-word (prefix-numeric-value (, prefix)))
           ;; We do want the side effects in the following progns:
           (if (> (+ (point) 1char-min-distance) a)
               (narrow-to-region (max (point-min) (- a 1char-min-distance))
                                 (progn (forward-word 1) (point)))
             (narrow-to-region (1- (point))
                               (progn (forward-word 1) (point))))
           (goto-char (point-max))
           (,@ code)
           cond
           (1char-expands-abbrevs
            (goto-char (point-max))
            (expand-abbrev))
           )
         )
       (goto-char (, p))))))

(defvar 1char-min-distance 80
  "The `previous word' for 1chars extends at least this many characters back.")

;; a 1char is now:
;; EXTERNALLY:
;;    - most chars: the first previous occurrance of that char.
;;    - digit N: get another char, C; give the N'th previous occurrance of C.
;;    - digit 0: read a number in the minibuffer
;;    - c-a (for future work): give a little minibuffer window to pick
;;      the place to work interactively.
;; INTERNALLY
;;    '(prev c n) -- c=char, n=count.  n<0 means go forward

(defun 1char-to-char (oc)
  "Converts a 1char OC to a char c.  So, `3N' would convert to `N'."
  (second oc))

(defun 1char-read (prompt)
  (message prompt)
  (let ((y "")
        (x (read-char)))
    (when (eq x ?-)
      (setq y "-")
      (setq x (read-char)))
    (while (memq x '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
      (setq y (concat y (char-to-string x)))
      (message "%s -" y)
      (setq x (read-char))
      )
    (cond
     ((string= y "") (setq y 1))
     ((string= y "-") (setq y -1))
     (t (setq y (string-to-int y))))
    (setq 1char-last-1char
          (case x
            (?\C-a 1char-last-1char)
            ((?\C-q ?=)
             (message "%s (c-q):")
             (list 'prev (read-char) y))
            (otherwise
             (list 'prev x y))))
    1char-last-1char))


(defmacro 1char-excursion (p oc &rest code)
  (let ((v (gensym)))
    (`
     (save-excursion
       (1char-goto-internal p (, oc))
       (,@ code)
       ))))

(put '1char-excursion 'lisp-indent-hook 2)

(defun 1char-goto-internal (p oc)
  "Go to the position given by the 1char OC.
Takes a prefix arg to tell it how many words back to go."
  (backward-word
   (1- (prefix-numeric-value p)))
  (let ((n 1)
        (c nil)
        )
    (cond
     ((eq (car oc) 'prev)
      (setq c (cadr oc)
            n (caddr oc))))
    (when (= n 0)
      (setq n 1))
    (cond
     ((null c)
      (error "Internal representation of 1char is screwed up."))
     ((and
       (> n 0)
       (search-backward (char-to-string c)
                        (point-min)
                        't
                        n)))
     ((and
       (< n 0)
       (search-forward (char-to-string c)
                       (point-max)
                       't
                       (- n)))
      (goto-char (match-beginning 0))
      )
     (t
      (error "I can't find a %c." c)))))
  

(defun 1char-goto (p oc)
  (interactive
   (list current-prefix-arg
         (1char-read "Goto (1char):")))
  (when (featurep 'positions)
    (stack-save-current-pos))
  (setq 1char-last-prefix-arg p
        1char-goto-last-position (point-marker)
        1char-last-1char oc
        1char-undoer
          (f:l () )
        1char-last-called-function
          (f:l (p oc) (goto-char 1char-goto-last-position) (1char-goto p oc)))
  (1char-goto-internal p oc))


(defun 1char-break-word (p oc)
  "break the previous word just before a 1char-specified position.
see the documentation of 1char-goto for details."
  (interactive
   (list
    current-prefix-arg
    (1char-read "Break before (1char):")))
  (1char-excursion p oc
    (setq 1char-marker (point-marker))
    (insert-before-markers " ")
    (backward-char 1)
    (if 1char-expands-abbrevs (expand-abbrev))
    (1char-maybe-expand-abbrevs)
    )
  (setq 1char-last-prefix-arg p
        1char-last-1char oc
        1char-undoer
           (f:l ()
             (delete-region (1- 1char-marker) 1char-marker ))
        1char-last-called-function
          (function 1char-break-word))
  )

(defun 1char-maybe-expand-abbrevs ()
  (when 1char-expands-abbrevs
    (unless (looking-at "\\>") (forward-word 1))
    (expand-abbrev)))

(defun 1char-transpose-chars (p oc)
  "transpose the character given by a 1char-specified position 
and the previous character."
  (interactive
   (list
    current-prefix-arg
    (1char-read "Twiddlebefore (1char):")))
  (1char-excursion p oc
    (transpose-chars nil)
    (setq 1char-marker (point-marker))
    (1char-maybe-expand-abbrevs)
    )
  (setq 1char-last-prefix-arg p
        1char-last-1char oc
        1char-undoer
          (f:l ()
            (goto-char (1- 1char-marker))
            (transpose-chars nil))
        1char-last-called-function (function 1char-transpose-chars))
  )


(defun 1char-change (p from to)
  "change the character given by a 1char to another character."
  (interactive
   (list
    current-prefix-arg
    (1char-read "Change (1char): ")
    (progn (message "To:") (read-char))))
  (1char-excursion p from
    (setq 1char-marker (point-marker))
    (delete-char 1)
    (insert to)
    (1char-maybe-expand-abbrevs))
  (setq 1char-last-prefix-arg p
        1char-change-to to
        1char-last-1char from
        1char-change-from (1char-to-char from)
        1char-undoer
          (f:l ()
            (save-excursion
              (goto-char (1+ 1char-marker))
              (backward-delete-char-untabify 1)
              (insert 1char-change-from)))
        1char-last-called-function
        (f:l (p oc)
             (1char-change p oc 1char-change-to)))
  )


(defun 1char-delete (p oc)
  "delete the character given by a 1char."
  (interactive
   (list
    current-prefix-arg
    (1char-read "Delete (1char):")))
  (1char-excursion p oc
    (setq 1char-marker (point-marker))
    (delete-char 1)
    (1char-maybe-expand-abbrevs))
  (setq 1char-last-prefix-arg p
        1char-last-1char oc
        1char-deleted-char (1char-to-char oc)
        1char-undoer
          (f:l () (save-excursion (goto-char 1char-marker) (insert 1char-deleted-char)))
        1char-last-called-function (function 1char-delete))
  )

(defun 1char-insert-before (p oc new)
  "insert a character just before the 1char c.  new is the new
character.  expands abbrevs according to 1char-expands-abbrevs."
  (interactive
   (list
    current-prefix-arg
    (1char-read "Insert before (1char): ")
    (progn (message "Char:") (read-char))))
  (1char-excursion p oc
    (setq 1char-marker (point-marker))
    (insert new)
    (1char-maybe-expand-abbrevs))
  (setq 1char-last-prefix-arg p
        1char-insert-char new
        1char-last-1char oc
        1char-undoer
          (f:l () (delete-region 1char-marker (1+ 1char-marker)))
        1char-last-called-function
          (f:l (p oc)
               (1char-insert-before p oc 1char-insert-char)))
  )

(defun 1char-insert-after (p oc new)
  "insert a character just after the 1char c.  new is the new
character.  expands abbrevs according to 1char-expands-abbrevs."
  (interactive
   (list
    current-prefix-arg
    (1char-read "Insert after (1char): ")
    (progn (message "Char:") (read-char))))
  (1char-excursion p oc
    (forward-char 1)
    (insert new)
    (setq 1char-marker (point-marker))
    (1char-maybe-expand-abbrevs))
  (setq 1char-last-prefix-arg p
        1char-insert-char new
        1char-last-1char oc
        1char-undoer
          (f:l () (delete-region  (1- 1char-marker) 1char-marker))
        1char-last-called-function
          (f:l (p oc)
               (1char-insert-after p oc 1char-insert-char))))

(defun 1char-one-further-out (n)
  (interactive "p")
  (1char-undo)
  (funcall 1char-last-called-function 1char-last-prefix-arg
           (1char-+ 1char-last-1char n))
  )

(defun 1char-undo ()
  (save-excursion
    (if 1char-undoer
        (funcall 1char-undoer)
      (error "Sorry, maarster, but I don't know how to fix that.")
      )))

(defun 1char-one-further-in (n)
  (interactive "p")
  (1char-one-further-out (- n)))

(defun 1char-+ (oc n)
  "Return a 1char which is OC but N further out. If the thing comes out having 0 repitition,
then make it (sgn N) -- on the ground that you're probably decrementing or incrementing
and want it to go in some direction."
  (case (car oc)
    (prev (list 'prev (second oc)
                (let ((sum (+ n (third oc))))
                  (cond
                   ((not (zerop sum))
                    sum)
                   ((not (zerop (signum n)))
                    (signum n))
                   (t 1))
                )))
    (t    (error "Doom: illegal 1char %s" (2str oc)))))
      
(defun 1char-negate (oc)
  "Returns a 1char which is OC negated. (forward <-> backward)"
  (case (car oc)
    (prev (list 'prev (second oc) (-  (third oc))))
    (t    (error "Doom: illegal 1char %s" (2str oc)))))


(defun undo-without-moving ()
  "undo one thing without moving point."
  (interactive)
  (let ((p (point-marker)))
    (undo)
    (goto-char p)))


(defun 1char-delete-through-1char-backwards (p oc)
  (interactive
   (list current-prefix-arg
         (1char-read "Delete backwards through (1char):")))
  (kill-region (point)
               (point-after (1char-goto-internal p oc)
                            )))

(defun 1char-delete-through-1char-forwards (p oc)
  (interactive
   (list current-prefix-arg
         (1char-read "Delete forwards through (1char):")))
  (kill-region (point)
               (point-after (1char-goto-internal p
                                                 (1char-negate oc)))))


(defun 1char-case-twiddle (p oc)
  "Change the case of a character a ways back."
  (interactive
   (list current-prefix-arg
         (1char-read "Case twiddle (1char):")))
  (1char-excursion p oc
     (let* ((c (string-to-char (buffer-substring (point) (1+ (point)))))
            (C (logxor 32 c))
            )
       (setq 1char-case-twiddle-char c
             1char-marker (point-marker))
       (when (or (and (<= c ?Z) (>= c ?A))
                 (and (<= c ?z) (>= c ?a)))
         (delete-char 1)
         (insert C))
       (setq 1char-last-prefix-arg p
             1char-last-1char oc
             1char-undoer
               (f:l ()
                 (goto-char 1char-marker)
                 (delete-char 1)
                 (insert 1char-case-twiddle-char))
             1char-last-called-function
               (function 1char-case-twiddle)))))
                 
     
     
(global-set-key "\C-h\C-j" 'join-words)
(global-set-key "\C-h\C-@" 'undo)
(global-set-key "\C-h\C-u" 'undo-without-moving)
(global-set-key "\C-h\C-l" 'recenter-top-para)
(global-set-key "\C-h\C-t" '1char-transpose-chars)
(global-set-key "\C-h\C-d" '1char-delete)
(global-set-key "\C-h\C-a" '1char-insert-after)
(global-set-key "\C-h\C-i" '1char-insert-before)
(global-set-key "\C-h\C-c" '1char-change)
(global-set-key "\C-h\C-b" '1char-break-word)
(global-set-key "\C-h\C-m" '1char-goto)
(global-set-key "\C-h<" '1char-one-further-out)
(global-set-key "\C-h," '1char-one-further-out)
(global-set-key "\C-h\C-n" '1char-one-further-out)
(global-set-key "\C-h>" '1char-one-further-in)
(global-set-key "\C-h." '1char-one-further-in)
(global-set-key "\C-h\C-p" '1char-one-further-in)
(global-set-key "\C-h\C-k\C-k" '1char-delete-through-1char-forwards)
(global-set-key "\C-h\C-k\C-d" '1char-delete-through-1char-backwards)
(global-set-key "\C-h~" '1char-case-twiddle)


