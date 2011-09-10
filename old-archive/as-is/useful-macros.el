;;;From: bard@THEORY.LCS.MIT.EDU
;;;Subject: useful macros
;;;Date: 15 Dec 88 20:02:43 GMT

; Useful macros and basic functions, many of them by Bard Bloom.
; Many of these are things from other LISPs which I keep trying to use
; and discover are missing.

; Donated to the world.  If they blow up your computer, I'll feel sad for
; a few minutes; otherwise no warranty.

;FUNCTIONS
; 
; (2str X) returns X converted to a string.  
; (abs n) returns the absolute value of n
; (rand0 n) returns a random number from 0 to n-1 inclusive
; (rand1 n) returns a random number from 1 to n inclusive
; (randij i j) returns a random number from i to j inclusive.
; (gensym [prefix]) is a decent approximation to a real gensym.
;     It creates an obscurely-named symbol not currently in use.
;     Mostly of use to macro writers.
; (point-after cmd1 ... cmdn)  returns the position that point would 
;     have after doing the commands.  E.g., 
;     (point-after (end-of-line 1)) returns the position of the
;     end of the current line.  It doesn't move point.
;
; FLOW CONTROL AND SUCH
;
; (prepend object list-var) stuffs object onto the front of the list given
;     by list-var.
;
; (when test cmd1 cmd2 ... cmdn)   \  One-sided conditionals.
; (unless test cmd1 cmd2 ... cmdn) /
; 
; (for-list var list cmd1 ... cmdn) evaluates the cmds once
;     with var bound to each element of list.
;
; (for var low high cmd1 ... cmdn) evaluates the commands
;     with var bound to each integer between low and high inclusive.
;     ;; Note: both for-list and for can be nested without trouble.
;
; (do* ((var1 init1 step1) ... (varN initN stepN))
;      (exit-if-this-is-true exit-cmds)
;      body)
;   A general stepped loop.  See documentation. Excessively slow
;   interpreted; fine when compiled.

(provide 'useful-macros)

(defmacro prepend (object lst)
  "Sticks OBJECT on the front of LIST."
  (list 'setq
	lst
	(list 'cons object lst)))

(defun 2str (x)
  "Returns a string with some reasonable print-representation of X.
If X is an integer, it is interpreted as an integer rather than 
a character: (2str 65) ==> \"65\" not \"A\"."
  (cond
   ((stringp x) x)
   ((symbolp x) (symbol-name x))
   ((numberp x) (int-to-string x))
   (t (prin1-to-string x))))


(defmacro when (condition &rest commands)
  "When CONDITION is true, execute the COMMANDS."
  (list 'if
	condition
	(cons 'progn commands)))

(defmacro unless (condition &rest commands)
  "When CONDITION is false, execute the COMMANDS."
  (list 'if
	(cons 'not (list condition))
	(cons 'progn commands)))

(defmacro point-after (&rest commands)
  "Returns the value of point after executing the COMMANDS.  Doesn't move
point.  (Expands to (save-excursion COMMANDS (point)))."
  (` (save-excursion
       (,@ commands)
       point)))


(defvar gensym-counter 0
  "Gensym uses gensym-counter to guess good suffixes of new symbols to intern.")

(defun gensym (&optional prefix)
  "Generates an obscurely-named, not-yet-interned symbol and interns it.  If 
the optional string PREFIX is nil, this makes up a symbol of the form 
c-g c-e c-n c-s c-y c-m - number.  Unlike a real gensym, any symbol with 
the same print name as the gensym-created one _is_ the gensym-created one."
  (cond
   ((null prefix) (setq prefix ""))
   (t             (setq prefix (2str prefix))))
  (let ((sym
          (format "%s-%d" prefix gensym-counter)))
    (while (intern-soft sym)
      (setq gensym-counter (1+ gensym-counter)
            sym (format "%s-%d" prefix gensym-counter)))
    (intern sym)))

(defmacro for-list (var list &rest body)
  "Bind VAR to successive cars of LIST and evaluate BODY."
  (let ((tmp-list (gensym)))
    (`
     (let* (((, tmp-list) (, list))
            ((, var)  (car (, tmp-list))))
       (while (, tmp-list)
         (,@ body)
         (setq (, tmp-list) (cdr (, tmp-list))
               (, var)      (car (, tmp-list))))))))

(defun rand0 (n)
  "Random number in [0, N)"
  (cond
   ((<= n 0) 0)
   (t (abs (% (random) n)))))
(defun abs (x)
  "Absolute value."
  (cond
   ((<= x 0) (- x))
   (t x)))
(defun rand1 (n) 
  "Random number [1,N]."
  (1+ (rand0 n)))
(defun randij (i j)
  "Random number [I,J]"
  (cond
   ((< i j) (+ i (rand0 (1+ (- j i)))))
   ((= i j) i)
   ((> i j) (+ j (rand0 (1+ (- i j)))))
   (t (error "randij wierdness %s %s" (2str i) (2str j)))))


;; expands to:
;; (let* ((v1 init1) (v2 init2) ... (vn initn))
;;   (while (not exit-if-this-is-true)
;;     body
;;     (setq v1 step1
;;           v2 step2
;;           ...
;;           vn stepn))
;;   do-when-exit)
;; 

(defmacro do* (var-forms test-and-exit &rest body)
 "(do*
   ( (v1 init1 step1) ... (vn initn stepn) )
   ( exit-if-this-is-true
      do-when-exit )
   body)
 Some var forms are special.  First arg is a keyword
    (let v init)  -- initialize v to init, and don't change it.
                 == (v init v)
    (always v init-step) -- set v to init-step on each pass.
                 == (v init-step init-step)
    (cdrs v list)
                 == (v list (cdr v))
 NOTE: this is very slow interpreted.  Should be compiled.
"
 (let ((v-init nil)                     ;will be the list ((v1 init1) (v2 init2) ...)
       (v-step nil)                     ;will be the list (v1 step1 v2 step2 ...)
       (exit-if-this-is-true (car test-and-exit))
       (do-when-exit (cdr test-and-exit)))
   (for-list vf var-forms
     (cond
      ((atom vf)
       (error "Bad format for do*: missing parens around a var-form?"))
      (t
       (let ((len (length vf))
             (v (nth 0 vf))
             (init (nth 1 vf))
             (step (nth 2 vf))
             (ordinary-case nil)
             )
         (cond
          ((= len 0) nil)
          ((= len 1) (prepend (list v 'nil) v-init)) ;to be reversed!
          ((= len 2) (error "I don't do lists of length 2"))
          ((eq v 'let)                  ; let v1     i1
           (setq v init                 ; v1  v1     i1
                 init step              ; v1  i1     i1
                 step v                 ; v1  i1     v1
                 ordinary-case t))
          ((eq v 'always)               ; always v1 i1
           (setq v init                 ; v1     v1 i1
                 init step              ; v1     i1 i1
                 ordinary-case t))
          ((eq v 'cdrs)                 ; cdrs v1 i1
           (setq v init                 ; v1   v1 i1
                 init step              ; v1   i1 i1
                 step (list 'cdr v)     ; v1   i1 (cdr v1)
                 ordinary-case t))
          (t
           (setq ordinary-case t)))
         (when ordinary-case
           (prepend (list v init) v-init)
           (unless (equal v step)
             (prepend v v-step)
             (prepend step v-step)))))))
   (setq v-init (nreverse v-init)
         v-step (nreverse v-step))
   (when (null v-init)
     (error "You must have some variables in do*"))
   (when (null exit-if-this-is-true)
     (error "You must have an exit test in do*"))
   (append
    (list 'let*
         v-init
         (append
          (list 'while
                (list 'not exit-if-this-is-true))
          body
          (if v-step
              (list
               (cons 'setq v-step))
            nil)))
    do-when-exit)))


(defmacro for (var low high &rest body)
  "For VAR := LOW to HIGH do BODY."
  (let ((tmp-high (gensym)))
    (`
     (let* (((, tmp-high) (, high))
            ((, var)  (, low)))
       (while (<= (, var) (, tmp-high))
         (,@ body)
         (setq (, var) (1+ (, var))))))))

; And we might as well get the indentation right.

(put 'when 'lisp-indent-hook 1)
(put 'unless 'lisp-indent-hook 1)
(put 'point-after 'lisp-indent-hook 0)
(put 'do* 'lisp-indent-hook 2)
(put 'for-list 'lisp-indent-hook 2)
(put 'for 'lisp-indent-hook 3)
