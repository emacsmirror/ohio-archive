;To: unix-emacs@bbn.com
;Date: 17 Feb 89 00:02:51 GMT
;From: "Eric L. Raible" <amelia!orville.nas.nasa.gov!raible@AMES.ARC.NASA.GOV>
;Subject: Re: Keyboard macros (was: Re: Copy From Above?)
;
;In article <50884@yale-celray.yale.UUCP> Ram-Ashwin@cs.yale.edu (Ashwin Ram) writes:
;>I wish there was a way of "decompiling" the macro, i.e., a function that
;>inserted, not the raw keystrokes, but the names of the commands that those
;>keytrokes were bound to.  Keyboard macros are neat for simple things, but if
;>you want to modify one you're out of luck.
;>
;
;See below.  I didn't write this code, but have occasionally found it to be
;useful.
;
;>While we're on the subject of keyboard macros, is there a way to tell Emacs
;>not to stop defining a macro when you do make an error?  I hate it when I'm
;>halfway through an enormously complicated keyboard macro and I have to do it
;>all over again because I hit the wrong key.
;>
;
;Although not it is not exactly what you want, you can give a prefix arg
;to start-kbd-macro to append to the last macro.  It then becomes useful
;to build complicated macros in steps.
;
;- Eric

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; module: 	generate.el	
;;;; version: 	2.0
;;;; author: 	Ciaran A Byrne ciaran@gec-rl-hrc.co.uk
;;;; date:	2:Sept:87
;;;;
;;;;;;;;;;;;;;;;;;;; macro lisp expansion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;	user commands:
;;;;		start-generating	- replaces start-kbd-macro ^X(
;;;;		stop-generating		-     "	   end-kbd-macro   ^X)
;;;;		expand-macro		- produces REAL emacs lisp code
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (global-set-key "\^X(" 'start-generating)
;; (global-set-key "\^X)" 'stop-generating)

(defun caar (x) (car (car x)))
(defun cadr (x) (car (cdr x)))
(defun cdar (x) (cdr (car x)))
(defun cddr (x) (cdr (cdr x)))

(defun caadr (x) (car (cadr x)))
(defun caddr (x) (car (cddr x)))
(defun cadar (x) (car (cdar x)))
(defun cdadr (x) (cdr (cadr x)))


(defvar gen-history '(first . last) "command-history subsection pair")
(defvar generate-on nil "true if recording commands")


(defun start-generating ()
  "Records commands issued until the command stop-generating is invoked.
The recorded commands can be turned into emacs lisp using
the command expand-macro.

Keystrokes are echoed in the minibuffer to remind you that
you are doing something weird"
  (interactive)
  (if generate-on (message "Already generating !")
    (progn
      (setq generate-on t)
      (message "Started generating")
      (rplaca gen-history command-history) ; note beginning of macro
      (unwind-protect
	  (command-loop-3)		; run soft command loop
	(stop-generating))
      (run-hooks 'generate-hook))))

(defun stop-generating ()
  "Ends command recording. See also: start-generating and expand-macro"

  (interactive)
  (rplacd gen-history  command-history)	; note end of macro
  (message "Stopped generating")
  (setq generate-on nil))


(defun expand-macro (buffer fname doc)
  "Expands the most recently recorded command sequence into emacs lisp.
Outputs into BUFFER and calls the function NAME with DOC string.

See also: start-generating, stop-generating"

  (interactive
    "BBuffer for expansion : \nSNew function name : \nsDoc string : ")
  (if generate-on (stop-generating))
  (let ((macro (rev-sub-list gen-history))) ; chop macro out
    (get-buffer-create buffer)
    (set-buffer buffer)
    (goto-char (point-max))
    (set-mark (point))
    (insert "\n(defun " (symbol-name fname) " () " ) ; function header
    (insert "\"" doc)
    (insert "\nmacroised by " (user-full-name))
    (insert " @ " (current-time-string)  "\"\n")
    (insert "\n(interactive)\n")
    (setq standard-output (get-buffer buffer))
    (mapcar 'print macro)
    (exchange-point-and-mark)
    (mapcar 'delete-matching-lines	; zap useless stuff
	    '(   "^$"
		 "start-generating"
		 "stop-generating"
		 "expand-macro"
		 "execute-extended-command nil"
					; etc ?
		 ))
    (narrow-to-region (point) (point-max))
    (emacs-lisp-mode)
    (indent-region (point) (point-max) nil) ; neaten it all up
    (mapcar 'merge-multiple-numeric-args
	    '(
	      previous-line
	      next-line
	      delete-backward-char
	      backward-delete-char-untabify
	      backward-kill-word
	      kill-word
	      forward-char
	      backward-char
					; etc ?
	      ))
    (goto-char (point-max))
    (insert "\n)\n")
    (widen)
    (run-hooks 'expand-macro-hook)))


(defun rev-sub-list (pp)
  "Returns sublist from INTERVAL eg. (beginning . end) ,
where beginning & end point into the same list.
The item at end should be nearer the front of the list.
The car of the result is the element at beginning."

  (let ((stop (car pp))
	(here (cdr pp))
	(result nil))
    (if (not (memq (car stop) here)) (message "bad arg to rev-sub-list")
      (while (not (eq here stop))
	(setq result (cons (car here) result)) ; build in reverse
	(setq here (cdr here))))
    result))

(defun command-loop-3 ()
  "Mimics the internal command_loop_1,
but locks the RECORD arg to command-execute to true.

Handles universal & prefix arguments, fakes self-insert-command.

Fixes up incremental searches in command-history so that the non-incremental
versions are used instead."
  (while generate-on			; global flag
    (if (null (input-pending-p)) (sit-for 2))
    (let* ((ks (read-key-sequence ""))
	   (last-command-char (string-to-char (substring ks -1)))
	   (kc (key-binding ks)) )
      (cond
	((eq kc 'universal-argument) (universal-argument))
	((eq kc 'digit-argument) (digit-argument prefix-arg))
	((eq kc 'self-insert-command) (log-self-insert prefix-arg))
	((eq kc 'stop-generating) (stop-generating))
	(t (command-execute kc 'record)))
      (cond				; now patch search commands
	((eq kc 'isearch-forward)
	 (rplaca command-history
		 (list 'search-forward search-last-string)))
	((eq kc 'isearch-backward)
	 (rplaca command-history
		 (list 'search-backward search-last-string)))
	((eq kc 'isearch-forward-regexp)
	 (rplaca command-history
		 (list 're-search-forward search-last-regexp)))
	((eq kc 'isearch-backward-regexp)
	 (rplaca command-history
		 (list 're-search-backward search-last-regexp)))))))


(defun string-copy (s n)
  "Returns STRING concatted N times."
  (let ((res ""))
    (while (> n 0)
      (setq res (concat res s))
      (setq n (1- n)))
    res))
	   
(defun log-self-insert (n)
  "Replaces self-insert-command (q.v.).
Adds an insert command to command-history, and amalgamates the current
insertion with a previous insert command in command-history, if there is one."
  (setq n (if (integerp n) n 1))
  (let ((ins (string-copy (char-to-string last-input-char) n)))
    (insert ins)
    (if (eq 'insert (caar command-history))
	(let* ((prev (cadar command-history))
	       (str (concat prev ins)))
	  (rplacd (car command-history) (list str)))
      (setq command-history
	    (cons (list 'insert ins) command-history)))))


(defconst numarg "[ \t]+\\([0-9]+\\)")

(defun merge-multiple-numeric-args (s)
  "Coalesces a pair of lisp lines invoking the same FUNCTION with
a numeric arg so that a single function with the 2 component args
added is used instead.
e.g. (previous-line 4), (previous-line 1) => (previous-line 5)"
  (goto-char (point-min))
  (if (symbolp s) (setq s (symbol-name s)))
  (while
      (re-search-forward 
	(concat s numarg ".*\n[ \t]*(" s numarg) (point-max) t)
    (let* ((md (match-data))
	   (arg1 (buffer-substring (nth 2 md) (nth 3 md)))
	   (arg2 (buffer-substring (nth 4 md) (nth 5 md)))  
	   (newarg (+ (string-to-int arg1) (string-to-int arg2))))
      (delete-region (nth 0 md) (nth 1 md))
      (insert s " " (int-to-string newarg))
      (goto-char (nth 0 md)))))

