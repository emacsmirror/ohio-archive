;;; Copyright (C) 1992 Aaron Larson.

;;; This file is not part of the GNU Emacs distribution (yet).

;;; This file is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY.  No author or distributor
;;; accepts responsibility to anyone for the consequences of using it
;;; or for whether it serves any particular purpose or works at all,
;;; unless he says so in writing.  Refer to the GNU Emacs General Public
;;; License for full details.

;;; Everyone is granted permission to copy, modify and redistribute
;;; this file, but only under the conditions described in the
;;; GNU Emacs General Public License.   A copy of this license is
;;; supposed to have been given to you along with GNU Emacs so you
;;; can know your rights and responsibilities.  It should be in a
;;; file named COPYING.  Among other things, the copyright notice
;;; and this notice must be preserved on all copies.

;;; region-key.el

;;; LCD Archive Entry:
;;; region-key|Aaron Larson|alarson@src.honeywell.com|
;;; Region specific modes for emacs.|
;;; 93-01-18|version 1.0|~/misc/region-key.el.Z|


;;; The following supports the establishment of keybindings for regions of
;;; a buffer.  The regions are defined by user supplied functions.  In this
;;; way, for example, it is possible to define a function which will return
;;; true when point is in the documentation string for a lisp function, and
;;; have the documentation string be processed in latex-mode, while the
;;; rest of buffer is processed in lisp-mode.  There is no limit to the
;;; number of different "region-specific" modes which can be defined for a
;;; buffer, although the time to process keybindings for local modes is
;;; proportional to the number of region-specific modes defined.
;;; 
;;; See define-region-specific-keybindings for more info.
;;; 
;;; To accomplish this, a "fake" local keymap is installed in the current
;;; buffer which has entries in it for the FIRST characters of all local key
;;; sequences for all region specific modes for this buffer (see region-
;;; specific-local-keys-only for an exception to this).  When one of these
;;; keys is typed, a dispatch function (region-specific-keybinding-exec) is
;;; called which then calls the "region definition predicate" functions for all
;;; the region specific modes in the buffer until one returns true.  A full
;;; key sequence is then read, and if it was defined by the local keymap for
;;; the associated mode, the local variables for the mode are established and
;;; the command is executed.  If the key sequence was defined by the global
;;; map, no local variable bindings are established.  (It could certianly be
;;; argued that in this situation, the other region specific modes should be
;;; consulted, or that the local variable binding should be established anyway,
;;; but due to the "bug" referenced in meta-region-specifc-keybinding-exec it
;;; turns out to be objectional to establish the keybindings since all meta
;;; commands then fall into this category.  Checking the other region specific
;;; modes creates a pseudo hierarchy of modes which I think would be hard for a
;;; user to reason about, but I have not actually tried it.)

;;; This file defines externally visible functions:
;;;   define-region-specific-keybindings

;;; The functions progv and mapkeymap should be moved to other files
;;; during some future emacs reorganization.

;;; Modification History:
;;; Written: Aaron Larson (alarson@src.honeywell.com) 12/7/91
;;; Modified:
;;;   Aaron Larson 2/25/92  Install syntax-table for all command executions.
;;;   Aaron Larson 5/11/92  Allow regions-specific-keybinding-exec
;;;   			    callable non interactively .

;;; Begin version 1.0

(require 'cl)
(provide 'region-key)

;;; ---------- This should be in a more general place. ---------- 
(defun mapkeymap (function keymap)
  "Call FUNCTION for each key in KEYMAP.  Function will be called with two 
arguments, a key, and its current binding."
  (if (consp keymap)
      (dolist (k (cdr keymap))
	(funcall function (car k) (cdr k)))
    (dotimes (i (length keymap))
      (funcall function i (aref keymap i)))))

(defmacro save-match-data (&rest forms)
  (let ((md (gensym)))
    (`(let (((, md) (match-data)))
       (unwind-protect
	    (progn (,@ forms))
	 (store-match-data (, md)))))))

;;; --------- Following should be in cl.el -------------

;;; (put 'progv 'common-lisp-indent-hook '(4 4 &body))
(defmacro progv (vars vals &rest body)
  "progv vars vals &body forms
bind vars to vals then execute forms.
If there are more vars than vals, the extra vars are unbound, if
there are more vals than vars, the extra vals are just ignored."
  (` (progv-runtime (, vars) (, vals) (function (lambda () (,@ body))))))

;;; To do this efficiently, it really needs to be a special form, or at
;;; least have a byte compiled representation.
(defun progv-runtime (vars vals body)
  (eval (let ((vars-n-vals nil)
	      (unbind-forms nil))
	  (do ((r vars (cdr r))
	       (l vals (cdr l)))
	      ((endp r))
	    (push (list (car r) (list 'quote (car l))) vars-n-vals)
	    (if (null l)
		(push (` (makunbound '(, (car r)))) unbind-forms)))
	  (` (let (, vars-n-vals) (,@ unbind-forms) (funcall '(, body)))))))

(defvar region-specific-local-keys-only nil
  "*If true, then only keys local to a region specific keymap dispatch to
region-specific-keybinding-exec, otherwise ALL keys will.  In general you
want all keys to go through region-specific-keybinding-exec since mode 
settings effect the operation of globally defined functions as well (e.g. 
forward-sexp), however on some platforms the overhead of this is too high, 
so setting this to true will result in the overhead being noticed only for
locally defined keys.")

(defstruct region-specific-info
  ;; the mode name (symbol) for this region-specific mode.
  mode-name
  ;; function of zero args, returns true if in a region of buffer that
  ;; this info applies to, the resulting value, if non nil, is passed to
  ;; prep-fun
  pred-fun
  ;; the keymap that should be used when reading the command key sequence
  keymap
  ;; A random buffer that is in the mode that this region of the buffer is
  ;; to be assumed to be in.
  buffer
  ;; The syntax table for this region
  syntax-table
  ;; A function of one arg that is called to "prepare" the buffer to
  ;; execute a region specific key binding.  Typical things for this
  ;; function to do are to narrow the buffer so that the region specific
  ;; command only sees the region of the buffer in in this mode
  prep-fun)

(defvar region-specific-info-list ()
  "list of regin-specific-info structs.  Structs are unique w.r.t mode-name")

(defvar original-keymap nil)

(defvar region-specific-dispatch-keymap nil)

(defun define-region-specific-keybindings (mode buffer predicate &optional prep-fun)
	      "MODE is the mode name (symbol) for the region, or nil to disable all
region-specific key bindings for this buffer.  BUFFER is a buffer in major
mode MODE, or nil to disable region-specific-key bindings for MODE in this
buffer.  PREDICATE, a function of no arguments, defines the regions of the
buffer where this mode will be in effect.  PREP-FUN (optional), will be
called, within a save-restriction, with the value returned from PREIDICATE
prior to calling the interactive command, and prior to establishing the
local variable bindings for MODE from BUFFER.

For any point in the current-buffer where PREDICATE returns non nil,
define-region-specific-keybindings causes any local key bindings defined in
the current-local-map of BUFFER to override the current-local-map of the
current-buffer (however, see variable region-specific-local-keys-only for an
exception to this.  Define-region-specific-keybindings may be called to define
any number of \"region specific\" local keymaps.  If the PREDICATE function
for more than one of the region specific keymaps returns non nil, one of
them will be arbitrarily choosen."

  (if (null mode)
      (progn
	;; disable everything and clean up.
	(use-local-map original-keymap)
	(kill-local-variable 'original-keymap)
	(kill-local-variable 'region-specific-info-list)
	(kill-local-variable 'region-specific-dispatch-keymap))
      (progn
	;; First do some initialization.
	(when (null original-keymap)
	  (make-local-variable 'original-keymap)
	  (setq original-keymap (current-local-map)))
	;; if someone introduces a new local key binding after this function runs,
	;; it will be hard for users to acquire the new binding.
	(when (null region-specific-dispatch-keymap)
	  (make-local-variable 'region-specific-dispatch-keymap)
	  (make-local-variable 'region-specific-info-list)
	  (if region-specific-local-keys-only
	      (setq region-specific-dispatch-keymap (make-sparse-keymap))
	      (progn
		(setq region-specific-dispatch-keymap (make-keymap))
		;; make them all dispatch.  We will later "reset" several
		;; of these key bindings to the same thing, but so what?
		(fillarray region-specific-dispatch-keymap 'region-specific-keybinding-exec)))
	  (use-local-map region-specific-dispatch-keymap)
	  ;; get the help key so that describe-bindings displays the
	  ;; "expected" thing.
	  (define-key region-specific-dispatch-keymap (char-to-string help-char)
	    'region-specific-keybinding-exec)
	  ;; see meta-region-specific-keybinding-exec for explanation.
	  (let ((kmap (make-keymap)))
	    (fillarray kmap 'meta-region-specific-keybinding-exec)
	    (define-key region-specific-dispatch-keymap (char-to-string meta-prefix-char) kmap)))
	(let ((struct (do ((l region-specific-info-list (cdr l)))
			  ((or (null l)
			       (eq mode (region-specific-info-mode-name (car l))))
			   (car l)))))
	  ;; delete the old one, we'll create a new one below.  Reusing the
	  ;; old instance requires setf of structure accessors, which
	  ;; doesn't work with old versions of the byte compiler.
	  (when struct
	    (setq region-specific-info-list (delq struct region-specific-info-list)))
	  (if (null buffer)
	      ;; null buffer --> ignore this mode from now on.
	      (when (null region-specific-info-list)
		;; this was the last one, might as well disable it all the way.
		(define-region-specific-keybindings nil nil nil))
	      (let* ((keymap (save-excursion (set-buffer buffer) (current-local-map)))
		     (syntax (save-excursion (set-buffer buffer) (syntax-table)))
		     (struct (make-region-specific-info :mode-name mode
							:pred-fun  predicate
							:keymap    keymap
							:buffer    buffer
							:prep-fun  (or prep-fun 'identity)
							:syntax-table syntax)))
		(push struct region-specific-info-list)
		;; for any key defined in the keymap for that mode, define
		;; it locally to call the dispatch function.
		(mapkeymap (function (lambda (k ignore)
			     ;; all meta chars are handled above.
			     (unless (eq k meta-prefix-char)
			       (define-key region-specific-dispatch-keymap
				   (char-to-string k)
				 'region-specific-keybinding-exec))))
			   keymap)
		)))))
  nil)

(defun read-cmd-in-map (map &optional install-map)
  (let ((omap (current-local-map)))
    (unwind-protect
	 (progn
	   (use-local-map map)
	   (let* ((seq (read-key-sequence nil))
		  (lcmd (lookup-key (current-local-map) seq))
		  (gcmd (lookup-key global-map seq))
		  (cmd (or (and (not (numberp lcmd)) lcmd)
			   (and (not (numberp gcmd)) gcmd))))
	     (if cmd
		 (values cmd (eq cmd lcmd))
		 (values 'undefined nil))))
      (unless install-map
	(use-local-map omap)))))

(defun region-specific-keybinding-exec (&optional CmD)
  "Execute a command from the original keymap, or from one of the region
specific modes depending on context.  If CMD is nil, then the command is 
determined by reading the keyboard in an appropriate keymap.  Otherwise
CMD should be a function to call with the variable bindings for the
appropriate mode established."
  (interactive)
  ;; see if any of the predicate functions identifies the point as being
  ;; within a "region-specific" region.
  (let* ((info nil)
	 (val nil))
    (do ((l region-specific-info-list (cdr l)))
	((or val (null l)))
      (when (setq val (funcall (region-specific-info-pred-fun (car l))))
	(setq info (car l))))

    ;; WARNING: there are lots of ordering dependencies in the following,
    ;; e.g. read-cmd-in-map must be done before calling the prep-fun, etc.  Be
    ;; careful if you reorder anything.  This is at least in part a
    ;; problem because many "functions" return state information
    ;; about the "current" situation, rather than taking arguments, e.g.
    ;; current-local-map, rather than bufer-local-map, etc.
    (let ((buf (current-buffer))
	  (orig-syntax-table (syntax-table))
	  dest-map dest-syntax-table)
      ;; leave the keymap the command is read from installed so that
      ;; things like describe-bindings gets the right bindings for the
      ;; given situation.
      (unwind-protect
	   (let ((Call-Fun 'call-interactively)
		 localp)
	     (if CmD
		 ;; CmD was specified as an argument.
		 (setq Call-Fun 'funcall
		       localp info)
		 (progn
		   ;; this assumes that this fun is bound to a "top level"
		   ;; command (i.e. not a sequence within a prefix map).
		   (setq unread-command-char last-input-char)
		   (if (null val)
		       (setq CmD (read-cmd-in-map original-keymap t)
			     localp nil)
		       (multiple-value-setq (CmD localp)
			 (read-cmd-in-map (region-specific-info-keymap info) t)))
		   (setq this-command CmD)))
	     (setq dest-map (current-local-map))
	     ;; this implements region specific modes, not a hierarchy of
	     ;; modes (i.e. if not found in this local mode, try the next
	     ;; local mode), which may be more appropriate.  
	     (if localp
		 (let ((var-alist (buffer-local-variables (region-specific-info-buffer info))))
		   (save-restriction
		     (funcall (region-specific-info-prep-fun info) val)
		     ;; set syntax table must be done AFTER calling region
		     ;; prep function since the prep function assumes it
		     ;; is still in the mode of the original buffer, and
		     ;; changing syntax on it could break things like sexp
		     ;; traversal. 
		     (set-syntax-table (setq dest-syntax-table
					     (region-specific-info-syntax-table info)))
		     ;; this will of course die miserably if the info-buffer has a
		     ;; local variable named "CmD" or Call-Fun!!
		     (progv (mapcar 'car var-alist)
			 (mapcar 'cdr var-alist)
		       (funcall Call-Fun CmD))))
		 (progn
		   (when val
		     ;; set syntax table when in a specific region, even
		     ;; for globally defined commands.  Necessary for
		     ;; things like forward-sexp!
		     (set-syntax-table (setq dest-syntax-table
					     (region-specific-info-syntax-table info))))
		   ;; cmd was defined in global map
		   (funcall Call-Fun CmD))))
	;; buffer may be deleted.
	(when (buffer-name buf)
	  (save-excursion
	    ;; cmd may have switched buffers!
	    (set-buffer buf)
	    ;; If the executed command didn't change the keymap, then
	    ;; restore the saved map.
	    (when (and (eq (current-local-map) dest-map)
		       ;; If the last command disabled all region specific
		       ;; bindings, then the map is the same, but the dispatch
		       ;; map has been eliminated.
		       region-specific-dispatch-keymap)
	      (use-local-map region-specific-dispatch-keymap))
	    ;; If the executed command didn't change the syntax table, then
	    ;; restore the saved one.
	    (when (eq (syntax-table) dest-syntax-table)
	      (set-syntax-table orig-syntax-table))))))))

;;; This is a highly bletcherous piece of cruft.  It is caused by the fact
;;; that the 18.55 keyboard reader, when reading meta characters, IGNORES
;;; local keymaps which have bindings for meta-prefix-char which are not
;;; themselves keymaps (see read_key_sequence in file keyboard.c in the
;;; vicinity of the string "Are the definitions prefix characters?").  So,
;;; to get around this, we define a keymap, make all the entries in it
;;; point to this function, re-meta the character the meta flag was
;;; stripped from, then proceed as usual.  
;;; Clear as mud.  [alarson:19910725.1157CST]
(defun meta-region-specific-keybinding-exec ()
  (interactive)
  (setq last-input-char (logior 256 last-input-char))
  (region-specific-keybinding-exec))


;;;; --------------------------------------------------------------------
;;;; The following is an example usage of the above.  It should really be
;;;; put in a separate file.
;;;; --------------------------------------------------------------------

(defvar latex-strings-mode nil)

(defvar latex-strings-mode-double-quote-is-lisp nil
  "*If true, a double quote character in a latex string will generate a double
quote rather than a pair of single quotes as in latex-mode.")

(defun latex-strings-mode (&optional arg)
  "A minor mode for lisp-mode modes that causes keyboard commands within
strings to be treated as though they were entered while in latex-mode.
Toggles mode on and off, with arg force on.

See also:
  variable latex-strings-mode-double-quote-is-lisp"

  (interactive "P")
  (when (not (assoc 'latex-strings-mode minor-mode-alist))
    (push '(latex-strings-mode  " LaTeX-Strings") minor-mode-alist))
  (make-local-variable 'latex-strings-mode)
  (setq latex-strings-mode (or arg (not latex-strings-mode)))
  ;; update mode line
  (set-buffer-modified-p (buffer-modified-p))
  (define-region-specific-keybindings
      'latex-strings-mode
      ;; nil disables bindings for this region-specific mode.
      (when latex-strings-mode
	(save-excursion
	  (set-buffer (get-buffer-create " *some-random-latex-mode-buffer*"))
	  (latex-mode)
	  (current-buffer)))
    'lisp-in-a-string-p
    'narrow-to-region-around-string)
  ;; double quote is both a latex-mode binding, and the terminator for
  ;; lisp strings.  If we leave it as a tex-mode thing, then you have a
  ;; mess of a time terminating lisp strings.  This slightly messes up the
  ;; describe-bindings documentation, but its just not liveable
  ;; otherwise.
  ;; Apparently some people prefer it. [alarson:19920225.0848CST]
  (when (and latex-strings-mode
	     latex-strings-mode-double-quote-is-lisp)
    (local-unset-key "\"")))

(defvar lisp-latex-string-begin "[^\\\"]*\""
  "*A regexp that matches the beginning of a lisp string which will be treated
as a latex-string.  It must not start with whitespace.  Typical usage is to
restrict latex strings to be a subset of all lisp strings.")

(defun lisp-in-a-string-p ()
  (save-match-data
   (let* ((val-list (parse-partial-sexp (save-excursion (beginning-of-defun)
							(point))
					(point)))
	  (in-a-string (nth 3 val-list))
	  (last-complete-sexp-begin (nth 2 val-list)))
     (when in-a-string
       (save-excursion
	 (if (null last-complete-sexp-begin)
	     ;; apparently if there is no complete sexp (e.g. a string as the
	     ;; first form in a file), this can be nil
	     (re-search-backward lisp-latex-string-begin (point-min) t)
	     (progn
	       (goto-char last-complete-sexp-begin)
	       (forward-sexp)
	       ;; ... lisp-skip-whitespace my lisp-mode function...
	       ;; Skip over whitespace and comments.
	       (re-search-forward "\\([ \t\n]+\\|;.*\\)*")))
	 (when (looking-at lisp-latex-string-begin) (point)))))))
  
(defun narrow-to-region-around-string (string-start)
  ;; condition case is because the string may not be terminated properly,
  ;; if not we still want the user cmd to execute.
  (save-match-data 
   (save-excursion
     (goto-char string-start)
     (condition-case v
	 ;; start region AFTER the opening quote.
	 (let ((beg (or (and (looking-at lisp-latex-string-begin)
			     (match-end 0))
			string-start)))
	   (forward-sexp)
	   ;; include a final newline if possible.  Several of the text mode
	   ;; commands (especially fill-paragraph) insert a final newline if
	   ;; one doesn't exist.
	   (when (looking-at "[ \t]*$")
	     (forward-line 1))
	   (narrow-to-region beg (point)))
       (error)))))
