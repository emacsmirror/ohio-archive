;; Faster apropos commands.
;; Copyright (C) 1989 Free Software Foundation, Inc.

;; This file is not officially part of GNU Emacs, but is being donated
;; to the Free Software Foundation.

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

;; Created by: Joseph Wells, jbw@bucsf.bu.edu
;; Created on: Mon Sep 24 17:16:38 1990
;; Last modified by: Joe Wells, jbw@maverick
;; Last modified on: Wed Jun 12 12:46:44 1991
;; Filename: fast-apropos.el
;; Purpose: Replacement for old slow apropos
;; Change log: 
;; 
;; Mon Sep 24 17:17:00 1990  Joseph Wells  (jbw at bucsf.bu.edu)
;; 
;; 	* Robert Potter <rpotter@grip.cis.upenn.edu> noticed that key
;; 	bindings were not printed out correctly with super-apropos.
;; 	apropos-match-keys refused to find any bindings unless regexp was
;; 	non-nil.  Simple fix.
;; 

;; The ideas for this package were derived from the C code in
;; src/keymap.c and elsewhere.  The functions in this file should
;; always be byte-compiled for speed.  Someone should rewrite this in
;; C (as part of src/keymap.c) for speed.

;; The idea for super-apropos is based on the original implementation
;; by Lynn Slater <lrs@esl.com>.

;; Old History:
;; Fixed bug, current-local-map can return nil.
;; Change, doesn't calculate key-bindings unless needed.
;; Added super-apropos capability, changed print functions.
;; Made fast-apropos and super-apropos share code.
;; Sped up fast-apropos again.
;; Added apropos-do-all option.
;; Added fast-command-apropos.
;; Changed doc strings to comments for helping functions.
;; Made doc file buffer read-only, buried it.
;; Only call substitute-command-keys if do-all set.
;; Added installation instructions

;; If you are using this package before GNU Emacs version 19, you must
;; install it properly.  Put the file in a directory that is included in
;; the Lisp variable load-path.  Then put the following statements either
;; in your .emacs file or in the system-wide lisp/defaults.el file:
;;
;;   (autoload 'fast-apropos "fast-apropos" nil t)
;;   (autoload 'fast-command-apropos "fast-apropos" nil t)
;;   (autoload 'super-apropos "fast-apropos" nil t)
;;   (define-key help-map "a" 'fast-command-apropos)

(defvar apropos-do-all nil
  "*Whether fast-apropos and super-apropos should do everything that they can.
Makes them run 2 or 3 times slower.  Set this non-nil if you have a fast
machine.")

;; If there isn't already a lisp variable named internal-doc-file-name, create
;; it and document it.  This is so the code will work right after RMS adds
;; internal-doc-file-name.
(or (boundp 'internal-doc-file-name)
    (setq internal-doc-file-name (concat exec-directory "DOC")))
(or (documentation-property 'internal-doc-file-name 'variable-documentation)
    (put 'internal-doc-file-name 'variable-documentation
	 "The complete pathname of the documentation file that contains all
documentation for functions and variables defined before Emacs is dumped."))

(defun fast-apropos (regexp &optional do-all pred)
  "Show all symbols whose names contain matches for REGEXP.
If optional argument DO-ALL is non-nil, does more (time-consuming) work such as
showing key bindings.  Optional argument PRED is called with each symbol, and
if it returns nil, the symbol is not shown.  Returns list of symbols and
documentation found."
  (interactive "sFast Apropos (regexp): \nP")
  (setq do-all (or apropos-do-all do-all))
  (let ((apropos-accumulate (apropos regexp pred t)))
    (fast-apropos-get-doc apropos-accumulate)
    (with-output-to-temp-buffer "*Help*"
      (apropos-print-matches apropos-accumulate regexp nil do-all))
    apropos-accumulate))

(defun fast-command-apropos (regexp)
  "Like fast-apropos but lists only symbols that are names of commands
\(interactively callable functions\).  Argument REGEXP is a regular expression
that is matched against command symbol names.  Returns list of symbols and
documentation found."
  (interactive "sFast Command Apropos (regexp): ")
  (let* ((message
	  (let ((standard-output (get-buffer-create "*Help*")))
	    (print-help-return-message 'identity)))
	 (result (fast-apropos regexp t 'commandp)))
    (and message (message message))
    result))

;; If "C-h a" still has its original binding of command-apropos, change it to
;; use fast-command-apropos.  I don't use substitute-key-definition because
;; it's slow.
(if (eq 'command-apropos (lookup-key help-map "a"))
    (define-key help-map "a" 'fast-command-apropos))

(defun fast-apropos-get-doc (list)
  "Helping function for fast-apropos."
;; Takes LIST of symbols and adds documentation.  Modifies LIST in place.
;; Resulting alist is of form ((symbol fn-doc var-doc) ...).  Should only be
;; called by fast-apropos.  Returns LIST.
  (let ((p list)
	fn-doc var-doc symbol)
    (while (consp p)
      (setq symbol (car p)
	    fn-doc (and (fboundp symbol)
			(condition-case nil
			    (documentation symbol)
			  (error "*illegal function documentation*")))
	    var-doc (condition-case nil
			(documentation-property symbol 'variable-documentation)
		      (error "*illegal variable documentation*"))
	    fn-doc (and fn-doc
			(if (stringp fn-doc)
			    (substring fn-doc 0 (string-match "\n" fn-doc))
			  "*illegal function documentation*"))
	    var-doc (and var-doc
			 (if (stringp var-doc)
			     (substring var-doc 0 (string-match "\n" var-doc))
			   "*illegal variable documentation*")))
      (setcar p (list symbol fn-doc var-doc))
      (setq p (cdr p)))
    list))

(defun super-apropos (regexp &optional do-all)
  "Show symbols whose names/documentation contain matches for REGEXP.
If optional argument DO-ALL is non-nil, does more (time-consuming) work such as
showing key bindings and documentation that is not stored in the documentation
file.  Returns list of symbols and documentation found."
  (interactive "sSuper Apropos: \nP")
  (setq do-all (or apropos-do-all do-all))
  (let (apropos-accumulate fn-doc var-doc item)
    (setq apropos-accumulate (super-apropos-check-doc-file regexp))
    (if do-all (mapatoms 'super-apropos-accumulate))
    (with-output-to-temp-buffer "*Help*"
      (apropos-print-matches apropos-accumulate nil t do-all))
    apropos-accumulate))

(defun super-apropos-check-doc-file (regexp)
  "Helping function for super-apropos."
;; Finds all documentation related to REGEXP in internal-doc-file-name.
;; Returns an alist of form ((symbol fn-doc var-doc) ...).
  (let ((doc-buffer (find-file-noselect internal-doc-file-name t))
	;;	(doc-buffer (or (get-file-buffer internal-doc-file-name)
	;;			(find-file-noselect internal-doc-file-name)))
	type symbol doc sym-list)
    (save-excursion
      (set-buffer doc-buffer)
      ;; a user said he might accidentally edit the doc file
      (setq buffer-read-only t)
      (bury-buffer doc-buffer)
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
	(search-backward "\C-_")
	(setq type (if (eq ?F (char-after (1+ (point))))
		       1		;function documentation
		     2)			;variable documentation
	      symbol (progn
		       (forward-char 2)
		       (read doc-buffer))
	      doc (buffer-substring
		   (point)
		   (progn
		     (if (search-forward "\C-_" nil 'move)
			 (1- (point))
		       (point))))
	      item (assq symbol sym-list))
	(or item
	    (setq item (list symbol nil nil)
		  sym-list (cons item sym-list)))
	(setcar (nthcdr type item) doc)))
    sym-list))

(defun super-apropos-accumulate (symbol)
  "Helping function for super-apropos."
;; This is passed as the argument to map-atoms, so it is called once for every
;; symbol in obarray.  Takes one argument SYMBOL, and finds any memory-resident
;; documentation on that symbol if it matches a variable regexp.  WARNING: this
;; function depends on the symbols fn-doc var-doc regexp and item being bound
;; correctly when it is called!
  (cond ((string-match regexp (symbol-name symbol))
	 (setq item (apropos-get-accum-item symbol))
	 (setcar (cdr item) (or (safe-documentation symbol)
				(nth 1 item)))
	 (setcar (nthcdr 2 item) (or (safe-documentation-property symbol)
				     (nth 2 item))))
	(t
	 (and (setq fn-doc (safe-documentation symbol))
	      (string-match regexp fn-doc)
	      (setcar (cdr (apropos-get-accum-item symbol)) fn-doc))
	 (and (setq var-doc (safe-documentation-property symbol))
	      (string-match regexp var-doc)
	      (setcar (nthcdr 2 (apropos-get-accum-item symbol)) var-doc))))
  nil)

(defun apropos-print-matches (matches &optional regexp spacing do-all)
  "Helping function for fast-apropos and super-apropos."
;; Prints the symbols and documentation in alist MATCHES of form ((symbol
;; fn-doc var-doc) ...).  Uses optional argument REGEXP to speed up searching
;; for keybindings.  The names of all symbols in MATCHES must match REGEXP.
;; Displays in the buffer pointed to by standard-output.  Optional argument
;; SPACING means put blank lines in between each symbol's documentation.
;; Optional argument DO-ALL means do more time-consuming work, specifically,
;; consulting key bindings.  Should only be called within a
;; with-output-to-temp-buffer.
  (setq matches (sort matches (function
			       (lambda (a b)
				 (string-lessp (car a) (car b))))))
  (let ((p matches)
	(old-buffer (current-buffer))
	item keys-done symbol)
    (save-excursion
      (set-buffer standard-output)
      (or matches (princ "No matches found."))
      (while (consp p)
	(setq item (car p)
	      symbol (car item)
	      p (cdr p))
	(or (not spacing) (bobp) (terpri))
	(princ symbol)		        ;print symbol name
	;; don't calculate key-bindings unless needed
	(cond ((and do-all (commandp symbol) (not keys-done))
	       (save-excursion
		 (set-buffer old-buffer)
		 (apropos-match-keys matches regexp))
	       (setq keys-done t)))
	(cond ((and do-all
		    (or (setq tem (nthcdr 3 item))
			(commandp symbol)))
	       (indent-to 30 1)
	       (if tem
		   (princ (mapconcat 'key-description tem ", "))
		 (princ "(not bound to any keys)"))))
	(terpri)
	(cond ((setq tem (nth 1 item))
	       (princ "  Function: ")
	       (princ (if do-all (substitute-command-keys tem) tem))))
	(or (bolp) (terpri))
	(cond ((setq tem (nth 2 item))
	       (princ "  Variable: ")
	       (princ (if do-all (substitute-command-keys tem) tem))))
	(or (bolp) (terpri)))))
  t)

(defun apropos-match-keys (alist &optional regexp)
  "Helping function for apropos-print-matches."
;; Find key bindings for symbols that are cars in ALIST.  Optionally, first
;; match the symbol name against REGEXP.  Modifies ALIST in place.  Each key
;; binding is added as a string to the end of the list in ALIST whose car is
;; the corresponding symbol.  The pointer to ALIST is returned.
  (let* ((current-local-map (current-local-map))
	 (maps (append (and current-local-map
			    (accessible-keymaps current-local-map))
		       (accessible-keymaps (current-global-map))))
	 map				;map we are now inspecting
	 sequence			;key sequence to reach map
	 i				;index into vector map
	 command			;what is bound to current keys
	 key				;last key to reach command
	 local				;local binding for sequence + key
	 item)				;symbol data item in alist
    ;; examine all reachable keymaps
    (while (consp maps)
      (setq map (cdr (car maps))
	    sequence (car (car maps))	;keys to reach this map
	    maps (cdr maps))
      (cond ((consp map)
	     (setq map (cdr map))))	;skip keymap symbol
      (setq i 0)
      (while (and map (< i 128))	;vector keymaps have 128 entries
	(cond ((consp map)
	       (setq command (cdr (car map))
		     key (car (car map))
		     map (cdr map)))
	      ((vectorp map)
	       (setq command (aref map i)
		     key i
		     i (1+ i))))
	;; if is a symbol, and matches optional regexp, and is a car
	;; in alist, and is not shadowed by a different local binding,
	;; record it
	(and (symbolp command)
	     (or (null regexp)
		 (string-match regexp (symbol-name command)))
	     (setq item (assq command alist))
	     (setq key (concat sequence (char-to-string key)))
	     ;; checking if shadowed by local binding.
	     ;; either no local map, no local binding, or runs off the
	     ;; binding tree (number), or is the same binding
	     (or (not current-local-map)
		 (not (setq local (lookup-key current-local-map key)))
		 (numberp local)
		 (eq command local))
	     ;; add this key binding to the item in alist
	     (nconc item (cons key nil))))))
  alist)

(defun apropos-get-accum-item (symbol)
  "Helping function for super-apropos-accumulate."
;; Get an alist item in alist apropos-accumulate whose car is SYMBOL.  Creates
;; the item if not already present.  Modifies apropos-accumulate in place.
  (or (assq symbol apropos-accumulate)
      (progn
	(setq apropos-accumulate
	      (cons (list symbol nil nil) apropos-accumulate))
	(assq symbol apropos-accumulate))))

(defun safe-documentation (function)
  "Like documentation, except it avoids calling get_doc_string().
Will return nil instead."
  (while (symbolp function)
    (setq function (if (fboundp function)
		       (symbol-function function)
		     0)))
  (if (not (consp function))
      nil
    (if (eq (car function) 'macro)
	(setq function (cdr function)))
    (if (not (memq (car function) '(lambda autoload)))
	nil
      (setq function (nth 2 function))
      (if (stringp function)
	  function
	nil))))

(defun safe-documentation-property (symbol)
  "Like documentation-property, except it avoids calling get_doc_string().
Will return nil instead."
  (setq symbol (get symbol 'variable-documentation))
  (if (numberp symbol)
      nil
    (if (stringp symbol)
	symbol
      "*illegal variable documentation*")))
