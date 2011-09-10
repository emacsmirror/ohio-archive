;;; @ min-bind.el  Bind  keymaps, syntax tables and variables in minor modes.
;;; Version 1.01

;;;   LCD Archive Entry:
;;;   min-bind|Inge Frick|inge@nada.kth.se|
;;;   Bind keymaps, syntax tables and varibales in minor modes|
;;;   09-Jun-1993|1.01|~/misc/min-bind.el.Z|

(provide 'min-bind)
(require 'kill-fix)

;;; @@ Copyright
;;;
;;; Copyright (C) 1993 Inge Frick (inge@nada.kth.se)
;;; 
;;; Almost all the code is by Inge Frick. Some code, much help and
;;; advise comes from Per Abrahamsen.
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;; This file contains special keymap handling code for emacs 18, emacs
;;; 19 and lemacs. Selection which code to use is done when compiling.
;;; As a consequence this file must be byte-compiled on the same type of
;;; system as where it will be used. The only exceptions are that if it is
;;; compiled for emacs 18, then it can be used (less optimally) in emacs
;;; 19 and if you compile for lemacs then you can use it (again less
;;; optimally) in emacs 19 if you also load the lucid compatibility package.

;;; @@ History:
;;;
;;; Version 1.01  Wed Jun  9 17:07:57 1993  inge@nada.kth.se
;;; In emacs18, full keymaps are converted to sparse keymaps. As a
;;; consequence full keymaps and sparse keymaps can be handled properly
;;; in both emacs 18 and 19.
;;;
;;; Version 1.00  Thu May 27 09:31:35 1993  inge@nada.kth.se
;;; First distributed version.

;;; @@ Description:
;;;
;;; The main purpose of min-bind is to define functions that can be used
;;; by e.g. minor modes to bind and unbind variables, keymaps and syntax
;;; table entries in a consistent way.
;;; That this is needed, is illustrated by the following simple example:
;;;  A variable 'varno' has originally value 0.
;;;  Enter minor mode 'mode1' that changes varno to 1 and remembers the
;;;  old value.
;;;  Enter minor mode 'mode2' that changes varno to 2 and remembers the
;;;  old value.
;;;  The value of varno is now 2.
;;;  If you exit the minor modes last in first out, all is well. Exiting
;;;  mode2, resets varno to 1 and exiting mode1 resets varno to 0.
;;;  If you exit mode1 first, varno is reset to 0 which mode2 doesn't
;;;  want. If you then exit mode2, it will reset varno to 1, which is not
;;;  the original value!
;;; The problem is that the hierarchy of old values of varno forms a
;;; stack and have to be restored last in first out.
;;;
;;; The functions in min-bind solves this problem by storing this
;;; hierarchy as an association list with a tag telling which mode each
;;; value belongs to.  This allows values to be restored in arbitrary
;;; order.
;;; The entries in the syntax table can be handled in a similar way, we
;;; have one alist for each character whose syntax has been modified.
;;;
;;; Keymaps are handled somewhat differently: Instead of an hierarchy of
;;; bindings for each key, we form a hierarchy of keymaps like this:
;;; If the original local keymap is map0, then mode1 adds keymap map1 and
;;; mode2 adds map2, we end up with the hierarchy: map2-map1-map0.
;;; Key lookup in the local keymap will now first look for a key in map2.
;;; If the key is not found in map2, map1 will be searched and then if
;;; necessary map0. Both emacs 19 and lemacs have mechanisms for building
;;; such hierarchies and for emacs 18 we can build such hierarchies for
;;; sparse keymaps. We also build these hierarchies for all keymaps reached
;;; through prefix keys.
;;;
;;; A minor mode must either be local or non-local. A local minor mode
;;; affects only buffer local values while a non-local minor mode only
;;; affects global values (for a buffer-local variable, it affects the
;;; default value). A minor mode that mixes local and non-local variables
;;; will have inconsistent effects depending on in which buffer you enter or
;;; exit it.
;;; Local minor modes are more common but a non-local minor mode could e.g.
;;; be used to affect all instances of a major mode.
;;; Min-bind supports both local and non-local minor modes. If a mode has
;;; not been declared non-local by the minor-non-local function (see below),
;;; then it is regarded as a local minor mode and the functions that set a
;;; variable (minor-set-variable and optionally minor-modify-syntax and
;;; minor-add-to-keymap) will moke the variable buffer-local.

;;; @@ min-bind defines the following public functions:
;;;
;;; minor-set-variable (mode variable value)
;;;   In MODE set VARIABLE to VALUE.
;;;   If MODE is local, this will make VARIABLE buffer-local.
;;;   If MODE is non-local, instead set the default value of VARIABLE.
;;;   The old value will be restored when `minor-unbind' is called.
;;;
;;; minor-modify-syntax-entry (mode char syntax &optional tablename)
;;;   In MODE set syntax for CHARacter according to string SYNTAX.
;;;   If the optional argument TABLENAME is present, modify instead the table
;;;   that is the value of TABLENAME.
;;;   See 'modify-syntax-entry' for the format of the SYNTAX string.
;;;   If MODE is local and TABLENAME is present, this will make TABLENAME
;;;   buffer-local.
;;;   If MODE is non-local, instead modify the table that is the default value
;;;   of TABLENAME wich must be present as there is no global syntax table.
;;;   The old syntax will be restored when `minor-unbind' is called.
;;;
;;; minor-add-to-keymap (mode keymap &optional keymapname)
;;;   In MODE, add copy of KEYMAP to the local (global) keymap.
;;;   If the optional argument KEYMAPNAME is present, add instead to the
;;;   keymap that is the value of keymapname.
;;;   If MODE is local then add to the local keymap or, if KEYMAPNAME is
;;;   present, to the value of KEYMAPNAME which is made buffer-local.
;;;   If MODE is non-local, instead add to the global keymap or to the
;;;   default value of KEYMAPNAME.
;;;   The old keymap will be restored when `minor-unbind' is called.
;;;
;;; minor-unbind (mode)
;;;   Undo changes made in MODE to variables, keymaps and syntax tables.
;;;
;;; The following functions are sometimes useful:
;;;
;;; minor-shadowed-variable (mode variable)
;;;   Get the value shadowed by MODE for VARIABLE.
;;;
;;; minor-shadowed-keymap (mode &optional keymapname)
;;;   Get keymap shadowed by addition in MODE to local (global) keymap or
;;;   KEYMAPNAME.
;;;   Mode and optional KEYMAPNAME has the same meaning as in
;;;   'minor-add-to-keymap'.
;;;
;;; minor-call-shadow (mode key &optional keymapname)
;;;   Call the function shadowed by MODE for KEY interactively.
;;;   Mode and optional KEYMAPNAME has the same meaning as in
;;;   'minor-add-to-keymap'.
;;;
;;; In most cases, the following macro is useful for defining minor modes:
;;;
;;; minor-mode-define (mode name cleanup doc &rest setup)
;;;   Define local minor MODE with NAME in minor-mode-alist.
;;;   CLEANUP is code for doing extra cleanup when leaving the mode.
;;;   DOC is the documentation for the minor mode.
;;;   The minor mode is setup by the SETUP commands.
;;;   Modifications done with mode as first argument to minor-set-variable,
;;;   minor-modify-syntax or minor-add-to-keymap, are undone at exit.
;;;
;;; If you want to define a non local minor mode, you need the following:
;;;
;;; minor-non-local (mode)
;;;   Declare minor mode with name MODE to be a non-local minor mode.
;;;   Undeclared minor modes are local minor modes.
;;;
;;; The MODE argument is used just as a tag, you can use any identifier,
;;; but it is a good idea to use the minor modes mode variable.
;;;
;;; The idea of the two minor-shadowed functions, is that you should be
;;; able to refer to the old variable or keymap binding.
;;;
;;; @@ Limitations:
;;;
;;; You will get an error: "Duplicated minor binding" if you try to
;;; modify a variable (or a keymap or a syntax) for the same mode (and
;;; char) a second time without unbinding it in between.
;;; If you have a minor mode that needs to change a (public) variable
;;; more than once, then it is a good idea to use a separate MODE
;;; identifier in minor-set-variable when setting this variable so that
;;; you can unbind it without having to unbind all the other minor mode
;;; variables. Just remember to unbind this variable as well as all the
;;; others when you exit the minor mode.
;;; Variables that are internal to a minor mode and not public do of
;;; course not need to be set by minor-set-variable.
;;; 
;;; Do not use define-key to modify a keymap that is also modified by
;;; minor-add-to-keymap. It will work until you unbind an addition to
;;; this keymap, then the binding you defined by define-key, will
;;; dissapear or not in a predictable but possibly unexpected way. If you
;;; need to define a single key binding, make a (sparse) keymap with your
;;; keybinding in it and then add this keymap.
;;;
;;; Keymaps in lemacs, emacs 19 and emacs 18 are different.
;;; In lemacs there is really no distinction between full and sparse
;;; keymaps. There is the concept of keymap parent, which allows building
;;; a hierarchy of keymaps.
;;; In emacs 19, sparse or full keymaps can have a parent keymap that can
;;; be a sparse or full keymap.
;;; In emacs 18, we can, for a sparse keymap, simulate a parent keymap,
;;; but this parent must also be a sparse keymap. Because of this, full
;;; keymaps are converted to sparse keymaps by min-bind when adding
;;; keymaps. This works, but it is better to avoid full keymaps in minor
;;; modes.

;;; @@ Binding precedence:
;;;
;;; If two minor modes bind the same variable, which binding takes
;;; precedence? In other words, where in the alist of values do you put a
;;; new binding?
;;; The answer for min-bind is that the present version implements dynamic
;;; binding, so that the latest binding takes precedence, i.e. new bindings
;;; are always put at the front of the alist of values. It is very easy to
;;; implement another binding scheme, all that is required is that the
;;; function minor-posn (see below) returns a pointer into the alist of
;;; values that points at the position after wich the new binding should be
;;; positioned.
;;; There is code that has been commented out in minor-posn that implements
;;; a static binding scheme based on the opposite order of
;;; minor-mode-alist. This has the attractive feature that the order of
;;; modes in the mode line tells the precedence order, with the major mode
;;; having the lowest precedence and the rightmost minor mode having the
;;; highest precedence. The problem with this static scheme is that the
;;; order of minor-mode-alist depends on the somewhat random order in which
;;; minor mode definitions have been loaded.
;;; Dynamic binding on the other hand leaves it to the user to determine
;;; the precedence order by the order in which he enters minor modes. It
;;; would be nice if the mode line would show the precedence order for
;;; dynamic binding also. This will probably be done in the next version of
;;; min-bind by changing the mode line code.

;;; The following variable is defined here to stop the byte compiler from
;;; complaining. minor-non-local is bound dynamically at entry to all public
;;; functions. It is used as a free variable by a number of functions.
(defvar minor-non-local nil)


;;; @@ public functions

(put 'minor-mode-define 'lisp-indent-hook 3)
(defmacro minor-mode-define (mode name cleanup doc &rest setup)
  "Define local minor MODE with NAME in minor-mode-alist.
CLEANUP is code for doing extra cleanup when leaving the mode.
DOC is the documentation for the minor mode.
The minor mode is setup by the SETUP commands.
Modifications done with mode as first argument to minor-set-variable,
minor-modify-syntax or minor-add-to-keymap, are undone at exit."
  (minor-do-define mode name nil cleanup doc setup))

(defun minor-non-local (mode)
  "Declare minor mode with name MODE to be a non-local minor mode"
  (put mode 'minor-non-local t))


(defun minor-set-variable (mode variable value)
  "In MODE set VARIABLE to VALUE.
If mode is local, this will make variable buffer-local.
If mode is non-local, instead set the default value of variable.
The old value will be restored when `minor-unbind' is called."
  (let* ((minor-non-local (minor-non-local-p mode))
         (valist (minor-get-values-alist 'variable variable))
	 (premal (minor-posn mode valist)))
    (if valist ()
      (minor-make-local-variable variable)
      (setq valist (list variable (cons nil (minor-get-val variable))))
      (minor-set-values-alist 'variable variable valist))
    (if premal (setq valist premal)
      (minor-set-val variable value))
    (setcdr valist (cons (cons mode value) (cdr valist)))))
      
(defun minor-shadowed-variable (mode variable)
  "Get the value shadowed by MODE for VARIABLE."
  (minor-get-shadow mode 'variable variable))


(defun minor-modify-syntax-entry (mode char syntax &optional tablename)
  "In MODE set syntax for CHARacter according to string SYNTAX.
If the optional argument TABLENAME is present, modify instead the table
that is the value of tablename.
See 'modify-syntax-entry' for the format of the syntax string.
If mode is local and tablename is present, this will make tablename
buffer-local.
If mode is non-local, instead modify the table that is the default value
of tablename which must be present as there is no global syntax table.
The old syntax will be restored when `minor-unbind' is called."
  (let* ((minor-non-local (minor-non-local-p mode))
         (valist (cdr-safe (minor-get-values-alist 'syntax-table tablename)))
	 (table (minor-get-table tablename))
	 (premal nil) entry malist)
    (if valist ()
      (minor-make-local-variable tablename)
      (setq valist (list (cons table (copy-syntax-table table))))
      (minor-set-values-alist 'syntax-table tablename (cons tablename valist))
      (minor-set-table tablename (setq table (minor-current valist))))
    (modify-syntax-entry char syntax table)
    (setq entry (cons mode (aref table char)))
    (cond
     ((not (setq malist (assq char (cdr valist)))) ; values alist for char.
      (setcdr valist (cons (setq malist (list char)) (cdr valist)))) ; New char
     ((setq premal (minor-posn mode malist))
      (aset table char (minor-current (cdr malist)))
      (setq malist premal)))
    (setcdr malist (cons entry (cdr malist)))))


(defun minor-add-to-keymap (mode keymap &optional keymapname)
  "For MODE, add copy of KEYMAP to the local (global) keymap or to KEYMAPNAME.
If the optional third argument KEYMAPNAME is not nil, it will be used
intstead of the current map.
For a non-local MODE add to global keymap or to default value of KEYMAPNAME."
  (let* ((minor-non-local (minor-non-local-p mode))
	 (valist (minor-get-values-alist 'keymap keymapname))
	 (premal (minor-posn mode valist))
	 (acs-list (minor-accessible-copied-keymaps keymap))
	 (maps acs-list)
	 pair nmap pred)
    (if valist ()
      (minor-make-local-variable keymapname)      
      (setq valist
	    (list keymapname
		  (cons nil (minor-try-accessible-keymaps keymapname))))
      (minor-set-values-alist 'keymap keymapname valist))
    (while maps			; For keymap and all sub-keymaps
      (setq pair (car maps))
      (setq nmap (minor-next-map pair (cdr (or premal valist))))
      (minor-replace-parent pair nil nmap)
      (if (and premal (setq pred (minor-pred-map-pair pair (cdr valist))))
	  (minor-replace-parent pred nmap (cdr pair)))
      (setq maps (cdr maps)))
    (if premal (setq valist premal)
      (minor-set-map keymapname (cdr (car acs-list))))
    (setcdr valist (cons (cons mode acs-list) (cdr valist)))))

(defun minor-shadowed-keymap (mode &optional keymapname)
  "Get keymap shadowed by addition in MODE to local (global) keymap.
Mode and optional KEYMAPNAME has the same meaning as in 'minor-add-to-keymap'."
  (minor-get-shadow mode 'keymap keymapname))

(defun minor-call-shadow (mode key &optional keymapname)
  "Call the function shadowed by MODE for KEY interactively.
Mode and optional KEYMAPNAME has the same meaning as in 'minor-add-to-keymap'."
  (let ((old-map (current-local-map)))
    (unwind-protect
	(progn
	  (use-local-map (minor-shadowed-keymap mode keymapname))
	  (while (keymapp (key-binding key))
	    (message key)
	    (setq key (concat key (char-to-string (read-char)))))
	  (command-execute key))
      (use-local-map old-map))))


(defun minor-unbind (mode)
  "Undo changes made in MODE to variables, keymaps and syntax tables."
  (let* ((minor-non-local (minor-non-local-p mode))
	 (types (cons nil (minor-get-data)))
	 (mld types) mal)
    (while (cdr types)			; For each type:
      (let* ((vars (car (cdr types)))
	     (fn (get (car vars) 'minor-remove-function)))
	(while (cdr vars)		; For each var
	  (setq mal (car (cdr vars)))
	  (setcdr mal (funcall fn mode (car mal) (cdr mal)))
	  (if (cdr-safe (cdr mal)) (setq vars (cdr vars)) ; Next var
	    (setcdr vars (cdr (cdr vars))))) ; Remove empty var
	(if (cdr (car (cdr types))) (setq types (cdr types)) ; Next type
	  (setcdr types (cdr (cdr types)))))) ; Remove empty type
    (minor-set-data (cdr mld))))


;;; @@ Simple list manipulation functions

(defun minor-list-find (element list &optional stop)
  ;; Return the sublist starting with ELEMENT in the alist LIST.
  (while (not (or (eq list stop) (eq element (car (car list)))))
    (setq list (cdr list)))
  (if (not (eq list stop)) list))

(defun minor-remassq (entry alist)
  ;; Remove first association (if any) for entry ENTRY in alist ALIST.
  (cond
   ((null alist) ())
   ((eq entry (car (car alist))) (cdr alist))
   (t (setcdr alist (minor-remassq entry (cdr alist))) alist)))

(defun minor-last (list)
  ;; Return last link in list list.
  (while (cdr list) (setq list (cdr list)))
  list)

;;; @@ Command support

(defun minor-do-define (mode name non-local cleanup doc setup)
  ;; Do the job for macro minor-mode-define.
  (` (progn
       (,@ (if (and cleanup (not non-local))
	       (list (` (put '(, mode) 'killing-local-variable-function
			     (function (lambda (&rest ignore)
					 (, cleanup))))))))
       (if (not (assoc '(, mode) minor-mode-alist))
	   (setq minor-mode-alist (cons (list '(, mode) (, (concat " " name)))
					minor-mode-alist)))
       (defvar (, mode) nil
	 (, (concat "Flag indicating whether " name " minor mode is active.")))
       ((, (if non-local 'minor-non-local 'make-variable-buffer-local))
	'(, mode))
       (defun (, mode) (&optional arg)
	 (, doc)
	 (interactive "P")
	 (cond
	  ((and arg
		(if (<= (prefix-numeric-value arg) 0) (not (, mode))
		  (, mode)))
	   ())				; Do nothing if already in wanted mode.
	  ((setq (, mode) (not (, mode))) (,@ setup))
	  (t (,@ (if cleanup (list cleanup)))
	     (minor-unbind '(, mode))))
	 ;; No-op, but updates mode line.
	 (set-buffer-modified-p (buffer-modified-p))))))

(defun minor-get-shadow (mode type variable)
  ;; Get the value shadowed by MODE for a type TYPE VARIABLE.
  ;; Returns nil if MODE not there.
  (let* ((minor-non-local (minor-non-local-p mode))
	 (mal (cdr-safe			;Skip this mode
	       (minor-list-find		;Find this mode for this type
		mode (minor-get-values-alist type variable)))))
    (if mal (funcall (get type 'minor-current-function) mal))))

(defun minor-non-local-p (mode)
  (get mode 'minor-non-local))

(defun minor-make-local-variable (var)
  (if (and var (not minor-non-local)) (make-local-variable var)))

;;; @@ Private variable

;;; All the hierarchies of values are stored in the global resp. buffer local
;;; variables minor-non-local-data and minor-local-data, both of which are an
;;; alist of alists of alists with the format:
;;; (.. (TYPE .. (VAR . VALIST) ..) ..)
;;; Here TYPE is 'variable, 'keymap, or 'syntax-table,
;;;      VAR is name of a variable, a keymap or a syntax-table,
;;;      VALIST varies according to TYPE.
;;; For variables the format for VALIST is:
;;;   (.. (MODE . VALUE) ..)
;;; for keymaps the format is:
;;;   (.. (MODE .. (KEY . MAP) ..) ..)
;;; and for syntax tables the format is:
;;;   ((OTABLE . TABLE) .. (CHAR .. (MODE . SYNTAX) ..) ..)
;;; Here MODE is the name of a minor mode,
;;;      VALUE is the value of VAR in mode MODE,
;;;      KEY is a prefix key sequence,
;;;      MAP is the (sub-) keymap of keymap VAR with prefix KEY in mode MODE,
;;;      OTABLE is the original syntax table value for VAR and TABLE is the
;;;       copy of OTABLE that is used,
;;;      CHAR is a character in the syntax table,
;;;      SYNTAX is the the syntax table entry for CHAR in MODE for the
;;;       syntaxtable VAR.
;;; For variables and keymaps, MODE nil represents the original value for VAR.
;;; For syntax-tables and keymaps, VAR nil is used to represent the current
;;; syntax-table or the local or global keymap.

(defvar minor-non-local-data nil)
(defvar minor-local-data nil)
(make-variable-buffer-local 'minor-local-data)

;;; @@ Functions for handling minor-data

(put 'minor-local-data 'killing-local-variable-function 'minor-unbind-all)
(defun minor-unbind-all (old-vars)
  ;; This function is called by a modified kill-all-local-variables when
  ;; minor-local-data is killed. Its purpose is to reset to base value
  ;; the minor variables that have not been killed.
  (let ((types (assq 'minor-local-data old-vars)) ; minor-local-data
	(locals (buffer-local-variables)) var valist)
    (while (setq types (cdr types))		; For each type:
      (let* ((vars (car types))
	     (cfn (get (car vars) 'minor-current-function))
	     (bfn (get (car vars) 'minor-base-function)))
	(while (setq vars (cdr vars))	; For each var (variable):
	  (and (setq var (car (car vars))) ; if it is not nil and
	       (assq var locals)	; not killed and
	       (eq (symbol-value var)	; still has its minor current
		   (funcall cfn (setq valist (cdr (car vars))))) ; value then
	       (set var (funcall bfn valist)))))))) ; reset it to base value

(defun minor-get-values-alist (type var)
  (assq var (cdr-safe (assq type (minor-get-data)))))

(defun minor-set-values-alist (type var valist)
  (let* ((types minor-local-data)
	 (vars (assq type types))
	 (mal (if vars (assq var (cdr vars)))))
    (if mal (setcdr mal (cdr valist))
      (if vars (setcdr vars (cons valist (cdr vars)))
	(minor-set-data (cons (list type valist) types))))))

(defun minor-get-data ()
  (if minor-non-local minor-non-local-data
    minor-local-data))

(defun minor-set-data (types)
  (if minor-non-local (setq minor-non-local-data types)
    (setq minor-local-data types)))

(defun minor-posn (mode valist)
  ;; Check for duplicate minor binding and return position after wich MODE
  ;; should be inserted into VALIST. Return nil for insertion at beginning
  ;; of valist.
  (let (mal)
    (cond
     ((null (setq valist (cdr-safe valist))) nil)
     ((assq mode valist) (error "Duplicated minor binding"))
     ;; The following code gives a static ordering of minor-mode variables,
     ;; based on the opposite order of that in minor-mode-alist. Tags not in
     ;; minor-mode-alist are ordered dynamically and before all tags in
     ;; minor-mode-alist.  If this code is commented out, you get a dynamic
     ;; ordering of all tags.
;;;  ((and (setq mal (minor-list-find mode minor-mode-alist)) ; a minor mode
;;;	   (progn (setq mal (cdr mal))	; with
;;;		  (not (minor-list-find (car (car valist)) ; lower priority
;;;				 minor-mode-alist mal)))) ; than current
;;;   (while (and (cdr valist)
;;;		  (not (minor-list-find (car (car (cdr valist)))
;;;					minor-mode-alist mal)))
;;;	(setq valist (cdr valist)))
;;;   valist)
     (t nil))))			; This line should not be commented out.
	

;;; @@ Support functions for variables

(put 'variable 'minor-current-function 'minor-current)
(defun minor-current (valist)
  (cdr (car valist)))

(put 'variable 'minor-base-function 'minor-base)
(defun minor-base (valist)
  (minor-current (minor-last valist)))

(put 'variable 'minor-remove-function 'minor-remove-variable-function)
(defun minor-remove-variable-function (mode variable valist)
  ;; Remove MODE variable from variable VARIABLE and values alist VALIST.
  (let ((entry (assq mode valist)))
    (cond
     ((null entry) valist)		; Do nothing if not there
     ((eq entry (car valist))		; Current added variable
      (minor-set-val variable (minor-current (cdr valist)))
      (cdr valist))
     (t (delq entry valist)))))		; Not current, remove from values alist

(defun minor-get-val (variable)
  (if minor-non-local (default-value variable)
    (symbol-value variable)))

(defun minor-set-val (variable value)
  (if minor-non-local (set-default variable value)
    (set variable value)))

;;; @@ Support functions for syntax table

(put 'syntax-table 'minor-current-function 'minor-current)

(put 'syntax-table 'minor-base-function 'minor-base-table)
(defun minor-base-table (valist)
  (car (car valist)))

(put 'syntax-table 'minor-remove-function 'minor-remove-syntax-table-function)
(defun minor-remove-syntax-table-function (mode tablename valist)
  ;; Remove MODE syntax table entries from syntax table TABLENAME and
  ;; values alist VALIST.
  (let ((table (minor-current valist)) (otable (minor-base-table valist))
	(mal valist))
    (while (cdr mal)
      (let* ((calist (car (cdr mal)))
	     (char (car calist)))
	(cond
	 ((not (eq mode (car (car (cdr calist))))) ; Not latest entry
	  (minor-remassq mode (cdr calist)) ; If there, remove it.
	  (setq mal (cdr mal)))
	 ((setcdr calist (cdr (cdr calist))) ; Not last entry.
	  (aset table char (minor-current (cdr calist)))
	  (setq mal (cdr mal)))
	 (t (aset table char (aref otable char)) ; No minor entries for this,
	    (setcdr mal (cdr (cdr mal))))))) ; remove it, restore base syntax.
    (if (not (cdr valist))
	(minor-set-table tablename otable)) ; Base syntax for all.
    valist))

(defun minor-get-table (tablename)
  (cond
   (tablename (minor-get-val tablename))
   (minor-non-local (error "No global syntax table"))
   (t (syntax-table))))

(defun minor-set-table (tablename table)
  (cond
   (tablename (minor-set-val tablename table))
   (t (set-syntax-table table))))


;;; @@ Support functions for keymaps

(put 'keymap 'minor-current-function 'minor-current-map)
(defun minor-current-map (valist)
  (cdr (car (cdr (car valist)))))

(put 'keymap 'minor-base-function 'minor-base-map)
(defun minor-base-map (valist)
  (minor-current-map (minor-last valist)))

(put 'keymap 'minor-remove-function 'minor-remove-keymap-function)
(defun minor-remove-keymap-function (mode keymapname valist)
  ;; Remove MODE keymap from keymap KEYMAPNAME and values alist VALIST.
  (let ((mapl (minor-list-find mode valist)) acs-list)
    (cond
     ((null mapl) valist)		; Do nothing if not there
     ((eq mapl valist)			; Latest added keymap
      (minor-set-map keymapname (minor-current-map (cdr valist)))
      (cdr valist))
     (t (setq acs-list (car mapl))	; Not latest, cut it out.
	(while (setq acs-list (cdr acs-list)) ; For each map and submap,
	  (let ((pred (minor-pred-map-pair (car acs-list) valist))) ; if fund
	    (if pred			; predesseor,
		(minor-replace-parent	; cut map out from keymap chain.
		 pred (cdr (car acs-list))
		 (minor-next-map pred (cdr mapl))))))
	(delq (car mapl) valist)))))	; remove from values alist

(defun minor-set-map (keymapname map)
  (cond
   (keymapname (minor-set-val keymapname map))
   (minor-non-local (use-global-map map))
   (t (use-local-map map))))

;;; The values alist for keymaps has the following format:
;;; (.. (MODE . ACS-LIST) ..)
;;; where ACS-LIST is the output of minor-accessible-copied-keymaps and has
;;; the format:
;;; (.. (KEY . MAP) ..)
;;; where KEY is the prefix key sequence for the keymap MAP and MAP is an
;;; entry in the parent chain that has the prefix KEY.
;;; The following two functions, move up resp. down the parent chain by
;;; searching up resp. down the values alist for maps with the same prefix.

(defun minor-pred-map-pair (pair valist)
  (let ((key (car pair)) (pred nil) tmp)
    ;; Search for the last (key . map) pair with prefix (car PAIR) that
    ;; comes before PAIR in VALIST.
    (while (and valist
		(or (not (setq tmp (assoc key (car valist)))) ; no map or
		    (and (not (eq tmp pair)) (setq pred tmp))))	; not eq pair
      (setq valist (cdr valist)))
    pred))

(defun minor-next-map (pair valist)
  (let ((key (car pair)) (map nil))
    ;; Find first keymap that has prefix key (car PAIR) in VALIST.
    (while (and valist (not (setq map (assoc key (cdr (car valist))))))
      (setq valist (cdr valist)))
    (cdr-safe map)))

;;; The following keymap handling code is partly system dependent. We use
;;; two macros to select the right code for emacs 18, emacs 19 or lemacs.

(put 'minor-iflemacs 'lisp-indent-hook 1)
(defmacro minor-iflemacs (yy &rest nn)
  (cond
   ((fboundp 'keymap-parent) yy)	; lemacs
   ((null nn) ())
   ((cdr nn) (cons 'progn nn))
   (t (car nn))))

(put 'minor-if19 'lisp-indent-hook 1)
(defmacro minor-if19 (yy &rest nn)
  (cond
   ((> (string-to-int emacs-version) 18) yy) ; lemacs or emacs 19
   ((null nn) ())
   ((cdr nn) (cons 'progn nn))
   (t (car nn))))

(defun minor-replace-parent (pred omap nmap)
  ;; In keymap chain (cdr PRED) replace map OMAP with map NMAP;
  (let (tmp)			; Look for element before omap in pred.
    (setq pred (cdr pred))
    (minor-if19 ()
      ;; In emacs 18 skip keymap header.
      (setq omap (cdr-safe omap))
      (setq nmap (cdr-safe nmap)))
    (while (and (not (eq omap
			 (setq tmp (minor-iflemacs (keymap-parent pred)
				     (cdr pred)))))
		(setq pred tmp)))
    (if pred (minor-iflemacs (set-keymap-parent pred nmap)
	       (setcdr pred nmap)))))

(defun minor-try-accessible-keymaps (keymapname)
  ;; Return the accessible-keymaps list for the keymap corresponding to
  ;; KEYMAPNAME.
  ;; For emacs 18 replace full keymaps by sparse.
  (let ((keymap (cond
		 (keymapname (minor-get-val keymapname))
		 (minor-non-local (current-global-map))
		 (t (current-local-map)))))
    (if (null keymap) (list (cons nil nil)) ; No keymap.
      (minor-if19 (accessible-keymaps keymap)
	(let* ((acs (accessible-keymaps keymap))
	       (tmp acs))
	  (while (and tmp (not (vectorp (cdr (car tmp))))) ; Look for full
	    (setq tmp (cdr tmp)))			   ; keymaps.
	  (if tmp			; If some full keymaps.
	      (cons (cons nil keymap)	; Original here to restore at end.
		    (minor-accessible-copied-keymaps keymap)) ; Use sparse.
	    acs))))))		; Only sparse keymaps.

(defun minor-accessible-copied-keymaps (keymap)
  ;; This function does two things:
  ;; 1. It copies KEYMAP. The only differrence from copy-keymap, is that it
  ;; for lemacs also copies the whole parent chain. It would be better if
  ;; only accessible keymaps where copied, but to do that one would need a
  ;; version of copy-keymap that didn't copy sub-keymaps.
  ;; 2. It returns an alist looking like that returned by accessible-keymaps
  ;; but it doesn't include keymaps that have not been copied, i.e. keymaps
  ;; accessible by indirection. It does include though, not shadowed keymaps
  ;; in the parent chain.
  ;; It would be much better if copy-keymap and accessible-keymaps had an
  ;; optional flag that made them do this.
  ;; In emacs 18 this function also converts all full keymaps to sparse
  ;; keymaps.
  (let* ((acs-list (list (cons (minor-if19 [] "")
			       (setq keymap (copy-keymap keymap)))))
	 (tail acs-list)
	 (this acs-list)
	 tmap keys thismap
	 (acs-help		; This function is called for each element of
	  (function			; a keymap with MAPQ as the binding of
	   (lambda  (key mapq)		; KEY in the keymap looked upon.
	     (and			; If it is
	      (keymapp mapq)		;  a keymap,
	      (not (symbolp mapq))	;  not an indirection trough a symbol
	      (progn (setq key (minor-if19 (vconcat keys (list key))
				 (concat keys (char-to-string key))))
		     (eq mapq (lookup-key keymap key))) ;  and not shadowed,
	      (setq tail (setcdr tail	;  then add at end of acs-list.
				 (list (cons key mapq)))))))))
    (while this
      (setq keys (car (car this)))
      (setq thismap (cdr (car this)))
      (minor-iflemacs
	  (while thismap
	    (map-keymap acs-help thismap)
	    (setq tmap thismap)
	    (if (setq thismap (keymap-parent thismap))
		(set-keymap-parent tmap
				   (setq thismap (copy-keymap thismap)))))
	(minor-if19 ()
	  (if (vectorp thismap)	; In emacs 18, convert full keymap to sparse.
	      (let ((nmap (list 'keymap)) (key (length thismap)) val)
		(while (>= (setq key (1- key)) 0)
		  (if (setq val (aref thismap key))
		      (setcdr nmap (cons (cons key val) (cdr nmap)))))
		(setcdr (car this) (setq thismap nmap))
		(if (not (equal keys "")) (define-key keymap keys thismap)))))
        (while (setq thismap (cdr thismap))
	  (let ((elem (car thismap)))
	    (minor-if19
		(cond
		 ((consp elem) (funcall acs-help (car elem) (cdr elem)))
		 ((vectorp elem)
		  (let ((key (length elem)))
		    (while (>= (setq key (1- key)) 0)
		      (funcall acs-help key (aref elem key))))))
	      (funcall acs-help (car elem) (cdr elem))))))
      (setq this (cdr this)))
    acs-list))

;;; @@ Emacs
 
;;; Local Variables:
;;; mode: emacs-lisp
;;; eval: (put 'minor-iflemacs 'lisp-indent-hook 1)
;;; eval: (put 'minor-if19 'lisp-indent-hook 1)
;;; outline-regexp: ";;; @+\\|(......"
;;; End:
