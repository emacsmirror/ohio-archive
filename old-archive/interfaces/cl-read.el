;; Customizable, Common Lisp like reader for Emacs Lisp.
;; 
;; Copyright (C) 1993 by Guido Bosch <Guido.Bosch@loria.fr>

;; LCD Archive Entry:
;; cl-read.el|Guido Bosch|Guido.Bosch@loria.fr|
;; Customizable, Common Lisp like reader for Emacs Lisp|
;; 21-Mar-94|1.19|~/interfaces/cl-read.el.Z|

;; This file is written in GNU Emacs Lisp, but not (yet) part of GNU Emacs.

;; The software contained in this file is free software; you can
;; redistribute it and/or modify it under the terms of the GNU General
;; Public License as published by the Free Software Foundation; either
;; version 2, or (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;; 
;; Please send bugs and comments to the author.
;;
;; <DISCLAIMER>
;; This program is still under development.  Neither the author nor
;; his employer accepts responsibility to anyone for the consequences of
;; using it or for whether it serves any particular purpose or works
;; at all.


;; Introduction
;; ------------
;;
;; This package replaces the standard Emacs Lisp reader (implemented
;; as a set of built-in Lisp function in C) by a flexible and
;; customizable Common Lisp like one (implemented entirely in Emacs
;; Lisp). During reading of Emacs Lisp source files, it is about 40%
;; slower than the built-in reader, but there is no difference in
;; loading byte compiled files - they dont contain any syntactic sugar
;; and are loaded with the built in subroutine `load'.
;;
;; The user level functions for defining read tables, character and
;; dispatch macros are implemented according to the Commom Lisp
;; specification by Steel's (2nd edition), but the read macro functions
;; themselves are implemented in a slightly different way, because the
;; basic character reading is done in an Emacs buffer, and not by
;; using the primitive functions `read-char' and `unread-char', as real
;; CL does.  To get 100% compatibility with CL, the above functions
;; (or their equivalents) must be implemented as subroutines.
;;
;; Another difference with real CL reading is that basic tokens (symbols
;; numbers, strings, and a few more) are still read by the original
;; built-in reader. This is necessary to get reasonable performance.
;; As a consquence, the read syntax of basic tokens can't be
;; customized.

;; Most of the built-in reader syntax has been replaced by lisp
;; character macros: parentheses and brackets, simple and double
;; quotes, semicolon comments and the dot. In addition to that, the
;; following new syntax features are provided:

;; Backquote-Comma-Atsign Macro: `(,el ,@list) 
;;
;; (the clumsy Emacs Lisp syntax (` ((, el) (,@ list))) is also
;; supported, but with one restriction: the blank behind the quote
;; characters is mandatory when using the old syntax. The cl reader
;; needs it as a landmark to distinguish between old and new syntax.
;; An example:
;;
;; With blanks, both readers read the same:
;; (` (, (head)) (,@ (tail))) -std-read->  (` (, (head)) (,@ (tail)))
;; (` (, (head)) (,@ (tail))) -cl-read->   (` (, (head)) (,@ (tail)))
;;
;; Without blanks, the form is interpreted differently by the two readers:
;; (`(,(head)) (,@(tail))) -std-read-> (` (, (head)) (,@ (tail)))
;; (`(,(head)) (,@(tail))) -cl-read->  ((` ((, ((head)))) ((,@ ((tail)))))
;;
;; 
;; Dispatch Character Macro" `#'
;;
;; #'<function>			function quoting
;; #\<charcter>			character syntax
;; #.<form>    			read time evaluation
;; #p<path>, #P<path> 		paths
;; #+<feature>, #-<feature> 	conditional reading
;; #<n>=, #<n># 		tags for shared structure reading
;;
;; Other read macros can be added easily (see the definition of the
;; above ones in this file, using the functions `set-macro-character'
;; and `set-dispatch-macro-character')
;;
;; The Cl reader is mostly downward compatile, (exception: backquote
;; comma macro, see above). E.g., this file, which is written entirely
;; in the standard Emacs Lisp syntax, can be read and compiled with the
;; cl-reader activated (see Examples below). 

;; This also works with package.el for Common Lisp packages.


;; Requirements
;; ------------
;; The package runs on Emacs 18 and Emacs 19 (FSF and Lucid) It is
;; built on top of Dave Gillespie's cl.el package (version 2.02 or
;; later).  The old one (from Ceazar Quiroz, still shiped with some
;; Emacs 19 disributions) will not do.

;; Usage
;; -----
;; The package is implemented as a kind of minor mode to the
;; emacs-lisp-mode. As most of the Emacs Lisp files are still written
;; in the standard Emacs Lisp syntax, the cl reader is only activated
;; on elisp files whose property lines contain the following entry:
;;
;; -*- Read-Syntax: Common-Lisp -*-
;;
;; Note that both property name ("Read-Syntax") and value
;; ("Common-Lisp") are not case sensitive. There can also be other
;; properties in this line: 
;;
;; -*- Mode: Emacs-Lisp; Read-Syntax: Common-Lisp -*-

;; Installation
;; ------------
;; Save this file in a directory where Emacs will find it, then
;; byte compile it (M-x byte-compile-file).
;;
;; A permanent installation of the package can be done in two ways:
;;
;; 1.) If you want to have the package always loaded, put this in your
;;     .emacs, or in just the files that require it:
;;
;; (require 'cl-read) 
;;
;; 2.) To load the cl-read package automatically when visiting an elisp
;;     file that needs it, it has to be installed using the
;;     emacs-lisp-mode-hook. In this case, put the following function
;;     definition and add-hook form in your .emacs:
;;
;; (defun cl-reader-autoinstall-function () 
;;   "Activates the Common Lisp style reader for emacs-lisp-mode buffers,
;; if the property line has a local variable setting like this: 
;; \;\; -*- Read-Syntax: Common-Lisp -*-"
;;
;;   (or (boundp 'local-variable-hack-done)
;;       (let (local-variable-hack-done
;;             (case-fold-search t))
;;         (hack-local-variables-prop-line 't)
;;         (cond 
;;          ((and (boundp 'read-syntax)
;;                read-syntax
;;                (string-match "^common-lisp$" (symbol-name read-syntax)))
;;           (require 'cl-read)
;;           (make-local-variable 'cl-read-active)
;;           (setq cl-read-active 't))))))
;;
;; (add-hook 'emacs-lisp-mode-hook 'cl-reader-autoinstall-function)
;;
;; The `cl-reader-autoinstall-function' function tests for the
;; presence of the correct Read-Syntax property in the first line of
;; the file and loads the cl-read package if necessary. cl-read
;; replaces the following standard elisp functions:
;;
;; 	- read
;; 	- read-from-string
;; 	- eval-current-buffer
;; 	- eval-buffer
;; 	- eval-region
;;	- eval-expression (to call reader explicitly)
;;
;; There may be other built-in functions that need to be replaced
;; (e.g. load).  The behavior of the new reader function depends on
;; the value of the buffer local variable `cl-read-active': if it is
;; nil, they just call the original functions, otherwise they call the
;; cl reader. If the cl reader is active in a buffer, this is
;; indicated in the modeline by the string "CL" (minor mode like). 
;;

;; Examples:
;; ---------
;; After having installed the package as described above, the
;; following forms can be evaluated (M-C-x) with the cl reader being
;; active. (make sure that the mode line displays "(Emacs-Lisp CL)")
;;
;; (setq whitespaces '(#\space #\newline #\tab))
;; (setq more-whitespaces `(#\page ,@whitespaces #\linefeed))
;; (setq whitespace-strings (mapcar #'char-to-string more-whitespaces))
;; 
;; (setq shared-struct '(#1=[hello world] #1# #1#))
;; (progn (setq cirlist '#1=(a b . #1#)) 't)
;;
;; This file, though written in standard Emacs Lisp syntax, can also be
;; compiled with the cl reader active: Type M-x byte-compile-file

;; TO DO List: 
;; -----------
;; - Provide a replacement for load so that uncompiled cl syntax
;;   source file can be loaded, too.  For now prohibit loading un-bytecompiled.
;; - Do we really need the (require 'cl) dependency?   Yes.
;; - More read macros: #S for structs, #A for array, #X for hex, #nR for radix
;; - Refine the error signaling mechanism.
;;     - invalid-cl-read-syntax is now defined. what else?


; Change History
; 
; $Log:	cl-read.el,v $
; Revision 1.19  94/03/21  19:59:24  liberte
; Add invalid-cl-read-syntax error symbol.
; Add reader::read-sexp and reader::read-sexp-func to allow customization
; based on the results of reading.
; Remove more dependencies on cl-package.
; Remove reader::eval-current-buffer, eval-buffer, and eval-region,
; and use elisp-eval-region package instead.
; 
; Revision 1.18  94/03/04  23:42:24  liberte
; Fix typos in comments.
; 
; Revision 1.17  93/11/24  12:04:09  bosch
; cl-packages dependency removed. `reader::read-constituent' and
; corresponding variables moved to cl-packages.el.
; Multi-line comment #| ... |# dispatch character read macro added.
; 
; Revision 1.16  1993/11/23  10:21:02  bosch
; Patches from Daniel LaLiberte integrated.
;
; Revision 1.15  1993/11/18  21:21:10  bosch
; `reader::symbol-regexp1' modified.
;
; Revision 1.14  1993/11/17  19:06:32  bosch
; More characters added to `reader::symbol-characters'.
; `reader::read-constituent' modified.
; defpackage form added.
;
; Revision 1.13  1993/11/16  13:06:41  bosch
; - Symbol reading for CL package convention implemented.
;   Variables `reader::symbol-characters', `reader::symbol-regexp1' and
;   `reader::symbol-regexp2' and functions `reader::lookup-symbol' and
;   `reader::read-constituent' added.
; - Prefix for internal symbols is now "reader::" (Common Lisp
;   compatible).
; - Dispatch character macro #: for reading uninterned symbols added.
;
; Revision 1.12  1993/11/07  19:29:07  bosch
; Minor bug fix.
;
; Revision 1.11  1993/11/07  19:23:59  bosch
; Comment added. Character read macro #\<char> rewritten. Now reads 
; e.g. #\meta-control-x. Needs to be checked. 
; fix in `reader::restore-shared-structure'. `cl-reader-autoinstall-function' improved.
;
; Revision 1.10  1993/11/06  18:35:35  bosch
; Included Daniel LaLiberte's Patches.
; Efficiency of `reader::restore-shared-structure' improved.
; Implementation notes for shared structure reading added.
;
; Revision 1.9  1993/09/08  07:44:54  bosch
; Comment modified.
;
; Revision 1.8  1993/08/10  13:43:34  bosch
; Hook function `cl-reader-autoinstall-function' for automatic installation added.
; Buffer local variable `cl-read-active' added: together with the above
; hook it allows the file specific activation of the cl reader.
;
; Revision 1.7  1993/08/10  10:35:21  bosch
; Functions `read*' and `read-from-string*' renamed into `reader::read'
; and `reader::read-from-string'. Whitespace character skipping after
; recursive reader calls removed (Emacs 19 should not need this).
; Functions `cl-reader-install'  and `cl-reader-uninstall' updated.
; Introduction text and  function comments added.
;
; Revision 1.6 1993/08/09 15:36:05 bosch Function `read*' now nearly
; elisp compatible (no functions as streams, yet -- I don't think I
; will ever implement this, it would be far too slow).  Elisp
; compatible function `read-from-string*' added.  Replacements for
; `eval-current-buffer', `eval-buffer' and `eval-region' added.
; Renamed feature `cl-dg' in `cl', as Dave Gillespie's cl.el package
; is rather stable now.  Function `cl-reader-install' and
; `cl-reader-uninstall' modified.
;
; Revision 1.5  1993/08/09  10:23:35  bosch
; Functions `copy-readtable' and `set-syntax-from-character' added.
; Variable `reader::internal-standard-readtable' added.  Standard
; readtable initialization modified. Whitespace skipping placed back
; inside the read loop.
;
; Revision 1.4  1993/05/14  13:00:48  bosch
; Included patches from Daniel LaLiberte.
;
; Revision 1.3  1993/05/11  09:57:39  bosch
; `read*' renamed in `reader::read-from-buffer'. `read*' now can read
; from strings.
;
; Revision 1.2  1993/05/09  16:30:50  bosch
; (require 'cl-read) added.
; Calling of `{before,after}-read-hook' modified.
;
; Revision 1.1  1993/03/29  19:37:21  bosch
; Initial revision
;
;

;; 
(require 'cl)

(provide 'cl-read)
;; load before compiling
(require 'cl-read)

;; bootstrapping with cl-packages
;; defpackage and in-package are ignored until cl-read is installed.
'(defpackage reader
  (:nicknames "rd")
  (:use el)
  (:export
   cl-read-active
   copy-readtable
   set-macro-character
   get-macro-character
   set-syntax-from-character
   make-dispatch-macro-character
   set-dispatch-macro-character
   get-dispatch-macro-character
   before-read-hook
   after-read-hook
   cl-reader-install
   cl-reader-uninstall
   read-syntax
   cl-reader-autoinstall-function))

'(in-package reader)


(autoload 'compiled-function-p "bytecomp")

;; This makes cl-read behave as a kind of minor mode: 

(make-variable-buffer-local 'cl-read-active)
(defvar cl-read-active nil
  "Buffer local variable that enables Common Lisp style syntax reading.")
(setq-default cl-read-active nil)

(or (assq 'cl-read-active minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(cl-read-active " CL") minor-mode-alist)))

;; Define a new error symbol: invalid-cl-read-syntax
(put 'invalid-cl-read-syntax 'error-conditions 
     '(error invalid-read-syntax invalid-cl-read-syntax))
(put 'invalid-cl-read-syntax 'error-message "Invalid CL read syntax")
     

(defun reader::error (msg &rest args)
  (signal 'invalid-cl-read-syntax (list (apply 'format msg args))))


;; The readtable

(defvar reader::readtable-size 256
  "The size of a readtable."
  ;; Actually, the readtable is a vector of size (1+
  ;; reader::readtable-size), because the last element contains the
  ;; symbol `readtable', used for defining `readtablep.
  )

;; An entry of the readtable must have one of the following forms:
;;
;; 1. A symbol, one of {illegal, constituent, whitespace}.  It means 
;;    the character's reader class.
;;
;; 2. A function (i.e., a symbol with a function definition, a byte
;;    compiled function or an uncompiled lambda expression).  It means the
;;    character is a macro character.
;;
;; 3. A vector of length `reader::readtable-size'. Elements of this vector
;;    may be `nil' or a function (see 2.). It means the charater is a
;;    dispatch character, and the vector its dispatch fucntion table.

(defvar *readtable*)
(defvar reader::internal-standard-readtable)

(defun* copy-readtable 
    (&optional (from-readtable *readtable*) 
	       (to-readtable 
		(make-vector (1+ reader::readtable-size) 'illegal)))
  "Return a copy of FROM-READTABLE \(default: *readtable*\). If the
FROM-READTABLE argument is provided as `nil', make a copy of a
standard \(CL-like\) readtable. If TO-READTABLE is provided, modify and
return it, otherwise create a new readtable object."

  (if (null from-readtable)
      (setq from-readtable reader::internal-standard-readtable))

  (loop for i to reader::readtable-size
	as from-syntax = (aref from-readtable i)
	do (setf (aref to-readtable i)
		 (if (vectorp from-syntax)
		     (copy-sequence from-syntax)
		   from-syntax))
	finally return to-readtable))


(defmacro reader::get-readtable-entry (char readtable)
  (` (aref (, readtable) (, char))))
   
(defun set-macro-character 
  (char function &optional readtable)
    "Makes CHAR to be a macro character with FUNCTION as handler.
When CHAR is seen by reader::read-from-buffer, it calls FUNCTION.
Returns always t. Optional argument READTABLE is the readtable to set
the macro character in (default: *readtable*)."
  (or readtable (setq readtable *readtable*))
  (or (reader::functionp function) 
      (reader::error "Not valid character macro function: %s" function)) 
  (setf (reader::get-readtable-entry char readtable) function)
  t)


(put 'set-macro-character 'edebug-form-spec 
     '(&define sexp function-form &optional sexp))
(put 'set-macro-character 'lisp-indent-function 1)

(defun get-macro-character (char &optional readtable)
   "Return the function associated with the character CHAR.
Optional READTABLE defaults to *readtable*. If char isn't a macro
character in READTABLE, return nil."
   (or readtable (setq readtable *readtable*))
   (let ((entry (reader::get-readtable-entry char readtable)))
     (if (reader::functionp entry) 
	 entry)))

(defun set-syntax-from-character 
  (to-char from-char &optional to-readtable from-readtable)   
  "Make the syntax of TO-CHAR be the same as the syntax of FROM-CHAR.
Optional TO-READTABLE and FROM-READTABLE are the corresponding tables
to use. TO-READTABLE defaults to the current readtable
\(*readtable*\), and FROM-READTABLE to nil, meaning to use the
syntaxes from the standard Lisp Readtable."
  (or to-readtable (setq to-readtable *readtable*))
  (or from-readtable 
      (setq from-readtable reader::internal-standard-readtable))
  (let ((from-syntax
	 (reader::get-readtable-entry from-char from-readtable)))
    (if (vectorp from-syntax)
	;; dispatch macro character table
	(setq from-syntax (copy-sequence from-syntax)))
    (setf (reader::get-readtable-entry to-char to-readtable)
	  from-syntax))
  t)


;; Dispatch macro character
(defun make-dispatch-macro-character (char &optional readtable)
  "Let CHAR be a dispatch macro character in READTABLE (default: *readtable*)."
  (or readtable (setq readtable *readtable*))
  (setf (reader::get-readtable-entry char readtable)
	;; create a dispatch character table 
	(make-vector reader::readtable-size nil)))


(defun set-dispatch-macro-character 
  (disp-char sub-char function &optional readtable)
  "Make reading CHAR1 followed by CHAR2 be handled by FUNCTION.
Optional argument READTABLE (default: *readtable*).  CHAR1 must first be 
made a dispatch char with `make-dispatch-macro-character'."
  (or readtable (setq readtable *readtable*))
  (let ((disp-table (reader::get-readtable-entry disp-char readtable)))
    ;; check whether disp-char is a valid dispatch character
    (or (vectorp disp-table)
	(reader::error "`%c' not a dispatch macro character." disp-char))
    ;; check whether function is a valid function 
    (or (reader::functionp function) 
	(reader::error "Not valid dispatch character macro function: %s" 
		       function))
    (setf (aref disp-table sub-char) function)))

(put 'set-dispatch-macro-character 'edebug-form-spec
     '(&define sexp sexp function-form &optional sexp))
(put 'set-dispatch-macro-character 'lisp-indent-function 2)


(defun get-dispatch-macro-character 
  (disp-char sub-char &optional readtable)
  "Return the macro character function for SUB-CHAR unser DISP-CHAR.
Optional READTABLE defaults to *readtable*.
Returns nil if there is no such function."
  (or readtable (setq readtable *readtable*))
  (let ((disp-table (reader::get-readtable-entry disp-char readtable)))
    (and (vectorp disp-table)
	 (reader::functionp (aref disp-table sub-char))
	 (aref disp-table sub-char))))


(defun reader::functionp (function)
  ;; Check whether FUNCTION is a valid function object to be used 
  ;; as (dispatch) macro character function.
  (or (and (symbolp function) (fboundp function))
      (compiled-function-p function)
      (and (consp function) (eq (first function) 'lambda))))
	   

;; The basic reader loop 

;; shared and circular structure reading
(defvar reader::shared-structure-references nil)
(defvar reader::shared-structure-labels nil)

(defun reader::read-sexp-func (point func)
  ;; This function is called to read a sexp at POINT by calling FUNC.
  ;; reader::read-sexp-func is here to be advised, e.g. by Edebug,
  ;; to do something before or after reading.
  (funcall func))

(defmacro reader::read-sexp (point &rest body)
  ;; Called to return a sexp starting at POINT.  BODY creates the sexp result
  ;; and should leave point after the sexp.  The body is wrapped in
  ;; a lambda expression and passed to reader::read-sexp-func.
  (` (reader::read-sexp-func (, point) (function (lambda () (,@ body))))))

(put 'reader::read-sexp 'edebug-form-spec '(form body))
(put 'reader::read-sexp 'lisp-indent-function 2)
(put 'reader::read-sexp 'lisp-indent-hook 1)  ;; Emacs 18


(defconst before-read-hook nil)
(defconst after-read-hook nil)

;; Set the hooks to `read-char' in order to step through the reader. e.g.
;; (add-hook 'before-read-hook '(lambda () (message "before") (read-char)))
;; (add-hook 'after-read-hook '(lambda () (message "after") (read-char)))

(defmacro reader::encapsulate-recursive-call (reader-call)
  ;; Encapsulate READER-CALL, a form that contains a recursive call to
  ;; the reader, for usage inside the main reader loop.  The macro
  ;; wraps two hooks around READER-CALL: `before-read-hook' and
  ;; `after-read-hook'.
  ;;
  ;; If READER-CALL returns normally, the macro exits immediately from
  ;; the surrounding loop with the value of READER-CALL as result.  If
  ;; it exits non-locally (with tag `reader-ignore'), it just returns
  ;; the value of READER-CALL, in which case the surrounding reader
  ;; loop continues its execution.
  ;;
  ;; In both cases, `before-read-hook' and `after-read-hook' are
  ;; called before and after executing READER-CALL.
  ;; Are there any other uses for these hooks?  Edebug doesn't need them.
  (` (prog2
	 (run-hooks 'before-read-hook)
	 ;; this catch allows to ignore the return, in the case that
	 ;; reader::read-from-buffer should continue looping (e.g.
	 ;; skipping over comments)
	 (catch 'reader-ignore
	   ;; this only works inside a block (e.g., in a loop): 
	   ;; go outside 
	   (return 
	    (prog1 
		(, reader-call)
	      ;; this occurence of the after hook fires if the 
	      ;; reader-call returns normally ...
	      (run-hooks 'after-read-hook))))
       ;; ... and that one if  it was thrown to the tag 'reader-ignore
       (run-hooks 'after-read-hook))))

(put 'reader::encapsulate-recursive-call 'edebug-form-spec '(form))
(put 'reader::encapsulate-recursive-call 'lisp-indent-function 0)

(defun reader::read-from-buffer (&optional stream reader::recursive-p)
  (or (bufferp stream)
      (reader::error "Sorry, can only read on buffers"))
  (if (not reader::recursive-p)
      ;; set up environment for shared structure reading
      (let (reader::shared-structure-references
	    reader::shared-structure-labels
	    tmp-sexp)
	;; the reader returns an unshared sexpr, possibly containing
	;; symbolic references
	(setq tmp-sexp (reader::read-from-buffer stream 't))
	(if ;; sexpr actually contained shared structures
	    reader::shared-structure-references
	    (reader::restore-shared-structure tmp-sexp)
	  ;; it did not, so don't bother about restoring
	  tmp-sexp))

    (loop for char = (following-char)
	  for entry = (reader::get-readtable-entry  char *readtable*)
	  if (eobp) do (reader::error "End of file during reading")
	  do 
	  (cond 

	   ((eq entry 'illegal)
	    (reader::error "`%c' has illegal character syntax" char))

	   ;; skipping whitespace characters must be done inside this
	   ;; loop as character macro subroutines may return without
	   ;; leaving the loop using (throw 'reader-ignore ...)
	   ((eq entry 'whitespace)
	    (forward-char 1)  
	    ;; skip all whitespace
	    (while (eq 'whitespace 
		       (reader::get-readtable-entry  
			(following-char) *readtable*))
	      (forward-char 1)))

	   ;; for every token starting with a constituent character
	   ;; call the built-in reader (symbols, numbers, strings,
	   ;; characters with ?<char> syntax)
	   ((eq entry 'constituent)    
	    (reader::encapsulate-recursive-call
	     (reader::read-constituent stream)))

	   ((vectorp entry)
	    ;; Dispatch macro character. The dispatch macro character
	    ;; function is contained in the vector `entry', at the
	    ;; place indicated by <sub-char>, the first non-digit
	    ;; character following the <disp-char>:
	    ;; 	<disp-char><digit>*<sub-char>
	    (reader::encapsulate-recursive-call
	      (loop initially do (forward-char 1)
		    for sub-char = (prog1 (following-char) 
				     (forward-char 1))
		    while (memq sub-char 
				'(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
		    collect sub-char into digit-args
		    finally 
		    (return 
		     (funcall 
		      ;; no test is done here whether a non-nil
		      ;; contents is a correct dispatch character
		      ;; function to apply.
		      (or (aref entry sub-char)
			  (reader::error
			   "Undefined subsequent dispatch character `%c'" 
			   sub-char))
		      stream
		      sub-char 
		      (string-to-int
		       (apply 'concat 
			      (mapcar 
			       'char-to-string digit-args))))))))
	    
	   (t
	    ;; must be a macro character. In this case, `entry' is
	    ;; the function to be called
	    (reader::encapsulate-recursive-call
	      (progn 
		(forward-char 1)
		(funcall entry stream char))))))))


;; Constituent reader fix for Emacs 18
(if (string-match "^19" emacs-version)
    (defun reader::read-constituent (stream)
      (reader::read-sexp (point)
	(reader::original-read stream)))

  (defun reader::read-constituent (stream)
    (reader::read-sexp (point)
      (prog1 (reader::original-read stream)
	;; For Emacs 18, backing up is necessary because the `read' function 
	;; reads one character too far after reading a symbol or number.
	;; This doesnt apply to reading chars (e.g. ?n).
	;; This still loses for escaped chars.
	(if (not (eq (reader::get-readtable-entry
		      (preceding-char) *readtable*) 'constituent))
	    (forward-char -1))))))


;; Make the default current CL readtable

(defconst *readtable*
  (loop with raw-readtable = 
	(make-vector (1+ reader::readtable-size) 'illegal)
	initially do (setf (aref raw-readtable reader::readtable-size)
			   'readtable)
	for entry in 
	'((constituent ?! ?@ ?$ ?% ?& ?* ?_ ?- ?+ ?= ?/ ?\\ ?0 ?1 ?2
		       ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?: ?~ ?> ?< ?a ?b
		       ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p
		       ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z ?A ?B ?C ?D
		       ?E ?F ?G ?H ?I ?J ?K ?L ?M ?N ?O ?P ?Q ?R
		       ?S ?T ?U ?V ?W ?X ?Y ?Z)
	  (whitespace ?  ?\t ?\n ?\r ?\f)

	  ;; The following CL character classes are only useful for
	  ;; token parsing.  We don't need them, as token parsing is
	  ;; left to the built-in reader.
	  ;; (single-escape ?\\)
	  ;; (multiple-escape ?|)
	  )
	do 
	(loop for char in (rest entry)
	      do (setf (reader::get-readtable-entry  char raw-readtable)
		       (first entry)))
	finally return raw-readtable)
  "The current readtable.")


;; Variables used non-locally in the standard readmacros
(defvar reader::context)
(defvar reader::stack)
(defvar reader::recursive-p)


;;;; Read macro character definitions

;;; Hint for modifying, testing and debugging new read macros: All the
;;; read macros and dispatch character macros below are defined in
;;; the `*readtable*'.  Modifications or
;;; instrumenting with edebug are effective immediately without having to
;;; copy the internal readtable to the standard *readtable*.  However,
;;; if you wish to modify reader::internal-standard-readtable, then
;;; you must recopy *readtable*.

;; Chars and strings

;; This is defined to distinguish chars from constituents 
;; since chars are read by the standard reader without reading too far.
(set-macro-character ?\?
  (function
   (lambda (stream char)
     (forward-char -1)
     (reader::read-sexp (point)
       (reader::original-read stream)))))

;; ?\M-\C-a

;; This is defined to distinguish strings from constituents
;; since backing up after reading a string is simpler.
(set-macro-character ?\"
  (function
   (lambda (stream char)
     (forward-char -1)
     (reader::read-sexp (point)
       (prog1 (reader::original-read stream)
	 ;; This is not needed with Emacs 19, but it is OK.  See above.
	 (if (/= (preceding-char) ?\")
	     (forward-char -1)))))))

;; Lists and dotted pairs
(set-macro-character ?\( 
  (function 
   (lambda (stream char)
     (reader::read-sexp (1- (point))
       (catch 'read-list
	 (let ((reader::context 'list) reader::stack )
	   ;; read list elements up to a `.'
	   (catch 'dotted-pair
	     (while t
	       (setq reader::stack (cons (reader::read-from-buffer stream 't) 
					 reader::stack))))
	   ;; In dotted pair. Read one more element
	   (setq reader::stack (cons (reader::read-from-buffer stream 't) 
				     reader::stack)
		 ;; signal it to the closing paren
		 reader::context 'dotted-pair)
	   ;; Next char *must* be the closing paren that throws read-list
	   (reader::read-from-buffer stream 't)
	   ;; otherwise an error is signalled
	   (reader::error "Illegal dotted pair read syntax")))))))

(set-macro-character ?\) 
  (function 
   (lambda (stream char)
     (cond ((eq reader::context 'list)
	    (throw 'read-list (nreverse reader::stack)))
	   ((eq reader::context 'dotted-pair)
	    (throw 'read-list (nconc (nreverse (cdr reader::stack)) 
				     (car reader::stack))))
	   (t 
	    (reader::error "`)' doesn't end a list"))))))
	
(set-macro-character ?\.
  (function 
   (lambda (stream char)
     (and (eq reader::context 'dotted-pair) 
	  (reader::error "No more than one `.' allowed in list"))
     (throw 'dotted-pair nil))))

;; '(#\a . #\b)
;; '(a . (b . c))

;; Vectors: [a b]
(set-macro-character ?\[
  (function
   (lambda (stream char)
     (reader::read-sexp (1- (point))
       (let ((reader::context 'vector))
	 (catch 'read-vector
	   (let ((reader::context 'vector)
		 reader::stack)
	     (while t (push (reader::read-from-buffer stream 't)
			    reader::stack)))))))))

(set-macro-character ?\] 
  (function 
   (lambda (stream char)
     (if (eq reader::context 'vector)
	 (throw 'read-vector (apply 'vector (nreverse reader::stack)))
       (reader::error "`]' doesn't end a vector"))))) 

;; Quote and backquote/comma macro
(set-macro-character ?\'
  (function
   (lambda (stream char)
     (reader::read-sexp (1- (point))
       (list (reader::read-sexp (point) 'quote)
	     (reader::read-from-buffer stream 't))))))

(set-macro-character ?\`
  (function
   (lambda (stream char)
     (if (= (following-char) ?\ )
	 ;; old backquote syntax. This is ambigous, because 
	 ;; (`(sexp)) is a valid form in both syntaxes, but 
	 ;; unfortunately not the same. 
	 ;; old syntax: read -> (` (sexp))
	 ;; new syntax: read -> ((` (sexp)))
	 (reader::read-sexp (1- (point)) '\`)
       (reader::read-sexp (1- (point))
	 (list (reader::read-sexp (point) '\`)
	       (reader::read-from-buffer stream 't)))))))

(set-macro-character ?\,
  (function
   (lambda (stream char)
     (cond ((eq (following-char) ?\ )
	    ;; old syntax
	    (reader::read-sexp (point) '\,))
	   ((eq (following-char) ?\@)
	    (forward-char 1)
	    (cond ((eq (following-char) ?\ )
		   (reader::read-sexp (point) '\,\@))
		  (t
		   (reader::read-sexp (- (point) 2)
		     (list 
		      (reader::read-sexp (point) '\,\@)
		      (reader::read-from-buffer stream 't))))))
	   (t
	    (reader::read-sexp (1- (point))
	      (list
	       (reader::read-sexp (1- (point)) '\,)
	       (reader::read-from-buffer stream 't))))))))

;; 'a
;; '(a b c)
;; (let ((a 10) (b '(20 30))) `(,a ,@b c))
;; the old syntax is also supported:
;; (let ((a 10) (b '(20 30))) (` ((, a) (,@ b) c)))

;; Single line character comment:  ; 
(set-macro-character ?\;
  (function
   (lambda (stream char)
     (skip-chars-forward "^\n\r")
     (throw 'reader-ignore nil))))



;; Dispatch character character #
(make-dispatch-macro-character ?\#)

(defsubst reader::check-0-infix (n)
  (or (= n 0) 
      (reader::error "Numeric infix argument not allowed: %d" n)))


(defalias 'search-forward-regexp 're-search-forward)

;; nested multi-line comments #| ... |#
(set-dispatch-macro-character ?\# ?\|
  (function 
   (lambda (stream char n)
     (reader::check-0-infix n)
     (let ((counter 0))
       (while (search-forward-regexp "#|\\||#" nil t)
	 (if (string-equal
	      (buffer-substring
	       (match-beginning 0) (match-end 0))
	      "|#")
	     (cond ((> counter 0)
		    (decf counter))
		   ((= counter 0)
		    ;; stop here
		    (goto-char (match-end 0))
		    (throw 'reader-ignore nil))
		   ('t
		    (reader::error "Unmatching closing multicomment")))
	   (incf counter)))
       (reader::error "Unmatching opening multicomment")))))

;; From cl-packages.el
(defconst reader::symbol-characters "[A-Za-z0-9-_!@$%^&*+=|~{}<>/]")
(defconst reader::symbol-regexp2
  (format "\\(%s+\\)" reader::symbol-characters))

(set-dispatch-macro-character ?\# ?\:
  (function
   (lambda (stream char n)
     (reader::check-0-infix n)
     (or (looking-at reader::symbol-regexp2)
	 (reader::error "Invalid symbol read syntax"))
     (goto-char (match-end 0))
     (make-symbol 
      (buffer-substring (match-beginning 0) (match-end 0))))))

;; Function quoting: #'<function>
(set-dispatch-macro-character ?\# ?\'
  (function
   (lambda (stream char n)
     (reader::check-0-infix n)
     ;; Probably should test if cl is required by current buffer.
     ;; Currently, cl will always be a feature because cl-read requires it.
     (reader::read-sexp (- (point) 2)
       (list 
	(reader::read-sexp (point) (if (featurep 'cl)  'function* 'function))
	(reader::read-from-buffer stream 't))))))

;; Character syntax: #\<char> 
;; Not yet implemented: #\Control-a #\M-C-a etc. 
;; This definition is not used - the next one is more general.
'(set-dispatch-macro-character ?# ?\\
  (function 
   (lambda (stream char n)
     (reader::check-0-infix n)
     (let ((next (following-char))
           name)
       (if (not (and (<= ?a next) (<= next ?z)))
           (progn (forward-char 1) next)
         (setq next (reader::read-from-buffer stream t))
         (cond ((symbolp next) (setq name (symbol-name next)))
               ((integerp next) (setq name (int-to-string next))))
         (if (= 1 (length name))
             (string-to-char name)
           (case next
             (linefeed  ?\n)
             (newline   ?\r)
             (space     ?\ )
             (rubout    ?\b)
             (page      ?\f)
             (tab       ?\t)
             (return    ?\C-m)
             (t
              (reader::error "Unknown character specification `%s'"
			     next))))))))
  )

(defvar reader::special-character-name-table
  '(("linefeed"	. ?\n)
    ("newline"	. ?\r)
    ("space"	. ?\ )
    ("rubout"	. ?\b)
    ("page"	. ?\f)
    ("tab"        . ?\t)
    ("return"	. ?\C-m)))

(set-dispatch-macro-character ?# ?\\
  (function 
   (lambda (stream char n)
     (reader::check-0-infix n)
     (forward-char -1)
     ;; We should read in a special package to avoid creating symbols.
     (let ((symbol (reader::read-from-buffer stream t))
	   (case-fold-search t)
	   name modifier character char-base)
       (setq name (symbol-name symbol))
       (if (string-match "^\\(meta-\\|m-\\|control-\\|c-\\)+" name)
	   (setq modifier (substring name
				     (match-beginning 1)
				     (match-end 1))
		 character (substring name (match-end 1)))
	 (setq character name))
       (setq char-base 
	     (cond ((= (length character) 1)
		    (string-to-char character))
		   ('t 
		    (cdr (assoc character 
				reader::special-character-name-table)))))
       (or char-base 
	   (reader::error
	    "Unknown character specification `%s'" character))
	
       (and modifier
	    (progn 
	      (and (string-match "control-\\|c-" modifier)
		   (decf char-base 32))
	      (and (string-match "meta-\\|m-" modifier)
		   (incf char-base 128))))
       char-base))))

;; '(#\meta-space #\tab #\# #\> #\< #\a #\A  #\return #\space)
;; (eq #\m-tab ?\M-\t)
;; (eq #\c-m-x #\m-c-x)
;; (eq #\Meta-Control-return #\M-C-return)
;; (eq #\m-m-c-c-x #\m-c-x)
;; #\C-space #\C-@ ?\C-@



;; Read and load time evaluation:  #.<form>
;; Not yet implemented: #,<form>
(set-dispatch-macro-character ?\# ?\.
  (function 
   (lambda (reader::stream reader::char reader::n)
     (reader::check-0-infix reader::n)
     ;; This eval will see all internal vars of reader, 
     ;; e.g. stream, reader::recursive-p.  Anything that might be bound.
     ;; We must use `read' here rather than read-from-buffer with 'recursive-p
     ;; because the expression must not have unresolved #n#s in it anyway.
     ;; Otherwise the top-level expression must be completely read before
     ;; any embedded evaluation(s) occur(s).  CLtL2 does not specify this.
     ;; Also, call `read' so that it may be customized, by e.g. Edebug
     (eval (read reader::stream)))))
;; '(#.(current-buffer) #.(get-buffer "*scratch*"))

;; Path names (kind of):  #p<string>, #P<string>,
(set-dispatch-macro-character ?\# ?\P
  (function 
   (lambda (stream char n)
     (reader::check-0-infix n)
     (let ((string (reader::read-from-buffer stream 't)))
       (or (stringp string) 
	   (reader::error "Pathname must be a string: %s" string))
       (expand-file-name string)))))

(set-dispatch-macro-character ?\# ?\p
  (get-dispatch-macro-character ?\# ?\P))

;; #P"~/.emacs"
;; #p"~root/home" 

;; Feature reading:  #+<feature>,  #-<feature>
;; Not yet implemented: #+<boolean expression>, #-<boolean expression>


(defsubst reader::read-feature (stream char n flag)
  (reader::check-0-infix n)
  (let (;; Use the original reader to only read the feature.
	;; This is not exactly correct without *read-suppress*.
	;; Also Emacs 18 read goes one too far,
	;; so we assume there is a space after the feature.
	(feature (reader::original-read stream))
	(object (reader::read-from-buffer stream 't)))
    (if (eq (featurep feature) flag)
	object
      ;; Ignore it.
      (throw 'reader-ignore nil))))

(set-dispatch-macro-character ?\# ?\+
  (function 
   (lambda (stream char n)
     (reader::read-feature stream char n t))))

(set-dispatch-macro-character ?\# ?\-
  (function 
   (lambda (stream char n)
     (reader::read-feature stream char n nil))))

;; (#+cl loop #+cl do #-cl while #-cl t (body))




;; Shared structure reading: #<n>=, #<n>#

;; Reading of sexpression with shared and circular structure read
;; syntax  is done in two steps:
;; 
;; 1. Create an sexpr with unshared structures, just as the ordinary
;;    read macros do, with two exceptions: 
;;    - each label (#<n>=) creates, as a side effect, a symbolic
;;      reference for the sexpr that follows it
;;    - each reference (#<n>#) is replaced by the corresponding
;;      symbolic reference. 
;;
;; 2. This non-cyclic and unshared lisp structure is given to the
;;    function `reader::restore-shared-structure' (see
;;    `reader::read-from-buffer'), which simply replaces
;;    destructively all symbolic references by the lisp structures the
;;    references point at. 
;;
;; A symbolic reference is an uninterned symbol whose name is obtained
;; from the label/reference number using the function `int-to-string': 
;;
;; There are two non-locally used variables (bound in
;; `reader::read-from-buffer') which control shared structure reading: 
;; `reader::shared-structure-labels': 
;;	A list of integers that correspond to the label numbers <n> in
;;      the string currently read. This is used to avoid multiple
;;      definitions of the same label.
;; `reader::shared-structure-references': 
;;      The list of symbolic references that will be used as temporary
;;      placeholders for the shared objects introduced by a reference
;;      with the same number identification.

(set-dispatch-macro-character ?\# ?\=
  (function 
   (lambda (stream char n)
     (and (= n 0) (reader::error "0 not allowed as label"))
     ;; check for multiple definition of the same label
     (if (memq n reader::shared-structure-labels)
	 (reader::error "Label defined twice")
       (push n reader::shared-structure-labels))
     ;; create an uninterned symbol as symbolic reference for the label
     (let* ((string (int-to-string n))
	    (ref (or (find string reader::shared-structure-references
			   :test 'string=)
		     (first 
		      (push (make-symbol string) 
			    reader::shared-structure-references)))))
       ;; the link between the symbolic reference and the lisp
       ;; structure it points at is done using the symbol value cell
       ;; of the reference symbol.
       (setf (symbol-value ref) 
	     ;; this is also the return value 
	     (reader::read-from-buffer stream 't))))))


(set-dispatch-macro-character ?\# ?\#
  (function
   (lambda (stream char n)
     (and (= n 0) (reader::error "0 not allowed as label"))
     ;; use the non-local variable `reader::recursive-p' (from the reader
     ;; main loop) to detect labels at the top level of an sexpr.
     (if (not reader::recursive-p)
	 (reader::error "References at top level not allowed"))
     (let* ((string (int-to-string n))
	    (ref (or (find string reader::shared-structure-references
			   :test 'string=)
		     (first
		      (push (make-symbol string) 
			    reader::shared-structure-references)))))
       ;; the value of reading a #n# form is a reference symbol
       ;; whose symbol value is or will be the shared structure. 
       ;; `reader::restore-shared-structure' then replaces the symbol by
       ;; its value.
       ref))))

(defun reader::restore-shared-structure (obj)
  ;; traverses recursively OBJ and replaces all symbolic references by
  ;; the objects they point at. Remember that a symbolic reference is
  ;; an uninterned symbol whose value is the object it points at. 
  (cond 
   ((consp obj)
    (loop for rest on obj
	  as lastcdr = rest
	  do
	  (if;; substructure is a symbolic reference
	      (memq (car rest) reader::shared-structure-references)
	      ;; replace it by its symbol value, i.e. the associated object
	      (setf (car rest) (symbol-value (car rest)))
	    (reader::restore-shared-structure (car rest)))
	  finally 
	  (if (memq (cdr lastcdr) reader::shared-structure-references)
	      (setf (cdr lastcdr) (symbol-value (cdr lastcdr)))
	    (reader::restore-shared-structure (cdr lastcdr)))))
   ((vectorp obj)
    (loop for i below (length obj)
	  do
	  (if;; substructure  is a symbolic reference
	      (memq (aref obj i) reader::shared-structure-references)
	      ;; replace it by its symbol value, i.e. the associated object
	      (setf (aref obj i) (symbol-value (aref obj i)))
	    (reader::restore-shared-structure (aref obj i))))))
  obj)


;; #1=(a b #3=[#2=c])
;; (#1=[#\return #\a] #1# #1#)
;; (#1=[a b c] #1# #1#)
;; #1=(a b . #1#)

;; Creation and initialization of an internal standard readtable. 
;; Do this after all the macros and dispatch chars above have been defined.

(defconst reader::internal-standard-readtable (copy-readtable)
  "The original (CL-like) standard readtable. If you ever modify this
readtable, you won't be able to recover a standard readtable using
\(copy-readtable nil\)")


;; Replace built-in functions that call the built-in reader
;; 
;; The following functions are replaced here: 
;;
;; read			by	reader::read
;; read-from-string	by	reader::read-from-string
;;
;; eval-expression	by	reader::eval-expression
;; Why replace eval-expression? Not needed for Lucid Emacs since the
;; reader for arguments is also written in Lisp, and so may be overridden.
;;
;; eval-current-buffer  by	reader::eval-current-buffer
;; eval-buffer		by	reader::eval-buffer
;; original-eval-region by	reader::original-eval-region


;; Temporary read buffer used for reading from strings
(defconst reader::tmp-buffer
  (get-buffer-create " *CL Read*"))

;; Save a pointer to the original read function
(or (fboundp 'reader::original-read)
    (fset 'reader::original-read  (symbol-function 'read)))

(defun reader::read (&optional stream reader::recursive-p)
  "Read one Lisp expression as text from STREAM, return as Lisp object.
If STREAM is nil, use the value of `standard-input' \(which see\).
STREAM or the value of `standard-input' may be:
 a buffer \(read from point and advance it\)
 a marker \(read from where it points and advance it\)
 a string \(takes text from string, starting at the beginning\)
 t \(read text line using minibuffer and use it\).

This is the cl-read replacement of the standard elisp function
`read'. The only incompatibility is that functions as stream arguments
are not supported."
  (if (not cl-read-active)
      (reader::original-read stream)
    (if (null stream)			; read from standard-input
	(setq stream standard-input))

    (if (eq stream 't)			; read from minibuffer
	(setq stream (read-from-minibuffer "Common Lisp Expression: ")))

    (cond 

     ((bufferp stream)			; read from buffer
      (reader::read-from-buffer stream reader::recursive-p))

     ((markerp stream)			; read from marker
      (save-excursion 
	(set-buffer (marker-buffer stream))
	(goto-char (marker-position stream))
	(reader::read-from-buffer (current-buffer) reader::recursive-p)))

     ((stringp stream)			; read from string
      (save-excursion
	(set-buffer reader::tmp-buffer)
	(auto-save-mode -1)
	(erase-buffer)
	(insert stream)
	(goto-char (point-min))
	(reader::read-from-buffer reader::tmp-buffer reader::recursive-p)))
     (t 
      (reader::error "Not a valid stream: %s" stream)))))

;; read-from-string
;; save a pointer to the original `read-from-string' function
(or (fboundp 'reader::original-read-from-string)
    (fset 'reader::original-read-from-string
	  (symbol-function 'read-from-string)))

(defun reader::read-from-string (string &optional start end)
  "Read one Lisp expression which is represented as text by STRING.
Returns a cons: (OBJECT-READ . FINAL-STRING-INDEX).
START and END optionally delimit a substring of STRING from which to read;
they default to 0 and (length STRING) respectively.

This is the cl-read replacement of the standard elisp function
`read-from-string'.  It uses the reader macros in *readtable* if
`cl-read-active' is non-nil in the current buffer."

  ;; Does it really make sense to have read-from-string depend on
  ;; what the current buffer happens to be?   Yes, so code that
  ;; has nothing to do with cl-read uses original reader.
  (if (not cl-read-active)
      (reader::original-read-from-string string start end)
    (or start (setq start 0))
    (or end (setq end (length string)))
    (save-excursion
      (set-buffer reader::tmp-buffer)
      (auto-save-mode -1)
      (erase-buffer)
      (insert (substring string 0 end))
      (goto-char (1+ start))
      (cons 
       (reader::read-from-buffer reader::tmp-buffer nil)
       (1- (point))))))

;; (read-from-string "abc (car 'a) bc" 4)
;; (reader::read-from-string "abc (car 'a) bc" 4)
;; (read-from-string "abc (car 'a) bc" 2 11)
;; (reader::read-from-string "abc (car 'a) bc" 2 11)
;; (reader::read-from-string "`(car ,first ,@rest)")
;; (read-from-string ";`(car ,first ,@rest)")
;; (reader::read-from-string ";`(car ,first ,@rest)")

;; We should replace eval-expression, too, so that it reads (and
;; evals) in the current buffer.  Alternatively, this could be fixed
;; in C.  In Lemacs 19.6 and later, this function is already written
;; in lisp, and based on more primitive read functions we already
;; replaced. The reading happens during the interactive parameter
;; retrieval, which is written in lisp, too.  So this replacement of
;; eval-expresssion is only required fro (FSF) Emacs 18 (and 19?).

(or (fboundp 'reader::original-eval-expression)
    (fset 'reader::original-eval-expression 
          (symbol-function 'eval-expression)))

(defun reader::eval-expression (reader::expression)
  "Evaluate EXPRESSION and print value in minibuffer.
Value is also consed on to front of variable `values'."
  (interactive 
   (list
    (car (read-from-string
          (read-from-minibuffer 
           "Eval: " nil 
           ;;read-expression-map ;; not for emacs 18
           nil ;; use default map
           nil ;; don't do read with minibuffer current.
           ;; 'edebug-expression-history ;; not for emacs 18
           )))))
  (setq values (cons (eval reader::expression) values))
  (prin1 (car values) t))

(require 'elisp-eval-region "eval-region")
(require 'advice)


;; installing/uninstalling the cl reader
;; These two should always be used in pairs, or just install once and
;; never uninstall. 
(defun cl-reader-install ()
  (interactive)
  (fset 'read 			'reader::read)
  (fset 'read-from-string 	'reader::read-from-string)
  (fset 'eval-expression 	'reader::eval-expression)
  (install-elisp-eval-region))

(defun cl-reader-uninstall ()
  (interactive)
  (fset 'read 		       
	(symbol-function 'reader::original-read))
  (fset 'read-from-string	
	(symbol-function 'reader::original-read-from-string))
  (fset 'eval-expression
	(symbol-function 'reader::original-eval-expression))
  (uninstall-elisp-eval-region))

;; Globally installing the cl-read replacement functions is safe, even
;; for buffers without cl read syntax. The buffer local variable
;; `cl-read-active' controls whether the replacement funtions of this
;; package or the original ones are actually called.
(cl-reader-install)
(cl-reader-uninstall)

;; Advise the redefined eval-region
(defadvice eval-region (around cl-read activate)
  "Use the reader::read instead of the original read if cl-read-active."
  (with-elisp-eval-region (not cl-read-active)
    (ad-do-it)))
;;(ad-unadvise 'eval-region)


(add-hook 'emacs-lisp-mode-hook 'cl-reader-autoinstall-function)

'(defvar read-syntax)

'(defun cl-reader-autoinstall-function () 
  "Activates the Common Lisp style reader for emacs-lisp-mode buffers,
if the property line has a local variable setting like this: 
\;\; -*- Read-Syntax: Common-Lisp -*-"
  ;; this is a hack to avoid recursion in the case that the prop line 
  ;; containes "Mode: emacs-lisp" entry
  (or (boundp 'local-variable-hack-done)
      (let (local-variable-hack-done
	    (case-fold-search t))
	;; Usually `hack-local-variables-prop-line' is called only after
	;; installation of the major mode. But we need to know about the
	;; local variables before that, so we call the local variable hack
	;; explicitly here:
	(hack-local-variables-prop-line 't)
	;; But hack-local-variables-prop-line not defined in emacs 18.
	(cond 
	 ((and (boundp 'read-syntax)
	       read-syntax
	       (string-match "^common-lisp$" (symbol-name read-syntax)))
	  (require 'cl-read)
	  (make-local-variable 'cl-read-active)
	  (setq cl-read-active 't))))))

;; Emacs 18 doesnt have hack-local-variables-prop-line.  So use this instead.
(defun cl-reader-autoinstall-function ()
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (cond ((re-search-forward 
	      "read-syntax: *common-lisp" 
	      (save-excursion 
		(end-of-line)
		(point))
	      t)
	     (require 'cl-read)
	     (make-local-variable 'cl-read-active)
	     (setq cl-read-active t))))))


(run-hooks 'cl-read-load-hooks)
;; end cl-read.el
