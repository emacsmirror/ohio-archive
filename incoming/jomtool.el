;;; jomtool.el --- generator of Java source code from OMTool files

;; Copyright (C) 1996-2000 Neil W. Van Dyke

;; Author:    Neil W. Van Dyke <nwv@acm.org>
;; Created:   04-Sep-1996
;; Version:   1.4
;; Keywords:  jomtool java omt omtool design code-generator
;; X-URL:     http://www.media.mit.edu/~nwv/jomtool/

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.
;;
;; This software is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;#############################################################################
;;; Commentary:

;; Jomtool generates Java code files from OMTool object-oriented design files.

;; Writing in Jul-2000: I don't expect that anyone will actually run this
;; anymore, seeing as how OMTool is all but extinct, so I'm releasing this code
;; only for the historically curious.

;; There's also a front-end shell script so that you can run Jomtool from
;; makefiles as part of a larger build process.  Email me if you want it.

;;---------------------------------------------------------------- Dependencies

;; Jomtool was developed under GNU Emacs 19.34.  It will probably work with
;; other recent versions, and may also work with the prodigal XEmacs.

;; Only version 1.2 of OMTool -- "(omt_version 4 2)" -- is supported, because
;; that's all I presently have access to.  I'd be very appreciative if someone
;; could tell me how to get a free license to use 2.0 or a later version.

;; Requires the Eieio 0.7 object system by Eric M. Ludlam, available from
;; `ftp://ftp.ultranet.com/pub/zappo/'.

;; Also requires the `cl-macs' portion of the Common Lisp-like extensions,
;; which is part of the standard Emacs distribution.

;; There is a `require' for the `time-stamp' standard package.  If this package
;; is loaded, the timestamp strings will be in one of my favorite formats.
;; Otherwise the `current-time-string' format will be used.

;; If `window-system' is non-nil, then `faces' is used to fontify the log 
;; buffer.

;;----------------------------------------------------------- What Jomtool Does

;; Jomtool does lots of stuff, and someday I may document said stuff.  Sorry,
;; no time for such luxuries now.

;;------------------------------------------------ Preparing Your OMTool Models

;; Jomtool will work with most correct OMTool models as-is, and it tries to do
;; something reasonable with many kinds of C++-isms that may have snuck into
;; your models.  A few tips you'll find helpful...

;; Use the C++ language mode.  This lets you specify more semantics that
;; Jomtool can use.

;; For each model, change it to not force unique method names.  To set the
;; model-wide defaults, choose menu item "Model -> Annotations -> Unique Names"
;; and ensure that "Unique methods" field is off.

;; Be careful of semicolons in the text portions of your model.  In a certain
;; case (i.e., in strings without spaces), the way that OMTool encodes them
;; breaks the kludgey way that Jomtool uses the Lisp reader.  For similar
;; reasons, try to avoid using square brackets except in type specifications
;; (which Jomtool has a builtin workaround for).

;; Add arrowheads to associations to indicate in which direction(s) they should
;; be implemented.  Add role names for the ends with arrowheads, unless you
;; want Jomtool to generate the names.  The generated names are just munged
;; versions of the class names with the first letter in lowercase.

;; When naming classes that you do *not* want Jomtool to generate code for, use
;; the fully-qualified class name (e.g., "java.awt.Component").  When naming
;; classes you *do* want Jomtool to gen code for, do not specify any package
;; name (e.g., "MyComponent").

;; When specifying array modifiers for types append them to the base type name,
;; as in the example:
;; 
;;     main(argv:String[])
;;
;; Do NOT append the modifiers to the name, like:
;;
;;     main(argv[]:String)    // ERROR!
;;
;; Don't worry, Jomtool will generate the Java code correctly.

;; There are some code generation parameters that can be set on a per-model
;; basis.  You can make an OMTool sheet named "(Jomtool Parameters)" and put
;; comments on it of the format:
;;
;;     <attribute> = <value>
;; 
;; For example:
;;
;;     TargetDir = "/home/billg/my-java-code/"
;;
;; and:
;;
;;     MungeIdents = yes
;;
;; See the "Option Variables" section of this code for what parameters can be
;; set.  The option variables provide the global defaults.

;;------------------------------------------------------------- Running Jomtool

;; To run Jomtool, just type:
;;
;;     M-x jomtool RET
;;
;; and enter the filename of your OMTool file.  Emacs filename completion will
;; be used.

;; Jomtool will run for a while (a few seconds to several minutes or more),
;; with oodles of messages rapidly flashing in the echo area.

;; After Jomtool is finished, you'll see the Jomtool log buffer.  The top of
;; the log shows the start and finish times and the error and warning counts.
;; The remainder of the log shows a hierarchy of messages for debugging
;; purposes.  If you're running Emacs with a window system, you'll notice some
;; moderate use of fontification.

;;------------------------------------------------------ Editing Generated Code

;; Much of the contents generated Java code files will be lost the next time
;; Jomtool generates that file.  What *will* be preserved is code between the
;; "cookie" pairs of "//@ImportsBegin" and "//@ImportsEnd", and
;; "//@MethodBodyBegin" and "//@MethodBodyEnd".  Everything else is derived
;; from the OMTool model, and changes to them should be made using OMTool.

;;------------------------------------------------------------ Acknowledgements

;; Thank you to Michael Kalman <mdk@cs.brown.edu> and Michael Radwin
;; <mjr@cs.brown.edu> for providing me with OMTool models to use as test data.

;; Also thanks to Richard Stallman <rms@gnu.ai.mit.edu>, Eric Ludlam
;; <zappo@choochoo.ultranet.com>, and the numerous other contributors to the
;; GNU Emacs toolset.

;;------------------------------------------- Things for the Author to Maybe Do

;; What commercial software vendor not only doesn't provide a feature list, but
;; gives you in lieu of that a complete list of known bugs/limitations?
;; (Actually, I did some of the things listed below already, but forgot to
;; remove them from the to-do list.)

;; Finish adding initializer support.

;; In the constructor cookie, don't include the class name.

;; Look into attribute descriptions.

;; Generate descriptions for attributes which implement associations.

;; Check to make sure that interface classes only have abstract methods.

;; Do something about generalization associations subclassing interfaces.
;; Either do what the user expects or (we don't do MI yet, remember), or give
;; an error.

;; If we have a method body for an abstract method, do something with it.

;; Made automatic return in empty method body support types such as "int" and
;; arrays.

;; Support specification and code gen of what exceptions a method throws.

;; It would be really nice if we could deal with class name renames, to
;; automatically preserve both the method bodies for the class and the
;; method bodies which have that class name in their signatures.

;; Support protection-specification of attributes if we don't do accessors!

;; Add generation of accessors.

;; Add check-in/check-out from version control system.

;; Add handling of handling of multiple inheritance in the OMTool model.

;; Add sorting of fields in generated code.

;; Add support for interfaces.

;; See what it does with class names that start with digits, just for grins.

;; Add use of "synchronized" appropriately, such as to accessors.

;; Replace some of the existing jomtool-error and jomtool-assert with
;; jomtool-log-*.

;; Add reporting of line numbers in the extraction error messages.

;; If attribute type is missing, but default value is given, maybe make it
;; infer type from value.

;; Add generation of warning for association with no implementable roles.

;; Make `jomtool-file' interactive.

;; Maybe do something with the default value of method (e.g., method body).

;; Make `jomtool-desc' methods for everyone and use them.

;; Add support for Java class initializer.

;; Switch `jomtool-extract(jomtool-class)' around from event->state to
;; state->event.  At least at this point that would let me reduce code size.

;; Make it complain about using Java reserved words for identifiers.

;; Turn off font-lock while generating Java files, to make it go faster.

;; Make sure `nil' and `t' are correctly scanned in the OMTool files.

;; Add support for association-as-class and maybe link attributes.

;; Add support for enums.

;; Do something with "anno cxx_field_access" in classes.

;;#############################################################################
;;; Change Log:

;; [Version 1.4, 22-Jul-2000, nwv@acm.org] Minor changes for public source
;; release.  Released under the GPL.

;; [Version 1.3, 10-Jun-1997, nwv@acm.org] Qualifiers are now correctly
;; supported.  Jomtool test functions are now disabled.  Some to-do list items
;; added.  It no longer barfs on unrecognized class annotations.  Added comment
;; about unique method names.  Fixed bug that resulted in the automatic
;; "return" not always being added to empty method body.  Don't leave orphan
;; method bodies around if they just have whitespace in them.  Added generation
;; of stub messages for empty method bodies.  Added forcing of class to
;; abstract if it has any abstract methods.  Added support for class
;; description indicating implements and is-interface.  Added processing and
;; printing of class and method descriptions.  Made terse for noninteractive
;; use.  Wrote shell script front-end.  Changed constructor method name to
;; "NEW".  Adding support for reading a file boilerplate comment from a file.
;; Fixed jomtool-log for command-line use.  Removed extraneous "-" that somehow
;; slipped into rarely-called code.  Made so when in noninteractive mode,
;; doesn't print message about not crunching/generating external classes.  Made
;; force methods to abstract if in interface.  Added support for specifying
;; methods as synchronized.  Removed ugly warning about not modifying file
;; directly unless know what doing.  Added support for "@final" in methods.
;; Made methods with empty bodies with return types of "int" and "float" return
;; valid types in stubs.  Made stubs also do right return value for "boolean".
;; Made add RCS "Source" variable.  Made allow final attributes.  Changed stub
;; messages from System.out to System.err.

;; [Version 1.2, 24-Oct-1996, nwv@acm.org] Made method visibility default to
;; `public'.  Replaced uses of `jomtool-add-*(jomtool-class)' with
;; `jomtool-oappend-fast'.  Same with `jomtool-add-genrel(jomtool-model)'.
;; Removed todo item about supporting classes not defined in model, since
;; that's already supported.  Made correctly parse array modifiers for types
;; and gen code for them.  Fixed stupid bug in how `jomtool-java-code
;; (jomtool-role)' chose collection class to use, and also made it qualify the
;; collection classes with the package name.  Made parser not puke on
;; `binary_constraint' and `constraint_arc'.  Changed `jomtool-forms-parse' to
;; generate warning on unrecognized construct, instead of barfing.  Added
;; warning for unimplemented associations.  Made constructor method names get
;; munged with upper-case first letter.  Made `jomtool' prompt for file name.
;; Comment changes.

;; [Version 1.1, 18-Oct-1996, nwv@acm.org] Role name munging for Java attribute
;; names now uses lower-case initial letter instead of upper-case.  If needed
;; role name is missing, a warning is generated and a name is derived from the
;; class name (just the munged class name with lower-case first letter).  Made
;; changes to the Lisp source code comments.

;; [Version 1.0, 08-Oct-1996, nwv@acm.org] First release.

;;#############################################################################
;;; Code:

;;-------------------------------------------------------- Package Dependencies

(require 'cl)
(require 'eieio)
(require 'time-stamp)
(if window-system (require 'faces))

;;------------------------------------------------------ Package Identification
 
(defconst jomtool-version "1.4")
(defconst jomtool-author "Neil W. Van Dyke <nwv@acm.org>")
(defconst jomtool-maintainer-address "nwv@acm.org")
(defconst jomtool-vc-id
  "$Id: jomtool.el,v 1.174 2000/07/22 21:06:56 nwv Exp $")
   
;;------------------------------------------------------------ Option Variables

(defvar jomtool-class-file-warning
  ""
  "*Comment to appear in Java code files, warning user file is generated.")

(defvar jomtool-boilerplate-fname nil
  "*!!!")
  
(defvar jomtool-default-arg-type-default "Object"
  "*Default type for method arguments if type not specified.
Overridable by `DefaultArgType=<string>' in the Jomtool parameters sheet.")

(defvar jomtool-default-attr-type-default "Object"
  "*Default type for attributes if type not specified.
Overridable by `DefaultAttrType=<string>' in the Jomtool parameters sheet.")

(defvar jomtool-indent-size 2
  "*Number of spaces to use per indentation level in Java code.")

(defvar jomtool-legal-message-default
  "Copyright (c) ____ ____, All rights reserved."
  "*Copyright message to appear in constant in Java classes.
Overridable by `LegalMessage=<string>' in the Jomtool parameters sheet.")

(defvar jomtool-munge-idents-default t
  "*If non-nil, adjust case of identifiers to fit Java conventions.
Overridable by `MungeIdents=<boolean>' in the Jomtool parameters sheet.")

(defvar jomtool-package-default nil
  "*Package name of generated code.
Overridable by `Package=<string>' in the Jomtool parameters sheet.")

(defvar jomtool-params-sheet-regexp "^(Jomtool Parameters)$"
  "*Pattern of OMTool sheet that contains Jomtool parameters.
Don't change this without good reason.")

(defvar jomtool-target-dir-default "./java"
  "*Target directory for Java code files.
Overridable by `TargetDir=<string>' in the Jomtool parameters sheet.")

(defvar jomtool-verbose (not noninteractive)
  "*!!!")

(defvar jomtool-use-final-methods-default nil
  "*If non-nil, some generated Java methods will be made final.
Overridable by `UseFinalMethods=<boolean>' in the Jomtool parameters sheet.")

(defvar jomtool-log-indent-size 4
  "*Number of spaces per level of indentation in Jomtool log.")

;;------------------------------------------------------------------- Constants

(defconst jomtool-cookie-id-sym-to-str-alist
  '((imports-begin     . "ImportsBegin")
    (imports-end       . "ImportsEnd")
    (method-body-begin . "MethodBodyBegin")
    (method-body-end   . "MethodBodyEnd")))
 
(defconst jomtool-cookie-id-str-to-sym-alist
  (mapcar (function (lambda (n) (cons (cdr n) (car n))))
          jomtool-cookie-id-sym-to-str-alist))

(defconst jomtool-cookie-prefix "//@")

(defconst jomtool-cookie-regexp
  (concat "^[ \t]*"
          (regexp-quote jomtool-cookie-prefix)
          "\\([a-zA-Z]+\\)\\([ \t]+\\(.*\\)\\)?$"))

(defconst jomtool-log-buffer-name "*Jomtool Log*")

;;------------------------------------------------------------ Global Variables

(defvar jomtool-boilerplate nil
  "!!!")
  
(defvar jomtool-current-class nil
  "Placeholder for current class.
Only picked up through dynamic scoping; `let', do not `set'.")

(defvar jomtool-current-model nil
  "Placeholder for current model.
Only picked up through dynamic scoping; `let', do not `set'.")

(defvar jomtool-log-error-count nil
  "Count of logged errors.")

(defvar jomtool-log-nesting nil
  "Depth of log message hierarchy nesting.")

(defvar jomtool-log-warning-count nil
  "Count of logged warnings.")

;;-------------------------------------------------------------- Generic: Faces

;;(defun jomtool-insert-with-face (str face)
;;  (jomtool-set-face-region (prog1 (point) (insert str)) (point) face))
 
(defun jomtool-set-face-region (begin end face)
  (put-text-property begin end 'face face))

;;------------------------------------------------------------- Generic: String

(defun jomtool-if-nonblank (str &optional str-if-blank)
  (if (jomtool-string-blank str)
      str-if-blank
    str))

(defun jomtool-split-lines (str)
  (let ((len (length str))
        (list '())
        (start 0))
    (while (< start len)
      (if (string-match "\n" str start)
          (setq list (append list
                             (list (substring str start (match-beginning 0))))
                start (match-end 0))
        (setq list (append list (list (substring str start)))
              start len)))
    list))

(defun jomtool-str-to-bool (str)
  (let ((s (intern (downcase (format "%s" str)))))
    (cond
     ((memq s '(yes y true t ja))   t)
     ((memq s '(no n false f nein)) nil)
     (t (jomtool-error "jomtool-str-to-bool: Invalid string \"%s\"." str)))))

(defun jomtool-string-blank (str)
  (or (null str) (string-match "^[ \t]*$" str)))

;;--------------------------------------------------------------- Generic: Time

(defun jomtool-cur-time-str ()
  (jomtool-time-str (current-time)))
  
(defun jomtool-time-str (time)
  (if (fboundp 'time-stamp-strftime)
      (time-stamp-strftime "%02H:%02M:%02S %02d-%3b-%04y %Z" time)
    (current-time-string time)))
 
;;------------------------------------------------------ Generic: Eieio Helpers

(defmacro jomtool-oappend (object attr value)
  `(let ((jomtool-oappend-object ,object))
     (oset jomtool-oappend-object ,attr
           (append (oref jomtool-oappend-object ,attr)
                   (list ,value)))))

(defmacro jomtool-oappend-fast (object attr value)
  `(oset ,object ,attr (append (oref ,object ,attr)
                               (list ,value))))

;;------------------------------------------------------------------- Misc. Goo
  
(defun jomtool-description-comment (desc indent)
  (if desc
      (concat (jomtool-indent indent) "/**\n"
              (mapconcat (function
                          (lambda (n)
                            (concat (jomtool-indent indent)
                                    " * "
                                    (if (consp n)
                                        (format "@%s %s" (car n) (cdr n))
                                      n)
                                    "\n")))
                         desc
                         "")
              (jomtool-indent indent) " */\n")
    ""))

(defun jomtool-object-key-and-name (object)
  (let ((name (oref object name))
        (key (oref object key)))
    (concat key
            (if name
                (concat " (\"" name "\")")
              ""))))

(defmacro jomtool-oset-munge-ident-maybe (object attr &optional first-letter)
  `(oset ,object
         ,attr
         (jomtool-munge-ident-maybe (oref ,object ,attr)
                                    ,first-letter)))

(defmacro jomtool-oset-munge-ident (object attr &optional first-letter)
  `(oset ,object
         ,attr
         (jomtool-munge-ident (oref ,object ,attr) ,first-letter)))

(defmacro jomtool-munge-idents ()
  (oref jomtool-current-model munge-idents))

(defun jomtool-fix-array-mods (o)
  (let ((type (oref o type)))
    (if type
        (if (string-match "^\\([^\\[]*\\)\\(\\[.*\\)$" type)
            (progn
              (oset o type (match-string 1 type))
              (oset o array-mods (match-string 2 type)))))))
    
(defun jomtool-type-and-name (o)
  (concat
   (or (oref o type) "???")
   " "
   (or (oref o name) "???")
   (or (oref o array-mods) "")))

;;-------------------------------------------------------------- Error-Handling

(defun jomtool-assert (condition format-string &rest args)
  (or condition
      (apply 'jomtool-error format-string args)))

(defun jomtool-error (format-string &rest args)
  (apply 'error format-string args))
  
;;--------------------------------------------------------------------- Logging

(defun jomtool-log (format &rest args)
  (let ((msg (apply 'format format args)))
    (save-excursion
      (switch-to-buffer jomtool-log-buffer-name)
      (goto-char (point-max))
      (insert (make-string (* jomtool-log-nesting jomtool-log-indent-size) 32)
              msg
              "\n"))
    (message msg)))

(defmacro jomtool-log-activity (what &rest body)
  `(let ((jomtool-log-activity-what ,what))
     (jomtool-log-activity-begin jomtool-log-activity-what)
     (prog1 (progn ,@body)
       (jomtool-log-activity-end jomtool-log-activity-what t))))

(defun jomtool-log-activity-begin (what)
  (if jomtool-verbose
      (progn
        (jomtool-assert (stringp what)
                        "jomtool-log-activity-begin: `what' arg not a string.")
        (message "%s..." what)
        (jomtool-log "%s..." what)
        (setq jomtool-log-nesting (1+ jomtool-log-nesting)))))
    
(defun jomtool-log-activity-end (what &optional same)
  (if jomtool-verbose
      (progn
        (message "%s...done" what)
        (setq jomtool-log-nesting (max (1- jomtool-log-nesting) 0))
        (if (not same)
            (jomtool-log "...%s" what)))))

(defun jomtool-log-begin ()
  (if noninteractive
      (message "Jomtool %s" jomtool-version))
  (save-excursion
    (switch-to-buffer jomtool-log-buffer-name)
    (setq buffer-read-only nil
          buffer-undo-list t)
    (delete-region (point-min) (point-max))
    (insert "*** NOTE: LOG WAS NOT ENDED PROPERLY. RUN PROBABLY FAILED. ***\n"
            "Start Time:       " (jomtool-cur-time-str) "\n"
            (make-string 79 ?-) "\n"))
  (setq jomtool-log-error-count   0
        jomtool-log-nesting       0
        jomtool-log-warning-count 0))

(defun jomtool-log-blank-line ()
  (jomtool-log ""))

(defun jomtool-log-end ()
  ;; Switch to log buffer.
  (switch-to-buffer jomtool-log-buffer-name)
  ;; Add bottom line.
  (goto-char (point-max))
  (insert (make-string 79 ?-) "\n")
  ;; Delete the incomplete error line, and add more headers.
  (goto-char (point-min))
  (delete-region (point) (progn (forward-line 1) (point)))
  (forward-line 1)
  (insert "Finish Time:      " (jomtool-cur-time-str) "\n"
          "Errors/Warnings:  " (format "%d / %d\n" 
                                       jomtool-log-error-count
                                       jomtool-log-warning-count))
  ;; Fontifty.
  (if (featurep 'faces)
      (let ((header t))
        (goto-char (point-min))
        (while (not (eobp))
          (if header
              (cond
               ((looking-at "-") (setq header nil))
               ((looking-at "[ ]*\\([^:]*:\\)")
                (jomtool-set-face-region (match-beginning 1)
                                         (match-end 1)
                                         'bold)))
            (if (looking-at "[ ]*\\(\\(ERROR\\|WARNING\\):\\)[ ]*\\(.*\\)$")
                (progn
                  (jomtool-set-face-region (match-beginning 1)
                                           (match-end 1)
                                           'bold)
                  (jomtool-set-face-region (match-beginning 3)
                                           (match-end 3)
                                           'italic))))
          (forward-line 1))))
  ;; Leave the buffer like we want it.
  (goto-char (point-min))
  (set-buffer-modified-p nil)
  (setq buffer-read-only t)
  (local-set-key "q" (function (lambda ()
                                 (interactive)
                                 (kill-buffer (current-buffer)))))
  ;; Give user a message.
  (if noninteractive
      (message "Jomtool complete.")
    (message "Type `q' in the `%s' buffer to kill it."
             jomtool-log-buffer-name)))

(defun jomtool-log-error (format &rest args)
  (setq jomtool-log-error-count (1+ jomtool-log-error-count))
  (apply 'jomtool-log (concat "ERROR: " format) args))

(defun jomtool-log-info (format &rest args)
  (apply 'jomtool-log format args))

(defun jomtool-log-warning (format &rest args)
  (setq jomtool-log-warning-count (1+ jomtool-log-warning-count))
  (apply 'jomtool-log (concat "WARNING: " format) args))

;;---------------------------------------------------------- Comment Block File

(defun jomtool-suck-boilerplate (fname)
  (let ((buf  (generate-new-buffer "*Jomtool Temp*"))
        (text nil))
    ;; Switch to the temp buffer.
    (set-buffer buf)
    ;; Insert the file.
    (condition-case nil
        (progn
          (insert-file-contents fname)
          (jomtool-trim-buf-initial-whitespace)
          (jomtool-trim-buf-trailing-whitespace)
          (setq text (buffer-substring (point-min) (point-max)))
          (or (string= text "")
              (setq text (concat text "\n"))))
      (error nil))
    ;; Kill the temporary buffer.
    (kill-buffer buf)
    ;; Return the text value.
    text))

(defun jomtool-trim-buf-initial-whitespace ()
  (save-excursion
    (let ((begin (goto-char (point-min))))
      (skip-chars-forward " \t\n\015")
      (delete-region begin (point)))))


(defun jomtool-trim-buf-trailing-whitespace ()
  (save-excursion
    (let ((end (goto-char (point-max))))
      (skip-chars-backward " \t\n\015")
      (delete-region (point) end))))

;;-------------------------------------------------------- Class: jomtool-model

(defclass jomtool-model ()
  ((bad)
   (binassocs)
   (classes)
   (classes-alist)
   (default-arg-type)
   (default-attr-type)
   (genrels)
   (language)
   (legal-message)
   (munge-idents)
   (name)
   (package)
   (source-name)
   (start-time)
   (target-dir)
   (use-final-methods))
  "")

(defmethod jomtool-init ((o jomtool-model))
  "Constructor for jomtool-model."
  (oset o default-arg-type  jomtool-default-arg-type-default)
  (oset o default-attr-type jomtool-default-attr-type-default)
  (oset o legal-message     jomtool-legal-message-default)
  (oset o munge-idents      jomtool-munge-idents-default)
  (oset o package           jomtool-package-default)
  (oset o target-dir        jomtool-target-dir-default)
  (oset o use-final-methods jomtool-use-final-methods-default))

(defmethod jomtool-add-class ((o jomtool-model) class)
  (let ((key (oref class key)))
    (jomtool-assert key "OMT class has no key.")
    (jomtool-assert (not (assq key (oref o classes)))
                    "Class %s already exists." key)
    (jomtool-oappend-fast o classes class)))

(defmethod jomtool-crunch ((o jomtool-model))
  (jomtool-log-activity
   "Crunching on model"
   (let ((jomtool-current-model o))
     ;; Build classes-alist.
     (oset o classes-alist (mapcar (function (lambda (n)
                                               (cons (oref n key) n)))
                                   (oref o classes)))
     ;; Crunch the genrels.
     (mapcar 'jomtool-crunch (oref o genrels))
     ;; Crunch the classes.
     (mapcar 'jomtool-crunch (oref o classes))
     ;; Crunch the binassocs.
     (mapcar 'jomtool-crunch (oref o binassocs))
     ;; Make sure target dir is accessible (still might not be writable).
     (let ((dir (oref o target-dir)))
       (if (not (file-accessible-directory-p dir))
           (progn
             (jomtool-log-error "Target directory \"%s\" not accessible." dir)
             (oset o bad t))))

     ;;
     )))

(defmethod jomtool-crunch-and-gen-java ((o jomtool-model))
  (jomtool-crunch o)
  (if (oref o bad)
      (jomtool-log-warning "Not generating code for model.")
    (jomtool-gen-java o)))

(defmethod jomtool-gen-java ((o jomtool-model))
  (jomtool-log-activity
   "Generating Java code for model"
   (let ((jomtool-current-model o))
     (mapcar 'jomtool-gen-java (oref o classes))
     (setq jomtool-current-model nil))))

(defmethod jomtool-get-class ((o jomtool-model) key)
  (or (cdr (assq key (oref o classes-alist)))
      (jomtool-error "Class %s not found." key)))

;;-------------------------------------------------------- Class: jomtool-class

(defclass jomtool-class ()
  ((base-path-name)
   (class-attribs)
   (class-methods)
   (class-methods-index)
   (constructors)
   (constructors-index)
   (description)
   (external)
   (full-path-name)
   (implements)
   (imports)
   (initializer)
   (instance-attribs)
   (instance-methods)
   (instance-methods-index)
   (is-abstract)
   (is-interface)
   (key)
   (linked-roles)
   (name)
   (orphan-methods)
   (subclasses)
   (superclass))
  "")

(defmethod jomtool-crunch ((o jomtool-class))
  ;; See if it's outside the package.
  (if (string-match "\\." (oref o name))
      ;; Outside the package.
      (progn
        (or noninteractive 
            (jomtool-log-info "Not crunching external class %s"
                              (jomtool-object-key-and-name o)))
        (oset o external t))
    ;; Inside the package.
    (jomtool-crunch-really o)))

(defmethod jomtool-crunch-really ((o jomtool-class))
  (jomtool-log-activity
   (format "Crunching on class %s" (jomtool-object-key-and-name o))

   ;; If no class name, derive one from the key; otherwise maybe munge.
   (if (jomtool-string-blank (oref o name))
       (oset o name (format "Class%d" (oref o key)))
     (jomtool-oset-munge-ident-maybe o name 'upper))

   ;; Crunch on its components.
   (let ((jomtool-current-class o))
     (mapcar 'jomtool-crunch (oref o constructors))
     (mapcar 'jomtool-crunch (oref o class-attribs))
     (mapcar 'jomtool-crunch (oref o class-methods))
     (mapcar 'jomtool-crunch (oref o instance-attribs))
     (mapcar 'jomtool-crunch (oref o instance-methods)))

   ;; Set base and full path names.
   (oset o base-path-name (concat (oref o name) ".java"))
   (oset o full-path-name (expand-file-name
                           (oref o base-path-name)
                           (oref jomtool-current-model target-dir)))

   ;; Build method indexes.
   (oset o class-methods-index (jomtool-method-index (oref o class-methods)))
   (oset o constructors-index (jomtool-method-index (oref o constructors)))
   (oset o instance-methods-index
         (jomtool-method-index (oref o instance-methods)))
   ;;
   ))

(defmethod jomtool-extract ((o jomtool-class))
  ;; Note: This needs to be turned to state->event rather than event->state.
  (jomtool-log-activity
   (format "Extracting info from file \"%s\"" (oref o base-path-name))
   (goto-char (point-min))
   (let ((good t)
         (state 'outer)
         method
         method-body)
     (while (not (eobp))
       ;; Act based on current state and contents of current line.
       (let* ((line (buffer-substring-no-properties
                     (progn (beginning-of-line) (point))
                     (progn (end-of-line) (point))))
              (cookie (jomtool-cookie-in-str line)))
         (if cookie
             ;; Cookie on current line.
             (let ((cookie-id-str (car cookie))
                   (cookie-arg (cdr cookie)))
               (ecase (jomtool-cookie-id-sym cookie-id-str)

                 ;; Cookie marks beginning of imports list.
                 ('imports-begin
                  (if (oref o imports)
                      (progn
                        (setq good nil)
                        (jomtool-log-error "Duplicate imports list."))
                    (jomtool-log-activity-begin "Extracting imports list")
                    (setq state 'imports)
                    (oset o imports (jomtool-verblines ""))))

                 ;; Cookie marks end of imports list.
                 ('imports-end
                  (ecase state
                    ('imports
                     (jomtool-log-activity-end "Extracting imports list" t)
                     (setq state 'outer))
                    ('method-body
                     (setq good nil)
                     (jomtool-log-error "\"%s\" cookie is in a method body."
                                        cookie-id-str))
                    ('outer
                     (setq good nil)
                     (jomtool-log-error
                      "\"%s\" cookie is not after imports list."
                      cookie-id-str))))

                 ;; Cookie marks beginning of method body.
                 ('method-body-begin
                  (ecase state
                    ('imports
                     (setq good nil)
                     (jomtool-log-error "\"%s\" cookie is in an imports list."
                                        cookie-id-str))
                    ('method-body
                     (setq good nil)
                     (jomtool-log-error "\"%s\" cookie is in method body."
                                        cookie-id-str))
                    ('outer
                     (if (not (string-match
                               (concat "^\\(class\\|constructor\\|instance\\)"
                                       "[ \t]+"
                                       "\\(.*\\)")
                               cookie-arg))
                         (progn
                           (setq good nil)
                           (jomtool-log-error "Invalid \"%s\" cookie syntax."
                                              cookie-id-str))
                       (let ((kind (intern (match-string 1 cookie-arg)))
                             (signature (match-string 2 cookie-arg)))
                         (jomtool-log-activity-begin
                          (format "Extracting %s method body \"%s\""
                                  kind signature))
                         (setq state 'method-body
                               method (jomtool-get-method o kind signature)
                               method-body (jomtool-verblines ""))
                         (if (not method)
                             (progn
                               (setq method (jomtool-method ""))
                               (oset method kind kind)
                               (oset method orphan t)
                               (oset method signature signature)
                               (jomtool-oappend-fast o orphan-methods method)))
                         (oset method body method-body))))))

                 ;; Cookie marks end of method body.
                 ('method-body-end
                  (ecase state
                    ('imports
                     (setq good nil)
                     (jomtool-log-error
                      "\"%s\" cookie is in an imports list."
                      cookie-id-str))
                    ('method-body
                     (jomtool-log-activity-end
                      (format "Extracting %s method body \"%s\""
                              (oref method kind)
                              (oref method signature))
                      t)
                     (setq state 'outer))
                    ('outer
                     (setq good nil)
                     (jomtool-log-error
                      "\"%s\" cookie is not after method body."
                      cookie-id-str))))

                 ;; Invalid cookie.
                 (t
                  (setq good nil)
                  (jomtool-log-error "Invalid cookie \"%s\"." cookie-id-str))
                 ;;
                 ))
           ;; No cookie on current line.
           (ecase state
             ('imports
              (jomtool-oappend (oref o imports) lines line))
             ('method-body
              (jomtool-oappend-fast method-body lines line))
             ('outer))))
       ;; Move forward a line.
       (forward-line 1))
     ;; After reach end of file, error-check.
     (ecase state
       ('method-body
        (setq good nil)
        (jomtool-log-error "Never saw end of \"%s\" method body."
                           (oref method signature)))
       ('outer))
     ;; Return success status.
     good)))

(defmethod jomtool-full-name ((o jomtool-class))
  (let ((package (oref jomtool-current-model package)))
    (concat (if package
                (concat package ".")
              "")
            (oref o name))))

(defmethod jomtool-gen-java ((o jomtool-class))
  (if (oref o external)
      (or noninteractive
          (jomtool-log-info "Not generating Java code for class \"%s\"."
                            (oref o name)))
    (jomtool-log-activity
     (format "Generating Java code for class \"%s\"" (oref o name))
     (find-file (oref o full-path-name))
     (if (not (jomtool-extract o))
         (jomtool-log-warning "Suspect file will not be changed.")
       (delete-region (point-min) (point-max))
       (insert (jomtool-java-code o))
       (goto-char (point-min))
       (save-buffer)))))

(defmethod jomtool-get-method ((o jomtool-class) kind signature)
  (cdr (assoc signature (ecase kind
                          ('class       (oref o class-methods-index))
                          ('constructor (oref o constructors-index))
                          ('instance    (oref o instance-methods-index))))))

(defmethod jomtool-java-code ((o jomtool-class))               
  (let ((jomtool-current-class o))
    (concat
     ;; RCS strings.
     "//$" "Id" "$\n"
     "//$" "Source" "$\n"
     
     ;; Boilerplate comment.
     (if jomtool-boilerplate
         (concat "\n"
                 jomtool-boilerplate)
       "")
           
     ;; Header
     (jomtool-java-code-header o)
     
     ;; Message.
     jomtool-class-file-warning
     
     ;; Package directive.
     (let ((package (oref jomtool-current-model package)))
       (if package
           (concat "\npackage " package ";\n")
         ""))
     
     ;; Import directives.
     "\n"
     (jomtool-cookie 0 'imports-begin)
     (let ((imports (oref o imports)))
       (if imports
           (jomtool-java-code imports)
         ""))
     (jomtool-cookie 0 'imports-end)
     
     ;; Class.
     "\n"
     (jomtool-description-comment (oref o description) 0)
     "public"
     (if (oref o is-interface)
         " interface "
       (concat
        (if (oref o is-abstract) " abstract" "")
        " class "))
     (oref o name)
     (let ((superclass (oref o superclass)))
       (if superclass
           (concat " extends " (oref superclass name))
         ""))
     (let ((implements (oref o implements)))
       (if implements
           (concat " implements " (mapconcat 'identity implements ", "))
         ""))
     " {\n"
     
     ;; Constants.
     (if (not (oref o is-interface))
         (concat
           "\n"
           (jomtool-indent 1) "private static final String classVcId =\n"
           (jomtool-indent 2) "\"$" "Id" "$\";\n"
           (let ((message (oref jomtool-current-model legal-message)))
             (if message
                 (concat
                  (jomtool-indent 1)
                  "private static final String classLegalMessage =\n"
                  (jomtool-indent 2) "\"" message "\";\n")
               "")))
       "")
     ;; Attributes, methods, etc.
     (jomtool-java-code-list o (oref o constructors)     "Constructors")
     (jomtool-java-code-list o (oref o class-attribs)    "Class Attributes")
     (jomtool-java-code-list o (oref o class-methods)    "Class Methods")
     (jomtool-java-code-list o (oref o linked-roles)     "Association Links")
     (jomtool-java-code-list o (oref o instance-attribs) "Instance Attributes")
     (jomtool-java-code-list o (oref o instance-methods) "Instance Methods")
     
     ;; Orphan method bodies.
     (let (orphans)
       (mapcar (function (lambda (method)
                           (if (not (jomtool-empty-body method))
                               (setq orphans (nconc orphans (list method))))))
               (oref o orphan-methods))
       (if orphans
           (concat
            (jomtool-java-separator 1 "Orphan Methods")
            (mapconcat 'jomtool-java-code orphans ""))
         ""))
     
     ;; End.
     "\n}\n"
     "\n//EOF\n")))

(defmethod jomtool-java-code-header ((o jomtool-class))
  (let ((border (concat "// +" (make-string 74 ?-) "+\n")))
    (concat
     "\n"
     border
     (mapconcat
      (function
       (lambda (n)
         (concat "// |"
                 (if (consp n)
                     (format "%10s:  %-61s" (car n) (cdr n))
                   (format "%-74s" n))
                 "|\n")))
      (list
       (cons "Filename"  (oref o base-path-name))
       (cons "Package"   (or (oref jomtool-current-model package) "(none)"))
       (cons "Source"    (or (oref jomtool-current-model source-name)
                             "(unknown)"))
       (cons "Generated" (concat (jomtool-time-str (oref jomtool-current-model
                                                         start-time))
                                 ", by Jomtool "
                                 jomtool-version))
       )
      "")
     border)))

(defmethod jomtool-java-code-list ((o jomtool-class) list desc)
  (let ((code (mapconcat 'jomtool-java-code list "")))
    (if (string= code "")
        ""
      (concat (jomtool-java-separator 1 desc)
              code))))

(defun jomtool-method-index (method-list)
  (mapcar (function (lambda (n) (cons (oref n signature) n)))
          method-list))

;;------------------------------------------------------- Class: jomtool-attrib

(defclass jomtool-attrib ()
  ((array-mods)
   (default)
   (description)
   (is-class)
   (is-final)
   (key) ;;!!! parse this
   (name)
   (bad)
   (type))
  "")

(defmethod jomtool-crunch ((o jomtool-attrib))
  (jomtool-log-activity
   (format "Crunching on attribute \"%s\"" (oref o name))
   ;;
   (if (jomtool-munge-idents)
       (progn
         (jomtool-oset-munge-ident o name 'lower)
         (jomtool-oset-munge-ident o type 'upper)))
   ;;
   (if (null (oref o type))
       (let ((default (oref jomtool-current-model default-attr-type)))
         (if default 
             (progn
               (jomtool-log-warning "Attribute has no type, so using \"%s\"."
                                    default)
               (oset o type default)))))
   ;;
   (let ((name (oref o name))
         (type (oref o type)))
     (if (null name)
         (progn
           (oset o bad t)
           (if (null type)
               (jomtool-log-error "Attribute has no name or type.")
             (jomtool-log-error "Attribute has no name.")))
       (if (null type)
           (progn
             (jomtool-log-error
              "Attribute has no type.")
             (oset o bad t)))))
   ;;
   (jomtool-fix-array-mods o)
   ;;
   ))

(defmethod jomtool-java-code ((o jomtool-attrib))               
  (if (oref o bad)
      ""
    (let ((default (oref o default))
          (name    (oref o name))
          (type    (oref o type)))
      (concat "\n"
              (jomtool-indent)
              ;;!!! why is this always public?!
              "public"
              (if (oref o is-class) " static" "")
              (if (oref o is-final) " final" "")
              " "
              (jomtool-type-and-name o)
              (if (jomtool-string-blank default)
                  ""
                (concat " = " default))
              ";\n"))))

;;------------------------------------------------------- Class: jomtool-method

(defclass jomtool-method ()
  ((access)
   (array-mods)
   (arglist)
   (bad)
   (body)
   (description)
   (is-abstract)
   (is-final)
   (is-synchronized)
   (is-virtual)
   (key) ;;!!! parse this
   (kind)
   (name)
   (orphan)
   (signature)
   (throws)
   (type))
  "")

(defmethod jomtool-crunch ((o jomtool-method))
  (jomtool-log-activity
   (format "Crunching on method \"%s\"" (oref o name))
   ;; Set kind to "instance" if not set already.
   (if (null (oref o kind))
       (oset o kind 'instance))
   ;; If no return type, set it to "void".
   (if (jomtool-string-blank (oref o type)) (oset o type "void"))
   ;; Mark it as bad if it's a C++ destructor.
   (if (string-match "^~" (oref o name))
       (progn
         (oset o bad t)
         (jomtool-log-error
          "Method C++ destructor notation invalid in Java.")))
   ;; If abstract, make sure class is abstract.
   (if (oref o is-abstract) 
       ;;!!! maybe do warning here.
       (oset jomtool-current-class is-abstract t))
   ;; Mark as abstract if class is an interface.
   (if (oref jomtool-current-class is-interface)
       (oset o is-abstract t))
   ;; Munge identifiers.
   (if (jomtool-munge-idents)
       (progn
         (jomtool-oset-munge-ident o name
                                   (if (eq (oref o kind) 'constructor)
                                       'upper
                                       'lower))
         (jomtool-oset-munge-ident o type 'upper)))
   ;;
   (jomtool-fix-array-mods o)
   ;; Crunch arguments, and mark method as bad if any of the args are bad.
   (let ((count 0))
     ;; Crunch arguments and count bad ones.
     (mapcar (function (lambda (n)
                         (jomtool-crunch n)
                         (if (oref n bad) (setq count (1+ count)))))
             (oref o arglist))
     ;; If any bad, mark method as bad and report.
     (if (> count 0)
         (progn
           (oset o bad t)
           (jomtool-log-warning
            (format "Method has %d invalid argument%s."
                    count
                    (if (= count 1) "" "s"))))))
   ;; Generate signature.
   (oset o signature (if (oref o bad)
                         "SIGNATURE_OF_BAD_METHOD"
                       (concat (oref o name)
                               "("
                               (mapconcat
                                (function
                                 (lambda (n)
                                   (concat (or (oref n type))
                                           (or (oref n array-mods)))))
                                (oref o arglist)
                                ",")
                               ")")))))

(defmethod jomtool-empty-body ((o jomtool-method))
  (let ((body (oref o body)))
    (if body
        (jomtool-empty body)
      t)))

(defmethod jomtool-java-code ((o jomtool-method))
  (if (oref o bad)
      ""
    (concat
     "\n"
     (if (not (oref o orphan))
       (concat
        (jomtool-description-comment (oref o description) 1)
        (jomtool-indent 1)
        (symbol-name (oref o access))
        (cond ((oref o is-abstract)
               " abstract")
              ((and (not (oref o is-final))
                    (or (oref o is-virtual)
                        (not (oref jomtool-current-model use-final-methods))))
               "")
              (t " final"))
        (if (oref o is-synchronized)
            " synchronized"
          "")
        (if (eq (oref o kind) 'constructor)
            ""
          (concat (if (eq (oref o kind) 'class) " static" "")
                  " " (oref o type)))
        " " (oref o name)
        (or (oref o array-mods) "")
        "("
        (mapconcat 'jomtool-java-code
                   (oref o arglist)
                   ", ")
        ")"
        (let ((throws (oref o throws)))
          (if throws
              (concat " throws " (mapconcat 'identity throws ", "))
            ""))
        (if (oref o is-abstract)
            ";\n"
          "")))

     (if (or (oref o orphan)
             (and (oref o is-abstract) (not (jomtool-empty-body o))))
         (concat (jomtool-indent) "ORPHAN_METHOD_BODY")
       "")
     
     (if (not (and (oref o is-abstract) (jomtool-empty-body o)))
       (let ((body       (oref o body))
             (empty-body (jomtool-empty-body o))
             (type       (oref o type)))
         (concat
          " {\n"
          (jomtool-cookie 2 'method-body-begin
                          (format "%s %s" (oref o kind)
                                  (oref o signature)))
          (if empty-body
              (concat "\n"
                      (jomtool-indent 2)
                      "\n\n")
            (jomtool-java-code body))
          (jomtool-cookie 2 'method-body-end)
          (if empty-body
              (concat
               (jomtool-indent 2)
               (format "System.err.println(\"[STUB:%s:%s:%s]\");\n"
                       (jomtool-full-name jomtool-current-class)
                       (oref o kind)
                       (oref o signature))

               (if (not (string= type "void"))
                   (concat (jomtool-indent 2)
                           "return "
                           (cond ((string= type "boolean") "false")
                                 ((string= type "float")   "0.0")
                                 ((string= type "int")     "0")
                                 (t                        "null"))
                           ";\n"))))
          (jomtool-indent 1)
          "}\n"))
       "")

       )))

;;---------------------------------------------------------- Class: jomtool-arg

(defclass jomtool-arg ()
  ((array-mods)
   (bad)
   (name)
   (type))
  "")

(defmethod jomtool-crunch ((o jomtool-arg))
  (jomtool-log-activity
   (format "Crunching on argument \"%s\"" (oref o name))
   (if (jomtool-munge-idents)
       (progn
         (jomtool-oset-munge-ident o name 'lower)
         (jomtool-oset-munge-ident o type 'upper)))
   ;;
   (if (null (oref o type))
       (let ((default (oref jomtool-current-model default-arg-type)))
         (if default 
             (progn
               (jomtool-log-warning "Argument has no type, so using \"%s\"."
                                    default)
               (oset o type default)))))
   ;;
   (let ((name (oref o name))
         (type (oref o type)))
     (if (null type)
         (progn
           (oset o bad t)
           (if (null name)
               (jomtool-log-error "Argument has no name or type.")
             (jomtool-log-error "Argument has no type.")))
       (if (null name)
           (progn
             (oset o bad t)
             (jomtool-log-error "Argument has no name.")))))
   ;;
   (jomtool-fix-array-mods o)
   ;;
   ))
  
(defmethod jomtool-java-code ((o jomtool-arg))
  (jomtool-type-and-name o))

;;------------------------------------------------------- Class: jomtool-genrel

(defclass jomtool-genrel ()
  ((key)
   (subclass-keys)
   (superclass-key))
  "")

(defmethod jomtool-crunch ((o jomtool-genrel))
  (jomtool-log-activity
   (format "Crunching on generalization association %s" (oref o key))
   ;; Make links from subclasses to superclass.
   (let* ((superclass-key (oref o superclass-key))
          (superclass (jomtool-get-class jomtool-current-model
                                         superclass-key)))
     (if superclass
         (mapcar (function
                  (lambda (subclass-key)
                    (let ((subclass (jomtool-get-class jomtool-current-model
                                                       subclass-key)))
                      (if subclass
                          (oset subclass superclass superclass)
                        (jomtool-log-error "Superclass %s not defined."
                                           superclass-key)))))
                 (oref o subclass-keys))
       (jomtool-log-error "Superclass %s not defined." superclass-key)))))

;;----------------------------------------------------- Class: jomtool-binassoc

(defclass jomtool-binassoc ()
  ((key)
   (name)
   (role1)
   (role2))
  "")

(defmethod jomtool-crunch ((o jomtool-binassoc))
  (jomtool-log-activity
   (format "Crunching on simple binary association %s" (oref o key))
   (let ((role1 (oref o role1))
         (role2 (oref o role2)))
     ;; Make inverse links from the role objects.
     (oset role1 binassoc o)
     (oset role2 binassoc o)
     ;; Crunch the role objects.
     (jomtool-crunch role1)
     (jomtool-crunch role2)
     ;; Warn if not implemented.
     (if (not (or (oref role1 is-arrow)
                  (oref role2 is-arrow)))
         (jomtool-log-warning "No arrows on %s." (jomtool-desc o)))
     ;;
     )))

(defmethod jomtool-desc ((o jomtool-binassoc))
  (let ((name (oref o name)))
    (concat 
     "simple binary association"
     (if name (concat " \"%s\"") "")
     " between \""
     (oref (oref (oref o role1) class) name)
     "\" and \""
     (oref (oref (oref o role2) class) name)
     "\"")))

(defmethod jomtool-other-role ((o jomtool-binassoc) role)
  (let ((role1 (oref o role1))
        (role2 (oref o role2)))
    (if (eq role role1)
        role2
      (jomtool-assert (eq role role2) "Bad role parameter.")
      role1)))

;;--------------------------------------------------------- Class: jomtool-role

(defclass jomtool-role ()
  ((binassoc)
   (class)
   (class-key)
   (is-arrow)
   (is-assembly)
   (is-ordered)
   (key)
   (mult)
   (name)
   (qualifier))
  "")

(defmethod jomtool-crunch ((o jomtool-role))
  (jomtool-log-activity
   "Crunching on role"
   (let ((class (jomtool-get-class jomtool-current-model (oref o class-key)))
         (other-role (jomtool-other-role o)))
     ;; Maybe munge role name.
     (jomtool-oset-munge-ident-maybe o name 'lower)
     ;; Make link to class object based on class-key.
     (oset o class class)
     ;; If arrow but no role name, report warning and gen one.
     (if (and (oref o is-arrow) (jomtool-string-blank (oref o name)))
         (let ((name (jomtool-munge-ident (oref class name)
                                          'lower)))
           (oset o name name)
           (jomtool-log-warning "Role has arrow but no name, so using \"%s\"."
                                name)))
     ;; If class should implement association, add link to *other* role.
     (if (oref other-role is-arrow)
         (jomtool-oappend-fast class linked-roles other-role))
     ;;
     )))

(defmethod jomtool-java-code ((o jomtool-role))
  (let ((class (oref o class)))
    (concat "\n"
            (jomtool-indent 1)
            "public"
            " "
            (cond
             ;; Qualified Many.
             ((oref (jomtool-other-role o) qualifier) "java.util.Hashtable")
             ;; Many.
             ((eq (oref o mult) 'many) "java.util.Vector")
             ;; One, Optional.
             (t (oref class name)))
            " "
            (oref o name)
            ";\n")))

(defmethod jomtool-other-role ((o jomtool-role))
  (jomtool-other-role (oref o binassoc) o))

;;---------------------------------------------------- Class: jomtool-qualifier

(defclass jomtool-qualifier ()
  ((array-mods)
   (key)
   (name)
   (type))
  "")

;;---------------------------------------------------- Class: jomtool-verblines

(defclass jomtool-verblines ()
  ((lines))
  "")

;;(defmethod jomtool-desc ((o jomtool-verblines))
;;  ;;!!!
;;  )

;;(defmethod jomtool-empty ((o jomtool-verblines))
;;  (not (oref o lines)))

(defmethod jomtool-empty ((o jomtool-verblines))
  (let ((lines (oref o lines)))
    (if lines
        ;;!!! make it return as soon as found something
        (let ((empty t))
          (mapcar (function (lambda (line)
                              (or (jomtool-string-blank line)
                                  (setq empty nil))))
                  lines)
          empty)
      t)))

(defmethod jomtool-java-code ((o jomtool-verblines))
  (mapconcat (function (lambda (n) (concat n "\n")))
             (oref o lines)
             ""))

;;--------------------------------------------------------------------- Cookies

(defun jomtool-cookie (indent-level id-sym &optional value)
  (concat (jomtool-indent indent-level) jomtool-cookie-prefix 
          (jomtool-cookie-id-str id-sym)
          ""
          (if value (concat " " value)) "\n"))

(defun jomtool-cookie-id-str (id-sym)
  (cdr (assq id-sym jomtool-cookie-id-sym-to-str-alist)))

(defun jomtool-cookie-id-sym (id-str)
  (cdr (assoc id-str jomtool-cookie-id-str-to-sym-alist)))

(defun jomtool-cookie-in-str (str)
  (if (string-match jomtool-cookie-regexp str)
      (cons (match-string 1 str)
            (match-string 3 str))
    nil))
  
;;---------------------------------------------------------- Identifier Munging

(defun jomtool-munge-ident (ident first-char)
  (cond
   ((eq ident nil) nil)
   ((member ident '("boolean" "byte" "char" "double" "float" "int" "short"
                    "void"))
    ident)
   ((string-match "^char[ \t]*\\*&*$" ident) "String")
   ((string-match "^char[ \t]*\\*\\*&*$" ident) "Vector")
   (t (jomtool-munge-ident-nonprimitive ident first-char))))

(defun jomtool-munge-ident-maybe (ident first-char)
  (if (oref jomtool-current-model munge-idents)
      (jomtool-munge-ident ident first-char)
    ident))

(defun jomtool-munge-ident-nonprimitive (ident first-char)
  (let* ((ident-len (length ident))
         (new (make-string ident-len ?Z))
         (new-used-len 0)
         (seen-first-letter nil)
         (next-letter-starts-word nil))
    (mapcar (function
             (lambda (n)
               (cond
                ;; Underscore.
                ((eq n ?_)
                 (setq next-letter-starts-word t))
                ;; Alphanumeric, "~", "[", "]".
                ;; Note: Leave "~" so we know it's destructor later.
                ((or (and (>= n ?a) (<= n ?z))
                     (and (>= n ?A) (<= n ?Z))
                     (and (>= n ?0) (<= n ?9))
                     (memq n '(?~ ?[ ?])))
                 (aset new new-used-len
                       (if seen-first-letter
                           (if next-letter-starts-word
                               (upcase n)
                             n)
                         (ecase first-char
                           ('lower (downcase n))
                           ('same  n)
                           ('upper (upcase n)))))
                 (setq new-used-len            (1+ new-used-len)
                       next-letter-starts-word nil
                       seen-first-letter       t)))))
            (if (let ((case-fold-search nil))
                  (string-match "[a-z]" ident))
                ident
              (downcase ident)))
    (substring new 0 new-used-len)))

;;--------------------------------------------------------- OMTool File Parsing

(defmacro jomtool-forms-parse (forms &rest conds)
  `(mapcar
    (function
     (lambda (n)
       (case (car n)
         ,@conds
         (t (jomtool-log-warning "Construct \"%s\" not recognized." (car n)))
         )))
    ,forms))

(defun jomtool-parse (d)
  (jomtool-log-activity
   "Parsing module"
   (let (o name)
     (jomtool-forms-parse
      (cdr d)
      ('anno)
      ('fcode_max)
      ('key)
      ('language)
      ('name (setq name (jomtool-parse-strval (nth 1 n))))
      ('omt_image (progn
                    (jomtool-assert o "Got image before model.")
                    (jomtool-parse-image o n)))
      ('omt_model (setq o (jomtool-parse-model n))))
     (jomtool-assert o "No \"omt_model\" found.")
     (oset o name name)
     o)))

(defun jomtool-parse-arg (d)
  (let ((o (jomtool-arg "")))
    ;;
    (jomtool-forms-parse
     (cdr d)
     ('default)
     ('key)
     ('name (oset o name (jomtool-parse-ident (nth 1 n))))
     ('type (oset o type (jomtool-parse-type n))))
    o))

(defun jomtool-parse-attrib (d)
  (let ((o (jomtool-attrib "")))
    (jomtool-forms-parse
     (cdr d)
     ('anno
      (case (nth 1 n)
        ('description
         (let ((desc (jomtool-parse-descval (nth 2 n)))
               (new  '()))
           (mapcar
            (function
             (lambda (item)
               (if (consp item)
                   (case (car item)
                     ('final
                      (oset o is-final t))
                     (t (setq new (nconc new (list item)))))
                 (setq new (nconc new (list item))))))
            desc)
           (oset o description new)))))
     ('default (oset o default (jomtool-parse-strval (nth 1 n))))
     ('is_class (oset o is-class t))
     ('key)
     ('name (oset o name (jomtool-parse-ident (nth 1 n))))
     ('type (oset o type (jomtool-parse-type n))))
    ;;
    o))
            
(defun jomtool-parse-class (d)
  (let ((o (jomtool-class ""))
        (class-name nil))
    ;;
    (jomtool-log-activity-begin "Parsing class")
    ;; Parse the class data.
    (jomtool-forms-parse
     (cdr d)
     ('anno
      (case (nth 1 n)
        ('abstract_class (oset o is-abstract t))
        ('abstract_method)
        ('const)
        ('cxx_field_access)
        ('cxx_postclass_pattern)
        ('cxx_postmember_pattern)
        ('cxx_preclass_pattern)
        ('cxx_premember_pattern)
        ('description
         (let ((desc (jomtool-parse-descval (nth 2 n)))
               (new  '()))
           (mapcar
            (function
             (lambda (item)
               (if (consp item)
                   (case (car item)
                     ('abstract
                      (oset o is-abstract t))
                     ('implements
                      (oset o implements (jomtool-parse-wordsval (cdr item))))
                     ('interface
                      (oset o is-interface t))
                     (t (setq new (nconc new (list item)))))
                 (setq new (nconc new (list item))))))
            desc)
           (oset o description new)))
        ('unique_fields)
        ('unique_methods)
        ('virtual_method)
        (t (jomtool-log-warning "Class annotation \"%s\" not recognized."
                                (nth 1 n)))
        ))
     ('field (let ((attrib (jomtool-parse-attrib n)))
               (if (oref attrib is-class)
                   (jomtool-oappend-fast o class-attribs attrib)
                 (jomtool-oappend-fast o instance-attribs attrib))))
     ('key (oset o key (jomtool-parse-keyval (nth 1 n))))
     ('method (let* ((method (jomtool-parse-method n))
                     (name (oref method name)))
                (cond
                 ;; Constructor.
                 ((or (string= name "NEW")
                      (and class-name
                           (string= (downcase name) (downcase class-name))))
                  (oset method name class-name)
                  (oset method kind 'constructor)
                  (jomtool-oappend-fast o constructors method))
                 ;; Class method.
                 ((eq (oref method kind) 'class)
                  (jomtool-oappend-fast o class-methods method))
                 ;; Instance method.
                 (t (jomtool-oappend-fast o instance-methods method)))))
     ('name (progn
              (setq class-name (jomtool-parse-ident (nth 1 n)))
              (oset o name class-name))))
    ;; Output status message.
    (jomtool-log-activity-end (format "Parsing class %s" 
                                      (jomtool-object-key-and-name o)))
    ;; Return class object.
    o))

(defun jomtool-parse-binassoc (d)
  (jomtool-log-activity
   "Parsing simple binary association"
   (let ((o (jomtool-binassoc "")))
     (jomtool-forms-parse
      (cdr d)
      ('anno
       (ecase (nth 1 n)
         ('cxx_role1_access)
         ('cxx_role2_access)
         ('role1_is_class)
         ('role2_is_class)
         ('cxx_assoc_impl)
         ('cxx_embed_link_class)
         ('description)
         ))
      ('key (oset o key (jomtool-parse-keyval (nth 1 n))))
      ('name (oset o name (jomtool-parse-ident (nth 1 n))))
      ('role (let ((role (jomtool-parse-role n)))
               (if (oref o role1)
                   (oset o role2 role)
                 (oset o role1 role))))
      )
     o)))

(defun jomtool-parse-comment (model d)
  (jomtool-log-activity
   "Parsing comment"
   (jomtool-forms-parse
    (cdr d)
    ('key)
    ('loc)
    ('text
     (let ((text (nth 1 n)))
       (if (string-match "^[ \t]*\\([a-zA-Z]+\\)[ \t]*=[ \t]*\\([^ \t].*\\)"
                         text)
           (let ((name (intern (match-string 1 text)))
                 (value (car (read-from-string (match-string 2 text)))))
             (ecase name
               ('DefaultArgType  (oset model default-arg-type value))
               ('DefaultAttrType (oset model default-attr-type value))
               ('LegalMessage    (oset model legal-message value))
               ('MungeIdents     (oset model munge-idents
                                       (jomtool-str-to-bool value)))
               ('Package         (oset model package value))
               ('TargetDir       (oset model target-dir value))
               ('UseFinalMethods (oset model use-final-methods
                                       (jomtool-str-to-bool value)))
               (t (jomtool-log-warning "Invalid Jomtool parameter name \"%s\"."
                                       name))))
         (jomtool-log-warning "Invalid Jomtool parameter \"%s\"." text)))))))

(defun jomtool-parse-descval (d)
  (let ((lines (jomtool-split-lines (jomtool-parse-strval d)))
        (list  '()))
    (mapcar
     (function
      (lambda (line)
        (if (string-match "^[ \t]*@\\([^ \t]+\\)[ \t]*\\(.*\\)$" line)
            (setq line (cons (intern (match-string 1 line))
                             (match-string 2 line)))
          (if (jomtool-string-blank line)
              (setq line nil)))
        (if line
            (setq list (nconc list (list line))))))
     lines)
    list))

(defun jomtool-parse-genrel (d)
  (jomtool-log-activity
   "Parsing generalization association"
   (let ((o (jomtool-genrel "")))
     (jomtool-forms-parse
      (cdr d)
      ('anno)
      ('key (oset o key (jomtool-parse-keyval (nth 1 n))))
      ('subclasses (oset o subclass-keys (cdr n)))
      ('superclass (oset o superclass-key (jomtool-parse-keyval (nth 1 n))))
      )
     o)))

(defun jomtool-parse-ident (d)
  (jomtool-if-nonblank (jomtool-parse-strval d)))

(defun jomtool-parse-image (model d)
  (jomtool-log-activity
   "Parsing image"
   (jomtool-forms-parse
    (cdr d)
    ('sheet (jomtool-parse-sheet model n))
    )))

(defun jomtool-parse-keyval (d)
  d)

(defun jomtool-parse-method (d)
  (let ((o (jomtool-method "")))
    (oset o access 'public)
    (jomtool-forms-parse
     (cdr d)
     ('anno
      (ecase (nth 1 n)
        ('abstract_method (oset o is-abstract t))
        ('const)
        ('cxx_method_access (oset o access (nth 2 n)))
        ('description
         (let ((desc (jomtool-parse-descval (nth 2 n)))
               (new  '()))
           (mapcar
            (function
             (lambda (item)
               (if (consp item)
                   (case (car item)
                     ('abstract
                      (oset o is-abstract t))
                     ('final
                      (oset o is-final t))
                     ('synchronized
                      (oset o is-synchronized t))
                     ('throws
                      (oset o throws (jomtool-parse-wordsval (cdr item))))
                     (t (setq new (nconc new (list item)))))
                 (setq new (nconc new (list item))))))
            desc)
           (oset o description new)))
        ('virtual_method (oset o is-virtual t))))
     ('arglist
      (oset o arglist
            (mapcar (function
                     (lambda (a)
                       (jomtool-assert (eq (car a) 'arg) "Non-arg in arglist.")
                       (jomtool-parse-arg a)))
                    (cdr n))))
     ('default)
     ('genfile)             
     ('genheader)
     ('genstatus)
     ('fcode)
     ('is_class (oset o kind 'class))
     ('key)
     ('name (oset o name (jomtool-parse-ident (nth 1 n))))
     ('type (oset o type (jomtool-parse-type n))))
    o))
            
(defun jomtool-parse-model (d)
  (jomtool-log-activity
   "Parsing model"
   (let ((o (jomtool-model "")))
     (jomtool-init o)
     (jomtool-forms-parse
      (cdr d)
      ('binary_association
       (jomtool-oappend-fast o binassocs (jomtool-parse-binassoc n)))
      ('binary_constraint)
      ('generalization_order)
      ('generalization_relation
       (jomtool-oappend-fast o genrels (jomtool-parse-genrel n)))
      ('omt_class (jomtool-add-class o (jomtool-parse-class n)))
      )
     o)))

(defun jomtool-parse-mult (d)
  ;; (mult  0   2 ) optional
  ;; (mult  0   * ) many
  ;; (mult  1   2 ) one
  ;; (mult  1  <N>) many
  ;; (mult <N> <N>) many
  (ecase (nth 1 d)
    ('0 (if (eq (nth 2 d) '2) 'optional 'many))
    ('1 (if (eq (nth 2 d) '2) 'one 'many))
    (t 'many)))

(defun jomtool-parse-qualifier (d)
  (jomtool-log-activity
   "Parsing qualifier"
   (let ((o (jomtool-qualifier "")))
     (jomtool-forms-parse
      (cdr d)
      ('key (oset o key (jomtool-parse-keyval (nth 1 n))))
      ('name (oset o name (jomtool-parse-ident (nth 1 n))))
      ('type (oset o type (jomtool-parse-type n)))
      )
     o)))

(defun jomtool-parse-role (d)
  (jomtool-log-activity
   "Parsing role"
   (let ((o (jomtool-role "")))
     (jomtool-forms-parse
      (cdr d)
      ('anno
       (ecase (nth 1 n)
         ('pattern)
         ))
      ('class (oset o class-key (jomtool-parse-keyval (nth 1 n))))
      ('is_arrow (oset o is-arrow t))
      ('is_assembly (oset o is-assembly t))
      ('key (oset o key (jomtool-parse-keyval (nth 1 n))))
      ('mult (oset o mult (jomtool-parse-mult n)))
      ('ordered (oset o is-ordered t))
      ('qualifier (oset o qualifier (jomtool-parse-qualifier n)))
      ('rolename (oset o name (jomtool-parse-ident (nth 1 n))))
      )
     o)))

(defun jomtool-parse-sheet (model d)
  (jomtool-log-activity
   "Parsing sheet"
   (let (params-sheet)
     (jomtool-forms-parse
      (cdr d)
      ('anno)
      ('binary_rel_arc)
      ('class_box)
      ('constraint_arc)
      ('gen_rel_arc)
      ('height)
      ('key)
      ('name (if (string-match jomtool-params-sheet-regexp
                               (jomtool-parse-strval (nth 1 n)))
                 (setq params-sheet t)))
      ('sheet_comment (if params-sheet (jomtool-parse-comment model n)))
      ('width)
      ))))

(defun jomtool-parse-strval (d)
  (if (stringp d)
      d
    (format "%s" d)))

(defun jomtool-parse-type (d)
  (jomtool-assert (eq (car d) 'type) "That's not an OMTool type construct!")
  (jomtool-if-nonblank (mapconcat 'jomtool-parse-ident (cdr d) "")))

(defun jomtool-parse-wordsval (d)
  (let ((pos 0)
        (words '()))
    (while (string-match "\\([^ \t]+\\)" d pos)
      (setq words (nconc words (list (match-string 1 d)))
            pos   (match-end 0)))
    words))

;;----------------------------------------------------- Java Generation Helpers

(defun jomtool-indent (&optional levels)
  (make-string (if levels
                   (* levels jomtool-indent-size)
                 jomtool-indent-size)
               32))

(defun jomtool-java-separator (indent-levels label)
  (let ((indent-str (jomtool-indent indent-levels)))
    (concat "\n" indent-str "//"
            (make-string (max 3 (- 79 3 (length indent-str) (length label)))
                         ?-)
            " " label "\n")))

;;------------------------------------------------------------------- Front-End

(defun jomtool (file-name)
  "Run Jomtool."
  (interactive "fJomtool OMTool file: ")
  (find-file file-name)
  (jomtool-buffer))

(defun jomtool-buffer ()
  "Run Jomtool on the current buffer."
  (jomtool-log-begin)
  (let ((fname jomtool-boilerplate-fname))
    (if fname
      (let ((text (jomtool-suck-boilerplate fname)))
        (if text
            (if (string= text "")
                (jomtool-log-warning "Boilerplate file is empty.")
              (setq jomtool-boilerplate text))
          (jomtool-log-warning "Boilerplate file could not be read.")))))
  (save-excursion
    (let ((model (jomtool-parse (jomtool-buffer-rawdata)))
          (start-time (current-time)))
      (oset model source-name (or (buffer-file-name) (buffer-name)))
      (oset model start-time start-time)
      (jomtool-crunch-and-gen-java model)))
  (jomtool-log-end))

(defun jomtool-buffer-rawdata ()
  (jomtool-log-activity
   "Reading data"
   (save-excursion
     (goto-char (point-min))
     (or (re-search-forward "^(omt_module\\>" nil t)
         (jomtool-error "No \"omt_module\" found in buffer."))
     (goto-char (match-beginning 0))
     (read (current-buffer)))))

;;--------------------------------------------------------------- Bug Reporting

(defun jomtool-submit-bug-report ()
  "Submit via mail a bug report on Jomtool."
  (interactive)
  (require 'reporter)
  (reporter-submit-bug-report
   jomtool-maintainer-address
   (concat "jomtool.el " jomtool-version " " jomtool-vc-id)
   '()
   nil
   nil
   (concat
    "Neil, "
    (let* ((m '("I despise Lisp!  Damned parentheses!"
                "I hope you're sitting down..."
                "I should have killed you when I had the chance."
                "I'm your biggest fan!  I have all your albums!"
                "Jomtool rocks my world."
                "it's over between us.  We both need to move on."
                "you are one sick Lisper.  Marry me?")))
      (nth (random (length m)) m)))))

;;--------------------------------------------------------------------- Testing

(defun jomtool-test ()
  (interactive)
  (dired "~/emacs/jomtool")
  (jomtool-test-file "sample1.omt"))

(defun jomtool-test-file (file-name)
  (jomtool-file file-name))

(defun jomtool-test-lots ()
  (interactive)
  (dired "~/emacs/jomtool")
  (mapcar 'jomtool-test-file (directory-files "." nil "^.*\\.omt$")))

;;-----------------------------------------------------------------------------
(provide 'jomtool)

;; jomtool.el ends here
