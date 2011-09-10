
;;;; -*-Emacs-Lisp-*- Functions for Examining the GNU Emacs Version Information
;;;; Written by Eric Eide, last modified on $Date: 1994/09/01 23:07:07 $.
;;;; (C) Copyright 1993, 1994, Eric Eide and the University of Utah
;;;;
;;;; COPYRIGHT NOTICE
;;;;
;;;; This program is free software; you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by the Free
;;;; Software Foundation; either version 2 of the License, or (at your option)
;;;; any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;;;; for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License along
;;;; with GNU Emacs.  If you did not, write to the Free Software Foundation,
;;;; Inc., 675 Mass Ave., Cambridge, MA 02139, USA.

;;;; AUTHOR
;;;;
;;;; This package was written by Eric Eide (eeide@cs.utah.edu).  Address:
;;;;
;;;;   Eric Eide (eeide@cs.utah.edu)
;;;;   University of Utah
;;;;   3190 Merrill Engineering Building
;;;;   Salt Lake City, Utah  84112

;;;; LISP CODE DIRECTORY INFORMATION
;;;;
;;;; LCD Archive Entry:
;;;; emacs-vers|Eric Eide|eeide@cs.utah.edu|
;;;; Functions for examining and testing the GNU Emacs version number and type|
;;;; $Date: 1994/09/01 23:07:07 $|$Revision: 1.3 $|~/misc/emacs-vers.el.Z|

;;;; SUMMARY
;;;;
;;;; This file provides many functions for examining and testing the version
;;;; information for GNU Emacs and its various derivatives, including Epoch,
;;;; Lucid GNU Emacs, and XEmacs.  The functions in this file make it easy for
;;;; other Emacs Lisp code to discern the following information about the Emacs
;;;; in which it is executing:
;;;;
;;;;   + The "type" or "flavor" of the Emacs system;
;;;;   + The "major version" number of the Emacs system; and
;;;;   + The "minor version" number of the Emacs system.
;;;;
;;;; By examining the Emacs type and version numbers, Emacs Lisp functions can
;;;; (indirectly) determine the capabilities of the Emacs in which they are
;;;; running, and can therefore "do the right thing" for several different
;;;; Emacs systems.  (Other identifying information, such as the "build count"
;;;; or the patch level or the beta version number, is generally not useful for
;;;; discerning the capabilities of the current Emacs system.  Therefore, this
;;;; file does not provide functions for examining these other identifiers.)
;;;;
;;;; Note that all of the current variants of GNU Emacs already provide a
;;;; built-in `emacs-version' function and a built-in `emacs-version' variable.
;;;; The values of those things are strings that must be parsed --- and in the
;;;; past, every Emacs Lisp package that needed version information parsed
;;;; these strings for itself.  The functions in this file address this need;
;;;; they take care of the parsing and make the identifying information more
;;;; directly accessible to other Lisp code.
;;;;
;;;; Addendum: Recent versions of GNU Emacs 19 (the FSF's GNU Emacs 19.23+,
;;;; Lucid GNU Emacs 19.10, and XEmacs) provide additional version information
;;;; variables, `emacs-major-version' and `emacs-minor-version', that obviate
;;;; some of the need to parse the `emacs-version' strings.  These new
;;;; variables do not reduce the usefulness of "emacs-vers.el", however.  This
;;;; file remains useful because (1) it provides parsed version data for *all*
;;;; versions of GNU Emacs, and in addition (2) it provides a complete suite of
;;;; predicate functions for testing the Emacs version data.

;;;; FUNCTION DOCUMENTATION
;;;;
;;;; In total, this file defines sixteen different functions for examining and
;;;; testing the Emacs version information:
;;;;
;;;;   emacs-type		emacs-major-version	emacs-minor-version
;;;;
;;;;   emacs-type-eq
;;;;
;;;;   emacs-version=		emacs-version>		emacs-version>=
;;;;   emacs-version/=		emacs-version<		emacs-version<=
;;;;
;;;;   emacs-minor-version=	emacs-minor-version>	emacs-minor-version>=
;;;;   emacs-minor-version/=	emacs-minor-version<	emacs-minor-version<=
;;;;
;;;; These functions are fully described below.
;;;;
;;;; EMACS TYPES
;;;;
;;;; The function `emacs-type' returns a symbol that identifies the "type" or
;;;; "flavor" of the current Emacs system.  Typically, each type has its own
;;;; set of special features (and misfeatures!) that are unlike those offered
;;;; by the other flavors of Emacs.  Currently, the identifying symbols and
;;;; their meanings are:
;;;;
;;;;   SYMBOL	 MEANING
;;;;   ------	 ------------------------------------------------------------
;;;;   fsf	 GNU Emacs as published by the Free Software Foundation (the
;;;;		   FSF).
;;;;   epoch	 Epoch, a version of GNU Emacs published by the University of
;;;;		   of Illinois, Urbana-Champaign.
;;;;   lucid	 Lucid GNU Emacs, a version of GNU Emacs published by Lucid,
;;;;		   Inc.
;;;;   xemacs	 XEmacs, a version of GNU Emacs that is the joint product of
;;;;		   Lucid, Inc., Sun Microsystems, Inc., and the University of
;;;;		   Illinois.  XEmacs is the successor to Lucid GNU Emacs.
;;;;   ------	 ------------------------------------------------------------
;;;;
;;;; Other identifying symbols may be defined in the future.
;;;;
;;;; The symbol `fsf' is the "default" value, meaning that this file determines
;;;; the Emacs type by first looking for attributes that distinguish the other
;;;; "offshoots" of the GNU Emacs tree.  If the current Emacs system doesn't
;;;; appear to be any of the known variants from outside the FSF, then the
;;;; `emacs-type' function assumes that the current Emacs system must be the
;;;; archetype GNU Emacs from the FSF.
;;;;
;;;; The function `emacs-type-eq' is a simple predicate that returns true if
;;;; the current Emacs type symbol is `eq' to the given symbol.  In other
;;;; words:
;;;;
;;;;        (emacs-type-eq <symbol>)  <==>  (eq (emacs-type) <symbol>)
;;;;
;;;; EMACS VERSION NUMBERS
;;;;
;;;; All of the known flavors of GNU Emacs have a version number that contains
;;;; two or three separate (integer) components, separated by periods:
;;;;
;;;;   + The first number is the "major version" number, which is incremented
;;;;	 for each new "major" release.  A new major version number indicates
;;;;	 that the Emacs program contains completely new features, abandons
;;;;	 obsolete features, contains other very significant changes, and may
;;;;	 not be code-compatible with previous major versions of Emacs.
;;;;
;;;;   + The second number is the "minor version" number, which is incremented
;;;;	 for each new "minor" release (i.e., each new revision of a particular
;;;;	 major version).  A new minor version number indicates that the Emacs
;;;;	 program contains new bug fixes, minor new features, or other minor
;;;;	 changes, but that there are few (if any) code-level incompatibilities
;;;;	 with the previous minor release of Emacs.
;;;;
;;;;   + The third number, if it appears, is either the "build count" number,
;;;;	 which is incremented each time Emacs is compiled, or a "beta version"
;;;;	 number, incremented for each new beta test release of Emacs.  In
;;;;	 either case, this number is not generally useful for discerning the
;;;;	 capabilities of the current Emacs system.
;;;;
;;;; The functions `emacs-major-version' and `emacs-minor-version' return the
;;;; major and minor version numbers, respectively, of the current Emacs
;;;; system.  Each of these numbers is a non-negative integer.
;;;;
;;;; This file provides six predicates for testing the Emacs version numbers:
;;;;
;;;;   emacs-version=		emacs-version>		emacs-version>=
;;;;   emacs-version/=		emacs-version<		emacs-version<=
;;;;
;;;; Each of these functions accepts two arguments: a major version number and
;;;; an optional minor version number.  If the minor version number is not
;;;; specified (or is the symbol `nil') then only the major version numbers are
;;;; considered in the test.
;;;;
;;;; These six predicates perform the obvious numeric comparisons between their
;;;; arguments and the version numbers of the current Emacs system.  For
;;;; example, in an Emacs system with major version 19 and minor version 22
;;;; (i.e., "Emacs 19.22"):
;;;;
;;;;  (emacs-version=  19   )  is true.    (emacs-version/= 19   )  is false.
;;;;  (emacs-version=  19 21)  is false.   (emacs-version/= 19 21)  is true.
;;;;  (emacs-version=  19 22)  is true.    (emacs-version/= 19 22)  is false.
;;;;  (emacs-version=  19 23)  is false.   (emacs-version/= 19 23)  is true.
;;;;  (emacs-version=  18   )  is false.   (emacs-version/= 18   )  is true.
;;;;  (emacs-version=  18 59)  is false.   (emacs-version/= 18 59)  is true.
;;;;
;;;;  (emacs-version>  19   )  is false.   (emacs-version<= 19   )  is true.
;;;;  (emacs-version>  19 21)  is true.    (emacs-version<= 19 21)  is false.
;;;;  (emacs-version>  19 22)  is false.   (emacs-version<= 19 22)  is true.
;;;;  (emacs-version>  19 23)  is false.   (emacs-version<= 19 23)  is true.
;;;;  (emacs-version>  18   )  is true.    (emacs-version<= 18   )  is false.
;;;;  (emacs-version>  18 59)  is true.    (emacs-version<= 18 59)  is false.
;;;;
;;;; In addition, there are six predicates for testing the Emacs minor version
;;;; number only:
;;;;
;;;;   emacs-minor-version=	emacs-minor-version>	emacs-minor-version>=
;;;;   emacs-minor-version/=	emacs-minor-version<	emacs-minor-version<=
;;;;
;;;; Each of these functions accepts one argument, which is compared against
;;;; the minor version number of the current Emacs system.  For example, in
;;;; Emacs version 19.22:
;;;;
;;;;  (emacs-minor-version>  21)  is true.
;;;;  (emacs-minor-version<= 21)  is false.

;;;; RUN-TIME DETERMINATION VERSUS COMPILE-TIME DETERMINATION
;;;;
;;;; By default, the version-examining functions in this file do "run-time"
;;;; determination of the Emacs version information.  That is, the values
;;;; returned by the functions `emacs-type', `emacs-version=', and so on are
;;;; determined by the Emacs system in which those functions are invoked.  This
;;;; is always the case for interpreted code and is the default case for
;;;; byte-compiled code.
;;;;
;;;; However, as an option for byte-compiled code, this file allows one to do
;;;; "compile-time" version determination instead.  In other words, when one
;;;; byte-compiles an Emacs Lisp file, one can tell the Emacs byte-compiler to
;;;; assume certain things about the Emacs system in which the compiled ".elc"
;;;; code will execute.  For example, one could tell the compiler to assume
;;;; that the compiled code will only be executed in Lucid GNU Emacs, or in
;;;; Emacs version 19, or whatever.  By making these assumptions the compiler
;;;; can often generate more efficient code.  Further, these assumptions allow
;;;; the compiler to ignore any parts of the code that are inappropriate for
;;;; the "target" Emacs system.  For example, if you tell the compiler to
;;;; assume that the compiled code will only be executed in the FSF's GNU Emacs
;;;; version 19, the compiler can then ignore the parts of the source code that
;;;; would only be executed by Lucid GNU Emacs, or Epoch, or GNU Emacs version
;;;; 18.  The effect is that the byte-compiler won't issue warnings about
;;;; "irrelevant" parts of the source code, and that the resultant ".elc" file
;;;; will be smaller.
;;;;
;;;; The variable `byte-optimize-use-compile-time-version-info' tells the
;;;; byte-compiler if and how it should incorporate compile-time version
;;;; information into the output ".elc" file.  The value of this variable must
;;;; be one of the following:
;;;;
;;;;   nil	In this case (the default), the byte-compiler will make no
;;;;		assumptions about the version of the target Emacs system in
;;;;		which the compiled code will execute.  In other words, all of
;;;;		the Emacs version-examining functions will do purely run-time
;;;;		version determination.
;;;;
;;;;   t	In this case, the compiler will assume that the target Emacs
;;;;		system version is identical to that of the Emacs system in
;;;;		which the byte-compiler itself is running.  In other words, the
;;;;		assumption is that the compiled code will execute in the same
;;;;		Emacs system that produced the ".elc" file.  The effect is that
;;;;		the version-examining functions will do purely compile-time
;;;;		version determination.
;;;;
;;;;   <an association list>
;;;;		In this case, the association list tells the byte-compiler what
;;;;		assumptions it should make about the "target" Emacs system
;;;;		version.  The association list may associate one or more of the
;;;;		following symbols to values:
;;;;
;;;;		  emacs-type		The (symbol) type of the target Emacs.
;;;;		  emacs-major-version	The (integer) major version.
;;;;		  emacs-minor-version	The (integer) minor version.
;;;;
;;;;		For example, to tell the byte-compiler to assume that the
;;;;		compiled code will be run only in Epoch 4.x, one would set the
;;;;		variable `byte-optimize-use-compile-time-version-info' to:
;;;;
;;;;		  '((emacs-type . epoch) (emacs-major-version . 4))
;;;;
;;;;		NOTE that one should be careful when telling the byte-compiler
;;;;		to "cross-compile" an Emacs Lisp file for a particular version
;;;;		of Emacs.  In particular, one must also make sure that the
;;;;		byte-compiler is set to generate the appropriate byte-codes for
;;;;		the target Emacs system.  Refer to the byte-compiler's own
;;;;		compatibility variables for more information.
;;;;
;;;; In summary, run-time version determination allows compiled code to be
;;;; maximally portable across different Emacs systems.  The cost of run-time
;;;; determination is generally not large, and so this behavior is the default.
;;;; Compile-time version determination, on the other hand, allows one to
;;;; "tailor" compiled code to a specific Emacs system.  This can be useful,
;;;; and so this behavior is available as an option.
;;;;
;;;; Note that compile-time version determination is only available when the
;;;; byte-compiler's source-code optimizer is enabled.  One important effect of
;;;; this is that one cannot do compile-time version determination with the
;;;; "old" non-optimizing Emacs 18-vintage byte-compiler.
;;;;
;;;; Also note that when one uses compile-time version determination, there is
;;;; no barrier to prevent one from loading the byte-compiled code into an
;;;; inappropriate version of Emacs (i.e., an Emacs system in which the code's
;;;; compiled-in version assumptions are untrue).  Perhaps someday this will be
;;;; fixed.

;;;; NOTES
;;;;
;;;; My wish is that these functions would be incorporated into the various GNU
;;;; Emacs distributions.  This would eliminate the need for this file to parse
;;;; and divine the various version data and would make the predicate functions
;;;; standard and widely useful.
;;;;
;;;; Much of the code below is for the "new" optimizing byte-compiler which is
;;;; distributed with the FSF's GNU Emacs version 19, Lucid GNU Emacs, and
;;;; XEmacs, and which is available for GNU Emacs 18 and for Epoch.  The
;;;; optimizer can inline the code for the predicates but it doesn't do as much
;;;; compile-time analysis as I would like.  (In particular, the optimizer
;;;; doesn't remove unnecessary `let' bindings.)  The special optimizer
;;;; functions in this file are also necessary in order to do compile-time
;;;; version determination.
;;;;
;;;; I considered writing the predicates as macros, in order to allow the
;;;; standard GNU Emacs 18 byte-compiler to inline the comparisons.  But I
;;;; decided against doing this because (1) I think that functions are more
;;;; appropriate for doing version determination, (2) Emacs 19 is arguably the
;;;; current version of Emacs, and (3) the optimizing byte-compiler is
;;;; available for Emacs 18.
;;;;
;;;; It would be nice to be able for this file to hook into the byte-compiler
;;;; function that emits the ".elc" file header.  In particular, when one uses
;;;; compile-time version determination, it would be nice to emit a header form
;;;; to check that the file's compiled-in version assumptions are valid.  I can
;;;; imagine changing the header to something like this:
;;;;
;;;;   (require 'emacs-vers)
;;;;   (elc-compatible-p '(<type> <major-version> <minor-version>)
;;;;                     <uses-v19-bytecodes-p>)
;;;;
;;;; where the various <...> forms are filled in as appropriate for the ".elc"
;;;; file.  This change would have a second small advantage: the above form is
;;;; smaller than the header that is currently emitted by the byte-compiler.
;;;; (Note that although changing `byte-compile-insert-header' wouldn't affect
;;;; the old v18-vintage byte-compiler, that's okay.  The old byte-compiler
;;;; can't perform compile-time version determination anyway, as described in
;;;; the previous comment section.)

;; (provide 'emacs-vers) at the end of this file.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Here are the global variable declarations.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar byte-optimize-use-compile-time-version-info nil
  "*This tells the byte-compiler how to use compile-time version information.
When creating optimized byte-compiled code, invocations of the various Emacs
version-examining functions can be compiled to use either \"run-time\" or
\"compile-time\" Emacs version information.  The (compile-time) value of this
variable determines which will be used:

  `nil' means to use the run-time version information.
  `t' means to use the compile-time version information.

Alternately, this variable may contain an association list that describes which
Emacs version assumptions should be made at compile-time.")

;;
;; I would put the following `defconst's inside the `let' form below, except
;; that doing so causes the byte-compiler to emit a lot of warnings.
;;

(defconst emacs-type nil
  "The symbol that describes the \"type\" or \"flavor\" of this Emacs system.
Do not refer to this variable directly; instead, use the function `emacs-type'
or `emacs-type-eq'.")

(defconst emacs-major-version nil
  ;; NOTE that this variable is predefined in the FSF's GNU Emacs 19.23+, Lucid
  ;; GNU Emacs 19.10, and XEmacs.  It does no harm to redefine it here.
  "The major version number of this Emacs system.
Do not refer to this variable directly; instead, use the function `emacs-major-
version' or one of the Emacs version predicates.")

(defconst emacs-minor-version nil
  ;; NOTE that this variable is predefined in the FSF's GNU Emacs 19.23+, Lucid
  ;; GNU Emacs 19.10, and XEmacs.  It does no harm to redefine it here.
  "The minor version number of this Emacs system.
Do not refer to this variable directly; instead, use the function `emacs-minor-
version' or one of the Emacs version predicates.")

;;
;; Now change the above constants (insert smiley here).
;;

(let ((old-match-data (match-data))
      (type nil)
      (major-version nil)
      (minor-version nil))
  (condition-case nil
      (unwind-protect
	  (progn
	    ;; Use various heuristics to determine the type of the current
	    ;; Emacs system.  If you change this code, be sure to update the
	    ;; appropriate comment above and documentation string below.
	    (setq type
		  (cond ((string-match "XEmacs" emacs-version)
			 ;; NOTE that this "XEmacs" test must come before the
			 ;; "Lucid" test.
			 'xemacs)
			((string-match "Lucid" emacs-version)
			 'lucid)
			((and (boundp 'epoch::version)
			      ;; I have heard of poorly-written packages that
			      ;; bind `epoch::version' to `nil', so we're extra
			      ;; paranoid here.
			      ;;
			      ;; This `symbol-value' stuff is here in order to
			      ;; avoid a warning from the byte-compiler about
			      ;; "reference to free variable epoch::version."
			      (stringp (symbol-value 'epoch::version))
			      (string-match "Epoch"
					    (symbol-value 'epoch::version)))
			 'epoch)
			(t
			 'fsf)))
	    ;; Parse the major and minor version numbers out of the built-in
	    ;; `emacs-version' string.
	    (if (string-match "\\`\\([0-9]+\\)\\.\\([0-9]+\\)" emacs-version)
		(setq major-version (string-to-int
				     (substring emacs-version
						(match-beginning 1)
						(match-end 1)))
		      minor-version (string-to-int
				     (substring emacs-version
						(match-beginning 2)
						(match-end 2)))
		      ))
	    )
	(store-match-data old-match-data))
    (error nil))
  
  (if (not (and type major-version minor-version))
      (error "Unable to determine the Emacs type and version information!"))
  
  (setq emacs-type type
	emacs-major-version major-version
	emacs-minor-version minor-version)
  ) ;; End of `let'.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Here are the version-examining functions and the declarations for the
;;;; optimizing byte-compiler.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-emacs-version-byte-optimizer (symbol &rest body)
  ;; Define an anonymous function with BODY to be the byte-optimizer function
  ;; for SYMBOL forms.  Within BODY, the variable FORM is the form to be
  ;; optimized.
  (list 'put (list 'quote symbol)
	''byte-optimizer
	(list 'function (cons 'lambda (cons '(form) body)))))

;;;
;;; First, the accessors...
;;;

(defun emacs-type ()
  "Return a symbol that describes the \"type\" or \"flavor\" of this Emacs.
The following table describes the symbols that may be returned:

    SYMBOL    MEANING
    ------    ------------------------------------------------------------
    fsf       GNU Emacs as published by the Free Software Foundation (the
                FSF).
    epoch     Epoch, a version of GNU Emacs published by the University of
                Illinois, Urbana-Champaign.
    lucid     Lucid GNU Emacs, a version of GNU Emacs published by Lucid,
                Inc.
    xemacs    XEmacs, a version of GNU Emacs that is the joint product of
                Lucid, Inc., Sun Microsystems, Inc., and the University of
                Illinois.  XEmacs is the successor to Lucid GNU Emacs.
    ------    ------------------------------------------------------------

Other identifying symbols may be defined in the future.

The symbol `fsf' is the \"default\" value, meaning that this function
determines the Emacs type by first looking for attributes that distinguish the
other \"offshoots\" of the GNU Emacs tree.  If the current Emacs system doesn't
appear to be any of the known variants from outside the FSF, then this function
assumes that the current Emacs system must be the archetype GNU Emacs from the
FSF."
  emacs-type)

(defun emacs-major-version ()
  "Return the major version number of this Emacs (as a non-negative integer)."
  emacs-major-version)

(defun emacs-minor-version ()
  "Return the minor version number of this Emacs (as a non-negative integer)."
  emacs-minor-version)

;;;

(define-emacs-version-byte-optimizer emacs-type
  (byte-optimize-emacs-version-accessor form 'emacs-type))

(define-emacs-version-byte-optimizer emacs-major-version
  (byte-optimize-emacs-version-accessor form 'emacs-major-version))

(define-emacs-version-byte-optimizer emacs-minor-version
  (byte-optimize-emacs-version-accessor form 'emacs-minor-version))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; Then the type predicates...
;;;

(defun emacs-type-eq (type)
  "Return true if TYPE is `eq' to the symbol describing this Emacs' type.
Refer to the documentation for the function `emacs-type' for more information."
  (eq emacs-type type))

;;;

(define-emacs-version-byte-optimizer emacs-type-eq
  (byte-optimize-emacs-version-walk-form
   '(eq emacs-type type)			;; The body of `emacs-type-eq'.
   '(type)					;; The formal arguments.
   (byte-optimize-emacs-version-args form 1)	;; The actual argument forms.
   '(t)						;; Always inline `type'.
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; Then the major/minor version number predicates...
;;;

(defun emacs-version= (major &optional minor)
  "Return true if the Emacs version is = to the given MAJOR and MINOR numbers.

The MAJOR version number argument is required, but the MINOR version number
argument is optional.  If the minor version number is not specified (or is the
symbol `nil') then only the major version numbers are considered in the test."
  (if (null minor)
      (= emacs-major-version major)
    (and (= emacs-major-version major)
	 (= emacs-minor-version minor))
    ))

(define-emacs-version-byte-optimizer emacs-version=
  (byte-optimize-emacs-version-major/minor-predicate
   form
   '(= emacs-major-version major)	;; minor arg form is `nil'.
   '(and (= emacs-major-version major)	;; minor arg form is a true constant.
	 (= emacs-minor-version minor))
   ))

;;;

(defun emacs-version/= (major &optional minor)
  "Return true if the Emacs version is /= to the given MAJOR and MINOR numbers.

The MAJOR version number argument is required, but the MINOR version number
argument is optional.  If the minor version number is not specified (or is the
symbol `nil') then only the major version numbers are considered in the test."
  (if (null minor)
      (/= emacs-major-version major)
    (or (/= emacs-major-version major)
	(/= emacs-minor-version minor))
    ))

(define-emacs-version-byte-optimizer emacs-version/=
  (byte-optimize-emacs-version-major/minor-predicate
   form
   '(/= emacs-major-version major)	;; minor arg form is `nil'.
   '(or (/= emacs-major-version major)	;; minor arg form is a true constant.
	(/= emacs-minor-version minor))
   ))

;;;

(defun emacs-version> (major &optional minor)
  "Return true if the Emacs version is > the given MAJOR and MINOR numbers.

The MAJOR version number argument is required, but the MINOR version number
argument is optional.  If the minor version number is not specified (or is the
symbol `nil') then only the major version numbers are considered in the test."
  (if (null minor)
      (> emacs-major-version major)
    (or (> emacs-major-version major)
	(and (= emacs-major-version major)
	     (> emacs-minor-version minor))
	)
    ))

(define-emacs-version-byte-optimizer emacs-version>
  (byte-optimize-emacs-version-major/minor-predicate
   form
   '(> emacs-major-version major)	;; minor arg form is `nil'.
   '(or (> emacs-major-version major)	;; minor arg form is a true constant.
	(and (= emacs-major-version major)
	     (> emacs-minor-version minor))
	)
   ))

;;;

(defun emacs-version< (major &optional minor)
  "Return true if the Emacs version is < the given MAJOR and MINOR numbers.

The MAJOR version number argument is required, but the MINOR version number
argument is optional.  If the minor version number is not specified (or is the
symbol `nil') then only the major version numbers are considered in the test."
  (if (null minor)
      (< emacs-major-version major)
    (or (< emacs-major-version major)
	(and (= emacs-major-version major)
	     (< emacs-minor-version minor))
	)
    ))

(define-emacs-version-byte-optimizer emacs-version<
  (byte-optimize-emacs-version-major/minor-predicate
   form
   '(< emacs-major-version major)	;; minor arg form is `nil'.
   '(or (< emacs-major-version major)	;; minor arg form is a true constant.
	(and (= emacs-major-version major)
	     (< emacs-minor-version minor))
	)
   ))

;;;

(defun emacs-version>= (major &optional minor)
  "Return true if the Emacs version is >= to the given MAJOR and MINOR numbers.

The MAJOR version number argument is required, but the MINOR version number
argument is optional.  If the minor version number is not specified (or is the
symbol `nil') then only the major version numbers are considered in the test."
  (if (null minor)
      (>= emacs-major-version major)
    (or (> emacs-major-version major)
	(and (=  emacs-major-version major)
	     (>= emacs-minor-version minor))
	)
    ))

(define-emacs-version-byte-optimizer emacs-version>=
  (byte-optimize-emacs-version-major/minor-predicate
   form
   '(>= emacs-major-version major)	;; minor arg form is `nil'.
   '(or (> emacs-major-version major)	;; minor arg form is a true constant.
	(and (=  emacs-major-version major)
	     (>= emacs-minor-version minor))
	)
   ))

;;;

(defun emacs-version<= (major &optional minor)
  "Return true if the Emacs version is <= to the given MAJOR and MINOR numbers.

The MAJOR version number argument is required, but the MINOR version number
argument is optional.  If the minor version number is not specified (or is the
symbol `nil') then only the major version numbers are considered in the test."
  (if (null minor)
      (<= emacs-major-version major)
    (or (< emacs-major-version major)
	(and (=  emacs-major-version major)
	     (<= emacs-minor-version minor))
	)
    ))

(define-emacs-version-byte-optimizer emacs-version<=
  (byte-optimize-emacs-version-major/minor-predicate
   form
   '(<= emacs-major-version major)	;; minor arg form is `nil'.
   '(or (< emacs-major-version major)	;; minor arg form is a true constant.
	(and (=  emacs-major-version major)
	     (<= emacs-minor-version minor))
	)
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; And last, the minor version number predicates...
;;;

(defun emacs-minor-version= (minor)
  "Return true if the Emacs minor version is = to the given MINOR version."
  (= emacs-minor-version minor))

(defun emacs-minor-version/= (minor)
  "Return true if the Emacs minor version is /= to the given MINOR version."
  (/= emacs-minor-version minor))

(defun emacs-minor-version> (minor)
  "Return true if the Emacs minor version is > the given MINOR version."
  (> emacs-minor-version minor))

(defun emacs-minor-version< (minor)
  "Return true if the Emacs minor version is < the given MINOR version."
  (< emacs-minor-version minor))

(defun emacs-minor-version>= (minor)
  "Return true if the Emacs minor version is >= to the given MINOR version."
  (>= emacs-minor-version minor))

(defun emacs-minor-version<= (minor)
  "Return true if the Emacs minor version is <= to the given MINOR version."
  (<= emacs-minor-version minor))

;;;

(define-emacs-version-byte-optimizer emacs-minor-version=
  (byte-optimize-emacs-version-minor-predicate form '=))

(define-emacs-version-byte-optimizer emacs-minor-version/=
  (byte-optimize-emacs-version-minor-predicate form '/=))

(define-emacs-version-byte-optimizer emacs-minor-version>
  (byte-optimize-emacs-version-minor-predicate form '>))

(define-emacs-version-byte-optimizer emacs-minor-version<
  (byte-optimize-emacs-version-minor-predicate form '<))

(define-emacs-version-byte-optimizer emacs-minor-version>=
  (byte-optimize-emacs-version-minor-predicate form '>=))

(define-emacs-version-byte-optimizer emacs-minor-version<=
  (byte-optimize-emacs-version-minor-predicate form '<=))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Here are the functions that optimize invocations of the Emacs version
;;;; functions in byte-compiled code.  Basically these invocations are inlined,
;;;; the requested compile-time version information is inserted, and extraneous
;;;; code is removed.
;;;;
;;;; NOTE that these optimizations only work with the "new" optimizing byte-
;;;; compiler.  This compiler is a standard component of the FSF's GNU Emacs
;;;; version 19, Lucid GNU Emacs, and XEmacs.  In addition, the optimizing
;;;; byte-compiler is available for GNU Emacs 18 and for Epoch.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; First, define the functions that constitute the heart of the code walker...
;;;

;;;
;;; Function byte-optimize-emacs-version-walk-form
;;;
;;; This function is the heart of the code that optimizes calls to the Emacs
;;; version-examining functions.  This function takes four arguments:
;;;
;;;   + The Lisp form that is to be "walked" and emitted.  Typically, this is
;;;	the body (or a portion of the body) of the Emacs version-examining
;;;	function that was invoked.
;;;
;;;   + The list of the formal arguments to the function that was invoked.
;;;
;;;   + The list of the actual argument forms in the invocation.
;;;
;;;   + A list that indicates whether or not we "know" that certain of the
;;;	actual argument forms may always be substituted for the corresponding
;;;	formal arguments in the emitted Lisp form.
;;;
;;; There is a one-to-one correspondence between elements of the formal and
;;; actual argument lists.  The "force substitutions" list may be shorter than
;;; the formal and actual lists.
;;;
;;; In a nutshell, what this function does is "walk" the given Lisp form, make
;;; appropriate value-for-variable substitutions into the form, wrap the form
;;; with any necessary `let' bindings, and then return the result.  As far as
;;; code walkers go, this function is pretty stupid, but it gets the job done
;;; for all of our needs.
;;;

(defun byte-optimize-emacs-version-walk-form (form
					      formals-list
					      actuals-list
					      &optional force-substs-list)
  (if (/= (length actuals-list) (length formals-list))
      (error "Bad invocation of byte-optimize-emacs-version-walk-form."))
  
  (let ((bindings-list nil)
	(substitutions-list
	 ;; The list of substitutions starts off with the list of Emacs version
	 ;; variable values that should be substituted at compile-time.
	 (byte-optimize-emacs-version-compile-time-substitutions-list)))
    
    ;; Determine which of the actual arguments are substitutable and which are
    ;; not.
    (let ((actuals actuals-list)
	  (formals formals-list)
	  (force-substs force-substs-list))
      (while actuals
	(if (or ;; If we have been told to force this substitution...
		(car force-substs)
		;; ...or the actual argument form is inlineable...
		(byte-optimize-emacs-version-inlineable-form-p (car actuals))
		)
	    ;; ...then prepare to inline the actual argument form.
	    (setq substitutions-list (cons (cons (car formals) (car actuals))
					   substitutions-list))
	  ;; Otherwise, prepare to make an appropriate `let' binding.
	  (setq bindings-list (cons (list (car formals) (car actuals))
				    bindings-list)))
	(setq actuals (cdr actuals)
	      formals (cdr formals)
	      force-substs (cdr force-substs))
	))
    
    ;; If we are going to make any `let' bindings then we need to watch out for
    ;; variable captures in the substitutions.  That is, we have to make sure
    ;; that none of the inlined actual argument forms contain variable
    ;; references that will incorrectly refer to one of our new `let' bindings.
    (if bindings-list
	(let ((bindings bindings-list))
	  (while bindings
	    (if (byte-optimize-emacs-version-maybe-bad-binding-p
		 (car (car bindings))
		 substitutions-list)
		;; Rats!  The binding variable at the front of `bindings' is
		;; possibly capturing variable references from one or more of
		;; the substitution forms.  We can fix this by changing the
		;; offensive binding variable to something that won't capture
		;; any references from the substitutions.
		(let ((new-binding-variable
		       (byte-optimize-emacs-version-choose-let-binding-variable
			bindings-list
			substitutions-list)))
		  ;; Now, in the form to be walked, we have to update all of
		  ;; references to the old binding variable --- in other words,
		  ;; we need to make an appropriate substitution!
		  ;;
		  ;; (NOTE that this new substitution won't be scanned in the
		  ;; current `while' loop.  This is important --- obviously,
		  ;; this new substitution would appear to have been captured
		  ;; by a binding!  But in this case, it's intentional.)
		  (setq substitutions-list (cons (cons (car (car bindings))
						       new-binding-variable)
						 substitutions-list))
		  ;; Finally, fix the bad binding.
		  (rplaca (car bindings) new-binding-variable))
	      )
	    (setq bindings (cdr bindings))
	    )))
    
    ;; Finally, walk the form.  Make the appropriate substitutions and if
    ;; necessary, appropriate `let' bindings.  Return the new form.
    (let ((new-form (if substitutions-list
			(byte-optimize-emacs-version-sublis substitutions-list
							    form)
		      form)))
      (if bindings-list
	  (setq new-form (cons 'let (list (nreverse bindings-list) new-form))))
      new-form)
    ))

;;;
;;; Function byte-optimize-emacs-version-compile-time-substitutions-list
;;;
;;; This function is an auxiliary for `byte-optimize-emacs-version-walk-form',
;;; above.
;;;
;;; This function returns an association list that determines which of the
;;; Emacs version variable references will be replaced with constant values in
;;; the form walked by `...-walk-form'.  In other words, this function decides
;;; which of the Emacs version variable values will be determined at compile-
;;; time (by having the `...-walk-form' function hard-code their values into
;;; the to-be-compiled output forms).
;;;
;;; The value of the variable `byte-optimize-use-compile-time-version-info'
;;; determines the result of this function.  For more information, read the
;;; comments at the top of this file --- or the code below.  (This function
;;; would be nice and short if it didn't do any sanity checking.)
;;;

(defun byte-optimize-emacs-version-compile-time-substitutions-list ()
  (cond ((null byte-optimize-use-compile-time-version-info)
	 ;; Don't do any compile-time substitutions for references to the Emacs
	 ;; version variables.
	 nil)
	
	((listp byte-optimize-use-compile-time-version-info)
	 ;; The compile-time version information is described by an association
	 ;; list.
	 (let ((warning-string
		"The inappropriate value for `%s' in `%s' was ignored.")
	       (subst-list nil)
	       pair)
	   ;; Check for `emacs-type'.
	   (if (setq pair (assq 'emacs-type
				byte-optimize-use-compile-time-version-info))
	       (if (not (symbolp (cdr pair)))
		   (byte-compile-warn
		    warning-string 'emacs-type
		    'byte-optimize-use-compile-time-version-info)
		 (setq subst-list (cons (cons 'emacs-type
					      (list 'quote (cdr pair)))
					subst-list))
		 ))
	   ;; Check for `emacs-major-version'.
	   (if (setq pair (assq 'emacs-major-version
				byte-optimize-use-compile-time-version-info))
	       (if (not (and (integerp (cdr pair))
			     (>= (cdr pair) 0)))
		   (byte-compile-warn
		    warning-string 'emacs-major-version
		    'byte-optimize-use-compile-time-version-info)
		 (setq subst-list (cons (cons 'emacs-major-version (cdr pair))
					subst-list))
		 ))
	   ;; Check for `emacs-minor-version'.
	   (if (setq pair (assq 'emacs-minor-version
				byte-optimize-use-compile-time-version-info))
	       (if (not (and (integerp (cdr pair))
			     (>= (cdr pair) 0)))
		   (byte-compile-warn
		    warning-string 'emacs-minor-version
		    'byte-optimize-use-compile-time-version-info)
		 (setq subst-list (cons (cons 'emacs-minor-version (cdr pair))
					subst-list))
		 ))
	   subst-list))
	
	(t
	 ;; Use the current values of the Emacs version variables.
	 (let ((warning-string
		"The current value of `%s' is bogus!  It was ignored.")
	       (subst-list nil))
	   ;; Check for `emacs-type'.
	   (if (not (symbolp emacs-type))
	       (byte-compile-warn warning-string 'emacs-type)
	     (setq subst-list (cons (cons 'emacs-type
					  (list 'quote emacs-type))
				    subst-list)))
	   ;; Check for `emacs-major-version'.
	   (if (not (and (integerp emacs-major-version)
			 (>= emacs-major-version 0)))
	       (byte-compile-warn warning-string 'emacs-major-version)
	     (setq subst-list (cons (cons 'emacs-major-version
					  emacs-major-version)
				    subst-list)))
	   ;; Check for `emacs-minor-version'.
	   (if (not (and (integerp emacs-minor-version)
			 (>= emacs-minor-version 0)))
	       (byte-compile-warn warning-string 'emacs-minor-version)
	     (setq subst-list (cons (cons 'emacs-minor-version
					  emacs-minor-version)
				    subst-list)))
	   subst-list))
	))

;;;
;;; Function byte-optimize-emacs-version-inlineable-form-p
;;;
;;; This function is an auxiliary for `byte-optimize-emacs-version-walk-form',
;;; above.
;;;
;;; This function returns a true value if the given actual argument form is
;;; such that it may be safely inlined everywhere that the corresponding formal
;;; argument occurs in the Lisp form to be walked.  In other words, this
;;; function returns true when it is safe to evaluate the actual argument form
;;; any number of times, including zero times.  This is true for numbers,
;;; quoted forms, and references to variables (i.e., symbols).
;;;

(defun byte-optimize-emacs-version-inlineable-form-p (form)
  (or (numberp form)
      (and (listp form)
	   (eq (car form) 'quote))
      (symbolp form)))

;;;
;;; Function byte-optimize-emacs-version-maybe-bad-binding-p
;;;
;;; This function is an auxiliary for `byte-optimize-emacs-version-walk-form',
;;; above.
;;;
;;; This function returns a true value if the given `let' binding variable is
;;; possibly (lexically) referenced by one of the actual argument forms (in the
;;; SUBSTITUTIONS-LIST) that will be inlined into the Lisp form walked by
;;; `byte-optimize-emacs-version-walk-form'.  If this is the case, then the
;;; variable may inadvertently "capture" one of substitutions, and the walker
;;; will have to pick a new variable name.
;;;

(defun byte-optimize-emacs-version-maybe-bad-binding-p (variable
							substitutions-list)
  (let ((substs substitutions-list)
	(maybe-bad nil))
    (while (and substs
		(not maybe-bad))
      (setq maybe-bad (byte-optimize-emacs-version-maybe-bad-binding-p-aux
		       variable
		       (cdr (car substs)))
	    substs (cdr substs))
      )
    maybe-bad))

(defun byte-optimize-emacs-version-maybe-bad-binding-p-aux (variable form)
  ;; This could be much smarter, but stupidity is safe is this case.
  (cond ((eq variable form)
	 t)
	((consp form)
	 (or (byte-optimize-emacs-version-maybe-bad-binding-p-aux variable
								  (car form))
	     (byte-optimize-emacs-version-maybe-bad-binding-p-aux variable
								  (cdr form))
	     ))
	(t nil)))

;;;
;;; Function byte-optimize-emacs-version-choose-let-binding-variable
;;;
;;; This is yet another auxiliary for `byte-optimize-emacs-version-walk-form',
;;; above.  This function chooses a name for a variable to be bound by `let'.
;;; This new name must not conflict with any of the other bindings, nor may it
;;; cause any of the planned substitutions to be captured by the new binding.
;;;

(defun byte-optimize-emacs-version-choose-let-binding-variable (bindings-list
								substs-list)
  (let* ((count 1)
	 (symbol nil))
    (while (not symbol)
      (setq symbol (intern (format "%%temp-%d" count)))
      (if (or (assq symbol bindings-list)
	      (byte-optimize-emacs-version-maybe-bad-binding-p symbol
							       substs-list))
	  (setq symbol nil
		count (+ count 1))
	))
    symbol))

;;;
;;; Function byte-optimize-emacs-version-sublis
;;;
;;; This function is an auxiliary for `byte-optimize-emacs-version-walk-form',
;;; above.  This is the ordinary `sublis' function from Common Lisp: walk the
;;; given form, make the substitutions described by the SUBST-ALIST, and return
;;; the new form.
;;;

(defun byte-optimize-emacs-version-sublis (subst-alist form)
  (let ((subst-pair (assq form subst-alist)))
    (cond (subst-pair (cdr subst-pair))
	  ((consp form)
	   (let ((new-car (byte-optimize-emacs-version-sublis subst-alist
							      (car form)))
		 (new-cdr (byte-optimize-emacs-version-sublis subst-alist
							      (cdr form))))
	     (if (and (eq new-car (car form))
		      (eq new-cdr (cdr form)))
		 form
	       (cons new-car new-cdr))
	     ))
	  (t form))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; Now define the functions that use the above code walker to optimize
;;; invocations of the Emacs version-examining functions.
;;;

;;;
;;; Function byte-optimize-emacs-version-accessor
;;;
;;; This function optimizes invocations of the Emacs version accessor functions
;;; `emacs-type', `emacs-major-version', and `emacs-minor-version'.  These are
;;; very simple to optimize.
;;;
;;; FORM is the invocation form and ACCESSOR-BODY is the Lisp form that is the
;;; body of the assessor function.
;;;

(defun byte-optimize-emacs-version-accessor (form accessor-body)
  ;; Call this for effect.
  (byte-optimize-emacs-version-args form 0)
  ;; Now walk the form.
  (byte-optimize-emacs-version-walk-form accessor-body '() '()))

;;;
;;; Function byte-optimize-emacs-version-minor-predicate
;;;
;;; This function optimizes invocations of the Emacs minor version number
;;; predicate functions: `emacs-minor-version=' and its ilk.  These are also
;;; very simple to optimize.
;;;
;;; FORM is the invocation form and PREDICATE-FUNCTION is the name of the
;;; appropriate comparison function: `=', `/=', `>', `<', `>=', or `<='.
;;;

(defun byte-optimize-emacs-version-minor-predicate (form predicate-function)
  (byte-optimize-emacs-version-walk-form
   (cons predicate-function '(emacs-minor-version minor))
   '(minor)
   (byte-optimize-emacs-version-args form 1)
   '(t) ;; The actual argument form will always be evaluated.
   ))

;;;
;;; Function byte-optimize-emacs-version-major/minor-predicate
;;;
;;; This function optimizes invocations of the Emacs major/minor version number
;;; predicate functions: `emacs-version=' and its ilk.  The arguments to this
;;; function are:
;;;
;;;   + FORM, the invocation form.
;;;
;;;   + FALSE-MINOR-FORM, the Lisp form that should be emitted when the "minor
;;;	version" actual argument form is a false (i.e., `nil') constant.
;;;
;;;   + TRUE-MINOR-FORM, the Lisp form that should be emitted when the "minor
;;;	version" actual argument form is a true (i.e., non-`nil') constant.
;;;
;;; If the "minor version" actual argument form is not a constant, then the
;;; "false" and "true" forms are combined into an appropriate `if' form.  Of
;;; course, in all cases, the to-be-emitted form is walked by the function
;;; `byte-optimize-emacs-version-walk-form' in order to inline the appropriate
;;; values and so on.
;;;

(defun byte-optimize-emacs-version-major/minor-predicate (form
							  false-minor-form
							  true-minor-form)
  (let* ((actual-arg-list   (byte-optimize-emacs-version-args form 1 2))
	 (minor-actual-form (car (cdr actual-arg-list)))
	 (false-minor-p     (byte-optimize-emacs-version-false-const-p
			     minor-actual-form))
	 (true-minor-p      (byte-optimize-emacs-version-true-const-p
			     minor-actual-form))
	 (form              (cond (false-minor-p false-minor-form)
				  (true-minor-p true-minor-form)
				  (t (list 'if '(null minor)
					   false-minor-form true-minor-form))
				  ))
	 )
    (byte-optimize-emacs-version-walk-form
     form             ;; The form to be walked.
     '(major minor)   ;; The formal arguments.
     actual-arg-list  ;; The actual argument forms.
     (list ;;
	   ;; The `major' actual argument form may certainly be substituted if
	   ;; the `major' formal argument is referenced only once.  The
	   ;; assumption is that the `form' is such that the `major' argument
	   ;; will always be referenced, and therefore, the substituted actual
	   ;; form will always be evaluated.
	   ;;
	   (>= 1 (byte-optimize-emacs-version-count-tree 'major form))
	   ;;
	   ;; The `minor' actual argument form may certainly be substituted if
	   ;; it is a constant.  We could just put `nil' here --- the walker
	   ;; would figure out that the form is constant later on.
	   ;;
	   (or false-minor-p true-minor-p))
     )
    ))

;;;
;;; Function byte-optimize-emacs-version-args
;;;
;;; This function is an auxiliary for the three invocation-optimizer functions
;;; above.
;;;
;;; This function takes a function invocation form and returns a list of the
;;; actual argument forms from that invocation (with any omitted &optional
;;; argument forms translated into `nil's).  The argument MIN-ARGS is the
;;; minimum number of actual arguments that must be present; the optional
;;; argument MAX-ARGS is the maximum number.  If the number of actual arguments
;;; is unacceptable, this function signals an error.
;;;

(defun byte-optimize-emacs-version-args (form min-args &optional max-args)
  (if (null max-args)
      (setq max-args min-args))
  (let ((function-name (if (consp form)
			   (car form)
			 "a bogus Emacs version function")))
    (if (or (not (consp form))
	    (let ((tail form))
	      (while (consp (cdr tail))
		(setq tail (cdr tail)))
	      (cdr tail)))
	(error "Invocation of %s is not a proper list." function-name))
    
    (let* ((actuals (cdr form))
	   (actuals-count (length actuals)))
      (cond ((< actuals-count min-args)
	     (error "Too few arguments given to %s." function-name))
	    ((> actuals-count max-args)
	     (error "Too many arguments given to %s." function-name))
	    ((= actuals-count max-args)
	     actuals)
	    (t
	     ;; Fill in the missing &optional arguments with `nil's.
	     (append actuals (make-list (- max-args actuals-count) nil)))
	    )
      )))

;;;
;;; Functions byte-optimize-emacs-version-false-const-p,
;;;           byte-optimize-emacs-version-true-const-p, and
;;;           byte-optimize-emacs-version-count-tree
;;;
;;; These functions are simple auxiliaries for `byte-optimize-emacs-version-
;;; major/minor-predicate', above.  Their purposes should be obvious.
;;;

(defun byte-optimize-emacs-version-false-const-p (form)
  ;; Return true if FORM is a false constant.
  (or (eq form 'nil)
      (equal form '(quote nil))
      ))

(defun byte-optimize-emacs-version-true-const-p (form)
  ;; Return true if FORM is a true constant.
  (or (numberp form)
      (and (listp form)
	   (eq (car form) 'quote)
	   (not (equal form '(quote nil))))
      ))

(defun byte-optimize-emacs-version-count-tree (item form)
  ;; Count and return the number of occurrences of ITEM with FORM.
  (cond ((eq item form)
	 1)
	((consp form)
	 (+ (byte-optimize-emacs-version-count-tree item (car form))
	    (byte-optimize-emacs-version-count-tree item (cdr form))))
	(t 0)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Finally, here is the `provide' statement.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'emacs-vers)

;; End of file.

