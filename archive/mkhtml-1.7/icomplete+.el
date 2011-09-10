;;; icomplete+.el --- Extensions to `icomplete.el'
;; 
;; Author: D. ADAMS
;; Maintainer: D. ADAMS
;; Copyright (C) 1996-2001, Drew Adams, all rights reserved.
;; Created: Mon Oct 16 13:33:18 1995
;; Version: $Id: icomplete+.el,v 1.3 2001/01/03 17:38:03 dadams Exp $
;;   Last modified by: 
;;   Last modified on: Wed Jan  3 09:38:00 2001
;;   Update count: 250
;; Keywords: help, abbrev, internal, extensions, local
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary:
;;
;;    Extensions to `icomplete.el'.
;;
;;  New user options (variables) defined here:
;;
;;    `icomplete-choices-face', `icomplete-determined-face'.
;;
;;
;;  ***** NOTE: The following functions defined in `icomplete.el'
;;              have been REDEFINED HERE:
;;
;;  `icomplete-completions' -
;;     Sorts alternatives and puts them in a different face.
;;  `icomplete-exhibit' - Doesn't insert if input begins with `('
;;
;;
;;  ***** NOTE: The following EMACS PRIMITIVES have been REDEFINED HERE
;;
;;  `read-from-minibuffer' -
;;     Resets `minibuffer-completion-table' to avoid icompletion.
;;  `read-no-blanks-input' - 
;;     Resets `minibuffer-completion-table' to avoid icompletion.
;;  `read-string' -
;;     Resets `minibuffer-completion-table' to avoid icompletion.
;;
;;
;;  This file should be loaded after loading the standard GNU file
;;  `icomplete.el'.  So, in your `~/.emacs' file, do this:
;;  (eval-after-load "icomplete" '(progn (require 'icomplete+)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; RCS $Log: icomplete+.el,v $
;; RCS Revision 1.3  2001/01/03 17:38:03  dadams
;; RCS *** empty log message ***
;; RCS
;; RCS Revision 1.2  2001/01/03 00:42:20  dadams
;; RCS *** empty log message ***
;; RCS
;; RCS Revision 1.1  2000/09/14 17:20:31  dadams
;; RCS Initial revision
;; RCS
; Revision 1.1  1997/03/20  07:57:31  dadams
; Initial revision
;
; Revision 1.15  1996/07/01  13:13:25  dadams
; (trivial)
;
; Revision 1.14  1996/06/14  13:46:53  dadams
; Updated file header Commentary to mention new user option defined here.
;
; Revision 1.13  1996/04/26  09:32:02  dadams
; Put escaped newlines on long-line strings.
;
; Revision 1.12  1996/04/05  14:28:46  dadams
; Improved Commentary:  List redefinitions.
;
; Revision 1.11  1996/03/20  16:07:31  dadams
; yes-or-no-p, read-string, read-from-minibuffer, read-no-blanks-input:
;     defun -> defsubst.
;
; Revision 1.10  1996/03/08  12:19:51  dadams
; 1. Copyright.
; 2. drew-faces.el -> std-faces.el.
;
; Revision 1.9  1996/02/12  10:00:37  dadams
; Updated header keywords (for finder).
;
; Revision 1.8  1996/02/06  11:00:56  dadams
; (trivial) Do face stuff only if can (boundp).
;
; Revision 1.7  1995/12/28  15:44:16  dadams
; 1. Require icomplete.el here (no longer in start.el).
; 2. Added ;;;###autoloads.
;
; Revision 1.6  1995/12/15  08:20:43  dadams
; Defined replacements that reset minibuffer-completion-table to avoid
; icompletion: read-string, read-from-minibuffer, read-no-blanks-input.
;
; Revision 1.5  1995/12/04  16:41:23  dadams
; dark-goldenrod-foreground-face -> darkgoldenrod-foreground-face
; sea-green-foreground-face -> seagreen-foreground-face
;
; Revision 1.4  1995/11/30  16:52:20  dadams
; Moved fset's to col 1 so imenu picks them up (cosmetic).
;
; Revision 1.3  1995/11/30  16:32:40  dadams
; Added redefinition of yes-or-no-p.
;
; Revision 1.2  1995/10/17  15:26:01  dadams
; 1) Cleanup (require cl.el).
; 2) Require drew-faces.el (forgot).
; 3) Added icomplete-choices-face and icomplete-determined-face.
; 4) Added icomplete-exhibit from icomplete.el: Doesn't insert if input
;    begins with `('  (e.g. repeat-complex-command).
;
; Revision 1.1  1995/10/16  15:32:31  dadams
; Initial revision
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code: 

(require 'icomplete)
(require 'cl) ;; when, unless

;; Get macro `define-face-const' when this is compiled,
;; or run interpreted, but not when the compiled code is loaded.
(eval-when-compile (require 'def-face-const))


(provide 'icomplete+)

;;;;;;;;;;;;;;;;;;;


;;;###autoload
(unless (boundp 'darkgoldenrod-foreground-face)
  (define-face-const "DarkGoldenrod" nil))
(defvar icomplete-choices-face darkgoldenrod-foreground-face
  "*Face for minibuffer reminder of possible completion suffixes.")

;;;###autoload
(unless (boundp 'seagreen-foreground-face)
  (define-face-const "SeaGreen" nil))
(defvar icomplete-determined-face seagreen-foreground-face
  "*Face for minibuffer reminder of possible completion prefix.")

;;;@@@Emacs20 (or (fboundp 'old-yes-or-no-p)
;;;@@@Emacs20 (fset 'old-yes-or-no-p (symbol-function 'yes-or-no-p)))

;;;@@@Emacs20 ;; REPLACES ORIGINAL (built-in function):
;;;@@@Emacs20 ;; Temporarily sets `icomplete-inhibit' to non-nil.
;;;@@@Emacs20 ;;
;;;@@@Emacs20 ;; (For some reason, the standard `yes-or-no-p' doesn't nullify
;;;@@@Emacs20 ;; `minibuffer-completion-table', and that is tested by
;;;@@@Emacs20 ;; `icomplete-pre-command-hook' to see if needs to do icompletion.)
;;;@@@Emacs20 (defsubst yes-or-no-p (prompt)
;;;@@@Emacs20   "Ask user a yes-or-no question.  Return t if answer is yes.
;;;@@@Emacs20 Takes one argument, which is the string to display to ask the question.
;;;@@@Emacs20 It should end in a space; `yes-or-no-p' adds `(yes or no) ' to it.
;;;@@@Emacs20 The user must confirm the answer with RET,
;;;@@@Emacs20 and can edit it until it has been confirmed."
;;;@@@Emacs20   (let ((icomplete-inhibit t)) (old-yes-or-no-p prompt)))


;; REPLACES ORIGINAL defined in `icomplete.el':
;; Doesn't insert if input begins with `(' (e.g. `repeat-complex-command').
;;;###autoload
(defun icomplete-exhibit ()
  "Insert icomplete completions display.
Should be run via minibuffer `post-command-hook'.
See `icomplete-mode' and `minibuffer-setup-hook'."
  (when (icomplete-simple-completing-p)
    (let ((contents (buffer-substring (point-min) (point-max)))
          (buffer-undo-list t))
      (save-excursion
        (goto-char (point-max))
                                        ; Register the end of input, so we
                                        ; know where the extra stuff
                                        ; (match-status info) begins:
        (unless (boundp 'icomplete-eoinput)
          ;; In case it got wiped out by major mode business:
          (make-local-variable 'icomplete-eoinput))
        (setq icomplete-eoinput (point))
                                        ; Insert the match-status information:
        (when (and (> (point-max) 1)
                   (save-excursion
                     (goto-char (point-min))
                     (not (looking-at   ; No (, ", ', 9 etc. at start.
                           "\\(\\s-+$\\|\\s-*\\(\\s(\\|\\s\"\\|\\s'\\|\\s<\\|\
[0-9]\\)\\)")))
		   (or
		    ;; Don't bother with delay after certain number of chars:
		    (> (point-max) icomplete-max-delay-chars)
		    ;; Don't delay if alternatives number is small enough:
		    (if minibuffer-completion-table
			(cond ((numberp minibuffer-completion-table)
			       (< minibuffer-completion-table
				  icomplete-delay-completions-threshold))
			      ((sequencep minibuffer-completion-table)
			       (< (length minibuffer-completion-table)
				  icomplete-delay-completions-threshold))
			      ))
		    ;; Delay - give some grace time for next keystroke, before
		    ;; embarking on computing completions:
		    (sit-for icomplete-compute-delay)))
          (insert-string
           (icomplete-completions contents minibuffer-completion-table
                                  minibuffer-completion-predicate
                                  (not minibuffer-completion-confirm))))))))


;; REPLACES ORIGINAL defined in `icomplete.el':
;; Sorts alternatives and puts them in a different face.
;;;###autoload
(defun icomplete-completions (name candidates predicate require-match)
  "Identify prospective candidates for minibuffer completion.

The display is updated with each minibuffer keystroke during
minibuffer completion.

Prospective completion suffixes (if any) are displayed, bracketed by
\"()\", \"[]\", or \"{}\".  The choice of brackets is as follows:

  \(...) - A single prospect is identified and matching is enforced.
  \[...] - A single prospect is identified and matching is optional.
  \{...} - Multiple prospects, separated by commas, are indicated,
           and further input is required to distinguish a single one.

The displays for unambiguous matches have \" [ Matched ]\" appended
\(whether complete or not), or \" \[ No match ]\", if no eligible
matches exist.  \(Keybindings for uniquely matched commands
are exhibited within the square braces.)"
  ;; 'all-completions' doesn't like empty
  ;; minibuffer-completion-table's (ie: (nil))
  (when (and (listp candidates) (null (car candidates)))
    (setq candidates nil))
  (let ((comps (sort (all-completions name candidates predicate) 'string<))
                                        ; "-determined" - only one candidate
        (open-bracket-determined (if require-match "   (" "   ["))
        (close-bracket-determined (if require-match ")" "]"))
                                        ;"-prospects" - more than one candidate
        (open-bracket-prospects "     { ")
        (close-bracket-prospects " }")
        prompt determined-part)
    (catch 'input
      (cond ((null comps)
             (setq prompt (setq determined-part
                                (format "\t%sNo match%s"
                                        open-bracket-determined
                                        close-bracket-determined))))
            ((null (cdr comps))         ;one match
             (setq prompt 
                   (setq determined-part
                         (concat (if (and (> (length (car comps))
                                             (length name)))
                                     (concat open-bracket-determined
                                             (substring (car comps)
                                                        (length name))
                                             close-bracket-determined)
                                   "")
                                 "\t[Matched"
                                 (let ((keys (and icomplete-show-key-bindings
                                                  (commandp (intern-soft (car comps)))
                                                  (icomplete-get-keys (car comps)))))
                                   (if keys
                                       (concat "; " keys)
                                     ""))
                                 "]"))))
            (t                          ;multiple matches
             (let* ((most (try-completion name candidates
                                          (and predicate
                                               ;; Wrap predicate in impatience - ie,
                                               ;; `throw' up when pending input is
                                               ;; noticed.  Adds some overhead to
                                               ;; predicate, but should be worth it.
                                               (function
                                                (lambda (item)
                                                  (if (input-pending-p)
                                                      (throw 'input "")
                                                    (apply predicate
                                                           item nil)))))))
                    (most-len (length most))
                    most-is-exact
                    (alternatives
                     (substring
		      (apply
                       (function concat)
                       (mapcar (function
                                (lambda (com)
                                  (if (input-pending-p) (throw 'input ""))
                                  (if (= (length com) most-len)
                                      ;; Most is one exact match,
                                      ;; note that and leave out
                                      ;; for later indication:
                                      (progn (setq most-is-exact t)
                                             nil)
                                    (concat ", "
                                            (substring com most-len)))))
                               comps))
                      1)))
               (setq prompt
                     (concat (and (> most-len (length name))
                                  (setq determined-part
                                        (concat open-bracket-determined
                                                (substring most (length name))
                                                close-bracket-determined)))
                             open-bracket-prospects
                             (if most-is-exact
                                 ;; Add a ',' at the front to indicate "complete but
                                 ;; not unique":
                                 (concat "," alternatives)
                               alternatives)
                             close-bracket-prospects)))))
      (put-text-property (length determined-part) (length prompt)
                         'face icomplete-choices-face prompt)
      (put-text-property 0 (length determined-part)
                         'face icomplete-determined-face prompt)
      prompt)))


;;; The following functions have been REDEFINED to reset the
;;; `minibuffer-completion-table' in order to avoid icompletion.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Note:  The function `read-input' is an alias for `read-string'.

(or (fboundp 'old-read-string)
(fset 'old-read-string (symbol-function 'read-string)))

;; REPLACES ORIGINAL:
;; Resets `minibuffer-completion-table' to avoid icompletion.
(defsubst read-string
  (prompt &optional initial-input history default-value inherit-input-method)
  "Read a string from the minibuffer, prompting with string PROMPT.
If non-nil, optional 2nd arg INITIAL-INPUT is a string to insert
    before reading.
The third arg HISTORY, if non-nil, specifies a history list and
    optionally the initial position in that list.
    See `read-from-minibuffer' for details of HISTORY argument.
Fourth arg DEFAULT-VALUE is the default value.  If non-nil, it is used
    for history commands and as the value to return if user enters an
    empty string.
Fifth arg INHERIT-INPUT-METHOD, if non-nil, means minibuffer inherits
    current input method and setting of `enable-multibyte-characters'."
  (setq minibuffer-completion-table nil) ; So won't icomplete by default.
  (old-read-string prompt initial-input history default-value inherit-input-method))


(or (fboundp 'old-read-from-minibuffer)
(fset 'old-read-from-minibuffer (symbol-function 'read-from-minibuffer)))

;; REPLACES ORIGINAL:
;; Resets `minibuffer-completion-table' to avoid icompletion.
(defsubst read-from-minibuffer
  (prompt
   &optional initial-contents keymap read hist default-value inherit-input-method)
  "Read a string from the minibuffer, prompting with string PROMPT.
If optional second arg INITIAL-CONTENTS is non-nil, it is a string
  to be inserted into the minibuffer before reading input.
  If INITIAL-CONTENTS is (STRING . POSITION), the initial input
  is STRING, but point is placed at position POSITION in the minibuffer.
Third arg KEYMAP is a keymap to use while reading;
  if omitted or nil, the default is `minibuffer-local-map'.
If fourth arg READ is non-nil, then interpret the result as a lisp object
  and return that object:
  in other words, do `(car (read-from-string INPUT-STRING))'
Fifth arg HIST, if non-nil, specifies a history list
  and optionally the initial position in the list.
  It can be a symbol, which is the history list variable to use,
  or it can be a cons cell (HISTVAR . HISTPOS).
  In that case, HISTVAR is the history list variable to use,
  and HISTPOS is the initial position (the position in the list
  which INITIAL-CONTENTS corresponds to).
  Positions are counted starting from 1 at the beginning of the list.
Sixth arg DEFAULT-VALUE is the default value.  If non-nil, it is
  available for history commands; but `read-from-minibuffer' does NOT
  return DEFAULT-VALUE if the user enters empty input.
  It returns the empty string.
Seventh arg INHERIT-INPUT-METHOD, if non-nil, means the minibuffer
  inherits the current input method and the setting of
  `enable-multibyte-characters'.
If variable `minibuffer-allow-text-properties' is non-nil, then the
  string returned includes whatever text properties were present in
  the minibuffer.  Otherwise the value has no text properties."
  (setq minibuffer-completion-table nil) ; So won't icomplete by default.
  (old-read-from-minibuffer
   prompt initial-contents keymap read hist default-value inherit-input-method))


(or (fboundp 'old-read-no-blanks-input)
(fset 'old-read-no-blanks-input (symbol-function 'read-no-blanks-input)))

;; REPLACES ORIGINAL:
;; Resets `minibuffer-completion-table' to avoid icompletion.
(defsubst read-no-blanks-input (prompt &optional initial-contents inherit-input-method)
  "Read a string from the minibuffer, not allowing blanks.
Arg PROMPT is a prompt string.

If optional 2nd arg INITIAL-CONTENTS is non-nil, it is a string
to be inserted into the minibuffer before reading input.

Third arg INHERIT-INPUT-METHOD, if non-nil, means the minibuffer
inherits the current input method and the setting of 
`enable-multibyte-characters'."
  (setq minibuffer-completion-table nil) ; So won't icomplete by default.
  (old-read-no-blanks-input prompt initial-contents inherit-input-method))




;;; Turn on icompletion.
(icomplete-mode 99)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; `icomplete+.el' ends here

