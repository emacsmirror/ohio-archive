;;; completing-help.el --- facility to display descriptions of completions

;; Copyright (C) 2000 Yuji 'bmonkey' Minejima <ggb01164@nifty.ne.jp>

;; Author: Yuji Minejima <ggb01164@nifty.ne.jp>
;; $Revision: 2.15 $
;; Keywords: local, help

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;; Commentary:

;; While entering an argument to a command which supports completion
;; in the minibuffer, by pressing `?', you see possible completions
;; of what you've entered so far. This is one of the standard features
;; of Emacs.
;;
;; With this library, you can see not only possible completions, but
;; also their descriptions.
;;
;; This library explicitly supports a certain number of standard
;; commands, and be able to infer, to some extent, what is appropriate
;; as descriptions, based on knowledge of the type of information you
;; are entering, so your custom commands or other standard commands
;; might work right off the bat without requiring any configuration
;; on your part.


;; "Completion" in the minibuffer:

;; Some commands ask you to enter the name of something (e.g. a file
;; name to visit, a command name to execute, etc.) in the minibuffer
;; (which sits at the bottom line of Emacs.)
;;
;; If those commands support Emacs standard "completion" mechanism,
;; you can get Emacs to guess what you want to enter.
;;
;; Halfway entering the information, you press <TAB> to give Emacs
;; a cue to guess and then complete what you've already entered in
;; the minibuffer.
;;
;; Emacs tries to fill in the missing part of your input as far as
;; possible, but sometimes Emacs can't be sure what you want to enter
;; because there are some possible alternatives. In that case, Emacs
;; displays those alternatives ("completions" in Emacs jargon) in a
;; help window hoping you will give it further clues.
;; 
;; If you press `?' instead of <TAB>, Emacs immediately displays
;; completions in a help window without doing completion in the
;; minibuffer.
;;
;; See "(emacs)Completion" ("Completion" section of the GNU Emacs manual)
;; for the canonical explanation.


;; Typical scenarios:

;; #1: Use with `M-x' (execute-extended-command)      see "(emacs)M-x"
;;
;;     When you want to invoke a command which is not bound to any
;;     key, (note this might imply that you're not that familiar with
;;     the command), you enter `M-x <COMMAND-NAME-YOU-WANT-TO-INVOKE>'.
;;
;;     While entering <COMMAND-NAME-YOU-WANT-TO-INVOKE> part, you
;;     can use completion. In the following description, we use
;;     `apropos' command as an example.
;;     
;;     You enter `M-x aprop<TAB>'. => Emacs completes it to "apropos".
;;     You enter <TAB>             => Emacs says "Complete, but not unique".
;;
;;     You've learned that there're some commands whose name start with
;;     "apropos", and wonder what they are.
;;     
;;     You enter `?'               => Something like the following is
;;                                    displayed in a help window.
;;
;;     ----------------------------------------------------------------
;;     Click mouse-2 on a completion to select it.
;;     In this buffer, type RET to select the completion near point.
;;                                                                     
;;     Possible completions are:
;;     apropos
;;       <C-h C-a>
;;       Show all bound symbols whose names match REGEXP.
;;       With optional prefix ARG or if `apropos-do-all' is non-nil, a$
;;       symbols and key bindings, which is a little more time-consumi$
;;       Returns list of symbols and documentation found.
;;                                        
;;     apropos-command
;;       <C-h a, menu-bar help-menu describe apropos-commands>
;;       Show commands (interactively callable functions) that match R$
;;       With optional prefix ARG, or if `apropos-do-all' is non-nil, $
;;       noninteractive functions.
;;       If VAR-PREDICATE is non-nil, show only variables, and only th$
;;       satisfy the predicate VAR-PREDICATE.
;;                                        
;;     apropos-documentation
;;       Show symbols whose documentation contain matches for REGEXP.
;;     -----------------------------------------------------------------
;;       *note* If the frame of your Emacs is wider than 79 columns, you
;;              probably see the whole descriptions without truncation.
;;
;;     You can either,
;;       * click on an item (usually the middle button of your mouse) to
;;         invoke a command,
;;       * go into the help window (M-v), then select an item (<RET>)
;;         to invoke a command, or just move around then go back into
;;         the minibuffer (C-x o),
;;       * scroll the help window (M-C-v),
;;       * or continue to enter in the minibuffer.
;;     See "(emacs)Completion" for other key bindings.


;; Requirement/compatibility:

;; Tested with FSF Emacs 20.6.2 and XEmacs 21.1.9.
;;
;; This library works with `icomplete.el' and `complete.el'
;; (both of which also enhance the completion mechanism.)
;;
;; I added support for, and tested with the following standard commands.
;;     bookmark-jump            (C-x r b)
;;     customize-group          (M-x customize-group, also on the menu-bar)
;;     customize-option         (M-x customize-option, also on the menu-bar)
;;     describe-function        (C-h f)
;;     describe-variable        (C-h v)
;;     execute-extended-command (M-x)
;;     find-function            (M-x find-function)
;;     find-variable            (M-x find-variable)
;;     setenv                   (M-x setenv)
;;     where-is                 (C-h w)
;;
;; Other commands might work with this library automatically.


;; Install:

;; * Put this file in one of the directories listed in `load-path'.
;;   You can see the contents of `load-path' by entering
;;   `M-x customize-option <RET> load-path'.
;;
;; * Enter `M-x byte-compile-file <RET>
;;          <DIR-YOU-PUT-THIS-FILE-IN>/completing-help.el <RET>'
;;   to byte-compile this file.
;;
;; * Put the following lines in your .emacs file.
;;
;;   (autoload 'completing-help-mode "completing-help"
;;             "Activate a facility to display descriptions of completions."
;;             t nil)
;;   (autoload 'completing-help-mode-turn-on "completing-help"
;;             "Turn on a facility to display descriptions of completions."
;;             t nil)
;;   (autoload 'completing-help-mode-turn-off "completing-help"
;;             "Turn off a facility to display descriptions of completions."
;;             t nil)
;;
;; * Restart Emacs or enter `M-x load-library <RET> completing-help'.


;; Activation:

;; * Enter `M-x completing-help-mode-turn-on' to activate this library.
;; * Enter `M-x completing-help-mode-turn-off' to deactivate this library.


;; Customization:

;; * Enter `M-x customize-group <RET> completing-help' to customize
;;   this library.
;;   You might need to enter `M-x load-library <RET> completing-help'
;;   in advance.


;; How this library works:

;; The key idea is to give `display-completion-list' a piece of "advice"
;; to display descriptions of completions.
;; I used "defadvice" facility of Emacs Lisp to do this.
;; See "(elisp)Completion Commands" and "(elisp)Advising Functions".
;;
;; The argument to `display-completion-list' is normally a list of
;; completions just returned by `all-completions'.
;; But it can also be a list whose element is a list of two strings,
;; which is printed as if the strings were concatenated.
;;
;; Our "advice" takes the argument to `display-completion-list' then
;; tries to add appropriate descriptions, using `completing-help-alist'.
;;
;;
;; When `display-completion-list' is called, this library's "advice" to
;; the function intercepts the call, then...
;;
;; 1: Calls the original `display-completion-list' unless the current
;;    value of `this-command' is a `member' of
;;    `completing-help-display-commands'.
;;
;; 2: Runs predicate functions which are the `car' of each element of
;;    `completing-help-alist' which looks like the following:
;;       ((predicate1 . description-function1)
;;        (predicate2 . description-function2)
;;        ... ).
;;
;;    If a predicate returns non-nil, stop calling the predicates,
;;    and then goto 3.
;;
;;    Calls the original `display-completion-list' after all of
;;    the predicates return `nil'.
;;
;; 3: Calls the description-function whose associated predicate has
;;    just returned non-nil, with the original argument to
;;    `display-completion-list'.
;;
;; 4: Calls the original `display-completion-list' with the return value
;;    of the description function.
;;


;;; Change Log:

;; Version 2.15 (26 May 2000):
;;  * Changed the name from "bm-hcmplt.el" to "completing-help.el",
;;    suggested by Francesco Potorti`.
;;  * Fixed `completing-help-delete-from-alist' to work.
;;  * Improved `completing-help-add-alist-str-dot-str-descriptions' and
;;      `completing-help-add-alist-str-pair-descriptions' to work with
;;      multiple lines.
;;  * Fixed an installation documentation bug, (incorrect `autoload' forms)
;;    reported by Hans van Dam.
;;  * Added some declarations to suppress byte-compiler warnings.
;;  * Added support for a simple user defined obarray.

;; Version 1.41 (18 May 2000):
;;  * Added `completing-help-get-keys' to display command key bindings, which
;;      was borrowed from Ken Manheimer's icomplete.el.

;; Version 1.39 (18 May 2000):
;;  * Fixed some comment error.
;;  * Added `completing-help-load-hook'

;; Version 1.36 (16 May 2000):
;;  * Added some documentation.
;;  * Added support for `bookmark-jump', `customize-group',
;;      `customize-option', `setenv', and 2 types of simply-structured alists.
;;  * Changed to display the whole documentation of ELisp objects.
;;  * Added a customization group for the default formatting mechanism.

;; Version 1.6 (06 May 2000):
;;  * Fixed `completing-help-function-wanted-p' and
;;      `completing-help-variable-wanted-p'
;;      to work properly.

;; Version 1.5 (06 May 2000):
;;  * First public release of this package.


;;; Code:

;;; customization ====================================================
(defgroup completing-help nil
  "Facility to display descriptions of completions.
While entering an argument to a command which supports completion
in the minibuffer, by pressing `?', you see possible completions
of what you've entered so far. This is one of the standard features
of Emacs.

With this library, you can see not only possible completions, but
also their descriptions.

This library explicitly supports a certain number of standard
commands, and be able to infer, to some extent, what is appropriate
as descriptions, based on knowledge of the type of information you
are entering, so your custom commands or other standard commands
might work right off the bat without requiring any configuration
on your part."
  :group 'minibuffer
  :group 'help)


(defcustom completing-help-mode nil
  "Toggle a facility to display descriptions of completions.
Setting this variable directly does not take effect;
use either \\[customize] or the commands `completing-help-mode',
`completing-help-mode-turn-on', and `completing-help-mode-turn-off'."
  :set        #'(lambda (symbol value) (completing-help-mode (if value 1 -1)))
  :initialize 'custom-initialize-default
  :type       'boolean
  :group      'completing-help
  :require    'completing-help)


(defcustom completing-help-display-commands
  '(minibuffer-completion-help PC-completion-help)
  "Minibuffer commands which should display descriptions of completions.
The default commands are usually activated via `?' or `M-?' in the minibuffer.
You should specify commands implemented in ELisp which call
`display-completion-list' inside them.
If you are an XEmacs user and want to see descriptions when you enter
<TAB>, add `minibuffer-complete'."
  :type '(repeat function)
  :group 'completing-help)


(defgroup completing-help-default-format nil
  "Default format used to display descriptions of completions."
  :group 'completing-help)

(defcustom completing-help-completions-buffer-truncate-lines t
  "Non-nil means `truncate-lines' option is set for the completions buffer."
  :type  'boolean
  :group 'completing-help-default-format)


(defcustom completing-help-description-start-column 2
  "Column position at which all description lines start."
  :type  'number
  :group 'completing-help-default-format)


(defvar completing-help-min-line-width 40
  "Long enough width to prevent `display-completion-list' from item-cramming.
The default value is slightly larger than the one hardcoded in minibuf.c.")


(defcustom completing-help-description-delimiter
  (make-string completing-help-min-line-width ?\ )
  "Used to separate each record of completions."
  :type       'string
  :set        #'(lambda (symbol value)
                  (let* ((right-space-num (- completing-help-min-line-width
                                            (length value)))
                         (right-spaces (when (> right-space-num 0)
                                         (make-string right-space-num ?\ ))))
                    (set symbol (concat value right-spaces))))
  :initialize 'custom-initialize-default
  :group      'completing-help-default-format
  :require    'completing-help)


(defvar completing-help-alist
  '()
  "Alist which is the core of `completing-help-mode'.
Used in a piece of \"advice\" to `display-completion-list'.
Usually looks like the following:
    ((predicate1 . description-function1)
     (predicate2 . description-function2))")


;;; modification functions for `completing-help-alist'
(defun completing-help-add-to-alist (pred func)
  "Register PRED and FUNC in `completing-help-alist'.
Search is done using PRED as a key.
If PRED is already registered, replace the old function with FUNC.
Otherwise, cons (PRED . FUNC) onto the rest."
  (let ((assoc (assoc pred completing-help-alist)))
    (if assoc
        (setcdr assoc func)
      (setq completing-help-alist
            (cons (cons pred func)
                  completing-help-alist)))))


(defun completing-help-delete-from-alist (pred)
  "Delete an association of PRED from `completing-help-alist'."
  (setq completing-help-alist
        (delete nil
                (mapcar
                 #'(lambda (assoc)
                     (if (eq (car assoc) pred)
                         nil
                       assoc))
                 completing-help-alist))))


;;;###autoload
(defun completing-help-mode (&optional prefix)
  "Activate a facility to display descriptions of completions.
Deactivate with negative universal argument.
This behavior is the same as that of `icomplete-mode'."
  (interactive "p")
  (or prefix (setq prefix 0))
  (cond
   ((>= prefix 0)
    (setq completing-help-mode t))
   (t (setq completing-help-mode nil))))


;;;###autoload
(defun completing-help-mode-turn-on ()
  "Turn on a facility to display descriptions of completions."
  (interactive)
  (completing-help-mode 1))


;;;###autoload
(defun completing-help-mode-turn-off ()
  "Turn off a facility to display descriptions of completions."
  (interactive)
  (completing-help-mode -1))


(defvar completing-help-completion-setup-hook
  nil
  "")

(add-hook 'completing-help-completion-setup-hook
          #'(lambda ()                  ; set/reset truncate-line option
              (with-current-buffer standard-output
                (setq truncate-lines
                      completing-help-completions-buffer-truncate-lines))))


;;; advice ===========================================================
(eval-when-compile                      ; suppress compiler warnings
  (defvar completion-highlight-first-word-only))

(defadvice display-completion-list (around completing-help
                                           activate preactivate)
  "Advice to add descriptions of completions."
  (if (or (not completing-help-mode)
          (not (member this-command completing-help-display-commands)))
      ad-do-it
    (let ((completion-highlight-first-word-only
           (when (boundp 'completion-highlight-first-word-only)
             completion-highlight-first-word-only)) ; for xemacs
          (completion-setup-hook (append completion-setup-hook nil))

          ;; create a playground
          (completing-help-completions-buffer-truncate-lines
           completing-help-completions-buffer-truncate-lines)
          (completing-help-description-start-column
           completing-help-description-start-column)
          (completing-help-min-line-width completing-help-min-line-width)
          (completing-help-description-delimiter
           completing-help-description-delimiter)
          
          (alist completing-help-alist)
          (completions (ad-get-arg 0)))
      (catch 'done
        (while alist
          (when (funcall (car (car alist)))
            (setq completion-highlight-first-word-only t)
            (add-hook 'completion-setup-hook
                      #'(lambda ()
                          (run-hooks 'completing-help-completion-setup-hook))
                      "APPEND AFTER COMPLETION-SETUP-FUNCTION")
            (ad-set-arg 0 (funcall (cdr (car alist)) completions))
            (throw 'done t))
          (setq alist (cdr alist))))
      ad-do-it)))


(defadvice minibuffer-completion-help (around completing-help
                                              activate preactivate)
  "Advice to emulate the original C function in Emacs Lisp.
`completing-help' package needs to call `display-completion-list'
within Emacs Lisp."
  (message "Making completion list...")
  (let ((completions (all-completions (buffer-string)
                                      minibuffer-completion-table
                                      minibuffer-completion-predicate)))
    (message nil)
    (if (null completions)
        ;; Sole purpose of this ad-do-it is to display " [No completions]"
        ad-do-it
      (with-output-to-temp-buffer "*Completions*"
        (display-completion-list (sort completions 'string<))))))


(defvar completing-help-completing-read-command
  nil
  "Command which is directly/indirectly calling `completing-read'.")

(defvar completing-help-completing-read-args
  nil
  "List of arguments to the current invocation of `completing-read'.")

(defadvice completing-read (around completing-help first
                                   activate preactivate)
  "Record the caller command and the arguments."
  (let ((completing-help-completing-read-command this-command)
        (completing-help-completing-read-args (ad-get-args 0)))
    ad-do-it))



;;; Utility functions to add descriptions ----------------------------
(defmacro completing-help-split-string (doc)
  `(split-string ,doc "[\f\n\r\v]+"))

(defun completing-help-format-description (item &optional desc &rest desc-rest)
  "Return a formatted string of DESC and DESC-REST.
Peculiarity of `display-completion-list':
 (display-completion-list
    ((\"Item1\" \"Description1\") (\"Item2\" \"Description2\")))

 * An item and its description is displayed like \"Item1Description1\".
 * A description shouldn't end with newline to prevent the indentation of
   the following lines from getting messy.
 * (length (concat \"Item1\" \"Description\")) should be greater than
   35 .. 40 to prevent 2 \"ItemDescriptions\"s from getting crammed in
   the same line."
  (let* ((left-space      (make-string
                           completing-help-description-start-column ?\ ))
         (right-space-num (- completing-help-min-line-width
                            completing-help-description-start-column
                            (length desc)))
         (formatted-desc  (concat
                           (if (> completing-help-description-start-column
                                  (length item))
                               (make-string
                                (- completing-help-description-start-column
                                   (length item)) ?\ )
                             (when (not (zerop (length desc)))
                               (concat " \n" left-space)))
                           desc
                           (when (> right-space-num 0)
                             (make-string right-space-num ?\ )))))
    (while desc-rest
      (setq right-space-num (- completing-help-min-line-width
                               completing-help-description-start-column
                               (length (car desc-rest)))

            formatted-desc  (concat formatted-desc
                                    "\n" left-space
                                    (car desc-rest)
                                    (when (> right-space-num 0)
                                      (make-string right-space-num ?\ )))

            desc-rest (cdr desc-rest)))

    (setq formatted-desc (concat formatted-desc
                                 "\n"
                                 completing-help-description-delimiter))))

(defun completing-help-add-descriptions (completions get-formatted-doc-func)
  "Change each element of COMPLETIONS to a list of the original element and its description.
Return COMPLETIONS."
  (let ((rest completions)
        item-str)
    (while rest
      (setq item-str (car rest))
      (setcar rest (list item-str (funcall get-formatted-doc-func item-str)))
      (setq rest (cdr rest)))
    completions))


;;; alist ((str . str) (str . str) ...) support ----------------------
(defun completing-help-alist-str-dot-str-p ()
  (and (consp minibuffer-completion-table)
       (stringp (cdar minibuffer-completion-table))))

(defun completing-help-add-alist-str-dot-str-descriptions (completions)
  (completing-help-add-descriptions
   completions
   #'(lambda (key)
       (apply 'completing-help-format-description
              key
              (completing-help-split-string
               (cdr (assoc key minibuffer-completion-table)))))))

  
(completing-help-add-to-alist
 'completing-help-alist-str-dot-str-p
 'completing-help-add-alist-str-dot-str-descriptions)


;;; alist ((str str ...) (str str ...) ...) support ------------------
(defun completing-help-alist-str-pair-p ()
  (and (consp minibuffer-completion-table)
       (consp (cdar minibuffer-completion-table))
       (stringp (car (cdar minibuffer-completion-table)))))

(defun completing-help-add-alist-str-pair-descriptions (completions)
  (completing-help-add-descriptions
   completions
   #'(lambda (key)
       (apply 'completing-help-format-description
              key
              (completing-help-split-string
               (car (cdr (assoc key minibuffer-completion-table))))))))
  
(completing-help-add-to-alist 'completing-help-alist-str-pair-p
                             'completing-help-add-alist-str-pair-descriptions)


;;; user obarray support ---------------------------------------------
(defun completing-help-user-obarray-p ()
  (and (vectorp minibuffer-completion-table)
       (not (eq minibuffer-completion-table obarray))))


(defun completing-help-add-user-obarray-descriptions (completions)
  (completing-help-add-descriptions
   completions
   #'(lambda (sym-name)
       (let* ((symbol (intern sym-name minibuffer-completion-table))
              (doc (when (and (boundp symbol)
                              (stringp (symbol-value symbol)))
                     (symbol-value symbol))))
         (apply 'completing-help-format-description
                sym-name
                (completing-help-split-string doc))))))

(completing-help-add-to-alist 'completing-help-user-obarray-p
                             'completing-help-add-user-obarray-descriptions)


;;; Emacs Lisp functions support -------------------------------------
(defvar completing-help-function-predicates
  '(byte-code-function-p commandp functionp subrp fboundp)
  "Set of predicates identifying various kinds of Emacs Lisp functions.")


(defun completing-help-function-wanted-p ()
  "Return t if the current minibuffer session seems to want a ELisp function."
  (member minibuffer-completion-predicate completing-help-function-predicates))


(defun completing-help-get-keys (func-name)
  "Return strings naming keys bound to `func-name', or nil if none.
I borrowed icomplete-get-keys from Ken Manheimer's icomplete.el.
If we're in the minibuffer, check the keymap of (other-buffer),
otherwise check that of (current-buffer)."
  (if (commandp func-name)
    (save-excursion
      (let* ((sym (intern func-name))
             (buf (if (active-minibuffer-window)
                      (if (featurep 'xemacs)
                          (other-buffer nil nil "VISIBLE-OK")
                        (other-buffer nil "VISIBLE-OK"))
                    (current-buffer)))
             (map (save-excursion (set-buffer buf) (current-local-map)))
             (keys (where-is-internal sym map)))
        (if keys
            (concat "<"
                    (mapconcat 'key-description
                               (sort keys
                                     #'(lambda (x y)
                                         (< (length x) (length y))))
                               ", ")
                    ">"))))))

(defun completing-help-add-function-descriptions (completions)
  "Add descriptions to COMPLETIONS which is full of ELisp function names."
  (completing-help-add-descriptions
   completions
   #'(lambda (sym-name)
       (let ((doc (or (documentation (intern sym-name)) "")))
         (apply 'completing-help-format-description
                sym-name
                (completing-help-get-keys sym-name)
                (completing-help-split-string doc))))))


(completing-help-add-to-alist 'completing-help-function-wanted-p
                             'completing-help-add-function-descriptions)



;;; Emacs Lisp variables support -------------------------------------
(defvar completing-help-variable-predicates
  '(user-variable-p boundp)
  "Set of predicates identifying various kinds of Emacs Lisp variables.")


(defvar completing-help-variable-completing-read-commands
  '(customize-option     customize-option-other-window
    customize-variable   customize-variable-other-window))


(defun completing-help-variable-wanted-p ()
  "Return t if the current minibuffer session seems to want a ELisp variable."
  (or
   (member minibuffer-completion-predicate completing-help-variable-predicates)
   (member completing-help-completing-read-command
           completing-help-variable-completing-read-commands)))


(defun completing-help-add-variable-descriptions (completions)
  "Add descriptions to COMPLETIONS which is full of ELisp variable names."
  (completing-help-add-descriptions
   completions
   #'(lambda (sym-name)
       (let ((doc (or (documentation-property (intern sym-name)
                                              'variable-documentation)
                      "")))
         (apply 'completing-help-format-description
                sym-name
                (completing-help-split-string doc))))))


(completing-help-add-to-alist 'completing-help-variable-wanted-p
                             'completing-help-add-variable-descriptions)



;;; Emacs Lisp custom groups support ---------------------------------
(defvar completing-help-custom-group-completing-read-commands
  '(customize-group customize-group-other-window))


(defun completing-help-custom-group-wanted-p ()
  ""
  (member completing-help-completing-read-command
          completing-help-custom-group-completing-read-commands))


(defun completing-help-add-custom-group-descriptions (completions)
  ""
  (completing-help-add-descriptions
   completions
   #'(lambda (sym-name)
       (let ((doc (or (get (intern sym-name) 'group-documentation) "")))
         (apply 'completing-help-format-description
                sym-name
                (completing-help-split-string doc))))))


(completing-help-add-to-alist 'completing-help-custom-group-wanted-p
                             'completing-help-add-custom-group-descriptions)



;;; Emacs Lisp custom faces support -----------------------------------
(eval-when-compile                      ; suppress compiler warnings
  (autoload 'face-property "faces"))

(defvar completing-help-custom-face-predicates
  '(custom-facep find-face)
  "")

(defun completing-help-custom-face-wanted-p ()
  ""
  (or (member minibuffer-completion-predicate
              completing-help-custom-face-predicates)
      (eq (nth 5 completing-help-completing-read-args) 'face-history)))


(defun completing-help-add-custom-face-descriptions (completions)
  ""
  (completing-help-add-descriptions
   completions
   #'(lambda (sym-name)
       (let* ((symbol (intern sym-name))
              (doc (or (if (fboundp 'face-property) ; for xemacs
                           (face-property (find-face symbol) 'doc-string)
                         (get symbol 'face-documentation))
                       "")))
         (apply 'completing-help-format-description
                sym-name
                (completing-help-split-string doc))))))


(completing-help-add-to-alist 'completing-help-custom-face-wanted-p
                             'completing-help-add-custom-face-descriptions)



;;; bookmark.el support ----------------------------------------------
(eval-when-compile
  (defvar bookmark-alist)
  (autoload 'bookmark-get-filename   "bookmark")
  (autoload 'bookmark-get-annotation "bookmark"))

(defun completing-help-bookmark-wanted-p ()
  ""
  (and (boundp 'bookmark-alist)
       (eq minibuffer-completion-table bookmark-alist)))

(defun completing-help-add-bookmark-descriptions (completions)
  ""
  (completing-help-add-descriptions
   completions
   #'(lambda (bookmark-name)
       (apply 'completing-help-format-description
              bookmark-name
              (bookmark-get-filename bookmark-name)
              (bookmark-get-annotation bookmark-name)))))

(completing-help-add-to-alist 'completing-help-bookmark-wanted-p
                             'completing-help-add-bookmark-descriptions)

;;; env.el support ---------------------------------------------------
(defun completing-help-environment-variable-wanted-p ()
  (eq completing-help-completing-read-command 'setenv))

(defun completing-help-add-environment-variable-descriptions (completions)
  (completing-help-add-descriptions
   completions
   #'(lambda (env-name)
       (completing-help-format-description env-name
                                     (concat "=" (getenv env-name))))))

(completing-help-add-to-alist
 'completing-help-environment-variable-wanted-p
 'completing-help-add-environment-variable-descriptions)



;;; Hook
(defvar completing-help-load-hook nil
  "Hook to run at the end of loading completing-help.")

(provide 'completing-help)
(run-hooks 'completing-help-load-hook)


;;; completing-help.el ends here
