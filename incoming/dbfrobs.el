;;; dbfrobs.el --- useful enhancements for debugging

;;; Copyright (C) 1994, 1995 Noah S. Friedman

;; Author: Noah Friedman <friedman@prep.ai.mit.edu>
;; Maintainer: friedman@prep.ai.mit.edu
;; Keywords: extensions, debug, frobs
;; Status: works in Emacs 19 and XEmacs.
;; Created: 1994-10-18

;; LCD Archive Entry:
;; dbfrobs|Noah Friedman|friedman@gnu.ai.mit.edu|
;; useful enhancements for debugging|
;; $Date: 1997/06/14 12:13:06 $|$Revision: 1.8 $|~/functions/dbfrobs.el.gz|

;; $Id: dbfrobs.el,v 1.8 1997/06/14 12:13:06 friedman Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 675 Massachusetts Avenue; Cambridge, MA 02139, USA.

;;; Commentary:

;; The user interface for this package consists of the following commands:
;;
;;        debug-on-interesting-errors
;;        toggle-debug-on-error
;;        debug-on-condition
;;        cancel-debug-on-condition
;;
;; In addition, it modifies debugger mode to run
;; `after-debugger-mode-hook' after setting up the debugger but before the
;; user begins interacting with it.

;;; Code:

(require 'advice)
(require 'backquote)


(defvar dbfrobs::uninteresting-error-conditions
  '(beginning-of-buffer
    buffer-read-only
    end-of-buffer
    end-of-file
    error
    file-error
    mark-inactive
    quit)
  "Error conditions which are not considered worth debugging.
Users generally should not modify this variable directly, but instead use
the functions `dbfrobs:debug-on-condition' and
`dbfrobs:cancel-debug-on-condition'.")

;; Conditions saved when toggle-debug-on-error sets debug-on-error to nil.
(defvar dbfrobs::saved-debug-on-error nil)

;;;###autoload
(defvar after-debugger-mode-hook nil
  "*Forms to execute after everything else in `debugger-mode'.

This hook is useful if, for example, you wish to disable line truncation
automatically.  To do so, load this file and put the following in your
.emacs:

    (add-hook 'after-debugger-mode-hook
              '(lambda ()
                 (setq truncate-lines nil)))")


;;;###autoload
(defun dbfrobs:debug-on-interesting-errors ()
  "Enter the debugger when \"interesting\" errors occur.
That is, set `debug-on-error' to the list of error conditions returned by
the function `dbfrobs:interesting-error-conditions'."
  (interactive)
  (setq debug-on-error (dbfrobs:interesting-error-conditions))
  (and (interactive-p)
       (message "%s" debug-on-error))
  debug-on-error)

;;;###autoload
(defun dbfrobs:toggle-debug-on-error ()
  "Toggle the current state of whether to debug on errors or not.
If debugging is currently enabled for any conditions at all, it
is turned off, but the list of conditions are saved.
If debugging is currently disabled, it is reenabled for any conditions
saved from previously disablement.  If no conditions have ever been saved,
debugging for all conditions is enabled.

Use `dbfrobs:debug-on-interesting-errors' or
`dbfrobs:debug-on-condition' if you wish to be more selective."
  (interactive)
  (cond (debug-on-error
         (setq dbfrobs::saved-debug-on-error debug-on-error)
         (setq debug-on-error nil))
        (dbfrobs::saved-debug-on-error
         (setq debug-on-error dbfrobs::saved-debug-on-error))
        (t
         (setq debug-on-error t)))
  (and (interactive-p)
       (message "%s" debug-on-error))
  debug-on-error)

(defun dbfrobs:error-conditions ()
  "Return a list of all known error conditions.
This means find all symbol names which have an `error-condition' property."
  (let (symlist)
    (mapatoms
     (function
      (lambda (sym)
        (and (get sym 'error-conditions)
             (setq symlist (cons sym symlist))))))
    symlist))

(defun dbfrobs:interesting-error-conditions ()
  "Return a list of interesting error conditions.
This is all known error conditions but those listed in the variable
`dbfrobs::uninteresting-error-conditions'."
  (let ((conditions (dbfrobs:error-conditions))
        (uninteresting dbfrobs::uninteresting-error-conditions))
    (while uninteresting
      (setq conditions (delq (car uninteresting) conditions))
      (setq uninteresting (cdr uninteresting)))
    conditions))

(defun dbfrobs:debug-on-condition-p (err-sym)
  "Return `t' if the symbol ERR-SYM is currently a debuggable error condition.
That is, if ERR-SYM is an error condition and debug-on-error is set so that
such a signal would presently invoke the lisp debugger, return `t'.

If debug-on-error is `nil' or does not include ERR-SYM or one of its
conditions, return `nil'."
  (cond
   ((null debug-on-error) nil)
   ((listp debug-on-error)
    (let ((errs (get err-sym 'error-conditions))
          (debugp nil))
      (while errs
        (if (memq (car errs) debug-on-error)
            (setq debugp t
                  errs nil)
          (setq errs (cdr errs))))
      debugp))
   (t t)))

(defun dbfrobs::symbol-list->obarray (list)
  (let ((new-obarray (make-vector (length list) 0)))
    (while list
      (intern (symbol-name (car list)) new-obarray)
      (setq list (cdr list)))
    new-obarray))

(defun dbfrobs::set-condition (type condition &optional permanent)
  (cond
   ((eq type 'interesting)
    (and (listp debug-on-error)
         (or (memq condition debug-on-error)
             (setq debug-on-error (cons condition debug-on-error))))
    (and permanent
         (setq dbfrobs::uninteresting-error-conditions
               (delq condition dbfrobs::uninteresting-error-conditions))))

   ((eq type 'uninteresting)
    (and (listp debug-on-error)
         (setq debug-on-error (delq condition debug-on-error)))
    (and permanent
         (not (memq condition dbfrobs::uninteresting-error-conditions))
         (setq dbfrobs::uninteresting-error-conditions
               (cons condition dbfrobs::uninteresting-error-conditions)))))
  debug-on-error)

;;;###autoload
(defun dbfrobs:debug-on-condition (condition &optional permanentp)
  "Make emacs trigger the debugger when condition CONDITION occurs.

If called interactively, this command prompts for a condition name out of
all known conditions that aren't already in the debugger list.  If called
with a prefix argument, this command also updates the \"permanent\" list of
uninteresting error conditions (`dbfrobs:uninteresting-error-conditions')
so that subsequent calls to `debug-on-interesting-conditions' will
automatically include this new condition.  Otherwise, calling
`dbfrobs:debug-on-interesting-conditions' may reset the list of debugging
conditions without including CONDITION.

If called as a lisp function, any symbol name may be given as a condition
name, with the second argument PERMANENTP indicating whether to update
`dbfrobs:uninteresting-error-conditions'."
  (interactive (list (intern (completing-read
                              "Debug on condition: "
                              (dbfrobs::symbol-list->obarray
                               (dbfrobs:error-conditions))
                              ;; Predicate to limit completion to one not
                              ;; already in debug-on-error.  Note the need
                              ;; to get the real symbol from the global
                              ;; obarray.
                              (function
                               (lambda (sym)
                                 (not (memq (intern (symbol-name sym))
                                            debug-on-error))))
                              'require-match))
                     (if current-prefix-arg t nil)))
  (dbfrobs::set-condition 'interesting condition permanentp))

;;;###autoload
(defun dbfrobs:cancel-debug-on-condition (condition &optional permanentp)
  "Refrain from triggering the debugger when condition CONDITION occurs.

If called interactively, this command prompts for a condition name out of
all known conditions in the debugger list.  If called with a prefix
argument, this command also updates the \"permanent\" list of uninteresting
error conditions (`dbfrobs:uninteresting-error-conditions') so that
subsequent calls to `debug-on-interesting-conditions' will automatically
exclude this new condition.  Otherwise, calling
`dbfrobs:debug-on-interesting-conditions' may reset the list of debugging
conditions to include CONDITION.

If called as a lisp function, any symbol name may be given as a condition
name, with the second argument PERMANENTP indicating whether to update
`dbfrobs:uninteresting-error-conditions'."
  (interactive (list (intern (completing-read
                              "Debug on condition: "
                              (dbfrobs::symbol-list->obarray
                               (and (listp debug-on-error)
                                    debug-on-error))
                              nil
                              'require-match))
                     (if current-prefix-arg t nil)))
  (dbfrobs::set-condition 'uninteresting condition permanentp))


(defun dbfrobs::insert-hooks (function head-hook &optional tail-hook)
  "Rewrite the function FUNCTION to run hooks.

If second argument HEAD-HOOK, a symbol referring to a named hook, is
non-nil, insert a form in FUNCTION that will run that hook before anything
else is done.

If optional third argument TAIL-HOOK is non-nil, insert a form in FUNCTION
that will run the hook as the last thing done.

Return value is new definition of FUNCTION.

FUNCTION may be an autoloaded function, subr, or lambda-expression, but may
not be a macro."
  (if head-hook
      (ad-add-advice
       function
       (ad-make-advice head-hook nil t
                       (` (advice lambda ()
                                  (, (format "Runs hook %s before all else."
                                             head-hook))
                                  (run-hooks '(, head-hook)))))
       'before 'first))
  (if tail-hook
      (ad-add-advice
       function
       (ad-make-advice tail-hook nil t
                       (` (advice lambda ()
                                  (, (format "Runs hook %s after all else."
                                             tail-hook))
                                  (run-hooks '(, tail-hook)))))
       'after 'last))
  (ad-activate function (byte-code-function-p (indirect-function function))))

;; XEmacs (as of 19.13) does not have eval-after-load or load-history.
(defun dbfrobs::eval-after-load(key &rest forms)
  "Add all FORMS to KEY in `auto-load-alist' that aren't already present.
If KEY is not in auto-load-alist, it and all FORMS are added.
If KEY is library which is already loaded, immediately evaluate all of the
forms which were not already present."
  (let ((node (assoc key after-load-alist))
        (new nil))
    (cond (node
           (while forms
             (or (member (car forms) (cdr node))
                 (setq new (cons (car forms) new)))
             (setq forms (cdr forms)))
           (cond (new
                  (setq new (nreverse new))
                  (nconc (cdr node) new))))
          (t
           (setq after-load-alist (cons (apply 'list key forms)
                                        after-load-alist))
           (setq new forms)))
    ;; Eval new forms now if library was already loaded.
    (and (boundp 'load-history)
         (assoc key load-history)
         (while new
           (eval (car new))
           (setq new (cdr new))))))


;; For user convenience:

;;;###autoload
(defalias 'debug-on-interesting-errors 'dbfrobs:debug-on-interesting-errors)

;;;###autoload
(defalias 'toggle-debug-on-error 'dbfrobs:toggle-debug-on-error)

;;;###autoload
(defalias 'debug-on-condition 'dbfrobs:debug-on-condition)

;;;###autoload
(defalias 'cancel-debug-on-condition 'dbfrobs:cancel-debug-on-condition)

;; No version of emacs presently implements after-debugger-mode-hook, so we
;; must modify `debugger-mode' with advice to use it.  Add an entry to
;; after-load-alist to implement this.  Also modify debugger-mode now, if
;; it's already loaded.
(let ((fn (if (fboundp 'eval-after-load)
              'eval-after-load
            'dbfrobs::eval-after-load)))
  (funcall fn "debug"
    '(dbfrobs::insert-hooks 'debugger-mode nil 'after-debugger-mode-hook)))

;; See if it ought to be fixed right away.
(and (fboundp 'debugger-mode)
     (dbfrobs::insert-hooks 'debugger-mode nil 'after-debugger-mode-hook))


(provide 'dbfrobs)

;;; dbfrobs.el ends here
