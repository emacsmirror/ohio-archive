; Path: dg-rtp!uunet!uunet!think.com!yale.edu!yale!mintaka.lcs.mit.edu!ai-lab!life.ai.mit.edu!friedman
; From: friedman@nutrimat.gnu.ai.mit.edu (Noah Friedman)
; Newsgroups: gnu.emacs.sources
; Subject: insert-hooks.el
; Date: 22 Nov 91 20:13:31 GMT
; Reply-To: friedman@prep.ai.mit.edu
; Organization: Free Software Foundation, 675 Mass Ave. Cambridge, MA 02139
; 
; 
;    Here's something I whipped up the other day primarily so I could add
; hooks to do-auto-save, only to discover it won't work right because it's a
; primitive.  Well, maybe someone else will find this useful.
; 
;;; insert-hooks, a function to insert pre- and post- hooks in an
;;; arbitrary function.
;;;
;;; LCD Archive Entry:
;;; insert-hooks|Noah Friedman|friedman@nutrimat.gnu.ai.mit.edu
;;; |Insert pre- and post- hooks in an arbitrary function.
;;; |91-11-22||~/functions/insert-hooks.el.Z|
;;;
;;; Copyright (C) 1991 Noah S. Friedman
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
;;; along with this program; if not, you can either send email to this
;;; program's author (see below) or write to: The Free Software
;;; Foundation, Inc., 675 Massachusetts Avenue, Cambridge, MA 02139, USA.  
;;;
;;; Please send requests for copies of the GPL, bug reports, fixes,
;;; comments, flames, etc. to friedman@prep.ai.mit.edu
;;;
;;; Last modified 22-Nov-91
;;;

;;;    Have you ever wished that a particular command or lisp function
;;; ran some sort of hooks so that you could customize its behavior,
;;; but didn't want to have to maintain your own copy of the function
;;; in question?  Well, here's a possible alternative. 
;;;
;;;    insert-hooks will take a named hook and add forms to the
;;; indicated function to run that hook as the very first thing it
;;; does.  If the optional third argument to insert-hooks is non-nil,
;;; then the forms to run that hook as the very *last* thing will be
;;; inserted into the function.  This is done in such a way that any
;;; return value from the original form of the function will not be
;;; lost. 
;;;
;;;    Note: modifying a primitive function can have unexpected
;;; results, because, while other true lisp routines may call the
;;; modified function, other subrs will call the original primitive.
;;; So for example, if you were planning to modify the `do-auto-save'
;;; subr, emacs would probably not pay any attention to your
;;; definition because of the way do-auto-save is called (i.e. by
;;; another subr).  However, it's possible to redefine some subrs and
;;; get useful results because they are usually only called from lisp
;;; code (e.g.  `current-time-string').
;;;
;;; I don't know for certain how robust this is.  I couldn't think of
;;; any cases where insert-hook will *break* a function, but I
;;; definitely want to know if you find an example.  The only bug I
;;; know of (and it can't be helped) is that the docstring for
;;; primitive functions will have all the keybindings resolved at the
;;; time insert-hooks is called, so if you rebind keys later, the
;;; docstring may be inaccurate.  This is because I had to use the
;;; `documentation' function which resolves the special syntax for key
;;; bindings in docstrings.
;;;
;;; Do not confuse insert-hooks with the planned-for add-hooks
;;; primitive in emacs 19.  That function adds forms to a hook itself,
;;; it doesn't add forms to a function for running hooks.

;;; Thanks to Roland McGrath for several ideas here (via his
;;; make-interactive function)

;;; Todo: 
;;;   1) determine whether a given hook is already present in the
;;;      function, and don't mutilate function more if it is (possibly
;;;      ask before proceeding)
;;;   2) Another function, remove-hooks, to undo parts or all of
;;;      changes made by insert-hooks.
;;;   3) option to re-byte-compile modified functions if they were
;;;      byte-compiled in the first place.

(require 'backquote)

(defun insert-hooks (function-to-frob head-hook &optional tail-hook)
  "If HEAD-HOOK, a symbol referring to a named hook, is non-nil, insert
a form in FUNCTION-TO-FROB that will run that hook before anything
else is done.

If optional TAIL-HOOK is non-nil, insert a form in FUNCTION-TO-FROB
that will run the hook as the last thing done. 

Return value is new definition of FUNCTION-TO-FROB."
  (let* ((func (symbol-function function-to-frob))
         (function-interactivep (commandp func))
         ;; Three possible results for function-interactive-form.  If
         ;; function-interactivep is `t', then function-interactive-form
         ;; represents the form calling `interactive' without any arguments.
         ;; Since commandp returns the interactive form if there's an argument,
         ;; function-interactive-form gets set to whatever commandp returns,
         ;; e.g. (interactive "P")
         (function-interactive-form
          (if (eq t function-interactivep)
              '(interactive)
            function-interactivep))
         ;; temp. variable to hold partial construction of new function.
         function-precursor)  
    (if (subrp func)
        ;; If function is a subr, define a wrapper around the subr to
        ;; do hooks. 
        (progn
          (if tail-hook
              ;; Insert form to run subr, saving return value in the process.
              ;; Run tail-hook, then return value from subr.
              (fset 'function-precursor
                    ;; Must also preserve state of interactive ability
                    (if function-interactivep
                        (` (let ((return-value 
                                  ;; Call subr interactively if this function
                                  ;; was called interactively (keep in mind
                                  ;; this form is being inserted into the new
                                  ;; function, not eval'ed in insert-hooks!)
                                  (if (interactive-p)
                                      (call-interactively (, func))
                                    (apply (, func) args))))
                             (run-hooks '(, tail-hook))
                             return-value))
                      ;; never interactive
                      (` (let ((return-value (apply (, func) args)))
                           (run-hooks '(, tail-hook))
                           return-value))))
            ;; No tail hook provided, so just apply function (no need to save
            ;; return values or run hooks in this case)
            (fset 'function-precursor
                  ;; preserve interactiveness
                  (if function-interactivep
                      (` (if (interactive-p)
                             (call-interactively (, func))
                           (apply (, func) args)))
                    ;; never interactive
                    (` (apply (, func) args)))))
          ;; Now finish constructing function
          (if head-hook
              (fset function-to-frob
                    ;; If head-hook is defined, insert form for
                    ;; running run-hooks.
                    (` (lambda (&rest args)
                         (, (documentation func))      ;might be nil
                         (, function-interactive-form) ;might also be nil
                         (run-hooks '(, head-hook))
                         (, (symbol-function 'function-precursor)))))
            (fset function-to-frob 
                  (` (lambda (&rest args)
                       (, (documentation func))       ;might be nil
                       (, function-interactive-form)  ;might also be nil
                       (, (symbol-function 'function-precursor)))))))
      ;; func not subr
      (let ((args (car (cdr func)))
            doc 
            body)
        (setq doc (car (nthcdr 2 func)))
        (if (stringp doc)
            (setq body (nthcdr 3 func))
          (setq doc nil
                body (nthcdr 2 func)))
        ;; remove function-interactive-form from body since body will not be a
        ;; top-level form anymore anyway (the interactive-form will be
        ;; re-inserted at the top-level elsewhere).
        (and function-interactive-form
             (setq body (delq function-interactive-form body)))
        (if tail-hook
            (fset 'function-precursor
                  ;; extra parens around form ((let ...)) so that below, when
                  ;; we evaluate (,@ (symbol-function 'function-precursor)), we
                  ;; get the proper form. 
                  (` ((let ((return-value (progn (,@ body))))
                       (run-hooks '(, tail-hook))
                       return-value))))
          (fset 'function-precursor body))
        (if head-hook
            (fset function-to-frob 
                  (` (lambda (, args)
                       (, doc)          ;might be nil
                       (, function-interactive-form) ;might also be nil
                       (run-hooks '(, head-hook))
                       (,@ (symbol-function 'function-precursor)))))
          (fset function-to-frob
                (` (lambda (, args)
                     (, doc)            ;might be nil
                     (, function-interactive-form) ;might also be nil
                     (,@ (symbol-function 'function-precursor))))))))))

;;; eof
