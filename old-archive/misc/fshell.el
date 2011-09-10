;;; fshell.el --- enhancements to shell.el

;; Copyright (C) 1988, 1993, 1994 Free Software Foundation, Inc.
;; Copyright (C) 1994, 1995 Noah S. Friedman

;; Author: Noah Friedman <friedman@prep.ai.mit.edu>
;; Maintainer: friedman@prep.ai.mit.edu
;; Keywords: extensions, processes
;; Created: 1994-06-21

;; LCD Archive Entry:
;; fshell|Noah Friedman|friedman@prep.ai.mit.edu|
;; Enhancements to shell.el|
;; 15-Mar-1995|1.4|~/misc/fshell.el.Z|

;; $Id: fshell.el,v 1.4 1995/03/15 00:36:30 friedman Exp $

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

;; If you give M-x fshell a prefix arg after loading this, it will create a
;; new shell buffer even if one already exists.  If you give it an explicit
;; numeric prefix arg, it will try to switch to that numbered shell buffer, or
;; create it.
;;
;; The alternative is to rename the current shell buffer and invoke M-x shell,
;; which is more keystrokes, especially if you decide to name your old shell
;; back when you're done with the newer one.

;; rms declined to add this functionality in emacs' shell.el, so I'm
;; maintaining it separately.

;;; Code:

(require 'shell)

;;;###autoload
(defvar fshell-default-make-new-shell nil
  "*If non-`nil', reverse the meaning of prefix arg to \\[fshell]")

;; See comments near fshell-pop-to-buffer for an explanation.
;;;###autoload (add-hook 'same-window-regexps "^\\*shell\\*\\(\\|<[0-9]+>\\)")

;;;###autoload
(defun fshell (&optional prefix)
  "Run an inferior shell, with I/O through buffer *shell*.

If buffer exists but shell process is not running, make new shell.
If buffer exists and shell process is running, just switch to buffer \"*shell*\".
If an explicit numeric prefix argument is given (or this function is called
  from lisp with a numeric argument), switch to the buffer named *shell*<prefix>
  e.g. \"*shell*<2>\".  If there is no process in that buffer, start one.
If a prefix argument is given but it is not a number, create a new buffer
  and start a shell process in it.  This is the same as calling the function
  from lisp with an argument of `t'.

The previous paragraph describes the behavior of this function whenever it
is called from lisp.  If it is called interactively and the variable
`fshell-default-make-new-shell' is non-`nil', then the meaning of
non-numeric prefix arguments is reversed,
i.e. typing `\\[fshell]' without a prefix argument creates a new shell,
and `\\[universal-argument] \\[fshell]' would switch to the buffer \"*shell*\".

Program used comes from variable `explicit-shell-file-name',
 or (if that is nil) from the ESHELL environment variable,
 or else from SHELL if there is no ESHELL.
If a file `~/.emacs_SHELLNAME' exists, it is given as initial input
 (Note that this may lose due to a timing error if the shell
  discards input when it starts up.)
The buffer is put in Shell mode, giving commands for sending input
and controlling the subjobs of the shell.  See `shell-mode'.
See also the variable `shell-prompt-pattern'.

The shell file name (sans directories) is used to make a symbol name
such as `explicit-sh-args'.  If that symbol is a variable,
its value is used as a list of arguments when invoking the shell.
Otherwise, one argument `-i' is passed to the shell.

Type \\[describe-mode] in the shell buffer for a list of commands."
  (interactive "P")

  (and fshell-default-make-new-shell
       (interactive-p)
       (not (numberp prefix))
       (setq prefix (not prefix)))

  (let ((shell-buffer "*shell*"))
    (cond
     ((and (null prefix)
           (comint-check-proc shell-buffer))
      (fshell-pop-to-buffer shell-buffer))
     ;; This next case is done all in the predicate (including side effects
     ;; like fshell-pop-to-buffer) to avoid extra string consing via multiple
     ;; concats.
     ((and (numberp prefix)
           (let ((bufname (concat shell-buffer "<" prefix ">")))
             (and (comint-check-proc bufname)
                  (fshell-pop-to-buffer bufname)))))
     (t
      (cond
       ((numberp prefix)
        (setq shell-buffer (concat shell-buffer "<" prefix ">")))
       (t
        (setq shell-buffer (generate-new-buffer-name shell-buffer))))
      (let* ((prog (or explicit-shell-file-name
                       (getenv "ESHELL")
                       (getenv "SHELL")
                       "/bin/sh"))
             (name (file-name-nondirectory prog))
             (startfile (concat "~/.emacs_" name))
             (xargs-name (intern-soft (concat "explicit-" name "-args"))))
	(set-buffer (apply 'fshell-make-shell shell-buffer prog
			   (if (file-exists-p startfile) startfile)
			   (if (and xargs-name (boundp xargs-name))
			       (symbol-value xargs-name)
			     '("-i"))))
        (shell-mode)
        (fshell-pop-to-buffer (current-buffer)))))))

;; This is just like comint.el:make-comint, except that it doesn't
;; implicitly put asterisks around the buffer name; it is assumed that's
;; already been done if it's desired.
(defun fshell-make-shell (name program &optional startfile &rest switches)
  "Make a comint process NAME in a buffer, running PROGRAM.
If there is already a running process in that buffer, it is not restarted.
Optional third arg STARTFILE is the name of a file to send the contents of to
the process.  Any more args are arguments to PROGRAM."
  (let ((buffer (get-buffer-create name)))
    ;; If no process, or nuked process, crank up a new one and put buffer in
    ;; comint mode.  Otherwise, leave buffer and existing process alone.
    (cond ((not (comint-check-proc buffer))
	   (save-excursion
	     (set-buffer buffer)
	     (comint-mode)) ; Install local vars, mode, keymap, ...
	   (comint-exec buffer name program startfile switches)))
    buffer))

;; Starting in Emacs 19.29, the variable same-window-regexps modifies how
;; pop-to-buffer works; in particular, if the name of the buffer being
;; switched to matches one of the regexps in same-window-regexps (which may
;; be buffer-local to the current buffer), then pop-to-buffer acts like
;; switch-to-buffer.  This gives users more control.
;; This also explains the related autoload cookie near the top of the file.
(defun fshell-pop-to-buffer (buffer)
  (if (fboundp 'same-window-regexps)
      (pop-to-buffer buffer)
    (switch-to-buffer buffer)))

(provide 'fshell)

;;; fshell.el ends here
