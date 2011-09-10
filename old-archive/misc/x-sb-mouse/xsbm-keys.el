;;; xsbm-keys.el : key bindings for x-sb-mouse
;;; Copyright (C) 1992 Sullivan Beck
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; The GNU General Public License is available by anonymouse ftp from
;;; prep.ai.mit.edu in pub/gnu/COPYING.  Alternately, you can write to
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139,
;;; USA.
;;--------------------------------------------------------------------

(defun x-mouse-global-set-key (key binding)
  "This allows you to set a default mouse binding.
When a mouse event occurs in a buffer, the buffers major-mode is checked
and if there is a mode specific binding for that mouse event for that
particular major-mode, it is executed.  Otherwise, the default binding is
executed."
  (interactive
   "sEnter the name of the mouse variable:
xEnter the binding: ")
  (x-mouse-define-key key t 'default binding))

(defun x-mouse-local-set-key (key binding &optional mode)
  "This allows you to set a mode specific mouse binding.
When a mouse event occurs in a buffer, the buffers major-mode is checked
and if there is a mode specific binding for that mouse event for that
particular major-mode, it is executed.  Otherwise, the default binding is
executed.  When this is called interactively, it can be called in one of
two ways:
      M-x x-mouse-local-set-key
  C-u M-x x-mouse-local-set-key
In both cases, the key and binding are prompted for in the minibuffer.
If no C-u prefix was entered at all, the major-mode of the current buffer
is used.  If the prefix was entered, the major-mode is prompted for in the
minibuffer.  When called non-interactively, if mode is present, it is
used.  Otherwise the current major-mode is used."
  (interactive
   "sEnter the name of the mouse variable:
xEnter the binding:
P")
  (if mode
      (if (equal mode '(4))
          (setq mode (read-minibuffer "Enter major mode: ")))
    (setq mode major-mode))
  (x-mouse-define-key key t mode binding))

(defun x-mouse-define-key (key override &rest list)
  "This allows you to bind mouse buttons to functions.
The mouse but bound is KEY which is a string which must be of the form
x-mouse-cms1-window-click.  If OVERRIDE is non-nil, any function currently
bound to that button for that major mode is replaced.  Otherwise, the new
binding will only be added if that button is not already bound for that
major mode.  LIST is of the form (MODE BINDING MODE BINDING ...) where
MODE is either 'default or a major-mode such as 'text-mode.  BINDING is
either a function or a lambda expression."
  (let* ((var (intern key))
         (val (if (boundp var)
                  (symbol-value var)
                (set var ())))
         mode binding)
    (while list
      (setq mode (nth 0 list)
            binding (nth 1 list)
            list (cdr (cdr list)))
      (if (assoc mode val)
          (if override
              (setcdr (assoc mode val) binding))
        (setq val (append val (list (cons mode binding)))))
      (set var val))))

(defun x-mouse-global-unset-key (key)
  "This sets a default mouse binding to do nothing.
The mode specific bindings are untouched."
  (interactive "sEnter the name of the mouse variable: ")
  (x-mouse-define-key key t 'default 'x--mouse-ignore))

(defun x-mouse-local-unset-key (key &optional mode)
  "This allows you to remove a mode specific mouse binding.
The binding is removed completely (not replaced by x--mouse-ignore) so
that the default mouse binding is executed when the mouse event occurs.
It can be called interactively or in a program (see x-mouse-local-set-key
for more instructions)."
  (interactive
   "sEnter the name of the mouse variable:
P")
  (if mode
      (if (equal mode '(4))
          (setq mode (read-minibuffer "Enter major mode: ")))
    (setq mode major-mode))
  (x-mouse-undefine-key key mode))

(defun x-mouse-undefine-key (key &rest list)
  "Allows you to remove mode specific bindings from a mouse button.
LIST is a list of MODEs which are to be removed."
  (let* ((var (intern key))
         (val (if (boundp var)
                  (symbol-value var)
                (set var ())))
         key)
    (while list
      (setq key (nth 0 list)
            list (cdr list))
      (if (assoc key val)
          (setq val (delq (assoc key val) val))))))


;;;************************************************************************

(defun x-mouse-get-function (var mode)
  "This returns the function stored in VAR for major-mode MODE.
VAR is a variable of the type x-mouse-c1-window-click which contains an
alist of major modes and functions.  MODE is the major mode."
  (let ((tmpfunc (symbol-value (intern var))))
    (cdr (or (assoc mode tmpfunc)
             (assoc 'default tmpfunc)))))

(defun x-mouse-get-function-doc (var mode)
  "This returns the documentation for the function stored in VAR for MODE.
VAR is a variable of the type x-mouse-c1-window-click which contains an
alist of major modes and functions.  MODE is the major mode.  Only the first
line is returned.  This works for documented functions or lambda expressions.
Undocumented functions returns the name of the function.  Undocumented lambda
expressions return 'Lambda expression'."
  (let* ((func (x-mouse-get-function var mode))
         (doc  (if (symbolp func)
                   (if (documentation func)
                       (documentation func)
                     (symbol-name func))
                 (if (stringp (nth 2 func))
                     (nth 2 func)
                   "Lambda expression")))

         (i 0)
         (done nil))
    (while (and (< i (length doc)) (< i 80) (not done))
      (if (= (aref doc i) ?\n)
          (setq done t)
        (setq i (1+ i))))
    (substring doc 0 i)))

(defun x-mouse-get-function-name (var mode)
  "This returns the name of the function or 'Lambda expression'."
  (let* ((tmpfunc (symbol-value (intern var)))
         (name (cdr (or (assoc mode tmpfunc)
                       (assoc 'default tmpfunc)))))
    (if (symbolp name) ()
      (setq name "Lambda expression"))
    name))


;;;************************************************************************

(x-mouse-define-key "x-mouse-1-window-click" nil
 'default           'x-mouse-set-point
 'gnus-Group-mode   '(lambda () (gnus-Group-prev-unread-group 1))
 'gnus-Subject-mode '(lambda () (previous-line 1)))
(x-mouse-define-key "x-mouse-c1-window-click" nil
 'default           'x-mouse-call-last-kbd-macro
 'gnus-Group-mode   'x-mouse-set-point
 'gnus-Subject-mode 'x-mouse-set-point)
(x-mouse-define-key "x-mouse-m1-window-click" nil
 'default           'x-mouse-insert-rect-000-here)
(x-mouse-define-key "x-mouse-s1-window-click" nil
 'default           'x-mouse-copy-thing)
(x-mouse-define-key "x-mouse-cm1-window-click" nil
 'default           'x-mouse-yank-here)
(x-mouse-define-key "x-mouse-cs1-window-click" nil
 'default           'x-mouse-scroll-to-top)
(x-mouse-define-key "x-mouse-ms1-window-click" nil
 'default           'x-mouse-copy-bol-to-x)
(x-mouse-define-key "x-mouse-cms1-window-click" nil
 'default           'x-mouse-scroll-to-proportion)

(x-mouse-define-key "x-mouse-2-window-click" nil
 'default           'x-mouse-paste-text
 'gnus-Group-mode   '(lambda () (gnus-Group-select-group ()))
 'gnus-Subject-mode '(lambda () (gnus-Subject-next-page ())))
(x-mouse-define-key "x-mouse-c2-window-click" nil
 'default           'x-mouse-paste-text
 'gnus-Group-mode   '(lambda () (gnus-Group-list-groups ())))
(x-mouse-define-key "x-mouse-m2-window-click" nil
 'default           'x-mouse-insert-rect-000)
(x-mouse-define-key "x-mouse-s2-window-click" nil
 'default           'x-mouse-cut-thing)
(x-mouse-define-key "x-mouse-cm2-window-click" nil
 'default           'x-mouse-yank-there)
(x-mouse-define-key "x-mouse-cs2-window-click" nil
 'default           'x-mouse-scroll-to-center)
(x-mouse-define-key "x-mouse-ms2-window-click" nil
 'default           'x-mouse-copy-line-to-x)
(x-mouse-define-key "x-mouse-cms2-window-click" nil
 'default           'x-mouse-paste-text)

(x-mouse-define-key "x-mouse-3-window-click" nil
 'default           'x-mouse-paste-there
 'gnus-Group-mode   '(lambda () (gnus-Group-next-unread-group 1))
 'gnus-Subject-mode '(lambda () (next-line 1)))
(x-mouse-define-key "x-mouse-c3-window-click" nil
 'default           'x-mouse-copy-kill-to-x
 'gnus-Group-mode   '(lambda () (gnus-Group-list-groups t)))
(x-mouse-define-key "x-mouse-m3-window-click" nil
 'default           'x-mouse-insert-rect-000-there)
(x-mouse-define-key "x-mouse-s3-window-click" nil
 'default           'x-mouse-paste-there)
(x-mouse-define-key "x-mouse-cm3-window-click" nil
 'default           'x-mouse-paste-there)
(x-mouse-define-key "x-mouse-cs3-window-click" nil
 'default           'x-mouse-scroll-to-bottom)
(x-mouse-define-key "x-mouse-ms3-window-click" nil
 'default           'x-mouse-copy-eol-to-x)
(x-mouse-define-key "x-mouse-cms3-window-click" nil
 'default           'x-mouse-execute-extended-command)

(x-mouse-define-key "x-mouse-1-mode-click" nil
 'default           'x-mouse-select)
(x-mouse-define-key "x-mouse-c1-mode-click" nil
 'default           'x-mouse-select-and-split)
(x-mouse-define-key "x-mouse-m1-mode-click" nil
 'default           'x-mouse-buffer-menu)
(x-mouse-define-key "x-mouse-s1-mode-click" nil
 'default           'x-mouse-help)
(x-mouse-define-key "x-mouse-cm1-mode-click" nil
 'default           'x-mouse-select)
(x-mouse-define-key "x-mouse-cs1-mode-click" nil
 'default           'x-mouse-select)
(x-mouse-define-key "x-mouse-ms1-mode-click" nil
 'default           'x-mouse-scroll-to-proportion)
(x-mouse-define-key "x-mouse-cms1-mode-click" nil
 'default           'x-mouse-scroll-to-proportion)

(x-mouse-define-key "x-mouse-2-mode-click" nil
 'default           'x-mouse-scroll-down)
(x-mouse-define-key "x-mouse-c2-mode-click" nil
 'default           'x-mouse-keep-one-window)
(x-mouse-define-key "x-mouse-m2-mode-click" nil
 'default           'x-mouse-scroll-down)
(x-mouse-define-key "x-mouse-s2-mode-click" nil
 'default           'x-mouse-get-file)
(x-mouse-define-key "x-mouse-cm2-mode-click" nil
 'default           'x-mouse-scroll-down)
(x-mouse-define-key "x-mouse-cs2-mode-click" nil
 'default           'x-mouse-scroll-down)
(x-mouse-define-key "x-mouse-ms2-mode-click" nil
 'default           'x-mouse-scroll-to-proportion)
(x-mouse-define-key "x-mouse-cms2-mode-click" nil
 'default           'x-mouse-scroll-to-proportion)

(x-mouse-define-key "x-mouse-3-mode-click" nil
 'default           'x-mouse-scroll-up)
(x-mouse-define-key "x-mouse-c3-mode-click" nil
 'default           'x-mouse-delete-this-window)
(x-mouse-define-key "x-mouse-m3-mode-click" nil
 'default           'x-mouse-scroll-up)
(x-mouse-define-key "x-mouse-s3-mode-click" nil
 'default           'x-mouse-scroll-up)
(x-mouse-define-key "x-mouse-cm3-mode-click" nil
 'default           'x-mouse-scroll-up)
(x-mouse-define-key "x-mouse-cs3-mode-click" nil
 'default           'x-mouse-scroll-up)
(x-mouse-define-key "x-mouse-ms3-mode-click" nil
 'default           'x-mouse-scroll-to-proportion)
(x-mouse-define-key "x-mouse-cms3-mode-click" nil
 'default           'x-mouse-scroll-to-proportion)

(x-mouse-define-key "x-mouse-1-border-click" nil
 'default           'x-mouse-select)
(x-mouse-define-key "x-mouse-c1-border-click" nil
 'default           'x-mouse-select)
(x-mouse-define-key "x-mouse-m1-border-click" nil
 'default           'x-mouse-select)
(x-mouse-define-key "x-mouse-s1-border-click" nil
 'default           'x-mouse-select)
(x-mouse-define-key "x-mouse-cm1-border-click" nil
 'default           'x-mouse-select)
(x-mouse-define-key "x-mouse-cs1-border-click" nil
 'default           'x-mouse-select)
(x-mouse-define-key "x-mouse-ms1-border-click" nil
 'default           'x-mouse-scroll-to-proportion)
(x-mouse-define-key "x-mouse-cms1-border-click" nil
 'default           'x-mouse-scroll-to-proportion)

(x-mouse-define-key "x-mouse-2-border-click" nil
 'default           'x-mouse-scroll-down)
(x-mouse-define-key "x-mouse-c2-border-click" nil
 'default           'x-mouse-scroll-down)
(x-mouse-define-key "x-mouse-m2-border-click" nil
 'default           'x-mouse-scroll-down)
(x-mouse-define-key "x-mouse-s2-border-click" nil
 'default           'x-mouse-scroll-down)
(x-mouse-define-key "x-mouse-cm2-border-click" nil
 'default           'x-mouse-scroll-down)
(x-mouse-define-key "x-mouse-cs2-border-click" nil
 'default           'x-mouse-scroll-down)
(x-mouse-define-key "x-mouse-ms2-border-click" nil
 'default           'x-mouse-scroll-to-proportion)
(x-mouse-define-key "x-mouse-cms2-border-click" nil
 'default           'x-mouse-scroll-to-proportion)

(x-mouse-define-key "x-mouse-3-border-click" nil
 'default           'x-mouse-scroll-up)
(x-mouse-define-key "x-mouse-c3-border-click" nil
 'default           'x-mouse-scroll-up)
(x-mouse-define-key "x-mouse-m3-border-click" nil
 'default           'x-mouse-scroll-up)
(x-mouse-define-key "x-mouse-s3-border-click" nil
 'default           'x-mouse-scroll-up)
(x-mouse-define-key "x-mouse-cm3-border-click" nil
 'default           'x-mouse-scroll-up)
(x-mouse-define-key "x-mouse-cs3-border-click" nil
 'default           'x-mouse-scroll-up)
(x-mouse-define-key "x-mouse-ms3-border-click" nil
 'default           'x-mouse-scroll-to-proportion)
(x-mouse-define-key "x-mouse-cms3-border-click" nil
 'default           'x-mouse-scroll-to-proportion)

(x-mouse-define-key "x-mouse-1-inter-click" nil
 'default           'x-mouse-buffer-menu)
(x-mouse-define-key "x-mouse-c1-inter-click" nil
 'default           'x-mouse-buffer-menu)
(x-mouse-define-key "x-mouse-m1-inter-click" nil
 'default           'x-mouse-buffer-menu)
(x-mouse-define-key "x-mouse-s1-inter-click" nil
 'default           'x-mouse-buffer-menu)
(x-mouse-define-key "x-mouse-cm1-inter-click" nil
 'default           'x-mouse-buffer-menu)
(x-mouse-define-key "x-mouse-cs1-inter-click" nil
 'default           'x-mouse-buffer-menu)
(x-mouse-define-key "x-mouse-ms1-inter-click" nil
 'default           'x-mouse-buffer-menu)
(x-mouse-define-key "x-mouse-cms1-inter-click" nil
 'default           'x-mouse-buffer-menu)

(x-mouse-define-key "x-mouse-2-inter-click" nil
 'default           'x-mouse-buffer-menu)
(x-mouse-define-key "x-mouse-c2-inter-click" nil
 'default           'x-mouse-buffer-menu)
(x-mouse-define-key "x-mouse-m2-inter-click" nil
 'default           'x-mouse-buffer-menu)
(x-mouse-define-key "x-mouse-s2-inter-click" nil
 'default           'x-mouse-buffer-menu)
(x-mouse-define-key "x-mouse-cm2-inter-click" nil
 'default           'x-mouse-buffer-menu)
(x-mouse-define-key "x-mouse-cs2-inter-click" nil
 'default           'x-mouse-buffer-menu)
(x-mouse-define-key "x-mouse-ms2-inter-click" nil
 'default           'x-mouse-buffer-menu)
(x-mouse-define-key "x-mouse-cms2-inter-click" nil
 'default           'x-mouse-buffer-menu)

(x-mouse-define-key "x-mouse-3-inter-click" nil
 'default           'x-mouse-buffer-menu)
(x-mouse-define-key "x-mouse-c3-inter-click" nil
 'default           'x-mouse-buffer-menu)
(x-mouse-define-key "x-mouse-m3-inter-click" nil
 'default           'x-mouse-buffer-menu)
(x-mouse-define-key "x-mouse-s3-inter-click" nil
 'default           'x-mouse-buffer-menu)
(x-mouse-define-key "x-mouse-cm3-inter-click" nil
 'default           'x-mouse-buffer-menu)
(x-mouse-define-key "x-mouse-cs3-inter-click" nil
 'default           'x-mouse-buffer-menu)
(x-mouse-define-key "x-mouse-ms3-inter-click" nil
 'default           'x-mouse-buffer-menu)
(x-mouse-define-key "x-mouse-cms3-inter-click" nil
 'default           'x-mouse-buffer-menu)

(x-mouse-define-key "x-mouse-1-mini-click" nil
 'default
 '(lambda ()
    "Repeats last complex command or goes to the previous complex command.
If point is in minibuffer goes to the previous complex command."
    (if (not (boundp 'repeat-complex-command-arg))
	(repeat-complex-command 1))
    (previous-complex-command 1)))
(x-mouse-define-key "x-mouse-c1-mini-click" nil
 'default           'x--mouse-ignore)
(x-mouse-define-key "x-mouse-m1-mini-click" nil
 'default           'x--mouse-ignore)
(x-mouse-define-key "x-mouse-s1-mini-click" nil
 'default           'x--mouse-ignore)
(x-mouse-define-key "x-mouse-cm1-mini-click" nil
 'default           'x--mouse-ignore)
(x-mouse-define-key "x-mouse-cs1-mini-click" nil
 'default           'x--mouse-ignore)
(x-mouse-define-key "x-mouse-ms1-mini-click" nil
 'default           'x--mouse-ignore)
(x-mouse-define-key "x-mouse-cms1-mini-click" nil
 'default           'x--mouse-ignore)

(x-mouse-define-key "x-mouse-2-mini-click" nil
 'default '(lambda ()
	     "Repeats last complex command or pastes X cut buffer.
If point is in minibuffer pastes X cut buffer."
	     (if (eq (window-buffer (minibuffer-window)) (current-buffer))
		 (x-mouse-paste-text)
	       (repeat-complex-command 1))))
(x-mouse-define-key "x-mouse-c2-mini-click" nil
 'default           'x--mouse-ignore)
(x-mouse-define-key "x-mouse-m2-mini-click" nil
 'default           'x--mouse-ignore)
(x-mouse-define-key "x-mouse-s2-mini-click" nil
 'default           'x--mouse-ignore)
(x-mouse-define-key "x-mouse-cm2-mini-click" nil
 'default           'x--mouse-ignore)
(x-mouse-define-key "x-mouse-cs2-mini-click" nil
 'default           'x--mouse-ignore)
(x-mouse-define-key "x-mouse-ms2-mini-click" nil
 'default           'x--mouse-ignore)
(x-mouse-define-key "x-mouse-cms2-mini-click" nil
 'default           'x--mouse-ignore)

(x-mouse-define-key "x-mouse-3-mini-click" nil
 'default '(lambda ()
	     "Repeats last complex command or goes to the next complex command.
If point is in minibuffer goes to the next complex command."
	     (if (not (boundp 'repeat-complex-command-arg))
		 (repeat-complex-command 1))
	     (next-complex-command 1)))
(x-mouse-define-key "x-mouse-c3-mini-click" nil
 'default           'x--mouse-ignore)
(x-mouse-define-key "x-mouse-m3-mini-click" nil
 'default           'x--mouse-ignore)
(x-mouse-define-key "x-mouse-s3-mini-click" nil
 'default           'x--mouse-ignore)
(x-mouse-define-key "x-mouse-cm3-mini-click" nil
 'default           'x--mouse-ignore)
(x-mouse-define-key "x-mouse-cs3-mini-click" nil
 'default           'x--mouse-ignore)
(x-mouse-define-key "x-mouse-ms3-mini-click" nil
 'default           'x--mouse-ignore)
(x-mouse-define-key "x-mouse-cms3-mini-click" nil
 'default           'x--mouse-ignore)

(x-mouse-define-key "x-mouse-1-window-drag" nil
 'default           'x-mouse-copy-text)
(x-mouse-define-key "x-mouse-c1-window-drag" nil
 'default           'x-mouse-scroll-line)
(x-mouse-define-key "x-mouse-m1-window-drag" nil
 'default           'x-mouse-copy-rect-to-000)
(x-mouse-define-key "x-mouse-s1-window-drag" nil
 'default           'x-mouse-copy-text-to-point)
(x-mouse-define-key "x-mouse-cm1-window-drag" nil
 'default           'x-mouse-copy-text)
(x-mouse-define-key "x-mouse-cs1-window-drag" nil
 'default           'x-mouse-copy-text)
(x-mouse-define-key "x-mouse-ms1-window-drag" nil
 'default           'x-mouse-copy-rect-to-x)
(x-mouse-define-key "x-mouse-cms1-window-drag" nil
 'default           'x-mouse-copy-text)

(x-mouse-define-key "x-mouse-2-window-drag" nil
 'default           'x-mouse-cut-text)
(x-mouse-define-key "x-mouse-c2-window-drag" nil
 'default           'x-mouse-cut-text)
(x-mouse-define-key "x-mouse-m2-window-drag" nil
 'default           'x-mouse-cut-rect-to-000)
(x-mouse-define-key "x-mouse-s2-window-drag" nil
 'default           'x-mouse-cut-text-to-point)
(x-mouse-define-key "x-mouse-cm2-window-drag" nil
 'default           'x-mouse-cut-text)
(x-mouse-define-key "x-mouse-cs2-window-drag" nil
 'default           'x-mouse-cut-text)
(x-mouse-define-key "x-mouse-ms2-window-drag" nil
 'default           'x-mouse-cut-rect-to-x)
(x-mouse-define-key "x-mouse-cms2-window-drag" nil
 'default           'x-mouse-cut-text)

(x-mouse-define-key "x-mouse-3-window-drag" nil
 'default           'x-mouse-append-drag)
(x-mouse-define-key "x-mouse-c3-window-drag" nil
 'default           'x-mouse-append-drag)
(x-mouse-define-key "x-mouse-m3-window-drag" nil
 'default           'x-mouse-open-rect)
(x-mouse-define-key "x-mouse-s3-window-drag" nil
 'default           'x-mouse-append-drag)
(x-mouse-define-key "x-mouse-cm3-window-drag" nil
 'default           'x-mouse-append-drag)
(x-mouse-define-key "x-mouse-cs3-window-drag" nil
 'default           'x-mouse-append-drag)
(x-mouse-define-key "x-mouse-ms3-window-drag" nil
 'default           'x-mouse-append-drag)
(x-mouse-define-key "x-mouse-cms3-window-drag" nil
 'default           'x-mouse-execute-extended-command)

(run-hooks 'x-mouse-bind-hook)
