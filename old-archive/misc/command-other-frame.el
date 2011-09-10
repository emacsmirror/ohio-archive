;;; command-other-frame.el -- Run commands in another frame.
;;
;; Copyright (c) 1995 CENA
;;
;; Author: Heddy Boubaker <boubaker@dgac.fr>
;; Maintainer: Heddy Boubaker <boubaker@dgac.fr>
;; Created: Wed Aug  9 15:11:43 1995
;; Last Modified: 1995/08/29 16:48:08
;; Version: 1.6
;; Keywords: command, frame
;; Tested for:
;;     XEmacs (Lucid GNU Emacs) >= 19.12
;;     Must work with FSF GNU Emacs >= 19.28 ;-)
;; Ftp access:
;;    ftp://ftp.cenatls.cena.dgac.fr/pub/export/command-other-frame.el.Z
;;    ftp://archive.cis.ohio-state.edu/pub/gnu/emacs/elisp-archive/misc/command-other-frame.el.Z
;; WWW access:
;;    http://www.cenatls.cena.dgac.fr/~boubaker
;;
;; LCD Archive Entry:
;; command-other-frame|Heddy Boubaker|boubaker@dgac.fr|
;; Run commands in another frame.|
;; 29-Aug-1995|1.6|~/misc/command-other-frame.el.gz|
;;
;; This file is NOT part of GNU Emacs but the same permissions apply.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;;  *** HOW TO READ THIS FILE ***
;;  Use (outline-mode) with (setq outline-regexp ";; @+").
;;
;; @ Purpose:
;; ==========
;;   Provide a function `command-other-frame' which create or reuse another
;;  frame and run a command in it. One advantage is that you can have one
;;  frame per command. You can `defadvise' or redefine some commands so
;;  that they will be run always in the same frame thanks to the frame
;;  reuse mechanism. Or you can use an automatic pseudo mode to do that
;;  (see WARNINGS section).
;;
;; @ Installation:
;; ===============
;;   Byte compile this file somewhere in your `load-path' and add in
;;  your .emacs:
;;  (require 'command-other-frame)
;;  or
;;  (autoload 'command-other-frame "command-other-frame"
;;   "Run command in another frame" t)
;;  (autoload 'command-other-frame-auto "command-other-frame"
;;   "Toggle automatic command-other-frame" t)
;;
;; @@ Sample usage:
;; ================
;;  then ...
;;
;;  (defadvice describe-mode (around describe-mode-o-f activate)
;;    (if (and (interactive-p)
;;             (not command-other-frame-active-p))
;;        (command-other-frame 'describe-mode)
;;      ad-do-it))
;;  ...
;;
;;  Or if you don't whant to use `defadvice' do the following:
;;  (defun my-describe-mode ()
;;    (interactive)
;;    (command-other-frame 'describe-mode))
;;  (define-key help-map "m" 'my-describe-mode)
;;  ...
;;
;;  (setq command-other-frame-alist 
;;    '((info . ("Info" nil exact))
;;      (describe-mode . ("Help" nil by-name "*Help*"))
;;      (describe-function . ("Help" nil by-name "*Help*"))
;;      (describe-variable . ("Help" nil by-name "*Help*"))))
;;
;;  This configuration will run info in it's own frame, and
;;  describe-{mode|function|variable} in always the same frame named "Help"
;;  and in one window only (the "*Help*" window).
;;
;;  There is an easiest way to automaticaly run commands in others frame...
;;  It is to setq `command-other-frame-alist' as described above with the
;;  commands you wish to be run in other frames and to add in your .emacs:
;;  ;; start command-other-frame-auto pseudo mode
;;  (command-other-frame-auto t) ;; (see WARNINGS section)
;;  You can use `command-other-frame-auto' interactively to enable/disable
;;  on the fly the auto pseudo mode. When `command-other-frame-auto' is
;;  enabled the string "[]" will be added to the modeline.
;;
;;  As `command-other-frame' is an interactive command too you can use it
;;  with M-x command-other-frame (it is actually bound on \C-x5x), it will
;;  prompt you for a command to run.
;;
;; @ Documentation: 
;; ================
;;  This section explain the internals mechanisms used and the interface to user.
;;
;; @@ Reuse mechanism:
;; ===================  
;;   When running a command with `command-other-frame' a frame is reused or a
;;  new frame is created, this explain the reuse mechanism.
;;   Fist a name for the frame is found: if you specify the NAME parameter this
;;  is the one which will be used, if it is nil we'll look in FRAME-PARAMS
;;  for the 'name parameter, if it is not found we'll look in user variable
;;  `command-other-frame-alist' for a name for this COMMAND, finally if all of
;;  these failed a name is build by concatenating the name of the current frame
;;  (selected-frame) and the COMMAND separated by a '-' (ex: "emacs-dired").
;;   Once the name had been found we try to find a frame to reuse according to
;;  the REUSE-POLICY if set, or in variable `command-other-frame-alist' for a
;;  reuse-policy for this COMMAND or to variable `command-other-frame-reuse-
;;  policy', they could contain a value among (create, exact, any, other,
;;  first, by-name or by-command) or a list of these values which are tryed
;;  turn by turn.
;;  What does these values means?, ok I'll explain:
;;  - create, means that we'll *never* try to reuse an old frame but we'll
;;  always create a new one.
;;  - exact, means that we'll try to reuse an old frame with the same name and
;;  in which we allready run the same COMMAND (with the use of `command-other-
;;  frame' only; If you create a frame manually and run the command in it
;;  command-other-frame has no way to know you did that).
;;  - any, means that any frame is reused (this should never fail because there
;;  is always at least one frame: the `selected-frame').
;;  - other, means that any frame EXCEPT the selected one is reused.
;;  - first, means that the first frame in `frame-list' is reused.
;;  - by-name, means that we'll try to found a frame with the same name.
;;  - by-command, means that the first frame in which we allready have launched
;;  the same COMMAND (throught `command-other-frame' only, see exact) is reused.
;;   One good thing to do when specifying a reuse-policy list is to order from
;;  the most restrictive to the less (ex: '(exact by-command by-name any)).
;;   If a frame has to be created the parameter FRAME-PARAMS is used as the
;;  frame-parameters of the frame, if FRAME-PARAMS is nil we look for one in
;;  `command-other-frame-alist'.
;;   One important thing to notice is that the 'name parameter is ALWAYS
;;  OVERLOADED by the name build for the frame; This is not really true see
;;  explanations for NAME parameter above to understand why...
;;   The variable `command-other-frame-create-by-default-p' indicate the
;;  behaviour of `command-other-frame' when no reusable frame had been found:
;;  if `t' it will create a new frame, else it will use the 'any reuse-policy.
;;
;; @@ the windows management:
;; ==========================
;;   Good the frame had be selected we'll run the command! ... ok! Now
;;  we're on the new/reused frame and will use the parameter ONE-WINDOW.
;;   It could have 3 kinds of values: nil, t or a string. If NIL nothing
;;  will change in windows configuration in the new frame, if TRUE after the
;;  COMMAND will be executed we'll call `delete-other-windows' so that there
;;  will be only one window in the frame, whatever it is. The string value
;;  should be the name of a buffer (allready existing after the COMMAND had
;;  been runned) in which we'll switch to and then delete the others windows
;;  so that window containing "ONE-WINDOW" will be only one window in the
;;  frame.
;;
;; @@ The `auto pseudo mode':
;; ==========================
;;   The AUTO-BEHAV parameter in `command-other-frame-alist' is used in `auto
;;  mode'. If the command is in the alist and not in the list of excluded 
;;  commands (`command-other-frame-auto-excluded') the value of AUTO-BEHAV 
;;  will be checked: If it's 'ask-me user will be interactively asked about 
;;  running this command in another frame, if it is 'not the command will 
;;  *not* be run in another frame, any other value will causes the command 
;;  to be run in another frame without asking.
;;
;; @ WARNINGS: 
;; ===========
;;  Known bugs, restrictions and problems
;;
;; @@ Disclaimer:
;; ==============
;;  THIS PACKAGE IS STILL CONSIDERED A BETA VERSION
;;
;; @@ Known bugs & restrictions:
;; =============================
;;   There are some problems with the `auto' stuff used in combination
;;  with ONE-WINDOW (if it is nil no problems).
;;   Sometimes the COMMAND doesn't seems to be run in the correct buffer
;;  and didn't found why (this could be embarrassing for, says,
;;  `describe-mode' which will describe another mode than those in the
;;  buffer). 
;;   I've got some stange errors with others commands in the same
;;  configuration which I couldn't yet expain too (a repetitive
;;  error with dired about a tag - exit - not found "no catch for tag:
;;  exit, nil", problem too with describe-{function|variable} with 
;;  completion)...
;;
;;   As a conclusion the use of this package by calling `command-other-frame'
;;  interactively or redefining and rebinding it's own commands have no
;;  major known problem, but using the `auto' stuff is more delicate (it
;;  seems to work perfectly with things as manual-entry, info, gnus ... but
;;  not with dired, describe-{mode|variable|function} at least in combination
;;  with ONE-WINDOW as described in the examples).
;;   You should try the command in some differents contexts (auto, adviced...)
;;  to find which one run better, and please report me any problems you could
;;  find so that I'll try to solve them all.
;;   I'm currently working on these problems but help from *real* Emacs
;;  Lisp wizards will be welcome, I'll be eternally grateful :)
;;
;; @ Service:
;; ==========
;; @@ Bug Reports:
;; ===============
;;   To report a bug please use function `command-other-frame-submit-bug-report'
;;   Please note that this bug-report facility uses Barry Warsaw's reporter.el
;;   which is part of GNU Emacs v19 and bundled with many other packages.
;;   If needed, you can obtain a copy of reporter.el at the elisp-archive.
;;
;; @@ ChangeLog:
;; =============
;;  v1.6: Documentation cleanup.
;;        Few performance improvements.
;;        Fixed global-mode-string bug.
;;        Fixed cof--auto bug if AUTO-BEHAV is ask-me.
;;  v1.5: Modeline-format modified if auto mode is on.
;;        Protection against use in a non windowed system.
;;        Adding AUTO-BEHAV in command-other-frame-alist.
;;  v1.4: More portability.
;;        Changes of behaviour in parameter ONE-WINDOW (with backward
;;        compatibility anyway).
;;        Adding command-other-frame-load-hook.
;;        Adding command-other-frame-auto pseudo mode.
;;        Adding command-other-frame-active-p.
;;        Minors bugs corrections.
;;  v1.3: Some more bugs corrections mainly in frame selection.
;;        Adding a new reuse policies: `other and `first.
;;  v1.2: Adding REUSE-POLICY and RECORD-FLAG in command-other-frame-alist.
;;        Adding ONE_WINDOW parameter.
;;        Some bugs corrections and performance improvements.
;;  v1.1: First public release. 
;;
;; @@ Contributors/Helpers:
;; ========================
;;  lepied@cenaath.cena.dgac.fr (Frederic Lepied [STERIA SIT] 69577103)
;;  goldman@src.honeywell.com (Robert P. Goldman)
;;  Hans Chalupsky <hans@cs.Buffalo.EDU> help for use with defadvice.
;;
;; @@ ToDo:
;; ========
;;  Correcting known bugs and others :)
;;  Improving dead-frames deletion.
;;  Improving command-other-frame-auto behaviour.
;;  Improving ONE-WINDOW parameter behaviour.
;;  Loocking for a better hook where to put cof--cleanup in FSF Emacs.
;;  Improving the way c-o-f-auto use pre-command-hook. 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

;; @ Implementation
;; ================
;; 
;; @@ Dependencies
;; ===============

(provide 'command-other-frame) 



;; @@ User variables
;; =================

;;;###autoload
(defvar command-other-frame-reuse-policy 'exact
  "*Variable to tell to `command-other-frame' how to reuse frames.

The possible values are:
- 'create     : Tell that a new frame must always be created.
- 'exact      : Tell to reuse frames for the same COMMAND
                and with the same NAME.
- 'any        : Any frame is reused.
- 'other      : Any other frame which is not the selected one.
- 'first      : The first frame found in `frame-list' is reused.
- 'by-command : Tell to reuse frames for the same COMMAND.
- 'by-name    : Tell to reuse frames with the same NAME.
The value could be a list of the above values which are tryed
turn by turn until a frame is found.

See also \\[describe-function] command-other-frame.
         \\[describe-function] frame-list.
")


;;;###autoload
(defvar command-other-frame-create-by-default-p t
  "*Non nil means create new frame when no reusable one found,
Else `command-other-frame' will reuse the selected one.
")


;;;###autoload
(defvar command-other-frame-alist nil
  "*Assoc list per COMMAND.

Each element look like (COMMAND . (NAME FRAME-PARAMS REUSE-POLICY ONE-WINDOW AUTO-BEHAV))
or (COMMAND . NAME). In fact there are the same as `command-other-frame'
parameters.

Calling `command-other-frame' with only the COMMAND parameter causes
the frame definition for the COMMAND to be searched in the
`command-other-frame-alist'.

The parameter AUTO-BEHAV is used only in `auto mode'.

See also: \\[describe-function] command-other-frame.
          \\[describe-variable] command-other-frame-reuse-policy.
          \\[describe-function] command-other-frame-auto.
")

;;;###autoload
(defvar command-other-frame-auto-excluded '(command-other-frame)
  "*List of commands excuded from automatic command-other-frame.

You could have set these commands in `command-other-frame-alist' but
not wanting them to be automaticaly run in other-frame if you're in
`command-other-frame-auto' pseudo mode.

See also: \\[describe-function] command-other-frame-auto.
          \\[describe-variable] command-other-frame-alist.
")

(defvar command-other-frame-active-p nil
  "When non nil `command-other-frame' is running now.

You should NEVER modify this variable. This is for helping
you to defadvice commands to avoid infinite recursion.
")

;;;###autoload
(defvar command-other-frame-before-run-hook nil
  "*Hook to be run just before executing the command.

You could access 3 variables from within this hook:
- frame         : Frame in wich the command will be run.
- command       : The command to be run.
- initial-frame : Frame in wich `command-other-frame' had been
                  launched.
")


;;;###autoload
(defvar command-other-frame-after-run-hook nil
  "*Hook to be run just after the command had be executed.

You could access 3 variables from within this hook:
- frame         : Frame in wich the command had been run.
- command       : The command which has been run.
- initial-frame : Frame in wich `command-other-frame' had been
                  launched.
")


;;;###autoload
(defvar command-other-frame-load-hook nil
  "*Hook to be run at command-other-frame load time.
")


(defvar command-other-frame-raise-p t
  "Non nil means the frame is raised on top of windows stack,

Else the frame is left as-is (invisible, iconified, ...).
It is REALLY BETTER to leave this variable be true.

See also \\[describe-function] command-other-frame.
")


(defconst command-other-frame-help-address "boubaker@dgac.fr"
  "E-Mail address of command-other-frame maintainer.
")

(defconst command-other-frame-version "1.6"
  "Current version of command-other-frame package
")



;; @@ Privates variables
;; =====================

;; I should think to maybe use
;; a hashtable for this.
(defvar cof--commands-alist nil
  "Variable for command-other-frame.
Alist of (COMMAND . ( list of frames )).
")

(defvar   cof--command-defs nil)
(defvar   cof--deleted 0)
(defvar   cof--intalled-p nil)
(defconst cof--cleanup-wakeup-time 8)
(defvar   cof--modeline-string nil)
(defconst cof--under-x (cond 
                        ((fboundp 'device-type)
                         (eq (device-type) 'x))
                        ((boundp window-system)
                         (eq window-system 'x))
                        (t nil))
  "Are we running under X-Windows system?.
")



;; @@ Privates functions
;; =====================

;; Portability functions of FSF GNUEmacs 19.28, thanks to Fred
(or (fboundp 'warn)
    (defun warn (str &rest args)
      "Portability function for command-other-frame pkg"
      (message str args)))

(or (fboundp 'frame-name)
    (defun frame-name (&optional f)
      "Portability function for command-other-frame pkg"
      (let ((alist (frame-parameters (or f (selected-frame)))))
	(if alist
	    (cdr-safe (assoc 'name alist))))))

(or (fboundp 'frame-iconified-p)
    (defun frame-iconified-p (f)
      "Portability function for command-other-frame pkg"
      (let ((alist (frame-parameters f)))
	(if alist
	    (eq (cdr-safe (assoc 'visibility alist)) 'icon)))))

(or (fboundp 'deiconify-frame)
    (defun deiconify-frame (f)
      "Portability function for command-other-frame pkg"
      (modify-frame-parameters f '((visibility . t)))))


(defun cof--find-frame-in-list (name flist &optional no-selected)
  ;; Find an alive frame in frame list FLIST, with
  ;; the same NAME if non nil, else take the first
  ;; alive frame in the list.
  (let ((frm nil))
    (cond 
     ((null flist)
      ;; no frame availaible
      nil)
     
     ((null name)
      ;; take the first in the list.
      (setq frm (car flist))
      (if (or (frame-live-p frm)
              (and no-selected
                   ;; we ask to choose a frame which is
                   ;; not the selected one.
                   (not (equal frm (selected-frame)))))
          frm
        (cof--find-frame-in-list name (cdr flist) no-selected)))
     
     (t
      ;; choose a frame with name == name
      (setq frm (car flist))
      (if (or (and (frame-live-p frm)
                   (equal name (frame-name frm)))
              (and no-selected
                   ;; we ask to choose a frame which is
                   ;; not the selected one.
                   (not (equal frm (selected-frame)))))
          frm
        (cof--find-frame-in-list name (cdr flist) no-selected))))))


(defun cof--find-frame (command name reuse-policy)
  ;; Try to find an existing frame for the
  ;; COMMAND according to it's NAME and it's
  ;; REUSE-POLICY. If REUSE-POLICY is a list
  ;; walk throught it until a frame is found.
  ;; Finally if no frame found return null.
  (let ((rp      (if (listp reuse-policy)
                     (car reuse-policy)
                   reuse-policy))
        (rp-next (if (listp reuse-policy)
                     (cdr reuse-policy)
                   (if command-other-frame-create-by-default-p
                       'create
                     'any))))
    (cond
     ((eq rp 'exact)
      ;; Search for a frame for the command with the same name
      (let* ((flist (cdr-safe (assq command cof--commands-alist)))
             (frm   (cof--find-frame-in-list name flist)))
        (if frm
            ;; frame found
            frm
          ;; continue reuse-policy
          (cof--find-frame command name rp-next))))
     
     ((eq rp 'by-command)
      ;; Search for a any frame for the command
      (let* ((flist (cdr-safe (assq command cof--commands-alist)))
             (frm   (cof--find-frame-in-list nil flist)))
        (if frm
            ;; frame found
            frm
          ;; continue reuse-policy
          (cof--find-frame command name rp-next))))
     
     ((eq rp 'by-name)
      ;; Search for a frame with the same name
      (let* ((flist (frame-list))
             (frm   (cof--find-frame-in-list name flist)))
        (if frm
            ;; frame found
            frm
          ;; continue reuse-policy
          (cof--find-frame command name rp-next))))

     ((eq rp 'other)
      ;; Choose any but the selected-frame.
      (let* ((flist (frame-list))
             (frm (cof--find-frame-in-list nil flist t)))
        (if frm
            ;; frame found
            frm
          ;; continue reuse-policy
          (cof--find-frame command name rp-next))))

     ((eq rp 'first)
      ;; Return the 1st frame in frame-list.
      ;; The frame-list must never be nil because
      ;; it must always contain at least the selected-frame
      (car (frame-list)))
     
     ((or (eq rp 'any)
          (and (not command-other-frame-create-by-default-p) (null rp)))
      ;; Search for any frame
      (let* ((flist (nreverse (frame-list)))
             (frm   (cof--find-frame-in-list nil flist)))
        ;; Even if no frame found there is no need to
        ;; continue reuse-policy because there is no
        ;; more frame to reuse. But this could never happen
        ;; because there is at least one frame: the current one.
        frm))
     
     ((or (eq rp 'create)
          (and command-other-frame-create-by-default-p (null rp)))
      ;; Always create a new frame
      nil)
     
     (t
      ;; What is this reuse-policy ?
      (warn "command-other-frame: Unknown reuse policy %s" rp)
      (cof--find-frame command name rp-next)))))


(defsubst cof--add-frame (command frame)
  ;; Add the FRAME for the COMMAND in
  ;; variable `cof--commands-alist'.
  ;; Add the COMMAND too if not allready in.
  (let ((ca (assq command cof--commands-alist)))
    (if (null ca)
        ;; new command to command alist
        (setq cof--commands-alist (append (cons (list command frame) '())
                                          cof--commands-alist))
      ;; add frame to command alist
      (setcdr ca (cons frame (cdr ca))))))


(defsubst cof--get-name (command name frame-params)
  ;; Get frame name from NAME parameter
  ;; or from 'name in FRAME-PARAMS
  ;; or from variable `command-other-frame-alist'
  ;; for the COMMAND. If none of the above work
  ;; the name is created by concatenating the
  ;; name of the current frame with "-COMMAND".
  (cond
   (name
    ;; we got the name directly
    name)
   ((assq 'name frame-params)
    ;; the name is within frame-parameters
    (cdr (assq 'name frame-params)))
   
   (t 
    ;; Search the name in command-other-frame-alist 
    (let ((cmd-def (or cof--command-defs
                       (setq cof--command-defs
                             (cdr-safe (assq command command-other-frame-alist)))))
          (fname   nil))
      (if cmd-def
          ;; command found in command-other-frame-alist
          (cond 
           ((stringp cmd-def)
            ;; only NAME specified
            (setq fname cmd-def))
           ((listp cmd-def)
            ;; name is the 1st element
            (setq fname (nth 0 cmd-def)))))
      (if fname
          fname
        ;; No name found anywhere build it
        (format "%s-%s" (frame-name) command))))))


(defsubst cof--get-frame-params (command name frame-params)
  ;; Get frames parameters from the
  ;; parameter FRAME-PARAMS or from the variable
  ;; `command-other-frame-alist' for the COMMAND
  ;; and overload 'name with NAME.
  (let ((fp nil))
    (if frame-params
        (setq fp frame-params)
      ;; Search the frame-params in command-other-frame-alist 
      (let ((cmd-def (or cof--command-defs
                         (setq cof--command-defs
                               (cdr-safe (assq command command-other-frame-alist))))))
            (if (listp cmd-def)
                ;; frame-params is the 2nd element
                (setq fp (nth 1 cmd-def)))))
      ;; replace/set name
      (append (list (cons 'name name)) fp)))


(defsubst cof--get-reuse-policy (command reuse-policy)
  ;; Get reuse-policy from the parameter REUSE-POLICY or
  ;; from the variable `command-other-frame-alist' for
  ;; the COMMAND or from the variable
  ;; `command-other-frame-reuse-policy'
  (if reuse-policy reuse-policy
    ;; Search the reuse-policy in command-other-frame-alist
    (let ((cmd-def (or cof--command-defs
                       (setq cof--command-defs
                             (cdr-safe (assq command command-other-frame-alist)))))
          (rp      command-other-frame-reuse-policy))
      (if (listp cmd-def)
          ;; reuse-policy is the 3rd element
          (setq rp (nth 2 cmd-def)))
      rp)))


(defsubst cof--get-one-window (command one-window)
  ;; Get one-window parameter from the parameter ONE_WINDOW or
  ;; from the variable `command-other-frame-alist' for
  ;; the COMMAND.
  (if one-window one-window
    ;; Search one-window in command-other-frame-alist
    (let ((cmd-def (or cof--command-defs
                       (setq cof--command-defs
                             (cdr-safe (assq command command-other-frame-alist))))))
      ;; one-window is the 4th element
      (nth 3 cmd-def))))


(defun cof--cleanup (&optional frame)
  "Cleanup cof--commands-alist of it's dead frames.

See Also \\[describe-function] command-other-frame.
"
  ;; This function is called from delete-frame-hook and
  ;; walk throught cof--commands-alist to delete dead
  ;; frames once every `cof--cleanup-wakeup-time' times.
  (setq cof--deleted (+ cof--deleted 1))
  (if (and cof--commands-alist
           (> cof--deleted cof--cleanup-wakeup-time))
      (progn
        (message "command-other-frame, cleaning ... ")
        ;; walking trought cof--commands-alist to find dead-frames
        (let* ((al cof--commands-alist)
               (li nil))
          (while (setq li (car-safe al))
            ;; look for dead frames for this command.
            (let ((fl       (cdr li))
                  (frm      nil)
                  (new-list nil)
                  (changes  nil))
              (while (setq frm (car-safe fl))
                (if (and (not (equal frame frm))
                         (frame-live-p frm))
                    (setq new-list (append new-list (list frm)))
                  ;; a dead-frame found 
                  (setq changes t))
                (setq fl (cdr-safe fl)))
              (if changes (setcdr li new-list)))
            (setq al (cdr-safe al))))
        ;; prepare for another trip
        (setq cof--deleted 0)
        (message "command-other-frame, cleaning ... done"))))


(defsubst cof--to-modeline (on)
  ;; Add or remove auto command-other-frame info
  ;; from modeline.
  (if (not on)
      ;; Remove
      (setq cof--modeline-string "  ")
    ;; Add
    (setq cof--modeline-string "[] ")
    (if global-mode-string
        (or (memq 'cof--modeline-string global-mode-string)
            (setq global-mode-string
                  (append '("" cof--modeline-string) global-mode-string)))
      (setq global-mode-string '("" cof--modeline-string)))))


(defun cof--auto ()
  "Catchup commands and call them with command-other-frame.

See Also \\[describe-function] command-other-frame-auto.
"
  ;; This function is called from pre-command-hook
  ;; and run commands found in command-other-frame-alist
  ;; throught command-other-frame.
  (let ((cal nil)
        (ab  nil))
    (if (and
         ;; this-command excluded
         (not (memq this-command command-other-frame-auto-excluded))
         ;; this-command not in alist
         (setq cal (assq this-command command-other-frame-alist))
         ;; Get AUTO-BEHAV from command-other-frame-alist
         ;; If eq 'not don't run in other frame
         (not (eq (setq ab  (nth 4 (cdr-safe cal))) 'not))
         ;; If eq 'ask-me ask user
         (if (eq 'ask-me ab)
             (y-or-n-p (format "Run %s in other frame? " this-command))
           t)
         ;; eq nil, t ... continue.
         )
        (let ((cmd this-command))
          (message "command-other-frame caught command: %s" cmd)
          (catch 'exit
            ;; Even with this I still have some
            ;; "no catch for tag: exit, nil" errors,
            ;; any clues??
            (condition-case var
                ;; this-command is reset in command-other-frame;
                ;; in fact in call-interactively.
                (command-other-frame cmd)
              (error
               (message "(command-other-frame %s) error %s" cmd var))))))))



;; @@ Load time functions
;; ======================

(if (boundp 'delete-frame-hook)
    (or (memq 'cof--cleanup delete-frame-hook)
        (add-hook 'delete-frame-hook 'cof--cleanup))
  ;; Use another well known hook!
  ;; This is for FSF Emacs users as far I didn't found
  ;; the equivalent of delete-frame-hook for this emacs.
  (or (memq 'cof--cleanup command-other-frame-after-run-hook)
      (add-hook 'command-other-frame-after-run-hook 'cof--cleanup)))

(if command-other-frame-load-hook
    (run-hooks 'command-other-frame-load-hook))

;; Maybe this shouldn't be here ??
(or (key-binding "\C-x5x")
    (define-key ctl-x-5-map "x" 'command-other-frame))
(or (key-binding "\C-x5a")
    (define-key ctl-x-5-map "a" 'command-other-frame-auto))



;; @@ User functions
;; =================
;;
;; @@@ command-other-frame-auto
;; ============================

;;;###autoload
(defun command-other-frame-auto (flag)
  "*Install/Remove automatic `command-other-frame'.

If FLAG non nil install `command-other-frame' auto if not already done, else
it is uninstalled.
`command-other-frame-auto' [\\[command-other-frame-auto]] catch calls of commands found in
`command-other-frame-alist' and not presents in `command-other-frame-auto-excluded'.

If present in `command-other-frame-alist' and not in `command-other-frame-auto-excluded'
we'll check the value of AUTO-BEHAV in `command-other-frame-alist' and if eq to:
- ask-me : user will be interactively asked about running command in another frame.
- not    : the command will not be run in another frame.
- any other value will run command in another frame without asking.

See Also \\[describe-function] command-other-frame.
         \\[describe-variable] command-other-frame-alist.
         \\[describe-variable] command-other-frame-auto-excluded.
"
  (interactive (list (y-or-n-p-minibuf
                      (if cof--intalled-p
                          "Removing automatic command-other-frame? "
                        "Installing automatic command-other-frame? "))))
  ;; The installation is done by adding a function to the
  ;; pre-command-hook for all buffer because pre-command-hook
  ;; should be buffer-local.
  (let ((bl (buffer-list t)))
    (cond 
     ((and cof--intalled-p
           (if (interactive-p) flag (not flag)))
      ;; Remove
      (message "Removing automatic command-other-frame ... ")
      (if (local-variable-p 'pre-command-hook (current-buffer) t)
          (progn
            ;; pre-command-hook local variable
            (and (memq 'cof--auto (default-value 'pre-command-hook))
                 ;; Remove from the default value as pre-command-hook
                 ;; is buffer-local
                 (setq-default pre-command-hook
                               (delq 'cof--auto (default-value 'pre-command-hook))))
            ;; Remove from all buffers
            (mapcar '(lambda (buff)
                       (save-excursion
                         (set-buffer buff)
                         (and (memq 'cof--auto pre-command-hook)
                              (remove-hook 'pre-command-hook 'cof--auto))))
                    bl))
        ;; pre-command-hook not local
        (and (memq 'cof--auto pre-command-hook)
             (remove-hook 'pre-command-hook 'cof--auto)))
      ;; Remove modeline infos
      (cof--to-modeline nil)
      (setq cof--intalled-p nil)
      (message "Removing automatic command-other-frame ... done"))
     
     ((and (not cof--intalled-p)
           flag)
      ;; Install
      (message "Installing automatic command-other-frame ... ")
      (if (local-variable-p 'pre-command-hook (current-buffer) t)
          (progn
            ;; pre-command-hook local variable
            (or  (memq 'cof--auto (default-value 'pre-command-hook))
                 ;; Add to the default value as pre-command-hook
                 ;; is buffer-local
                 (setq-default pre-command-hook
                               (append (default-value 'pre-command-hook) (list 'cof--auto))))
            ;; Add to all buffers
            (mapcar '(lambda (buff)
                       (save-excursion
                         (set-buffer buff)
                         (or (memq 'cof--auto pre-command-hook)
                             (add-hook 'pre-command-hook 'cof--auto))))
                    bl))
        ;; pre-command-hook not local
        (or (memq 'cof--auto pre-command-hook)
            (add-hook 'pre-command-hook 'cof--auto)))
      ;; Add modeline infos
      (cof--to-modeline t)
      (setq cof--intalled-p t)
      (message "Installing automatic command-other-frame ... done")))))


;; @@@ command-other-frame
;; =======================

;;;###autoload
(defun command-other-frame (command &optional name frame-params reuse-policy one-window record-flag)
  "*Run COMMAND in another frame [\\[command-other-frame]].

 The parameter NAME is used to find a reusable frame or to give it's
name when creating a new frame. If it is nil the variable `command-other-
frame-alist' is used to find the NAME, and finally if COMMAND is not
found in the alist a NAME is created by appending the name of the current
frame with \"-COMMAND\" \(ex: \"emacs-dired\"\).

 The parameter FRAME-PARAMS indicate the frame-parameters to use when
creating a new frame. If it is nil `command-other-frame-alist' is used
to find it. One important fact is that the 'name frame-parameter is
always overloaded by the calculated NAME (see above). 

 The parameter REUSE-POLICY control how frames are reused, if it is nil
it's value is taken from `command-other-frame-alist' for the COMMAND or
from  variable `command-other-frame-reuse-policy'.

 The parameter ONE-WINDOW control how windows are managed,if it is nil 
it's value is taken from `command-other-frame-alist' for the COMMAND.
If it is true `delete-other-windows' is called after the COMMAND had
been executed; It could be a string too: thats means that a buffer with
the same name is found (`get-buffer' is called), selected and displayed
in the only one window of the frame.

For a description of RECORD-FLAG parameter see function `call-interactively'.

See also: \\[describe-variable] command-other-frame-alist.
          \\[describe-variable] command-other-frame-reuse-policy.
          \\[describe-variable] command-other-frame-raise-p.
          \\[describe-variable] command-other-frame-before-run-hook.
          \\[describe-variable] command-other-frame-after-run-hook.
          \\[describe-variable] default-frame-alist.
          \\[describe-function] call-interactively.
"
  (interactive "CCommand: ")  
  (or (commandp command) (error "%s not a command" command))
  ;; Protection for use not under a windowed system
  (if (not cof--under-x)
      (progn
        (warn "Unadapted environment for: (command-other-frame '%s)." command)
        (call-interactively command))
    ;; reset internals 
    (setq cof--command-defs            nil
          command-other-frame-active-p t)
    (let* ((initial-frame (selected-frame))
           ;;(initial-win   (selected-window))
           (initial-buf   (current-buffer))
           ;; Get parameters from function args or
           ;; from command-other-frame-alist.
           (fname         (cof--get-name         command name  frame-params))    
           (frame-params  (cof--get-frame-params command fname frame-params)) 
           (rp            (cof--get-reuse-policy command reuse-policy)) 
           (frame         nil)) 
      ;; Try to find a frame
      (if (setq frame (cof--find-frame command fname rp)) 
          nil 
        ;; Create a new frame
        (setq frame (make-frame frame-params))     
        (if frame
            (cof--add-frame command frame)
          (warn "command-other-frame: Could not create new frame for running %s" command) 
          (setq frame initial-frame)))
      
      ;; Go the the selected frame and raise it.
      (select-frame frame) 
      (if command-other-frame-raise-p
          (progn
            (or (frame-visible-p frame)
                (make-frame-visible frame))
            (and (frame-iconified-p frame)
                 (deiconify-frame frame))
            (raise-frame frame)))
      
      ;; We're in the other frame; Run the command.
      (save-excursion
        ;; Before running the command we'll make sure
        ;; that we're in the initial buffer (from which
        ;; command-other-frame should have been called.
        ;; Why in `auto mode' this doesn't work with
        ;; says `describe-mode' ??
        (switch-to-buffer initial-buf)
        (run-hooks 'command-other-frame-before-run-hook)
        ;; this_command is set here.
        (call-interactively command record-flag)
        (run-hooks 'command-other-frame-after-run-hook)) 
      
      ;; Use only one window in the frame ?? 
      (let ((ow (cof--get-one-window command one-window))
            (win nil))
        (cond
         ((and ow (stringp ow)) 
          ;; We've got the name of the buffer where to go 
          (if (setq win (get-buffer-window ow t))
              (progn
                (select-window win)
                ;; The test below doen't seems to be
                ;; necessary but sometimes I like to
                ;; write clean programs. You didn't
                ;; notice it?? Really?? 
                (if (not (one-window-p t 1))
                    (delete-other-windows)))))
         
         ((and ow (not (one-window-p t 1)))
          ;; Only one window asked
          (delete-other-windows))))
      
      (if (not command-other-frame-raise-p) 
          ;; Back to initial-frame 
          (select-frame initial-frame)))
    
    ;; Reset internals
    (setq command-other-frame-active-p nil)))


;; @@@ command-other-frame-submit-bug-report
;; =========================================

;;;###autoload
(defun command-other-frame-submit-bug-report ()
  "*Submit via mail a bug report on command-other-frame v1.6."
  (interactive)
  (and
   (y-or-n-p "Do you REALLY want to submit a report on command-other-frame? ")
   (require 'reporter)
   (reporter-submit-bug-report
    command-other-frame-help-address
    (concat "command-other-frame " command-other-frame-version)
    (list
     ;; Interesting command-other-frame variables
     'command-other-frame-reuse-policy
     'command-other-frame-create-by-default-p
     'command-other-frame-alist
     'command-other-frame-auto-excluded
     'command-other-frame-before-run-hook
     'command-other-frame-after-run-hook
     'command-other-frame-load-hook
     'command-other-frame-raise-p
     )
    nil
    nil
    "Hi Heddy,

I want to report a bug.  I've read the `Bugs' section of `Info' on Emacs, so I
know how to make a clear and unambiguous report.  To reproduce the bug:

Start a fresh Emacs via \"`invocation-name' -no-init-file -no-site-file\".
In the `*scratch*' buffer, evaluate:")))


;; @ Local variables
;; =================
;;; Local variables:
;;; eval: (outline-minor-mode 1)
;;; outline-regexp: ";; @+"
;;; End:
;;; command-other-frame.el ends here
