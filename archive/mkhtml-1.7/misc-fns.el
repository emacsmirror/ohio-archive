;;; misc-fns.el --- Miscellaneous non-interactive functions.
;; 
;; Author: D. ADAMS
;; Maintainer: D. ADAMS
;; Copyright (C) 1996-2001, Drew Adams, all rights reserved.
;; Created: Tue Mar  5 17:21:28 1996
;; Version: $Id: misc-fns.el,v 1.3 2001/01/03 17:41:29 dadams Exp $
;;   Last modified by: 
;;   Last modified on: Wed Jan  3 09:41:26 2001
;;   Update count: 123
;; Keywords: internal, unix, lisp, extensions, local
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;;
;;    Miscellaneous non-interactive functions.
;; 
;;  Main new functions defined here:
;;
;;    `another-buffer', `current-line', `display-in-mode-line',
;;    `do-files', `force-time-redisplay', `interesting-buffer-p',
;;    `live-buffer-name', `make-transient-mark-mode-buffer-local',
;;    `notify-user-of-mode', `special-display-buffer-p',
;;    `undefine-keys-bound-to', `undefine-killer-commands'.
;;
;;  Main new user options (variables) defined here:
;;
;;    `buffer-modifying-cmds', `mode-line-reminder-duration',
;;    `notify-user-of-mode-face', `notifying-user-of-mode'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; RCS $Log: misc-fns.el,v $
;; RCS Revision 1.3  2001/01/03 17:41:29  dadams
;; RCS *** empty log message ***
;; RCS
;; RCS Revision 1.2  2001/01/03 00:56:06  dadams
;; RCS *** empty log message ***
;; RCS
;; RCS Revision 1.1  2000/09/14 17:23:14  dadams
;; RCS Initial revision
;; RCS
; Revision 1.2  1999/03/17  15:03:35  dadams
; 1. Require (compile only): strings.
; 2. Removed require: std-faces.
; 3. Added: live-buffer-name.
; 4. interesting-buffer-p: buffer-live-p -> live-buffer-name.
; 5. notify-user-of-mode: message if display-in-minibuffer not defined;
;    protect with fboundp.
;
; Revision 1.1  1997/03/19  16:41:12  dadams
; Initial revision
;
; Revision 1.8  1996/06/25  12:51:57  dadams
; (trivial: doc string cleanup)
;
; Revision 1.7  1996/06/06  14:25:55  dadams
; 1. Require std-faces.el.
; 2. Commented out popper-show-or-pop-to-buffer.
; 3. Update of file dependency comments (e.g. "Autoloaded from...").
;
; Revision 1.6  1996/04/23  08:37:45  dadams
; Added: undefine-keys-bound-to, undefine-killer-commands,
; buffer-modifying-cmds.
;
; Revision 1.5  1996/04/04  16:59:19  dadams
; Added special-display-buffer-p.
;
; Revision 1.4  1996/03/18  17:11:34  dadams
; 1. Removed update-mode-line (coded inline in force-time-redisplay).
; 2. Simplified display-in-mode-line.
;
; Revision 1.3  1996/03/18  15:52:54  dadams
; notify-user-of-mode: message -> display-in-minibuffer.
;
; Revision 1.2  1996/03/08  13:47:56  dadams
; other-buffer-anywhere -> another-buffer. Redefined in terms of other-buffer.
;
; Revision 1.1  1996/03/06  07:38:24  dadams
; Initial revision
;
;; 
;; Previous Change Log (as `drew-util-19.el'):
;; 
; Revision 1.18  1996/02/28  16:45:55  dadams
; Moved forward-char-same-line from here to drew-misc-19.el (interactive).
;
; Revision 1.17  1996/02/14  15:59:06  dadams
; Added forward-char-same-line.
;
; Revision 1.16  1996/02/12  09:27:45  dadams
; Updated header keywords (for finder).
;
; Revision 1.15  1996/02/06  10:55:41  dadams
; Put variable-interactive property on appropriate user option vars.
;
; Revision 1.14  1996/02/02  15:05:46  dadams
; Added current-env, with-env, with-current-env.
;
; Revision 1.13  1995/11/22  15:40:38  dadams
; 1) Removed to new file drew-strings.el:
;    current-line-string, echo-in-buffer, empty-name-p, non-empty-name-p,
;    ordinal-suffix, pick-some-words, region-description,
;    symbol-around-point, symbol-named-at-point, variable-at-point,
;    word-around-point.
; 2) Removed to new file drew-windows.el:
;    1-window-frames-on, flash-ding, flash-ding-minibuffer-frame,
;    frames-on, get-a-frame, get-frame-name, multi-window-frames-on, windows-on.
; 3) Removed to drew-cal.el: diary-french-date.
; 4) Removed: chmod, save-screen/window-excursion, select-frame/screen,
;    find-file-other-screen/window, popper-show-or-display-buffer,
;    current-word (use version in simple.el), member-w-test-fn,
;    member-w-equal.
;
; Revision 1.12  1995/11/10  16:42:11  dadams
; make-transient-mark-mode-buffer-local: Added arg and set new default value
; from existing default value, not from existing (possibly local) value.
;
; Revision 1.11  1995/11/09  14:48:47  dadams
; Added make-transient-mark-mode-buffer-local.
;
; Revision 1.10  1995/10/31  12:25:11  dadams
; Some autoloads.
;
; Revision 1.9  1995/10/25  09:45:02  dadams
; force-time-redisplay: Use update-mode-line macro.
;
; Revision 1.8  1995/09/04  13:59:55  dadams
; Changed header to GNU std.
;
; Revision 1.7  1995/08/24  13:16:13  dadams
; Added macro flash-ding-minibuffer-frame.
;
; Revision 1.6  1995/08/18  06:29:06  dadams
; 1) get-a-frame-named -> get-a-frame.
; 2) get-a-frame accepts frame too as arg.
; 3) buffer-live-p: Fixed bug when get-buffer -> nil: returns nil now.
;
; Revision 1.5  1995/08/11  15:01:50  dadams
; 1) Added corrected version of diary-french-date (from cal-french.el).
; 2) Updated ordinal-suffix with newer, more correct version in
; diary.el.
;
; Revision 1.4  1995/08/10  13:43:15  dadams
; Added from show-bind.el: display-in-mode-line, force-time-redisplay,
;   mode-line-reminder-duration (was called display-bindings-sit-time).
;
; Revision 1.3  1995/08/08  12:50:03  dadams
; Added variable-at-point from help.el.
;
; Revision 1.2  1995/08/04  14:59:48  dadams
; Added: get-a-frame-named, windows-on, frames-on, 1-window-frames-on,
; multi-window-frames-on buffer-live-p.  Added mod+ from elect-mbuf.el.
;
; Revision 1.1  1995/08/02  09:20:10  dadams
; Initial revision

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

(require 'cl) ;; endp, when, unless, dolist, push

;; Get macro `define-face-const' when this is compiled,
;; or run interpreted, but not when the compiled code is loaded.
(eval-when-compile (require 'def-face-const))

 ;; (autoload 'display-in-minibuffer "strings")

(provide 'misc-fns)
(require 'misc-fns)                 ; Ensure loaded before compile.

;;;;;;;;;;;;;;;;;;;;;


;;; MODE-LINE ----------------------------------------------------------------

;; From `show-bind.el'.
;;;###autoload
(defvar mode-line-reminder-duration 10
  "*Maximum number of seconds to display a reminder in the mode-line.")
(put 'mode-line-reminder-duration 'variable-interactive
     "nMax seconds to display key reminders in mode-line: ")

;; From `show-bind.el'.
;;;###autoload
(defun display-in-mode-line (text)
  "Display TEXT in mode line for `mode-line-reminder-duration' seconds."
  (let ((mode-line-format (list text)))
    (force-time-redisplay)
    (sit-for mode-line-reminder-duration))
  (force-time-redisplay))

;;;###autoload
(defun force-time-redisplay ()
  (save-excursion (set-buffer (other-buffer)))
  (set-buffer-modified-p (buffer-modified-p))
  (sit-for 0))



;;; BUFFERS ------------------------------------------------------------------

;;;###autoload
(defun another-buffer (&optional buffer visible-ok)
  "First (most recently selected) buffer in `buffer-list'
whose name does not start with a space.
Arg BUFFER is excepted if another can be found, besides \"*scratch*\".

Differs from `other-buffer' in:
 1) If BUFFER is nil, `current-buffer' is used as the excepted BUFFER.
 2) BUFFER is used, not *scratch* buffer, if no other buffer exists
    (unless BUFFER starts with a space).  That is, BUFFER is excepted
    only if another, besides *scratch*, can be found."
  (setq buffer (or buffer (current-buffer))) ; Default.
  (let ((buf (other-buffer buffer visible-ok)))
    (if (and (eq (get-buffer-create "*scratch*") buf)
             (not (eq 0 (string-match  " " (buffer-name buffer)))))
        buffer ;; This buffer is better than *scratch*.
      buf))) ;; `other-buffer' was good enough.


;; This differs from the standard Emacs function `buffer-live-p' in that:
;; 1. The BUFFER arg may be a buffer or its name.
;; 2. This returns the buffer name, not `t', if the buffer is live.
(defun live-buffer-name (buffer)
  "Return BUFFER's name if a buffer that has not been deleted, else nil.
BUFFER may be either a buffer or its name (a string)."
  (setq buffer (and buffer (get-buffer buffer))) ; Convert to buffer, if any.
  (and buffer (buffer-name buffer)))    ; Return buffer's name.

;; Adapted from code in `help.el'.
;;;###autoload
(defun special-display-buffer-p (buffer)
  "Return BUFFER's name if it is a special display buffer, else nil.
BUFFER may be either a buffer or its name (a string)."
  (unless (stringp buffer)              ; Convert buffer to its name.
    (setq buffer (and (bufferp buffer) (buffer-name buffer))))
  (and buffer (or (member buffer special-display-buffer-names)
                  (let ((regexps special-display-regexps)
                        found)
                    (while (and regexps (not found))
                      (when (string-match (car regexps) buffer) (setq found t))
                      (pop regexps))
                    (and found buffer))))) ; Return buffer's name.

;;;###autoload
(defun interesting-buffer-p (buffer)
  "Non-nil if BUFFER is a live buffer whose name does not start with SPC."
  (and buffer (setq buffer (live-buffer-name buffer)) ; Live buffer's name.
       (or (zerop (length buffer))      ; Not an empty name.
           (not (char-equal ?\  (aref buffer 0)))))) ; Starts with non-blank.

;; Stolen from file `intes.el.2'
;;;###autoload
(defun current-line ()
  "Current line number of cursor."
  (+ (count-lines (point-min) (point))
     (if (= (current-column) 0) 1 0)))



;;; REGION -------------------------------------------------------------------

;;;###autoload
(defun make-transient-mark-mode-buffer-local (&optional default)
  "Make `transient-mark-mode' permanent-local everywhere.  Set default.
New default value is arg DEFAULT, if non-nil, else `default-value' of
`transient-mark-mode'.  This means that if already buffer-local, its
default value is not changed."
  (make-variable-buffer-local 'transient-mark-mode)
  (put 'transient-mark-mode 'permanent-local t)
  (setq-default transient-mark-mode (or default
                                        (default-value 'transient-mark-mode))))



;;; MODE ---------------------------------------------------------------------

;;;###autoload
(defvar notifying-user-of-mode t
  "*Non-nil <=> Displaying messages notifying user of mode changes.
See function `notify-user-of-mode'.")

(unless (boundp 'blue-foreground-face) (define-face-const "Blue" nil))
(defvar notify-user-of-mode-face blue-foreground-face
  "*Face used for notifying user of current major mode.
See function `notify-user-of-mode'.")

;;;###autoload
(defun notify-user-of-mode (&optional buffer anyway)
  "Display msg naming major mode of BUFFER (default: current buffer).
No msg is displayed if not `notifying-user-of-mode' or BUFFER is
internal, unless optional 2nd arg ANYWAY is non-nil.  In that case,
msg is displayed anyway."
  (setq buffer (buffer-name (and buffer (get-buffer buffer)))) ; Default curr.
  (when (and buffer
             (or (and notifying-user-of-mode ; Global var controls display.
                      (interesting-buffer-p buffer)) ; Not internal buffer.
                 anyway))               ; Override.
    (if (fboundp 'display-in-minibuffer)
	(display-in-minibuffer
         'new "Buffer `" (list notify-user-of-mode-face buffer) "' is in "
         (list notify-user-of-mode-face mode-name)
         " mode.   For info on the mode: `"
         (list notify-user-of-mode-face
               (substitute-command-keys "\\[describe-mode]")) "'.")
      (message
       "Buffer `%s' is in %s mode.   For info on the mode: `%s'."
       buffer mode-name
       (substitute-command-keys "\\[describe-mode]")))))


;;; FILES --------------------------------------------------------------------

;;;###autoload
(defun do-files (files fn &optional kill-buf-after)
  "Visit each file in list FILES, executing function FN once in each.
Optional arg KILL-BUF-AFTER non-nil means kill buffer after saving it."
  (let ((notifying-user-of-mode nil))    ; No msg on mode.
    (dolist (file files)
      (set-buffer (find-file-noselect file))
      (funcall fn)
      (setq buffer-backed-up t)           ; Do not back it up.
      (save-buffer)                       ; Just save new version.
      (when kill-buf-after
        (kill-buffer (current-buffer))))))


;;; KEYS ---------------------------------------------------------------------

(defsubst undefine-keys-bound-to (command keymap &optional oldmap)
  "Bind to `undefined' all keys currently bound to COMMAND in KEYMAP.
If optional argument OLDMAP is specified, rebinds in KEYMAP as 
`undefined' all keys that are currently bound to COMMAND in OLDMAP."
  (substitute-key-definition command 'undefined keymap oldmap))

;;;###autoload
(defvar buffer-modifying-cmds
  '(delete-char quoted-insert transpose-chars kill-region yank kill-word
                indent-new-comment-line kill-sentence fill-paragraph
                transpose-words yank-pop zap-to-char just-one-space
                indent-for-comment delete-indentation kill-sexp split-line
                transpose-sexps backward-kill-sentence)
  "*Buffer-modifying commands used in `undefine-killer-commands'.")

(defsubst undefine-killer-commands (keymap &optional oldmap)
  "Bind to `undefined' KEYMAP keys bound to buffer-modifying commands.
If optional arg OLDMAP is specified, rebinds in KEYMAP as `undefined'
the keys that are currently bound to buffer-modifying commands in
OLDMAP.  The buffer-modifying commands used: `buffer-modifying-cmds'."
  (mapcar (function (lambda (cmd) (undefine-keys-bound-to cmd keymap oldmap)))
          buffer-modifying-cmds))

;;;; ;;;###autoload
;;;; (defun name+key (cmd)
;;;;   "Returns string naming command CMD (a symbol), with its current bindings."
;;;;   (let ((keys (mapconcat 'key-description
;;;;                          (where-is-internal cmd (current-local-map))
;;;;                          ", ")))
;;;;     (format "%s%s" cmd (if keys (concat " (" keys ")") ""))))

;;;; ;; Swap two keys.  Stolen from Emacs FAQ.
;;;; ;; When Emacs receives a character, you can make Emacs behave as though it
;;;; ;; received another character by setting the value of keyboard-translate-table.
  
;;;; ;; WARNING: the value of C-g (7) is still hard coded in one place in the
;;;; ;; minibuffer code.  Thus, swapping C-g with another key may cause a minor
;;;; ;; problem.  (Fixed in Emacs 18.58.)
;;;; ;;;###autoload
;;;; (defun swap-keys (key1 key2)
;;;;   "Swap keys KEY1 and KEY2 using function map-key."
;;;;   (map-key key1 key2)
;;;;   (map-key key2 key1))
  
;;;; (defun map-key (from to)
;;;;   "Make key FROM behave as though key TO was typed instead."
;;;;   (setq keyboard-translate-table
;;;;         (concat keyboard-translate-table
;;;;                 (let* ((i (length keyboard-translate-table))
;;;;                        (j from)
;;;;                        (k i)
;;;;                        (str (make-string (max 0 (- j (1- i))) ?X)))
;;;;                   (while (<= k j)
;;;;                     (aset str (- k i) k)
;;;;                     (setq k (1+ k)))
;;;;                   str)))
;;;;   (aset keyboard-translate-table from to)
;;;;   (let ((i (1- (length keyboard-translate-table))))
;;;;     (while (and (>= i 0) (eq (aref keyboard-translate-table i) i))
;;;;       (setq i (1- i)))
;;;;     (setq keyboard-translate-table
;;;;           (if (eq i -1)
;;;;               nil
;;;;             (substring keyboard-translate-table 0 (1+ i))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; `misc-fns.el' ends here
