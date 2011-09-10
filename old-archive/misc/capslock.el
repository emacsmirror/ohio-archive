;; capslock.el -- quick and dirty Emacs 19 caps-lock minor mode
;;                for lusers with DIN keyboards (Shift lock!)
;;
;; LCD Archive Entry:
;; capslock|Eberhard Mattes|mattes@azu.informatik.uni-stuttgart.de|
;; Reverse Shift state for letter keys|
;; 19-May-1993|2.0|~/misc/capslock.el.Z|
;;
;; Version 2.0  19-May-1993  Author: mattes@azu.informatik.uni-stuttgart.de
;;
;; Copyright (c) 1993 by Eberhard Mattes
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License version 1 as
;; published by the Free Software Foundation.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; This file is not part of GNU Emacs.
;;
;; Comments:
;;
;;   Caps-lock mode is disabled in the minibuffer.
;;
;;   Unfortunately, every character inserted with caps-lock-self-insert
;;   gets a separate entry on the undo stack. Fixing this seems to require
;;   changes to Emacs itself. No other problems known.
;;
;;   Works with 8-bit character sets and with 8-bit keymaps.
;;
;;   Maybe a local keymap should be used.
;;
;;   If you find a simpler solution, please tell me.
;;
;; Usage:
;;
;;   caps-lock-mode      toggle caps-lock mode
;;   caps-lock-disable   clean-up
;;
(provide 'capslock)

(defvar caps-lock-mode nil
  "*t enables caps-lock mode.
Setting it makes it local to the current buffer.")

(make-variable-buffer-local 'caps-lock-mode)

(defun caps-lock-mode (arg)
  "Toggle caps-lock mode.
With arg, turn caps-lock mode on iff arg is positive.
In caps-lock mode, characters typed in are translated to upper case."
  (interactive "P")
  (and (setq caps-lock-mode
            (if (null arg) (not caps-lock-mode)
              (> (prefix-numeric-value arg) 0)))
       (eq (lookup-key global-map "a") 'self-insert-command)
       (caps-lock-init 'self-insert-command 'caps-lock-self-insert))
  (set-buffer-modified-p (buffer-modified-p)))

(defun caps-lock-self-insert (arg)
  "Insert a character according to the setting of the caps-lock variable.
Prefix arg is repeat-count.
If caps-lock is nil, the character is inserted as-is. Otherwise, the
case of the character is changed (if possible) before inserting.
While the cursor is in the minibuffer, caps-lock mode is disabled."
  (interactive "*p")
  (and caps-lock-mode
       (not (eq (selected-window) (minibuffer-window)))
       (if (< last-command-char 0)      ; seems to be signed
           (setq last-command-char (+ last-command-char 256))
         t)
       (setq last-command-char
             (if (= (downcase last-command-char) last-command-char)
                 (upcase last-command-char)
               (downcase last-command-char))))
  (self-insert-command arg))

(or (assq 'caps-lock-mode minor-mode-alist)
    (setq minor-mode-alist
          (cons '(caps-lock-mode " Caps") minor-mode-alist)))

(defun caps-lock-disable ()
  "Turn off caps-lock mode and restore global key map."
  (interactive)
  (setq caps-lock-mode nil)
  (caps-lock-init 'caps-lock-self-insert 'self-insert-command)
  (set-buffer-modified-p (buffer-modified-p)))

(defun caps-lock-init (from to)
  (let ((key 0))
    (while (< key 256)
      (and (eq (lookup-key global-map (vector key)) from)
           (or (/= key (downcase key)) (/= key (upcase key)))
           (global-set-key (vector key) to))
      (setq key (1+ key)))))

;; end of capslock.el
