;;; killbuf.el --- Emacs 19--style kill-buffer for emacs 18

;;; Copyright (C) 1994 Noah S. Friedman

;;; Author: Noah Friedman <friedman@prep.ai.mit.edu>
;;; Maintainer: friedman@prep.ai.mit.edu
;;; Keywords: extensions
;;; Status: for use with Emacs 18.
;;; Created: 1994-06-21

;;; $Id: killbuf.el,v 1.1 1994/06/21 23:44:48 friedman Exp $
;;; LCD Archive Entry:
;;; killbuf|Noah Friedman|friedman@gnu.ai.mit.edu|
;;; Emacs 19--style kill-buffer for emacs 18|
;;; 21-Jun-1994|1.1|~/misc/killbuf.el.gz|

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
;;; program's maintainer or write to: The Free Software Foundation,
;;; Inc.; 675 Massachusetts Avenue; Cambridge, MA 02139, USA.

;;; Commentary:

;;; This package implements kill-buffer-hook and
;;; kill-buffer-query-functions for emacs 18.

;;; Code:

(defvar kill-buffer-hook nil
  "*Hooks run by kill-buffer just before killing the buffer.
The current buffer is set to the buffer to be killed.

This hook should not be used to query whether the buffer should actually be
killed; use kill-buffer-query-functions for that.")

(defvar kill-buffer-query-functions nil
  "*List of functions called with no args to query before killing a buffer.

If any of the functions return nil, then do not kill the buffer.")


;; Save original
(let ((orig (symbol-function 'kill-buffer)))
  (and (subrp orig)
       (fset 'kill-buffer-subr orig)))

(defun kill-buffer (buffer)
  "Kill the buffer BUFFER.
The argument may be a buffer or may be the name of a buffer.
An argument of nil means kill the current buffer.

Value is t if the buffer is actually killed, nil if user says no.

The value of `kill-buffer-hook' (which may be local to that buffer),
if not void, is a list of functions to be called, with no arguments,
before the buffer is actually killed.  The buffer to be killed is current
when the hook functions are called.

Any processes that have this buffer as the `process-buffer' are killed
with `delete-process'."
  (interactive "bKill buffer: ")
  (setq buffer
        (if buffer
            (get-buffer buffer)
          (current-buffer)))
  (save-excursion
    (set-buffer buffer)
    (let ((continue t))
      (and (interactive-p)
           (buffer-file-name)
           (buffer-modified-p)
           (setq continue
                 (yes-or-no-p
                  (format "Buffer %s modified; kill anyway? (yes or no) "
                          (buffer-name)))))
      (cond
       (continue
        (let ((queries kill-buffer-query-functions))
          (while (and continue queries)
            (setq continue (funcall (car queries))
                  queries (cdr queries))))
        (and continue
             (progn
               (run-hooks 'kill-buffer-hook)
               (let ((kill-buffer-query-functions nil)
                     (kill-buffer-hook nil))
                 ;; If the buffer was modified we've already asked about
                 ;; it; prevent kill-buffer-subr from doing so again.
                 (set-buffer-modified-p nil)
                 (kill-buffer-subr buffer)))))))))

;;; killbuf.el ends here
