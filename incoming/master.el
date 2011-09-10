;;; master.el --- make a buffer the master over another buffer

;; Copyright (C) 1999  Alexander Schroeder

;; Author: Alex Schroeder <a.schroeder@bsiag.ch>
;; Maintainer: Alex Schroeder <a.schroeder@bsiag.ch>
;; Version: 1.0.1
;; Keywords: comm

;; CVS stuff:

;; $Author: alex $
;; $Date: 2000/02/06 18:11:32 $
;; $Revision: 1.1 $

;; This file is not part of GNU Emacs.

;; master.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2, or (at your option) any
;; later version.

;; master.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; master-mode is a minor mode which enables you to scroll another
;; buffer (the slave) without leaving your current buffer (the master).
;; It is used by sql.el, for example: The SQL buffer is the master and
;; the SQLi buffer commands are sent to is the slave.  This allows you
;; to scroll the SQLi buffer containing the output of the commands you
;; sent it from the SQL buffer.

;; The way to add this to SQL mode, for example, is indicated below.
;; The variable sql-buffer contains the buffer that is supposed to be
;; the slave of the SQL buffer.  sql-mode provides a hook that is run
;; whenever sql-buffer is changed through a specific function.

;; (autoload 'master-mode "master" "Master mode minor mode." t)
;; (add-hook 'sql-mode-hook
;;	   (function (lambda ()
;;		       (master-mode t)
;;		       (master-set-slave sql-buffer))))
;; (add-hook 'sql-set-sqli-hook
;;	   (function (lambda ()
;;		       (master-set-slave sql-buffer))))

;;; Thanks to all the people who helped me out:

;; Rob Riepel <networking.stanford.edu>



;;; Code:

(require 'easy-mmode)

;; Variables that don't need initialization.
  
(defvar master-of nil
  "Slave buffer of the current buffer.  See `master-mode'.

You can set this variable using `master-set-slave'.")

(defvar master-set-slave-hook nil
  "Hook run after the slave is changed using \\[master-set-slave].")

;;; Define master mode.

(easy-mmode-define-minor-mode master-mode
  "Toggle Master mode.

With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When Master mode is enabled, you can scroll the slave buffer using the
following commands:

\\{master-mode-map}

The slave buffer is stored in the buffer-local variable `master-of'.
You can set this variable using `master-set-slave'.  You can show
yourself the value of `master-of' by calling `master-show-slave'."
 ;; The initial value.
 nil
 ;; The indicator for the mode line.
 nil
 ;; The minor mode bindings.
 '(("\C-c\C-n" . master-says-scroll-up)
   ("\C-c\C-p" . master-says-scroll-down)
   ("\C-c<" . master-says-beginning-of-buffer)
   ("\C-c>" . master-says-end-of-buffer)
   ("\C-c\C-l" . master-says-recenter)))

;; Initialize Master mode by setting a slave buffer.

(defun master-set-slave (buffer)
  "Makes BUFFER the slave of the current buffer.

Use \\[master-mode] to toggle control of the slave buffer."
  (interactive "bSlave: ")
  (make-local-variable 'master-of)
  (setq master-of buffer)
  (run-hooks 'master-set-slave-hook))

(defun master-show-slave ()
  "Displays a message with the name of the slave buffer."
  (interactive)
  (message "This buffer is the master of %s.  Master-mode is %S." 
	   (or master-of "none")
	   master-mode))



;;; Functions that the master buffer can call in the slave buffer.

(defun master-says-scroll-up ()
  "Display and scroll the slave buffer up.  See `scroll-up'."
  (interactive)
  (master-says 'scroll-up))

(defun master-says-scroll-down ()
  "Display and scroll the slave buffer down.  See `scroll-down'."
  (interactive)
  (master-says 'scroll-down))

(defun master-says-beginning-of-buffer ()
  "Display and move to the beginning of the slave buffer.

See `beginning-of-buffer'."
  (interactive)
  (master-says 'beginning-of-buffer))

(defun master-says-end-of-buffer ()
  "Display and move to the end of the slave buffer.

See `end-of-buffer'."
  (interactive)
  (master-says 'end-of-buffer))

(defun master-says-recenter ()
  "Recenter the slave buffer.  See `recenter'."
  (interactive)
  (master-says 'recenter))

;; The master function doing the stuff.

(defun master-says (&optional command arg)
  "Display slave buffer and execute COMMAND with ARG in its window."
  (interactive)
  (if (null (buffer-live-p (get-buffer master-of)))
      (error "Slave buffer has disappeared")
    (let ((window  (selected-window)))
      (if (not (eq (window-buffer window) (get-buffer master-of)))
	  (switch-to-buffer-other-window master-of))
      (if command (condition-case nil (apply command arg) (error nil)))
      (select-window window))))

(provide 'master)

;;; master.el ends here
