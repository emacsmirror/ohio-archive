;;; keep-buffers.el -- Attempt to prevent named buffers from deletion.

;; Copyright (C) 2000  Steve Kemp

;; Author: Steve Kemp <skx@tardis.ed.ac.uk>
;; Maintainer: skx@tardis.ed.ac.uk
;; Keywords: extensions
;; Status: Tested with NT Emacs 20.[45].1
;; Created: 20/01/2000

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
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;;  This package allows you to protect buffers, based upon their
;; names, from deletion.
;;
;;  There are two ways you can use this package to protect buffers,
;; either via the customize interface, or via Lisp.
;;
;;  Via Lisp:
;;
;;  (require 'keep-buffers)                     ;; Load the package.
;;  (keep-buffers-protect-buffer "*scratch*")   ;; Protect the *scratch* buffer
;;  (keep-buffers-protect-buffer "*Messages*")  ;; Protect the *Messages* buffer
;;  ; (keep-buffers-erase-on-kill t)             ;; Erase the buffer contents on kill attempt

;;
;;  Via Customize:
;;
;;   M-x customize-group
;;   keep-buffers
;;
;;   Then simply insert the names of the buffers that you'd like protected in
;; the appropriate field.
;;   (This will only take effect if you have (require 'keep-buffers) in your .emacs file)

;;; History:
;;
;;   Version 1.0  -- Initial version.
;;   Version 1.1  -- Added the customize entries.
;;   Version 1.2  -- Added the keep-buffers-erase-on-kill function.
;;

;;; Code:
(defgroup keep-buffers nil
  "Disable deletion of certain, named, buffers."
  :tag "keep-buffers"
  :group 'convenience
  )

(defcustom keep-buffers-protected-list nil
  "Maintain a list of buffers that are to be protected, and not killed.
Buffers are added to the list with `keep-buffers-add-buffer'"
  :type '(repeat string)
  :group 'keep-buffers
  )

(defcustom keep-buffers-erase t
  "Specify whether killing a protected buffer should erase its contents."
  :type 'boolean
  :group 'keep-buffers)



(defun keep-buffers-protect-buffer (buffer)
  "Add a BUFFER name to the list of buffers that we are going to keep.
All buffers that are in this list will not be killed"
  (interactive "BBuffer to protect: ")
  (if (not (assq (quote buffer) keep-buffers-protected-list))
      (add-to-list 'keep-buffers-protected-list buffer)))


(defun keep-buffers-protect-current-buffer ()
  "Prevent the current buffer from being deleted."
  (interactive)
  (keep-buffers-protect-buffer (buffer-name)))


(defun keep-buffers-unprotect-buffer (buffer)
  "Remove a BUFFER name from the list of buffers that we are going to keep."
  (interactive "BBuffer to un-protect: ")
  (setq keep-buffers-protected-list (remove buffer keep-buffers-protected-list)))


(defun keep-buffers-unprotect-current-buffer ()
  "Prevent the current buffer from being deleted."
  (interactive)
  (keep-buffers-unprotect-buffer (buffer-name)))


(defun keep-buffers-query ()
  "The query function that disable deletion of buffers we have protected.
When an attempt to delete a buffer which is protected this function burys it."
    (if (not (find-in-list keep-buffers-protected-list (buffer-name)))
	t
      (progn
	(if keep-buffers-erase
	    (erase-buffer))
	(bury-buffer)
	nil)))


(defun find-in-list (list value)
  "Attempt to find an item in a LIST.
Return t if the item has been found, nil otherwise
Argument VALUE is the value to search the list for."
  (interactive)
  (let ((result nil))
    (while (car list)
      (if (equal  (car list) value)
	  (setq result t))
      (setq list (cdr list)))
    result))


(defun keep-buffers-erase-on-kill (value )
  "Specify whether the act of killing a buffer should erase its contents."
  (interactive)
  (if value
      (setq keep-buffers-erase t)
    (setq keep-buffers-erase nil))
  keep-buffers-erase)


;; Setup the hook
(add-hook 'kill-buffer-query-functions 'keep-buffers-query)


(provide 'keep-buffers)
;;; keep-buffers.el ends here
