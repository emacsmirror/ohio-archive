;;; switch-assoc.el --- switch to an associated buffer
;;; version 0.5

;;; LCD Archive Entry:
;;; switch-assoc|Mark A. Plaksin|mplaksin@ai.uga.edu|
;;; Quick switching to associated buffers (e.g. from a .c file to a .h file).|
;;; 5-May-1994|0.5|~/misc/switch-assoc.el.Z|

;;; Copyright (c) 1994 by Mark A. Plaksin (mplaksin@ai.uga.edu)
;;;
;;; This file is not part of GNU Emacs, but the same permissions apply.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;;; Commentary:
;;; This was written to provide a quick way to switch back and forth
;;; between myfile.c and myfile.h and the like.

;;; map-switch-to-associated-buffer switches from the current buffer
;;; to one that is "associated" with it.  If such a buffer does not
;;; exist but an associated file exists in the current directory,
;;; that file is loaded and its buffer made current.  If no associations 
;;; are found, it prompts to create an associated file.

;;; Association is defined by map-buffer-switch-alist.  Each element of
;;; map-buffer-switch-alist is a list containing a file extension and a
;;; list of associated extensions.  For example, (".h" (".c" ".cc"))
;;; associates .h files with .c and .cc files.

;;; Installation:
;;; Put this file in your load-path, and add the following to your .emacs:
;;; (autoload 'map-switch-to-associated-buffer "switch-assoc"
;;;  "Switch to an associated buffer" t)
;;;
;;; You can bind it to a key using something like this:
;;; (global-set-key "\C-ca" 'map-switch-to-associated-buffer)
;;;
;;; This binds it to C-c a
;;;

;;; History:
;;; 0.0  initial version; a hack
;;; 0.5  added map-buffer-switch-alist; a rewrite

;;; Code:

(defvar map-buffer-switch-alist
  '( (".c" (".h"))
     (".cc" (".h"))
     (".h" (".c" ".cc"))
     )
  "Associates file extensions for map-switch-to-associated-buffer
Each element of map-buffer-switch-alist is a list containing a file extension
and a list of associated extensions.  For example, (\".h\" (\".c\" \".cc\"))
associates .h files with .c and .cc files.")

(defun map-switch-to-associated-buffer ()
  "Switch to a buffer that is associated with the current buffer.
Association is defined by map-buffer-switch-alist.  If no associated buffers
exist, but an associated file exists, it is loaded and made current."
  (interactive)
  (let* ( (buffer (buffer-name (current-buffer)))
	  (last-dot (map-get-last-dot-position buffer))
	  (buffer-length (length buffer))
	  (buffer-extension (substring buffer last-dot buffer-length))
	  (buffer-pre-extension (substring buffer 0 last-dot))
	  (new-extension-list
	   (car (cdr (assoc buffer-extension map-buffer-switch-alist))))
	  (orig-extension-list new-extension-list))
    (catch 'done
      (progn
	;; do any suitable buffers exist?  if so switch to one
	(while new-extension-list
	  (if (bufferp
	       (get-buffer
		(concat buffer-pre-extension (car new-extension-list))))
	      (progn
		(switch-to-buffer
		 (concat buffer-pre-extension (car new-extension-list)))
		(throw 'done t)))
	  (setq new-extension-list (cdr new-extension-list)))
	
	;; how about suitably named files?  if so find one
	(setq new-extension-list orig-extension-list)
	(while new-extension-list
	  (if (file-readable-p
	       (concat buffer-pre-extension (car new-extension-list)))
	      (progn
		(find-file
		 (concat buffer-pre-extension (car new-extension-list)))
		(throw 'done t)))
	  (setq new-extension-list (cdr new-extension-list)))
	
	;; if we're here, we couldn't find anything
	(setq new-extension-list orig-extension-list)
	(while new-extension-list
	  (if (y-or-n-p
	       (concat "No associated buffers or files found.  Create "
		       (concat buffer-pre-extension (car new-extension-list))
		       "?  "))
	      (progn
		(find-file
		 (concat buffer-pre-extension (car new-extension-list)))
		(throw 'done t)))
	  (setq new-extension-list (cdr new-extension-list)))
	(message "No associated buffer of file found.")))))


(defun map-get-last-dot-position (filename)
  "Returns the index of the last period in FILENAME"
  (let ((last-dot 0))
    (while (string-match "[.]" filename last-dot)
      (setq last-dot (match-end 0)))
    (- last-dot 1)))
