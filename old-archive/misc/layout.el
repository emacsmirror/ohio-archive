;;;; layout.el - functions to switch between named window configurations
;;; Copyright (C) 1992 Lennart Staflin
;;; Last edited: Sun May 21 18:50:23 1995 by lenst@godot.lysator.liu.se (Lennart Staflin)
;;; $Id: layout.el,v 1.5 1995/05/21 16:51:35 lenst Exp $
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
;;; A copy of the GNU General Public License can be obtained from this
;;; program's author (send electronic mail to les@ida.liu.se) or from
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.
;;;
;;; Send bug reports to lenst@lysator.liu.se

;; LCD Archive Entry:
;; layout|Lennart Staflin|lenst@lysator.liu.se|
;; Switching between named window configurations|
;; 21-May-1995|1.5|~/misc/layout.el.gz|

;; This package allows you to have a named window configuration for
;; every task you are currently working on in Emacs.  And to switch
;; between these configurations easily.  I call the named window
;; configurations for layouts, as they potentially contain more
;; information than the Emacs Lisp window configuration.  

;; For example, suppose that you are reading news with Gnus, then you
;; can have a layout named "gnus"; and you are hacking on some lisp
;; code in a layout named "hack".

;; To switch to the "gnus" layout type: `C-x C-j g n u s RET'.

;; Now the current layout is "gnus", and this is recorded in the
;; variable `current-layout-name'.  If you switch to another layout,
;; the current window configuration together with the name in
;; `current-layout-name' will be stored in a table (`layout-table').
;; Then the new layout will be fetched from the table and installed as
;; current.

;; When you use `C-x C-j' to switch to a layout, the layout does not
;; have to exist.  If it doesn't exist, a new layout is created and
;; the `*scratch*' buffer is seleced.  The newly created layout is not
;; yet in the tabel of layouts, but will be stored when you switch to
;; another layout.

;; If you type `C-x C-j RET', Emacs will switch to a newly created
;; layout without any name (`current-layout-name' is nil).  An unnamed
;; layout will never be saved in the tabel of layouts.

;; You can change the name of the current layout with `M-x layout-define'.
;; There may still be a layout with the previous name in the tabel.
;; To delete a layout use `M-x delete-layout'.

;; You can also switch to the previous layout (unless it was unnamed) with
;; `C-u C-x C-j' or `M-x switch-layout'.


;; Save this file as "layout.el" in a Lisp directory that Emacs knows about
;; (i.e. via load-path).
;;
;; Suggested setup in your .emacs file:
;;
;; (global-set-key "\C-x\C-j" 'layout-select)
;; (autoload 'layout-select "layout" 
;;	  "Select the window layout NAME from saved layouts." t)
;; (autoload 'layout-define "layout"
;;	  "Give the current window layout a name." t)
;; (autoload 'switch-layout "layout"
;;	  "Select previous layout." t)


(provide 'layout)


;;;; The layout data type

;;; This is a tupel <name, window configuration, buffer>
;;;   buffer is the buffer that is second in the buffer list,
;;;   this allows restoring the first two elements of the buffer list.

;;; This type is pal with the layout table type, and has promised to
;;; be a cons-cell with car a string that is the name of the layout.
;;; (This allows the use of assoc on layout table).  It has not
;;; promised anything about the cdr part, but currently it is a cons.

(defun make-layout (wconf &optional name other-buffer)
  (cons name (cons wconf other-buffer)))

(defun layout-wconf (layout)
  (car (cdr layout)))

(defun layout-name (layout)
  (car layout))

(defun layout-other-buffer (layout)
  (cdr (cdr layout)))

(defun layout-replace (old-layout new-layout)
  (setcar old-layout (car new-layout))
  (setcdr old-layout (cdr new-layout)))


;;;; Datastructures for current layout

(defvar current-layout-name nil
  "Name of the window layout currently active.
This will be stored with the layout when it is pushed, to allow
retrieval by name.")

(defun get-layout-name ()
  ;; Return the name of the current layout.
  current-layout-name)

(defun set-layout-name (name)
  ;; Give the current window layout a name.  This will ensure that this
  ;; layout is saved if another layout is selected with layout-select.
  (setq current-layout-name name))

(defun get-current-layout ()
  (make-layout (current-window-configuration)
	       (get-layout-name)
	       (other-buffer)))



;;;; The layout table data structure and type

;;; This is a table of layouts.  A mapping from name (a string) to
;;; layout.  As this type is pal with the layout type, it knows that
;;; layout is a cons with the name as car.

(defvar layout-table (list nil)		; Non-empty for completion routines
					; to work.
  "Table of named layouts")

(defun layout-exists (name)
  (assoc name layout-table))

(defun get-layout (name)
  (or (stringp name)			; Just checking
      (error "Arguments to get-layout wrong: %s" name))
  (assoc name layout-table))

(defun store-layout (layout)
  (if (layout-exists (layout-name layout))
      (layout-replace (get-layout (layout-name layout))
		      layout)
    (setq layout-table (cons layout layout-table))))

(defun delete-layout (name)
  "Delete the layout NAME from the table of layouts."
  (interactive
   (list (completing-read
	  (format "Window layout to delete: " (get-layout-name))
	  layout-table			; Completion table
	  nil				; predicate
	  t				; require match
	  )))
  (setq layout-table
	(delq (get-layout name) layout-table)))


;;;; Entering and leaving layouts

(defvar layout-auto-name-number 0)

(defun layout-checkpoint (&optional auto-name)
  "Update the table of layouts with the current layout."
  (interactive)
  (and auto-name
       (null (get-layout-name))
       (set-layout-name
	(format "T%d" (setq layout-auto-name-number
			    (1+ layout-auto-name-number)))))
  (and (get-layout-name)
       (progn
	 ;;(setq previous-layout-name (get-layout-name))     ***
	 (store-layout (get-current-layout)))))

(defun layout-revert ()
  "Restore current layout from table."
  (interactive)
  (let ((l (get-layout (get-layout-name))))
    (if (null l)
	(error "Reverting layout %s that is not in the table"
	       (get-layout-name)))
    (set-window-configuration (layout-wconf l))
    ;;(set-layout-name (layout-name l))
    (switch-to-buffer 
     (prog1 (current-buffer)
       (and (buffer-name (layout-other-buffer l)) ; Check if buffer exists.
	    (switch-to-buffer (layout-other-buffer l)))))
    (message "Entering layout %s" (get-layout-name))))

;;;; User interface to layout table

(defvar previous-layout-name nil)

;;;###autoload
(defun layout-define (name)
  "Give the current window layout a name.
This will save the current window layout in the layout table.  It will
also ensure that this layout is saved if another layout is selected
with layout-select.  If called interactively with no name,
layou-define will keep the old name but still save the current layout
in the table, this can be used to checkpoint the layout."
  (interactive
   (list (read-string (format "Name (%s): " (get-layout-name)))))
  (if (not (equal name ""))
      (set-layout-name name))
  (layout-checkpoint))

;;;###autoload
(defun layout-select (name)
  "Select the window layout NAME from saved layouts.
If NAME is a string, the layout is looked up in the table of saved
layouts, and if not found, an empty layout with name NAME is created.
If NAME is nil a new unnamed layout is selected, if called
interactively and a prefix argument is given, the previously selected
layout is selected."
  (interactive
   (list (if current-prefix-arg
	     previous-layout-name
	   (completing-read
	    (format "Window layout %s -> " (get-layout-name))
	    layout-table))))
  (if (equal name "")
      (setq name nil))
  (layout-checkpoint (null name))
  (setq previous-layout-name (get-layout-name))
  (set-layout-name name)
  (switch-to-buffer "*scratch*")
  (delete-other-windows)
  (if (layout-exists name)
      (layout-revert)
    (layout-checkpoint)))

;;;###autoload
(defun switch-layout ()
  "Select previous layout.
Same as select-layout with prefix argument."
  (interactive)
  (layout-select previous-layout-name))

;;; layout.el ends here
