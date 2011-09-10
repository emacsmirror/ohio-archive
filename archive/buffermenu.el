;; buffermenu.el: modifies the xemacs buffer menu to organize files by directory
;;
;; This file does three things:
;; 
;; 1. Modifies the xemacs buffer menu so that files are sorted
;; according to their directory. I personally find this useful for working
;; on large software projects where I might have 6 different Makefiles
;; open at the same time, for instance. This allows me to quickly go to the
;; correct one.
;;
;; 2. Adds a new menu called mouse-bufferkill-menu. It looks exactly
;; like the buffer menu, except that selecting a buffer immediatly kills it.
;; I find this useful for quickly killing a set of buffers.
;;
;; 3. Binds the buffer menu to (control button1) and the bufferkill menu
;; to (control button2)
;;
;; This code only works for xemacs, and works with version 19.14 and later.
;; 
;; Please send comments, criticisms, and suggestions to:
;;
;;        Brady Montz (bradym@balestra.org)

;; Copyright (C) 1997 Brady Montz
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA

;; Emacs Lisp Archive Entry
;; Filename: buffermenu.el
;; Version: 1.0
;; Author: Brady Montz <bradym@balestra.org>
;; Maintainer: Brady Montz <bradym@balestra.org>
;; Description: modifies the xemacs buffer menu to organize files by directory
;; Keywords: xemacs, buffer, menu
;; Compatibility: XEmacs
;; Incompatibility: Emacs

;; $Id: buffermenu.el,v 1.6 2000/08/16 22:43:29 bradym Exp $


; Mark modified and read-only buffers in menu

(setq buffers-menu-format-buffer-line-function 'my-buffers-menu-format)

; Make seperate sub menu for each directory
; Default is to just draw a line between groups
(setq buffers-menu-submenus-for-groups-p t)

; Sort and group based on directory file is in, and put buffers with
; no file and buffers starting with a "*" in Misc category

(setq buffers-menu-sort-function 'sort-buffers-menu-by-dir-then-alphabetically)
(setq buffers-menu-grouping-function 'group-buffers-menu-by-dir)

(defun mouse-buffer-menu (event)
  (interactive "e")
  (popup-buffer-menu event))

(defun mouse-bufferkill-menu (event)
  (interactive "e")
  (let ((window (and (event-over-text-area-p event)
		     (event-window event)))
	(bmenu nil)
	(buffers-menu-switch-to-buffer-function 'kill-buffer))
    (or window
	(error "Pointer must be in a normal window"))
    (select-window window)
    (if current-menubar
	(setq bmenu (assoc "Buffers" current-menubar)))
    (if (null bmenu)
	(setq bmenu (assoc "Buffers" default-menubar)))
    (if (null bmenu)
	(error "Can't find the Buffers menu"))
    (popup-menu (cons "Kill Buffer" (cdr bmenu)))))

; This displays buffer read-only and modified status in buffer-menu
; The default behavior is to just show buffer name
(defun my-buffers-menu-format (buffer)
  (format "%s%s %s"
	  (if (symbol-value-in-buffer 'buffer-read-only buffer) "%" " ")
	  (if (buffer-modified-p buffer) "*" " ")
	  (buffer-name buffer)))

(defun sort-buffers-menu-by-dir-then-alphabetically (buf1 buf2)
  "For use as a value of `buffers-menu-sort-function'. 
It sorts buffers first by directory, and then alphabetically.
The group containing buffers beginning with a star is listed last.
It only makes sense if `buffers-menu-grouping-function' is 
`group-buffers-menu-by-dir'."
  (let* ((nam1 (buffer-name buf1))
	 (nam2 (buffer-name buf2))
	 (path1 (buffer-file-name buf1))
	 (path2 (buffer-file-name buf2))
	 (misc1-p (or (not path1) 
		      (not (null (string-match "\\`*" nam1)))))
	 (misc2-p (or (not path2) 
		      (not (null (string-match "\\`*" nam2)))))
	 (group1 (if misc1-p "*Misc*" (file-name-directory path1)))
	 (group2 (if misc2-p "*Misc*" (file-name-directory path2))))
    (cond ((and misc1-p misc2-p) (string-lessp nam1 nam2))
	  ((not (equal misc1-p misc2-p)) (not misc1-p))
	  ((string-equal group1 group2) (string-lessp nam1 nam2))
	  (t (string-lessp group1 group2)))))

(defun sort-buffers-menu-by-dir (buf1 buf2)
  "For use as a value of `buffers-menu-sort-function'. 
It sorts buffers first by directory. The group containing buffers beginning
with a star is listed last. It only makes sense if 
`buffers-menu-grouping-function' is `group-buffers-menu-by-dir'."
  (let* ((nam1 (buffer-name buf1))
	 (nam2 (buffer-name buf2))
	 (path1 (buffer-file-name buf1))
	 (path2 (buffer-file-name buf2))
	 (misc1-p (or (not path1) 
		      (not (null (string-match "\\`*" nam1)))))
	 (misc2-p (or (not path2) 
		      (not (null (string-match "\\`*" nam2)))))
	 (group1 (if misc1-p "*Misc*" (file-name-directory path1)))
	 (group2 (if misc2-p "*Misc*" (file-name-directory path2))))
    (cond ((not (equal misc1-p misc2-p)) (not misc1-p))
	  (t (string-lessp group1 group2)))))

(defun group-buffers-menu-by-dir (buf1 buf2)
  "For use as a value of `buffers-menu-grouping-function'. It groups
buffers by directory. It only makes sense if `buffers-menu-sorting-function' is
`sort-buffers-menu-by-dir-then-alphabetically' or 
`sort-buffers-menu-by-dir'"
  (let* ((nam1 (buffer-name buf1))
	 (nam2 (buffer-name buf2))
	 (path1 (buffer-file-name buf1))
	 (path2 (buffer-file-name buf2))
	 (misc1-p (or (not path1) 
		      (not (null (string-match "\\`*" nam1)))))
	 (misc2-p (or (not path2) 
		      (not (null (string-match "\\`*" nam2)))))
	 (group1 (if misc1-p "*Misc*" (file-name-directory path1)))
	 (group2 (if misc2-p "*Misc*" (file-name-directory path2))))
    (cond ((null buf2) group1)
	  ((string= group1 group2) nil)
	  (t group1))))

(define-key global-map [(control button1)] 'mouse-buffer-menu)
(define-key global-map [(control button2)] 'mouse-bufferkill-menu)

(provide 'buffermenu)

