;;; FSS.EL - Set and save/load text-properties for a file.
;;; Copyright (C) 1994 G Dinesh Dutt
;;; Version   : 1.2
;;; Author    : G. Dinesh Dutt (brat@htilbom.ernet.in)
;;; Maintainer: G. Dinesh Dutt (brat@htilbom.ernet.in)
;;; Keywords  : fonts, document-writing, retaining fonts
;;;
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
;;; A copy of the GNU General Public License can be obtained from this
;;; program's author
;;; (send electronic mail to <brat@htilbom.ernet.in>) 
;;; or from the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.
;;;

;;; LCD Archive Entry:
;;; fss|G. Dinesh Dutt|brat@htilbom.ernet.in|
;;; Minor mode to write pretty documents and retain settings between sessions|
;;; 17-Jun-1994|1.2|~/modes/fss.el.Z|

;;; Commentary :
;;;
;;; This package is to write documents within Emacs with the added
;;; ability to set arbitrary parts of the text to different
;;; fonts/colours. In addition, one can save this format and restore
;;; it between Emacs sessions. Thus, from within Emacs, one can do
;;; away with having to resorting to complex, proprietary
;;; word-processors to write good-looking documents. 
;;; Using packages like ps-print, we can print these same documents retaining
;;; all the fonts. 
;;;
;;; This is a minor-mode (I didn't know how else to get the job done).
;;; Like any other minor-mode, it can be set and unset using {M-x fss-mode}
;;; 

;;; Installation :
;;;
;;; Set up the following in your .emacs :
;;;		(require 'fss)
;;; Byte compile this file and save it someplace in your load-path.
;;; In the document where you wish to set fonts arbitrarily and see them
;;; between Emacs sessions, say :
;;;	M-x fss-mode
;;; This can also be used to turn off fss-mode. Like a normal minor mode, this
;;; is a toggle.
;;;

;;; Extending FSS :
;;;	You could extend FSS by adding more properties which you wish to toggle
;;; by binding them to keys in the FSS mode map. There is a macro,
;;; `toggle-prop' which is used to permit one to pass arguments to the lower
;;; level function `toggle-region-prop' which actually toggles the given
;;; text-property with the given value. There is also a function called
;;; `make-color' which permits one to define a new font with its foreground
;;; color the same as the font name. So, one can create a red font, yellow font
;;; etc. and use key bindings to toggle the color of regions. 

;;; Bugs :
;;;
;;; If the very first character in the region is highlighted, the whole buffer
;;; will fail. I am working on fixing this.

;;; Variables - User customisable and non.

(defconst fss-version (substring "$Revision: 1.2 $" 11 -2)
  "$Id: fss.el,v 1.2 1994/06/17 04:58:01 brat Exp brat $

 Report bugs to: G Dinesh Dutt <brat@htilbom.ernet.in>")

(defvar fss-property-save-type 'public
  "*This variable controls the loading and saving of the property lists.
The possible values for this variable are :
private - the property list is saved in the file \"~/.emacs-props\".
public  - the property list is saved in the file \".emacs-props\" under the
          same directory as the text file.
local   - this will save the property-list within the file as local var.

*I have not yet implemented local facility.*")

(defvar fss-mode-hooks nil
  "*List of hook functions to be executed on loading `fss-mode'.")

(defvar fss-private-filename "~/.emacs-props"
  "*This is the file into which the locally visible property values are stored.")

(defvar fss-load-active nil
  "To prevent recursive calling.")

(defvar fss-mode nil
  "Variable to toggle fss-prop minor mode.")

;;; Code commences

(defun fss-save-props ()
  "This saves the property-list for the entire buffer in the appropriate file."
  (interactive)
  (let ((prop-list)
	 (spos (point-min))
	 (epos (point-min))
	 (localfile (buffer-file-name))
	 (localdir (file-name-directory (buffer-file-name)))
	 (local-marker)
	 (prop-file-buffer))
     (save-excursion
       ;
       ; Figure out which file the property information needs to be saved in.
       ;
       (cond ((or (equal fss-property-save-type 'private)
		  (not (file-writable-p (concat localdir "/.emacs-props"))))
	      (setq prop-file-buffer (find-file-noselect
				      fss-private-filename)))
	     ((and (equal fss-property-save-type 'public)
		   (file-writable-p (concat localdir "/.emacs-props")))
		   (setq prop-file-buffer (find-file-noselect
					   (concat localdir ".emacs-props")))))
       (set-buffer prop-file-buffer)
       (goto-char (point-min))
       ;
       ; Does an entry for this file already exist ? If so, clear it
       ; and rewrite it with the new list.
       ;
       (if (re-search-forward (concat "^(File: " localfile) (point-max) t)
	   (progn
	     (re-search-forward "^(fss-mode 1)" (point-max) t)
	     (setq local-marker (point-marker))
	     (if (re-search-forward "^(File: " (point-max) t)
		 (progn 
		   (forward-line -1)
		   (end-of-line)
		   (backward-char 2))
	       (progn
		 (goto-char (point-max))
		 (backward-char 2)))
	     (delete-region local-marker (point)))
	 (progn
	   (goto-char (point-max))
	   (insert (concat "\n(File: " localfile))
	   (insert "\n(progn\n(fss-mode 1)\n))")
	   (backward-char 2))))
     ;
     ; We have now setup the buffer with the entry for the file all ready
     ; to be used. Now generate the information. Entries for a file are of
     ; the form :
     ; (File: <absolute pathname of the file>
     ; (progn
     ; (fss-mode 1)
     ; (add-text-properties <start-region> <end-region> <prop-list>)
     ; (add-text-properties ..... )))
     ;
     (catch 'foo
       (while 
	   (progn
	     (setq spos (next-property-change epos))
	     spos)
	 (setq prop-list (text-properties-at spos))
	 (setq epos (or (next-property-change spos)
			(point-max)))
	 (if epos
	     (princ (concat "\n(add-text-properties " spos " " epos " '")
		    prop-file-buffer))
	 (princ prop-list prop-file-buffer)
	 (princ " )" prop-file-buffer)
	 (if (= (point-max) epos)
	     (throw 'foo nil))))
     (save-excursion
       (set-buffer prop-file-buffer)
       (save-buffer))))

(defun fss-load-props ()
  "This loads the text properties for the buffer from the required files."
  (interactive)
  (let ((prop-list)
	(mystring)
	(spos)
	(epos)
	(localfile (buffer-file-name))
	(prop-file-buffer)
	(prop-filename)
	(modified (buffer-modified-p))
	(cwd))
    (catch 'foo
      (save-excursion
	(if (buffer-file-name)
	    (setq cwd (file-name-directory (buffer-file-name)))
	  (setq cwd (default-directory)))
	(if (equal fss-property-save-type 'public)
	    (setq prop-filename (concat cwd ".emacs-props"))
	  (setq prop-filename fss-private-filename))
	(if (and (file-exists-p prop-filename) (file-readable-p prop-filename))
	    (setq prop-file-buffer (find-file-noselect prop-filename))
	  (throw 'foo nil))
	;;	(progn
	;;	  (if (and (file-exists-p fss-private-filename)
	;;		   (file-readable-p fss-private-filename))
	;;	      (setq prop-file-buffer (find-file-noselect fss-private-filename)))))
	(set-buffer prop-file-buffer)
	(goto-char (point-min))
	(if (re-search-forward (concat "File: " localfile) (point-max) t)
	    (progn
	      (end-of-line)
	      (forward-char)
	      (setq spos (point))
	      (setq epos (or (re-search-forward "^(File: " (point-max) t)
			     (- (point-max) 1)))
	      (setq mystring (buffer-substring spos epos)))))
      (if mystring
	  (eval (read mystring)))
      (set-buffer-modified-p modified))))

(defun fss-mode (&optional arg)
  "Toggle fss (font set and save) mode.
This is a minor mode which permits you to set various text properties 
for various regions of the buffer and retain them between Emacs sessions.
I designed it basically to produce nicer documents using ASCII.
Loading this mode runs fss-mode-hooks as the last action of loading this
mode."
  (interactive "P")
  (let ((on-p (if (null arg)
		  (not fss-mode)
		(> (prefix-numeric-value arg) 0))))
    (cond (on-p
	   (set (make-local-variable 'fss-mode) on-p)
;;	   (add-hook 'find-file-hooks 'fss-load-props)
	   (add-hook 'local-write-file-hooks 'fss-save-props)
	   (run-hooks 'fss-mode-hooks))
	  ((not on-p)
	   (set 'fss-mode on-p)
;;	   (remove-hook 'find-file-hooks 'fss-load-props)
	   (remove-hook 'local-write-file-hooks 'fss-save-props)))))

;;;
;;; Sample code for writing pretty documents (with bold, italic,
;;; underline etc. fonts). This probably belongs elsewhere and must be
;;; stored as a separate file and .el.
;;;

;; The following function is used to toggle the property of the region
;; It works by using the next-property-change function to keep detecting 
;; changes in the given property for the entire region and for each
;; section found, it toggles the property. 
;; This function might be a little more complex than necessary. Hence, a
;; simpler version of the code is retained. Only usage will determine which 
;; is preferable.
;;
;;
;;(defun toggle-region-prop (spos epos prop value)
;;  "This function toggles the property of the region."
;;  (let ((mpos spos))
;;    (progn
;;      (setq inhibit-read-only 't)
;;    (while (and spos (< spos epos))
;;      (setq mpos (next-single-property-change spos prop))
;;      (if (and mpos (> mpos epos))
;;	  (if (equal (get-text-property spos prop) value)
;;	      (remove-text-properties spos epos (list prop value))
;;	    (add-text-properties spos epos (list prop value)))
;;	(if (equal (get-text-property spos prop) value)
;;	    (remove-text-properties spos mpos (list prop value))
;;	  (add-text-properties spos mpos (list prop value))))
;;      (setq spos mpos))
;;    (setq inhibit-read-only nil))))

(defun toggle-region-prop (prop value)
  "This function toggles the property of the region."
  (let ((spos (mark))
	(epos (point)))
  (setq inhibit-read-only t)
  (if (equal (get-text-property spos prop) value)
      (remove-text-properties spos epos (list prop value))
    (add-text-properties spos epos (list prop value)))
  (setq inhibit-read-only nil)))

(defun make-color (color)
  "Make a new font with the color specified.
Usage is `(make-color 'COLOR-NAME)'. For eg., (make-color 'red)."
  (interactive "SColor: ")
  (let (newface)
    (setq newface (make-face color))
    (set-face-foreground newface (symbol-name color))))

(defmacro toggle-prop (prop value)
  "Macro to enable quick keybinding to toggle `value' of `prop'."
  (` (function (lambda ()
		      (interactive)
		      (toggle-region-prop (, prop) (, value))))))

(defun plain-region (spos epos)
  "Revert to default font for the region specified."
  (interactive "r")
  (add-text-properties spos epos (list 'face 'default)))

(setq fss-mode-map (make-sparse-keymap))
(define-key fss-mode-map "\C-c\C-b" (toggle-prop 'face 'bold))
(define-key fss-mode-map "\C-c\C-u" (toggle-prop 'face 'underline))
(define-key fss-mode-map "\C-c\C-i" (toggle-prop 'face 'italic))
(define-key fss-mode-map "\C-c\C-v" (toggle-prop 'face 'bold-italic))
(define-key fss-mode-map "\C-c\C-@" 'plain-region)
(define-key fss-mode-map "\C-x\C-r" (toggle-prop 'read-only 't))

(or (assq 'fss-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(fss-mode " FSS") minor-mode-alist)))

(setq minor-mode-map-alist (cons (cons 'fss-mode fss-mode-map)
				 minor-mode-map-alist))

(add-hook 'find-file-hooks 'fss-load-props t)

(provide 'fss)

;;; end of fss.el
