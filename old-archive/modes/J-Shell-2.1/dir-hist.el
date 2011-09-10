;; -*-Emacs-Lisp-*-
;; $Id: dir-hist.el,v 1.1 1994/02/23 21:42:39 jimt Exp $

;; Jim's Directory-History Mode for GNU Emacs (dir-hist)
;; Copyright (C) 1993 James C. Thompson, jimt@sugar.neosoft.com

;; Dir-hist is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; Dir-hist is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; -----------------------------------------------------------

;; To use dir-hist with J-Shell, load with j-shell.el:
;;
;;    (require 'j-shell)
;;    (require 'dir-hist)
;;


(defconst dh-version  (substring "$Revision: 1.1 $" 11 -2)
  "$Id: dir-hist.el,v 1.1 1994/02/23 21:42:39 jimt Exp $

Report bugs to: James C. Thompson <jimt@neosoft.com>")

(defconst dh-version-string (concat "Dir-Hist, version " dh-version))

(defconst dh-dir-remaps
  '((""))
  )

(defun dh-make-dirhist (dh-sym)
  "Create a new directory-history in SYMBOL."
  (if (and (symbolp dh-sym)
	   dh-sym)
      (progn
	(set dh-sym nil)
	(put dh-sym 'directory-list t)
	(if (and (setq buffer (get dh-sym 'dh-buffer))
		 (memq buffer (buffer-list)))
	    (dh-update-buffer buffer nil))
	dh-sym)))  

(defun dirhist-p (dh-sym)
  "T if SYMBOL contains a directory-history"
  (and (symbolp dh-sym)
       (boundp dh-sym)
       (listp (symbol-value dh-sym))
       (get dh-sym 'directory-list)))

(defun dh-alist-compare (a b)
  (string< (car a) (car b)))

(defun dh-add-dir (dh-sym dir)
  "To the history in SYMBOL, add the directory DIR.

The resulting list is sorted alphabetically and assigned as the value
of SYMBOL.  If the history is displayed in a buffer, the buffer is
updated to include the new directory."

  (let ((dirs (symbol-value dh-sym))
	(dir (expand-file-name dir)))
    (if (assoc dir dirs)
	nil
      (setq dirs (append dirs (list (cons dir nil))))
      (setq dirs (sort dirs 'dh-alist-compare))
      (set dh-sym dirs)
      (if (and (setq buffer (get dh-sym 'dh-buffer))
	       (memq buffer (buffer-list)))
	  (dh-update-buffer buffer dirs)))
    dirs))

(defun dh-update-buffer (buffer dirs)
  (let ((rbuf (current-buffer)))
    (unwind-protect
	(progn
	  (set-buffer buffer)
	  (let ((search-dir (concat "^"
				    (regexp-quote
				     (buffer-substring
				      (save-excursion (beginning-of-line)
						      (point))
				      (save-excursion (end-of-line) (point))))
				    "$"))
		(search-col (current-column))
		(buffer-read-only nil))
	    (delete-region (point-min) (point-max))
	    (dh-show-dirs dirs)
	    (goto-char (point-min))
	    (if (re-search-forward search-dir nil t)
		(progn
		  (move-to-column search-col)
		  (if (setq window (get-buffer-window (current-buffer)))
		      (set-window-point window (point)))))))
      (set-buffer rbuf))))

(defun dh-show-dirs (dirs)
  (let ((buffer-read-only nil))
    (while dirs
      (insert (car (car dirs)) "\n")
      (setq dirs (cdr dirs)))))

(defun dh-display (dh-sym)
  "Display the directory history in SYMBOL in a buffer.

A new buffer is created, if necessary, or the existing one is updated."
  (if (and (setq buffer (get dh-sym 'dh-buffer))
	   (memq buffer (buffer-list)))
      (set-buffer buffer)
    (setq buffer (get-buffer-create "*Directory History*"))
    (put dh-sym 'dh-buffer buffer)
    (set-buffer buffer)
    (dh-mode)
    (message dh-version-string)
    (setq dh-owner-buffer (get dh-sym 'dh-owner-buffer))
    (dh-update-buffer buffer (symbol-value dh-sym))
    (goto-char (point-min))
    (end-of-line))
  (switch-to-buffer-other-window buffer))

(defun dh-mode ()

  "Mode for viewing a directory history.  To choose from the listed
directories, use the following keys:

\\<dh-mode-map>\\[dh-return-dir] - to insert the directory into the target shell buffer.
\\[dh-return-dir-and-hide-history] - to insert the directory into the target shell buffer; this command
and the following commands hide the history buffer after making the
substitution.
\\[dh-cd-return] - to insert \"cd <directory>\" into the shell buffer.
\\[dh-pushd-return] - to insert \"pushd <directory>\" into the shell buffer.

If more than one shell buffer exists, the insertion is made into the
\"target\" shell buffer; the target buffer is the last buffer to
display its directory history.  In J-Shell, type C-c SPC to display
the history."

  (interactive)
  (setq major-mode 'dh-mode)
  (setq mode-name "Dir-Hist")

  (if (and (boundp 'dh-mode-map)
	   dh-mode-map)
      nil
    (setq dh-mode-map (make-keymap))
    (suppress-keymap dh-mode-map)
    (define-key dh-mode-map " " 'dh-return-dir)
    (define-key dh-mode-map "\C-m"  'dh-return-dir-and-hide-history)
    (define-key dh-mode-map "c" 'dh-cd-return)
    (define-key dh-mode-map "p" 'dh-pushd-return))

  (make-variable-buffer-local 'dh-owner-buffer)

  (setq buffer-read-only t)

  (use-local-map dh-mode-map))

(defun dh-own-history (dh-sym buffer)

  "Specify that the directory history in SYMBOL is owned by shell
BUFFER.  This makes the shell buffer the target for insertions from
the *Directory History*."

  (put dh-sym 'dh-owner-buffer buffer)
  (if (and (setq dh-buffer (get dh-sym 'dh-buffer))
	   (memq dh-buffer (buffer-list)))
      (save-excursion
	(set-buffer (get dh-sym 'dh-buffer))
	(setq dh-owner-buffer buffer))))

(defun dh-return-dir (&optional pre)
  "Insert the directory into the target shell buffer.

Optional argument PRE is a string inserted before the directory."

  (interactive)
  (setq dir (buffer-substring (save-excursion (beginning-of-line) (point))
			      (save-excursion (end-of-line) (point))))
  (pop-to-buffer dh-owner-buffer)
  (if pre
      (insert pre))
  (insert dir))

(defun dh-return-dir-and-hide-history (&optional pre)
  "Insert the directory into the target shell buffer, then hide the
history buffer.

Optional argument PRE is a string inserted before the directory."

  (interactive)
  (setq dh-buffer (current-buffer))
  (dh-return-dir pre)
  (delete-windows-on dh-buffer)
  (bury-buffer dh-buffer))

(defun dh-cd-return ()
  "Insert \"cd <directory>\" into the target shell buffer."

  (interactive)
  (dh-return-dir-and-hide-history "cd "))

(defun dh-pushd-return ()
  "Insert \"pushd <directory>\" into the target shell buffer."
  (interactive)
  (dh-return-dir-and-hide-history "pushd "))

(provide 'dir-hist)
