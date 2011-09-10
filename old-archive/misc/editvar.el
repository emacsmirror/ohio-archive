
;;; editvar.el --- Edit variable contents

;; Copyright (C) 1995 Lennart Staflin

;; Author: Lennart Staflin <lenst@lysator.liu.se>
;; Version: $Id: editvar.el,v 1.1 1995/08/26 20:17:56 lenst Exp lenst $
;; Keywords: 
;; Last edited: Sat Aug 26 22:35:55 1995 by lenst@tiny.lysator.liu.se (Lennart Staflin)

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
;;; program's author (send electronic mail to lenst@lysator.liu.se) or from
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.
;;;

;; LCD Archive Entry:
;; editvar|Lennart Staflin|lenst@lysator.liu.se|
;; Edit a variable|
;; 21-May-1995|1.1|~/misc/editvar.el.gz|

;;; Commentary:

;; Provides a command to edit an Emacs Lisp variable in a buffer.
;; Any variable can be edited, not only user options.


;;; Code:

;;;###autoload
(defun edit-variable (var)
  "Edit a variable in a pop up window, and recursive edit.
Exit with C-c C-c.  Pretty print (reformat) the edited variable value
with C-c C-p."
  (interactive (list
		(intern
		 (completing-read "Variable: " obarray 'boundp t)))) 
  (let ((val (eval var))
	(buf (get-buffer-create
	      (concat "*Edit variable " (symbol-name var) "*")))
	(stab (syntax-table)))
    (set
     var
     (save-window-excursion
       (save-excursion
	 (pop-to-buffer buf)
	 (erase-buffer)
	 (emacs-lisp-mode)
	 (use-local-map (copy-keymap (current-local-map)))
	 (local-set-key "\C-c\C-c" 'exit-recursive-edit)
	 (local-set-key "\C-c\C-p" 'edit-variable-pretty-print)
	 ;;(set-syntax-table emacs-lisp-mode-syntax-table)
	 (cond
	  ((documentation-property var 'variable-documentation)
	   (insert (documentation-property var 'variable-documentation))
	   (insert "\n\n")
	   (goto-char (point-min))
	   (while (not (eobp))
	     (insert ";;; ")
	     (forward-line 1))))
	 (print val (current-buffer))
	 (message
	  "Exit with C-c C-c and abort with C-]; pretty print with C-c C-p")
	 (recursive-edit)
	 (goto-char (point-min))
	 (prog1 (read buf)
	   (kill-buffer buf)))))))

(defun edit-variable-pretty-print ()
  "Pretty print the sexp in the buffer."
 ;;; This code is stolen from some pretty print function posted on UseNet
 ;;; who sholud be attributed?
  (interactive)
  (goto-char (point-max))
  (forward-sexp -1)
  (save-excursion
    (while (not (eobp))
      ;; (message "%06d" (- (point-max) (point)))
      (cond
       ((looking-at "\\s(")
	(while (looking-at "\\s(")
	  (forward-char 1)))
       ;; (looking-at "quote")
       ((condition-case err-var
	    (prog1 t (down-list 1))
	  (error nil))
	(backward-char 1)
	(skip-chars-backward " \t\n")
	(delete-region
	 (point)
	 (progn (skip-chars-forward " \t\n") (point)))
	(insert ?\n))
       ((condition-case err-var
	    (prog1 t (up-list 1))
	  (error nil))
	(while (looking-at "\\s)")
	  (forward-char 1))
	(skip-chars-backward " \t")
	(delete-region
	 (point)
	 (progn (skip-chars-forward " \t\n") (point)))
	(insert ?\n))
       (t (goto-char (point-max))))))
  (indent-sexp))

;;; editvar.el ends here
