;;; rcs-clean.el --- replaces the $ around RCS keywords making them inert

;;; Copyright (C) 1993 Lawrence R. Dodd

;; Author: Lawrence R. Dodd <dodd@roebling.poly.edu>
;; Maintainer: Lawrence R. Dodd <dodd@roebling.poly.edu>
;; Created: 29 April 1993
;; Version: 1.16
;; Keywords: extensions

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; !Modified: Sat Sep 18 09:58:22 EDT 1993 by dodd !
;;; !Id: rcs-clean.el,v 1.16 1993/09/18 13:59:04 dodd Exp !
;;; !Revision: 1.16 !

;;; bugs to: dodd@roebling.poly.edu (please mention the version number).

;;; LCD Archive Entry:
;;; rcs-clean|Lawrence R. Dodd|dodd@roebling.poly.edu|
;;; replaces the $ around RCS keywords making them inert.|
;;; 19-Sep-1993|1.16|~/misc/rcs-clean.el.Z|

;;; USAGE: 
;;;  
;;; Save as `rcs-clean.el' in a directory where emacs can find it. Stick 
;;; 
;;;                   (require 'rcs-clean) 
;;;                   
;;; in your ~/.emacs or the site initialization file.  Intended to be called 
;;; interactively: "M-x rcs-clean" or "M-x rcs-clean-region"
;;; 
;;; the user-defined variables are:
;;;  
;;;     rcs-clean-list
;;;     rcs-clean-replacement
;;;  
;;; to find out more about these variables, load this file, put your cursor at 
;;; the end of any of the above lines, and hit C-h v [RET].
;;;  
;;; to append to the user variable `rcs-clean-list' do something like
;;; 
;;;   (setq rcs-clean-list (append rcs-clean-list (list "Foobar")))


;;; Code:

(defvar rcs-clean-list '("Author" "Date" "Header" "Id" "Locker" "Log"
                         "RCSfile" "Revision" "Source" "State")
  "List of RCS keywords as strings.")

(defvar rcs-clean-replacement "!"
  "*String used to replace the `$' prefixing and postfixing the RCS keywords.
Used by routine `(rcs-clean)'.")

(defun rcs-clean-region (beg end)

  "in region replace $ around `rcs-clean-list' with `rcs-clean-replacement'.
allows one to clean a piece of code containing RCS keywords making sure that
these keywords can not be overwritten accidentally later on."

  (interactive "*r")
  
  (save-excursion
    
    (let ((tlist rcs-clean-list)
          (orig_ro (if buffer-read-only (toggle-read-only))) ; make readable
          (case-fold-search nil); case-sensitive searches
          search-string elt
          (replacement-string
           (concat rcs-clean-replacement "\\1" rcs-clean-replacement)))
      
      ;; replace $ with ! (or whatever is in `rcs-clean-replacement')
      (while (and tlist (setq elt (car tlist)))
        
        ;; go to top of buffer and set search string
        (goto-char beg)
        (setq search-string (concat "\\$\\(" elt ":?[^\n\\$]* ?\\)\\$"))

        ;; can we find the keyword anywhere in buffer?
        (while (re-search-forward search-string end t)
          (replace-match replacement-string t))
        
        ;; decrement
        (setq tlist (cdr tlist)))

      ;; if the buffer was originally read-only, then make it so again
      (if orig_ro (toggle-read-only)))))

(defun rcs-clean ()

  "in buffer replace $ around `rcs-clean-list' with `rcs-clean-replacement'.
allows one to clean a piece of code containing RCS keywords making sure that
these keywords can not be overwritten accidentally later on."

  (interactive)

  ;; replace `;; Version: !Revision...!', of lisp-mnt.el-type
  ;; header, with `;; Version: num'
  (let ((orig_ro (if buffer-read-only (toggle-read-only)))
        (case-fold-search nil))

    (save-excursion
      (goto-char (point-min))
     
      (and (re-search-forward
            (concat "\\(^[; ]* [Vv]ersion: \\) *\\$" 
                    "Revision: "
                    "\\([^\n\\$]+\\) \\$")
            (point-max) t)
           (replace-match "\\1\\2" t)))

    (rcs-clean-region (point-min) (point-max))))


(provide 'rcs-clean)

;;; rcs-clean.el ends here
