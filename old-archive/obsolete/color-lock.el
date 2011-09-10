;;; color-lock.el ---  colour font-lock fontification.

;; Author: Simon Marshall <Simon.Marshall@mail.esrin.esa.it>
;; Keywords: faces
;; Version: 1.00

;; LCD Archive Entry:
;; color-lock|Simon Marshall|Simon.Marshall@mail.esrin.esa.it|
;; Color (colour) for font-lock mode.|
;; 27-May-1994|1.00|~/misc/color-lock.el.Z|

;; The archive is archive.cis.ohio-state.edu in /pub/gnu/emacs/elisp-archive.

;;; This file is not part of GNU Emacs.

;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Purpose:
;;
;; To make font-lock-mode use colours instead of the default fonts.

;; Installation:
;; 
;; Put in your ~/.emacs:
;;
;; (eval-after-load "font-lock" '(load "color-lock"))
;;
;; If you wish to change the colours, or change which faces are coloured, look
;; at the variable `color-lock-faces'.  Set this variable in your ~/.emacs.

;; Feedback:
;;
;; Please send me bug reports, bug fixes, and extensions, so that I can
;; merge them into the master source.
;;     - Simon Marshall (Simon.Marshall@mail.esrin.esa.it)

;; History:
;;

(require 'font-lock)

(defvar color-lock-faces
  '((font-lock-comment-face . "Firebrick")
    (font-lock-string-face . "RosyBrown")
    (font-lock-doc-string-face . "Brown")
    (font-lock-keyword-face . "Purple")
    (font-lock-function-name-face . "Blue")
    (font-lock-type-face . "SeaGreen"))
  "List of colours for faces.
Each element of the list is in the form (FACE-NAME . COLOUR-NAME).
FACE-NAME should be a symbol, e.g., font-lock-comment-face.
COLOUR-NAME should be a string describing the foreground colour.")

(if (x-display-color-p)
    (let ((colour-faces color-lock-faces))
      (while colour-faces
	(let* ((face (car (car colour-faces)))
	       (colour (cdr (car colour-faces)))
	       (colour-face (intern (concat (symbol-name face) "-colour"))))
	  (make-face colour-face)
	  (set-face-foreground colour-face colour)
	  (set face colour-face)
	  (setq colour-faces (cdr colour-faces))))))

;;; color-lock.el ends here
