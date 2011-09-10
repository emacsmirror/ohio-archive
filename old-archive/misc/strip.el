;;; @(#) strip.el - hook for removing trailing blanks from files
;;;
;;; Copyright (C) 1993 Lawrence R. Dodd
;;;
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
;;;
;;; !Modified: Tue May 11 10:26:11 EDT 1993 by dodd !
;;; !Revision: 1.4 !
;;; !Id: strip.el,v 1.4 1993/05/11 14:26:13 dodd Exp !
;;;
;;; LCD Archive Entry:
;;; strip|Lawrence R. Dodd|dodd@roebling.poly.edu|
;;; Hook for removing trailing blanks from files|
;;; 11-May-1993|1.4|~/misc/strip.el.gz|
;;;
;;; USAGE: stick some where emacs can find it, byte-compile it, and stick
;;; "(require 'strip)" in your ~/.emacs
;;;
;;; hook name:
;;;
;;;  (delete-horizontal-space-hook)
;;;
;;; interactive functions:
;;;
;;;  (delete-horizontal-space-buffer)
;;;  (delete-horizontal-space-region)

;;; delete horizontal space hook
(defun delete-horizontal-space-hook ()
  "Deletes horizontal spaces at the end of every line in buffer.  Meant to be
called as a hook."
  (delete-horizontal-space-region (point-min) (point-max))
  ;; unconditionally return `nil'
  nil)

;;; delete horizontal space for the whole buffer
(defun delete-horizontal-space-buffer ()
  "Deletes horizontal spaces at the end of every line in buffer."
  (interactive)
  (delete-horizontal-space-region (point-min) (point-max)))

;;; delete horizontal space in region
(defun delete-horizontal-space-region (beg-region end-region)
  "Deletes horizontal spaces at the end of every line in the region.
BEG-REGION and END-REGION are args which specify the region boundaries."
  (interactive "*r")
  (let ((end-region-mark (make-marker))
        (save-point (point-marker)))
    (set-marker end-region-mark end-region)
    (goto-char beg-region)
    (end-of-line)
    (delete-horizontal-space)
    (while (and  (= (forward-line 1) 0)
                 (< (point) end-region-mark))
      (end-of-line)
      (delete-horizontal-space))
    (goto-char save-point)
    (set-marker end-region-mark nil)
    (set-marker save-point nil)))

;;; prepend to write-file-hooks - checking first whether or not it is already
;;; there

;; Contributed by Ken Laprade <laprade@trantor.harris-atd.com>
;; Really should use some sort of add-hook - 16 Feb 93 - KCL
(or (and (listp write-file-hooks) (not (eq (car write-file-hooks) 'lambda)))
    (setq write-file-hooks (list write-file-hooks)))

(or (memq 'delete-horizontal-space-hook write-file-hooks)
    (setq write-file-hooks
          (append '(delete-horizontal-space-hook) write-file-hooks)))


;;; provide package
(provide 'strip)
