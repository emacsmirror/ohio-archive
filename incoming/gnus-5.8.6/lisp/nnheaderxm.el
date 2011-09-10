;;; nnheaderxm.el --- making Gnus backends work under XEmacs
;; Copyright (C) 1996,97,98,99 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(defun nnheader-xmas-run-at-time (time repeat function &rest args)
  (start-itimer
   "nnheader-run-at-time"
   `(lambda ()
      (,function ,@args))
   time repeat))

(fset 'nnheader-run-at-time 'nnheader-xmas-run-at-time)
(fset 'nnheader-cancel-timer 'delete-itimer)
(fset 'nnheader-cancel-function-timers 'ignore)

(provide 'nnheaderxm)

;;; nnheaderxm.el ends here.
