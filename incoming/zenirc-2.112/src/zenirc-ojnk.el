;;; zenirc-ojnk.el --- Send ojnks - example of adding commands to zenirc

;; Copyright (C) 1993, 1994 Ben A. Mesander

;; Author: Ben A. Mesander <ben@gnu.ai.mit.edu>
;; Maintainer: ben@gnu.ai.mit.edu
;; Keywords: extensions
;; Created: 1993/06/03

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 675 Massachusetts Avenue; Cambridge, MA 02139, USA.

;;; Commentary:

;; Examples of defining your own commands in zenirc.
;; Modeled after ircII code by dmarcher@autarch.acsu.buffalo.edu

;; /ojnk victim & /ojnkflood victim

;; OJNK is the sound a PJGLET makes.

;;; Code:

(require 'zenirc)

(defvar zenirc-command-ojnk-hook '(zenirc-command-ojnk))
(defvar zenirc-command-ojnkflood-hook '(zenirc-command-ojnkflood))

;; /ojnk victim
(defun zenirc-command-ojnk (proc cmd)
  (process-send-string
   proc (concat "PRIVMSG " (cdr cmd)
		" :\C-aOJNK! ^. .^\C-a\C-aOJNK! ( @ )\C-a\n")))

;; /ojnkflood victim
(defun zenirc-command-ojnkflood (proc cmd)
  (process-send-string
   proc (concat "PRIVMSG " (cdr cmd)
		" :\C-aOJNK! ^. .^\C-a\C-aOJNK! ( @ )\C-a\C-aOJNK! ojnk!\C-a\C-aOJNK! ^. .^\C-a\C-aOJNK! ( @ )\C-a\C-aOJNK! ojnk!\C-a\C-aOJNK! ^. .^\C-a\C-aOJNK! ( @ )\C-a\C-aOJNK! ojnk!\C-a\C-aOJNK! ^. .^\C-a\C-aOJNK! ( @ )\C-a\C-aOJNK! ojnk!\C-a\C-aOJNK! ^. .^\C-a\C-aOJNK! ( @ )\C-a\C-aOJNK! ojnk!\C-a\C-aOJNK! ^. .^\C-a\C-aOJNK! ( @ )\C-a\C-aOJNK! ojnk!\C-a\C-aOJNK! ^. .^\C-a\C-aOJNK! ( @ )\C-a\C-aOJNK! ojnk!\C-a\C-aOJNK! ^. .^\C-a\C-aOJNK! ( @ )\C-a\C-aOJNK! ojnk!\C-a\C-aOJNK! ^. .^\C-a\C-aOJNK! ( @ )\C-a\C-aOJNK! ojnk!\C-a\C-aOJNK! ^. .^\C-a\C-aOJNK! ( @ )\C-a\C-aOJNK! ojnk!\C-a\C-aOJNK! ^. .^\C-a\C-aOJNK! ( @ )\C-a\C-aOJNK! ojnk!\C-a\n")))

(provide 'zenirc-ojnk)

;;; zenirc-ojnk.el ends here
