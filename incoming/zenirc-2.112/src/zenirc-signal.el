;;; zenirc-signal.el --- Fancy signal formatting for ZenIRC

;; Copyright (C) 1993, 1994 Ben A. Mesander
;; Copyright (C) 1998 Per Persson

;; Author: Mark Bailen <msbailen@msbdcolka.cr.usgs.gov>
;;         Ben A. Mesander <ben@gnu.ai.mit.edu>
;; Maintainer: pp@sno.pp.se
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

;; This code detects if the message that triggered the signal was
;; a PRIVMSG, and if so, formats it differently. Otherwise, it calls
;; the regular zenirc-signal-hook subroutine(s).

;;; Code:

(require 'zenirc)

(defun zenirc-signal-privmsg (proc msg)
  (let ((pmsg (zenirc-parse-server-message msg)))
    (cond ((string= "PRIVMSG" (aref pmsg 0))
           (zenirc-message nil "[%s] %s->%s: %s"
                           (buffer-name)
                           (zenirc-run-hook 'zenirc-format-nickuserhost-hook
                                            (aref pmsg 1))
                           (aref pmsg 2)
                           (aref pmsg 3))
           (setq zenirc-run-next-hook nil)))))

(provide 'zenirc-signal)

(zenirc-add-hook 'zenirc-signal-hook 'zenirc-signal-privmsg)

;; zenirc-signal.el ends here
