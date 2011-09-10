;;; zenirc-yow-filter.el --- neutralize yowage

;; Copyright (C) 1997 Noah S. Friedman

;; Author: Noah Friedman <friedman@prep.ai.mit.edu>
;; Maintainer: friedman@prep.ai.mit.edu
;; Keywords: zenirc, extensions, oink, yow
;; Created: 1997-02-10

;; $Id: zenirc-yow-filter.el,v 2.4 1997/03/15 03:20:56 friedman Exp $

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
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; I estimate that loading this file grows emacs' permanent
;; heap by about 650K.

;;; Code:

(require 'zenirc)

(or (boundp 'yow-vector)
    (boundp 'yow-file)
    (load "yow"))

(defconst zenirc-yow-filter-table nil)

(defun zenirc-yow-filter-table-snarf (&optional file)
  (let ((table (make-vector 509 0))
        (yowfile (or file
                     (and (boundp 'yow-file)
                          yow-file)
                     (concat data-directory "yow.lines")))
        (buf (generate-new-buffer " *Yow!*"))
        (snarf-buf (function
                    (lambda ()
                      (goto-char (point-min))
                      (while (progn
                               (skip-chars-forward " \t\n\r\f")
                               (not (eobp)))
                        (intern (buffer-substring (prog1
                                                      (point)
                                                    (search-forward "\0"))
                                                  (1- (point)))
                                table))))))
    (save-excursion
      (save-match-data
        (set-buffer buf)
        (setq buffer-undo-list t)
        (insert-file-contents yowfile)
        (search-forward "\0")
        (delete-region (point-min) (point))
        (while (re-search-forward "\n" nil t)
          (delete-char -1))
        (funcall snarf-buf)
        (goto-char (point-min))
        (while (re-search-forward "[ \t\n\r\f]+" nil t)
          (replace-match " "))
        (funcall snarf-buf)))
    (kill-buffer buf)
    table))

(defun zenirc-yow-filter (proc parsedmsg)
  (or zenirc-yow-filter-table
      (setq zenirc-yow-filter-table (zenirc-yow-filter-table-snarf)))
  (cond ((intern-soft (aref parsedmsg 3) zenirc-yow-filter-table)
         (and zenirc-debug-ignore
              (zenirc-message proc 'debug (format "Ignored: %s" parsedmsg)))
         (setq zenirc-run-next-hook nil))))

(zenirc-add-hook 'zenirc-server-PRIVMSG-hook 'zenirc-yow-filter)
(zenirc-add-hook 'zenirc-server-NOTICE-hook  'zenirc-yow-filter)

(provide 'zenirc-yow-filter)

;;; zenirc-yow-filter.el ends here.
