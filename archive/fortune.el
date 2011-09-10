;;; fortune.el --- Mode for editing fortune files

;; Copyright (C) 2000 by Michael Abraham Shulman

;; Emacs Lisp Archive Entry
;; Filename: fortune.el
;; Author: Michael Abraham Shulman <viritrilbia@users.sourceforge.net>
;; Version: 1.0

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file defines a mode that can be used for editing fortune
;; files, i.e. files containing quotations separated by `%'
;; characters.  Since such files do not have a consistent extension,
;; you must use file local variables lists or modify auto-mode-alist
;; yourself if you wish this mode to load automatically.  E.g.
;;
;; (add-to-list 'auto-mode-alist
;;              '("\\`/usr/share/games/fortunes/" . fortune-mode))

;;; Code:

(define-derived-mode fortune-mode text-mode "Fortune"
  "Major mode for editing fortune files."
  (set (make-local-variable 'paragraph-start) "[ \t\n\f]\\|%")
  (set (make-local-variable 'paragraph-separate) "[ \t\f]*$\\|%$")
  (local-set-key "\C-c\C-c" 'fortune-strfile-file))

(defun fortune-strfile-file ()
  "Run strfile on the current file"
  (interactive)
  (shell-command (format "strfile %s" (buffer-file-name))))

(provide 'fortune)

;;; fortune.el ends here