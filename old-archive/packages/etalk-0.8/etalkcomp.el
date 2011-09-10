;;; compile the etalk program

;;;
;;; Copyright (C) 1994 Free Software Foundation
;;;
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
;;; program's author (see below) or write to:
;;;
;;;              The Free Software Foundation, Inc.
;;;              675 Mass Ave.
;;;              Cambridge, MA 02139, USA. 
;;;
;;; Please send bug reports, etc. to zappo@gnu.ai.mit.edu.
;;;

(load-library "bytecomp")

(let* ((cdir (expand-file-name default-directory))
       (udir (substring cdir 0 (- (length cdir) 1))))

  (setq load-path (cons udir load-path))

  (message "PATH:\n%s" load-path)

  ;; We must load the libraries so emacs knows about local variables
  ;; across multiple files.
  (load-library "etalk.el")
  (load-library "etalk-spec.el")
  (load-library "etalk-tyrn.el")
  (load-library "etalk-mini.el")
  (load-library "tyrn-ai.el")

  (message "Calling recompile on %s" udir)

  (byte-recompile-directory udir 0)

  (etalk-version)
)
