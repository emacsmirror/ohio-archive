;; $Header: /home/abel/cvs/src/misc/emacs/edo-tools.el,v 1.4 1997/12/29 22:36:20 abel Exp $

;; edo-tools.el - Useful routines for emacs

;; Author: Alexander L. Belikoff (abel@bfr.co.il)
;; Version:  1.0 ($Revision: 1.4 $)
;; Keywords: routines, faces

;; Copyright (C) 1997 Alexander L. Belikoff

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; Commentary:

;; The following is a bunch of generally useful routines I happened to
;; replicate in each package I wrote. Therefore I've isolated them in
;; this very file.


;; emacs brand identification

(if (string-match "[Xx][Ee]macs" emacs-version)
    (setq edo-emacs-is-xemacs t)
  (setq edo-emacs-is-xemacs nil))


;; faces

(defmacro edo-make-face-once (face &optional proto fg bg fn)
  "This macro checks whether the face FACE exists, and if it doesn't, it
copies it from the face PROTO (if non-nil) or creates it. The FG
argument is a sting containing a color name.  If it is supplied, it is
used to define the new face's foregroung color.  The same applies to
BG and FN arguments - they are used to set the face's backgroung and
font respectively.

Examples:

  (edo-make-face-once 'foo-some-face nil)
  (edo-make-face-once 'foo-other-face 'bold \"DarkGreen\")
  (edo-make-face-once 'foo-one-more-face nil \"DarkGreen\" \"Yellow\" \"6x13\")


\(edo-make-face-once FACE &optional PROTO FG BG FN\)"

  `(if (not (facep ,face))
       (progn
	 (if ,proto
	     (copy-face ,proto ,face)
	   (make-face ,face))
	 (if ,fg
	     (set-face-foreground ,face ,fg))
	 (if ,bg
	     (set-face-background ,face ,bg))
	 (if ,fn
	     (set-face-font ,face ,fn)))))



(provide 'edo-tools)

;; end of $Source: /home/abel/cvs/src/misc/emacs/edo-tools.el,v $
