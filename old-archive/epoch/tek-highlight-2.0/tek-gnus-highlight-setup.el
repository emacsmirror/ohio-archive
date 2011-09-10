;;*****************************************************************************
;;
;; Filename:	tek-gnus-highlight-setup.el
;;
;; Copyright (C) 1992  Rod Whitby
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; Modified by:		Rod Whitby, <rwhitby@research.canon.oz.au>
;; Author:		Ken Wood, <kwood@austek.oz.au>
;;
;; Description:	Set up highlighting under GNUS if running epoch.
;;
;;		See the INSTALL file that comes with this package for
;;		installation details.
;;
;;*****************************************************************************

;; $Id: tek-gnus-highlight-setup.el,v 1.5 1992/08/18 04:13:28 rwhitby Rel $

(require 'unique-hooks)

(if (boundp 'epoch::version)
    (progn

      (defun tek-init-gnus-highlighting ()
	(require 'tek-gnus-highlight)
	nil)

      (prepend-unique-hook 'gnus-Startup-hook 'tek-init-gnus-highlighting)
      ))

(provide 'tek-gnus-highlight-setup)
