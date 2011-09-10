;;*****************************************************************************
;;
;; Filename:	tek-mh-e-highlight-setup.el
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
;; Based on modifications to tek-gnus-highlight-setup.el by M. Burgett
;;	(burgett@adobe.com), 2 Nov 91.
;;
;; Description:	Set up highlighting under mh-e if running epoch.
;;
;;		See the INSTALL file that comes with this package for
;;		installation details.
;;
;;
;;*****************************************************************************

;; $Id: tek-mh-e-highlight-setup.el,v 1.3 1992/08/18 04:16:24 rwhitby Rel $

(require 'unique-hooks)

(if (boundp 'epoch::version)
    (progn

      (defun tek-init-mh-e-highlighting ()
	(require 'tek-mh-e-highlight)
	nil)

      (prepend-unique-hook 'mh-inc-folder-hook 'tek-init-mh-e-highlighting)
      ))

(provide 'tek-mh-e-highlight-setup)
