;;*****************************************************************************
;;
;; Filename:	tek-dired-highlight-setup.el
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
;; Author:		Rod Whitby, <rwhitby@research.canon.oz.au>
;;
;; Description:	Set up highlighting under dired if running epoch.
;;
;;		See the INSTALL file that comes with this package for
;;		installation details.
;;
;;*****************************************************************************

;; $Id: tek-dired-highlight-setup.el,v 1.1 1992/08/18 04:13:20 rwhitby Rel $ 

(require 'unique-hooks)

(if (boundp 'epoch::version)
    (progn

      (defun tek-init-dired-highlighting ()
	(require 'tek-dired-highlight)
	nil)
      
      (prepend-unique-hook 'dired-load-hook 'tek-init-dired-highlighting)
      ))

(provide 'tek-dired-highlight-setup)
