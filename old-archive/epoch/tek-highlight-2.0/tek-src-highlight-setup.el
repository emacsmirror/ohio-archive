;;*****************************************************************************
;;
;; Filename:	tek-src-highlight-setup.el
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
;; Description:	Set up to do comment highlighting in source code buffers.
;;		Highlighting is updated on find-file and save-buffer.
;;
;;		See the INSTALL file that comes with this package for
;;		installation details.
;;
;;*****************************************************************************

;; $Id: tek-src-highlight-setup.el,v 1.5 1992/08/18 04:16:24 rwhitby Rel $

(require 'unique-hooks)

(if (boundp 'epoch::version)
    (progn
      ;; Update zones when files are loaded or saved.
      (postpend-unique-hook 'find-file-hooks
			   'tek-highlight-comments-on-find)
      (postpend-unique-hook 'write-file-hooks
			   'tek-highlight-comments-on-write)

      ;; Where to find the comment highlighting functions.
      (autoload 'tek-highlight-comments-on-find
		"tek-src-highlight" "autoloadable function" t)
      (autoload 'tek-highlight-comments-on-write
		"tek-src-highlight" "autoloadable function" t)
      ))

(provide 'tek-src-highlight-setup)
