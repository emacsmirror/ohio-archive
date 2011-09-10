;*****************************************************************************
;
; Filename:	tek-gnus-hilite-setup.el
;
; Copyright (C) 1991  Ken Wood
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 1, or (at your option)
; any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;
; Author:		Ken Wood, <kwood@austek.oz.au>
; Organisation:		Austek Microsystems Pty Ltd, Australia.
; Released with permission from Austek Microsystems.
;
; Description:	Set up highlighting under GNUS if running epoch.
;
;		See the INSTALL file that comes with this package for
;		installation details.
;
;*****************************************************************************

; $Id: tek-gnus-hilite-setup.el,v 1.3 1991/10/23 02:15:10 kwood Exp $

(require 'maclib-toolbox)
(require 'epoch-running)
(provide 'tek-gnus-hilite-setup)

(if running-epoch
    (progn

      (defun tek-init-gnus-hiliting ()
	(require 'tek-gnus-hilite)
	nil)

      (prepend-unique-hook 'gnus-Startup-hook 'tek-init-gnus-hiliting)
      ))
