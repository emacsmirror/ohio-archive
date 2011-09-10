;*****************************************************************************
;
; Filename:	tek-mh-e-hilite-setup.el
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
; Description:	Set up highlighting under mh-e if running epoch.
;
;		See the INSTALL file that comes with this package for
;		installation details.
;
;  Based on modifications to tek-gnus-hilite-setup.el by M. Burgett
;	(burgett@adobe.com), 2 Nov 91.
;
;*****************************************************************************

; $Id: tek-mh-e-hilite-setup.el,v 1.2 1991/11/21 03:22:30 kwood Exp $

(require 'maclib-toolbox)
(require 'epoch-running)
(provide 'tek-mh-e-hilite-setup)

(if running-epoch
    (progn

      (defun tek-init-mh-e-hiliting ()
	(require 'tek-mh-e-hilite)
	nil)

      (prepend-unique-hook 'mh-inc-folder-hook 'tek-init-mh-e-hiliting)
      ))
