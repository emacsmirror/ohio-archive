;*****************************************************************************
;
; Filename:	tek-man-setup.el
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
; Description:	Set up to use enhanced manual browser.
;
;		See the INSTALL file that comes with this package for
;		installation details.
;
;*****************************************************************************

; $Id: tek-man-setup.el,v 1.5 1991/10/23 02:16:57 kwood Exp $

(require 'epoch-running)
(provide 'tek-man-setup)

(if (fboundp 'manual-entry)
    (fmakunbound 'manual-entry))

(if (fboundp 'man)
    (fmakunbound 'man))

; Load emacs-based manual
(autoload 'manual-entry "tek-man" "Improved manual browsing" t)
(autoload 'man "tek-man" "Improved manual browsing" t)
