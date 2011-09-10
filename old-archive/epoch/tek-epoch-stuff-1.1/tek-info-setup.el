;*****************************************************************************
;
; Filename:	tek-info-setup.el
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
; Description:	Set up to use Dave Gillespie's "kitchen sink" info browser
;		under epoch. Provides highlighting and mouse bindings.
;
;		See the INSTALL file that comes with this package for
;		installation details.
;
;*****************************************************************************

; $Id: tek-info-setup.el,v 1.6 1991/12/12 05:38:50 kwood Exp $

(require 'epoch-running)
(require 'maclib-toolbox)
(provide 'tek-info-setup)

(if (fboundp 'info)
    (fmakunbound 'info))

(if running-epoch
    (progn
      
      (defun tek-info-init-for-epoch ()
	"Initialise the info browser for use under epoch."
	(require 'tek-info-buttons))


      (prepend-unique-hook 'Info-load-hook 'tek-info-init-for-epoch)
      (prepend-unique-hook 'Info-mode-hook 'Info-setup-mouse-map)
      (prepend-unique-hook 'Info-select-hook 'Info-setup-buttons)
      ))

; Load emacs-based info
(autoload 'info "tek-info" "autoloadable function" t)
