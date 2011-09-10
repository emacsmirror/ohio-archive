;*****************************************************************************
;
; Filename:	tek-motion-setup.el
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
; Description:	Set up the highlighting style used for mouse drag regions
;		under epoch.
;
;		The button style may be customised by means of X11 resources.
;		The resource name to use is "motion".
;		See the file tek-style-utils.el for details.
;
;*****************************************************************************

; $Id: tek-motion-setup.el,v 1.4 1991/10/23 02:15:10 kwood Exp $

(require 'epoch-running)
(provide 'tek-motion-setup)

(if running-epoch
    (progn
      
      (require 'motion)
      (require 'tek-style-utils)

      (defvar tek-motion-foreground "goldenrod1"
	"\
Foreground color used to underline the drag region if no value is
defined in the X11 resources and the display device supports color.")

      (defvar tek-motion-underline "goldenrod1"
	"\
Foreground color used to underline the drag region if no value is
defined in the X11 resources and the display device supports color.")

      (if(and (> (number-of-colors) 2)
	      (or tek-motion-underline tek-motion-foreground))
	  (tek-build-style "motion" motion::style
			   (or tek-bold-fixed-font
			       tek-italic-bold-fixed-font
			       tek-italic-fixed-font)
			   tek-motion-foreground (background)
			   (background) (foreground)
			   tek-motion-underline)
	(tek-build-style "motion" motion::style
			 (or tek-bold-fixed-font
			     tek-italic-bold-fixed-font
			     tek-italic-fixed-font)
			 (foreground) (background)
			 (background) (foreground)
			 (foreground)))
      )) ; end: running-epoch test
