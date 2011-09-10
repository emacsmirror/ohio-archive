;*****************************************************************************
;
; Filename:	tek-vm-hilite.el
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
; Description:	Highlight fields in messages displayed by the VM
;		mailer under epoch.
;
;		Button styles may be customised by means of X11 resources.
;		The resource names to use are "VM-from" and
;		"VM-subject". See the file tek-style-utils.el for details.
;
;		See the INSTALL file that comes with this package for
;		installation details.
;
;*****************************************************************************

; $Id: tek-vm-hilite.el,v 1.5 1991/11/21 02:56:58 kwood Exp $

(provide 'tek-vm-hilite)
(require 'epoch-running)

(if running-epoch
    (progn
      
      (require 'tek-style-utils)

      (defvar tek-vm-from-foreground "blue3"
	"\
Foreground color used to highlight From: fields in VM if no value is
defined in the X11 resources and the display device supports color. On
monochrome screens a different font is used in place of the different
color.")

      (defvar tek-vm-from-styleorattribute
	; If the display supports multiple colors and a default color
	; is specified, define the style to use a different color.
	(if (and (> (number-of-colors) 2) tek-vm-from-foreground)
	    (tek-build-style "VM-from"
			     nil nil
			     tek-vm-from-foreground (background)
			     (background) (foreground))
	  ; Otherwise, define the style to use a different font.
	  (tek-build-style "VM-from" nil (or tek-italic-bold-fixed-font
						 tek-bold-fixed-font
						 tek-italic-fixed-font)
			   (foreground) (background)
			   (background) (foreground)))
	"\
Style or attribute used to display From: fields in mail messages
displayed by VM.")


      (defvar tek-vm-subject-foreground "red3"
	"\
Foreground color used to highlight Subject: fields in VM if no value is
defined in the X11 resources and the display device supports color. On
monochrome screens a different font is used in place of the different
color.")

      (defvar tek-vm-subject-underline "red3"
	"\
Foreground color used to underline Subject: fields in VM if no value is
defined in the X11 resources and the display device supports color. On
monochrome screens a different font is used in place of the different
color.")

      (defvar tek-vm-subject-styleorattribute
	; If the display supports multiple colors and a default color
	; is specified, define the style to use a different color.
	(if(and (> (number-of-colors) 2)
		(or tek-vm-subject-underline
		    tek-vm-subject-foreground))
	    (tek-build-style "VM-subject" nil nil
			     tek-vm-subject-foreground (background)
			     (background) (foreground)
			     tek-vm-subject-underline)
	  (tek-build-style "VM-subject" nil
			   (or tek-bold-fixed-font
			       tek-italic-bold-fixed-font
			       tek-italic-fixed-font)
			   (foreground) (background)
			   (background) (foreground)
			   (foreground)))
	"\
Style or attribute used to display Subject: fields in mail messages
displayed by VM.")


      ; Select V3 or V4 button behaviour
      (if tek-highlight-use-attributes
	  (progn
	    ; Do things the old way - using attributes.
      
	    (defvar tek-vm-from-style tek-vm-from-styleorattribute
	      "\
Style used for displaying From: fields in mail messages displayed
by VM when attributes are used to mark buttons.")

	    ; Modify the variable used with add-button to be an attribute
	    (setq tek-vm-from-styleorattribute (reserve-attribute))

	    ;Bind the from-style to the from-attribute
	    (set-attribute-style tek-vm-from-styleorattribute
				 tek-vm-from-style)

	    (defvar tek-vm-subject-style tek-vm-subject-styleorattribute
	      "\
Style used for displaying Subject: fields in mail messages displayed
by VM when attributes are used to mark buttons.")

	    ; Modify the variable used with add-button to be an attribute
	    (setq tek-vm-subject-styleorattribute (reserve-attribute))

	    ;Bind the subject-style to the subject-attribute
	    (set-attribute-style tek-vm-subject-styleorattribute
				 tek-vm-subject-style)
	    ))


      ; Define the highlighting function. Basically just redefine the
      ; standard VM function so it uses epoch buttons.
      (defun vm-highlight-headers (message window)
	"\
Highlight From: and Subject: fields in mail messages displayed by
VM."
	(let ((debug-on-error t))
	      (save-excursion
	       ;; As of v18.52, this call to save-window-excursion is needed!
	       ;; Somehow window point can get fouled in here, and drag the
	       ;; buffer point along with it.  This problem only manifests
	       ;; itself when operating VM from the summary buffer, subsequent
	       ;; to using vm-beginning-of-message or vm-end-of-message.
	       ;; After running a next or previous message command, point
	       ;; somehow ends up at the end of the message.
	      (save-window-excursion
		 (progn
		   (clear-buttons)
		   (goto-char (point-min))
		   (if (re-search-forward "^From: \\(.*\\)" nil t)
		       (add-button (match-beginning 1) (match-end 1)
				   tek-vm-from-styleorattribute))
		   (goto-char (point-min))
		   (if (re-search-forward "^Subject: \\(.*\\)" nil t)
		       (add-button (match-beginning 1) (match-end 1)
				   tek-vm-subject-styleorattribute))
		   )))))
      )) ; end: running-epoch test
