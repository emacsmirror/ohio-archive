;*****************************************************************************
;
; Filename:	tek-gnus-hilite.el
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
; Description:	Highlight fields in articles displayed by the GNUS
;		newsreader under epoch.
;
;		Button styles may be customised by means of X11 resources.
;		The resource names to use are "gnus-from" and
;		"gnus-subject". See the file tek-style-utils.el for details.
;
;		See the INSTALL file that comes with this package for
;		installation details.
;
;*****************************************************************************

; $Id: tek-gnus-hilite.el,v 1.5 1991/11/21 02:58:32 kwood Exp $

(provide 'tek-gnus-hilite)
(require 'epoch-running)

(if running-epoch
    (progn
      
      (require 'tek-style-utils)

      (defvar tek-gnus-from-foreground "blue3"
	"\
Foreground color used to highlight From: fields in GNUS if no value is
defined in the X11 resources and the display device supports color. On
monochrome screens a different font is used in place of the different
color.")

      (defvar tek-gnus-from-styleorattribute
	; If the display supports multiple colors and a default color
	; is specified, define the style to use a different color.
	(if (and (> (number-of-colors) 2) tek-gnus-from-foreground)
	    (tek-build-style "gnus-from"
			     nil nil
			     tek-gnus-from-foreground (background)
			     (background) (foreground))
	  ; Otherwise, define the style to use a different font.
	  (tek-build-style "gnus-from" nil (or tek-italic-bold-fixed-font
						 tek-bold-fixed-font
						 tek-italic-fixed-font)
			   (foreground) (background)
			   (background) (foreground)))
	"\
Style or attribute used to display From: fields in news articles
displayed by GNUS.")


      (defvar tek-gnus-subject-foreground "red3"
	"\
Foreground color used to highlight Subject: fields in GNUS if no value is
defined in the X11 resources and the display device supports color. On
monochrome screens a different font is used in place of the different
color.")

      (defvar tek-gnus-subject-underline "red3"
	"\
Foreground color used to underline Subject: fields in GNUS if no value is
defined in the X11 resources and the display device supports color. On
monochrome screens a different font is used in place of the different
color.")

      (defvar tek-gnus-subject-styleorattribute
	; If the display supports multiple colors and a default color
	; is specified, define the style to use a different color.
	(if(and (> (number-of-colors) 2)
		(or tek-gnus-subject-underline
		    tek-gnus-subject-foreground))
	    (tek-build-style "gnus-subject" nil nil
			     tek-gnus-subject-foreground (background)
			     (background) (foreground)
			     tek-gnus-subject-underline)
	  (tek-build-style "gnus-subject" nil
			   (or tek-bold-fixed-font
			       tek-italic-bold-fixed-font
			       tek-italic-fixed-font)
			   (foreground) (background)
			   (background) (foreground)
			   (foreground)))
	"\
Style or attribute used to display Subject: fields in news articles
displayed by GNUS.")


      ; Select V3 or V4 button behaviour
      (if tek-highlight-use-attributes
	  (progn
	    ; Do things the old way - using attributes.
      
	    (defvar tek-gnus-from-style tek-gnus-from-styleorattribute
	      "\
Style used for displaying From: fields in news articles displayed
by GNUS when attributes are used to mark buttons.")

	    ; Modify the variable used with add-button to be an attribute
	    (setq tek-gnus-from-styleorattribute (reserve-attribute))

	    ;Bind the from-style to the from-attribute
	    (set-attribute-style tek-gnus-from-styleorattribute
				 tek-gnus-from-style)

	    (defvar tek-gnus-subject-style tek-gnus-subject-styleorattribute
	      "\
Style used for displaying Subject: fields in news articles displayed
by GNUS when attributes are used to mark buttons.")
	    
	    ; Modify the variable used with add-button to be an attribute
	    (setq tek-gnus-subject-styleorattribute (reserve-attribute))

	    ;Bind the subject-style to the subject-attribute
	    (set-attribute-style tek-gnus-subject-styleorattribute
				 tek-gnus-subject-style)
	    ))


      ; Define the highlighting function
      (defun tek-article-hilite ()
	"\
Highlight From: and Subject: fields in news articles displayed by
GNUS."
	(let (
	      (starting-buffer (current-buffer))
	      )
	  (set-buffer gnus-Article-buffer)
	  (save-excursion
	    (clear-buttons)
	    (goto-char (point-min))
	    (if (re-search-forward "^From: \\(.*\\)" nil t)
		(add-button (match-beginning 1) (match-end 1)
			    tek-gnus-from-styleorattribute))
	    (goto-char (point-min))
	    (if (re-search-forward "^Subject: \\(.*\\)" nil t)
		(add-button (match-beginning 1) (match-end 1)
			    tek-gnus-subject-styleorattribute))
	    )
	  (set-buffer starting-buffer)))


      ; Set up the hook to run the highlighting function after displaying
      ; each article. Have to explicitly set the variable to an assumed
      ; value, since the default GNUS setup doesn't initialise it to
      ; a proper list of hook functions.
      (setq gnus-Select-article-hook (list 'gnus-Subject-show-thread
					   'tek-article-hilite))
      
      )) ; end: running-epoch test
