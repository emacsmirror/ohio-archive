;;*****************************************************************************
;;
;; Filename:	tek-mh-e-highlight.el
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
;; Based on modifications to tek-gnus-highlight.el by M. Burgett
;;	(burgett@adobe.com), 2 Nov 91.
;;
;;
;; Description:	Highlight fields in messages displayed by the mh-e
;;		mailer under epoch.
;;
;;		Button styles may be customised by means of X11 resources.
;;		The resource names to use are "mh-e-from" and
;;		"mh-e-subject". See the file tek-style-utils.el for details.
;;
;;		See the INSTALL file that comes with this package for
;;		installation details.
;;
;;*****************************************************************************

;; $Id: tek-mh-e-highlight.el,v 1.5 1992/08/18 04:14:06 rwhitby Rel $

(if (boundp 'epoch::version)
    (progn
      
      (require 'tek-style-utils)

      (defvar tek-mh-e-from-foreground "blue3"
	"\
Foreground color used to highlight From: fields in mh-e if no value is
defined in the X11 resources and the display device supports color. On
monochrome screens a different font is used in place of the different
color.")

      (defvar tek-mh-e-from-styleorattribute
	;; If the display supports multiple colors and a default color
	;; is specified, define the style to use a different color.
	(if (and (> (number-of-colors) 2) tek-mh-e-from-foreground)
	    (tek-build-style "mh-e-from" nil nil
			     tek-mh-e-from-foreground (background)
			     (background) (foreground))
	  ;; Otherwise, define the style to use a different font.
	  (tek-build-style "mh-e-from" nil
			   (or tek-italic-bold-fixed-font
			       tek-bold-fixed-font
			       tek-italic-fixed-font)
			   (foreground) (background)
			   (background) (foreground)))
	"\
Style or attribute used to display From: fields in mail messages
displayed by mh-e.")


      (defvar tek-mh-e-subject-foreground "red3"
	"\
Foreground color used to highlight Subject: fields in mh-e if no value is
defined in the X11 resources and the display device supports color. On
monochrome screens a different font is used in place of the different
color.")

      (defvar tek-mh-e-subject-underline "red3"
	"\
Foreground color used to underline Subject: fields in mh-e if no value is
defined in the X11 resources and the display device supports color. On
monochrome screens a different font is used in place of the different
color.")

      (defvar tek-mh-e-subject-styleorattribute
	;; If the display supports multiple colors and a default color
	;; is specified, define the style to use a different color.
	(if(and (> (number-of-colors) 2)
		(or tek-mh-e-subject-underline
		    tek-mh-e-subject-foreground))
	    (tek-build-style "mh-e-subject" nil nil
			     tek-mh-e-subject-foreground (background)
			     (background) (foreground)
			     tek-mh-e-subject-underline)
	  (tek-build-style "mh-e-subject" nil
			   (or tek-bold-fixed-font
			       tek-italic-bold-fixed-font
			       tek-italic-fixed-font)
			   (foreground) (background)
			   (background) (foreground)
			   (foreground)))
	"\
Style or attribute used to display Subject: fields in mail messages
displayed by mh-e.")


      ;; Select V3 or V4 button behaviour
      (if tek-highlight-use-attributes
	  (progn
	    ;; Do things the old way - using attributes.
      
	    (defvar tek-mh-e-from-style tek-mh-e-from-styleorattribute
	      "\
Style used for displaying From: fields in mail messages displayed
by mh-e when attributes are used to mark buttons.")
	    
	    ;; Modify the variable used with add-button to be an attribute
	    (setq tek-mh-e-from-styleorattribute (reserve-attribute))
	    
	    ;;Bind the from-style to the from-attribute
	    (set-attribute-style tek-mh-e-from-styleorattribute
				 tek-mh-e-from-style)

	    (defvar tek-mh-e-subject-style tek-mh-e-subject-styleorattribute
	      "\
Style used for displaying Subject: fields in mail messages displayed
by mh-e when attributes are used to mark buttons.")

	    ;; Modify the variable used with add-button to be an attribute
	    (setq tek-mh-e-subject-styleorattribute (reserve-attribute))

	    ;;Bind the subject-style to the subject-attribute
	    (set-attribute-style tek-mh-e-subject-styleorattribute
				 tek-mh-e-subject-style)
	    ))


      (defun tek-mh-e-highlight ()
	"\
Highlight From: and Subject: fields in mail messages displayed by
mh-e."
	(let (
	      (starting-buffer (current-buffer))
	      )
	  (set-buffer mh-show-buffer)
	  (save-excursion
	    (clear-buttons)
	    (goto-char (point-min))
	    (if (re-search-forward "^From: \\(.*\\)" nil t)
		(add-button (match-beginning 1) (match-end 1)
			    tek-mh-e-from-styleorattribute))
	    (goto-char (point-min))
	    (if (re-search-forward "^Subject: \\(.*\\)" nil t)
		(add-button (match-beginning 1) (match-end 1)
			    tek-mh-e-subject-styleorattribute))
	    )
	  (set-buffer starting-buffer)))


      ;; Set up the hook to run the highlighting function after displaying
      ;; each message.
      (postpend-unique-hook 'mh-Select-letter-hook 'tek-mh-e-highlight)
      
      )) ;; end: running-epoch test

(provide 'tek-mh-e-highlight)
