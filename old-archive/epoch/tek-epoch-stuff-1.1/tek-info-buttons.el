;*****************************************************************************
;
; Filename:	tek-info-buttons.el
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
; Original concept by:	David Carlton (carlton@linus.mitre.org or 
;			carlton@husc9.harvard.edu).
;
; Description:	Epoch support for Dave Gillespie's "Kitchen Sink" info
;		browser. Provides button highlighting and mouse bindings
;		which match those Dave provides under emacs.
;
;		Button styles may be customised by means of X11 resources.
;		The resource name to use is "info".
;		See the file tek-style-utils.el for details.
;
;		See the INSTALL file that comes with this package for
;		installation details.
;
;*****************************************************************************

; $Id: tek-info-buttons.el,v 1.5 1991/11/21 02:58:45 kwood Exp $

(require 'epoch-running)
(provide 'tek-info-buttons)

; Put the whole guts inside a test to get it to compile under emacs.
(if running-epoch
    (progn
      
      (require 'tek-style-utils)

      ; Regular expressions which should match all active areas in info
      ; pages.
      (defvar Info-header-button-regexp
	"Up:\\|Next:\\|File:\\|Prev:\\|Previous:"
	"Regexp used when searching the header for Info buttons to highlight.")

      (defvar Info-button-regexp
	(format "\\*%s[ \n][^:]+::?\\|^\\* [^:]+::?" Info-footnote-tag)
	"\
Regexp used when searching for Info buttons to highlight outside the header.")

      ; Set up the highlighting attribute first
      
      (defvar Info-button-foreground "blue"
	"\
Foreground color used for info buttons if no value is defined in the
X11 resources and the display device supports color. On monochrome
screens a different font is used in place of the different color.")

      (defvar Info-button-styleorattribute
	; If the display supports multiple colors and a default color
	; is specified, define the style to use a different color.
	(if (and (> (number-of-colors) 2) Info-button-foreground)
	    (tek-build-style "info" nil nil
			     Info-button-foreground (background)
			     (background) (foreground))
	  ; Otherwise, define the style to use a different font.
	  (tek-build-style "info" nil (or tek-bold-fixed-font
					  tek-italic-bold-fixed-font
					  tek-italic-fixed-font)
			   (foreground) (background)
			   (background) (foreground)))
	"Style or attribute used to display characters in info buttons.")


      ; Select V3 or V4 button behaviour
      (if tek-highlight-use-attributes
	  (progn
	    ; Do things the old way
      
	    (defvar Info-button-style Info-button-styleorattribute
	      "\
Style used for displaying info buttons when attributes are
used to mark buttons.")

	    ; Modify the variable used with add-button to be an attribute
	    (setq Info-button-styleorattribute (reserve-attribute))

	    ;Bind the info style to the info attribute
	    (set-attribute-style Info-button-styleorattribute
				 Info-button-style)
	    ))


      (defvar Info-mouse-map (create-mouse-map mouse::global-map)
	"Mousemap for Info buttons.")


      ; Bind the mouse buttons to useful functions.
      
      (define-mouse Info-mouse-map mouse-left mouse-down
	'Info-mouse-scroll-up)
      (define-mouse Info-mouse-map mouse-middle mouse-down
	'Info-mouse-select-item)
      (define-mouse Info-mouse-map mouse-right mouse-down
	'Info-mouse-scroll-down)
      (define-mouse Info-mouse-map mouse-left mouse-shift
	'Info-mouse-next)
      (define-mouse Info-mouse-map mouse-middle mouse-shift
	'Info-mouse-last)
      (define-mouse Info-mouse-map mouse-right mouse-shift
	'Info-mouse-prev)
      (define-mouse Info-mouse-map mouse-middle mouse-control
	'Info-mouse-up)


      (defun Info-setup-mouse-map ()
	"Use the Info mouse bindings in the current buffer."
	(use-local-mouse-map Info-mouse-map))

      (defun Info-setup-buttons ()
	"Setup all buttons in an info-node."
	(clear-buttons)
	(save-excursion
	  ; Set up header buttons.
	  (goto-char (point-min))
	  (forward-line 1)
	  (let ((line2-start (point)))
	    (goto-char (point-min))
	    ; Search for header buttons will be bound by the start
	    ; of the second line.
	    (while (re-search-forward Info-header-button-regexp line2-start t)
	      (add-button (match-beginning 0) (match-end 0)
			  Info-button-styleorattribute)))
	  ; Setup menu and cross-reference buttons. Point should already
	  ; be at the start of the second line in the buffer.
	  (while (re-search-forward Info-button-regexp nil t)
	    (if (not (string-equal
		      (buffer-substring (match-beginning 0)
					(match-end 0))
		      "* Menu:"))
		(add-button (match-beginning 0) (match-end 0)
			    Info-button-styleorattribute)))))

      
      (defun Info-mouse-select-item (mouse-data)
	"Select the info node at the mouse cursor."
	(let ((orig-window (selected-window)))
	  (select-window (nth 2 mouse-data))
	  (Info-follow-nearest-node (car mouse-data))
	  (select-window orig-window)))
      
      (defun Info-mouse-scroll-up (mouse-data)
	(let ((orig-window (selected-window)))
	  (select-window (nth 2 mouse-data))
	  (scroll-up nil)
	  (select-window orig-window)))
      
      (defun Info-mouse-scroll-down (mouse-data)
	(let ((orig-window (selected-window)))
	  (select-window (nth 2 mouse-data))
	  (scroll-down nil)
	  (select-window orig-window)))
      
      (defun Info-mouse-prev (mouse-data)
	(let ((orig-window (selected-window)))
	  (select-window (nth 2 mouse-data))
	  (Info-prev)
	  (select-window orig-window)))
      
      (defun Info-mouse-next (mouse-data)
	(let ((orig-window (selected-window)))
	  (select-window (nth 2 mouse-data))
	  (Info-next)
 	  (select-window orig-window)))
     
      (defun Info-mouse-up (mouse-data)
	(let ((orig-window (selected-window)))
	  (select-window (nth 2 mouse-data))
	  (Info-up)
 	  (select-window orig-window)))
     
      (defun Info-mouse-last (mouse-data)
	(let ((orig-window (selected-window)))
	  (select-window (nth 2 mouse-data))
	  (Info-last)
	  (select-window orig-window)))
      
      )) ; end: running-epoch test
