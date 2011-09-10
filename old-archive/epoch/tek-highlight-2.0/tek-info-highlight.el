;;*****************************************************************************
;;
;; Filename:	tek-info-highlight.el
;;
;; Copyright (C) 1992  Rod Whitby
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation;; either version 1, or (at your option)
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
;; Original concept by:	David Carlton (carlton@linus.mitre.org or 
;;			carlton@husc9.harvard.edu).
;;
;; Description:	Epoch highlighting support for Dave Gillespie's "info-dg"
;;		info browser.
;;
;;		Button styles may be customised by means of X11 resources.
;;		The resource name to use is "info".
;;		See the file tek-style-utils.el for details.
;;
;;		See the INSTALL file that comes with this package for
;;		installation details.
;;
;;*****************************************************************************

;; $Id: tek-info-highlight.el,v 1.7 1992/08/18 04:13:36 rwhitby Rel $

;; Put the whole guts inside a test to get it to compile under emacs.
(if (boundp 'epoch::version)
    (progn
      
      (require 'tek-style-utils)

      ;; Regular expressions which should match all active areas in info
      ;; pages.
      (defvar tek-info-header-zone-regexp
	"Up:\\|Next:\\|File:\\|Prev:\\|Previous:"
	"Regexp used when searching the header for Info zones to highlight.")

      (defvar tek-info-zone-regexp
	(format "\\*%s[ \n][^:]+::?\\|^\\* [^:]+::?" Info-footnote-tag)
	"\
Regexp used when searching for Info zones to highlight outside the header.")

      ;; Set up the highlighting attribute first
      
      (defvar tek-info-zone-foreground "blue"
	"\
Foreground color used for info zones if no value is defined in the
X11 resources and the display device supports color. On monochrome
screens a different font is used in place of the different color.")

      (defvar tek-info-zone-styleorattribute
	;; If the display supports multiple colors and a default color
	;; is specified, define the style to use a different color.
	(if (and (> (number-of-colors) 2) tek-info-zone-foreground)
	    (tek-build-style "info" nil nil
			     tek-info-zone-foreground (background)
			     (background) (foreground))
	  ;; Otherwise, define the style to use a different font.
	  (tek-build-style "info" nil (or tek-bold-fixed-font
					  tek-italic-bold-fixed-font
					  tek-italic-fixed-font)
			   (foreground) (background)
			   (background) (foreground)))
	"Style or attribute used to display characters in info zones.")


      ;; Select V3 or V4 zone behaviour
      (if tek-highlight-use-attributes
	  (progn
	    ;; Do things the old way
      
	    (defvar tek-info-zone-style tek-info-zone-styleorattribute
	      "\
Style used for displaying info zones when attributes are
used to mark zones.")

	    ;; Modify the variable used with add-zone to be an attribute
	    (setq tek-info-zone-styleorattribute (reserve-attribute))

	    ;;Bind the info style to the info attribute
	    (set-attribute-style tek-info-zone-styleorattribute
				 tek-info-zone-style)
	    ))


      (defun tek-info-highlight ()
	"Setup all zones in an info-node."
	(clear-zones)
	(save-excursion
	  ;; Set up header zones.
	  (goto-char (point-min))
	  (forward-line 1)
	  (let ((line2-start (point)))
	    (goto-char (point-min))
	    ;; Search for header zones will be bound by the start
	    ;; of the second line.
	    (while (re-search-forward tek-info-header-zone-regexp
				      line2-start t)
	      (add-zone (match-beginning 0) (match-end 0)
			  tek-info-zone-styleorattribute)))
	  ;; Setup menu and cross-reference zones. Point should already
	  ;; be at the start of the second line in the buffer.
	  (while (re-search-forward tek-info-zone-regexp nil t)
	    (if (not (string-equal
		      (buffer-substring (match-beginning 0)
					(match-end 0))
		      "* Menu:"))
		(add-zone (match-beginning 0) (match-end 0)
			    tek-info-zone-styleorattribute)))))

      
      )) ;; end: running-epoch test

(provide 'tek-info-highlight)
