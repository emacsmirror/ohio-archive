;;*****************************************************************************
;;
;; Filename:	tek-style-utils.el
;;
;; Copyright (C) 1992  Rod Whitby
;;
;; This program is free software;; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation;; either version 1, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY;; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program;; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; Modified by:		Rod Whitby, <rwhitby@research.canon.oz.au>
;; Author:		Ken Wood, <kwood@austek.oz.au>
;;
;; Description:	Set up some default fonts for highlighting etc, and
;;		define a function for building styles from default
;;		option values and X11 resource defaults.
;;
;; IMPORTANT:	Check that the variable tek-highlight-use-attributes is
;;		set correctly (see below) before attempting to use this
;;		package.
;;
;; When loaded, this package attempts to define 3 non-proportional fonts
;; which match the default minibuffer font in with and height: a non-bold
;; italic font, a bold italic font and a bold upright font.
;; 
;; A function for defining and modifying attribute styles based on X
;; resources is also provided for use by the other packages. You may
;; modify the these styles by setting X11 resources (usually in your
;; .Xdefaults file). Up to 9 options may be specified for each style.
;; Sensible defaults are used for any options that you do not specify.
;; The options are represented by the following X11 resources:
;; 
;; 	Emacs.STYLE-NAME.style-font: Font.
;; 
;; 	Emacs.STYLE-NAME.style-foreground: Foreground color.
;; 
;; 	Emacs.STYLE-NAME.style-background: Background color.
;; 
;; 	Emacs.STYLE-NAME.style-cursor-foreground: Cursor foreground color.
;; 
;; 	Emacs.STYLE-NAME.style-cursor-background: Cursor background color.
;; 
;; 	Emacs.STYLE-NAME.style-underline: Underline color.
;; 
;; 	Emacs.STYLE-NAME.style-stipple: Stipple pattern.
;; 
;; 	Emacs.STYLE-NAME.style-cursor-stipple: Stipple pattern.
;; 
;; 	Emacs.STYLE-NAME.style-background-stipple: Stipple pattern.
;; 
;; 	Emacs.STYLE-NAME.style-pixmap: Pixmap for graphic zones.
;; 
;; The values of STYLE-NAME currently used are:
;; 
;; 	motion: Mouse drag regions.
;; 
;; 	src-comment: Comments in source code.
;; 
;; 	info: Info browser buttons.
;; 
;; 	VM-from: From: lines in VM.
;; 
;; 	VM-subject: Subject: lines in VM.
;; 
;; 	gnus-from: From: lines in GNUS.
;; 
;; 	gnus-subject: Subject: lines in GNUS.
;; 
;; 	mh-e-from: From: lines in mh-e.
;; 
;; 	mh-e-subject: Subject: lines in mh-e.
;; 
;; 	manual-seealso: "See Also" sections in man pages.
;; 
;; 	manual-usersupplied: User supplied options in man pages.
;; 
;; 	manual-heading: Fixed options in man pages. These may be shown
;; 	in the default font depending on your implementation of the
;; 	"man" program.
;; 
;; The above X11 resources must be loaded before you start up epoch, in
;; order for them to take effect.
;;
;; Note: the stipple options have not been tested and I don't really understand
;; stipple patterns, so there may be some problems here.
;; 
;;*****************************************************************************

;; $Id: tek-style-utils.el,v 1.9 1992/08/18 04:14:26 rwhitby Rel $

(provide 'tek-style-utils)

(defvar tek-highlight-use-attributes
  (string-match "^Epoch 3" epoch::version)
  "\
If non-nil, then do highlighting as for Epoch version 3 - using
attributes with the style attached to the attribute. Otherwise do it
as for Epoch version 4 - using the style directly.")

;; Select appropriate functions for adding and clearing buttons/zones
(if tek-highlight-use-attributes
    (progn
      (fset 'add-zone 'add-button)
      (fset 'clear-zones 'clear-buttons)))

(defvar tek-default-font nil
  "\
Default font used in the minibuffer. This font is the basis for a
number of special fonts used for highlighting in various places.")

(defvar tek-italic-fixed-font nil
  "\
A non-bold italic or oblique fixed-width font, as similar as possible
to the default minibuffer font, or nil if there are no such fonts.")

(defvar tek-italic-bold-fixed-font nil
  "\
A bold italic or oblique fixed-width font, as similar as possible to
the default minibuffer font, or nil if there is no such font.")

(defvar tek-bold-fixed-font nil
  "\
A bold roman (upright) fixed-width font, as similar as possible to the
default minibuffer font, or nil if there is no such font.")

(defvar tek-stipple '(32 2 "\125\125\125\125\252\252\252\252")
  "\
A default stipple pattern, designed to \"grey-out\" areas.")

;;
;; Now, search for & load some commonly needed fonts.
;;
(let (
      ;; Variables to hold default font information
      default-font-info
      default-font-name
      default-font-height
      default-font-width
      ;; String specifying the required font width.
      font-width
      ;;
      ;; Variables to hold the possible values of style attributes to try
      ;;
      height-try-list
      ;; List of font weights to try for bold fonts.
      (bold-weight-try-list (list "-bold" "-demibold"))
      ;; List of slant options to try for italic fonts.
      (slant-try-list (list "-i" "-o"))
      ;; List of spacing options to try.
      (spacing-try-list (list "-m" "-c"))
      )
  ;; Get details of current default font for the minibuffer
  (setq default-font-info (font nil (minibuf-screen)))
  (setq default-font-name (car default-font-info))
  (setq default-font-width (cadr default-font-info))
  (setq default-font-height (cadr (cdr default-font-info)))
  ;; Convert the font-width to a string, since it will be used
  ;; directly. Width must be multiplied by 10 first.
  (setq font-width
	(concat "-" (int-to-string (* default-font-width 10))))
  ;; Generate a list of font height strings to try for fixed-width fonts
  ;; from the default font height.
  (setq height-try-list
	(list (concat "-" (int-to-string default-font-height))
	      (concat "-" (int-to-string (1+ default-font-height)))
	      (concat "-" (int-to-string (1- default-font-height)))))
  ;;
  ;; First of all, record the default font.
  (setq tek-default-font default-font-name)
  ;;
  ;;
  ;; Now, try to find an italic font similar to the minibuffer font.
  ;; From most preferred to least preferred, we'd like the
  ;; following options:
  ;;	1) monospaced (m), then character spacing (c).
  ;; 	2) italic (i), then oblique (o).
  ;;	3) same height, then 1 pixel taller, then 1 pixel shorter.
  (if (not tek-italic-fixed-font)
      (setq tek-italic-fixed-font
	    (let (
		  ;; Variable to hold the font name currently being tried
		  font-try-name
		  ;; Font finally selected, nil if none
		  (font-try-val nil)
		  ;; Loop variables
		  (current-height-list height-try-list)
		  current-slant-list
		  current-spacing-list
		  )
	      ;; Loop through the height options
	      (while (and (not font-try-val) current-height-list)
		;; Set up the list of slant options
		(setq current-slant-list slant-try-list)
		;; Loop through the slant options
		(while (and (not font-try-val) current-slant-list)
		  ;; Set up the list of spacing options
		  (setq current-spacing-list spacing-try-list)
		  ;; Loop through the spacing options
		  (while (and (not font-try-val) current-spacing-list)
		    (setq font-try-name
			  (concat "-*-*-medium"
				  (car current-slant-list)
				  "-*-*"
				  (car current-height-list)
				  "-*-*-*"
				  (car current-spacing-list)
				  font-width
				  "-*-*"))
		    (setq font-try-val (get-font font-try-name))
		    ;; Remove the first element from the spacing list
		    (setq current-spacing-list (cdr current-spacing-list))
		    ) ;; end spacing loop
		  ;; Remove the first element from the slant list
		  (setq current-slant-list (cdr current-slant-list))
		  ) ;; end slant loop
		;; Remove the first element from the height list
		(setq current-height-list (cdr current-height-list))
		) ;; end height try list
	      ;; Check to see if we found a suitable font and return it or nil.
	      (if font-try-val
		  font-try-name
		nil))))
  ;;
  ;; Now, try to find a bold italic font similar to the minibuffer font.
  ;; From most preferred to least preferred, we'd like the following
  ;; options:
  ;;	1) monospaced (m), then character spacing (c).
  ;;	2) italic (i), then oblique (o).
  ;;	3) bold, then demibold.
  ;;	4) same height, then 1 pixel taller, then 1 pixel shorter.
  (if (not tek-italic-bold-fixed-font)
      (setq tek-italic-bold-fixed-font
	    (let (
		  ;; Variable to hold the font name currently being tried
		  font-try-name
		  ;; Font finally selected, nil if none
		  (font-try-val nil)
		  ;; Loop variables
		  (current-height-list height-try-list)
		  current-weight-list
		  current-slant-list
		  current-spacing-list
		  )
	      ;; Loop through the height options
	      (while (and (not font-try-val) current-height-list)
		;; Set up the list of weight options
		(setq current-weight-list bold-weight-try-list)
		;; Loop through the weight options
		(while (and (not font-try-val) current-weight-list)
		  ;; Set up the list of slant options
		  (setq current-slant-list slant-try-list)
		  ;; Loop through the slant options
		  (while (and (not font-try-val) current-slant-list)
		    ;; Set up the list of spacing options
		    (setq current-spacing-list spacing-try-list)
		    ;; Loop through the spacing options
		    (while (and (not font-try-val) current-spacing-list)
		      (setq font-try-name
			    (concat "-*-*"
				    (car current-weight-list)
				    (car current-slant-list)
				    "-*-*"
				    (car current-height-list)
				    "-*-*-*"
				    (car current-spacing-list)
				    font-width
				    "-*-*"))
		      (setq font-try-val (get-font font-try-name))
		      ;; Remove the first element from the spacing list
		      (setq current-spacing-list (cdr current-spacing-list))
		      ) ;; end spacing loop
		    (setq font-try-val (get-font font-try-name))
		    ;; Remove the first element from the slant list
		    (setq current-slant-list (cdr current-slant-list))
		    ) ;; end slant loop
		  ;; Remove the first element from the weight list
		  (setq current-weight-list (cdr current-weight-list))
		  ) ;; end weight loop
		;; Remove the first element from the height list
		(setq current-height-list (cdr current-height-list))
		) ;; end height try list
	      ;; Check to see if we found a suitable font and return it or nil.
	      (if font-try-val
		  font-try-name
		nil))))
  ;; Now, try to find a bold fixed font similar to the minibuffer font.
  ;; From most preferred to least preferred, we'd like the following
  ;; options:
  ;;	1) monospaced (m), then character spacing (c).
  ;;	2) bold, then demibold.
  ;;	3) same height, then 1 pixel taller, then 1 pixel shorter.
  (if (not tek-bold-fixed-font)
      (setq tek-bold-fixed-font
	    (let (
		  ;; Variable to hold the font name currently being tried
		  font-try-name
		  ;; Font finally selected, nil if none
		  (font-try-val nil)
		  ;; Loop variables
		  (current-height-list height-try-list)
		  current-weight-list
		  current-spacing-list
		  )
	      ;; First of all, just try appending "bold" to the current font
	      ;; name. This will work with some aliases, such as "6x13".
	      (setq font-try-name (concat default-font-name "bold"))
	      (setq font-try-val (get-font font-try-name))
	      ;; If the above fails, loop through the normal options as usual.
	      ;; Loop through the height options
	      (while (and (not font-try-val) current-height-list)
		;; Set up the list of weight options
		(setq current-weight-list bold-weight-try-list)
		;; Loop through the weight options
		(while (and (not font-try-val) current-weight-list)
		  ;; Set up the list of spacing options
		  (setq current-spacing-list spacing-try-list)
		  ;; Loop through the spacing options
		  (while (and (not font-try-val) current-spacing-list)
		    (setq font-try-name
			  (concat "-*-*"
				  (car current-weight-list)
				  "-r-*-*"
				  (car current-height-list)
				  "-*-*-*"
				  (car current-spacing-list)
				  font-width
				  "-*-*"))
		    (setq font-try-val (get-font font-try-name))
		    ;; Remove the first element from the spacing list
		    (setq current-spacing-list (cdr current-spacing-list))
		    ) ;; end spacing loop
		  ;; Remove the first element from the weight list
		  (setq current-weight-list (cdr current-weight-list))
		  ) ;; end weight loop
		;; Remove the first element from the height list
		(setq current-height-list (cdr current-height-list))
		) ;; end height try list
	      ;; Check to see if we found a suitable font and return it or nil.
	      (if font-try-val
		  font-try-name
		nil))))
  )


;; Define the prefix to be use when looking for X resources
(defvar tek-resource-prefix "Emacs"
  "\
String to be prepended to resource names prior to looking up their
value.")

(defun tek-get-color (color)
  ;; Return X-Cardinal for COLOR, which should be an X-Cardinal or
  ;; string, including the special case "foreground" or "background"
  ;; for the current value of (foreground) and (background).
  ;; We could check what type of resource, but don't bother since this
  ;; error will be caught when we try and use the alleged color.
  (cond ((resourcep color)
	 color)
	((and (stringp color)
	      (equal "foreground" (downcase color)))
	 (foreground))
	((and (stringp color)
	      (equal "background" (downcase color)))
	 (background))
	((stringp color)
	 (get-color color))
	(t
	 nil)))

(defun tek-get-font (font &optional offset)
  ;; Return X-Font for FONT, which should be an X-Font, or a string containing
  ;; an opaque font specification or a string containing a font name.
  (cond ((resourcep font)
	 font)
	((and (stringp font)
	      (string-match "^opaque-\\([0-9]+\\)x\\([0-9]+\\)\\+\\([0-9]+\\)"
			    font))
 	 (epoch::define-opaque-font
	  font
	  (string-to-int (substring font (match-beginning 2) (match-end 2)))
	  (string-to-int (substring font (match-beginning 1) (match-end 1)))
	  (string-to-int (substring font (match-beginning 3) (match-end 3)))
	  ))
	((stringp font)
	 (get-font font))
	(t
	 nil)))

(defun tek-get-bitmap (bitmap)
  ;; Return X-Bitmap for BITMAP, which should be an X-Bitmap, or a string
  ;; containing the path of a bitmap file, or list
  ;; (WIDTH HEIGHT STRING).
  (cond ((resourcep bitmap)
	 bitmap)
	((stringp bitmap)
	 ;; (epoch::read-bitmap-file pixmap)
	 nil)
	((listp bitmap)
	 (apply 'make-bitmap bitmap))
	(t
	 nil)))

(defun tek-get-pixmap (pixmap)
  ;; Return X-Pixmap for PIXMAP, which should be an X-Pixmap, or a string
  ;; containing the path of a pixmap file, or a list
  ;; (.. to be defined ..).
  (cond ((resourcep pixmap)
	 pixmap)
	((stringp pixmap)
	 (epoch::read-pixmap-file pixmap))
	((listp pixmap)
	 ;; (apply 'make-pixmap pixmap)
	 nil)
	(t
	 nil)))

;; Define a function to initialise a style
(defun tek-build-style (style-name
			&optional package-style
			&optional default-style-font
			&optional default-style-foreground
			&optional default-style-background
			&optional default-style-cursor-foreground
			&optional default-style-cursor-background
			&optional default-style-underline
			&optional default-style-stipple
			&optional default-style-cursor-stipple
			&optional default-style-background-stipple
			&optional default-style-pixmap)
  "\
Return a style object, with values set as specified by the X11
resources for STYLE-NAME, a string. If optional PACKAGE-STYLE is not
given or nil, then create a new style.

There are 9 style fields which may be specified for the package: font,
foreground, background, cursor-foreground, cursor-background,
underline, stipple, cursor-stipple and background-stipple. The value
of `tek-resource-prefix' is prepended to the package name,
before looking up the value of the resource. Thus, the possible
resources that may be specified are:

RESOURCE.STYLE-NAME.style-font
RESOURCE.STYLE-NAME.style-foreground
RESOURCE.STYLE-NAME.style-background
RESOURCE.STYLE-NAME.style-cursor-foreground
RESOURCE.STYLE-NAME.style-cursor-background
RESOURCE.STYLE-NAME.style-underline
RESOURCE.STYLE-NAME.style-stipple
RESOURCE.STYLE-NAME.style-cursor-stipple
RESOURCE.STYLE-NAME.style-background-stipple
RESOURCE.STYLE-NAME.style-pixmap

See make-style and the style-* and set-style-* functions for details
of style options.

This function also takes 9 optional arguments: FONT, FOREGROUND,
BACKGROUND, CURSOR-FOREGROUND, CURSOR-BACKGROUND, UNDERLINE, STIPPLE,
CURSOR-STIPPLE and BACKGROUND-STIPPLE. These are the default values to
use no X11 resources for the package are defined. If any X11 resources
are not defined, the default value will be used to specify that style
option.

If any style option is not specified by either X11 resource and the
corresponding default value is not specified or is nil, then that
style field will not be set."
  (let (
	;; Use a local variable for the package style.
	(new-package-style package-style)
	;; Names of the specified X11 resources.
	(font-resource-name
	 (concat tek-resource-prefix "."
		 style-name ".style-font"))
	(foreground-resource-name
	 (concat tek-resource-prefix "."
		 style-name ".style-foreground"))
	(background-resource-name
	 (concat tek-resource-prefix "."
		 style-name ".style-background"))
	(cursor-foreground-resource-name
	 (concat tek-resource-prefix "."
		 style-name ".style-cursor-foreground"))
	(cursor-background-resource-name
	 (concat tek-resource-prefix "."
		 style-name ".style-cursor-background"))
	(underline-resource-name
	 (concat tek-resource-prefix "."
		 style-name ".style-underline"))
	(stipple-resource-name
	 (concat tek-resource-prefix "."
		 style-name ".style-stipple"))
	(cursor-stipple-resource-name
	 (concat tek-resource-prefix "."
		 style-name ".style-cursor-stipple"))
	(background-stipple-resource-name
	 (concat tek-resource-prefix "."
		 style-name ".style-backgound-stipple"))
	(pixmap-resource-name
	 (concat tek-resource-prefix "."
		 style-name ".style-pixmap"))
	;; Calculated style field values.
	(package-font nil)
	(package-foreground nil)
	(package-background nil)
	(package-cursor-foreground nil)
	(package-cursor-background nil)
	(package-underline nil)
	(package-stipple nil)
	(package-cursor-stipple nil)
	(package-background-stipple nil)
	(package-pixmap nil)
	)
    ;; Attempt to find the required settings in the X11 resources.
    (setq package-font
	  (get-default font-resource-name))
    (setq package-foreground
	  (get-default foreground-resource-name))
    (setq package-background
	  (get-default background-resource-name))
    (setq package-cursor-foreground
	  (get-default cursor-foreground-resource-name))
    (setq package-cursor-background
	  (get-default cursor-background-resource-name))
    (setq package-underline
	  (get-default underline-resource-name))
    (setq package-stipple
	  (get-default stipple-resource-name))
    (setq package-cursor-stipple
	  (get-default cursor-stipple-resource-name))
    (setq package-background-stipple
	  (get-default background-stipple-resource-name))
    (setq package-pixmap
	  (get-default pixmap-resource-name))
    ;; If any of the X11 resources were not defined, use the defaults.
    (if (not package-font)
	(setq package-font default-style-font))
    (if (not package-foreground)
	(setq package-foreground default-style-foreground))
    (if (not package-background)
	(setq package-background default-style-background))
    (if (not package-cursor-foreground)
	(setq package-cursor-foreground default-style-cursor-foreground))
    (if (not package-cursor-background)
	(setq package-cursor-background default-style-cursor-background))
    (if (not package-underline)
	(setq package-underline default-style-underline))
    (if (not package-stipple)
	(setq package-stipple default-style-stipple))
    (if (not package-cursor-stipple)
	(setq package-cursor-stipple default-style-cursor-stipple))
    (if (not package-background-stipple)
	(setq package-background-stipple default-style-background-stipple))
    (if (not package-pixmap)
	(setq package-pixmap default-style-pixmap))
    ;; Create a new style if necessary.
    (if (not new-package-style)
	(setq new-package-style (make-style)))
    ;; Set each of the style fields if a value is defined for it.
    (if package-font
	(set-style-font new-package-style
			(tek-get-font package-font)))
    (if package-foreground
	(set-style-foreground new-package-style
			      (tek-get-color package-foreground)))
    (if package-background
	(set-style-background new-package-style
			      (tek-get-color package-background)))
    (if package-cursor-foreground
	(set-style-cursor-foreground new-package-style
				     (tek-get-color
				      package-cursor-foreground)))
    (if package-background
	(set-style-background new-package-style
			      (tek-get-color package-background)))
    (if package-underline
	(set-style-underline new-package-style
			     (tek-get-color package-underline)))
    (if package-stipple
	(set-style-stipple new-package-style
			   (tek-get-bitmap package-stipple)))
    (if package-cursor-stipple
	(set-style-cursor-stipple new-package-style
				  (tek-get-bitmap
				   package-cursor-stipple)))
    (if package-background-stipple
	(set-style-background-stipple new-package-style
				      (tek-get-bitmap
				       package-background-stipple)))
    (if package-pixmap
	(set-style-pixmap new-package-style
			  (tek-get-pixmap package-pixmap)))
    ;; return the style
    new-package-style
    ))
