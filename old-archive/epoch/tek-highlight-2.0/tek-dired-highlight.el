;; $Id: tek-dired-highlight.el,v 1.4 1992/08/18 04:13:24 rwhitby Rel $ 
;; $File: ~elib/tek/tek-dired-highlight.el $ 

;; Copyright (C) 1992 Rod Whitby <rwhitby@research.canon.oz.au>

;; A significantly modified version of dired-x11.el which was distributed
;; under the following copyright notice:

;; Copyright (C) 1991 Tim Wilson and Sebastian Kremer
;; Tim.Wilson@cl.cam.ac.uk
;; Sebastian Kremer <sk@thp.uni-koeln.de>

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
;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to the above address) or from
;; Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; OVERVIEW ===========================================================

;; Alter the appearance (e.g. color) of directories, symlinks,
;; executables, sockets, setuid/setgid files, backup files,
;; auto-save files and files that match certain regexps.

;; INSTALLATION =======================================================
;;
;; To install, add the following to your `dired-load-hook':
;;
;;	   (if (boundp 'epoch::version)
;;		(load "tek-dired-highlight"))
;;
;; It is recommended to load tek-dired-highlight after dired-x because then
;; your settings for to be omitted files will be used for the set of
;; `boring' files (see below).

;; Note that you need Epoch 3.2 or later.  These functions do not work
;; with Epoch 3.1.  They are known to work with Epoch 3.2 and 
;; Epoch 4.0 Beta patchlevel 0.

;; This package will not work with standard (e.g. 18.57) Dired, you
;; need Sebastian Kremer's Tree Dired, available for ftp from
;;
;;     ftp.cs.buffalo.edu:pub/Emacs/diredall.tar.Z
;;
;; or
;;
;;     ftp.thp.uni-koeln.de[134.95.64.1]:/pub/gnu/emacs/diredall.tar.Z

;; CUSTOMIZATION ======================================================

;; Backup files, auto-save files, directories, symbolic links, executables,
;; setuid and setgid files, sockets and files that match certain regexps
;; are distinguished from other types of file (which appear in the default
;; font, color, etc).

;; Highlighting styles may be customised by means of X11 resources.
;; The resource names to use are the elements of the list contained in the
;; variable `tek-dired-all-style-types'.
;; See the file tek-style-utils.el for details.

(if (boundp 'epoch::version)
    (progn
      
      (require 'tek-style-utils)
      (require 'unique-hooks)
      
      (require 'dired)

      (defvar tek-dired-re-boring (if (fboundp 'dired-omit-regexp)
				      (dired-omit-regexp)
				    nil) "\
Regexp to match boring files.")
      
      (defvar tek-dired-boring-foreground "grey" "\
Foreground color used to highlight \"boring\" files in dired if no value is
defined in the X11 resources and the display device supports color.")
      
      (defvar tek-dired-boring-mono '(stipple) "\
Effect used to highlight \"boring\" files in dired if no value is
defined in the X11 resources and the display device does not support color.")
      
      (defvar tek-dired-boring-icon-pixmap nil "\
Pixmap used for the icon for \"boring\" files in dired if no value is
defined in the X11 resources and the display device supports color.")
      
      (defvar tek-dired-boring-icon-font nil "\
Font used for the icon for \"boring\" files in dired if no value is
defined in the X11 resources and the display device supports color.")
      
      (defvar tek-dired-boring-icon-bitmap nil "\
Bitmap used for the icon for \"boring\" files in dired if no value is
defined in the X11 resources and the display device does not support color.")
      
      (defvar tek-dired-auto-save-foreground "grey" "\
Foreground color used to highlight auto-save files in dired if no value is
defined in the X11 resources and the display device supports color.")
      
      (defvar tek-dired-auto-save-mono '(stipple) "\
Effect used to highlight auto-save files in dired if no value is
defined in the X11 resources and the display device does not support color.")
      
      (defvar tek-dired-auto-save-icon-pixmap nil "\
Pixmap used for the icon for auto-save files in dired if no value is
defined in the X11 resources and the display device supports color.")
      
      (defvar tek-dired-auto-save-icon-font nil "\
Font used for the icon for auto-save files in dired if no value is
defined in the X11 resources and the display device supports color.")
      
      (defvar tek-dired-auto-save-icon-bitmap nil "\
Bitmap used for the icon for auto-save files in dired if no value is
defined in the X11 resources and the display device does not support color.")
      
      (defvar tek-dired-backup-foreground "grey" "\
Foreground color used to highlight backup files in dired if no value is
defined in the X11 resources and the display device supports color.")
      
      (defvar tek-dired-backup-mono '(stipple) "\
Effect used to highlight backup files in dired if no value is
defined in the X11 resources and the display device does not support color.")
      
      (defvar tek-dired-backup-icon-pixmap nil "\
Pixmap used for the icon for backup files in dired if no value is
defined in the X11 resources and the display device supports color.")
      
      (defvar tek-dired-backup-icon-font nil "\
Font used for the icon for backup files in dired if no value is
defined in the X11 resources and the display device supports color.")
      
      (defvar tek-dired-backup-icon-bitmap nil "\
Bitmap used for the icon for backup files in dired if no value is
defined in the X11 resources and the display device does not support color.")
      
      (defvar tek-dired-directory-foreground "red3" "\
Foreground color used to highlight directories in dired if no value is
defined in the X11 resources and the display device supports color.")
      
      (defvar tek-dired-directory-mono '(underline) "\
Effect used to highlight directories in dired if no value is
defined in the X11 resources and the display device does not support color.")
      
      (defvar tek-dired-directory-icon-pixmap nil "\
Pixmap used for the icon for directories in dired if no value is
defined in the X11 resources and the display device supports color.")
      
      (defvar tek-dired-directory-icon-font nil "\
Font used for the icon for directories in dired if no value is
defined in the X11 resources and the display device supports color.")
      
      (defvar tek-dired-directory-icon-bitmap nil "\
Bitmap used for the icon for directories in dired if no value is
defined in the X11 resources and the display device does not support color.")
      
      (defvar tek-dired-executable-foreground "green3" "\
Foreground color used to highlight executable files in dired if no value is
defined in the X11 resources and the display device supports color.")
      
      (defvar tek-dired-executable-mono '(font) "\
Effect used to highlight executable files in dired if no value is
defined in the X11 resources and the display device does not support color.")
      
      (defvar tek-dired-executable-icon-pixmap nil "\
Pixmap used for the icon for executable files in dired if no value is
defined in the X11 resources and the display device supports color.")
      
      (defvar tek-dired-executable-icon-font nil "\
Font used for the icon for executable files in dired if no value is
defined in the X11 resources and the display device supports color.")
      
      (defvar tek-dired-executable-icon-bitmap nil "\
Bitmap used for the icon for executable files in dired if no value is
defined in the X11 resources and the display device does not support color.")
      
      (defvar tek-dired-setuid-foreground "red3" "\
Foreground color used to highlight setuid files in dired if no value is
defined in the X11 resources and the display device supports color.")
      
      (defvar tek-dired-setuid-mono nil "\
Effect used to highlight setuid files in dired if no value is
defined in the X11 resources and the display device does not support color.")
      
      (defvar tek-dired-setuid-icon-pixmap nil "\
Pixmap used for the icon for setuid files in dired if no value is
defined in the X11 resources and the display device supports color.")
      
      (defvar tek-dired-setuid-icon-font nil "\
Font used for the icon for setuid files in dired if no value is
defined in the X11 resources and the display device supports color.")
      
      (defvar tek-dired-setuid-icon-bitmap nil "\
Bitmap used for the icon for setuid files in dired if no value is
defined in the X11 resources and the display device does not support color.")
      
      (defvar tek-dired-socket-foreground "gold3" "\
Foreground color used to highlight sockets in dired if no value is
defined in the X11 resources and the display device supports color.")
      
      (defvar tek-dired-socket-mono nil "\
Effect used to highlight sockets in dired if no value is
defined in the X11 resources and the display device does not support color.")
      
      (defvar tek-dired-socket-icon-pixmap nil "\
Pixmap used for the icon for sockets in dired if no value is
defined in the X11 resources and the display device supports color.")
      
      (defvar tek-dired-socket-icon-font nil "\
Font used for the icon for sockets in dired if no value is
defined in the X11 resources and the display device supports color.")
      
      (defvar tek-dired-socket-icon-bitmap nil "\
Bitmap used for the icon for sockets in dired if no value is
defined in the X11 resources and the display device does not support color.")
      
      (defvar tek-dired-symlink-foreground "blue3" "\
Foreground color used to highlight symbolic links in dired if no value is
defined in the X11 resources and the display device supports color.")
      
      (defvar tek-dired-symlink-mono '(font) "\
Effect used to highlight symbolic links in dired if no value is
defined in the X11 resources and the display device does not support color.")
      
      (defvar tek-dired-symlink-icon-pixmap nil "\
Pixmap used for the icon for symbolic links in dired if no value is
defined in the X11 resources and the display device supports color.")
      
      (defvar tek-dired-symlink-icon-font nil "\
Font used for the icon for symbolic links in dired if no value is
defined in the X11 resources and the display device supports color.")
      
      (defvar tek-dired-symlink-icon-bitmap nil "\
Bitmap used for the icon for symbolic links in dired if no value is
defined in the X11 resources and the display device does not support color.")
      
      ;; If you need more elaborate customization, use function
      ;; tek-dired-edit-file-type-style and save the setting afterwards.
      
      (defvar tek-dired-highlight-threshold (* 100 1024) "\
If non-nil, a buffer size threshold (in bytes) above which
highlighting will not take place (because it would be too slow).")
      
;;; End of customization
      
;;; Install ourselves in the right hooks:
      
      ;; If dired-x.el is also loaded, arrange it so that highlighting will
      ;; be done after omitting uninteresting files, thus saving time:
      
      (postpend-unique-hook 'dired-after-readin-hook 'tek-dired-highlight)
      
      
;;; Handling the gory X11 details
      
      (defvar tek-dired-color (> (number-of-colors) 2) "\
Whether we have a color display.")
      
;;; File attribute types

      (defconst tek-dired-attribute-types
	'(auto-save backup directory executable setuid socket symlink) "\
List of all types of files that Dired will highlight according to the file
attributes.

The attribute types are represented by the following symbols:

    auto-save   - auto-save files
    backup      - backup files
    directory	- directories
    executable	- executable plain files
    setuid	- setuid or setgid plain files
    socket	- sockets in the file system
    symlink	- symbolic links
")
      
;;; File regexp types

      (defvar tek-dired-regexp-types
	'(boring) "\
List of all types of files that Dired will highlight according to the file
name.

The regexp types are represented by the following symbols:

    boring	- boring files (those that match `tek-dired-re-boring')
")
      
      (defvar tek-dired-regexp-alist
	(list
	 (cons 'boring tek-dired-re-boring)
	 ) "\
Alist describing file regexp types and their regexps in Dired.
Each element looks like

   \(REGEXP-TYPE REGEXP\)

REGEXP-TYPE is one of the symbols in the variable
`tek-dired-file-regexp-types', e.g. `boring'.

See also function `tek-dired-edit-regexp' for advanced customization.
")
  
      ;; Access functions

      (defun tek-dired-get-regexp-elt (regexp-type)
	;; Get the element whose car is REGEXP-TYPE
	;; (e.g. `boring').
	;; Its second element (`cdr') is REGEXP-TYPE's regexp.
	(assq regexp-type tek-dired-regexp-alist))
      
      (defun tek-dired-get-regexp (regexp-type)
	;; Get the regexp for the regexp type REGEXP-TYPE
	;; (e.g. `boring').
	(cdr (tek-dired-get-regexp-elt regexp-type)))
      
      
;;; All file types

      (defconst tek-dired-all-types
	(append tek-dired-attribute-types
		tek-dired-regexp-types) "\
List of all types of files that Dired will highlight.
See `tek-dired-attribute-types' and `tek-dired-regexp-types'.
")
      
;;; Highlight style types

      (defun tek-dired-highlight-type (file-type)
	;; Convert a file type into a highlight style.
	(intern (concat (symbol-name file-type) "-highlight")))
      
      (defconst tek-dired-all-highlight-types
	(mapcar 'tek-dired-highlight-type tek-dired-all-types) "\
List of all style types that Dired will use to highlight files.")
      
;;; Icon style types

      (defun tek-dired-icon-type (file-type)
	;; Convert a file type into an icon style type.
	(intern (concat (symbol-name file-type) "-icon")))
      
      (defconst tek-dired-all-icon-types
	(mapcar 'tek-dired-icon-type tek-dired-all-types) "\
List of all style types that Dired will use to display icons for files.")
      
;;; All style types

      (defconst tek-dired-all-style-types
	(append tek-dired-all-highlight-types
		tek-dired-all-icon-types) "\
List of all style types that Dired will use to highlight files and
display icons.")
      
;;; There's no reason why these effects shouldn't be used for
;;; color too -- but with all those lovely colors, who would want
;;; to stipple or underline?
      
      (defconst tek-dired-mono-effects-alist
	(list (cons 'underline "foreground")
	      (cons 'stipple tek-stipple)
	      (cons 'font  (or tek-italic-bold-fixed-font
			       tek-bold-fixed-font
			       tek-italic-fixed-font))
	      ) "\
Effects which may be selected by the tek-dired-*-mono-effect variables")
      
      (defun tek-dired-mono-effects (effects)
	;; Return an alist of style fields according the the elements of
	;; EFFECTS.  If any styles are selected (ie the result is not nil)
	;; the list also includes foreground and background colors.
	;; (This doesn't work properly if an element of EFFECTS is not
	;; a proper value.)
	(let ((style-fields
	       (mapcar
		(function
		 (lambda (x) (assq x tek-dired-mono-effects-alist)))
		effects)))
	  (if style-fields
	      (append '((foreground . "foreground")
			(background . "background"))
		      style-fields)
	    nil)))
      
      (defvar tek-dired-style-alist
	;; Rather than complicating the code later we always explicitly set
	;; the foreground and background here (the defaults are not usually
	;; suitable).
	
	;; By allowing the special ``colors'' "background" and "foreground"
	;; we achieve that tek-dired-style-alist can be set in ~/.emacs as a
	;; _constant_ list (without having to splice in the value of
	;; function foreground etc.), possibly with the help of
	;; tek-dired-edit-*-style.
	
	;; Thus, the user in his ~./emacs doesn't need to do what we do
	;; here: splicing in the values of the color customization
	;; variables.
	
	(list
	 (list 'auto-save-highlight
	       (list 'color
		     (list (cons 'foreground tek-dired-auto-save-foreground)
			   (cons 'background "background")))
	       (list 'mono
		     (tek-dired-mono-effects tek-dired-auto-save-mono)))
	 (list 'auto-save-icon
	       (list 'color
		     (list (cons 'foreground "foreground")
			   (cons 'background "background")
			   (cons 'pixmap tek-dired-auto-save-icon-pixmap)
			   (cons 'font tek-dired-auto-save-icon-font)
			   ))
	       (list 'mono
		     (list (cons 'foreground "foreground")
			   (cons 'background "background")
			   (cons 'stipple tek-dired-auto-save-icon-bitmap))))
	 (list 'backup-highlight
	       (list 'color
		     (list (cons 'foreground tek-dired-backup-foreground)
			   (cons 'background "background")))
	       (list 'mono
		     (tek-dired-mono-effects tek-dired-backup-mono)))
	 (list 'backup-icon
	       (list 'color
		     (list (cons 'foreground "foreground")
			   (cons 'background "background")
			   (cons 'pixmap tek-dired-backup-icon-pixmap)
			   (cons 'font tek-dired-backup-icon-font)
			   ))
	       (list 'mono
		     (list (cons 'foreground "foreground")
			   (cons 'background "background")
			   (cons 'stipple tek-dired-backup-icon-bitmap))))
	 (list 'directory-highlight
	       (list 'color
		     (list (cons 'foreground tek-dired-directory-foreground)
			   (cons 'background "background")))
	       (list 'mono
		     (tek-dired-mono-effects tek-dired-directory-mono)))
	 (list 'directory-icon
	       (list 'color
		     (list (cons 'foreground "foreground")
			   (cons 'background "background")
			   (cons 'pixmap tek-dired-directory-icon-pixmap)
			   (cons 'font tek-dired-directory-icon-font)
			   ))
	       (list 'mono
		     (list (cons 'foreground "foreground")
			   (cons 'background "background")
			   (cons 'stipple tek-dired-directory-icon-bitmap))))
	 (list 'executable-highlight
	       (list 'color
		     (list (cons 'foreground tek-dired-executable-foreground)
			   (cons 'background "background")))
	       (list 'mono
		     (tek-dired-mono-effects tek-dired-executable-mono)))
	 (list 'executable-icon
	       (list 'color
		     (list (cons 'foreground "foreground")
			   (cons 'background "background")
			   (cons 'pixmap tek-dired-executable-icon-pixmap)
			   (cons 'font tek-dired-executable-icon-font)
			   ))
	       (list 'mono
		     (list (cons 'foreground "foreground")
			   (cons 'background "background")
			   (cons 'stipple tek-dired-executable-icon-bitmap))))
	 (list 'setuid-highlight
	       (list 'color
		     (list (cons 'foreground tek-dired-setuid-foreground)
			   (cons 'background "background")))
	       (list 'mono
		     (tek-dired-mono-effects tek-dired-setuid-mono)))
	 (list 'setuid-icon
	       (list 'color
		     (list (cons 'foreground "foreground")
			   (cons 'background "background")
			   (cons 'pixmap tek-dired-setuid-icon-pixmap)
			   (cons 'font tek-dired-setuid-icon-font)
			   ))
	       (list 'mono
		     (list (cons 'foreground "foreground")
			   (cons 'background "background")
			   (cons 'stipple tek-dired-setuid-icon-bitmap))))
	 (list 'socket-highlight
	       (list 'color
		     (list (cons 'foreground tek-dired-socket-foreground)
			   (cons 'background "background")))
	       (list 'mono
		     (tek-dired-mono-effects tek-dired-setuid-mono)))
	 (list 'socket-icon
	       (list 'color
		     (list (cons 'foreground "foreground")
			   (cons 'background "background")
			   (cons 'pixmap tek-dired-socket-icon-pixmap)
			   (cons 'font tek-dired-socket-icon-font)
			   ))
	       (list 'mono
		     (list (cons 'foreground "foreground")
			   (cons 'background "background")
			   (cons 'stipple tek-dired-socket-icon-bitmap))))
	 (list 'symlink-highlight
	       (list 'color
		     (list (cons 'foreground tek-dired-symlink-foreground)
			   (cons 'background "background")))
	       (list 'mono
		     (tek-dired-mono-effects tek-dired-symlink-mono)))
	 (list 'symlink-icon
	       (list 'color
		     (list (cons 'foreground "foreground")
			   (cons 'background "background")
			   (cons 'pixmap tek-dired-symlink-icon-pixmap)
			   (cons 'font tek-dired-symlink-icon-font)
			   ))
	       (list 'mono
		     (list (cons 'foreground "foreground")
			   (cons 'background "background")
			   (cons 'stipple tek-dired-symlink-icon-bitmap))))
	 (list 'boring-highlight
	       (list 'color
		     (list (cons 'foreground tek-dired-boring-foreground)
			   (cons 'background "background")))
	       (list 'mono
		     (tek-dired-mono-effects tek-dired-boring-mono)))
	 (list 'boring-icon
	       (list 'color
		     (list (cons 'foreground "foreground")
			   (cons 'background "background")
			   (cons 'pixmap tek-dired-boring-icon-pixmap)
			   (cons 'font tek-dired-boring-icon-font)
			   ))
	       (list 'mono
		     (list (cons 'foreground "foreground")
			   (cons 'background "background")
			   (cons 'stipple tek-dired-boring-icon-bitmap))))
	 ) "\
Alist describing style types and their styles in Dired.
Each element looks like

   \(STYLE-TYPE (color ((STYLE-FIELD1 . VALUE1)
                        (STYLE-FIELD2 . VALUE2)
                         ...))
	        (mono  ((STYLE-FIELD1 . VALUE1)
             	        (STYLE-FIELD2 . VALUE2)
                         ...))\)

STYLE-TYPE is one of the symbols in the variable `tek-dired-all-style-types',
e.g. `directory-highlight'.

The `color' alist describes attributes used on a color display, the
optional `mono' alist those used on a monochrome display.

The possible STYLE-FIELDs (symbols, cf. function `make-style') and
VALUEs (names of colors (as string), stipple patterns etc.) are
described in `tek-dired-all-style-fields'.

See also functions `tek-dired-edit-*-style' for advanced customization.
")
      
      ;; Access functions.
      
      (defun tek-dired-get-style-alist-elt (style-type)
	;; Get the element whose car is STYLE-TYPE
	;; (e.g. `directory-highlight').
	;; Its second element (`cadr', or `nth 1') is STYLE-TYPE's style alist.
	(assq (if tek-dired-color
		  'color
		'mono)
	      (assq style-type tek-dired-style-alist)))
      
      (defun tek-dired-get-style-alist (style-type)
	;; Get the style-alist for the style type STYLE-TYPE
	;; (e.g. `directory-highlight').
	(nth 1 (tek-dired-get-style-alist-elt style-type)))
      
      
;;; Styles control the appearance of text.
;;;
;;; In Epoch 3.2:
;;;   Buttons are created with attributes.
;;;   An attribute is an index into a table of styles.
;;;   Buttons are placed in buffers with `add-button'.
;;;
;;; In Epoch 4:
;;;   Zones are created with styles
;;;   Zones are placed in buffers with `add-zone'.
      
      
      (defconst tek-dired-all-style-fields
	'(foreground
	  background
	  cursor-foreground
	  cursor-background
	  underline
	  stipple
	  cursor-stipple
	  background-stipple
	  font
	  pixmap) "\
List of all style fields known to dired.
The symbols and their meanings are:

    foreground
      The text foreground color, as a string or X-Cardinal
      representing the color.

    background
      The text background color.  You almost always want to set this
      to the special string `\"background\"', which is replaced by the
      value of `(background)' by dired.

    cursor-foreground
      The character foreground color when the text cursor is on the
      character.

    cursor-background
      The character background color when the text cursor is on the
      character.  You almost always want to set this to the string
      `\"background\"'.

    stipple
      The stipple pattern to use for the text.	This is an X-Bitmap
      resource or list (WIDTH HEIGHT STRING).

    cursor-stipple
      The stipple to use when the cursor is on the tex.

    background-stipple
      The stipple to use for the background. Bits that are set in the
      stipple are displayed in the screen background color.  Cleared
      bits are displayed in the style background color.	 See stipple.

    underline
      The color to use for underlining. The value `\"foreground\"' is
      useful.

    font
      The font for the text, as an X-Font resource or string.  The
      display will be messy unless this is a character-cell font of
      the same pixel width as the default font.

    pixmap
      The pixmap to use for the icon for the text.  This is an X-Pixmap
      resource or list (.. to be defined ..).
")
      
      
      
      ;; Functions for getting and setting colors, bitmaps, and fonts.
      
      (defun tek-dired-make-style (style-name style style-alist)
	;; Edit STYLE or make a new style, with values from alist STYLE-ALIST
	;; STYLE-ALIST is a table of style-field and value, e.g
	;; ((foreground . "Grey")
	;;  (background . #<X-Cardinal 0>))
	;; Note that the values may be raw or cooked.
	
	(if (string-match "^Epoch 4" epoch::version)
	    ;; Epoch Buttons reference styles directly in epoch 4
	    (setq style
		  (tek-build-style style-name style
				   (cdr (assq 'font style-alist))
				   (cdr (assq 'foreground style-alist))
				   (cdr (assq 'background style-alist))
				   (cdr (assq 'cursor-foreground style-alist))
				   (cdr (assq 'cursor-background style-alist))
				   (cdr (assq 'underline style-alist))
				   (cdr (assq 'stipple style-alist))
				   (cdr (assq 'cursor-stipple style-alist))
				   (cdr (assq 'background-stipple style-alist))
				   (cdr (assq 'pixmap style-alist))
				   ))
	  ;; else use highlighting as per epoch version 3 with attributes.
	  ;; As with epoch 4 but additionally allocate and return corresponding
	  ;; attribute.
	  (let ((attr (or style
			  (reserve-attribute))))
	    (setq style
		  (tek-build-style style-name (attribute-style attr)
				   (cdr (assq 'font style-alist))
				   (cdr (assq 'foreground style-alist))
				   (cdr (assq 'background style-alist))
				   (cdr (assq 'cursor-foreground style-alist))
				   (cdr (assq 'cursor-background style-alist))
				   (cdr (assq 'underline style-alist))
				   (cdr (assq 'stipple style-alist))
				   (cdr (assq 'cursor-stipple style-alist))
				   (cdr (assq 'background-stipple style-alist))
				   ;; No pixmaps in version 3
				   nil))
	    (set-attribute-style attr style)
	    attr)))
      
      (defconst tek-dired-highlight-alist
	(mapcar
	 (function
	  ;; returns e.g. `(directory . 3)' if directories are to be
	  ;; highlighted with style #3.
	  (lambda (x)
	    (let* ((x-highlight (tek-dired-highlight-type x))
		   (style-alist (tek-dired-get-style-alist x-highlight)))
	      ;; This test prevents tek-dired-highlight placing a button
	      ;; with default attributes over non-special files -- the
	      ;; default attributes are not necessarily the same as no
	      ;; attributes, so this may lead to unintentional highlighting.
	      (and style-alist
		   (cons x
			 (tek-dired-make-style
			  (concat "dired-" (symbol-name x-highlight))
			  ;; Make a new style
			  nil
			  style-alist))))))
	 tek-dired-all-types)
	"Alist with elements

    \(TYPE ATTRIBUTE)

TYPE is a symbol describing a file type, see `tek-dired-all-types'.
ATTRIBUTE describes how files of type TYPE are highlighted and
is computed at load time from `tek-dired-style-alist'.")
      
      
      (defconst tek-dired-icon-alist
	(mapcar 
	 (function
	  ;; returns e.g. `(directory . 3)' if directory icons are to be
	  ;; style #3.
	  (lambda (x)
	    (let* ((x-icon (tek-dired-icon-type x))
		   (style-alist (tek-dired-get-style-alist x-icon)))
	      ;; This test prevents tek-dired-highlight placing a button
	      ;; with default attributes over non-special files -- the
	      ;; default attributes are not necessarily the same as no
	      ;; attributes, so this may lead to unintentional highlighting.
	      (and style-alist
		   (cons x
			 (tek-dired-make-style
			  (concat "dired-" (symbol-name x-icon))
			  ;; Make a new style
			  nil
			  style-alist))))))
	 tek-dired-all-types)
	"Alist with elements

    \(TYPE ATTRIBUTE)

TYPE is a symbol describing a file icon type, see `tek-dired-all-types'.
ATTRIBUTE describes how file icons of type TYPE are displayed and
is computed at load time from `tek-dired-style-alist'.")
      
      
      ;; Interactive changing of the appearance of file types
      
      (defun tek-dired-edit-highlight-style (file-type) "\
Edit interactively the style of highlighting for files of type FILE-TYPE.
Useful to try out different colors.
See variable `tek-dired-all-style-fields' for an explanation of the
allowed fields and their meanings.

This function changes the value of `tek-dired-style-alist' to reflect
the changes.  You may want to set this variable to its new value in your
~/.emacs for future sessions if the normal customization variables don't
suffice for you."
	(interactive
	 (list (tek-dired-read-file-type
		"Change appearance of highlighting of which file type? ")))
	(let* ((style (cdr (assq file-type tek-dired-highlight-alist)))
	       (style-alist-elt
		(tek-dired-get-style-alist-elt
		 (tek-dired-highlight-type file-type)))
	       (style-alist (nth 1 style-alist-elt)))
	  (setq style-alist
		(tek-dired-read-style-alist
		 (symbol-name file-type) style-alist))
	  ;; if the alist has been enlarged we have to store it back into
	  ;; tek-dired-style-alist:
	  (setcdr style-alist-elt (list style-alist))
	  ;; Send nil in for the style-name, so X11 resources are not picked up
	  (tek-dired-make-style nil style style-alist)))
      
      (defun tek-dired-edit-icon-style (file-type) "\
Edit interactively the style of the icon for files of type FILE-TYPE.
Useful to try out different colors.
See variable `tek-dired-all-style-fields' for an explanation of the
allowed fields and their meanings.

This function changes the value of `tek-dired-style-alist' to reflect
the changes.  You may want to set this variable to its new value in your
~/.emacs for future sessions if the normal customization variables don't
suffice for you."
	(interactive
	 (list (tek-dired-read-file-type
		"Change appearance of icon of which file type? ")))
	(let* ((style (cdr (assq file-type tek-dired-icon-alist)))
	       (style-alist-elt
		(tek-dired-get-style-alist-elt
		 (tek-dired-icon-type file-type)))
	       (style-alist (nth 1 style-alist-elt)))
	  (setq style-alist
		(tek-dired-read-style-alist
		 (symbol-name file-type) style-alist))
	  ;; if the alist has been enlarged we have to store it back into
	  ;; tek-dired-style-alist:
	  (setcdr style-alist-elt (list style-alist))
	  ;; Send nil in for the style-name, so X11 resources are not picked up
	  (tek-dired-make-style nil style style-alist)))
      
      (defun tek-dired-read-style-alist (type alist)
	;; Let user edit the current fields of ALIST or add new fields.
	;; TYPE is the file-type.  It is used for prompts only.
	;; Changes ALIST destructively and returns its new value.
	;; ALIST's keys must be symbols (i.e. assq instead of assoc will be
	;; used).
	(let ((key-table
	       (tek-dired-symbol-list-to-table tek-dired-all-style-fields))
	      key-str key elt value)
	  (while (not (equal ""
			     (setq key-str
				   (completing-read
				    (concat "Edit which key of "
					    type
					    " (RET=end, ?=show): ")
				    key-table nil nil))))
	    (if (equal "?" key-str)
		(with-output-to-temp-buffer "*Dired X11 Alist*"
		  (princ (format
			  "Dired X11 appearance for files of type `%s':\n\n"
			  type))
		  (if (fboundp 'pp-to-string)	; from pp.el by Randal Schwartz
		      (princ (pp-to-string alist)) ; pretty print it
		    (prin1 alist)))
	      (setq key (intern key-str))
	      (setq value
		    (read-string (format "Set %s of %s to (current is %s): "
					 key type (cdr (assq key alist)))))
	      (if (setq elt (assq key alist))
		  ;; modify in place
		  (setcdr elt value)
		;; add a new element to alist
		(setq alist (cons (cons key value) alist)))))
	  alist))
      
      (defun tek-dired-symbol-list-to-table (list)
	;; Convert a list of symbols to a table suitable for completing-read.
	(mapcar (function (lambda (x) (list (symbol-name x))))
		list))
      
      (defun tek-dired-read-file-type (prompt)
	(intern (completing-read
		 prompt (tek-dired-symbol-list-to-table tek-dired-all-types)
		 nil t)))
      
      ;; Interactive changing of the file regexp types
      
      (defun tek-dired-edit-regexp (file-type) "\
Edit interactively the regexp for files of type FILE-TYPE.
Useful to try out different regexps.

This function changes the value of `tek-dired-regexp-alist' to reflect
the changes.  You may want to set this variable to its new value in your
~/.emacs for future sessions if the normal customization variables don't
suffice for you."
	(interactive
	 (list (tek-dired-read-regexp-type
		"Change regexp for which file type? ")))
	(let* ((regexp-elt
		(tek-dired-get-regexp-elt file-type))
	       (regexp (cdr regexp-elt)))
	  (setq regexp
		(tek-dired-read-regexp
		 (symbol-name file-type) regexp))
	  ;; if the alist has been changed we have to store it back into
	  ;; tek-dired-regexp-alist:
	  (setcdr regexp-elt regexp)))
      
      (defun tek-dired-read-regexp (type regexp)
	;; Let user edit REGEXP.
	;; TYPE is the file-type.  It is used for prompts only.
	;; Changes REGEXP destructively and returns its new value.
	(read-string (format "Set regexp of %s to (current is %s): "
			     type regexp)))
      
      (defun tek-dired-read-regexp-type (prompt)
	(intern (completing-read
		 prompt (tek-dired-symbol-list-to-table tek-dired-regexp-types)
		 nil t)))
      
;;; Regexps to match file types.
      
      ;; Not all of them are used in highlighting.
      ;; On some systems the setgid and sticky bits of directories mean
      ;; something but we don't provide regexps for them.
      
      (defvar dired-re-socket
	(concat dired-re-maybe-mark dired-re-inode-size "s"))
      
      (defvar dired-re-block-device
	(concat dired-re-maybe-mark dired-re-inode-size "b"))
      
      (defvar dired-re-character-device
	(concat dired-re-maybe-mark dired-re-inode-size "c"))
      
      (defvar dired-re-named-pipe
	(concat dired-re-maybe-mark dired-re-inode-size "p"))
      
      (defvar dired-re-setuid;; setuid plain file (even if not executable)
	(concat dired-re-maybe-mark dired-re-inode-size
		"-[-r][-w][Ss][-r][-w][sx][-r][-w][xst]"))
      
      (defvar dired-re-setgid;; setgid plain file (even if not executable)
	(concat dired-re-maybe-mark dired-re-inode-size
		"-[-r][-w][-x][-r][-w][Ss][-r][-w][xst]"))
      
      (defvar dired-re-sticky;; sticky plain file (even if not executable)
	(concat dired-re-maybe-mark dired-re-inode-size
		"-[-r][-w][-x][-r][-w]s[-r][-w][Tt]"))
      
      (defun tek-dired-assoc-regexp (pathname) "\
Find the first regexp in tek-dired-regexp-alist that matches PATHNAME and
return the regexp type."
	(interactive "FName of file: ")
	(let ((alist tek-dired-regexp-alist)
	      (regexp-type nil))
	  (let ((case-fold-search (eq system-type 'vax-vms)))
	    (while (and (not regexp-type) alist)
	      (if (string-match (cdr (car alist)) pathname)
		  (setq regexp-type (car (car alist))))
	      (setq alist (cdr alist))))
	  regexp-type))

;;; Functions to actually highlight the files
      
      ;; This is nice, but too slow to use it for highlighting:
      ;;(defun dired-map (fun)
      ;;  "Run FUN, a function of zero args,
      ;;at the beginning of each dired file line."
      ;;  (save-excursion
      ;;    (let (file buffer-read-only)
      ;;	(goto-char (point-min))
      ;;	(while (not (eobp))
      ;;	(save-excursion
      ;;	  (and (not (eolp))
      ;;	       (not (dired-between-files))
      ;;	       (progn (beginning-of-line)
      ;;		      (funcall fun))))
      ;;	(forward-line 1)))))
      
      (defun tek-dired-no-highlight-p () "\
Function to decide whether to highlight current dired buffer.
If it returns non-nil, highlighting is suppressed."
	(or
	 ;; we depend on the ls -l permission bit info for highlighting
	 (let (case-fold-search)
	   (not (string-match "l" dired-actual-switches)))
	 ;; we don't want to highlight if it would take too long
	 (and (integerp tek-dired-highlight-threshold)
	      (> (buffer-size) tek-dired-highlight-threshold))))
      
      (defun tek-dired-highlight ()
	;; Look at each file name and (if special) place a button over it
	;; with appropriate attribute.
	(if (tek-dired-no-highlight-p)
	    nil				
	  (message "Highlighting...")
	  (let (buffer-read-only beg end pathname type attr)
	    (save-excursion
	      (goto-char (point-min))
	      (while (not (eobp))
		(and (not (eolp))
		     ;;(not (dired-between-files)); not needed
		     (setq beg (dired-move-to-filename)
			   end (and beg (dired-move-to-end-of-filename t))
			   pathname (and beg end (buffer-substring beg end)))
		     ;; here if pathname non-nil
		     (progn
		       (beginning-of-line)	; for the re matches below
		       (setq type
			     (cond
			      ;; -- Does it match any regexps?
			      ((tek-dired-assoc-regexp pathname))
			      ;; -- Is it an auto-save file?
			      ((auto-save-file-name-p
				(file-name-nondirectory pathname))
			       'auto-save)
			      ;; -- Is it a backup file?
			      ((backup-file-name-p
				(file-name-nondirectory pathname))
			       'backup)
			      ;; -- Is it a directory?
			      ((looking-at dired-re-dir)
			       'directory)
			      ;; -- Is it a symbolic link?
			      ((looking-at dired-re-sym)
			       'symlink)
			      ;; Is it a setuid or setgid plain file?
			      ;; Test this before the test for being executable
			      ((or (looking-at dired-re-setuid)
				   (looking-at dired-re-setgid))
			       'setuid)
			      ;; -- Is it an executable file?
			      ((looking-at dired-re-exe)
			       'executable)
			      ;; -- Is it a socket?
			      ((looking-at dired-re-socket)
			       'socket)
			      ;; -- Else leave it alone.
			      ;; Plain file, or block or character special
			      ;; file. We don't need to draw attention to
			      ;; these.
			      ))
		       (setq attr
			     (cdr (assq type tek-dired-highlight-alist)))
		       (setq icon
			     (cdr (assq type tek-dired-icon-alist)))
		       (if attr
			   (add-zone beg end attr))
		       (if icon
			   (add-zone (1- beg) beg icon))))
		(forward-line 1))))
	  (message "Highlighting...done")))
      
      
      (run-hooks 'tek-dired-load-hook)
      
      )) ; end: running-epoch test

(provide 'tek-dired-highlight)
