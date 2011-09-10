;;; html-helper-mode.el --- Major mode for composing html files.
;;; v 3.0.2.2

;; Mantainer : Gian Uberto Lauri <lauri@eng.it>, <saint@dei.unipd.it>*
;;                                * works only from DEI, Padova.
;;             http://www.gest.unipd.it/~saint/
;; Original Author: Nelson Minar <nelson@santafe.edu>
;; Original version Maintainer: Nelson Minar <nelson@santafe.edu>
;; Changes by by: Gian Uberto Lauri <lauri@eng.it>, <saint@dei.unipd.it>*
;;                                * works only from DEI, Padova.
;; Credits : Larry Smith and Tony Graham for the ASP/PHP matching regexp
;;           prototype.
;;           Stan Lanning for the defadvice code that prevents indenting
;;           of <PRE></PRE>, for the defadvice code that leaves the cursor
;;           where it is during narrowing to script code, enhancments to
;;           the scripting narrowing
;;           Charles Curley for the commentary of tempo.el behaviour
;;           Samir Barjoud for giving me the luser cap when I didn't notice
;;           that *ALL* I needed to write  html-helper-match-asp-php was in
;;           font-lock.el.
;;           Theodore A. Jump for fixing fold tags in this source (after I
;;           broke them
;;           David J. Biesack for suggesting a good version checking.

;; URL: http://www.gest.unipd.it/~saint/html-helper-mode.el.gz

;; Created: 01 Feb 1994
;; $Id: html-helper-mode.el,v 3.0.2.2 1998/08/06 18:53:03 nelson Exp $
;; Keywords: HTML major-mode

;; LCD Archive Entry:
;; html-helper-mode|Gian Uberto Lauri|lauri@eng.it|
;; Major mode for editing HTML.|
;; 26-Oct-99|Version 3.?.?|http://www.gest.unipd.it/~saint/html-helper-mode.el.gz

;; Copyright (C) 1994 Nelson Minar
;; Copyright (C) 1995 Nelson Minar and Ulrik Dickow
;; Copyright (C) 1999 Nelson Minar, Ulrik Dickow and Gian Uberto Lauri

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Massachusettes Ave,
;; Cambridge, MA 02139, USA.

;;{{{ Commentary

;; Installation:
;;   Add this line in your .emacs:
;;     (autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
;;   To invoke html-helper-mode automatically on .html files, do this:
;;     (setq auto-mode-alist (cons '("\\.html$" . html-helper-mode) auto-mode-alist))
;;   To invoke html-helper-mode automatically on .asp files, do this:
;;     (setq auto-mode-alist (cons '("\\.asp$" . html-helper-mode) auto-mode-alist))
;;   To invoke html-helper-mode automatically on .phtml files, do this:
;;     (setq auto-mode-alist (cons '("\\.phtml$" . html-helper-mode) auto-mode-alist))

;;
;;   This mode requires another lisp file, tempo.el. This can be
;;     retrieved from ftp://ftp.lysator.liu.se/pub/emacs/tempo.el
;;   Xemacs users need to have auc-menu installed.
;;   Emacs 18 users need to have auc-menu and add-hook installed.
;;   If your OS has broken 14 character filenames
;;      this mode will also work with the name "html-mode.el".

;; Configuration:
;;   see the "user variables" section, or the documentation on configuration
;;   in http://www.santafe.edu/~nelson/tools/. There are variables you want to
;;   configure, particularly html-helper-address-string and
;;   html-helper-use-expert-menu

;; Description:
;;   html-helper-mode makes it easier to write HTML documents. This mode
;;   handles inserting HTML codes in a variety of ways (keybindings, menus,
;;   completion in the buffer). It also supports indentation, timestamps,
;;   skeletons for new documents, hilit19 patterns, and a variety of other
;;   things. For the full skinny, see the HTML documentation that comes
;;   with the package or is at http://www.santafe.edu/~nelson/tools/

;; Thank yous:
;;   David K�gedal <davidk@lysator.liu.se> for the tempo code which
;;     forms the core of the HTML insertion, as well as the HTML+ tag.
;;   Marc Hedlund <march@europa.com> for general encouragement and
;;     many helpful suggestions, especially with HTML/2.0 compliance
;;     and form design.
;;   Ulrik Dickow <dickow@nbi.dk> for the font-lock code
;;   Denis Howe <dbh@doc.ic.ac.uk> for writing browse-url.
;;   Magnus Homann <d0asta@dtek.chalmers.se> and Jamshid Afshar
;;     <jamshid@ses.com> for timestamp suggestions.
;;   Everyone who sent me a version of menus (16 in all!)
;;   Marc Andreessen <marca@mcom.com> for writing the original html-mode

;; The newest version of html-helper-mode should always be available from
;;   http://www.gest.unipd.it/~saint/hth.html

;; Changes in version 3.0.2.0

;; patched asp/php matching in font lock. It doesn't (hopefully) break the
;; stack anymore.

;; Changes in version 3.0.2.0

;; Fixed folding.el tags.
;; fixed general tag matching regexp

;; 
;; This code was writting using folding.el, a wonderful folding editor
;; minor mode for emacs. That's what the strange {{{ comments are for.

;;}}}

;;{{{ Code:

(defconst html-helper-mode-version (substring "$Revision: 3.0.2.2 $" 11 15))

;;{{{ user variables

(defgroup html-helper nil
  "Customizing html-helper-mode"
  :group 'languages
  :group 'hypermedia
  :group 'local)

(defgroup html-helper-faces nil
  "Customizing html-helper-mode custom faces"
  :group 'html-helper
  :group 'faces)

(defcustom html-helper-mode-uses-visual-basic t
  "Non nil to require visual-basic-mode"
  :type 'boolean
  :initialize 'custom-initialize-default
  :group 'html-helper
  :require 'html-helper-mode)

(defcustom html-helper-mode-uses-bold-italic nil
  "Non nil to use the bold-italic font (if your font supports it)"
  :type 'boolean
  :initialize 'custom-initialize-default
  :group 'html-helper
  :require 'html-helper-mode)

(defcustom html-helper-mode-uses-KG-style nil
  "Non nil to make Emacs consider PHP/ASP code blocks beginning in 
the first column"
  :type 'boolean
  :initialize 'custom-initialize-default
  :group 'html-helper
  :require 'html-helper-mode)

;; Visual basic mode is not in the standard distribution, so I let the user
;; override html-helper-mode-uses-visual-basic with a nil value.
(cond (html-helper-mode-uses-visual-basic (require 'visual-basic-mode)))
(require 'cc-mode)


;; Set this to be whatever signature you want on the bottom of your pages.
(defvar html-helper-address-string ""
  "*The default author string of each file.")

;; Features; these are all good to have on. (see also tempo.el)

(defvar html-helper-use-expert-menu t
  "*If not nil, then use the full HTML menu.")

(defvar html-helper-do-write-file-hooks t
  "*If not nil, then modify `local-write-file-hooks' to do timestamps.")

(defvar html-helper-build-new-buffer t
  "*If not nil, then insert `html-helper-new-buffer-strings' for new buffers.")

;; variables to configure (these defaults are reasonable.)

(defvar html-helper-htmldtd-version "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML//EN\">\n"
  "*Version of HTML DTD you're using.")

(defvar html-helper-user-menu nil
  "*Extra items to put in the HTML expert menu.
The value of this symbol is appended to the beginning of the expert
menu that is handed off to easymenu for definition. It should be a
list of vectors or lists which themselves are vectors (for submenus).")

(defvar html-helper-basic-offset 2
  "*Basic indentation size used for list indentation")

(defvar html-helper-item-continue-indent 4
  "*Indentation of lines that follow a <li> item.
Default is 4, the length of things like \"<li>\" and \"<dd>\".")

(defvar html-helper-never-indent nil
  "*If not nil, the indentation code for html-helper is turned off.")

;; hooks (see also tempo.el)

;; On prompts... Charles Curley (http://w3.trib.com/~ccurley/): It took
;; some time to figure this out... The (p "prompt: ") and (r "prompt:
;; ") entries indicate where the prompting mode should prompt for a
;; field in the tag. (p ) indicates a parameter, such as the color of a
;; <font> tag. (r ) indicates a region, where the text to be surrounded
;; by the tag should go, such as the text to be turned that color. The
;; difference is this: when prompting mode is turned off and the user
;; is surrounding a region with the tag, the (r ) (region) parameter
;; indicates where the surrounded region will go. The first (p )
;; (parameter) is where the cursor will go, ready to input the first
;; parameter to the tag.

;; So when you have prompting on, and use the font with color and size
;; tag, put the cursor where you want the modified text to go. Start
;; inserting the tag. You will be prompted for the color, the size, and
;; then the text to display that way. When you have prompting turned
;; off, and don't have a region blocked, insert the font tag, and the
;; cursor will be at the the first parameter. Then tab over to the
;; space between the two parts of the tag, and type in your text. If
;; you have region blocked, C-u followed by the tag will surround the
;; region with the tag. The blocked region goes into the (r )
;; parameter. Then the cursor is placed at the first (p ) location,
;; ready for you to type in a parameter, such as the color of the text.

(defvar html-helper-mode-hook nil
  "*Hook run when html-helper-mode is started.")

(defvar html-helper-load-hook nil
  "*Hook run when html-helper-mode is loaded.")

(defvar html-helper-timestamp-hook 'html-helper-default-insert-timestamp
  "*Hook called for timestamp insertion.
Override this for your own timestamp styles.")

;; strings you might want to change

(defvar html-helper-new-buffer-template
  '(html-helper-htmldtd-version
    "<html> <head>\n"
    "<title>" p "</title>\n</head>\n\n"
    "<body>\n"
    "<h1>" p "</h1>\n\n"
    p
    "\n\n<hr>\n"
    "<address>" html-helper-address-string "</address>\n"
    html-helper-timestamp-start
    html-helper-timestamp-end
    "\n</body> </html>\n")
  "*Template for new buffers.
Inserted by `html-helper-insert-new-buffer-strings' if
`html-helper-build-new-buffer' is set to t")

(defvar html-helper-new-ASP-buffer-template
  '("<%@ LANGUAGE=\"" p "\" %>\n"
    "<html> <head>\n"    
    "<%\n\n%>\n"
    "</head><body>\n"
    "<% '<!-- " html-helper-timestamp-start "  " html-helper-timestamp-end
    " --> %>\n"
    "\n</body></html>\n")
"*Template for new ASP buffers.
Inserted by `html-helper-insert-new-ASP-buffer-strings' if
`html-helper-build-new-buffer' is set to t")

(defvar html-helper-new-PHP-buffer-template
  '("<html> <head>\n"    
    "<? PHP\n\n?>"
    "</head><body>\n"
    "<? /* " html-helper-timestamp-start "\n\n"
    html-helper-timestamp-end
    " */ ?>\n"
    "\n</body></html>\n"
    )
"*Template for new PHP buffers.
Inserted by `html-helper-insert-new-PHP-buffer-strings' if
`html-helper-build-new-buffer' is set to t")

(defvar html-helper-timestamp-start "<!-- hhmts start --> "
  "*Start delimiter for timestamps.
Everything between `html-helper-timestamp-start' and
`html-helper-timestamp-end' will be deleted and replaced with the output
of the functions `html-helper-timestamp-hook' if
`html-helper-do-write-file-hooks' is t")

(defvar html-helper-timestamp-end "<!-- hhmts end -->"
  "*End delimiter for timestamps.
Everything between `html-helper-timestamp-start' and
`html-helper-timestamp-end' will be deleted and replaced with the output
of the function `html-helper-insert-timestamp' if
`html-helper-do-write-file-hooks' is t")

;; control over what types of tags to load. By default, we load all the
;; ones we know of.

(defvar html-helper-types-to-install
  '(anchor list header logical phys textel entity image table head form script)
  "*List of tag types to install when html-helper-mode is first loaded.
If you want to not install some type of tag, override this variable.
Order is significant: menus go in this order.")

;; emacs18 detection.

(defvar html-helper-emacs18
  (and (boundp 'emacs-version)
       (or (and (boundp 'epoch::version) epoch::version)
           (string-lessp emacs-version "19")))
  "I'll do minimal emacs18 support, grumble.")

;;}}} end of user variables

;;{{{ Prologue

(require 'tempo)			;essential part of html-helper-mode
(condition-case nil			;menu support, standard in emacs19
    (require 'auc-menu)			;add-on for XEmacs. *why* does this
  (error (require 'easymenu)))		;package have to have two names?

;;}}}

;;{{{ html-helper-mode-syntax-table and html-helper-mode-abbrev-table

;; emacs doesn't seem to be able to really handle SGML like syntax. In
;; particular, comments are a loss.
;; We do try this, though: give < and > matching semantics

(defvar html-helper-mode-syntax-table nil
  "Syntax table for html-helper.")

(if html-helper-mode-syntax-table
    ()
  (setq html-helper-mode-syntax-table (make-syntax-table text-mode-syntax-table))
  (modify-syntax-entry ?<  "(>  " html-helper-mode-syntax-table)
  (modify-syntax-entry ?>  ")<  " html-helper-mode-syntax-table)
  (modify-syntax-entry ?\" ".   " html-helper-mode-syntax-table)
  (modify-syntax-entry ?\\ ".   " html-helper-mode-syntax-table)
  (modify-syntax-entry ?'  "w   " html-helper-mode-syntax-table))

(defvar html-helper-mode-abbrev-table nil
  "Abbrev table used while in html-helper-mode.")
(define-abbrev-table 'html-helper-mode-abbrev-table ())

;;}}}

;;{{{ type based keymap and menu variable and function setup

;; Our basic keymap.
(defvar html-helper-mode-map (make-sparse-keymap)
  "Keymap for html-helper")
(defvar html-helper-mode-menu nil
  "Menu for html-helper. Clobbered and rebuilt by `html-helper-install-menu'")

;; html-helper-mode has a concept of "type" of tags. Each type is a
;; list of tags that all go together in one keymap and one menu.
;; Types can be added to the system after html-helper has been loaded,
;; briefly by doing html-helper-add-type-to-alist, then
;; html-helper-install-type, then html-helper-add-tag (for each tag)
;; then html-helper-rebuild-menu. See the mode documentation for more detail.

(defconst html-helper-type-alist nil
  "Alist: type of tag -> keymap, keybinding, menu, menu string.
Add to this with `html-helper-add-type-to-alist'.")

(defun html-helper-keymap-for (type)
  "Accessor function for alist: for type, return keymap or nil"
  (nth 0 (cdr-safe (assq type html-helper-type-alist))))

(defun html-helper-key-for (type)
  "Accessor function for alist: for type, return keybinding or nil"
  (nth 1 (cdr-safe (assq type html-helper-type-alist))))

(defun html-helper-menu-for (type)
  "Accessor function for alist: for type, return menu or nil"
  (nth 2 (cdr-safe (assq type html-helper-type-alist))))

(defun html-helper-menu-string-for (type)
  "Accessor function for alist: for type, return menustring or nil"
  (nth 3 (cdr-safe (assq type html-helper-type-alist))))

(defun html-helper-normalized-menu-for (type)
  "Helper function for building menus from submenus: add on string to menu."
  (cons (html-helper-menu-string-for type)
	(eval (html-helper-menu-for type))))

(defun html-helper-add-type-to-alist (type)
  "Add a type specification to the alist.
The spec goes (type . (keymap-symbol keyprefix menu-symbol menu-string)).
See code for an example."
  (setq html-helper-type-alist (cons type html-helper-type-alist)))

;; Here are the types provided by html-helper-mode.
(mapcar 'html-helper-add-type-to-alist
  '((entity  . (nil nil html-helper-entity-menu "Insert Character Entities"))
    (textel  . (nil nil html-helper-textel-menu "Insert Text Elements"))
    (head    . (html-helper-head-map "\C-c\C-h" html-helper-head-menu "Insert Structural Elements"))
    (header  . (html-helper-header-map "\C-c\M-h" html-helper-header-menu "Insert Headers"))
    (anchor  . (html-helper-anchor-map "\C-c\C-a" html-helper-anchor-menu "Insert Hyperlinks"))
    (logical . (html-helper-logical-map "\C-c\M-l" html-helper-logical-menu "Insert Logical Styles"))
    (phys    . (html-helper-phys-map "\C-c\C-p" html-helper-phys-menu "Insert Physical Styles"))
    (list    . (html-helper-list-map "\C-c\C-l" html-helper-list-menu "Insert List Elements"))
    (form    . (html-helper-form-map "\C-c\C-f" html-helper-form-menu "Insert Form Elements"))
    (image   . (html-helper-image-map "\C-c\C-i" html-helper-image-menu "Insert Inlined Images"))
    (table   . (html-helper-table-map "\C-c\C-t" html-helper-table-menu "Insert Tables"))
    (script  . (html-helper-script-map "\C-c\C-s" html-helper-script-menu "Insert Scripts"))
    ))

;; Once html-helper-mde is aware of a type, it can then install the
;; type: arrange for keybindings, menus, etc.

(defconst html-helper-installed-types nil
  "The types that have been installed (used when building menus).
There is no support for removing a type once it has been installed.")

(defun html-helper-install-type (type)
  "Install a new tag type: add it to the keymap, menu structures, etc.
For this to work, the type must first have been added to the list of types
with html-helper-add-type-to-alist."
  (setq html-helper-installed-types (cons type html-helper-installed-types))
  (let ((keymap (html-helper-keymap-for type))
	(key (html-helper-key-for type))
	(menu (html-helper-menu-for type))
	(menu-string (html-helper-menu-string-for type)))
    (and key
	 (progn
	   (set keymap nil)
	   (define-prefix-command keymap)
	   (if html-helper-emacs18
	       (progn
		 (set keymap (make-sparse-keymap))
		 (define-key html-helper-mode-map key (eval keymap)))
	     (define-key html-helper-mode-map key keymap))))
    (and menu
	 (progn
	   (set menu nil)))))

;; install the default types.
(mapcar 'html-helper-install-type html-helper-types-to-install)

;; special mode keys
(mapcar
 (function (lambda (l) (define-key html-helper-mode-map (car l) (nth 1 l))))
 '(("\M-\C-f" tempo-forward-mark)
   ("\M-\C-b" tempo-backward-mark)
   ("\M-\t"   tempo-complete-tag)))

;; Extra commands that HTML helper supports that aren't insertions
(defvar html-helper-mode-functions-map nil
  "Keymap for extra HTML mode functions")

(define-prefix-command 'html-helper-mode-functions-map)
(define-key html-helper-mode-map "\C-c\C-z"
  'html-helper-mode-functions-map)
(define-key html-helper-mode-functions-map "t"
  'html-helper-insert-timestamp-delimiter-at-point)
(define-key html-helper-mode-functions-map "a"
  'html-script-narrow-to-asp)
(define-key html-helper-mode-functions-map "p"
  'html-script-narrow-to-php)
(define-key html-helper-mode-functions-map "b"
  'html-script-narrow-to-vbscript)
(define-key html-helper-mode-functions-map "j"
  'html-script-narrow-to-javascript)
 
;; indentation keys - only rebind these if the user wants indentation
(if html-helper-never-indent
    ()
  (define-key html-helper-mode-map "\t" 'html-helper-indent-command)
  (define-key html-helper-mode-map "\C-m" 'newline-and-indent))

;; browse url stuff
(if (fboundp 'browse-url-of-file)
    (define-key html-helper-mode-functions-map "v" 'browse-url-of-file))
(if (and (boundp 'browse-url-browser-function) (fboundp browse-url-browser-function))
    (define-key html-helper-mode-functions-map "u" browse-url-browser-function))

;;}}}

;;{{{ html-helper-add-tag function for building basic tags

(defvar html-helper-tempo-tags nil
  "List of tags used in completion.")

;; this while loop is awfully Cish
;; isn't there an emacs lisp function to do this?
(defun html-helper-string-to-symbol (input-string)
  "Given a string, downcase it and replace spaces with -.
We use this to turn menu entries into good symbols for functions.
It's not entirely successful, but fortunately emacs lisp is forgiving."
  (let* ((s (copy-sequence input-string))
	 (l (1- (length s))))
    (while (> l 0)
      (if (char-equal (aref s l) ?\ )
	  (aset s l ?\-))
      (setq l (1- l)))
    (concat "html-" (downcase s))))


(defun html-helper-add-tag (l)
  "Add a new tag to html-helper-mode.
Builds a tempo-template for the tag and puts it into the
appropriate keymap if a key is requested. Format:
`(html-helper-add-tag '(type keybinding completion-tag menu-name template doc)'"
  (let* ((type (car l))
	 (keymap (html-helper-keymap-for type))
	 (menu (html-helper-menu-for type))
	 (key (nth 1 l))
	 (completer (nth 2 l))
	 (name (nth 3 l))
	 (tag (nth 4 l))
	 (doc (nth 5 l))
	 (command (if (string-equal completer "function")
		      (nth 4 l)
		      (tempo-define-template (html-helper-string-to-symbol name)
					 tag completer doc
					 'html-helper-tempo-tags))))

    (if (null (memq type html-helper-installed-types))    ;type loaded?
	t                                                 ;no, do nothing.
      (if (stringp key)			                  ;bind key somewhere?
	  (if keymap			                  ;special keymap?
	      (define-key (eval keymap) key command)      ;t:   bind to prefix
	    (define-key html-helper-mode-map key command));nil: bind to global
	t)
      (if menu				                  ;is there a menu?
	  (set menu			                  ;good, cons it in
	       (cons (vector name command t) (eval menu))))
      )))

;; for backwards compatability
(fset 'html-helper-add-cookie 'html-helper-add-tag)

;;}}}

;;{{{ most of the HTML tags


(defun html-helper-insert-or-wipe (string)
  "Propmts for the value of an optional attribute named STRING and
inserts it in the current buffer. Inserts nothing if the users replies
with a null string."
  (cond ((and (stringp string)
	      tempo-interactive)
	 (let ((val (read-from-minibuffer (concat string " :"))))
	   (cond ((> (string-bytes val) 0) 
		  (insert-string (concat " " string "=\"" val "\"" )))	 
	  )))
	;; just to tell that there's something weird in the calling
	;; code... But behaves like a no op if tempo-interactive is 
	;; nil
	(tempo-interactive 
	 (error (concat "Wrong type argument: stringp, " string)))))

;; These tags are an attempt to be HTML/2.0 compliant, with the exception
;; of container <p>, <li>, <dd>, <dt> (we adopt 3.0 behaviour).
;; For reference see <URL:http://www.w3.org/hypertext/WWW/MarkUp/MarkUp.html>

;; order here is significant: within a tag type, menus and mode help
;; go in the reverse order of what you see here. Sorry about that, it's
;; not easy to fix.

(mapcar
 'html-helper-add-tag
 '(
   ;;entities
   (entity  "\C-c#"   "&#"              "Ascii Code"      ("&#" (r "Ascii: ") ";"))
   (entity  "\C-c\""  "&quot;"          "Quotation mark"  ("&quot;"))
   (entity  "\C-c$"   "&reg;"           "Registered"      ("&reg;"))
   (entity  "\C-c@"   "&copy;"          "Copyright"       ("&copy;"))
   (entity  "\C-c-"   "&shy;"           "Soft Hyphen"     ("&shy;"))
   (entity  "\C-c "   "&nbsp;"		"Nonbreaking Space"  ("&nbsp;"))
   (entity  "\C-c&"   "&amp;"		"Ampersand"	  ("&amp;"))
   (entity  "\C-c>"   "&gt;"	  	"Greater Than"       ("&gt;"))
   (entity  "\C-c<"   "&lt;"		"Less Than"	  ("&lt;"))
   ;; letters with accents common in italian
   (entity  "\C-ca"   "&agrave;"        "a` (&&agrave;)"          ("&agrave;"))        
   (entity  "\C-ce"   "&egrave;"        "e` (&&egrave;)"          ("&egrave;"))
   (entity  "\C-cE"   "&eacute;"        "e' (&&eacute;)"          ("&eacute;"))
   (entity  "\C-co"   "&ograve;"        "o` (&&ograve;)"          ("&ograve;"))
   (entity  "\C-ci"   "&igrave;"        "i` (&&igrave;)"          ("&igrave;"))
   (entity  "\C-cu"   "&ugrave;"        "u` (&&ugrave;)"          ("&ugrave;"))

   ;; logical styles
   (logical "b"       "<blockquote>"	"Blockquote"     	  
	    ("<blockquote>" (r "Quote: ") "</blockquote>"))
   (logical "c"       "<code>"		"Code"           	  
	    ("<code>" (r "Code: ") "</code>"))
   (logical "x"       "<samp>"		"Sample"         	  
	    ("<samp>" (r "Sample code") "</samp>"))
   (logical "r"       "<cite>"		"Citation"       	  
	    ("<cite>" (r "Citation: ") "</cite>"))
   (logical "k"       "<kbd>"		"Keyboard Input"       	  
	    ("<kbd>" (r "Keyboard: ") "</kbd>"))
   (logical "v"       "<var>"		"Variable"       	  
	    ("<var>" (r "Variable: ") "</var>"))
   (logical "d"       "<dfn>"		"Definition"     	  
	    ("<dfn>" (r "Definition: ") "</dfn>"))
   (logical "a"	      "<address>"	"Address"		  
	    ("<address>" r "</address>"))
   (logical "e"       "<em>"		"Emphasized"     	  
	    ("<em>" (r "Text: ") "</em>"))
   (logical "s"       "<strong>"	"Strong"         	  
	    ("<strong>" (r "Text: ") "</strong>"))
   (logical "p"       "<pre>"		"Preformatted"   	  
	    ("<pre>" (r "Text: ") "</pre>"))

   ;;physical styles
   (phys    "s"       "<strike>"	"Strikethru"         
	    ("<strike>" (r "Text: ") "</strike>"))
   (phys    "u"       "<u>"		"Underline"          
	    ("<u>" (r "Text: ") "</u>"))
   (phys    "i"       "<i>"		"Italic"             
	    ("<i>" (r "Text: ") "</i>"))
   (phys    "b"	      "<b>"    		"Bold"               
	    ("<b>" (r "Text: ") "</b>"))
   (phys    "f"       "<tt>"		"Fixed"              
	    ("<tt>" (r "Text: ") "</tt>"))
   (phys    "c"       "<center>"        "Center"             
	    ("<center>" (r "Text: ") "</center>"))

;; html4.0 stuff, omitted

;    (phys    "5" "<span style=" "Spanning style" 
; 	     ("<span style=\"" (p "style: ") "\">" 'r "</span>"))
;    (phys    "l" "<span class=" "Spanning class"
; 	     ("<span class=\"" (p "class: ") "\">" 'r "</span>"))
 

   ;;headers
   (header  "6"       "<h6>"		"Header 6"       	  
	    ("<h6>" (r "Header: ") "</h6>"))
   (header  "5"       "<h5>"		"Header 5"       	  
	    ("<h5>" (r "Header: ") "</h5>"))
   (header  "4"       "<h4>"		"Header 4"       	  
	    ("<h4>" (r "Header: ") "</h4>"))
   (header  "3"       "<h3>"		"Header 3"       	  
	    ("<h3>" (r "Header: ") "</h3>"))
   (header  "2"       "<h2>"		"Header 2"       	  
	    ("<h2>" (r "Header: ") "</h2>"))
   (header  "1"	      "<h1>"     	"Header 1"       	  
	    ("<h1>" (r "Header: ") "</h1>"))

   ;; forms
   (form    "o"       "<option>"        "Option"                 
	    (& "<option>" > ))
   (form    "v"       "<option value"   "Option with Value"      
	    (& "<option value=\"" (r "Value: ") "\">" >))
   (form    "s"       "<select"		"Selections"	          
	    ("<select" 
	     (html-helper-insert-or-wipe "name") ">\n<option>" > "\n</select>")"<select")
   (form    "z"	      "<input"		"Reset Form"    	  
	    ("<input type=\"RESET\"" 
	     (html-helper-insert-or-wipe "value") ">"))
   (form    "m"	      "<input"		"Submit Form"   	  
	    ("<input type=\"SUBMIT\"" 
	     (html-helper-insert-or-wipe "value") ">"))
   (form    "b"       "<input"          "Button"                  
	    ("<input type=\"BUTTON\"" 
	     (html-helper-insert-or-wipe "value") 
	     (html-helper-insert-or-wipe "name") ">"))
   (form    "i"	      "<input"		"Image Field"   	  
	    ("<input type=\"IMAGE\"" 
	     (html-helper-insert-or-wipe "Name")
	     (html-helper-insert-or-wipe "src") ">"))
   (form    "h"       "<input"          "Hidden Field"            
	    ("<input type=\"HIDDEN\"" 
	     (html-helper-insert-or-wipe "name")
	     (html-helper-insert-or-wipe "value") ">"))
   (form    "p"	      "<textarea"	"Text Area"	  
	    ("<textarea" 
	     (html-helper-insert-or-wipe "name")
	     (html-helper-insert-or-wipe "rows")
	     (html-helper-insert-or-wipe "cols") ">" r "</textarea>"))
   (form    "c"	      "<input"		"Checkbox"   	          
	    ("<input type=\"CHECKBOX\"" 
	     (html-helper-insert-or-wipe "name") ">"))
   (form    "r"	      "<input"		"Radiobutton"   	  
	    ("<input type=\"RADIO\"" 
	     (html-helper-insert-or-wipe "Name") ">"))
   (form    "t"	      "<input"		"Text Field"	          
	    ("<input type=\"TEXT\"" 
	     (html-helper-insert-or-wipe "name")
	     (html-helper-insert-or-wipe "size") ">"))
   (form    "f"	      "<form"           "Form"		          
	    ("<form" 
	     (html-helper-insert-or-wipe "action")
	     (html-helper-insert-or-wipe "method") ">\n</form>\n"))

   ;;lists
   (list    "t"       "<dt>"            "Definition Item"         
	    (& "<dt>" > (r "Term: ") "\n<dd>" > 
	       (r "Definition: ")))
   (list    "l"       "<li>"            "List Item"               
	    (& "<li>" > (r "Item: ")))
   (list    "r"	      "<dir>"		"DirectoryList"      	  
	    (& "<dir>" > "\n<li>" > (r "Item: ") "\n</dir>" >))
   (list    "m"	      "<menu>"		"Menu List"		  
	    (& "<menu>" > "\n<li>" > (r "Item: ") "\n</menu>" >))
   (list    "o"	      "<ol>"		"Ordered List"   	  
	    (& "<ol>" > "\n<li>" > (r "Item: ") "\n</ol>" >))
   (list    "d"	      "<dl>"		"Definition List" 	  
	    (& "<dl>" > "\n<dt>" > 
	       (p "Term: ") "\n<dd>" > 
	       (r "Definition: ") "\n</dl>" >))
   (list    "u"	      "<ul>"		"Unordered List" 	  
	    (& "<ul>" > "\n<li>" > (r "Item: ") "\n</ul>" >))

   ;;anchors
   (anchor  "n"	      "<a name="	"Link Target"	  
	    ("<a " (html-helper-insert-or-wipe "name") ">" 
	     (r "Anchor text: ") "</a>"))
   (anchor  "l"	      "<a href="        "Hyperlink"          	  
	    ("<a href=\"" (p "href: ") "\" >" 
	     (r "Anchor text: ") "</a>"))                

   ;;graphics
;    (image   "a"       nil               "Aligned Image"	  
; 	    ("<img align=\"" 
; 	     (r "Alignment: ") "\" src=\"" (r "Image URL: ") "\">"))
;    (image   "i"       "<img src="	"Image"		  
; 	    ("<img src=\"" 
; 	     (r "Image URL: ") "\">"))
;    (image   "e"       "<img align="     "Aligned Image With Alt. Text"	  
; 	    ("<img align=\"" 
; 	     (r "Alignment: ") "\" src=\"" 
; 	     (r "Image URL: ") "\" alt=\"" 
; 	     (r "Text URL: ") "\">"))
;    (image   "t"       "<img alt="	"Image With Alternate Text"	  
; 	    ("<img alt=\"" 
; 	     (r "Text URL: ") "\" src=\"" 
; 	     (r "Image URL: ") "\">"))
;; New, (almost) all including, single menu item entry
;; src has to be there!
   (image	"a"	nil	"Image" 
		("<img src=\""
		 (p "src" ) "\" "
		 (html-helper-insert-or-wipe  "alt" )
		 (html-helper-insert-or-wipe  "height" )
		 (html-helper-insert-or-wipe  "width" )
		 (html-helper-insert-or-wipe  "align" )
		 ">"))
   ;; table 
   (table   "t"       "<table>"         "Table"           
	    ("<table" 
	     (html-helper-insert-or-wipe  "border" )
	     (html-helper-insert-or-wipe "width" )">\n</table>"))
   (table   "r"       "<tr>"         "Table Row"           
	    ("<TR>\n</TR>"))
   (table   "h"       "<th>"         "Table Header"        
	    ("<TH"
	     (html-helper-insert-or-wipe "rowspan" )
	     (html-helper-insert-or-wipe "colspan")">\n</TH>"))
   (table   "d"       "<td>"         "Table Data"           
	    ("<TD" 
	     (html-helper-insert-or-wipe "align" )
	     (html-helper-insert-or-wipe "colspan")"></TD>"))
   (table   "p"	"<caption>"	"html table caption"	
	    ("<caption>" (r . "Table: ") "</caption>"))   
   ;;text elements
   (textel  "\C-c="    nil		"Horizontal Line"	  
	    (& "<hr>\n"))
   (textel  "\C-c\C-m" nil		"Line Break"		  
	    ("<br>\n"))
   (textel  "\e\C-m"  nil		"Paragraph"	  
	    ("<p>"
	     (r "Text: ") "</p>"))
   
   ;;head elements
   (head    "H"       "<head>"          "Head"            
	    ("<head>\n" "</head>\n"))
   (head    "B"       "<body>"          "Body"            
	    ("<body>\n" "</body>\n"))
   (head    "i"	      "<isindex>"	"Isindex"         
	    ("<isindex>\n"))
   (head    "n"	      "<nextid>"	"Nextid"          
	    ("<nextid>\n"))
   (head    "h"       "<meta http-equiv=" "HTTP Equivalent" 
	    ("<meta" 
	     (html-helper-insert-or-wipe "http-equiv") " content=\"" 
	     (r "Content: ") "\">\n"))
   (head    "m"       "<meta name="     "Meta Name"       
	    ("<meta" 
	     (html-helper-insert-or-wipe "name") " content=\"" 
	     (r "Content: ") "\">\n"))
   (head    "l"	      "<link"		"Link"            
	    ("<link href=\"" p "\">"))
   (head    "b"       "<base"		"Base"            
	    ("<base href=\"" r "\">"))
   (head    "t"	      "<title>"		"Title"           
	    ("<title>" (r "Document title: ") "</title>"))
   ;; scripting elements
   (script  "j"    "<SCRIPT>"       "JavaScript"        
	    ("<SCRIPT TYPE=\"text/javascript\">\n" 
	     (r "Script") "</SCRIPT>"))
   (script  "v"    "<SCRIPT>"       "VBScript"        
	    ("<SCRIPT TYPE=\"text/vbscript\">\n" 
	     (r "Script") "</SCRIPT>"))
   (script  "%"    "<%="            "ASP output"      
	    ("<%="(p " Variabile: ")"%>"))
   (script  "a"    "<%xx%>"         "ASP code"        
	    ("<%\n"(r "Code: " )"\n%>"))
   (script  "<"    "<%xx%>"         "ASP break"        
	    ("%>\n"(r "Code: " )"\n<%"))
   (script  "="    "<?="            "PHP output"      
	    ("<?="(p " Variabile: ")"?>"))
   (script  "p"    "<?xx?>"         "PHP code"      
	    ("<? PHP\n"(r "Code: " )"\n?>"))
   (script  "?"    "<?xx?>"         "PHP break"        
	    ("?>\n"(r " Code: " )"\n<? PHP"))
   ))

;;}}}

;;{{{ html-helper-smart-insert-item

;; there are two different kinds of items in HTML - those in regular
;; lists <li> and those in dictionaries <dt>..<dd>
;; This command will insert the appropriate one depending on context.

(defun html-helper-smart-insert-item (&optional arg)
  "Insert a new item, either in a regular list or a dictionary."
  (interactive "*P")
  (let ((case-fold-search t))
    (if
        (save-excursion
          (re-search-backward "<li>\\|<dt>\\|<ul>\\|<ol>\\|<dd>\\|<menu>\\|<dir>\\|<dl>" nil t)
          (looking-at "<dt>\\|<dl>\\|<dd>"))
        (tempo-template-html-definition-item arg)
      (tempo-template-html-list-item arg))))

;; special keybindings in the prefix maps (not in the list of tags)
(and (boundp 'html-helper-list-map)
     (define-key html-helper-list-map "i" 'html-helper-smart-insert-item))

;; and, special menu bindings
(and (boundp 'html-helper-list-menu)
     (setq html-helper-list-menu
	   (cons '["List Item" html-helper-smart-insert-item t] html-helper-list-menu)))

;;}}}

;;{{{ menu support

;; menus are built for easymenu. html-helper-add-tag builds
;; submenus based on tag type, the expert menu code lumps them
;; together into one list and calls easy-menu-define

(defvar html-helper-novice-menu
  '("HTML"
    ["Insert Paragraph" tempo-template-html-paragraph t]
    ["Insert Hyperlink" tempo-template-html-hyperlink t]
    ["Insert Big Header" tempo-template-html-header-2 t]
    ["Insert Unordered List" tempo-template-html-unordered-list t]
    ["Insert List Item" html-helper-smart-insert-item t]
    ["Insert Inlined Image" tempo-template-html-image-with-alternate-text t]
    ["Turn on Expert Menu" html-helper-toggle-expert-menu t])
  "Menu for novices, only installed if `html-helper-use-expert-menu is nil'")

(defun html-helper-menu nil
  "Return the proper menu. Looks at `html-helper-use-expert-menu'"
  (if html-helper-use-expert-menu
      (html-helper-expert-menu)
    html-helper-novice-menu))

(defun html-helper-rebuild-menu nil
  "Rebuild and install the HTML menu (using `easy-menu-define').
If `html-helper-use-expert-menu' is nil, then just use a novice menu."
  (let ((menu (html-helper-menu)))
    (easy-menu-remove menu)
    (easy-menu-define html-helper-mode-menu-symbol
		      html-helper-mode-map "HTML menus" menu)
    (easy-menu-add menu html-helper-mode-map)))

(defun html-helper-toggle-expert-menu (&optional arg)
  "Toggle full HTML menus. Optional arg acts like minor-mode args."
  (interactive "P")
  (setq html-helper-use-expert-menu
	(if (null arg) (not html-helper-use-expert-menu)
	  (> (prefix-numeric-value arg) 0)))
  (html-helper-rebuild-menu))

;; If browse-url loaded, add this in the novice menu.
(if (fboundp 'browse-url-of-file)
    (setq html-helper-novice-menu
	  (append html-helper-novice-menu 
		  (list ["Load This Buffer in Browser" browse-url-of-file t]))))

;; Narrrowing to scripts, this don't use tempo because has to call functions
;; and not insert templates


;; Expert menus: consed up out of html-helper-installed-types
(defun html-helper-expert-menu ()
  "This menu is based on the current value of `html-helper-installed-types'.
This function can be called again, it redoes the entire menu."
  ;; first, reset this so we can call this again and again.
  (setq html-helper-mode-menu nil)
  
  ;; Cons in the toggle of the menu
  (setq html-helper-mode-menu
	(cons '["Turn on Novice Menu"
		html-helper-toggle-expert-menu t]
	      html-helper-mode-menu))

  ;; Now add in user-provided menu stuff
  (setq html-helper-mode-menu
	(append html-helper-user-menu html-helper-mode-menu))

  ;; Now cons in the browse-url functions
  (if (fboundp 'browse-url-of-file)
    (setq html-helper-mode-menu
	  (cons '["Load this Buffer in Browser" browse-url-of-file t]
		html-helper-mode-menu)))
  (if (and (boundp 'browse-url-browser-function) (fboundp browse-url-browser-function))
    (setq html-helper-mode-menu
	  (cons (vector "Browse URL at point" browse-url-browser-function t)
		html-helper-mode-menu)))

  ;; cons in the timestamp delimiters
  (setq html-helper-mode-menu
	(cons '["Insert Timestamp Delimiter"
		html-helper-insert-timestamp-delimiter-at-point t]
	      html-helper-mode-menu))

  ;; cons script narrowing
  (setq html-helper-mode-menu
	(append html-helper-mode-menu 
		(list ["Narrow to ASP" html-script-narrow-to-asp t])))
  (setq html-helper-mode-menu
	(append html-helper-mode-menu 
		(list ["Narrow to PHP" html-script-narrow-to-php t])))
  (setq html-helper-mode-menu
	(append html-helper-mode-menu 
		(list ["Narrow to VBScript" html-script-narrow-to-vbscript t])))
   (setq html-helper-mode-menu
	 (append html-helper-mode-menu 
		 (list ["Narrow to JavaScript" html-script-narrow-to-javascript t])))
  
  ;; now cons up the main menu out of the submenus
  (mapcar
   (function (lambda (type)
	       (setq html-helper-mode-menu
		     (cons (html-helper-normalized-menu-for type)
			   html-helper-mode-menu))))
	  html-helper-installed-types)

  ;; now tack on our name
  (setq html-helper-mode-menu (cons "HTML" html-helper-mode-menu))
  html-helper-mode-menu)

(html-helper-rebuild-menu)

;;}}}

;;{{{ context guessing

;; guess where we are in indented lists based on the last list token.
;; it would be much better to try to match </ul> to <ul>, and </ol> to <ol>
;; etc, but that is pretty unwieldy and slow.
;; Note, we make select/option look like a list structure too, so indentation
;; works. This is a bit weird, but it's ok.

(defvar html-helper-any-list-item-start "<li>\\|<dt>\\|<dd>\\|<option\\|<th>\\|<td>")
(defvar html-helper-any-list-item-end "</li>\\|</dt>\\|</dd>\\|</th>\\|</td>")
(defvar html-helper-any-list-start "<dl>\\|<ul>\\|<ol>\\|<menu>\\|<dir>\\|<select\\|<table\\|<tr>")
(defvar html-helper-any-list-end "</dl>\\|</ul>\\|</ol>\\|</menu>\\|</dir>\\|</select>\\|</table>\\|</tr>")
(defvar html-helper-any-list
  (format "\\(%s\\)\\|\\(%s\\)\\|\\(%s\\)\\|\\(%s\\)"
	  html-helper-any-list-start
	  html-helper-any-list-end
	  html-helper-any-list-item-start
	  html-helper-any-list-item-end))

(defvar html-helper-indentation-list
  (format "\\(%s\\)\\|\\(%s\\)\\|\\(%s\\)"
	  html-helper-any-list-start
	  html-helper-any-list-end
	  html-helper-any-list-item-start))
(defvar html-helper-search-limit 2000 "limit on how far back we search")

(defun html-helper-context-symbol ()
  "Return the symbol the last match (against `html-helper-any-list') found."
  (cond ((match-beginning 1) 'list-start)
	((match-beginning 2) 'list-end)
	((match-beginning 3) 'item-start)
	((match-beginning 4) 'item-end)
	(t 'error)))

(defun html-helper-guess-prev-context ()
  "Figure out the last list-type tag before point relevant to indentation.
Returns 'item-start if the last list tag is a list item start
        'start      if the last list tag is the start of a list
        'end        if the last list tag is the end of a list.
Ignores list item ends, because those aren't reliable for indentation."
  (save-excursion
    (let* ((lim (max (point-min) (- (point) html-helper-search-limit)))
	   (context (if (re-search-backward html-helper-indentation-list lim t)
			(html-helper-context-symbol)
		      nil)))
      (cons context (current-indentation)))))

(defun html-helper-print-prev-context ()
  (interactive)
  (message "%s" (html-helper-guess-prev-context)))

;;}}}

;;{{{ indentation

(defvar html-helper-print-indent-info nil
  "If t, indent will print out information as a message.")

(defun html-helper-indent-command ()
  "Command for indenting HTML to the appropriate column.
Calls `html-helper-indent' which tries to examine how many levels down
in nested lists we are and does the appropriate indentation.'
See also `html-helper-basic-offset', `html-helper-item-continue-indent',
and `html-helper-never-indent'."
  (interactive)
  (html-helper-indent))

;; some ideas borrowed from cc-mode.el.
;; Basic logic:
;;   if this line is some sort of list token, indent according to prev context:
;;     if previous context was a list-end or item-start, use its indentation
;;     if previous context was a list start, indent forward basic-offset
;;     ignore previous list-ends, their indentation is unreliable.
;;     then if this is some sort of list-item, do special case fixups:
;;       if this is a item start or end and previous was a list end, go back
;;           item-continue-indent (the </ul> is probably indented for an <li>
;;       if this is a list end and previous was a list end, go back
;;           item-continue-indent (the </ul> is probably indented for an <li>
;;       if this is a list end and prev *not* a list end, go back basic-offset
;;   else if this line is not a list item, and previous line is a item-start
;;     indent continue-indent, because this is part of the item

;; code to never indent <PRE></PRE> sections. Many thanks to 
;; Stan Lanning <lanning@pobox.com>
(defun html-helper-indent-leave-alone-p ()
  (let* ((pre (save-excursion (search-backward "<pre>" nil t)))
	 (endpre (save-excursion (search-backward "</pre>" pre t))))
    (and pre (null endpre))))

(defadvice html-helper-indent (around leave-pre-alone activate)
  (cond ((not (html-helper-indent-leave-alone-p))
	 ad-do-it)
	(html-helper-print-indent-info
	 (message "In <pre> -- skipping indentation"))
	(t nil)))

(defun html-helper-indent ()
  "Indentation workhorse function."
  (if html-helper-never-indent
      ()
    (let ((m (point-marker))
	  (bol (progn (beginning-of-line) (point))))

      ;; unindent the line
      (delete-region (point) (progn (back-to-indentation) (point)))

      (let* ((where (html-helper-guess-prev-context))
	     (prev-context (car where))
	     (this-context nil)
	     (previ (cdr where))
	     (newi (cond 
		    ((eq prev-context 'list-end) previ)
		    ((eq prev-context 'item-start) previ)
		    ((eq prev-context 'list-start) (+ previ html-helper-basic-offset))
		    (t previ))))

	;; newi is set to the basic indentation, now adjust indentation
	;; based on what the current line is.
	(if (looking-at html-helper-any-list)
	    (progn
	      (setq this-context (html-helper-context-symbol))
	      (cond
	       ;; item start or end and last line was a list-end: go backwards
	       ((and
		 (or (eq this-context 'item-start) (eq this-context 'item-end))
		 (eq prev-context 'list-end))
		(setq newi (- newi html-helper-item-continue-indent)))
	       
	       ;; end of list and last line was an end: go backwards twice
	       ((and (eq this-context 'list-end) (eq prev-context 'list-end))
		(setq newi (- newi html-helper-item-continue-indent html-helper-basic-offset)))
	       
	       ;; Any other end of list? Indent negative
	       ((and (eq this-context 'list-end))
		(setq newi (- newi html-helper-basic-offset)))
	       
	       ;; start of list and last line beginning of item, go forwards
	       ((and (eq this-context 'list-start) (eq prev-context 'item-start))
		(setq newi (+ newi html-helper-item-continue-indent)))))
	  
	  ;; default: no special case, indent forward for text
	  (cond
	   ;; last line an item? Beginning of continued item - go forward
	   ((eq prev-context 'item-start)
	    (setq newi (+ newi html-helper-item-continue-indent)))))

	(if html-helper-print-indent-info
	    (message "Last Context: %s, This Context: %s, Previous: %s New: %s" prev-context this-context previ newi))
	
	;; just in case
	(if (< newi 0)
	    (setq newi 0))
	(indent-to newi newi)
	
	;; adjust point to where it was before, or at start of indentation
	(goto-char (marker-position m))
	(if (< (current-column) (current-indentation))
	    (back-to-indentation))))))

;;}}}

;;{{{ completion finder for tempo

(defvar html-helper-completion-finder
  (if html-helper-emacs18
      'html-helper-emacs18-completion-finder
    "\\(\\(<\\|&\\).*\\)\\=")
  "Passed to tempo-use-tag-list, used to find tags to complete.")

;; The regexp finds everything between the last < or & and point,
;; which is good enough to match the tags HTML might complete.
;; emacs18 doesn't have the \= for regexps, though, so we do something
;; more hackish.

(defun html-helper-emacs18-completion-finder ()
  "Unfortunately emacs18 doesn't support \\= in regexps, so we do this hack.
If you have problems with it, maybe you should upgrade to emacs19 :-)"
  (let* ((where nil)
         (s (buffer-substring
             (point)
             (setq where (save-excursion
                           (re-search-backward "<\\|&" (min (point-min) 100) t)
                           (point))))))
    (cons s where)))

;;}}}

;;{{{ timestamps

(defun html-helper-update-timestamp ()
  "Basic function for updating timestamps.
It finds the timestamp in the buffer by looking for
`html-helper-timestamp-start', deletes all text up to
`html-helper-timestamp-end', and runs `html-helper-timestamp-hook' which
will should insert an appropriate timestamp in the buffer."
  (save-excursion
    (goto-char (point-max))
    (if (not (search-backward html-helper-timestamp-start nil t))
	(message "timestamp delimiter start was not found")
      (let ((ts-start (+ (point) (length html-helper-timestamp-start)))
	    (ts-end (if (search-forward html-helper-timestamp-end nil t)
			(- (point) (length html-helper-timestamp-end))
		      nil)))
	(if (not ts-end)
	    (message "timestamp delimiter end was not found. Type C-c C-t to insert one.")
	  (delete-region ts-start ts-end)
	  (goto-char ts-start)
	  (run-hooks 'html-helper-timestamp-hook)))))
  nil)

(defun html-helper-default-insert-timestamp ()
  "Default timestamp insertion function."
  (let ((time (current-time-string)))
    (insert "Last modified: "
	    (substring time 0 20)
	    (nth 1 (current-time-zone))
	    " "
	    (substring time -4)
	    " ")))

(defun html-helper-insert-timestamp-delimiter-at-point ()
  "Simple function that inserts timestamp delimiters at point.
Useful for adding timestamps to existing buffers."
  (interactive)
  (insert html-helper-timestamp-start)
  (insert html-helper-timestamp-end))

;;}}}

;;{{{ html-helper-insert-new-buffer-strings

(tempo-define-template "html-skeleton" html-helper-new-buffer-template
		       nil
		       "Insert a skeleton for a HTML document")

(tempo-define-template "ASP-skeleton" html-helper-new-ASP-buffer-template
		       nil
		       "Insert a skeleton for a ASP document")

(tempo-define-template "PHP-skeleton" html-helper-new-PHP-buffer-template
		       nil
		       "Insert a skeleton for a PHP document")

(defun html-helper-insert-new-buffer-strings ()
  "Insert `html-helper-new-buffer-strings'."
  (tempo-template-html-skeleton))

(defun html-helper-insert-new-ASP-buffer-strings ()
  "Insert `html-helper-new-ASP-buffer-strings'."
  (tempo-template-ASP-skeleton))

(defun html-helper-insert-new-PHP-buffer-strings ()
  "Insert `html-helper-new-PHP-buffer-strings'."
  (tempo-template-PHP-skeleton))

;;}}}

;;{{{ html-helper-mode

(defun base-html-helper-mode (mode)
  "basic mode, called by the exported modes with MODE telling what 
is the mode to run (that's the skeleton to insert in empty files)"
  (kill-all-local-variables)

  (use-local-map html-helper-mode-map)
  (setq local-abbrev-table html-helper-mode-abbrev-table)
  (set-syntax-table html-helper-mode-syntax-table)

  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-column)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'indent-line-function)
  (make-local-variable 'html-helper-count)

  ;; font-lock setup for various emacsen: XEmacs, Emacs 19.29+, Emacs <19.29.
  ;; By Ulrik Dickow <dickow@nbi.dk>.  (Last update: 05-Sep-1995).
  (cond	((string-match "XEmacs\\|Lucid" (emacs-version)) ; XEmacs/Lucid
	 (put major-mode 'font-lock-keywords-case-fold-search t))
	;; XEmacs (19.13, at least) guesses the rest correctly.
	;; If any older XEmacsen don't, then tell me.
	;;
	((string-lessp "19.28.89" emacs-version) ; Emacs 19.29 and later
	 (make-local-variable 'font-lock-defaults)
	 (setq font-lock-defaults '(html-helper-font-lock-keywords t t)))
	;;
	(t ; Emacs 19.28 and older
	 (make-local-variable 'font-lock-keywords-case-fold-search)
	 (make-local-variable 'font-lock-keywords)
	 (make-local-variable 'font-lock-no-comments)
	 (setq font-lock-keywords-case-fold-search t)
	 (setq font-lock-keywords html-helper-font-lock-keywords)
	 (setq font-lock-no-comments t)))

  (setq comment-start "<!-- "
	comment-end " -->"
	comment-start-skip "<!--[ \t]*"
	comment-column 0
	indent-line-function 'html-helper-indent)

  (tempo-use-tag-list 'html-helper-tempo-tags html-helper-completion-finder)
  
  (if html-helper-do-write-file-hooks
      (add-hook 'local-write-file-hooks 'html-helper-update-timestamp))

  (if (and html-helper-build-new-buffer (zerop (buffer-size)))
      (cond ((string= mode "HTML")
	     (html-helper-insert-new-buffer-strings))
	    ((string= mode "ASP")
	     (html-helper-insert-new-ASP-buffer-strings))
	    ((string= mode "PHP")
	     (html-helper-insert-new-PHP-buffer-strings))
	    ))

  (easy-menu-add (html-helper-menu) html-helper-mode-map)

  (run-hooks 'text-mode-hook)
  (run-hooks 'html-mode-hook)
;; qui i keybinding
  (run-hooks 'html-helper-mode-hook))

(cond (html-helper-mode-uses-visual-basic
       (defun asp-html-helper-mode ()
  "Mode for editing HTML documents with ASP server scripts.

The main function html-helper-mode provides is a menu and keybindings
for the HTML tags one inserts when writing HTML documents. Selecting
the menu item or typing the key sequence for a command inserts the
corresponding tag and places point in the right place. If a prefix
argument is supplied, the tags is instead wrapped around the region.
Alternately, one can type in part of the tag and complete it with M-TAB.

There is also code for indentation, timestamps, skeletons for new
documents, and lots of other neat features.

Uses :`visual-basic-mode'  for ASP e VBSCript
      `easymenu' for menu creation
      `cc-mode'  for javascript support
      `tempo'    for templates
Supports server (actually ASP & PHP, JSP to come) and client
(JavaScript, VBScript) scripting

Customizable flags you would like to alter

html-helper-mode-uses-visual-basic : non nil requires visual-basic-mode
html-helper-mode-uses-bold-italit : non nil creates a bold italic face (could fail if there's not such face available)

Special command (not in menu): [f4] attempts a smart narrowing to the current scripting block. Fails in client script containing server script sections.
\\{html-helper-mode-map}
Written by nelson@santafe.edu, http://www.santafe.edu/~nelson/
Mantained by lauri@eng.it, http:/www.gest.unipd.it/~saint/"
  (interactive)
  (base-html-helper-mode "ASP")
  (setq mode-name "HTML/ASP helper")
  (setq major-mode 'asp-html-helper-mode)
)))

(defun html-helper-mode ()
  "Mode for editing HTML documents.

The main function html-helper-mode provides is a menu and keybindings
for the HTML tags one inserts when writing HTML documents. Selecting
the menu item or typing the key sequence for a command inserts the
corresponding tag and places point in the right place. If a prefix
argument is supplied, the tags is instead wrapped around the region.
Alternately, one can type in part of the tag and complete it with M-TAB.

There is also code for indentation, timestamps, skeletons for new
documents, and lots of other neat features.

Uses :`visual-basic-mode' (optional - see below ) for ASP and VBScript
      `easymenu' for menu creation 
      `cc-mode'  for javascript support
      `tempo'    for templates

Supports server (actually ASP & PHP, JSP to come) and client
(JavaScript, VBScript) scripting

Customizable flags you would like to alter

`html-helper-mode-uses-visual-basic' : non nil requires visual-basic-mode and activates ASP and VBScript support functions
`html-helper-mode-uses-bold-italic' : non nil creates a bold italic face (could fail if there's not such face available)

Special command (not in menu): [f4] attempts a smart narrowing to the current scripting block. Fails in client script containing server script sections.
\\{html-helper-mode-map}
Written by nelson@santafe.edu, http://www.santafe.edu/~nelson/
Mantained by lauri@eng.it, http:/www.gest.unipd.it/~saint/
"
  (interactive)
  (base-html-helper-mode "HTML")
  (setq mode-name "HTML helper")
  (setq major-mode 'html-helper-mode))

(defun php-html-helper-mode ()
  "Mode for editing HTML documents with PHP server scripts.

The main function html-helper-mode provides is a menu and keybindings
for the HTML tags one inserts when writing HTML documents. Selecting
the menu item or typing the key sequence for a command inserts the
corresponding tag and places point in the right place. If a prefix
argument is supplied, the tags is instead wrapped around the region.
Alternately, one can type in part of the tag and complete it with M-TAB.

There is also code for indentation, timestamps, skeletons for new
documents, and lots of other neat features.

Uses :`visual-basic-mode' (optional) for ASP e VBSCript
      `easymenu' for menu creation
      `cc-mode'  for javascript support
      `tempo'    for templates

Supports server (actually ASP & PHP, JSP to come) and client
(JavaScript, VBScript) scripting

Customizable flags you would like to alter

`html-helper-mode-uses-visual-basic : non nil requires visual-basic-mode  and activates ASP and VBScript support functions
`html-helper-mode-uses-bold-italic' : non nil creates a bold italic face (could fail if there's not such face available)

Special command (not in menu): [f4] attempts a smart narrowing to the current scripting block. Fails in client script containing server script sections.
\\{html-helper-mode-map}
Written by nelson@santafe.edu, http://www.santafe.edu/~nelson/
Mantained by lauri@eng.it, http:/www.gest.unipd.it/~saint/
"
  (interactive)
  (base-html-helper-mode "PHP")
  (setq mode-name "HTML helper")
  (setq major-mode 'php-html-helper-mode))

;;}}}

;;{{{ text faces

;; By Ulrik Dickow <dickow@nbi.dk>.
;;
;; Originally aimed at Emacs 19.29.  Later on disabled syntactic fontification
;; and reordered regexps completely, to be compatible with XEmacs (it doesn't
;; understand OVERRIDE=`keep').
;;
;; We make an effort on handling nested tags intelligently.

(make-face 'info-menu-6)

;; font-lock compatibility with XEmacs/Lucid and older Emacsen (<19.29).
;;
(if (string-match "XEmacs\\|Lucid" (emacs-version))
    ;; XEmacs/Lucid
    ;; Make needed faces if the user hasn't already done so.
    ;; Respect X resources (`make-face' uses them when they exist).
    (let ((change-it
 	   (function (lambda (face)
 		       (or (if (fboundp 'facep)
 			       (facep face)
 			     (memq face (face-list)))
 			   (make-face face))
 		       (not (face-differs-from-default-p face))))))
      (if (funcall change-it 'html-helper-bold-face)
 	  (progn (make-face 'html-helper-bold-face)
 		 (make-face-bold 'html-helper-bold-face)))
      (if (funcall change-it 'html-helper-italic-face)
 	  (progn (make-face 'html-helper-italic-face)
 		 (make-face-italic 'html-helper-italic-face)))
      (if (funcall change-it 'html-helper-underline-face)
 	  (set-face-underline-p 'html-helper-underline-face t))
      (if (funcall change-it 'font-lock-variable-name-face)
 	  (set-face-foreground 'font-lock-variable-name-face "salmon"))
      (if (funcall change-it 'font-lock-reference-face)
 	  (set-face-foreground 'font-lock-reference-face "violet")))
  ;; Emacs
  ;;
  ;; Note that Emacs evaluates the face entries in `font-lock-keywords',
  ;; while XEmacs doesn't.  So XEmacs doesn't use the following *variables*,
  ;; but instead the faces with the same names as the variables.
  
  ;; New predicate on suggestion by "David J. Biesack" <sasdjb@unx.sas.com>
  (if (or (not (boundp 'emacs-major-version)) ; t if prior to 19.23
	   (< emacs-major-version 20)         ; t if prior to 20.0.0
	   (and (= emacs-major-version 20)    ; t if prior to 20.4.1
		(< emacs-minor-version 4)))    
       (progn
	(defvar html-helper-bold-face
	  (make-face 'html-helper-bold-face))
	(make-face-bold 'html-helper-bold-face)
	(defvar html-helper-italic-face
	  (make-face 'html-helper-italic-face))
	(make-face-italic 'html-helper-italic-face)
	(cond (html-helper-mode-uses-bold-italic
	       (defvar html-helper-bold-italic-face
		 (make-face 'html-helper-bold-italic-face))
	       (make-face-bold-italic 'html-helper-bold-italic-face)))
	(defvar html-helper-underline-face 
	  (make-face 'html-helper-underline-face))
	(set-face-underline-p 'html-helper-underline-face t)
	(defvar html-tag-face 
	  (make-face 'html-tag-face))
	(make-face-bold 'html-tag-face)
	;; Support for both old font-lock-background-mode and new
	;; frame-background-mode, plus a default value if neither of the two
	;; is non nil
	(let ((internal-background-mode 
	       (or (if (boundp 'font-lock-background-mode)
                  font-lock-background-mode frame-background-mode)
              (setq internal-background-mode 'light))))
	  (progn
	    (cond ((eq internal-background-mode 'light)
		   (set-face-foreground html-tag-face "dodger blue"))
		  ((eq internal-background-mode 'dark)
		   (set-face-foreground html-tag-face "deep sky blue")))
	    (cond ((eq internal-background-mode 'light)
		   (set-face-foreground html-helper-bold-face "peru"))
		  ((eq internal-background-mode 'dark)
		   (set-face-foreground 'html-helper-bold-face "wheat")))
	    (cond ((eq internal-background-mode 'light)
		   (set-face-foreground 'html-helper-italic-face "medium sea green"))
		  ((eq internal-background-mode 'dark)
		   (set-face-foreground 'html-helper-italic-face "spring green")))
	    (cond (html-helper-mode-uses-bold-italic
		   (cond ((eq internal-background-mode 'light)
			  (set-face-foreground 'html-helper-bold-italic-face "orange"))
			 ((eq internal-background-mode 'dark)
			  (set-face-foreground 'html-helper-bold-italic-face "peachpuff")))))
	    (cond ((eq internal-background-mode 'light)
		   (set-face-foreground html-helper-underline-face "goldenrod"))
		  ((eq internal-background-mode 'dark)
		   (set-face-foreground 'html-helper-underline-face "cornsilk"))))))
    ;; Use customization. I don't recall if earier version support it...
    (progn
      (defvar html-tag-face 
	(defface html-tag-face
	  '((((class color)
	      (background dark))
	     (:foreground "deep sky blue" :bold t))
	    (((class color)
	      (background light))
	     (:foreground "dodger blue" :bold t))
	    (t
	     (:foreground "dodger blue" :bold t)))
	  "Face to use for HTML tags."
	  :group 'html-helper-faces))
      (defvar html-helper-bold-face 
	(defface html-helper-bold-face
	  '((((class color)
	      (background dark))
	     (:foreground "wheat" :bold t))
	    (((class color)
	      (background light))
	     (:foreground "peru" :bold t))
	    (t
	     (:foreground "peru" :bold t)))
	  "Custom bold face."
	  :group 'html-helper-faces))
	  (defvar html-helper-italic-face 
	    (defface html-helper-italic-face
	      '((((class color)
		  (background dark))
		 (:foreground "spring green" :italic t))
		(((class color)
		  (background light))
		 (:foreground "medium sea green" :italic t))
		(t
		 (:foreground "medium sea green" :italic t)))
	      "Custom italic face."
	      :group 'html-helper-faces))
	  (cond (html-helper-mode-uses-bold-italic
		 (defvar html-helper-bold-italic-face 
		   (defface html-helper-bold-italic-face
		     '((((class color)
			 (background dark))
			(:foreground "peachpuff" :bold t:italic t))
		       (((class color)
			 (background light))
			(:foreground "orange" :bold t :italic t))
		       (t
			(:foreground "orange" :bold t :italic t)))
		     "Custom bold italic face."
		     :group 'html-helper-faces))))
	  (defvar html-helper-underline-face 
	    (defface html-helper-underline-face
	      '((((class color)
		  (background dark))
		 (:foreground "cornsilk" :underline t))
		(((class color)
		  (background light))
		 (:foreground "goldenrod" :underline t))
		(t
		 (:foreground "goldenrod" :underline t)))
	      "Custom underline face."
	      :group 'html-helper-faces))))
  ;;
  (if (string-lessp "19.28.89" emacs-version)
      ()
    ;; Emacs 19.28 and older
    ;; Define face variables that don't exist until Emacs 19.29.
    (defvar font-lock-variable-name-face 'font-lock-doc-string-face
      "Face to use for variable names -- and some HTML keywords.")
    (defvar font-lock-reference-face 'underline ; Ugly at line breaks
      "Face to use for references -- including HTML hyperlink texts.")))

(defvar html-helper-font-lock-keywords
  (let (;; Titles and H1's, like function defs.
	;;   We allow for HTML 3.0 attributes, like `<h1 align=center>'.
	(tword "\\(h1\\|title\\)\\([ \t\n]+[^>]+\\)?")
	;; Names of tags to boldify.
	(bword "\\(b\\|h[2-4]\\|strong\\)\\([ \t\n]+[^>]+\\)?")
	;; Names of tags to italify.
	(iword "\\(address\\|cite\\|em\\|i\\|var\\)\\([ \t\n]+[^>]+\\)?")
	;; Regexp to match shortest sequence that surely isn't a bold end.
	;; We simplify a bit by extending "</strong>" to "</str.*".
	;; Do similarly for non-italic and non-title ends.
	(not-bend (concat "\\([^<%?]\\|<\\([^/]\\|/\\([^bhs]\\|"
			  "b[^>]\\|"
			  "h\\([^2-4]\\|[2-4][^>]\\)\\|"
			  "s\\([^t]\\|t[^r]\\)\\)\\)\\)"))
	(not-iend (concat "\\([^<%?]\\|<\\([^/]\\|/\\([^aceiv]\\|"
			  "a\\([^d]\\|d[^d]\\)\\|"
			  "c\\([^i]\\|i[^t]\\)\\|"
			  "e\\([^m]\\|m[^>]\\)\\|"
			  "i[^>]\\|"
			  "v\\([^a]\\|a[^r]\\)\\)\\)\\)"))
	(not-tend (concat "\\([^<%?]\\|<\\([^/]\\|/\\([^ht]\\|"
			  "h[^1]\\|t\\([^i]\\|i[^t]\\)\\)\\)\\)")))
    (list ; Avoid use of `keep', since XEmacs will treat it the same as `t'.
     ;; First fontify the text of a HREF anchor.  It may be overridden later.
     ;; Anchors in headings will be made bold, for instance 
     '("<a\\s-+href[^>]*>\\([^>]+\\)</a>"
      1 font-lock-warning-face t)
     ;; Titles and level 1 headings (anchors do sometimes appear in h1's)
     (list (concat "<" tword ">\\(" not-tend "*\\)</\\1>")
	   0 'font-lock-function-name-face t)
     ;; Underline is rarely used. Only handle it when no tags inside.
     '("<u>\\([^<]*\\)</u>" 1 html-helper-underline-face t)
     ;; Forms, anchors & images (also fontify strings inside)
     '("<\\(i\\(mg\\|nput\\)\|a\\)\\>[^>\n]*>"
       0  font-lock-constant-face t)
     ;; Any tag, general rule, just after bold/italic stuff.
     ;; w3 org says that a tag is <element-name> not < element-name>
     '("\\(<[^%a=> \t][^>]*>\\)" 1 font-lock-function-name-face t)
     '("\\(<[^%a=> \t][^>\n]*>\\)" 1 html-tag-face t)
     ;; Large-scale structure keywords (like "program" in Fortran).
     ;;   "<html>" "</html>" "<body>" "</body>" "<head>" "</head>" "</form>"
     '("\\(</?\\(body\\|form\\|h\\(ead\\|tml\\)\\)[^>\n]*>\\)"
       0 font-lock-variable-name-face t)
     ;; SGML things like <!DOCTYPE ...> with possible <!ENTITY...> inside.
     '("<![a-z]+\\>[^<>]*\\(<[^>]*>[^<>]*\\)*>"
       0 font-lock-keyword-face t)
     ;; Paint [PA][HS]P skripts in font-lock-builtin-face,
     '(html-helper-match-asp-php 0 font-lock-builtin-face t t)
     ;; This one is to pick
     ;; Tag pairs like <b>...</b> etc.
     ;; Cunning repeated fontification to handle common cases of overlap.
     ;; Bold complex --- possibly with arbitrary other non-bold stuff inside.
     (list (concat "<" bword ">\\(" not-bend "*\\)</\\1>")
	   0 'html-helper-bold-face t)
     ;; Italic complex --- possibly with arbitrary non-italic kept inside.
     (list (concat "<" iword ">\\(" not-iend "*\\)</\\1>")
	   0 'html-helper-italic-face t)
     ;; Bold simple --- first fontify bold regions with no tags inside.
     (list (concat "<" bword ">\\("  "[^<]"  "*\\)</\\1>")
	   0 'html-helper-bold-face t)
     ; string stuff is pretty weird with asp. You can have strings
     ; containing asp code containing strings and empty
     ; strings. Replaced original [^\"] with this one...
     '("[=(&]?[ \t\n]*\\(\"[^\"\n]*<%[^\"\n]*\\(\"[^\"\n]*\"\\)[^\"\n]*%>[^\"\n]*\\)" 1 font-lock-string-face t)
     '("[=(&]?[ \t\n]*\\(\"[^\"\n]*\"\\)"  1 font-lock-string-face t)
     ;; HTML special characters
     '("&[a-zA-Z0-9#]+;" 0 font-lock-warning-face t)
     ; after painting strings, you have to restore asp stuff inside strings
      '("\\(<%=\\s*\\)" 1 font-lock-builtin-face t)
      '("\\(\")[^\"\n]*%>\\)" 1 font-lock-builtin-face t)
      '("\\(<%=[^%]*%>\\)" 1 font-lock-builtin-face t)
      '("\\(<\\?=\\s*\\)" 1 font-lock-builtin-face t)
      '("\\(\")[^\"\n]*\\?>\\)" 1 font-lock-builtin-face t)
      '("\\(<\\?=[^%]*\\?>\\)" 1 font-lock-builtin-face t)
      ;; That's krazy, strings higlight matches ) too, so i paint
      ;; parantheses...
      '("\\(<%\\|\\s(\\)" 1 font-lock-function-name-face t)
      '("\\(\\s)\\|%>\\)" 1 font-lock-function-name-face t)
      '("\\(<\\?\\|\\s(\\)" 1 font-lock-function-name-face t)
      '("\\(\\s)\\|\\?>\\)" 1 font-lock-function-name-face t)
      '("\\([\"]\\)" 0 font-lock-string-face t)
     ;; Comment declarations according to the HTML 2.0 spec at
     ;; <URL:http://www.w3.org/pub/WWW/MarkUp/html-spec/html-spec_3.html>.
     ;; Usually `<!-- ... -->', but also e.g the single, complete declaration
     ;; `<!--c1--  -- c2 -- -->c3 (still comment) ----c4- c4--   >'.
     ;; Note that e.g. Netscape 3.01 Gold doesn't fully live up to the spec.
    '("<!\\(--\\([^-]\\|-[^-]\\)*--\\s-*\\)*>" 0 font-lock-comment-face t)))
    "Additional expressions to highlight in HTML helper mode.")

;; internal variables

(defvar html-helper-count 0 "Counter during server script matching")

(defvar html-helper-verbose t 
  "Non nil to show a counter during server script matching")

;; Function to match an asp script (hopefully) without overflowing the regexp stack
;;
;; regexp by Larry Smith <lsmith@cio2000.eds.com>: '("<%[\000-\177]*%>" 1
;; font-lock-builtin-face). Regexp by Tony Graham <tkg@menteith.com> character coding independent:
;; '("\\(<%\\([^%]\\|%[^>]\\|\\)*%>\\)" 
;; Final regexp, now a character string can contain the ASP script end marker, i.e.
;; " foobar %> ", discards "\n" to prevent overflow.
;;
(defun html-helper-match-asp-php (last)
  (cond (html-helper-verbose
	 (message "Fontifying %s... (PHP/ASP..%s)" bufname (make-string (incf html-helper-count) ?.))))
  (cond ((search-forward-regexp "<[?%]" last t)
	 (backward-char 2)
	 (let ((beg (point)))
	   (cond ((search-forward-regexp "\\([^%?\n]\\|[%?][^>]\\|\\(\"\\([%?]>\\|.\\)*\"\\)\\)*[?%]>" last t)
		  (set-match-data (list beg (point)))
		  t )
		 (t nil))))
	(t nil)))

(defun html-helper-fontify-region (beg end verbose)
  (setq html-helper-count 0)
  (let ((loudly (and verbose
		     (> (- end beg) (/ (buffer-size) 2)))))
    (setq html-helper-verbose loudly)
    (font-lock-default-fontify-region beg end loudly)))

(set (make-local-variable font-lock-fontify-region-function) 
     'html-helper-fontify-region)

(defun html-helper-fontify-buffer ()
  (setq html-helper-count 0)
  (setq html-helper-verbose (if (numberp font-lock-verbose)
				(> (buffer-size) font-lock-verbose)
			      font-lock-verbose))
  (font-lock-default-fontify-buffer))

(set (make-local-variable font-lock-fontify-buffer-function) 
     'html-helper-fontify-buffer)

;;}}} faces

;;{{{ patterns for hilit19

;; Define some useful highlighting patterns for the hilit19 package.
;; These will activate only if hilit19 has already been loaded.
;; Thanks to <dickow@nbi.dk> for some pattern suggestions

(if (featurep 'hilit19)
    (hilit-set-mode-patterns
     'html-helper-mode
     '(("<!--" "-->" comment)
       ("<![a-z]+\\>[^<>]*\\(<[^>]*>[^<>]*\\)*>" nil comment) ;<!DOCTYPE ...>
       ("<title>" "</title>" defun)
       ("<h[1-6]>" "</h[1-6]>" bold) ;only colour inside tag
       ("<a\\b" ">" define)
       ("</a>" nil define)
       ("<img\\b" ">" include)
       ("<option\\|</?select\\|<input\\|</?form\\|</?textarea" ">" include)
       ;; First <i> highlighting just handles unnested tags, then do nesting
       ("<i>[^<]*</i>" nil italic)
       ("<b>" "</b>" bold)
       ("<i>" "</i>" italic)
       ("<u>" "</u>" underline)
       ("&[^;\n]*;" nil string)
       ;; w3 org says that a tag is <element-name> not < element-name>
       ("<[^ \t]" ">" keyword))
     nil 'case-insensitive)
  nil)
;; These are for supporting html-script. With small changes can be useful for
;; javascript


;;{{{ context guessing

;; guess where we are in indented lists based on the last list token.
;; it would be much better to try to match </ul> to <ul>, and </ol> to <ol>
;; etc, but that is pretty unwieldy and slow.
;; Note, we make select/option look like a list structure too, so indentation
;; works. This is a bit weird, but it's ok.

(defvar html-helper-any-list-item-start "<li>\\|<dt>\\|<dd>\\|<option\\|<th>\\|<td>")
(defvar html-helper-any-list-item-end "</li>\\|</dt>\\|</dd>\\|</th>\\|</td>")
(defvar html-helper-any-list-start "<dl>\\|<ul>\\|<ol>\\|<menu>\\|<dir>\\|<select\\|<table\\|<tr>")
(defvar html-helper-any-list-end "</dl>\\|</ul>\\|</ol>\\|</menu>\\|</dir>\\|</select>\\|</table>\\|</tr>")
(defvar html-helper-any-list
  (format "\\(%s\\)\\|\\(%s\\)\\|\\(%s\\)\\|\\(%s\\)"
	  html-helper-any-list-start
	  html-helper-any-list-end
	  html-helper-any-list-item-start
	  html-helper-any-list-item-end))

(defvar html-helper-indentation-list
  (format "\\(%s\\)\\|\\(%s\\)\\|\\(%s\\)"
	  html-helper-any-list-start
	  html-helper-any-list-end
	  html-helper-any-list-item-start))
(defvar html-helper-search-limit 2000 "limit on how far back we search")

;;}}}
;;{{{ indentation

(defvar html-helper-print-indent-info nil
  "If t, indent will print out information as a message.")

;; some ideas borrowed from cc-mode.el.
;; Basic logic:
;;   if this line is some sort of list token, indent according to prev context:
;;     if previous context was a list-end or item-start, use its indentation
;;     if previous context was a list start, indent forward basic-offset
;;     ignore previous list-ends, their indentation is unreliable.
;;     then if this is some sort of list-item, do special case fixups:
;;       if this is a item start or end and previous was a list end, go back
;;           item-continue-indent (the </ul> is probably indented for an <li>
;;       if this is a list end and previous was a list end, go back
;;           item-continue-indent (the </ul> is probably indented for an <li>
;;       if this is a list end and prev *not* a list end, go back basic-offset
;;   else if this line is not a list item, and previous line is a item-start
;;     indent continue-indent, because this is part of the item

;;}}}
;; These are for supporting html-script. With small changes can be useful for
;; javascript

;; Stan Lanning <lanning@pobox.com> wrote these defadvice to preserve
;; cursor position. Thank you very much Stan!
(defadvice html-script-narrow-to-asp (around save-excursion activate)
  (save-excursion
    ad-do-it))
(defadvice html-script-narrow-to-php (around save-excursion activate)
  (save-excursion
    ad-do-it))
(defadvice html-script-narrow-to-vbscript (around save-excursion activate)
  (save-excursion
    ad-do-it))
(defadvice html-script-narrow-to-javascript (around save-excursion activate)
  (save-excursion
    ad-do-it))
(defadvice html-script-release-region (around save-excursion activate)
  (save-excursion
    ad-do-it))

(defun html-script-narrow-to-asp ()
  "Narrows to an ASP script and setups visual basic mode. Does nothing if html-helper-mode-uses-visual-basic is nil"
  (interactive)
  (cond (html-helper-mode-uses-visual-basic
	 (cond ( html-helper-mode-uses-KG-style
		 (search-backward-regexp "^<%") )
	       (t (search-backward "<%" )))
	 (let ((beg (point)))
	   (cond ( html-helper-mode-uses-KG-style
		   (search-forward-regexp "^%>" ))
		 ( t (search-forward "%>" nil t)))
	   (narrow-to-region beg (point)))
	 (visual-basic-mode)
  (goto-char 0))))

(defun html-script-narrow-to-php ()
  "Narrows to an ASP script and setups c-mode"
  (interactive)
  (search-backward "<?")
  (let ((beg (point)))
    (search-forward "?>" nil t)
    (narrow-to-region beg (point)))
  (c-mode)
  (goto-char 0))

(defun html-script-narrow-to-vbscript ()
  "Narrows to a VB Script script and setups visual basic mode. Does nothing if html-helper-mode-uses-visual-basic is nil"
  (interactive)
  (cond (html-helper-mode-uses-visual-basic
	 (search-backward-regexp "<SCRIPT[ \t]+\\(LANGUAGE=\"VBScript\"\\|TYPE=\"text/vbscript\"\\)[ \t]*>")
	 (let ((beg (point)))
	   (search-forward "</SCRIPT>" nil t)
	   (narrow-to-region beg (point)))
	 (visual-basic-mode)
	 (goto-char 0))))

(defun html-script-narrow-to-javascript ()
  "Narrows to a JavaScript script and setups java mode"
  (interactive)
  (search-backward-regexp "<SCRIPT[ \t]+\\(LANGUAGE=\"JavaScript\"\\|TYPE=\"text/javascript\"\\)[ \t]*>")
  (let ((beg (point)))
    (search-forward "</SCRIPT>" nil t)
    (narrow-to-region beg (point)))
  (java-mode)
  (goto-char 0))

(defun html-script-release-region()
   "widend the window to the complete buffer and runs html-helper-mode"
   (interactive)
   (goto-char 0)
   (widen)
   (html-helper-mode))

(defun html-script-release-setup()
  (interactive)
  (local-set-key [C-f12] 'html-script-release-region))

(cond (html-helper-mode-uses-visual-basic 
      (cond 
       (visual-basic-mode-hook 
	(add-hook 'visual-basic-mode-hook 'html-script-release-setup))
       (t (setq visual-basic-mode-hook 'html-script-release-setup)))))
(cond 
 (java-mode-hook
  (add-hook 'java-mode-hook 'html-script-release-setup))
 (t (setq java-mode-hook 'html-script-release-setup)))

(cond 
 (c-mode-hook
  (add-hook 'c-mode-hook 'html-script-release-setup))
 (t (setq c-mode-hook 'html-script-release-setup)))

;; Still from Stan Lanning here it comes the code for a "smart switch" to 
;; the appropriate scripting mode.

(defvar html-script-toggle-key [f4])

(defvar html-script-narrow-alist
  `((,(regexp-quote "<%") . html-script-narrow-to-asp)
    (,(regexp-quote "<?") . html-script-narrow-to-php)
    ("<SCRIPT[ \t]+LANGUAGE=\"VBScript\"[ \t]*>" . html-script-narrow-to-vbscript)
    ("<SCRIPT[ \t]+TYPE=\"text/vbscript\"[ \t]*>" . html-script-narrow-to-vbscript)
    ("<SCRIPT[ \t]+LANGUAGE=\"JavaScript\"[ \t]*>" . html-script-narrow-to-javascript)
    ("<SCRIPT[ \t]+TYPE=\"text/javascript\"[ \t]*>" . html-script-narrow-to-javascript)))

(defvar html-script-start-regexp
  (concat "\\(" (mapconcat (lambda (x) (car x)) html-script-narrow-alist "\\|") "\\)"))

(defun html-script-toggle-narrow ()
  (interactive)
  (let ((handler-fn (save-excursion
		      (if (re-search-backward html-script-start-regexp nil t)
			  (catch 'handler-found
			    (mapcar (lambda (p)
				      (if (looking-at (car p))
					  (throw 'handler-found (cdr p))))
				    html-script-narrow-alist)
			    nil)))))
    (if handler-fn
	(apply handler-fn nil)
      (error "No script tag found"))))

(defun html-script-install-toggle-key ()
  (local-set-key html-script-toggle-key 'html-script-toggle-narrow))

(add-hook 'html-helper-mode-hook 'html-script-install-toggle-key)

(defadvice html-script-release-setup (after key-binding activate)
  (local-set-key html-script-toggle-key 'html-script-release-region))

;;}}}

;;{{{ Epilogue

(provide 'html-helper-mode)
(provide 'php-html-helper-mode)
(provide 'asp-html-helper-mode)
(provide 'html-mode)			;for 14 character filename
(run-hooks 'html-load-hook)
(run-hooks 'html-helper-load-hook)

;;}}}

;;}}}

;;; html-helper-mode.el ends here


