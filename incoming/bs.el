;;; bs.el --- menu for selecting and displaying buffers, 
;;; alternative for C-xC-b.

;;; Copyright (C) 1997-1999 Olaf Sylvester
;;;
;;; Author: Olaf Sylvester <Olaf.Sylvester@netsurf.de>
;;; Web site: http://home.netsurf.de/olaf.sylvester/emacs
;;; Keywords: extensions
;;; Version: $Id: bs.el,v 1.16  1999/10/25 23:51:39 sylvestr Exp $

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; A copy of the GNU General Public License can be obtained from this
;;; program's author (send electronic mail to kyle@uunet.uu.net) or from
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.
;;;

;;; For upgrading see web site: http://home.netsurf.de/olaf.sylvester/emacs
;;; or watch news group gnu.emacs.sources

;;; Commentary:

;; The bs-package contains a main-function bs-show for poping up a 
;; buffer in a way similar to electric-buffer-list:
;; 
;; -----------------------------------------------------------------------
;; | MR Buffer          Size  Mode          File                         |
;; | -- ------          ----  ----          ----                         |
;; |.   bs.el           14690  Emacs-Lisp    /home/sun/sylvester/el/bs.e$|
;; |  % executable.el    9429  Emacs-Lisp    /usr/share/emacs/19.34/lisp$|
;; |  % vc.el          104893  Emacs-Lisp    /usr/share/emacs/19.34/lisp$|
;; |  % test_vc.el        486  Emacs-Lisp    /home/sun/sylvester/el/test$|
;; |  % vc-hooks.el     43605  Emacs-Lisp    /usr/share/emacs/19.34/lisp$|
;; -----------------------------------------------------------------------

;;; Quick Installation und Customization:

;; (require 'bs)
;; (global-set-key "\C-x\C-b" 'bs-show) ;; or another key
;;
;; For Customization: M-x bs-customize

;;; More Commentary:

;; bs-show will generate a new buffer named *buffer-selection*, which shows
;; all buffers or a subset of them, and has possibilities for deleting, 
;; saving and selecting buffers and so on...
;; 
;; The package bs combines the advantages of the Emacs functions 
;; list-buffers and electric-buffer-list: 
;; 
;; features from function electric-buffer-list:
;;  - goto in new generated buffer       
;;  - restore window configuration when leaving buffer-selection
;;
;; features from function buffer-list
;;  - bs implements nearly all functions of buffer-list 
;;    (see following 'Use' section)
;;  - work over keyboard macros (no own eventloop)
;;  
;; new features:
;;  - cyclic navigation:
;;      - goes to top of buffer list if you are on last line and press down.
;;      - goes to end of buffer list if you are on first line and press up.
;;  - configurable list of buffers (show only file etc.)
;;  - show sorted list of buffers
;;  - you can toggle the `showing status' of a buffer with 'm'
;; 
;; Installation:
;; =============
;; You only need (if bs.el is in your load-path)
;; (require 'bs)
;; or
;; (load ".../somewhere/.../bs")
;; Bind a key to bs-show like 
;; (global-set-key "\C-x\C-b" 'bs-show)
;; 
;; Use:
;; ======
;; With bs-show a buffer named *buffer-selection* pops up and then you can
;; - choose a buffer with SPACE or RETURN
;; - choose a buffer in another window with 'o'
;; - quit selection with 'q', 'z' or C-g
;; - toggle "show all buffers" with a
;; - mark a buffer for showing always with '+'
;; - toggle configurations with 'c'
;; - save a buffer with 's'
;; - toggle read-only with '%'
;; - mark unmodified with '~'
;; - delete a buffer with 'd'
;; 
;; Overview
;; --------
;; Select a buffer                  : SPACE or RETURN.
;; Navigate                         : UP or DOWN, 'p' or 'n'
;; Fast navigate                    : '1' upto '9' and navigation keys.
;; Toggle `show all'                : 'a'
;; Show in one window               : '!'
;; Select in other window and remain in bs. : C-o
;; Show in other window             : 'o'
;; Visit tags                       : 't'
;; Save                             : 's'
;; Delete                           : 'd'
;; Step through configurations      : 'c'
;; Quit                             : 'q' or 'z' or C-g
;; Bury buffer                      : 'b'
;; Toggle read-only                 : '%'
;; Set `not modified'               : '~'
;; Show list sorted in several ways : 'S'
;; Marking                          : 'm'
;; Unmarking                        : 'u'
;; Mark to show always              : '+'
;; Mark to show never               : '-'
;; View buffer                      : 'v'

;;; Cycling through buffers

;; If you want to cycle through buffer list you can use 
;; bs-cycle-next or bs-cycle-previous.
;; Bind these function to a key like
;;   (global-set-key [(f9)]     'bs-cycle-previous)
;;   (global-set-key [(f10)]    'bs-cycle-next)
;;
;; Cycling through buffers ignored sorting because sorting destroys
;; the logical buffer list. If buffer list is sorted by size you 
;; won't cycle to last buffer 
 
;;; Customization:

;; There is an customization group called bs in group convenience
;; Start customization by M-x bs-customize
 
;; Configurations
;; ==============
;; You can define configurations by defining a function, which sets the
;; variables for configuration. There are some predefined configuration
;; (see list bs-configurations). Give your configuration a name and add
;; a new entry to bs-configurations. Activate your configuration with
;;      (bs-set-configuration "your-configuration-name")
;; You can change a configuration with 
;;     M-x bs-set-configuration
;; or
;;     pressing key 'c' in buffer *buffer-selection*
;; 
;; The default configuration is the configuration whose name is in variable
;; bs-default-configuration (here "files" which means to show only files)
;;  
;; if you always want to see all buffers, you have to set
;;    (setq bs-default-configuration "all")
;; 
;; 
;; How do I configure?
;; -------------------
;; ;; If you don't want to see internal buffers beginning with '*'
;; ;; you have to set
;; (setq bs-dont-show-regexp "^\\*")
;;
;; 
;; ;; If you don't want to see internal buffers beginning with '*'
;; ;; but you want to see buffer *scratch* then:
;; (setq bs-dont-show-regexp "^\\*")
;; (setq bs-must-show-regexp "^\\*scratch\\*")
;; 
;;
;; ;; If you want to show only buffers containing a file then
;; ;; you have to set
;; (setq bs-dont-show-function 'bs-visits-non-file)
;; 
;; Configure sorting
;; -----------------
;; You can define functions for sorting the buffer list.
;; When selecting buffers, you can step through available sorting 
;; methods with key 'S'.
;; To define a new way of sorting, for example:
;; (bs-define-sort-function 
;;    "by something" 
;;    (function my-sort-function))
;; 
;; There are four basic functions for sorting: 
;;   by buffer name, by mode, by size or by filename
;; 
;; Finally
;; -------
;; My favorite configuration:
;; 
;; ;; I want to see *-buffers at the end
;; (setq bs-buffer-sort-function 'bs-sort-buffer-interns-are-last)
;; 
;; ;; Don't show files which don't belong to a file
;; (setq bs-dont-show-function 'bs-visits-non-file)
;; 
;; ;; But show buffer named "*scratch*"
;; (setq bs-must-show-regexp "^\\*scratch\\*")
;; 
;; 
;; More about cycling:
;; When cycling through buffer list the functions for cycling will use 
;; the current configuration of bs to calculate the buffer list.
;; If you want to use a different configuration for cycling you have to set
;; the variable bs-cycle-configuration-name
;;
;; (setq bs-cycle-configuration-name "files-and-scratch")
;;  and you can cycle through all file buffers and *scratch* although 
;; you current configuration perhaps is "files"
;; 
;;; Change Log:
			 

;; $Log: bs.el,v $
;; Revision 1.16  1999/10/25 23:51:39  sylvestr
;; 
;; !!!!!!!! !!!!!!!! !!!!!!!! !!!!!!!! !!!!!!!! !!!!!!!! !!!!!!!! 
;; Removed (global-set-key "\C-x\C-b" 'bs-show) from package bs
;; !!!!!!!! !!!!!!!! !!!!!!!! !!!!!!!! !!!!!!!! !!!!!!!! !!!!!!!! 
;; 
;; - many changes because of adapting electric-buffer-list behavior
;; - Customization:
;;   - Funktion `bs-customize' for customization
;;   - customization group bs in group convenience
;; 
;; - multiple selection of buffer (like in buffer-menu or 
;;   electric-buffer-list):    mark buffer by key 'm'
;;                           unmark buffer by key 'u'
;; - faster bs-delete command
;; - signal an error if on header line
;; - wrapped some long lines
;; - full redesign of prefix argument handling:
;;   bs-show       : use current configuration
;;   M-<n> bs-show : use n'th configuration of bs-configurations
;;   C-u bs-show   : use configuration bs-alternative-configuration 
;;                   (default is "all")
;; - every key 0 upto 9 is a digit argument for faster navigation
;; - key '-' is negative-argument for faster navigation with '0' to '9'
;; - for selection in other window use key o.  
;; - for selection in one window (close others) use key !.
;; - new key C-o: select line's buffer in other window and remain in bs.
;; - new function bs-view bound to key 'v'
;; - small change of bs-delete: additional go to next line
;;   Therefore new function bs-delete-backward bound to key 'C-d'
;; 
;; 
;; Revision 1.15  1999/10/12 22:01:01  sylvestr
;; - new function bs-select-in-one-window
;; - new function bs-visit-tags-table: Visit the tags table of 
;;   current buffer
;; - new key binding for key t   -> visit tags
;; - new key binding for key 1   -> select in one window
;; - new key binding for key 2   -> select in other window
;; - new key binding for C-c C-c -> leave buffer selection like q
;; - bs-show now supports C-u    -> show all buffers
;; - new variable bs-must-always-show-regexp regular expression
;;   to specify buffers that must always be shown regardless of 
;;   the configuration.
;; - now function bs-refresh exists.
;; - avoid font lock messages.
;; 
;; Revision 1.14  1999/10/5 19:34:01  sylvestr
;; - font lock integration
;; - new customization variable bs-max-window-height
;; - new function bs-select-other-window bound to key 4 and key o
;; 
;; Revision 1.13  1999/5/10 22:10:51  sylvestr
;; - Bug in bs-cycle-* removed.
;; - Minor bug in bs--insert-one-entry removed.
;; - Now cycling takes current buffer into account even if he isn't 
;;   in buffer list of bs.
;; - More messages in status line.
;; - now bs--format-aux doesn't use function format to format 
;;   a string; so text properties are saved.
;; 
;; $Log: bs.el,v $
;; Revision 1.12  1999/4/12 23:30:11  sylvestr
;; Main changes:
;; - new interactive functions bs-cycle-next and bs-cycle-previous !!!
;; - new way of displaying buffer attributes (variable bs-attributes-list)
;; 
;; Details
;; - New functions for selecting a buffer in a new frame.
;; - New interactive function bs-cycle-next
;; - New interactive function bs-cycle-previous
;; - New function bs-previous-buffer
;; - New function bs-next-buffer
;; - Now it's possible to configure how a buffer appear in a line:
;;   Variable for customization: bs-attributes-list
;; - New constant bs-header-lines-length instead of bs-header-lines
;; 
;; 
;; Revision 1.11  1999/1/6 00:01:11  sylvestr
;; - some XEmacs integrations (thanks Matthias Helmling)
;; 
;; Revision 1.10  1998/11/30 22:51:11  sylvestr
;; - Totally overwork of sorting possibilities which override the 
;;   normal way of sorting. There exists a settled sort function 
;;   which won't be removed after leaving buffer selection.
;;   Turning into a new buffer selection action, the old override 
;;   of sorting will be used.
;; - supports any methods for sorting buffer list 
;; - new user function bs-define-sort-function
;; 
;; $Log: bs.el,v $
;; Revision 1.9  1998/10/12 22:50:01  sylvestr
;; - bs-up and bs-down work with positive numeric arguments.
;; - new variables for user configuration: 
;;          bs-minimal-buffer-name-column
;;          bs-maximal-buffer-name-column
;; - new function bs-set-current-buffer-to-show-always
;; - new function bs-set-current-buffer-to-show-never
;; - new function bs-help
;; 
;; 
;; Third state for marking buffers.
;; Now: normally, never and always
;;
;; Revision 1.7  1998/03/12 22:59:55  sylvestr
;; Implementing new features:
;; - marking/unmarking buffers for showing by 'm'
;; - set configurations by 'c'
;; - more comments
;;
;; Revision 1.6  1997/11/08 16:28:09  sylvestr
;; Insert code for toggle show-all.
;; New binding for key 'a'.
;;
;; Revision 1.5  1997/11/08 01:04:03  sylvestr
;; Fixed problems deleting current buffer.
;; Correct window height after deleting.
;; Insert save-window-excursion at vc-toggle-read-only.
;;
;; Revision 1.4  1997/11/08 00:27:22  sylvestr
;; New key definitions for 'n' and 'p'.
;; Variable truncate-lines set to t.
;;
;; Revision 1.3  1997/11/05 20:40:22  sylvestr
;; Always show buffer we are starting bs-show from.
;; Insert new email.
;;
;; Revision 1.2  1997/11/01 20:07:59  sylvestr
;; Functions bs-sort-buffer-interns-are-last and
;; bs-visits-non-file now belong to this package.
;;
;; Revision 1.1  1997/11/01 20:02:02  sylvestr
;; Initial revision
;;
;; Thanks for suggestions:
;;
;; Christian Mondrup
;; Christoph Conrad
;; Diego Calvanese
;; John Wiegly
;; Juanma Barranquero
;; Kai Grossjohann
;; Kin Cho
;; Matthias Helmling
;; Nick Sieger
;; Nishina Shigeaki
;; Richard Everson
;; 
;; dizhao@mjordan.hq.primeon.com
;; 
;;; Code:

;;; ----------------------------------------------------------------------
;;; Globals for customization
;;; ----------------------------------------------------------------------

(defgroup bs nil
  "Buffer Selection: Maintaining buffers by buffer menu."
  :group 'convenience
  )

(defgroup bs-appearence nil
  "Buffer Selection appearence: Appearence of bs buffer menu."
  :group 'bs
  )

(defcustom bs-attributes-list
  '(
    (""       1   1 left  bs--get-current-p)
    ("M"      1   1 left  bs--get-m)
    ("R"      2   2 left  bs--get-r)
    ("Buffer" bs--get-name-length 10 left  bs--get-name)
    (""       1   1 left  " ")
    ("Size"   8   8 right bs--get-size)
    (""       1   1 left  " ")
    ("Mode"   12 12 right bs--get-mode-name)
    (""       2   2 left  "  ")
    ("File"   12 12 left  bs--get-file-name)
    (""       2   2 left  "  ")
    )
  "List of attribute specifications which describe how to present 
a buffer in bs. Each entry specifies a column and is a list of:
  HEADER         : string for header for first line or a function 
                   which calculates header string.
  MINIMUM LENGTH : minimum length of column (number or name of function)
  MAXIMUM LENGTH : maximum length of column (number or name of function) 
                   (currently ignored)
  ALIGNMENT      : alignment of colum: (left right middle)
  FUNCTION       : Name of an function for calculating the value.
  and the rest for paramaters for the function.
The function gets following parameters:
  - the buffer we have started buffer selection
  - the list of all buffers to show.
"
  :group 'bs-appearence
  :type '(repeat sexp))


(defvar bs--running-in-xemacs (string-match "XEmacs" (emacs-version))
  "Whether we are running under XEmacs")

(defun bs--make-header-match-string ()
  (let ((res "^\\(")
	(ele  bs-attributes-list)
	)
    (while ele
      (setq res (concat res (car (car ele)) " *"))
      (setq ele (cdr ele)))
    (concat res "$\\)"))
  )

;;; Font-Lock-Settings
(defvar bs-mode-font-lock-keywords
  (list
   ;; header in font-lock-type-face
   (list (bs--make-header-match-string)
	 '(1 font-lock-type-face append) '(1 'bold append))
   ;; Buffername embedded by *
   (list "^\\(.*\\*.*\\*.*\\)$"    
	 1 (if bs--running-in-xemacs 
	       ;; problem in XEmacs with font-lock-constant-face
	       (if (facep 'font-lock-constant-face)
		   'font-lock-constant-face
		 'font-lock-comment-face)
	     'font-lock-constant-face))
   ;; Dired-Buffers
   '("^..\\(.*Dired by .*\\)$" 1 font-lock-function-name-face) 
   ;; the star for modified buffers
   '("^.\\(\\*\\) +[^\\*]"     1 font-lock-comment-face) 
   )
  "Default font lock expressions for buffer selection menu mode.")

(defcustom bs-max-window-height 20
  "Maximal window height of *buffer-selection*"
  :group 'bs-appearence
  :type 'integer
  )

(defvar bs-dont-show-regexp nil
  "Regular expression for specifing buffers by name 
which must not be shown.")

(defvar bs-must-show-regexp nil
  "Regular expression for specifing buffers by name which must be shown.
Note that this variable is temporary: if the configuration is changed 
it is reset to nil. Use bs-must-always-show-regexp to specify buffers 
that must always be shown regardless of the configuration.")

(defcustom bs-must-always-show-regexp nil
  "Regular expression for specifing buffers by name which must always be 
shown regardless of the configuration."
  :group 'bs
  :type '(choice (const :tag "Nothing at all" nil) regexp))

(defvar bs-dont-show-function nil
  "Function (predicate), for specifing buffers which must not be shown.")

(defvar bs-must-show-function nil
  "Function (predicate), for specifing buffers which must be shown.")

(defvar bs-buffer-sort-function nil
  "Sort function to sort the buffers that appear in *buffer-selection*.")

(defcustom bs-maximal-buffer-name-column 45
  "Maximum width of column for buffer names."
  :group 'bs-appearence
  :type 'integer)

(defcustom bs-minimal-buffer-name-column 15
  "Minimum width of column for buffer names."
  :group 'bs-appearence
  :type 'integer)

(defconst bs-header-lines-length 2
  "Number of headerlines.")

(defvar bs-configurations 
  '(
    ("all"                . bs-config--all)
    ("files"              . bs-config--only-files)
    ("files-and-scratch"  . bs-config--files-and-scratch)
    ("all-intern-last"    . bs-config--all-intern-last)
    )
  "Alist of CONFIGURATION-NAMEs and the FUNCTION-NAMEs to set the
corresponding configurations.")

(defcustom bs-default-configuration "files"
  "Name of default configuration used by bs.
Will be changed using key 'c'.
Must be a string of names used in `bs-configurations'."
  :group 'bs
  :type 'string)

(defcustom bs-alternative-configuration "all"
  "Name of alternative configuration which is used using 
function bs-show with universal-argument C-u. 
Must be a string of names used in `bs-configurations'."
  :group 'bs
  :type  'string)

(defvar bs-current-configuration bs-default-configuration
  "Name of current configuration. 
Must be a string of names used in `bs-configurations'.")

(defcustom bs-cycle-configuration-name
  nil
  "Name of configuration which is used when cycle through buffer list.
NIL means using the current configuration.
Must be a string of names used in `bs-configurations'."
  :group 'bs
  :type '(choice 
	  (const :tag "like current configuration" nil) 
	  string))

(defcustom bs-string-show-always    "+"
  "String of length 1 for indicating: buffer will always be shown."
  :group 'bs-appearence
  :type 'string)

(defcustom bs-string-show-never     "-"
  "String of length 1 for indicating: buffer will never be shown."
  :group 'bs-appearence
  :type 'string)

(defcustom bs-string-current        "."
  "String of length 1 for indicating: buffer is current buffer."
  :group 'bs-appearence
  :type 'string)

(defcustom bs-string-current-marked        "#"
  "String of length 1 for indicating: buffer is current and marked buffer."
  :group 'bs-appearence
  :type 'string)

(defcustom bs-string-marked        ">"
  "String of length 1 for indicating: buffer is marked."
  :group 'bs-appearence
  :type 'string)

(defcustom bs-string-show-normally  " "
  "String of length 1 for indicating: buffer will be shown normally."
  :group 'bs-appearence
  :type 'string)

(defvar bs--name-entry-length 20
  "Temporary length of buffer names.")

;;; ----------------------------------------------------------------------
;;; Intern globals
;;; ----------------------------------------------------------------------

(defvar bs-buffer-show-flag nil
  "Flag for the current mode for showing this buffer.")

(make-variable-buffer-local 'bs-buffer-show-flag)

;; Make face named region (for XEmacs)
(if (facep 'region)
    nil
  (make-face 'region)
  (set-face-background 'region "gray75"))


(defun bs--sort-by-name (b1 b2)
  (string< (buffer-name b1) 
	   (buffer-name b2)))

(defun bs--sort-by-filename (b1 b2)
  (string< (or (buffer-file-name b1) "")
	   (or (buffer-file-name b2) "")))

(defun bs--sort-by-mode (b1 b2)
  (save-excursion
    (string< (progn (set-buffer b1) (format "%s" major-mode))
	     (progn (set-buffer b2) (format "%s" major-mode)))))

(defun bs--sort-by-size (b1 b2)
  (save-excursion
    (< (progn (set-buffer b1) (buffer-size))
       (progn (set-buffer b2) (buffer-size)))))

(defcustom bs-sort-functions
  '()
  "Inventary of all possible sorting aspects as alist."
  :group 'bs
  :type '(repeat sexp)
  )

(defun bs-define-sort-function (name function &optional 
				     regexp-for-sorting face)
  "Defines a new function FUNCTION (a predicate) for sorting 
with the name NAME."
  (let ((tupel (assoc name bs-sort-functions))
	)
    (if tupel
	(setcdr tupel (list function regexp-for-sorting face))
      (setq bs-sort-functions 
	    (cons (list name function regexp-for-sorting face)
		  bs-sort-functions)))))

;; Define a sorting which does nothing.
(bs-define-sort-function 
 "by nothing" nil nil) 
  
(defcustom bs-default-sort-name "by nothing"
  "Name of sort behavior.
A string of cars of list `bs-sort-functions'.
Default is \"by nothing\" which means no sorting."
  :group 'bs
  :type  'string)

;; Define some more sortings.
(bs-define-sort-function 
 "by filename" (function bs--sort-by-filename) "File" 'region)
(bs-define-sort-function 
 "by mode" (function bs--sort-by-mode) "Mode" 'region)
(bs-define-sort-function 
 "by size" (function bs--sort-by-size) "Size" 'region)
(bs-define-sort-function 
 "by name" (function bs--sort-by-name) "Buffer" 'region)

(defvar bs--current-sort-function 
  (assoc bs-default-sort-name bs-sort-functions)
  "Description of the current function for sorting the buffer list.")

(defvar bs--buffer-coming-from nil)

(defvar bs--show-all nil)

(defvar bs--window-config-coming-from nil)

(defvar bs--intern-show-never "^ \\|\\*buffer-selection\\*")

(defvar bs-current-list nil
  "Intern list of buffers shown in buffer *buffer-selection*
and corresponding to lines of buffer.")

(defvar bs--marked-buffers nil
  "Currently marked buffers.")

(defvar bs-mode-map ()
  "Keymap of bs-mode.")

(if bs-mode-map
    ()
  (setq bs-mode-map (make-sparse-keymap))
  (define-key bs-mode-map " "       'bs-select)
  (define-key bs-mode-map "f"       'bs-select)
  (define-key bs-mode-map "v"       'bs-view)
  (define-key bs-mode-map "!"       'bs-select-in-one-window)
  (define-key bs-mode-map [mouse-2] 'bs-mouse-select) ;; for GNU EMACS
  (define-key bs-mode-map [button2] 'bs-mouse-select) ;; for XEmacs
  (define-key bs-mode-map "F"       'bs-select-other-frame)

  (let ((key ?1))
    (while (<= key ?9)
      (define-key bs-mode-map (char-to-string key) 'digit-argument)
      (setq key (1+ key))
      ))
  (define-key bs-mode-map "-"       'negative-argument)
  (define-key bs-mode-map "\e-"     'negative-argument)

  (define-key bs-mode-map "o"       'bs-select-other-window)
  (define-key bs-mode-map "\C-o"    'bs-tmp-select-other-window)
  ;; for GNU EMACS
  (define-key bs-mode-map [mouse-3] 'bs-mouse-select-other-frame) 
  ;; for XEmacs
  (define-key bs-mode-map [button3] 'bs-mouse-select-other-frame) 
  (define-key bs-mode-map [up]      'bs-up)
  (define-key bs-mode-map "n"       'bs-down)
  (define-key bs-mode-map "p"       'bs-up)
  (define-key bs-mode-map [down]    'bs-down)
  (define-key bs-mode-map "\C-m"    'bs-select)
  (define-key bs-mode-map "b"       'bs-bury-buffer)
  (define-key bs-mode-map "s"       'bs-save)
  (define-key bs-mode-map "S"       'bs-show-sorted)
  (define-key bs-mode-map "a"       'bs-toggle-show-all)
  (define-key bs-mode-map "d"       'bs-delete)
  (define-key bs-mode-map "\C-d"    'bs-delete-backward)
  (define-key bs-mode-map "k"       'bs-delete)
  (define-key bs-mode-map "g"       'bs-refresh)
  (define-key bs-mode-map "C"       'bs-set-configuration-and-refresh)
  (define-key bs-mode-map "c"       'bs-select-next-configuration)
  (define-key bs-mode-map "q"       'bs-kill)
  (define-key bs-mode-map "z"       'bs-kill)
  (define-key bs-mode-map "\C-c\C-c" 'bs-kill)
  (define-key bs-mode-map "\C-g"    'bs-abort)
  (define-key bs-mode-map "\C-]"    'bs-abort)
  (define-key bs-mode-map "%"       'bs-toggle-readonly)
  (define-key bs-mode-map "~"       'bs-clear-modified)
  (define-key bs-mode-map "M"       'bs-toggle-current-to-show)
  (define-key bs-mode-map "+"       'bs-set-current-buffer-to-show-always)
  ;;(define-key bs-mode-map "-"       'bs-set-current-buffer-to-show-never)
  (define-key bs-mode-map "t"       'bs-visit-tags-table)
  (define-key bs-mode-map "m"       'bs-mark-current)
  (define-key bs-mode-map "u"       'bs-unmark-current)
  (define-key bs-mode-map ">"       'scroll-right)
  (define-key bs-mode-map "<"       'scroll-left)
  (define-key bs-mode-map "\e\e"    nil)
  (define-key bs-mode-map "\e\e\e"  'bs-kill)
  (define-key bs-mode-map [escape escape escape] 'bs-kill)
  (define-key bs-mode-map "?"       'bs-help)
  )

;;; ----------------------------------------------------------------------
;;; Functions
;;; ----------------------------------------------------------------------

(defun bs-buffer-list (&optional list sort-description)
  "Return list of buffers to be shown.
If SORT-DESCRIPTION isn't nil, the list will be sorted by 
a special function."
  (setq sort-description (or sort-description bs--current-sort-function))
  (if (null list)
      (setq list (buffer-list)))
  (let ((result nil))
    (while list
      (let* ((buffername (buffer-name (car list)))
	     (intern-show-never
	      (string-match bs--intern-show-never buffername))
	     (extern-show-never
	      (and bs-dont-show-regexp
		   (string-match bs-dont-show-regexp buffername)))
	     (extern-must-show 
	      (or (and bs-must-always-show-regexp 
		       (string-match bs-must-always-show-regexp buffername))
		  (and bs-must-show-regexp 
		       (string-match bs-must-show-regexp buffername))))
	     (extern-show-never-from-fun 
	      (and bs-dont-show-function
		   (funcall bs-dont-show-function (car list))))
	     (extern-must-show-from-fun 
	      (and bs-must-show-function
		   (funcall bs-must-show-function (car list))))
	     (show-flag (save-excursion 
			  (set-buffer (car list)) 
			  bs-buffer-show-flag))
	     )
	(if (or (eq show-flag 'always)
		(and (or bs--show-all (not (eq show-flag 'never)))
		     (not intern-show-never)
		     (or bs--show-all
			 extern-must-show 
			 extern-must-show-from-fun
			 (and (not extern-show-never)
			      (not extern-show-never-from-fun)))))
	    (setq result (cons (car list)
			       result)))
	(setq list (cdr list))))
    (setq result (reverse result))
    ;; buffer we are coming from should be shown in list, so
    ;; that we can leave with space and be back in the buffer we 
    ;; started bs-show from
    (if (and bs--buffer-coming-from
	     (buffer-live-p bs--buffer-coming-from)
	     (not (memq bs--buffer-coming-from result)))
	(setq result (cons bs--buffer-coming-from result)))
    ;; sorting
    (if (and sort-description 
	     (nth 1 sort-description))
	(setq result (sort result (nth 1 sort-description)))
      ;; standard sorting
      (bs-buffer-sort result))
      ))

(defun bs-buffer-sort (buffer-list)
  (if bs-buffer-sort-function
      (sort buffer-list bs-buffer-sort-function)
    buffer-list))

(defun bs--redisplay (&optional keep-line-p sort-description)
  (let ((line (1+ (count-lines 1 (point)))))
    (bs-show-in-buffer (bs-buffer-list nil sort-description))
    (if keep-line-p
	(goto-line line))
    (beginning-of-line)))

(defun bs--goto-current-buffer ()
  "Goto line which represents the current buffer;
actually the line which begins with character in bs-string-current or
bs-string-current-marked."
  (let (point)
    (save-excursion
      (goto-char (point-min))
      (if (search-forward-regexp 
	   (concat "^" (regexp-quote bs-string-current)
		   "\\|^" (regexp-quote bs-string-current-marked))
	   nil t)
	  (setq point (- (point) 1))))
    (if point
	(goto-char point))))

(defun bs--current-config-message ()
  (if bs--show-all
      "Show all buffers."
    (format "Show buffer by configuration %S" 
	    bs-current-configuration)))

(defun bs-mode ()
  "Major mode for bs."
  (interactive)
  (kill-all-local-variables)
  (use-local-map bs-mode-map)
  ;;(make-local-variable 'bs--name-entry-length)
  (setq major-mode 'bs-mode
	mode-name "Buffer-Selection-Menu"
	buffer-read-only t
	truncate-lines t)
  (set (make-local-variable 'font-lock-defaults)
       '(bs-mode-font-lock-keywords t))
  (set (make-local-variable 'font-lock-verbose)
       nil)
  (run-hooks 'bs-mode-hook))

(defun bs-kill ()
  "Let buffer disappear and reset window-configuration."
  (interactive)
  (bury-buffer (current-buffer))
  (set-window-configuration bs--window-config-coming-from)
  )

(defun bs-abort ()
  "Make ding and go back to configuration coming from."
  (interactive)
  (ding)
  (bs-kill))

(defun bs-set-configuration-and-refresh ()
  "Ask user for a configuration and apply selected configuration."
  (interactive)
  (call-interactively 'bs-set-configuration)
  (bs--redisplay t))

(defun bs-refresh ()
  "Refresh buffer."
  (interactive)
  (bs--redisplay t))

(defun bs--window-for-buffer (buffer-name)
  "Return window (in current frame) which shows a buffer whose name is
BUFFER-NAME; or nil if there isn't any."
  (let ((window nil))
    (walk-windows (function (lambda (wind)
			      (if (string= (buffer-name 
					    (window-buffer wind))
					   buffer-name)
				  (setq window wind)))))
    window))

(defun bs--set-window-height ()
  "Correct the height of window to a suitable height."
  (if (not (one-window-p (selected-window)))
      (shrink-window 
       (- (window-height (selected-window))
	  ;; window-height in xemacs includes mode-line
	  (+ (if bs--running-in-xemacs 3 1)
	     bs-header-lines-length
	     (min (length bs-current-list) bs-max-window-height)
	     )))))

(defun bs--current-buffer ()
  "Return buffer on current position."
  (beginning-of-line)
  (let ((line (+ (- bs-header-lines-length) 
		 (count-lines 1 (point)))))
    (if (< line 0)
	(error "You are on a header row."))
    (nth line bs-current-list)))


(defun bs--update-current-line ()
  (let ((buffer (bs--current-buffer))
	(inhibit-read-only t))
    (beginning-of-line)
    (delete-region (point) (save-excursion (end-of-line)(point)))
    (bs--insert-one-entry buffer)
    (beginning-of-line)
    ))

(defun bs-view ()
  "View buffer on current line by function view-buffer."
  (interactive)
  (let ((buffer (bs--current-buffer)))
    (view-buffer buffer)
    ))

(defun bs-select ()
  "Do action by selection: Restore window-configuration and 
select buffer on current line."
  (interactive)
  (let ((buffer (bs--current-buffer)))
    (bury-buffer (current-buffer))
    (set-window-configuration bs--window-config-coming-from)
    (switch-to-buffer buffer)
    (if bs--marked-buffers
	;; Some marked buffers for selection
	(let* ((all (delq buffer bs--marked-buffers))
	       (height (/ (1- (frame-height)) (1+ (length all))))
	       )
	  (delete-other-windows)
	  (switch-to-buffer buffer)
	  (while all
	    (split-window nil height)
	    (other-window 1)
	    (switch-to-buffer (car all))
	    (setq all (cdr all)))
	  ;; goto window we have started bs.
	  (other-window 1)
	  ))
    ))

(defun bs-select-other-window ()
  "Do action by selection: Restore window-configuration
and select buffer on current line in other window"
  (interactive)
  (let ((buffer (bs--current-buffer)))
    (bury-buffer (current-buffer))
    (set-window-configuration bs--window-config-coming-from)
    (switch-to-buffer-other-window buffer)
    ))
 
(defun bs-tmp-select-other-window ()
  "Do action by selection: select buffer on current line 
in other window. Stay in bs."
  (interactive)
  (let ((buffer (bs--current-buffer)))
    (display-buffer buffer t)
    ))
 
(defun bs-select-other-frame ()
  "Do action by selection: Restore window-configuration in current frame
and select buffer on current line in a new created frame"
  (interactive)
  (let ((buffer (bs--current-buffer)))
    (bury-buffer (current-buffer))
    (set-window-configuration bs--window-config-coming-from)
    (switch-to-buffer-other-frame buffer)
    ))

(defun bs-mouse-select-other-frame (event)
  "Select buffer in a new created frame on current mouse-click."
  (interactive "e")
  (mouse-set-point event)
  (bs-select-other-frame))

(defun bs-mouse-select (event)
  "Select buffer on current mouse-click."
  (interactive "e")
  (mouse-set-point event)
  (bs-select))

(defun bs-select-in-one-window ()
  "Do action by selection: Restore window-configuration
and select buffer on current line in one window. Kill other windows."
  (interactive)
  (bs-select)
  (delete-other-windows))

(defun bs-bury-buffer ()
  "Bury buffer on current line."
  (interactive)
  (bury-buffer (bs--current-buffer))
  (bs--redisplay t))

(defun bs-save ()
  "Save buffer on current line."
  (interactive)
  (let ((buffer (bs--current-buffer)))
    (save-excursion
      (set-buffer buffer)
      (save-buffer))
    (bs--update-current-line)))

(defun bs-visit-tags-table ()
  "Visit the tags table in the buffer on this line.
See `visit-tags-table'."
  (interactive)
  (let ((file (buffer-file-name (bs--current-buffer))))
    (if file
	(visit-tags-table file)
      (error "Specified buffer has no file"))))

(defun bs-toggle-current-to-show ()
  "Toggle status of showing flag for buffer in current line."
  (interactive)
  (let ((buffer (bs--current-buffer))
	res)
    (save-excursion
      (set-buffer buffer)
      (setq res (cond ((null bs-buffer-show-flag)
		       'never)
		      ((eq bs-buffer-show-flag 'never)
		       'always)
		      (t nil)))
      (setq bs-buffer-show-flag res))
    (bs--update-current-line)
    (bs--set-window-height)
    (bs--show-config-message res)))

(defun bs-set-current-buffer-to-show-always ()
  "Toggle status of buffer on line to `always shown'."
  (interactive)
  (bs--set-toggle-to-show (bs--current-buffer) 'always))

(defun bs-set-current-buffer-to-show-never ()
  "Toggle status of buffer on line to `never shown'."
  (interactive)
  (bs--set-toggle-to-show (bs--current-buffer) 'never))

(defun bs--set-toggle-to-show (buffer what)
  "Set status of showing flag to WHAT for buffer BUFFER."
  (save-excursion
    (set-buffer buffer)
    (setq bs-buffer-show-flag what))
  (bs--update-current-line)
  (bs--set-window-height)
  (bs--show-config-message what))

(defun bs-mark-current ()
  (interactive)
  (let ((buffer (bs--current-buffer)))
    (if buffer 
	(setq bs--marked-buffers (cons buffer bs--marked-buffers)))
    (bs--update-current-line)
    (bs-down 1)
    ))

(defun bs-unmark-current ()
  (interactive)
  (let ((buffer (bs--current-buffer)))
    (if buffer 
	(setq bs--marked-buffers (delq buffer bs--marked-buffers)))
    (bs--update-current-line)
    (bs-down 1)
    ))

(defun bs--show-config-message (what)
  "Show message indicating the new showing status for a buffer."
  (bs-message-without-log
   (cond ((null what)
	  "Buffer will be shown normally.")
	 ((eq what 'never)
	  "Mark buffer to never be shown.")
	 (t "Mark buffer to show always."))))

(defun bs-delete ()
  "Kill buffer on current line."
  (interactive)
  (let ((current (bs--current-buffer))
	(inhibit-read-only t))
    (setq bs-current-list (delq current bs-current-list))
    (kill-buffer current)
    (beginning-of-line)
    (delete-region (point) (save-excursion 
			     (end-of-line)
			     (if (eobp) (point) (1+ (point)))))
    (if (eobp)
	(progn
	  (backward-delete-char 1)
	  (beginning-of-line)
	  (recenter -1))
      )
    (bs--set-window-height)))

(defun bs-delete-backward ()
  "Like `bs-delete' but goes to buffer in front of current."
  (interactive)
  (let ((on-last-line-p (save-excursion (end-of-line) (eobp))))
    (bs-delete)
    (if (not on-last-line-p)
	(bs-up 1))))

(defun bs-show-sorted ()
  "Show buffer list sorted by buffer name."
  (interactive)
  (setq bs--current-sort-function
	(bs-next-config-aux (car bs--current-sort-function)
			    bs-sort-functions))
  (bs--redisplay)
  (bs--goto-current-buffer)
  (bs-message-without-log
   "Sorted %s" (car bs--current-sort-function))
  )

(defun bs-apply-sort-faces (&optional sort-description)
  "Set text properties for the sort described by sort-description."
  (let ((sort-description (or sort-description
			      bs--current-sort-function)))
    (save-excursion
      (goto-char (point-min))
      (if (and window-system
	       (nth 2 sort-description)
	       (search-forward-regexp (nth 2 sort-description) nil t))
	  (let ((inhibit-read-only t)
		)
	    (put-text-property (match-beginning 0)
			       (match-end 0)
			       'face 
			       (or (nth 3 sort-description) 
				   'region)))))))

(defun bs-toggle-show-all ()
  "Toggle show all buffers / show buffers with current configuration."
  (interactive)
  (setq bs--show-all (not bs--show-all))
  (bs--redisplay)
  (bs--goto-current-buffer)
  (bs-message-without-log "%s" (bs--current-config-message)))

(defun bs-toggle-readonly ()
  "Toggle read-only for buffer on current line.
Uses Function vc-toggle-read-only."
  (interactive)
  (let ((buffer (bs--current-buffer)))
    (save-excursion
      (set-buffer buffer)
      (vc-toggle-read-only))
    (bs--update-current-line)))

(defun bs-clear-modified ()
  "Reset to nil the modified flag for the buffer on current line."
  (interactive)
  (let ((buffer (bs--current-buffer)))
    (save-excursion
      (set-buffer buffer)
      (set-buffer-modified-p nil)))
  (bs--update-current-line))

(defun bs--nth-wrapper (count fun &rest args)
  "Call Function FUN COUNT times with arguments ARGS."
  (setq count (or count 1))
  (while (> count 0)
    (apply fun args)
    (setq count (1- count))))

(defun bs-up (arg)
  "Move cursor up in bs-mode."
  (interactive "p")
  (if (and arg (numberp arg) (< arg 0))
      (bs--nth-wrapper (- arg) 'bs--down)
    (bs--nth-wrapper arg 'bs--up)))

(defun bs--up ()
  "Move cursor up in bs-mode."
  (interactive "p")
  (previous-line 1)
  (if (<= (count-lines 1 (point)) (1- bs-header-lines-length))
      (progn 
	(goto-char (point-max))
	(beginning-of-line)
	(recenter -1))
    (beginning-of-line)))

(defun bs-down (arg)
  "Move cursor down in bs-mode."
  (interactive "p")
  (if (and arg (numberp arg) (< arg 0))
      (bs--nth-wrapper (- arg) 'bs--up)
    (bs--nth-wrapper arg 'bs--down)))

(defun bs--down ()
  "Move cursor down in bs-mode."
  (let ((last (save-excursion (end-of-line) (point))))
    (if (eq last (point-max))
	(goto-line (1+ bs-header-lines-length))
      (next-line 1))))


(defun bs-visits-non-file (buffer)
  "Predicate:
T:   Buffer BUFFER belongs to no file.
NIL: Buffer BUFFER belongs to a file."
  (not (buffer-file-name buffer)))


(defun bs-sort-buffer-interns-are-last (b1 b2)
  "Sortingpredicate to show intern buffers beginning with *
at the end of all buffers."
  (if (string-match "^\\*" (buffer-name b2))
      t
    nil))

;;; ----------------------------------------------------------------------
;;; Configurations:
;;; ----------------------------------------------------------------------

(defun bs-config-clear()
  (setq bs-dont-show-regexp nil
	bs-must-show-regexp nil
	bs-dont-show-function nil
	bs-must-show-function nil
	bs-buffer-sort-function nil))

(defun bs-config--only-files ()
  (bs-config-clear)
  ;; I want to see *-buffers at the end
  (setq bs-buffer-sort-function 'bs-sort-buffer-interns-are-last)
  ;; Don't show files who don't belong to a file
  (setq bs-dont-show-function 'bs-visits-non-file)
  )

(defun bs-config--files-and-scratch ()
  (bs-config-clear)
  ;; I want to see *-buffers at the end
  (setq bs-buffer-sort-function 'bs-sort-buffer-interns-are-last)
  ;; Don't show files who don't belong to a file
  (setq bs-dont-show-function 'bs-visits-non-file)
  ;; Show *scratch* buffer.
  (setq bs-must-show-regexp "^\\*scratch\\*")
  )

(defun bs-config--all ()
  (bs-config-clear))

(defun bs-config--all-intern-last ()
  (bs-config-clear)
  ;; I want to see *-buffers at the end
  (setq bs-buffer-sort-function 'bs-sort-buffer-interns-are-last)
  )

(defun bs-set-configuration (name)
  "Ask user for a configuration and apply selected configuration."
  (interactive 
   (list 
    (completing-read "Use configuration: " 
		     bs-configurations
		     nil
		     t )))
  (let ((fun (assoc name bs-configurations)))
    (if fun
	(progn
	  (setq bs-current-configuration name)
	  (funcall (cdr fun)))
      (ding)
      (bs-message-without-log
       "No bs-configuration named %S." name))))

(defun bs-help ()
  "Help for bs-show."
  (interactive)
  (describe-function 'bs-show))

(defun bs-next-config-aux (start-name liste)
  "Get the next assoc of LISTE after START-NAME. Will return 
the first if START-NAME is at end."
  (let ((assocs liste)
	(length (length liste))
	pos)
    (while (and assocs (not pos))
      (if (string= (car (car assocs)) start-name)
	  (setq pos (- length (length assocs))))
      (setq assocs (cdr assocs)))
    (setq pos (1+ pos))
    (if (eq pos length)
	(car liste)
      (nth pos liste))
    ))

(defun bs-next-config (&optional start-name)
  "The next configuration with respect to the current one."
  (bs-next-config-aux (or start-name bs-current-configuration)
		      bs-configurations))

(defun bs-select-next-configuration (&optional start-name)
  "Select and apply the next available configuration for listing buffers."
  (interactive)
  (let ((config (bs-next-config start-name)))
    (bs-set-configuration (car config))
    (setq bs-default-configuration bs-current-configuration)
    (bs--redisplay t)
    (bs--set-window-height)
    (bs-message-without-log
     "Selected config: %s." (car config))) 
  )

(defun bs-show-in-buffer (list)
  (setq bs-current-list list)
  (switch-to-buffer (get-buffer-create "*buffer-selection*"))
  (bs-mode)
  (let* ((inhibit-read-only t)
	 (max-length-of-names 
	  (apply 'max
		 (cons 0 (mapcar
			  (lambda (entry)
			    (length (buffer-name entry)))
			  list))))
	 (name-entry-length
	  (min bs-maximal-buffer-name-column
	       (max bs-minimal-buffer-name-column max-length-of-names)))
	 ); let binding
    (erase-buffer)
    (setq bs--name-entry-length name-entry-length)
    (bs--show-header)
    (while list
      (bs--insert-one-entry (car list))
      (insert "\n")
      (setq list (cdr list)))
    (delete-backward-char 1)
    (bs--set-window-height)
    (bs--goto-current-buffer)
    (bs-apply-sort-faces)
    ))

(defun bs-next-buffer (&optional sorting-p buffer-list-to-use)
  "Return next buffer of buffer list which is used by bs.
Ignore sorting when sorting-p is nil."
  (let* ((bs--current-sort-function 
	    (if sorting-p bs--current-sort-function nil))
	 (bs-buffer-list (or buffer-list-to-use (bs-buffer-list)))
	)
    (cons 
     (or (car (cdr bs-buffer-list))
	 (car bs-buffer-list)
	 (current-buffer))
     bs-buffer-list)))

(defun bs-previous-buffer (&optional sorting-p buffer-list-to-use)
  "Return last buffer of buffer list which is used by bs.
Ignore sorting when sorting-p is nil."
  (let* ((bs--current-sort-function 
	    (if sorting-p bs--current-sort-function nil))
	 (bs-buffer-list (or buffer-list-to-use (bs-buffer-list))))
    (cons 
     (or (car (last bs-buffer-list))
	 (current-buffer))
     bs-buffer-list)))

(defun bs-message-without-log (&rest args)
  "Like message but don't log it on the message log."
  (let ((message-log-max nil))
    (apply 'message args)))

(defvar bs--cycle-list nil
  "Buffer list used for cycling.")

;;;###autoload
(defun bs-cycle-next ()
  "Rotation of buffers via bs: Select next buffer and buries current."
  (interactive)
  ;;(y-or-n-p (format "%S" bs--cycle-list))
  (let ((bs--buffer-coming-from (current-buffer))
	(bs-dont-show-regexp   bs-dont-show-regexp)
	(bs-must-show-regexp   bs-must-show-regexp)
	(bs-dont-show-function bs-dont-show-function)
	(bs-must-show-function bs-must-show-function)
	(bs--show-all          bs--show-all)
	)
    (if bs-cycle-configuration-name
	(bs-set-configuration bs-cycle-configuration-name))
    (let ((bs-buffer-sort-function nil)
	  (bs--current-sort-function nil))
      (let* ((tupel (bs-next-buffer 
		     nil 
		     (if (or (eq last-command 'bs-cycle-next)
			     (eq last-command 'bs-cycle-previous))
			 bs--cycle-list)
		     ))
	     (next (car tupel))
	     (cycle-list (cdr tupel))
	     )
	(setq bs--cycle-list (append (cdr cycle-list) 
				     (list (car cycle-list))))
	(bury-buffer)
	(switch-to-buffer next)
	(bs-message-without-log 
	 "Next buffers: %s" 
	 (or (cdr bs--cycle-list) "this buffer"))))))


;;;###autoload
(defun bs-cycle-previous ()
  "Rotation of buffers via bs: Select last buffer."
  (interactive)
  (let ((bs--buffer-coming-from (current-buffer))
	(bs-dont-show-regexp   bs-dont-show-regexp)
	(bs-must-show-regexp   bs-must-show-regexp)
	(bs-dont-show-function bs-dont-show-function)
	(bs-must-show-function bs-must-show-function)
	(bs--show-all          bs--show-all)
	)
    (if bs-cycle-configuration-name
	(bs-set-configuration bs-cycle-configuration-name))
    (let ((bs-buffer-sort-function nil)
	  (bs--current-sort-function nil))
      (let* ((tupel (bs-previous-buffer
		     nil 
		     (if (or (eq last-command 'bs-cycle-next)
			     (eq last-command 'bs-cycle-previous))
			 bs--cycle-list)
		     ))
	     (prev-buffer (car tupel))
	     (cycle-list (cdr tupel))
	     )
	(setq bs--cycle-list (append (last cycle-list)
				     (reverse (cdr (reverse cycle-list)))))
	(switch-to-buffer prev-buffer)
	(bs-message-without-log 
	 "Previous buffers: %s" 
	 (or (reverse (cdr bs--cycle-list)) "this buffer")
	 )))))
  
(defun bs--apply (fun &optional args)
  "Apply function FUN with argunents ARGS. 
Return result of evaluation.
Return FUN if FUN is a number or a string."
  (cond ((numberp fun)
	 fun)
	((stringp fun)
	 fun)
	(t (apply fun args))))
	
(defun bs--get-current-p (start-buffer all-buffers)
  (cond 
   ((eq (current-buffer) start-buffer) 
    (if (memq (current-buffer) bs--marked-buffers)
	bs-string-current-marked
      bs-string-current))
   ((memq (current-buffer) 
	  bs--marked-buffers)          bs-string-marked)
   ((null bs-buffer-show-flag)         bs-string-show-normally)
   ((eq bs-buffer-show-flag 'never)    bs-string-show-never)
   (t                                  bs-string-show-always)))

(defun bs--get-m (start-buffer all-buffers)
  (if (buffer-modified-p) "*" " "))

(defun bs--get-r (start-buffer all-buffers)
  (if buffer-read-only "%" " "))

(defun bs--get-size (start-buffer all-buffers)
  (int-to-string (buffer-size)))

(defun bs--get-name (start-buffer all-buffers)
  (let ((name (copy-sequence (buffer-name))))
    (put-text-property 0 (length name) 'mouse-face 'highlight name)
    (if (< (length name) bs--name-entry-length)
	(concat name
		(make-string (- bs--name-entry-length (length name)) ? ))
      name)))
		

(defun bs--get-mode-name (start-buffer all-buffers)
  mode-name)

(defun bs--get-file-name (start-buffer all-buffers)
  (let ((string (copy-sequence (if (member major-mode 
					   '(shell-mode dired-mode))
				   default-directory
				 (or buffer-file-name "")))))
    (put-text-property 0 (length string) 'mouse-face 'highlight string)
    string))


(defun bs--insert-one-entry (buffer)
  "Generate one entry for buffer BUFFER."
  (let ((string "")
	(columns bs-attributes-list)
	(to-much 0)
	)
    (save-excursion
      (while columns
	(set-buffer buffer)
	(let ((min   (bs--apply (nth 1 (car columns))))
	      ;;(max   (bs--apply (nth 2 (car columns)))) refered no more
	      (align (nth 3 (car columns)))
	      (fun   (nth 4 (car columns)))
	      (args  (nthcdr 5 (car columns)))
	      (val   nil)
	      new-string
	      )
	  (setq val (bs--apply 
		     fun 
		     (append (list bs--buffer-coming-from bs-current-list)
			     args)))
	  (setq new-string (bs--format-aux val align (- min to-much)))
	  (setq string (concat string new-string))
	  (if (> (length new-string) min)
	      (setq to-much (- (length new-string) min)))
	  ) ; let
	(setq columns (cdr columns))))
      (insert string)
    string))

(defun bs--format-aux (string align len)
  (let ((length (length string)))
    (if (>= length len)
	string
      (if (eq 'right align)
	  (concat (make-string (- len length) ? ) string)
	  (concat string (make-string (- len length) ? ))))))
		  
;;   (format 
;;    (format 
;;     "%%%s%ds" 
;;     (cond ((eq align 'left)  "-")
;; 	  ((eq align 'right) "")
;; 	  (t "-"))
;;     len) ;inside format
;;    string))

(defun bs--show-header ()
  "Insert Header."
  (mapcar '(lambda (string)
	     (insert string "\n"))
	  (bs--create-header)))

(defun bs--get-name-length ()
  "Return bs--name-entry-length."
  bs--name-entry-length)

(defun bs--create-header ()
  "Return all header lines as a list of strings."
  (list (mapconcat (lambda (column)
		     (bs--format-aux 
		      (bs--apply (car column))
		      (nth 3 column)   ; align
		      (bs--apply (nth 1 column)))) ; min length
		   bs-attributes-list
		   "")
	(mapconcat (lambda (column)
		     (bs--format-aux 
		      (make-string (length (bs--apply (car column))) ?-)
		      (nth 3 column)               ; align
		      (bs--apply (nth 1 column)))) ; min length
		   bs-attributes-list
		   "")))

(defun bs--show-with-config (name &optional arg)
  "Display a new buffer *buffer-selection* for editing a list of
buffers with configuration called NAME."
  (bs-set-configuration name)
  (let ((bs--show-all (or bs--show-all arg))
	)
  (if (not (string= "*buffer-selection*" (buffer-name)))
      ;; Only when not in buffer *buffer-selection*
      ;; we have to set the buffer we started the command
      (progn
	(setq bs--buffer-coming-from (current-buffer))
	(setq bs--window-config-coming-from (current-window-configuration))))
  (let ((liste (bs-buffer-list))
	(active-window (bs--window-for-buffer "*buffer-selection*"))
	)
    (if active-window
	(select-window active-window)
      (if (> (window-height (selected-window)) 5)
	  (progn
	    (split-window-vertically)
	    (other-window 1))))
    (bs-show-in-buffer liste)
    (bs-message-without-log "%s" (bs--current-config-message))
    )))

(defun bs--configuration-name-for-prefix-argument (arg)
  "Convert prefix argument ARG to name of bs configuration."
  (cond 
   ;; usually activation
   ((null arg)
    bs-default-configuration)
   ;; call with integer as prefix argument
   ((integerp arg)
    (if (and (< 0 arg) (<= arg (length bs-configurations)))
	(car (nth (1- arg) bs-configurations))
      bs-default-configuration))
   ;; call by prefix argument C-u
   (t bs-alternative-configuration)))
  
;;; ----------------------------------------------------------------------
;;; Main function bs-customize and bs-show
;;; ----------------------------------------------------------------------

;;;###autoload
(defun bs-customize ()
  "Customization of the group bs."
  (interactive)
  (customize-group "bs")
  )

;;;###autoload
(defun bs-show (arg)
  "Display a new buffer *buffer-selection* for editing a list of
buffers. User can move with [up] or [down], select a buffer
by [space] or [return]\n\n
\\{bs-mode-map}"
  (interactive "P")
  (setq bs--marked-buffers nil)
  (bs--show-with-config (bs--configuration-name-for-prefix-argument arg)))


;;; set default configuration
(bs-set-configuration bs-default-configuration)

;;; Now provide feature bs
(provide 'bs)

;;; bs.el ends here
