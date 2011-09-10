;; autolisp.el -- Major mode to edit AutoLISP in emacs
;;
;; Emacs Lisp Archive Entry
;; Filename: autolisp.el
;; Author: Reini Urban <rurban@xarch.tu-graz.ac.at>
;; Version: 0.5
;; Created: 2000-01-12
;; Time-stamp: <2001-01-14 04:32:22 rurban>
;; Keywords: AutoLISP major-mode
;; Description: Major mode to edit AutoLISP in emacs
;; URL: http://xarch.tu-graz.ac.at/autocad/lsp_tools/ntemacs/autolisp.el.gz
;; Compatibility: Emacs19, Emacs20, XEmacs21
;;
;; $Id: autolisp.el 0.5 2000/04/09 14:54:01 rurban Exp $
;;;
;;{{{ Credits: The authors of scheme.el and lisp-mode.el,
;;             Steve Kamp for the COM patch
;;}}}
;;{{{ Copyright (C) 2000 Reini Urban
;;
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
;;}}}
;;{{{ Commentary:
;;   For GNU Emacs and XEmacs
;;   Some features don't work yet.
;;
;; Installation:
;; Add this line in your .emacs:
;; (autoload 'autolisp-mode "autolisp" "AutoLISP" t)
;; To invoke autolisp-mode automatically on .LSP files, add this:
;; (setq auto-mode-alist (adjoin '("\\.LSP$" . autolisp-mode) auto-mode-alist))
;;}}}
;;{{{ Configuration:
;;
;; OLE/Interactive Mode:
;;   This mode will be enabled to use a patched ntemacs, written by
;; Steve Kemp <skx@tardis.ed.ac.uk>, resp. the w32ole emodule written by 
;; myself, to be able to connect via OLE to a running Visual Lisp for 
;; AutoCAD 2000 session.
;;   Or maybe by using a proposed FFI for [X]Emacs later.
;;   VLISP is since AutoCAD 2000 (R15) a COM server so we may
;; talk to it via IDispatch.
;; The FSF emacs 20.5.1 patch is available at
;;   http://xarch.tu-graz.ac.at/autocad/lsp_tools/ntemacs/
;; The FSF emacs 20.4.1 patch is available at
;;   http://www.tardis.ed.ac.uk/~skx/win/NTEmacs6.html
;; The XEmacs w32ole emodule (not ready yet) is at 
;;   http://xarch.tu-graz.ac.at/autocad/lsp_tools/ntemacs/w32ole.c
;; An ilisp mode is also in development.
;;}}}
;;{{{ Todo:
;; * fix etags support:  TAGS should be case insensitive
;; * font-lock:
;;   fix ;| |; comments [works partially now],
;;   add standard keywords [almost done]
;; * fix XEmacs menu buffer-local, not global
;; * make vlisp-colors buffer-local [done 0.4], fix restauration for GNU Emacs
;; * help support (describe, balloon-help support, improve Ctrl-F1 hook to winhelp)
;; * interactive mode: run-vlisp via OLE
;; * tempo support:
;;   dynamic expansions for most constructs, upcase funcnames when writing, ...
;; * fold or fontify at ;|#- feature|; ... ;|END #- feature|;
;;   maybe as quasi-feature minor mode. all, #+ features, #- features
;; * vlisp-like project frame
;;   define-project, edit-project, speedbar: project-members-only
;; * startup with window width of xx chars [for now 80]
;;}}}
;;{{{ Revisions:
;;; $Log: autolisp.el $
;;; 2000-09-12 15:20:05 rurban
;;; added autolisp-font-lock-vertical and fixed autolisp-font-lock-init
;;; 2000-04-20 16:58:08 rurban
;;; added folding
;;; 2000-04-16 12:41:56 rurban
;;; fixed indentation for PROGN, WHILE, IF to 2
;;;
;;; Revision 0.5  2000/04/09 14:54:01  rurban
;;; fixed imenu and xemacs menu,
;;; changed to easy-menu, provide "Goto" imenu submenu
;;; improved help
;;;
;;; 0.4 2000-03-09 15:58:48 rurban
;;; fixed vlisp-colors buffer-local
;;; 
;;; 2000-03-06 12:33:21 rurban
;;; fixed autolisp-font-lock-init face load error
;;;
;;; 0.3 2000-02-27 13:59:33 rurban
;;; bugfix for GNU Emacs: font-lock-defaults range error
;;; bugfix for autolisp-mode-syntax-table
;;;
;;; 0.2 2000-02-26 19:06:36 rurban
;;; added xemacs, fixed syntax table, 
;;; added colors, fixed font-lock-keywords
;;;
;;; 0.1 2000-01-12 rurban
;;; Initial revision
;;; wrong ;| |; fontification, FSF Emacs only
;;}}}

;;{{{ init

(require 'lisp-mode)
(require 'cl)                           ; for subst
(require 'font-lock)

(defconst autolisp-mode-version (substring "$Revision: 0.5 $" 11 15))

(defvar running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))
;;}}}

;;{{{ some defcustom's
(defgroup autolisp nil
  "Editing AutoLISP code"
  :group 'languages
  :group 'lisp)

;(defcustom autolisp-menu-path nil
;  "*Path where to add the autolisp menu.
;A value of nil means add it as top-level menu.
;For more information look up the documentation of `add-menu'."
;  :type '(choice (const :tag "Top Level" nil)
;		 (sexp :tag "Menu Path"))
;  :group 'autolisp)

(defcustom autolisp-use-imenu t 
  "Non-nil to add an imenu Index menu."
  :type 'boolean
  :group 'autolisp)

(defcustom autolisp-frame-width 72
  "A number to set the frame width or nil.
73 (for 72 columns) or 80 are good numbers. 
See `autolisp-set-frame-width'."
  :group 'autolisp)

(defcustom autolisp-vertical-bar t
  "Mark the rightmost column (`autolisp-frame-width' or 80) with a vertical bar."
  :group 'autolisp)

(defconst Vlisp    'VLisp)
(defconst Standard 'Standard)
(defconst New      'New)
(defconst Stdlib   'Stdlib)
(defconst Dynamic  'Dynamic)

;;; not yet functional
;(defcustom autolisp-font-lock-keywords-choice Vlisp
;   "Select which keywords to highlight.
; Standard: all old AutoLISP keywords (R14 without Vlisp)
; Vlisp:    plus the basic VL- keywords
; New:      plus VLA-, VLAX- and VLR-
; Stdlib:   plus all Stdlib functions
; Dynamic:  plus all actually defined functions (slow)"
;  :type '(choice (const Standard)
;		(const Vlisp)
;		(const New)
;		(const Stdlib)
;		(const Dynamic))
;  :group 'autolisp)

(defcustom autolisp-mode-hook nil
  "Normal hook (list of functions) run when entering autolisp-mode.
See `run-hooks'."
  :type 'hook
  :group 'autolisp)

(defcustom autolisp-vlisp-colors nil
  "Non-nil to define Visual Lisp colors. nil is default.

	comments: violet on grey background
	parens:   red
	strings:  magenta
	keywords: blue
	numbers:  green
	fontface: fixedsys"
  :type 'boolean
  :group 'autolisp)
;;}}}

;;{{{ autolisp-imenu-generic-expression
;; This is from scheme
(defvar autolisp-imenu-generic-expression
       ;; (MENU-TITLE REGEXP INDEX)
  (reverse '((nil 			
	      "^(defun\\s-+\\(\\sw+\\)" 1)	; first col only
	     ("Globals"
	      "^(setq\\s-+\\(\\sw+\\)" 1)	; first col only
	     ("Constants"			; first col only
	      ;; STDLIB specific: std-defvar '<sym>, std-defconstant '<sym>
	      "^(std-def\\(keyword\\|constant\\|default\\|default-type\\)\\s-+'\\(\\sw+\\)" 2)
	     ;;Todo: sage-clos classes and methods
	     ("SageClass"
	      "^\\s-*(defclass\\s-+'\\(\\sw+\\)" 1)
	     ("SageMethods"
	      "^\\s-*(def\\(method\\|generic\\)\\s-+'(\\(\\sw+\\)" 2)
	     ))
  "Imenu generic expression for AutoLISP mode.  
See `imenu-generic-expression'.")


;; This is from lisp-mode
(defvar autolisp-mode-map nil
  "Keymap for AutoLISP mode. See `make-keymap'")
;;}}}

;;{{{ autolisp-menu definition
(defvar autolisp-menu
  '("AutoLISP"
    ["Uncomment Out Region"
     (function (lambda (beg end)
		 (interactive "r")
		 (comment-region beg end -3)))
     (fboundp 'comment-region)]
    ["Comment Out Region"
     (function (lambda (beg end)
		 (interactive "r")
		 (comment-region beg end 3)))
     (fboundp 'comment-region)]
    ["Indent Region" indent-region     (fboundp 'indent-region)]
    ["Indent Line"   lisp-indent-line  (fboundp 'lisp-indent-line)]
    "----"
    ("Goto" 
     ["*Rescan*" imenu nil]
      )
    ("Tools"
     ["Create Menu Bitmap DLL..." autolisp-create-bitmap-dll t]
     )
    ["80 Cols" (function (lambda ()
		  (interactive)
		  (autolisp-set-frame-width 80))) t]
    "----"
    ("Remote"
     ["Connect to VLISP..." run-vlisp (fboundp 'run-vlisp)]
     ["New Acad session..." start-acad (fboundp 'start-acad)]
     )
    ("Help"
     ["AutoLISP Help" autolisp-help (fboundp 'autolisp-help)]
     ["Help on keyword" autolisp-keyword-help t]
     ))
  "AutoLISP menu definition")

(defun autolisp-keymap-init ()
  "Portably initialize the keymap"
  (if running-xemacs
    (setq autolisp-mode-map (copy-keymap shared-lisp-mode-map))
    (setq autolisp-mode-map
	  (nconc (make-sparse-keymap "AutoLISP") shared-lisp-mode-map))))
;;}}}

;;{{{ autolisp-menu-init
(defun autolisp-menu-init ()
  "Initialize the keymap and display the AutoLISP pulldown menu."
  (unless autolisp-mode-map
    (autolisp-keymap-init))
  (define-key autolisp-mode-map "\e\t" 'lisp-complete-symbol)
  (define-key autolisp-mode-map "\e\C-q" 'indent-sexp)
  (if running-xemacs
      (define-key autolisp-mode-map [(control f1)] 'autolisp-keyword-help)
    (define-key autolisp-mode-map [(control f1)] 'autolisp-keyword-help))
  ;;(define-key autolisp-mode-map "\e\C-h" 'autolisp-help)
  ;;(define-key autolisp-mode-map "\e\C-x" 'remote-eval-defun)
  (define-key autolisp-mode-map "\e\C-x" 'eval-defun)
  (use-local-map autolisp-mode-map)
  (require 'easymenu)
  (easy-menu-define autolisp-menu-symbol autolisp-mode-map
		    "AutoLISP menu" autolisp-menu)
  (easy-menu-add autolisp-menu autolisp-mode-map)
  (when autolisp-use-imenu
;    (easy-menu-add-item autolisp-menu '("AutoLISP" "Goto") imenu-menu-filter)
    (imenu-add-to-menubar "Goto")
    )
  )
;;}}}

;;{{{ autolisp-syntax-table-init
(defvar autolisp-mode-syntax-table nil "")

(defun autolisp-syntax-table-init ()
  "The AutoLISP syntax is different from Lisp, Elisp or Scheme.
Inline comments: ;|...|;
# [ ] , are valid symbol constituents (though they should not be used)"
  (if (not autolisp-mode-syntax-table)
    (progn 
      (setq autolisp-mode-syntax-table
	    (copy-syntax-table emacs-lisp-mode-syntax-table))
      ;; multi-line comments must be defined via syntax-table
      (modify-syntax-entry ?\;  "' 58" lisp-mode-syntax-table)
      (modify-syntax-entry ?\|  ". 67" lisp-mode-syntax-table))
    ;;(modify-syntax-entry ?\;  "< 58" autolisp-mode-syntax-table)
    ;;(modify-syntax-entry ?\|  ". 67" autolisp-mode-syntax-table)
    ;; (modify-syntax-entry ?\;  " 14" autolisp-mode-syntax-table)
    ;; (modify-syntax-entry ?\|  ". 23" autolisp-mode-syntax-table)
    ;; those are valid word chars in AutoLISP
    (modify-syntax-entry ?#   "_   " autolisp-mode-syntax-table)
    (modify-syntax-entry ?\[  "_   " autolisp-mode-syntax-table)
    (modify-syntax-entry ?\]  "_   " autolisp-mode-syntax-table)
    (modify-syntax-entry ?,   "_   " autolisp-mode-syntax-table)
    ))

;;}}}
;
(defvar autolisp-mode-abbrev-table nil "")
(define-abbrev-table 'autolisp-mode-abbrev-table ())
;

;;{{{ autolisp-mode-variables
;;; This is from scheme
(defun autolisp-mode-variables ()
  (unless autolisp-mode-syntax-table
    (autolisp-syntax-table-init))
  (set-syntax-table autolisp-mode-syntax-table)
  (setq local-abbrev-table autolisp-mode-abbrev-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'fill-paragraph-function)
  (setq fill-paragraph-function 'lisp-fill-paragraph)
  ;; Adaptive fill mode gets in the way of auto-fill,
  ;; and should make no difference for explicit fill
  ;; because lisp-fill-paragraph should do the job.
  (make-local-variable 'adaptive-fill-mode)
  (setq adaptive-fill-mode nil)
  (make-local-variable 'normal-auto-fill-function)
  (setq normal-auto-fill-function 'lisp-mode-auto-fill)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'lisp-indent-line)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (make-local-variable 'outline-regexp)
  (setq outline-regexp ";;; \\|(....")
  (make-local-variable 'comment-start)
  (setq comment-start ";")
  (make-local-variable 'comment-start-skip)
  ;; Look within the line for a ; following an even number of backslashes
  ;; after either a non-backslash or the line beginning.
  ;; To be fixed for ;| |; instead of ;# #;
  (setq comment-start-skip "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\);+[ \t]*")
  ;; XEmacs change
  (set (make-local-variable 'block-comment-start) ";;")
  (set (make-local-variable 'comment-column) 40)
  (set (make-local-variable 'comment-indent-function) 'lisp-comment-indent)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'lisp-indent-function) 'lisp-indent-function)
; (setq mode-line-process '("" lisp-mode-line-process))
  ;; XEmacs change
  (set (make-local-variable 'dabbrev-case-fold-search) t)  ;ignore case
  (set (make-local-variable 'dabbrev-case-replace) t)	;preserve case
  (set (make-local-variable 'case-fold-search) t)	;ignore case
  (set (make-local-variable 'tags-always-exact) nil)
  ;; override some lisp-indent vars from lisp-mode.el
  (put 'lambda 'lisp-indent-function 'defun)
  (put 'progn 'lisp-indent-function 0)
  (put 'while 'lisp-indent-function 0)
  (put 'if 'lisp-indent-function 0)

  (when autolisp-use-imenu
    (require 'imenu)
    (make-local-variable 'imenu-case-fold-search)
    (setq imenu-case-fold-search t)
    (make-local-variable 'imenu-generic-expression)
    (setq imenu-generic-expression autolisp-imenu-generic-expression)
    (make-local-variable 'imenu-syntax-alist)
    (setq imenu-syntax-alist '(("+-*/.<>=?!$%_&~^:" . "w")))
    )

  )
;;}}}

;;{{{ autolisp-mode-initialize
(defun autolisp-mode-initialize ()
  (setq major-mode 'autolisp-mode)
  (setq mode-name "AutoLISP")
  (autolisp-font-lock-init)
  (autolisp-menu-init)
  (autolisp-syntax-table-init)

  (if running-xemacs
      (progn
	(require 'folding)
	(fold-add-to-marks-list 'autolisp-mode ";;{{{ " ";;}}}" nil))
      (progn
	(require 'outline)
	(outline-minor-mode)
	))
	
  (if autolisp-frame-width
    (autolisp-set-frame-width autolisp-frame-width))
)
;;}}}
;
;;{{{ autolisp-mode
;;;###autoload
(defun autolisp-mode ()
  "Major mode for editing AutoLISP code to run in Emacs.
Editing commands are similar to those of `lisp-mode'.

Interactive Mode: (not yet:)
In addition, if a connection via COM to VisualLISP is established,
some additional commands will be defined, for evaluating expressions
and controlling the interpreter, and the state of the process will be
displayed in the modeline of all AutoLISP buffers.
The names of commands that interact with the VisualLISP process
start with \"vlisp-\".  For more information see the documentation
for `vlisp-interaction-mode'.

Commands:
Delete converts tabs to spaces as it moves back. (?)
Blank lines separate paragraphs.  Semicolons start comments.
\\{autolisp-mode-map}
Entry to this mode calls the value of `autolisp-mode-hook'
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (autolisp-mode-variables)
  (autolisp-mode-initialize)
  (run-hooks 'autolisp-mode-hook))
;;}}}
;
;;{{{ autolisp-font-lock keywords
;;; This is much simplier (=faster) than elisp/lisp/scheme
(defconst autolisp-font-lock-keywords-1
  (eval-when-compile
    (list
     '("^(\\(defun\\)[ \t'\(]*\\([^ \t\n\)]+\\)?" . font-lock-function-name-face)
     ;'("\\(^\\|[^\$\\\]\\)#.*" . font-lock-comment-face)
;     '("\\<;|\\>" (0 font-lock-comment-face) ("\\<|;\\>"  nil nil (0 font-lock-comment-face)))
     )
   )
  "Highlight DEFUN only in AutoLISP modes.")

(defconst autolisp-font-lock-keywords-2
  (append autolisp-font-lock-keywords-1
   (eval-when-compile
     (list
      ;; Control structures.
      (cons
       (concat "(" (regexp-opt
		    '("cond" "while" "repeat" "foreach" "if" "lambda"
		      "and" "or" "quote" "mapcar" "progn"
		      "setq" "list" "length" "cons" "append" "reverse"
		      "assoc" "strcat" "princ" "apply" "member"
		      "car" "cadr" "cdr" "substr" "function" "alert" 
		      "eq" "null" "not" "defun"
		      ) t)
	       "\\>")
       font-lock-keyword-face)
      '("\\<<\\sw+>\\>" . font-lock-type-face)
      ;;
      ;; `:' keywords as quasi  builtins.
      ;; AutoLISP has no : keywords, only the stdlib supports it.
      '("\\<:\\sw+\\>" . font-lock-keyword-face)
      ;; stdlib and vlisp stuff
      '("(\\(std-\\|vl[ar]?-\\|(vlax-\\)[^ \t\n(]*\\>"
;       (concat "(" (regexp-opt
;		    '("std-" "vl-" "vla-" "vlr-" "vlax-") t))
	. font-lock-type-face)
      )))
  "Highlight more in AutoLISP mode.")

(defvar autolisp-font-lock-keywords autolisp-font-lock-keywords-1
  "Default expressions to highlight in AutoLISP mode.")

(defun autolisp-x-dots (num)
  "Return a string of NUM dots."
  (make-string num ?\.))

(defvar autolisp-font-lock-vertical-face 'autolisp-font-lock-vertical-face
  "Vertical end of column marker.")

(require 'font-lock)
;(set-face-background 'autolisp-font-lock-vertical-face "gray80" (current-buffer))
;(set-face-foreground 'autolisp-font-lock-vertical-face "magenta4" (current-buffer))
(defface autolisp-font-lock-vertical-face
  '((((class color) (background dark))  
     (:foreground "white") (:background "gray80"))
    (((class color) (background light)) 
     (:foreground "black") (:background "gray80"))
    (t (:underline t)))
  "Vertical end of column marker."
  :group 'autolisp)

(defvar autolisp-font-lock-vertical
  (eval-when (load compile eval)
    (list
     ;; Vertical bar
     (cons (concat "^" (autolisp-x-dots (1- (or autolisp-frame-width 80)))
		   "\\(.\\)")
	   autolisp-font-lock-vertical-face))))

(defconst autolisp-font-lock-keywords-v 
  (append autolisp-font-lock-keywords autolisp-font-lock-vertical))
(defconst autolisp-font-lock-keywords-v1
  (append autolisp-font-lock-keywords-1 autolisp-font-lock-vertical))
(defconst autolisp-font-lock-keywords-v2 
  (append autolisp-font-lock-keywords-2 autolisp-font-lock-vertical))

;(defgroup autolisp-helper-faces nil
;  "Customizing autolisp-helper-mode custom faces"
;  :group 'autolisp
;  :group 'faces)

;; Custom VLISP faces
;;     comments: violet on grey background
;;     parens:   red
;;     strings:  magenta
;;     keywords: blue
;;     numbers:  green
;;     fontface: fixedsys

;(defun autolisp-fix-face (key face new)
;  (let ((spec (get face 'face-defface-spec)))
;    (face-spec-set face (subst (cons key (list new))
;			       (assoc key spec)
;			       spec))))

;;; (setq face font-lock-comment-face)
;;; (setq key '((class color) (background light)))
;;; (setq spec (get font-lock-comment-face 'face-defface-spec))
;;; (setq new '(:background "gray80" :foreground "magenta4" :bold t))
;;; (font-lock-use-default-colors)
;;}}}

;;{{{ autolisp-font-lock-init
(defun autolisp-font-lock-init ()
  (interactive)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
	(append
	 (if autolisp-vertical-bar
	     '((autolisp-font-lock-keywords-v
		autolisp-font-lock-keywords-v1 
		autolisp-font-lock-keywords-v2)
	       nil t)
	     '((autolisp-font-lock-keywords
		autolisp-font-lock-keywords-1 
		autolisp-font-lock-keywords-2)
	       nil t))
	  ;; GNU Emacs is different from XEmacs here
	 (if running-xemacs
	     (nthcdr 3 (get 'lisp-mode 'font-lock-defaults))
	     (nthcdr 3 (cdr (assq 'lisp-mode font-lock-defaults-alist))))))

  ;;(or autolisp-frame-width 80)
  (if autolisp-vlisp-colors
      (progn
	;;    (let ((key '((class color) (background light))))
	(require 'font-lock)
	(set-face-background 'font-lock-comment-face "gray80" (current-buffer))
	(set-face-foreground 'font-lock-comment-face "magenta4" (current-buffer))
	(set-face-foreground 'font-lock-string-face "magenta" (current-buffer))
	(set-face-foreground 'font-lock-keyword-face "blue" (current-buffer))
	;; OLD:
;      (autolisp-fix-face key 
;		       font-lock-comment-face
;		       '(:background "gray80"
;			 :foreground "magenta4" 
;			 ; :bold t
;			 ))
;      (autolisp-fix-face key 
;		       font-lock-string-face
;		       '(:foreground "magenta" :bold t))
;      (autolisp-fix-face key 
;		       font-lock-function-name-face
;		       '(:foreground "black" :bold t))
;      (autolisp-fix-face key 
;		       font-lock-keyword-face
;		       '(:foreground "blue"))
    )
    ;; else
    (if (and running-xemacs (fboundp 'font-lock-use-default-colors))
	(font-lock-use-default-colors)
      ;; TODO: How to restore GNU Emacs default faces
    )
  )
)

;;;###autoload
(defun autolisp-vlisp-colors (&optional off)
  "Set Visual LISP colors. With prefix turn it off. 
Controlled by the custom variable `autolisp-vlisp-colors'."
  (interactive)
  (if ;;(eq major-mode 'autolisp-mode)
      (local-variable-p 'autolisp-vlisp-colors nil t)
    (progn
      (hack-one-local-variable 'autolisp-vlisp-colors (if off nil t))
      (autolisp-font-lock-init)
      (font-lock-fontify-buffer))
    (message "Can only be used with autolisp-mode. ") ; or from modes deriving from it
    ))
;;}}}

;;{{{ autolisp-set-frame-width
;;;###autoload
(defun autolisp-set-frame-width (&optional width)
  "Set screen width to 'width' columns.
Controlled by the custom variable `autolisp-frame-width'."
  (interactive "nFrame width: ")
  (let ((old autolisp-frame-width))
    (if (and autolisp-frame-width (not width))
      (setq width autolisp-frame-width))
    (set-frame-width (selected-frame) width)
    (when (/= old width)
	(setq autolisp-font-lock-vertical
	      (list
	       (cons (concat "^" (autolisp-x-dots (1- (or autolisp-frame-width 80)))
			     "\\(.\\)")
		     autolisp-font-lock-vertical-face)))
	(if autolisp-vertical-bar
	    (progn
	      (setq autolisp-frame-width width)
	      (autolisp-font-lock-init)
	      (font-lock-fontify-buffer))))
    (message (format "Frame width %s" width))))
;;}}}
;^L
;;{{{ some help functions

;;{{{ w32-help system
(defcustom w32-help-command
  ;; winhlp32.exe doesn't support a topic argument, so we have to use a custom binary
  ;; xemacs convention
  (if running-xemacs
      ;; for xemacs we'll better write an emodule for such binary callouts
      (if (file-exists-p (concat (getenv "EMACS_DIR") "/site-packages/bin/w32-help.exe"))
	  (concat (getenv "EMACS_DIR") "/site-packages/bin/w32-help.exe")
	"w32-help.exe")
    ;; emacs convention
    (if (file-exists-p (concat (getenv "EMACS_DIR") "/bin/w32-help.exe"))
	(concat (getenv "EMACS_DIR") "/bin/w32-help.exe")
      (if (file-exists-p (concat (getenv "EMACS_DIR") "/site-bin/w32-help.exe"))
	  (concat (getenv "EMACS_DIR") "/site-bin/w32-help.exe")
	"w32-help.exe"
	)
      )
    )
  "Location of the w32-help program to call to retrieve help about a 
specific item from a specific help file.
winhlp32.exe doesn't support a topic argument, so we have to use a custom binary.
If this doesn't exist winhlp32.exe is used."
  :type 'string
  :group 'autolisp) ; orginally w32-help

;; Call this when the topic is known as a string
;;;###autoload
(defun w32-help-on-topic (file &rest topic)
  "*Get help from the specified Windows help FILE for the specified TOPIC"
  (interactive "fName of help file: ")
  (unless (file-exists-p w32-help-command)
    (if running-xemacs
	(mswindows-shell-execute nil "winhlp32.exe" file)
      (call-process "winhlp32.exe" nil 0 nil file))
    ;;else
    (let (command
	(ori_shell (getenv "SHELL"))
	(ori_sfn shell-file-name))
    (unless running-xemacs
	(setenv "SHELL" (expand-file-name (concat (getenv "EMACS_DIR") "/bin/cmdproxy.exe")))
	(setq-default shell-file-name '"cmdproxy.exe")
	)
    (if (string-equal "" (apply 'concat topic))
	(setq command "CONTENTS")
      (setq command (apply 'concat topic))
      )
    (call-process w32-help-command nil 0 nil file command)
    (unless running-xemacs
      (setenv "SHELL" ori_shell)
      (setq-default shell-file-name ori_sfn)
      )
    )
  ))

;; Call this when the topic is currently selected in the buffer
;;;###autoload
(defun w32-help-on-region (file start end)
"*Get help from the specified Windows help FILE for the topic specified
by the current region"
   (interactive "fName of help file: \nr")
   (w32-help-on-topic file (buffer-substring start end))
   )

;;;###autoload
(defun w32-help (&optional keyword)
  (interactive)
  (unless keyword (setq keyword (current-word 'strict)))
  (w32-help-on-topic (w32-help-file) keyword))

(defvar w32-help-file-alist
  '((emacs-lisp-mode . ("lispref.info" . info))
    (autolisp-mode   . ("P:\\Acad2000\\Help\\acad_dev.hlp" . w32-help))
    (c-mode   	     . ("clib" . w32-help))
    (perl-mode 	     . ("PERL5.HLP" . w32-help))
    (cperl-mode	     . ("PERL5.HLP" . w32-help))
    (c++-mode  	     . ("cpplib" . w32-help))
   )
  "Association of (major-mode . (help-file help-command)) where 
help-command defaults to info.
We have at least five different helpfile formats and functions:
  texinfo
  man
  html
  winhelp
  htmlhelp
The help-commands need two arguments: file and topic"
)

(defun w32-help-file-path (file)
  "Searches in the registry for path or, if not found, in some win32 and 
cygwin specific paths."
  (let ((cygprefix "/usr/hlp/")
	(w32prefix "")
	(search-path nil))
    (if (getenv "CYGROOT")
	(if (file-directory-p (concat (getenv "CYGROOT") "usr\\hlp"))
	    (setq cygprefix (concat (getenv "CYGROOT") "usr\\hlp\\")
		  search-path (cons cygprefix search-path))))
    (if (getenv "SystemRoot")
	(progn
	  (if (file-directory-p (concat (getenv "SystemRoot") "\\HELP"))
	      (setq w32prefix (concat (getenv "SystemRoot") "\\HELP\\")
		    search-path (cons w32prefix search-path)))
	  (setq	search-path (cons (getenv "SystemRoot") search-path))))
    ;; add default  extension .HLP
    (cond
     ;;registry-lookup
     ;;search-paths
     ((string= (upcase file) "DIR.HLP") (concat cygprefix file))
     (t file))))

;;; TODO: lookup the registry under [HKLM\Software\Microsoft\Windows\Help]
;;; for an unknown helpfile path
(defun w32-help-file ()
  "Returns the help file matching the current mode.
The filename without path should be normally sufficient if the help file 
is registered in the registry, which happens automatically if you invoked 
it once. Otherwise the WinHelp API looks it up in its search path: 
  `<WINNT>/HELP'"
  (w32-help-file-path
   (let ((match (assoc major-mode w32-help-file-alist)))
     (if match (cadr match)
       ;; else default gnu top node at /usr/hlp/
       "dir.hlp"))))
;;}}}

;;{{{ autolisp-help
(defvar autolisp-help-history nil)

;;;###autoload
(defun autolisp-help ()
  (interactive)
  (w32-help-on-topic (w32-help-file)))

;;; Ctrl-F1 keyword help : see query-replace documentation replace.el
;;;###autoload
(defun autolisp-keyword-help (&optional keyword)
"Display the AutoCAD winhelp entry on the word at the cursor."
   (interactive 
    (let ((def (current-word 'strict))
	  key)
      (setq key (read-from-minibuffer "Help for AutoLISP keyword: "
				      def nil nil
				      'autolisp-help-history))
      (list (or key def))))
  (let ((file (w32-help-file)))
    (message "Looking up keyword %S in helpfile %s ..." keyword file)
    (w32-help-on-topic file keyword)))
;;}}}
;;}}}
;^L
;;{{{ Create a menu bitmap dll
;;;=========================
;;; instructions see 
;;; http://xarch.tu-graz.ac.at/autocad/docs/make-menudll-readme.txt 

;1. Create the list of bitmaps
;  DIR /b *.bmp >bmp.lst
;saves the list on the file bmp.lst
;
; emacs: parse the <menufile>.mnu or <menufile>.mns and check for 
; bitmaps files and the menugroup name => <menugroup>
; a) scan for _Button("\([^"]+\)",[\t ]*"\([^"]+\)",,[\t ]*"\([^"]+\)")
; b) scan for ^\*\*\*MENUGROUP=\(?+\)$

; foreach bitmap check if it's in the same dir as the menufile
; else error

;2. Create the <menufile>.rc (resource definition file)
;   from the bmp.lst
; 'prefix' stands for the application 2-4 letter abbrevation

;  prefix_16_ARCTXT   BITMAP DISCARDABLE "arctxt16.bmp"
;  prefix_24_ARCTXT   BITMAP DISCARDABLE "arctxt24.bmp"

; or for small only:
;  prefix_16_ARCTXT   BITMAP DISCARDABLE "arctxt.bmp"

; To do it manually in emacs:
; Q: how to add the tab character to replace-regexp?
; with M-x you just hit <tab> instead of \t
; M-x replace-regexp RET ^\(.+\)\.bmp RET prefix_16_\1 TAB BITMAP DISCARDABLE "\&"

; save this buffer as <menufile>.rc

;3. create <menufile>.def 
; with one line:
; LIBRARY <menugroup>
; Q: can this be made optional by supplying a switch to msvc link?

; 4. compile and link it
; 
;  RC -r <menufile>.rc
;  LINK /nodefaultlib /DLL /MACHINE:IX86 /DEF:<menufile>.def /OUT:<menufile>.dll /NOENTRY <menufile>.res
;  DEL <menufile>.exp
;  DEL <menufile>.lib
;  DEL <menufile>.res

; 5. fix the menufile to use the symbolic names instead of the bitmap filenames
; M-x replace-regexp RET "\([^"\.]+\)\.bmp" RET "prefix_16_\1"

(defun force-underscore (s)
  (if (not (eq (elt s (1- (length s))) ?\_))
    (concat (upcase s) "_")
    (upcase s)))

;;;###autoload
(defun autolisp-create-bitmap-dll (&optional menu prefix)
  "Creates a resource dll from the bitmap definitions in an AutoCAD menufile.
The bitmaps must already exist in the same directory as the menufile.
The MSVC resource compiler and linker compiles all found bitmaps to the DLL
(Error if it is in use by AutoCAD: unload it) and the buffer is changed to 
use the resource ID's instead of the bitmap filenames.

For detailed instructions see `http://xarch.tu-graz.ac.at/autocad/docs/make-menudll-readme.txt'"

  (interactive "@bSelect Menufilebuffer to convert (.mnu/.mns):\nsSelect a short prefix string (ACET): " )
  (if (null prefix) (setq prefix menu))
  (setq prefix (force-underscore prefix))
  (if (> (length prefix) 5)
    (setq prefix (force-underscore (substring prefix 0 4))))

  ;; emacs: parse the <menufile>.mnu or <menufile>.mns and check for 
  ;; bitmaps files and the menugroup name => <menugroup>
  (save-excursion
   (save-restriction
     (catch 'Abort
       (let ((mod-p (buffer-modified-p))
	     (buffer-read-only nil)
; ori:
;ID_ARP         [_Button("AutoRaumPoly", "ARP.bmp", "ARP.bmp")]^C^CAUTORAUMPOLY
; afterwards:
;ID_ARP         [_Button("AutoRaumPoly", "GDM_16_ARP", "GDM_16_ARP")]^C^CAUTORAUMPOLY
	     (bitmap-regex 
	       "_Button(\"\\([^\"]+?\\)\",[\t ]*?\"\\([^\"]+\\)\",[\t ]*?\"\\([^\"]+?\\)\")")
	     (menugroup-regex "^\\*\\*\\*MENUGROUP=\\(.+\\)$")
	      menugroup bitmap16 bitmap32 bitmaps)
	 (widen)
	 (goto-char (point-min))
	 (if (re-search-forward menugroup-regex nil t)
	   (progn
             (setq menugroup (match-string 1))
             (message "Menugroup: %s" menugroup)
	   )
	   (message "Warning: no Menugroup definition found")
	 )
	 (if (not (re-search-forward "^\\*\\*\\*TOOLBARS" nil t))
	   (progn
             (message "no toolbar definition found")
	     (throw 'Abort nil)))
	 (require 'cl)
	 (while (re-search-forward bitmap-regex nil t)
           (setq bitmap16 (match-string 2))
           (setq bitmap32 (match-string 3))
           (setf bitmaps (adjoin bitmap16 bitmaps))
           (setf bitmaps (adjoin bitmap32 bitmaps))
	   (message "Button: %s %S %S" (match-string 2) bitmap16 bitmap32)
	 )

  ))))

  ;; foreach bitmap check if it's in the same dir as the menufile
  ;; else error
  nil
)
;;}}}
;^L
(provide 'autolisp)
;;; end of file -- autolisp.el --
