;;
;; hilit v1.3. Copyright (C) 1991 by Paul Nakada. pnakada@oracle.com
;;
;; A package allowing customizable hiliting of epoch buffers.
;;
;;
;; This file is part of GNU Emacs.

;; LCD Archive Entry:
;; hilit|Paul Nakada|pnakada@us.oracle.com|
;; Mode switched highlighting of regions of buffers with colors and fonts.|
;; 92-04-02|v1.3|~/epoch/hilit.el.Z|

;; HISTORY 
;; 2-Apr-1992          pnakada@oracle.com
;;   Rolled in fix from Bob Weiner for hyperbol problem.
;;   added archive version info to header.
;;   release as v1.3
;; 12-Mar-1992          pnakada@oracle.com
;;   Rolled in bug fixes from jsegal@analagous.com
;; 21-Oct-1991          pnakada@oracle.com
;;   Rolled in epoch-3.2 compatibilty.
;; 14-Oct-1991          pnakada@oracle.com
;;   Upgraded to epoch-4.0.
;;   Remove references to attributes and create global list of epoch styles. 
;;   Remove code that deals with tags.  They just were't as useful as I 
;;   thougt they would be.
;;   Added support for fonts to be specified as a style attribute.
;; 5-Sep-1991		bristor@simba	
;;   Change button-create-internal to tag buttons with 'hilit instead of with
;;   (current-buffer).  Changed unhilit-buffer to remove all buttons in the
;;   current buffer that have that tag.  Added buffer-hilited & toggle-hilit.
;;   Many renamings for consistency.  Added do-hiliting, (buffer-local)
;;   buffer-hilited, toggle-hilit.  No more use of modes; use mode-list
;;   instead.  Both patterns are regular expressions.  Access macros for
;;   pieces of a pattern.  Initialization now at load time.
;; 1-Sep-1991		bristor@simba	
;;   Simplified hilit-buffer by making it use match-{beginning,end}.
;; 30-Aug-1991		bristor@simba	
;;   Added makefile mode hiliting, mode-list, mode-update, and set-modes.
;;   These latter allow users to more easily manage the modes list, avoiding
;;   duplicate entries when adding new packages.
;; 19-Aug-1991		foo@simba	
;;   Added support for Compilation mode.  See also compile.el.
;; 24-Jul-1991		bristor@simba	
;;   Added support for Makefile mode.

;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY.  No author or distributor
;;; accepts responsibility to anyone for the consequences of using it
;;; or for whether it serves any particular purpose or works at all,
;;; unless he says so in writing.  Refer to the GNU Emacs General Public
;;; License for full details.

;;; Everyone is granted permission to copy, modify and redistribute
;;; GNU Emacs, but only under the conditions described in the
;;; GNU Emacs General Public License.   A copy of this license is
;;; supposed to have been given to you along with GNU Emacs so you
;;; can know your rights and responsibilities.  It should be in a
;;; file named COPYING.  Among other things, the copyright notice
;;; and this notice must be preserved on all copies.
;;;

;;;
;;; This package works as follows
;;;
;;;      It can hilit regions of text specified by search expressions.
;;;      Regions are hilited as the file is read into the epoch buffer. 
;;;      When the buffer is saved, no information about these regions is saved.
;;;
;;; how I use hilit:
;;;
;;;  I basically use it to hilit different types of code.
;;;  For C source code, comments in red, function headers, prototypes in blue, 
;;;    and case labels in green.
;;;  For Lisp source code, comments in red.
;;;  For shell scripts, comments in red.
;;; 
;;;  to override default style settings, use (hilit::style-set) in your 
;;;  .emacs after loading hilit.
;;;  please look at the same .emacs file which follows
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlights regions of a file specific to a mode.  Uses variable
;; hilit::mode-list to determine what to hilit for each mode.  Each element
;; of the list has the form
;; (\"modename\" ](\"pstart\" \"pend\" attribute-num)
;;               (\"pstart\" \"pend\" attribute-num)... )
;; pstart and pend are regular expressions denoting the starting and ending
;; of a portion of the buffer to be hilited according to attribute-num.  If
;; pend is nil, the end of line terminates the region hilited.
;;
;; attribute-num is an index into hilit::attrs, not an epoch attribute.
;; Each of the entries in that table are, however, epoch attributes.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar hilit::init-done nil
  "Flag signifying that pn-hilit subsystem has been initialized.")

(defvar hilit::epoch-version4 nil
  "Flag indicating version 4 epoch is running.")

(defvar hilit::saving nil
  "'semaphore' set while in process of saving a file.")

(defvar hilit::styles nil
  "List of styles/attrs depending on epoch version.")

(defvar hilit::read-hooks nil
  "List of hooks to call after visiting a file.")

(defvar hilit::write-hooks nil
  "List of hooks to call before writing a file.")

(defvar hilit::mode-list nil
  "A-list of mode names and regex/styles to use.")

(defvar hilit::do-hiliting t
  "T if we should hilit buffers as we find 'em, nil otherwise.")

(defvar hilit::buffer-hilited nil
  "Non-nil if the current buffer is hilited.")

(make-variable-buffer-local 'hilit::buffer-hilited)
(setq-default hilit::buffer-hilited nil)

(defmacro pattern-start (p) (` (nth 0 (, p))))
(defmacro pattern-end (p)   (` (nth 1 (, p))))
(defmacro pattern-attr (p)  (` (nth 2 (, p))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hilit::hilit-buffer ()
  "Highlights regions of a buffer specific to the buffer's mode."
  (interactive)
  (if hilit::mode-list
      (let ((m (assoc mode-name hilit::mode-list)) (patterns))
      (if (not m) 
	nil				; do nothing if we don't know the mode
	(setq patterns (cdr m))
	(while (setq p (car patterns))
	  (let ((pstart (pattern-start p))
	      (pend (pattern-end p))
	      (pattr (pattern-attr p))
	    )
	    (save-excursion
	      (goto-char 0)
	      (if pend
		(while (re-search-forward pstart nil t nil)
		  (setq hilit::buffer-hilited t)
		  (goto-char (setq start (match-beginning 0)))
		  (if (re-search-forward pend nil t nil)
		    (hilit::button-create-internal
		      start (match-end 0) pattr nil)
		    (forward-char 1)	; to avoid looping
		  )
		)
		(while (re-search-forward pstart nil t nil)
		  (setq hilit::buffer-hilited t)
		  (hilit::button-create-internal
		    (match-beginning 0) (match-end 0)
		    pattr nil)
		  (goto-char (match-end 0)))
	      )
	    )
	  )
	  (setq patterns (cdr patterns))
	)
      )
    )
  )
)

(defun hilit::unhilit-buffer ()
  "Unhilits any regions highlighted by hilit::hilit-buffer."
  (interactive)
  (let ((buttons (button-list))
	b)
    (while (setq b (car buttons))
      (if (eq 'hilit (button-data b))
	  (delete-button b))
      (setq buttons (cdr buttons)))
    (setq hilit::buffer-hilited nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find/write file hooks.  They cause automatic hiliting/rehiliting when
;; files are read/written.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hilit::unhilit-before-save ()
  "Write a hilited buffer to a file.  Useful as a write-file hook."
  (if hilit::do-hiliting
      (if hilit::saving
	  nil
	(hilit::unhilit-buffer)
	(setq hilit::saving t)
	(save-buffer)
	(setq hilit::saving nil)
	(hilit::hilit-buffer)
	(set-buffer-modified-p nil)
	t)))

(defun hilit::hilit-after-find ()
  "Hilit the current buffer, without modifying it.  Useful as a find-file hook."
  (if hilit::do-hiliting
      (progn
        (hilit::hilit-buffer)
        (set-buffer-modified-p nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Button creating and deleting.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hilit::button-clear-internal (start end)
  "Clear all epoch buttons between START and END."
  (let ((pos start button))
    (while (/= end pos)
      (epoch::delete-button-at pos (current-buffer))
      (setq pos (+ 1 pos)))))

(defun hilit::button-create-internal (start end styleindex nuke) 
  "Put a hilit button on the region from START to END having ATTR; if
NUKE then clear any buttons already in that region."
  (if nuke 
      (hilit::button-clear-internal start end))
  (epoch::add-button start end (nth styleindex hilit::styles) 'hilit))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To add a new mode to the hilit list, call hilit::mode-list-update.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hilit::mode-list-update (mode-name mode-list)
  "Adds or updates the hiliting instructions for MODE-NAME in MODE-LIST to the
hiliting mode list."
  (if (assoc mode-name hilit::mode-list)
      (setq hilit::mode-list
	    (delete-if '(lambda (x) (equal (car x) mode-name))
		       hilit::mode-list)))
  (setq hilit::mode-list (cons (cons mode-name mode-list)
                               hilit::mode-list)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To set a style, call hilit::style-set
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun hilit::style-set (styleindex foreground background font)
  "set the foreground, background, and/or font of a given hilit style"
  (let ((style (nth styleindex hilit::styles)))
    (if style
        (if hilit::epoch-version4
            (progn
              (if foreground
                  (set-style-foreground style foreground))
              (if background
                  (set-style-background style background))
              (if font
                  (set-style-font style font)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User interface cruft
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hilit::re-hilit (arg)
  "Re-hilit the current buffer.  With arg, unhilit the current buffer."
  (interactive "P")
  (hilit::unhilit-buffer)
  (setq hilit::buffer-hilited nil)
  (if (not arg)
      (hilit::hilit-buffer))
  )

(global-set-key "\C-c\C-h" 'hilit::re-hilit)

(defun hilit::toggle-hilit (arg)
  "Globally toggle hiliting of this and future files read.  With arg,
forces hiliting off."
  (interactive "P")
  (if arg
      (progn
	(setq hilit::buffer-hilited nil)
	(setq hilit::do-hiliting t))
    (setq hilit::buffer-hilited (not hilit::buffer-hilited))
    )
  (setq hilit::do-hiliting hilit::buffer-hilited)
  (hilit::unhilit-buffer)
  (if hilit::do-hiliting
      (hilit::hilit-buffer))
  (message (format "Hiliting is %s"
		   (if hilit::buffer-hilited
		       "on" "off")))
  )

(global-set-key "\C-c\C-t" 'hilit::toggle-hilit)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization.  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (not hilit::init-done)
    (progn
      (if (boundp 'hyperb:version)
          (var:append 'find-file-hooks '(hilit::hilit-after-find))
        (push 'hilit::hilit-after-find find-file-hooks))
      (push 'hilit::unhilit-before-save write-file-hooks)

      (setq hilit::epoch-version4 
            (string-match " 4" epoch::version))

      (if (not hilit::epoch-version4)
          ;; version 3 init code
          (setq hilit::styles (reserve-attributes 20)))

      (mapcar '(lambda (a) 
                 ;; version 4 init code
                 (if hilit::epoch-version4
                     (let (style)
                       (setq style (make-style))
                       (setq hilit::styles (cons style hilit::styles))
                       (if (cadr a)
                           (set-style-foreground style (cadr a))
                         (set-style-foreground style (foreground)))
                       (set-style-background style (background))
                       (if (caddr a)
                           (set-style-font style (caddr a))))

                   ;; version 3 init code
                   (set-attribute-global
                    (nth (car a) hilit::styles)
                    (if (cadr a)
                        (get-color (cadr a))
                      (foreground))
                    (background))))

              '((0 "red" nil)
                (1 "magenta" nil )
                (2 "blue" nil)
                (3 "purple" nil)
                (4 "RoyalBlue" nil)
                (5 "DarkGoldenrod" nil)
                (6 "firebrick" nil)
                (7 "DarkOrange" nil)
                (8 "DeepPink" nil)
                (9 "ForestGreen" nil)
                (10 nil nil)
                (11 nil nil)
                (12 nil nil)
                (13 nil nil)
                (14 nil nil)
                (15 nil nil)
                (16 nil nil)
                (17 nil nil)
                (18 nil nil)
                (19 nil nil)))

      (if hilit::epoch-version4
          ;; version 4 init code
          (setq hilit::styles (reverse hilit::styles)))

      (setq hilit::init-done t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Built-in support for various modes.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq c-mode-hilit
      '(("/\\*" "\\*/" 0)
	("^[_a-zA-Z][^=\n]*(\\(.*;\\|[^{]*\\)" nil 2) ;; fn decls
	("^#.*" nil 9)
	("^typedef.*" nil 1)
	("^struct.*" nil 1)
	("^enum.*" nil 1)))
(hilit::mode-list-update "C" c-mode-hilit)

(setq c++-mode-hilit
      '(("/\\*" "\\*/" 0)
	("^[_a-zA-Z][^=\n]*(\\(.*;\\|[^{]*\\)" nil 2) ;; fn decls
	("//.*" nil 0)
	("^/.*" nil 0)
	("^#.*" nil 9)
	("^typedef.*" nil 1)
	("^struct.*" nil 1)
	("^class.*" nil 1)
	("^enum.*" nil 1)))
(hilit::mode-list-update "C++" c++-mode-hilit)

(setq text-mode-hilit
      '(("^#.*" nil 0)
        ("[^$]#.*" nil 0)))
(hilit::mode-list-update "Text" text-mode-hilit)

(setq fundamental-mode-hilit
      '(("^#.*" nil 0)
        ("[^$]#.*" nil 0)))
(hilit::mode-list-update "Fundamental" fundamental-mode-hilit)

(setq compilation-mode-hilit
      '(("^[^ \t]*:[0-9]+:.*$" nil 0)
        ("^[^ \t]*:[0-9]+: warning:.*$" nil 2)))
(hilit::mode-list-update "Compilation" compilation-mode-hilit)
(hilit::mode-list-update "grep" compilation-mode-hilit)

(setq makefile-mode-hilit
      '(("^#.*" nil 0)			; comments
	("[^$]#.*" nil 0)		; comments
	("^%.*" nil 1)			; rules
	("^[.][a-zA-Z][a-zA-Z]?\..*" nil 1) ; rules
	("^[_a-zA-Z0-9]+ *\+?=" nil 2)	; variable definition
	("\$[_a-zA-Z0-9]" nil 2)	; variable reference
	("\${[_a-zA-Z0-9]+}" nil 2)	; variable reference
	("\$\([_a-zA-Z0-9]+\)" nil 2)	; variable reference
	("\\( \\|:=\\)[_a-zA-Z0-9]+ *\\+=" nil 2) ; variable definition
	("^include " nil 9)		; include
	))
(hilit::mode-list-update "Makefile" makefile-mode-hilit)

(setq emacs-lisp-mode-hilit
      '(("^;.*" nil 0)
	(";;.*" nil 0)
	("\(defun.*" nil 2)
	("\(defmacro.*" nil 7)
	("\(defvar.*" nil 1)
	("\(defconst.*" nil 5)
	("\(provide.*" nil 9)
	("\(require.*" nil 9)))
(hilit::mode-list-update "Emacs-Lisp" emacs-lisp-mode-hilit)

(provide 'hilit)

;;
;; the following is the epoch specific portion of my .emacs file
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; do any epoch-dependent processing here
;;; running-epoch is a better name for the variable that tells us is we are
;;; in epoch or not
;(defvar running-epoch (boundp 'epoch::version))
;
;(if running-epoch
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; figure out which version of epoch is running
;;;    
;    (let (version4)
;      (setq version4 (not (string= epoch::version "Epoch 3.2.1")))
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load epoch elisp code.  Most/all of this should have been loaded at the
;;; time epoch was dumped.  Load any of these files if necessary.  Change
;;; these "require" statements to "load-file" to pick up local changes to
;;; standard code that was dumped.
;;;
;      (require 'mini-cl)
;      (require 'wrapper)
;      (require 'epoch-util)
;      (require 'epoch)	
;      (require 'event)
;      (require 'mouse)
;      (require 'motion)
;      (require 'property)
;      (require 'button)
;      (if version4
;          (require 'selection))
;      (require 'message)
;      (if (not version4)
;          (progn
;            (require 'server)))		;; not dumped - this will always load
;
;      (if version4
;          (setq load-path
;                (append
;                 (list "/tools/libsun4-os4/epoch-4.0/contrib/hyper/")
;                 load-path))
;        (setq load-path
;              (append
;               (list "/tools/libsun4-os4/epoch-3.2/contrib/hyper/")
;               load-path)))
;            
;      (setq term-file-prefix nil)  ;; don't load xterm stuff!
;      (setq auto-raise-screen nil) ;; don't autoraise
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; set inverse selection style
;;;
;      (set-style-underline  motion::style nil)
;      (set-style-foreground motion::style (foreground))
;      (set-style-background motion::style "grey85")
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; for monochrome users only
;;;      (set-style-underline  motion::style nil)
;;;      (set-style-foreground motion::style (background))
;;;      (set-style-background motion::style (foreground))
;;; 
;
;;; hyperinfo stuff
;      (load "info"     )
;      (load "hyper-man")
;
;;; color hilighting stuff
;      (if version4
;          (load "hilit")
;        (load "hilit"))
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pl/sql mode hilit info  (how to add a hilit mode)
;;;
;      (if version4
;          (progn
;            (setq ada-mode-hilit
;                  '(("--.*" nil 0)
;                    ("^[ \t]*function.*" nil 2)
;                    ("^[ \t]*procedure.*" nil 2)))
;            (hilit::mode-list-update "Ada" ada-mode-hilit)))
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; gnus modes hilit info
;;;
;      (if version4
;          (progn
;            (hilit::style-set 0 nil nil "fixed")
;            (setq gnus-article-mode-hilit
;                  '(("^Subject: .*" nil 10)))
;            (hilit::mode-list-update "Article" gnus-article-mode-hilit)
;            (setq gnus-subject-mode-hilit
;                  '(("^D.*" nil 11)
;                    (".*\\+\\[.*" nil 10)))
;            (hilit::mode-list-update "Subject" gnus-subject-mode-hilit)
;            (hilit::style-set 10 "red" (background) nil)
;            (hilit::style-set 11 "grey75" (background) nil)))
;
;            
;      (if version4
;          (progn
;            (setq gnus-Subject-prepare-hook
;                  '(lambda ()
;                     (hilit::hilit-buffer)))
;            (setq gnus-Article-prepare-hook
;                  '(lambda ()
;                     (hilit::hilit-buffer)))
;            (setq gnus-Mark-article-hook
;                  '(lambda nil 
;                     (let ((cb (current-buffer)))
;                       (or (memq gnus-current-article gnus-newsgroup-marked) 
;                           (gnus-Subject-mark-as-read gnus-current-article))
;                       (gnus-Subject-set-current-mark "+")
;                       (switch-to-buffer gnus-Subject-buffer)
;                       (hilit::hilit-buffer)
;                       (switch-to-buffer cb))))))
;))


