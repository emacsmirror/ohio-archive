;; @(#) tinymatch.el -- Highlight matched text in a buffer easily

;; @(#) $Id: tinymatch.el,v 1.7 1995/11/08 07:43:06 jaalto Release_4 jaalto $
;; @(#) $Keywords: highlight, matching text $
;; $KnownCompatibility: 19.28 $
;; $outlineRegexp: '@+' $
;; $bookMarkRegexp: '[-~+=*#.,'`^]+ &+\\([^ \t]+\\) [-~+=*#.,'`^]+' $
;; $PackageInstallRe: '^[ \t]*;;+[*] ' $

;; This file is *NOT* part of GNU emacs


;;{{{ Documentation

;; Copyright (C) 1995 Jari Aalto
;; Author:       Jari Aalto <jari.aalto@ntc.nokia.com>
;; Maintainer:   Jari Aalto <jari.aalto@ntc.nokia.com>
;; Created:      Apr 5 1995
;;
;; To get information on this program use ident(1) or do M-x tima-version
;; Look at the code with folding.el, tinyfold.el, tinybm.el

;; LCD Archive Entry:
;; tinymatch|Jari Aalto|jari.aalto@ntc.nokia.com|
;; Highlight matched text in a buffer easily, re-search-fwd, re-search-bck ..|
;; 08-Nov-1995|1.7|~/misc/tinymatch.el.Z|

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs.  If you did not, write to the Free Software Foundation,
;; Inc., 675 Mass Ave., Cambridge, MA 02139, USA.


;; ....................................................... &t-install ...
;; - Put this file on your Emacs-Lisp load path, add following into your
;;   ~/.emacs startup file
;;
;;	(require 'tinymatch)
;;
;; - Or use the autoload, much quicker.
;;
;;	(autoload 'tima-re-search-backward	"tinymatch" t t)
;;	(autoload 'tima-re-search-forward	"tinymatch" t t)
;;	(autoload 'tima-looking-at		"tinymatch" t t)
;;	(autoload 'tima-mark-region		"tinymatch" t t)
;;	(autoload 'tima-mouse-mark-region	"tinymatch" t t)
;;	(autoload 'tima-mouse-unmark-region	"tinymatch" t t)
;;
;; - Suggested keybings
;;
;;      (global-unset-key "\C-z")
;;	(global-set-key "\C-zm" 'tima-mark-region)
;;      (global-set-key "\C-zu"
;;			'(lambda (beg end) (interactive "r")
;;			   (tima-mark-region beg end 'remove))))
;;
;;   Bind the mouse functions too


;; ..................................................... &t-commentary ...
;; Briefly:
;; o Shows matched text with color in the buffer.
;; o This is *NOTHING* like font-lock, lazy-lock or hilit19,
;;   which are demand driven packages intended for certain major modes.
;;   Use this package to "manually" mark interesting things in any buffer.
;; o latest highlighting can be undone.

;; RESERVED prefix FOR FUNCTIONS IN THIS FILE
;; - To avoid collisions to other modules I use "tima-" in front of
;;   every function & variable. It stands for '(ti)ny (ma)tch
;; - variable names contain letters 'tima-:', except version vars.


;; PREFACE
;; ========================================
;; - Now, when this hilit stuff is availabel in 19.xx, why don't we
;;   use it for marking texts too, especially when we are looking for
;;   documentation of some program and we want to find certain words, or
;;   that we want to see TABS in the buffer.
;; - This little .el eases the job of visualizing items in buffer.
;;   Now the hilit capability is within everybody's hand with this :-/
;;
;; - Where I use this most is inside GNUS/DING?RMAIL hooks, to mark
;;   interesting peoples articles and seeing where is my articles.
;;   The font-lock/lazy-lock are a little bit too heavy for those things,
;;   because these buffers hold "solid" data, text, that does not change,
;;   and I feel that executing arbitrary 'time-re-search' command to highligh
;;   the headers I want is more easier that with font-lock, especially
;;   when I can change the faces in the fly with 'tima-re-search'.
;;
;;   But the font-lock/lazy-lock are unreplacable in programming modes,
;;   which has to deal with dynamic text.
;;
;; User functions
;; .......................
;; - Mostly this package is designed for lisp programmers, who use provided
;;   function in their hooks, but for quick text highlighting, you can use
;;   these functions:
;;
;;	tima-re-search-forward
;;	tima-re-search-backward
;;	tima-undo
;;
;; Setting different face
;; .......................
;; - If you want permanetly change the face, when marking text
;;   use commands
;;
;;    	tima-search-face-set     ;to set
;;	tima-search-face-reset	 ;to get default color back when your're done
;;
;; - If you want temporarily use some face, supply direct FACE parameter
;;   when you call search functions, like:
;;
;;	 tima-re-search-forward (re &optional level face)
;;
;; WARNING
;; ...........
;; - This is for simple text highlighting only. Like finding certain items
;;   or marking something quickly and temporarily (great for text files)
;; - It is not adviced to mix font-lock/hilit19 and TIMA package, because
;;   If you use tima-clear-buffer-properties, you will wipe out all
;;   properties. But of course if you know what you're doing, then go ahead!


;;}}}
;;{{{ history

;; CHANGE HISTORY
;; ........................................................ &t-history ...
;; [jari]   = jari.aalto@ntc.nokia.com(work), ssjaaa@uta.fi(University)
;;
;; Nov	8	1995    [jari]		19.28	v1.7		Release_4
;; - Many times I had in mind that undo capability of last acion, now
;;   it's there.
;;   tinymatch-load-hook		-> tima-:load-hook
;;   tima-:last-xxx			:+ new variables to save last data
;;   tima-save-data			:+ store search data
;;   tima-undo				:+ undo last highlight
;;   tima-re-search			:! undo support
;;   tima-looking-at			:! undo support
;;   my-gnus-summary-hilit		:! example updated
;;   my-buffer-menu-hook-func		:! example updated
;;
;; Sep	2521	1995    [jari]		19.28	v1.6		NotReleased
;; - I needed mouse funcs that would mark region of important text easily...
;; - updated installation for new funcs
;;   tima-re-search			:! new parameter MODE
;;   tima-re-search-forward		:! now accepts prefix arg for LEVEL
;;   tima-re-search-backward		:! now accepts prefix arg for LEVEL
;;   tima-mouse-mark-region		:+ new
;;   tima-mouse-unmark-region		:+
;;   tima-mark-region			:+
;;   tinymatch-load-hook		:+
;;
;; Aug	21	1995    [jari]		19.28	v1.5		Release_3
;; - Only small fine tunes to examples, function descriptions adjusted
;;
;; Aug	15	1995    [jari]		19.28	v1.4		Release_2
;; - Added optional 'face' parameter to every function, so that the
;;   tima-search-face-set isn't needed for temporary face changes.
;; - tima-re-search :  Corrected the main function a little.
;; - Added more examples : dired, new-buffer-menu.
;;
;; Aug	10	1995    [jari]		19.28	v1.3		Release_1
;; - Just some cleanup.
;;
;; Apr	5	1995    [jari]		19.28	v1.2		NotReleased
;; - So that we can use several colors, I added restoring macro.
;; - Ready example for using these functions in GNUS/DING is provided
;;   at the end. See also other examples.
;; - Added parameter maxp to tima-re-search to limit the search
;;
;; Apr	5	1995    [jari]		19.28	v1.1		NotReleased
;; - Just what I needed for now

;; To do list:
;; ========================================
;; - Some undo history list, when I have time...

;;}}}



;;; Code:


;;; ......................................................... &require ...


;;; .......................................................... &v-bind ...

;;; ......................................................... &v-hooks ...

(defvar tima-:load-hook nil
  "*Hook run when file has been loaded")


;;; ... private variables ................................. &v-private ...

(defvar tima-:last-start-point nil
"*Private. Holds last search data item.")

(defvar tima-:last-func nil
"*Private. Holds last search data item.")

(defvar  tima-:last-re nil
"*Private. Holds last search data item.")

(defvar  tima-:last-level nil
"*Private. Holds last search data item.")

(defvar   tima-:last-mode nil
"*Private. Holds last search data item.")


;;; ... user configurable .................................. &v-config ...


(defvar tima-:face-search-default 'highlight
  "*Default face used when marking searched text.")


;;; For now, only search face is used, but maybe in the future the others..
;;;
(defconst tima-:face-table
  '(
    (search . highlight)
    (warn   . region)
    (head   . bold)
    )
  "*Faces used for marking text")

;;{{{ version

;;; ... version info ...................................... &v-version ...

(defconst tima-version
  "$Revision: 1.7 $"
  "Latest version number.")


(defconst tima-version-id
  "$Id: tinymatch.el,v 1.7 1995/11/08 07:43:06 jaalto Release_4 jaalto $"
  "Latest modification time and version number.")

(defconst tima-version-doc
  "tinymatch.el -- Hililight matched text in a buffer

First created: Apr. 5 1995
Author       : Jari Aalto <jari.aalto@ntc.nokia.com
Maintainer   : Jari Aalto <jari.aalto@ntc.nokia.com

Suggestions and enchancements welcome!"
  "Brief version and contact info")

;;; ----------------------------------------------------------------------
(defun tima-version ()
  "version information."
  (interactive)
  (let* ((ob (current-buffer))
	 (bp (get-buffer-create "*version*"))
	 )
      (set-buffer bp)			;for insert
      (erase-buffer)
      (switch-to-buffer-other-window bp)
      (insert
       tima-version-doc
       "\n\ncurrent version:\n" tima-version-id)
      (pop-to-buffer  ob)
    ))

;;}}}

;;; ########################################################### &funcs ###



;;; ----------------------------------------------------------------------
;;;
(defmacro tima-search-face-reset ()
  "If you use many colors to hilight text. Remeber to call this macro
when you're finished."
  (list
   'setcdr (list 'assq ''search 'tima-:face-table)
  'tima-:face-search-default))


;;; ----------------------------------------------------------------------
;;;
(defmacro tima-search-face-set (face)
  "To change search color. use this macro."
  (list 'setcdr (list 'assq ''search 'tima-:face-table) face))

;;; ----------------------------------------------------------------------
;;;
(defmacro tima-face (face)
  "Returns real face when logical face is given."
  ;;  This way the global variable does not float around the file
  (list 'cdr (list 'assoc face 'tima-:face-table)))




;;; ----------------------------------------------------------------------
;;;
(defun tima-save-data (re level func mode beg)
  "Saving the search values for undo"
  (setq tima-:last-start-point beg
	tima-:last-func	      func
	tima-:last-re	      re
	tima-:last-level       level
	tima-:last-mode	      mode
	))

;;; ----------------------------------------------------------------------
;;;
(defun tima-undo ()
  "Undoes last highlighting."
  (interactive)
  (let* ((func	    tima-:last-func)
	 (beg       tima-:last-start-point)
	 (func      tima-:last-func)
	 (re	    tima-:last-re)
	 (level	    tima-:last-level)
	 (mode	    tima-:last-mode)
	 )
    (save-excursion
      (goto-char beg)
      (cond
       ((eq func 'looking-at)
	(tima-looking-at re level 'default))
       (t
	(tima-re-search
	 re
	 (if (eq func 're-search-backward)
	     'back nil)
	 level
	 nil
	 'default
	 mode)
	))
      )))


;;; ----------------------------------------------------------------------
;;;
(defun tima-clear-buffer-properties ()
  "Removes properties from buffer"
  (interactive)
  (let* ()
    (tima-clear-region-properties (point-min) (point-max))
    (if (interactive-p) (message "*properties cleared"))
    (redraw-display)
    ))

;;; ----------------------------------------------------------------------
;;;
(defun tima-clear-region-properties (beg end)
  "Removes properties from BEG - END. Affects read only buffers too."
  (interactive "r")
  (let* ((state buffer-read-only)
	 )
    (set-text-properties beg end nil)
    (set-buffer-modified-p nil)
    (setq buffer-read-only state)	;restore
    ))


;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tima-re-search (re &optional direction level maxp face mode)
  "Highlight found text with search face.

Input:
  re		,str  ,regexp
  direction	,bool ,non-nil means backward
  level		,nbr  ,which subexpression in re to highlight, defaut is 0
  maxp		,nbr  ,last search point [default until bob/eob]
  face		,sym  ,face symbol
  mode		,nbr  ,signifies that func should hightlight all matches
                       that occur within LEVEL..NBR
                       if you have lot's of xx(match)yy|zz(match)tt|
                       the subexpression are counted from lefto to
		       right: 1,2 ...
"
  (let* ((func	    (if direction 're-search-backward 're-search-forward))
	 (face	    (or face (tima-face 'search)))
	 (state	    buffer-read-only)
	 (level	    (if level level 0))
	 (maxp	    (or maxp
			(if direction
			    (point-min)
			  (point-max))))
	 (max-level (1+ (or mode level)))
	 count
	 bp ep				;beg/end points
	 loop
	 )

    ;;   Saving the search values for possible undo.
    (tima-save-data re level func mode (point))

    (setq buffer-read-only nil)		;allow changes
    (while (funcall func re nil maxp)
      (setq count level)
      (while (<= count max-level)
	(setq  bp (match-beginning count)  ep (match-end count))
;;;	(d! level (read-match count) (read-match 0) re)
	(if (null bp)
	    nil				;no match in this level
	  (put-text-property bp ep 'face face))
	(setq count (1+ count))
	))
    (set-buffer-modified-p nil)
    (setq buffer-read-only state)	;restore
    ))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tima-looking-at (re &optional level face )
  "Highlight found text with FACE. The LEVEL is subexpression to hilit."
  (interactive "slook at: ")
  (let* (
	 (face	    (or face (tima-face 'search)))
	 (state	    buffer-read-only)
	 (level	    (or level 0))
	 )
    (tima-save-data re level 'looking-at nil (point))

    (setq buffer-read-only nil)		;allow changes
    (if (and (looking-at re)
	     (match-end level))
	(put-text-property
	 (match-beginning level) (match-end level) 'face face))
    (set-buffer-modified-p nil)
    (setq buffer-read-only state)	;restore
    ))


;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tima-re-search-forward (re &optional level face mode)
  "Search and highlight forward until point-max. Optional prefix arg
tells which SUBLEVEL in RE we should highlight.
"
  (interactive "sRE: \nP")
  (tima-re-search re nil level nil face mode))


;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tima-re-search-backward (re &optional level face mode)
  "Search and highlight forward until point-min. Optional prefix arg
tells which SUBLEVEL in RE we should highlight.
"
  (interactive "sRE: \nP")
  (tima-re-search re 'back level nil face mode))


;;; ----------------------------------------------------------------------
;;; - These are handy when you want to "mark" ceratin texts for quick ref.
;;;
;;;###autoload
(defun tima-mouse-mark-region (beg end event)
  "Highlights region."
  (interactive "r\ne")
  (tima-mark-region beg end))


;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tima-mouse-unmark-region (beg end event)
  "Removes highlight from region."
  (interactive "r\ne")
  (tima-mark-region beg end 'remove))

;;; ----------------------------------------------------------------------
;;;
;;;###autoload
(defun tima-mark-region (beg end &optional remove face)
  "Highlights region. With optional prefix arg REMOVE, removes
all faces."
  (interactive "r\nP")
  (let* ((face   (if remove
		     'default
		   (or face (tima-face 'search))))
	 (state	 buffer-read-only)
	)
    (put-text-property beg end 'face face)))



;;{{{ examples

;;; ############################################### &example-gnus/ding ###
;;; - This is brief example that you can use staight away in GNUS and in
;;;   any version of DING.
;;;
;;; - The examples can be automatically extracted with function
;;;   ti::m-pkg-rip-magic from tinylib.el, follow the instructions
;;;   there.
;;;
;;;   PMIN is a macro from tinylibm.el to go point-min

;;* ;;  Purposively use SETQ, since I want to have only one main func
;;* (setq gnus-summary-prepare-hook 'my-gnus-summary-prepare-hook)
;;* (setq gnus-select-article-hook  'my-gnus-article-prepare-hook)

;;* ;;; ----------------------------------------------------------------------
;;* ;;;
;;* (defun my-gnus-summary-prepare-hook ()
;;*   "Things to do when entering group."
;;*   ;; (my-gnus-summary-set-keys)
;;*   (my-gnus-summary-hilit)
;;*   ;; (my-gnus-summary-kill-default)
;;*   nil
;;*   )

;;* ;;; ----------------------------------------------------------------------
;;* ;;;
;;* (defun my-gnus-article-prepare-hook ()
;;*   "Activates when article is displayed."
;;*   (let* ((n  (buffer-name (current-buffer)))
;;* 	 (art gnus-article-buffer)
;;* 	 )
;;*     (save-excursion
;;*       (set-buffer art)
;;*       ;; (my-gnus-article-use-map)   ;private keymap
;;*       (my-gnus-article-hilit)
;;*       (toggle-read-only 1)
;;*       nil
;;*       )))


;;* _
;;* ;;; ----------------------------------------------------------------------
;;* ;;;
;;* (defun my-gnus-summary-hilit ()
;;*   "Hilights some thigs from the Summary buffer"
;;*   (interactive)
;;*   (let* (
;;* 	 (grp gnus-newsgroup-name)
;;* 	 (aid (if (boundp 'my-anon-account) my-anon-account nil))
;;* 	 (an  (if (boundp 'my-anon-nick-name) my-anon-nick-name nil))
;;* 	 (re
;;* 	  (concat
;;* 	   "\\("			;start the regexp
;;* 	   ;; The perl group Gurus!
;;*            "tchrist\\|lwall\\|merlyn\\|ian@pipex\\|Tim.Bu"
;;* _
;;* 	   "\\|lindfors\\|n151595\\|Timo.Sinervo\\|kemo"
;;* 	   "\\|jukka@datact\\|Markus.Lam"
;;* _
;;* 	   ;; Games group
;;* 	   "\\|buzz"			;thrustmaster joysticks
;;* 	   "\\|ratava"			;elite WWW page
;;* 	   "\\|microprose"		;any new games ?
;;* 	   "\\)[^\] ]+" ))		;end the regexp
;;* _
;;* 	 (re-my				;my account names
;;* 	  (concat
;;* 	   "\\("
;;* 	   "ssjaaa\\|jaalto\\|jari.aal"
;;* 	   (and aid (concat "\\|" aid))
;;* 	   (and an  (concat "\\|" an))
;;* 	   "\\)[^\] ]+" ))
;;* _
;;* 	 (re-games1
;;* 	  (concat
;;* 	   "C[&]C\\|C +[&] +C"
;;* 	   "\\|\\].*Com.*conq\\(uer\\)?"
;;* 	   "\\|Silent hunter"
;;* 	   "\\|pirates\\|quake\\|Apache"
;;* 	   "\\|\\(Wing Commander\\|WC\\).*\\(III\\|3\\|4\\|IV\\)"
;;* ;;	   "\\|"
;;* _
;;* 	   ))
;;* _
;;* _
;;* 	 (re-hot
;;* 	  (concat
;;* 	   "c[ae][ae]sar\\|jagged.*"
;;* 	   "\\|Pax Imperia"
;;* 	   "\\|moo\\|master.*orion"
;;* 	   "\\|us[mn]f\\|privateer"
;;* 	   "\\|Fleet Defender"
;;* 	   "\\|Master Of Antares\\|Moa"
;;* 	   "\\|Silent Hunter"
;;* 	   ))
;;* _
;;* 	 )
;;* _
;;*     (setq RE re-games1)
;;*     (save-excursion
;;*       (tima-clear-buffer-properties)	;allows successive calls
;;*       (PMIN) (tima-re-search-forward re)
;;*       (PMIN) (tima-re-search-forward re-my 0     'region)
;;*       (PMIN) (tima-re-search-forward re-games1 0 'my-face-yellow)
;;*       (PMIN) (tima-re-search-forward re-hot 0    'region)
;;*       )))
;;* _


;;* ;;; ----------------------------------------------------------------------
;;* ;;;
;;* (defun  my-gnus-article-hilit ()
;;*   "Article hilit."
;;*   ;; Same color is enough this time
;;*   (save-excursion
;;*     (PMIN)
;;*     ;; I'm interested in DOMAIN name only, what's the country ?
;;*     (tima-re-search-forward "From: .*@\\([^ \t\n>]+\\)" 1)
;;*     (PMIN) (tima-re-search-forward  "Subject: \\(.*\\)" 1))
;;*   )

;;; #################################################### &example-tabs ###
;;; - You want to see where the TABS are? This toggles TAB viewing.

;;* ;;  (global-set-key "\C-ct" 'my-tabs-hilit)
;;* ;;
;;* (defun my-tabs-hilit ()
;;*   "toggless hilit/dehiliting tabs in buffer."
;;*   (interactive)
;;*   (let* (prop
;;* 	 )
;;*     (save-excursion
;;*       (goto-char (point-min))
;;*       (if (null (re-search-forward "\t" nil t))
;;* 	  nil				;no tabs in buffer
;;* 	;; is the tab marked?
;;* 	(setq prop (get-text-property (point) 'face))
;;* 	(cond
;;* 	 ((or (eq prop nil) (eq prop 'default))
;;* 	  ;; Do hilit
;;* 	  (beginning-of-line)
;;* 	  (tima-re-search-forward "\t+")
;;* 	  )
;;* 	 (t
;;* 	  ;; Remove hilit
;;* 	  (beginning-of-line)
;;* 	  (tima-re-search-forward "\t+" 0 'default )
;;* 	  ))
;;* 	))
;;*     ))

;;; ################################################### &example-dired ###
;;; Let's go dired then...


;;* (add-hook 'dired-after-readin-hook	'my-dired-after-readin-hook)
;;* _
;;* _
;;* (defun my-dired-after-readin-hook ()
;;*   "After each dired read"
;;*   (let* ((re "\\.o$\\|~$")          ;delete obj & backup files
;;* 	 )
;;*     ;;  Is this new directory buffer ..
;;*     (if (null (string-match "dired" mode-name))
;;* 	nil
;;*       ;; (dired-sort-by-size)       ;see dired-sort.el
;;*       (toggle-read-only)		;dired goes read-only, *hmpf*
;;*       (flush-lines re)			;don't wanna see these
;;*       (toggle-read-only)
;;* _
;;*       (my-dired-hilit)
;;*       )
;;*     nil
;;*     ))
;;* _
;;* ;;; ----------------------------------------------------------------------
;;* ;;;
;;* (defun my-dired-hilit ()
;;*   "Dired file hilit."
;;*   (let* ((re1 "[-.a-zA-Z0-9_]+\\(\\.el\\|\\.cc\\)$")
;;* 	 (re2 "[-.a-zA-Z0-9_]+\\(\\.g?[z]\\|\\.ZIP\\)$")
;;* 	 )
;;*     (save-excursion
;;*       (PMIN) (tima-re-search-forward re1 1 'highlight)
;;*       (PMIN) (tima-re-search-forward re2 1 'region)
;;*       )
;;*     ))
;;* _



;;; ############################################# &example-buffer-menu ###
;;; ** requires new-buffer.el
;;; ** Better buffer-menu package by mernst@theory.lcs.mit.edu (Michael Ernst)
;;;
;;; - this one highlights some filenames in buffer menu.


;;* ;;; Make some faces for me. The defvar prevents redefinition.
;;* _
;;* (defvar my-bg-color-default
;;*   (if window-system
;;*       (x-get-resource ".background" "*Background")
;;*     "DeepSkyBlue3")
;;*   "My bg color")
;;* _
;;* (defvar my-fc-blue
;;*   (progn
;;*     (make-face 'my-face-blue)
;;*     (set-face-foreground 'my-face-blue "blue")
;;*     (set-face-background 'my-face-blue my-bg-color-default)
;;*     'my-face-blue))
;;* _
;;* (defvar my-fc-yellow
;;*   (progn
;;*     (make-face 'my-face-yellow)
;;*     (set-face-foreground 'my-face-yellow "yellow")
;;*     (set-face-background 'my-face-yellow my-bg-color-default)
;;*     'my-face-yellow))
;;* _
;;* _
;;* (add-hook 'buffer-menu-hook 'my-buffer-menu-hook-func)
;;* _
;;* _
;;* _
;;* ;;; ----------------------------------------------------------------------
;;* ;;; See Buffer-menu-kill-regexp
;;* ;;;
;;* (defun my-buffer-menu-hook-func ()
;;*   "Some arranging/hilit in buffer menu"
;;*   (let* (
;;* _
;;*          ;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ file extensions ^^^
;;* 	 (re-el    "\\([-.a-zA-Z0-9_]+\\.el\\)[ \t]+[0-9]")
;;* _
;;* 	 ;;  These are my emacs config files, like:  '.emacs.dired'
;;* 	 (re-ema   "\\.emacs\\.\\([-.a-zA-Z0-9_]+\\)[ \t]+[0-9]")
;;* _
;;* 	 ;;  C/C++
;;* 	 (re-cc	   "[-.a-zA-Z0-9_]+\\(\\.cc\\)[ \t]+[0-9]")
;;* 	 (re-h	   "[-.a-zA-Z0-9_]+\\(\\.h\\)[ \t]+[0-9]")
;;* 	 (re-cc2   "wmp\\(...\\)mx\\(\\.h\\|\\.cc\\)[ \t]+[0-9]")
;;* _
;;* 	 ;;  Perl
;;* 	 (re-pl    "[-.a-zA-Z0-9_]+\\(\\.pls?\\|\\.pm\\)[ \t]+[0-9]")
;;* _
;;* 	 ;;  Others
;;* 	 (re-otw   "[-.a-zA-Z0-9_]+\\(\\.html\\)[ \t]+[0-9]")
;;* _
;;* 	 ;;  Shell
;;* 	 (re-sh    "[-.a-zA-Z0-9_]+\\(\\.c?sh\\|.awks?\\)[ \t]+[0-9]")
;;* _
;;* _
;;* _
;;*          ;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ modes ^^^
;;* 	 (re-m-mail "[0-9][\t ]*\\(news\\|r?mail\\)")
;;* _
;;* 	 ;;  Others
;;* 	 (re-m-otw "[0-9][\t ]*\\(Dired\\|Man\\)")
;;* _
;;* 	 ;;  Special
;;* 	 (re-m-spe   "[0-9][\t ]*Internal[ \t]+\\(Ange-ftp\\)")
;;* _
;;*          ;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ dir ^^^
;;* 	 (re-d-txt  "/\\(w?txt\\|vax\\)/")
;;* 	 (re-d-code "/\\(elisp\\)/")
;;* 	 (re-d-bin  "/\\(bin2?\\)/")
;;* 	 (re-d-uta  "/\\(uta\\)/")
;;* _
;;* _
;;* _
;;*          ;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ kill ^^^
;;* 	 ;;  I DON'T wanna see these buffers
;;* 	 (kill-re
;;* 	  (concat "Completion\\|Help\\|[*].?te?mp\\|ispell"
;;* 		  "\\|compile-log\\|[*]Info"))
;;* _
;;* 	 (yellow (concat
;;* 		  re-el
;;* 		  "\\|" re-cc2
;;* 		  "\\|" re-d-code
;;* 		  ))
;;* 	 (blue   (concat
;;* 		  re-otw
;;* 		  "\\|" re-sh
;;* 		  "\\|" re-d-bin
;;* 		  "\\|" re-m-otw
;;* 		  ))
;;* _
;;* 	 (spe    (concat
;;* 		  re-m-spe
;;* 		  ))
;;* _
;;* 	 (region (concat
;;* 		  re-ema
;;* 		  "\\|" re-h
;;* 		  "\\|" re-m-mail
;;* 		  ))
;;* 	 (norm   (concat
;;* 		  re-cc
;;* 		  "\\|" re-d-txt
;;* 		  ))
;;* _
;;* 	 (file1  (concat
;;* 		  re-pl
;;* 		  ))
;;* _
;;* 	 )
;;*     (define-key Buffer-menu-mode-map [mouse-3] 'Buffer-menu-other-window)
;;* _
;;*     (save-excursion
;;*       ;; PMIN is macro for point-min, see tinylibm.el
;;*       (PMIN) (tima-re-search-forward yellow 1   'my-face-yellow 3)
;;*       (PMIN) (tima-re-search-forward blue   1   'my-face-blue 4)
;;*       (PMIN) (tima-re-search-forward spe    1   'my-face-yellow 1)
;;* _
;;*       (PMIN) (tima-re-search-forward region 1   'region 2)
;;*       (PMIN) (tima-re-search-forward norm   1   'highlight 3)
;;*       (PMIN) (tima-re-search-forward file1  1   'highlight 3)
;;* _
;;*       ;;  Special case, overwrites  part of region face
;;*       (PMIN) (tima-re-search-forward "\\.\\(sun\\).*[0-9]" 1  )
;;* _
;;*       ;;  can't catenate these regexps because first one matched is
;;*       ;;  used and rest forgotten.
;;*       (PMIN) (tima-re-search-forward "ema\\(crs\\).*[ \t]+[0-9]"
;;* 				     1 'my-face-yellow)
;;*       (PMIN) (tima-re-search-forward "\\(mail\\|csh\\)rc.*[ \t]+[0-9]"
;;* 				     1 'region)
;;* _
;;*       )
;;*     ;; Deleting unwanted buffers from the view
;;*     (PMIN)     (toggle-read-only)
;;*     (flush-lines kill-re)
;;*     nil					;hook ret val
;;*     ))


;;}}}



(provide     'tinymatch)
(run-hooks   'tima-:load-hook)
;;; ................ end of tinymatch.el ...................................
