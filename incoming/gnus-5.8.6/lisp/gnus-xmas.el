;;; gnus-xmas.el --- Gnus functions for XEmacs
;; Copyright (C) 1995,96,97,98,99 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'text-props)
(defvar menu-bar-mode (featurep 'menubar))
(require 'messagexmas)
(require 'wid-edit)

(defgroup gnus-xmas nil
  "XEmacsoid support for Gnus"
  :group 'gnus)

(defcustom gnus-xmas-glyph-directory nil
  "Directory where Gnus logos and icons are located.
If this variable is nil, Gnus will try to locate the directory
automatically."
  :type '(choice (const :tag "autodetect" nil)
		 directory)
  :group 'gnus-xmas)

;;(format "%02x%02x%02x" 114 66 20) "724214"

(defvar gnus-xmas-logo-color-alist
  '((flame "#cc3300" "#ff2200")
    (pine "#c0cc93" "#f8ffb8")
    (moss "#a1cc93" "#d2ffb8")
    (irish "#04cc90" "#05ff97")
    (sky "#049acc" "#05deff")
    (tin "#6886cc" "#82b6ff")
    (velvet "#7c68cc" "#8c82ff")
    (grape "#b264cc" "#cf7df")
    (labia "#cc64c2" "#fd7dff")
    (berry "#cc6485" "#ff7db5")
    (dino "#724214" "#1e3f03")
    (neutral "#b4b4b4" "#878787")
    (september "#bf9900" "#ffcc00"))
  "Color alist used for the Gnus logo.")

(defcustom gnus-xmas-logo-color-style 'dino
  "*Color styles used for the Gnus logo."
  :type '(choice (const flame) (const pine) (const moss)
		 (const irish) (const sky) (const tin)
		 (const velvet) (const grape) (const labia)
		 (const berry) (const neutral) (const september)
		 (const dino))
  :group 'gnus-xmas)

(defvar gnus-xmas-logo-colors
  (cdr (assq gnus-xmas-logo-color-style gnus-xmas-logo-color-alist))
  "Colors used for the Gnus logo.")

(defcustom gnus-article-x-face-command
  (if (or (featurep 'xface)
	  (featurep 'xpm))
      'gnus-xmas-article-display-xface
    "{ echo '/* Width=48, Height=48 */'; uncompface; } | icontopbm | ee -")
  "*String or function to be executed to display an X-Face header.
If it is a string, the command will be executed in a sub-shell
asynchronously.	 The compressed face will be piped to this command."
  :type '(choice string function))

;;; Internal variables.

;; Don't warn about these undefined variables.

(defvar gnus-group-mode-hook)
(defvar gnus-summary-mode-hook)
(defvar gnus-article-mode-hook)

;;defined in gnus.el
(defvar gnus-active-hashtb)
(defvar gnus-article-buffer)
(defvar gnus-auto-center-summary)
(defvar gnus-current-headers)
(defvar gnus-level-killed)
(defvar gnus-level-zombie)
(defvar gnus-newsgroup-bookmarks)
(defvar gnus-newsgroup-dependencies)
(defvar gnus-newsgroup-selected-overlay)
(defvar gnus-newsrc-hashtb)
(defvar gnus-read-mark)
(defvar gnus-refer-article-method)
(defvar gnus-reffed-article-number)
(defvar gnus-unread-mark)
(defvar gnus-version)
(defvar gnus-view-pseudos)
(defvar gnus-view-pseudos-separately)
(defvar gnus-visual)
(defvar gnus-zombie-list)
;;defined in gnus-msg.el
(defvar gnus-article-copy)
(defvar gnus-check-before-posting)
;;defined in gnus-vis.el
(defvar gnus-article-button-face)
(defvar gnus-article-mouse-face)
(defvar gnus-summary-selected-face)
(defvar gnus-group-reading-menu)
(defvar gnus-group-group-menu)
(defvar gnus-group-misc-menu)
(defvar gnus-summary-article-menu)
(defvar gnus-summary-thread-menu)
(defvar gnus-summary-misc-menu)
(defvar gnus-summary-post-menu)
(defvar gnus-summary-kill-menu)
(defvar gnus-article-article-menu)
(defvar gnus-article-treatment-menu)
(defvar gnus-mouse-2)
(defvar standard-display-table)
(defvar gnus-tree-minimize-window)

(defun gnus-xmas-set-text-properties (start end props &optional buffer)
  "You should NEVER use this function.  It is ideologically blasphemous.
It is provided only to ease porting of broken FSF Emacs programs."
  (if (stringp buffer)
      nil
    (map-extents (lambda (extent ignored)
                   (remove-text-properties
                    start end
                    (list (extent-property extent 'text-prop) nil)
                    buffer)
		   nil)
                 buffer start end nil nil 'text-prop)
    (gnus-add-text-properties start end props buffer)))

(defun gnus-xmas-highlight-selected-summary ()
  ;; Highlight selected article in summary buffer
  (when gnus-summary-selected-face
    (when gnus-newsgroup-selected-overlay
      (delete-extent gnus-newsgroup-selected-overlay))
    (setq gnus-newsgroup-selected-overlay
	  (make-extent (gnus-point-at-bol) (gnus-point-at-eol)))
    (set-extent-face gnus-newsgroup-selected-overlay
		     gnus-summary-selected-face)))

(defcustom gnus-xmas-force-redisplay nil
  "*If non-nil, force a redisplay before recentering the summary buffer.
This is ugly, but it works around a bug in `window-displayed-height'."
  :type 'boolean
  :group 'gnus-xmas)

(defun gnus-xmas-switch-horizontal-scrollbar-off ()
  (when (featurep 'scrollbar)
    (set-specifier scrollbar-height (cons (current-buffer) 0))))

(defun gnus-xmas-summary-recenter ()
  "\"Center\" point in the summary window.
If `gnus-auto-center-summary' is nil, or the article buffer isn't
displayed, no centering will be performed."
  ;; Suggested by earle@mahendo.JPL.NASA.GOV (Greg Earle).
  ;; Recenter only when requested.  Suggested by popovich@park.cs.columbia.edu.
  ;; Force redisplay to get properly computed window height.
  (when gnus-xmas-force-redisplay
    (sit-for 0))
  (when gnus-auto-center-summary
    (let* ((height (if (fboundp 'window-displayed-height)
		       (window-displayed-height)
		     (- (window-height) 2)))
	   (top (cond ((< height 4) 0)
		      ((< height 7) 1)
		      (t (if (numberp gnus-auto-center-summary)
			     gnus-auto-center-summary
			   2))))
	   (bottom (save-excursion (goto-char (point-max))
				   (forward-line (- height))
				   (point)))
	   (window (get-buffer-window (current-buffer))))
      (when (get-buffer-window gnus-article-buffer)
	;; Only do recentering when the article buffer is displayed,
	;; Set the window start to either `bottom', which is the biggest
	;; possible valid number, or the second line from the top,
	;; whichever is the least.
	;; NOFORCE parameter suggested by Daniel Pittman <daniel@danann.net>.
	(set-window-start
	 window (min bottom (save-excursion (forward-line (- top)) (point))) 
	 t))
      ;; Do horizontal recentering while we're at it.
      (when (and (get-buffer-window (current-buffer) t)
		 (not (eq gnus-auto-center-summary 'vertical)))
	(let ((selected (selected-window)))
	  (select-window (get-buffer-window (current-buffer) t))
	  (gnus-summary-position-point)
	  (gnus-horizontal-recenter)
	  (select-window selected))))))

(defun gnus-xmas-summary-set-display-table ()
  ;; Setup the display table -- like `gnus-summary-setup-display-table',
  ;; but done in an XEmacsish way.
  (let ((table (make-display-table))
	(i 32))
    ;; Nix out all the control chars...
    (while (>= (setq i (1- i)) 0)
      (aset table i [??]))
    ;; ... but not newline and cr, of course.  (cr is necessary for the
    ;; selective display).
    (aset table ?\n nil)
    (aset table ?\r nil)
    ;; We keep TAB as well.
    (aset table ?\t nil)
    ;; We nix out any glyphs over 126 below ctl-arrow.
    (let ((i (if (integerp ctl-arrow) ctl-arrow 160)))
      (while (>= (setq i (1- i)) 127)
	(unless (aref table i)
	  (aset table i [??]))))
    ;; Can't use `set-specifier' because of a bug in 19.14 and earlier
    (add-spec-to-specifier current-display-table table (current-buffer) nil)))

(defun gnus-xmas-add-text-properties (start end props &optional object)
  (add-text-properties start end props object)
  (put-text-property start end 'start-closed nil object))

(defun gnus-xmas-put-text-property (start end prop value &optional object)
  (put-text-property start end prop value object)
  (put-text-property start end 'start-closed nil object))

(defun gnus-xmas-extent-start-open (point)
  (map-extents (lambda (extent arg)
		 (set-extent-property extent 'start-open t))
	       nil point (min (1+ (point)) (point-max))))

(defun gnus-xmas-article-push-button (event)
  "Check text under the mouse pointer for a callback function.
If the text under the mouse pointer has a `gnus-callback' property,
call it with the value of the `gnus-data' text property."
  (interactive "e")
  (set-buffer (window-buffer (event-window event)))
  (let* ((pos (event-closest-point event))
	 (data (get-text-property pos 'gnus-data))
	 (fun (get-text-property pos 'gnus-callback)))
    (goto-char pos)
    (when fun
      (funcall fun data))))

(defun gnus-xmas-move-overlay (extent start end &optional buffer)
  (set-extent-endpoints extent start end buffer))

(defun gnus-xmas-kill-all-overlays ()
  "Delete all extents in the current buffer."
  (map-extents (lambda (extent ignore)
		 (delete-extent extent)
		 nil)))

(defun gnus-xmas-window-top-edge (&optional window)
  (nth 1 (window-pixel-edges window)))

(defun gnus-xmas-tree-minimize ()
  (when (and gnus-tree-minimize-window
	     (not (one-window-p)))
    (let* ((window-min-height 2)
	   (height (1+ (count-lines (point-min) (point-max))))
	   (min (max (1- window-min-height) height))
	   (tot (if (numberp gnus-tree-minimize-window)
		    (min gnus-tree-minimize-window min)
		  min))
	   (win (get-buffer-window (current-buffer)))
	   (wh (and win (1- (window-height win)))))
      (when (and win
		 (not (eq tot wh)))
	(let ((selected (selected-window)))
	  (select-window win)
	  (enlarge-window (- tot wh))
	  (select-window selected))))))

;; Select the lowest window on the frame.
(defun gnus-xmas-appt-select-lowest-window ()
  (let* ((lowest-window (selected-window))
	 (bottom-edge (car (cdr (cdr (cdr (window-pixel-edges))))))
         (last-window (previous-window))
         (window-search t))
    (while window-search
      (let* ((this-window (next-window))
             (next-bottom-edge (car (cdr (cdr (cdr
                                               (window-pixel-edges
						this-window)))))))
        (when (< bottom-edge next-bottom-edge)
	  (setq bottom-edge next-bottom-edge)
	  (setq lowest-window this-window))

        (select-window this-window)
        (when (eq last-window this-window)
	  (select-window lowest-window)
	  (setq window-search nil))))))

(defmacro gnus-xmas-menu-add (type &rest menus)
  `(gnus-xmas-menu-add-1 ',type ',menus))
(put 'gnus-xmas-menu-add 'lisp-indent-function 1)

(defun gnus-xmas-menu-add-1 (type menus)
  (when (and menu-bar-mode
	     (gnus-visual-p (intern (format "%s-menu" type)) 'menu))
    (while menus
      (easy-menu-add (symbol-value (pop menus))))))

(defun gnus-xmas-group-menu-add ()
  (gnus-xmas-menu-add group
    gnus-group-reading-menu gnus-group-group-menu gnus-group-misc-menu))

(defun gnus-xmas-summary-menu-add ()
  (gnus-xmas-menu-add summary
    gnus-summary-misc-menu gnus-summary-kill-menu
    gnus-summary-article-menu gnus-summary-thread-menu
    gnus-summary-post-menu ))

(defun gnus-xmas-article-menu-add ()
  (gnus-xmas-menu-add article
    gnus-article-article-menu gnus-article-treatment-menu))

(defun gnus-xmas-score-menu-add ()
  (gnus-xmas-menu-add score
    gnus-score-menu))

(defun gnus-xmas-pick-menu-add ()
  (gnus-xmas-menu-add pick
    gnus-pick-menu))

(defun gnus-xmas-topic-menu-add ()
  (gnus-xmas-menu-add topic
    gnus-topic-menu))

(defun gnus-xmas-binary-menu-add ()
  (gnus-xmas-menu-add binary
    gnus-binary-menu))

(defun gnus-xmas-agent-summary-menu-add ()
  (gnus-xmas-menu-add agent-summary
    gnus-agent-summary-menu))

(defun gnus-xmas-agent-group-menu-add ()
  (gnus-xmas-menu-add agent-group
    gnus-agent-group-menu))

(defun gnus-xmas-agent-server-menu-add ()
  (gnus-xmas-menu-add agent-server
    gnus-agent-server-menu))

(defun gnus-xmas-tree-menu-add ()
  (gnus-xmas-menu-add tree
    gnus-tree-menu))

(defun gnus-xmas-draft-menu-add ()
  (gnus-xmas-menu-add draft
    gnus-draft-menu))

(defun gnus-xmas-server-menu-add ()
  (gnus-xmas-menu-add menu
    gnus-server-server-menu gnus-server-connections-menu))

(defun gnus-xmas-browse-menu-add ()
  (gnus-xmas-menu-add browse
    gnus-browse-menu))

(defun gnus-xmas-grouplens-menu-add ()
  (gnus-xmas-menu-add grouplens
    gnus-grouplens-menu))

(defun gnus-xmas-read-event-char ()
  "Get the next event."
  (let ((event (next-command-event)))
    (sit-for 0)
    ;; We junk all non-key events.  Is this naughty?
    (while (not (or (key-press-event-p event)
		    (button-press-event-p event)))
      (dispatch-event event)
      (setq event (next-command-event)))
    (cons (and (key-press-event-p event)
	       (event-to-character event))
	  event)))

(defun gnus-xmas-define ()
  (setq gnus-mouse-2 [button2])
  (setq gnus-mouse-3 [button3])
  (setq gnus-widget-button-keymap widget-button-keymap)

  (unless (memq 'underline (face-list))
    (and (fboundp 'make-face)
	 (funcall (intern "make-face") 'underline)))
  ;; Must avoid calling set-face-underline-p directly, because it
  ;; is a defsubst in emacs19, and will make the .elc files non
  ;; portable!
  (unless (face-differs-from-default-p 'underline)
    (funcall (intern "set-face-underline-p") 'underline t))

  (cond
   ((fboundp 'char-or-char-int-p)
    ;; Handle both types of marks for XEmacs-20.x.
    (fset 'gnus-characterp 'char-or-char-int-p))
   ;; V19 of XEmacs, probably.
   (t
    (fset 'gnus-characterp 'characterp)))

  (fset 'gnus-make-overlay 'make-extent)
  (fset 'gnus-delete-overlay 'delete-extent)
  (fset 'gnus-overlay-put 'set-extent-property)
  (fset 'gnus-move-overlay 'gnus-xmas-move-overlay)
  (fset 'gnus-overlay-end 'extent-end-position)
  (fset 'gnus-kill-all-overlays 'gnus-xmas-kill-all-overlays)
  (fset 'gnus-extent-detached-p 'extent-detached-p)
  (fset 'gnus-add-text-properties 'gnus-xmas-add-text-properties)
  (fset 'gnus-put-text-property 'gnus-xmas-put-text-property)
  (fset 'gnus-deactivate-mark 'ignore)
  (fset 'gnus-window-edges 'window-pixel-edges)

  (if (and (<= emacs-major-version 19)
 	   (< emacs-minor-version 14))
      (fset 'gnus-set-text-properties 'gnus-xmas-set-text-properties))

  (when (fboundp 'turn-off-scroll-in-place)
    (add-hook 'gnus-article-mode-hook 'turn-off-scroll-in-place))

  (unless (boundp 'standard-display-table)
    (setq standard-display-table nil))

  (defvar gnus-mouse-face-prop 'highlight)

  (defun gnus-byte-code (func)
    "Return a form that can be `eval'ed based on FUNC."
    (let ((fval (indirect-function func)))
      (if (compiled-function-p fval)
	  (list 'funcall fval)
	(cons 'progn (cdr (cdr fval))))))

  (fset 'gnus-x-color-values
	(if (fboundp 'x-color-values)
	    'x-color-values
	  (lambda (color)
	    (color-instance-rgb-components
	     (make-color-instance color))))))

(defun gnus-xmas-redefine ()
  "Redefine lots of Gnus functions for XEmacs."
  (fset 'gnus-summary-set-display-table 'gnus-xmas-summary-set-display-table)
  (fset 'gnus-visual-turn-off-edit-menu 'identity)
  (fset 'gnus-summary-recenter 'gnus-xmas-summary-recenter)
  (fset 'gnus-extent-start-open 'gnus-xmas-extent-start-open)
  (fset 'gnus-article-push-button 'gnus-xmas-article-push-button)
  (fset 'gnus-window-top-edge 'gnus-xmas-window-top-edge)
  (fset 'gnus-read-event-char 'gnus-xmas-read-event-char)
  (fset 'gnus-group-startup-message 'gnus-xmas-group-startup-message)
  (fset 'gnus-tree-minimize 'gnus-xmas-tree-minimize)
  (fset 'gnus-appt-select-lowest-window
	'gnus-xmas-appt-select-lowest-window)
  (fset 'gnus-mail-strip-quoted-names 'gnus-xmas-mail-strip-quoted-names)
  (fset 'gnus-character-to-event 'character-to-event)
  (fset 'gnus-mode-line-buffer-identification
	'gnus-xmas-mode-line-buffer-identification)
  (fset 'gnus-key-press-event-p 'key-press-event-p)
  (fset 'gnus-region-active-p 'region-active-p)
  (fset 'gnus-annotation-in-region-p 'gnus-xmas-annotation-in-region-p)
  (fset 'gnus-mime-button-menu 'gnus-xmas-mime-button-menu)

  (add-hook 'gnus-group-mode-hook 'gnus-xmas-group-menu-add)
  (add-hook 'gnus-summary-mode-hook 'gnus-xmas-summary-menu-add)
  (add-hook 'gnus-article-mode-hook 'gnus-xmas-article-menu-add)
  (add-hook 'gnus-score-mode-hook 'gnus-xmas-score-menu-add)

  (add-hook 'gnus-pick-mode-hook 'gnus-xmas-pick-menu-add)
  (add-hook 'gnus-topic-mode-hook 'gnus-xmas-topic-menu-add)
  (add-hook 'gnus-tree-mode-hook 'gnus-xmas-tree-menu-add)
  (add-hook 'gnus-binary-mode-hook 'gnus-xmas-binary-menu-add)
  (add-hook 'gnus-grouplens-mode-hook 'gnus-xmas-grouplens-menu-add)
  (add-hook 'gnus-server-mode-hook 'gnus-xmas-server-menu-add)
  (add-hook 'gnus-browse-mode-hook 'gnus-xmas-browse-menu-add)

  (add-hook 'gnus-group-mode-hook 'gnus-xmas-setup-group-toolbar)
  (add-hook 'gnus-summary-mode-hook 'gnus-xmas-setup-summary-toolbar)

  (add-hook 'gnus-agent-summary-mode-hook 'gnus-xmas-agent-summary-menu-add)
  (add-hook 'gnus-agent-group-mode-hook 'gnus-xmas-agent-group-menu-add)
  (add-hook 'gnus-agent-server-mode-hook 'gnus-xmas-agent-server-menu-add)

  (add-hook 'gnus-draft-mode-hook 'gnus-xmas-draft-menu-add)
  (add-hook 'gnus-summary-mode-hook
	    'gnus-xmas-switch-horizontal-scrollbar-off)
  (add-hook 'gnus-tree-mode-hook 'gnus-xmas-switch-horizontal-scrollbar-off))


;;; XEmacs logo and toolbar.

(defun gnus-xmas-group-startup-message (&optional x y)
  "Insert startup message in current buffer."
  ;; Insert the message.
  (setq gnus-xmas-glyph-directory (message-xmas-find-glyph-directory "gnus"))
  (erase-buffer)
  (cond
   ((and (console-on-window-system-p)
	 (or (featurep 'xpm)
	     (featurep 'xbm)))
    (let* ((logo-xpm (expand-file-name "gnus.xpm" gnus-xmas-glyph-directory))
	   (logo-xbm (expand-file-name "gnus.xbm" gnus-xmas-glyph-directory))
	   (glyph (make-glyph
		   (cond ((featurep 'xpm)
			  `[xpm
			    :file ,logo-xpm
			    :color-symbols
			    (("thing" . ,(car gnus-xmas-logo-colors))
			     ("shadow" . ,(cadr gnus-xmas-logo-colors))
			     ("background" . ,(face-background 'default)))])
			 ((featurep 'xbm)
			  `[xbm :file ,logo-xbm])
			 (t [nothing])))))
      (insert " ")
      (set-extent-begin-glyph (make-extent (point) (point)) glyph)
      (goto-char (point-min))
      (while (not (eobp))
	(insert (make-string (/ (max (- (window-width) (or x 35)) 0) 2)
			     ?\ ))
	(forward-line 1))
      (setq gnus-simple-splash nil))
    (goto-char (point-min))
    (let* ((pheight (+ 20 (count-lines (point-min) (point-max))))
	   (wheight (window-height))
	   (rest (- wheight pheight)))
      (insert (make-string (max 0 (* 2 (/ rest 3))) ?\n))))
   (t
    (insert
     (format "              %s
          _    ___ _             _
          _ ___ __ ___  __    _ ___
          __   _     ___    __  ___
              _           ___     _
             _  _ __             _
             ___   __            _
                   __           _
                    _      _   _
                   _      _    _
                      _  _    _
                  __  ___
                 _   _ _     _
                _   _
              _    _
             _    _
            _
          __

"
	     ""))
    ;; And then hack it.
    (gnus-indent-rigidly (point-min) (point-max)
			 (/ (max (- (window-width) (or x 46)) 0) 2))
    (goto-char (point-min))
    (forward-line 1)
    (let* ((pheight (count-lines (point-min) (point-max)))
	   (wheight (window-height))
	   (rest (- wheight pheight)))
      (insert (make-string (max 0 (* 2 (/ rest 3))) ?\n)))
    ;; Paint it.
    (put-text-property (point-min) (point-max) 'face 'gnus-splash-face)))
  (setq modeline-buffer-identification
	(list (concat gnus-version ": *Group*")))
  (set-buffer-modified-p t))


;;; The toolbar.

(defcustom gnus-use-toolbar (if (featurep 'toolbar)
				'default-toolbar
			      nil)
  "*If nil, do not use a toolbar.
If it is non-nil, it must be a toolbar.  The five valid values are
`default-toolbar', `top-toolbar', `bottom-toolbar',
`right-toolbar', and `left-toolbar'."
  :type '(choice (const default-toolbar)
		 (const top-toolbar) (const bottom-toolbar)
		 (const left-toolbar) (const right-toolbar)
		 (const :tag "no toolbar" nil))
  :group 'gnus-xmas)

(defvar gnus-group-toolbar
  '([gnus-group-get-new-news gnus-group-get-new-news t "Get new news"]
    [gnus-group-get-new-news-this-group
     gnus-group-get-new-news-this-group t "Get new news in this group"]
    [gnus-group-catchup-current
     gnus-group-catchup-current t "Catchup group"]
    [gnus-group-describe-group
     gnus-group-describe-group t "Describe group"]
    [gnus-group-unsubscribe gnus-group-unsubscribe t "Unsubscribe group"]
    [gnus-group-subscribe gnus-group-subscribe t "Subscribe group"]
    [gnus-group-kill-group gnus-group-kill-group t "Kill group"]
    [gnus-group-exit gnus-group-exit t "Exit Gnus"])
  "The group buffer toolbar.")

(defvar gnus-summary-toolbar
  '([gnus-summary-prev-unread
     gnus-summary-prev-page-or-article t "Page up"]
    [gnus-summary-next-unread
     gnus-summary-next-page t "Page down"]
    [gnus-summary-post-news
     gnus-summary-post-news t "Post an article"]
    [gnus-summary-followup-with-original
     gnus-summary-followup-with-original t
     "Post a followup and yank the original"]
    [gnus-summary-followup
     gnus-summary-followup t "Post a followup"]
    [gnus-summary-reply-with-original
     gnus-summary-reply-with-original t "Mail a reply and yank the original"]
    [gnus-summary-reply
     gnus-summary-reply t "Mail a reply"]
    [gnus-summary-caesar-message
     gnus-summary-caesar-message t "Rot 13"]
    [gnus-uu-decode-uu
     gnus-uu-decode-uu t "Decode uuencoded articles"]
    [gnus-summary-save-article-file
     gnus-summary-save-article-file t "Save article in file"]
    [gnus-summary-save-article
     gnus-summary-save-article t "Save article"]
    [gnus-uu-post-news
     gnus-uu-post-news t "Post a uuencoded article"]
    [gnus-summary-cancel-article
     gnus-summary-cancel-article t "Cancel article"]
    [gnus-summary-catchup
     gnus-summary-catchup t "Catchup"]
    [gnus-summary-catchup-and-exit
     gnus-summary-catchup-and-exit t "Catchup and exit"]
    [gnus-summary-exit gnus-summary-exit t "Exit this summary"])
  "The summary buffer toolbar.")

(defvar gnus-summary-mail-toolbar
  '(
    [gnus-summary-prev-unread
     gnus-summary-prev-unread-article t "Prev unread article"]
    [gnus-summary-next-unread
     gnus-summary-next-unread-article t "Next unread article"]
    [gnus-summary-mail-reply gnus-summary-reply t "Reply"]
    [gnus-summary-mail-originate gnus-summary-post-news t "Originate"]
    [gnus-summary-mail-save gnus-summary-save-article t "Save"]
    [gnus-summary-mail-copy gnus-summary-copy-article t "Copy message"]
    [gnus-summary-mail-forward gnus-summary-mail-forward t "Forward message"]
    [gnus-summary-caesar-message
     gnus-summary-caesar-message t "Rot 13"]
    [gnus-uu-decode-uu
     gnus-uu-decode-uu t "Decode uuencoded articles"]
    [gnus-summary-save-article-file
     gnus-summary-save-article-file t "Save article in file"]
    [gnus-summary-save-article
     gnus-summary-save-article t "Save article"]
    [gnus-summary-catchup
     gnus-summary-catchup t "Catchup"]
    [gnus-summary-catchup-and-exit
     gnus-summary-catchup-and-exit t "Catchup and exit"]
    [gnus-summary-exit gnus-summary-exit t "Exit this summary"])
  "The summary buffer mail toolbar.")

(defun gnus-xmas-setup-group-toolbar ()
  (and gnus-use-toolbar
       (message-xmas-setup-toolbar gnus-group-toolbar nil "gnus")
       (set-specifier (symbol-value gnus-use-toolbar)
		      (cons (current-buffer) gnus-group-toolbar))))

(defun gnus-xmas-setup-summary-toolbar ()
  (let ((bar (if (gnus-news-group-p gnus-newsgroup-name)
		 gnus-summary-toolbar gnus-summary-mail-toolbar)))
    (and gnus-use-toolbar
	 (message-xmas-setup-toolbar bar nil "gnus")
	 (set-specifier (symbol-value gnus-use-toolbar)
			(cons (current-buffer) bar)))))

(defun gnus-xmas-mail-strip-quoted-names (address)
  "Protect mail-strip-quoted-names from NIL input.
XEmacs compatibility workaround."
  (if (null address)
      nil
    (mail-strip-quoted-names address)))

(defun gnus-xmas-call-region (command &rest args)
  (apply
   'call-process-region (point-min) (point-max) command t '(t nil) nil
   args))

(defface gnus-x-face '((t (:foreground "black" :background "white")))
  "Face to show X face"
  :group 'gnus-xmas)

(defun gnus-xmas-article-display-xface (beg end)
  "Display any XFace headers in the current article."
  (save-excursion
    (let ((xface-glyph
	   (cond
	    ((featurep 'xface)
	     (make-glyph (vector 'xface :data
				 (concat "X-Face: "
					 (buffer-substring beg end)))))
	    ((featurep 'xpm)
	     (let ((cur (current-buffer)))
	       (save-excursion
		 (gnus-set-work-buffer)
		 (insert (format "%s" (buffer-substring beg end cur)))
		 (gnus-xmas-call-region "uncompface")
		 (goto-char (point-min))
		 (insert "/* Width=48, Height=48 */\n")
		 (gnus-xmas-call-region "icontopbm")
		 (gnus-xmas-call-region "ppmtoxpm")
		 (make-glyph
		  (vector 'xpm :data (buffer-string))))))
	    (t
	     (make-glyph [nothing]))))
	  (ext (make-extent (progn
			      (goto-char (point-min))
			      (re-search-forward "^From:" nil t)
			      (point))
			    (1+ (point)))))
      (set-glyph-face xface-glyph 'gnus-x-face)
      (set-extent-begin-glyph ext xface-glyph)
      (set-extent-property ext 'duplicable t))))

(defvar gnus-xmas-modeline-left-extent
  (let ((ext (copy-extent modeline-buffer-id-left-extent)))
    ext))

(defvar gnus-xmas-modeline-right-extent
  (let ((ext (copy-extent modeline-buffer-id-right-extent)))
    ext))

(defvar gnus-xmas-modeline-glyph
  (progn
    (setq gnus-xmas-glyph-directory (message-xmas-find-glyph-directory "gnus"))
    (let* ((file-xpm (expand-file-name "gnus-pointer.xpm"
				       gnus-xmas-glyph-directory))
	   (file-xbm (expand-file-name "gnus-pointer.xbm"
				       gnus-xmas-glyph-directory))
	   (glyph (make-glyph
		   ;; Gag gag gag.
		   (cond ((featurep 'xpm)
			  ;; Let's try a nifty XPM
			  `[xpm :file ,file-xpm])
			 ((featurep 'xbm)
			  ;; Then a not-so-nifty XBM
			  `[xbm :file ,file-xbm])
			 ;; Then the simple string
			 (t [string :data "Gnus:"])))))
      (set-glyph-face glyph 'modeline-buffer-id)
      glyph)))

(defun gnus-xmas-mode-line-buffer-identification (line)
  (let ((line (car line))
	chop)
    (cond
     ;; This is some weird type of id.
     ((not (stringp line))
      (list line))
     ;; This is non-standard, so we just pass it through.
     ((not (string-match "^Gnus:" line))
      (list line))
     ;; We have a standard line, so we colorize and glyphize it a bit.
     (t
      (setq chop (match-end 0))
      (list
       (if gnus-xmas-modeline-glyph
	   (cons gnus-xmas-modeline-left-extent gnus-xmas-modeline-glyph)
	 (cons gnus-xmas-modeline-left-extent (substring line 0 chop)))
       (cons gnus-xmas-modeline-right-extent (substring line chop)))))))

(defun gnus-xmas-splash ()
  (when (eq (device-type) 'x)
    (gnus-splash)))

(defun gnus-xmas-annotation-in-region-p (b e)
  (or (map-extents (lambda (e u) t) nil b e nil nil 'mm t)
      (if (= b e)
	  (eq (cadr (memq 'gnus-undeletable (text-properties-at b))) t)
	(text-property-any b e 'gnus-undeletable t))))

(defun gnus-xmas-mime-button-menu (event)
  "Construct a context-sensitive menu of MIME commands."
  (interactive "e")
  (let ((response (get-popup-menu-response
		   `("MIME Part"
		     ,@(mapcar (lambda (c) `[,(caddr c) ,(car c) t])
			       gnus-mime-button-commands)))))
    (set-buffer (event-buffer event))
    (goto-char (event-point event))
    (funcall (event-function response) (event-object response))))

(defun gnus-group-add-icon ()
  "Add an icon to the current line according to `gnus-group-icon-list'."
  (let* ((p (point))
	 (end (progn (end-of-line) (point)))
	 ;; now find out where the line starts and leave point there.
	 (beg (progn (beginning-of-line) (point))))
    (save-restriction
      (narrow-to-region beg end)
      (goto-char beg)
      (when (search-forward "==&&==" nil t)
	(let* ((group (gnus-group-group-name))
	       (entry (gnus-group-entry group))
	       (unread (if (numberp (car entry)) (car entry) 0))
	       (active (gnus-active group))
	       (total (if active (1+ (- (cdr active) (car active))) 0))
	       (info (nth 2 entry))
	       (method (gnus-server-get-method group (gnus-info-method info)))
	       (marked (gnus-info-marks info))
	       (mailp (memq 'mail (assoc (symbol-name
					  (car (or method gnus-select-method)))
					 gnus-valid-select-methods)))
	       (level (or (gnus-info-level info) gnus-level-killed))
	       (score (or (gnus-info-score info) 0))
	       (ticked (gnus-range-length (cdr (assq 'tick marked))))
	       (group-age (gnus-group-timestamp-delta group))
	       (inhibit-read-only t)
	       (list gnus-group-icon-list)
	       (mystart (match-beginning 0))
	       (myend (match-end 0)))
	  (goto-char (point-min))
	  (while (and list
		      (not (eval (caar list))))
	    (setq list (cdr list)))
	  (if list
	      (let* ((file (cdar list))
		     (glyph (gnus-group-icon-create-glyph
			     (buffer-substring mystart myend)
			     file)))
		(if glyph
		    (progn
		      (mapcar 'delete-annotation (annotations-at myend))
		      (let ((ext (make-extent mystart myend))
			    (ant (make-annotation glyph myend 'text)))
			;; set text extent params
			(set-extent-property ext 'end-open t)
			(set-extent-property ext 'start-open t)
			(set-extent-property ext 'invisible t)))
		  (delete-region mystart myend)))
	    (delete-region mystart myend))))
      (widen))
    (goto-char p)))

(defun gnus-group-icon-create-glyph (substring pixmap)
  "Create a glyph for insertion into a group line."
  (and
   gnus-group-running-xemacs
   (or
    (cdr-safe (assoc pixmap gnus-group-icon-cache))
    (let* ((glyph (make-glyph
		   (list
		    (cons 'x
			  (expand-file-name pixmap gnus-xmas-glyph-directory))
		    (cons 'mswindows
			  (expand-file-name pixmap gnus-xmas-glyph-directory))
		    (cons 'tty substring)))))
      (setq gnus-group-icon-cache
	    (cons (cons pixmap glyph) gnus-group-icon-cache))
      (set-glyph-face glyph 'default)
      glyph))))

(provide 'gnus-xmas)

;;; gnus-xmas.el ends here
