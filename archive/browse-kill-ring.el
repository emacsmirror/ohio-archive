;;; browse-kill-ring.el --- interactively insert items from kill-ring

;; Copyright (C) 2001 Colin Walters

;; Emacs Lisp Archive Entry
;; Author: Colin Walters <walters@cis.ohio-state.edu>
;; Created: 7 Apr 2001
;; Version: 0.6
;; X-RCS: $Id: browse-kill-ring.el,v 1.33 2001/04/14 21:23:28 walters Exp $
;; URL: http://www.cis.ohio-state.edu/~walters
;; Keywords: convenience

;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Ever feel that 'C-y M-y M-y M-y ...' is not a great way of trying
;; to find that piece of text you know you killed a while back?  Then
;; browse-kill-ring.el is for you.

;; This package is simple to install; add (require 'browse-kill-ring)
;; to your ~/.emacs file, after placing this file somewhere in your
;; `load-path'.  To use, type 'M-x browse-kill-ring'.  You can bind
;; `browse-kill-ring' to a key, like:

;; (global-set-key (kbd "C-c k") 'browse-kill-ring)

;; Note that the command keeps track of the last window displayed to
;; handle insertion of chosen text; this might have unexpected
;; consequences if you do 'M-x browse-kill-ring', then switch your
;; window configuration, and try to use the same *Kill Ring* buffer
;; again.

;;; Change Log:

;; Changes from 0.5 to 0.6:
;; * Fix bug in `browse-kill-ring-forward' which sometimes would cause
;;   a message "Wrong type argument: overlayp, nil" to appear.
;; * New function `browse-kill-ring-update'.
;; * New variable `browse-kill-ring-highlight-current-entry'.
;; * New variable `browse-kill-ring-display-duplicates'.
;; * New optional advice `browse-kill-ring-no-kill-new-duplicates',
;;   and associated variable `browse-kill-ring-no-duplicates'.  Code
;;   from Klaus Berndl <Klaus.Berndl@sdm.de>.
;; * Bind "?" to `describe-mode'.  Patch from Dave Pearson
;;   <dave@davep.org>.
;; * Fix typo in `browse-kill-ring-display-style' defcustom form.
;;   Thanks "Kahlil (Kal) HODGSON" <kahlil@discus.anu.edu.au>.

;; Changes from 0.4 to 0.5:

;; * New function `browse-kill-ring-delete', bound to "d" by default.
;; * New function `browse-kill-ring-undo', bound to "U" by default.
;; * New variable `browse-kill-ring-maximum-display-length'.
;; * New variable `browse-kill-ring-use-fontification'.
;; * New variable `browse-kill-ring-hook', called after the
;;   "*Kill Ring*" buffer is created.

;; Changes from 0.3 to 0.4:

;; * New functions `browse-kill-ring-forward' and
;;   `browse-kill-ring-previous', bound to "n" and "p" by default,
;;   respectively.
;; * Change the default `browse-kill-ring-display-style' to
;;   `separated'.
;; * Removed `browse-kill-ring-original-window-config'; Now
;;   `browse-kill-ring-restore' just buries the "*Kill Ring*" buffer
;;   and deletes its window, which is simpler and more intuitive.
;; * New variable `browse-kill-ring-separator-face'.

;;; Bugs:

;; * Sometimes, in Emacs 21, the cursor will jump to the end of an
;;   entry when moving backwards using `browse-kill-ring-previous'.
;;   This doesn't seem to occur in Emacs 20 or XEmacs.

;;; Code:

(eval-when-compile
  (require 'cl))

(when (or (featurep 'xemacs)
	  (string-match "XEmacs\\|Lucid" (emacs-version)))
  (require 'overlay))

(defun browse-kill-ring-depropertize-string (str)
  "Return a copy of STR with text properties removed."
  (let ((str (copy-sequence str)))
    (set-text-properties 0 (length str) nil str)
    str))

(cond ((fboundp 'propertize)
       (defalias 'browse-kill-ring-propertize 'propertize))
      ;; Maybe save some memory :)
      ((fboundp 'ibuffer-propertize)
       (defalias 'browse-kill-ring-propertize 'ibuffer-propertize))
      (t
       (defun browse-kill-ring-propertize (string &rest properties)
	 "Return a copy of STRING with text properties added.

 [Note: this docstring has been copied from the Emacs 21 version]

First argument is the string to copy.
Remaining arguments form a sequence of PROPERTY VALUE pairs for text
properties to add to the result."
	 (let ((str (copy-sequence string)))
	   (add-text-properties 0 (length str)
				properties
				str)
	   str))))

(defgroup browse-kill-ring nil
  "A package for browsing and inserting the items in `kill-ring'.")

(defcustom browse-kill-ring-display-style 'separated
  "How to display the kill ring items.
If `one-line', then replace newlines with \"\\n\" for display.
If `separated', then display `browse-kill-ring-separator' between
entries."
  :type '(choice (const :tag "One line" one-line)
		 (const :tag "Separated" separated))
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-separator "-------"
  "The string separating entries in the `separated' style.
See `browse-kill-ring-display-style'."
  :type 'string
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-highlight-current-entry nil
  "If non-nil, highlight the currently selected `kill-ring' entry."
  :type 'boolean
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-separator-face 'bold
  "The face in which to highlight the `browse-kill-ring-separator'."
  :type 'face
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-use-fontification
  (not
   (not (cond ((boundp 'global-font-lock-mode)
	       global-font-lock-mode)
	      ((boundp 'font-lock-mode)
	       font-lock-mode)
	      (t
	       nil))))
  "Whether or not to use extra fontification.
If non-nil, highlight `browse-kill-ring-separator' in
`browse-kill-ring-separator-face', and make newlines in the `one-line'
style bold."
  :type 'boolean
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-maximum-display-length nil
  "Whether or not to limit the length of displayed items.

If this variable is an integer, the display of `kill-ring' will be
limited to that many characters.
Setting this variable to nil means no limit."
  :type '(choice (const :tag "None" nil)
		 integer)
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-display-duplicates t
  "If non-nil, then display duplicate items in `kill-ring'."
  :type 'boolean
  :group 'browse-kill-ring)

(defadvice kill-new (around browse-kill-ring-no-kill-new-duplicates)
  "An advice for not adding duplicate elements to `kill-ring'.
Even after being \"activated\", this advice will only modify the
behavior of `kill-new' when `browse-kill-ring-no-duplicates'
is non-nil."
  (if browse-kill-ring-no-duplicates
      (setq kill-ring (delete (ad-get-arg 0) kill-ring)))
  ad-do-it)

(defcustom browse-kill-ring-no-duplicates nil
  "If non-nil, then the `b-k-r-no-kill-new-duplicates' advice will operate.
This means that duplicate entries won't be added to the `kill-ring'
when you call `kill-new'.

If you set this variable via customize, the advice will be activated
or deactivated automatically.  Otherwise, to enable the advice, add

 (ad-enable-advice 'kill-new 'around 'browse-kill-ring-no-kill-new-duplicates)

to your init file."
  :type 'boolean
  :set (lambda (symbol value)
         (set symbol value)
         (if value
             (ad-enable-advice 'kill-new 'around
			       'browse-kill-ring-no-kill-new-duplicates)
           (ad-disable-advice 'kill-new 'around
			      'browse-kill-ring-no-kill-new-duplicates))
         (ad-activate 'kill-new))
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-depropertize nil
  "If non-nil, remove text properties from `kill-ring' items.
This only changes the items for display and insertion from
`browse-kill-ring'; if you call `yank' directly, the items will be
inserted with properties."
  :type 'boolean
  :group 'browse-kill-ring)

(defcustom browse-kill-ring-hook nil
  "A list of functions to call after `browse-kill-ring'."
  :type 'hook
  :group 'browse-kill-ring)

(defvar browse-kill-ring-original-window nil
  "The window in which chosen kill ring data will be inserted.
It is probably not a good idea to set this variable directly; simply
call `browse-kill-ring' again.")
(make-variable-buffer-local 'browse-kill-ring-original-window)

(if (or (featurep 'xemacs)
	(string-match "XEmacs\\|Lucid" (emacs-version)))
    (defun browse-kill-ring-mouse-insert (e)
      "Insert the chosen text in the last selected buffer."
      (interactive "e")
      (browse-kill-ring-do-insert (event-buffer e)
				  (event-closest-point e)))
  (defun browse-kill-ring-mouse-insert (e)
    "Insert the chosen text in the last selected buffer."
    (interactive "e")
    (let* ((end (event-end e))
	   (win (posn-window end)))
      (browse-kill-ring-do-insert (window-buffer win)
				  (posn-point end)))))

(defun browse-kill-ring-undo-other-window ()
  "Undo the most recent change in the other window's buffer.
You most likely want to use this command for undoing an insertion of
yanked text from the *Kill Ring* buffer."
  (interactive)
  (with-current-buffer (window-buffer browse-kill-ring-original-window)
    (undo)))

(defun browse-kill-ring-insert ()
  "Insert the kill ring item at point into the last selected buffer."
  (interactive)
  (browse-kill-ring-do-insert (current-buffer)
			      (point)))

(defun browse-kill-ring-delete ()
  "Remove the item at point from the `kill-ring'."
  (interactive)
  (let ((over (car (overlays-at (point)))))
    (unless (overlayp over)
      (error "No kill ring item here"))
    (unwind-protect
	(progn
	  (setq buffer-read-only nil)
	  (let ((target (overlay-get over 'browse-kill-ring-target)))
	    (delete-region (overlay-start over)
			   (1+ (overlay-end over)))
	    (setq kill-ring (delete target kill-ring)))
	  (when (get-text-property (point) 'browse-kill-ring-extra)
	    (let ((prev (previous-single-property-change (point)
							 'browse-kill-ring-extra))
		  (next (next-single-property-change (point)
						     'browse-kill-ring-extra)))
	      ;; This is some voodoo.
	      (when prev
		(incf prev))
	      (when next
		(incf next))
	      (delete-region (or prev (point-min))
			     (or next (point-max))))))
      (setq buffer-read-only t)))
  (browse-kill-ring-forward 0))
  
(defun browse-kill-ring-do-insert (buf pt)
  (let ((str
	 (with-current-buffer buf
	   (save-excursion
	     (let ((overs (overlays-at pt)))
	       (or (and overs
			(overlay-get (car overs) 'browse-kill-ring-target))
		   (error "No kill ring item here")))))))
    (let ((orig (current-buffer)))
      (unwind-protect
	  (progn
	    (unless (window-live-p browse-kill-ring-original-window)
	      (error "Window %s has been deleted; Try calling `browse-kill-ring' again"
		     browse-kill-ring-original-window))
	    (set-buffer (window-buffer browse-kill-ring-original-window))
	    (save-excursion
	      (insert (if browse-kill-ring-depropertize
			  (browse-kill-ring-depropertize-string str)
			str))))
	(set-buffer orig)))))

(defun browse-kill-ring-forward (&optional arg)
  "Move forward by ARG `kill-ring' entries."
  (interactive "p")
  (beginning-of-line)
  (while (not (zerop arg))
    (if (< arg 0)
	(progn
	  (incf arg)
	  (if (overlays-at (point))
	      (progn
		(goto-char (overlay-start (car (overlays-at (point)))))
		(goto-char (previous-overlay-change (point)))
		(goto-char (previous-overlay-change (point))))
	    (progn
	      (goto-char (1- (previous-overlay-change (point))))
	      (unless (bobp)
		(goto-char (overlay-start (car (overlays-at (point)))))))))
      (progn
	(decf arg)
	(if (overlays-at (point))
	    (progn
	      (goto-char (overlay-end (car (overlays-at (point)))))
	      (goto-char (next-overlay-change (point))))
	  (goto-char (next-overlay-change (point)))
	  (unless (eobp)
	    (goto-char (overlay-start (car (overlays-at (point))))))))))
  ;; This could probably be implemented in a more intelligent manner.
  ;; Perhaps keep track over the overlay we started from?  That would
  ;; break when the user moved manually, though.
  (when (and browse-kill-ring-highlight-current-entry
	     (overlays-at (point)))
    (let ((overs (overlay-lists))
	  (current-overlay (car (overlays-at (point)))))
      (mapcar #'(lambda (o)
		  (overlay-put o 'face nil))
	      (nconc (car overs) (cdr overs)))
      (overlay-put current-overlay 'face 'highlight))))

(defun browse-kill-ring-previous (&optional arg)
  "Move backward by ARG `kill-ring' entries."
  (interactive "p")
  (browse-kill-ring-forward (- arg)))

(defun browse-kill-ring-restore ()
  "Delete the current window, and bury the *Kill Ring* buffer."
  (interactive)
  (bury-buffer)
  (unless (= (count-windows) 1)
    (delete-window)))

(define-derived-mode browse-kill-ring-mode fundamental-mode
  "Kill Ring"
  "A major mode for browsing the `kill-ring'.
You most likely do not want to call `browse-kill-ring-mode' directly; use
`browse-kill-ring' instead."
  (define-key browse-kill-ring-mode-map (kbd "q") 'browse-kill-ring-restore)
  (define-key browse-kill-ring-mode-map (kbd "U") 'browse-kill-ring-undo-other-window)
  (define-key browse-kill-ring-mode-map (kbd "d") 'browse-kill-ring-delete)
  (define-key browse-kill-ring-mode-map (kbd "g") 'browse-kill-ring-update)
  (define-key browse-kill-ring-mode-map (kbd "n") 'browse-kill-ring-forward)
  (define-key browse-kill-ring-mode-map (kbd "p") 'browse-kill-ring-previous)
  (define-key browse-kill-ring-mode-map [(mouse-2)] 'browse-kill-ring-mouse-insert)
  (define-key browse-kill-ring-mode-map (kbd "?") 'describe-mode)
  (define-key browse-kill-ring-mode-map (kbd "y") 'browse-kill-ring-insert)
  (define-key browse-kill-ring-mode-map (kbd "RET") 'browse-kill-ring-insert))

(defmacro browse-kill-ring-add-overlays-for (item &rest body)
  (let ((beg (gensym "browse-kill-ring-add-overlays-"))
	(end (gensym "browse-kill-ring-add-overlays-")))
    `(let ((,beg (point))
	   (,end
	    (progn
	      ,@body
	      (point))))
       (let ((o (make-overlay ,beg ,end)))
	 (overlay-put o 'browse-kill-ring-target ,item)
	 (overlay-put o 'mouse-face 'highlight)))))
;; (put 'browse-kill-ring-add-overlays-for 'lisp-indent-function 1)

(defun browse-kill-ring-elide (str)
  (if (and browse-kill-ring-maximum-display-length
	   (> (length str)
	      browse-kill-ring-maximum-display-length))
      (concat (substring str 0 (- browse-kill-ring-maximum-display-length 3))
	      (if browse-kill-ring-use-fontification
		  (browse-kill-ring-propertize "..." 'face 'bold)
		"..."))
    str))

(defun browse-kill-ring-insert-as-one-line (items)
  (dolist (item items)
    (browse-kill-ring-add-overlays-for item
      (let* ((item (browse-kill-ring-elide item))
	     (len (length item))
	     (start 0)
	     (newl
	      (if browse-kill-ring-use-fontification
		  (browse-kill-ring-propertize "\\n" 'face 'bold)
		"\\n")))
	(while (and (< start len)
		    (string-match "\n" item start))
	  (insert (substring item start (match-beginning 0))
		  newl)
	  (setq start (match-end 0)))
	(insert (substring item start len))))
    (insert "\n")))

(defun browse-kill-ring-insert-as-separated (items)
  (while (cdr items)
    (browse-kill-ring-insert-as-separated-1 (car items) t)
    (setq items (cdr items)))
  (when items
    (browse-kill-ring-insert-as-separated-1 (car items) nil)))

(defun browse-kill-ring-insert-as-separated-1 (origitem separatep)
  (let* ((item (browse-kill-ring-elide origitem))
	 (len (length item)))
    (browse-kill-ring-add-overlays-for origitem
	(insert item))
    (insert "\n")
    (when separatep
      (insert (apply #'browse-kill-ring-propertize browse-kill-ring-separator
		     'browse-kill-ring-extra t
		     (when browse-kill-ring-use-fontification
		       (list 'face browse-kill-ring-separator-face))))
      (insert "\n"))))

(defun browse-kill-ring-update ()
  "Update the buffer to reflect outside changes to `kill-ring'."
  (interactive)
  (browse-kill-ring-setup (current-buffer)
			  browse-kill-ring-original-window))

(defun browse-kill-ring-setup (buf window)
  (with-current-buffer buf
    (unwind-protect
	(progn
	  (browse-kill-ring-mode)
	  (setq buffer-read-only nil)
	  (when (eq browse-kill-ring-display-style
		    'one-line)
	    (setq truncate-lines t))
	  (erase-buffer)
	  (setq browse-kill-ring-original-window window
		browse-kill-ring-original-window-config (current-window-configuration))
	  (let ((browse-kill-ring-maximum-display-length
		 (if (and browse-kill-ring-maximum-display-length
			  (<= browse-kill-ring-maximum-display-length 3))
		     4
		   browse-kill-ring-maximum-display-length))
		(items (mapcar
			(if browse-kill-ring-depropertize
			    #'browse-kill-ring-depropertize-string
			  #'copy-sequence)
			kill-ring)))
	    (when (not browse-kill-ring-display-duplicates)
	      ;; I'm not going to rewrite `delete-duplicates'.  If
	      ;; someone really wants to rewrite it here, send me a
	      ;; patch.
	      (require 'cl)
	      (setq items (delete-duplicates items :test #'equal)))
	    (funcall (intern
		      (concat "browse-kill-ring-insert-as-"
			      (symbol-name browse-kill-ring-display-style)))
		     items)
	    (if browse-kill-ring-display-duplicates
		(message "%s entries in the kill ring" (length kill-ring))
	      (message "%s (of %s) entries in the kill ring shown"
		       (length items) (length kill-ring)))
	    (set-buffer-modified-p nil)
	    (goto-char (point-min))
	    (browse-kill-ring-forward 0)
	    (run-hooks 'browse-kill-ring-hook)))
      (progn
	(setq buffer-read-only t)))))

(defun browse-kill-ring ()
  "Display items in the `kill-ring' in another buffer."
  (interactive)
  (let ((orig-buf (current-buffer))
	(buf (get-buffer-create "*Kill Ring*")))
    (browse-kill-ring-setup buf (selected-window))
    (pop-to-buffer buf)
    nil))

(provide 'browse-kill-ring)

;;; browse-kill-ring.el ends here
