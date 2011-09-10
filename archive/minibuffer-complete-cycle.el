;;; -*-unibyte: t;-*-

;;;; minibuffer-complete-cycle.el -- Cycle through the *Completions* buffer.

;;; RCS $Id: minibuffer-complete-cycle.el,v 1.18 2000/11/21 21:36:53 kevinr Exp $

;;; Description:
;;; 
;;; The `minibuffer-complete' command, bound by default to TAB in the
;;; minibuffer completion keymaps, displays the list of possible
;;; completions when no additional characters can be completed.
;;; Subsequent invocations of this command cause the window displaying
;;; the *Completions* buffer to scroll, if necessary.
;;; 
;;; This package advises the `minibuffer-complete' command so that
;;; subsequent invocations instead insert each of the possible
;;; completions in turn into the minibuffer, and highlight it in the
;;; *Completions* buffer.  As before, the window displaying the possible
;;; completions is scrolled if necessary.  This enhancement is enabled
;;; or disabled by setting or unsetting the `minibuffer-complete-cycle'
;;; option.

;;; Acknowledgments:
;;; 
;;; The technique of deleting the minibuffer contents, then (for file
;;; name completion) inserting the directory component of the initial
;;; input, and then inserting the completion string itself is based on
;;; cycle-mini.el (1.03) by Joe Reiss <jreiss@vt.edu>.

;;; Copyright:
;;; 
;;; Copyright © 1997,1998,2000 Kevin Rodgers
;;; 
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; at your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;; 
;;; My employer (Information Handling Services) has not disclaimed any
;;; copyright interest in minibuffer-complete-cycle.el.
;;; 
;;; Kevin Rodgers <kevinr@ihs.com>          Lead Software Engineer
;;; Information Handling Services           Electronic Systems Development
;;; 15 Inverness Way East, M/S A201         GO BUFFS!
;;; Englewood CO 80112-5776 USA             1+ (303) 397-2807[voice]/-2244[fax]

;;; Emacs Lisp Archive Entry:
;;; Filename: minibuffer-complete-cycle.el
;;; Author: Kevin Rodgers <kevinr@ihs.com>
;;; Version: $Revision: 1.18 $
;;; Description: Cycle through the *Completions* buffer.
;;; Keywords: completion
;;; Last-Updated: $Date: 2000/11/21 21:36:53 $


;;; Package interface:
(provide 'minibuffer-complete-cycle)


;;; User options:
(defvar minibuffer-complete-cycle t
  "*If non-nil, `minibuffer-complete' cycles through the possible completions.
If `auto', `minibuffer-complete' inserts the first possible completion
immediately.")
(put 'minibuffer-complete-cycle 'variable-interactive
     "XEnable cycling through *Completions*? (t, 'auto, or nil): ")


;;; Internal variables:
(defvar mcc-completion-begin nil	; point in the *Completions* buffer
  "If non-nil, the beginning of the last selected completion.")
(defvar mcc-completion-end nil		; point in the *Completions* buffer
  "If non-nil, the end of the last selected completion.")

(defvar mcc-completion-property
  (cond ((string-match "XEmacs" emacs-version) 'list-mode-item)
	(t 'mouse-face))
  "The text property used to identify completions.")

(defvar mcc-overlay
  (let ((face (or (car (memq 'minibuffer-complete-cycle (face-list)))
		  (copy-face 'secondary-selection 'minibuffer-complete-cycle))))
    (cond ((and (fboundp 'make-extent) (fboundp 'set-extent-property)) ; XEmacs
	   (let ((extent (make-extent 1 1)))
	     (set-extent-property extent 'face 'minibuffer-complete-cycle)
	     extent))
	  ((and (fboundp 'make-overlay) (fboundp 'overlay-put))
	   (let ((overlay (make-overlay 1 1)))
	     (overlay-put overlay 'face 'minibuffer-complete-cycle)
	     overlay))))
  "If non-nil, the overlay used to highlight the *Completions* buffer.")


;;; Commands:
(defadvice minibuffer-complete (around cycle (&optional count) activate compile)
  "If the `minibuffer-complete-cycle' option is set, then instead of
just scrolling the window of possible completions, insert each one in
turn in the minibuffer and highlight it in the *Completions* buffer with
the `minibuffer-complete-cycle' face.  To cycle backward, type `M-TAB'."
;;;`\\<minibuffer-local-completion-map>\\[minibuffer-complete-backward]'
  (interactive "p")
  (if (and minibuffer-complete-cycle
	   ;; See Fminibuffer_complete:
	   (or (eq last-command this-command)
	       (and (eq minibuffer-complete-cycle 'auto)
		    ad-do-it))
	   minibuffer-scroll-window
	   (window-live-p minibuffer-scroll-window))
      ;; Delete the current completion, then insert and display the
      ;; next completion:
      (let ((incomplete-path
	     (if (eq minibuffer-completion-table 'read-file-name-internal)
		 (buffer-substring (if (fboundp 'minibuffer-prompt-end)	; Emacs 21
				       (minibuffer-prompt-end)
				     (point-min))
				   (point-max)))))
	(delete-region (if (fboundp 'minibuffer-prompt-end) ; Emacs 21
			   (minibuffer-prompt-end)
			 (point-min))
		       (point-max))
	(if incomplete-path
	    (insert (if (and mcc-completion-begin mcc-completion-end
			     (file-directory-p incomplete-path))
			(file-name-directory
			 (directory-file-name incomplete-path))
		      (file-name-directory incomplete-path))))
	(insert (mcc-completion-string count))
	(mcc-display-completion (< count 0)))
    ;; Reset the mcc variables and proceed normally:
    (progn
      (setq mcc-completion-begin nil
	    mcc-completion-end nil)
      ad-do-it)))

(defun minibuffer-complete-backward (&optional count)
  "Just like `minibuffer-complete', but cycle to the previous completion."
  (interactive "p")
  (setq this-command 'minibuffer-complete)
  (minibuffer-complete (- count)))


;;; Functions:
(defun mcc-define-backward-key ()	; mcc-minor-mode & -keymap
  "If the `minibuffer-complete-cycle' option is set, bind `M-TAB' to
`minibuffer-complete-backward', unless it is already defined in the
local keymap."
  (if (and minibuffer-complete-cycle
	   (null (local-key-binding "\M-\t")))
      (local-set-key "\M-\t" 'minibuffer-complete-backward)))

(add-hook 'minibuffer-setup-hook 'mcc-define-backward-key)

(defun mcc-completion-string (n)
  "Return the Nth next completion.
If N is negative, return the Nth previous completion."
  (let ((completion-buffer (window-buffer minibuffer-scroll-window)))
    ;; Verify the buffer and window configuration:
    (or (eq completion-buffer (get-buffer "*Completions*"))
	(error "minibuffer-scroll-window isn't displaying \
the *Completions* buffer"))
    (save-excursion
      (set-buffer completion-buffer)
      ;; Find the beginning and end of the completion:
      (if (< n 0)
	  (while (< n 0)
	    (setq mcc-completion-end
		  (or (and mcc-completion-begin
			   (previous-single-property-change mcc-completion-begin
							    mcc-completion-property))
		      (point-max)))
	    (setq mcc-completion-begin
		  (previous-single-property-change mcc-completion-end
						   mcc-completion-property
						   nil (point-min)))
	    (setq n (1+ n)))
	(while (> n 0)
	  (setq mcc-completion-begin
		(next-single-property-change (if (and mcc-completion-end
						      (< mcc-completion-end
							 (point-max)))
						 mcc-completion-end
					       (point-min))
					     mcc-completion-property))
	  (setq mcc-completion-end
		(next-single-property-change mcc-completion-begin
					     mcc-completion-property
					     nil (point-max)))
	  (setq n (1- n))))
      ;; Return the next completion (buffer-substring-no-properties?):
      (buffer-substring mcc-completion-begin mcc-completion-end))))

(defun mcc-display-completion (&optional backward)
  "Highlight the current completion and scroll the *Completions* buffer
forward or BACKWARD if necessary."
  (let ((completion-buffer (window-buffer minibuffer-scroll-window))
	(minibuffer-window (selected-window)))
    (if mcc-overlay
	(cond ((fboundp 'set-extent-endpoints) ; XEmacs
	       (set-extent-endpoints mcc-overlay mcc-completion-begin mcc-completion-end
				     completion-buffer))
	      ((fboundp 'move-overlay)
	       (move-overlay mcc-overlay mcc-completion-begin mcc-completion-end
			     completion-buffer))))
    (unwind-protect
	(progn
	  (select-window minibuffer-scroll-window) ; completion-buffer
	  (or (pos-visible-in-window-p mcc-completion-begin)
	      (if backward
		  (if (= (window-start) (point-min))
		      (set-window-point (selected-window) (point-max))
		    (scroll-down))
		(if (= (window-end) (point-max))
		    (set-window-point (selected-window) (point-min))
		  (scroll-up)))))
      (select-window minibuffer-window))))

;;;; minibuffer-complete-cycle.el ends here
