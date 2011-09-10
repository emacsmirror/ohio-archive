;;; dsg-frames.el --- set frame parameters based on major mode

;;; Author:		Dave Goldberg <dsg@mitre.org>
;;; Maintainer:		Dave Goldberg <dsg@mitre.org>
;;; Last Modified:	30-June-1994
;;; Version:		2.0
;;; Keywords:		frames,titles,parameters
;;;
;;; LCD Archive Entry:
;;; dsg-frames|Dave Goldberg|dsg@mitre.org|
;;; Set frame parameters based on major mode|
;;; 30-Jun-1994|2.0|~/misc/dsg-frames.el.Z|

;;; ========== Standard Disclaimer ==========
;;; This file is not part of the GNU Emacs distribution (yet).

;;; This file is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY.  No author or distributor accepts
;;; responsibility to anyone for the consequences of using it or for
;;; whether it serves any particular purpose or works at all, unless he
;;; says so in writing.  Refer to the GNU Emacs General Public License
;;; for full details. You should consider this code to be covered under
;;; the terms of the GPL.

;;; Everyone is granted permission to copy, modify and redistribute
;;; this file, but only under the conditions described in the GNU Emacs
;;; General Public License.  A copy of this license is supposed to have
;;; been given to you along with GNU Emacs so you can know your rights
;;; and responsibilities.  It should be in a file named COPYING.  Among
;;; other things, the copyright notice and this notice must be
;;; preserved on all copies.

;;; Commentary

;;; Since I started using FSF emacs v.19, I've been annoyed at having
;;; to modify frame parameters for some buffers by hand.  In
;;; particular, having to set the frame title based on file name or
;;; application name every time I entered a file or ran, say GNUS, got
;;; old quickly.  In epoch, there exists something called the
;;; epoch-mode-alist that allows screen (the epoch equivalent of a
;;; frame) properties to be set based on the mode of the buffer in the
;;; new screen.  I've ported the concept to emacs 19 (due to the
;;; different mechanisms, the code is significantly different).  It's
;;; more complicated than I'd like it to be, but so far it seems to
;;; work for me.  Any tips/improvements/bug reports are welcome.
;;;
;;; The package works by defining a frame-creation-function that looks
;;; at the major mode of the buffer in the frame being created, then
;;; checks two alists for matches on that mode.  The
;;; dsg-mode-pointer-shape alist allows different pointer shapes to be
;;; specified for a mode, and dsg-mode-frame-paramters allows other
;;; frame parameters to be specified for a mode.  I don't know why
;;; pointer shape is not part of frame parameters, but it is separate,
;;; so I had to work with it that way (I tried combining into one data
;;; structure but couldn't get it to work - any help from those better
;;; in LISP than I would be welcome).  I also added some advice to
;;; switch-to-buffer-other-frame to set up frame parameters before
;;; creating the frame.
;;;
;;; In a version 1.0, I had some additional functions for setting the
;;; title.  In version 2.0 I ripped all that out and use title.el from
;;; Terry Glanfield <terry@ppsl.demon.co.uk>, which is much cleaner
;;; and has better functionality.  However, in addition to setting
;;; frame-title-format in mode hooks, with dsg-frames, you can set up
;;; the title in dsg-mode-frame-parameters.  This allows the title to
;;; be set upon frame creation.  However, as noted above, setting up
;;; frame parameters upon creation isn't 100% consistent, and to set
;;; the pointer shape, the frame has to already exist.  Therefore, I
;;; also set up an after-make-frame-hook that ensures everything is
;;; set properly.  I realize there's some wasted cycles here, but I
;;; don't know what else to do about it at this point, other than
;;; making this alot more complicated than it already is.
;;;
;;; Finally, the function dsg-other-frame-extended-command is provided
;;; to allow just about any command to be run and have the results
;;; popped up in a new frame.  I typically use this for mh-rmail and
;;; gnus.

;;;; Customization

;;; Several variables can be customized; all are best customized in .emacs.
;;; The variable frame-title-format is from title.el.
;;;
;;; (setq-default frame-title-format "emacs:%u@%s(%b)")
;;; ;; so most of my windows have names like emacs:dsg@blackbird(buffername)
;;;
;;; (setq dsg-mode-frame-parameters
;;;      (list (cons 'mh-folder-mode
;;;		  (list (list (cons 'name (make-title "emacs:%u@%s(MH %b)"))
;;;			      (cons 'height 48))))
;;;	    (cons 'gnus-newsgroup-mode
;;;		  (list (list (cons 'permanent-name
;;;				    (make-title "emacs:%u@%s(Gnus)"))
;;;			      (cons 'height 48))))))
;;; ;; so mh-folder-mode frames have a slightly different name and
;;; ;; newsgroup frames have a permanent name (it doesn't change if
;;; ;; another buffer is selected in that frame).
;;; 
;;;(setq dsg-mode-pointer-shape-alist
;;;      (list (cons 'gnus-group-mode x-pointer-coffee-mug)
;;;	    (cons 'mh-folder-mode x-pointer-hand1)))
;;; ;; this causes MH and GNUS frames to have different pointer
;;; ;; shapes.  Note that if I select a different buffer in the MH
;;; ;; frame, the pointer shape doesn't change with it.  Only the
;;; ;; title is affected by buffer changes.
;;;
;;; In addition to using dsg-other-frame-extended-command, advice can
;;; be set up for functions that you always want to have in a separate
;;; frame.  I use the following for Info.
;;;
;;;(defadvice info (around do-it-other-frame activate)
;;;  "Run Info in a separate frame"
;;;  (ad-deactivate-regexp "set-title")
;;;  (let (otherbuf)
;;;    (save-window-excursion
;;;      ad-do-it
;;;      (setq otherbuf (current-buffer)))
;;;    (ad-activate-regexp "set-title")
;;;    (switch-to-buffer-other-frame otherbuf)))

;;;; Installation

;;; Put this file and title.el in a directory in your load path and
;;; compile them.  Then add the line (require 'dsg-frames) to your
;;; .emacs file.

;;;; Code

(require 'advice)
(require 'title)
(provide 'dsg-frames)

;;;; User customizable variables

(defvar dsg-mode-pointer-shape-alist nil
  "Alist of (major-mode . pointer-shape) pairs.
Causes dsg-frame-creation-function to set the pointer shape based
on the mode of the buffer in the new frame.")

(defvar dsg-mode-frame-parameters nil
  "Alist of (major-mode . (frame-parameters)) pairs.
Causes dsg-frame-creation-function to specify frame parameters based
on the mode of the buffer in the new frame.")

(defvar dsg-default-mouse-color "black"
  "The mouse color to use if none is set in dsg-mode-frame-parameters.")

(defvar dsg-default-pointer-shape x-pointer-xterm
  "The pointer shape to use if none is set in dsg-mode-pointer-shape-alist.")

;;;; Functions

(defun dsg-set-mouse-shape (form)
  "Sets the mouse shape to FORM.
The names for the shapes are defined in x-win.el."
  (interactive 
   (let (formato)
     (setq formato (completing-read "Shape: "
				    obarray 'boundp t "x-pointer-"))
     (list (intern formato))))
  (setq x-pointer-shape (symbol-value form))
  (set-mouse-color (cdr (assq  'mouse-color (frame-parameters)))))


(defun dsg-current-frame-parameters ()
  "Return a list of frame parameters for the current buffer.
The list will be based on the dsg-frames alist variables, the return
>from the (frame-title) function and default-frame-alist."
  (let* ((alist (car (cdr (assoc major-mode dsg-mode-frame-parameters))))
	 (ptitle (assq 'permanent-name alist)))
    (if (not ptitle)
	(setq alist (append (list (cons 'name (frame-title))) alist))
      (setq alist (append (list (cons 'name (cdr ptitle))) alist)))
    (append alist default-frame-alist)))

(defun dsg-modify-frame-parameters (&optional optlist)
  "Set the selected frame to specs based on the dsg-frames alists.
To allow other packages to specify parameters that should override
those returned by dsg-current-frame-parameters, if the optional
argument OPTLIST, a list of frame parameters, is non-nil, it is
prepended to them.  This is intended to be called by
after-make-frame-hook."
  (interactive)
  (let* ((alist (dsg-current-frame-parameters))
	 (x-pointer-shape
	  (or (cdr (assoc major-mode dsg-mode-pointer-shape-alist))
	      dsg-default-pointer-shape))
	 (pointer-color (car (cdr (assoc 'mouse-color alist)))))
    (modify-frame-parameters (selected-frame) (append optlist alist))
    (if pointer-color
	(set-mouse-color pointer-color)
      (set-mouse-color dsg-default-mouse-color))))

(add-hook 'after-make-frame-hook 'dsg-modify-frame-parameters)

(defun dsg-frame-creation-function (alist)
  "A frame creation function that defines parameters based on the dsg-frames lists.
This is simply a wrapper around x-create-frame-with-faces."
  (select-frame (x-create-frame-with-faces
		 (append alist (dsg-current-frame-parameters)))))

(setq frame-creation-function 'dsg-frame-creation-function)

;;; This bit of advice causes titles etc to be set upon frame creation.

(defadvice switch-to-buffer-other-frame (around set-parms-first activate)
  "Set up frame parameters according to dsg-frames before creating the frame"
  (save-excursion
    (set-buffer (ad-get-arg 0))
    (let ((pop-up-frame-alist (append
			     (list (cons 'name (frame-title)))
			     (dsg-current-frame-parameters))))
      ad-do-it)))

;;; The following wrapper allows me to run things like mh-rmail and
;;; gnus, automatically putting the buffer in a new frame.

(defun dsg-other-frame-extended-command (cmd)
  "Run \\[whatever] in another frame."
  (interactive "CM-x ")
  (if (not (commandp cmd))
      (error "%s is not a command!" (symbol-name cmd))
    (let (otherbuf)
      (ad-deactivate-regexp "set-title")
      (save-window-excursion
	(call-interactively cmd)
	(setq otherbuf (current-buffer)))
      (ad-activate-regexp "set-title")
      (switch-to-buffer-other-frame otherbuf))))

(define-key ctl-x-5-map "x" 'dsg-other-frame-extended-command)

;;; dsg-frames.el ends here
