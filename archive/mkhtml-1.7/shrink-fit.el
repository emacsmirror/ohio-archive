;;; shrink-fit.el --- Shrink-wrap frames to fit their selected window.
;; 
;; Author: 
;; Maintainer: 
;; Copyright (C) 2000, 2001, Drew Adams, all rights reserved.
;; Created: Thu Dec  7 09:32:12 2000
;; Version: $Id: shrink-fit.el,v 1.4 2001/01/03 01:14:57 dadams Exp $
;;   Last modified by: 
;;   Last modified on: Tue Jan  2 17:14:40 2001
;;   Update count: 76
;; Keywords: internal, extensions, local
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;;    Shrink-wrap frames to fit their selected window.
;;
;;
;;  Functions and user options (variables) are provided here to shrink
;;  a frame to fit its selected window, either automatically or upon
;;  request.  The command to shrink frames is `shrink-frame-to-fit'.
;;  The main user options are `create-frame-max-*[-percent]'.
;; 
;;  To take full advantage of the functionality provided here, you
;;  should load the companion file `shrink-fit-all.el'.  It will
;;  automatically load this file.
;;
;;  You may want to put this in your `~/.emacs' file, in order to
;;  provide for automatic frame resizing:
;;
;;  (require 'shrink-fit)   ; or 'shrink-fit-all, to always shrink-fit
;;  (if (not (featurep 'shrink-fit)) nil
;;    (add-hook 'after-make-frame-functions 'shrink-frame-to-fit)
;;    (add-hook 'after-make-frame-functions 'making-frame-done-msg)
;;    (add-hook 'before-make-frame-hook 'making-frame-msg))
;;
;;
;;  Main new functions defined here:
;;
;;    `create-frame-max-height', `create-frame-max-width',
;;    `making-frame-msg', `making-frame-done-msg',
;;    `shrink-frame-to-fit', `shrink-wrap-1-window-frames-on'.
;;
;;  Main new user options (variables) defined here:
;;
;;    `create-empty-frame-height', `create-empty-frame-width',
;;    `create-empty-special-display-frame-height',
;;    `create-empty-special-display-frame-width',
;;    `create-frame-max-height', `create-frame-max-height-percent',
;;    `create-frame-max-width', `create-frame-max-width-percent',
;;    `create-frame-min-height', `create-frame-min-width',
;;    `enable-shrink-frame-to-fit'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; RCS $Log: shrink-fit.el,v $
;; RCS Revision 1.4  2001/01/03 01:14:57  dadams
;; RCS *** empty log message ***
;; RCS
;; RCS Revision 1.3  2001/01/02 23:42:20  dadams
;; RCS Moved here from compile-.el: shrink-wrap-1-window-frames-on.
;; RCS
;; RCS Revision 1.2  2000/12/08 01:18:06  dadams
;; RCS Clarified doc strings: create-empty-frame-*, create-empty-special-*.
;; RCS
;; RCS Revision 1.1  2000/12/07 19:54:58  dadams
;; RCS Initial revision
;; RCS
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(require 'cl) ;; when
(eval-when-compile (require 'misc-fns)) ;; special-display-buffer-p
(eval-when-compile (require 'strings)) ;; minibuffer-empty-p, read-number


(provide 'shrink-fit)

;; Free variable here: NFRAME

;;;;;;;;;;;;;;;;;;;;;;;



;;; User options ---------------------------------------------------

;;;###autoload
(defvar create-empty-frame-width (or (cdr (assq 'width default-frame-alist))
                                     80)
  "*Width, in characters, for new empty frames,
if `shrink-frame-to-fit' is used in `after-make-frame-functions'.")

;;;###autoload
(defvar create-empty-frame-height (or (cdr (assq 'height default-frame-alist))
                                      35)
  "*Height, in lines, for new empty frames
if `shrink-frame-to-fit' is used in `after-make-frame-functions'.")

;;;###autoload
(defvar create-empty-special-display-frame-width 80
  "*Width, in characters, for new empty special-display frames
if `shrink-frame-to-fit' is used in `after-make-frame-functions'.
If this is nil, it is ignored.")

;;;###autoload
(defvar create-empty-special-display-frame-height 9
  "*Height, in lines, for new empty special-display frames
if `shrink-frame-to-fit' is used in `after-make-frame-functions'.
If this is nil, it is ignored.")

;;;###autoload
(defvar create-frame-min-width 20
  "*Minimum width, in characters, for new frames
if `shrink-frame-to-fit' is used as an `after-make-frame-functions'.
The actual minimum is at least the greater of this and
`window-min-width'.")

;;;###autoload
(defvar create-frame-max-width nil
  "*Maximum width, in characters, for new frames
when `shrink-frame-to-fit' is used in `after-make-frame-functions'.

If nil, then the function `create-frame-max-width' is used instead.")

;;;###autoload
(defvar create-frame-max-width-percent 94
  "*Max percent of the total display width to give to a new frame
when `shrink-frame-to-fit' is used in `after-make-frame-functions'.
See function `create-frame-max-width'.")

;;;###autoload
(defvar create-frame-min-height window-min-height
  "*Minimum height, in lines, for new frames
if `shrink-frame-to-fit' is used in `after-make-frame-functions'.
The actual minimum is at least the greater of this and
`window-min-height'.")

;;;###autoload
(defvar create-frame-max-height nil
  "*Maximum height, in lines, for new frames
when `shrink-frame-to-fit' is used in `after-make-frame-functions'.

If nil, then the function `create-frame-max-height' is used instead.")

;;;###autoload
(defvar create-frame-max-height-percent 82
  "*Max percent of the total display height to give to a new frame
when `shrink-frame-to-fit' is used in `after-make-frame-functions'.
See function `create-frame-max-height'.")

;;;###autoload
(defvar enable-shrink-frame-to-fit t
  "*Command `shrink-frame-to-fit' does nothing iff this is nil.")


;;; Non-interactive Functions -------------------------------------------

;;;###autoload
(defun shrink-wrap-1-window-frames-on (buf &optional ignored)
  "Shrink buffer BUF's one-window frame(s) to fit it.
Usable, e.g., as a member of `compilation-finish-functions'."
  ;; Optional arg IGNORED is ignored.
  ;; It is for compatibility with `compilation-finish-functions'.
  (when (fboundp 'shrink-frame-to-fit)
    (let ((frs (1-window-frames-on buf)))
      (dolist (fr frs)
        (shrink-frame-to-fit fr)))))

;;;###autoload
(defun create-frame-max-width ()
  "Maximum width, in characters, for new frames
when `shrink-frame-to-fit' is used in `after-make-frame-functions',
and `create-frame-max-width' is nil.

The value is relative to your display size and the frame's character
size, and depends on the value of `create-frame-max-width-percent':

  (/ (* create-frame-max-width-percent (x-display-pixel-width))
     (* 100 (frame-char-width)))"
  (/ (* create-frame-max-width-percent (x-display-pixel-width))
     (* 100 (frame-char-width))))

;;;###autoload
(defun create-frame-max-height ()
  "Maximum height, in characters, for new frames
when `shrink-frame-to-fit' is used in `after-make-frame-functions',
and `create-frame-max-height' is nil.

The value is relative to your display size and the frame's character
size, and depends on the value of `create-frame-max-height-percent':

  (/ (* create-frame-max-height-percent (x-display-pixel-height))
     (* 100 (frame-char-height)))"
  (/ (* create-frame-max-height-percent (x-display-pixel-height))
     (* 100 (frame-char-height))))

;;;###autoload
(defun making-frame-done-msg (&optional frame)
  "Display \"Making frame ... done.\" msg.  Use in `after-make-frame-functions'."
  (and minibuffer-empty-p               ; Defined in `strings.el'.
       (message "Making frame ... done.")))

;;;###autoload
(defun making-frame-msg (&optional frame)
  "Display \"Making frame ...\" msg.  Intended as `before-make-frame-hook'."
  (and minibuffer-empty-p               ; Defined in `strings.el'.
       (message "Making frame ...")))


;;; Commands ---------------------------------------------------

;;;###autoload
(defun shrink-frame-to-fit (&optional frame width height)
  "Shrink FRAME to fit its selected window.
Usable in `after-make-frame-functions'.

This does nothing if `enable-shrink-frame-to-fit' is nil.

FRAME defaults to the current (i.e. selected) frame.  When FRAME arg
is supplied, the FRAME is shrunk to fit the window determined by
`select-frame'.

Interactively, supplying a non-negative prefix arg means you will be
prompted for the new frame width and height.  A negative prefix arg
means to use (1 more than) the current value of `fill-column' for the
new frame width, and the frame height is not changed.

Otherwise, the new frame width and height will be as follows.

With no (or null) args WIDTH & HEIGHT:

  If the frame is empty (i.e. has only one window, with an empty
  buffer), then:

    If the frame's buffer is a special display buffer, then:
      The new width is `create-empty-special-display-frame-width'.
      The new height is `create-empty-special-display-frame-height'.

    Otherwise:
      The new width is `create-empty-frame-width'.
      The new height is `create-empty-frame-height'.

  If the frame is not empty, then:

    The new frame width is the maximum of:
      1) `create-frame-min-width',
      2) `window-min-width', and
      3) the minimum of: `create-frame-max-width' variable or, if nil,
                         `create-frame-max-width' function,
         and the widest line currently in the `selected-window'.

    The new frame height is the maximum of:
      1) `create-frame-min-height',
      2) `window-min-height', and
      3) the minimum of: `create-frame-max-height' variable or, if nil,
                         `create-frame-max-height' function,
         and the number of lines currently in the `selected-window'.

    Note that there are two intended uses of `create-frame-min-*':
      1) Use the variable, if you want to specify an absolute size, in
         characters.
      2) Use the function (variable = nil), if you want to specify a
         relative size, in percent of display size.  Frames will then
         appear the same relative size on different displays.

When used in `after-make-frame-functions', the current `frame-width' and
`frame-height' are those of the newly created frame.

If optional args WIDTH and HEIGHT are `natnump's:

    They are the values to use for the new frame size.  (In this case,
    \"shrinking\" could really be \"expanding\", depending on the
    argument values.)"
  (interactive "P")
  (when (and (interactive-p) frame)     ; Non-nil FRAME => prefix arg given.
    (setq frame nil)                    ; Not the real FRAME. Need to redefine.
    (cond ((natnump (prefix-numeric-value current-prefix-arg))
           (setq width (read-number "New width: "))
           (setq height (read-number "New height: ")))
          (t (setq width (1+ fill-column)) (setq height (frame-height)))))
  (when enable-shrink-frame-to-fit
    (let ((fr (or frame (if (or (interactive-p) (not (boundp 'nframe)))
                            (selected-frame)
                          ;; NFRAME is free here. It is bound by `make-frame' to
                          ;; current frame, so it makes sense only when
                          ;; `shrink-frame-to-fit' is in `after-make-frame-functions'.
                          ;; `make-frame' is defined in `frame.el'.
                          nframe)))
          (max-width 0)
          (nb-lines 2)                  ; 1 for empty, 1 extra.
          empty-buf-p specbuf-p)
      (set-frame-size
       fr
       (or width
           (save-window-excursion
             (select-frame fr)
             (and (setq empty-buf-p (= (point-min) (point-max)))
                  (one-window-p (selected-window))
                  (if (setq specbuf-p (special-display-buffer-p
                                       (window-buffer)))
                      create-empty-special-display-frame-width
                    create-empty-frame-width)))
           (max create-frame-min-width window-min-width
                (min (or create-frame-max-width (create-frame-max-width))
                     (save-window-excursion
                       (select-frame fr)
                       (save-excursion
                         (goto-char (point-min))
                         (while (not (eobp))
                           (end-of-line)
                           (setq max-width (max (current-column) max-width))
                           (forward-line 1)
                           (incf nb-lines)))
                       (incf max-width)))))
       (or height
           (and empty-buf-p (if specbuf-p
                                create-empty-special-display-frame-height
                              create-empty-frame-height))
           (max create-frame-min-height window-min-height
                (min (or create-frame-max-height (create-frame-max-height))
                     nb-lines))))
      (show-frame fr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; `shrink-fit.el' ends here
