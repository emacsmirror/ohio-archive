;;; pb-popup.el --- ensure display of a process buffer when new output arrives

;; Copyright (C) 1994, 1995 Noah S. Friedman

;; Author: Noah Friedman <friedman@prep.ai.mit.edu>
;; Maintainer: friedman@prep.ai.mit.edu
;; Keywords: extensions
;; Status: Works in Emacs 19 and XEmacs/Lucid Emacs.
;; Created: 1994-06-23

;; LCD Archive Entry:
;; pb-popup|Noah Friedman|friedman@prep.ai.mit.edu|
;; ensure display of a process buffer when new output arrives|
;; 25-Mar-1995|1.2|~/misc/pb-popup.el.gz|

;; $Id: pb-popup.el,v 1.2 1995/03/25 08:46:09 friedman Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 675 Massachusetts Avenue; Cambridge, MA 02139, USA.

;;; Commentary:

;; The function `pb-popup' may be attached to a process filter, via
;; whatever hook mechanism available or by direct call, to make sure
;; that a window with a buffer's recently added contents is visible.
;;
;; If no such window is visible, it splits the largest visible window
;; (according to various parameters; see the variables below) and
;; displays the portion of the process buffer with the most recently
;; inserted text.  This is done rather than moving point in an
;; existing window (if any show the buffer at all) because it is
;; assumed that if you are elsewhere in the buffer, it is for a
;; reason.  Likewise, it doesn't take over other windows, but creates
;; always creates a new one.
;;
;; You can also choose to create new frames instead of splitting windows.
;;
;; This functionality can be disabled at any time by disabling
;; pb-popup-mode.  This variable can be made buffer-local, but it is
;; not by default.

;; For an example of this library's usage, see comint-popup.el.

;; Note that throughout this program, the Emacs "frame" terminology is
;; used.  Lucid never switched to the "frame" nomenclature adopted by Emacs
;; after the two sets of sources diverged; it uses "screen" instead.  To
;; ensure that this library works in both versions, this program uses its
;; own aliases for the various frame/screen operations.  However, all
;; variables and functions defined here use the Emacs terminology.

;;; Code:


;;;###autoload
(defvar pb-popup-mode t
  "*If non-nil, then display buried process buffers when messages appear.")

;;;###autoload
(defvar pb-popup-ratio 4
  "*Reciprocal proportion of a window to use up when splitting windows.
This value should be an integer greater than zero.

This variable is used to determine how much of an old window to use
for the new one.  The reciprocal is used, so a value of 1 causes the window
to appear full-screen, 2 makes the window half of the screen, 3 makes it
one third, etc.

See the documentation for the function `pb-popup-window' for more info.")

;;;###autoload
(defvar pb-popup-min-height (default-value 'window-min-height)
  "*Minimum height of popup windows, expressed as an integer.
When splitting windows, pb-popup-ratio is used to determined its size.
However, the actual height of the new window will be no less than the
number of lines specified by pb-popup-min-height or window-min-height,
whichever is larger.

If pop-up-windows is nil, no windows will pop up at all.

See the documentation for the function `pb-popup-window' for more info.")

;;;###autoload
(defvar pb-popup-available-frames 'visible
  "*Value used to determine which frames to search for buffer windows.

This variable determines which frames will be searched to see if a
buffer is already visible in a window.  It may be set as follows:

* If `t', then search all frames.
* If `nil', search only current frame.
* If `visible', search all frames which are visible
  (i.e. not iconified or completely obscured).
* If `0', search all visible or iconified frames.
* If set to a specific frame object, consider only that frame.
  This is useful if you have a dedicated frame for that process.

Multiple frames only exist in emacs 19; if using emacs 18, the value of
this variable doesn't matter.")

;;;###autoload
(defvar pb-popup-make-new-frames nil
  "*If `t' and it is possible to make a new frame to display buffer, do so.
The following conditions must be met in order to make a new frame:

* Both this variable and `pop-up-frames' must be non-`nil'.
* No other frame must currently have a window displaying the end of
  process output for the buffer in question.
* It must be possible to create new frames, i.e. in a window system
  and in a version of emacs which supports multiple frames.

If you enable the creation of new frames, you may also want to set
`pb-popup-available-frames' to `t' or `visible' so all interesting frames
can be searched first.")


;;;###autoload
(defun pb-popup-mode (&optional prefix)
  "Enable or disable window popups for a process buffer.

A negative prefix argument disables this mode.
No argument or any non-negative argument enables it.

The user may also enable or disable this mode simply by setting the
variable of the same name.

This function does nothing to install window-popups for any process; it
only determines whether the popup routines will do anything if installed."
  (interactive "P")
  (setq pb-popup-mode (>= (prefix-numeric-value prefix) 0))
  (cond ((not (interactive-p)))
        (pb-popup-mode
         (message "pb-popup-mode is enabled"))
        (t
         (message "pb-popup-mode is disabled")))
  pb-popup-mode)


(defun pb-popup (object &optional mark)
  "Find a window displaying buffer with output from a process, or create one.
The following describes the arguments to this function:

* OBJECT (required)
  A process object, a buffer, or a buffer name.  If a process, the process
  should have a buffer associated with it where its output normally goes.

* MARK   (optional)
  A marker or a symbol of a variable containing a marker representing the
  end of the process output, which indicates what portion of the buffer it
  is desirable to see.
  Usually this will be the process-mark for the process associated with
  OBJECT; that is the default if not specified.  If no process exists for
  OBJECT and no marker is specified, the default is the end of the buffer
  associated with OBJECT.
  If MARK is a symbol, it may be a buffer-local variable containing some
  other arbitrary marker that will be assumed to be the end marker for that
  buffer.  This may be useful if you have buffers used to interact with a
  process, but which isn't the primary \"process buffer\".

If no windows currently display the relevant buffer, or some do but the
point of insertion for new output isn't visible in them (perhaps because
the window is scrolled to a prior region), pb-popup finds the largest
visible window and splits it, putting the buffer in the new window at a
point showing the new text.

`pb-popup-ratio' is used to determine how much of the old window to use for
the creation of a new one.  However, the actual height of the new window
will be no less than the number of lines specified by `pb-popup-min-height'
or `window-min-height', whichever is larger.

If `pop-up-windows' or `pb-popup-mode' are nil, no windows will pop up.
The former is an emacs-wide variable; the latter affects only this function."
  (let* ((orig-window (selected-window))
         (orig-buffer (current-buffer))
         (proc (cond ((processp object)
                      object)
                     ((or (bufferp object)
                          (stringp object))
                      (get-buffer-process object))))
         (buffer (cond ((processp object)
                        (process-buffer object))
                       ((bufferp object)
                        object)
                       ((stringp object)
                        (get-buffer object))))
         marker pop-up-p found)

    (unwind-protect
        (progn
          ;; marker symbol, pb-popup-mode, and other pb-* parameters may be
          ;; buffer-local, so do everything in the context of the
          ;; potential buffer to be popped.
          (set-buffer buffer)

          (setq pop-up-p (and pb-popup-mode pop-up-windows))
          (setq marker (cond ((null mark)
                              (process-mark proc))
                             ((markerp mark)
                              mark)
                             ((symbolp mark)
                              (symbol-value mark))
                             (t
                              (point-max))))

          (cond
           (pop-up-p
            (walk-windows (function (lambda (win)
                                      (and (not found)
                                           (eq (window-buffer win) buffer)
                                           (<= marker (window-end win))
                                           (>= marker (window-start win))
                                           (setq found t)))
                                    nil pb-popup-available-frames))

            (cond
             ((and (not found)
                   pb-popup-make-new-frames
                   window-system
                   (boundp 'pop-up-frames)
                   pop-up-frames
                   (fboundp 'pb-make-frame))
              (pb-popup-frame buffer marker))
             ((not found)
              (pb-popup-window buffer marker))))))
      (select-window orig-window)
      (set-buffer orig-buffer))))

;; This function should only be called when the popup-buffer is
;; current; otherwise, buffer-local pb-* parameters may not be in
;; effect when referenced.
(defun pb-popup-window (buffer marker)
  (let* ((bigwin (if (fboundp 'pb-make-frame)
                     (get-largest-window pb-popup-available-frames)
                   (get-largest-window)))
         (min-height (max window-min-height
                          pb-popup-min-height))
         (ratio (cond
                 ((and (natnump pb-popup-ratio)
                       (> pb-popup-ratio 0))
                  pb-popup-ratio)
                 ;; Choose some ratio too big to be useful.
                 (t (window-height bigwin)))))

    (split-window bigwin
                  (min (- (window-height bigwin) min-height)
                       (- (window-height bigwin)
                          (/ (window-height bigwin) ratio))))

    (select-window (next-window bigwin 'no-minibuf))
    ;; Must use switch-to-buffer to make permanent selection of buffer
    ;; to display in new window.
    (switch-to-buffer buffer)
    (goto-char marker)
    (recenter -1)))

(defun pb-popup-frame (buffer marker)
  (let ((frame (pb-make-frame))
        (orig-frame (pb-selected-frame)))
    (pb-select-frame frame)
    (set-buffer buffer)
    (goto-char marker)
    (recenter -1)
    (pb-select-frame orig-frame)))


;;; Ensure smooth operation in both Emacs and XEmacs/Lucid.
;;; Don't just alias frame equivalent functions because that may
;;; potentially confuse other programs.

(cond ((fboundp 'make-frame)
       ;; Emacs 19
       (defalias 'pb-make-frame         'make-frame)
       (defalias 'pb-select-frame       'select-frame)
       (defalias 'pb-selected-frame     'selected-frame))

      ((fboundp 'make-screen)
       ;; XEmacs
       (defalias 'pb-make-frame         'make-screen)
       (defalias 'pb-select-frame       'select-screen)
       (defalias 'pb-selected-frame     'selected-screen)))

(provide 'pb-popup)

;; pb-popup.el ends here



