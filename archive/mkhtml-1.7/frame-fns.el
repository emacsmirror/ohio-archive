;;; frame-fns.el --- Non-interactive frame and window functions.
;; 
;; Author: D. ADAMS
;; Maintainer: D. ADAMS
;; Copyright (C) 1996-2001, Drew Adams, all rights reserved.
;; Created: Tue Mar  5 16:15:50 1996
;; Version: $Id: frame-fns.el,v 1.4 2001/01/03 17:36:39 dadams Exp $
;;   Last modified by: 
;;   Last modified on: Wed Jan  3 09:36:37 2001
;;   Update count: 40
;; Keywords: internal, extensions, local
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;;
;;    Non-interactive frame and window functions.
;;
;;  Files `frame-cmds.el', `shrink-fit.el' and `shrink-fit-all.el'
;;  contain command (interactive function) definitions.
;;
;;  Main new functions defined here:
;;
;;    `1-window-frames-on', `distance', `flash-ding', `frames-on',
;;    `get-a-frame', `get-frame-name', `multi-window-frames-on',
;;    `read-frame', `window-coords'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; RCS $Log: frame-fns.el,v $
;; RCS Revision 1.4  2001/01/03 17:36:39  dadams
;; RCS *** empty log message ***
;; RCS
;; RCS Revision 1.3  2001/01/03 00:38:31  dadams
;; RCS *** empty log message ***
;; RCS
;; RCS Revision 1.2  2000/12/07 19:48:08  dadams
;; RCS *** empty log message ***
;; RCS
;; RCS Revision 1.1  2000/09/14 17:20:12  dadams
;; RCS Initial revision
;; RCS
; Revision 1.1  1999/03/26  15:06:22  dadams
; Initial revision
;
; Revision 1.5  1997/03/18  09:37:55  dadams
; active-minibuffer-frame-background: "gray90" -> "light gray"
;
; Revision 1.4  1996/04/05  14:26:49  dadams
; Improved Commentary:  List redefinitions.
;
; Revision 1.3  1996/03/19  10:13:50  dadams
; 1. Moved here from setup.el: (in)active-minibuffer-frame-background,
;    color-(in)active-minibuffer-frame.  ;;;###autoload them.
; 2. Added redefinition of y-or-n-p that colors minibuffer frame.
;
; Revision 1.2  1996/03/08  14:59:18  dadams
; drew-util-19.el -> misc-fns.el.
;
; Revision 1.1  1996/03/05  15:20:32  dadams
; Initial revision
;;
;; Previous Change Log (as `drew-windows.el'):
;; 
; Revision 1.8  1996/03/05  15:15:18  dadams
; 1. Copyright.  2. Renamed to frame-fns.el.
;
; Revision 1.7  1996/02/14  16:01:21  dadams
; 1. Added: window-coords, distance.
; 2. Autoload avoid.el.
;
; Revision 1.6  1996/02/12  09:33:34  dadams
; Updated header keywords (for finder).
;
; Revision 1.5  1996/01/25  16:18:00  dadams
; windows-on: 2 new optional args (compatible with walk-windows).
; Previously, optional frame arg wasn't even used (!?).
;
; Revision 1.4  1996/01/09  09:20:49  dadams
; Moved delete-windows-on from drew-windows.el to drew-window-cmds.el, and made
; it interactive (the original was interactive).
;
; Revision 1.3  1995/12/28  15:22:38  dadams
; Removed require of drew-util-19.el, since autoloaded.
;
; Revision 1.2  1995/12/12  16:50:37  dadams
; Moved delete-windows-on here from drew-misc-19.el.
;
; Revision 1.1  1995/11/22  15:43:22  dadams
; Initial revision
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code: 

(require 'cl) ;; when, unless, dolist, push
(eval-when-compile (require 'avoid)) ;; mouse-avoidance-point-position

(provide 'frame-fns)

;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun window-coords (&optional position)
  "Return window coordinates of buffer POSITION (default: point).
If POSITION is nil, (point) is used."
  (unless (fboundp 'mouse-avoidance-point-position) (require 'avoid))
  (cdr (mouse-avoidance-point-position)))

;;;###autoload
(defun distance (pt1 pt2)
  "Distance as the crow flies between PT1 and PT2.
PT1 and PT2 are each a cons of the form (X . Y)."
  (let ((xdiff (abs (- (car pt1) (car pt2))))
        (ydiff (abs (- (cdr pt1) (cdr pt2)))))
    (sqrt (+ (* xdiff xdiff) (* ydiff ydiff)))))

;;;###autoload
(defun get-frame-name (&optional frame)
  "Return the string that names FRAME (a frame).  Default is selected frame."
  (unless frame (setq frame (selected-frame)))
  (if (framep frame)
      (cdr (assq 'name (frame-parameters frame)))
    (error "GET-FRAME-NAME:  Argument not a frame: %s." frame)))

;;;###autoload
(defun get-a-frame (frame)
  "Return a frame, if any, named FRAME (a frame or a string).
If none, return nil.
If FRAME is a frame, it is returned."
  (cond ((framep frame) frame)
        ((stringp frame)
         (car (member-if
               (function (lambda (fr) (string= frame (get-frame-name fr))))
               (frame-list))))
        (t
         (error "GET-A-FRAME:  Arg neither a string nor a frame: %s." frame))))

;;;###autoload
(defun read-frame (prompt &optional default existing)
  "Read the name of a frame, and return it as a string.
Prompts with 1st arg, PROMPT (a string).

The default frame is named by the optional 2nd arg, DEFAULT, if a
string or a frame, or by the `selected-frame', if nil.

Non-nil optional 3rd arg, EXISTING, means to allow only names of
existing frames."
  (setq default (or default (get-frame-name)))
  (unless (stringp default)
    (error "read-frame: DEFAULT arg is neither a frame nor a string."))
  (completing-read prompt (frame-alist)
                   ;; To limit to live frames:
                   ;; (function (lambda (fn+f)(frame-live-p (cdr fn+f))))
                   nil existing default 'minibuffer-history))

;;;###autoload
(defun frames-on (buffer &optional frame)
  "List of all live frames showing BUFFER (a buffer or its name).
The optional FRAME argument is as for function `get-buffer-window'."
  (filtered-frame-list (function (lambda (fr) (get-buffer-window buffer fr)))))

;;;###autoload
(defun 1-window-frames-on (buffer)
  "List of all visible 1-window frames showing BUFFER."
  (setq buffer (get-buffer buffer))
  (let ((frs nil))
    (save-excursion
      (set-buffer buffer)
      (when (buffer-live-p buffer)      ; Do nothing if dead buffer.
        (dolist (fr (frames-on buffer)) ; Is it better to search through 
          (save-window-excursion        ; frames-on or windows-on?
            (select-frame fr)
            (when (one-window-p t fr) (push fr frs))))))
    frs))

;;;###autoload
(defun multi-window-frames-on (buffer)
  "List of all visible multi-window frames showing BUFFER."
  (setq buffer (get-buffer buffer))
  (let ((frs nil))
    (save-excursion
      (set-buffer buffer)
      (when (buffer-live-p buffer)      ; Do nothing if dead buffer.
        (dolist (fr (frames-on buffer)) ; Is it better to search through 
          (save-window-excursion        ; frames-on or windows-on?
            (select-frame fr)
            (when (not (one-window-p t fr)) (push fr frs))))))
    frs))

;;;###autoload
(defun flash-ding (&optional do-not-terminate frame)
  "Ring bell (`ding'), after flashing FRAME (default: current), if relevant.
Terminates any keyboard macro executing, unless arg DO-NOT-TERMINATE non-nil."
  (save-window-excursion
    (when frame (select-frame frame))
    (let ((visible-bell t))             ; Flash.
      (ding do-not-terminate)))
  (let ((visible-bell nil))
    (ding do-not-terminate)))		; Bell.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; `frame-fns.el' ends here
