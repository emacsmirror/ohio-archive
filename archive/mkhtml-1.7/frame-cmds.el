;;; frame-cmds.el --- Frame and window commands (interactive functions).
;; 
;; Author: D. ADAMS
;; Maintainer: D. ADAMS
;; Copyright (C) 1996-2001, Drew Adams, all rights reserved.
;; Created: Tue Mar  5 16:30:45 1996
;; Version: $Id: frame-cmds.el,v 1.9 2001/01/03 17:36:19 dadams Exp $
;;   Last modified by: 
;;   Last modified on: Wed Jan  3 09:36:17 2001
;;   Update count: 918
;; Keywords: internal, extensions, mouse, local
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;;
;;    Frame and window commands (interactive functions).
;;
;;
;;  See files `shrink-fit.el' and `shrink-fit-all.el' for other frame
;;  commands.
;;
;;
;;  Main new functions defined here:
;;
;;    `delete-1-window-frames-on', `delete/iconify-window',
;;    `delete/iconify-windows-on', `hide-everything', `hide-frame',
;;    `iconify-everything', `iconify/map-frame',
;;    `mouse-iconify/map-frame', `mouse-remove-window',
;;    `mouse-show-hide-from-minibuffer', `remove-window',
;;    `remove-windows-on', `rename-frame',
;;    `rename-non-minibuffer-frame', `show-*Help*-buffer',
;;    `show-a-frame-on', `show-buffer-menu', `show-frame',
;;    `show-hide'.
;;
;;
;;  ***** NOTE: The following EMACS PRIMITIVES have been REDEFINED HERE:
;;
;;  `delete-window' - If only one window in frame, `delete-frame'.
;;  `delete-windows-on' - 
;;     1) Uses `read-buffer'.
;;     2) Calls `delete-window', so this also deletes frames where
;;        window showing the BUFFER is the only window.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; RCS $Log: frame-cmds.el,v $
;; RCS Revision 1.9  2001/01/03 17:36:19  dadams
;; RCS *** empty log message ***
;; RCS
;; RCS Revision 1.8  2001/01/03 00:38:18  dadams
;; RCS *** empty log message ***
;; RCS
;; RCS Revision 1.7  2000/12/07 19:47:23  dadams
;; RCS 1. Removed to new file `shrink-fit.el':
;; RCS    create-empty-frame-*, create-empty-special-*,
;; RCS    create-frame-max-*[-percent], create-frame-min-*,
;; RCS    enable-shrink-frame-to-fit, making-frame-[done-]msg, shrink-frame-to-fit.
;; RCS 2. Removed to new file `shrink-fit-all.el':
;; RCS    display-buffer, fit-frame-if-one-window*, pop-to-buffer,
;; RCS    switch-to-buffer.
;; RCS 3. Removed require of misc-fns.el.
;; RCS
;; RCS Revision 1.6  2000/12/07 17:26:13  dadams
;; RCS 1. Added: functions create-frame-max-*, vars create-frame-max-*-percent.
;; RCS 2. Changed default values to nil: create-frame-max-*.
;; RCS 3. shrink-frame-to-fit: Use, by priority, var, then fn create-frame-max-*.
;; RCS
;; RCS Revision 1.5  2000/11/28 20:17:12  dadams
;; RCS Optional require's via 3rd arg=t now.
;; RCS
;; RCS Revision 1.4  2000/11/27 17:27:25  dadams
;; RCS hide frame: fixed bug: Added get-a-frame for frame name read.
;; RCS
;; RCS Revision 1.3  2000/11/08 15:53:22  dadams
;; RCS shrink-frame-to-fit: added show-frame at end.
;; RCS
;; RCS Revision 1.2  2000/09/27 22:09:32  dadams
;; RCS 1. Reordered.
;; RCS 2. create-empty-special-display-frame-height: 20 -> 9.
;; RCS 3. Added: fit-frame-when-pop-to-p, fit-frame-when-display-p,
;; RCS    fit-frame-when-switch-to-p, frame-iconified-p, display-buffer.
;; RCS 4. defsubst's -> defun's (and autoload them).
;; RCS 5. autoload delete-windows-on.
;; RCS 6. remove-window: only make-frame-invisible if not iconified (HACK).
;; RCS 7. switch-to-buffer: added fit-frame-if-one-window-and-cond.
;; RCS 8. pop-to-buffer: raise-frame -> fit-frame-if-one-window-and-cond.
;; RCS
;; RCS Revision 1.1  2000/09/14 17:20:09  dadams
;; RCS Initial revision
;; RCS
; Revision 1.7  1999/10/06  07:29:08  dadams
; create-frame-max-height: 40 for SGI, 55 for Windows.
;
; Revision 1.6  1999/10/05  14:44:59  dadams
; rename-frame: fixed bug if only 1 frame and old-name was a frame.
;
; Revision 1.5  1999/08/25  13:50:22  dadams
; Made create-frame-max-height smaller if SGI.
;
; Revision 1.4  1999/08/25  12:47:05  dadams
; 1. Added: hide-everything, show-buffer-menu, show-hide.
; 2. Increased create-frame-max-height (40 to 50).
;
; Revision 1.3  1999/04/02  14:26:47  dadams
; making-frame-msg: Removed sit-for 1/2 sec.
;
; Revision 1.2  1999/03/17  14:07:38  dadams
; 1. Added require: frame-fns, strings (compile only).
; 2. delete-1-window-frames-on: ensure a buffer object (not a name).
; 3. Removed: pop-to-buffer, switch-to-buffer.
;
; Revision 1.1  1997/03/19  16:16:24  dadams
; Initial revision
;
; Revision 1.22  1996/06/28  12:50:46  dadams
; 1. Removed mouse-tear-off-window to new file mouse+.el.
; 2. shrink-frame-to-fit: Treat negative prefix arg.
;
; Revision 1.21  1996/06/06  13:45:22  dadams
; Update of file dependency comments (e.g. "Autoloaded from...").
;
; Revision 1.20  1996/04/26  14:09:03  dadams
; delete/iconify-windows-on, show-a-frame-on: Do nothing if null buffer.
;
; Revision 1.19  1996/04/16  09:02:05  dadams
; shrink-frame-to-fit: Interactively, allow for either shrink wrapping or
;                      specifying new width and height.
;
; Revision 1.18  1996/04/15  08:13:58  dadams
; shrink-frame-to-fit:
;   1. Added FRAME arg.
;   2. set-frame-width + set-frame-height -> set-frame-size
;      (set-frame-width appears bugged: It didn't always take effect)
;   3. Test empty-buf-p before one-window-p (trivial).
;
; Revision 1.17  1996/04/12  15:50:14  dadams
; Added enable-shrink-frame-to-fit.  Use in shrink-frame-to-fit.
;
; Revision 1.16  1996/04/12  13:39:43  dadams
; 1. Added: create-frame-max-width(height).
; 2. shrink-frame-to-fit: Use create-frame-max-width(height), not current.
;
; Revision 1.15  1996/04/05  14:26:07  dadams
; Improved Commentary:  List redefinitions.
;
; Revision 1.14  1996/04/04  17:10:44  dadams
; 1. Added create-empty-frame-width(height),
;    create-empty-special-display-frame-width(height).
; 2. shrink-frame-to-fit: Took these into account.
;
; Revision 1.13  1996/04/04  07:33:16  dadams
; shrink-frame-to-fit: Added nb-lines: Count lines when look for longest.
;
; Revision 1.12  1996/04/03  13:21:05  dadams
; shrink-frame-to-fit: Removed outside save-excursion.
;                      Added save-excursion around movements to calc width.
;
; Revision 1.11  1996/04/02  07:39:57  dadams
; shrink-frame-to-fit: Added save-window-excursion select-frame to use frame.
;
; Revision 1.10  1996/03/20  17:14:11  dadams
; Added making-frame-msg and making-frame-done-msg.
;
; Revision 1.9  1996/03/20  15:45:32  dadams
; delete-window, delete-windows-on, show-frame, hide-frame, show-a-frame-on,
; show-*Help*-buffer, pop-to-buffer: defun -> defsubst.
;
; Revision 1.8  1996/03/18  14:35:38  dadams
; 1. create-frame-min-width: 40 -> 20.
; 2. show-frame, hide-frame, show-a-frame-on, delete-1-window-frames-on,
;    delete-window, delete-windows-on, pop-to-buffer, switch-to-buffer:
;        defsubst -> defun.
;
; Revision 1.7  1996/03/15  15:52:12  dadams
; Added create-frame-min-width, create-frame-min-height; shrink-frame-to-fit
; now respects these.
;
; Revision 1.6  1996/03/15  15:20:13  dadams
; shrink-frame-to-fit: Added optional args.
;
; Revision 1.5  1996/03/15  08:18:52  dadams
; Added shrink-frame-to-fit.
;
; Revision 1.4  1996/03/14  14:08:36  dadams
; show-frame, hide-frame, show-a-frame-on, delete-1-window-frames-on,
; delete-window, delete-windows-on, pop-to-buffer, switch-to-buffer:
;    defun -> defsubst.
;
; Revision 1.3  1996/03/12  15:30:28  dadams
; delete/iconify-window: Unless one-window-p, do old-delete-window outside of
;                        save-window-excursion.
;
; Revision 1.2  1996/03/08  14:58:33  dadams
; 1. drew-util-19.el -> misc-fns.el.
; 2. delete-windows-on: a. Fixed incorrect interactive spec (bad paren).
;                       b. Second arg FRAME also provided interactively now.
; 3. Added: delete/iconify-window, delete/iconify-windows-on.
;
; Revision 1.1  1996/03/05  15:32:59  dadams
; Initial revision
;
;; 
;; Previous Change Log (as `drew-window-cmds.el'):
;; 
; Revision 1.9  1996/02/27  12:53:33  dadams
; show-frame: Call make-frame-visible.
;
; Revision 1.8  1996/02/12  09:28:30  dadams
; Updated header keywords (for finder).
;
; Revision 1.7  1996/02/09  07:23:27  dadams
; Moved show-*Help*-buffer here from drew-misc-19.el.
;
; Revision 1.6  1996/01/30  10:16:03  dadams
; 1. show-frame: Don't make-frame-visible.  Done by raise-frame anyway.
; 2. Added show-a-frame-on.
;
; Revision 1.5  1996/01/12  16:59:13  dadams
; Added redefinitions of pop-to-buffer and switch-to-buffer.
;
; Revision 1.4  1996/01/09  09:20:44  dadams
; Moved delete-windows-on from drew-windows.el to drew-window-cmds.el, and
; made it interactive (the original was interactive).
;
; Revision 1.3  1996/01/08  13:55:01  dadams
; Added rename-non-minibuffer-frame.  Use in iconify-everything,
; iconify/map-frame, mouse-iconify/map-frame.
;
; Revision 1.2  1995/12/28  15:05:44  dadams
; Removed requires for drew-util-19.el and elect-mbuf.el, since
; autoloaded.
;
; Revision 1.1  1995/12/12  16:53:53  dadams
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

(require 'cl) ;; when, unless, dolist
(eval-when-compile (require 'strings)) ;; read-buffer
(require 'frame-fns) ;; frames-on, get-frame-name, get-a-frame, read-frame

(require 'icomplete+ nil t) ;; (no error if not found): read-from-minibuffer


(provide 'frame-cmds)

;;;;;;;;;;;;;;;;;;;;;;;



;;; Commands ---------------------------------------------------

;;;###autoload
(defun iconify-everything ()
  "Iconify all frames of session at once."
  (interactive)
  (dolist (frame (visible-frame-list))
    (rename-non-minibuffer-frame frame)
    (iconify-frame frame)))

;;;###autoload
(defun hide-everything ()
  "Hide all frames of session at once.
Iconify minibuffer frame; make all others invisible."
  (interactive)
  (let ((minibuf-frame-name
         (and (boundp 'minibuffer-frame)
              (cdr (assq 'name (frame-parameters minibuffer-frame))))))
    (dolist (frame (frame-list))
      (if (eq minibuf-frame-name
              (cdr (assq 'name (frame-parameters frame))))
          (iconify-frame frame)         ; minibuffer frame
        (make-frame-invisible frame))))) ; other frames
    
;;;###autoload
(defun show-buffer-menu ()
  "Call `buffer-menu' after making all frames visible.
Useful after using `hide-everything' because of a Windows bug that
doesn't let you display frames that have been made visible after 
being made invisible."
  (interactive)
  (let ((minibuf-frame-name
         (and (boundp 'minibuffer-frame)
              (cdr (assq 'name (frame-parameters minibuffer-frame))))))
    (dolist (frame (frame-list))
      (if (eq minibuf-frame-name
              (cdr (assq 'name (frame-parameters frame))))
          (make-frame-visible frame)    ; minibuffer frame
        (iconify-frame frame)))         ; other frames
    (buffer-menu)))

;;;###autoload
(defun show-hide ()
  "< 2 frames visible => `show-buffer-menu'; else `hide-everything'."
  (interactive)
  (if (< (length (visible-frame-list)) 2)
      (show-buffer-menu)
    (hide-everything)))
    
;;;###autoload
(defun mouse-show-hide-from-minibuffer (click)
  "`show-hide' if mouse clicked in minibuffer; else `mouse-buffer-menu'."
  (interactive "e")
  (if (window-minibuffer-p (posn-window (event-start click)))
      (show-hide)
    (mouse-buffer-menu click)))
    
;;;###autoload
(defun iconify/map-frame (&optional iconify-all)
  "Iconify selected frame if now mapped.  Map it if now iconified.
With non-nil prefix arg ICONIFY-ALL, iconify all visible frames."
  (interactive "P")
  (if iconify-all
      (iconify-everything)
    (rename-non-minibuffer-frame)
    (iconify-or-deiconify-frame)))

;;;###autoload
(defun mouse-iconify/map-frame (click)
  "Iconify frame clicked on, if now mapped.  Map it if now iconified."
  (interactive "e")
  (select-window (posn-window (event-start click)))
  (rename-non-minibuffer-frame)
  (iconify-or-deiconify-frame))



(or (fboundp 'old-delete-window)
    (fset 'old-delete-window (symbol-function 'delete-window)))

;; REPLACES ORIGINAL (built-in):
;; If WINDOW is the only one in its frame, `delete-frame'.
;;;###autoload
(defun delete-window (&optional window)
  "Remove WINDOW from the display.  Default is `selected-window'.
If WINDOW is the only one in its frame, then `delete-frame' too."
  (interactive)
  (setq window (or window (selected-window)))
  (select-window window)
  (if (one-window-p) (delete-frame) (old-delete-window (selected-window))))


(or (fboundp 'old-delete-windows-on)
    (fset 'old-delete-windows-on (symbol-function 'delete-windows-on)))

;; REPLACES ORIGINAL (built-in):
;; 1) Uses `read-buffer' in interactive spec.
;; 2) Calls `delete-window', so if use my `delete-window' this also deletes
;;    frames where window showing the BUFFER is the only window.
;;;###autoload
(defun delete-windows-on (buffer &optional frame)
  "Delete windows showing BUFFER.

Optional second arg FRAME controls which frames are considered.
  If nil or omitted, delete all windows showing BUFFER in any frame.
  If `t', delete only windows showing BUFFER in the selected frame.
  If `visible', delete all windows showing BUFFER in any visible frame.
  If a frame, delete only windows showing BUFFER in that frame.

Interactively, FRAME depends on the prefix arg, as follows:
  Without a prefix arg (prefix = nil), FRAME is nil (all frames).
  With prefix arg >= 0, FRAME is `t' (this frame only).
  With prefix arg < 0,  FRAME is `visible' (all visible frames)."
  (interactive
   (list (read-buffer "Delete windows on buffer: " (current-buffer) 'existing)
         (and current-prefix-arg
              (or (natnump (prefix-numeric-value current-prefix-arg))
                  'visible))))
  ;; `get-buffer-window' interprets FRAME oppositely for t and nil, so switch.
  (setq frame (if (eq t frame) nil (if (eq nil frame) t frame)))
  (let (win)
    (while (setq win (get-buffer-window buffer frame)) (delete-window win))))

(defsubst frame-iconified-p (frame)
  (and (frame-live-p frame) (eq (frame-visible-p frame) 'icon)))

;;;###autoload
(defun remove-window (&optional window)
  "Remove WINDOW from the display.  Default is `selected-window'.
If WINDOW is the only one in its frame, then:
   If WINDOW is dedicated to its buffer, then make its frame invisible.
   Otherwise, delete its frame (as well as the window)."
  (interactive)
  (setq window (or window (selected-window)))
  (select-window window)
  (if (and (window-dedicated-p (selected-window))
           (one-window-p t))
      (let ((fr (selected-frame)))
        ;; HACK because of Emacs bug: `raise-frame' won't raise a frame
        ;; that was first iconified and then made invisible.
        ;; So, here we don't make an iconified frame invisible.
        (unless (frame-iconified-p fr)
          (make-frame-invisible fr)))
    (delete-window)))

;;;###autoload
(defun remove-windows-on (buffer)
  "Remove all windows showing BUFFER.  This calls `remove-window'
on each window showing BUFFER."
  (interactive
   (list (read-buffer "Remove all windows showing buffer: "
                      (current-buffer) 'existing)))
  (setq buffer (get-buffer buffer))     ; Convert to buffer.
  (when buffer                          ; Do nothing if null BUFFER.
    (dolist (fr (frames-on buffer t))
      (remove-window (get-buffer-window buffer t)))))

;;;###autoload
(defun mouse-remove-window (click)
  "Remove the window you click on.  (This calls `remove-window'.)
This command must be bound to a mouse click."
  (interactive "e")
  (mouse-minibuffer-check click)
  (remove-window (posn-window (event-start click))))

;;;###autoload
(defun delete/iconify-window (&optional window frame-p)
  "Delete or iconify WINDOW (default: `selected-window').
If WINDOW is the only one in its frame (`one-window-p'), then optional
arg FRAME-P determines the behavior regarding the frame, as follows:
  If FRAME-P is nil, then the frame is deleted (with the window).
  If FRAME-P is `t', then the frame is iconified.
  If FRAME-P is a symbol naming a function, the function is applied
             to WINDOW as its only arg.
             If the result is nil, then the frame is deleted.
             If the result is non-nil, then the frame is iconified.
  If FRAME-P is anything else, then behavior is as if FRAME-P were the
             symbol `window-dedicated-p': the frame is iconified if
             WINDOW is dedicated, otherwise the frame is deleted.

Interactively, FRAME-P depends on the prefix arg, as follows:
  Without a prefix arg (prefix = nil), FRAME-P is `window-dedicated-p'.
  With prefix arg < 0, FRAME-P is `t'.  The frame is iconified.
  With prefix arg >= 0, FRAME-P is nil.  The frame is deleted."
  (interactive
   (list nil (if current-prefix-arg
                 (not (natnump (prefix-numeric-value current-prefix-arg)))
               'window-dedicated-p)))
  (setq window (or window (selected-window)))
  (let ((one-win-p t))
    (save-window-excursion
      (select-window window)
      (if (one-window-p)
          (if frame-p
              (if (eq t frame-p)
                  (iconify-frame)
                (unless (and (symbolp frame-p) (fboundp frame-p))
                  (setq frame-p 'window-dedicated-p))
                (if (funcall frame-p window) (iconify-frame) (delete-frame)))
            (delete-frame))             ; Default.
        (setq one-win-p nil)))
    ;; Do this outside `save-window-excursion'.
    (unless one-win-p (old-delete-window window))))

;;;###autoload
(defun delete/iconify-windows-on (buffer &optional frame frame-p)
  "For each window showing BUFFER: delete it or iconify its frame.
\(This calls `delete/iconify-window' on each window showing BUFFER.)

Optional second arg FRAME controls which frames are considered.
  If nil or omitted, treat all windows showing BUFFER in any frame.
  If `t', treat only windows showing BUFFER in the selected frame.
  If `visible', treat all windows showing BUFFER in any visible frame.
  If a frame, treat only windows showing BUFFER in that frame.

Optional third arg FRAME-P controls what to do with one-window frames.
  If FRAME-P is nil, then one-window frames showing BUFFER are deleted.
  If FRAME-P is `t', then one-window frames are iconified.
  If FRAME-P is a symbol naming a function, the function is applied
             to each window showing buffer in a frame by itself.
             If the result is nil, then the frame is deleted.
             If the result is non-nil, then the frame is iconified.
  If FRAME-P is anything else, then behavior is as if FRAME-P were the
             symbol `window-dedicated-p': One-window frames are
             iconified if window is dedicated, else they are deleted.

Interactively, FRAME is nil, and FRAME-P depends on the prefix arg:
  Without a prefix arg (prefix = nil), FRAME-P is `window-dedicated-p'.
  With prefix arg < 0, FRAME-P is `t'.  The frame is iconified.
  With prefix arg >= 0, FRAME-P is nil.  The frame is deleted."
  (interactive
   (list (read-buffer "Delete windows on buffer: "
                      (current-buffer) 'existing)
         nil
         (if current-prefix-arg
             (not (natnump (prefix-numeric-value current-prefix-arg)))
           'window-dedicated-p)))
  (setq buffer (get-buffer buffer))     ; Convert to buffer.
  (when buffer                          ; Do nothing if null BUFFER.
    ;; `get-buffer-window' interprets FRAME oppositely for t and nil,
    ;; so switch.
    (setq frame (if (eq t frame) nil (if (eq nil frame) t frame)))
    (dolist (fr (frames-on buffer frame))
      (delete/iconify-window (get-buffer-window buffer frame) frame-p))))

;;;###autoload
(defun rename-frame (&optional old-name new-name all-named)
  "Rename a frame named OLD-NAME to NEW-NAME.
Prefix arg ALL-NAMED non-nil => Rename all frames named FRAME to NEWNAME.

OLD-NAME may be a frame, its name, or nil.  Default is `selected-frame'.

NEW-NAME is a string or nil.  Default NEW-NAME is current `buffer-name'."
  (interactive
   (list (read-frame (concat "Rename " (and current-prefix-arg "all ")
                             "frame" (and current-prefix-arg "s named") ": ")
                     nil t)             ; Default = selected.  Must exist.
         (read-from-minibuffer "Rename to (new name): " (cons (buffer-name) 1))
         current-prefix-arg))
  (setq old-name (or old-name (get-frame-name))) ; Batch default: current.
  (setq new-name (or new-name (buffer-name))) ; Batch default: buffer name.
  ;; Convert to frame if string.
  (let ((fr (get-a-frame old-name)))
    (if all-named
        (while fr
          (modify-frame-parameters fr (list (cons 'name new-name)))
          (setq fr (get-a-frame old-name))) ; Get another.
      (when (string= (get-frame-name fr) (get-frame-name))
        (setq fr (selected-frame)))
      (modify-frame-parameters fr (list (cons 'name new-name))))))

;;;###autoload
(defun rename-non-minibuffer-frame (&optional old-name new-name all-named)
  "Unless OLD-NAME names the `minibuffer-frame', use `rename-frame'
to rename a frame named OLD-NAME to NEW-NAME.

Prefix arg ALL-NAMED non-nil => Rename all frames named FRAME to NEWNAME.
OLD-NAME may be a frame, its name, or nil.  Default is `selected-frame'.
NEW-NAME is a string or nil.  Default NEW-NAME is current `buffer-name'."
  (interactive
   (list (read-frame (concat "Rename " (and current-prefix-arg "all ")
                             "frame" (and current-prefix-arg "s named") ": ")
                     nil t)             ; Default = selected.  Must exist.
         (read-from-minibuffer "Rename to (new name): " (cons (buffer-name) 1))
         current-prefix-arg))
  (setq old-name (or old-name (get-frame-name))) ; Batch default: current.
  (setq new-name (or new-name (buffer-name))) ; Batch default: buffer name.
  (let ((fr (get-a-frame old-name)))    ; Convert to frame if string.
    (if (and (boundp 'minibuffer-frame)
             (eq (cdr (assq 'name (frame-parameters minibuffer-frame)))
                 (cdr (assq 'name (frame-parameters fr)))))
        (and (interactive-p)
             (error
              "Use `rename-frame' if you really want to rename minibuffer."))
      (rename-frame))))

;;;###autoload
(defun show-frame (frame)
  "Make FRAME visible and raise it, without selecting it.
FRAME may be a frame or its name."
  (interactive (list (read-frame "Frame to make visible: ")))
  (setq frame (get-a-frame frame))
  (make-frame-visible frame)
  (raise-frame frame))

;;;###autoload
(defun hide-frame (frame &optional prefix)
  "Make FRAME invisible.  Like `make-frame-invisible', but reads frame name.
Non-nil PREFIX makes it invisible even if all other frames are invisible."  
  (interactive (list (read-frame "Frame to make invisible: ")))
  (make-frame-invisible (get-a-frame frame) prefix))

;;;###autoload
(defun show-a-frame-on (buffer)
  "Make visible and raise a frame showing BUFFER, if there is one.
Neither the frame nor the BUFFER are selected.
BUFFER may be a buffer or its name (a string)."
  (interactive
   (list (read-buffer "Show a frame showing buffer: " (other-buffer)
                      'existing)))
  (when buffer                          ; Do nothing if null BUFFER.
    (let ((fr (car (frames-on buffer)))) (when fr (show-frame fr)))))

;;;###autoload
(defun show-*Help*-buffer ()
  "Raise a frame showing buffer *Help*, without selecting it."
  (interactive) (show-a-frame-on "*Help*"))

;;;###autoload
(defun delete-1-window-frames-on (buffer)
  "Delete all visible 1-window frames showing BUFFER."
  (interactive
   (list (read-buffer "Delete all visible 1-window frames showing buffer: "
                      (current-buffer) 'existing)))
  (setq buffer (get-buffer buffer))
  (save-excursion
    (when (buffer-live-p buffer)        ; Do nothing if dead buffer.
      (dolist (fr (frames-on buffer))   ; Is it better to search through 
        (save-window-excursion          ; `frames-on' or `get-buffer-window-list'?
          (select-frame fr)
          (when (one-window-p t fr) (delete-frame)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; `frame-cmds.el' ends here
