;;; motif-keys.el --- Key bindings that follow the Motif standard.
;;; Copyright (C) 1994 Pete Forman

;; Author: Pete Forman <pete.forman@bedford.waii.com>
;; Maintainer: Pete Forman <pete.forman@bedford.waii.com>

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


;; LISPDIR ENTRY for the Elisp Archive ===============================
;;    LCD Archive Entry:
;;    motif-keys|Pete Forman|pete.forman@airgun.wg.waii.com|
;;    Key bindings that follow the Motif standard.|
;;    11-Nov-1994||~/terms/motif-keys.el.Z|

;; INSTALLATION ======================================================
;; To use this file, byte-compile it, install it somewhere in your
;; load-path, and put in your ~/.emacs, or site-init.el, etc.:
;;
;;   (load "motif-keys")

;; CODE ==============================================================
;;
;; @(#)motif-keys.el	1.2
;;
;; Emacs-19 key bindings that follow the Motif VirtualBindings standard.
;;
;; This makes the key usage in emacs consistent with Motif applications.
;; No use is made of Motif code.
;;
;; The comment has the Motif Virtual Key followed by the typical keystrokes on a
;; 101 key keyboard.
;;
;; References:  Motif 1.2.3 VirtualBindings, s-region.el (emacs 19.24 and later)
;;
;; Redundant definitions have not been included:
;;
;;     KDown, KUp, KLeft, KRight, KPageUp, KPageDown, KInsert, KTab, KSpace
;;
;;
;; These have not been implemented yet:
;;
;;     KMenuBar   F10       Can't get at the menubar with keys (only with MS)
;;     KMenu      F4        Can't get at popups with keys (only cMS1, cMS3)
;;     KClear               No standard key on a 101 keyboard
;;     KCancel    Esc       Can't use under emacs
;;
;;
;; These are normally grabbed by the window manager:
;;
;;     KNextWindow (aTab), KPrevWindow (saTab), KWindowMenu (sEsc, aSpc)
;;
;; as are the alt function keys.
;;
;;
;; These have no obvious relevance in emacs (until menus work from keys):
;;
;;     KActivate, KSelect, KExtend, KNextMenu, KPrevMenu, KNextField,
;;     KPrevField, KAddMode, KSelectAll (c/ might be useful in some modes),
;;     KDeselectAll (c\ might be used for dired-unmark-all-files),
;;     KPrimaryPaste, KPrimaryCopy, KPrimaryCut, KNextFamilyWindow,
;;     KPrevFamilyWindow, KQuickPaste, KQuickCopy, KQuickCut, KQuickExtend,
;;     KReselect
;;

(global-set-key [f1] 'help)                      ; KHelp         F1
(global-set-key [f6] 'other-window)              ; KNextPane     F6
(global-set-key [delete] 'delete-char)           ; KDelete       Del
(global-set-key [home] 'beginning-of-line)       ; KBeginLine    Hom
(global-set-key [end] 'end-of-line)              ; KEndLine      End
(global-set-key [C-home] 'beginning-of-buffer)   ; KBeginData    cHom
(global-set-key [C-end] 'end-of-buffer)          ; KEndData      cEnd
(global-set-key [C-left] 'backward-word)         ; KPrevWord     cLf
(global-set-key [C-right] 'forward-word)         ; KNextWord     cRt
(global-set-key [C-prior] 'scroll-right)         ; KPageRight    cPgUp
(global-set-key [C-next] 'scroll-left)           ; KPageLeft     cPgDn
(global-set-key [C-delete] 'kill-line)           ; KEraseEndLine cDel
(global-set-key [C-down] 'forward-paragraph)     ; KNextPara     cDn
(global-set-key [C-up] 'backward-paragraph)      ; KPrevPara     cUp
(global-set-key [M-backspace] 'undo)             ; KUndo         aBS

;; Next definitions apply to selections.  We rely on s-region.el to actually do
;; the global-set-key for the entries commented here.  Its prime purpose is to
;; make the shift key work for keyboard selection.  The emacs kill-ring is used
;; rather than the X selection or cut buffers but this is mostly sorted out ok
;; elsewhere.

(require 's-region)
;;(global-set-key [S-delete] 'kill-region)         ; KCut          sDel
;;(global-set-key [C-insert] 'copy-region-as-kill) ; KCopy         cIns
;;(global-set-key [S-insert] 'yank)                ; KPaste        sIns

;;; motif-keys.el ends here
