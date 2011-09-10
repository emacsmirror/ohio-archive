;;; brief.el -- Brief 3.1 (TM) keybinding for emacs

;;; Copyright (C) 1994 Alexander Rezinsky <alexr@msil.sps.mot.com>
;;; $Id: brief.el,v 1.5 1995/02/07 10:59:07 alexr Exp $
;;;
;;; Author:   Alex Rezinsky <alexr@msil.sps.mot.com>
;;; Created:  8 Sept 1993
;;; Version:  1.1
;;; Keywords: brief emulation
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; COMMENTARY
;;; ----------
;;;
;;; Most  of these commands  are Brief3.1(TM) editor emulation.  Some
;;; of them are  working   in little different  manner  then original
;;; brief commands.
;;; 
;;; Original emacs key binding NOT REDEFINED.
;;; 
;;;     Cursor over end of line
;;;     -----------------------
;;; In emacs you can't  place cursor over end  of line.  In brief you
;;; free  to  do that.   If you  wish to work  in   brief style - use
;;; "picture mode".
;;; 
;;;     Truncate mode
;;;     -------------
;;; Truncate mode. Brief always truncate your lines on screen if thay
;;; are too long.  After  that in brief you  may work with horisontal
;;; scrolling to edit long lines.  In emacs you can use this style as
;;; well,  and  you can use  "fold  mode". By  default emacs works in
;;; "fold mode" -  long lines are fold  and you  may edit it  without
;;; horisontal scrolling.  If you want  brief behavior by default you
;;; should add follow    line  in your  .emacs  file:
;;; (setq-default truncate-lines t)
;;; 
;;;     Search and replace
;;;     ------------------
;;; Default  emacs search style  - incremental  search.  If you  want
;;; brief  search style -  use seacrh  through F5  key. In search and
;;; replace (F5 and F6) you   able to use  up/down  arrows to get  in
;;; minibuffer previous/next search (or replace) string. If you press
;;; Shift-mouse-button you will  get menu of previous contexts  (only 
;;; if you work in emacs own X-window).
;;; 
;;;     Bookmarks
;;;     ---------
;;; Little  different from brief.  Press Alt-Meta-1 ... Alt-Meta-9 to
;;; set bookmark and Alt-1 ... Alt-9 to go to the bookmark.
;;; 
;;;     Saving environment 
;;;     ------------------
;;; By default if you  work with this  package it tries to  save your
;;; editor environment  between  different  emacs session.  It saves:
;;; several  last  search and replace   contexts,  bookmarks and same
;;; internal     parameters.  File     to saving     environment    -
;;; "~/.emacs.save".
;;; 
;;;     Work inside XTerm window 
;;;    -------------------------
;;; Will cause to several problems... But  if you very like work with
;;; editor inside xterm window do follows:
;;; 
;;; 1. Insert in your ~/.Xdefaults file:
;;; 
;;; XTerm.VT100.Translations:       \
;;; #override   <Key>R1:          string(0x1B) string("[208z")\n\
;;;             <Key>R2:          string(0x1B) string("[209z")\n\
;;;             <Key>R3:          string(0x1B) string("[210z")\n\
;;;             <Key>R4:          string(0x1B) string("[211z")\n\
;;;             <Key>R5:          string(0x1B) string("[212z")\n\
;;;             <Key>R6:          string(0x1B) string("[213z")\n\
;;;             <Key>R7:          string(0x1B) string("[214z")\n\
;;;             <Key>R9:          string(0x1B) string("[216z")\n\
;;;             <Key>R11:         string(0x1B) string("[218z")\n\
;;;             <Key>R13:         string(0x1B) string("[220z")\n\
;;;             <Key>R15:         string(0x1B) string("[222z")\n\
;;;             <Key>KP_Decimal:  string(0x1B) string("[223z")\n\
;;;             <Key>Help:        string(0x1B) string("[224z")\n\
;;;             <Key>KP_Add:      string(0x1B) string("[29~")\n\
;;;             <Key>KP_Subtract: string(0x1B) string("[34~")\n\
;;;             <Key>Insert:      string(0x1B) string("[32~")\n
;;; 
;;; 2. Insert in your ~/.xinitrc file:
;;; 
;;; xmodmap -e "keycode 16 = F11"
;;; xmodmap -e "keycode 18 = F12"
;;; xmodmap -e "keycode 57 = KP_Decimal"
;;; 
;;; 3. Restart your X-window system.
;;; 
;;; After this you will get possibility to work with brief-style key binding
;;; inside XTerm window, but ...
;;; 
;;;      1. Alt modifier will not work.
;;;      2.   Shift, Meta  and  Cntrl modifiers   will not work  with
;;; functional and keypad keys. To  save functionality connected with
;;; these modifiers  here define prefix keys -  R1 instead of Alt, R2
;;; instead of Shift and R3 instead of Ctrl.
;;;
;;;     Note about XTerm windows
;;;     ------------------------
;;; It tested and work OK on Sun-3, Sun-4, Sun-sparc-2.

;;;
;;;     KEY BINDING
;;;     -----------
;;;
;;;   Alt-1...Alt-9  - Goto bookmark
;;;   Alt-b          - Buffer list
;;;   Alt-c          - Mark rectangle region
;;;   Alt-e          - Load file
;;;   Alt-f          - Print full name of current file
;;;   Alt-g          - Goto line
;;;   Alt-i          - Switch insert/overwrite mode
;;;   Alt-j          - Goto bookmark
;;;   Alt-l          - Mark line region
;;;   Alt-m          - Mark stream region
;;;   Alt-n          - Next buffer
;;;   Alt-q          - Quoted insert
;;;   Alt-t          - Toggle truncate mode (see descripton above)
;;;   Alt-s          - Save current buffer
;;;   Alt-w          - Write current buffer to other file.
;;;   Alt-x          - Exit from emacs.
;;;   Alt-Meta-x     - Exit from emacs with save all buffers
;;;   Alt /\         - Up to first nonblanc character
;;;   Alt \/         - Down to first nonblanc character
;;;
;;;   Cntrl ->       - Horisontal scroll right
;;;   Cntrl <-       - Horisontal scroll left
;;;   Cntrl /\       - Vertical scroll up
;;;   Cntrl \/       - Vertical scroll down
;;;  
;;;   F1             - Jump to other window
;;;   F2 ->          - Enlarge window horizontally
;;;   F2 <-          - Shrink window horizontally
;;;   F2 \/          - Enlarge window vertically
;;;   F2 /\          - Shrink window vertically
;;;   F3 ->          - Split window horizontally
;;;   F3 \/          - Split window vertically
;;;   F4             - Delete all other windows
;;;   Alt-F4         - Delete current window
;;;   F5             - Search
;;;   Shift-F5       - Repeat last search
;;;   Cntrl-F5       - Toggle case sensitive/unsensitive search
;;;   F6             - Replace
;;;   Shift-F6       - Repeat last replace
;;;   Cntrl-F6       - Toggle regular expression search
;;;   F7             - Begin/end keyboard macro definition
;;;   Alt-F7         - Save keyboard macro to external file
;;;   F8             - Execut keyboard macro
;;;   Alt-F8         - Read keyboard macro from external file
;;;   F10            - Execute shell command
;;;  
;;;   Home           - Begin of current line
;;;   Home Home      - Begin of current window
;;;   Home Home Home - Begin of current buffer
;;;   End            - End of current line
;;;   End End        - End of current window
;;;   End End End    - End of current file
;;;   Del            - Delete current character.
;;;  
;;;   Ins       - Paste region (line/stream/rectangle) from yanc-buffer
;;;   Alt-Ins   - Paste region (line/stream/rectangle) from register
;;;   Grey +    - Copy region (line/stream/rectangle) to yanc-buffer
;;;   Alt-Grey+ - Copy region (line/stream/rectangle) to register
;;;   Grey -    - Cut region (line/stream/rectangle) to yanc-buffer
;;;   Alt-Grey- - Cut region (line/stream/rectangle) to register
;;;  

;;; KNOWN BUGS
;;; ----------
;;;    Keyboard binding tested only on sun4 keyboard.
;;; But I think it is quite easy to port it to another keyboard.
;;;
;;;    During keyboard macro definition Cntrl-G key does not
;;; processed appropriately.

;;; LCD Archive Entry:
;;; brief|Alex Rezinsky|alexr@msil.sps.mot.com|
;;; Brief editor emulation|
;;; 07-Feb-1995|1.1|~/modes/brief.el.Z|

;;; HISTORY
;;; -------
;;; v1.0 September 8 1993  Alex Rezinsky
;;;   First release.

;;; THANKS TO
;;; ---------
;;; I borrowed a few pieces of code for marking region. 
;;; Excuse me, but I forgot where. Anyway thanks to author
;;; of this code!

;;;
;;; KEYBOARD BINDING
;;; ----------------
;;;
;;; You available to change this part to your own keyboard binding

(if (string=  window-system "x")
  (progn

    ;; ====================  SEPARATE X-WINDOW  ================================
    ;; =========================================================================

    ;; Mouse binding
    (global-set-key [vertical-scroll-bar S-mouse-2] 'mouse-split-window-vertically)
    (global-set-key [vertical-line       S-mouse-2] 'mouse-split-window-vertically)
    (global-set-key [mode-line           S-mouse-2] 'mouse-split-window-horizontally)

    ;; Alt key binding
    (global-set-key [?\A-1] '(lambda() (interactive nil) (goto-bookmark 1)))
    (global-set-key [?\A-2] '(lambda() (interactive nil) (goto-bookmark 2)))
    (global-set-key [?\A-3] '(lambda() (interactive nil) (goto-bookmark 3)))
    (global-set-key [?\A-4] '(lambda() (interactive nil) (goto-bookmark 4)))
    (global-set-key [?\A-5] '(lambda() (interactive nil) (goto-bookmark 5)))
    (global-set-key [?\A-6] '(lambda() (interactive nil) (goto-bookmark 6)))
    (global-set-key [?\A-7] '(lambda() (interactive nil) (goto-bookmark 7)))
    (global-set-key [?\A-8] '(lambda() (interactive nil) (goto-bookmark 8)))
    (global-set-key [?\A-9] '(lambda() (interactive nil) (goto-bookmark 9)))
    (global-set-key [?\A-b] 'buffer-menu)
    (global-set-key [?\A-c] 'mark-c)
    (global-set-key [?\A-e] 'find-file)
    (global-set-key [?\A-f] '(lambda() (interactive nil) (message (buffer-file-name nil))))
    (global-set-key [?\A-g] 'goto-line)
    (global-set-key [?\A-i] 'overwrite-mode)
    (global-set-key [?\A-j] 'goto-book)
    (global-set-key [?\A-l] 'mark-l)
    (global-set-key [?\A-m] 'mark-m)
    (global-set-key [?\A-n] 'bury-buffer)
    (global-set-key [?\A-t] 'invert-truncation)
    (global-set-key [?\A-q] 'quoted-insert)
    (global-set-key [?\A-s] 'save-buffer)
    (global-set-key [?\A-w] 'write-file)
    (global-set-key [?\A-x] 'save-buffers-kill-emacs)

    ;; Alt-Meta key binding
    (global-set-key [?\A-\M-1]    '(lambda() (interactive nil) (set-bookmark 1)))
    (global-set-key [?\A-\M-2]    '(lambda() (interactive nil) (set-bookmark 2)))
    (global-set-key [?\A-\M-3]    '(lambda() (interactive nil) (set-bookmark 3)))
    (global-set-key [?\A-\M-4]    '(lambda() (interactive nil) (set-bookmark 4)))
    (global-set-key [?\A-\M-5]    '(lambda() (interactive nil) (set-bookmark 5)))
    (global-set-key [?\A-\M-6]    '(lambda() (interactive nil) (set-bookmark 6)))
    (global-set-key [?\A-\M-7]    '(lambda() (interactive nil) (set-bookmark 7)))
    (global-set-key [?\A-\M-8]    '(lambda() (interactive nil) (set-bookmark 8)))
    (global-set-key [?\A-\M-9]    '(lambda() (interactive nil) (set-bookmark 9)))
    (global-set-key [?\A-\M-e]    'find-file-other-frame)
    (global-set-key [?\A-\M-x]    '(lambda() (interactive nil) (save-some-buffers t) (kill-emacs)))
    (global-set-key [?\A-\M-s]    'desktop-save)

    ;; FUNCTIONAL KEY BINDING
    (global-set-key [f1]         'other-window)
    (global-set-key [A-f1]       '(lambda() (interactive nil) (raise-frame (next-frame))))

    (global-set-key [f2 down]    'enlarge-window)
    (global-set-key [f2 left]    'shrink-window-horizontally)
    (global-set-key [f2 right]   'enlarge-window-horizontally)
    (global-set-key [f2 up]      'shrink-window)
    (global-set-key [f3 down]    'split-window-vertically)
    (global-set-key [f3 right]   'split-window-horizontally)

    (global-set-key [f4]         'delete-other-windows)
    (global-set-key [A-f4]       'delete-window)
    (global-set-key [A-M-f4]     'delete-frame)

    (global-set-key [f5]         'mysearch)
    (global-set-key [S-f5]       'myresearch)
    (global-set-key [A-f5]       'mysearch-back)
    (global-set-key [A-S-f5]     'myresearch-back)
    (global-set-key [f6]         'myquery-replace)
    (global-set-key [S-f6]       'myrequery-replace)
    (global-set-key [C-f5]       'case-sensitive-search-toggle)
    (global-set-key [C-f6]       'regexp-search-toggle)

    (global-set-key [f7]         'define-kbd-macro)
    (global-set-key [A-f7]       'read-kbd-macro)
    (global-set-key [f8]         'execut-kbd-macro)
    (global-set-key [A-f8]       'save-kbd-macro)

    (global-set-key [f9]         'load-file)
    (global-set-key [A-f9]       'load-library)
    (global-set-key [f10]        'shell-command)
    (global-set-key [f11]        'mark-c)                      ; F11 or Stop
    (global-set-key [f12]        'repeat-complex-command)      ; F12 or Again
    (global-set-key [f13]        'mark-m)                      ; Props
    (global-set-key [f14]        'undo)                        ; Undo
    (global-set-key [f24]        'kill-buffer)                 ; R4
    (global-set-key [help help]  'help-for-help)               ; Help Help
    (global-set-key [help]       'help-command)                ; Help

    ;; Keypad binding
    (global-set-key [insert]        'insert-key)               ; Ins
    (global-set-key [f18]           'insert-key)               ;   and Paste
    (global-set-key [A-insert]      'insert-register)          ; Alt Ins
    (global-set-key [f33]           'end)                      ; End
    (global-set-key [f35]           'pgdn)                     ; PgDn
    (global-set-key [f27]           'home)                     ; Home
    (global-set-key [f29]           'pgup)                     ; PgUp
    (global-set-key [kp-add]        'grey-plus)                ; Grey +
    (global-set-key [f16]           'grey-plus)                ;   and Copy
    (global-set-key [A-kp-add]      'grey-plus-to-reg)         ; Alt Grey +
    (global-set-key [delete]        'delete-backward-char)     ; Delete
    (global-set-key [backspace]     'backward-char)            ; BackSpace
    (global-set-key [kp-decimal]    'delete-char)              ; Del
    (global-set-key [kp-subtract]   'grey-minus)               ; Grey -
    (global-set-key [f20]           'grey-minus)               ;   and Cut
    (global-set-key [A-kp-subtract] 'grey-minus-to-reg)        ; Alt Grey -
  
    ;; =========================================================================
    ;; ====================  END OF SEPARATE X-WINDOW  =========================

  )
  (progn

    ;; ====================  XTERM WINDOW  =====================================
    ;; =========================================================================

    (setq F1  "\e[11~")
    (setq F2  "\e[12~")
    (setq F3  "\e[13~")
    (setq F4  "\e[14~")
    (setq F5  "\e[15~")
    (setq F6  "\e[17~")
    (setq F7  "\e[18~")
    (setq F8  "\e[19~")
    (setq F9  "\e[20~")
    (setq F10 "\e[21~")
    (setq F11 "\e[23~")
    (setq F12 "\e[24~")
    (setq F13 "\e[25~")
    (setq F14 "\e[26~")

    (setq R1  "\e[208z")
    (setq R2  "\e[209z")
    (setq R3  "\e[210z")
    (setq R4  "\e[211z")
    (setq R5  "\e[212z")
    (setq R6  "\e[213z")
    (setq R7  "\e[214z")
    (setq R9  "\e[216z")
    (setq R11 "\e[218z")
    (setq R13 "\e[220z")
    (setq R15 "\e[222z")

    (setq Up  "\eOA")
    (setq Lf  "\eOD")
    (setq Rg  "\eOC")
    (setq Dn  "\eOB")

    (setq Help  "\e[224z")
    (setq Ins   "\e[32~")
    (setq Del   "\e[223z")
    (setq G+    "\e[29~")
    (setq G-    "\e[34~")

    ;; LOGICAL DEFINITIONS
    (setq Al R1)
    (setq Sh R2)
    (setq Ct R5)
    (setq ExMac F8)

    ;; UNSETTING
    (global-unset-key "\e[")
    (global-unset-key "\e[1")
    (global-unset-key "\e[2")
    (global-unset-key F2)
    (global-unset-key F3)
    (global-unset-key Al)
    (global-unset-key Sh)
    (global-unset-key Ct)

    ;; Alt key binding
    (global-set-key (concat Al "1")   '(lambda() (interactive nil) (goto-bookmark 1)))
    (global-set-key (concat Al "2")   '(lambda() (interactive nil) (goto-bookmark 2)))
    (global-set-key (concat Al "3")   '(lambda() (interactive nil) (goto-bookmark 3)))
    (global-set-key (concat Al "4")   '(lambda() (interactive nil) (goto-bookmark 4)))
    (global-set-key (concat Al "5")   '(lambda() (interactive nil) (goto-bookmark 5)))
    (global-set-key (concat Al "6")   '(lambda() (interactive nil) (goto-bookmark 6)))
    (global-set-key (concat Al "7")   '(lambda() (interactive nil) (goto-bookmark 7)))
    (global-set-key (concat Al "8")   '(lambda() (interactive nil) (goto-bookmark 8)))
    (global-set-key (concat Al "9")   '(lambda() (interactive nil) (goto-bookmark 9)))
    (global-set-key (concat Al "b")   'buffer-menu)
    (global-set-key (concat Al "c")   'mark-c)
    (global-set-key (concat Al "e")   'find-file)
    (global-set-key (concat Al "f")   '(lambda() (interactive nil) (message (buffer-file-name nil))))
    (global-set-key (concat Al "g")   'goto-line)
    (global-set-key (concat Al "i")   'overwrite-mode)
    (global-set-key (concat Al "j")   'goto-book)
    (global-set-key (concat Al "l")   'mark-l)
    (global-set-key (concat Al "m")   'mark-m)
    (global-set-key (concat Al "n")   'bury-buffer)
    (global-set-key (concat Al "t")   'invert-truncation)
    (global-set-key (concat Al "q")   'quoted-insert)
    (global-set-key (concat Al "s")   'save-buffer)
    (global-set-key (concat Al "w")   'write-file)
    (global-set-key (concat Al "x")   'save-buffers-kill-emacs)

    ;; Alt-Meta key binding
    (global-set-key (concat Al "\e1") '(lambda() (interactive nil) (set-bookmark 1)))
    (global-set-key (concat Al "\e2") '(lambda() (interactive nil) (set-bookmark 2)))
    (global-set-key (concat Al "\e3") '(lambda() (interactive nil) (set-bookmark 3)))
    (global-set-key (concat Al "\e4") '(lambda() (interactive nil) (set-bookmark 4)))
    (global-set-key (concat Al "\e5") '(lambda() (interactive nil) (set-bookmark 5)))
    (global-set-key (concat Al "\e6") '(lambda() (interactive nil) (set-bookmark 6)))
    (global-set-key (concat Al "\e7") '(lambda() (interactive nil) (set-bookmark 7)))
    (global-set-key (concat Al "\e8") '(lambda() (interactive nil) (set-bookmark 8)))
    (global-set-key (concat Al "\e9") '(lambda() (interactive nil) (set-bookmark 9)))
    ;;(global-set-key (concat Al "\ee") 'find-file-other-frame)
    (global-set-key (concat Al "\ex") '(lambda() (interactive nil) (save-some-buffers t) (kill-emacs)))
    (global-set-key (concat Al "\es") 'desktop-save)

    ;; FUNCTIONAL KEY BINDING
    (global-set-key F1                 'other-window)

    (global-set-key (concat F2 Dn)     'enlarge-window)
    (global-set-key (concat F2 Lf)     'shrink-window-horizontally)
    (global-set-key (concat F2 Rg)     'enlarge-window-horizontally)
    (global-set-key (concat F2 Up)     'shrink-window)
    (global-set-key (concat F3 Dn)     'split-window-vertically)
    (global-set-key (concat F3 Rg)     'split-window-horizontally)

    (global-set-key F4                 'delete-other-windows)
    (global-set-key (concat Al F4)     'delete-window)

    (global-set-key F5                 'mysearch)
    (global-set-key (concat Sh F5)     'myresearch)
    (global-set-key (concat Al F5)     'mysearch-back)
    (global-set-key (concat Al Sh F5)  'myresearch-back)
    (global-set-key F6                 'myquery-replace)
    (global-set-key (concat Sh F6)     'myrequery-replace)
    (global-set-key (concat Ct F5)     'case-sensitive-search-toggle)
    (global-set-key (concat Ct F6)     'regexp-search-toggle)

    (global-set-key F7                 'define-kbd-macro)
    (global-set-key (concat Al F7)     'read-kbd-macro)
    (global-set-key F8                 'execut-kbd-macro)
    (global-set-key (concat Al F8)     'save-kbd-macro)

    (global-set-key F9                 'load-file)
    (global-set-key (concat Al F9)     'load-library)

    (global-set-key F10                'shell-command)
    (global-set-key F11                'mark-c)                 ; F11 or Stop
    (global-set-key F12                'repeat-complex-command) ; F12 or Again
    (global-set-key F13                'mark-m)                 ; Props
    (global-set-key F14                'undo)                   ; Undo
    (global-set-key R4                 'kill-buffer)            ; R4
    (global-set-key (concat Help Help) 'help-for-help)          ; Help Help
    (global-set-key Help               'help-command)           ; Help

    ;; Keypad binding
    (global-set-key Ins                'insert-key)             ; Ins
    (global-set-key (concat Al Ins)    'insert-register)        ; Alt GIns
    (global-set-key R13                'end)                    ; End
    (global-set-key R15                'pgdn)                   ; PgDn
    (global-set-key R7                 'home)                   ; Home
    (global-set-key R9                 'pgup)                   ; PgUp
    (global-set-key G+                 'grey-plus)              ; Grey +
    (global-set-key (concat Al G+)     'grey-plus-to-reg)       ; Alt Grey +
    (global-set-key "\C-?"             'delete-backward-char)   ; Delete
    (global-set-key "\C-h"             'backward-char)          ; BackSpace
    (global-set-key Del                'delete-char)            ; Del
    (global-set-key G-                 'grey-minus)             ; Grey -
    (global-set-key (concat Al G-)     'grey-minus-to-reg)      ; Alt Grey -

    ;; =========================================================================
    ;; ====================  END OF XTERM WINDOW  ==============================

  )
)

;;;
;;; CODE, NOTHING TO CUSTOMIZE BELOW HERE
;;; -------------------------------------
;;;

;======== INITIALIZATION OF VARIABLES ================================
;=====================================================================

; CUSTOMIZABLE VARIABLES
(defvar replace-history-list-save 10)
(defvar save-environment-filename "~/.emacs.save")
(defvar search-history-list-save  10)
(defvar search-replace-verbose    nil)
(defvar search-regexp-enabled     nil)
(defvar x-show-stack-reverse      nil)
(defvar window-hscroll-step       10)

; HOOKS
(add-hook 'kill-emacs-hook 'save-emacs-environment)

; ADDITIONS TO EXISTING KEY MAPS
(define-key  minibuffer-local-completion-map [up]   'previous-history-element)
(define-key  minibuffer-local-completion-map [down] 'next-history-element)
(define-key  minibuffer-local-map [up]		    'previous-history-element)
(define-key  minibuffer-local-map [down]	    'next-history-element)
(define-key  minibuffer-local-ns-map [up]	    'previous-history-element)
(define-key  minibuffer-local-ns-map [down]	    'next-history-element)
(define-key  minibuffer-local-isearch-map [up]	    'previous-history-element)
(define-key  minibuffer-local-isearch-map [down]    'next-history-element)
(define-key  minibuffer-local-must-match-map [up]   'previous-history-element)
(define-key  minibuffer-local-must-match-map [down] 'next-history-element)
(define-key  read-expression-map [up]		    'previous-history-element)
(define-key  read-expression-map [down]		    'next-history-element)



; NEW KEY MAP FOR SEARCH COMMAND
(defvar repeat-search-command-map (copy-alist minibuffer-local-map))
(define-key repeat-search-command-map  [down]      'next-stack)
(define-key repeat-search-command-map  [up]        'previous-stack)
(define-key repeat-search-command-map  [S-mouse-1] 'x-show-stack)
(define-key repeat-search-command-map  [S-mouse-2] 'x-show-stack)
(define-key repeat-search-command-map  [S-mouse-3] 'x-show-stack)

; WORK VARIABLES
(setq keyb-buffer-status        nil)
(setq keyb-macro-defined        nil)
(setq last-last-command         nil)
(setq mark-list                 nil)
(setq mark-max-count            10)
(setq reg-flag                  nil)
(setq reg-in-scrap-flag         nil)
(setq reg-point                 nil)
(setq replace-from-history-list nil)
(setq replace-to-history-list   nil)
(setq search-history-list       nil)


;======== INITIALIZATION OF MY FUNCTIONS =============================
;=====================================================================


;--------------------------------------------------------------------------
;                                             MISCELANEOUS SIMPLE FUNCTIONS
(defun dummy-function ()
  "Dummy function. Do nothing."
  (interactive nil)
)


(defun check-in-window ()
  "Check if current point in window, and perform horisontal scrolling else."
  (interactive nil)
  (condition-case info
    (if truncate-lines
      (cond
       ((< (current-column) (window-hscroll))
        (scroll-right (- (window-hscroll) (current-column)))
       )
       ((> (+ (current-column) 2) (+ (window-hscroll) (window-width)))
        (scroll-left (+ (- (current-column) (+ (window-hscroll) (window-width))) 3))
       )
      )
    )
    (error
     (ding)
     (remove-hook 'post-command-hook 'check-in-window)
     (message "check-in-window error: %s" info)
    )
  )
)
(add-hook 'post-command-hook 'check-in-window)

(defun invert-truncation ()
  "Invert truncation-lines variable."
  (interactive nil)
  (cond
    ((eq truncate-lines t)
      (setq truncate-lines nil)
      (message "Truncate lines mode is now OFF.")
    )
    (t
      (setq truncate-lines t)
      (message "Truncate lines mode is now ON.")
    )
  )
  (redraw-display)
)


;--------------------------------------------------------------------------
;                                                     USER STACK OPERATIONS
;
; Usage of this functions (previous-stack/next-stack/x-show-stack) :
;
;(define-key <something>-command-map [up]              'previous-stack)
;(define-key <something>-command-map [down]            'next-stack)
;(define-key <something>-command-map <any_mouse_event> 'x-show-stack)
;
; ......
;    (setq stack-number 0)
;    (setq your-current-stack (cons "" <list_which_represent_your_stack>))
;    (setq <something> (read-from-minibuffer 
;                        "<Your_prompt>" 
;                        "" 
;                        <something>-command-map
;                      )
(defun previous-stack ()
  "Insert next element of your stack into the minibuffer"
  (interactive nil)
  (let ((cur-str (nth (1+ stack-number) your-current-stack)))
    (if cur-str
      (progn
        (setq stack-number (1+ stack-number))
        (erase-buffer)
        (insert cur-str)
        (goto-char (point-min))
      )
      (error "No preceeding item in your stack")
    )
  )
)

(defun next-stack ()
  "Insert previous element of your stack into the minibuffer"
  (interactive nil)
  (let ((cur-str (nth (1- stack-number) your-current-stack)))
    (if (and cur-str (> stack-number 0))
      (progn
        (setq stack-number (1- stack-number))
        (erase-buffer)
        (insert cur-str)
        (goto-char (point-min))
      )
      (error "No following item in your stack")
    )
  )
)

(defun x-show-stack (click)
  "Select item in user stack by x-menu"
  (interactive "e")
  (if (string= window-system "x")
    (let ((pos click) (x-user-menu-ex nil) (x-user-menu-ed nil)
          (response nil) (i 0)
         )
      (if x-show-stack-reverse
        (setq i (1- (length your-current-stack)))
        (setq i 1)
      )
      (while (if x-show-stack-reverse 
               (> i 0) 
               (< i (length your-current-stack))
             )
        (setq x-user-menu-ex (append x-user-menu-ex 
                                     (list (cons (nth i your-current-stack)
                                                 (concat "X" (int-to-string i))
                                           )
                                     )
                             )
        )
        (setq x-user-menu-ed (append x-user-menu-ed 
                                     (list (cons (nth i your-current-stack)
                                                 (concat "E" (int-to-string i))
                                           )
                                     )
                             )
        )
        (if x-show-stack-reverse 
          (setq i (1- i)) 
          (setq i (1+ i))
        )
      )
      (setq x-user-menu (list "User selection"
                              (cons "Execute immediately" x-user-menu-ex)
                              (cons " Get to minibuffer " x-user-menu-ed)
                        )
      )
      (setq response (x-popup-menu pos x-user-menu))
      (if response
        (progn
          (setq stack-number (string-to-int (substring response 1)))
          (erase-buffer)
          (insert (nth stack-number your-current-stack))
          (cond 
            ((string= (substring response 0 1) "E")
             (goto-char (point-min))
            )
            ((string= (substring response 0 1) "X")
             (exit-minibuffer)
            )
            (t
             (message (format 
                       "Error in x-user-menu building. Cannot be \"%s\"."
                       response
                      )
             )
            )
          )
        )
      )
    )
  )
)


;--------------------------------------------------------------------------
;                                                 SAVING/TRUNCATING BUFFERS
(defun save-user-stack (user-stack user-stack-name user-stack-save-deep)
  (if (<= (length user-stack) user-stack-save-deep)
    (insert (format "(setq %s '%s)\n" user-stack-name
                    (prin1-to-string user-stack)))
    (let ((i 0)(user-stack-temp nil))
      (while (< i user-stack-save-deep)
        (setq user-stack-temp (cons (nth i user-stack) user-stack-temp))
        (setq i (1+ i))
      )
      (insert (format "(setq %s '%s)\n" user-stack-name
                      (prin1-to-string (reverse user-stack-temp))))
    )
  )
)

(defun save-emacs-environment ()
  "Saving of all emacs environment to to file."
  (save-excursion
    (set-buffer (get-buffer-create "*emacs-environment*"))
    (let ((p1 (point)))

      ; Saving of search context
      (save-user-stack search-history-list
                       "search-history-list" search-history-list-save)

      ; Saving of replace context
      (save-user-stack replace-from-history-list
                       "replace-from-history-list" replace-history-list-save)
      (save-user-stack replace-to-history-list
                       "replace-to-history-list" replace-history-list-save)

      ; Saving of bookmark list
      (if mark-list
        (let ((i 0)(el nil))
          (insert "(setq mark-list\n  (list\n")
          (while (< i mark-max-count)
            (setq el (assoc i mark-list))
            (if el
              (insert (format "    %s\n" (prin1-to-string 
                                           (list 'list 
                                           (car el) 
                                           (car (cdr el))
                                           (car (cdr (cdr el)))
                                           (car (cdr (cdr (cdr el))))
                                           nil))))
            )
            (setq i (1+ i))
          )
          (insert "  )\n)\n")
        )
        (insert "(setq mark-list nil)\n")
      )
      (insert (format "(setq register-alist '%s)\n" 
                      (prin1-to-string register-alist)))
;;;      (insert (format "(setq kill-ring '%s)\n"                             
;;;                      (prin1-to-string kill-ring)))                        
;;;      (insert (format "(setq killed-rectangle '%s)\n"                      
;;;                      (prin1-to-string killed-rectangle)))                 
;;;      (insert (format "(setq kill-ring-yank-pointer '%s)\n"                
;;;                      (prin1-to-string kill-ring-yank-pointer)))           
;;;      (insert (format "(setq reg-in-scrap-flag '%s)\n" reg-in-scrap-flag)) 
      (write-region p1 (point) save-environment-filename)
    )
    (kill-buffer "*emacs-environment*")
  )
)


;--------------------------------------------------------------------------
;                                                                 LINES ...
(defun beg-line-reg-point (pnt)
  (save-excursion
    (goto-char pnt)
    (beginning-of-line)
    (point)
  )
)

(defun end-line-reg-point (pnt)
  (save-excursion
    (goto-char pnt)
    (beginning-of-line)
    (next-line 1)
    (beginning-of-line)
    (point)
  )
)

(defun delete-line ()
  "Delete current line."
  (interactive nil)
  (kill-region (beg-line-reg-point (point)) (end-line-reg-point (point)))
  (message "Simple line cut to scrap")
)

(defun cut-lines (start end)
  "Cut lines region."
  (interactive nil)
  (kill-region (beg-line-reg-point start) (end-line-reg-point end))
)

(defun copy-lines (start end)
  "Copy lines region."
  (interactive nil)
  (copy-region-as-kill (beg-line-reg-point start) (end-line-reg-point end))
)

(defun copy-line ()
  "Copy current line in yanc-buffer."
  (interactive nil)
  (copy-region-as-kill (beg-line-reg-point (point)) 
                       (end-line-reg-point (point)))
  (message "Simple line copied to scrap")
)



;--------------------------------------------------------------------------
;                                                                  HOME/END
(defun home ()
  "Home - begin of line, once more - screen, once more - buffer."
  (interactive nil)
  (cond
    ((and (eq last-command 'home) (eq last-last-command 'home))
     (goto-char (point-min))
    )
    ((eq last-command 'home)
     (move-to-window-line 0)
    )
    (t
     (beginning-of-line)
    )
  )
  (setq last-last-command last-command)
)

(defun end ()
  "End - end of line, once more - screen, once more - buffer."
  (interactive nil)
  (cond
    ((and (eq last-command 'end) (eq last-last-command 'end))
     (goto-char (point-max))
    )
    ((eq last-command 'end)
     (move-to-window-line -1)
     (end-of-line)
    )
    (t
     (end-of-line)
    )
  )
  (setq last-last-command last-command)
)


;--------------------------------------------------------------------------
;                                                        SCROLLING & PAGING
(defun pgdn (&optional ignore)
  "Page down."
  (interactive nil)
  (let ((wc (current-column))
        (wl (1- (count-lines (window-start) (point))))
       )
    (if (eq wc 0)
        (setq wl (1+ wl))
    )
    (setq rpt current-prefix-arg)
    (if (not rpt)
      (setq rpt 1)
    )
    (while (> rpt 0)
       (scroll-up (- (window-height) 2))
       (setq rpt (1- rpt))
    )
    (move-to-window-line wl)
  )
)

(defun pgup (&optional ignore)
  "Page up."
  (interactive nil)
  (let ((wc (current-column))
        (wl (1- (count-lines (window-start) (point))))
       )
    (if (eq wc 0)
        (setq wl (1+ wl))
    )
    (setq rpt current-prefix-arg)
    (if (not rpt)
      (setq rpt 1)
    )
    (while (> rpt 0)
       (scroll-down (- (window-height) 2))
       (setq rpt (1- rpt))
    )
    (move-to-window-line wl)
  )
)


;--------------------------------------------------------------------------
;                                                                 BOOKMARKS
(defun set-bookmark (mrk)
  "Set bookmark routine."
  (interactive nil)
  (setq wc (current-column) wl (count-lines 1 (point)))
    (if (eq wc 0)
	(setq wl (1+ wl))
    )
  (setq found-element (assoc mrk mark-list))
  (cond
    ((and (not (null found-element))
          (car (cdr (cdr (cdr (cdr found-element)))))
     )
      (cond
        ((y-or-n-p (format "Bookmark #%d exist (buffer:\"%s\" L:%d C:%d). Overwrite it ? "
			   (car found-element)
			   (car (cdr (cdr (cdr found-element))))
			   (car (cdr found-element))
			   (1+ (car (cdr (cdr found-element))))
		    )
         )
	  (setq found-element (list mrk wl wc (buffer-name) t))
	  (setq mark-list (cons found-element mark-list))
	  (message "Bookmark #%d dropped in buffer \"%s\" L:%d C:%d."
		   (car found-element)
		   (car (cdr (cdr (cdr found-element))))
		   (car (cdr found-element))
		   (1+ (car (cdr (cdr found-element))))
          )
        )
        (t
	  (message "Bookmark #%d not overwrited" (car found-element))
        )
      )
     )
     (t
       (setq mark-list (cons (list mrk wl wc (buffer-name) t) mark-list))
       (message "Bookmark #%d dropped in buffer \"%s\" L:%d C:%d."
		 mrk (buffer-name) wl (1+ wc))
     )
  )
)

(defun goto-book ()
  "Goto bookmark"
  (interactive nil)
  (message "Goto Bookmark #")
  (goto-bookmark (- (read-char) ?0))
)

(defun goto-bookmark (mrk)
  "Goto bookmark"
  (interactive nil)
  (setq found-element (assoc mrk mark-list))
  (cond
    ((null found-element)
      (message "Bookmark number #%d not exist !!!" mrk)
    )
    (t
      (switch-to-buffer (car (cdr (cdr (cdr found-element)))))
      (set-buffer (car (cdr (cdr (cdr found-element)))))
      (goto-line (car (cdr found-element)))
      (move-to-column (car (cdr (cdr found-element))))
    )
  )
)


;--------------------------------------------------------------------------
;                                                          SEARCH & REPLACE
(defun mysearch (&optional arg)
  "Search forward"
  (interactive nil)
  (if arg
    (setq src-str arg)
    (setq stack-number 0)
    (setq your-current-stack (cons "" search-history-list))
    (setq src-str (read-from-minibuffer 
                   "String to search : " 
                   "" 
                   repeat-search-command-map
                  )
    )
    (setq search-history-list (cons src-str search-history-list))
  )
  (if (if search-regexp-enabled
        (re-search-forward src-str (point-max) t 1)
        (search-forward src-str (point-max) t 1)
      )
    (progn
      (if search-replace-verbose
        (message "<%s> found in line = %d & column = %d"
                 src-str
                 (if (bolp)
                   (1+ (count-lines 1 (point)))
                   (count-lines 1 (point))
                 )
                 (1+ (- (current-column) (length src-str)))
        )
        (message "<%s> found." src-str)
      )
      (goto-char (match-beginning 0))
      t
    )
    (message "<%s> not found." src-str)
    nil
  )
)

(defun mysearch-back (&optional arg)
  "Search backward"
  (interactive nil)
  (if arg
    (setq src-str arg)
    (setq stack-number 0)
    (setq your-current-stack (cons "" search-history-list))
    (setq src-str (read-from-minibuffer 
                   "String to backsearch : " 
                   "" 
                   repeat-search-command-map
                  )
    )
    (setq search-history-list (cons src-str search-history-list))
  )
  (if (if search-regexp-enabled
        (re-search-backward src-str 0 t 1)
        (search-backward src-str 0 t 1)
      )
    (progn
      (if search-replace-verbose
        (message "<%s> found in line = %d & column = %d"
                 src-str
                 (if (bolp)
                   (1+ (count-lines 1 (point)))
                   (count-lines 1 (point))
                 )
                 (1+ (current-column) )
        )
        (message "<%s> found." src-str)
      )
      t
    )
    (message "<%s> not found." src-str)
    nil
  )
)

(defun myresearch ()
  "Research forward"
  (interactive nil)
  (cond ((eq (car search-history-list) nil)
	 (message "Nothing to research")
        )
	(t (goto-char (1+ (point)))
	   (if (eq (mysearch (car search-history-list)) nil)
	     (goto-char (1- (point)))
           )
        )
  )
)

(defun myresearch-back ()
  "Research backward"
  (interactive nil)
  (cond ((eq (car search-history-list) nil)
	 (message "Nothing to research")
        )
	(t (mysearch-back (car search-history-list))
        )
  )
)

(defun myquery-replace ()
  "My query replace."
  (interactive nil)

  (setq stack-number 0)
  (setq your-current-stack (cons "" replace-from-history-list))
  (setq from-string (read-from-minibuffer
                     "Query replace: "
                     "" 
                     repeat-search-command-map
                    )
  )
  (setq replace-from-history-list (cons from-string replace-from-history-list))

  (setq your-current-stack (cons "" replace-to-history-list))
  (setq to-string (read-from-minibuffer
                   (concat "Query replace <" from-string "> with : ")
                   (nth stack-number your-current-stack)
                   repeat-search-command-map
                  )
  )
  (setq replace-to-history-list (cons to-string replace-to-history-list))

  (perform-replace from-string to-string t search-regexp-enabled nil)
  (message "Done")
)

(defun myrequery-replace ()
  "My re- query replace."
  (interactive nil)
  (cond
    ((or (null replace-from-history-list) (null replace-to-history-list))
      (message "Nothing to re-replace")
    )
    (t
      (perform-replace (car replace-from-history-list) 
                       (car replace-to-history-list) t search-regexp-enabled nil)
      (message "Done")
    )
  )
)

(defun case-sensitive-search-toggle ()
  "Toggle case sensitivity of search."
  (interactive nil)
  (if (setq case-fold-search (not case-fold-search))
      (message "Case unsensitive search.")
    (message "Case sensitive search.")
  )
)

(defun regexp-search-toggle ()
  "Regular expression search toggle."
  (interactive nil)
  (if (setq search-regexp-enabled (not search-regexp-enabled))
      (message "Regular expression search/replace enabled.")
    (message "Regular expression search/replace disabled.")
  )
)


;--------------------------------------------------------------------------
;                                                    CUT/COPY/PASTE REGIONS
(defun mark-c ()
  "Mark rectangle region."
  (interactive nil)
  (mark-common 'c)
  (message "Left upper corner of column (rectangle) region marked")
)

(defun mark-l ()
  "Mark line region."
  (interactive nil)
  (mark-common 'l)
  (message "Line region marked")
)

(defun mark-m ()
  "Mark stream region"
  (interactive nil)
  (mark-common 'm)
  (message "Stream region marked")
)

(defun mark-common (new-reg-flag)
  "Mark stream/line/rectangle region common routine"
  (set-mark-command nil)
  (if reg-flag
    (remove-mark-hook)
  )
  (setq reg-point (point))
  (setq reg-flag new-reg-flag)
  (install-mark-hook)
)

(defun grey-minus ()
  "Perform functions of Grey Minus in Brief style."
  (interactive nil)
  (cond
    ((eq reg-flag 'c)
      (kill-rectangle reg-point (point))
      (setq reg-in-scrap-flag 'c)
      (remove-mark-hook)
      (message "Rectangle region cut to scrap")
    )
    ((eq reg-flag 'm)
      (kill-region reg-point (point))
      (setq reg-in-scrap-flag 'm)
      (remove-mark-hook)
      (message "Stream region cut to scrap")
    )
    ((eq reg-flag 'l)
      (if (< (point) reg-point)
	  (cut-lines (point) reg-point)
	  (cut-lines reg-point (point))
      )
      (setq reg-in-scrap-flag 'l)
      (remove-mark-hook)
      (message "Line region cut to scrap")
    )
    (t
      (delete-line)
      (setq reg-in-scrap-flag 'l)
    )
  )
  (setq reg-flag nil)
)

(defun grey-minus-to-reg (char)
  "Perform functions of Grey Minus in Brief style (to register)."
  (interactive "cCut region to register:")
  (cond
    ((eq reg-flag 'c)
      (copy-rectangle-to-register char reg-point (point) t)
      (setq reg-in-scrap-flag 'c)
      (remove-mark-hook)
      (message (format "Rectangle region cut to register %c" char))
    )
    ((eq reg-flag 'm)
      (copy-to-register char reg-point (point) t)
      (setq reg-in-scrap-flag 'm)
      (remove-mark-hook)
      (message (format "Stream region cut to register %c" char))
    )
    ((eq reg-flag 'l)
      (if (< (point) reg-point)
          (copy-to-register char 
                            (beg-line-reg-point (point))
                            (end-line-reg-point reg-point) t)
          (copy-to-register char 
                            (beg-line-reg-point reg-point)
                            (end-line-reg-point (point)) t)
      )
      (setq reg-in-scrap-flag 'l)
      (remove-mark-hook)
      (message (format "Line region cut to register %c" char))
    )
    (t
      (copy-to-register char
                        (beg-line-reg-point (point))
                        (end-line-reg-point (point)) t)

      (setq reg-in-scrap-flag 'l)
      (message (format "Simple line cut to register %c" char))
    )
  )
  (setq reg-flag nil)
)

(defun grey-plus ()
  "Perform functions of Grey Plus in Brief style."
  (interactive nil)
  (cond
    ((eq reg-flag 'c)
      (setq killed-rectangle (extract-rectangle reg-point (point)))
      (setq reg-in-scrap-flag 'c)
      (remove-mark-hook)
      (message "Rectangle region copied to scrap")
    )
    ((eq reg-flag 'm)
      (copy-region-as-kill reg-point (point))
      (setq reg-in-scrap-flag 'm)
      (remove-mark-hook)
      (message "Stream region copied to scrap")
    )
    ((eq reg-flag 'l)
      (if (< (point) reg-point)
	  (copy-lines (point) reg-point)
	  (copy-lines reg-point (point))
      )
      (setq reg-in-scrap-flag 'l)
      (remove-mark-hook)
      (message "Line region copied to scrap")
    )
    (t
      (copy-line)
      (setq reg-in-scrap-flag 'l)
    )
  )
  (setq reg-flag nil)
)

(defun grey-plus-to-reg (char)
  "Perform functions of Grey Plus in Brief style (to register)."
  (interactive "cCopy region to register:")
  (cond
    ((eq reg-flag 'c)
      (copy-rectangle-to-register char reg-point (point))
      (setq reg-in-scrap-flag 'c)
      (remove-mark-hook)
      (message (format "Rectangle region copy to register %c" char))
    )
    ((eq reg-flag 'm)
      (copy-to-register char reg-point (point))
      (setq reg-in-scrap-flag 'm)
      (remove-mark-hook)
      (message (format "Stream region copy to register %c" char))
    )
    ((eq reg-flag 'l)
      (if (< (point) reg-point)
          (copy-to-register char 
                            (beg-line-reg-point (point))
                            (end-line-reg-point reg-point))
          (copy-to-register char 
                            (beg-line-reg-point reg-point)
                            (end-line-reg-point (point)))
      )
      (setq reg-in-scrap-flag 'l)
      (remove-mark-hook)
      (message (format "Line region copy to register %c" char))
    )
    (t
      (copy-to-register char
                        (beg-line-reg-point (point))
                        (end-line-reg-point (point)))

      (setq reg-in-scrap-flag 'l)
      (message (format "Simple line copy to register %c" char))
    )
  )
  (setq reg-flag nil)
)

(defun insert-key ()
  "Perform functions of Ins in Brief style."
  (interactive nil)
  (cond
    ((eq reg-in-scrap-flag 'c)
      (yank-rectangle)
      (message "Rectangle region inserted")
    )
    ((eq reg-in-scrap-flag 'm)
      (yank)
      (message "Stream region inserted")
    )
    ((eq reg-in-scrap-flag 'l)
      (save-excursion
	(beginning-of-line)
	(yank)
      )
      (message "Line region inserted")
    )
    (t
      (message "Nothing to insert")
    )
  )
)

(defun install-mark-hook ()
  (setq rm-overlay-list nil)
  (setq rm-force (eq major-mode 'picture-mode))
  (setq old-transient-mark-mode transient-mark-mode)
  (setq old-ctrl-g (key-binding "\C-g"))
  (local-set-key "\C-g" '(lambda() 
                           (interactive nil) 
                           (remove-mark-hook) 
                           (funcall old-ctrl-g)
                         )
  )
  (setq transient-mark-mode nil)
  (add-hook 'post-command-hook 'mark-post-command-hook)
)

(defun remove-mark-hook ()
  (remove-hook 'post-command-hook 'mark-post-command-hook)
  (setq transient-mark-mode old-transient-mark-mode)
  (local-set-key "\C-g" old-ctrl-g)
  (mapcar (function delete-overlay) rm-overlay-list)
  (and transient-mark-mode mark-active (deactivate-mark))
)

(defun mark-post-command-hook ()
  (condition-case info
    (save-excursion
      (cond
       ((eq reg-flag 'm)
        (mark-piece reg-point (point))
       )
       ((eq reg-flag 'l)
        (mark-piece (beg-line-reg-point reg-point) 
                    (end-line-reg-point (point)))
       )
       ((eq reg-flag 'c)
        ;; Calculate the rectangular region represented by point and
        ;; reg-point, putting beg in the north-west corner and end in
        ;; the south-east corner.
        (let ((beg reg-point) (end (point)))
          (and (> beg end) 
               (setq beg (prog1 end (setq end beg)))     ; beg <-> end
          )
          (let ((beg-col (save-excursion (goto-char beg) (current-column)))
                (end-col (save-excursion (goto-char end) (current-column)))
               )
            (and (> beg-col end-col)
                 (setq beg-col (prog1 end-col (setq end-col beg-col))
                       beg (save-excursion
                             (goto-char beg)
                             (move-to-column beg-col rm-force)
                             (point)
                           )
                       end (save-excursion
                             (goto-char end)
                             (move-to-column end-col rm-force)
                             (point)
                           )
                 )
            )
            (let ((old rm-overlay-list) (new nil) overlay)
              ;; Iterate over each line within the rectangle.
              (goto-char beg)
              (while (< (point) end)
                (let ((beg (point))
                      (end (progn
                             (move-to-column end-col rm-force)
                             (point)
                           )
                      )
                     )
                  ;; Trim old leading overlays.
                  (while (and old
                              (setq overlay (car old))
                              (< (overlay-start overlay) beg)
                              (/= (overlay-end overlay) end)
                         )
                    (delete-overlay overlay)
                    (setq old (cdr old))
                  )
                  ;; Reuse an overlay if possible, otherwise create one.
                  (if (and old
                           (setq overlay (car old))
                           (or (= (overlay-start overlay) beg)
                               (= (overlay-end overlay) end)
                           )
                      )
                    (progn
                      (move-overlay overlay beg end)
                      (setq new (cons overlay new) old (cdr old))
                    )
                    (setq overlay (make-overlay beg end))
                    (overlay-put overlay 'face 'region)
                    (setq new (cons overlay new))
                  )
                  (forward-line 1)
                  (or (eobp) (move-to-column beg-col rm-force)))
              )
              ;; Trim old trailing overlays.
              (mapcar (function delete-overlay) old)
              (setq rm-overlay-list (nreverse new))
            )
          )
        )
       )
       (t
        nil
       )
      )
    )
    (error
     (remove-mark-hook)
     (ding)
     (message "mark error: %s" info)
    )
  )
)

(defun mark-piece (beg end)
  (let ((old rm-overlay-list) (new nil) overlay)
    ;; Trim old leading overlays.
    (while (and old
                (setq overlay (car old))
                (< (overlay-start overlay) beg)
                (/= (overlay-end overlay) end)
           )
      (delete-overlay overlay)
      (setq old (cdr old))
    )
    ;; Reuse an overlay if possible, otherwise create one.
    (if (and old
             (setq overlay (car old))
             (or (= (overlay-start overlay) beg)
                 (= (overlay-end overlay) end)
             )
        )
      (progn
        (move-overlay overlay beg end)
        (setq new (cons overlay new) old (cdr old))
      )
      (setq overlay (make-overlay beg end))
      (overlay-put overlay 'face 'region)
      (setq new (cons overlay new))
    )
    (mapcar (function delete-overlay) old)
    (setq rm-overlay-list (nreverse new))
  )
)


;--------------------------------------------------------------------------
;                                                           KEYBOARD MACROS
(defun define-kbd-macro ()
  "Start/End defenition of keyboard macro."
  (interactive nil)
  (if keyb-buffer-status
    (progn
      (end-kbd-macro nil)
      (setq keyb-macro-defined t)
      (setq keyb-buffer-status nil)
    )
    (progn
      (if keyb-macro-defined
          (if (y-or-n-p "Keyboard macro defined. Do you want overwrite it ? ")
            (define-kbd-macro-internal)
          )
        (define-kbd-macro-internal)
      )
    )
  )
)

(defun define-kbd-macro-internal ()
  (if (string= window-system "x")
      (global-set-key [f8]  'dummy-execut-kbd-macro)
    (global-set-key ExMac 'dummy-execut-kbd-macro)
  )
  (start-kbd-macro nil)
  (if (string= window-system "x")
      (global-set-key [f8]  'execut-kbd-macro)
    (global-set-key ExMac 'execut-kbd-macro)
  )
  (setq keyb-buffer-status t)
)

(defun dummy-execut-kbd-macro ()
  "Dummy execution keyboard macro in macro defenition process."
  (interactive nil)
  (setq keyb-buffer-status t)
  (error "Execution keyboard macro during macro definition !!!")
)

(defun execut-kbd-macro ()
  "Execution of keyboard macro."
  (interactive nil)
  (cond (keyb-buffer-status
	 (message "Can not execute keyboard macro while definition it !!!")
	)
	(t
          (if (string= window-system "x")
            (global-set-key [f8]  'dummy-function)
            (global-set-key ExMac 'dummy-function)
          )
	  (setq rpt current-prefix-arg)
	  (if (not rpt)
	    (setq rpt 1)
	  )
	  (while (> rpt 0)
	    (call-last-kbd-macro)
	    (setq rpt (1- rpt))
	  )
          (if (string= window-system "x")
            (global-set-key [f8]  'execut-kbd-macro)
            (global-set-key ExMac 'execut-kbd-macro)
          )
        )
   )
)

(defun read-kbd-macro(filename)
  "Read keyboard macro from file."
  (interactive "fFile name with keyboard macro : ")
  (cond (keyb-macro-defined
	 (cond ((y-or-n-p "Keyboard macro defined. Do you want overwrite it ? ")
		(if (load-file filename)
		    (setq keyb-macro-defined t)
		)
               )
         )
        )
	(t
	 (if (load-file filename)
	     (setq keyb-macro-defined t)
	 )
        )
  )
)

(defun save-kbd-macro ()
  "Save keyboard macro (if defined) to file."
  (interactive nil)
  (cond (keyb-macro-defined
	  (call-interactively 'save-kbd-macro1)
	)
	(t
          (message "Keyboard macro not defined")
	)
  )
)

(defun save-kbd-macro1 (filename)
  "Real saving keyboard macro to file."
  (interactive "FFile name for saving keyboard macro : ")
  (set-buffer (get-buffer-create "*last-keyb-macro*"))
  (setq p1 (point))
  (insert (format "(setq last-kbd-macro %s)" (prin1-to-string last-kbd-macro)))
  (setq p2 (point))
  (write-region p1 p2 filename)
  (kill-buffer "*last-keyb-macro*")
)

;======== Reading of saved emacs environment =========================
;=====================================================================

(and
  (file-exists-p   save-environment-filename)
  (file-readable-p save-environment-filename)
  (load-file       save-environment-filename)
)

;;; end of brief.el
