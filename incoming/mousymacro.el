;;; $Id: mousymacro.el,v 1.12 1997/03/16 13:24:34 queinnec Exp $
;;; Copyright (C) 1994-1997 by C.Queinnec (Polytechnique & INRIA)

;;; LCD Archive Entry:
;;; mousymacro|Christian Queinnec|Christian.Queinnec@inria.fr|
;;; Run macros via simple mouse clicks.|
;;; $Date: 1997/03/16 13:24:34 $|$Revision: 1.12 $|
;;; ~/misc/mousymacro.el.Z|

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; The purpose of this package is to allow to run Emacs functions
;;; with the sole help of the mouse. For instance, when indexing a
;;; book, it is easier to index words by simply clicking (or dragging)
;;; on them rather than moving the dot towards them then running
;;; \C-x\C-e (as one of possible choice).  With MousyMacro, you just
;;; define what clicks mean and then it is up to you to click
;;; wherever you want.  Usually the left button means "run the
;;; currently associated macro", the middle button means "stop
;;; MousyMacro" and the right one allows to choose with a menu the
;;; macro to be associated to the left button as well as other actions
;;; such as Undo! The Left and Middle buttons can be programmed.

;;; Installation and Demonstration:

;;; To use this package, add to your .emacs (taking care of load-path)
;;;            (require 'mousymacro)
;;; MousyMacro is under your fingers with
;;;      \M-x mousy-macro
;;; Click on the right button to see the defaults, then choose the "funny"
;;; item, then click where you want in the buffer and observe the Minibuffer.
;;; Click on the middle button to stop MousyMacro.

;;; BUG:
;;; MousyMacro runs with its own keyboard map, that's why you cannnot run
;;; the last keyboard macro since it must usually be run with the map
;;; that was current when MousyMacro was invoked.

;;; Repository:

;;; Newer versions will be sent to the LCD Archive but may appear earlier
;;; on ftp.inria.fr:INRIA/Projects/icsla/Miscellaneous/mousymacro.el
;;; Other Emacs packages can be found with World Wide Web with URL:
;;;     file://ftp.inria.fr/INRIA/Projects/icsla/WWW/elisp.html

;;; Code:

(if (and window-system 
         (string-match "^\\([0-9]*\\.[0-9]*\\)" emacs-version)
         (>= (car (read-from-string emacs-version 
                                    (match-beginning 1)
                                    (match-end 1) ))
             19.21 ) )
    nil ; this is OK
  (error "Can only run on Emacs 19.21 (or later) with a window system.") )

(defvar mm-default-menu 
      (list "MousyMacro Default Menu"
            (cons "MousyMacro"
                  (list (cons "Undo Last Effect" '(undo))
                        (cons "Exit from MousyMacro"
                              '(mm-exit-mode) )
                        (cons "-------- Set Left --------"
                              'nothing )
                        ;(cons "Last Keyboard Macro" 
                        ;      '(setq mm-current-macro 
                        ;             'call-last-kbd-macro ) )
                        (cons "Index current word" 
                              '(setq mm-current-macro 
                                     'mm-latex-index-current-region ) )
                        (cons "Funny echo word"
                              '(setq mm-current-macro
                                     'mm-funny-echo-word ) )
                        (cons "-------- Set Middle --------"
                              'nothing )
                        ;(cons "Last Keyboard Macro" 
                        ;      '(setq mm-current-alternate-macro 
                        ;             'call-last-kbd-macro ) )
                        (cons "Index current word" 
                              '(setq mm-current-alternate-macro 
                                     'mm-latex-index-current-region ) )
                        ) ) )
  "The default menu that appears under the right button of the mouse.
It allows to change the macro that will be run by MousyMacro with the 
left button of the mouse. The examples of macros below the line in the menu
respectively represents:
    -- The last keyboard macro (see the BUGS section),
    -- A macro to index words under LaTeX,
    -- A (funny) example for experimentation only... Try it first!
" )

(defvar mm-default-cursor x-pointer-pencil
  "This is the cursor to be used when the MousyMacro mode is operating." )

(defvar mm-previous-cursor nil
  "Buffer-specific. Holds the cursor that was current before MousyMacro." )

(defvar mm-menu nil
  "Buffer-specific menu for MousyMacro. It normally appears under the right
button of the mouse and allows to change the current action that the left
button of the mouse invokes." )

(defvar mousymacro-before-hook nil
  "This hook is run when MousyMacro is started. You can use it to install
an appropriate cursor or menu or macro to be run." )

(defvar mousymacro-after-hook nil
  "This hook is run when MousyMacro finishes. You can use it to postprocess
various things." )

(defvar mm-current-macro nil
  "Buffer-specific. Holds the current macro (a function name to be called
interactively) to be run when clicking the left button of the mouse." )

(defvar mm-current-alternate-macro 'mm-exit-mode
  "Buffer-specific. Holds the current alternate macro (a function name to 
be called interactively) to be run when clicking the middle button of the 
mouse." )

(defvar mm-map nil
  "Keymap of the MousyMacro mode." )

(defvar mm-map-active nil
  "Buffer-specific boolean indicating if the keymap of the MousyMacro 
mode is active or not." )

(defun mm-initialize-map ()
  "Set up the minor mode keymap used by MousyMacro."
  (setq mm-map (make-keymap))
  ;; any character aborts MousyMacro.
  (let ((i 1))
    (while (< i 128)
      (define-key mm-map (char-to-string i) 'mm-exit-mode)
      (setq i (+ i 1)) ) )
  ;; leave scrollbar and menu events as they are.
  ;; Only mouse events make MousyMacro react
  (define-key mm-map [mouse-1]      'mm-undefined)
  (define-key mm-map [down-mouse-1] 'mm-run-macro)
  (define-key mm-map [mouse-2]      'mm-undefined)
  (define-key mm-map [down-mouse-2] 'mm-run-alternate-macro)
  (define-key mm-map [down-mouse-3] 'mm-change-parameters)
  ;(define-key mm-map [mouse-movement] 'mm-scroll)
  ;; Only in the current frame
  (define-key mm-map [switch-frame] 'mm-exit-mode)
  ;; install it as a minor mode map
  (let ((km (assq 'mm-map-active minor-mode-map-alist)))
    (if km (setcdr km mm-map)
      (setq minor-mode-map-alist
            (cons (cons 'mm-map-active mm-map)
                  minor-mode-map-alist ) ) ) )
  ;; But if there is a problem, let ^G exit MousyMacro either.
  (define-key mm-map "\C-g" 'mm-exit-mode)
  mm-map )

(defun mousy-macro (&optional cursor menu)
  "Enter MousyMacro mode. Within this mode, mouse clicks invoke operations
on buffer and thus offer an easy way to invoke macros on particular pieces
of text. MousyMacro defines the following keymap: 
\\{mm-map}
Complaints or comments should be emailed to Christian.Queinnec@inria.fr  "
  (interactive "")
  (let ((menu                (or menu mm-default-menu))
        (cursor              (or cursor mm-default-cursor))
        (previous-mode-name  mode-name)
        (previous-cursor     x-pointer-shape)
        (cursor-in-echo-area nil) )
    (or mm-map (mm-initialize-map))
    (mm-set-cursor cursor)
    (make-local-variable 'mm-map-active)
    (or mm-current-macro (setq mm-current-macro 'call-last-kbd-macro))
    (make-local-variable 'mm-menu)
    (setq mm-menu menu)
    (setq mode-name "MousyMacro")
    (push-mark (point))
    (setq mm-map-active t)
    (run-hooks 'mousymacro-before-hook)
    (message "MousyMacro ON!")
    (unwind-protect (recursive-edit)
      (run-hooks 'mousymacro-after-hook)
      (setq mm-map-active nil)
      (message "MousyMacro OFF!")
      (pop-mark)
      (mm-set-cursor previous-cursor)
      (setq mode-name previous-mode-name) ) ) )

(defun mm-exit-mode ()
  "Exit MousyMacro mode."
  (interactive)
  (exit-recursive-edit) )

(defun mm-undefined (event)
  "Swallow whatever is given to this function."
  (interactive)
  'nothing )

(defun mm-set-cursor (cursor)
  "Change the cursor while MousyMacro is operating."
  (let ((color (cdr (assoc 'mouse-color (frame-parameters)))))
    (setq x-pointer-shape cursor)
    (set-mouse-color color) ) )
;;; Test: (mm-set-cursor x-pointer-xterm)

;;; Two modes exist to call the current macro.
;;;   You can click this sets the point and runs the macro
;;;   You can drag a region to run the macro on.

(defun mm-run-macro (start-event)
  "Handle a down-mouse event, try to figure if it is a drag or a click and
runs the currently selected macro on the region."
  (interactive "e")
  (mouse-set-point start-event)
  (set-mark (point))
  (setq last-command nil)
  (mouse-drag-region start-event)
  (if mm-current-macro
      (call-interactively mm-current-macro)
    (error "MousyMacro: No current macro to run!") ) )

(defun mm-run-alternate-macro (start-event)
  "Handle a down-mouse event, try to figure if it is a drag or a click and
runs the currently selected macro on the region."
  (interactive "e")
  (mouse-set-point start-event)
  (set-mark (point))
  (setq last-command nil)
  (mouse-drag-region start-event)
  (if mm-current-alternate-macro
      (call-interactively mm-current-alternate-macro)
    (error "MousyMacro: No current alternate macro to run!") ) )

;;; Maybe in some time but it is not really useful since the scrollbar is
;;; still active.

(defun mm-scroll (event)
  "Try to scroll intelligently the window according to mouse movements."
  (interactive "e")
  'not-implemented )

(defun mm-change-parameters (event)
  "Display a menu under the mouse and evaluates the selected item. This
is typically used to change the current macro that MousyMacro runs."
  (interactive "e")
  (let ((selection (x-popup-menu event mm-menu)))
    (if selection (eval selection)) ) )

;;;ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
;;; Small examples of MousyMacro invoked functions. 
;;; Don't forget to make them callable with call-interactively ie
;;; start their body with (interactive "").

;;; This macro indexes for LaTeX the current word or region. Just click
;;; on a word or drag a set of words.

;(defvar mm-latex-indexation-number 0
;  "The number of indexed words." )

(defun mm-latex-index-current-region (begin end)
  (interactive "r")
  ;(message "[%d] begin=%d, end=%d, word=\"%s\""
  ;         mm-latex-indexation-number
  ;         begin end (buffer-substring begin end) )
  ;(setq mm-latex-indexation-number (+ 1 mm-latex-indexation-number))
  (if (= begin end)
      ;; This is a simple click on a word, compute a region
      (progn
        (setq begin (progn (backward-word 1)(point)))
        (setq end (progn (forward-word 1) (point))) ) )
  (save-excursion
    ;; Index the region
    (let ((word (buffer-substring begin end))
          here there )
      (beginning-of-line)
      (insert "\\index{")
      (setq here (point))
      (insert word)
      (setq there (point))
      ;; remove trailing spaces
      (skip-syntax-backward " ")
      (delete-region (point) there)
      (setq there (point))
      (insert "}")
      (newline)(scroll-up 1)
      ;; remove leading spaces
      (goto-char here)
      (skip-syntax-forward " ")
      (setq there (- there (- (point) here)))
      (delete-region here (point))
      ;; remove newlines from word
      (subst-char-in-region here there ?\n ? ) ) ) )

;;; Another example that echoes the clicked word in the Minibuffer.
;;; Even if you drag a region, only the current word is considered. 
;;; This was to give an example of a non-region-sensitive macro.

(defun mm-funny-echo-word ()
  (interactive "")
  (save-excursion
    (message (concat "You clicked on "
                     (buffer-substring (progn (backward-word 1)(point))
                                       (progn (forward-word 1) (point)) )
                     "!" )) ) )

;;; Test:      (mousy-macro)

(provide 'mousymacro)

;;; end of mousymacro.el
