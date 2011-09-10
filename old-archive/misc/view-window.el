;;; view-window.el 
;;; less like file/buffer browser in either current or other window.

;;; Copyright (C) 1994 Yasuhiko WATANABE 
;;; E-mail: <ywata@k2.t.u-tokyo.ac.jp>

;;; Any suggestions or bug fixes are wellcomed.
;;; Send mail to the above address.

;;;; Installation
;     Put this program into your load-path directory.
;     And add below code  into your .emacs
; (autoload 'view-window-mode "view-window") .
;     To bind 'view-window-mode to some key is strongly recommended.
; Example:     (define-key global-map "\C-x8" 'view-window-mode)


;;;; Usage
; M-x view-window-mode or use your own binding.

;;;; Getting Help
; Pressing "z" in the view-window-mode.

;; LCD Archive Entry:
;; view-window|Yasuhiko WATANABE|ywata@k2.t.u-tokyo.ac.jp|
;; manipulete other/current windows with less like keybinding|
;; 01-Dec-1994|1.21|~/misc/view-window.el.Z|


;;;;     "view-less.el" inspired me to write the original version of 
;;;;  this software. It began with extending  of "view-less.el".
;;;;  Because of this, many features of it resemble to and/or are derived 
;;;;  from "view-less.el". But this program does not support all of its 
;;;;  functionallity nor behave as it.

;;;;      The function named "scroll-chose-window" in 
;;;;  "scroll-in-place.el" greatly influenced in window-selecting algorithm.

;;;;     "scroll-in-place.el" was written by Eric Eide <eeide@cs.utah.edu>
;;;;   and "view-less.el" was written by David Gudeman <gudeman@arizona.edu>

;;;;     The author usually load "active-buffers.el" (not included in emacs 
;;;;  standard distribution) written by Mike Williams <byrd@comp.vuw.ac.nz>.
;;;;  view-window-next-buffer and view-window-prev-buffer in this program 
;;;;  emulate its functionality of next-active-buffer and prev-active-buffer 
;;;;  in"active-buffers.el" They are almost derived from "active-buffers.el".

;;;; COPYRIGHT NOTICE
;;;;
;;;; This program is free software; you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by the Free
;;;; Software Foundation; either version 2 of the License, or (at your option)
;;;; any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;;;; for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License along
;;;; with GNU Emacs.  If you did not, write to the Free Software Foundation,
;;;; Inc., 675 Mass Ave., Cambridge, MA 02139, USA.

;;;; Platforms
;;;;   The author tested the view-window-mode under only Nemacs-3.3.2 (Japanese extended 
;;;; version of emacs-18.55) and  Mule-2.1 (multi lingual extended version of emacs-18.27).
;;;; No Nemacs nor Mule specific commands are used. So it surelly work on the emacs18 and
;;;; emacs19.

;;;  Purposes
;;;     The  main  purporse of this  program is to reduce  your
;;;  fruitless key hittings to  move  from  window to window
;;;  in emacs.
;;;     Commands work either on current window or other window.
;;;  You can manipulate any visible window in Emacs without using window 
;;;  switching commands. (C-x o etc.)
;;;  Possible manipulation commands are for scrolling (current or 
;;;  other window), finding files, search,  stepping into other window,
;;;  insertion and selecting the target window for manipulations.

;;;     A macro for extending features for other or current windows is
;;;  available. Use view-window-macro for the purpose.

;;;     Scrolling and searching command and etc  are bounded 
;;;  like less or vi.
;;;  scroll: h j k l SPC u 
;;;  search: / ? n
;;;  others: p q c .

;;;    Try "." for re-selecting the target window for manipulations.
;;;  For example, if you have two windows and current target window is
;;;  current window, so you can scroll the current window from itself. 
;;;  Then if you type  "." in the current window, you can scroll other
;;;  window, so you can browse, search and etc. the other window's buffer
;;;  staying in the current buffer.

;;;     It is especially useful when you glance at some source code, 
;;;  other part of the buffer in other window and browsing the emacs help
;;;  staying at the current window.

;;;;;; Suggestions 
;;;;;; If you want view-window-mode work automatically when you read a
;;;;;; write-protected file, add the following into your .emacs file.
;;;;;; (or (member 'auto-view-window-mode find-file-hooks)
;;;;;; (setq find-file-hooks (cons 'auto-view-window-mode find-file-hooks))

;;;;;; Browsing manuals with view-window-mode
;;;;;; (define-key your-own-keymap "some-key" '(lambda nil
;;;;;;      (interactive)
;;;;;;      (view-window-mode)
;;;;;;      (manual-entry nil)))

;;;;;;  Changing target window buffer actively
;;;;;;  if you have active-buffers
;;;;;; (require 'active-buffers)
;;;;;; (view-window-macro view-window-next-active-buffer next-active-buffer nil t nil)
;;;;;; (view-window-macro  view-window-prev-active-buffer prev-active-buffer nil t nil)

;;;;;; Splitting window and call shell buffer
;;;;;; (view-window-macro sh (lambda nil
;;;;;;                          (progn (shell))) nil t t)

;;; Known bugs.
;;; Quitting the mode from the buffer which have no file correspponed to it
;;; will cause "File mode specification error".


(defvar view-window-version 
"$Id: view-window.el,v 1.21 1994/12/01 17:34:29 ywata Exp ywata $")

(defvar view-window-search-string ""
  "Last string searched for with view-window-search functions.")
(defvar view-window-search-arg 1
  "Arg sent to search command.")

(defvar view-window-sit-for-time 1
"cursor blinking time in other window")
(defvar view-window-marked nil)

(defvar view-window-last-insert-command "")
(make-local-variable 'view-window-last-insert-command)
(make-local-variable 'view-window-search-string)
;(make-local-variable 'view-window-search-arg)

(defvar view-window-last-mode)

(defvar view-window-buffer-read-only nil 
"Local variable for restoring original buffer mode")
(make-local-variable 'view-windows-buffer-read-only)


(defvar view-window-mode-map nil)
(if view-window-mode-map
    nil
  (progn
  (setq view-window-mode-map (make-keymap))
  (suppress-keymap view-window-mode-map)
  (define-key view-window-mode-map "f" 'view-window-find-file)     ; File
  (define-key view-window-mode-map "o" 'other-window)               ; Other window
  (define-key view-window-mode-map "x" 'view-window-quit-del-other-window);eXit
  (define-key view-window-mode-map "q" 'view-window-quit)           ;Quit
  (define-key view-window-mode-map "," 'view-window-quit)
  (define-key view-window-mode-map " " 'scroll-up-other-window)     ;less
  (define-key view-window-mode-map "j" 'scroll-up-1-other-window)   ;less
  (define-key view-window-mode-map "k" 'scroll-down-1-other-window) ;less
  (define-key view-window-mode-map "/" 'view-window-search-forward) ;less
  (define-key view-window-mode-map "?" 'view-window-search-backward);less
  (define-key view-window-mode-map "n" 'view-window-re-search)      ;less
  (define-key view-window-mode-map "y" 'scroll-down-1-other-window) ;less
  (define-key view-window-mode-map "u" 'scroll-down-other-window)   ;less  
;  (define-key view-window-mode-map "b" 'scroll-down-other-window)   ;less
  (define-key view-window-mode-map "p"  'view-window-goto-percent)  ;less
  (define-key view-window-mode-map "s"  'view-window-show-point)   ;Show

  (define-key view-window-mode-map "i" 'view-window-insert);

  (define-key view-window-mode-map "." 'view-window-select);
  
  (define-key view-window-mode-map "h" 'backward-char-other-window) ;vi
  (define-key view-window-mode-map "l" 'forward-char-other-window)  ;vi

;;; The two functions below does not work well now.
;;; Help! 
;  (define-key view-window-mode-map "m" 'view-window-set-mark)      ;Mark
;  (define-key view-window-mode-map "g" 'view-window-kill-ring-save)

  (define-key view-window-mode-map "c"  'view-window-select)        ;Choose
  (define-key view-window-mode-map "e"  'view-window-prev-active-buffer)
  (define-key view-window-mode-map "r"  'view-window-next-active-buffer)
  (define-key view-window-mode-map "z"  'describe-mode)

  (define-key view-window-mode-map "d"  'view-window-duplicate)
))

(defun other-window-cons ()
"Return cons of slected-window and other-window you can check you
have only a window or more by checcking the EQuality of slected-window 
and other-window.
(selected-window. other-window) will be returned."
  (let* ((selected-window (selected-window))
	 (other-window (if (and (eq selected-window (minibuffer-window))
				minibuffer-scroll-window
				;; `window-point' is `nil' if the window has
				;; been deleted.
				(window-point minibuffer-scroll-window))
			   minibuffer-scroll-window
			 (next-window selected-window))))
           (if view-window-marked
              (cons selected-window view-window-marked)
           (cons selected-window other-window))))

(defun view-window-select nil
"Window which cursors is in is selected after executing 
this function"
(interactive)
(let ((current-window (selected-window)))
  (if (eq current-window view-window-marked)
    (setq view-window-marked nil)
    (setq view-window-marked current-window))))

;;; Experimental 
(defun view-window-select2 nil
"simple interface for selecting a window"
(interactive)
(let ((current-window (selected-window))
      (next-window)
      (select-or-not)
      (break t))
  (while
      (and break (not (eq current-window (setq next-window (next-window)))))
      (select-window next-window)
      (setq select-or-not (sit-for-millisecs 1500))
      (if select-or-not
	  nil
	  (progn
	    (setq break nil)
	    (setq view-window-marked (selected-window)))))
  (select-window current-window)))

(defun view-window-show-point ()
"To show where the point is in the `other buffer'"
  (interactive )
  (let* ((window-cons (other-window-cons))
	 (other-window (cdr window-cons)) 
	 (selected-window (car window-cons))
	 (one-window-flag (eq other-window selected-window)))
	; (selected-window . other-window)
    (progn
      (if one-window-flag nil
	  (select-window other-window))
	(sit-for view-window-sit-for-time)
     (if one-window-flag nil
       (select-window selected-window)))))

(defun view-window-insert (command)
"Insert a string into the other window's point position.
When you have a shell buffer in the other window, the inserted
command is send to shell. If command is not specified, the last
command is again inserted."
  (interactive "sinsert-command: ")
  (let* ((window-cons (other-window-cons))
	 (other-window (cdr window-cons)) 
	 (selected-window (car window-cons))
	 (one-window-flag (eq other-window selected-window)))
	; (selected-window . other-window)
    (progn
      (if (equal "" command)
	  (setq command view-window-last-insert-command)
	(setq view-window-last-insert-command command))
      (select-window other-window)
      (cond
       ((equal mode-name "Shell");;; to execute command in other window
	(goto-char (point-max))
	(insert-string command)
	(if (featurep 'comint) ;;; for emacs19 or mule.
	    (comint-send-input)
	  (shell-send-input)))
       (t
	(insert-string command)))
      (select-window selected-window))))

(defun view-window-find-file (filename)
"Find a file into other window if you have two windows or more.
Otherwise window is splited and load the file."
  (interactive "Ffile name: ")
  (let* ((window-cons (other-window-cons))
	 (other-window (cdr window-cons)) 
	 (selected-window (car window-cons))
	 (one-window-flag (eq other-window selected-window)))
	; (selected-window . other-window)
    (progn
      (if one-window-flag 
	  (find-file-other-window filename)
	(progn
	  (select-window other-window)
	  (find-file filename)))
      (select-window selected-window))))


;;;; This function does not work well.
;;;; Help me!
;;;; The author only want to set mark in the target window.
(defun view-window-set-mark ()
"set mark in other window"
  (interactive)
  (let* ((window-cons (other-window-cons))
	 (other-window (cdr window-cons)) 
	 (selected-window (car window-cons))
	 (one-window-flag (eq other-window selected-window)))
	; (selected-window . other-window)
    (progn
      (if one-window-flag nil
	  (select-window other-window))
	(push-mark (point))(sit-for 1)
     (if one-window-flag nil
       (select-window selected-window)))))

;;;; This function does not work well.
;;;; Help me!
(defun view-window-kill-ring-save ()
"set mark in other window"
  (interactive)
  (let* ((window-cons (other-window-cons))
	 (other-window (cdr window-cons)) 
	 (selected-window (car window-cons))
	 (one-window-flag (eq other-window selected-window)))
	; (selected-window . other-window)
    (progn
      (if one-window-flag nil
	  (select-window other-window))
	(kill-ring-save (point) (pop-mark))
	(sit-for 1)
     (if one-window-flag nil
       (select-window selected-window)))))

(defun scroll-up-other-window (&optional lines)
"scroll the other window down vertically.
If optional argument lines does not exist,
it acts like normal scroll-down command."
  (interactive "P")
  (let* ((cannot-scroll-flag nil)
	 (point-bound-flag nil)
	 (window-cons (other-window-cons))
	 (other-window (cdr window-cons)) 
	 (selected-window (car window-cons))
	 (one-window-flag (eq other-window selected-window)))
	; (selected-window . other-window)
    (progn
      (if one-window-flag nil
	(select-window other-window))
      (if
	  (save-excursion
	    (not (pos-visible-in-window-p (point-max))))
	  (progn 
	    (scroll-up lines)
	    (if lines (forward-line lines)))
	(progn 
	  (setq cannot-scroll-flag t)
	  (if lines 
	      (forward-line lines)
	    (goto-char (point-max))))))
      (setq point-bound-flag (= (point) (point-max)))
      (sit-for view-window-sit-for-time)
      (if one-window-flag nil
	(select-window selected-window))
      (if cannot-scroll-flag
	  (if point-bound-flag
	      (error "cannot scroll")))))

(defun scroll-down-other-window (&optional lines)
"scroll the other window down vertically.
If optional argument lines does not exist,
it acts like normal scroll-down command."
  (interactive "P")
  (let* ((cannot-scroll-flag nil)
	 (point-bound-flag nil)
	 (window-cons (other-window-cons))
	 (other-window (cdr window-cons)) 
	 (selected-window (car window-cons))
	 (one-window-flag (eq other-window selected-window)))
	; (selected-window . other-window)
    (progn
      (if one-window-flag nil
	  (select-window other-window))
     (if
	 (save-excursion
	   (not (pos-visible-in-window-p (point-min))))
	 (progn 
	   (scroll-down lines)
	   (if lines (forward-line (- lines))))
       (progn 
	 (setq cannot-scroll-flag t)
	 (if lines 
	     (forward-line (- lines))
	   (goto-char (point-min)))))
     (setq point-bound-flag (= (point) (point-min)))
     (sit-for view-window-sit-for-time)
     (if one-window-flag nil
       (select-window selected-window))
     (if cannot-scroll-flag
	 (if point-bound-flag
	     (error "cannot scroll"))))))

(defun view-window-goto-percent (&optional p)
"move the curssor in other window around p percent position"
  (interactive "P")
  (let* ( (window-cons (other-window-cons))
	 (other-window (cdr window-cons)) 
	 (selected-window (car window-cons))
	 (one-window-flag (eq other-window selected-window)))
	; (selected-window . other-window)
    (progn
      (if (null p) (setq p 0))
      (if one-window-flag nil
	  (select-window other-window))
      (goto-char (+ (point-min) (/ (* p (- (point-max) (point-min))) 100)))
      (beginning-of-line)
      (if one-window-flag nil
	  (select-window selected-window)))))


(defun forward-char-other-window ()
"Move the cursor in other window forward"
  (interactive)
  (let* ((window-cons (other-window-cons))
	 (other-window (cdr window-cons)) 
	 (selected-window (car window-cons))
	 (one-window-flag (eq other-window selected-window)))
    (progn
      (if one-window-flag nil
	  (select-window other-window))
      (if (eq (point) (point-max))
	(progn
	  (beep)
	  (message "cannot move"))
	(forward-char))
      (sit-for view-window-sit-for-time)
     (if one-window-flag nil
       (select-window selected-window)))))

(defun backward-char-other-window ()
"Move the cursor in other window backward"
  (interactive)
  (let* ((window-cons (other-window-cons))
	 (other-window (cdr window-cons)) 
	 (selected-window (car window-cons))
	 (one-window-flag (eq other-window selected-window)))
    (progn
      (if one-window-flag nil
	  (select-window other-window))
      (if (eq (point) (point-min)) 
	  (progn
	    (beep)
	    (message "cannot move"))
	(backward-char))

      (sit-for view-window-sit-for-time)
     (if one-window-flag nil
       (select-window selected-window)))))

(defun scroll-up-1-other-window (&optional lines)
"scrollup  the other window one line"
(interactive "P")
  (if (null lines)	
      (scroll-up-other-window 1)
    (scroll-up-other-window lines)))

(defun scroll-down-1-other-window (&optional lines)
"scroll down  the other window one line"
(interactive)
  (if (null lines)	
      (scroll-down-other-window 1)
    (scroll-down-other-window lines)))




;;;###autoload
(defun view-window-mode (&optional current-window-flag)
"Commands are applyed to other-window.
With optional argument current-window-flag is set,
 current window is set to other-window.
Commands listed below can also work in this buffer.
Move:
  \\[scroll-down-other-window]         scroll down
  \\[scroll-up-other-window]       scroll up
  \\[scroll-up-1-other-window]         scroll down one line
  \\[scroll-down-1-other-window]         scroll up one line
  \\[backward-char-other-window]         backward character
  \\[forward-char-other-window]         forward character
  \\[view-window-goto-percent]         go to percent
Select window:
  \\[view-window-select]         choose the window
  \\[view-window-duplicate]        duplicate the current window
Quit :
  \\[view-window-quit-del-other-window]         exit and delete other window
  \\[view-window-quit]         quit 
Search and insert:
  \\[view-window-search-forward]         search string forward 
  \\[view-window-search-backward]       search string backward
  \\[view-window-re-search]         re-search string again
  \\[view-window-insert]         insert a string
Others:
  \\[view-window-find-file]         view other file
  \\[other-window]         step into other window
  \\[show-point-other-window]         show the point
  \\[describe-mode]         describe view-window-mode

Target window is determined in the following rules:
1) If you have only one window in your emacs,
   the window is the target.
2) If you have two windows or more and you do not specified
   target window by \\[view-window-select] or M-x view-window-select,
   then, target window is specified by next-window 
   (emacs lisp) command.
3) If you execute \\[view-window-select] or M-x view-window-select,
   target window is changed into the one in which the command is carried
   out.

Key bindngs:
\\{view-window-mode-map}
"
  (interactive)


;;  (kill-all-local-variables) ; No, this is very bad.  Don't reset mode.
;; this lets the prevailing local map be accessible too.

  (let ((map (copy-keymap view-window-mode-map)))
;    (set-keymap-parent map (current-local-map))
	(use-local-map map))
  ;; Save previous major-mode
  (if (and (boundp 'view-window-last-mode)
	   (eq major-mode 'view-window-mode))
      nil
    (set (make-local-variable 'view-window-last-mode) major-mode))
    (setq mode-name "View Window")
    (setq major-mode 'view-window-mode)
    (setq mode-line-buffer-identification (list "View Window: %17b"))
    (if view-window-buffer-read-only
	(setq view-window-buffer-read-only buffer-read-only)
      	(setq view-window-buffer-read-only nil)
      )
    (setq buffer-read-only t)
    (if current-window-flag
	(setq view-window-marked (selected-window)))
  (redraw-display)
  (message "Press z to get simple help"))



(defun view-window-mode-describe ()
  (interactive)
  (let ((mode-name "View Window mode")
	(major-mode 'view-window-mode))
    (describe-mode)))

;;; kill-on-exit is not used now
(defun view-window-quit (&optional kill-on-exit)
"Quit from the view window mode."
  (interactive)
  (message "View Window mode exitting")
  (let ((old-mode (and (boundp 'view-window-last-mode) view-window-last-mode)))
    (setq buffer-read-only view-window-buffer-read-only)
    (normal-mode)
      ;; this might not be a file buffer; if normal-mode didn't put us
      ;; in some sensible mode, switch to the previous mode if possible.
    (if (and old-mode
	     (eq major-mode (or default-major-mode 'fundamental-mode))
	     (not (eq old-mode major-mode)))
	(condition-case nil
	    (progn
	      (funcall old-mode))
		(error " error")))
  (setq view-window-marked nil);
  (redraw-display)))


(defun view-window-quit-del-other-window ()
"Quitting the view window mode and delete other window"
  (interactive)
  (view-window-quit)
  (delete-other-windows))

(defun view-window-quit-kill-other-window ()
"Quitting the view window mode and kill-buffer in other-window"
  (interactive)
  (view-window-quit t))


(defun view-window-normal-mode ()
  "Switch from view window mode to the usual mode for this buffer"
  (interactive)
  (view-window-quit))

(defun view-window-search-forward (s p)
"Search a word in the other window.
If you do not specify the first argument,
it tries to search view-window-search-string.
When you specify the second argument,  the p'th 
matched string is desplayed.
In any case the string for search is stored into 
view-window-search-string for next search."
  (interactive "sView search: \np")
  (let* ((matched-point nil)
	 (search-succeed nil)
	 (window-cons (other-window-cons))
	 (other-window (cdr window-cons)) 
	 (selected-window (car window-cons))
	 (one-window-flag (eq other-window selected-window)))
    (if one-window-flag nil
      (select-window other-window))

    (if (string= "" s) nil
      (setq view-window-search-string s))
    (unwind-protect
	(setq search-succeed (search-forward
	      (if (string= "" s) view-window-search-string s) nil t p)))
    (if search-succeed
	(progn
	  (sit-for view-window-sit-for-time)
	  (if (null p)
	    (setq view-window-search-arg 1)
	    (setq view-window-search-arg p))
	  (if one-window-flag nil
	      (select-window selected-window)))
      (progn
	(select-window selected-window)
	(error "search failed")))))


(defun view-window-search-backward (s p)
"Search backward in other window"
  (interactive "sView search backward: \np")
  (view-window-search-forward s 
    (cond ((null p) (- 1))
	  (t (-(prefix-numeric-value p))))))
     
(defun view-window-re-search (p)
"Re-search string in other window"
  (interactive "P")
  (view-window-search-forward
   view-window-search-string
   (cond ((null p) view-window-search-arg)
	 ((eq p '-) (- 1))
	 (t (prefix-numeric-value p)))))

(defun auto-view-window-mode0 ()
  "If the file of the current buffer is not writable, call view-window-mode.
  This is meant to be added to find-file-hooks."
  (if (and buffer-file-name
           (not (file-writable-p buffer-file-name)))
      (view-window-mode t)))

(defun auto-view-window-mode ()
(cond
 ((string-match "^*" (buffer-name))
  nil)
 ((and buffer-file-name 
       (not (file-writeable-p buffer-file-name)))
  (view-window-mode t))))

(defun view-window-duplicate ()
(interactive)
  (let* ((current-buffer-name (buffer-name))
	 (window-cons (other-window-cons))
	 (other-window (cdr window-cons)) 
	 (selected-window (car window-cons))
	 (one-window-flag (eq other-window selected-window)))
    (progn
      (if one-window-flag 
	    (split-window-vertically))
      (set-window-buffer (cdr (other-window-cons))
			 current-buffer-name))))


(defmacro view-window-macro (funcname function 
      inter-opt unselect-cw split-window &optional sit-for-p)
"The first argnument funcname is the name of function to be defined.
The second argument function is a function to be applied in the other
window. The 3rd inter-opt is a option to be specified in interactive 
function. The 4th determines current window is select or not when
the funcname is called. The 5th argument defines whether window split 
before function execution.
The optional 6th argument sit-for-p is used
for determining performing sit-for or not."

;; This macro gets easier using Common lisp pacage.
(append
 (list 'defun funcname)
 (if inter-opt
     (list '(arg1 &optional sit-for-p)
	   (list 'interactive inter-opt))
   (list '(&optional sit-for-p)
	 '(interactive)))

 (list
  (list 'if split-window '(split-window-vertically))

  (list 'if unselect-cw
	'(setq view-window-marked nil))

  (list 
   'let* '((window-cons (other-window-cons))
	   (other-window (cdr window-cons)) 
	   (selected-window (car window-cons))
	   (one-window-flag (eq other-window selected-window)))
  (list
   'progn
   '(if one-window-flag nil
      (select-window other-window))
   (if inter-opt
       (list function 'arg1)
     (list function))
   (list 
    'if sit-for-p
    (list 'sit-for view-window-sit-for-time))
   '(if one-window-flag nil
      (select-window selected-window)))))))

;;; loadablep does not exist in emacs18
(defun file-in-directory-p (filename path-list)
  (let ((path nil)
	(return nil))
    (while (not (null path-list))
      (setq path (car path-list)
	    path-list (cdr path-list))
      (if (and (file-exists-p (concat path "/" filename))
	       (file-readable-p (concat path "/" filename)))
	       (setq return t)))
    return))

(defun view-window-buffer-p (buffer)
  (let ((buffer-name (buffer-name buffer)))
    (if (string-match "\\*" buffer-name)
	t
      nil)))

(defun view-window-buffer-list nil
  (let ((buf (buffer-list))
	(buffer-list nil)
	(buffer-list-car nil))
    (while (not (null buf))
      (setq buffer-list-car (car buf))
      (if (not (view-window-buffer-p buffer-list-car))
	  (setq buffer-list 
		(append buffer-list (list buffer-list-car))))
      (setq buf (cdr buf)))
    buffer-list))

(defun view-window-next-buffer nil
  (interactive)
  (bury-buffer (current-buffer))
  (switch-to-buffer (car (view-window-buffer-list))))

(defun view-window-prev-buffer nil
  (interactive)
  (bury-buffer (current-buffer))
  (switch-to-buffer (car (cdr (nreverse (view-window-buffer-list))))))

(if (or (file-in-directory-p "active-buffers.elc" load-path)
	 (file-in-directory-p "active-buffers.el" load-path))
    (progn
      (require 'active-buffers)
      ;; These are useful changing buffer.
      (view-window-macro view-window-next-active-buffer next-active-buffer nil t nil)
      (view-window-macro  view-window-prev-active-buffer prev-active-buffer nil t nil))
  (progn
    (view-window-macro view-window-next-active-buffer view-window-next-buffer nil t nil)
    (view-window-macro view-window-prev-active-buffer view-window-prev-buffer nil t nil)))



(provide 'view-window)
;;; view-window.el ends here
