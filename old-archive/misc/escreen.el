; There was some reason why, years ago, I felt like writing this instead of
; using wicos.el (which is a perfectly good alternative), but I don't
; remember what it was anymore.  Anyway, I've been using this for several
; years.
; 
; This program originally predates emacs 19, which is why it may seem
; anachronistic when emacs has support for multiple X or ascii frames.  But I
; rarely use X, and I wanted this program to work in any version of XEmacs or
; Emacs, including version 18.  There are so many incompatibilities in the
; way frame objects are implemented (and in some cases they are
; unimplemented), that I decided not to use those interfaces, and instead
; stick with what was portable.
; 
; Note that you can narrow different portions of the same buffer in different
; screens without having to resort to "indirect buffers".  On the other hand,
; inserting text in the buffer can mess up the narrowing in the other screen
; unless you are using Emacs 19.30, which lets you change the "insertion type"
; of markers.
; 
; Enjoy.
; 
; 
;;; escreen.el --- emacs window session manager

;;; Copyright (C) 1992, 1994, 1995 Noah S. Friedman

;;; Author: Noah Friedman <friedman@prep.ai.mit.edu>
;;; Maintainer: friedman@prep.ai.mit.edu
;;; Keywords: extensions
;;; Created: 1992-03-23

;;; $Id: escreen.el,v 1.7 1995/12/03 07:22:40 friedman Exp $

;; LCD Archive Entry:
;; escreen|Noah Friedman|friedman@gnu.ai.mit.edu|
;; emacs window session manager|
;; 11-Dec-1995|1.7|~/misc/escreen.el.gz|

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
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Inspired by an early implementation of wicos.el, which was written
;; by Heikki Suopanki <suopanki@phoenix.oulu.fi>; as well as by `screen',
;; written by Oliver Laumann, Juergen Weigert, and Michael Schroeder.
;; I make no claims that escreen.el lives up the quality of the work done
;; by these people.

;; To install, put this file in your load-path, byte-compile it, and add
;; the following to your .emacs:
;;
;;   (load "escreen")
;;   (escreen-install)

;; TODO:
;; Ability to save escreen configuration to disk and load it back up at
;; emacs start time.  Things to consider: present state of emacs when
;; escreen gets loaded.  Append it using escreen-create-screen?  Trash
;; present screen configuration?  And how best to get rid of the dependency
;; on current-window-configuration?  Use tapestry.el?
;;
;; Preserve order of buffer-list (probably have to do a lot of set-buffer
;; and bury-buffer calls) when switching screens.  New buffers created
;; since screen was last switched should be "buried" in the current screen
;; but the order should be preserved otherwise, if possible.

;;; Code:

;; Variable declarations -- can be set by user

(defvar escreen-max-screens 10
  "*Maximum number of screens that may be created.")

(defvar escreen-new-screen-default-buffer "*scratch*"
  "*Default buffer to display in newly-created screens.")

(defvar escreen-restore-killed-buffers nil
  "*If non-nil, automatically revisit files if they have been killed.
That is, if a buffer was killed while in another screen session,
recreate them, visiting whatever file they were visiting.")

(defvar escreen-number-mode t
  "*If non-nil, display current escreen number in mode line.")

(defvar escreen-goto-screen-hook nil
  "*Hook to run after `escreen-goto-screen' completes.
An example function that can make use of this hook is
`escreen-enable-number-mode-if-more-than-one-screen'.")

(defvar escreen-menu-mode-hook nil
  "*Hook to run by `escreen-menu' after everything else.")

;; If you want to add or change this list, it's best to set
;; escreen-configuration-alist to nil and run escreen-install afterward.
;; Otherwise, the new table will be used with old data and may cause errors.
;;
;; Note that resetting escreen in this way loses all but the current
;; window configuration.
(defvar escreen-map-data-format
  (list
   (cons (function (lambda ()
                     (escreen-make-marker (window-start))))
         (function (lambda (p)
                     (and (escreen-position-valid-p p)
                          (set-window-start (selected-window) p t)))))

   (cons 'mark-marker
         (function
          (lambda (mark)
            (and (escreen-position-valid-p mark)
                 (set-marker (mark-marker)
                             (marker-position mark)
                             (marker-buffer mark))))))

   (cons 'escreen-save-point      'escreen-restore-point)
   (cons 'escreen-narrowed-region 'escreen-restore-narrowed-region)

   (cons (function (lambda () truncate-lines))
         (function (lambda (v) (setq truncate-lines v))))

   ;; XEmacs 19.13 does not declare mode-line-inverse-video
   ;; Instead, it uses face properties, but I was too lazy to add support
   ;; for that here.
   (cons (function (lambda ()
                     (and (boundp 'mode-line-inverse-video)
                          mode-line-inverse-video)))
         (function (lambda (v)
                     (and (boundp 'mode-line-inverse-video)
                          (setq mode-line-inverse-video v)))))

   ;; Emacs 19.30 supports menu bars on ascii terminals, but beware of
   ;; turning them off or on once escreen is loaded; if a stored window
   ;; configuration was for a frame with a menu bar, but there is no menu
   ;; bar presently, that will crash emacs.  Hopefully this will be fixed
   ;; in Emacs 19.31.
   (cons (function (lambda ()
                     (and (boundp 'menu-bar-mode)
                          menu-bar-mode)))
         (function (lambda (v)
                     (cond ((fboundp 'menu-bar-mode)
                            (if v
                                (menu-bar-mode 1)
                              (menu-bar-mode -1)))))))
   ))


;; Keybindings

(defvar escreen-prefix-char "\C-\\"
  "*Character prefixing escreen commands.
If you wish to change this, you must also do

   (global-set-key escreen-prefix-char 'escreen-prefix)

to update the prefix in the global keymap.")

(defvar escreen-map nil
  "*Keymap for escreen commands.")
(cond
 ((null escreen-map)
  (setq escreen-map (make-sparse-keymap))
  (define-key escreen-map escreen-prefix-char 'escreen-goto-last-screen)
  (define-key escreen-map "0"    'escreen-goto-screen-0)
  (define-key escreen-map "1"    'escreen-goto-screen-1)
  (define-key escreen-map "2"    'escreen-goto-screen-2)
  (define-key escreen-map "3"    'escreen-goto-screen-3)
  (define-key escreen-map "4"    'escreen-goto-screen-4)
  (define-key escreen-map "5"    'escreen-goto-screen-5)
  (define-key escreen-map "6"    'escreen-goto-screen-6)
  (define-key escreen-map "7"    'escreen-goto-screen-7)
  (define-key escreen-map "8"    'escreen-goto-screen-8)
  (define-key escreen-map "9"    'escreen-goto-screen-9)
  (define-key escreen-map "?"    'escreen-help)
  (define-key escreen-map "\C-b" 'escreen-menu)
  (define-key escreen-map "a"    'escreen-get-active-screen-numbers)
  (define-key escreen-map "b"    'escreen-get-current-screen-number)
  (define-key escreen-map "c"    'escreen-create-screen)
  (define-key escreen-map "g"    'escreen-goto-screen)
  (define-key escreen-map "k"    'escreen-kill-screen)
  (define-key escreen-map "n"    'escreen-goto-next-screen)
  (define-key escreen-map "p"    'escreen-goto-prev-screen)))

(fset 'escreen-prefix escreen-map)
(global-set-key escreen-prefix-char 'escreen-prefix)


;; Internal variables.  Do not set these yourself.

;; This should not be modified by the user.  The information it provides is
;; critical and the calling conventions are different than for
;; escreen-map-data-format.  The order here is important too.
(defvar escreen-map-critical-data-format
  (list 'escreen-window-edges
        'current-buffer
        (function (lambda () (buffer-name)))
        'buffer-file-name))

;; Keeps track of escreen state (window config, buffers, etc.)
(defvar escreen-configuration-alist nil)

;; Current screen number.  Smallest possible screen number is 0.
(defvar escreen-current-screen-number 0)

;; Current screen number as a string.
;; Smallest possible screen number is 0.
(defvar escreen-current-screen-string
  (int-to-string escreen-current-screen-number))

;; Last-visited screen number.  Smallest possible screen number is 0.
(defvar escreen-last-screen-number 0)

;; Highest screen number currently in use.
(defvar escreen-highest-screen-number-used 0)

;; It's ok to change this, but it makes use of internal variables
(defvar escreen-mode-line-format
  '(escreen-number-mode ("S" escreen-current-screen-string " ")))


(defun escreen-install ()
  (interactive)
  ;; Install screen number on global-mode-string
  (let ((elt '("" escreen-mode-line-format)))
    (or (escreen-member elt global-mode-string)
        (setq global-mode-string
              (cons elt global-mode-string))))
  (escreen-number-mode 1)
  ;; Initialize escreen-configuration-alist by placing current window
  ;; config in it.
  (escreen-save-current-screen-configuration))

(defun escreen-number-mode (&optional prefix)
  "*Toggle escreen-number-mode (see variable docstring).
If called with a positive prefix argument, always enable.
If called with a negative prefix argument, always disable.
If called with no prefix argument, toggle current state."
  (interactive "P")
  (setq escreen-number-mode
        (cond ((null prefix)
               (not escreen-number-mode))
              (t
               (>= (prefix-numeric-value prefix) 0)))))


(defun escreen-create-screen ()
  "Create a new screen and switch to it.
New screen will display one window with the buffer specified by
`escreen-new-screen-default-buffer'."
  (interactive)
  (let* ((new-screen-number (escreen-first-unused-screen-number))
         (buffer-list (buffer-list))
         (buffer-list-length (length buffer-list)))
    (or new-screen-number
        (error "escreen: No more screens (see \"escreen-max-screens\")"))
    ;; Save window configuration before switching to a new one.
    (escreen-save-current-screen-configuration)
    (and (> new-screen-number escreen-highest-screen-number-used)
         (setq escreen-highest-screen-number-used new-screen-number))
    (setq escreen-last-screen-number escreen-current-screen-number)
    (setq escreen-current-screen-number new-screen-number
          escreen-current-screen-string (int-to-string new-screen-number))
    (delete-other-windows)
    (switch-to-buffer escreen-new-screen-default-buffer)
    ;; Save new window configuration so that it's in the alist.
    (escreen-save-current-screen-configuration))
  ;; We run this hook because, in a sense, we have gone to a new
  ;; screen. but we don't actually call escreen-goto-screen because of the
  ;; extra setup work here.
  (run-hooks 'escreen-goto-screen-hook))

(defun escreen-kill-screen (&optional number)
  "Kill current screen, or screen given by optional argument NUMBER.
No error occurs if the specified screen number doesn't exist.
You cannot kill the last existing screen.
Switch to previous screen if killing active one."
  (interactive)
  (let* ((screen-number (or number escreen-current-screen-number))
         (killing-current-screen-p (eq escreen-current-screen-number
                                       screen-number))
         (screen-assq (assq screen-number escreen-configuration-alist))
         previous-screen)
    (and screen-assq
         (progn
           (and killing-current-screen-p
                (>= 1 (length escreen-configuration-alist))
                (error "escreen: only one screen, can't kill."))
           ;; Don't bother looking for previous screen number unless killing
           ;; current screen, because only then do we need to switch screens.
           (and killing-current-screen-p
                (setq previous-screen (escreen-get-prev-screen-number)))
           (setq escreen-configuration-alist
                 (delq screen-assq escreen-configuration-alist))
           (and (eq screen-number escreen-highest-screen-number-used)
                ;; We're killing the screen with the highest number.  Look
                ;; for the next highest number.
                (setq escreen-highest-screen-number-used
                      (car (sort (mapcar 'car escreen-configuration-alist)
                                 '>))))
           (and killing-current-screen-p
                (escreen-goto-screen previous-screen 'dont-update-current))))))


(defun escreen-goto-screen (number &optional dont-update-current)
  "Switch to screen number N.
Optional arg DONT-UPDATE-CURRENT means don't save the current screen
configuration, though this isn't intended to be used interactively."
  (interactive "NGo to escreen number: ")
  (let ((screen-assq (escreen-screen-defined number)))
    (or screen-assq
        (error "escreen: %d: invalid screen number." number))
    (or dont-update-current
        (escreen-save-current-screen-configuration))
    (escreen-set-screen-map (cdr screen-assq))
    (setq escreen-current-screen-string (int-to-string number))
    (or dont-update-current
        (setq escreen-last-screen-number escreen-current-screen-number))
    (setq escreen-current-screen-number number))
  (run-hooks 'escreen-goto-screen-hook))

(defun escreen-goto-last-screen ()
  "Switch to the last visited screen."
  (interactive)
  (let ((n (if (= escreen-last-screen-number escreen-current-screen-number)
               (escreen-get-next-screen-number escreen-last-screen-number)
             escreen-last-screen-number)))
    (setq escreen-last-screen-number escreen-current-screen-number)
    (escreen-goto-screen n)))

(defun escreen-goto-prev-screen (&optional n)
  "Switch to the previous screen.
This is the nearest lower-numbered existing screen from the current one,
wrapping around list of screens if necessary.
If prefix arg N given, jump to the Nth previous screen."
  (interactive "p")
  (if (< n 0)
      (escreen-goto-prev-or-next-screen-internal (- n) 'next)
    (escreen-goto-prev-or-next-screen-internal n 'prev)))

(defun escreen-goto-next-screen (&optional n)
  "Switch to the next screen.
This is the nearest greater-numbered existing screen from the current one,
wrapping around list of screens if necessary.
If prefix arg N given, jump to the Nth next screen."
  (interactive "p")
  (if (< n 0)
      (escreen-goto-prev-or-next-screen-internal (- n) 'prev)
    (escreen-goto-prev-or-next-screen-internal n 'next)))

(defun escreen-goto-prev-or-next-screen-internal (n prev-or-next)
  (let ((total (length (escreen-get-active-screen-numbers)))
        (func (if (eq prev-or-next 'next)
                  'escreen-get-next-screen-number
                'escreen-get-prev-screen-number))
        (i 0)
        (screen-number escreen-current-screen-number))
    (and (> n total)
         ;; Trim off excess amount so we do fewer iterations, since
         ;; wrapping over the total number of screens even once is
         ;; wasteful and slow.
         (setq n (- n (* (/ n total) total))))
    (while (< i n)
      (setq screen-number (funcall func screen-number)
            i (1+ i)))
    (escreen-goto-screen screen-number)))

(defun escreen-goto-screen-0 () (interactive) (escreen-goto-screen 0))
(defun escreen-goto-screen-1 () (interactive) (escreen-goto-screen 1))
(defun escreen-goto-screen-2 () (interactive) (escreen-goto-screen 2))
(defun escreen-goto-screen-3 () (interactive) (escreen-goto-screen 3))
(defun escreen-goto-screen-4 () (interactive) (escreen-goto-screen 4))
(defun escreen-goto-screen-5 () (interactive) (escreen-goto-screen 5))
(defun escreen-goto-screen-6 () (interactive) (escreen-goto-screen 6))
(defun escreen-goto-screen-7 () (interactive) (escreen-goto-screen 7))
(defun escreen-goto-screen-8 () (interactive) (escreen-goto-screen 8))
(defun escreen-goto-screen-9 () (interactive) (escreen-goto-screen 9))


(defun escreen-get-current-screen-number ()
  "Returns the currently selected screen number.
If called interactively, also print this result in the minibuffer."
  (interactive)
  (if (interactive-p)
      (message "escreen: current screen is number %d"
               escreen-current-screen-number)
    escreen-current-screen-number))

(defun escreen-get-active-screen-numbers ()
  "Print a list of the active screen numbers in the echo area.
Returns a list of numbers which represent screen numbers presently in use."
  (interactive)
  (let ((screen-list (sort (mapcar 'car escreen-configuration-alist) '<))
        (str "escreen: active screens:"))
    (and (interactive-p)
         (progn
           (mapcar (function (lambda (num)
                               (setq str (format "%s %d" str num))))
                   screen-list)
           (message str)))
    screen-list))

(defun escreen-help ()
  "Display a short summary of escreen commands."
  (interactive)
  (if (string-lessp emacs-version "19")
      ;; emacs 18 couldn't list only bindings with a common prefix.
      (describe-bindings)
    ;; Emacs 19 can handle escreen-prefix-char (as a string) directly, but
    ;; for XEmacs, it must be converted to a vector.
    (describe-bindings (escreen-string-to-vector escreen-prefix-char))))

(defun escreen-string-to-vector (s)
  (let* ((l (length s))
         (v (make-vector l nil))
         (i 0))
    (while (< i l)
      (aset v i (aref s i))
      (setq i (1+ i)))
    v))


;; Return the first unused number available for designation as a screen
;; number, or nil if  escreen-max-screens  screens are already in use.
(defun escreen-first-unused-screen-number ()
  (let ((number 0))
    (while (and (< number escreen-max-screens)
                (assq number escreen-configuration-alist))
      (setq number (1+ number)))
    (and (< number escreen-max-screens) number)))

;; Save window configuration, buffer configuration, and current marks and
;; point for each displayed buffer for the current screen.
(defun escreen-save-current-screen-configuration ()
  (let ((screen-assq (escreen-screen-defined))
        (new-alist-member))
    (if screen-assq
        (setcdr screen-assq (escreen-get-screen-map))
      (setq new-alist-member (cons escreen-current-screen-number
                                   (escreen-get-screen-map)))
      (setq escreen-configuration-alist
            (cons new-alist-member escreen-configuration-alist)))))

;; Return attributes for screen SCREEN-NUMBER, or nil if it doesn't exist.
(defun escreen-screen-defined (&optional screen-number)
  (or screen-number (setq screen-number escreen-current-screen-number))
  (assq screen-number escreen-configuration-alist))

;; Return nearest number less than current screen number that is
;; an active screen, wrapping around end of screen list if necessary.
(defun escreen-get-prev-screen-number (&optional current-screen-number)
  (or current-screen-number
      (setq current-screen-number escreen-current-screen-number))
  (if (eq 0 escreen-highest-screen-number-used)
      0
    ;; Decrement/wrap current screen number
    (setq current-screen-number (1- current-screen-number))
    (and (< current-screen-number 0)
         (setq current-screen-number escreen-highest-screen-number-used))
    (while (not (assq current-screen-number escreen-configuration-alist))
      ;; Decrement/wrap current screen number
      (setq current-screen-number (1- current-screen-number))
      (and (< current-screen-number 0)
           (setq current-screen-number escreen-highest-screen-number-used)))
    current-screen-number))

;; Return nearest number greater than current screen number that is
;; an active screen, wrapping around end of screen list if necessary.
(defun escreen-get-next-screen-number (&optional current-screen-number)
  (or current-screen-number
      (setq current-screen-number escreen-current-screen-number))
  (if (eq 0 escreen-highest-screen-number-used)
      0
    ;; Increment/wrap current screen number
    (setq current-screen-number (1+ current-screen-number))
    (and (> current-screen-number escreen-highest-screen-number-used)
         (setq current-screen-number 0))
    (while (not (assq current-screen-number escreen-configuration-alist))
      ;; Increment/wrap current screen number
      (setq current-screen-number (1+ current-screen-number))
      (and (> current-screen-number escreen-highest-screen-number-used)
           (setq current-screen-number 0)))
    current-screen-number))


(defun escreen-get-screen-map ()
  (let ((config (current-window-configuration))
        (win-data nil)
        (first-window (if (eq (selected-window) (minibuffer-window))
                          (next-window)
                        (selected-window)))
        (window nil))
    (save-excursion
      (save-window-excursion
        (select-window first-window)
        (while (not (eq window first-window))
          (setq window (select-window (next-window)))
          (set-buffer (window-buffer (selected-window)))
          (setq win-data
                (cons (cons (mapcar 'funcall escreen-map-critical-data-format)
                            (mapcar (function (lambda (cell)
                                                (funcall (car cell))))
                                    escreen-map-data-format))
                      win-data)))))
    (cons config (nreverse win-data))))

(defun escreen-set-screen-map (map)
  (let* ((config (car map))
         (win-data (cdr map))
         (edges (car (car (car win-data))))
         size)
    (set-window-configuration config)
    (while (not (equal edges (escreen-window-edges (selected-window))))
      (select-window (next-window)))
    (while win-data
      (setq data (car win-data))
      (setq win-data (cdr win-data))

      (escreen-restore-buffer (car data))
      (widen)
      (setq data (cdr data))
      (let ((funlist escreen-map-data-format))
        (while (and data funlist)
          (funcall (cdr (car funlist)) (car data))
          (setq funlist (cdr funlist))
          (setq data (cdr data))))
      (select-window (next-window)))
    (select-window (previous-window))))

(defun escreen-restore-buffer (data)
  (cond ((escreen-killed-buffer-p (nth 1 data))
         (cond ((null escreen-restore-killed-buffers)
                (set-window-buffer (selected-window)
                                   (get-buffer-create
                                    escreen-new-screen-default-buffer)))
               ((stringp (nth 3 data))
                (let ((buffer (find-file-noselect (nth 3 data))))
                  (set-window-buffer (selected-window) buffer))
                (and (not (get-buffer (nth 2 data)))
                     (rename-buffer (nth 2 data))))
               (t
                (set-window-buffer (selected-window)
                                   (get-buffer-create
                                    escreen-new-screen-default-buffer)))))
        (t
         (set-window-buffer (selected-window) (nth 1 data)))))

(defun escreen-killed-buffer-p (buffer)
  ;; killed buffers have no names.
  (not (buffer-name buffer)))

(defun escreen-narrowed-region ()
  (cons (and (> (point-min) 1)
             (escreen-make-marker (point-min)))
        (and (<= (point-max) (buffer-size))
             (escreen-make-marker (point-max) nil t))))

(defun escreen-restore-narrowed-region (reg)
  (let ((size (1+ (buffer-size)))
        (beg (or (car reg) (point-min)))
        (end (or (cdr reg) (point-max))))
    (and (escreen-position-valid-p beg)
         (<= beg size)
         (<= end size)
         (narrow-to-region beg end))))

(defun escreen-save-point ()
  ;; If there is a process mark in the current buffer
  ;; and point is at it, then return the process mark
  ;; also.  That way, when we return to this screen,
  ;; point will be at the end of the process output even
  ;; if that has advanced since then.  Otherwise, just
  ;; use a before-insertion marker (if supported).
  (let* ((point-mark (escreen-make-marker (point-marker) nil t))
         (proc (get-buffer-process (current-buffer)))
         (proc-mark (and proc (process-mark proc))))
    (if (and (escreen-position-valid-p proc-mark)
             (= proc-mark (point)))
        (cons proc-mark point-mark)
      point-mark)))

(defun escreen-restore-point (pos)
  (cond ((consp pos)
         (cond ((escreen-position-valid-p (car pos))
                (goto-char (car pos)))
               ((escreen-position-valid-p (cdr pos))
                (goto-char (cdr pos)))))
        (t
         (and (escreen-position-valid-p pos)
              (goto-char pos)))))

(defun escreen-position-valid-p (pos)
  (cond ((numberp pos)
         (<= pos (1+ (buffer-size))))
        ((markerp pos)
         (and (eq (marker-buffer pos) (current-buffer))
              (numberp (marker-position pos))
              (<= pos (1+ (buffer-size)))))
        (t nil)))

;; Copy existing marker, or make a new one from point.
;; Emacs 19.30 and later can create markers which are advanced if text is
;; inserted before them, without needing to call insert-before-markers
;; explicitly.  This is useful for storing point, mark, etc. since the
;; buffer may be edited while we are in other escreens.
(defun escreen-make-marker (pos &optional buffer insertion-type)
  (let ((new-marker nil))
    (cond ((markerp pos)
           (setq new-marker (copy-marker pos))
           (and buffer
                (set-marker new-marker (marker-position pos) buffer)))
          (t
           (setq new-marker (make-marker))
           (set-marker new-marker pos buffer)))
    (and (fboundp 'set-marker-insertion-type)
         (set-marker-insertion-type new-marker insertion-type))
    new-marker))


(defun escreen-menu ()
  (interactive)

  ;; Sort the alist so that they are in order numerically.
  (setq escreen-configuration-alist
        (sort escreen-configuration-alist
              (function (lambda (a b)
                          (< (car a) (car b))))))

  (let ((escreen-menu-buffer (get-buffer-create "*Escreen List*"))
        (alist escreen-configuration-alist)
        list sub-alist)
    ;; Display buffer now so update of screen cofiguration will be correct.
    (display-buffer escreen-menu-buffer)
    ;; Update escreen-configuration-alist to contain up-to-date information
    ;; on current screen, since we'll be displaying data about it.
    (escreen-save-current-screen-configuration)
    (save-excursion
      (set-buffer escreen-menu-buffer)
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert " Screen Buffers\n ------ -------\n")
      (while alist
        (setq list (car alist))
        (setq alist (cdr alist))
        (if (= (car list) escreen-current-screen-number)
            (insert (format "*%-6d " (car list)))
          (insert (format " %-6d " (car list))))
        (setq list (nthcdr 2 list))
        (while list
          (insert (if (> (current-column) 0) "" "        ")
                  (nth 2 (car (car list)))
                  "\n")
          (setq list (cdr list)))
        (insert "\n"))
      (escreen-menu-mode))))

(defun escreen-menu-mode ()
  (fundamental-mode)
  (kill-all-local-variables)
  (setq buffer-undo-list t)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq major-mode 'escreen-menu-mode)
  (setq mode-name "Escreen Menu")
  (run-hooks 'escreen-menu-mode-hook))


;; Install this by doing
;;
;;    (add-hook 'escreen-goto-screen-hook
;;              'escreen-enable-number-mode-if-more-than-one-screen)
;;
;; By doing so, escreen-number-mode is disabled whenever only a single
;; escreen is in use.  The only reason for doing this, however, is to save
;; valuable mode line real estate.
(defun escreen-enable-number-mode-if-more-than-one-screen ()
  (if (> (length (escreen-get-active-screen-numbers)) 1)
      (escreen-number-mode 1)
    (escreen-number-mode -1))
  (escreen-force-mode-line-update t))


;; Compatiblity tweaks for Emacs 19, 18, and XEmacs.

(cond ((fboundp 'window-edges)
       (fset 'escreen-window-edges 'window-edges))
      (t
       ;; for XEmacs 19.13.
       (fset 'escreen-window-edges 'window-pixel-edges)))

(cond ((fboundp 'force-mode-line-update)
       (fset 'escreen-force-mode-line-update 'force-mode-line-update))
      (t
       (defun escreen-force-mode-line-update (&optional all)
         (and all (save-excursion (set-buffer (other-buffer))))
         (set-buffer-modified-p (buffer-modified-p)))))

;; Emacs 18 doesn't have `member'.
(cond ((fboundp 'member)
       (fset 'escreen-member 'member))
      (t
       (defun escreen-member (x y)
         (while (and y (not (equal x (car y))))
           (setq y (cdr y)))
         y)))


(provide 'escreen)

;;; escreen.el ends here
