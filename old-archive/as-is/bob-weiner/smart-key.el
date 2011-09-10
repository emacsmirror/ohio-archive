From gatech!uflorida!novavax!weiner@bbn.com Fri May 19 14:37:22 1989
To: unix-emacs@bbn.com
Date: 28 Apr 89 21:29:12 GMT
From: Bob Weiner <gatech!uflorida!novavax!weiner@bbn.com>
Sender: arpa-unix-emacs-request@bbn.com
Subject: smart-key.el, one key does it all in many major modes
Organization: Nova University, Fort Lauderdale, FL
Source-Info:  From (or Sender) name not authenticated.

;;!emacs
;;
;; FILE:         smart-key.el
;; SUMMARY:      Use key or mouse key to perform multiple functions
;;                 that vary by major mode.  See the function doc strings
;;                 for precise information.
;; USAGE:        GNU Emacs Lisp Library
;;
;; AUTHOR:       Bob Weiner, Applied Research, Motorola, Inc.
;; E-MAIL:       USENET:  weiner@novavax.UUCP
;; ORIG-DATE:    04-Feb-89
;; LAST-MOD:     28-Apr-89 at 00:33:50 by Bob Weiner
;;
;; Copyright (C) 1989 Bob Weiner and Free Software Foundation, Inc.
;; Available for use and distribution under the same terms as GNU Emacs.
;;
;; This file is not yet part of GNU Emacs.
;;
;; Needs a 'smart-rnews' module; this should function somewhat like the
;; 'smart-rmail' functions.
;;
;; Using the smart-key with a mouse to browse through and delete files from
;; Dired listings is exceptionally nice, just as it is in Rmail.
;;
;; DESCRIPTION:  
;;;
;;; This code is machine independent.  It works best with a pointing device but
;;; may also be used from a keyboard.  When used with a pointing device it
;;; requires an Emacs command that sets point to the location of the pointing
;;; device's cursor.
;;;
;;; NOTE:  This is a complete superset replacement for 'info-mouse'.  If you
;;;        are currently using this package: replace all uses of
;;;        'mouse-non-info-command' with 'smart-key-other-mode-cmd' and
;;;        'mouse-meta-non-info-command' with
;;;        'smart-key-meta-other-mode-cmd' in your init files.  Then
;;;        replace all mouse key bindings of 'Info-mouse' with
;;;        'smart-key-mouse' and 'Info-mouse-meta' with 'smart-key-mouse-meta'
;;;        and all loads of 'Info-mouse' with 'smart-info'.  Then follow the
;;;        installation instructions, most of which you will not need to
;;;        perform.
;;;
;;; To install:
;;;
;;; In your <GNUEMACS-LISP-LOCAL> directory, add this file and byte
;;; compile it.  Make sure that this directory is in your 'load-path'
;;; variable; we use the following lines in our
;;; <GNUEMACS-LISP-LOCAL>/{site-load,default}.el files:
;;;
;;;   ;; Add "lisp-local" directory to front of default library search
;;;   ;; path list 
;;;   (setq load-path '("/usr/local/gnu/emacs/lisp-local"
;;;                     "/usr/local/gnu/emacs/lisp")) 
;;;
;;; In your "site-load.el" file, add the line:
;;;
;;;   (load "smart-key")
;;;
;;; See the doc for the variable 'mouse-set-point-command' in this
;;; file and set this to an appropriate value in one of your Emacs init files
;;; if you plan to use these functions with a pointing device.
;;;
;;; In your "site-init.el" file, add something akin to:
;;;
;;;   ;; Perform Apollo-specific setup using Zubkoff's Apollo DM
;;;   ;; extensions 
;;;   (let ((term (getenv "TERM")))
;;;     (if (equal (substring (getenv "TERM") 0
;;;                   (string-match "[-_]" (getenv "TERM")))
;;;                "apollo"))
;;;         ;;
;;;         ;; The following settings are used by routines in
;;;         ;; <GNUEMACS-DIR>/lisp-local/smart-key.el
;;;         ;;
;;;         (setq mouse-set-point-command 'apollo-mouse-move-point)
;;;         (bind-apollo-mouse-button "M2D"
;;;                                   'smart-key
;;;                                   'smart-key-meta)
;;;         (unbind-apollo-mouse-button "M2U")))
;;;
;;;  OR
;;;
;;;   ;; Perform Sun-specific setup
;;;   ;; THIS SETUP NOT TESTED
;;;   ;; Make sure that UP transition of mouse key is unbound !
;;;
;;;   (let ((term (getenv "TERM")))
;;;     (if (equal term "sun")
;;;     (if (equal (substring (getenv "TERM") 0
;;;                   (string-match "[-_]" (getenv "TERM")))
;;;                "sun")
;;;         ;;
;;;         ;; The following settings are used by routines in
;;;         ;; <GNUEMACS-DIR>/lisp-local/smart-key.el
;;;         ;;
;;;         (setq mouse-set-point-command 'mouse-move-point)
;;;         (global-set-mouse '(text        middle) 'smart-key)
;;;         (global-set-mouse '(text meta        middle) 'smart-key-meta))))
;;;
;;;
;;;   If you want your mouse key or smart key to perform another function
;;;   when not within any of the major modes specified here, then set
;;;   'smart-key-other-mode-cmd' to the function you do want it to perform.
;;;   Use 'smart-key-meta-other-mode-cmd' to bind the meta-key.
;;;   Look at the default bindings below, then if you want to change them
;;;   in your ~/.emacs file add something like the following:
;;;
;;;         (setq smart-key-other-mode-cmd 'info)
;;;         (setq smart-key-meta-other-mode-cmd 'list-buffers)
;;;
;;;
;;; NOTE:  You should add the expression: (run-hooks 'info-mode-hook) at
;;;        the end of the 'Info-mode' function in info.el and then byte-compile
;;;        it.  I do not know why it is not there, but it is used by the code
;;;        in this file.
;;;
;;; NOTE:  You should add the expression: (run-hooks 'unix-apropos-mode-hook) at
;;;        the end of the 'unix-apropos-mode' function in either
;;;        unix-apropos.el or man-apropos.el if you have either file.  Then
;;;        byte-compile it.  This hook variable is used by the code
;;;        in this file.  If you don't have this code, you should eliminate all
;;;        of the 'unix-apropos' and 'man-apropos' references in this file.
;;;
;;; NOTE:  You should change the 'manual-entry' command in "man.el" to set its
;;;        buffer read-only.  This allows you to scroll through such entries
;;;        with your smart key.  The end of this command should look like this
;;;        when you are done:
;;;
;;;          (set-buffer-modified-p nil)
;;;          ;; Next line added by <WHOMEVER YOU ARE>
;;;          (setq buffer-read-only t)
;;;          (message ""))))
;;;
;;; NOTE:  You should add the following hook variable evaluation lines:
;;;        These should be part of the standard code, but are not.
;;;        These hooks are used by the code in this file.
;;;
;;;        In "info.el" as the last line in the 'Info-mode' function, add:
;;;
;;;          (run-hooks 'info-mode-hook)
;;;
;;;        In "unix-apropos.el" or "man-apropos.el" if you have either, add the
;;;        following as the last line of the 'unix-apropos-mode' function:
;;;
;;;          (run-hooks 'unix-apropos-mode-hook))
;;;
;;;
;;; NOTE:  You should change the 'view-mode' function in "view.el" to set its
;;;        major-mode.  This should be part of the standard code, but isn't.
;;;        This allows you to scroll through buffers in view-mode with your
;;;        smart key. Near the beginning of this command should look like this
;;;        when you are done: 
;;;
;;;          (unwind-protect
;;;	     (let ((buffer-read-only t)
;;;            ;; Next 2 lines added by <WHOMEVER YOU ARE>
;;;	       (major-mode 'view-mode)
;;;	       (mode-name "View")
;;;
;;; NOTE:  I have changed the names of many of the functions in cal.el.
;;;        You will probably have to slightly modify the code in the
;;;        'smart-calendar' section so that it uses the default function names
;;;        or change the names to fit this code.
;;;
;;;
;;; The following are general purpose, local function definitions used in this
;;; package, uncomment them and add them to a local Emacs init file.
;;;
;;; ************************************************************************
;;;
;;; (defun append-to-var (var-symbol-name list-to-add)
;;;   "If VAR-SYMBOL-NAME is bound, append LIST-TO-ADD (a list) to value of
;;; variable (a list) given by var-symbol-name.  If unbound, variable is set
;;; to list-to-add.  Often used to append to 'hook' variables."
;;;   (set-variable var-symbol-name
;;;		(append (and (boundp var-symbol-name)
;;;			     (symbol-value var-symbol-name))
;;;			list-to-add)))
;;;
;;; (defun first-line-p ()
;;;   "Returns true if point is on the first line of the buffer."
;;;   (save-excursion
;;;     (beginning-of-line)
;;;     (bobp)))
;;; 
;;; (defun last-line-p ()
;;;   "Returns true if point is on the last line of the buffer."
;;;   (save-excursion
;;;     (end-of-line)
;;;     (eobp)))
;;;
;;; Appended here for those who have Leonard Zubkoff's GNU Emacs 18.52
;;; extensions for Apollo's DM window system is a function that is not
;;; included in that distribution that unbinds a mouse key.
;;; It is referred to in the Apollo installation notes above.
;;;
;;;(defun unbind-apollo-mouse-button (mouse-button)
;;;  "Disable an Apollo Mouse Key and return its control to DM."
;;;  (interactive "sMouse Key: ")
;;;  (let ((numeric-code (cdr (assoc mouse-button *apollo-mouse-buttons*))))
;;;    (if (null numeric-code)
;;;	(error "%s is not a legal Apollo function key name" mouse-button))
;;;    (if (stringp numeric-code)
;;;	(setq numeric-code
;;;	      (cdr (assoc numeric-code *apollo-mouse-buttons*))))
;;;    (disable-apollo-mouse-button numeric-code)))
;;;
;;; ************************************************************************
;;;
;;; If Emacs dumps on your system, rebuild it.
;;;
;;; Run 'emacs'.  The key that you have bound will perform various
;;; functions in the named major modes; otherwise it will use the
;;; 'smart-key-other-mode-cmd' variable.
;;;
;;; For terminals without mice, {M-RTN} will run 'smart-key-meta'.  With an
;;; argument, it will run 'smart-key'.  In most special read-only modes with
;;; their own keymaps, in fact all that I have set up here, one can use {RTN}
;;; to run 'smart-key' and {M-RTN} to run 'smart-key-meta'.
;;;
;; DESCRIP-END.

(require 'smart-menu)

;; ************************************************************************
;; Global smart-key key bindings
;; ************************************************************************

; Make smart menu of subsystems available from keyboard
(global-set-key "\C-hh" 'smart-menu)
(global-set-key "\C-x4s" 'smart-menu)

(global-set-key
  "\M-\C-m"
  '(lambda (arg)
     (interactive "P")
     (funcall (if arg 'smart-key 'smart-key-meta))))

;; ************************************************************************
;; User configurable smart-key variable definitions
;; ************************************************************************

(defvar mouse-set-point-command nil
  "*Command that sets point to mouse cursor position.")

(defvar smart-key-other-mode-cmd 'smart-menu
  "*Command to run when key bound to 'smart-key' is pressed in a buffer with an
unspecified major mode.")

(defvar smart-key-meta-other-mode-cmd 'shell
  "*Command to run when meta-key bound to 'smart-key-meta' is pressed in a
buffer with an unspecified major mode.")

;; ************************************************************************
;; smart-key support functions
;; ************************************************************************

;; Most 'smart' functions use the end-of-line position to scroll a buffer up or
;; down a screen.  These next two functions are used to keep point at the end
;; of line when using a keyboard key and meta-key to execute 'smart' functions.
;; Each subsequent push of such a key repeats the scroll action.

(defun scroll-up-eol ()
  (scroll-up)
  (end-of-line))

(defun scroll-down-eol ()
  (scroll-down)
  (end-of-line))

;; ************************************************************************
;; smart-key driver functions
;; ************************************************************************

(defun smart-key-mouse ()
  "Sets point to the current mouse cursor position and executes 'smart-key'."
  (interactive)
  (smart-key t))

(defun smart-key-mouse-meta ()
  "Sets point to the current mouse cursor position and executes 'smart-key-meta'."
  (interactive)
  (smart-key-meta t))

(defun smart-key (&optional mouse)
  "Use one key to perform functions that vary by buffer.
Default function is given by 'smart-key-other-mode-cmd' variable.
Use \\[smart-menu] to pop up a menu whose items are selected with the smart
key.  Each item typically displays a subsystem buffer in which the smart key is
useful.

BE SURE TO CLEAR ANY BINDING OF THE 'UP' TRANSITION OF ANY MOUSE KEY TO WHICH
YOU USE BIND THIS COMMAND.

Optional argument MOUSE non-nil sets point to current mouse cursor position.

Returns nil if 'mouse-set-point-command' or 'smart-key-other-mode-cmd'
variables are not bound to valid functions when needed."
  (interactive)
  (if (and mouse (not (fboundp mouse-set-point-command)))
      nil
    ;;
    ;; Set point to cursor position if called via mouse key press
    ;;
    (and mouse (funcall mouse-set-point-command))
    ;;
    ;; Branch on current buffer
    ;;
    (cond ((eq major-mode 'smart-menu-mode) (smart-menu-select))
	  ((eq major-mode 'Info-mode) (smart-info))
	  ((eq major-mode 'unix-apropos-mode)
	   (smart-man))
	  ((or (eq major-mode 'rmail-mode)
	       (eq major-mode 'rmail-summary-mode))
	   (smart-rmail))
	  ((eq major-mode 'Buffer-menu-mode) (smart-buffer-menu))
	  ((eq major-mode 'dired-mode) (smart-dired))
	  ((eq major-mode 'view-mode) (View-scroll-lines-forward))
	  ((eq major-mode 'calendar-mode) (smart-calendar))
	  (buffer-read-only (scroll-up))
	  ((fboundp smart-key-other-mode-cmd)
	   (funcall smart-key-other-mode-cmd)))))

(defun smart-key-meta (&optional mouse)
  "Use one meta-key to perform functions that vary by buffer.
Default function is given by 'smart-key-meta-other-mode-cmd' variable.
Use \\[smart-menu] to pop up a menu whose items are selected with the smart
key.  Each item typically displays a subsystem buffer in which the smart key is
useful.  This command exits from the menu.

BE SURE TO CLEAR ANY BINDING OF THE 'UP' TRANSITION OF ANY MOUSE KEY TO WHICH
YOU USE BIND THIS COMMAND.

Optional argument MOUSE non-nil sets point to current mouse cursor position.

Returns nil if 'mouse-set-point-command' or 'smart-key-other-mode-cmd'
variables are not bound to valid functions when needed."
  (interactive)
  (if (and mouse (not (fboundp mouse-set-point-command)))
      nil
    ;;
    ;; Set point to cursor position if called via mouse key press
    ;;
    (and mouse (funcall mouse-set-point-command))
    ;;
    ;; Branch on current buffer
    ;;
    (cond ((eq major-mode 'smart-menu-mode) (smart-menu-quit))
	  ((eq major-mode 'Info-mode) (smart-info-meta))
	  ((eq major-mode 'unix-apropos-mode)
	   (smart-man-meta))
	  ((or (eq major-mode 'rmail-mode)
	       (eq major-mode 'rmail-summary-mode))
	   (smart-rmail-meta))
	  ((eq major-mode 'Buffer-menu-mode) (smart-buffer-menu-meta))
	  ((eq major-mode 'dired-mode) (smart-dired-meta))
	  ((eq major-mode 'view-mode) (View-scroll-lines-backward))
	  ((eq major-mode 'calendar-mode) (smart-calendar-meta))
	  (buffer-read-only (scroll-down))
	  ((fboundp smart-key-meta-other-mode-cmd)
	   (funcall smart-key-meta-other-mode-cmd)))))


;; ************************************************************************
;; smart-buffer-menu functions
;; ************************************************************************

(let ((proc
	'((lambda ()
	    (define-key Buffer-menu-mode-map "\C-m" 'smart-buffer-menu)
	    (define-key Buffer-menu-mode-map "\M-\C-m"
	      'smart-buffer-menu-meta)))))
  (if (boundp 'Buffer-menu-mode-map)
      (eval proc)
    (append-to-var 'buffer-menu-mode-hook proc)))

(defun smart-buffer-menu ()
  "Uses a single key or mouse key to manipulate buffer-menu entries.

Invoked via a key press when in Buffer-menu-mode.  It assumes that its
caller has already checked that the key was pressed in an appropriate buffer
and has moved the cursor there.

If key is pressed:
 (1) on the first column of an entry, the selected buffer is marked for
     display; 
 (2) on the second column of an entry, the selected buffer is marked to be
     saved;
 (3) at the end of an entry line, the buffer menu is scrolled up one screen;
 (4) anywhere else within an entry line, all saves and deletes are done, and
     selected buffers are displayed, including the one just clicked on;
 (5) on or after the last line in the buffer, all saves and deletes are done."

  (interactive)
  (cond ((last-line-p) (Buffer-menu-execute))
	((bolp) (Buffer-menu-mark))
	((eolp) (scroll-up-eol))
        ((save-excursion
             (goto-char (1- (point)))
	     (bolp))
	 (Buffer-menu-save))
	((Buffer-menu-select))))

(defun smart-buffer-menu-meta ()
  "Uses a single meta-key or mouse meta-key to manipulate buffer-menu entries.

Invoked via a meta-key press when in Buffer-menu-mode.  It assumes that its
caller has already checked that the meta-key was pressed in an appropriate
buffer and has moved the cursor there.

If meta-key is pressed:
 (1) on the first or second column of an entry, the selected buffer is unmarked
     for display and for saving or deletion; 
 (2) at the end of an entry line, the buffer menu is scrolled down one screen;
 (3) anywhere else within an entry line, the selected buffer is marked for
     deletion;
 (4) on or after the last line in the buffer, all display, save, and delete
     marks on all entries are undone."

  (interactive)
  (cond ((last-line-p) (progn (list-buffers) (forward-line 3)))
	((bolp) (Buffer-menu-unmark))
	((eolp) (scroll-down-eol))
        ((save-excursion
             (goto-char (1- (point)))
	     (bolp))
	 (Buffer-menu-unmark))
	((Buffer-menu-delete))))


;; ************************************************************************
;; smart-info require functions
;; ************************************************************************

(require 'smart-info)
(let ((proc
	'((lambda ()
	    (define-key Info-mode-map "\C-m" 'smart-info)
	    (define-key Info-mode-map "\M-\C-m" 'smart-info-meta)))))
  (if (boundp 'Info-mode-map)
      (eval proc)
    (append-to-var 'info-mode-hook proc)))

;; ************************************************************************
;; smart-man functions
;; ************************************************************************

;; You may have what I call 'man-apropos.el' as 'unix-apropos.el' on
;; your system.
;;
(let ((proc
	'((lambda ()
	    (define-key unix-apropos-map "\C-m" 'smart-man)
	    (define-key unix-apropos-map "\M-\C-m" 'smart-man-meta)))))
  (if (boundp 'unix-apropos-map)
      (eval proc)
    (append-to-var 'unix-apropos-mode-hook proc)))

(defun smart-man ()
  "Moves through UNIX man apropos listings by using one key or mouse key.

Invoked via a key press when in unix-apropos-mode.  It assumes that
its caller has already checked that the key was pressed in an appropriate
buffer and has moved the cursor to the selected buffer.

If key is pressed:
 (1) on a UNIX man apropos entry, the man page for that entry is displayed in
     another window;
 (2) on or after the last line, the buffer in the other window is scrolled up
     one screen;
 (3) anywhere else, the window clicked in is scrolled up one screen."

  (interactive)
  (cond ((last-line-p) (scroll-other-window))
	((eolp) (scroll-up-eol))
	((unix-apropos-get-man))))

(defun smart-man-meta ()
  "Moves through UNIX man apropos listings by using one meta-key or mouse meta-key.

Invoked via a meta-key press when in unix-apropos-mode.  It assumes that
its caller has already checked that the meta-key was pressed in an appropriate
buffer and has moved the cursor to the selected buffer.

If meta-key is pressed:
 (1) on a UNIX man apropos entry, the man page for that entry is displayed in
     another window;
 (2) on or after the last line, the buffer in the other window is scrolled down
     one screen;
 (3) anywhere else, the window clicked in is scrolled down one screen."

  (interactive)
  (cond ((last-line-p) (scroll-other-window (- 3 (window-height))))
	((eolp) (scroll-down-eol))
	((unix-apropos-get-man))))


;; ************************************************************************
;; smart-rmail functions
;; ************************************************************************

(let ((proc
	'((lambda ()
	    (define-key rmail-mode-map "\C-m" 'smart-rmail)
	    (define-key rmail-mode-map "\M-\C-m" 'smart-rmail-meta)))))
  (if (boundp 'rmail-mode-map)
      (eval proc)
    (append-to-var 'rmail-mode-hook proc)))

(let ((proc
	'((lambda ()
	    (define-key rmail-summary-mode-map "\C-m" 'smart-rmail)
	    (define-key rmail-summary-mode-map "\M-\C-m"
	      'smart-rmail-meta)))))
  (if (boundp 'rmail-summary-mode-map)
      (eval proc)
    (append-to-var 'rmail-summary-mode-hook proc)))

(defun smart-rmail ()
  "Uses a key or mouse key to move through Rmail messages and summaries.

Invoked via a key press when in rmail-mode or rmail-summary-mode.
It assumes that its caller has already checked that the key was pressed in an
appropriate buffer and has moved the cursor to the selected buffer.

If key is pressed within:
 (1) an RMAIL buffer, on or after the last line, the next undeleted message is
     displayed;
 (2) an RMAIL buffer at the end of a message line, the window is scrolled up one
     screen; 
 (3) an RMAIL-summary buffer on a header entry, the message corresponding to
     the header is displayed in the RMAIL buffer window;
 (4) an RMAIL-summary buffer, on or after the last line, the messages marked
     for deletion are expunged;
 (5) an RMAIL-summary buffer at the end of a header line, the window is
     scrolled up one screen."

  (interactive)
  ;;
  ;; Branch on buffer type
  ;;
  (cond ((eq major-mode 'rmail-mode)
	 (if (last-line-p)
	     (rmail-next-undeleted-message 1)
	   (scroll-up)))
	;;
	;; Assume are in RMAIL-summary buffer
	;;
	((last-line-p) (rmail-summary-expunge))
	((eolp) (scroll-up-eol))
	((rmail-summary-goto-msg))))

(defun smart-rmail-meta ()
  "Uses a meta-key or mouse meta-key to move through Rmail messages and summaries.

Invoked via a meta-key press when in rmail-mode or rmail-summary-mode.
It assumes that its caller has already checked that the meta-key was pressed in
an appropriate buffer and has moved the cursor to the selected buffer.


If meta-key is pressed within:
 (1) an RMAIL buffer, on or after the last line, the previous undeleted message is
     displayed;
 (2) an RMAIL buffer at the end of a message line, the window is scrolled down one
     screen; 
 (3) an RMAIL-summary buffer on a header entry, the message corresponding to
     the header is marked as deleted;
 (4) an RMAIL-summary buffer, on or after the last line, all messages are marked
     undeleted;
 (5) an RMAIL-summary buffer at the end of a header line, the window is
     scrolled down one screen."

  (interactive)
  ;;
  ;; Branch on buffer type
  ;;
  (cond ((eq major-mode 'rmail-mode)
	 (if (last-line-p)
	     (rmail-previous-undeleted-message 1)
	   (scroll-down)))
	;;
	;; Assume are in RMAIL-summary buffer
	;;
	((last-line-p) (rmail-summary-undelete-many))
	((eolp) (scroll-down-eol))
	((rmail-summary-delete-forward))))


;; ************************************************************************
;; smart-dired functions
;; ************************************************************************

(let ((proc
	'((lambda ()
	    (define-key dired-mode-map "\C-m" 'smart-dired)
	    (define-key dired-mode-map "\M-\C-m" 'smart-dired-meta)))))
  (if (boundp 'dired-mode-map)
      (eval proc)
    (append-to-var 'dired-mode-hook proc)))

(defun smart-dired ()
  "Uses a single key or mouse key to manipulate directory entries.

Invoked via a key press when in dired-mode.  It assumes that its
caller has already checked that the key was pressed in an appropriate buffer
and has moved the cursor there.

If key is pressed:
 (1) at the end of an entry line, the dired buffer is scrolled up one screen;
 (2) anywhere else within an entry line, the selected buffer is displayed for
     viewing;
 (3) on or after the last line in the buffer, all deletes are performed after
     user verification."

  (interactive)
  (cond ((last-line-p) (dired-do-deletions))
	((eolp) (scroll-up-eol))
	((dired-view-file))))

(defun smart-dired-meta ()
  "Uses a single meta-key or mouse meta-key to manipulate directory entries.

Invoked via a meta-key press when in dired-mode.  It assumes that its
caller has already checked that the meta-key was pressed in an appropriate
buffer and has moved the cursor there.

If meta-key is pressed:
 (1) on a '~' character, all backup files in the directory are marked for
     deletion;
 (2) on a '#' character, all auto-save files in the directory are marked for
     deletion;
 (3) at the end of an entry line, the dired buffer is scrolled down one screen;
 (4) anywhere else within an entry line, the selected buffer is marked for
     deletion;
 (5) on or after the last line in the buffer, all delete marks on all entries
     are undone."

  (interactive)
  (cond ((last-line-p)
	 (progn (dired-unflag (- (goto-char (point-max))))
		(goto-char (point-max))))
	((eolp) (scroll-down-eol))
	((looking-at "~") (dired-flag-backup-files))
	((looking-at "#") (dired-flag-auto-save-files))
	((dired-flag-file-deleted 1))))

;; ************************************************************************
;; smart-calendar functions
;;
;; NOTE: I have changed the names of many of the functions in cal.el.
;;       You probably will have to slightly modify this code to use the default
;;       names or change the names to fit this code.
;; ************************************************************************

(let ((proc
	'((lambda ()
	    (define-key calendar-mode-map "\C-m" 'smart-calendar)
	    (define-key calendar-mode-map "\M-\C-m" 'smart-calendar-meta)))))
  (if (boundp 'calendar-mode-map)
      (eval proc)
    (append-to-var 'calendar-mode-hook proc)))

(defun smart-calendar ()
  "Uses a single key or mouse key to manipulate the scrolling calendar.

Invoked via a key press when in calendar-mode.  It assumes that its
caller has already checked that the key was pressed in an appropriate buffer
and has moved the cursor there.

If key is pressed:
 (1) at the end of a line, the calendar is scrolled forward 3 months;
 (2) to the left of any dates on a calendar line, the calendar is scrolled
     backward 3 months;
 (3) on a date, the diary entries for the date, if any, are displayed."

  (interactive)
  (cond ((eolp) (calendar-point-to-nearest-date)
	 (calendar-shift-left-three-months 1))
	((< (current-column) 5) (calendar-point-to-nearest-date)
	 (calendar-shift-right-three-months 1))
	((progn (calendar-point-to-nearest-date)
		(calendar-view-diary-entries 1)))))

(defun smart-calendar-meta ()
  "Uses a single meta-key or mouse meta-key to manipulate the scrolling calendar.

Invoked via a meta-key press when in calendar-mode.  It assumes that its
caller has already checked that the meta-key was pressed in an appropriate
buffer and has moved the cursor there.

If meta-key is pressed:
 (1) at the end of a line, the calendar is scrolled backward 3 months;
 (2) to the left of any dates on a calendar line, the calendar is scrolled
     forward 3 months;
 (3) anywhere else, all dates with marking diary entries are marked in the
     calendar window."

  (interactive)
  (cond ((eolp) (calendar-point-to-nearest-date)
	 (calendar-shift-right-three-months 1))
	((< (current-column) 5) (calendar-point-to-nearest-date)
	 (calendar-shift-left-three-months 1))
	((calendar-mark-diary-entries))))

(provide 'smart-key)

;; ************************************************************************
;; end of smart-key library
;; ************************************************************************
-- 
Bob Weiner, Motorola, Inc.,   USENET:  ...!gatech!uflorida!novavax!weiner
(407) 738-2087


