;Return-Path: <liebman.LSBGEGPCAD@xerox.com>
;Sender: liebman.LSBGEGPCAD@xerox.com
;Date: 19 May 89 03:25:53 PDT (Friday)
;Subject: RCS additions to dired mode
;From: liebman.LSBGEGPCAD@xerox.com
;To: info-gnu-emacs@prep.ai.mit.edu
;Reply-To: liebman.LSBGEGPCAD@xerox.com
;
;
;Hi,
;   The included elisp code adds the capability check files in and out using the RCS
;revision control system.  I thought that this may be of use for other RCS users.
;As this is my first attempt at writing lisp code please excuse and dumb errors.
;Improvements gladly accepted, flames to /dev/null please!
;
;On another subject, has anyone ported emacs to run under the Xt toolkit rather than the
;Xlib (X11 of course).
;
;Chris Liebman
;liebman.lsbgegpcad@Xerox.Com
;uunet!smsdpg!ra!liebman
;sundc!ra!liebman
;
;------------ Cut here for dired-rcs.el --------------
;;;
;;;    RCS hack for dired.
;;;
;;; By:
;;;    Christopher B. Liebman
;;;    May 16, 1989
;;;
;;;    liebman.lsbgegpcad@Xerox.Com
;;;    uunet!smsdpg!ra!liebman
;;;    sundc!ra!liebman
;;;
;;; If you load this file from your .emacs file then the following key bindings are in
;;; effect in dired mode:
;;;
;;; l = display rcs log for file in other window.
;;; D - run rcsdiff -c on file, show output in other window.
;;; I - pop open a window, accept a log message, and do a "ci -u" on file.
;;; 	(use ^C^C to terminate the log message.)
;;; O - do a "co -l" on file.
;;; L - Lock file.
;;; U - unlock file
;;;

(setq dired-mode-hook 'dired-rcs-setup)

(defun dired-rcs-setup ()
  "Setup rcs key bindings for dired."
  (local-set-key "l" 'dired-rcs-log)
  (local-set-key "D" 'dired-rcs-diff)
  (local-set-key "I" 'dired-rcs-ci)
  (local-set-key "O" 'dired-rcs-co)
  (local-set-key "L" 'dired-rcs-lock)
  (local-set-key "U" 'dired-rcs-unlock))

;;;
;;; command to quit a dired session...
;;;

(defun dired-exit ()
  "Quit editing this directory."
  (interactive)
  (kill-buffer (current-buffer)))

;;;
;;;    RCS interface support.
;;;

;;;
;;; Global variables.
;;;

(defvar rcs-ci-mode-map (make-sparse-keymap)
  "Keymap for creating rcs log messages.")

;;;
;;; Mode hooks:
;;;

(defvar rcs-ci-mode-hook nil
  "*Invoked in rcs-ci-mode on a new log message.")

;;;
;;;    Check out a file optionaly locked.
;;;

(defun rcs-co (filename &optional lock)
  "Check out a file from rcs."
  (if lock
      (rcs-exec "co" "-l" filename)
      (rcs-exec "co"      filename)))

;;;
;;;    Check in a file.
;;;

(defun rcs-ci (filename logmessage)
  "Check in a file to rcs."
  (rcs-exec "ci" "-u" (concat "-m" logmessage) filename))

;;;
;;;    Lock a file using rcs.
;;;

(defun rcs-lock (filename)
  "Lock a file in rcs."
  (rcs-exec "rcs" "-l" filename))

;;;
;;;    Unlock a file using rcs.
;;;

(defun rcs-unlock (filename)
  "Unlock a file in rcs."
  (rcs-exec "rcs" "-u" filename))

;;;
;;;    Read the rcs log messages for a file into a buffer.
;;;

(defun rcs-log (filename)
  "Read the rcs log for filename."
  (pop-to-buffer (generate-new-buffer (concat "rlog: " filename)))
  (erase-buffer)
  (call-process "rlog" nil t nil filename)
  (goto-char (point-min)))

;;;
;;;   Diff the current working file against the last checked in version.
;;;

(defun rcs-diff (filename)
  "Diff the current working file against the last checked in version."
  (pop-to-buffer (generate-new-buffer (concat "rcsdiff: " filename)))
  (erase-buffer)
  (call-process "rcsdiff" nil t nil "-c" filename)
  (goto-char (point-min)))

;;;
;;;   Execute rcs type commads sending error messages to minibuffer with a beep.
;;;

(defun rcs-exec (command &rest options)
  "Execute RCS type command"
  
  ;;
  ;; First create and clear the output buffer.
  ;;
  
  (set-buffer (get-buffer-create "*rcs-errors*"))
  (erase-buffer)
  
  ;;
  ;;    Now execute the command adding the -q (quiet) option.
  ;;
  
  (apply 'call-process command nil t nil "-q" options)
  
  ;;
  ;;    If the buffer is not empty then there was an error.
  ;;
  
  (if (= (point-min) (point-max))
      t
      (message (buffer-substring (point-min) (- (point-max) 1)))
      (beep))
  (kill-buffer (current-buffer)))

;;;
;;;  Create a buffer that has ^C^C bound to rcs-do-checkin
;;;

(defun rcs-checkin (filename)
  "Start the checkin process"
  (interactive)
  (rcs-setup-log-edit filename
		      (generate-new-buffer (concat "rcs-ci: " filename))
					   (current-window-configuration)))

(defun rcs-setup-log-edit (filename buffer config)
  "Start the checkin process"
  (pop-to-buffer buffer)
  (rcs-ci-mode)
  (make-local-variable 'rcs-filename)
  (make-local-variable 'rcs-window-config)
  (setq rcs-filename filename)
  (setq rcs-window-config config))

;;;
;;; A mode for composing rcs checkin logs.
;;;

(defun rcs-ci-mode ()
    "Mode for creating RCS log messages.
\\{rcs-ci-mode-map}"
  (text-mode)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^[ \t]*[-_][-_][-_]+$\\|" paragraph-start))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate
	(concat "^[ \t]*[-_][-_][-_]+$\\|" paragraph-separate))
  (use-local-map rcs-ci-mode-map)
  (setq major-mode 'rcs-ci-mode)
  (setq mode-name "rcs ci")
  (run-hooks 'rcs-ci-mode-hook))

;;;
;;;    Do the checkin.
;;;

(defun rcs-do-checkin ()
  (interactive)
  "do the checkin using current buffer as log message."
  (let ((buffer (current-buffer)) (config rcs-window-config) (filename rcs-filename))
    (rcs-ci filename (buffer-string))
    (set-buffer buffer)
    (set-buffer-modified-p nil)
    (kill-buffer buffer)
    (set-window-configuration config)
    (dired-revert)))

;;;
;;; Dired commands.
;;;

(defun dired-rcs-co ()
  "In dired, checkout a file (locked) with RCS"
  (interactive)
  (let* ((filename (dired-get-filename)) (buffer (current-buffer)) (buffer-read-only nil))
    (rcs-co filename t)
    (set-buffer buffer)
    (dired-redisplay filename)))

(defun dired-rcs-ci ()
  "In dired, checkin a file (locked) with RCS"
  (interactive)
  (rcs-checkin (dired-get-filename)))

(defun dired-rcs-lock ()
  (interactive)
  "In dired, lock a file with RCS"
  (rcs-lock (dired-get-filename)))

(defun dired-rcs-unlock ()
  "In dired, unlock a file with RCS"
  (interactive)
  (rcs-unlock (dired-get-filename)))

(defun dired-rcs-log ()
  "In dired, read an rcs log."
  (interactive)
  (rcs-log (dired-get-filename)))

(defun dired-rcs-diff ()
  "In dired, diff the working file against the latest checked in one."
  (interactive)
  (rcs-diff (dired-get-filename)))

;;;
;;;    Build the ci-mode map.
;;;

(define-key rcs-ci-mode-map "\C-C\C-C" 'rcs-do-checkin)


