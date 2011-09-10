;Return-Path: <talos!kjones@uunet.uu.net>
;Date: Tue, 25 Apr 89 12:02:37 edt
;From: talos!kjones@uunet.uu.net (Kyle Jones)
;To: info-gnu-emacs@prep.ai.mit.edu
;Subject: new version of vkill
;
;It's been a couple of years, so here's the latest version of vkill.  The
;mark commands take prefix args now, some dired compatibie key bindings
;were added, a control variable was added for workstation users, and the
;send signal command now accepts kill(1) style signal names as well as
;numbers.
;
;Scream at the peacocks,
;
;kyle jones   <kjones@talos.UUCP>   ...!uunet!talos!kjones
;----------------------
;;; View and kill Unix processes from within Emacs
;;; Copyright (C) 1987, 1989 Kyle E. Jones
;;;
;;; Verbatim copies of this file may be freely redistributed.
;;;
;;; Modified versions of this file may be redistributed provided that this
;;; notice remains unchanged, the file contains prominent notice of
;;; author and time of modifications, and redistribution of the file
;;; is not further restricted in any way.
;;;
;;; This file is distributed `as is', without warranties of any kind.

;; M-x vkill creates a buffer containing ps(1) output and allows you to
;; mvoe around in it marking processes to be sent a signal.  Type a `?'
;; in the Process Info buffer for more help.
;;
;; The commands vkill and list-unix-processes are the package entry points.
;;
;; To autoload, use
;;     (autoload 'vkill "vkill" nil t)
;;     (autoload 'list-unix-processes "vkill" nil t)
;; in your .emacs file.

(provide 'vkill)

(defvar vkill-show-all-processes nil
  "*Non-nil means to show all processes on the system even if you're not
the superuser.")

(defvar vkill-keymap nil
  "Keymap for vkill commands")

(if vkill-keymap
    ()
  (setq vkill-keymap (make-sparse-keymap))
  (define-key vkill-keymap " " 'next-line)
  (define-key vkill-keymap "n" 'next-line)
  (define-key vkill-keymap "p" 'previous-line)
  (define-key vkill-keymap "\C-?" 'previous-line)
  (define-key vkill-keymap "?" 'vkill-help)
  (define-key vkill-keymap "d" 'vkill-mark-process) ; Dired compatibility
  (define-key vkill-keymap "m" 'vkill-mark-process)
  (define-key vkill-keymap "M" 'vkill-mark-all-processes)
  (define-key vkill-keymap "P" 'vkill-update-process-info)
  (define-key vkill-keymap "g" 'revert-buffer) ; Dired compatibility
  (define-key vkill-keymap "q" 'vkill-quit)
  (define-key vkill-keymap "u" 'vkill-unmark-process)
  (define-key vkill-keymap "U" 'vkill-unmark-all-processes)
  (define-key vkill-keymap "x" 'vkill-send-signal) ; Dired compatibility
  (define-key vkill-keymap "k" 'vkill-send-signal))

(defconst vkill-quick-help-string
  "(n)ext, (p)revious, (m)ark, (u)nmark, (k)ill, (q)uit  (type ? for more help)"
  "Quick help string for vkill.")

(defmacro vkill-signum (n)
  (list 'if (list '> n 0) 1
    (list 'if (list 'zerop n) 0 -1)))

(defmacro vkill-decrement (variable)
  (list 'setq variable (list '1- variable)))

(defun vkill-abs (n) (if (< n 0) (- n) n))

(defun vkill (&optional list)
  "Mode for displaying all UNIX processes owned by the current user
\(all the processes on the system if invoked by the superuser) and allowing
the user to mark processes to be sent a certain signal.  Processes are
marked by moving the cursor to the line displaying information
about the victim process and typing `m' to mark the process.

If invoked with a prefix arg (optional first arg non-nil from a program)
the window displaying the process information will be displayed but not
selected.

Commands:
    SPC, n  - next line
    DEL, p  - previous line

    m, d    - mark process
    u       - unmark process
    M       - mark all processes
    U       - unmark all processes

    P, g    - update process information
    k, x    - send signal to marked processes (signal read from minibuffer)

    ?       - help"
  (interactive "P")
  (let ((vkill-buffer (get-buffer-create "*Process Info*")) new)
    (set-buffer vkill-buffer)
    (setq new (zerop (buffer-size)))
    (cond (new
	   (make-local-variable 'goal-column)
	   (make-local-variable 'revert-buffer-function)
	   (abbrev-mode 0)
	   (auto-fill-mode 0)
	   (setq buffer-read-only t
		 truncate-lines t
		 revert-buffer-function 'vkill-revert
		 major-mode 'vkill-mode
		 mode-name "Vkill"
		 goal-column 0)
	   (use-local-map vkill-keymap)))

    (if (or new list)
	(progn
	  (vkill-update-process-info list)
	  (goto-line 2)))

    (if list
	(display-buffer vkill-buffer)
      (pop-to-buffer vkill-buffer)
      (message "type q to quit, ? for help"))))

(fset 'vkill-mode 'vkill)
(put 'vkill-mode 'mode-class 'special)

(defun list-unix-processes (&optional activate)
  "List UNIX processes owned by the current user using the ps(1) command.
If run by the superuser, all processes are listed.  The buffer used to
display the listing is put into a special major mode similar to Dired
and Buffer Menu; you can mark processes to be sent a signal using this buffer.
See the documentation for `vkill-mode' for more information."
  (interactive "P")
  (vkill t))

(defun vkill-mark-process (&optional count)
  "Mark the process listed on the current line and move forward a line.
With prefix arg COUNT, move forward that many lines, while marking the
corrseponding processes.  A negative COUNT means move backwards."
  (interactive "p")
  (or count (setq count 1))
  (let (buffer-read-only
	(direction (vkill-signum count)))
    (setq count (vkill-abs count))
    (while (and (not (zerop count)) (not (eobp)) (not (bobp)))
      (beginning-of-line)
      (if (not (bobp))
	  (progn
	    (insert "*")
	    (delete-char 1)))
      (forward-line direction)
      (vkill-decrement count))))

(defun vkill-mark-all-processes ()
  "Mark all listed processes."
  (interactive)
  (save-excursion
    (let (buffer-read-only)
      (goto-line 2)
      (while (not (eobp))
	(insert "*")
	(delete-char 1)
	(forward-line)))))

(defun vkill-unmark-all-processes ()
  "Remove marks from all listed processes."
  (interactive)
  (save-excursion
    (let (buffer-read-only)
      (goto-line 2)
      (while (not (eobp))
	(insert " ")
	(delete-char 1)
	(forward-line)))))

(defun vkill-unmark-process (&optional count)
  "Un-mark from the process listed on the current line and move forward a line.
With prefix arg COUNT, move forward that many lines, unmarking the
corresponding processes.  A negative COUNT means move backwards."
  (interactive "p")
  (or count (setq count 1))
  (let (buffer-read-only
	(direction (vkill-signum count)))
    (setq count (vkill-abs count))
    (while (and (not (zerop count)) (not (eobp)) (not (bobp)))
      (beginning-of-line)
      (if (not (bobp))
	  (progn
	    (insert " ")
	    (delete-char 1)))
      (forward-line direction)
      (vkill-decrement count))))

(defun vkill-quit ()
  "End current vkill session without sending a signal to any of the marked
processes."
  (interactive)
  (if (one-window-p)
      (progn
	(switch-to-buffer (other-buffer))
	(bury-buffer (other-buffer)))
    (bury-buffer (current-buffer))
    (delete-window)))

(defun vkill-update-process-info (&optional quietly)
  "Update the vkill process information.  This throws away all process marks."
  (interactive)
  (or quietly (message "Updating process information..."))
  (let (buffer-read-only)
    (erase-buffer)
    (shell-command-on-region (point-min) (point-max)
			     (if (or vkill-show-all-processes
				     (zerop (user-real-uid)))
				 "ps -auxg" "ps -uxg") t)
    (goto-char (point-min))
    (while (not (eobp))
      (insert "  ")
      (forward-line))
    (goto-line 2))
  (or quietly (input-pending-p)
      (message "Updating process information... done.")))

(defun vkill-revert (&rest args)
  (vkill-update-process-info))

(defun vkill-send-signal (signal)
  "Send a SIGNAL to the marked processes.  SIGNAL may be a string (HUP, INT,
etc.) or a number.  When called interactively, SIGNAL is always read from the
minibuffer."
  (interactive "sSignal (default TERM): ")
  (if (equal signal "")
      (setq signal "TERM"))
  (let ((workbuf (get-buffer-create " *vkill*")))
    (copy-to-buffer workbuf (point-min) (point-max))
    (set-buffer workbuf)
    (goto-char (point-min))
    (delete-matching-lines "^  ")
    (goto-char (point-min))
    (if (not (looking-at "\\* "))
	(error "No processes marked"))
    (while (re-search-forward "\\* [^ ]+[ ]+\\([0-9]+\\).*\n" nil t)
      (replace-match " \\1" t nil))
    (goto-char (point-min))
    (insert "kill -" (if (numberp signal) (int-to-string signal) signal))
    (call-process shell-file-name nil 0 nil "-c" (buffer-string))
    (kill-buffer workbuf)))

(defun vkill-help ()
  "Provide help for the vkill user."
  (interactive)
  (if (eq last-command 'vkill-help)
      (describe-mode)
    (message vkill-quick-help-string)))

