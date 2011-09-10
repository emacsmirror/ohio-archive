;;; agchat.el --- comint-based emacs interface for chatting on AgChat

;; Copyright (C) 1995 Noah S. Friedman

;; Author: Noah Friedman <friedman@prep.ai.mit.edu>
;; Maintainer: friedman@prep.ai.mit.edu
;; Keywords: communication, extensions
;; Status: Works in emacs 19.
;; Created: 1995-01-02

;; $Id: agchat.el,v 1.7 1995/08/07 00:35:41 friedman Exp $

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
;; Inc.; 59 Temple Place - Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; AgChat is a chat system written by silver Harloe <silver@eden.com>.

;; Todo:
;;   * automatic login?

;;; Code:

(require 'comint)

(or (featurep 'pb-popup)
    (load "pb-popup" t))


(defvar agchat-buffer-name-format "*agchat-%s*"
  "*Basic buffer name for AgChat sessions.
This string is fed to `format' as the format specifier with the host name
of the chat system, as a string.  If you don't wish for the name to appear
in the buffer name, don't include a `%s' in it.
Also, use `%%' to get a single `%' in the buffer name.")

(defvar agchat-mode-hook nil
  "*Hook to run at the end of agchat-mode.")

(defvar agchat-host "chat.silverchat.com"
  "*A string specifying the internet name or IP address of the agchat host.")

(defvar agchat-port 2317
  "*An integer specifying the TCP port of the agchat server on agchat-host.")

(defvar agchat-output-filter-functions
  '(agchat-default-output-filter agchat-popup)
  "Functions to call after output is inserted into the buffer.
These functions get one argument, a string containing the text just inserted.
Also, the buffer is narrowed to the region of the inserted text.

Note that the string received by these functions contain the raw string as
received by the server; they do not reflect any alterations made by filters.

Some possible functions to add are `agchat-popup-window' and
`agchat-default-output-filter'.")

(defvar agchat-default-output-filter-regexp-list
  '(;; All lines are terminated by a carriage return
    "\C-m"
    ;; strip bells; use a separate filter to beep if desired.
    "\C-g"
    ;; These are telnet handshakes that appears when first connecting.
    "\377\373\C-c"
    "\377\373\C-a"
    ;; This is a "nudge" sent periodically by the server to see if the
    ;; client is still awake; it consists of a space followed by a vt100
    ;; backspace.
    " \e\\[D")
  "*Regular expression identifying substrings to strip from incoming text.
This may consist of characters like carriage returns, backspaces, etc.")

;; Do not change this without changing agchat-color-filter appropriately.
(defconst agchat-color-mode-regexp
  "\e\\[0;3\\([0-9]\\)\\(;1\\)?;4\\([0-9]\\)m")

(defvar agchat-color-table
  '((0 "black"   "black")
    (1 "red"     "bright red")
    (2 "green"   "bright green")
    (3 "yellow"  "bright yellow")
    (4 "blue"    "bright blue")
    (5 "magenta" "bright magenta")
    (6 "cyan"    "bright cyan")
    (7 "white"   "bright white")))


;;; Variables only of use if you have pb-popup.el

(makunbound 'agchat-popup-regexp-list)
(defvar agchat-popup-regexp-list
  '("^#" "\C-g")
  "*List of regular expressions matching events worthy of popup notification.
For any input from the server matching one of these regular expressions,
if the agchat buffer is not presently visible, a new window will be created
displaying it.  See function `agchat-popup-window'.")


;;;###autoload
(defun agchat-mode ()
  "Major mode for agchat sessions.

If the `comint' library is available, `comint-mode' is called to
implement a command history, etc.  Otherwise, `text-mode' is called.
This means either `comint-mode-hook' or `text-mode-hook' may be run, but
almost certainly not both.

It is best to put agchat mode--specific hooks on `agchat-mode-hook'."
  (interactive)
  (comint-mode)

  (make-local-variable 'comint-prompt-regexp)
  (setq comint-prompt-regexp "^")

  (make-local-variable 'comint-input-sender)
  (setq comint-input-sender 'agchat-simple-send)

  (make-local-variable 'comint-process-echoes)
  (setq comint-process-echoes t)

  (setq mode-name "agchat")
  (setq major-mode 'agchat-mode)
  (setq mode-line-process '(":%s"))
  (make-local-variable 'case-fold-search)
  (setq case-fold-search t)
  (make-local-variable 'scroll-step)
  (setq scroll-step 1)

  (run-hooks 'agchat-mode-hook))

;;;###autoload
(defun agchat (host &optional port &optional newp)
  "Connect to a agchat system.
The user is prompted for the host and port number.
The default for each is inserted in the minibuffer to be edited if desired.

With a prefix argument, always create a new chat session even if there is
already an existing connection to that chat.  Otherwise, try to switch to an
existing session on that host."
  (interactive (list (read-from-minibuffer "agchat host: " agchat-host)
                     (read-from-minibuffer "agchat port: "
                                           (number-to-string agchat-port))
                     current-prefix-arg))
  (setq port (string-to-number port)
        agchat-port port
        agchat-host host)
  (let* ((buf-fun (if newp 'generate-new-buffer 'get-buffer-create))
         (buffer (funcall buf-fun (format agchat-buffer-name-format host)))
         (proc (get-buffer-process buffer)))
    (switch-to-buffer buffer)
    (cond
     ((and proc
           (memq (process-status proc) '(run stop open))))
     (t
      (goto-char (point-max))
      (setq proc (open-network-stream "agchat" buffer host port))
      (set-process-buffer proc buffer)
      (set-marker (process-mark proc) (point-max))
      (agchat-mode)
      ;; These must be done after calling agchat-mode (which calls
      ;; comint-mode) since that function may set its own process
      ;; filter and sentinel for this process.
      ;; Also, avoid killing local variables set below.
      (set-process-filter proc 'agchat-filter)
      (set-process-sentinel proc 'agchat-sentinel)
      (cond
       ;; Done for Emacs 19 only.
       ((string-lessp "19" emacs-version)
        (agchat-make-local-variables 'kill-buffer-hook 'pre-command-hook)
        (add-hook 'kill-buffer-hook 'agchat-delete-process)
        (add-hook 'pre-command-hook
                  'agchat-goto-eob-on-insert-before-process-mark)))))))

(defun agchat-filter (proc string)
  (let ((orig-buffer (current-buffer)))
    (set-buffer (process-buffer proc))

    (let* ((saved-point (point-marker))
           (marker (process-mark proc))
           (buffer (process-buffer proc))
           (window (get-buffer-window buffer)))
      (save-restriction
        (widen)
        (narrow-to-region marker marker)

        (goto-char (point-min))
        (insert-before-markers string)
        (and window
             (= (marker-position marker) (window-start window))
             (set-window-start window (point-min) 'noforce))

        (let ((fns agchat-output-filter-functions))
        (while fns
          (goto-char (point-min))
          (funcall (car fns) string)
          (setq fns (cdr fns)))))
      (goto-char saved-point))
    (set-buffer orig-buffer)))

(defun agchat-sentinel (proc event)
  (let ((orig-buffer (current-buffer)))
    (unwind-protect
        (progn
          (set-buffer (process-buffer proc))
          (goto-char (point-max))
          (insert (format "\n\nProcess %s %s\n" proc event)))
      (set-buffer orig-buffer)))
  (agchat-delete-process proc))

(defun agchat-delete-process (&optional proc)
  (or proc
      (setq proc (get-buffer-process (current-buffer))))
  (and (processp proc)
       (delete-process proc)))

;; This should be added to pre-command-hook.
(defun agchat-goto-eob-on-insert-before-process-mark (&optional proc)
  (or proc (setq proc (get-buffer-process (current-buffer))))
  (cond
   ((null proc))
   ((not (eq this-command 'self-insert-command)))
   ((>= (point) (process-mark proc)))
   (t
    (goto-char (point-max)))))

(defun agchat-make-local-variables (&rest symlist)
  (let (sym)
    (while symlist
      (setq sym (car symlist))
      (cond
       ((assq sym (buffer-local-variables)))
       ((and (boundp sym)
             (sequencep sym))
        (make-local-variable sym)
        (set sym (copy-sequence (default-value sym))))
       (t
        (make-local-variable sym)))
      (setq symlist (cdr symlist)))))


(defun agchat-default-output-filter (&optional string)
  (let ((re agchat-default-output-filter-regexp-list))
    (while re
      (goto-char (point-min))
      (while (re-search-forward (car re) nil t)
        (delete-region (match-beginning 0) (match-end 0)))
      (setq re (cdr re)))))


(defun agchat-color-filter (&optional string)
  (goto-char (point-min))
  (while (re-search-forward agchat-color-mode-regexp nil t)
    (let* ((beg (match-beginning 0))
           (end (point-max))
           (s (agchat-match-string 0))
           (foreground (string-to-int (agchat-match-string 1)))
           (highlight (if (match-beginning 2) 1 0))
           (background (string-to-int (agchat-match-string 3))))

      ;; Get rid of the escape sequences; we no longer need them.
      (delete-region beg (match-end 0))

      (and (re-search-forward agchat-color-mode-regexp nil t)
           (setq end (point)))

      (agchat-overlay-region beg end foreground highlight background)
      (goto-char beg))))

;; This is not finished.
(defun agchat-overlay-region (beg end fore high back)
  (let ((ovl (make-overlay beg end))
)))

(defun agchat-popup (s)
  (and (fboundp 'pb-popup)
       (let ((re agchat-popup-regexp-list)
             (data (match-data)))
         (while re
           (cond
            ((string-match (car re) s)
             (setq re nil)
             (pb-popup (current-buffer)))
            (t
             (setq re (cdr re)))))
         (store-match-data data))))

;; comint-input-sender is set to this function, so that it is called by
;; comint-send-input.  This differs from comint-simple-send in that it
;; terminates the line with a carriage return instead of a linefeed;
;; although Eden Agchat can be toggled to map LF->CR, that makes it
;; impossible to send multi-line input.
(defun agchat-simple-send (proc string)
  (comint-send-string proc string)
  (comint-send-string proc "\r"))

(defun agchat-match-string (n &optional str)
  (and (match-beginning n)
       (if str
           (substring str (match-beginning n) (match-end n))
         (buffer-substring (match-beginning n) (match-end n)))))


(provide 'agchat)

;; agchat.el ends here.
