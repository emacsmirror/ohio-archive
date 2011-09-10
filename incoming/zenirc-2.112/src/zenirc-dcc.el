;;; zenirc-dcc.el --- CTCP DCC module for ZenIRC

;; Copyright (C) 1993, 1994 Ben A. Mesander
;; Copyright (C) 1995 Noah S. Friedman
;; Copyright (C) 1998 Per Persson

;; Author: Ben A. Mesander <ben@gnu.ai.mit.edu>
;;         Noah Friedman <friedman@prep.ai.mit.edu>
;;         Per Persson <pp@sno.pp.se>
;; Maintainer: pp@sno.pp.se
;; Keywords: zenirc, extensions
;; Created: 1994-01-23

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
;; Inc.; 675 Massachusetts Avenue; Cambridge, MA 02139, USA.

;;; Commentary:

;; DCC `send' isn't yet implemented because emacs does not provide server
;; sockets.  One way to get around this would be to write a small C or perl
;; program to do the listening and interface it with emacs.

;; This file is divided into 4 sections:
;;   * Variables related to all DCC operations
;;   * Misc utility macros and functions
;;   * Interactive command handling
;;   * Server message handling (i.e. messages from remote users)
;;   * GET handling
;;   * CHAT handling

;;; Code:

(require 'zenirc)

;; zenirc-dcc-alist looks like:
;;   (("nick!user@host" GET  proc-or-nil ip-address port size filename)
;;    ("nick!user@host" CHAT proc-or-nil ip-address port))
(defvar zenirc-dcc-alist nil)

(defvar zenirc-verbose-dcc t)

;; This function takes all the usual args as open-network-stream, plus one
;; more: the entry data from zenirc-dcc-alist for this particular process.
;; Emacs 18 cannot open-network-stream on IP numbers, so instead the
;; hostname from the nick must be used; of course, these may not equivalent,
;; but there's nothing that can be done about that.  Use emacs 19.
(defvar zenirc-dcc-connect-function 'zenirc-dcc-open-network-stream)


;;; Section 2: Misc macros and utility functions

(defmacro zenirc-dcc-catalog-entry (sym)
  `(zenirc-lang-retrieve-catalog-entry ,sym))

(defun zenirc-dcc-install-message-catalogs ()
  (zenirc-lang-define-catalog 'english
   '((dcc-chat-discarded . "[dcc] note: previous chat request from %s discarded.\n")
     (dcc-chat-ended . "[dcc] chat with %s ended %s: %s\n")
     (dcc-chat-no-request . "[dcc] chat request from %s not found.\n")
     (dcc-chat-offered . "[dcc] chat offered by %s\n")
     (dcc-chat-privmsg . "=%s= %s\n")
     (dcc-closed . "[dcc] Closed %s from %s%s\n")
     (dcc-command-undefined . "[dcc] no such subcommand: %s\n")
     (dcc-ctcp-errmsg . "`%s' is not a DCC subcommand known to this client.")
     (dcc-ctcp-unknown . "[info] unknown dcc subcommand `%s' from %s\n")
     (dcc-get-bytes-received . "[dcc] %s: %d bytes received.")
     (dcc-get-complete . "[dcc] file %s transfer complete (%d bytes).\n")
     (dcc-get-file-exists . "[dcc] File %s exists.  Overwrite? ")
     (dcc-get-file-too-long . "[dcc] %s: File longer than sender claimed; aborting transfer.")
     (dcc-get-filename . "[dcc] Local filename (default %s): ")
     (dcc-get-notfound . "[error] %s hasn't offered %s for DCC transfer.\n")
     (dcc-list-end  . "[dcc] End of list.\n")
     (dcc-list-head . "[dcc] From      Type Active Size Filename\n")
     (dcc-list-item . "[dcc] %-9s %4s %-6s %4s %s\n")
     (dcc-list-line . "[dcc] ----      ---- ------ ---- --------\n")
     (dcc-malformed . "[dcc] error: %s sent malformed `%s' request: %s\n")
     (dcc-privileged-port . "[dcc] possibly bogus request: %s is a privileged port.\n")
     (dcc-request-bogus . "[dcc] bogus dcc `%s' from user %s\n")
     (dcc-send-offered . "[dcc] file %s offered by user %s (size %s)\n")
     )))

;; Return the elt of alist which contains nick and type, e.g.
;; (zenirc-dcc-member "noah!friedman@prep.ai.mit.edu" 'CHAT zenirc-dcc-alist)
;; => ("noah!friedman@prep.ai.mit.edu" CHAT <proc> <ipaddr> <port>)
(defun zenirc-dcc-member (nick type &optional alist)
  (or alist (setq alist zenirc-dcc-alist))
  (let ((nicklen (length nick))
        (nickuserhostp (save-match-data
                         (string-match "!" nick)))
        (elt nil)
        (result nil))
    (while alist
      (setq elt (car alist))
      (setq alist (cdr alist))
      (cond ((and nickuserhostp
                  (eq (car (cdr elt)) type)
                  (zenirc-names-equal-p (car elt) nick))
             (setq result elt)
             (setq alist nil))
            ((and (zenirc-names-equal-p (zenirc-extract-nick (car elt)) nick)
                  (eq (car (cdr elt)) type))
             (setq result elt)
             (setq alist nil))))
    result))

;; msa wrote this nifty little frob to convert an n-byte integer to a packed
;; string.
(defun zenirc-packed-int (value count)
  (if (> count 0)
      (concat (zenirc-packed-int (/ value 256) (1- count))
	      (char-to-string (% value 256)))
    ""))

(defun zenirc-dcc-open-network-stream (procname buffer addr port entry)
  (and (string-lessp emacs-version "19")
       (setq addr (zenirc-extract-host (car entry))))
  (open-network-stream procname buffer addr port))


;;; Section 3: Interactive command handling

(defvar zenirc-command-dcc-hook '(zenirc-command-dcc))

(defvar zenirc-dcc-get-default-directory nil
  "*Default directory for incoming DCC file transfers.
If this is nil, then the current value of `default-directory' is used.")

;; parser for /dcc command. This figures out the dcc subcommand and calls
;; the appropriate routine to handle it.
;; The function dispatched should be named "zenirc-dcc-do-FOO-command",
;; where FOO is one of `close', `get', `list', etc.
(defun zenirc-command-dcc (proc parsedcmd)
  (let* ((cmd (zenirc-parse-firstword (cdr parsedcmd)))
         (fn (intern-soft (concat "zenirc-dcc-do-" (car cmd) "-command"))))
    (if (and fn (fboundp fn))
        (funcall fn proc cmd)
      (zenirc-message proc 'dcc-command-undefined (car cmd)))))

(defun zenirc-dcc-do-chat-command (proc subcommand)
  (let* ((nick (car (zenirc-parse-firstword (cdr subcommand))))
         (elt (zenirc-dcc-member nick 'CHAT)))
    (if elt
        (zenirc-dcc-chat elt proc)
      (zenirc-message proc 'dcc-chat-no-request nick))))

;; /dcc close type nick
;; both type and nick are optional, but type must be specified if nick is.
(defun zenirc-dcc-do-close-command (proc subcommand)
  (let* ((tmp (zenirc-parse-n-words 2 (cdr subcommand)))
	 (type (if (string= "" (car tmp))
                   nil
                 (intern (upcase (car tmp)))))
         (nick (car (cdr tmp)))
         (alist zenirc-dcc-alist)
         (elt nil))
    (while alist
      (setq elt (car alist))
      (setq alist (cdr alist))

      (cond
       ;; Skip this elt if a type was specified and this one doesn't match.
       ((and type
             (not (eq (car (cdr elt)) type))))
       ((or (string= nick "")
            (zenirc-names-equal-p nick (zenirc-extract-nick (car elt))))

        ;; Delete process if it exists.
        (setq tmp (nth 2 elt))
        (and (processp tmp)
             (delete-process tmp))

        (setq zenirc-dcc-alist (delq elt zenirc-dcc-alist))
        (zenirc-message proc 'dcc-closed
                        (car (cdr elt))
                        (zenirc-extract-nick (car elt))
                        (if (eq (car (cdr elt)) 'SEND)
                            (concat ", file " (nth 6 elt))
                          "")))))))

(defun zenirc-dcc-do-get-command (proc parsedcmd)
  (let* ((tmp (zenirc-parse-n-words 2 (cdr parsedcmd)))
         (nick (car tmp))
	 (filename (car (cdr tmp)))
         (elt (zenirc-dcc-member nick 'GET)))
    (if elt
        (let* ((msg (zenirc-dcc-catalog-entry 'dcc-get-filename))
               (file (read-file-name (format msg (file-name-nondirectory
                                                  filename))
                                     (or zenirc-dcc-get-default-directory
                                         default-directory)
                                     (file-name-nondirectory filename))))

          (cond ((file-exists-p file)
                 (if (yes-or-no-p (format (zenirc-dcc-catalog-entry
                                           'dcc-get-file-exists)
                                          file))
                     (zenirc-dcc-get-file elt file proc)
                   (zenirc-message proc 'dcc-get-cmd-aborted
                                   nick filename)))
                (t
                 (zenirc-dcc-get-file elt file proc))))
      (zenirc-message proc 'dcc-get-notfound nick filename))))

;; this is the handler for the /dcc list command - it lists the current state
;; of zenirc-dcc-alist in an easy to read manner.
(defun zenirc-dcc-do-list-command (proc parsedcmd)
  (let ((alist zenirc-dcc-alist)
        size elt)
    (zenirc-message proc 'dcc-list-head)
    (zenirc-message proc 'dcc-list-line)
    (while alist
      (setq elt (car alist))
      (setq alist (cdr alist))

      (setq size (nth 5 elt))
      (cond ((null size)
             (setq size ""))
            ((string= size "")
             (setq size "unknown")))

      (zenirc-message proc 'dcc-list-item
                      (zenirc-extract-nick (car elt))
                      (nth 1 elt)
                      (if (processp (nth 2 elt))
                          (process-status (nth 2 elt))
                        "no")
                      size
                      (or (nth 6 elt) "")))
    (zenirc-message proc 'dcc-list-end)))


;;; Section 4: Server message handling (i.e. messages from remote users)

;; Hook variable for CTCP DCC queries
(defvar zenirc-ctcp-query-DCC-hook '(zenirc-ctcp-query-DCC))

(defvar zenirc-dcc-query-handler-alist
  '(("SEND" . zenirc-dcc-handle-ctcp-send)
    ("CHAT" . zenirc-dcc-handle-ctcp-chat)))

;; zenirc-ctcp-query-DCC is the function called when a CTCP DCC
;; request is detected by the client. It examines the DCC subcommand,
;; and either calls the appropriate routine for that subcommand, or
;; sends a ctcp errmsg to the sender.
(defun zenirc-ctcp-query-DCC (proc parsedctcp from to)
  (let* ((cmd (car (zenirc-parse-firstword (cdr parsedctcp))))
         (handler (cdr (assoc cmd zenirc-dcc-query-handler-alist))))
    (if handler
        (funcall handler proc parsedctcp from to)
      (let ((fmt (zenirc-dcc-catalog-entry 'dcc-ctcp-errmsg)))
        (zenirc-ctcp-errmsg nil from to (concat "DCC " cmd)
                            (format fmt cmd) proc)
        (and zenirc-verbose-ctcp
             (zenirc-message proc 'dcc-ctcp-unknown cmd
               (zenirc-run-hook 'zenirc-format-nickuserhost-hook from)))))))

;; This is called a CTCP DCC SEND subcommand is sent to the client.
;; It extracts the information about the dcc request and adds it to
;; zenirc-dcc-alist.
(defun zenirc-dcc-handle-ctcp-send (proc parsedctcp from to)
  (let ((str (cdr parsedctcp))
        (sender (zenirc-run-hook 'zenirc-format-nickuserhost-hook from))
        ;;                filename    ipaddr        port       size(optional)
        (regexp  "^SEND \\([^ ]+\\) \\([0-9]+\\) \\([0-9]+\\) *\\([0-9]*\\)")
        localfile filename ip port size)
    (if (not (zenirc-names-equal-p to zenirc-nick t))
        ;; DCC SEND requests must be sent to you, and you alone.
	(zenirc-message proc 'dcc-request-bogus 'SEND sender)
      (cond
       ((string-match regexp str)
        (setq filename (zenirc-match-string 1 str))
        (setq ip       (zenirc-match-string 2 str))
        (setq port     (zenirc-match-string 3 str))
        (setq size     (zenirc-match-string 4 str))

        ;; a warning really should also be sent
        ;; if the ip address != the host the dcc sender is on.
        (zenirc-message proc 'dcc-send-offered filename sender
                        (if (string= size "") "unknown" size))

        (and (< (string-to-int port) 1025)
             (zenirc-message proc 'dcc-privileged-port port))

        (setq zenirc-dcc-alist
              (cons (list from 'GET nil ip port size filename)
                    zenirc-dcc-alist)))
       (t
        (zenirc-message proc 'dcc-malformed sender 'SEND str))))))

(defun zenirc-dcc-handle-ctcp-chat (proc parsedctcp from to)
  (let* ((str (cdr parsedctcp))
         (sender (zenirc-run-hook 'zenirc-format-nickuserhost-hook from))
         ;;                   ipaddr       port
         (regexp "^CHAT +chat +\\([0-9.]+\\) +\\([0-9]+\\)")
         (elt (zenirc-dcc-member from 'CHAT))
         ip port)
    ;; DCC CHAT requests must be sent to you, and you alone.
    (if (not (zenirc-names-equal-p to zenirc-nick t))
        (zenirc-message proc 'dcc-request-bogus 'CHAT sender)
      (cond ((string-match regexp str)
             (setq ip   (zenirc-match-string 1 str))
             (setq port (zenirc-match-string 2 str))

             ;; A warning really should also be sent if the ip
             ;; address != the host the dcc sender is on.
             (zenirc-message proc 'dcc-chat-offered sender)
             (and (< (string-to-int port) 1025)
                  (zenirc-message proc 'dcc-privileged-port port))

             (cond (elt
                    (setcar (nthcdr 3 elt) ip)
                    (setcar (nthcdr 4 elt) port)
                    (zenirc-message proc 'dcc-chat-discarded sender))
                   (t
                    (setq zenirc-dcc-alist (cons (list from 'CHAT nil ip port)
                                                 zenirc-dcc-alist)))))
            (t
             (zenirc-message proc 'dcc-malformed sender 'CHAT str))))))


;;; Section 5: GET handling

;; zenirc-dcc-get-file does the work of setting up a transfer from the remote
;; client to the local one over a tcp connection. This involves setting
;; up a process filter and a process sentinel, and making the connection.
(defun zenirc-dcc-get-file (entry file parent-proc)
  (let* ((obuf (current-buffer))
         (buffer (generate-new-buffer (file-name-nondirectory file)))
         proc)
    (unwind-protect
        (progn
          (set-buffer buffer)
          (fundamental-mode)
          (setq mode-line-process '(":%s"))
          (setq buffer-read-only t)
          (set-visited-file-name file)
          (make-local-variable 'zenirc-dcc-parent-process)
          (setq zenirc-dcc-parent-process parent-proc)
          (make-local-variable 'zenirc-dcc-entry-data)
          (setq zenirc-dcc-entry-data entry)
          (make-local-variable 'zenirc-dcc-size)
          (setq zenirc-dcc-size (nth 5 entry))
          (make-local-variable 'zenirc-dcc-byte-count)
          (setq zenirc-dcc-byte-count 0)
          (setq proc
                (funcall zenirc-dcc-connect-function
                         "zenirc-dcc-get" buffer
                         (nth 3 entry) (string-to-int (nth 4 entry)) entry))
          (set-process-buffer proc buffer)
          (set-process-filter proc 'zenirc-dcc-get-filter)
          (set-process-sentinel proc 'zenirc-dcc-get-sentinel)
          (setcar (nthcdr 2 entry) proc))
      (set-buffer obuf))))

;; This is the process filter for transfers from other clients to this one.
;; It reads incoming bytes from the network and stores them in the DCC
;; buffer, and sends back the replies after each block of data per the DCC
;; protocol spec.  Well not really.  We write back a reply after each read,
;; rather than every 1024 byte block, but nobody seems to care.
(defun zenirc-dcc-get-filter (proc str)
  (let ((obuf (current-buffer)))
    (unwind-protect
	(progn
          (set-buffer (process-buffer proc))
          (setq buffer-read-only nil)
          (goto-char (point-max))
          (insert str)

          (setq zenirc-dcc-byte-count (+ (length str) zenirc-dcc-byte-count))
          (and zenirc-verbose-dcc
               (zenirc-message nil 'dcc-get-bytes-received
                               (file-name-nondirectory buffer-file-name)
                               zenirc-dcc-byte-count))
          (cond
           ((and (numberp zenirc-dcc-size)
                 (> zenirc-dcc-byte-count zenirc-dcc-size))
            (zenirc-message nil 'dcc-get-file-too-long
                            (file-name-nondirectory buffer-file-name))
            (delete-process proc))
           (t
	    (process-send-string
             proc (zenirc-packed-int zenirc-dcc-byte-count 4)))))
      (setq buffer-read-only t)
      (set-buffer obuf))))


;; This is the process sentinel for CTCP DCC SEND connections.
;; It shuts down the connection and notifies the user that the
;; transfer is complete.
(defun zenirc-dcc-get-sentinel (proc event)
  (let ((obuf (current-buffer)))
    (unwind-protect
        (progn
          (set-buffer (process-buffer proc))
          (delete-process proc)
          (setq buffer-read-only nil)
          (setq zenirc-dcc-alist (delq zenirc-dcc-entry-data zenirc-dcc-alist))
          (zenirc-message zenirc-dcc-parent-process 'dcc-get-complete
                          (file-name-nondirectory buffer-file-name)
                          (buffer-size))
          (save-buffer))
      (set-buffer obuf))))


;;; Section 6: CHAT handling

(defvar zenirc-dcc-chat-buffer-name-format "ZenIRC-DCC-%s")

(defvar zenirc-dcc-chat-mode-hook nil)
(defvar zenirc-dcc-chat-connect-hook nil)
(defvar zenirc-dcc-chat-exit-hook nil)

(defvar zenirc-dcc-chat-filter-hook '(zenirc-dcc-chat-parse-output)
  "*Hook to run after doing parsing (and possible insertion) of DCC messages.")

(defvar zenirc-dcc-chat-mode-map '()
  "Sparse keymap for zenirc-dcc-mode")
(cond ((not zenirc-dcc-chat-mode-map)
       (setq zenirc-dcc-chat-mode-map (make-sparse-keymap))
       (define-key zenirc-dcc-chat-mode-map "\n" 'zenirc-dcc-chat-send-line)
       (define-key zenirc-dcc-chat-mode-map "\r" 'zenirc-dcc-chat-send-line)))

(defun zenirc-dcc-chat-mode ()
  "Major mode for wasting time via DCC chat."
  (interactive)
  (kill-all-local-variables)
  (setq mode-line-process '(":%s"))
  (setq mode-name "ZenIRC-DCC-Chat")
  (setq major-mode 'zenirc-dcc-chat-mode)
  (use-local-map zenirc-dcc-chat-mode-map)
  (zenirc-run-hook 'zenirc-dcc-chat-mode-hook))

(defun zenirc-dcc-chat-send-line ()
  "Send current line to other client."
  (interactive)
  (end-of-line)
  (insert "\n")
  (let ((proc (get-buffer-process (current-buffer)))
        (string (buffer-substring zenirc-process-mark (point))))
    (set-marker zenirc-process-mark (point))
    (process-send-string proc string)))

(defun zenirc-dcc-chat (entry parent-proc)
  (let* ((nick (zenirc-extract-nick (car entry)))
         (buffer (generate-new-buffer
                  (format zenirc-dcc-chat-buffer-name-format
                          (zenirc-extract-nick (car entry)))))
         proc)
    (pop-to-buffer buffer)
    (zenirc-dcc-chat-mode)
    (make-local-variable 'zenirc-dcc-parent-process)
    (setq zenirc-dcc-parent-process parent-proc)
    (make-local-variable 'zenirc-dcc-from)
    (setq zenirc-dcc-from nick)
    (make-local-variable 'zenirc-dcc-entry-data)
    (setq zenirc-dcc-entry-data entry)
    (make-local-variable 'zenirc-dcc-unprocessed-output)
    (setq zenirc-dcc-unprocessed-output "")
    (make-local-variable 'zenirc-process-mark)
    (setq zenirc-process-mark (set-marker (make-marker) (point-max)))
    (setq proc
          (funcall zenirc-dcc-connect-function
                   "zenirc-dcc-chat" buffer
                   (nth 3 entry) (string-to-int (nth 4 entry)) entry))
    (set-process-buffer proc buffer)
    (set-process-filter proc 'zenirc-dcc-chat-filter)
    (set-process-sentinel proc 'zenirc-dcc-chat-sentinel)
    (setcar (nthcdr 2 entry) proc)
    (zenirc-run-hook 'zenirc-dcc-chat-connect-hook proc)))

(defun zenirc-dcc-chat-filter (proc str)
  (let ((orig-buffer (current-buffer)))
    (unwind-protect
	(progn
          (set-buffer (process-buffer proc))
          (setq zenirc-dcc-unprocessed-output
                (concat zenirc-dcc-unprocessed-output str))
          (zenirc-run-hook 'zenirc-dcc-chat-filter-hook proc
                           zenirc-dcc-unprocessed-output))
      (set-buffer orig-buffer))))

(defun zenirc-dcc-chat-parse-output (proc str)
  (save-match-data
    (let ((posn 0)
          line)
      (while (string-match "\n" str posn)
        (setq line (substring str posn (match-beginning 0)))
        (setq posn (match-end 0))
        (zenirc-message proc 'dcc-chat-privmsg zenirc-dcc-from line))
      (setq zenirc-dcc-unprocessed-output (substring str posn)))))

(defun zenirc-dcc-chat-sentinel (proc event)
  (let ((buf (current-buffer))
        (tm (current-time-string)))
    (unwind-protect
        (progn
          (set-buffer (process-buffer proc))
          (delete-process proc)
          (setq zenirc-dcc-alist (delq zenirc-dcc-entry-data zenirc-dcc-alist))
          (zenirc-run-hook 'zenirc-dcc-chat-exit-hook proc)
          (zenirc-message zenirc-dcc-parent-process 'dcc-chat-ended
                          zenirc-dcc-from tm event)
          (zenirc-message proc 'dcc-chat-ended
                          zenirc-dcc-from tm event))
      (set-buffer buf))))


(provide 'zenirc-dcc)

(zenirc-dcc-install-message-catalogs)

;;; zenirc-dcc.el ends here
