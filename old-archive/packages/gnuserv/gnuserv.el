; Lisp Interface code between GNU Emacs and gnuserv.
;
; This file is part of GNU Emacs.
;
; Copying is permitted under those conditions described by the GNU
; General Public License.
;
; Copyright (C) 1989 Free Software Foundation, Inc.
;
; Author: Andy Norman (ange@hplb.hpl.hp.com) based on
;         'lisp/server.el' from the 18.52 GNU Emacs distribution.
;
; Please mail bugs and suggestions to the author at the above address.


(defconst gnuserv-rcs-header-id "$Header: gnuserv.el,v 1.12 91/03/13 15:59:27 ange Exp $")

(provide 'gnuserv)

(defvar server-program "gnuserv"
  "*The program to use as the edit server")

(defvar server-process nil 
  "the current server process")

(defvar server-string ""
  "the last input string from the server")

(defvar current-client nil
  "the client we are currently talking to")

(defvar server-clients nil
  "List of current server clients.
Each element is (CLIENTID BUFFER...) where CLIENTID is an integer
that can be given to the server process to identify a client.
When a buffer is killed, it is removed from this list.")

(defvar server-buffer-clients nil
  "List of clientids for clients requesting editing of current buffer.")

(make-variable-buffer-local 'server-buffer-clients)
(setq-default server-buffer-clients nil)
(or (assq 'server-buffer-clients minor-mode-alist)
    (setq minor-mode-alist (cons '(server-buffer-clients " Server") minor-mode-alist)))

(defun server-log (string)
  "If a *server* buffer exists, write STRING to it for logging purposes."
  (if (get-buffer "*server*")
      (save-excursion
	(set-buffer "*server*")
	(goto-char (point-max))
	(insert string)
	(or (bolp) (newline)))))


(defun server-sentinel (proc msg)
  (cond ((eq (process-status proc) 'exit)
	 (server-log (message "Server subprocess exited")))
	((eq (process-status proc) 'signal)
	 (server-log (message "Server subprocess killed")))))


(defun server-process-display-error (string)
  "When an error has been reported from the server, display the error in a
pop-up window."
  (let ((cur (selected-window))
	(pop-up-windows t))
    (pop-to-buffer (get-buffer-create "*server*"))
    (set-window-start (selected-window) (point))
    (server-log string)
    (select-window cur)))
    

(defun server-process-filter (proc string)
  "Process incoming requests from the server for GNU Emacs to do some actions."
  (setq server-string (concat server-string string))
  (if (string-match "\n$" server-string) ;wait till request ends with a newline
      (if (string-match "^[0-9]+" server-string) ;client request id
	(progn
	  (server-log server-string)
	  (let ((header (read-from-string server-string)))
	    (setq current-client (car header))
	    (condition-case oops
		(eval (car (read-from-string server-string (cdr header))))
	      (error (setq server-string "")
		     (server-write-to-client current-client oops)
		     (setq current-client nil)
		     (signal (car oops) (cdr oops)))
	      (quit (setq server-string "")
		    (server-write-to-client current-client oops)
		    (setq current-client nil)
		    (signal 'quit nil)))
	    (setq server-string "")))
	(progn				;error string from server
	  (server-process-display-error server-string)
	  (setq server-string "")))))


(defun server-release-outstanding-buffers ()
  "Release all buffers that have clients waiting for them to be finished."
  (interactive)
  (while server-clients
    (let ((buffer (nth 1 (car server-clients)))) ;need to do this for all buffers
      (server-buffer-done buffer))))	; destructively modifies server-clients


(defun server-start (&optional leave-dead)
  "Allow this Emacs process to be a server for client processes.
This starts a server communications subprocess through which
client \"editors\" can send editing commands to this Emacs job.

Prefix arg means just kill any existing server communications subprocess."
  (interactive "P")
  ;; kill it dead!
  (if server-process
      (progn
	(server-release-outstanding-buffers)
	(set-process-sentinel server-process nil)
	(condition-case ()
	    (delete-process server-process)
	  (error nil))))
  ;; If we already had a server, clear out associated status.
  (if leave-dead
      nil
    (if server-process
	(server-log (message "Restarting server")))
    (setq server-string "")
    (setq current-client nil)
    (let ((process-connection-type t))
      (setq server-process (start-process "server" nil server-program)))
    (set-process-sentinel server-process 'server-sentinel)
    (set-process-filter server-process 'server-process-filter)
    (process-kill-without-query server-process)))


(defun server-write-to-client (client form)
  "Write the given form to the given client via the server process."
  (if (and client
	   (eq (process-status server-process) 'run))
      (let ((s (format "%s:%s\n" client form)))
	(send-string server-process s)
	(server-log s))))


(defun server-eval (form)
  "Evaluate form and return result to client."
  (server-write-to-client current-client (eval form))
  (setq current-client nil))


(defun server-eval-quickly (form)
  "Let client know that we've received the request, but eval the form
afterwards in order to not keep the client waiting."
  (server-write-to-client current-client nil)
  (setq current-client nil)
  (eval form))


(defun server-make-window-visible ()
  "Try to make this window even more visible."
  (if (and (boundp 'window-system)
	   (boundp 'window-system-version)
	   (eq window-system 'x)
	   (eq window-system-version 11)
	   (fboundp 'x-remap-window))
      (progn
	(x-remap-window)
	(accept-process-output))))	; give window chance to re-display text


(defun server-find-file (file)
  "Edit file FILENAME.
Switch to a buffer visiting file FILENAME,
creating one if none already exists."
  (let ((obuf (get-file-buffer file)))
    (if (and obuf (set-buffer obuf))
	(if (file-exists-p file)
	    (if (or (not (verify-visited-file-modtime obuf))
		    (buffer-modified-p obuf))
		(revert-buffer t nil))
	  (if (y-or-n-p
	       (concat "File no longer exists: "
		       file
		       ", write buffer to file? "))
	      (write-file file)))
      (set-buffer (find-file-noselect file))))
  (switch-to-buffer (current-buffer)))


(defun server-edit-files-quickly (l)
  "For each (lineno . file) pair in the given list, edit the file and goto the
given line number. Note that unlike server-edit-files, no information is saved
about clients waiting for this buffer to be killed."
  (server-write-to-client current-client nil)
  (setq current-client nil)
  (server-make-window-visible)
  (while l
    (let ((line (car (car l)))
	  (path (cdr (car l))))
      (server-find-file path)
      (goto-line line))
    (setq l (cdr l))))


(defun server-edit-files (l)
  "For each (lineno . file) pair in the given list, edit the given file for the
client and save enough information such that server-kill-buffer can let the client
know when the buffer has been finished with."
  (server-make-window-visible)
  (while l
    (let ((line (car (car l)))
	  (path (cdr (car l))))
      (server-find-file path)
      (let ((old-clients (assq current-client server-clients))
	    (buffer (current-buffer)))
	(goto-line line)
	(setq server-buffer-clients
	      (cons current-client server-buffer-clients))
	(if old-clients			;client already waiting for buffers?
	    (nconc old-clients (list buffer)) ;yes -- append this one as well
	  (setq server-clients		;nope -- make a new record
		(cons (list current-client buffer)
		      server-clients)))))
      (setq l (cdr l)))
  (message (substitute-command-keys
	    "When done with a buffer, type \\[server-edit].")))


(defun server-get-buffer (buffer)
  "One arg, a string or a buffer. Return either a buffer object or
throw an error if the buffer named was not a buffer."
  (if (null buffer)
      (current-buffer)
    (let ((buf (get-buffer buffer)))
      (if (null buf)
	  (if (stringp buffer)
	      (error "No buffer named %s" buffer)
	    (error "Invalid buffer argument"))
	buf))))


(defun server-kill-buffer (buffer)
  "One arg, a string or a buffer.  Get rid of the specified buffer.
NOTE: This function has been enhanced to allow for remote editing
in the following way:

If the buffer is waited upon by one or more clients, and a client is
not waiting for other buffers to be killed, then the client is told
that the buffer has been killed."
  (interactive "bKill buffer ")
  (setq buffer (server-get-buffer buffer))
  (if (buffer-name buffer)
      (save-excursion
	(set-buffer buffer)
	(let ((old-clients server-clients))
	  (server-real-kill-buffer buffer) ;try to kill it
	  (if (buffer-name buffer)	;succeeded in killing?
	      nil			;nope
	    (while old-clients
	      (let ((client (car old-clients)))
		(delq buffer client)
		(if (cdr client)	;pending buffers?
		    nil			;yep
		  (server-write-to-client (car client) nil) ;nope, tell client
		  (setq server-clients (delq client server-clients))))
	      (setq old-clients (cdr old-clients))))))))


(defun server-kill-all-local-variables ()
  "Eliminate all the buffer-local variable values of the current buffer.
This buffer will then see the default values of all variables.
NOTE: This function has been modified to ignore the variable 
server-buffer-clients."
  (let ((clients server-buffer-clients))
    (server-real-kill-all-local-variables)
    (if clients
	(setq server-buffer-clients clients))))


(or (fboundp 'server-real-kill-buffer)
  (fset 'server-real-kill-buffer (symbol-function 'kill-buffer)))

(fset 'kill-buffer 'server-kill-buffer)

(or (fboundp 'server-real-kill-all-local-variables)
    (fset 'server-real-kill-all-local-variables
	  (symbol-function 'kill-all-local-variables)))

(fset 'kill-all-local-variables 'server-kill-all-local-variables)


(defun server-buffer-done (buffer)
  "Mark BUFFER as \"done\" for its client(s).
Buries the buffer, and returns another server buffer
as a suggestion for what to select next."
  (let ((next-buffer nil)
	(old-clients server-clients))
    (while old-clients
      (let ((client (car old-clients)))
	(or next-buffer 
	    (setq next-buffer (nth 1 (memq buffer client))))
	(delq buffer client)
	;; If client now has no pending buffers,
	;; tell it that it is done, and forget it entirely.
	(if (cdr client)
	    nil
	  (server-write-to-client (car client) nil)
	  (setq server-clients (delq client server-clients))))
      (setq old-clients (cdr old-clients)))
    (if (buffer-name buffer)
	(save-excursion
	  (set-buffer buffer)
	  (setq server-buffer-clients nil)))
    (bury-buffer buffer)
    next-buffer))


(defun mh-draft-p (buffer)
  "Return non-nil if this BUFFER is an mh <draft> file.
Since MH deletes draft *BEFORE* it is edited, the server treats them specially."
 ;; This may not be appropriately robust for all cases.
  (string= (buffer-name buffer) "draft"))


(defun server-done ()
  "Offer to save current buffer, mark it as \"done\" for clients,
bury it, and return a suggested buffer to select next."
  (let ((buffer (current-buffer)))
    (if server-buffer-clients
	(progn
 	  (if (mh-draft-p buffer)
 	      (progn (save-buffer)
		     (write-region (point-min) (point-max)
				   (concat buffer-file-name "~"))
		     (kill-buffer buffer))
	    (if (and (buffer-modified-p)
		     (y-or-n-p (concat "Save file " buffer-file-name "? ")))
		(save-buffer buffer)))
	  (server-buffer-done buffer)))))


(defun server-edit (&optional arg)
  "Switch to next server editing buffer; say \"Done\" for current buffer.
If a server buffer is current, it is marked \"done\" and optionally saved.
MH <draft> files are always saved and backed up, no questions asked.
When all of a client's buffers are marked as \"done\", the client is notified.

If invoked with a prefix argument, or if there is no server process running, 
starts server process and that is all.  Invoked by \\[server-edit]."
  (interactive "P")
  (if (or arg
	  (not server-process)
	  (memq (process-status server-process) '(signal exit)))
      (server-start nil)
    (server-switch-buffer (server-done))))

(defun server-switch-buffer (next-buffer)
  "Switch to another buffer, preferably one that has a client.
Arg NEXT-BUFFER is a suggestion; if it is a live buffer, use it."
  (if next-buffer
      (if (and (bufferp next-buffer)
	       (buffer-name next-buffer))
	  (switch-to-buffer next-buffer)
	;; If NEXT-BUFFER is a dead buffer,
	;; remove the server records for it
	;; and try the next surviving server buffer.
	(server-switch-buffer
	 (server-buffer-done next-buffer)))
    (if server-clients
	(server-switch-buffer (nth 1 (car server-clients)))
      (switch-to-buffer (other-buffer)))))

(global-set-key "\C-x#" 'server-edit)
