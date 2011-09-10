;; This file contains -*-Emacs-Lisp-*-
;; 
;; LCD Archive Entry:
;; mrc-lib|Michael Walker|m.d.walker@larc.nasa.gov|
;; Drive NCSA Mosaic via the remote-control interface|
;; 21-Nov-1994|1.17|~/interfaces/mrc-lib.el.Z|
;;
;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 1, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;;;;;
;;
;; A set of functions to make use of the remote-control feature (see
;; http://www.ncsa.uiuc.edu/SDG/Software/Mosaic/Docs/remote-control.html)
;; of the NCSA Mosaic X application from within GNU emacs (and particularly
;; from within other emacs applications.)
;;
;; The most interesting public interfaces are:
;;
;;   mrc-view-url-from-region
;;     treats the current region as a URL to be viewed by mosaic
;;
;;   mrc-query-view-urls-this-window
;;   mrc-query-view-urls-other-window
;;     searches a buffer for URLs then offers to view them in mosaic
;;
;;   mrc-view-buffer
;;     save the current buffer in a temp file and view it in mosaic
;;
;; which are best bound to a key-sequence for easy invocation:
;;
;;   (global-set-key "\^C\^V" 'mrc-view-url-from-region)
;;   (global-set-key "\^C\^Q" 'mrc-query-view-urls-this-window)
;;   (global-set-key "\^C\^O" 'mrc-query-view-urls-other-window)
;;   (global-set-key "\^C\^B" 'mrc-view-buffer)
;;
;;
;; Other commands are:
;;
;;   mrc-view-url        - view any url
;;   mrc-view-last-url   - edit and then re-send the most recently used URL
;;   mrc-view-local      - view a local file
;;   mrc-view-region     - view the current region
;;   mrc-query-view-urls - search any buffer for URLs and offer to view them
;;   mrc-toggle-view-command - toggle between goto and newwin
;;
;;
;; The first time you use one of the view functions it will prompt you
;; for the mosaic process-id. You can change this at any time by invoking
;; mrc-query-pid. 
;;
;; You can use this to control a remote Mosaic process on a trusted host
;; by setting mrc-remote-host to the name of that machine. You can also
;; specify the Mosaic process id as pid@host to indicate the remote system.
;;
;;
;; For a quick demo, load this into emacs, trim any non-lisp headers, etc.
;; and type
;;
;;   M-x eval-current-buffer <ret>
;;   M-x mrc-query-view-urls-this-window <ret>
;;
;; which should prompt you to view the remote-control interface document
;; from NCSA.
;;
;;
;; Note: the remote control interface is changing (significantly) in Mosaic
;;       2.5. This pacakge does not support the new interface yet...
;; 
;;
;; Send comments, suggestions, enhancements, etc. to
;;   Mike Walker  <m.d.walker@larc.nasa.gov>
;;


;; customize these to your liking
;;

(defvar mrc-no-pid-query nil
  "If t doesn't ask the user to verify the Mosaic pid found by the function
mrc-find-pid (but still may ask if that function can't find a pid.)")

(defvar mrc-control-file-prefix "/tmp/Mosaic."
  "Prefix of the file used to pass commands to NCSA Mosaic. The mosaic process
id will be appended to this (this convention is dictated by mosaic itself.)
Should be /tmp/xmosaic. for versions of Mosaic prior to 2.1.")

(defvar mrc-remote-host nil
  "If not nil, the name of a remote machine where your Mosaic process is
running. To be useful that host most trust the host(s) where you are running
emacs so that rsh & rcp will work. If equal to system-name, it has no effect.")

(defvar mrc-temp-file (concat "/tmp/mrc_" (user-login-name) ".html")
  "Name of a temporary file used to save a buffer or region when you invoke
mrc-view-buffer or mrc-view-region.")

(defvar mrc-view-command "goto"
  "One of the strings goto or newwin. Goto tells mosaic to display a URL in the
most recently used window, while newwin causes a new window to be created for
each URL.")

(defvar mrc-url-regexp
  "\\(file\\|local\\|http\\|gopher\\|news\\|telnet\\|tn3270\\|wais\\|ftp\\)://[^ 
\t()>\"]+"
  "Regexp to pick out a URL in a buffer. Improvments?")

(defvar mrc-pid-file "~/.mosaicpid"
  "File created by modern Mosaic to records it's pid.")

(defvar mrc-ps-command "/bin/ps"
  "The command used to try and find your Mosaic process when looking for the
process id.")


;; no reason to change these
;;

(defconst mrc-file-mode 384
  "Permissions applied to files created with mrc-lib (0600).")

(defconst mrc-signal-command "kill -USR1 "
  "Signal used to inform the mosaic process to read it's remote control file.")

(defconst mrc-rsh-command "rsh "
  "Prefix used (along with the remote host name) to signal Mosaic on a remote
machine.")

(defconst mrc-rcp-command "rcp "
  "Command used to copy the control file to a remove host running Mosaic.")

(defconst mrc-echo-command "echo "
  "How to echo something on the remote machine.")

(defconst mrc-output-redirect-suffix " > /dev/null"
  "How to discard stdout.")


;; these will be set when needed
;;

(defvar mrc-process-id nil
  "Set to the mosaic process id the first time mrc-send-command is executed.
You can specify pid@host to indicate that the mosaic process to be controlled
is runnong on another machine. See mrc-remote-host for more information.")

(defvar mrc-control-file nil
  "Set to the full path to the remote control file used to communicate with
mosaic.")

(defvar mrc-last-url ""
  "Last URL sent to mosaic.")


;; macros
;;

(defmacro mrc-mosaic-is-remote ()
  "Returns t if the Mosaic process is on a remote host (mrc-remote-host is
set and is not equal to system-name)."
  '(and mrc-remote-host (not (string= mrc-remote-host (system-name)))))

(defmacro mrc-rcp (lpath rpath)
  "Copy a file from the local machine to the machine where Mosaic is running."
  (list 'shell-command
	(list 'concat 'mrc-rcp-command lpath " " 'mrc-remote-host ":" rpath)))

(defmacro mrc-rsh (command)
  "Execute a command on the remote Mosaic machine."
  (list 'shell-command
	(list 'concat 'mrc-rsh-command  'mrc-remote-host " "
	      command 'mrc-output-redirect-suffix)))

(defmacro mrc-default-pid ()
  "Try to come up with a Mosaic pid."
  '(or mrc-process-id (mrc-find-pid)))


;; setup functions (findind Mosaic process id and setting file-names)
;;

(defun mrc-toggle-view-command ()
  "Toggle the command used to view URLs between goto or newwin."
  (interactive)
  (setq mrc-view-command (if (string= mrc-view-command "goto")
			     "newwin" "goto")))

(defun mrc-find-pid ()
  "Find the Mosaic process-id to be used when sending remote-control commands.
First consult the file ~/.mosaicpid (created by 2.1 and above) or if that
doesn't work run ps and look for Mosaic on this machine."
  (let ((buf (generate-new-buffer " mosaic-set-pid-tmp"))
	mosaic-pid)
    (if (file-readable-p mrc-pid-file)
	(progn
	  (set-buffer buf)
	  (erase-buffer)
	  (insert-file-contents mrc-pid-file nil)
	  (goto-char (point-min))
	  (if (re-search-forward "^ *\\([0-9]+\\)")
	      (setq mosaic-pid
		    (buffer-substring (match-beginning 1)
				      (match-end 1)))))
      (call-process mrc-ps-command nil buf nil "x")
      (set-buffer buf)
      (goto-char (point-min))
      (if (re-search-forward "[Mm]osaic" (point-max) t)
	  (progn
	    (re-search-backward "^ *\\([0-9]+\\)")
	    (setq mosaic-pid
		  (buffer-substring (match-beginning 1)
				    (match-end 1))))))
    (erase-buffer)
    mosaic-pid))

(defun mrc-set-pid (pid)
  "Set the pid to be used for signalling mosaic to read the control file. You
can indicate that Mosaic is running on a remote host with the syntax pid@host."
  (if (string-match "\\(@\\)" pid)
      (setq mrc-process-id (substring pid 0 (match-beginning 0))
	    mrc-remote-host (substring pid (match-end 0)))
    (setq mrc-process-id pid))
  (setq mrc-control-file (concat mrc-control-file-prefix mrc-process-id)))

(defun mrc-query-pid ()
  "Query user for the pid to be used for signalling mosaic to read the control
file. You can indicate that Mosaic is running on a remote host with the syntax
pid@host."
  (interactive)
  (mrc-set-pid (read-string "Mosaic process id: "
			    (or (mrc-default-pid) ""))))

(defun mrc-setup ()
  "Perform the inital setup which needs to be done in order to remotely control
NCSA Mosaic."
  (interactive)
  (if (null mrc-process-id)
      (if (and mrc-no-pid-query (mrc-default-pid))
	  (mrc-set-pid (mrc-default-pid))
	(mrc-query-pid))))


;; low-level file/signalling routines
;;

(defun mrc-delete-file (path)
  "Quietly delete a file if it exists."
  (if (file-exists-p path)
      (delete-file path)))

(defun mrc-write-region (beg end path &optional where)
  "Write a region to a file on the local host. Copy it to the remote host if
needed."
  (mrc-delete-file path)
  (write-region beg end path nil 'quiet)
  (set-file-modes path mrc-file-mode)
  (if (and (mrc-mosaic-is-remote) (or (not where) (equal where 'remote)))
      (mrc-rcp path path)))

(defun mrc-send-command (url &optional command)
  "Send a command to the mosaic process to view something. The first argument,
a URL, tells mosaic what to display. The optional second argument can be the
string ''goto'' or ''newwin''. A goto tells mosaic to use the most recent
window for displaying the URL, while newwin tells mosaic to create a new
window."
  (let (mosaic-command)
    (if (> (length url) 0)
	(progn
	  (mrc-setup)
	  (setq mrc-last-url url)
	  (if (mrc-mosaic-is-remote)
	      (mrc-rsh 
	       (concat "'("
		       mrc-echo-command (or mrc-view-command command) " ; "
		       mrc-echo-command url ") > " mrc-control-file
		       " && " mrc-signal-command mrc-process-id "'"))
	    (progn
	      (mrc-delete-file mrc-control-file)
	      (save-excursion
		(set-buffer (get-buffer-create mrc-control-file))
		(erase-buffer)
		(insert (concat (or mrc-view-command command) "\n" url "\n"))
		(mrc-write-region
		 (point-min) (point-max) mrc-control-file 'local)
		(shell-command 
		 (concat mrc-signal-command mrc-process-id
			 mrc-output-redirect-suffix)))))))))


;; the interesting user commands
;;

(defun mrc-view-url (url)
  "View any URL in NCSA Mosaic."
  (interactive "sURL: ")
  (mrc-send-command url))

(defun mrc-view-last-url ()
  "Edit/view the last URL sent to mosaic."
  (interactive)
  (mrc-view-url (read-string "URL: " mrc-last-url)))

(defun mrc-view-url-from-region ()
  "Treat the current region as a URL."
  (interactive)
  (mrc-send-command (buffer-substring (region-beginning) (region-end))))

(defun mrc-view-local (path &optional copy-done)
  "View a file from the local file-system in NCSA Mosaic. If using a remote
Mosaic the file is copied to that machine unless the second optional argument
is non-nill."
  (interactive "fFile to view: ")
  (let
      ((lpath (expand-file-name path))
       vpath)
    (cond
     ((and (mrc-mosaic-is-remote) (not copy-done))
      (mrc-delete-file (setq vpath mrc-temp-file))
      (mrc-rcp lpath vpath))
     (t (setq vpath lpath)))
    (mrc-send-command (concat "file://" vpath))))

(defun mrc-view-buffer (b)
  "View an emacs buffer in NCSA Mosaic."
  (interactive "bBuffer: ")
  (save-excursion
    (set-buffer b)
    (mrc-delete-file mrc-temp-file))
    (mrc-write-region (point-min) (point-max) mrc-temp-file)
    (mrc-view-local mrc-temp-file 'remote-copy-done))

(defun mrc-view-region ()
  "View the current region in NCSA Mosaic."
  (interactive)
  (save-excursion
    (mrc-delete-file mrc-temp-file)
    (mrc-write-region (region-beginning) (region-end) mrc-temp-file)
    (mrc-view-local mrc-temp-file 'remote-copy-done)))

;; look for a url in the current buffer
(defun mrc-search-for-url ()
  "Search the current buffer for something that looks like a URL."
  (if (re-search-forward mrc-url-regexp (point-max) t)
      (buffer-substring (match-beginning 0) (match-end 0))
    nil))

;; find each url in a buffer and offer to view it
;; (borrows heavily from replace.el in the emacs distribution)
(defun mrc-query-view-urls (b &optional stay-put)
  "For each URL in a buffer, query the user as to whether or not they want to
view that URL in mosaic. Responses are:
  n or <delete> - no, skip it
  y or <space>  - yes, view it
  e             - edit the URL, then view it
  .             - yes, then quit searching
  q             - no, and quit searching
  ?             - help
  ^L            - redisplay screen

The optional second argument, stay-put, starts searching from the current point
rather than the begining of the buffer (if not nil).
"
  (interactive "bBuffer: ")
  (save-excursion
    (set-buffer b)
    (if (not stay-put)
	(goto-char (point-min)))
    (let ((keep-going t)
	  (prompt "[yneq.] ")
	  url char)
      (while (and keep-going
		  (not (eobp))
		  (setq url (mrc-search-for-url)))
	(let (done)
	  (while (not done)
	    (message "Query viewing URL %s %s" url prompt)
	    (setq char (read-char))
	    (cond ((= char ??)
		   (describe-function 'mrc-query-view-urls))
		  ((= char ?q)
		   (setq keep-going nil)
		   (setq done t))
		  ((or (= char ?\ )
		       (= char ?y))
		   (mrc-view-url url)
		   (setq done t))
		  ((= char ?\.)
		   (mrc-view-url url)
		   (setq keep-going nil)
		   (setq done t))
		  ((or (= char ?\177)
		       (= char ?n))
		   (setq done t))
		  ((= char ?\C-l)
		   (recenter nil))
		  ((= char ?e)
		   (mrc-view-url (read-string "URL: " url)))
		  (t t))))))))

(defun mrc-query-view-urls-this-window ()
  "Query the user as to whether or not they want to view the URLs in the
current window/buffer. See mrc-query-view-urls for more information."
  (interactive)
  (mrc-query-view-urls (current-buffer)))

(defun mrc-query-view-urls-other-window ()
  "Query the user as to whether or not they want to view the URLs in the
other window/buffer. See mrc-query-view-urls for more information."
  (interactive)
  (save-excursion
    (save-window-excursion
      (other-window 1)
      (mrc-query-view-urls (current-buffer)))))

;;;;
;; make a menu if under emacs-19
;;;;

(if (string-match "^19" emacs-version)
    (progn
      (require 'easymenu)
      (easy-menu-define
       'mosaic global-map "Mosaic"
       '("Mosaic"
	 ["Prompt for and view any URL" mrc-view-url t]
	 ["Edit & resend last URL" mrc-view-last-url t]
	 ["Send region as URL" mrc-view-url-from-region t]
	 ["---" t t]
	 ["Search any buffer for URLs" mrc-query-view-urls t]
	 ["Query-view URLs this window" mrc-query-view-urls-this-window t]
	 ["Query-view URLs other window" mrc-query-view-urls-other-window t]
	 ["---" t t]
	 ["View current region" mrc-view-region t]
	 ["View current buffer" mrc-view-buffer t]
	 ["View local file" mrc-view-local t]
	 ["---" t t]
	 ["Toggle between goto and newwin" mrc-toggle-view-command t]
	 ["Prompt for Mosaic process id" mrc-query-pid t]))))
