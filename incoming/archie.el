;;; archie.el --- query archie servers and parse the results
;; Copyright (C) 1997 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <lmi@gnus.org>
;; Keywords: util, network

;; This file is not yet part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; The `M-x archie' command will connect to an archie server and send
;; a search request.  It'll return control to the user immediately,
;; and a window with the search results will be popped up when the
;; results have arrived.
;;
;; You can do several concurrent searches using different servers, but
;; you can only have one outstanding request to each server at a time.
;;
;; Put the following in your .emacs:
;;
;;  (autoload 'archie "archie" nil t)

;;; Code:

(require 'cl)
(require 'custom)

(defgroup archie nil
  "Query archie servers and parse the results."
  :group 'util)

(defcustom archie-default-server "archie.luth.se"
  "*Default server to query."
  :group 'archie
  :type '(radio (const "archie.rutgers.edu")
		(const "archie.sura.net")
		(const "archie.mcgill.ca")
		(const "archie.funet.fi")
		(const "archie.luth.se")
		(const "archie.au")
		(const "archie.doc.ic.ac.uk")
		(const "archie.wide.ad.jp")
		(const "archie.ncu.edu.tw")
		(string)))

(defcustom archie-default-query-method 'substring
  "*Default query method.
Legal values are `exact', `substring', `case-substring' and `regexp'."
  :group 'archie
  :type '(radio-button-choice
	  (const :tag "exact" exact)
	  (const :tag "substring" substring)
	  (const :tag "case sensitive substring" case-substring)
	  (const :tag "regular expression" regexp)))

(defcustom archie-prompt "> \\'"
  "*Prompt to wait for from the archie server before doing searches."
  :group 'archie
  :type 'string)

(defcustom archie-verbose t
  "In non-nil, give progress messages.
If nil, Archie won't message you on progress."
  :group 'archie
  :type 'boolean)

(defcustom archie-date-format "%Y-%m-%d %H:%M:%S"
  "Format of date strings."
  :group 'archie
  :type 'string)

(defcustom archie-login-parameters
  '("unset pager" "set autologout 1" "set output_format machine")
  "List of commands to send to the archie server on startup.
Fiddling with this variable without knowing what you are doing
may lead to Archie not doing anything useful."
  :group 'archie
  :type '(repeat string))

(defcustom archie-telnet-command "telnet"
  "Command used to connect to the archie server."
  :group 'archie
  :type 'string)

(defcustom archie-telnet-switches '("-8")
  "Switches supplied to the command used to connect to the archie server."
  :group 'archie
  :type '(repeat string))

(defcustom archie-ftp-name "anonymous"
  "User name used when logging in anonymously."
  :group 'archie
  :type '(radio (const "anonymous")
		(const "ftp")
		(string)))

(defvar archie-server-alist 
  '(("archie.rutgers.edu" "USA [NJ]")
    ("archie.sura.net" "USA [MD]")
    ("archie.mcgill.ca" "Canada")
    ("archie.funet.fi" "Finland/Mainland Europe")
    ("archie.luth.se" "Sweden")
    ("archie.au" "Australia")
    ("archie.doc.ic.ac.uk" "Great Britain/Ireland")
    ("archie.wide.ad.jp" "Japan")
    ("archie.ncu.edu.tw" "Taiwan"))
  "Alist of known archie servers.")

;;; Internal variables

(defvar archie-buffer-alist nil)
(defvar archie-current-server nil)
(defvar archie-default-method nil)
(defvar archie-doing-search nil)
(defvar archie-previous-server nil)
(defvar archie-previous-method nil)
(defvar archie-change-functions nil)
(defvar archie-min-point nil)
(defvar archie-process nil)

(defvar archie-search-methods
  '(("regexp" . "regex")
    ("substring" . "sub")
    ("exact" . "exact")
    ("case-substring" . "subcase"))
  "Mapping from our search names to archie search names.")

;;; User commands.

;;;###autoload
(defun archie (string &optional server method)
  "Query an archie server for STRING.
If given a prefix, also prompt for SERVER and search METHOD.  If not,
query `archie-default-server' using `archie-default-method'.

You can also give Un*x-like switches at the beginning of the search
string.  \"-r\" means regexp search, \"-s\" means substring search,
\"-e\" means exact search, and \"-c\" means case sensitive substring
search."
  (interactive
   (list
    (read-string "Search string: ")
    (if current-prefix-arg
	(setq archie-previous-server
	      (completing-read
	       "Server: " archie-server-alist nil nil 
	       (cons (or archie-previous-server "") 0)))
      archie-default-server)
    (if current-prefix-arg
	(setq archie-previous-method
	      (completing-read "Match type: " archie-search-methods
			       nil nil (cons (or archie-previous-method "")
					     0)))
      archie-default-query-method)))
  (let ((server (or server archie-default-server))
	(method (or method archie-default-query-method)))
    ;; We want symbolic search names.
    (when (stringp method)
      (setq method (intern method)))
    ;; Parse Un*x-like switches.
    (when (string-match " *-\\([rsce]\\) +" string)
      (let ((switch (aref string (match-end 1))))
	(setq string (substring string (match-end 0)))
	(setq method (cdr (assq switch
				'((?e . exact) (?s . substring)
				  (?r . regexp) (?c . case-substring)))))))
    (if (not (archie-connection-opened-p server))
	;; This also sends an asynch query after logging in.
	(unless (archie-open-connection string server method)
	  (error "Couldn't open a connection to %s" string server method))
      ;; The connection is open, so we send off a simple asynch query.
      (archie-send-query string server method))))

(defun archie-close ()
  "Close all archie connections."
  (interactive)
  (while archie-buffer-alist
    (archie-delete-process-buffer (caar archie-buffer-alist))))

;;; Internal functions.

(defun archie-open-connection (string server method)
  "Open a connection to archie SERVER asynchronously.
Also set up the first asynch query."
  ;; Remove any old buffers/data.
  (archie-delete-process-buffer server)
  (let (buffer process)
    (save-excursion
      ;; Internal buffer.
      (set-buffer (setq buffer (get-buffer-create
				(format " *archie %s*" server))))
      (set (make-local-variable 'archie-current-server) server)
      (set (make-local-variable 'archie-doing-search) nil)
      (set (make-local-variable 'archie-process) nil)
      (set (make-local-variable 'after-change-functions) 
	   '(archie-after-change-function))
      ;; Set up the sequence of asynch functions to be called
      ;; when logging in.
      (set (make-local-variable 'archie-change-functions)
	   ;; First log in.
	   `(("ogin:"
	      (lambda ()
		(archie-send-string "archie\n")))
	     ;; Send login parameters.
	     ,@(mapcar
		(lambda (string)
		  (list archie-prompt
			`(lambda ()
			   (archie-send-string ,(concat string "\n")))))
		archie-login-parameters)
	     ;; Set up the search type.
	     (,archie-prompt
	      (lambda ()
		(archie-send-string
		 ,(format "set search %s\n"
			  (cdr (assoc (symbol-name method)
				      archie-search-methods))))))
	     ;; Send the query.
	     (,archie-prompt
	      (lambda ()
		(if archie-verbose
		    (message "Querying %s about %s (%s)..."
			     ,server ,string ',method))
		(archie-send-string ,(format "find %s\n" string))
		(setq archie-doing-search t)))
	     ;; This is the default function used as the process
	     ;; filter after logging in.
	     (t (lambda ()
		  (archie-after-search-function beg end len)))))
      (set (make-local-variable 'archie-min-point) (point-min))
      (erase-buffer)
      (when archie-verbose
	(message "Logging in on %s..." server))
      (setq archie-process
	    (setq process (apply 'start-process "nntpd" buffer
				 archie-telnet-command
				 archie-telnet-switches)))
      (when (and process
		 (memq (process-status process) '(run open)))
	(push (list server process buffer) archie-buffer-alist)
	(archie-wait-for ">")
	(erase-buffer)
	(process-send-string process (format "open %s\n" server))
	;; We return a success.  The login process itself
	;; is performed from `after-change-functions'.
	t))))

(defun archie-send-string (string)
  "Function for sending strings when logging in."
  (setq archie-min-point (point-max))
  (process-send-string archie-process string))

(defun archie-after-change-function (beg end len)
  "The `after-change-functions' function that controls everything."
  (when (and archie-change-functions
	     (or (eq (caar archie-change-functions) t)
		 (prog2
		     (beginning-of-line)
		     (re-search-forward (caar archie-change-functions) nil t)
		   (goto-char end))))
    (if (eq (caar archie-change-functions) t)
	(funcall (cadar archie-change-functions))
      (funcall (cadr (pop archie-change-functions))))))

;; Do a normal, non-login asynch query.
(defun archie-send-query (string server method)
  (save-excursion
    (set-buffer (archie-process-buffer server))
    (if archie-doing-search
	(error "Search already in progress")
      (setq archie-min-point (point-min))
      (archie-send-command
       (format "set search %s"
	       (cdr (assoc (symbol-name method) archie-search-methods))))
      (archie-send-command (format "find %s" string))
      (when archie-verbose
	(message "Querying %s about %s (%s)..." server string method))
      (setq archie-doing-search t))))

(defun archie-send-command (string)
  "Wait for the prompt and send STRING to the server."
  (archie-wait-for archie-prompt)
  (erase-buffer)
  (process-send-string archie-process (concat string "\n")))

(defmacro archie-gethash (string hashtable)
  `(symbol-value (intern-soft ,string ,hashtable)))

(defmacro archie-sethash (string value hashtable)
  `(set (intern ,string ,hashtable) ,value))

;; When the seach has completed, this function parses
;; the data and formats it nicely in a new buffer.
(defun archie-after-search-function (beg end len &optional force)
  (when (or force
	    (and archie-doing-search
		 (prog2
		     (beginning-of-line)
		     (and (> (point) archie-min-point)
			  (re-search-forward archie-prompt nil t))
		   (goto-char end))))
    (save-excursion
      (setq archie-doing-search nil)
      (message "Making archie buffer...")
      (let ((hashtb (make-vector 1023 0))
	    host dir entry info type file)
	(goto-char (point-min))
	;; Eek.  
	(while (re-search-forward "\\([0-9A-Z]+\\) \\([^ ]+\\) \\([0-9]+\\) bytes \\(..........\\) \\(.*\\)$"
				  nil t)
	  (setq host (match-string 2)
		entry (archie-gethash host hashtb)
		info (list nil (match-string 1)
			   (match-string 3)
			   (match-string 4) (match-string 5)))
	  (setcar info (file-name-directory (nth 4 info)))
	  (archie-sethash host (cons info entry) hashtb))
	;; We now have the information.
	(pop-to-buffer (format "*archie %s search results*" 
			       archie-current-server))
	(archie-mode)
	(let (buffer-read-only)
	  (erase-buffer)
	  (mapatoms
	   (lambda (sym)
	     (let ((host (symbol-name sym))
		   (data (sort (symbol-value sym)
			       (lambda (s1 s2)
				 (string< (car s1) (car s2))))))
	       (archie-put 
		(point)
		(prog2
		    (insert host)
		    (point)
		  (insert "\n"))
		(archie-file host))
	       (while (setq entry (pop data))
		 (unless (equal (car entry) dir)
		   (insert "\n    Location: ")
		   (archie-put
		    (point)
		    (prog2
			(insert (setq dir (car entry)))
			(point)
		      (insert "\n"))
		    (archie-file host dir)))
		 (setq type (aref (nth 2 entry) 0))
		 (insert "      " (cond 
				   ((= type ?-)
				    "FILE      ")
				   ((= type ?l)
				    "LINK      ")
				   (t
				    "DIRECTORY "))
			 (nth 3 entry) " "
			 (format "%7s bytes" (nth 2 entry)) " "
			 (format-time-string archie-date-format
					     (archie-date (nth 1 entry)))
			 "  ")
		 (setq file (file-name-nondirectory (nth 4 entry)))
		 (archie-put
		  (point)
		  (prog2
		      (insert file)
		      (point)
		    (insert "\n"))
		  (archie-file host dir file)))
	       (setq dir nil)
	       (insert "\n")))
	   hashtb))
	(goto-char (point-min))
	(message "Making archie buffer...done")))))

(defun archie-date (date)
  "Transform an archie date into Emacs date."
  (encode-time
   (string-to-number (substring date 12 14))
   (string-to-number (substring date 10 12))
   (string-to-number (substring date 8 10))
   (string-to-number (substring date 6 8))
   (string-to-number (substring date 4 6))
   (string-to-number (substring date 0 4))
   (string-to-number (substring date 14))))

(defun archie-file (host &optional dir file)
  "Make an ange-ftp file name from HOST, DIR and FILE."
  (concat "/" archie-ftp-name "@" host ":" (or dir "") (or file "")))
	
(defun archie-connection-opened-p (server)
  "Say whether a connection to SERVER has been opened."
  (let ((process (archie-process server)))
    (and process
	 (memq (process-status process) '(run open)))))

(defun archie-process (server)
  "Return the connection to SERVER."
  (nth 1 (assoc server archie-buffer-alist)))

(defun archie-process-buffer (server)
  "Return the buffer that connects to SERVER."
  (nth 2 (assoc server archie-buffer-alist)))

(defun archie-delete-process-buffer (server)
  "Clean up the connection to SERVER."
  (let ((entry (assoc server archie-buffer-alist)))
    (when entry
      (when (buffer-live-p (nth 2 entry))
	(kill-buffer (nth 2 entry)))
      (setq archie-buffer-alist (delq entry archie-buffer-alist)))))

(defun archie-put (beg end file)
  "Put highlights on region between BEG and END.
Also create a callback to load FILE when triggered."
  (put-text-property beg end 'archie-file file)
  (put-text-property beg end 'mouse-face 'highlight))

(defun archie-wait-for (regexp)
  "Wait until string arrives in the buffer."
  (let ((buf (current-buffer)))
    (goto-char (point-min))
    (while (not (re-search-forward regexp nil t))
      (accept-process-output (archie-process archie-current-server))
      (set-buffer buf)
      (goto-char (point-min)))))

;;;
;;; Archie mode
;;;

(defvar archie-mode-hook nil
  "Hook run in Archie mode buffers.")

(defvar archie-mode-map nil)
(unless archie-mode-map
  (setq archie-mode-map (make-sparse-keymap))
  (define-key archie-mode-map "\r" 'archie-fetch)
  (define-key archie-mode-map [mouse-2] 'archie-push)
  (define-key archie-mode-map "c" 'archie-copy))
  
(defun archie-make-menu-bar ()
  (unless (boundp 'archie-menu)
    (easy-menu-define
     archie-menu archie-mode-map ""
     '("Archie"
       ["Fetch" archie-fetch (archie-filename)]
       ["Copy" archie-copy (archie-filename)]))))

(defun archie-mode ()
  "Major mode for displaying results from archie searches.

\\{archie-mode-map}"
  (interactive)
  (buffer-disable-undo (current-buffer))
  (archie-make-menu-bar)
  (kill-all-local-variables)
  (setq major-mode 'archie-mode)
  (setq mode-name "Archie")
  (setq mode-line-process nil)
  (use-local-map archie-mode-map)
  (setq buffer-read-only t)
  (run-hooks 'archie-mode-hook))

(defun archie-fetch (file)
  "Fetch the file under point."
  (interactive (list (archie-filename)))
  (if file
      (find-file file)
    (error "No file under point")))

(defun archie-push (e)
  "Fetch the file under the mouse pointer."
  (interactive "e")
  (mouse-set-point e)
  (archie-fetch (archie-filename)))

(defun archie-copy (file dir)
  "Copy the file under point."
  (interactive
   (let ((file (archie-filename)))
     (if (not file)
	 (error "No file under point")
       (list file (read-file-name "Copy to: ")))))
  (copy-file file dir 0))

;;; Internal functions.
 
(defun archie-filename ()
  "Return the name of the file under point."
  (get-text-property (point) 'archie-file))
  
(provide 'archie)

;;; archie.el ends here
