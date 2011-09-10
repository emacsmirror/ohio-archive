;; LCD Archive Entry:
;; meta-server|William M. Perry|wmperry@indiana.edu|
;; Major mode for watching netrek site info.|
;; 92-1-19|2.0|~/games/meta-server.tar.Z|

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This is a major mode that parses the output of Andy McFadden's          ;;;
;;; Metaserver-II program to provide a list of all the netrek servers out   ;;;
;;; there with an active game going.                                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Most of the defaults should work for this program, but you will want to ;;;
;;; change the variables metaserver-borg-binary & metaserver-netrek-binary  ;;;
;;; to point to the right executables for your system/tastes.  If the       ;;;
;;; is somewhere in your path, then you don't need to specify a path.       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; You will need to either use the install script provided with this       ;;;
;;; distribution or add the following lines to your .emacs file:            ;;;
;;; (setq load-path (cons (expand-file-name "~/lisp/") load-path))          ;;;
;;; (autoload 'metaserver-refresh "meta-server" "Netrek info" t)            ;;;
;;; (global-set-key "\C-c\C-n" 'metaserver-refresh)                         ;;;
;;;                                                                         ;;;
;;; Add the following if you want to use the backquote program to start     ;;;
;;; processes:                                                              ;;;
;;; (autoload 'background "background" "Background processes" t)            ;;;
;;; (setq metaserver-use-background t)                                      ;;;
;;;                                                                         ;;;
;;; Add the following if you have the ck_players program installed:         ;;;
;;; (setq metaserver-auto-fallback t)                                       ;;;
;;; (setq metaserver-ck-players-program "<full-path-to-program>")           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Usage:                                                                  ;;;
;;; To start the metaserver, type M-x metaserver-refresh (or whatever keys  ;;;
;;; you bound it to.  Basic keys are RETURN to run a client, l to list all, ;;;
;;; servers, b to toggle borghood, R to toggle RSA usage, and r to refresh  ;;;
;;; the listing of servers.  CTRL-H m for more info.                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Known Bugs:                                                             ;;;
;;;    None so far!                                                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1992, 1993 by William M. Perry (wmperry@indiana.edu)      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file is not part of GNU Emacs but the same permissions apply.
;; 
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;

(defvar metaserver-borg-binary "sunborg3" "*Borg netrek client to run.")
(defvar metaserver-ck-players-program "ck_players" "*ck_players program.")
(defvar metaserver-headers-kill-to "^-h *" "*Last line of headers.")
(defvar metaserver-list-all nil "*List every server, open or not.")
(defvar metaserver-netrek-binary "netrek_udp" "*Normal netrek client to run.")
(defvar metaserver-ping-packets 10 "*# of pings to send to a server.")
(defvar metaserver-ping-program "/usr/etc/ping -s" "*Ping Program.")
(defvar metaserver-ping-sizeof 65 "*Size of packets to send a server.")
(defvar metaserver-port 3521 "*Which port to query on metaserver")
(defvar metaserver-rsa-binary "rsa_client" "*RSA client to run.")
(defvar metaserver-site "charon.amdahl.com" "*What machine the metaserver on?")
(defvar metaserver-use-background nil "*Use the background.el package or not?")
(defvar metaserver-use-borg nil "*Use borg or not?")
(defvar metaserver-use-rsa nil "*Use rsa client or not?")

(defvar metaserver-auto-fallback nil
  "*Use ck_players to get player list if the server doesn't respond to the
port-1 query?")

(defvar metaserver-binary-alist nil
  "*alist of the form '( (server normal-binary borg-binary rsa binary) ...)
  If the current server is found in the alist, then the appropriate binary
description is extracted and used to connect to the server.  Otherwise,
metaserver-borg-binary, metaserver-rsa-binary, or metaserver-netrek-binary
are used, depending on borg/rsa status.")

(defvar metaserver-brief-description
  "- B Borg Status - L List All - R Refresh - C Check Players - RETURN Run -"
  "One line description of key bindings, etc.")

(defvar metaserver-data-regexp "-h \\([^ ]*\\) *-p \\([^ ]*\\)\\(.*\\)"
  "*Parenthesized regular expression where the first parenthesized regexp
matches the host, the second matches the port, and the third matches any
data to be kept in the MetaServer buffer.")

(defvar metaserver-empty-site-regexp "Not Responding\\|Nobody"
  "*Regular expression that matches empty sites.")

(defvar metaserver-info-list nil
  "alist of the form ( (sitename . information ) . . .)")

(defvar metaserver-mode-hooks nil
  "*Function or list of functions to run when entering metaserver-mode.")

(defvar metaserver-site-order "descending"
  "*How to sort the open sites....  can be \"ascending\" or \"descending\".
Ascending means put sites with wait queues at the bottom of the list.")

(defvar metaserver-trailing-headers ".*That's.*"
  "String that comes at the end of the metaserver output")

(defvar metaserver-version
  "Metaserver v2.0 By William M. Perry (wmperry@indiana.edu) 1/20/93")

(defvar metaserver-mode-map () "mode map used in metaserver-mode")

;;; Load up background.el if it is needed....
(if (and metaserver-use-background (not boundp 'background))
    (load "background" nil t))

(if metaserver-mode-map ()
  (setq metaserver-mode-map (make-keymap)))

(define-key metaserver-mode-map "b"     'metaserver-toggle-borg)
(define-key metaserver-mode-map "B"     'metaserver-toggle-borg)
(define-key metaserver-mode-map "c"     'metaserver-check-players)
(define-key metaserver-mode-map "C"     'metaserver-check-players)
(define-key metaserver-mode-map "i"     'metaserver-show-info)
(define-key metaserver-mode-map "I"     'metaserver-show-info)
(define-key metaserver-mode-map "n"     'next-line)
(define-key metaserver-mode-map "p"     'previous-line)
(define-key metaserver-mode-map "P"     'metaserver-ping)
(define-key metaserver-mode-map "q"     'metaserver-quit)
(define-key metaserver-mode-map "Q"     'metaserver-quit)
(define-key metaserver-mode-map "l"     'metaserver-toggle-list)
(define-key metaserver-mode-map "L"     'metaserver-toggle-list)
(define-key metaserver-mode-map "r"     'metaserver-refresh)
(define-key metaserver-mode-map "R"     'metaserver-toggle-rsa)
(define-key metaserver-mode-map "v"     'metaserver-print-version)
(define-key metaserver-mode-map "V"     'metaserver-print-version)
(define-key metaserver-mode-map " "     'scroll-up)
(define-key metaserver-mode-map "\C-?"  'scroll-down)
(define-key metaserver-mode-map "\C-m"  'metaserver-run-netrek)
(define-key metaserver-mode-map "?"     'metaserver-describe-briefly)
(define-key metaserver-mode-map "h"     'metaserver-describe-briefly)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous functions, probably not useful in anything else :)        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun grab-current-line ()
  (buffer-substring (progn (beginning-of-line) (point))
		    (progn (end-of-line) (point))))

(defun flip-list (thelist)
  (if (cdr thelist)
      (append (flip-list (cdr thelist)) (list (car thelist)))
    (list (car thelist))))

(defun metaserver-insert-headers ()
  "Insert headers in metaserver buffer.... makes it easier to read."
  (erase-buffer)
  (insert (format "%53s\n%-40s%35s\n" "Mins" "Site"
		  "Ago   Status        Flags"))
  (while (< (current-column) 79) (insert "-"))
  (insert "\n"))

(defun metaserver-insert-list (thelist)
  "Insert a list of (server . port . data) into the buffer"
  (let* ((tmp thelist))
    (while tmp
      (let* ((site (car (car tmp)))
	     (data (car (cdr (cdr (car tmp))))))
	(insert (format "%-40s%39s\n" site data)))
      (setq tmp (cdr tmp)))))

(defun metaserver-describe-briefly ()
  "Describe briefly the keys used in metaserver-mode."
  (interactive)
  (message metaserver-brief-description))

(defun metaserver-return-site-and-port (&optional the-site)
  "Return the site & port # for the current line's server..."
  (let* ((tmp (buffer-substring (progn (beginning-of-line) (point))
				(progn (end-of-line) (point))))
	 (site (if the-site the-site (substring tmp 0 (string-match " " tmp))))
	 (port 2592)
	 (retval nil)
	 (tmplist (if metaserver-list-all
		      (append metaserver-open-list metaserver-empty-list)
		    metaserver-open-list)))
    (while tmplist
      (let* ((current-site (car (car tmplist)))
	     (current-port (car (cdr (car tmplist)))))
	(if (equal current-site site)
	    (progn
	      (setq tmplist nil)
	      (setq retval (list current-site current-port)))
	  (setq tmplist (cdr tmplist)))))
    (beginning-of-line)
    retval))

(defun metaserver-lookup-binary (site)
  (let* ((tmp metaserver-binary-alist))
    (while (and tmp (not (equal (car (car tmp)) site)))
      (setq tmp (cdr tmp)))
    (cond
     (metaserver-use-rsa (car (cdr (cdr (cdr (car tmp))))))
     (metaserver-use-borg (car (cdr (cdr (car tmp)))))
     (t (car (cdr (car tmp)))))))

(defun metaserver-run-netrek ()
  "Run a netrek client in the background."
  (interactive)
  (if (and metaserver-use-background (not (boundp 'background)))
      (load "background" nil t))
  (if (not (getenv "DISPLAY"))
      (message "DISPLAY variable not set... must use Xwindows to play netrek!")
    (let* ((tmp (metaserver-return-site-and-port))
	   (site (car tmp))
	   (port (car (cdr tmp)))
	   (binary (metaserver-lookup-binary site)))
      (if (not binary)
	  (setq binary
		(cond
		 (metaserver-use-rsa metaserver-rsa-binary)
		 (metaserver-use-borg metaserver-borg-binary)
		 (t metaserver-netrek-binary))))
      (if tmp
	  (let* ((the-command (concat binary " -h " site " -p "
				      (int-to-string port))))
	    (if metaserver-use-background
		(background the-command)
	      (start-process "netrek" nil shell-file-name "-c" 
			     (concat "exec " (message the-command)))))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Toggle functions, make each one update the mode line                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun metaserver-toggle-list ()
  "Toggles whether to list all the servers and updates mode line accordingly."
  (interactive)
  (if (equal (buffer-name) "*MetaServer*")
      (progn
	(if buffer-read-only (toggle-read-only))
	(setq metaserver-list-all (not metaserver-list-all))
	(metaserver-update-mode-line)
	(if (not metaserver-list-all)
	    (save-excursion
	      (goto-char (point-min))
	      (delete-matching-lines metaserver-empty-site-regexp))
	  (save-excursion
	    (goto-char (point-max))
	    (if (equal metaserver-site-order "descending")
		(metaserver-insert-list (flip-list metaserver-empty-list))
	      (metaserver-insert-list metaserver-empty-list))))
	(if (not buffer-read-only) (toggle-read-only)))))

(defun metaserver-toggle-borg ()
  "Toggles whether to use the borg binary or the netrek binary.  Updates the
mode line accordingly."
  (interactive)
  (setq metaserver-use-borg (not metaserver-use-borg))
  (metaserver-update-mode-line))

(defun metaserver-toggle-rsa ()
  (interactive)
  (setq metaserver-use-rsa (not metaserver-use-rsa))
  (metaserver-update-mode-line))

;;; Update the mode line to show flags of borg-use and list-all
(defun metaserver-update-mode-line ()
  "Change mode-line to include -Borg and/or -All if appropriate, and force an
update."
  (setq mode-line-process 
	(concat (if metaserver-use-borg "-Borg" "")
		(if metaserver-list-all "-All" "")
		(if metaserver-use-rsa "-RSA" "")))
  (set-buffer-modified-p nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Server information code.  Grab site information from port 3524 of the   ;;;
;;; metaserver and pop it up in another buffer.  Only needs to be run once  ;;;
;;; since the info. won't change in a day.                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun metaserver-grab-info ()
  (set-buffer (get-buffer-create " msitmp"))
  (set-process-sentinel
   (open-network-stream "metaserver-info" " msitmp"
			metaserver-site 3524) 'metaserver-parse-info))

(defun metaserver-info-get-one ()
  (let* ((site (buffer-substring (progn (beginning-of-line) (point))
				 (progn (end-of-line) (point))))
	 (data (buffer-substring (progn (forward-char 1) (point))
				 (progn (re-search-forward "^
" nil  t)
					(point)))))
    (setq site (if (string-match ":" site)
		   (substring site 0 (1- (length site)))
		 site))
    (list (cons site data))))

(defun metaserver-parse-info (proc string)
  (setq metaserver-info-list nil)
  (set-buffer " msitmp")
  (goto-char (point-min))
  (kill-region (point-min) (progn
			     (re-search-forward "^Comments and server" nil t)
			     (end-of-line)
			     (1+ (point))))
  (kill-line 6)
  (goto-char (point-min))
  (while (not (equal (grab-current-line) "--- END ---"))
    (let* ((tmp (metaserver-info-get-one)))
      (setq metaserver-info-list (append tmp metaserver-info-list))
      (goto-char (point-min))
      (kill-region (point-min)
		   (progn 
		     (re-search-forward "^
" nil t)
		     (point)))))
  (setq metaserver-info-list metaserver-info-list)
  (kill-buffer " msitmp")
  (set-buffer "*MetaServer*"))

(defun metaserver-show-info (&optional site-name)
  (interactive)
  (let* ((site (if site-name
		   site-name
		 (car (metaserver-return-site-and-port))))
	 (data (assoc site metaserver-info-list)))
    (if data
	(progn
	  (set-buffer (get-buffer-create (concat site " info")))
	  (if buffer-read-only (toggle-read-only))
	  (metaserver-mode)
	  (insert "Server information for: " site "\n")
	  (while (< (current-column) 79) (insert "-"))
	  (insert "\n" (cdr data))
	  (if (not buffer-read-only) (toggle-read-only))
	  (switch-to-buffer-other-window (concat site " info"))
	  (goto-char (point-min)))
      (message "Couldn't find data for %s!" site))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Check players code: use /usr/ucb/telnet instead of open-network-stream  ;;;
;;; to get better host-name resolution.                                     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun metaserver-check-players (&optional site)
  (interactive)
  (let* ((tmp (if (not site) (metaserver-return-site-and-port)
		(metaserver-return-site-and-port site)))
	 (site (car tmp))
	 (port (1- (car (cdr tmp)))))
    (if (get-buffer site) (bury-buffer site))
    (if tmp
	(save-window-excursion
	  (message "Trying %s %d..." site port)
	  (if (get-buffer site) (progn
				  (if buffer-read-only (toggle-read-only))
				  (set-buffer site)
				  (erase-buffer)))
	  (let ((the-process
		 (start-process (format "%s" site)
				nil
				"/usr/ucb/telnet" site (int-to-string port))))
	    (set-process-sentinel the-process
				  'metaserver-check-players-sentinel)
	    (set-process-filter the-process
				'metaserver-check-players-filter))))))

(defun metaserver-check-players-old (site)
  (interactive)
  (let* ((tmp (metaserver-return-site-and-port site))
	 (port (car (cdr tmp))))
    (if (get-buffer site) (bury-buffer site))
    (if tmp
	(save-window-excursion
	  (if (get-buffer site) (progn
				  (set-buffer site)
				  (if buffer-read-only (toggle-read-only))
				  (erase-buffer)))
	  (let ((the-process
		 (start-process (format "%s" site)
				nil
				metaserver-ck-players-program
				site
				(int-to-string port))))
	    (set-process-sentinel the-process
				  'metaserver-check-players-old-sentinel)
	    (set-process-filter the-process
				'metaserver-check-players-old-filter))))))

(defun metaserver-check-players-old-filter (process string)
  (let* ((the-buffer (process-name process)))
    (if (string-match "Queue with \\([0-9]*\\)" string)
	(if (get-buffer the-buffer)
	    (progn
	      (set-process-sentinel process nil)
	      (set-process-filter process nil)
	      (kill-buffer the-buffer)
	      (kill-process process)
	      (switch-to-buffer "*MetaServer*")
	      (message "Wait queue of %s at %s..." 
		       (substring string (match-beginning 1) (match-end 1))
		       (substring the-buffer 0 (string-match "<" the-buffer))))
				  
	  (kill-process process))
      (progn
	(set-buffer (get-buffer-create the-buffer))
	(insert string)))))

(defun metaserver-check-players-filter (process string)
  (let* ((the-buffer (process-name process)))
    (if (string-match "refuse" string)
	(if (get-buffer the-buffer)
	    (progn
	      (set-process-sentinel process nil)
	      (set-process-filter process nil)
	      (kill-buffer the-buffer)
	      (kill-process process)
	      (switch-to-buffer "*MetaServer*")
	      (message "%s not resonding to port-1 query..." the-buffer)
	      (if metaserver-auto-fallback
		  (metaserver-check-players-old the-buffer)))
	  (kill-process process))
      (progn
	(set-buffer (get-buffer-create the-buffer))
	(insert string)))))

(defun metaserver-check-players-old-sentinel (process string)
  (if (not (string-match "kill" string))
      (let* ((the-buffer (process-name process)))
	(switch-to-buffer-other-window the-buffer)
	(metaserver-mode)
	(if buffer-read-only (toggle-read-only))
	(goto-char (point-min))
	(replace-regexp "" "" nil)
	(goto-char (point-min))
	(kill-line 2)
	(if (not buffer-read-only) (toggle-read-only)))))
  
(defun metaserver-check-players-sentinel (process string)
  (if (not (string-match "kill" string))
      (let* ((the-buffer (process-name process)))
	(switch-to-buffer-other-window the-buffer)
	(metaserver-mode)
	(if buffer-read-only (toggle-read-only))
	(goto-char (point-min))
	(replace-regexp "" "" nil)
	(goto-char (point-min))
	(kill-line 3)
	(replace-regexp ".*closed by foreign.*" "")
	(goto-char (point-min))
	(if (not buffer-read-only) (toggle-read-only)))))
	      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pinging code...                                                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun metaserver-ping ()
  "Get the millisecond seek time to the current line's server."
  (interactive)
  (save-excursion
    (save-window-excursion
      (let* ((tmp (grab-current-line))
	     (server (substring tmp 0 (string-match " " tmp))))
	(message "Pinging %s..." server)
	(shell-command (concat metaserver-ping-program " "
			       server " "
			       (int-to-string metaserver-ping-sizeof) " "
			       (int-to-string metaserver-ping-packets)))
	(let ((ping-str (progn (set-buffer "*Shell Command Output*")
			       (buffer-string)))
	      (ping-regexp (concat ".*packets transmitted, "
				   ".* packets received, \\(.*\\)\\\n"
				   "round-trip (ms) \\(.*\\)")))
	  (kill-buffer "*Shell Command Output*")
	  (if (string-match ping-regexp ping-str)
	      (message "%s:%s"
		       (substring ping-str (match-beginning 1) (match-end 1))
		       (substring ping-str (match-beginning 2) (match-end 2)))
	    (message "%s is not responding..." server)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The Main routines.  Filter the streams output and keep it in order then ;;;
;;; send it to the parser routine which makes an open-list and a closed     ;;;
;;; list.  Then dump them into the metaserver buffer                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun metaserver-filter (proc string)
  "Filter for the stream to the metaserver.  Calls parser when appropriate."
  (setq metaserver-server-output (concat metaserver-server-output string))
  (if (string-match metaserver-trailing-headers string)
      (progn
	(delete-process proc)
	(metaserver-parser))))

(defun metaserver-parser ()
  "Parses out the metaserver string into a list.  Constructs buffer."
  (setq metaserver-server-output (substring 
		     metaserver-server-output
		     (string-match metaserver-headers-kill-to
				   metaserver-server-output)
		     nil))
  (setq metaserver-server-output 
	(substring metaserver-server-output
		   0 (1- (string-match metaserver-trailing-headers 
				       metaserver-server-output))))
  (while (not (equal "" metaserver-server-output))
    (let* ((tmp (substring metaserver-server-output 
			   0 (string-match "$" metaserver-server-output))))
      (setq metaserver-server-output 
	    (substring metaserver-server-output
		       (1+ (string-match "$" metaserver-server-output))
				   nil))
      (string-match metaserver-data-regexp tmp)
      (let* ((site (substring tmp (match-beginning 1) (match-end 1)))
	     (port (string-to-int 
		    (substring tmp (match-beginning 2) (match-end 2))))
	     (data (substring tmp (match-beginning 3) (match-end 3))))
	(if (not (string-match metaserver-empty-site-regexp data))
	    (setq metaserver-open-list
		  (append (cons (cons site (cons port (cons data '()))) '())
			  metaserver-open-list))
	  (setq metaserver-empty-list
		(append (cons (cons site (cons port (cons data '()))) '())
			metaserver-empty-list))))))
  (save-excursion
    (switch-to-buffer "*MetaServer*")
    (goto-char (point-min))
    (metaserver-insert-headers)
    (if (equal "descending" metaserver-site-order)
	(metaserver-insert-list (flip-list metaserver-open-list))
      (metaserver-insert-list metaserver-open-list))
    (if metaserver-list-all 
	(if (equal "descending" metaserver-site-order)
	    (metaserver-insert-list (flip-list metaserver-empty-list))
	  (metaserver-insert-list metaserver-empty-list)))
    (message "")
    (if (not buffer-read-only) (toggle-read-only))))
	  
(defun metaserver-refresh ()
  "Start the metaserver.  Get site info, put in buffer, etc, etc."
  (interactive)
  (message "Calling metaserver at %s %d..." metaserver-site metaserver-port)
  (switch-to-buffer "*MetaServer*")
  (metaserver-mode)
  (if buffer-read-only (toggle-read-only))
  (erase-buffer)
  (setq metaserver-server-output nil
	metaserver-empty-list nil
	metaserver-open-list nil)
  (set-process-filter
   (open-network-stream "metaserver" nil metaserver-site metaserver-port)
   'metaserver-filter))

(defun metaserver-quit ()
  "Quit the current buffer and return to a full-size *MetaServer* window if
we can, otherwise exit."
  (interactive)
  (kill-buffer (current-buffer))
  (if (get-buffer "*MetaServer*")
      (progn
	(switch-to-buffer "*MetaServer*")
	(delete-other-windows))
    (delete-other-windows)))

(defun metaserver-mode ()
"   This is a major mode for viewing active netrek servers.  A list of active
servers is put in a buffer, displaying wait queue status, and time the server
was last checked.

    Default Keymap Is:
      h/?: Print out a brief help message in the minibuffer.
      b/B: Switch between using borg and non-borg mode.
      c/C: Get a player listing for the current server and pop it up in
           a separate window.
      i/I: Get information on the current site and put it in another buffer. 
      l/L: Toggle whether to list all the servers or just the ones with
           active games.
        P: Ping the current server and print the statistics in the minibuffer.
      q/Q: Quit the metaserver (or the current player list), kills the buffer.
        r: Refresh the server list from scratch.
        R: Switch between using an RSA client and borg/normal binary.
      v/V: Print current version number.
        n: Next line.
        p: Previous line.
      SPC: Scroll down one page.
      DEL: Scroll up one page.
      RET: Run the current netrek program on the current lines server.

    User modifiable variables are: (Check documentation for full description)
      metaserver-auto-fallback: use the ck_players if no answer from port-1
                                query for player list?
       metaserver-binary-alist: a list of binaries to run on specific servers.
     metaserver-use-background: use 'background.el' package or not?
   metaserver-trailing-headers: headers that specify end of MSII output
      metaserver-netrek-binary: Normal netrek client binary
        metaserver-borg-binary: Borg netrek client binary
  metaserver-empty-site-regexp: Regular expression that matches empty sites
         metaserver-site-order: how to sort the server list
  metaserver-brief-description: brief description of key bindings
        metaserver-data-regexp: regular expression to match site/port/data
    metaserver-headers-kill-to: Regular expression that matches last line of
                                the metaserver headers.
       metaserver-ping-program: Program to use to ping servers
       metaserver-ping-packets: # of packets to send to server when pinging
        metaserver-ping-sizeof: Size in bytes of packets sent during a ping
           metaserver-list-all: Show all servers or just open ones?
           metaserver-use-borg: Run normal client or borg client?
         metaserver-site-order: Ascending or descending order when
                                printing sites?  
               metaserver-site: Home of the metaserver
               metaserver-port: Port of the metaserver
         metaserver-mode-hooks: Hooks to run when entering metaserver-mode."
  (kill-all-local-variables)
  (use-local-map metaserver-mode-map)
  (setq major-mode 'metaserver-mode)
  (setq mode-name "MetaServer")
  (metaserver-update-mode-line)
  (make-local-variable 'goal-column)
  (run-hooks metaserver-mode-hooks)
  (setq goal-column 0))

(defun metaserver-print-version ()
  (interactive)
  (message metaserver-version))

;;; If no data on servers is available, go grab it
(if (not metaserver-info-list)
    (metaserver-grab-info))

