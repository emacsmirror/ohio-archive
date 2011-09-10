;; LCD Archive Entry:
;; meta-server|William M. Perry|wmperry@indiana.edu|
;; Major mode for watching netrek site info.|
;; 93-3-5|2.6|~/games/meta-server.tar.Z|

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
;;; This file is not part of GNU Emacs but the same permissions apply.
;;; 
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;;

(defvar metaserver-borg-binary "sunborg3" "*Borg netrek client to run.")
(defvar metaserver-ck-players-program "ck_players" "*ck_players program.")
(defvar metaserver-headers-kill-to "^-h *" "*Last line of headers.")
(defvar metaserver-in-lucid (string-match "Lucid" (emacs-version)) "in lucid?")
(defvar metaserver-list-all nil "*List every server, open or not.")
(defvar metaserver-netrek-binary "netrek_udp" "*Normal netrek client to run.")
(defvar metaserver-ping-packets 10 "*# of pings to send to a server.")
(defvar metaserver-ping-program "/usr/etc/ping -s" "*Ping Program.")
(defvar metaserver-ping-sizeof 65 "*Size of packets to send a server.")
(defvar metaserver-port 3521 "*Which port to query on metaserver")
(defvar metaserver-rsa-binary "rsa_client" "*RSA client to run.")
(defvar metaserver-save-file "~/.metaserver" "*Where to save the netrek info.")
(defvar metaserver-site "charon.amdahl.com" "*What machine the metaserver on?")
(defvar metaserver-use-background nil "*Use the background.el package or not?")
(defvar metaserver-use-borg nil "*Use borg or not?")
(defvar metaserver-use-rsa nil "*Use rsa client or not?")
(defvar metaserver-use-save-file t "*Use the save file if charon unavailable?")
(defvar metaserver-use-telnet nil "*Use telnet for better name resolution.")
(defvar metaserver-force-save-file nil "*Force metaserver to use backup file.")

(defvar metaserver-doc-variables
  '(
    metaserver-auto-fallback
    metaserver-binary-alist
    metaserver-borg-binary
    metaserver-ck-players-program
    metaserver-data-regexp
    metaserver-empty-site-regexp
    metaserver-force-save-file
    metaserver-info-list
    metaserver-info-trailer
    metaserver-list-all
    metaserver-mode-hooks
    metaserver-netrek-binary
    metaserver-ping-packets
    metaserver-ping-program
    metaserver-ping-sizeof
    metaserver-port
    metaserver-rsa-binary
    metaserver-save-file
    metaserver-site
    metaserver-site-order
    metaserver-use-background
    metaserver-use-borg
    metaserver-use-rsa
    metaserver-use-telnet
    )
  "Variables to document in the help buffer")

(defvar metaserver-doc-functions
  '(
    metaserver-toggle-borg
    metaserver-check-players
    metaserver-show-info
    metaserver-toggle-list
    metaserver-ping
    metaserver-quit
    metaserver-refresh
    metaserver-toggle-rsa
    metaserver-save-lists
    metaserver-print-version
    metaserver-suspend
    metaserver-run-netrek
    metaserver-tally-players
    metaserver-describe-briefly
    metaserver-restore-from-file
    )
  "Documentation strings to put in the help buffer")

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
  "- B Borg Status - L List All - r Refresh - C Check Players - RETURN Run -"
  "One line description of key bindings, etc.")

(defvar metaserver-data-regexp "-h \\([^ ]*\\) *-p \\([^ ]*\\)\\(.*\\)"
  "*Parenthesized regular expression where the first parenthesized regexp
matches the host, the second matches the port, and the third matches any
data to be kept in the MetaServer buffer.")

(defvar metaserver-empty-site-regexp "Not Responding\\|Nobody"
  "*Regular expression that matches empty sites.")

(defvar metaserver-info-trailer "--- end of list ---"
  "*Normal string that matches end of server info.")

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
  "Metaserver v2.60 By William M. Perry (wmperry@indiana.edu) 3/5/93")

(defvar metaserver-mode-map () "mode map used in metaserver-mode")

;;; Load up background.el if it is needed....
(if (and metaserver-use-background (not (boundp 'background)))
    (load "background" nil t))

(if metaserver-mode-map ()
  (setq metaserver-mode-map (make-keymap)))

(define-key metaserver-mode-map "b"     'metaserver-toggle-borg)
(define-key metaserver-mode-map "B"     'metaserver-toggle-borg)
(define-key metaserver-mode-map "c"     'metaserver-check-players)
(define-key metaserver-mode-map "C"     'metaserver-check-players)
(define-key metaserver-mode-map "h"     'metaserver-describe-briefly)
(define-key metaserver-mode-map "H"     'metaserver-help)
(define-key metaserver-mode-map "i"     'metaserver-show-info)
(define-key metaserver-mode-map "I"     'metaserver-show-info)
(define-key metaserver-mode-map "l"     'metaserver-toggle-list)
(define-key metaserver-mode-map "L"     'metaserver-toggle-list)
(define-key metaserver-mode-map "n"     'next-line)
(define-key metaserver-mode-map "p"     'previous-line)
(define-key metaserver-mode-map "P"     'metaserver-ping)
(define-key metaserver-mode-map "q"     'metaserver-quit)
(define-key metaserver-mode-map "Q"     'metaserver-quit)
(define-key metaserver-mode-map "r"     'metaserver-refresh)
(define-key metaserver-mode-map "R"     'metaserver-toggle-rsa)
(define-key metaserver-mode-map "s"     'metaserver-save-lists)
(define-key metaserver-mode-map "S"     'metaserver-save-lists)
(define-key metaserver-mode-map "v"     'metaserver-print-version)
(define-key metaserver-mode-map "V"     'metaserver-print-version)
(define-key metaserver-mode-map "z"     'metaserver-suspend)
(define-key metaserver-mode-map "Z"     'metaserver-suspend)
(define-key metaserver-mode-map " "     'scroll-up)
(define-key metaserver-mode-map "\C-?"  'scroll-down)
(define-key metaserver-mode-map "\C-m"  'metaserver-run-netrek)
(define-key metaserver-mode-map "?"     'metaserver-describe-briefly)
(define-key metaserver-mode-map "#"     'metaserver-tally-players)
(define-key metaserver-mode-map "\C-[l" 'metaserver-restore-from-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous functions, probably not useful in anything else :)        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun metaserver-suspend ()
  "Hide the metaserver buffer, but don't kill it."
  (interactive)
  (let* ((buflist (cdr (buffer-list)))
	 (thebufr (current-buffer)))
    (cond
     ((not (cdr buflist)) (switch-to-buffer (get-buffer-create "*scratch*")))
     ((and
       (not (equal " *Minibuf-0*" (buffer-name (car buflist))))
       (not (equal "*MetaServer*" (buffer-name (car buflist)))))
      (switch-to-buffer (car buflist)))
     (t (switch-to-buffer (get-buffer-create "*scratch*"))))
    (bury-buffer thebufr)))

(defun nuke-ctrls (str)
  (cond
   ((equal 13 (string-to-char str)) (concat "RET" (substring str 1 nil)))
   ((equal 27 (string-to-char str)) (concat "ESC-" (substring str 1 nil)))
   (t str)))

(defun list-to-string (ls)
  (let* ((tmp ls)
	 (str ""))
    (cond
     ((null tmp) "")
     ((null (cdr tmp)) (nuke-ctrls (car ls)))
     (t (concat (nuke-ctrls (car ls)) "," (list-to-string (cdr ls)))))))

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
	 (tmplist (append metaserver-open-list metaserver-empty-list)))
    (while tmplist
      (let* ((current-site (car (car tmplist)))
	     (current-port (car (cdr (car tmplist))))
	     (current-info (car (cdr (cdr (car tmplist))))))
	(if (equal current-site site)
	    (progn
	      (setq tmplist nil)
	      (setq retval (list current-site current-port current-info)))
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

(defun metaserver-run-netrek (&optional thesite)
  "Run a netrek client in the background."
  (interactive)
  (if (and metaserver-use-background (not (boundp 'background)))
      (load "background" nil t))
  (if (not (getenv "DISPLAY"))
      (message "DISPLAY variable not set... must use Xwindows to play netrek!")
    (let* ((tmp (metaserver-return-site-and-port thesite))
	   (site (car tmp))
	   (port (car (cdr tmp)))
	   (info (car (cdr (cdr tmp))))
	   (binary (metaserver-lookup-binary site)))
      (if (not binary)
	  (setq binary
		(cond
		 (metaserver-use-rsa metaserver-rsa-binary)
		 (metaserver-use-borg metaserver-borg-binary)
		 (t metaserver-netrek-binary))))
      (if (and (let ((case-fold-search nil)) (string-match "R" info))
	       (not metaserver-use-rsa)
	       (not (equal metaserver-rsa-binary binary))
	       (y-or-n-p (concat "RSA Detected: Use rsa client "
				 metaserver-rsa-binary " ")))
	  (setq binary metaserver-rsa-binary))
      (if tmp
	  (let* ((the-command (concat binary " -h " site " -p "
				      (int-to-string port))))
	    (if metaserver-use-background
		(background the-command)
	      (start-process "netrek" nil shell-file-name "-c" 
			     (concat "exec " (message the-command)))))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Automatic bug submission.                                               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun metaserver-submit-bug ()
  (interactive)
  (mail-other-window)
  (mail-to)
  (insert "wmperry@indiana.edu")
  (mail-subject)
  (insert "Bug found in metaserver")
  (re-search-forward mail-header-separator nil t)
  (next-line 1)
  (string-match "Metaserver \\([^ ]*\\).*) \\(.*\\)" metaserver-version)
  (insert "Metaserver Version: "
	  (substring metaserver-version (match-beginning 1) (match-end 1))
	  ", of "
	  (substring metaserver-version (match-beginning 2) (match-end 2))
	  "\n"
	  "     Emacs Version: "
	  (substring (emacs-version) 0 (string-match " of" (emacs-version)))
	  "\n"
	  "       System Type: "
	  (prin1-to-string system-type) "\n")
  (while (< (current-column) 29) (insert "-"))
  (insert "Description of Problem:")
  (while (< (current-column) 79) (insert "-"))
  (insert "\n\n"))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Toggle functions, make each one update the mode line                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun metaserver-toggle-list ()
  "Toggles whether to list all the servers."
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
	    (if (equal metaserver-site-order "ascending")
		(metaserver-insert-list (flip-list metaserver-empty-list))
	      (metaserver-insert-list metaserver-empty-list))))
	(if (not buffer-read-only) (toggle-read-only)))))

(defun metaserver-toggle-borg ()
  "Toggles whether to use the borg binary or the netrek binary."
  (interactive)
  (setq metaserver-use-borg (not metaserver-use-borg))
  (metaserver-update-mode-line))

(defun metaserver-toggle-rsa ()
  "Toggle whether to use the rsa binary or the netrek binary."
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
;;; Auto-documentation code.  Didn't like how \\{metaserver-mode-map} in a  ;;;
;;; docstring wouldn't print a functions doc string, just its name.  So I   ;;;
;;; wrote this.                                                             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun metaserver-help ()
  "Print documentation on metaserver mode."
  (interactive)
  (let* ((funcs metaserver-doc-functions)
	 (funcstr "")
	 (vars metaserver-doc-variables)
	 (varstr "")
	 (keys nil))
    (while funcs
      (setq keys (where-is-internal (car funcs) metaserver-mode-map))
      (setq funcstr
	    (concat funcstr "\n" (format "%5s: %s"
					(list-to-string keys)
					(documentation (car funcs)))))
      (setq funcs (cdr funcs)))
    (while vars
      (let* ((thevar (prin1-to-string (car vars)))
	     (doc (documentation-property (car vars) 'variable-documentation)))
	(setq varstr
	      (concat varstr "\n" (format
				   "%25s: %s\n" thevar
				   (if (> (+ (length thevar) (length doc)) 80)
				       (concat "\n" doc)
				     doc))))
	(setq vars (cdr vars))))
    (set-buffer (get-buffer-create "*MetaServer Help*"))
    (if buffer-read-only (toggle-read-only))
    (erase-buffer)
    (insert "Current keybindings:\n====================\n" funcstr)
    (insert "\n\nModifiable variables:\n=====================\n" varstr)
    (if (not buffer-read-only) (toggle-read-only))
    (goto-char (point-min))
    (metaserver-mode)
    (switch-to-buffer-other-window "*MetaServer Help*")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Show the total # of players right now                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun metaserver-tally-players ()
  "Show the total # of people playing netrek now."
  (interactive)
  (let* ((tmp metaserver-open-list)
	 (data nil)
	 (tot 0)
	 (waitq 0))
    (while tmp
      (setq info (car (cdr (cdr (car tmp))))
	    data (progn
		   (string-match ": \\([0-9]*\\)" info)
		   (substring info (match-beginning 1) (match-end 1)))
	    tmp (cdr tmp)
	    tot (+ tot (if (string-match "W" info)
			   (progn (setq waitq (+ waitq (string-to-int data)))
				  16)
			 (string-to-int data)))))
    (message "There are %d people playing, %d on the queue." tot waitq)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Server information code.  Grab site information from port 3524 of the   ;;;
;;; metaserver and pop it up in another buffer.  Only needs to be run once  ;;;
;;; since the info. won't change in a day.                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun metaserver-grab-info ()
  (set-buffer (get-buffer-create " msitmp"))
  (condition-case ()
      (set-process-sentinel
       (metaserver-open-stream "metaserver-info" " msitmp" metaserver-site
			       3524) 'metaserver-parse-info)
    (error
     (metaserver-restore-from-file))))

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
  (if metaserver-use-telnet
      (progn
	(replace-regexp "" "" nil)
	(goto-char (point-min))))
  (kill-region (point-min) (progn
			     (re-search-forward "^Comments and server" nil t)
			     (end-of-line)
			     (1+ (point))))
  (kill-line 6)
  (goto-char (point-min))
  (while (not (equal (grab-current-line) metaserver-info-trailer))
    (let* ((tmp (metaserver-info-get-one)))
      (setq metaserver-info-list (append tmp metaserver-info-list))
      (goto-char (point-min))
      (kill-region (point-min)
		   (progn 
		     (re-search-forward "^
" nil t)
		     (point)))))
  (setq metaserver-info-list metaserver-info-list)
  (kill-buffer " msitmp"))

(defun metaserver-show-info (&optional site-name)
  "Show the stats for a server.  Source version, hours, etc."
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
  "Try to find a list of players on a site."
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
;;; Check Players code, part II - use ck_players program if its installed   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
	(let ((waitq (substring string (match-beginning 1) (match-end 1))))
	  (if (get-buffer the-buffer)
	      (progn
		(set-process-sentinel process nil)
		(set-process-filter process nil)
		(kill-buffer the-buffer)
		(kill-process process)
		(switch-to-buffer "*MetaServer*")
		(message "Wait queue of %s at %s..." waitq
			 (substring the-buffer 0
				    (string-match "<" the-buffer))))
	    (kill-process process))
	  (if metaserver-auto-fallback (metaserver-check-players-last
					(substring
					 the-buffer 0
					 (string-match "<" the-buffer))
					waitq)))
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
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Check players code, part III.  Use this if the ck_players code isn't    ;;;
;;; installed or didn't work (wait q, etc)                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun metaserver-check-players-last (site waitq)
  (if (get-buffer site) (bury-buffer site))
  (save-window-excursion
    (message "Trying the metaserver....")
    (if (get-buffer site) (progn
			    (if buffer-read-only (toggle-read-only))
			    (set-buffer site)
			    (erase-buffer)))
    (set-process-sentinel (metaserver-open-stream waitq
						  site
						  metaserver-site
						  (1+ metaserver-port))
			  'metaserver-check-players-last-sentinel)))

(defun metaserver-check-players-last-sentinel (process string)
  (if (not (string-match "kill" string))
      (let* ((the-buffer (process-buffer process))
	     (waitq (process-name process)))
	(set-buffer the-buffer)
	(kill-region (point-max) (progn
				   (goto-char (point-max))
				   (re-search-backward "That's it!" nil t)
				   (beginning-of-line)
				   (point)))
	(goto-char (point-min))
	(if (re-search-forward (concat "^Server: "
				       (substring (buffer-name the-buffer)
						  0
						  (string-match
						   "<"
						   (buffer-name the-buffer))))
			       nil t)
	    (progn
	      (kill-region (point-min) (progn (beginning-of-line) (point)))
	      (if (re-search-forward "^Server: *" nil t 2)
		  (kill-region (progn (beginning-of-line) (point))
			       (point-max)))
	      (metaserver-mode)
	      (goto-char (point-min))
	      (if (not buffer-read-only) (toggle-read-only))
	      (if (re-search-forward "queue" nil t)
		  (progn
		    (kill-buffer the-buffer)
		    (message "Wait Queue of %s" waitq))
		(switch-to-buffer-other-window the-buffer)))
	  (progn
	    (message "%s's data was not found!" the-buffer)
	    (kill-buffer the-buffer)
	    (set-buffer "*MetaServer*"))))))

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
;;; Routines to save lisp format of the metaserver info.  Change suggested  ;;;
;;; Alec Habig (ahabig@riscgs1.lngs.infn.it)                                ;;;
;;; This function gotten from Gnus 3.14.3 by Masanobu UMEDA                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun metaserver-convert-to-lisp-file ()
  (if buffer-read-only (toggle-read-only))
  (erase-buffer)
  (let* ((variables
	  '(metaserver-open-list metaserver-empty-list metaserver-info-list))
	 (variable nil)
	 (metaserver-open-list metaserver-open-list)
	 (metaserver-empty-list metaserver-empty-list))
    (while variables
      (setq variable (car variables))
      (and (boundp variable)
	   (symbol-value variable)
	   (insert "(setq " (symbol-name variable) " '"
		   (prin1-to-string (symbol-value variable))
		   ")\n"))
      (setq variables (cdr variables)))))

(defun metaserver-save-lists ()
  "Save the current server states in the file set by metaserver-save-file"
  (interactive)
  (let ((make-backup-files nil)
	(version-control nil)
	(require-final-newline t))
    (message "Saving to file %s." metaserver-save-file)
    (set-buffer (get-buffer-create " *msitmp*"))
    (metaserver-convert-to-lisp-file)
    (write-file metaserver-save-file)
    (kill-buffer (file-name-nondirectory metaserver-save-file))))

(defun metaserver-restore-from-file ()
  "Restore the metaserver information from metaserver-save-file"
  (interactive)
  (if (file-readable-p metaserver-save-file)
      (progn
	(load metaserver-save-file nil t)
	(save-excursion
	  (switch-to-buffer "*MetaServer*")
	  (metaserver-mode)
	  (if buffer-read-only (toggle-read-only))
	  (erase-buffer)
	  (goto-char (point-min))
	  (metaserver-insert-headers)
	  (if (equal "ascending" metaserver-site-order)
	      (metaserver-insert-list (flip-list metaserver-open-list))
	    (metaserver-insert-list metaserver-open-list))
	  (if metaserver-list-all 
	      (if (equal "ascending" metaserver-site-order)
		  (metaserver-insert-list (flip-list metaserver-empty-list))
		(metaserver-insert-list metaserver-empty-list)))
	  (message "")
	  (if (not buffer-read-only) (toggle-read-only)))
	(switch-to-buffer "*MetaServer*")
	(goto-char (point-min))
	(if metaserver-in-lucid (metaserver-update-menu-bar))
	(message "Using old info from %s" metaserver-save-file))
    (message "Could not access %s!" metaserver-save-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lucid emacs support.                                                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun metaserver-build-menu ()
  (setq test-menu nil
	open-menu nil
	empty-menu nil)
  (let* ((open metaserver-open-list)
	 (empty metaserver-empty-list))
    (while empty 
      (setq empty-menu
	    (cons
	     (vector (car (car empty))
		     (list 'metaserver-run-netrek (car (car empty))) t)
	     empty-menu))
      (setq empty (cdr empty)))
    (setq empty-menu (cons "Empty Sites" empty-menu))
    (while open
      (setq open-menu
	    (cons
	     (vector (car (car open))
		     (list 'metaserver-run-netrek (car (car open))) t)
	     open-menu))
      (setq open (cdr open))))
  (setq open-menu (cons "Open Sites" open-menu))
  (setq test-menu (cons "Netrek Sites" (list open-menu empty-menu))))

(defun metaserver-mouse-run-netrek (e)
  (interactive "e")
  (mouse-set-point e)
  (beginning-of-line)
  (metaserver-run-netrek))

(defun metaserver-mouse-get-info (e)
  (interactive "e")
  (mouse-set-point e)
  (beginning-of-line)
  (metaserver-show-info))

(defun metaserver-lucid-setup ()
  (setq metaserver-menu
		'("Netrek Info"
		  ["Ping Server" metaserver-ping t]
		  ["Check Players" metaserver-check-players t]
		  ["Tally Players" metaserver-tally-players t]
		  ["Show Server Info" metaserver-show-info t]
		  ["Run Netrek" metaserver-run-netrek t]
		  "----"
		  ["Toggle Borg Mode" metaserver-toggle-borg t]
		  ["Toggle List All" metaserver-toggle-list t]
		  ["Toggle RSA Mode" metaserver-toggle-rsa t]
		  "----"
		  ["Update Listing" metaserver-refresh t]
		  ["Load Old Info" metaserver-restore-from-file t]
		  "----"
		  ["Suspend" metaserver-suspend t]
		  ["Quit" metaserver-quit t]))
  (define-key metaserver-mode-map 'button1 'metaserver-mouse-run-netrek)
  (define-key metaserver-mode-map 'button2 'metaserver-mouse-get-info)
  (define-key metaserver-mode-map 'button3 'metaserver-popup-menu))

(defun metaserver-popup-menu (e)
  (interactive "e")
  (mouse-set-point e)
  (beginning-of-line)
  (popup-menu metaserver-menu))

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
  (while (not (or (equal "" metaserver-server-output)
		  (equal "" metaserver-server-output)))
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
    (if (equal "ascending" metaserver-site-order)
	(metaserver-insert-list (flip-list metaserver-open-list))
      (metaserver-insert-list metaserver-open-list))
    (if metaserver-list-all 
	(if (equal "ascending" metaserver-site-order)
	    (metaserver-insert-list (flip-list metaserver-empty-list))
	  (metaserver-insert-list metaserver-empty-list)))
    (message "")
    (if (not buffer-read-only) (toggle-read-only)))
  (if metaserver-in-lucid (metaserver-update-menu-bar)))

(defun metaserver-update-menu-bar ()
  (metaserver-build-menu)
  (set-buffer-menubar (copy-sequence default-menubar))
  (add-menu nil "Netrek" (cdr test-menu)))

(defun metaserver-refresh ()
  "Start the metaserver.  Get site info, put in buffer, etc, etc."
  (interactive)
  (if (not metaserver-force-save-file)
      (progn
	(message "Calling %s %d..." metaserver-site metaserver-port)
	(switch-to-buffer "*MetaServer*")
	(metaserver-mode)
	(if buffer-read-only (toggle-read-only))
	(erase-buffer)
	(setq metaserver-server-output nil
	      metaserver-empty-list nil
	      metaserver-open-list nil)
	(condition-case ()
	    (set-process-filter 
	     (metaserver-open-stream
	      "metaserver" nil metaserver-site metaserver-port)
	     'metaserver-filter)
	  (error
	   (metaserver-restore-from-file))))
    (metaserver-restore-from-file)))

(defun metaserver-quit ()
  "Kill current buffer and go to a full-size *MetaServer* window if we can."
  (interactive)
  (kill-buffer (current-buffer))
  (if (get-buffer "*MetaServer*")
      (progn
	(switch-to-buffer "*MetaServer*")
	(delete-other-windows))
    (progn
      (delete-other-windows)
      (metaserver-save-lists))))

(defun metaserver-mode ()
"   This is a major mode for viewing active netrek servers.  A list of active
servers is put in a buffer, displaying wait queue status, and time the server
was last checked.

    Current Keymap Is:
      \\{metaserver-mode-map}

    User modifiable variables are: (Check documentation for full description)
      metaserver-auto-fallback: use the ck_players if no answer from port-1
                                query for player list?
       metaserver-binary-alist: a list of binaries to run on specific servers.
     metaserver-use-background: use 'background.el' package or not?
      metaserver-use-save-file: save the MSII information in a file?
    metaserver-force-save-file: force program to use old info?
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
          metaserver-save-file: where to save the information, if necessary.
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
  "Show current version/credits in the minibuffer."
  (interactive)
  (message metaserver-version))

(if metaserver-use-telnet
    (progn
      (message "Switching to telnet...")
      (defun metaserver-open-stream (name buffer host service)
	(start-process name buffer "/usr/ucb/telnet" host
		       (int-to-string service)))
      (if (not (equal "" (substring metaserver-data-regexp -1 nil)))
	  (setq metaserver-data-regexp (concat metaserver-data-regexp ""))))
  (progn
    (defun metaserver-open-stream (name buffer host service)
      (open-network-stream name buffer host service))
    (if (equal "" (substring metaserver-data-regexp -1 nil))
	(setq metaserver-data-regexp
	      (substring metaserver-data-regexp 0 -1)))))

;;; If no data on servers is available, go grab it
(if (not metaserver-info-list)
    (metaserver-grab-info))

(if metaserver-in-lucid (metaserver-lucid-setup))
