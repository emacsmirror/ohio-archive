; Path: utkcs2!emory!samsung!usc!rutgers!mcnc!xanth!talos!kjones
; >From: elves@magic-tree.keebler.com (Those Keebler Elves)
; Newsgroups: comp.emacs
; Subject: support for Berkeley msgs(1) under GNU Emacs
; Date: 25 Jul 90 18:42:28 GMT
; 
; The following file of Lisp code provides just enough support for
; the Berkeley msgs(1) bulletin system so that you can use existing
; mail tools under Emacs to read them.  Installation instructions
; are in the comments at the top of the file.
; 
; Once the package is installed, "M-x msgs-check", or (msgs-check)
; msgs(1) system, and display an indicator in the mode line if any
; such messages are found.
; 
; "M-x msgs-gobble-messages", or (setq msgs-auto-gobble-messages t)
; found in the msgs(1) system to the folder specified by the
; variable msgs-folder (default ~/MSGS).  ~/.msgsrc is updated to
; mark these messages as read.
; 
; The spool checker is driver by an interval timer, so my timer
; package must be installed for msgs-check to work.  The rest of
; the package can live without timers.  You can get the timer
; package from the OSU Emacs-Lisp archives or directly from me.
; 
; kyle jones   <kjones@talos.pm.com>   ...!uunet!talos!kjones
; 
; ----------------------------------------------------------------------
;;; Check for and/or gather new messages in the msgs(1) bulletin system.
;;; Copyright (C) 1990 Kyle E. Jones
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; A copy of the GNU General Public License can be obtained from this
;;; program's author (send electronic mail to kyle@cs.odu.edu) or from
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.
;;;
;;; Send bug reports to kyle@cs.odu.edu.

;; Save this package in a file named msgschk.el in a directory
;; that Emacs will search for Lisp libraries.  Byte-compile it.
;;
;; This package is autoloadable.  Use
;;    (autoload 'msgs-check "msgschk" nil t)
;;    (autoload 'msgs-gobble-messages "msgschk" nil t)
;; in your .emacs file.
;;
;; Once the package is installed, "M-x msgs-check", or
;; (msgs-check) from the .emacs will cause Emacs to check for new
;; messages in the msgs(1) system, and display an indicator in
;; the mode line if any such messages are found.
;;
;; "M-x msgs-gobble-messages", or (setq msgs-auto-gobble-messages t)
;; in the .emacs will cause emacs to append any new messages found
;; to the folder specified by the variable msgs-folder (default
;; ~/MSGS).  ~/.msgsrc is updated to mark these messages as read.

(require 'timer)

(provide 'msgs-check)

(defvar msgs-directory "/usr/msgs"
  "*Directory where msgs(1) messages are spooled.")

(defvar msgs-rc "~/.msgsrc"
  "*File that keeps track of what msgs(1) message you've read.")

(defvar msgs-check-interval 300
  "*Number of seconds between checks for new messages.")

(defvar msgs-folder "~/MSGS"
  "*Folder where message gathered from the msgs(1) system are stored.")

(defvar msgs-auto-gobble-messages nil
  "*Non-nil value causes the msgs checker to automatically append any new
messages posted to the msgs(1) bulletin board into the folder specified
by the variable msgs-folder.  Your .msgsrc is updated to indicate
that these new messages have been read.")

(defvar msgs-unread-messages-string nil)

(defun msgs-check ()
  "Periodically check for new messages (see msgs(1)).
If there are unread message, an indicator will apear in the mode
line.  The variable msgs-check-interval controls how often the
spool is checked for new messages."
  (interactive)
  ;; sanity check msgs-check-interval
  (if (< msgs-check-interval 5)
      (setq msgs-check-interval 5))
  ;; install the display string
  (or global-mode-string
      (setq global-mode-string '("")))
  (or (memq 'msgs-unread-messages-string global-mode-string)
      (setq global-mode-string
	    (append global-mode-string '(msgs-unread-messages-string))))
  ;; if the timer already exists, destroy it.
  (and (get-timer "msgs-check") (delete-timer "msgs-check"))
  ;; schedule the check function to be run immediately,
  ;; then every msgs-check-interval seconds thereafter.
  (start-timer "msgs-check" 'msgs-check-function 1 msgs-check-interval))

(defun msgs-check-function ()
  (setq msgs-unread-messages-string nil)
  (cond ((msgs-unread-messages-p)
	 (setq msgs-unread-messages-string " [new msgs] ")
	 (and msgs-auto-gobble-messages msgs-folder (msgs-gobble-messages)))
	((and msgs-folder (file-exists-p msgs-folder)
	      (not (zerop (nth 7 (file-attributes msgs-folder)))))
	 (setq msgs-unread-messages-string " [msgs] ")))
  ;; Force mode line updates
  (save-excursion (set-buffer (other-buffer)))
  (set-buffer-modified-p (buffer-modified-p))
  (sit-for 0))

(defun msgs-unread-messages-p ()
  (let (last-msg-read min-msg max-msg)
    (save-excursion
      (set-buffer (get-buffer-create " *msgs check*"))
      (erase-buffer)
      (condition-case ()
	  (insert-file-contents msgs-rc)
	(error (insert "0\n")))
      (goto-char (point-max))
      (condition-case ()
	  (insert-file-contents (concat msgs-directory "/bounds"))
	(error (insert "0 0\n")))
      (goto-char (point-min))
      (setq last-msg-read (1- (msgs-read-number))
	    min-msg (msgs-read-number)
	    max-msg (msgs-read-number)))
    (< last-msg-read max-msg)))

(defun msgs-gobble-messages ()
  "Append any new messsages from msgs(1) to the file specifed by msgs-folder.
This file should be considered a spool area.  If you're using VM or RMAIL
to read mail under Emacs, it is advised that you not visit this file directly,
Rather, you should set the appropriate mailer variables so that
you mail reader will look for messages in this file in addition
to your normal system mailbox."
  (interactive)
  (let (last-msg-read min-msg max-msg)
    (save-excursion
      (set-buffer (get-buffer-create " *msgs check*"))
      (erase-buffer)
      (condition-case ()
	  (insert-file-contents msgs-rc)
	(error (insert "0\n")))
      (goto-char (point-max))
      (condition-case ()
	  (insert-file-contents (concat msgs-directory "/bounds"))
	(error (insert "0 0\n")))
      (goto-char (point-min))
      (setq last-msg-read (1- (msgs-read-number))
	    min-msg (msgs-read-number)
	    max-msg (msgs-read-number))
      (if (< last-msg-read max-msg)
	  (progn
	    (erase-buffer)
	    (goto-char (point-max))
	    (while (<= last-msg-read max-msg)
	      (setq last-msg-read (1+ last-msg-read))
	      (condition-case ()
		  (progn
		    (insert-file-contents
		     (concat msgs-directory "/" (int-to-string last-msg-read)))
		    (goto-char (point-max))
		    (insert "\n"))
		(file-error nil)))
	    (write-region (point-min) (point-max) msgs-folder t 0)
	    (let (start)
	      (setq start (point))
	      (insert (int-to-string last-msg-read) "\n")
	      (write-region start (point) msgs-rc nil 0)))))))

(defun msgs-read-number ()
  (condition-case ()
      (read (current-buffer))
    (error 0)))


