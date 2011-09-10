;;; etalk connectin via mail routines
;;;
;;; Copyright (C) 1994 Free Software Foundation
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, you can either send email to this
;;; program's author (see below) or write to:
;;;
;;;              The Free Software Foundation, Inc.
;;;              675 Mass Ave.
;;;              Cambridge, MA 02139, USA. 
;;;
;;; Please send bug reports, etc. to zappo@gnu.ai.mit.edu.
;;;
;;; Purpose:
;;;   To provide a last ditch effort to connect to someone via TCP talk
;;; style program when all other avenues of connection fail, via a mail
;;; facility.

(defun etalk-mail-portnumber (address)
  "Create a mail buffer with some stuff in it to create the ''rining''
effect for talk."

  (mail)
  (insert address)
  (next-line 1)
  (end-of-line)
  (insert (format "Talk request on port %s\nEtalk-Reply-To: %s@%s"
		  etalk-remote-socket etalk-announce-as 
		  (system-name)))
  (next-line 2)
  (insert "This is a mail message requesting the use of emacs talk.
To answer, enter emacs and load the library \"etalk\" and use 
etalk-mail-reply with the port number above to connect."))

(defun etalk-mail-extract-portnumber ()
  "Pull information from a mail message from a requesting talk program
to reply to.  Returns a list (\"address\" port#)"

  (if (not (or (equal major-mode 'vm-mode)
	       (equal major-mode 'rmail-mode)))
      (progn
	(message "Switch to a window displaying the mail message.")
	nil)
    (let ((s "")
	  (name "")
	  (port 0))
      (goto-char 1)
      (if (string-match "\\(Etalk-Reply-To: \\)[^ ]+\\(\n\\)" 
			(setq s (buffer-string)))
	  (progn
	    (setq name (substring s (match-end 1) 
				  (match-beginning 2))) 
	    (if (string-match "\\(Talk request on port \\)" 
			      (setq s (buffer-string))) 
		(progn
		  (setq port (string-to-int 
			      (substring s (match-end 1) 
					 (+ (match-end 1) 4))))
		  (list name port))
	      (progn
		(message "Can't detemine port.")
		nil)))
	(progn
	  (message "Can't determine sender.")
	  nil))))
  )

;;; end lisp
(provide 'etalk-mail)