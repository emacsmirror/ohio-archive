;From ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!mips!apple!oracle!news Wed Feb  7 16:23:37 1990
;Article 1319 of comp.emacs:
;Path: ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!mips!apple!oracle!news
;From nhess@dvlseq.oracle.com (Nate Hess)
;Newsgroups: comp.emacs
;Subject: Terminal lock program for GNU Emacs
;Keywords: ASCII terminal lock
;Message-ID: <1990Feb5.183922.20072@oracle.com>
;Date: 5 Feb 90 18:39:22 GMT
;Sender: news@oracle.com
;Reply-To: nhess@dvlseq.oracle.com (Nate Hess)
;Organization: Oracle Corporation, Belmont, CA
;Lines: 163
;
;Here is a little something that I wrote a couple of years ago.  Someone
;might find it useful; I used it on my terminal at home all the time to
;prevent Binky, my cat, from doing nasty things to my Emacs session.
;
;To my knowledge, no one has been able to break the lock.
;
;
;------------------------------ cut here ------------------------------
;;;
;;;	Copyright (C) 1990 Nathan R. Hess
;;;
;;;	Verbatim copies of this file may be freely redistributed.
;;;
;;;	Modified versions of this file may be redistributed provided
;;;	that this notice remains unchanged, the file contains prominent
;;;	notice of author and time of modifications, and redistribution
;;;	of the file is not further restricted in any way.
;;;
;;;	This file is distributed `as is', without warranties of any kind.
;;;
;;;
;;;	Author:  Nathan Hess  (nhess@oracle.com)
;;;
;;;	Purpose:  Terminal Lock utility for GNU Emacs;
;;;		  intended primarily for use on ASCII terminals.
;;;
;;;	Pseudo-Disclaimer:  This is a couple hour hack.
;;;
;;;	Change History:
;;;	11/02/87 NRH	First version.
;;;	11/19/87 NRH	Don't echo the password, and ask the user to
;;;			verify it.  Also, added variable for longest
;;;			allowable password.
;;;	12/15/87 NRH	Kick up the LOCKED buffer to cover the whole
;;;                     screen, and restore the previous window
;;;                     configuration when proper password is given.
;;;	01/07/88 NRH	Prompt the user for an optional message to be
;;;                     displayed along with the "enter password"
;;;                     message.  A worthwhile possible enhancement to
;;;                     this would be to have a set of canned messages
;;;                     ("Gone to lunch", etc.) displayed in a menu.
;;;                     The user would either pick one of those or would
;;;                     type one in.
;;;	02/01/88 NRH	Added a default signature to the optional message.
;;;	02/05/90 NRH	Some cleanup before posting.
;;;
;;
;;
(defvar max-term-lock-password-length 100
  "*Largest password allowed in term-lock mode.")

(defvar term-lock-signature nil
  "*Default signature at bottom of optional message, none if nil.")

(defun term-lock ()
  "Prompts user for a password, verifies the password,
then won't leave the ***LOCKED*** buffer 'til it's typed in again.
Prompts for an optional message to display in the middle of the screen."
  (interactive)
  (let ((echo-keystrokes 0)
	first-try
	informative-message
	second-try)
    (setq first-try (read-string-no-echo "Enter password:"))
    (setq second-try (read-string-no-echo "Verify password:"))
    (if	(not (string-equal first-try second-try))
	(message "Password not verified.")
      (if (> (length first-try) max-term-lock-password-length)
	  (message "Password longer than max-term-lock-password-length")
	(if (= (length first-try) 0)
	    (message "Null password is not valid.")
	  (setq informative-message
		(read-string "Enter optional message: "))
	  (setq window-configuration-before-lock
		(current-window-configuration))
	  (switch-to-buffer "***LOCKED***")
	  (delete-other-windows)
	  (insert "Enter password followed by <CR> to exit")
	  (center-line)
	  (goto-char (point-max))
	  ; This is obviously a klunky way of centering text...
	  (insert "\n\n\n\n\n\n\n\n\n\n")
	  (insert informative-message)
	  (insert "\n")
	  (if (and
		(stringp term-lock-signature)
		(not (string-equal informative-message "")))
	      (insert term-lock-signature "\n"))
	  (center-region
	   (save-excursion
	     (search-backward "\n\n\n")
	     (+ (point) 2))
	   (point-max))
	  (goto-char (point-min))
	  ; Just to rub it in...  :->#
	  (setq buffer-read-only t)
	  (setq typed-password "")
	  (term-lock-mode first-try)))))
) ;term-lock

(defun term-lock-mode (password)
  "Kicks up a '***LOCKED***' buffer, and waits for the password."
  (kill-all-local-variables)
  (setq major-mode 'term-lock-mode)
  (setq mode-name "Term-Lock")
  (setq initial-password password)
  (use-local-map term-lock-map)
) ;term-lock-mode

(defun add-to-typed-password ()
  "Append the character to the typed password."
  (interactive)
  (setq typed-password (concat typed-password
			       (substring (recent-keys) -1)))
  (if (> (length typed-password) max-term-lock-password-length)
      (progn
	(setq typed-password "")
	(message "Did something fall on your terminal?")
	(sit-for 3 t))
    (message ""))
) ;add-to-typed-password

(defun check-typed-password ()
  "Compare passwords, and exit if they are the same."
  (interactive)
  (if (string-equal initial-password typed-password)
      (progn
	(kill-buffer (current-buffer))
	(set-window-configuration window-configuration-before-lock))
    (setq typed-password "")
    (message "Get a real password."))
) ;check-typed-password

(defun read-string-no-echo (msg)
  "Return a string read from the keyboard, without echo to the minibuffer.
Keep MSG, a string, in the minibuffer during entry of the string."
  (interactive)
  (message msg)
  (let ((char (read-char))
	(cursor-in-echo-area t)
	(string ""))
    (while (/= char ?\C-m)
      (setq string (concat string (list char)))
      (message msg)
      (setq char (read-char)))
    string)
) ;read-string-no-echo

(setq term-lock-map (make-keymap))
; Note that we are rebinding C-g, here.  Sport Death!
(substitute-key-definition nil 'add-to-typed-password term-lock-map)
(define-key term-lock-map "\C-l" 'recenter)
(define-key term-lock-map "\C-m" 'check-typed-password)
;------------------------------ cut here ------------------------------
;
;
;Hope y'all find this useful!
;--woodstock
;-- 
;	   "What I like is when you're looking and thinking and looking
;	   and thinking...and suddenly you wake up."   - Hobbes
;
;nhess@dvlseq.oracle.com or ...!uunet!oracle!nhess or (415) 598-3046


