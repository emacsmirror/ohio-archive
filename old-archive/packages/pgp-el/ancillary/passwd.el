;;; passwd.el --- Prompting for passwords semi-securely

;; Copyright (C) 1994 Free Software Foundation, Inc.

;; Author: Jamie Zawinski <jwz@lucid.com>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Change Log:
;;
;;  Sun Jun 12 04:19:30 1994 by sandy on ibm550.sissa.it
;;    Added support for password histories and (provide 'passwd)

;;; Code:
(provide 'passwd)

(defvar passwd-echo ?.
  "*The character which should be echoed when typing a password,
or nil, meaning echo nothing.")

(defun passwd-make-keymap ()
  (let ((i 0)
	(s (make-string 1 0))
	map)
    (if (fboundp 'set-keymap-parent)
	(set-keymap-parent (setq map (make-keymap)) minibuffer-local-map)
      ;; v18/FSFmacs compatibility
      (setq map (copy-keymap minibuffer-local-map)))
    (if (fboundp 'set-keymap-name)
	(set-keymap-name map 'read-passwd-map))
    (while (< i 127)
      (aset s 0 i)
      (define-key map s 'self-insert-command)
      (setq i (1+ i)))
    map))

(defvar read-passwd-map
  (let ((map (passwd-make-keymap)))
    (define-key map "\b" 'delete-backward-char)
    (define-key map "\C-g" 'keyboard-quit)
    (define-key map "\r" 'exit-minibuffer)
    (define-key map "\n" 'exit-minibuffer)
    (define-key map "\C-u" 'passwd-erase-buffer)
    (define-key map "\C-q" 'quoted-insert)
    (define-key map "\177" 'delete-backward-char)
    (define-key map "\e"
      (let ((emap (passwd-make-keymap)))
	(define-key emap "n" 'passwd-next-history-element)
	(define-key emap "p" 'passwd-previous-history-element)
	(define-key emap "\b" 'backward-kill-word)
	emap))
    map)
  "Keymap used for reading passwords in the minibuffer.
The \"bindings\" in this map are not real commands; only a limited
number of commands are understood.  The important bindings are:
\\<read-passwd-map>
	\\[passwd-erase-buffer]	Erase all input.
	\\[quoted-insert]	Insert the next character literally.
	\\[delete-backward-char]	Delete the previous character.
	\\[exit-minibuffer]	Accept what you have typed.
	\\[keyboard-quit]	Abort the command.

All other characters insert themselves (but do not echo.)")

;;; internal variables

(defvar passwd-history nil)
(defvar passwd-history-posn 0)
(defconst passwd-emacs-flavour
  ;; Be careful about match data, because ange-ftp and efs may prompt for
  ;; passwords from filter functions.
  (let ((md (match-data)))
    (unwind-protect
	(cond
	 ((string-match "Lucid" emacs-version)
	  'lucid)
	 ((string-match "^19" emacs-version)
	  'fsf)
	 (t 18))
      (store-match-data md))))

;;;###autoload
(defun read-passwd (prompt &optional confirm default)
  "Prompts for a password in the minibuffer, and returns it as a string.
If PROMPT may be a prompt string or an alist of elements 
'\(prompt . default\).
If optional arg CONFIRM is true, then ask the user to type the password
again to confirm that they typed it correctly.
If optional arg DEFAULT is provided, then it is a string to insert as
the default choice (it is not, of course, displayed.)

If running under X, the keyboard will be grabbed (with XGrabKeyboard())
to reduce the possibility that evesdropping is occuring.

When reading a password, all keys self-insert, except for:
\\<read-passwd-map>
	\\[read-passwd-erase-line]	Erase the entire line.
	\\[quoted-insert]	Insert the next character literally.
	\\[delete-backward-char]	Delete the previous character.
	\\[exit-minibuffer]	Accept what you have typed.
	\\[keyboard-quit]	Abort the command.

The returned value is always a newly-created string.  No additional copies
of the password remain after this function has returned.

NOTE: unless great care is taken, the typed password will exist in plaintext
form in the running image for an arbitrarily long time.  Priveleged users may
be able to extract it from memory.  If emacs crashes, it may appear in the
resultant core file.

Some steps you can take to prevent the password from being copied around:

 - as soon as you are done with the returned string, destroy it with
   (fillarray string 0).  The same goes for any default passwords
   or password histories.

 - do not copy the string, as with concat or substring - if you do, be
   sure to keep track of and destroy all copies.

 - do not insert the password into a buffer - if you do, be sure to 
   overwrite the buffer text before killing it, as with the functions 
   `passwd-erase-buffer' or `passwd-kill-buffer'.  Note that deleting
   the text from the buffer does NOT necessarily remove the text from
   memory.

 - be careful of the undo history - if you insert the password into a 
   buffer which has undo recording turned on, the password will be 
   copied onto the undo list, and thus recoverable.

 - do not pass it as an argument to a shell command - anyone will be
   able to see it if they run `ps' at the right time.

Note that the password will be temporarily recoverable with the `view-lossage'
command.  This data will not be overwritten until another hundred or so 
characters are typed.  There's not currently a way around this."

  (save-excursion
    (let ((input (get-buffer-create " *password*"))
	  (passwd-history-posn 0)
	  passwd-history)
      (if (listp prompt)
	  (setq passwd-history prompt
		default (cdr (car passwd-history))))
      (set-buffer input)
      (buffer-disable-undo input)
      (use-local-map read-passwd-map)
      (unwind-protect
	  (progn
	    (passwd-grab-keyboard)
	    (read-passwd-1 input prompt nil default)
	    (set-buffer input)

	    (if (not confirm)
		(buffer-string)
	      (let ((ok nil)
		    passwd)
		(while (not ok)
		  (set-buffer input)
		  (setq passwd (buffer-string))
		  (read-passwd-1 input prompt "[Retype to confirm]")
		  (if (passwd-compare-string-to-buffer passwd input)
		      (setq ok t)
		    (fillarray passwd 0)
		    (setq passwd nil)
		    (beep)
		    (read-passwd-1 input prompt "[Mismatch. Start over]")
		    ))
		passwd)))
	;; protected
	(passwd-ungrab-keyboard)
	(passwd-kill-buffer input)
	(if (eq passwd-emacs-flavour 18)
	    (message "")
	  (message nil))
	))))


(defun read-passwd-1 (buffer prompt &optional prompt2 default)
  (set-buffer buffer)
  (passwd-erase-buffer)
  (if default (insert default))
  (catch 'exit ; exit-minibuffer throws here
    (while t
      (set-buffer buffer)
      (let* ((minibuffer-completion-table nil)
	     (cursor-in-echo-area t)
	     (echo-keystrokes 0)
	     (key (passwd-read-key-sequence
		   (concat (if (listp prompt)
			       (car (nth passwd-history-posn passwd-history))
			     prompt)
			   prompt2
			   (if passwd-echo
			       (make-string (buffer-size) passwd-echo)))))
	     (binding (key-binding key)))
	(setq prompt2 nil)
	(set-buffer buffer)		; just in case...
	(if (eq passwd-emacs-flavour 'lucid)
	    ;; XEmacs
	    (setq last-command-event (aref key (1- (length key)))
		  last-command-char (event-to-character last-command-event))
	  ;; GNU Emacs compatibility
	  (setq last-command-char (aref key (1- (length key)))))
	(setq this-command binding)
	(condition-case c
	    (command-execute binding)
	  (error
	   (beep)
	   (if (fboundp 'display-error)
	       ;; XEmacs
	       (display-error c t)
	     ;; GNU Emacs
	     (message (concat (or (get (car-safe c) 'error-message) "???")
			      (if (cdr-safe c) ": ")
			      (mapconcat 
			       (function (lambda (x) (format "%s" x)))
			       (cdr-safe c) ", "))))
	   (sit-for 2)))
	))))

(defun passwd-previous-history-element (n)
  (interactive "p")
  (or passwd-history
      (error "Password history is empty."))
  (let ((l (length passwd-history)))
    (setq passwd-history-posn
	  (% (+ n passwd-history-posn) l))
    (if (< passwd-history-posn 0)
	(setq passwd-history-posn (+ passwd-history-posn l))))
  (let ((obuff (current-buffer))) ; want to move point in passwd buffer
    (unwind-protect
	(progn
	  (set-buffer " *password*")
	  (passwd-erase-buffer)
	  (insert (cdr (nth passwd-history-posn passwd-history))))
      (set-buffer obuff))))

(defun passwd-next-history-element (n)
  (interactive "p")
  (passwd-previous-history-element (- n)))

(defun passwd-erase-buffer ()
  ;; First erase the buffer, which will simply enlarge the gap.
  ;; Then insert null characters until the gap is filled with them
  ;; to prevent the old text from being visible in core files or kmem.
  ;; (Actually use 3x the size of the buffer just to be safe - a longer
  ;; passwd might have been typed and backspaced over.)
  (interactive)
  (widen)
  (let ((s (* (buffer-size) 3)))
    (erase-buffer)
    (while (> s 0)
      (insert ?\000)
      (setq s (1- s)))
    (erase-buffer)))

(defun passwd-kill-buffer (buffer)
  (save-excursion
    (set-buffer buffer)
    (buffer-disable-undo buffer)
    (passwd-erase-buffer)
    (set-buffer-modified-p nil))
  (kill-buffer buffer))


(defun passwd-compare-string-to-buffer (string buffer)
  ;; same as (equal string (buffer-string)) but with no dangerous consing.
  (save-excursion
    (set-buffer buffer)
    (goto-char (point-min))
    (let ((L (length string))
	  (i 0))
      (if (/= L (- (point-max) (point-min)))
	  nil
	(while (not (eobp))
	  (if (/= (following-char) (aref string i))
	      (goto-char (point-max))
	    (setq i (1+ i))
	    (forward-char)))
	(= (point) (+ i (point-min)))))))


(defun passwd-grab-keyboard ()
  (cond ((not (and (fboundp 'x-grab-keyboard) ; lemacs 19.10+
		   (eq 'x (live-screen-p (selected-screen)))))
	 nil)
	((x-grab-keyboard)
	 t)
	(t
	 (message "Unable to grab keyboard - waiting a second...")
	 (sleep-for 1)
	 (cond ((x-grab-keyboard)
		(message "Keyboard grabbed on second try.")
		t)
	       (t
		(beep)
		(message "WARNING: keyboard is insecure (unable to grab!)")
		(sleep-for 3)
		nil)))))

(defun passwd-ungrab-keyboard ()
  (if (and (fboundp 'x-ungrab-keyboard) ; lemacs 19.10+
	   (eq 'x (live-screen-p (selected-screen))))
      (x-ungrab-keyboard)))

;; v18 compatibility
(or (fboundp 'buffer-disable-undo)
    (fset 'buffer-disable-undo 'buffer-flush-undo))

;; read-key-sequence echoes the key sequence in Emacs 18.
(if (eq passwd-emacs-flavour 18)
    (defun passwd-read-key-sequence (prompt)
      (let ((inhibit-quit t)
	    str)
	(while (or (null str) (keymapp (key-binding str)))
	  (message prompt)
	  (setq str (concat str (char-to-string (read-char)))))
	(setq quit-flag nil)
	str))
  (fset 'passwd-read-key-sequence 'read-key-sequence))

;;; passwd.el ends here
