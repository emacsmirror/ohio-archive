;;; etalk talk buffer edit controls
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
;;;   To encompass all editing functions associated with etalk tcp connections.


(defvar etalk-nice-fill-offset 5
  "*Any SPC after (fill-column - etalk-nice-fill-offset) inserts a CR
automatically.")

(defvar etalk-hard-fill-offset 5
  "*Any char after (fill-column + etalk-hard-fill-offset) column will
fill that line down right away.")

(defvar etalk-insert-pester-limit 1000 
  "*Limit at which talk pesters you about inserting _THAT MUCH_ across
the network during any of the insert-file commands.")

(defun etalk-action-to-local (key-or-function)
  "Internal: User keypress, is interpreted by other function, which
calls this function to 1) send a key or 2) simply edit the local buffer
by applying a function to it at talk-point."

  (let ((buffer (if (equal mode-name etalk-remote-mode-string)
		    ;; we are in a remote window. output goes only to
		    ;; the single remote process.
		    (current-buffer)
		  ;; we are in some other buffer of unknown origin, so
		  ;; we then send output to all remote talk processes.
		  nil))
	(af nil)) ;; default.. don't do auto fill
  
    (if (integerp key-or-function) ;; check if key, then send it to process.
	(etalk-send-output buffer (char-to-string key-or-function)))
  
    (save-excursion   ;; modify the buffer.
      (set-buffer (get-buffer (etalk-format etalk-local-buffer-name)))
      (goto-char etalk-point)
      (if (integerp key-or-function)
	  (insert-char key-or-function 1)
	(eval key-or-function))
      (move-marker etalk-point (point))
      ;; Ok!  Now lets see if we should fill.
      (if (and
	   (or (fboundp 'auto-fill-function)
	       (and 
		;; make sure it exists.
		(boundp 'auto-fill-function)
		;; is the fill hook on?
		auto-fill-function))
	   (or 
	    ;; fill on the SPC key?
	    (and (> (current-column) (- fill-column etalk-nice-fill-offset))
		 (equal key-or-function ?\ ))
	    ;; i have to fill now!!
	    (> (current-column) (+ fill-column etalk-hard-fill-offset))))
	  (setq af t)))
    (if af (etalk-auto-fill-now))	;must be outside of save-excursion
    (etalk-move-realpoint (get-buffer (etalk-format etalk-local-buffer-name))
			 (save-excursion
			   (set-buffer 
			    (get-buffer (etalk-format etalk-local-buffer-name)))
			   etalk-point))))

(defun etalk-move-realpoint (buffer &optional themark)
  "Internal: Move the real point assocaited with buffer to the
location pointed at by etalk-point."

  (if (not (bufferp buffer))
      (error "That buffer no longer exists!"))

  (if (not themark)
      (setq themark etalk-point))

  ;; This is to check to makesure that you aren't in the minibuffer when 
  ;; this command is run.  If so, simply wait till later to move the point.
  ;; Otherwise minibuffer gets all confused.
  (if (and (windowp (minibuffer-window))
	   (not (equal (selected-window) (minibuffer-window))))
      (progn
	
	(if (eq (current-buffer) buffer)
	    (goto-char themark)
	  
	  (if (get-buffer-window buffer)
	      (let ((oldwindow (selected-window)))
		(setq oldwindow (selected-window))
		(select-window (get-buffer-window buffer))
		(goto-char themark)
		(select-window oldwindow))
	    (let ((oldbuffer (current-buffer)))
	      (set-buffer buffer)
	      (goto-char themark)
	      (set-buffer oldbuffer)))))))

;; -------------------------------------------------------------- ;;
;; The following functions are mostly key functions and functions ;;
;; passed to talk-action-local, and used by talk-filter-proc.     ;;
;; -------------------------------------------------------------- ;;

(defun etalk-insert-char ()
  "called by all keypresses, except a few, and sends them to
the remote, which is basically all talk does too."
  (interactive)
  (etalk-action-to-local last-input-char))

(defun etalk-auto-fill-now ()
  "This function will be called to automatically wrap a word
down to the next line for all versions of talk."
  (interactive)
  (let ((buflist (etalk-send-where)))
    (if (not (equal last-input-char ?\ ))
	(progn
	  (etalk-action-to-local '(etalk-delete-word-backwards-not-past-eob)) 
	  (if (< (length (car kill-ring-yank-pointer)) fill-column)
	      (progn
		(etalk-send-output buflist 
				  (substring etalk-edit-characters-mine 2 3))
		(etalk-action-to-local 10)
		(etalk-yank-text))
	    (save-excursion
	      (set-buffer (get-buffer (etalk-format etalk-local-buffer-name)))
	      (insert-before-markers (car kill-ring-yank-pointer))
	      (etalk-action-to-local 10)))
	  (setq kill-ring-yank-pointer (cdr kill-ring-yank-pointer)))
      (etalk-action-to-local 10))))

(defun etalk-string-to-remote (text)
  "Send the string in text into the local buffer, and send
to remote talk as well.  Sends formatted as in string with no fill"

  (interactive)
  (let ((buffer (if (equal mode-name etalk-remote-mode-string)
		    (current-buffer) nil)))
    (etalk-send-output buffer text)
    (save-excursion
      (set-buffer (get-buffer (etalk-format etalk-local-buffer-name)))
      (goto-char etalk-point)
      (insert text)
      (move-marker etalk-point (point))))
  (etalk-move-realpoint (get-buffer (etalk-format etalk-local-buffer-name))
		       (save-excursion
			 (set-buffer 
			  (get-buffer (etalk-format etalk-local-buffer-name)))
			 etalk-point)))

(defun etalk-yank-text ()
  "Copy the contents of the kill ring into a talk window."
  (interactive)
  (etalk-string-to-remote (car kill-ring-yank-pointer)))

(defun etalk-insert-file (filenm)
  "Read in a file name, and insert it into the conversation buffer."
  (interactive "FTalk insert: ")
  (let ((flag t))
    (if (> (nth 7 (file-attributes filenm)) etalk-insert-pester-limit)
	(if (y-or-n-p (format "%s has %d bytes.  Really insert?"
			      filenm (nth 7 (file-attributes filenm))))
	    ()
	  (setq flag nil)))
    (if flag
	(progn
	  (let ((fname (find-file-noselect filenm)))
	    (etalk-string-to-remote
	     (save-excursion
	       (set-buffer fname)
	       (buffer-substring (point-min) (point-max)))))))))

(defun etalk-insert-file-buffer (filenm)
  "Read in a file, and make it appear on both sides of talk connection."
  (interactive "FTalk insert buffered: ")
  (let ((flag t))
    (if (> (nth 7 (file-attributes filenm)) etalk-insert-pester-limit)
	(if (y-or-n-p (format "%s has %d bytes.  Really insert?"
			      filenm (nth 7 (file-attributes filenm))))
	    ()
	  (setq flag nil)))
    (if flag
	(progn
	  (let ((fname (find-file-noselect filenm))
		(buffer (if (equal mode-name etalk-remote-mode-string)
			    (current-buffer) nil)))
	    (etalk-send-output buffer (format "\03\07%s\n" filenm))
	    (etalk-send-output buffer
			      (save-excursion
				(set-buffer fname)
				(make-local-variable 'etalk-tag)
				(setq etalk-tag t)
				(buffer-substring (point-min) (point-max))))
	    (etalk-send-output buffer "\03\07End of file.\n")
	    (etalk-setup-windows-plus fname))))))

(defun etalk-beep ()
  "This command will beep."
  (interactive)
  (let ((buffer (if (equal mode-name etalk-remote-mode-string)
		    (current-buffer) nil)))
    (if (not etalk-remote-is-emacs)
	(etalk-insert-char)
      (ding)
      (etalk-send-output buffer "\C-g"))))
  
(defun etalk-backward-delete-not-past-eob ()
  "Delete one word backwards, but don't go past beginning of
line"
  (if (or (not (bolp)) etalk-remote-is-emacs) (backward-delete-char 1)))

(defun etalk-delete-backwards ()
  "Send delete a character in your buffer, and send delete 
character." 

  (interactive)
  ;; we must check not to send too many, or multi-groups get baffled.
  (if (or (not (bolp)) etalk-remote-is-emacs)
      (etalk-send-output (etalk-send-where) 
			 (char-to-string (aref etalk-edit-characters-mine 0))))
  (etalk-action-to-local '(etalk-backward-delete-not-past-eob)))

(defun etalk-RET ()
  "Send carriage return."
  (interactive)
  (etalk-action-to-local 10))

(defun etalk-delete-backward-to-char-or-bol (DACHAR)
  "Delete from the cursor backwards to CHAR, or to beginning
of line if CHAR is nil.  Store result in the kill ring."

  ;; if DACHAR is nil, it kills to bol.
  (save-excursion
    ;; first go until non-space or bol
    (let* ((beg-word 0)
	   (str (buffer-substring 
		 (point)
		 (save-excursion
		   (while (and (equal (preceding-char) DACHAR) (not (bolp)))
		     (forward-char -1)) 
		   (while (not (or (equal (preceding-char) DACHAR) (bolp) ))
		     (forward-char -1))
		   (setq beg-word (point))))))
      (delete-region (point) beg-word)
      (setq kill-ring-yank-pointer (cons str kill-ring-yank-pointer)))))

(defun etalk-delete-word-backwards-not-past-eob ()
  "Delete a word backwards, but don't go past bol."
  (if (and (bolp) etalk-remote-is-emacs) (backward-delete-char 1))
  (if (not (bolp))
    (etalk-delete-backward-to-char-or-bol (string-to-char " "))))

(defun etalk-delete-word-backwards ()
  "Delete character backwards in local and remote."
  (interactive)
  (if (or (not (bolp)) etalk-remote-is-emacs)
      (etalk-send-output (etalk-send-where)
			 (char-to-string (aref etalk-edit-characters-mine 2))))
  (etalk-action-to-local '(etalk-delete-word-backwards-not-past-eob)))

(defun etalk-delete-line (&optional sendit)
  "Delete line backwards in local and remote."
  (interactive)
  (if (and etalk-remote-is-emacs (bolp))
      (etalk-backward-delete-not-past-eob))
  (if (not sendit)
      (etalk-send-output (etalk-send-where) 
			 (char-to-string (aref etalk-edit-characters-mine 1))))
  (etalk-action-to-local '(etalk-delete-backward-to-char-or-bol nil)))

(defun etalk-brief-help ()
  "Brief description of valid keys."
  (interactive)
  (if (equal last-command 'etalk-brief-help)
      (progn
	(describe-mode)
	(etalk-setup-windows "*Help*"))
    (message 
 "[DEL] char [C-w] Del word [C-k] Del line [C-c C-c] Hangup [C-h] more help."
     )))

(defun etalk-clear-window ()
  "clears the current window, and places the cursor on the top line 
of the window with whatever line was being edited.  Doesn't delete 
anything from any buffers."

  (interactive)
  (etalk-send-output (etalk-send-where) "")
  (let ((oldbuffer (current-buffer)))
    (set-buffer (get-buffer (etalk-format etalk-local-buffer-name)))
    (recenter 0)
    (set-buffer oldbuffer))
  (redraw-display))

;;; end lisp
(provide 'etalk-edit)

