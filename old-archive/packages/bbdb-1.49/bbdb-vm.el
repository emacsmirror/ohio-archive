;;; -*- Mode:Emacs-Lisp -*-

;;; This file is the part of the Insidious Big Brother Database (aka BBDB),
;;; copyright (c) 1991, 1992, 1993 Jamie Zawinski <jwz@lucid.com>.
;;; Interface to VM (View Mail) 5.31 or greater.  See bbdb.texinfo.
;;; last change 24-jul-93.

;;; The Insidious Big Brother Database is free software; you can redistribute
;;; it and/or modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 1, or (at your
;;; option) any later version.
;;;
;;; BBDB is distributed in the hope that it will be useful, but WITHOUT ANY
;;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;; details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(require 'bbdb)
(require 'vm)
;(require 'vm-motion) ; not provided, dammit!
;(require 'vm-summary)
(if (not (fboundp 'vm-record-and-change-message-pointer)) (load-library "vm-motion"))
(if (not (fboundp 'vm-su-from)) (load-library "vm-summary"))
(or (boundp 'vm-mode-map) (load-library "vm-vars"))

(defun bbdb/vm-update-record (&optional offer-to-create)
  "returns the record corresponding to the current VM message, creating or
modifying it as necessary.  A record will be created if 
bbdb/mail-auto-create-p is non-nil, or if OFFER-TO-CREATE is true and
the user confirms the creation."
  (save-excursion
  (and vm-mail-buffer (set-buffer vm-mail-buffer))
  (if bbdb-use-pop-up
      (bbdb/vm-pop-up-bbdb-buffer offer-to-create)
    (let ((msg (car vm-message-pointer))
	  (inhibit-local-variables nil)	; vm binds this to t...
	  (enable-local-variables t)	; ...or vm bind this to nil.
	  (inhibit-quit nil))	; vm damn well better not bind this to t!
      ;; this doesn't optimize the case of moving thru a folder where
      ;; few messages have associated records.
      (or (bbdb-message-cache-lookup msg nil)  ; nil = current-buffer
	  (and msg
	    ;; ## Note: once VM uses mail-extr.el, we should just get the
	    ;; ## name and address from `vm-su-full-name' and `vm-su-from'
	    ;; ## instead of parsing it again here.
	    (save-excursion
	      (save-restriction
		;; Select the buffer containing the message.
		;; Needed to handle VM virtual folders.
		(set-buffer (marker-buffer (vm-start-of msg)))
		(widen)
		(narrow-to-region (vm-start-of msg) (vm-end-of msg))
		(let ((from (mail-fetch-field "from")))
		  (if (or (null from)
			  (string-match (bbdb-user-mail-names)
			    ;; mail-strip-quoted-names is too broken!
			    ;;(mail-strip-quoted-names from)
			    (car (cdr (mail-extract-address-components
				       from)))))
		      ;; if logged in user sent this, use recipients.
		      (setq from (or (mail-fetch-field "to") from)))
		  (if from
		      (bbdb-encache-message msg
			(bbdb-annotate-message-sender from t
			  (or (bbdb-invoke-hook-for-value
			       bbdb/mail-auto-create-p)
			      offer-to-create)
			  offer-to-create))))))))))))

(defun bbdb/vm-annotate-sender (string)
  "Add a line to the end of the Notes field of the BBDB record 
corresponding to the sender of this message."
  (interactive (list (if bbdb-readonly-p
			 (error "The Insidious Big Brother Database is read-only.")
			 (read-string "Comments: "))))
  (vm-follow-summary-cursor)
  (bbdb-annotate-notes (bbdb/vm-update-record t) string))


(defun bbdb/vm-edit-notes (&optional arg)
  "Edit the notes field or (with a prefix arg) a user-defined field
of the BBDB record corresponding to the sender of this message."
  (interactive "P")
  (vm-follow-summary-cursor)
  (let ((record (or (bbdb/vm-update-record t) (error ""))))
    (bbdb-display-records (list record))
    (if arg
	(bbdb-record-edit-property record nil t)
      (bbdb-record-edit-notes record t))))

(defun bbdb/vm-show-sender ()
  "Display the contents of the BBDB for the sender of this message.
This buffer will be in bbdb-mode, with associated keybindings."
  (interactive)
  (vm-follow-summary-cursor)
  (let ((record (bbdb/vm-update-record t)))
    (if record
	(bbdb-display-records (list record))
	(error "unperson"))))


(defun bbdb/vm-pop-up-bbdb-buffer (&optional offer-to-create)
  "Make the *BBDB* buffer be displayed along with the VM window(s),
displaying the record corresponding to the sender of the current message."
  (bbdb-pop-up-bbdb-buffer
    (function (lambda (w)
      (let ((b (current-buffer)))
	(set-buffer (window-buffer w))
	(prog1 (eq major-mode 'vm-mode)
	  (set-buffer b))))))
  (let ((bbdb-gag-messages t)
	(bbdb-use-pop-up nil)
	(bbdb-electric-p nil))
    (let ((record (bbdb/vm-update-record offer-to-create))
	  (bbdb-elided-display (bbdb-pop-up-elided-display))
	  (b (current-buffer)))
      (bbdb-display-records (if record (list record) nil))
      (set-buffer b)
      record)))

(defun bbdb/vm-record-and-change-message-pointer (old new)
  (prog1 (bbdb-orig-vm-record-and-change-message-pointer old new)
    (bbdb/vm-update-record nil)))

(defun bbdb-insinuate-vm ()
  "Call this function to hook BBDB into VM."
  (cond ((boundp 'vm-show-message-hook)
	 (bbdb-add-hook 'vm-show-message-hook 'bbdb/vm-update-record)
	 ;; Here too?  I don't use preview, so I don't know if this would win.
	 ;(bbdb-add-hook 'vm-preview-message-hook 'bbdb/vm-update-record)
	 )
	(t
	 ;; Hack on to vm-record-and-change-message-pointer, since VM 5.32
	 ;; doesn't have vm-show-message-hook.
	 (or (fboundp 'bbdb-orig-vm-record-and-change-message-pointer)
	     (fset 'bbdb-orig-vm-record-and-change-message-pointer
		   (symbol-function 'vm-record-and-change-message-pointer)))
	 (fset 'vm-record-and-change-message-pointer
	       (symbol-function 'bbdb/vm-record-and-change-message-pointer))
	 ))
  (define-key vm-mode-map ":" 'bbdb/vm-show-sender)
  (define-key vm-mode-map ";" 'bbdb/vm-edit-notes)
  )

(provide 'bbdb-vm)
