;From: ange@hplb.hpl.hp.com (Andy Norman)
;Newsgroups: comp.emacs,gnu.emacs
;Subject: ange-crypt: simple encrypt / decrypt support for GNU Emacs
;Message-ID: <ANGE.89Aug14184858@anorman.hpl.hp.com>
;Date: 14 Aug 89 22:48:58 GMT
;Organization: Hewlett-Packard Laboratories, Bristol, UK.
;Lines: 196
;Keywords: encrypt,decrypt,GNU Emacs,ange-crypt,minor mode
;Summary: Simple encrypt / decrypt support for GNU Emacs
;
;I enclose 'ange-crypt.el' which enables GNU Emacs to encrypt and decrypt files
;upon reading and writing.
;
;Enjoy...
;
;P.S. I don't wish to open the worm-can surrounding exportation of crypt from
;the USA. If you haven't got crypt, use something different.
;
;--------------------------------------------------------------------------------
; -*-Emacs-Lisp-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         ange-crypt.el
; RCS:          $Header: ange-crypt.el,v 1.6 89/08/14 18:35:50 ange Exp $
; Description:  semi-transparent encrypt / decrypt for files
; Author:       Andy Norman, Kraken
; Created:      Fri Aug 11 16:59:26 1989
; Modified:     Mon Aug 14 18:35:05 1989 (Ange) ange@hplb.hpl.hp.com
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ange-crypt-mode is a minor mode that can be used with most buffers, and when
;; switched on makes sure that all future writes of the buffer are piped through
;; your favorite encryption command. Visiting a file created by saving a buffer
;; with this mode on will cause the buffer to be initially piped through your
;; favorite decryption command.
;; 
;; Note that by default auto-saving is disabled upon entering ange-crypt-mode so
;; that the plain-text version of the buffer never gets written to file. This
;; action can be disabled by setting the variable ange-crypt-disable-auto-saving
;; to nil.
;;
;; This file is not part of GNU Emacs, but FSF are welcome to it if they want it.
;;
;; Copying is permitted under those conditions described by the GNU General
;; Public License.
;;
;; Copyright (C) 1989 Andy Norman.
;;
;; Author: Andy Norman (ange@hplb.hpl.hp.com)
;;
;; Please mail bugs and suggestions to the author at the above address.

(defvar ange-crypt-encryption-cmd "crypt %s"
  "*The command (with key) to pipe the buffer through to get encryption")

(defvar ange-crypt-decryption-cmd "crypt %s"
  "*The command (with key) to pipe the buffer through to get decryption")

(defvar ange-crypt-disable-auto-saving t
  "*Disable auto-saving when in ange-crypt mode.")

(defvar ange-crypt-key nil
  "The key to encrypt / decrypt a buffer.")

;; Put the next three lines in your .emacs if you wish to have different keys
;; for different buffers.
;;
;; (make-variable-buffer-local 'ange-crypt-key)
;; (setq-default ange-crypt-mode nil)

(defvar ange-crypt-mode nil
  "This buffer is to be read/written encrypted.")

(make-variable-buffer-local 'ange-crypt-mode)
(setq-default ange-crypt-mode nil)

(or (assq 'ange-crypt-mode minor-mode-alist)
    (setq minor-mode-alist (cons '(ange-crypt-mode " Crypt") minor-mode-alist)))

(defun ange-crypt-mode (arg)
  "Toggle ange-crypt mode.
With arg, turn ange-crypt mode on iff arg is positive.
In ange-crypt mode, when the buffer is saved, it is encrypted first,
and when restored, it is decrypted first."
  (interactive "P")
  (setq ange-crypt-mode
	(if (null arg) (not ange-crypt-mode)
	  (> (prefix-numeric-value arg) 0)))
  (if ange-crypt-disable-auto-saving
      (progn
	(delete-auto-save-file-if-necessary)
	(auto-save-mode (not ange-crypt-mode)))) ;enable/disable auto-saves
  (set-buffer-modified-p t))

(defun ange-crypt-get-key ()
  "Prompt the user for the key to encrypt / decrypt the current buffer.
Echos a . for each character typed.
End with <cr>, <lf>, or <esc>.  DEL or backspace rubs out."
  (let ((pass "")
	(c 0)
	(echo-keystrokes 0))
    (while (and (/= c ?\r) (/= c ?\n) (/= c ?\e))
      (message "Enter key to encrypt/decrypt %s: %s"
	       (buffer-name)
	       (make-string (length pass) ?.))
      (setq c (read-char))
      (if (and (/= c ?\b) (/= c ?\177))
	  (setq pass (concat pass (char-to-string c)))
	(if (> (length pass) 0)
	    (setq pass (substring pass 0 -1)))))
    (setq pass (substring pass 0 -1))))

(defun ange-decrypt-buffer ()
  "Decrypts the current buffer if in ange-crypt-mode."
  (if ange-crypt-mode
      (progn
	(let ((mod-p (buffer-modified-p)))
	  (or ange-crypt-key (setq ange-crypt-key (ange-crypt-get-key)))
	  (save-excursion
	    (goto-char (point-max))
	    (search-backward "\n\^L" (max (- (point-max) 3000) (point-min)) 'move)
	    (delete-region (point) (point-max))
	    (message "decrypting...")
	    (shell-command-on-region (point-min)
				     (point-max)
				     (format ange-crypt-decryption-cmd ange-crypt-key)
				     t)
	    (message "decrypting... done"))
	  (if ange-crypt-disable-auto-saving
	      (progn
		(delete-auto-save-file-if-necessary)
		(auto-save-mode 0)))		;disable auto-saving
	  (set-buffer-modified-p mod-p)))))

(defun ange-encrypt-and-write-buffer ()
  "If the buffer is in ange-crypt-mode then encrypt and write to file, otherwise
return nil. This function is mean to be used as a write-file-hook entry."
  (if (not ange-crypt-mode)
      nil				;let basic-save-buffer do the write
    (let ((contents (buffer-substring (point-min) (point-max))))
      (or ange-crypt-key (setq ange-crypt-key (ange-crypt-get-key)))
      (save-excursion
	(message "encrypting...")
	(shell-command-on-region (point-min)
				 (point-max)
				 (format ange-crypt-encryption-cmd ange-crypt-key)
				 t)
	(message "encrypting... done")
	(goto-char (point-max))
	(insert "\n\f\nLocal variables:\nange-crypt-mode:1\nend:\n")
	(if file-precious-flag
	    ;; If file is precious, rename it away before
	    ;; overwriting it.
	    (let ((rename t)
		  (file (concat buffer-file-name "#")))
	      (condition-case ()
		  (progn (rename-file buffer-file-name file t)
			 (setq setmodes (file-modes file)))
		(file-error (setq rename nil)))
	      (unwind-protect
		  (progn (clear-visited-file-modtime)
			 (write-region (point-min) (point-max)
				       buffer-file-name nil t)
			 (setq rename nil))
		;; If rename is still t, writing failed.
		;; So rename the old file back to original name,
		(if rename
		    (progn
		      (rename-file file buffer-file-name t)
		      (clear-visited-file-modtime))
		  ;; Otherwise we don't need the original file,
		  ;; so flush it.
		  (condition-case ()
		      (delete-file file)
		    (error nil)))))
	  ;; If file not writable, see if we can make it writable
	  ;; temporarily while we write it.
	  ;; But no need to do so if we have just backed it up
	  ;; (setmodes is set) because that says we're superseding.
	  (cond ((and tempsetmodes (not setmodes))
		 ;; Change the mode back, after writing.
		 (setq setmodes (file-modes buffer-file-name))
		 (set-file-modes buffer-file-name 511)))
	  (write-region (point-min) (point-max) 
			buffer-file-name nil t)
	  (erase-buffer)
	  (insert contents)
	  (set-buffer-modified-p nil))
	t					;have done the write already
      ))))

(or (memq 'ange-encrypt-and-write-buffer  write-file-hooks)
    (setq write-file-hooks
	  (append write-file-hooks
		  (list 'ange-encrypt-and-write-buffer)))) ;stick it on the end

(or (memq 'ange-decrypt-buffer find-file-hooks)
    (setq find-file-hooks
	  (append find-file-hooks
		  (list 'ange-decrypt-buffer))))

--
					-- ange --

					ange@hplb.hpl.hp.com
