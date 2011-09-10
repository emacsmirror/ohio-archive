;;; labbook-sign.el --- provides support for PGP signing the labbook.
;; $Revision: 1.1 $
;; $Date: 2000/04/15 17:57:44 $

;; This file is not part of Emacs

;; Author: Phillip Lord<p.lord@hgmp.mrc.ac.uk>
;; Maintainer: Phillip Lord<p.lord@hgmp.mrc.ac.uk>
;; Keywords: logging labbook signing

;;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Status:
;;
;; This file is somewhat hacky and will probably remain so. It should
;; at the moment be considered beta release software until its got
;; a little more use. 

;;; Commentary:
;;
;; This file is an add-on to labbook.el and provides additional
;; support for PGP signing of the labbook. This file uses MailCrypt
;; for the majority of its functionality. At the moment it can not
;; automatically indentify start of the signature, you have to set the
;; `labbook-sign-start-of-signature' variable up for this. Its
;; defaults to pgp 5.0.

;;; Bugs
;;
;; 1) Not exactly a bug, but this uses the string which changelog
;; puts into the buffer at the begining of each entry. I didnt really
;; want this anyway and would want to remove it at some point, which
;; will of course screw up this functionality. At some point I would
;; like labbook.el to provide some sort of "foward entry"
;; functionality anyway, which would do what I want better.

;;; Todo
;; 
;; 1) The signature narrowing functions are frankly crap, and need to
;; be massively improved. Should pick through mail-crypt to find out
;; whether it provides this sort of additional functionality. Yes it
;; does! See code for `mc-verify-signature' which has stuff for doing this.
;; 2) Need to fit stuff in for mailcrypt so that it verifies correctly.

;;; History
;;
;; $Log: labbook-sign.el,v $
;; Revision 1.1  2000/04/15 17:57:44  phil
;; Initial checkin
;;


(require 'labbook)
(require 'mailcrypt)
(load "mc-toplev")

(defvar labbook-sign-alist '(change-log-mode
			     (verify . labbook-sign-verify)
			     (sign .labbook-sign-mc-sign)))

(setq mc-modes-alist
      (cons labbook-sign-alist
	    mc-modes-alist))
			     

(add-hook 'labbook-before-enter-hook 'labbook-sign-entering-mode)
(add-hook 'labbook-after-enter-hook 'labbook-mailcrypt-select-mode)
(add-hook 'labbook-exit-hook 'labbook-sign-resign-if-necessary)

(defun labbook-sign-entering-mode()
  "Narrows the buffer to ignore the PGP signatures"
  ;;we want to save the excursion here, because otherwise the entry
  ;;name will be not be found
  (save-excursion
    (set-buffer (find-file-noselect labbook-file))
    (labbook-sign-narrow-to-contents)
    (font-lock-fontify-buffer)))

(defun labbook-sign-narrow-to-contents()
  (narrow-to-region
   (progn (labbook-sign-narrow-to-message-start)
	  (point-max))
   (progn (labbook-sign-narrow-to-signature)
	  (point-min))))

(defun labbook-mailcrypt-select-mode()
  (if view-mode
      (mc-read-mode)
    (mc-write-mode)))

(defun labbook-sign-narrow-to-message-start()
  "Narrow the buffer to the message start information."
  (widen)
  (goto-char (point-min))
  (narrow-to-region
   ;;from the start of the buffer
   (point-min)
   ;;the start of the buffer or the end of the given line
   (or (if (re-search-forward "Hash: SHA1[ ]*$" nil t)
	   ;;make sure that we blitz an extra new lines
	   (progn (re-search-forward "[a-zA-Z0-9]" nil t)
		  (beginning-of-line)
		  (point)))
       1)))
   
(defun labbook-sign-narrow-to-signature()
  "Narrow the buffer to the signature itself."
  (widen)
  (goto-char (point-min))
  (narrow-to-region
   ;;find the start of the sig, if there or else point max
   (if (search-forward "BEGIN PGP SIGNATURE" nil t)
       (progn (beginning-of-line)
	      (point))
     (point-max))
   (if (search-forward "END PGP SIGNATURE" nil t)
       (progn (end-of-line)
	      (point))
     (point-max))))

(defun labbook-sign-resign-if-necessary()
  (save-restriction
    (labbook-sign-narrow-to-signature)
    ;; if there is any signature in the first place
    (if (not (eq (point-min) (point-max)))
	(progn 
	  (widen)
	  ;;this bit doesnt work. mc-verify does not appear to parse
	  ;;the output of pgp just stuffs it out to screen. Will have
	  ;;to do something like having a pgp last signed file
	  (if (labbook-sign-signature-out-of-date-p)   
	      (if (y-or-n-p "Signature out of date. Resign? ")
		  (labbook-sign-resign))))
      (if (y-or-n-p "Labbook is not signed. Sign now? ")
	  (progn (widen)
		 (labbook-sign-signature))))))

(defun labbook-sign-mc-sign (ignore all the arguments here)
  (labbook-sign-resign)
  (labbook-sign-narrow-to-contents)
  (goto-char(point-min)))
  
(defun labbook-sign-resign()
  (save-excursion
    (save-restriction
      (if view-mode
	  (view-mode-exit))
      (labbook-sign-narrow-to-signature)
      (delete-region (point-min) (point-max))
      (labbook-sign-narrow-to-message-start)
      (delete-region (point-min) (point-max))
      (labbook-sign-signature))))

(defun labbook-sign-signature-out-of-date-p()
  ;;if the buffer is modified then the sig is out of date
  (if (buffer-modified-p (get-file-buffer labbook-file))
      t
    ;;if not this may be because it has been manually saved, so we have
    ;;to compare modified dates with a file we keep for the purpose.
    (let* ((compare-file (labbook-sign-get-compare-file))
	   (compare-file-date 
	    (nth 5 (file-attributes compare-file)))
	   (labbook-file-date
	    (nth 5 (file-attributes labbook-file))))
      (if (not compare-file-date)
	  t
	(and (<= (nth 0 compare-file-date) (nth 0 labbook-file-date)) 
	     (<  (nth 1 compare-file-date) (nth 1 labbook-file-date)))))))
    

(defun labbook-sign-signature()
  (save-excursion
    (save-restriction
      ;;sign the labbook buffer
      (widen)
      (set-buffer (get-file-buffer labbook-file))
      (mc-sign-generic nil nil nil nil nil)
      ;;save it now..
      (save-buffer)
      ;;is there a better way to do a touch in lisp?
      (set-buffer (find-file-noselect (labbook-sign-get-compare-file)))
      (erase-buffer)
      (insert " ")
      (save-buffer nil)
      (kill-buffer (current-buffer)))))


(defun labbook-sign-get-compare-file()
  (concat (file-name-directory labbook-file)
	  "." (file-name-nondirectory labbook-file) ".sigdate"))


(defun labbook-sign-verify()
  (save-restriction
    (widen)
    (mc-verify-signature)))
  
(provide 'labbook-sign)







