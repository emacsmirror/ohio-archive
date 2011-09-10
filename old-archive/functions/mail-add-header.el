; Newsgroups: dg.ml.emacs.vm
; Path: dg-rtp!uunet!uunet!elroy.jpl.nasa.gov!decwrl!waikato.ac.nz!aukuni.ac.nz!mike-w
; From: mike-w@cs.aukuni.ac.nz (Mike Williams)
; Subject: Re: Correcting/Augmenting Mail Headers (was: User setting From: address in sendmail input)
; Organization: University of Auckland, New Zealand.
; Date: 3 Jul 91 09:26:27
; 
;   Here's a couple of elisp functions which facilitate adding and removing
;   mail (or news) headers.  Just stick the appropriate calls in your
;   mail-setup-hook, eg.
; 
;     | (setq mail-setup-hook 'my-mail-setup-hook)
;     | 
;     | (defun my-mail-setup-hook ()
;     |   (mail-add-header "Return-Receipt-To" my-email-address)
;     |   (mail-add-header "Reply-To" my-email-address 'replace)
;     |   )
; 
; --- Shnip 'ere ------------------------------------------------------------
; 
(require 'mail-utils)

;; LCD Archive Entry:
;; mail-add-header|Mike Williams|mike-w@cs.aukuni.ac.nz
;; |Functions to add and remove named mail headers
;; |91-07-03||~/functions/mail-add-header.el.Z

(defun mail-add-header (HEADER CONTENTS &optional REPLACE)
  "Add the specified HEADER to the current mail message, with the given 
CONTENTS.  
If the header already exists, the contents are left unchanged, unless optional 
argument REPLACE is non-nil."
  (save-excursion
    (let ((header-exists (mail-position-on-field HEADER)))
      ;; Delete old contents if REPLACE is set
      (if (and header-exists REPLACE)
	  (let ((end (point))
		(beg (progn
		       (re-search-backward (concat HEADER ": "))
		       (goto-char (match-end 0)))))
	    (delete-region beg end)))
      ;; Add new contents if REPLACE is set, or this is a new header.
      (if (or (not header-exists) REPLACE)
	  (progn (insert CONTENTS) CONTENTS)))))

(defun mail-remove-header (HEADER)
  "Remove the specified HEADER from the current mail message."
  (save-excursion
    (if (mail-position-on-field HEADER 'soft)
	(let ((end (point))
	      (beg (progn
		     (re-search-backward (concat HEADER ": "))
		     (goto-char (match-beginning 0)))))
	  (delete-region beg (1+ end))))))
