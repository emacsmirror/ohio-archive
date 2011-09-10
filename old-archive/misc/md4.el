;; md4.el -- MD4 support for GNUS
;;
;; SCCS Status     : @(#)@ md4	1.5
;; Author          : Johan Vromans
;; Created On      : Sat May 11 09:10:04 1991
;; Last Modified By: Dave Brennan
;; Last Modified On: Mon Jul  6 16:42:14 1992
;; Update Count    : 13
;; Status          : OK
;;
;; LCD Archive Entry:
;; md4|Johan Vromans|jv@mh.nl|
;; MD4 support for GNUS|
;; 92-06-16|1.5|~/misc/md4.el.Z|
;;
;; This file defines functions to calculate a MD4 signature, add
;; it to outgoing postings, and validate it on incoming postings.
;;
;; It uses "gnus-Inews-article-hook", called by GNUS just before passing
;; the articel to inews, to install the signature.
;;
;; "gnus-Article-prepare-hook" is used to validate the signature on
;; an article if you read it.
;;
;; Advised usage: load this file after loading gnus, e.g. from the
;; gnus-Startup-hook.
;; You also can do this explicitly:
;;
;;      (load "gnus" nil t)
;;	(load "md4" t nil)	; optional
;;	(gnus)
;;
;; This file, if useful, is covered by the GPL.
;;
;;	Johan Vromans <jv@mh.nl>

;; HISTORY 
;; 16-Jun-1992		Johan Vromans	
;;    Added LCD entry.
;; 22-May-1991		Johan Vromans	
;;    Enhanced comments and improved customization.
;;    Added provide 'md4 and require 'add-hook.
;;    Normalized .signature file inclusion.

(provide 'md4)
(or (fboundp 'add-hook)
    (require 'add-hook))	; by Dan LaLiberte <liberte@cs.uiuc.edu>

;; Customizations
;;
;; This function only uses program md4; it doesn't need md4hash nor 
;; md4check.
;; The md4 programs can be retrieved from your nearest comp.sources.misc
;; archive site. Contact the moderator of comp.sources.misc for details.
;;
(defvar md4-command "md4"
  "*Where to find the md4 program. This program should be in your PATH.")

(defvar md4-insertion t
  "*Controls MD4 signature insertion. If nil, no signature is inserted.")

(defvar md4-validation 1
  "*Controls MD4 signature validation. If nil, no validation is
  performed. If t, validation is performed, and failures are reported.
  Any other value causes validation to be performed, and failures as
  well as successes to be reported.")

;; If the variable gnus-signature-file is not null, GNUS will append
;; this file to the article before posting. If null, your .signature
;; is supposed to be added by your news system. In this case, the md4
;; calculation will temporary insert the signature to make sure a
;; correct checksum is calculated.
;; You may have to change the md4-signature-separator if needed.

(defvar md4-signature-separator "-- \n"
  "*If your news posting system appends your .signature file for you, 
  then set this variable to the separator string used. In this case, 
  the signature will be added on behalf of the calculation of the MD4
  checksum, and removed before the article is transferred to the news
  system.
  In general, set it to "--\\n" or "-- \\n" for classic B-news or C-news.")

;;
;; End of customizations
;;

(defvar md4-signature-header "X-Md4-Signature")

;; Hook definitions and insertions.

(add-hook 'gnus-Inews-article-hook 'md4-add-signature)
(add-hook 'gnus-Article-prepare-hook 'md4-validate-signature)
;;
;; Calcuates the MD4 signature for the article to be posted, which
;; is assumed to be in the current buffer.
;;
(defun md4-add-signature ()
  "Adds a MD4-signature to the article being posted. Must be called
from gnus-Inews-article-hook."
  (interactive)

  (if (null md4-insertion)
      nil
    (let (start-of-body end-of-body sigfile)

      ;; .signature handling. may be system specific
      (goto-char (point-max))
      (setq end-of-body (point-marker))
      (if (not gnus-signature-file)	;skip if gnus-sig set
	  (if (file-exists-p
	       (setq sigfile (expand-file-name "~/.signature")))
	      (progn
		(insert md4-signature-separator)
		(insert-file sigfile))
	    (setq sigfile nil)))	;signal 'no file'

      (goto-char (point-min))
      (search-forward "\n\n")
      (setq start-of-body (point-marker))	; remember where
      
      ;; Run md4 and add the signature.
      (forward-line -1)
      (insert md4-signature-header ": ")
      (insert (md4-signature-region start-of-body (point-max)))
      (insert "\n")

      (if sigfile
	  (delete-region end-of-body (point-max)))
      )))

;;
;; Validate MD4 signature. A message is shown with the result.
;; If the signature does not match, buffer "*MD4 Buffer*" holds more
;; information.
;;
(defun md4-validate-signature ()
  "Checks a MD4-signature in the article being read. May be called
from gnus-article-prepare-hook."
  (interactive)

  (if (null md4-validation)
      nil
    (let (start-of-body)
      (goto-char (point-min))
      (search-forward "\n\n")
      (setq start-of-body (point-marker))	; remember where

      ;; Check if a signature header is present
      (goto-char (point-min))
      (if (search-forward 
	   (concat "\n" md4-signature-header ": ")
	   start-of-body t)
	  (let (signature (here (point)))
	    (forward-line 1)
	    (setq signature (buffer-substring here (1- (point))))

	    ;; Validate
	    (if (string= 
		 signature
		 (md4-signature-region start-of-body (point-max)))
		(progn
		  (if (not (equal md4-validation t))
		      (message "MD4 signature valid."))
		  (bury-buffer md4-buffer))
	      (beep)
	      (save-excursion
		(set-buffer md4-buffer)
		(goto-char (point-min))
		(insert (message "MD4 signature mismatch!")
			"\nPosted:     " signature
			"\nCalculated: ")
		(goto-char (point-min))))
	    )))))

(defun md4-signature-region (start end)
  "Calculates MD4 signature."

  ;; Get buffer and clear it
  (setq md4-buffer (get-buffer-create "*MD4 Buffer*"))
  (save-excursion
    (set-buffer md4-buffer)
    (erase-buffer))

  ;; Run md4
  (call-process-region start end
		       md4-command nil md4-buffer nil)

  ;; Verify normal result
  (save-excursion
    (set-buffer md4-buffer)
    (if (= (buffer-size) 33)
	(buffer-substring (point-min) (1- (point-max)))
      (error "Unexpected result from %s: %s" md4-command
	     (buffer-substring (point-min) (point-max))))))
