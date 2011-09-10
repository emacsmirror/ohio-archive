;;; mew-cache.el --- Cache management for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Mar 23, 1997
;; Revised: Aug 31, 1999

;;; Code:

(defconst mew-cache-version "mew-cache.el version 0.08")

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Prepare new message --- caching
;;

(defmacro mew-cache-decode-syntax (buf)
  (` (save-excursion
       (set-buffer (, buf))
       mew-decode-syntax)))

(defmacro mew-cache-decode-error (buf)
  (` (save-excursion
       (set-buffer (, buf))
       mew-decode-error)))

(defmacro mew-cache-multi-form (buf)
  (` (save-excursion
       (set-buffer (, buf))
       mew-syntax-multi-form)))

(defmacro mew-cache-icon-spec (buf)
  (` (save-excursion
       (set-buffer (, buf))
       mew-syntax-icon-spec)))

(defmacro mew-cache-privacy-result (buf)
  (` (save-excursion
       (set-buffer (, buf))
       mew-syntax-privacy-result)))

(defvar mew-cache nil
  "A list of decoded messages cache. 
The (new ... old) order of ((\"+folder\" . \"message\") . cache-buffer)")

(defmacro mew-cache-buffer-get (entry)
  (` (cdr (, entry))))

(defmacro mew-cache-entry-make (fld-msg buf)
  (` (cons (, fld-msg) (, buf))))

(defmacro mew-cache-hit (fld-msg)
  "Return value associated with key."
  (` (mew-cache-buffer-get (assoc (, fld-msg) mew-cache))))

(defun mew-cache-sort (entry)
  (setq mew-cache (cons entry (delete entry mew-cache))))

(defun mew-cache-add (fld-msg)
  "Adding (fld-msg . buf) to the top of 'mew-cache'.
Returning its cache buffer."
  (let ((len (length mew-cache))
	buf)
    (if (< len mew-cache-size)
	(setq buf (get-buffer-create (format "%s%d" mew-buffer-cache len)))
      (setq buf (mew-cache-buffer-get (nth (1- len) mew-cache)))
      (setcdr (nthcdr (- len 2) mew-cache) nil))
    (setq mew-cache (cons (mew-cache-entry-make fld-msg buf) mew-cache))
    buf))

(defun mew-cache-delete ()
  "Delete the most recent cache entry."
  (let ((buf (mew-cache-buffer-get (car mew-cache))))
    ;; must preserve the buffer itself because the buffer creation
    ;; depends on the length of mew-cache.
    (setq mew-cache (nconc (cdr mew-cache)
			   (list (mew-cache-entry-make nil buf))))))


(defmacro mew-cache-attribute-get (file)
  (` (list (mew-file-get-time (, file)) (mew-file-get-size (, file)))))
       
(defun mew-cache-message (fld msg &optional force)
  (let* ((fld-msg (cons fld msg))
	 (hit (mew-cache-hit fld-msg))
	 (file (mew-expand-folder fld msg))
	 (decode nil))
    (if hit
	(save-excursion
	  (mew-cache-sort (mew-cache-entry-make fld-msg hit))
	  (set-buffer hit)
	  (if (or (and (mew-folder-localp fld)
		       (not (equal mew-cache-attribute ;; buffer-local 
				   (mew-cache-attribute-get file))))
		  (and force mew-decode-not-decrypted)
		  (and force mew-decode-error))
	      ;; cache is invalid
	      (setq decode t)))
      (setq hit (mew-cache-add fld-msg))
      (setq decode t))
    (if decode
	(condition-case nil
	    (save-excursion
	      (set-buffer hit)
	      ;; in cache buffer
	      (setq mew-cache-folder fld)
	      (setq mew-cache-message-number msg)
	      (if (mew-folder-localp fld)
		  (setq mew-cache-attribute (mew-cache-attribute-get file))
		(setq mew-cache-attribute nil))
	      (if force
		  (let ((mew-header-max-length nil)
			(mew-header-max-depth nil))
		    (mew-decode fld msg))
		(mew-decode fld msg)))
	  (quit
	   (mew-cache-delete)
	   (message "MIME decoding for %s/%s is quitted." fld msg)
	   nil))) ;; will not be used
    hit)) ;; retrun value

(defun mew-cache-clean-up ()
  "A function to flush all decoded messages in cache list."
  (interactive)
  (mew-decode-syntax-delete)
  (let ((n 0))
    (while (< n mew-cache-size)
      (mew-kill-buffer (format "%s%d" mew-buffer-cache n))
      (setq n (1+ n))))
  (mew-current-set 'cache nil)
  (mew-current-set 'message nil)
  (mew-current-set 'part nil)
  (setq mew-cache nil))

(fset 'mew-cache-flush (symbol-function 'mew-cache-clean-up))

(provide 'mew-cache)

;;; Copyright Notice:

;; Copyright (C) 1997, 1998, 1999 Mew developing team.
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. Neither the name of the team nor the names of its contributors
;;    may be used to endorse or promote products derived from this software
;;    without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE TEAM AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE TEAM OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; mew-cache.el ends here
