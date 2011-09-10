;;; mew-sort.el --- Sorting messages for Mew

;; Author:  Takashi P.KATOH <p-katoh@shiratori.riec.tohoku.ac.jp>
;; Created: Feb  6, 1996
;; Revised: Aug 30, 1999

;;; Code:

(defconst mew-sort-version "mew-sort.el version 0.15")

(require 'mew)

(defun mew-summary-sort-subr (msgs &optional method from)
  (if (not (mew-summary-exclusive-p))
      ()
    (mew-mark-clean)
    (let* ((folder (buffer-name))
	   (sort-key (or (cdr (assoc folder 
				     mew-sort-default-key-alist))
			 mew-sort-default-key))
	   (field-mode (mew-input-sort-key sort-key))
	   (field (car field-mode))
	   (mode (cdr field-mode))
	   rbeg)
      (if (not (listp msgs)) (setq msgs (list msgs)))
      (setq mew-summary-buffer-process t)
      (message "Sorting %s by %s (%s mode) ... " folder field mode)
      (apply (function call-process)
	     mew-prog-imsort
	     nil nil nil
	     (concat "--src=" folder)
	     (concat "--field=" field)
	     (concat "--mode=" mode)
	     (append mew-prog-im-arg msgs)) ;; xxx
      (message "Sorting %s by %s ... done" folder field)
      (setq mew-summary-buffer-process nil)
      (if from 
	  (progn (mew-summary-jump-message from)
		 (setq rbeg (point)))) ;; beginning of region
      (widen)
      (mew-elet
       (delete-region (or rbeg (point-min)) (point-max))) ;; for update
      (mew-summary-scan-body mew-prog-imls
			     'mew-summary-mode
			     folder
			     mew-cs-scan
			     (mew-update-range)))))

(defun mew-summary-sort (&optional arg)
  "Sort messages in the folder according to inputed key."
  (interactive "P")
  (mew-summary-only
   (if arg
       (mew-summary-sort-region (region-beginning) (region-end) "region")
     (mew-summary-sort-region (point-min) (point-max)))))

(defun mew-summary-sort-region (r1 r2 &optional method)
  "Sort messages in the region according to inputed key."
  (interactive "r")
  (mew-summary-only
   (let (from to)
     (save-excursion
       (goto-char (min r1 r2))
       (setq from
	     (or (mew-summary-message-number)
		 (progn
		   (re-search-backward mew-summary-message-regex nil t)
		   (mew-summary-message-number))))
       (goto-char (max r1 r2))
       (setq to
	     (or (mew-summary-message-number)
		 (progn
		   (re-search-backward mew-summary-message-regex nil t)
		   (mew-summary-message-number))))
       (goto-char (min r1 r2))
       (beginning-of-line)
       (mew-summary-sort-subr (concat from "-" to) method from)))))

(defun mew-summary-mark-sort (&optional r1 r2)
  (interactive)
  "Sort message marked with '*'."
  (mew-summary-only
   (mew-summary-sort-subr
    (mew-summary-mark-collect
     mew-mark-review (or r1 (point-min)) (or r2 (point-max)))
    "marked messages")))

(provide 'mew-sort)

;;; Copyright Notice:

;; Copyright (C) 1996, 1997, 1998, 1999 Mew developing team.
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

;;; mew-sort.el ends here
