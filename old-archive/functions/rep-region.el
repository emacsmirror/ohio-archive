;;!emacs
;;
;; LCD Archive Entry:
;; rep-region|Bob Weiner|rsw@cs.brown.edu|
;; Replace things in region only.|
;; 92-04-02||~/functions/rep-region.el.Z|
;;
;; SUMMARY:      Replace things in region only.
;; USAGE:        GNU Emacs Lisp Library
;;
;; AUTHOR:       Bob Weiner, Applied Research, Motorola, Inc.
;; E-MAIL:       weiner@mot.com
;;
;; ORIG-DATE:    04/14/88
;; LAST-MOD:      2-Apr-92 at 17:18:40 by Bob Weiner
;;
;; Copyright (C) 1988-92 Bob Weiner and Free Software Foundation, Inc.
;; Available for use and distribution under the same terms as GNU Emacs.
;;
;; This file is not yet part of GNU Emacs.
;;
;; DESCRIPTION:  
;;
;;   Simplifies life by replacing regular expressions or strings in current
;;   region only, whether point is at start or end of region.
;;
;;   I personally use the following key binding:
;;
;;     (global-set-key "\M-r" 'replace-region)
;;
;; DESCRIP-END.
;;

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

(defun replace-region (str-replace)
  "Replaces a regular expression or string, given prefix arg STR-REPLACE, in a region."
  (interactive "P")
  (let (current-prefix-arg)
    (call-interactively
      (if str-replace
	  'replace-string-region
	'replace-regexp-region))))

(defun replace-regexp-region (regexp to-string &optional delimited no-msg)
  "Replace things in region, even before point, matching REGEXP with
TO-STRING.  Preserve case in each match if case-replace and case-fold-search
are non-nil and REGEXP has no uppercase letters.  Optional third arg DELIMITED,
prefix arg if interactive, non-nil means replace only matches surrounded by
word boundaries.  In TO-STRING, \\& means insert what matched REGEXP, and
\\=\\<n> means insert what matched <n>th \\(...\\) in REGEXP.  Optional fourth
arg NO-MSG non-nil means skip printing of 'Done' message."
  (interactive "sReplace regexp in region: \nsReplace regexp %s with: \nP")
  (let ((not-inter (not (interactive-p))))
    (if (null (mark))
	(error "Mark is not set, no region to replace within. ")
      (save-excursion
	(save-restriction
	  (narrow-to-region (point) (mark))
	  (goto-char (point-min))
	  (and delimited (setq regexp (concat "\\b" regexp "\\b")))
	  (while (re-search-forward regexp nil t)
	    (replace-match to-string not-inter nil))))
      (or no-msg not-inter (message "Done")))))

(defun replace-string-region (from-string to-string &optional delimited no-msg)
  "Replace occurrences in region, even before point, of FROM-STRING with
TO-STRING.  Preserve case in each match if case-replace and case-fold-search
are non-nil and FROM-STRING has no uppercase letters.  Optional third arg
DELIMITED, prefix arg if interactive, non-nil means replace only matches
surrounded by word boundaries.  Optional fourth arg NO-MSG non-nil means skip
printing of 'Done' message."
  (interactive "sReplace string in region: \nsReplace string %s with: \nP")
  (let ((not-inter (not (interactive-p))))
    (if (null (mark))
	(error "Mark is not set, no region to replace within. ")
      (save-excursion
	(save-restriction
	  (narrow-to-region (point) (mark))
	  (goto-char (point-min))
	  (let ((search-function 'search-forward))
	    (and delimited
		 (setq from-string
		       (concat "\\b" (regexp-quote from-string) "\\b")
		       search-function 're-search-forward))
	    (while (funcall search-function from-string nil t)
	      (replace-match to-string not-inter t)))))
      (or no-msg not-inter (message "Done")))))

(provide 'rep-region)
--
Send me detailed ideas on what you want in a Personalized Information
Environment.  You have to know what you want or need to get it.  -- Bob
