From apple!bloom-beacon!tut.cis.ohio-state.edu!ukma!uflorida!novavax!weiner@bbn.com Fri May 19 14:36:27 1989
To: unix-emacs@bbn.com
Date: 28 Apr 89 06:17:50 GMT
From: Bob Weiner <apple!bloom-beacon!tut.cis.ohio-state.edu!ukma!uflorida!novavax!weiner@bbn.com>
Sender: arpa-unix-emacs-request@bbn.com
Subject: rep-region.el, replace things in region only
Organization: Nova University, Fort Lauderdale, FL
Source-Info:  From (or Sender) name not authenticated.

;;!emacs
;;
;; SUMMARY:      Replace things in region only.
;; USAGE:        GNU Emacs Lisp Library
;;
;; AUTHOR:       Bob Weiner, Applied Research, Motorola, Inc.
;; E-MAIL:       USENET:  weiner@novavax.UUCP
;; ORIG-DATE:    04/14/88
;; LAST-MOD:     04/26/89
;;
;; Copyright (C) 1989 Bob Weiner and Free Software Foundation, Inc.
;; Available for use and distribution under the same terms as GNU Emacs.
;;
;; This file is not yet part of GNU Emacs.
;;
;; DESCRIPTION:  
;;
;;   Simplifies life by replacing regular expressions or strings in current
;;   region only.
;;
;; DESCRIP-END.
;;

(defun replace-regexp-region (regexp to-string &optional delimited no-msg)
  "Replace things in region, even before point, matching REGEXP with
TO-STRING.  Preserve case in each match if case-replace and case-fold-search
are non-nil and REGEXP has no uppercase letters.  Optional third arg DELIMITED
(prefix arg if interactive) non-nil means replace only matches surrounded by
word boundaries.  In TO-STRING, \\& means insert what matched REGEXP, and
\\=\\<n> means insert what matched <n>th \\(...\\) in REGEXP.  Optional fourth
arg NO-MSG non-nil means skip printing of 'Done' message."
  (interactive "sReplace regexp in region: \nsReplace regexp %s with: \nP")
  (save-excursion
    (save-restriction
      (narrow-to-region (point) (mark))
      (goto-char (point-min))
      (perform-replace regexp to-string nil t delimited)))
  (or no-msg (message "Done")))

(defun replace-string-region (from-string to-string &optional delimited)
  "Replace occurrences in region, even before point, of FROM-STRING with
TO-STRING.  Preserve case in each match if case-replace and case-fold-search
are non-nil and FROM-STRING has no uppercase letters.  Optional third arg
DELIMITED (prefix arg if interactive) non-nil means replace only matches
surrounded by word boundaries.  Optional fourth arg NO-MSG non-nil means skip
printing of 'Done' message."
  (interactive "sReplace string in region: \nsReplace string %s with: \nP")
  (save-excursion
    (save-restriction
      (narrow-to-region (point) (mark))
      (goto-char (point-min))
      (perform-replace from-string to-string nil nil delimited)))
  (or no-msg (message "Done")))


-- 
Bob Weiner, Motorola, Inc.,   USENET:  ...!gatech!uflorida!novavax!weiner
(407) 738-2087


