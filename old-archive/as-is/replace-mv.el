;Return-Path: <kgk@cs.brown.edu>
;Date: Wed, 19 Apr 89 10:51:36 EDT
;From: kgk@cs.brown.edu
;To: bug-gnu-emacs@prep.ai.mit.edu
;Subject: replace-string
;Reply-To: kgk@cs.brown.edu (Keiji Kanazawa)
;
;
;In GNU Emacs 18.53.1 of Sun Apr 16 1989 on fred (berkeley-unix)
;
;I like replace-string and replace-regexp to stay at point after
;performing any replacements, like in Zmacs.  The following patch
;is one possible way to implement it.  The default would remain as
;with the current behavior.

(defvar replace-move-point t
  "If non-nil, if there are any replacements made in a non-query
replacement, then move to the end of the last replacement made.
Otherwise, stay at point.")

(defun perform-replace (from-string to-string
		        query-flag regexp-flag delimited-flag)
  (if (or replace-move-point query-flag)
      (perform-replace-1 from-string to-string query-flag
                         regexp-flag delimited-flag)
      (save-excursion
        (perform-replace-1 from-string to-string query-flag
                           regexp-flag delimited-flag))))

;; The following function would be the present perform-replace.

;(defun perform-replace-1 (...)
;  )
