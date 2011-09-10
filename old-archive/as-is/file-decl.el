;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; file-declarations.el --- Processing of file property declarations in headers
;; Author          : Lynn Slater
;; Created On      : Tue Feb  9 15:37:47 1988
;; Last Modified By: Lynn Slater
;; Last Modified On: Fri Sep  9 09:50:31 1988
;; Update Count    : 15
;; Status          : Unknown, Use with caution!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copyright (C) 1988 Lynn Randolph Slater, Jr.
;; This file might become part of GNU Emacs.
;;
;; This file is distributed in the hope that it will be useful,
;; but without any warranty.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.
;;
;; Everyone is granted permission to copy, modify and redistribute
;; this file, but only under the conditions described in the
;; document "GNU Emacs copying permission notice".   An exact copy
;; of the document is supposed to have been given to you along with
;; this file so that you can know how you may redistribute it all.
;; It should be in a file named COPYING.  Among other things, the
;; copyright notice and this notice must be preserved on all copies.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make this a file called file-declarations.el in your path
;; You should byte-compile it and load it in your .emacs or site-init.el

;; Many emacs versions seem to support the declaration of various
;; properties the files. These seem to be in the first non-blank line and
;; surrounded by -*- strings.  Gnu-emacs handles the MODE: declaration, but
;; all others are ignored.  In this file are routines to support processing
;; of these declarations in a standard manner.
;;
;; The basic idea is that whenever code is available that can use such a
;; declaration, it can add itself to a structure which will insure that it
;; gets called with the proper values whenever a file of the proper mode is
;; read in. (There is a pseudo-mode called 'default that holds the
;; routines to call for any mode not specifying any other action.)
;;
;; The creation of these declaration lines is not addressed in this file.
;; At the end of the file is a sample usage.
;;
;; This is being distributed to bug-gnu-emacs instead of info-gnu-emacs
;; because only emacs-hackers will install uses of these declarations.
;;
;; This file is safe and upward compatable.

(provide 'file-declarations)

(defun extract-file-declarations ()
  "Looks for file declarations in the first non blank line.
   Returns a list of (keyword . value) pairs.

   Declarations must be in the first non-blank line ande be bracketed by
   -*- strings.  Each declaration is of the form <keyword>: <value>;
   <keyword> is interpreted as always upper case, <value> is case sensitive.
   The ; is optional."
  (save-excursion
    ;; only look on the first non-blank line
    (goto-char (point-min))
    (skip-chars-forward " \t\n")
    ;; check for the -*- bracketing strings. Establish search bounds
    (if (re-search-forward "-\\*-.*-\\*-"
			   (save-excursion (end-of-line) (point))
			   t)
	(let ((stop (- (match-end 0) 3))
	      (exp "[ \t]*\\([a-zA-Z\-_]+\\):[ \t]*\\([a-zA-Z0-9\-_]+\\)")
	      md keyword value
	      (declarations nil))
	  (goto-char (match-beginning 0))
	  ;; Grab all pairs of Keyword: value
	  (while (re-search-forward exp stop t)
	    ;; below is some interesting uses of match data.
	    ;; I sure wish that emacs supported  symbolic identification of
	    ;; match data pairs. (HINT)
	    (setq md (match-data))
	    (setq keyword (upcase
			    (buffer-substring
			      (marker-position (car (cdr (cdr md))))
			      (marker-position (car (cdr (cdr (cdr md))))))))
	    (setq value (buffer-substring
			  (marker-position (car (cdr (cdr (cdr (cdr md))))))
			  (marker-position (car (cdr (cdr (cdr (cdr (cdr md)))))))))					
	    (setq declarations (cons (cons keyword value) declarations))
	    )
	  (nreverse declarations))
      ;; no match was found. Return nil
      nil
      )))

(defvar file-declaration-action-list nil
  "A alist of (mode-id . (alist-of-keyword . actions))). Used by
   apply-file-declarations.")

(defun register-file-declaration-action (mode-id keystring function-to-call)
  "Accepts MODE-ID, KEYSTRING, and FUNCTION-TO-CALL. Records the
   FUNCTION-TO-CALL as the appropiate action to take if the KEYSTRING is
   found in the declarations line in a file of the proper mode.

   Nil MODE-ID means to take the action in any mode that does not specify
   some other action.

   Calling this fcn twice with the same MODE-ID and KEYSTRING overwrites
   the previous FUNCTION-TO-CALL"
  (let ((key (upcase keystring))
	(mode (or mode-id 'default))
	ml kl)
    (if (setq ml (assoc mode file-declaration-action-list))
	(if (setq kl (assoc key ml))
	    (setcdr kl function-to-call) ;; overwrite old defn
	  (setcdr ml (cons (cons key function-to-call) (cdr ml))))
      ;; This is a new mode for us. Add to the master alist
      (setq file-declaration-action-list
	    (cons (list mode (cons key function-to-call))
		  file-declaration-action-list))))
  file-declaration-action-list)

(defun declared-actions-for-keystring-mode (keystring mode-id)
  "Returns the declared action to take if KEYSTRING is found in a
   buffer of mode MODE-ID.  Nil MODE-ID means return the action to take
   if no mode specific action is declared."
  (let* ((mode (or mode-id 'default))
	 (kl (or (assoc mode file-declaration-action-list)
		 ;; Look in 'default if an action is not already found
		 (assoc 'default file-declaration-action-list)))
	 (pair (if kl (assoc keystring kl))))
    (if pair (cdr pair))))

(defun apply-file-declarations ()
  "Scans a list of file declarations and calls the appropiate mode specific
   or default functions."
  (let ((file-declarations (extract-file-declarations))
	action)
    (while file-declarations
      (setq action (declared-actions-for-keystring-mode
		     (car (car file-declarations))
		     major-mode))
      (if action (funcall action (cdr (car file-declarations))))
      (setq file-declarations (cdr file-declarations))
      )))

;; Now, enable the whole package by adding it to find-file-hooks.
(if (not (memq 'apply-file-declarations find-file-hooks))
      (setq find-file-hooks (cons 'apply-file-declarations find-file-hooks)))

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; How to use this package
;;
;;   Suppose I was the author of c-style and wanted different files to be
;;   able to have different c-styles.  I would include the following in the
;;   c-style code.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'file-declarations)

(defun set-c-style-from-string (value)
  (if (not (fboundp 'set-c-style))
      (load "c-style" t))
  (if (not (fboundp 'set-c-style))
      (message "C-Style Declaration ignored, style not supported")
    (set-c-style (intern value))))

(if (fboundp 'register-file-declaration-action);; In case of failed load
    ;; install the declarations parsing
    (register-file-declaration-action 'C "C-Style"
				      'set-c-style-from-string))


