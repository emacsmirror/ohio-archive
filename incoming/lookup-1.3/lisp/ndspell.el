;;; ndspell.el --- Lookup spell checker
;; Copyright (C) 1999 Lookup Development Team <lookup@ring.gr.jp>

;; Author: Keisuke Nishida <kei@psn.net>
;; Version: $Id: ndspell.el,v 1.5 1999/07/27 14:50:45 tsuchiya Exp $

;; This file is part of Lookup.

;; Lookup is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; Lookup is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Lookup; if not, write to the Free Software Foundation,
;; Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Code:

(require 'lookup)

(defconst ndspell-version "1.0")

;;;
;:: Customizable variables
;;;

(defgroup ndspell nil
  "Lookup spell checker."
  :group 'lookup-agents)

(defcustom ndspell-ispell-program "ispell"
  "*Program name of Ispell."
  :type 'string
  :group 'ndspell)

(defcustom ndspell-grep-program "grep"
  "*Program name of grep."
  :type 'string
  :group 'ndspell)

(defcustom ndspell-words-dictionary "/usr/share/dict/words"
  "*Dictionary file."
  :type 'file
  :group 'ndspell)

(defcustom ndspell-dictionary-title "Spell Checker"
  "*Title of ndspell dictionary."
  :type 'string
  :group 'ndspell)

(defcustom ndspell-search-dictionaries t
  "*List of dictionary ID to be searched by dynamic reference.
`t' means all selected dictionaries are used."
  :type '(radio (const :tag "Selected dictionaries" t)
		(repeat :tag "Specific dictionaries" (string :tag "ID")))
  :group 'ndspell)

(defcustom ndspell-process-coding-system lookup-process-coding-system
  "*Coding system for Ispell process."
  :type 'symbol
  :group 'ndspell)


;;;
;:: types
;;;

(put 'ndspell ':methods '(exact prefix suffix substring regexp))

;;;
;:: Interface functions
;;;

(put 'ndspell 'setup 'ndspell-setup)
(defun ndspell-setup (agent)
  (list (lookup-new-dictionary agent nil ndspell-ispell-program
			       ndspell-dictionary-title)))

(put 'ndspell 'clear 'ndspell-clear)
(defun ndspell-clear (agent)
  (ndspell-process-kill))

(put 'ndspell 'search 'ndspell-dictionary-search)
(defun ndspell-dictionary-search (dictionary query)
  (when (or (not lookup-search-found) lookup-force-update)
    (let ((string (lookup-query-string query)))
      (when (or (not (fboundp 'find-charset-string))
		(equal (find-charset-string string) '(ascii))
		(equal (find-charset-string string) 'nil))
	(mapcar (lambda (candidate)
		  (let* ((heading (format "[%s -> %s]" string candidate))
			 (reference (lookup-make-reference dictionary
							   candidate heading)))
		    (lookup-reference-make-dynamic reference
						   'ndspell-dynamic-search)
		    reference))
		(if (eq (lookup-query-method query) 'exact)
		    (ndspell-check-spelling string)
		  (ndspell-search-spelling
		   (lookup-query-to-regexp query))))))))

(defun ndspell-dynamic-search (reference)
  (let ((self (lookup-entry-dictionary reference))
	(query (lookup-make-query 'exact (lookup-entry-code reference)))
	(lookup-proceeding-message "Rechecking") entries)
    (lookup-proceeding-message nil)
    ;; search entries
    (setq entries
	  (apply 'append
		 (mapcar (lambda (dic)
			   (when (and (not (eq dic self))
				      (if (eq ndspell-search-dictionaries t)
					  (lookup-dictionary-selected-p dic)
					(member (lookup-dictionary-id dic)
						ndspell-search-dictionaries)))
			     (lookup-proceeding-message
			      (format "by %s..."
				      (lookup-dictionary-title dic)))
			     (lookup-vse-search-query dic query)))
			 (lookup-module-dictionaries
			  (lookup-session-module lookup-current-session)))))
    ;; rebuild entry
    (setq entries
	  (mapcar (lambda (entry)
		    (let ((heading (lookup-entry-heading reference)))
		      (string-match "->" heading)
		      (setq heading (concat (substring heading 0 (match-end 0))
					    "] " (lookup-entry-heading entry)))
		      (lookup-make-entry (lookup-entry-dictionary entry)
					 (lookup-entry-code entry) heading)))
		  entries))
    (lookup-proceeding-message t)
    entries))


;;;
;:: Internal functions
;;;

(defun ndspell-check-spelling (string)
  (let ((check (ndspell-spell-check string)))
    (cond ((consp check) (nreverse check))
	  ((stringp check) (list check)))))

(defun ndspell-spell-check (string)
  (let ((output (ndspell-process-require string)))
    (cond
     ((string= output "") nil)			; empty
     ((eq (aref output 0) ?*) t)		; match
     ((eq (aref output 0) ?-) 'compound)	; compound
     ((eq (aref output 0) ?#) nil)		; no match
     ((string-match "^\\+ \\(.*\\)" output)	; root match
      (downcase (match-string 1 output)))
     ((string-match "^&[^:]*: " output)		; some candidates
      (lookup-split-string (substring output (match-end 0)) "[,\n] ?")))))

(defun ndspell-search-spelling (regexp)
  (with-temp-buffer
    (call-process ndspell-grep-program nil t nil
		  regexp ndspell-words-dictionary)
    (let ((candidates nil))
      (while (not (bobp))
	(setq candidates (cons (buffer-substring-no-properties
				(1- (point)) (progn (forward-line -1) (point)))
			       candidates)))
      candidates)))

;;;
;:: Ispell process
;;;

(defvar ndspell-process nil)

(defun ndspell-get-process ()
  (unless (and ndspell-process (eq (process-status ndspell-process) 'run))
    (if ndspell-process (lookup-process-kill ndspell-process))
    (let ((buffer (lookup-open-process-buffer " *ndspell*")))
      (setq ndspell-process
	    (start-process "ndspell" buffer ndspell-ispell-program
			   "-a" "-m" "-C"))
      (process-kill-without-query ndspell-process)
      (accept-process-output ndspell-process)
      (let ((coding ndspell-process-coding-system))
	(when coding
	  (set-process-coding-system ndspell-process coding coding)))))
  ndspell-process)

(defun ndspell-process-require (string)
  (lookup-process-require (ndspell-get-process) (concat string "\n") "^\n"))

(defun ndspell-process-kill ()
  (if ndspell-process (lookup-process-kill ndspell-process)))

(provide 'ndspell)

;;; ndspell.el ends here
