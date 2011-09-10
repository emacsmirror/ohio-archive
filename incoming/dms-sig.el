;;; dms-sig.el --- Multiple automatically selected signatures
     
;; Copyright (C) 1997 Dewey M. Sasser
     
;; Author: Dewey M. Sasser <dewey@newvision.com>
;; Maintainer: <dms-signature-maintainer@newvision.com>
;; Created: Mon Mar 31 16:53:08 1997
;; Version: 2.2
;; Keywords: multiple mail signature, random quotes
;;
;; This file is not part of EMACS
;;
;;; Features
;;
;;    o Allows specification of arbitrary number of signatures, each
;;      named by a symbol and defined by the contents of either a file
;;      or a string
;;    o Allows specification of quote methods for each of these
;;      signatures.  Quotes can be developed either by random
;;      selection from a quote file, running an external program or
;;      running an emacs lisp function
;;    o Allows automatic selection of signature based on regular
;;      expression match on the "To:" mail field or the GNUS
;;      newsgroup.  This automatic selection can be bypassed with a
;;      prefix-argument 
;;    o Works in mail-mode and message-mode (and should work in any
;;      mode) 
;;    o Remembers the location of the last inserted signature and
;;      deletes it before inserting the new one.
;;
;;; Commentary
;;
;;    Briefly, this package is designed to be a drop in replacement
;;    for the default emacs signatures.  You should be able to just
;;    (require 'dms-sig) and go, and extend it as and when you want.
;;    If you can't just use with it, please send a bug report and tell
;;    me what happened (see the section on Bugs).
;;
;;    This package gives you, instead of one signature, an arbitrary
;;    number of signature each named by a symbol.  You also associate
;;    signature symbol names with signature files (or strings) and
;;    options.  Currently, options supported are only whether or not
;;    to include a random quote, and which method to use to include
;;    that quote.
;;    
;;    To extend it, check out the variables
;;    `dms-signature-to-name-map', where you can specify regexps that
;;    match against the "to" filed, and also
;;    `dms-signature-to-groupname-map', to match against the
;;    "newsgroup" field when posting news (or reading mail with
;;    GNUS).  (And see the "Sample Setup" section in this header.)
;;    
;;    To use automatic random quote insertion, set
;;    `dms-signature-use-quotes' to t (it uses a random quote from
;;    "~/.quotes" by default).  Also, check out the variable
;;    `dms-signature-quote-methods'.  Like signatures, quote methods
;;    are named (therefore arbitrary signatures can use arbitrary
;;    quote methods)
;;
;;    By the way, if you drop me a note
;;    (mailto:dms-signature-maintainer@newvision.com) and tell me
;;    you're using this package, not only will it make me feel good,
;;    but I'll send you mail whenever I release a new version (BBDB
;;    makes that easy). 
;;
;;; Incompatibilities
;;
;;    Unfortunately, the syntax of several variables used by this
;;    package has changed since my last release.  This current syntax
;;    should remain stable for quite some time, but the functions
;;    `define-dms-signature-quote', `define-dms-signature',
;;    `dms-signature-make-signature' and `dms-signature-make-quote'
;;    will be even more stable.  Use what you want.
;;
;;; New Features
;;
;;    o Calling programs or functions to get the appropriate quote
;;    o insert signature at point if prefix arg is negative
;;    o Remembers where the previous signature was inserted so that
;;      the old signature can be deleted even if it occurs in the
;;      middle of the buffer
;;    o Supplies default when prompting for signature if inserting
;;      signature at point
;;    
;;; Bugs
;;  
;;    Please use the function `dms-signature-bug' to submit bugs.  If
;;    the bug reports an error (as in emacs signaling an error),
;;    please send me a stack trace by setting the variable
;;    `debug-on-error' to t and sending me the contents (via
;;    dms-signature-bug, of course) of the *Backtrace* buffer.
;;
;;; Future directions
;;    o Add inheritance semantics to both signatures and quote methods
;;    o optionally use BBDB to find signature
;;
;;; Example Setup
;;
;;    (setq dms-signature-to-name-map '(("majordomo" . none)
;;    				  ("newvision\\.com" . nvs)))
;;    
;;    (setq dms-signature-to-groupname-map
;;          '(
;;    	("list-maintainance" . nvs-official)
;;    	("monolith-users" . nvs-official)
;;    	("nvs" . nvs)))
;;    
;;    (setq dms-signature-to-file-map
;;          '(
;;    	(personal :file ".signature.personal")
;;    	(personal-no-quote :file ".signature.personal" :dont-insert-quote)
;;    	(normal :file ".signature")
;;    	(normal-no-quote :file ".signature" :dont-insert-quote)	
;;    	(none :dont-insert-quote)
;;    	(nvs)
;;    	(wonko :file ".signature.wonko" :dont-insert-quote)
;;    	(nvs-official :file ".signature.nvs-official" :dont-insert-quote)
;;    	(bmw :file ".signature.bmw-digest" :dont-insert-quote)))
;;    
;;    (define-dms-signature 'informal :string "Dewey")
;;    (define-dms-signature-quote 'test-quote :program "cat"
;;                                            :program-args '(".cshrc"))
;;    
;;
;;; Copyright
;;  
;;    Copyright 1997 Dewey M. Sasser.  This file is distributed under
;;    the GNU General Public Liscense.  You should have obtained a
;;    copy of that liscense with emacs.
;;    
;;; Warranty and Liability
;;    
;;    The author of this package assumes no liability for anything
;;    arising from the use of this package and specificaly disclaims
;;    any warranty whatsoever.
;; 
;;; Code:

(require 'cl)

(defvar dms-signature-use-newsgroup-variable t
  "*Should we use the gnus-newsgroup-name variable?
This variable seems to be broken with some Gnus.  If this variable is
nil, we use the \"Newsgroups\" header instead.")

(defvar dms-signature-load-hook nil
  "*Hook called when file is loaded")

(defvar dms-signature-quote-default-directory "~"
  "*Director where the quotes files can be found")

(defvar dms-signature-quotes-file "~/.quotes"
  "*This variable is obsolete, but used for \"defaults\" purposes.

See definition of function `define-dms-signature-quote'
instead.

Name of file to find quotes.
This is in emacs cookie format (Entries separated by \"\\n%%\\n\"")

(defvar dms-signature-use-quotes nil
  "*Variable determines if quotes should be used.  This can be overridden for specific signatures with :dont-insert-quote or :insert-quote")

(defvar dms-signature-default 'normal
  "*Signature to use if nothing else is appropriate")

(defvar dms-signature-default-dir "~"
  "*Directory that contains signature files")

(defvar dms-signature-dir dms-signature-default-dir
  "This variable is obsolete.  See dms-signature-default-dir instead")

(defvar dms-signature-to-name-map '( ("majordomo" . none))
  "*List of regexp . symbol to determine which signature (named by the symbol)
to use given a match in the \"to\" header of the regexp"  )

(defvar dms-signature-to-groupname-map nil
  "*Alist of groupname-regexp . signature")

(defvar dms-signature-quote-separator "---"
  "*Separator to use to separate the quote from the rest of the message.  This should not be \"-- \"")

(defvar dms-signature-quote-methods
  (list (list 'file ':file dms-signature-quotes-file)
	'(fortune :program "fortune"))
  "AList of quote methods.
The first element of each list should be a symbol which names the
quote method.  Successive argument describe the method using keyword
value pairs.  Keywords can be:

:file <name-of-file>
:program <name-of-program> or
  program will be called with argument from list :program-args
:function <lisp-symbol-or-lambda>
  function will be called with the name of the signature and quote
method and any arguments specified with list :function-args


Some future version will probably add an :inherit option. 
")

(defvar dms-signature-default-quote-method 'file
  "*Name of method to use by default for quotes")

  
;;(setq dms-signature-to-groupname-map
;;      '(
;;	("list-maintainance" . nvs-official)
;;	("monolith-users" . nvs-official)
;;	("nvs" . nvs)))

(defvar dms-signature-to-file-map
  '(
    (none :dont-insert-quote)
    (normal :file ".signature")
    (normal-no-quote :file ".signature" :dont-insert-quote))
  "*List where each entry is
(symbol :file string-file-name [:insert-quote] [:dont-insert-quote]")

(defun* dms-signature-make-signature (signature &rest args
						   &key
						   dont-insert-quote
						   file
						   string
						   insert-quote
						   quote-method)
  "*Build a proper entry for dms-signature-to-file-map"
  (append (list signature) args))


(defun define-dms-signature (&rest args)
  "Define a new signature"
    (setq dms-signature-to-file-map
	(cons (apply 'dms-signature-make-signature args)
	      dms-signature-to-file-map)))

(defun* dms-signature-make-quote (quote-name &rest args
						&key
						file
						directory
						program
						program-args
						function
						function-args)
  "*Build a proper entry for dms-signature-quote-methods"
  (let ((count 0))
  (if (and function-args (not function))
      (error ":function-args specified without :function"))
  (if (and program-args (not program))
      (error ":program-args specified without :program"))
  (if (and directory (not file))
      (error ":directory specified without :file"))
  (if function (incf count))
  (if program (incf count))
  (if file (incf count))
  (if (> count 1)
      (error
       "More than one of file, program or function specified in quote method"))
  (append (list quote-name) args)))


(defun* define-dms-signature-quote (&rest args)
  "Define a new quote type and add it to the list"
  (setq dms-signature-quote-methods
	(cons (apply 'dms-signature-make-quote args)
	      dms-signature-quote-methods)))
  
						
    
(defvar dms-signature-bbdb-field 'dms-signature
  "*Name of the bbdb field to use to find signature")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                  End of user configuration variables               ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar dms-signature-location nil
  "Track where we inserted the signature")
(make-variable-buffer-local 'dms-signature-location)

(defvar dms-signature-maintainer "dms-signature-maintainer@newvision.com")

(defvar dms-signature-version "2.2"
  "Version of this package")

(defvar dms-signature-completion-cache nil
  "Cache of signature completion alist")

(defmacro dms-signature-in-buffer (buffer &rest body)
  "Execute code in BUFFER"
  (` (let ((buf (current-buffer)))
    (unwind-protect
	 (progn
	   (set-buffer (, buffer))
	   (,@ body))
       (set-buffer buf)))))

(defmacro dms-signature-with-flags (flags args &rest body)
  "Arrange so that variables in FLAGS are bound to something that is
true or nil depending on their presence (as a key word) in ARGS, then
execute BODY"
  (let ((bindings
	 (mapcar (function (lambda (var)
		     (let ((keyword (intern (concat ":" (symbol-name
							 var)))))
		       (` ( (, var) (member '(, keyword) (, args)))))))
		 flags)))
    (` (let (, bindings)
	 (,@ body)))))


(defmacro dms-signature-in-temp-buffer (&rest body)
  (` (let ((buf (current-buffer))
	(new-buf (generate-new-buffer " *temp*")))
    (set-buffer new-buf)
;    (set-window-buffer (selected-window) (current-buffer))
    (unwind-protect
	(progn (,@ body))
      (set-buffer buf)
;      (set-window-buffer (selected-window) (current-buffer))
      (kill-buffer new-buf)))))


(defun dms-signature-completion-alist ()
  "return the signature completion alist"
  (if (and dms-signature-completion-cache
	   (equal (car dms-signature-completion-cache)
		  dms-signature-to-file-map))
      (cdr dms-signature-completion-cache)
    (setq dms-signature-completion-cache
	  (cons dms-signature-to-file-map
		(loop for x in dms-signature-to-file-map
		      collect (cons (symbol-name (car x))
				    (cdr x)))))
    (cdr dms-signature-completion-cache)))

(defmacro dms-signature-binding-from-other-buffer (buffer variable &rest default)
  (` (if (and (boundp '(, buffer))
	    (bufferp (, buffer))
	    (buffer-name (, buffer))
	    )
       (dms-signature-in-buffer
	(, buffer) (, variable))
       (,@ default))))


(defun dms-signature-match-to-regexp-alist (string list)
  "Match string against the cars in list, returning the cdr if we get a
match"
  (loop for x in list
	if (funcall
	    (function (lambda (x)
		(if (string-match (car x) string)
		    (cdr x))))
	    x)
	return (cdr x)))


(defun dms-signature-bbdb-lookup (field)
  "Check the bbdb for signature to use (if BBDB is in use)"
  (if (or (fboundp 'bbdb)
	  ;; dms-signature-force-bbdb
	  )
      (let* ((first-entry (if (string-match "[^,]*" field)
			      (substring field (match-beginning 0)
					 (match-end 0))
			    field))
	     (canonical-name (if first-entry
				 (mail-extract-address-components first-entry)))
	     (bbdb-record (if (and canonical-name
				   (or (car canonical-name)
				       (cadr canonical-name)))
			      (bbdb-search-simple (car canonical-name)
						  (cdr canonical-name))))
	     (value (if bbdb-record
			(bbdb-record-getprop bbdb-record dms-signature-bbdb-field))))
	(if value
	    (intern value)))))
	  


(defun dms-signature-discover-mail-signature-entry (&optional sig)
  "return the mail signature file"
  (let (symbol value)
    (or
     ;; first, see if we've got an argument
     (setq symbol sig)

     ;; next, see if we get a signature from bbdb for the primary
     ;; target

     ;; then, see if we get a name match
     (setq symbol
	   (let* ((to-field (mail-fetch-field "to")))
	     (if to-field
		 (or (dms-signature-bbdb-lookup to-field)
		     (dms-signature-match-to-regexp-alist
		      to-field
		      dms-signature-to-name-map)))))
     ;; next, see if we get a signature for the group name
     (setq symbol
	   (if dms-signature-use-newsgroup-variable
	       (if (and (boundp 'gnus-newsgroup-name)
			(stringp gnus-newsgroup-name)
			)
		   (dms-signature-match-to-regexp-alist gnus-newsgroup-name
							dms-signature-to-groupname-map))
	     ;; obvious cut-and-paste here
	   (let* ((to-field (mail-fetch-field "newsgroups")))
	     (if to-field
		 (dms-signature-match-to-regexp-alist
		  to-field
		  dms-signature-to-groupname-map)))))
     ;; last, see if we get a signature set by the group
     (setq symbol
	   (dms-signature-binding-from-other-buffer gnus-summary-buffer
						    dms-signature-default))
     ;; very last, set it to the default
     (setq symbol dms-signature-default))
    (setq value (assoc symbol dms-signature-to-file-map))
    (if (not value)
	(progn
	  (message "Warning:  No entry for signature %s" symbol)
	  (beep)
	  (sleep-for 1)
	  (assoc dms-signature-default dms-signature-to-file-map))
      value)))
;    (setq value (assoc symbol dms-signature-to-file-map))
;    (cdr value)))


(defun dms-signature-get-quote-method-entry (quote-name)
  "Get and return the quote method structure"
  (assoc (or quote-name dms-signature-default-quote-method)
	 dms-signature-quote-methods))

(defun* dms-signature-insert-random-quote (method-name &key
						       signature
						       file
						       directory
						       program
						       program-args
						       function
						       function-args)
  "insert a random quote at the current point"
  (setq directory (or directory
		      dms-signature-quote-default-directory))
  (let ((full-name (and file
			(if (not (file-name-absolute-p file))
		       (concat directory "/" file)
		     file))))
  (cond
   ((and file (file-exists-p full-name))
    (require 'cookie1)
    (insert
     (cookie full-name
	     "Fetching Quote"
	     "Fetching Quote....Done"))
    (delete-char -2))
   (program
    (let ((what
    (dms-signature-in-temp-buffer
     (apply 'call-process program nil t nil program-args)
     (buffer-substring (point-min) (point-max)))))
      (insert what)))
   (function
    (let ((results (apply function signature method-name function-args)))
      (if results
	  (insert results)))))))



(defun dms-mail-signature (&optional arg)
  "Insert the mail signature into the current buffer.
Uses options taken from `dms-signature-to-name-map' and
`dms-signature-to-groupname-map'.
Optional prefix argument will prompt for which signature to use.
Negative prefix argument will insert signature at point instead of at
end. 

Subsequent invocations replace the existing signature.
"
  (interactive "P")
  (let* ((insert-at-point (and arg (eq arg '-)))
	(use-default insert-at-point)
	quote
	(prompt-for-sig  arg)
	(default-entry (dms-signature-discover-mail-signature-entry nil))
	(symbol nil))
    (if prompt-for-sig
	(setq symbol (intern
		      (completing-read "Which signature? "
				       (dms-signature-completion-alist)
				       nil
				       't
				       (if use-default
					   (symbol-name (car default-entry)))
				       ))))
    (save-excursion
      (if dms-signature-location
	  (progn
	    (goto-char (car dms-signature-location))
	    (if (looking-at "^-- ")
		  (kill-region (1- (car dms-signature-location))
			       (cdr dms-signature-location))))))
    (save-excursion
      (let* ((entry (dms-signature-discover-mail-signature-entry symbol)))
	(unless insert-at-point
	  (goto-char (point-max)))
	(insert "\n")
	(save-restriction
	  (narrow-to-region (point) (point))
	  (setq dms-signature-location (cons (point-marker)
					     (make-marker)))
	  (unwind-protect
	      (apply 'dms-signature-insert-internal entry)
	    (set-marker (cdr dms-signature-location) (point-max)))
	  )))))

(defun* dms-signature-insert-internal (signature &rest args
						 &key file
						 string
						 quote-method &allow-other-keys )
  "Do the actual signature insertion"
  (dms-signature-with-flags
   (dont-insert-quote insert-quote) args
   (let* ((directory dms-signature-default-dir)
	  (use-quote (and (or quote-method
			      (and dms-signature-default-quote-method
				   (or insert-quote dms-signature-use-quotes)))
			  (not dont-insert-quote)))
	  (quote-entry (dms-signature-get-quote-method-entry quote-method)))
     (if (or file use-quote)
	 (insert "-- \n"))
     (goto-char (point-max))
     (cond
      (file
       (insert-file (expand-file-name (concat directory "/" file))))
      (string
       (insert string "\n")))
     (if use-quote
	 (progn
	   (goto-char (point-max))
	   (if dms-signature-quote-separator
	       (insert dms-signature-quote-separator "\n"))
	   (apply 'dms-signature-insert-random-quote
		  (and quote-entry (car quote-entry))
		  :signature signature
		  (and quote-entry (cdr quote-entry))))))))

(defun dms-signature-bug ()
  "Submit a bug about dms-signature"
  (interactive)
  (if (yes-or-no-p "Submit a bug on dms-signature? ")
      (progn
	(require 'reporter)
	(reporter-submit-bug-report dms-signature-maintainer
				    (concat "dms-signature"
					    " version "
					    dms-signature-version)
				    '(
				      dms-signature-load-hook
				      dms-signature-quote-default-directory
				      dms-signature-quotes-file
				      dms-signature-use-quotes
				      dms-signature-default
				      dms-signature-default-dir
				      dms-signature-dir
				      dms-signature-to-name-map
				      dms-signature-to-groupname-map
				      dms-signature-quote-separator
				      dms-signature-quote-methods
				      dms-signature-default-quote-method
				      dms-signature-to-file-map
				      dms-signature-location
				      dms-signature-version
				      dms-signature-completion-cache)))))

(defalias 'dms-signature-submit-bug 'dms-signature-bug)

(defun dms-signature-insinuate ()
  (fset 'message-insert-signature 'dms-mail-signature)
  (fset 'mail-signature 'dms-mail-signature)
  (setq message-signature "")
  (setq dms-signature-already-insinuated t))

;(dms-signature-insinuate)
;(add-hook 'mail-mode-hook 'dms-signature-insinuate)
;(add-hook 'message-mode-hook 'dms-signature-insinuate)

(run-hooks 'dms-signature-load-hook)

(provide 'dms-sig)
;;; dms-sig.el ends here
