;;; cycle-mini.el --- Cycle through completions with arrow keys
;; Copyright (C) 1994, 1995 Joseph W. Reiss

;; Author:   Joe Reiss <jreiss@vt.edu>
;; Created:  26 Aug 1994
;; Version:  1.03
;; Keywords: minibuffer, completion
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or 
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; LCD Archive Entry:
;; cycle-mini|Joe Reiss|jreiss@vt.edu|
;; Make arrow keys cycle through completions in minibuffer.|
;; 07-Apr-1995|1.03|~/misc/cycle-mini.el.Z|

;;; Commentary:

;; This is an extension to the completing-read commands in the
;; minibuffer.  It allows you to cycle through the current list of
;; completions.  This works when changing buffers, with any command
;; which reads a function or variable name, or with a programmer 
;; specified completion list.  It even works with functions which
;; read a file name!  In addition, if you have part of a name already
;; typed in, cycle-mini will use that string to narrow down the matches
;; and will only cycle through the completions which contain that 
;; initial substring.

;;; Default bindings:

;; ^P,[up]	Display previous matching completion.
;; ^N,[down]	Display next matching completion.
;; TAB          Accept currently displayed completion and move cursor
;;              to end of line.  If no completion is displayed, call
;;              minibuffer-complete as usual.
;;
;; Typing any other key will edit the currently displayed completion
;; as normal.

;;; Installation:

;; Byte-compile, then put this file in one of your elisp load
;; directories and add the following line to your .emacs file
;;
;; (load "cycle-mini")

;;; History:

;; This code was inspired by elec-mini.el, found on the elisp-archive
;; but without any credit given to the original author.  I think any
;; code originally borrowed from that file has since been rewritten
;; beyond all recognition.  Some portions of this code are also
;; borrowed from Ken Manheimer's icomplete.el and Dave Gillespie's
;; complete.el in the GNU Emacs distribution.
;;
;; v1.03
;;  - removed the clearing of the displayed completion upon any
;;    editing command.  I decided it really didn't make that much
;;    sense, and the user could just use ^K to get rid of an unwanted
;;    match.  Removed now obsolete cycle-mini-allow-edit and
;;    cycle-mini-exit-commands
;;  - included support for completion-ignored-extensions, borrowed
;;    from complete.el
;;  - made cycle-mini play nice with complete.el
;; v1.02
;;  - added variables cycle-mini-sort-buffers, cycle-mini-wrap,
;;    cycle-mini-allow-edit, and cycle-mini-disable-bindings,
;;    which toggle several new options.  See variable docstrings
;;    for details
;;  - renamed several functions to make them more manageable
;; v1.01
;;  - fixed bug with file completions under Xemacs
;;  - added bindings for ^N and ^P
;;  - removed old code from elec-mini
;; v1.00
;;  - first public release

;;; Known bugs: (?)

;; Not really a bug, but an ugly kludge.  In order to make file
;; completions work, cycle-mini has to make some assumptions about
;; minibuffer-completion-predicate, namely that if it's a string and
;; minibuffer-completion-table is a symbol, then we're performing file
;; name completion.  Currently, of all the elisp code and all the
;; source distributed with GNU Emacs, at least for versions 19.25 and
;; above, and with Xemacs, only file completions use the predicate
;; this strange way.  But if that use should change, or if someone
;; else should use a string for a predicate in some other way, then...
;;
;; Also note that this has been tested with FSF Emacs 19.25 and
;; above and Xemacs 19.11 and above.  I don't think it should be too
;; difficult to make cycle-mini work with other variations, but I just
;; don't have access to them.  Anyone who wants to try is welcome to,
;; but please send me your modifications.

;; User modifiable variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar cycle-mini-sort-buffers t
  "* Sort buffer names lexiographically during completion cycling if non-nil.
Otherwise, leave buffers sorted in natural order.")

(defvar cycle-mini-wrap t
  "* Wrap around when we reach either end of the completion list, if non-nil.
Otherwise, stop and ring the bell.")

(defvar cycle-mini-disable-bindings nil
  "* Don't set up any bindings for cycle-mini functions, if non-nil.
Otherwise, create some nice initial bindings.

MUST be set before cycle-mini loads.")
  

;; Internal variables.  Modified during execution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar cycle-mini-eoinput 1
  "Indicates where actual user input ends.")
(defvar cycle-mini-last-completion nil
  "Indicates where we are in the list of possible completions.")
(defvar cycle-mini-completion-list nil
  "List of possible completions to cycle through.")
(defvar cycle-mini-completion-type 'other
  "The type of completion we are doing.")
(defvar cycle-mini-completion-function
  (lookup-key minibuffer-local-completion-map "\t" t)
  "Function to call to perform tab completion.")

(defvar cycle-mini-ignored-extensions 'empty-cache)
(defvar cycle-mini-ignored-regexp nil)

(defconst cycle-mini-local-vars
  '(pre-command-hook post-command-hook cycle-mini-eoinput
    cycle-mini-last-completion cycle-mini-completion-list
    cycle-mini-completion-type cycle-mini-completion-function
    cycle-mini-ignored-extensions cycle-mini-ignored-regexp))

;; Code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if cycle-mini-disable-bindings
    ()
  (define-key minibuffer-local-completion-map [down]
    'cycle-mini-next-completion)
  (define-key minibuffer-local-completion-map [up]
    'cycle-mini-previous-completion)
  (define-key minibuffer-local-completion-map "\C-n"
    'cycle-mini-next-completion)
  (define-key minibuffer-local-completion-map "\C-p"
    'cycle-mini-previous-completion)
  (define-key minibuffer-local-completion-map "\t"
    'cycle-mini-accept-completion)

  (define-key minibuffer-local-must-match-map [down]
    'cycle-mini-next-completion)
  (define-key minibuffer-local-must-match-map [up]
    'cycle-mini-previous-completion)
  (define-key minibuffer-local-must-match-map "\C-n"
    'cycle-mini-next-completion)
  (define-key minibuffer-local-must-match-map "\C-p"
    'cycle-mini-previous-completion)
  (define-key minibuffer-local-must-match-map "\t"
    'cycle-mini-accept-completion)
  )

(defun cycle-mini-no-completion ()
  "Reset cycle-mini to remember that no completion is currently displayed."
  (setq cycle-mini-eoinput (point-max)
	cycle-mini-last-completion nil
	cycle-mini-completion-list nil))

(defun cycle-mini-accept-completion ()
  "Treat completed string as if it were part of the user input.
If there is no completed string, call minibuffer-complete."
  (interactive)
  (if (null cycle-mini-last-completion)	; Don't have completion displayed
      (call-interactively cycle-mini-completion-function)
    (goto-char (point-max))
    (cycle-mini-no-completion)))

(defun cycle-mini-cull-completions ()
  "Generate list of possible completions."
  (setq cycle-mini-completion-list
	(all-completions (buffer-string)
			 minibuffer-completion-table
			 minibuffer-completion-predicate))

  ;; Handle completion-ignored-extensions
  (and (eq cycle-mini-completion-type 'file)
       (let ((p2 cycle-mini-completion-list) (p nil))
	 (while p2
	   (or (string-match cycle-mini-ignored-regexp (car p2))
	       (setq p (cons (car p2) p)))
	   (setq p2 (cdr p2)))
	 ;; If there are "good" names, use them
	 (and p (setq cycle-mini-completion-list p))))
			
  ;; Sort completion list if appropriate
  (if (or (not (eq cycle-mini-completion-type 'buffer))
	  cycle-mini-sort-buffers)
      (setq cycle-mini-completion-list
	    (sort cycle-mini-completion-list 'string<)))

  cycle-mini-completion-list)

(defun cycle-mini-next-completion (&optional incr)
  "Replace input by next possible completion."
  (interactive)
  (or incr (setq incr 1))
  (let* ((input (buffer-substring (point-min)(point-max)))
	 (inlen (length input))
	 (comps (or cycle-mini-completion-list
		    (cycle-mini-cull-completions)))
	 (complen (length comps))
	 (filecomp (eq cycle-mini-completion-type 'file)))

    (cond
     ((null comps)			; No matches
      (save-excursion
	(goto-char (point-max))
	(ding)
	(insert " [No match]")
	(goto-char (+ (point-min) inlen))
	(sit-for 2)
	(delete-region (point) (point-max))
	(setq cycle-mini-last-completion nil)))
     ((= 1 complen)			; Only one exact match
      (erase-buffer)
      (insert
       (if filecomp
	   (file-name-directory input) "")
       (car comps) " [Sole completion]")
      (goto-char (+ (point-min)
		    (if filecomp
			(length (file-name-directory input)) 0)
		    (length (car comps))))
      (sit-for 2)
      (delete-region (point) (point-max))
      (setq cycle-mini-last-completion 0))
     (t
      (erase-buffer)
      (if (not (and (numberp cycle-mini-last-completion)
		    (>= cycle-mini-last-completion 0)
		    (< cycle-mini-last-completion complen)))
	  (setq cycle-mini-last-completion
		(cond
		 ;; If doing filename completion, start after dot files
		 ((and filecomp
		       (string= input
				(file-name-directory input)))
		  (if (< incr 0) (1- complen)
		    (let ((i 0))
		      (while (<= (aref (nth i comps) 0) ?.)
			(setq i (1+ i)))
		      i)))
		 ;; If we have exact match, start with *next* match
		 ((and (string= input
				(concat
				 (if filecomp (file-name-directory input)
				   "")
				 (car comps)))
		       (> complen 1))
		  (if (> incr 0) 1 (1- complen)))
		 ;; Otherwise, start at beginning (or end if going up)
		 (t (if (> incr 0) 0 (1- complen)))))
	(setq cycle-mini-last-completion (+ cycle-mini-last-completion incr))
	(if (eq cycle-mini-last-completion -1)
	    (setq cycle-mini-last-completion
		  (if cycle-mini-wrap (1- complen)
		    (ding) 0)))
	(if (eq cycle-mini-last-completion complen)
	    (setq cycle-mini-last-completion
		  (if cycle-mini-wrap 0
		    (ding) (1- complen)))))
      (insert
       (if filecomp
	   (file-name-directory input) "")
       (nth cycle-mini-last-completion comps))))
    (goto-char (+ (point-min) inlen))
    (setq cycle-mini-eoinput (+ (point-min) inlen))))

(defun cycle-mini-previous-completion ()
  "Replace input by previous possible completion."
  (interactive)
  (cycle-mini-next-completion -1))

(defun cycle-mini-pre-command-hook ()
  "Do all necessary setup before a command runs in the minibuffer."
  (if (or (eq this-command 'cycle-mini-next-completion)
	  (eq this-command 'cycle-mini-previous-completion))
      (if (> cycle-mini-eoinput (point-max))
	  ;; Oops, got rug pulled out from under us - reinit:
	  (cycle-mini-no-completion)
	(let ((buffer-undo-list buffer-undo-list)) ; prevent entry
	  (delete-region cycle-mini-eoinput (point-max))))))

(defun cycle-mini-post-command-hook ()
  "Do all necessary cleanup after a command runs in the minibuffer."
  (if (or (eq this-command 'cycle-mini-next-completion)
	  (eq this-command 'cycle-mini-previous-completion))
      ()
    (cycle-mini-no-completion)))

(defun cycle-mini-reset ()
  "Reset minibuffer completion list to the beginning before we begin."
  (let ((vars cycle-mini-local-vars))
    (while vars
      (make-local-variable (car vars))
      (setq vars (cdr vars))))

  (add-hook 'pre-command-hook 'cycle-mini-pre-command-hook)
  (add-hook 'post-command-hook 'cycle-mini-post-command-hook)
  (setq cycle-mini-completion-type
	(cond
	 ((and (symbolp minibuffer-completion-table)
	       (stringp minibuffer-completion-predicate))
	  'file)
	 ((and (listp minibuffer-completion-table)
	       (bufferp (cdr (car minibuffer-completion-table))))
	  'buffer)
	 (t
	  'other)))
  (cycle-mini-no-completion)

  ;; Handle completion-ignored-extensions
  (and (eq cycle-mini-completion-type 'file)
       ;; Build a regular expression representing the extensions list
       (or (equal completion-ignored-extensions
		  cycle-mini-ignored-extensions)
	   (setq cycle-mini-ignored-regexp
		 (concat "\\("
			 (mapconcat
			  'regexp-quote
			  (setq cycle-mini-ignored-extensions
				completion-ignored-extensions)
			  "\\|")
			 "\\)\\'"))))
  )

(add-hook 'minibuffer-setup-hook 'cycle-mini-reset)

(provide 'cycle-mini)
