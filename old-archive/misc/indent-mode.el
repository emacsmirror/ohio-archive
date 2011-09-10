;;
;; Purpose of this package:
;; 	Just a first try to implement a minor-mode for indentation
;; 	that is like auto-indent (without the bug of inserting to much
;; 	spaces when inserting more blank lines) and the capability
;; 	to backindent to the next level (You may compare this with the
;; 	editing features of the Borland Turbo-Pascal/C-Editor in the IDE).
;;
;; Installation instructions
;;	No, just load and use it (and hope it works)
;;
;; Usage instructions:
;;	Typing M-x indent-mode toggles between on and of. See
;;	documentation.
;;
;; Known bugs:
;;	Soon will come.
;;
;; LCD Archive Entry:
;; indent-mode|PREINING Norbert|ep-prei@ophelia.tuwien.ac.at|
;; Minor mode for correct autoindentation and backindentation|
;; 10-May-1994|0.1|~/misc/indent-mode.el.Z|
;;
**** file indent-mode.el following ****
;;;
;;; File:		indent-mode.el
;;; Date:		Thu Apr 13 17:01:41 1994
;;; Author:		PREINING Norbert (ep-prei)
;;; Email:              ep-prei@ophelia.tuwien.ac.at
;;; 
;;; Version 0.1
;;;
;;; Just a first try to implement a minor-mode for indentation
;;; that is like auto-indent (without the bug of inserting to much
;;; spaces when inserting more blank lines) and the capability
;;; to backindent to the next level (You may compare this with the
;;; editing features of the Borland Turbo-Pascal/C-Editor in the IDE).
;;; Maybe this stuff is implemented already somewhere else, but I 
;;; didn't find it. If there are bugs (and there will!), try to fix
;;; them and send improvement to me, this minor-mode was my first
;;; attempt. Thanxs a lot!
;;;

; not to come into conflict with max-lisp-search-depth or so. This
; value was heuritically found.
(defvar max-indent-search-depth 0
  "Search-depth for indentation.")
(setq max-indent-search-depth 45) 

(defvar indent-mode-map ()
  "Keymap used in Indent minor-mode.")

(if indent-mode-map
    ()
  (setq indent-mode-map (make-sparse-keymap))
  (define-key indent-mode-map "\C-j"   'reindent-then-newline-and-indent)
  (define-key indent-mode-map "\177"    'indent-delete)
  (define-key indent-mode-map "\r"    'newline-and-indent))

(make-variable-buffer-local 'indent-mode)
(setq indent-mode nil)

(if (not (assoc 'indent-mode minor-mode-alist))
    (setq minor-mode-alist
	  (cons '(indent-mode " Indent") minor-mode-alist)))

(if (not (assoc 'indent-mode minor-mode-map-alist))
    (setq minor-mode-map-alist
	  (cons (cons 'indent-mode indent-mode-map)
		minor-mode-map-alist)))

; save the old indent-line-function!
(make-variable-buffer-local 'indent-save-indfun)
(setq-default indent-save-indfun nil)

(make-variable-buffer-local 'indent-line-function)

(defun indent-mode (&optional arg)
  "Toggle indent mode.
With arg, turn indent mode on if and only if arg is positive.
Minor mode for editing indented Text. Like relative-indent. 
Is  INDENT-BACKINDENT not nil, DEL deletes a char or if there are only
whitespaces in direction to the beginning of the line, backindents to
the next outer level, otherwise just backward-delete-char-untabify.
At the beginning of a line DEL backs up to the previous line."
  (interactive "P")
  (prog1 (setq indent-mode
	       (if (if (null arg)
		       (not indent-mode)
		     (> (prefix-numeric-value arg) 0))
		   (progn
		     (setq indent-save-indfun indent-line-function)
		     (setq indent-line-function 'indent-indent-line)
		     " Indent")
		 (progn
		   (setq indent-line-function indent-save-indfun)
		   nil)))
    (set-buffer-modified-p (buffer-modified-p))))
		          
(defun indent-indent-line (&optional bod)
  "Indent current line.
Return the amount the indentation changed by."
  (indent-to (calculate-indent-indent)))

(defun indent-indent-command (&optional bod)
  (interactive)
  (indent-to (calculate-indent-indent)))
  
(defun calculate-indent-indent ()
  "Return appropriate indentation for current line.
In usual case returns an integer: the column to indent to."
  (defun calc-indent (arg)
    (beginning-of-line 0)
    (let ((ind (indentation-of-line)))
      (cond 
       ((not (eq ind -1))
	ind)
       ((or (eq (point) (point-min))
	    (= arg 0))
	0)
       (t
	(calc-indent (- arg 1))))))
  (save-excursion
    (calc-indent max-indent-search-depth)))
	 	  
(defun indentation-of-line (&optional arg)
  "Return the amount of indentation of line, with ARG supplied moves
forward ARG - 1 lines first."
  (interactive)
  (save-excursion
    (beginning-of-line arg)
    (skip-chars-forward " \t")
    (if (looking-at "\n")
	-1
      (current-column))))

(defvar indent-backindent 1 "If not nil backindent in indent-mode.")

(defun calculate-indent-backindent ()
  "Calculates the backindent."
  (let ((act (current-column)))
    (defun calc-back (arg)
      (cond 
       ((eq (point) (point-min))
	(cond
	 ((< (indentation-of-line) act)
	  (- act (indentation-of-line)))
	 (t
	  act)))
       ((= arg 0)
	act)
       (t
	(let ((prev (calculate-indent-indent)))
	  (if (< prev act)
	      (- act prev)
	    (forward-line -1)
	    (calc-back (- arg 1)))))))
    (save-excursion
      (beginning-of-line)
      (if (eq (point) (point-min))
	  act
	(calc-back max-indent-search-depth)))))
	  
(defun indent-delete (&optional arg)
  "If indent-backindent is non-nil and point is on the beginning of a
line or only preceded by white-spaces backs to the last
indentation-position. If ARG is supplied, this just calls
backward-delete-char-untabify passing along ARG.

If indent-backindent is nil, just call backward-delete-char-untabify."
  (interactive)
  (cond
   ((or (eq (point) (point-min))
	(eq (current-column) 0))
    (backward-delete-char 1))
   ((and indent-backindent (not arg))
    (if (save-excursion
	  (skip-chars-backward " \t")
	  (eq 0 (current-column)))
	(backward-delete-char-untabify (calculate-indent-backindent))
      (backward-delete-char-untabify 1)))
   (arg
    (backward-delete-char-untabify arg))
   (t
    (backward-delete-char-untabify 1))))
   
;;; end of indent-mode.el
