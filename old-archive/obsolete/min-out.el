;;; @ min-out.el - Outline minor mode.
;;;
;;; $Id: min-out.el,v 1.18 1993/07/27 22:50:51 amanda Exp $
;;;
;;; LCD Archive Entry:
;;; min-out|Per Abrahamsen|abraham@iesd.auc.dk|
;;; Improved outline-minor-mode for Emacs 18 and 19|
;;; 27-Jul-1993|1.18|~/modes/min-out.el.Z|

(provide 'min-out)
(require 'min-bind)

;;; @@ Copyright
;;;
;;; Created 1987 by Per Abrahamsen at University of Aalborg, Denmark.
;;; Please report improvents and bugs to abraham@iesd.auc.dk.
;;;
;;; My modifications are donated to the public domain, but is 
;;; might contain code from the original outline.el, so...
;;;
;;; Outline mode commands for Emacs
;;; Copyright (C) 1986 Free Software Foundation, Inc.
;;;
;;; This file is part of GNU Emacs.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY.  No author or distributor
;;; accepts responsibility to anyone for the consequences of using it
;;; or for whether it serves any particular purpose or works at all,
;;; unless he says so in writing.  Refer to the GNU Emacs General Public
;;; License for full details.
;;;
;;; Everyone is granted permission to copy, modify and redistribute
;;; GNU Emacs, but only under the conditions described in the
;;; GNU Emacs General Public License.   A copy of this license is
;;; supposed to have been given to you along with GNU Emacs so you
;;; can know your rights and responsibilities.  It should be in a
;;; file named COPYING.  Among other things, the copyright notice
;;; and this notice must be preserved on all copies.

;;; @@ Installation
;;;  
;;; Make sure to evaluate the following lines: 
;;;
;;;    (make-variable-buffer-local 'outline-prefix-char)
;;;    (make-variable-buffer-local 'outline-regexp)
;;;    (make-variable-buffer-local 'outline-level-function)
;;; 
;;; before you make any attempt to give them a local value.
;;; Use setq-default to give them a global value.
;;;
;;; To be safe, add the three lines to your local init file (e.g. .emacs).

;;; @@ Exported Variables
;;;
;;; We try to do the right thing in the cases where either the local
;;; or global value of these are already set.

(make-variable-buffer-local 'outline-prefix-char)

 (if (null 'outline-prefix-char)
     (setq outline-prefix-char "\C-c\C-o"))

 (if (null (default-value 'outline-prefix-char))
     (setq-default outline-prefix-char "\C-c\C-o"))

 (defvar outline-prefix-char "\C-c\C-o"
   "*Key prefix for all outline commands.")

(make-variable-buffer-local 'outline-regexp)

 (if (null 'outline-regexp)
     (setq outline-regexp "[*\^l]+"))

 (if (null (default-value 'outline-regexp))
     (setq-default outline-regexp "[*\^l]+"))

 (defvar outline-regexp "[*\^l]+"
   "*Regular expression matching outline header lines.")

(make-variable-buffer-local 'outline-level-function)

 (if (null 'outline-level-function)
     (setq outline-level-function 'outline-level-default))

 (if (null (default-value 'outline-level-function))
     (setq-default outline-level-function 'outline-level-default))

 (defvar outline-level-function 'outline-level-default
   "*Function to calculate the outline level of a header line.
Point will be at the beginning of a header line.")

;;; @@ Outline Major Mode
;;;
;;; We need the code from the major mode.

(if (featurep 'outline)
    ()
  (load-library "outline")
  (provide 'outline))

;;; Nuke the Emacs 19 keymap for outline-minor-mode

(and (boundp 'minor-mode-map-alist)
     (assoc 'outline-minor-mode minor-mode-map-alist)
     (setcdr (assoc 'outline-minor-mode minor-mode-map-alist)
	     (make-sparse-keymap)))

;;; @@ Redefinitions
;;; 
;;; The following functions from the major mode is changed to allow
;;; read-only buffers and to speed up nonmodified buffers.

(defun outline-flag-region (from to flag)
  "Hides or shows lines from FROM to TO, according to FLAG.  If FLAG
is \\n (newline character) then text is hidden, while if FLAG is \\^M
\(control-M) the text is shown."
  (let ((modp (buffer-modified-p))
	(read-only buffer-read-only))
    (if read-only (toggle-read-only))
    (unwind-protect
        (subst-char-in-region from to
			      (if (= flag ?\n) ?\^M ?\n)
			      flag t)
      (progn
	(if read-only (toggle-read-only))
	(set-buffer-modified-p modp)))))

(defun hide-region-body (start end)
  "Hide all body lines in the region, but not headings."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (let ((modp (buffer-modified-p)))
	(set-buffer-modified-p t)
	(while (not (eobp))
	  (outline-flag-region (point) (progn (outline-next-preface)
					      (point)) ?\^M)
	  (if (not (eobp))
	      (forward-char
	       (if (looking-at "[\n\^M][\n\^M]")
		   2 1))))
	(set-buffer-modified-p modp)))))

;;; @@ Header Level

(defun outline-level-default ()
  "Return the depth to which a statement is nested in the outline.
Point must be at the beginning of a header line.
This is actually the length of whatever outline-regexp matches."
  (save-excursion
    (looking-at outline-regexp)
    (- (match-end 0) (match-beginning 0))))

(defun outline-level ()
  "Return the depth to which a this heading is nested in the outline.
This is done by a call to the value of outline-level-function, which
default to outline-level-default."
  (funcall outline-level-function))

;;; @@ Minor Map and Menu

(defvar outline-old-prefix "\C-c"
  "The prefix for outline minor mode commands last used.")

(defvar outline-minor-mode-map
  (let ((outline (make-sparse-keymap)))
    (define-key outline outline-old-prefix (make-sparse-keymap))
    outline)
  "Keymap used for outline commands.")

(defun outline-define-key (key binding)
  ;; Add KEY BINDING to outline-minor-mode-map if unbound.
  (let ((index (concat outline-old-prefix key)))
    (if (null (lookup-key outline-minor-mode-map index))
	(define-key outline-minor-mode-map index binding))))

(outline-define-key "\C-n" 'outline-next-visible-heading)
(outline-define-key "\C-p" 'outline-previous-visible-heading)
(outline-define-key "\C-f" 'outline-forward-same-level)
(outline-define-key "\C-b" 'outline-backward-same-level)
(outline-define-key "\C-u" 'outline-up-heading)
(outline-define-key "\C-t" 'hide-body)
(outline-define-key "\C-a" 'show-all)
(outline-define-key "\C-h" 'hide-subtree)
(outline-define-key "\C-s" 'show-subtree)
(outline-define-key "\C-i" 'show-children)
(outline-define-key "\C-c" 'hide-entry)
(outline-define-key "\C-e" 'show-entry)
(outline-define-key "\C-l" 'hide-leaves)
(outline-define-key "\C-x" 'show-branches)

(minor-iflemacs
    ()
  (defun outline-strip-nil (map)
    "Strip nil entries from MAP."
    (let ((item map))
      (while (cdr item)
	(let ((entry (car (cdr item))))
	(if (consp entry)
	    (let ((value (cdr entry)))
	      (cond ((null value)
		     (setcdr item (cdr (cdr item))))
		    ((keymapp value)
		     (outline-strip-nil value)
		     (setq item (cdr item)))
		    (t
		     (setq item (cdr item)))))
	  (setq item (cdr item))))))))

(defun outline-update-prefix ()
  "Update prefix key for outline minor map."
  (if (string-equal outline-old-prefix outline-prefix-char)
      ()
    (let ((map (lookup-key outline-minor-mode-map outline-old-prefix)))
      (define-key outline-minor-mode-map outline-old-prefix nil)
      (define-key outline-minor-mode-map outline-prefix-char map)
      (setq outline-old-prefix outline-prefix-char)
      (minor-iflemacs
	  ()
	(outline-strip-nil outline-minor-mode-map)))))

(outline-update-prefix)

;;; @@ Minor Mode
;;;
;;; We want Outline to appear in the mode line when selective-display
;;; is on. 

(defvar outline-minor-mode nil
  "True, iff we are in outline-minor-mode.")

 (make-variable-buffer-local 'outline-minor-mode)

(if (not (assoc 'outline-minor-mode minor-mode-alist))
    (setq minor-mode-alist (cons '(outline-minor-mode " Outline")
				 minor-mode-alist)))

;;; And we want to display all text if we change major mode.
(put 'selective-display 'killing-local-variable-function
     (function (lambda (&rest args) (show-all))))

(defun outline-minor-mode (&optional arg)
  "Toggle outline minor mode.
With prefix arg, turn ouline mode on iff arg is positive.

Minor mode for editing outlines with selective display.  

Headings are lines which matches the regular expression defined in the
variable `outline-regexp', by default lines starting with asterisks.
Each heading has a level calculated by calling the function specified
by the variable `outline-level-function', the default function use the
length of the string matched by the outline-regexp.  That is, one
asterisk for major headings, two for subheadings, etc.  Lines not
matched by outline-regexp are body lines.

Body text or subheadings under a heading can be made temporarily
invisible, or visible again.  Invisible lines are attached to the end
of the heading, so they move with it, if the line is killed and yanked
back.  A heading with text hidden under it is marked with an ellipsis
at the end (...).

The commands available will be bound to key specified by the variable
`outline-prefix-char'.  The default value is \"\\C-c\\C-o\\.  This
sequence has been chosen to minimize the conflict with global key
bindings, and you will probably want to overwrite it with a shorter
sequence, such as \"\\C-o\" or \"\\C-l\".  To get complete
compatibility with the outline major mode nindings, set it to
\"\\C-c\". 

Commands:

\\{outline-minor-mode-map}"

  (interactive "P")
  (if (or (and (null arg) selective-display)
	  (<= (prefix-numeric-value arg) 0))
      ;; Turn it off
      (if (not selective-display)
	  ()				;Already off
	(minor-unbind 'outline-minor-mode)
	(show-all))
    ;; Turn it on
    (if selective-display
	()				;Already on
      (minor-set-variable 'outline-minor-mode 'selective-display t)
      (minor-set-variable 'outline-minor-mode 'outline-minor-mode t)
      (outline-update-prefix)
      (minor-add-to-keymap 'outline-minor-mode outline-minor-mode-map)))
  ;; No-op, but updates mode line.
  (set-buffer-modified-p (buffer-modified-p)))

;;; @@ Emacs

;;; Local Variables:
;;; mode: emacs-lisp
;;; mode: outline-minor
;;; outline-regexp: ";;; @+\\|(......"
;;; End:
