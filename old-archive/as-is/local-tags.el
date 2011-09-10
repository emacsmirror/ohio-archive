;Sender: arpa-unix-emacs-request@bbn.com
;Subject: Re: generalized tags..
;Message-Id: <4300044@m.cs.uiuc.edu>
;References: <5361@cbnews.ATT.COM>
;Source-Info:  From (or Sender) name not authenticated.
;Status: RO
;
;
;Here is local-tags.el, which is two replacement functions for tags.el.
;This allows you to provide mode-specific tags finding.
;
;dan
;---
;; local-tags.el  - buffer-local tag finding
;; Copyright (C) 1989 Daniel LaLiberte

;; This file is not yet part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;;-----------------------------------------------------------------
;; Buffer-local tag finding

;; Two hooks are provided.
;; These variables should be made buffer-local and assigned
;; functions appropriate to the buffer's major mode.

;; find-tag-default-hook is called to find a default tag for find-tag.

;; find-tag-hook is called by find-tag after a tag is found.
;; If find-tag-hook is nil for a buffer, then
;; the find-tag-hook for the current-buffer when find-tag was called
;; is used.  (This is to support a find-tag-hook for Info-mode
;; since the default major-mode for info files is not Info-mode.)

;; Dan LaLiberte
;; uiucdcs!liberte
;; liberte@cs.uiuc.edu
;; liberte%a.cs.uiuc.edu@uiucvmd.bitnet

(require 'tags)

(defvar find-tag-default-hook nil
  "Function to call to create a default tag.
Make it buffer-local in a mode hook.
The function is called with no args.")


(defvar find-tag-hook nil
  "Function to call after a hook is found.
Make it buffer-local in a mode hook.
The function is called with no args.")


(defun find-tag-tag (string)
  "Modified to use find-tag-default-hook"
  (let* ((default (or (and find-tag-default-hook
			   (funcall find-tag-default-hook))
		      (find-tag-default)))
	 (spec (read-string
		(if default
		    (format "%s(default %s) " string default)
		  string))))
    (list (if (equal spec "")
	      default
	    spec))))


(defun find-tag (tagname &optional next other-window)
  "Find tag (in current tag table) whose name contains TAGNAME.
 Selects the buffer that the tag is contained in
and puts point at its definition.
 If TAGNAME is a null string, the expression in the buffer
around or before point is used as the tag name.
 If second arg NEXT is non-nil (interactively, with prefix arg),
searches for the next tag in the tag table
that matches the tagname used in the previous find-tag.

See documentation of variable tags-file-name.

Modified to use find-tag-hook."
  (interactive (if current-prefix-arg
		   '(nil t)
		 (find-tag-tag "Find tag: ")))
  (let ((local-find-tag-hook find-tag-hook)  ; get find-tag-hook now
	buffer file linebeg startpos)
    (save-excursion
     (visit-tags-table-buffer)
     (if (not next)
	 (goto-char (point-min))
       (setq tagname last-tag))
     (setq last-tag tagname)
     (while (progn
	      (if (not (search-forward tagname nil t))
		  (error "No %sentries containing %s"
			 (if next "more " "") tagname))
	      (not (looking-at "[^\n\177]*\177"))))
     (search-forward "\177")
     (setq file (expand-file-name (file-of-tag)
				  (file-name-directory tags-file-name)))
     (setq linebeg
	   (buffer-substring (1- (point))
			     (save-excursion (beginning-of-line) (point))))
     (search-forward ",")
     (setq startpos (read (current-buffer))))
    (if other-window
	(find-file-other-window file)
      (find-file file))
    (widen)
    (push-mark)
    (let ((offset 1000)
	  found
	  (pat (concat "^" (regexp-quote linebeg))))
      (or startpos (setq startpos (point-min)))
      (while (and (not found)
		  (progn
		   (goto-char (- startpos offset))
		   (not (bobp))))
	(setq found
	      (re-search-forward pat (+ startpos offset) t))
	(setq offset (* 3 offset)))
      (or found
	  (re-search-forward pat nil t)
	  (error "%s not found in %s" pat file)))
    (beginning-of-line)
    (if find-tag-hook
	(funcall find-tag-hook)
      (if local-find-tag-hook
	 (funcall local-find-tag-hook))))
  (setq tags-loop-form '(find-tag nil t))
  ;; Return t in case used as the tags-loop-form.
  t)


;;-----------------------------------
;; Example buffer-local tag finding

(defun elisp-default-tag ()
  "Function to return a default tag for elisp-mode."
  (symbol-name
   (or (variable-at-point)
       (function-called-at-point)))
  )

;; Add these to your emacs-lisp-mode-hook
;; (make-local-variable 'find-tag-default-hook)
;; (setq find-tag-default-hook 'elisp-default-hook)


(defun Info-find-tag-hook ()
  "Function to call after finding a tag in Info-mode."
  (let ((onode Info-current-node)
	(ofile Info-current-file)
	(opoint (point)))
    (if (not (string= "*info*" (buffer-name)))
	(progn				; replace current *info* file
	  (kill-buffer "*info*")
	  (rename-buffer "*info*")))
    (or (eq major-mode 'Info-mode)
	(Info-mode))
    (setq Info-current-file
	  (file-name-sans-versions buffer-file-name))
    (Info-select-node)
    (or (and (equal onode Info-current-node)
	     (equal ofile Info-current-file))
	(setq Info-history (cons (list ofile onode opoint)
				 Info-history)))))

;; Info-mode does not have a hook, so patch in the necessary calls.

(require 'info)

;; Only do this once
(fset 'Info-mode
      (append (symbol-function 'Info-mode)
	      (list '(make-local-variable 'find-tag-hook)
		    '(setq find-tag-hook 'Info-find-tag-hook)
		    '(modify-syntax-entry ?\' "."))))

