;;; tcov.el -- Highlight a buffer according to tcov output.

;;; Copyright (C) 1994 Free Software Foundation, Inc.

;;; Author: Tom Tromey <tromey@busco.lanl.gov>
;;; Keywords: coverage
;;; Version: $Revision: 1.4 $
;;; Created: Tue Sep 13 1994

;;; This file is not yet part of GNU Emacs, but is released under the
;;; same copyright.  See the GPL for details.

;;; Commentary:

;;; LCD Archive Entry:
;;; tcov|Tom Tromey|tromey@busco.lanl.gov|
;;; Highlight a buffer according to coverage output|
;;; 20-Sep-1994|1.4|~/misc/tcov.el.Z|

;;;
;;; This code provides fontification based on the coverage output of
;;; compilers. It doesn't require "tcov".  This code is largely
;;; untested; in particular I have only used it with the Sun C++
;;; compiler.  I have no idea if it works for any other sort of
;;; compiler, and in fact it probably doesn't.
;;;
;;; The primary interface is tcov-fontify-buffer.  This looks for the
;;; ".d" file associated with the current buffer, and then does the
;;; fontification.
;;;
;;; You can use tcov-fontify-directory to do an entire directory at
;;; once.
;;;
;;; Put:
;;;
;;; (autoload 'tcov-fontify-directory "tcov" "Coverage-fontify a directory" t)
;;; (autoload 'tcov-fontify-buffer "tcov" "Coverage-fontify buffer" t)
;;;
;;; in your .emacs.
;;;
;;; tcov-default-faces will set up the faces in a simple way.

;;; Change log:
;;; $Log: tcov.el,v $
;;; Revision 1.4  1994/09/20  17:43:04  tromey
;;; Added LCD entry
;;;
;;; Revision 1.3  1994/09/13  21:12:48  tromey
;;; Removed debugging code.
;;;
;;; Revision 1.2  1994/09/13  20:01:02  tromey
;;; *** empty log message ***
;;;
;;;Revision 1.1  1994/09/13  17:32:28  tromey
;;;Initial revision
;;;



;;; Code:

(defvar tcov-bins [0]
  "Vector of integers controlling operation of `tcov-fontify-file'.

The vector specifies bins mapping the number of times a line is
executed onto the faces used in fontification.  The number of bins is
one greater than the length of the array.

Each face is given a name like `tcov-face-N', where N is the bin
number.

The default value is [0].  Thus by default lines that are never
executed are displayed with tcov-face-0 and all other lines are
displayed with tcov-face-1.")

;; Call this to set up some default faces.  Looks ok on a white
;; background.
(defun tcov-default-faces ()
  "Set up some default faces for `tcov-fontify-buffer'."
  (interactive)
  (let ((face (make-face 'tcov-face-0)))
    (set-face-foreground face "red")
    ;; face-1 is just default.
    (make-face 'tcov-face-1)))

;; Read a ".d" file into a list.
(defun tcov-snarf-d-file (d-file)
  (save-excursion
    (let* ((buffer (get-buffer-create " *tcov-junk*"))
	   (standard-input buffer)
	   result
	   one)
      (set-buffer buffer)
      (insert-file-contents d-file nil nil nil t)
      (goto-char (point-min))
      (while (< (point) (point-max))
	(let ((here (point)))
	  (skip-syntax-forward "-")
	  (skip-chars-forward "0-9")
	  (setq one (string-to-int (buffer-substring here (point))))
	  (setq here (point))
	  (skip-syntax-forward "-")
	  (skip-chars-forward "0-9")
	  (setq result (cons
			(cons one
			      (string-to-int (buffer-substring here (point))))
			result))))
      ;; I've found that tcov can sometimes output information for a
      ;; given line twice.  In this case we want to use the bigger
      ;; number.  So we first sort on the minor key and then the
      ;; major, relying on the fact that sort is stable.
      (sort (sort result (function (lambda (x y)
				     (< (cdr x) (cdr y)))))
	    (function (lambda (x y)
			(< (car x) (car y))))))))

;; Find .d file for .c/.cc file.
(defun tcov-d-file ()
  (let* ((name (file-name-nondirectory (buffer-file-name)))
	 (dir (file-name-directory (buffer-file-name)))
	 (index (cond
		 ((string-equal (substring name -2) ".c")
		  -2)
		 ((string-equal (substring name -3) ".cc")
		  -3)
		 (t nil))))
    (if index
	(let ((real (concat (substring name 0 index) ".d")))
	  (and (file-exists-p (concat dir real))
	       (concat dir real))))))

(defun tcov-fontify-buffer ()
  "Display coverage information for the current buffer.
Display is controlled by `tcov-bins', which see.
Buffer's file name must end in `.c' or `.cc'."
  (interactive)
  (let ((file (tcov-d-file)))
    (if file
	(save-excursion
	  (goto-char (point-min))
	  (let ((last-line 0)
		(d-info (tcov-snarf-d-file file))
		i
		slot
		this-line
		count
		start
		end
		(modified (buffer-modified-p))
		(len (length tcov-bins)))
	    ;; Loop over every line in the .d file.
	    (while (and d-info
			(<= last-line (caar d-info)))
	      ;; Find this entry in the vector.
	      (setq i 0)
	      (setq slot nil)
	      (setq this-line (caar d-info))
	      (setq count (cdar d-info))
	      (while (and (not slot) (< i len))
		(if (<= count (aref tcov-bins i))
		    (setq slot i))
		(setq i (1+ i)))
	      (or slot
		  (setq slot len))
	      ;; Find beginning and end of desired region.
	      (goto-line last-line)
	      (setq start (point))
	      (goto-line (1+ this-line))
	      (setq end (point))
	      ;; Set face on those lines.
	      (let ((face (intern (concat "tcov-face-" slot))))
		(make-face face)
		(put-text-property start end 'face face))
	      ;; Next.
	      (setq last-line this-line)
	      (setq d-info (cdr d-info)))
	    (and (buffer-modified-p)
		 (not modified)
		 (set-buffer-modified-p nil))))
      (error "No .d file for this source file."))))

(defun tcov-fontify-directory (dir)
  "Look at results of `tcov' output for all files in directory.
Loads each source (\".c\" or \".cc\") file that has a corresponding
\".d\" file, and then fontifies it."
  (interactive "DDirectory holding coverage source: ")
  (let ((files (directory-files dir nil "\\.d$"))
	base)
    (while files
      (setq base (substring (car files) 0 -2))
      (cond
       ((file-exists-p (concat dir base ".c"))
	(find-file (concat dir base ".c"))
	(message "Coverage-fontifying %s.c..." base)
	(tcov-fontify-buffer)
	(message "Coverage-fontifying %s.c... done" base))
       ((file-exists-p (concat dir base ".cc"))
	(find-file (concat dir base ".cc"))
	(message "Coverage-fontifying %s.cc..." base)
	(tcov-fontify-buffer)
	(message "Coverage-fontifying %s.cc... done" base)))
      (setq files (cdr files)))))

;;; tcov.el ends here
