;;; wiki-sum.el --- Write a summary of the wiki

;; Copyright (C) 2001  Alex Schroeder <alex@gnu.org>

;; Emacs Lisp Archive Entry
;; Filename: wiki-sum.el
;; Version: 1.0.2
;; Keywords: hypermedia
;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: Alex Schroeder <alex@gnu.org>
;; Description: Write a summary of the wiki
;; Compatibility: Emacs20, XEmacs21

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; Eventually this code should generate description and keyword meta
;; tags for my wiki pages.

;;; Code:

(require 'wiki)

(defgroup wiki-sum nil
  "Summarizing a wiki."
  :group 'wiki)

(defcustom wiki-summary-exclude-pages nil
  "Pages to exclude from the summary."
  :group 'wiki-sum
  :type '(choice (const :tag "Include all files" nil)
		 (repeat :tag "Files" (string :tag "Page"))))

(defcustom wiki-summary-name "SiteSummary"
  "Name of the summary page."
  :group 'wiki-sum
  :type 'string)

(defcustom wiki-summary-include-nulls t
  "Wether to include pages without summary information in the summary."
  :group 'wiki-sum
  :type 'boolean)

;; Main functions

(defun wiki-summary ()
  "Show a summary of the wiki."
  (interactive)
  (let ((buf (get-buffer-create "*wiki*"))
	(sum (wiki-summarize-files)))
    (message "Pretty printing summary...")
    (set-buffer buf)
    (erase-buffer)
    (pp sum buf)
    (emacs-lisp-mode)
    (wiki-mode 1)
    (switch-to-buffer buf)
    (message "Pretty printing summary...done")))

(defun wiki-write-summary ()
  "Write a summary wiki."
  (interactive)
  (let ((file (wiki-expand-name wiki-summary-name))
	(summaries (wiki-summarize-files))
	summary)
    (setq summaries (sort summaries
			  (lambda (p1 p2)
			    (string< (upcase (car p1))
				     (upcase (car p2))))))
    (message "Writing %s..." file)
    (with-temp-buffer
      (while summaries
	(setq summary (car summaries)
	      summaries (cdr summaries))
	(when (or wiki-summary-include-nulls
		  (cdr summary))
	  (insert (car summary) "\n\n" 
		  (or (cdr summary) "(No summary available.)")
		"\n\n")))
      (write-file file))
    (message "Writing %s...done" file)))

;; Functions creating the actual data structure

(defun wiki-summarize-files ()
  "Summarize all the wiki files.
This returns an alist where each element has the form
\(PAGENAME . SUMMARY)"
  (let ((files (wiki-existing-file-names))
	file name
	summaries)
    (while files
      (setq file (cdr (car files))
	    name (car (car files))
	    files (cdr files))
      (message "Reading %s" file)
      (with-temp-buffer
	;; fake an existing buffer-file-name in the temp buffer
	(insert-file-contents file)
	(setq summaries (cons (cons name (wiki-summarize))
			      summaries))))
    (let ((pages wiki-summary-exclude-pages)
	  page)
      (while pages
	(setq page (car pages)
	      pages (cdr pages))
	(setq summaries (delete (assoc page summaries) summaries))))
    (message "Done")
    summaries))

(defun wiki-summarize ()
  "Summarize the current buffer.
Pick the first paragraph with more than one sentence and return it as a
string."
  (goto-char (point-min))
  (let (summary)
    (while (and (< (point) (point-max))
		(not summary))
      (setq summary (wiki-summary-paragraph)))
    summary))

(defun wiki-summary-paragraph ()
  "Return current paragraph if it has more than two sentences.
This assumes `text-mode' like paragraphs."
  (let* ((paragraph-start "[ \t]*$")
	 (start (point))
	 (end (progn (forward-paragraph) (point))))
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (when (< 0 (skip-chars-forward " \t\n" end))
	(setq start (point)))
      (condition-case nil
	  (forward-sentence 2)
	(error))
      (if (equal (point) end)
	  nil
	(goto-char end)
	(skip-chars-backward " \t\n" start)
	(buffer-substring start (point))))))

(provide 'wiki-sum)

;; wiki-sum.el ends here

