;;;Date: 22 Jan 88 19:22:27 GMT
;;;From: Wolfgang Rupprecht <wolfgang@MGM.MIT.EDU>
;;;Organization: Freelance Software Consultant, Cambridge, Ma.
;;;Subject: Re: A GREP-like interface to DIFF ???

;;;Here is a little hack, show-diff that I have used in the past, to find
;;;and examine diffs. Showdiff will find the files corresponding to the
;;;diff, and display them in two separate windows. It will also put the
;;;point and mark around the diffs in each file for you. This makes it
;;;trivial to copy/kill the sectio from one buffer to the next. Just the
;;;thing for selectively applying patches.

;;;Showdiff is meant for examining whole, direcory tree diffs, but will
;;;work for single files too. You just have to make sure that the diff
;;;*looks* like a recursive (non-comtext) diff, with 'diff -r fromfile
;;;tofile' preceeding the actual diffs for that file.

;;;enjoy,
;;;-wolfgang

;;;Wolfgang Rupprecht	ARPA:  wolfgang@mgm.mit.edu (IP 18.82.0.114)
;;;Freelance Consultant	UUCP:  mit-eddie!mgm.mit.edu!wolfgang
;;;Boston, Ma.		VOICE: Hey_Wolfgang!_(617)_267-4365

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;									     ;;
;;	File:     showdiff.el						     ;;
;;	Author:   Wolfgang Rupprecht					     ;;
;;	Created:  Tue Apr 14 20:48:35 EDT 1987				     ;;
;;	Contents: parse a diff listing and finds the files		     ;;
;;									     ;;
;;	Copyright (c) 1987 Wolfgang Rupprecht.				     ;;
;;									     ;;
;;	$Log$								     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; GNU Emacs and this file showdiff.el, are distributed in the hope
;; that they will be useful, but WITHOUT ANY WARRANTY.  No author or
;; distributor accepts responsibility to anyone for the consequences of
;; using them or for whether they serve any particular purpose or work at
;; all, unless he says so in writing.  Refer to the GNU Emacs General
;; Public License for full details.

;; Everyone is granted permission to copy, modify and redistribute GNU
;; Emacs and showdiff.el, but only under the conditions described in the
;; GNU Emacs General Public License.  A copy of this license is supposed
;; to have been given to you along with GNU Emacs so you can know your
;; rights and responsibilities.  It should be in a file named COPYING.
;; Among other things, the copyright notice and this notice must be
;; preserved on all copies.

;; If you like this diff hack, and would like other custom
;; non-proprietary GnuEmacs extensions, let me know. I may be
;; interested in doing them for you on a contract basis. -wsr

(defvar diff-line-regexp
    "^diff[ \t]+\\([^ \t\n]+[ \t]+\\)*\\([^ \t\n]+\\)[ \t]+\\([^ \t\n]+\\)[ \t]*$"
  "rexexp for finding file names in a 'diff -r' listing")
(defvar changed-line-regexp
    "^\\(^[0-9]+\\)\\(,\\([0-9]+\\)\\)?[acd]\\([0-9]+\\)\\(,\\([0-9]+\\)\\)?$"
    "regexp for finding the line that descibes the changed line
numbers in a diff listing")

(defun show-diff ()
  "Find the two files corresponding to the section of the diff listing
in buffer and display the apropriate sections in two windows.  The
point and mark in both files will be around the diffs. The diff must
be a non-context diff. It must also be a diff -r so that the \"diff
file1 file2\" will appear in the output (you may also edit this in by
hand).  The file with the lower number in the name extension is
usually the 'to' file.  The 'from' file will be write-protected to
avoid inadvertant editing of the wrong file.

In short, use as follows:
\\[compile]
diff -r /absolute/path/foodir-old /absolute/path/foodir
\\[switch-to-buffer]
*compilation*
(or redirect the diff output to a file, and \\[find-file] filename)
move to diff that you want to examine.
\\[show-diff]"
  (interactive)
  (let (from-file to-file to-point from-point to-mark
		  from-mark (diff-directory default-directory))
    (save-excursion
      (end-of-line 1)
      (if (re-search-backward diff-line-regexp nil t)
	  (setq from-file (buffer-substring
			   (match-beginning 2) (match-end 2))
		to-file (buffer-substring
			 (match-beginning 3) (match-end 3)))
	  (error "Not inside a diff (couldn't find a 'diff * file1 file2')")))
    (save-excursion
      (end-of-line 1)
      (if (re-search-backward changed-line-regexp nil t)     
	  (setq 
	   from-point (match-as-int 1)
	   from-mark (if (nth 6 (match-data))
			 (1+ (match-as-int 3)) ; 1+ I think
			 (1+ from-point))
	   to-point (match-as-int 4)
	   to-mark (if (nth 12 (match-data))
		       (1+ (match-as-int 6)) ; ditto 
		       (1+ to-point)))
	  (error "Couldn't find the change descriptor line (eg. 15,17c24,33)")))
    (find-file-other-window to-file)
    (goto-line to-mark)
    (set-mark (point))
    (goto-line to-point)
    (find-file-other-window
     (if (file-name-absolute-p from-file)	; we might have changed directories
	 from-file
	 (concat diff-directory from-file)))
    (goto-line from-mark)
    (set-mark (point))
    (goto-line from-point)
    (setq buffer-read-only t)		; don't go editing the wrong one !!!
    (other-window 1)			;put the cursor in 'to' file
    (message "%s:(%d,%d) -> %s:(%d,%d)" from-file from-point from-mark
	     to-file to-point to-mark)))

(defun match-as-int (n)
  "Extract an integer for match NUMBER-N"
  (string-to-int (buffer-substring (match-beginning n) (match-end n))))

Wolfgang Rupprecht	ARPA:  wolfgang@mgm.mit.edu (IP 18.82.0.114)
Freelance Consultant	UUCP:  mit-eddie!mgm.mit.edu!wolfgang
Boston, Ma.		VOICE: Hey_Wolfgang!_(617)_267-4365
