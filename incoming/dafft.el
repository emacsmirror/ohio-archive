;;;; dafft.el --- minor mode for traversing and working with diffs
;;;; Copyright (C) 1996 Jim Blandy

;; Author: Jim Blandy
;; Maintainer: Jim Blandy

;; Dafft is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; Dafft is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a copy of the GNU General Public License, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:

;; Dafft is a minor mode for working with the output from the "diff"
;; program (also known as "patches").  It introduces bindings for:
;; - finding the next and previous diff hunks,
;; - finding the next and previous files in a multi-file diff, and
;; - jumping to the file position the current hunk would apply to.
;; Perhaps future revisions will be able to apply the patch as well, by
;; running the "patch" program.
;; 
;; Dafft is a minor mode because patches often appear in buffers which
;; have other major modes, like those containing mail messages or
;; netnews articles.  Like any other minor mode, the user can switch
;; on dafft and use its bindings without disturbing the current major
;; mode.
;;
;; To see documentation, load this file, and type `C-h C-f dafft-mode RET'.
;; To try it out, load this file, bring up a buffer containing a patch,
;; and type `M-x dafft-mode'.

;;; Code:


;;;; Standard minor mode machinery.

(defvar dafft-mode nil
  "*If true in the current buffer, enable bindings for navigating patches.
See the documentation for the `dafft-mode' command for more details.")

(make-variable-buffer-local 'dafft-mode)

(defun dafft-mode (&optional arg)
  "Toggle dafft minor mode, for working with patches.
With an argument, turn dafft mode on iff the argument is positive.

Dafft mode provides the following bindings:\\<dafft-mode-map>
\\[dafft-next-hunk]\tMove point to the top of the next hunk in the patch.
\\[dafft-prev-hunk]\tMove to the previous hunk.
\\[dafft-next-file]\tMove to the next file in the patch.
\\[dafft-prev-file]\tMove to the previous file.
\\[dafft-find-target]\tFind the file the patch would apply to, and 
\tmove point to the position in that file affected by the current hunk.

If the variable `dafft-recenter' is non-nil, the motion commands
automatically scroll to display the current hunk at the top of the
window."
  (interactive "P")
  (setq dafft-mode
	(if arg (> (prefix-numeric-value arg) 0)
	  (not dafft-mode))))

;;; Add an element to minor-mode-alist for dafft-mode.
(or (assq 'dafft-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(dafft-mode " Dafft") minor-mode-alist)))


;;;; Moving about between hunks.

(defvar dafft-recenter nil
  "*If non-nil, display hunks at the top of the window after moving.
This variable changes the behavior of the dafft-mode hunk motion
commands; see the documentation for the `dafft-mode' command.")

(defun dafft-maybe-recenter ()
  "If `dafft-recenter' is non-nil, show the current hunk at the window top."
  (if dafft-recenter
      (save-excursion
	(forward-line -2)
	 (set-window-start (selected-window) (point)))))


(defconst dafft-hunk-regexp
  "^\\*\\*\\*\\*+\n\\*\\*\\* \\([0-9]+\\),\\([0-9]+\\) \\*+$")

(defun dafft-hunk-line-range ()
  "Extract the starting and ending line range to which a hunk applies.
Return a list of the form (START END).
This function assumes that the last regexp match was against
dafft-hunk-regexp."
  (list (string-to-int (buffer-substring (match-beginning 1) (match-end 1)))
	(string-to-int (buffer-substring (match-beginning 2) (match-end 2)))))


(defun dafft-next-hunk (&optional n)
  "Move point to the next hunk boundary.
With numeric argument N, move to the top of the N'th subsequent hunk."
  (interactive "p")
  
  ;; If we're at the top of a hunk, don't count that.
  (forward-line 1)

  ;; Find the start of N'th subsequent hunk.
  (or (re-search-forward dafft-hunk-regexp nil t n)
      (error "last hunk in patch"))
  (goto-char (match-beginning 0))

  (dafft-maybe-recenter))

(defun dafft-prev-hunk (&optional n)
  "Move point to the previous hunk boundary.
With numeric argument N, move to the top of the N'th previous hunk."
  (interactive "p")
  
  ;; Find the start of N'th previous hunk.
  (or (re-search-backward dafft-hunk-regexp nil t n)
      (error "first hunk in patch"))
  (goto-char (match-beginning 0))

  (dafft-maybe-recenter))



;;;; Moving about between files within a patch.

(defconst dafft-file-regexp
  (concat "^\\*\\*\\*[ \t]+\\([^ \t]*[a-zA-Z/][^ \t]*\\).*\n"
	  "---[ \t]+\\([^ \t]*[a-zA-Z/][^ \t]*\\).*\n"))

(defun dafft-hunk-filenames ()
  "Extract the starting and ending line range to which a hunk applies.
Return a list of the form (START END).
This function assumes that the last regexp match was against
dafft-hunk-regexp."
  (list (buffer-substring (match-beginning 1) (match-end 1))
	(buffer-substring (match-beginning 2) (match-end 2))))


(defun dafft-next-file (&optional n)
  "Move point to the next file boundary.
With numeric argument N, move to the top of the N'th subsequent file."
  (interactive "p")
  
  ;; Find the first hunk boundary after then N'th subsequent file boundary.
  (or (re-search-forward dafft-file-regexp nil t n)
      (error "last file in patch"))
  (goto-char (match-end 0))
  (dafft-maybe-recenter))

(defun dafft-prev-file (&optional n)
  "Move point to the previous file boundary.
With numeric argument N, move to the top of the N'th previous file."
  (interactive "p")
  (save-excursion
    ;; If we're sitting just after a file boundary, don't count that.
    (if (= (point)
	   (save-excursion (re-search-backward dafft-file-regexp nil t)
			   (match-end 0)))
	(goto-char (match-beginning 0)))
  
    ;; Find the first hunk boundary after the N'th previous file boundary.
    (or (re-search-backward dafft-file-regexp nil t n)
	(error "first file in patch")))

  (goto-char (match-end 0))
  (dafft-maybe-recenter))


;;;; Guessing the name of the file to which a given hunk applies.

(defvar dafft-target-directory nil
  "The directory under which we found the last patch target.
Used to concoct a reasonable default filename for `dafft-find-target'.
If nil, use the current directory of the buffer containing the patch.

The default filename used by `dafft-find-target' is the filename
associated with the current patch hunk, with the first few directory
elements stripped off the front (`dafft-target-strip-count' says how
many), relative to `dafft-target-directory'.")

(defvar dafft-target-strip-count 0
  "By default, strip this many elements from the paths appearing in patches.
Used to concoct a reasonable default filename for `dafft-find-target'.
For more details, see the documentation for the variable
`dafft-target-directory'.")

(make-variable-buffer-local 'dafft-target-directory)
(make-variable-buffer-local 'dafft-target-strip-count)

(defun dafft-current-target ()
  "Return the filename and line range for the hunk containing point.
The result has the form (FILENAME START END)."
  (save-excursion
    (or (looking-at dafft-hunk-regexp)
	(re-search-backward dafft-hunk-regexp nil t)
	(error "can't find top of hunk containing point"))
    (let ((line-range (dafft-hunk-line-range)))
      (or (re-search-backward dafft-file-regexp nil t)
	  (error "can't find name of file to which current hunk applies"))
      (let ((filenames (dafft-hunk-filenames)))
	;; Use the shorter of the two filenames.  Most of the time, one
	;; or the other has a ".orig" or ".~4~" suffix.
	(cons (if (< (length (car filenames)) (length (nth 1 filenames)))
		  (car filenames) (nth 1 filenames))
	      line-range)))))

(defun dafft-default-filename (hunk-filename)
  "Guess a good target filename, given the filename from the current hunk.

We strip the first few path components from the filename given in the
hunk (the `dafft-target-strip-count' says how many), and then make the
result relative to `dafft-target-directory'.  (If the latter is nil,
we use the directory of the buffer containing the patch instead.)

After we've presented this guess to the user, and given her a chance
to edit it, we call `dafft-adjust-defaults' on what we got back.  That
function adjusts `dafft-target-directory' and `dafft-target-strip-count'
to the most conservative values that would have produced what the user
typed."

  ;; Remove the first few components from hunk-filename.  We could do
  ;; this with less string copying, but I don't care to at the moment.
  (let ((i dafft-target-strip-count))
    (while (and (> i 0) (string-match "/" hunk-filename))
      (setq hunk-filename (substring hunk-filename (match-end 0)))
      (setq i (1- i)))
      
    ;; Make it relative to dafft-target-directory, or the patch's
    ;; current directory.
    (expand-file-name hunk-filename (or dafft-target-directory
					default-directory))))

(defun dafft-adjust-defaults (hunk actual)
  "Adjust the target filename guessing machinery to do better next time.
HUNK is the filename from the current hunk; ACTUAL is the filename the
user says the hunk actually applies to."
  
  ;; How many path components do HUNK and ACTUAL have in common at the
  ;; end?  For example /home/scoobie/src/libguile/eval.c and
  ;; libguile/eval.c have two components in common.
  (let* ((common 0)
	 (strip-hunk hunk)		; Sounds fun!
	 (strip-actual actual)
	 hunk-dir actual-dir)
    (while (and (string= (file-name-nondirectory strip-hunk)
			 (file-name-nondirectory strip-actual))
		(progn
		  (setq common (1+ common))
		  (setq hunk-dir (file-name-directory strip-hunk)
		        actual-dir (file-name-directory strip-actual))
		  (and hunk-dir actual-dir)))
      (setq strip-hunk (directory-file-name hunk-dir)
	    strip-actual (directory-file-name actual-dir)))
    
    ;; Don't change anything unless they have something in common; if
    ;; not even the final components match, something weird's going on.
    (if (> common 0)
	(progn

	  ;; hunk-dir is now the non-common portion of the hunk path.
	  ;; The strip-count should be set to as many slashes as appear
	  ;; there.  Can you think of a better way to count the number
	  ;; of slashes in a string?
	  (let ((strip-count 0)
		(scan "/"))
	    (while (and hunk-dir (string-match scan hunk-dir))
	      (setq scan (concat scan ".*/")
		    strip-count (1+ strip-count)))
	    (setq dafft-target-strip-count strip-count))

	  ;; actual-dir is now the proper prefix for subsequent guesses.
	  (setq dafft-target-directory actual-dir)))))


;;;; Visiting the file to which a given patch hunk applies

(defun dafft-find-target (filename line)
  "Visit the file and location to which the current patch hunk applies."
  (interactive
   (let* ((target (dafft-current-target))
	  (guess (dafft-default-filename (car target)))
	  (actual (read-file-name "Find hunk target in: "
				  (file-name-directory guess)
				  guess t
				  (file-name-nondirectory guess))))
     (dafft-adjust-defaults (car target) actual)
     (list actual (car (cdr target)))))
  (find-file filename)
  (goto-line line))



;;;; Key bindings

(defvar dafft-mode-map (make-sparse-keymap))
(define-key dafft-mode-map "\C-c\C-f" 'dafft-next-hunk)
(define-key dafft-mode-map "\C-c\C-b" 'dafft-prev-hunk)
(define-key dafft-mode-map "\C-c\C-n" 'dafft-next-file)
(define-key dafft-mode-map "\C-c\C-p" 'dafft-prev-file)
(define-key dafft-mode-map "\C-c\C-t" 'dafft-find-target)

;;; Add an element to minor-mode-map-alist for dafft-mode.
(or (assq 'dafft-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'dafft-mode dafft-mode-map) minor-mode-map-alist)))


(provide 'dafft)
