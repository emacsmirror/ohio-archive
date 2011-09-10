;; FILE-COMPLETE.EL -- Display file name completions in mod-time order.
;; Copyright (c) 1989 Free Software Foundation, Inc.
;;
;; LCD Archive Entry:
;; file-complete|Ashwin Ram, Joe Wells|Ran-Ashwin@cs.yale.edu|
;; Display file name completions in mod-time order.|
;; 1992-10-13||~/misc/file-complete.el.Z|
;;
;; This file is not part of the GNU Emacs distribution (yet).
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.
;;
;; Everyone is granted permission to copy, modify and redistribute
;; this file, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.
;;
;; Comments, corrections, and improvements should be sent to:
;;
;;     Ashwin Ram
;;
;;     ARPA:   Ram-Ashwin@cs.yale.edu
;;     UUCP:   {decvax,ucbvax,harvard,cmcl2,...}!yale!Ram-Ashwin
;;     BITNET: Ram@yalecs
;;
;;
;; MODIFICATION HISTORY:
;;
;; 03/13/89 Ashwin Ram <Ram-Ashwin@cs.yale.edu>
;;          Initial release.
;;
;; 03/18/89 Joe Wells <jbw%bucsf.bu.edu@bu-it.bu.edu>
;;          Optimized sort, did temp-minibuf-message, etc.
;;
;; 03/21/89 Joe Wells
;;          optimized sort even more by using sh -c "ls name*"
;;
;; 03/22/89 Joe Wells
;;          more sort optimization, very fast now
;;
;; Oct-13-1992 Joe Wells
;;          "fixed" temp-minibuf-message so it works with 18.57+
;;
;; DOCUMENTATION:
;;
;; Display file name completions in order of modification date instead
;; of alphabetically.
;;
;; For the curious, when doing a completing read for a filename,
;; minibuffer-completion-table is read-file-name-internal and
;; minibuffer-completion-predicate is the current-buffer's directory.


(defun file-name-completion-help ()
  "Display a list of possible completions of the current minibuffer
contents.  If the minibuffer is completing filenames, print the list
in file modification time order.  Should only be called from inside a
completing read in the minibuffer."
  (interactive)
  ;; if not completing filenames, do normal behavior
  (if (not (eq minibuffer-completion-table 'read-file-name-internal))
      (minibuffer-completion-help)
    (message "Making completion list...")
    (let* ((buffer-string (buffer-string)) ; minibuffer contents
	   (string-dir	                ; directory part of minibuf contents
	    (file-name-directory
	     (substitute-in-file-name buffer-string)))
	   (real-dir	                ; directory for completion
	    (if string-dir
		(expand-file-name
		 string-dir
		 minibuffer-completion-predicate)
	      minibuffer-completion-predicate))
	   (completions
	    (all-completions buffer-string
			     minibuffer-completion-table
			     minibuffer-completion-predicate)))
      (cond (completions
	     (with-output-to-temp-buffer " *Completions*"
	       (display-completion-list
		(sort-files-by-modtime completions real-dir)))
	     (temp-minibuf-message "")) ;clear message
	    (t
	     (ding)
	     (temp-minibuf-message " [No completions]"))))))

(defun minibuffer-completion-help-unsorted ()
  "Display a list of possible completions of the current minibuffer
contents.  Prints the list in the order that it is returned from
all-completions (unsorted).  Should only be called from inside a
completing read in the minibuffer."
  (interactive)
  (message "Making completion list...")
  (let ((completions
         (all-completions (buffer-string)
                          minibuffer-completion-table
                          minibuffer-completion-predicate)))
    (cond (completions
           (with-output-to-temp-buffer " *Completions*"
             (display-completion-list completions))
	   (temp-minibuf-message ""))	;clear message
          (t
           (ding)
           (temp-minibuf-message " [No completions]")))))

(defun sort-files-by-modtime (files &optional dir)
  "Sort a list of FILES by the files' modification times.  Optional
argument DIR is the directory the files are located in, which defaults
to the default-directory of the current buffer.  This is not an
in-place sort.  Deletes any files named . or .. from the list.
Returns the new head of the list.  Note the items returned are new
strings.  None of the previous members of the list are returned."
  (let* ((real-dir (file-name-as-directory
		    (expand-file-name (or dir default-directory))))
	 (buffer (get-buffer-create " *ls results*"))
	 (p files))
    ;; remove trailing slashes and delete "." and ".."
    (setq files nil)
    (while (consp p)
      (cond ((string-match "\\`\\.\\.?/\\'" (car p))) ;ignore . and ..
	    ((string-match "/\\'" (car p)) ;trailing backslashes
	     (setq files (cons (substring (car p) 0 -1) files)))
	    (t
	     (setq files (cons (car p) files))))
      (setq p (cdr p)))
    ;; we're going to collect the output of the ls -t command in a
    ;; buffer and put that back into a list.
    (save-excursion
      (set-buffer buffer)
      ;; run ls in correct directory
      (if (not (file-directory-p real-dir))
	  (error "%s is not a directory" real-dir)
	(setq default-directory real-dir))
      (erase-buffer)
      ;; ls options:
      ;; 1 - one column
      ;; d - list a directories name, not its contents
      ;; t - sorted by time
      ;; F - fancy listing (* for executable, / for directory, etc.)
      ;; L - use stat not lstat
      (apply 'call-process "/bin/ls" nil t nil "-1dtFL" files)
      ;; grab the filenames from the ls output
      (goto-char (point-min))
      (setq files nil)
      (while (not (eobp))
	(skip-chars-forward "\n")
	(let ((begin (point)))
	  (skip-chars-forward "^\n")
	  (setq files (cons (buffer-substring begin (point)) files)))
	(skip-chars-forward "\n"))
      ;; files are now in reverse order
      (setq files (nreverse files))
      ;; save memory
      (erase-buffer)
      files)))

;; Too bad this isn't in src/minibuf.c:
;;
;; DEFUN ("temp-minibuf-message", Ftemp_minibuf_message, Stemp_minibuf_message,1, 1, 0,
;;   "Documentation.")
;;   (s)
;;      Lisp_Object s;
;; {
;;   CHECK_STRING (s);
;;   temp_minibuf_message (XSTRING(s)->data);
;;   return Qnil;
;; }
;;
;; defsubr (&Stemp_minibuf_message);

(defun temp-minibuf-message2 (m)
  "Prints string MESSAGE in the current buffer to the right of all
text in the buffer.  It is used mainly for putting messages in the
minibuffer while also showing the minibuffer text."
  (let ((osize (point-max))
        (inhibit-quit t))
    (save-excursion
      (goto-char osize)
      (insert m)
      (goto-char osize)
      ;; The next statement is a gross hack.
      ;; The purpose is to set echo_area_contents = 0, so that the contents
      ;; of the minibuffer will show.
      ;; *** fix this so it doesn't assume RET exits minibuffer
      (let ((unread-command-char ?\C-m))
	(read-from-minibuffer "" nil nil nil))
      ;; This sets prev_echo_area_contents = echo_area_contents (which is 0)
      ;; *** fix this so it doesn't assume RET exits minibuffer
      (let ((unread-command-char help-char)
	    (help-form '(setq unread-command-char ?\C-m)))
	(read-key-sequence nil))
      (sit-for 2)
      (delete-region osize (point-max))
      (if quit-flag
	  (setq quit-flag nil
		unread-command-char ?\C-g)))))

;; Check if temp-minibuf-message has been fixed in the C code.
(or (and (fboundp 'temp-minibuf-message)
	 (subrp (symbol-function 'temp-minibuf-message)))
    (fset 'temp-minibuf-message 'temp-minibuf-message2))

;;(define-key minibuffer-local-completion-map "|" 'file-name-completion-help)
;;(define-key minibuffer-local-completion-map "=" 'minibuffer-completion-help-unsorted)

(provide 'file-complete)

;; Avoid calling stat() more than once per file, at the expense of
;; some extra consing in file-attributes.  That's ok, because the
;; consing is O(n) and the stats were O(n lg n).  We grab the file
;; modtime and put it in the list with the filename.  Then we call
;; sort with a predicate that compares the modtimes.  The modtimes are
;; in this format: (HIGH LOW) where HIGH and LOW are 16 bit integers.
;; During the sort, the list is in this format: ((FILENAME HIGH LOW)
;; ...).  This sort occurs in place.
;; (defun sort-files-by-modtime (files &optional dir)
;;   "Sort a list of FILES by the files' modification times.  Optional
;; argument DIR is the directory the files are located in, which defaults
;; to the default-directory of the current buffer.  This is not an
;; in-place sort.  Deletes any files named . or .. from the list.
;; Returns the new head of the list."
;;   (let ((p files)
;;         time1 time2)
;;     (setq files nil)
;;     (while (consp p)
;;       (if (string-match "\\`\\.\\.?/\\'" (car p))
;; 	  nil
;; 	(setq files (cons
;; 		     (cons (car p)
;; 			   (nth 5 (file-attributes
;; 				   (expand-file-name (car p) dir))))
;; 		     files)))
;;       (setq p (cdr p)))
;;     (setq files
;;           (sort files
;;                 (function
;;                  (lambda (f1 f2)
;;                    (setq time1 (cdr f1)
;;                          time2 (cdr f2))
;;                    (or (> (car time1) (car time2))
;;                        (and (= (car time1) (car time2))
;;                             (> (car (cdr time1)) (car (cdr time2)))))))))
;;     (setq p files)
;;     (while (consp p)
;;       (setcar p (car (car p)))
;;       (setq p (cdr p))))
;;   files)
