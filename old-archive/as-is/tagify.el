;To: unix-emacs@BBN.COM
;Date: 16 Jun 89 15:54:00 GMT
;From: csd4.milw.wisc.edu!uxc.cso.uiuc.edu!uxc.cso.uiuc.edu!m.cs.uiuc.edu!liberte@BBN.COM
;Sender: arpa-unix-emacs-request@BBN.COM
;Subject: Re: Is there a fix for "File tags not a
;References: <380001@hpfcdq.HP.COM>
;Source-Info:  From (or Sender) name not authenticated.
;
;
;I needed a texinfo TAGS file, and I looked at etags.c to consider
;extending it, briefly.  Instead, I wrote tagify.el.  Send me your
;extensions and I will probably add them.
;
;Dan LaLiberte
;uiucdcs!liberte
;liberte@cs.uiuc.edu
;liberte%a.cs.uiuc.edu@uiucvmd.bitnet
;
;---------------
;; tagify.el  - make TAGS files.
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
;; To build a TAGS file for a set of files, first make sure there is a
;; make-tag function associated with the major mode for each file.
;; These are stored in tagify-mode-alist. Then call tagify-files.  You
;; will be asked for a list of file names and the TAGS file.  The TAGS
;; file is saved for you after it is created.  If a major-mode does
;; not have a make-tag function, you will be asked to name one that
;; will be used for the remainder of the files.

;; To update the TAGS file, use retagify-files.  You are asked to
;; specify the TAGS file.  retagify-files will then regenerate
;; tags for just those files in the TAGS file that are newer than the TAGS
;; file.

;; Daniel LaLiberte
;; uiucdcs!liberte
;; liberte@cs.uiuc.edu
;; liberte%a.cs.uiuc.edu@uiucvmd.bitnet

;; Need:
;; add-file-tags filenames
;; delete-file-tags filenames

(provide 'tagify)

(defconst tagify-mode-alist '
  ((texinfo-mode . make-texinfo-tag)
   (c-mode . make-c-tag)
   (emacs-lisp-mode . make-elisp-tag)
   )
  "Association list between major modes and tag matching functions.
The function should find the next tag and return it, or return nil if
there are no more tags for the current buffer.  The tag is only used
to sort the entries for each file.  The function should leave point
after the tag.")

;; -----------------------------------------------
;; Example make-tag functions

(defun make-texinfo-tag ()
  "Function to make next texinfo tag."
  (if (re-search-forward
       "^@def\\(un\\|var\\|opt\\|const\\|cmd\\|spec\\) \\([^ \n]+\\) ?\\|^@node \\([^,]+\\)" nil t)
      (if (match-beginning 2)
	  (buffer-substring (match-beginning 2)
			    (match-end 2))
	;; @node found
	(buffer-substring (match-beginning 3)
			  (match-end 3)))
    ))

(defun make-info-tag ()
  "Function to make next tag for an info file."
  (if (re-search-forward
       "^\\* \\(Function\\|Command\\|Macro\\|Special form\\|Variable\\|Option\\|Constant\\): \\([^ \n]+\\) ?" nil t)
      (buffer-substring (match-beginning 2)
			(match-end 2))
    ))

(defun make-elisp-tag ()
  "Function to make the next tag for an elisp file."
  (if (re-search-forward
       "^ ?(def\\(un\\|var\\|const\\) \\([^ \n]+\\) ?" nil t)
      (buffer-substring (match-beginning 2)
			(match-end 2))
    ))
  
(defun make-c-tag-not-done ()
  "Function to make next c tag."
  ;; #defines are the easy part
  ;; I dont know how to match function declarations
  (if (re-search-forward
       "^[ \t]*#define[ \t]+\\([a-zA-Z_]+\\)")
      (buffer-substring (match-beginning 1)
			(match-end 1))))

;;-----------------------------------------------------------

(defun find-tags-table (file)
    "Tell tags commands to use tag table file FILE.
FILE should be the name of a file created with the `etags' program,
or tagify-files, or it may be a new file.
A directory name is ok too; it means file TAGS in that directory."
  (interating tags file, if any, is replaced.
FILENAMES are files to search for tags.  The FILENAMES argument may
actually specify several files, with shell wildcards, e.g. \"*.c *.h\".
Paths may be either absolute or relative to the current directory.
How to make tags is determined from tagify-mode-alist and the major mode
of each file."

  (interactive "sMake TAGS for files: ")
  (let (TAGS-buffer
	tag-entries
	file
	(file-list (files-matching filenames))
	tagifier
	(local-tagify-alist tagify-mode-aliting tags file, if any, is replaced.
FILENAMES are files to search for tags.  The FILENAMES argument may
actually specify several files, with shell wildcards, e.g. \"*.c *.h\".
Paths may be either absolute or relative to the current directory.
How to make tags is determined from tagify-mode-alist and the major mode
of each file."

  (interactive "sMake TAGS for files: ")
  (let (TAGS-buffer
	tag-entries
	file
	(file-list (files-matching filenames))
	tagifier
	(local-tagify-alist tagify-mode-alist)  ; may be added to
	)

    (save-excursion
      ;; find and erase the TAGS file
      (call-interactively find-tags-table-hook)
      (set-buffer (or (get-file-buffer tags-file-name)
		      (progn
			(setq tag-table-files nil)
			(find-file-noselect tags-file-name))))
      (setq TAGS-buffer (current-buffer))
      (erase-buffer)  ;; to add new files, just dont erase

      ;; tagify each file
      (while file-list
	(setq file (car file-list))
	(setq file-list (cdr file-list))

	(save-excursion
	  (set-buffer 
	   (find-file-noselect file))
	  (message "Tagify: %s" file)
	  (if (setq tagifier (cdr (assq major-mode local-tagify-alist)))
	      nil
	    (if (setq tagifier (tagify-read-tagifier))
		(setq local-tagify-alist
		      (cons (cons major-mode tagifier) local-tagify-alist))
	      (error "Abort tagifying.")
	      ))
	  (setq tag-entries (tagify-current-buffer tagifier)))

	(finish-tagging-file file tag-entries TAGS-buffer)
	)

      ;; finish off by writing the TAGS file
      (message "Saving %s" tags-file-name)
      (save-excursion
	(set-buffer TAGS-buffer)
	(write-file tags-file-name))
      )))


(defun retagify-files ()
  "Update the TAGS file replacing entries only for files that
have changed since the TAGS file was saved."
;; try combining this with tagify-files
  (interactive)
  (let (TAGS-buffer
	file
	tag-entries
	startpt size
	tagifier
	(local-tagify-alist tagify-mode-alist)  ; may be added to
	)

    (save-some-buffers)
    (save-excursion
      ;; find the TAGS file
      (call-interactively find-tags-table-hook)
      (visit-tags-table-buffer)
      (setq TAGS-buffer (current-buffer))
      (goto-char (point-min))

      ;; check each file
      (while (not (eobp))
	;; get the next file name
	(forward-line 1)
	(end-of-line)
	(skip-chars-backward "^,\n")
	(save-excursion (setq size (read (current-buffer))))
	(setq file (buffer-substring (1- (point))
				     (progn (beginning-of-line) (point))))
	(setq startpt (- (point) 2))  ; before leading \^L
	(forward-line 1)
	(forward-char size) ; at end of tags for file

	(if (file-newer-than-file-p file tags-file-name)
	    (progn
	      ;; strip out old entries for this file
	      (delete-region startpt (point))

	      (save-excursion
		(set-buffer 
		 (find-file-noselect file))
		(message "Retagify: %s" file)
		(if (setq tagifier (cdr (assq major-mode local-tagify-alist)))
		    nil
		  (if (setq tagifier (tagify-read-tagifier))
		      (setq local-tagify-alist
			    (cons (cons major-mode tagifier)
				  local-tagify-alist))
		    (error "Abort tagifying.")
		    ))
		(setq tag-entries (tagify-current-buffer tagifier)))
	      
	      (finish-tagging-file file tag-entries TAGS-buffer)
	      )))

      ;; finish off by writing the TAGS file
      (message "Saving %s" tags-file-name)
      (save-excursion
	(set-buffer TAGS-buffer)
	(write-file tags-file-name))
      )))


(defun tagify-read-tagifier ()
  "Read a tagifier function for the current mode."
  (let ((name
	 (completing-read (format "Specify tagifier for %s (RET to exit): "
				  major-mode)
			  obarray 'fboundp 'match)))
    (if (> (length name) 0)
	(intern name)
      nil)))


(defun tagify-current-buffer (make-tag)
  "Return the list of tag entries for the current buffer.
Tags are found with the MAKE-TAG function.
See tagify-mode-alist."
  (let ((tag-entries nil)
	(last-line 1)
	(last-char 1)
	tag text line char)
    (save-excursion
      (goto-char (point-min))
      
      ;; find all tags in the file
      (while (setq tag (funcall make-tag))
	
	(save-excursion
	  (setq text
		(buffer-substring
		 (point)
		 (progn
		   (beginning-of-line)
		   (point))))
	  (setq line (+ last-line
			(count-lines last-char (point))))
	  (setq char (point))
	  (setq last-line line)
	  (setq last-char char)
	  (setq tag-entries
		(cons (list tag text line char) tag-entries))
	  )))
    tag-entries
    ))



;;---------------------------------------------------
;; Format of a TAGS file

;; A TAGS file consists of a sequence of file sections.
;; Each file section begins with \014 (^L).
;; The file name may be relative to the directory that the
;; TAGS file is in.  The file name is followed by ",n", where
;; n is the length, in chars, of the tag entry lines for this file.

;; \014
;; filename,n
;; <n chars of entries>
;; <including newlines>
;; \014
;; nextfile ...

;; Each entry is the text at the start of the tagged line,
;; including the text of the tag itself.
;; Following that is "\177n,m", where n is the line number
;; of the tagged line and m is the character number of the
;; newline before the tagged line.  Exact numbers do not matter
;; since find-tag looks in the neighborhood.

;; examples:
;; text of line before tag\17713,400
;; text of another line before tag\1775,30

;; The entries are sorted by the tags of all entries for each file.
;;-----------------------------------------------------------------

(defun finish-tagging-file (filename tag-entries TAGS-buffer)
  "Finish tagging FILENAME by inserting all TAG-ENTRIES to the TAGS-BUFFER.
Insertion is at point and may be in middle of buffer."
  ;; could make it more general by just returning a string that may be
  ;; inserted anywhere
  (setq tag-entries
	(sort tag-entries
	      (function (lambda (e1 e2)
			  (string< (car e1) (car e2))))))
  (let (startp endp entry length)
    (set-buffer TAGS-buffer)
    (insert ?\^L ?\n filename ?\n)
    (setq startpt (point))
    ;; insert the tag entries
    (while tag-entries
      (setq entry (car tag-entries))
      (insert (nth 1 entry) ?\177
	      (format "%d,%d\n"
		      (nth 2 entry)
		      (nth 3 entry)))
      (setq tag-entries (cdr tag-entries)))
    (setq endpt (point))
    (goto-char (1- startpt))		; end of filename line
    (setq length (format ",%d" (- endpt startpt)))
    (insert length)
    (goto-char (+ endpt (length length)))
    ))


;;--------------------------------------------------
;; Handy utility section

(defun files-matching (&rest filenames)
  "Return a list of files matching FILENAMES.
FILENAMES may include shell wildcards.
ls is used."
  (interactive "sFilenames: ")
  (let (filename
	file-list)
    (save-excursion
      (with-output-to-temp-buffer "*Directory*"
	(buffer-flush-undo standard-output)
	(funcall 'call-process (getenv "SHELL")
		 nil standard-output nil "-c"
		 (concat "ls " (mapconcat 'identity filenames " ")))

	(set-buffer "*Directory*")
	(goto-char (point-min))
	(while (not (eobp))
	  (setq file-list (cons
			   (buffer-substring (point)
					     (progn (end-of-line) (point)))
			   file-list))
	  (if (not (eobp))
	      (forward-char 1))))
      (kill-buffer "*Directory*"))
    (nreverse file-list)))

