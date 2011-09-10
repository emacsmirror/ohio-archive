;; LCD Archive Entry:
;; complete-fn|Juergen Nickelsen|nickel@cs.tu-berlin.de|
;; File name completion in a buffer.|
;; 1993-02-25|1.1|~/functions/complete-fn.el.Z|


; complete-fn.el implements completion of a file name which is typed in
; a buffer. If you really like to type file names like
; 
; /home/stone/shape/development/src/atfs/conf/s-ultrix_4/AtFS/Data/config.h
; 
; you don't need it. (This is a real world example!)
; 
; I have complete-file-name bound to C-c TAB, and when I realize that I
; am just, typing a monster like the example above, I can type somewhere
; in the middle C-c TAB and finish typing the file name in the
; minibuffer with the usual completion.
; 
; If you invoke it, it grabs a string from your current buffer that is
; surrounded by the following characters: ]\n\t |&;:\$*?()['"`<> 
; This string is then used as initial input for the minibuffer.  (If you
; *do* use these characters in file names, change the value of
; file-name-boundary-regexp.)
; 
; Sometimes a file name occurs next to a character that could belong to
; a file name, but does not, e.g. in Makefiles:
; 
;     INCLUDEPATH=/usr/local/include
; 
; If you have already typed
; 
;     INCLUDEPATH=/usr/loca
; 
; and realize that you want to use complete-file-name, you can go back
; to the first "/", set the mark, and give a single C-u prefix-argument
; to complete-file-name. The string is then grabbed from the buffer
; beginning at the mark. (Or should I make it at the point?)
; 
; If you don't want a string grabbed from the buffer at all, type C-u
; twice before invoking complete-file-name.
; 
; All other prefix arguments are silently ignored.
; 
; 
; Some time a ago I posted a first version of this function, which had
; a bug: The completion said "complete, but not unique" even when the
; file name *was* unique -- I had not really understood how completion
; using completing-read worked. Handling of the prefix arguments was
; also different then.
; 
; Have fun! I appreciate any comments.
; 
;;; $Header: complete-fn.el[1.1] Thu Feb 25 23:53:22 1993 nickel@cs.tu-berlin.de saved $
;;; Author: nickel@cs.tu-berlin.de
;;; complete-fn.el
;;; The command complete-filename lets you use filename completion on
;;; a filename the cursor is at or after in a text buffer.
;;; complete-filename tries to guess what text in the buffer already
;;; belongs to a filename. file-name-boundary-regexp is used to find
;;; possible delimiters of the filename.

(defvar file-name-boundary-regexp "[]\n\t |&;:\\\\\\$*?()['\"`<>]"
  "Regexp matching characters that are supposed not to occur in filenames.")

(defun complete-file-name (arg)
  "Complete filename point is at or after using the minibuffer.
With one C-u, use the value of mark as the beginning of the filename.
With two C-u's, don't grab an initial string from the buffer.
Any other prefix argument is ignored."
  (interactive "P")
  (let* ((grab-from (if (equal arg '(4))
			(mark-marker)))
	 (dont-grab (equal arg '(16)))
	 (file-name
	  (if dont-grab
	      ""
	    (grab-file-name nil grab-from)))
	 (template (complete-file-name-internal file-name nil nil 'use-it))
	 (completion (completing-read "Complete Filename: "
				      'complete-file-name-internal
				      nil nil template)))
    (if (equal file-name completion)
	(message "No completion")
      (or dont-grab (grab-file-name 'delete grab-from))
      (insert completion))))


(defun complete-file-name-internal (file-name pred flag &optional use-it)
  "Internal subroutine for complete-file-name. Do not call this."
  (let ((dir (file-name-directory file-name))
	(nondir (file-name-nondirectory file-name)))
    (cond ((null flag)
	   (let ((completion (file-name-completion nondir (or dir ""))))
	     (if (stringp completion)
		 (concat dir completion)
	       (if use-it
		   file-name
		 completion))))
	  ((eq flag t)
	   (file-name-all-completions nondir (or dir "")))
	  ((eq flag 'lambda)
	   (file-name-all-completions nondir (or dir ""))))))


(defun grab-file-name (delete begin)
  "Return filename point is at or after. If DELETE is non-nil, delete
the filename from the buffer. If BEGIN is non-nil, this is the
beginning of the filename."
  (save-excursion
    (if (and (or (eobp) (looking-at file-name-boundary-regexp))
	     (or (bobp) (string-match file-name-boundary-regexp
				      (buffer-substring (1- (point))
							(point)))))
	""
      (let* ((fnbeg (if begin
			begin
		      (save-excursion
			(re-search-backward file-name-boundary-regexp
					    (point-min) 'just-move)
			(if (looking-at file-name-boundary-regexp)
			    (forward-char 1))
			(point))))
	     (fnend (progn (re-search-forward file-name-boundary-regexp
					      (point-max) 'just-move)
			   (or (eobp) (forward-char -1))
			   (point))))
	(prog1
	    (buffer-substring fnbeg fnend)
	  (if delete
	      (delete-region fnbeg fnend)))))))
