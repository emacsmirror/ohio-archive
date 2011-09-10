; Path: hal.com!olivea!uunet!pipex!demon!edscom!kevin
; From: kevin@edscom.demon.co.uk (Kevin Broadey)
; Newsgroups: gnu.emacs.sources
; Subject: Automatic file carbon-copying
; Date: 18 Nov 92 16:37:06 GMT
; Organization: EDS-Scicon, Milton Keynes, UK
; 
; A while ago Ray Nickson <Ray.Nickson@comp.vuw.ac.nz> posted
; carbon-file.el (that's what it says in my file header, anyway!).  This
; sits in your `write-file-hooks' and automatically writes your buffer
; contents to one or more `carbon-copy' files every time you save it.
; 
; I found this very useful, except that I often forgot to set up the
; carbon-copy file name.
; 
; To get around this I wrote auto-carbon.el which sits in your
; `find-file-hooks'.  It uses a regular expression to trap files which you
; may want to carbon-copy and a "replace-match" string to generate the
; copy file name.
; 
; This post includes carbon-file.el for those of you who haven't already
; got it.
; 
; Bug reports, suggestions, improvements and praise to:-
; 
; 	kbroadey@edscom.demon.co.uk

 
; ------------------------------------------------------------------------
;; auto-carbon.el - automatically call carbon-buffer-to-file from
;;                  find-file-hooks
;;
;; Written 04-Nov-92 by Kevin Broadey <kbroadey@edscom.demon.co.uk>

;; LCD Archive Entry:
;; auto-carbon|Kevin Broadey|kevin@edscom.demon.co.uk|
;; Automatically write buffer contents to "carbon copy" file on saves.|
;; 92-11-04||~/misc/auto-carbon.el.Z|

;;
;; Usage:
;;
;;    (require 'auto-carbon)
;;    (or (memq 'auto-carbon find-file-hooks)
;;        (setq find-file-hooks (cons 'auto-carbon find-file-hooks)))

(provide 'auto-carbon)

(defvar auto-carbon-alist nil
  "ALIST of source regexps and target patterns for automatic file carbon
copying.

Each element looks like  (SOURCE . TARGET)  where SOURCE is a regular
expression and TARGET is a  replace-match  compliant replacement string.
This means that \\1 in TARGET is replaced by the first \\( ... \\) expression
in SOURCE and \\& is replaced by the whole of SOURCE.

Note that SOURCE is not anchored by default, so you must use ^ and $ to
anchor the match to the beginning or end of the file name.")

(defun auto-carbon ()
  "Function for inclusion in `find-file-hooks' which uses `auto-carbon-alist'
to determine whether to carbon-copy a file.

Calls `carbon-buffer-to-file' to arrange for carbon-copying."
  (let ((alist auto-carbon-alist)
	(orig-buffer-file-name buffer-file-name)
	carbon-file-name)

    ;; Check whether buffer is visiting a file.  Error if not.
    (or buffer-file-name
	(error "Buffer is not visiting a file."))

    ;; Scan the alist looking for all matches
    (while alist
      (if (string-match (car (car alist)) orig-buffer-file-name)
	  ;; We've got a match.  Switch to a temporary buffer and use it to
	  ;; apply the target pattern to the source regexp using
	  ;; `replace-match'.  This does the "\&" and "\1" stuff for us.
	  ;; Let me know if you know of a version of replace-match that can be
	  ;; applied to a string!
	  (let ((orig-buf (current-buffer))
		(buf (get-buffer-create " *auto-carbon-scratchpad* ")))
	    (set-buffer buf)
	    (widen)
	    (erase-buffer)
	    (insert orig-buffer-file-name)
	    (goto-char (point-min))
	    (re-search-forward (car (car alist))) ; sets up match data
	    (replace-match (cdr (car alist)) t nil)
	    (setq carbon-file-name (buffer-substring (point-min) (point-max)))
	    (set-buffer orig-buf)
	    (kill-buffer buf)

	    ;; Ask whether to do the carbon copy.
	    ;; Note that we have to be back in the original buffer before we
	    ;; call carbon-buffer-to-file because it sets a buffer-local
	    ;; variable.
	    (if (y-or-n-p (format "Carbon copy to %s? " carbon-file-name))
		(carbon-buffer-to-file carbon-file-name)
	      )))

      ;; Try next element is alist.
      (setq alist (cdr alist)))))
------------------------------------------------------------------------
;;;carbon-file.el
;;;
;;;Authorizing-Users: Ray Nickson <Ray.Nickson@comp.vuw.ac.nz>

;;;To use, just M-x carbon-buffer-to-file to the remote file name when
;;;you find the local one (or vice versa).
;;;(I had to chamge it for distribution; hope it still works)

;;;You can also put the call in the file's Local Variables section with
;;;an eval, or just set buffer-carbon-file-names there.

(defvar buffer-carbon-file-names nil
  "List of files to carbon-copy this buffer into.")
(make-variable-buffer-local 'buffer-carbon-file-names)

(defun carbon-buffer-to-file (file)
  "Make FILE be a carbon-copy of the file visited by this buffer.
Any time you save the buffer, changes will go both to the buffer's own file
and to FILE.  Yes, you can carbon to many files at once; the list of files
being carbonned to is in the variable buffer-carbon-file-names."
  (interactive "FCarbon to file: ")
  (setq buffer-carbon-file-names (cons file buffer-carbon-file-names)))

(defun write-carbon-files ()
  "A write-file-hook.  See \\[carbon-buffer-to-file]."
  (save-restriction
    (widen)
    (mapcar
     (function (lambda (file)
       (write-region (point-min) (point-max) file)))
     buffer-carbon-file-names))
  nil) ; hook must return nil

(setq write-file-hooks (cons 'write-carbon-files write-file-hooks))
