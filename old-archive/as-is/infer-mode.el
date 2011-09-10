;To: unix-emacs@bbn.com
;Date: 25 Jan 89 19:45:04 GMT
;From: Ashwin Ram <Ram-Ashwin@yale.ARPA>
;Subject: Re: mode by file contents
;
;In article <GHH.89Jan25110326@thought.princeton.edu>, ghh@thought.princeton.edu (Gilbert Harman) writes:
;> I would like to put something in my .emacs that would
;> determine the major mode of a buffer on the basis of the
;> contents of the file read into the buffer rather than on the
;> basis of something about the name of the file.
;> 
;> For example, if the file began with "\documentstyle... "
;> the buffer would be automatically put into latex mode.
;
;See if you like this.  To use, put this in your ~/.emacs file:
;
;    (setq find-file-hooks '(infer-file-mode))
;
;The function infer-file-mode uses infer-mode-alist (analogous to
;auto-mode-alist) to search for a regexp within the first infer-mode-limit
;characters (nil means search entire buffer).  In your example, you would use:
;
;    (setq infer-mode-alist '(("\\documentstyle" . latex-mode)))
;
;-----------------------------------------------------------------------------
;; INFER-MODE.EL -- Infer file mode based on contents.
;; Ashwin Ram, 11/5/88.

(defvar infer-mode-alist '(("\\documentstyle" . latex-mode))
   "Alist of file content patterns vs corresponding major mode functions.
Each element looks like (REGEXP . FUNCTION).
Visiting a file whose which contains REGEXP within the first infer-mode-limit
characters causes FUNCTION to be called.")

(defvar infer-mode-limit 1000
   "Number of characters to search when inferring mode based on file contents.
Nil means search entire buffer.  See infer-mode-alist.")

(defun infer-file-mode ()
   "Infer mode of file based on its contents.
To use, add this function to find-file-hooks.
To customize, see the variable infer-mode-alist."
   (let ((mode-alist infer-mode-alist))
      (catch 'found
         (while mode-alist
            (if (save-excursion
                   (goto-char (point-min))
                   (re-search-forward (caar mode-alist) infer-mode-limit t))
                (throw 'found t)
                (setq mode-alist (cdr mode-alist)))))
      (if mode-alist
          (funcall (cdar mode-alist)))))
;-----------------------------------------------------------------------------
;
;-- Ashwin.
;
;ARPA:    Ram-Ashwin@cs.yale.edu
;UUCP:    {decvax,ucbvax,harvard,cmcl2,...}!yale!Ram-Ashwin
;BITNET:  Ram@yalecs
