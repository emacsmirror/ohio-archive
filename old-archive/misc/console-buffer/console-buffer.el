;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; console-buffer.el
;;; put your system console in a buffer
;;;
;;; Ben Mesander <ben@piglet.cr.usgs.gov>
;;; Noah Freidman <friedman@gnu.ai.mit.edu>
;;;
;;; this file is part of:
;;; brrcrftp.cr.usgs.gov:/pub/ben/console-buffer.tar.gz
;;;
;;; NO COPYRIGHT, NO WARRANTY
;;;
;;; $Header: /home/ben/src/console-buffer/RCS/console-buffer.el,v 1.3 1993/09/30 01:32:58 ben Exp $
;;;
;;; $Log: console-buffer.el,v $
; Revision 1.3  1993/09/30  01:32:58  ben
; Added documentation and autoload cookies.
;
; Revision 1.2  1993/09/26  15:16:46  ben
; Minor comment changes
;
; Revision 1.1  1993/09/26  00:45:02  ben
; Initial Revision
;
;;;
;;; Instructions for use
;;;
;;; (1) Compile the C program emacs-console.c and place it somewhere
;;; in your path, possibly ~/bin. Uncomment one of the console
;;; redirection methods defined in it. If you port this program to
;;; another os, please send ben@piglet.cr.usgs.gov mail so I can
;;; include it in the next release.
;;;
;;; (2) Add this file to a directory in your emacs load path. If you
;;; do not have a place to put emacs lisp libraries, this is one way
;;; to do it:
;;; 
;;; mkdir ~/lib/elisp
;;;
;;; then add in your .emacs file:
;;;
;;; (setq load-path (cons (expand-file-name "~/lib/elisp") load-path))
;;;
;;; (3) Byte compile this file for speed. To do this, start up emacs
;;; and then do:
;;;
;;; M-x byte-compile-file ~/lib/elisp/console-buffer.el
;;;
;;; (4) Add a line in your .emacs file 
;;; 
;;; (autoload 'console-buffer "console-buffer"
;;;   "Put your system console in a buffer." t)
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defvar console-buffer-name "*console*"
  "*Basic buffer name for console.")

;;;###autoload
(defun console-buffer ()
  "Put your system console in a buffer."
  (interactive)
  (let ((process-connection-type t)
	(console-buffer (generate-new-buffer console-buffer-name)))
    (switch-to-buffer console-buffer)
    (set (make-local-variable 'console-process)
	  (start-process 
	   "console-process" 
	   console-buffer
	   "emacs-console")))
  (set-marker (process-mark console-process) (point-max))
  (set-process-filter console-process 'console-filter))

;;; for a good time, email tower@gnu.ai.mit.edu and ask to be added
;;; to emacs-sex-l
(defun console-filter (proc string)
  (let (proc-mark region-begin window)
    (save-excursion
      (set-buffer (process-buffer proc))
      (setq proc-mark (process-mark proc)
	    region-begin (point)
	    window (and (/= 0 scroll-step)
			(get-buffer-window (current-buffer))))
      (goto-char proc-mark)
      (insert-before-markers string)
      (goto-char region-begin)
      (while (search-forward "\C-m" proc-mark 'goto-end)
	(delete-char -1)))
      (and window
	   (>= (window-start window) region-begin)
	   (set-window-start window region-begin 'noforce))))

;;; end of console-buffer.el
