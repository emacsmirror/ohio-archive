;;;; A [file of] X mouse function[s] for use in Dired mode [, more later..].
;;;; Russell Ritchie, Scottish HCI Centre, <russell@uk.ac.strath.hci>.
;;;; Tue Jul 19 12:01:30 1988 

(provide 'dired-x)

(defun x-mouse-inspect-file (arg &optional other-window-p)
  "Inspect the file the mouse is pointing at:
       if it ends in \".dvi\", use TeX-screen-print-command to show it
       otherwise do a dired-find-file on it.
If optional second arg OTHER-WINDOW-P is non-nil do it in another window."
  (eval-in-window
    x-mouse-window
    (x-mouse-set-point arg)
    (let ((filename (dired-get-filename)))
      (cond
       ((string-match "\.dvi$" filename) 
	(let* ((preview-program (if (boundp 'TeX-screen-print-command) 
				    TeX-screen-print-command
				  ;; Load TeX-mode to bind TeX-screen-print-command
				  (require 'TeX-mode)
				  TeX-screen-print-command))
	       (preview-prog-and-args
		(let ((program-and-args nil)
		      (result nil))
		  (while (string-match "[ \t\n]" preview-program)
		    (setq program-and-args
			  (cons 
			   (substring preview-program 0 (1- (match-end 0))) program-and-args))
		    (setq preview-program (substring preview-program (match-end 0))))
		  (setq program-and-args
			(nreverse (if (string= "" preview-program)
				      program-and-args
				    (cons preview-program program-and-args))))
		  (if (string= "" (car program-and-args))
		      (cdr program-and-args)
		    program-and-args)))
	       (previewer (car preview-prog-and-args)))
	  (if previewer
	      (if (file-exists-path-p previewer exec-path)
		  ;; TeX-screen-print-command could contain options separated by whitespace,
		  ;; check only up to the end of the first 'word' which *should* be
		  ;; the program name.
		  (apply 
		   'start-process
		   previewer nil previewer (append (cdr preview-prog-and-args) (list filename)))
		(error
		 "%s cannot be found in the current environment. Wrong $PATH?" previewer))
	    (if (file-exists-path-p preview-program exec-path)
		(start-process preview-program nil preview-program filename)))))
       (t (if (save-excursion
		(beginning-of-line)
		(looking-at "  d"))	; If it's a directory use dired on it.
	      (if other-window-p
		  (dired-other-window filename)
		(dired filename))
	    (if other-window-p
		(find-file-other-window filename)
	      (find-file filename))))))))

(defun x-mouse-inspect-file-other-window (arg)
  "Inspect the file the mouse is pointing at in another window:
       if it ends in \".dvi\", use TeX-screen-print-command to show it
       otherwise do a dired-find-file-other-window on it."
  (x-mouse-inspect-file arg t))