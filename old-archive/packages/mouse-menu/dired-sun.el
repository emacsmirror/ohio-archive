;;;; A [file of] mouse function[s] for use in Dired mode under SunView.
;;;; Russell Ritchie, Scottish HCI Centre, <russell@uk.ac.strath.hci>.
;;;; Tue Jul 19 12:01:30 1988

(provide 'dired-sun)

(defun mouse-inspect-file (window x y &optional other-window-p)
  "Inspect the file the mouse is pointing at:
	if it ends in \".dvi\", use TeX-screen-print-command to show it
	otherwise do a dired-find-file on it.
If optional second arg OTHER-WINDOW-P is non-nil do it in another window." 
  (let ((point (point))			; Don't muck with POINT.
	(buffer (current-buffer)))
    (mouse-move-point window x y)
    (let ((filename (dired-get-filename)))
      (cond ((string-match "\.dvi$" filename) 
	     (let* ((preview-program (if (boundp 'TeX-screen-print-command) 
					 TeX-screen-print-command
				     (require 'TeX-mode)
				     TeX-screen-print-command))
		    (previewer
		     (if (string-match "[ \t\n]" preview-program)
			 (substring preview-program 0 (1- (match-end 0)))))
		    (previewer-explicit-args
		     (if previewer (substring preview-program (match-end 0)))))
	       (if previewer
		   (if (file-exists-path-p previewer exec-path)
		       ;; TeX-screen-print-command could contain
		       ;; options separated by whitespace, 
		       ;; check only up to the end of the first 'word'
		       ;; which *should* be the program name. 
		       (start-process
			previewer
			nil previewer previewer-explicit-args filename)
		     (error
		      "%s can't be found in the current env. Wrong $PATH?"
		      previewer))
		 (if (file-exists-path-p preview-program exec-path)
		     (start-process
		      preview-program nil preview-program filename)))))
	    (t (if (save-excursion
		     (beginning-of-line)
		     (looking-at "  d")) ; If it's a directory use dired on it.
		   (if other-window-p
		       (dired-other-window filename)
		     (dired filename))
		 (if other-window-p
		     (find-file-other-window filename)
		   (find-file filename))))))
    (set-buffer buffer)
    (goto-char point)))

(defun mouse-inspect-file-other-window (window x y)
  "Inspect the file the mouse is pointing at in another window:
       if it ends in \".dvi\", use TeX-screen-print-command to show it
       otherwise do a dired-find-file-other-window on it."
  (mouse-inspect-file window x y t))
