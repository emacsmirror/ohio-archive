;From: julian@uhccux.uhcc.hawaii.edu (Julian Cowley)
;Date: 14 Jun 89 04:43:47 GMT
;Organization: University of Hawaii at Manoa

(defun kill-buffer-undo-auto-save (buf)
  "One arg, a string or a buffer.  Get rid of the specified buffer.
If the buffer is associated with a file and an auto-save file exists,
the auto-save file is deleted."
  (interactive "bKill buffer: ")
  (save-excursion
    (set-buffer buf)
    (let ((auto-save-file buffer-auto-save-file-name))
      (kill-buffer buf)
      (and (not (get-buffer buf))
	   auto-save-file
	   delete-auto-save-files
	   (condition-case ()
	       (delete-file auto-save-file)
	     (file-error nil))))))
