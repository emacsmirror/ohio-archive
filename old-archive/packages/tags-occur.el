; Date: Thu, 20 Jun 91 18:45:21 EDT
; From: macrakis@osf.org
; Subject: tags-occur
; 
; A few months ago, you expressed interest in a tags-occur package.
; Here it is.  It's brand new, but appears to work without problems.
; Let me know if you have any problems or comments.
; 	-s
; 
;; Doesn't take numlines arg (I never use it and it complicates implementation)
;; Unlike occur, doesn't use markers, so if buffers are modified
;; between running occur and looking up, it won't track the changes.

(defun tags-occur (regexp)
  "Show lines in the tags files containing a match for REGEXP.
The lines are shown in a buffer named *Occur*, which serves as a menu
to find occurences.  \\[describe-mode] in that buffer will explain how."
  (interactive "sList tag-file lines matching (regexp): ")
  (next-file t)
  (goto-char (point-min))
  (with-output-to-temp-buffer "*Occur*"
    (save-excursion
      (set-buffer standard-output)
      (insert "Lines matching ")
      (prin1 regexp)
      (insert " in tags files.\n")
      (tags-occur-mode))
    (while next-file-list
      (let ((curfile (buffer-file-name))
	    (curbuf (current-buffer)))
	(while (re-search-forward regexp (point-max) t)
	  (forward-line 0)
	  (let ((curline (count-lines (point-min) (point)))
		(beg (point))
		(end (progn (forward-line 1) (point))))
	    (save-excursion
	      (set-buffer standard-output)
	      (insert (format "%s:%d: " curfile curline))
	      (insert-buffer-substring curbuf beg end)))))
      (next-file)
      (goto-char (point-min)))))

(defvar tags-occur-mode-map ())

(if tags-occur-mode-map
    ()
  (setq tags-occur-mode-map (make-sparse-keymap))
  (define-key tags-occur-mode-map "\C-c\C-c" 'tags-occur-mode-goto-occurrence))

(defun tags-occur-mode ()
  "Major mode for output from \\[tags-occur].
Move point to one of the occurrences in this buffer,
then use \\[tags-occur-mode-goto-occurrence] to go to the same occurrence
in the buffer that the occurrences were found in.
\\{tags-occur-mode-map}"
  (kill-all-local-variables)
  (use-local-map tags-occur-mode-map)
  (setq major-mode 'tags-occur-mode)
  (setq mode-name "Tags Occur"))

(defun tags-occur-mode-goto-occurrence ()
  "Go to the line this occurrence was found in, in the buffer it was found in."
  (interactive)
  (forward-line 0)
  (if (not (looking-at "\\([^ \n]+\\):\\([0-9]+\\): "))
	       ;; Allows colons but not spaces in filenames; extra
	       ;; cost negligeable for interactive operation
      (error "Buffer not in Tags Occur format"))
  (let ((filename (buffer-substring (match-beginning 1) (match-end 1)))
	(lineno (string-to-int (buffer-substring (match-beginning 2) (match-end 2)))))
    (find-file filename)
    (push-mark)
    (goto-char (point-min))
    (forward-line lineno)))

