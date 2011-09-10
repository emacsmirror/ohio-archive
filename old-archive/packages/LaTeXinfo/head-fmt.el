(require 'latexinfo)

(defvar *index* nil)
(defvar *latexinfo-index-file* nil)

(defun head-fmt-hook ()
  (save-excursion
    (setq *index* nil)
    (setq *latexinfo-index-file* nil)
    (goto-char (point-min))
    (search-forward "\\latexinfoindexname")
    (skip-chars-forward " 	{")
    (setq *latexinfo-index-file*
	  (expand-file-name
	   (buffer-substring 
	    (point)
	    (progn
	      (skip-chars-forward "^ 	}\n")
	      (point))))))
  )

(load-library "informat")
;; We replace Info-split because we can't split and keep the index entries
;; the same.
(defun Info-split ()
  (interactive)
  (let ((lastmessage "Making tags table for Info file..."))
    (message lastmessage)
    (Info-tagify)
    (message
     (setq lastmessage "Making index for Info file..."))
    (make-head-index)
    (goto-char (point-min))
    (replace-string "\b" " ")
    (message (concat lastmessage
		     (if (interactive-p)
			 "done.  Now save it." "done."))))
  )

(defun make-head-index ()
  (let ((entries  (mapcar 'find-entry *index*)))
    (save-excursion
      (set-buffer (head-find-buffer *latexinfo-index-file*))
      (erase-buffer)
      (mapcar 'add-entry  entries)
      (insert "\n")
      (goto-char (point-min))
      (mapcar 'add-byte *index*)
      (sort-lines nil (point-min) (point-max))
      (goto-char (point-min))
      (delete-char 1)
      (setq buffer-file-name (expand-file-name *latexinfo-index-file*))
      (save-buffer)
      (bury-buffer (buffer-name))))
  (goto-char (point-min)))

(defun find-entry (byte)
  (goto-char byte)
  (buffer-substring byte
		    (progn
		      (skip-chars-forward "^\n\b(")
		      (point))))

(defun add-entry (entry)
  (insert
   ?\" entry ?\"
   "\n"))

(defun add-byte (byte)
  (end-of-line)
  (insert (concat " " byte))
  (forward-char 1))

(defun head-find-buffer (filename)
  (let ((blist (buffer-list))
	found)
    (while blist
      (save-excursion
        (set-buffer (car blist))
	(if (and (equal (buffer-name) (file-name-nondirectory filename))
		 (eq major-mode 'fundamental-mode))
	    (setq found (car blist)
		  blist nil)
	  (setq blist (cdr blist)))))
    (or found
	(create-file-buffer filename))))

(put 'setindexname 'latexinfo-format 'latexinfo-discard-line-with-args)
(defun latexinfo-format-chapter-1 (belowchar)
  (let ((arg (latexinfo-parse-arg-discard)))
    (setq *index* (cons (1+ latexinfo-command-start) *index*))
    (insert ?\n arg ?\n "\\SectionPAD " belowchar?\n)
    (forward-line -2)))

(put 'head  'latexinfo-format 'latexinfo-format-head)
(defun latexinfo-format-head ()
  (let ((type))
    (save-excursion
      (forward-sexp 2)
      (setq type (buffer-substring
	       (1+ (point)) (1- (save-excursion (end-of-line 1) (point))))))
      (kill-line)
      (delete-char 1)
    (setq *index* (cons latexinfo-command-start *index*))
    (insert (latexinfo-parse-arg-discard))
    ;; use back-space as a marker to show the end of the command for indexing
    (insert "\b")
    (insert-char ?\  (- (+ 7 fill-column) (current-column) (length type)))
    (insert type))
  (goto-char latexinfo-command-start))

;; a dead head - an incomplete line that is neither indexed but nor LaTeX'ed
(put 'dhead  'latexinfo-format 'latexinfo-dhead)
(defun latexinfo-dhead ()
  (goto-char latexinfo-command-start)
  (delete-region (point) (progn (forward-line 2) (point))))


(defun latexinfo-format-chapter-1 (belowchar)
  (let ((arg (latexinfo-parse-arg-discard)))
    (setq *index* (cons (1+ latexinfo-command-start) *index*))
;;    (insert ?\n arg ?\b ?\n "\\SectionPAD " belowchar?\n)
    (insert ?\n arg ?\n "\\SectionPAD " belowchar?\n)
    (forward-line -2)))


