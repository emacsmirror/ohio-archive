;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; du-sort.el --- 
;; Author          : Lynn Slater
;; Created On      : Thu Feb  4 14:42:32 1988
;; Last Modified By: Lynn Slater
;; Last Modified On: Fri Oct 21 11:53:45 1988
;; Update Count    : 49
;; Status          : Apparently useful
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Instructions for Lynns disk space hierarchy tool:
;;    cd <someplace interesting>
;;    ~lrs/bin/huntbiggies > tempfile
;;    
;;    From Gmacs;
;;    Meta-X Load-file ~lrs/emacs/du-sort
;;    Meta-X Find-File tempfile
;;    Meta-X Group-buffer-lines

(defun group-buffer-lines (&optional limit)
  "Groupes lines with common prefixes together.
   This fcn assumes an input buffer with lines that hold a noise word
   followed by a file name. It is presumed that the buffer has been
   sorted in some manner. The first line is printed, and all the
   later entries whose file names match are printed and stripped.

   If there are more later entries than the LIMIT (passed or taken
   from the universal argument), the later lines are not printed and
   skipped until they are reached in their normal sequence. This
   prevents all files from being summarized under entries such as '.'.
  
   Typical use: from unix
   cd <someplace interesting>
   du | awk '$1 >= 1000 {print $1 '\t' $2}' > temp1
   sort -nr -1 temp1 > tempfile

   The above sequence is encapsulated in ~lrs/bin/huntbiggies. To use, type
   cd <someplace interesting>
   ~lrs/bin/huntbiggies > tempfile
   
   From Gmacs;
   Meta-X Load-file ~lrs/emacs/du-sort
   Meta-X Find-File tempfile
   (Edit out some of the top level entries if appropiate)
   Meta-X Save File
   Meta-X Group-buffer-lines
   Meta-X Revert-Buffer"
  (interactive "pFilter Limit:")
  (if (or (not limit) (< limit 2)) (setq limit 30))
  (if (buffer-modified-p)
      (error "This fcn temporarely modifies the buffer. Save the buffer first!"))
  (with-output-to-temp-buffer "*Grouped*"
    ;; Make the header entry
    (save-excursion
      (set-buffer standard-output)
      (if (not (fboundp 'make-header))
	  (load "~lrs/emacs/header" t))
      (if (fboundp 'make-header)
	  (make-header)))
    ;; get the next top level defn
    (beginning-of-buffer)
    (while (re-search-forward "^[0-9]+[ \t]+\\([^ \t\n]+\\)" () t)
      (let* (;;(executing-macro "true");; suppress "Mark Set" messages
	     (name;; get the file name
	       (buffer-substring
		 (marker-position (car (cdr (cdr (match-data)))))
		 (marker-position (car (cdr (cdr (cdr (match-data))))))))
	     (regexp (concat "[ \t]*" (regexp-quote name) "/")))
	;; Remove the line from all future consideration
	(beginning-of-line)
	(delete-line)
	;;(message "%s" name)
	;; Write the line in the new buffer
	(save-excursion
	  (set-buffer standard-output)
	  (end-of-buffer)
	  (yank)
	  ;;(sit-for 0)
	  )
	;; Are there only a few entries under this line?
	(if (not (more-matchesp regexp limit))
	    ;; Find all other files with this name as part of its substring
	    (extract-and-recurse regexp name)
	  (princ (format "\t\t(Not summarized because of more than %s Sub-Entries)\n" limit)))
	;; Go back to the top, make sure that we do not skip anything
	(beginning-of-buffer)
	)))
  (revert-buffer t t)
  (switch-to-buffer "*Grouped*")
  (delete-other-windows)
  )

(defun extract-and-recurse (regexp parent-name)
  (beginning-of-buffer)
  ;; find all references
  (while (re-search-forward regexp () t)
    (beginning-of-line)
    ;; grab the file name
    (re-search-forward "^[0-9]+[ \t]+\\([^ \t\n]+\\)" () t)
    (let* ((p)
	   (name
	     (buffer-substring
	       (marker-position (car (cdr (cdr (match-data)))))
	       (marker-position (car (cdr (cdr (cdr (match-data))))))))
	   (new-regexp (concat "[ \t]*" (regexp-quote name) "/")))

      ;; kill this line
      (beginning-of-line)
      (kill-word 1);; save number in the kill ring
      (delete-region (point) (progn (forward-line 1) (point)))
      ;; print in the other buffer
      (save-excursion
	(set-buffer standard-output)
	(end-of-buffer)
	(move-to-column-force (+ 8 (length parent-name)))
	(insert (substring name (length parent-name)  (length name)))
	(move-to-column-force 70)
	(setq p (point))
	(yank);; print the saved number
	(insert "\n"))
      ;; now, find sub divisions of this new name
      (extract-and-recurse new-regexp name)
      (beginning-of-buffer)
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun more-matchesp (regexp limit)
  "Return t if there are more matches of REGEXP than LIMIT"
  (let ((count 0) opoint)
    (save-excursion
      (while (and (not (eobp))
		  (<= count limit)
		  (progn (setq opoint (point))
			 (re-search-forward regexp nil t)))
	(if (= opoint (point))
	    (forward-char 1)
	  (setq count (1+ count))))
      (> count limit))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun move-to-column-force (column)
  "Move to column COLUMN in current line.
Differs from move-to-column in that it creates or modifies whitespace
if necessary to attain exactly the specified column.

This version (non-standard) insures that the column is visible,
scrolling if needed."
  (move-to-column column)
  (let ((col (current-column)))
    (if (< col column)
        (indent-to column)
      (if (and (/= col column)
               (= (preceding-char) ?\t))
          (let (indent-tabs-mode)
            (delete-char -1)
            (indent-to col)
            (move-to-column column)))))
  (point-wisiwig)
  )


(defun point-wisiwig ()
  "scrolls the window horozontally to make point visible"
  (let*  ((min (window-hscroll))
          (max (- (+ min (window-width)) 2))
          (here (current-column))
          (delta (/ (window-width) 2))
          )
    (if (< here min)
        (scroll-right (max 0 (+ (- min here) delta)))
      (if (>= here  max)
          (scroll-left (- (- here min) delta))
        ))))	
