; Path: dg-rtp!rock!mcnc!rutgers!stanford.edu!snorkelwacker.mit.edu!bu.edu!lll-winken!elroy.jpl.nasa.gov!usc!zaphod.mps.ohio-state.edu!cis.ohio-state.edu!ifi.uio.no!hallvard
; From: hallvard@ifi.uio.no (Hallvard B Furuseth)
; Newsgroups: gnu.emacs.sources
; Subject: Re: Wanted: Uncompress on load / Compress on save
; Date: 23 Jul 91 05:24:25 GMT

;; LCD Archive Entry:
;; load-z|Hallvard B Furuseth|hallvard@ifi.uio.no
;; |As load-file, but looks for .Z files too
;; |91-07-23||~/functions/load-z.el.Z|

(defun load-z (lz-file &optional lz-noerr lz-nomsg lz-nosuf)
  "As load-file, but looks for .Z files too."
  (interactive "fLoad file (accept .Z): ")
  (setq lz-file (where-is-file load-path lz-file
			       (if lz-nosuf ":.Z"
				 ".elc:.elc.Z:.el:.el.Z::.Z")))
  (cond ((null lz-file)
	 (and (not lz-noerr)
	      (error "Cannot open load file")))
	((equal (substring lz-file -2) ".Z")
	 (or lz-nosuf
	     (message "Loading %s..." lz-file))
	 (eval (save-excursion
		 (set-buffer (get-buffer-create " *Z-elisp*"))
		 (unwind-protect
		     (progn
		       (erase-buffer)
		       (insert "(progn\n\n\n)")
		       (forward-char -3)
		       (call-process "uncompress" lz-file t nil)
		       (goto-char (point-min))
		       (prog1 (read (current-buffer))
			 (or (eobp)
			     (error "Invalid read syntax"))))
		   (kill-buffer (current-buffer)))))
	 (or lz-nosuf
	     (message "Loading %s...done" lz-file))
	 t)
	((load lz-file lz-noerr lz-nomsg t))))

(defun require-z (rq-feature &optional rq-file)
  (or (featurep rq-feature)
      (progn (load-z (or rq-file (symbol-name rq-feature)) nil t)
	     (or (featurep rq-feature)
		 (error "required feature %s was not provided" rq-feature))))
  rq-feature)

(defun where-is-file (path file &optional suffixes)
  "Search through PATH (list) for a readable FILENAME, expanded by one of the
optional SUFFIXES (string of suffixes separated by \":\"s).  Interactively,
SUFFIXES (default \".elc:.el:\") is prompted when there is a prefix arg."
  (interactive
   (list (let ((path (read-minibuffer "Search path: " "load-path")))
	   (if (and (consp path) (or (stringp (car path)) (null (car path))))
	       path
	     (eval path)))
	 (read-string "Locate file: ")
	 (if current-prefix-arg
	     (read-string "Suffixes: " ".elc:.el:")
	   ".elc:.el:")))
  (if (not (equal file ""))
      (let ((filelist nil) pos temp templist)
	;; Make list of possible file names
	(setq filelist
	      (if suffixes
		  (progn
		    (while (setq pos (string-match ":[^:]*\\'" suffixes))
		      (setq filelist (cons (concat file (substring suffixes
								   (1+ pos)))
					   filelist))
		      (setq suffixes (substring suffixes 0 pos)))
		    (cons (concat file suffixes) filelist))
		(list file)))
	;; Search PATH for a readable file in filelist
	(catch 'bar
	  (if (file-name-absolute-p file) (setq path '(nil)))
	  (while path
	    (setq templist filelist)
	    (while
		(progn
		  (setq temp (expand-file-name (car templist) (car path)))
		  (cond ((file-readable-p temp)
			 (if (interactive-p)
			     (message "%s" temp))
			 (throw 'bar temp))
			((setq templist (cdr templist))))))
	    (setq path (cdr path)))
	  (if (interactive-p)
	      (message "(File %s not found)" file))
	  nil))))
