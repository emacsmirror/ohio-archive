;; Convert latexinfo files to info files.
;; Copyright (C) 1985, 1986, 1988 Free Software Foundation, Inc.

;; This file is not yet a part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.


(defvar latexinfo-formats-directory
  (file-name-as-directory (getenv "LATEXINFO")))

(if (not (memq latexinfo-formats-directory
	       load-path))
    (setq load-path (cons
		     latexinfo-formats-directory
		     load-path)))

(defvar latexinfo-known-document-styles
      '(latexinfo 11pt 12pt twoside titlepage psfonts smallverb format))

(autoload 'latexinfo-mode "latexinfo-mode"
	    "Major mode for editing latexinfo files." t)

(autoload 'Info-tagify "informat" "Info Tagify" t)
(autoload 'Info-split "informat" "Info Split" t)

(put 'latexinfoversion 'latexinfo-format 'latexinfo-format-latexinfoversion)
(defun latexinfo-format-latexinfoversion ()
  (latexinfo-parse-noarg)
  (insert "1.2"))

(defvar latexinfo-format-syntax-table nil)

(defvar latexinfo-vindex)
(defvar latexinfo-findex)
(defvar latexinfo-cindex)
(defvar latexinfo-cpsubindex)
(defvar latexinfo-pindex)
(defvar latexinfo-tindex)
(defvar latexinfo-kindex)
(defvar latexinfo-last-node)
(defvar latexinfo-node-names)

(if latexinfo-format-syntax-table
    nil
  (setq latexinfo-format-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\" " " latexinfo-format-syntax-table)
  (modify-syntax-entry ?* "w" latexinfo-format-syntax-table)
  (modify-syntax-entry ?\\ "\\" latexinfo-format-syntax-table)
  (modify-syntax-entry ?\[ "(]" latexinfo-format-syntax-table)
  (modify-syntax-entry ?\] ")[" latexinfo-format-syntax-table)
  (modify-syntax-entry ?\( "." latexinfo-format-syntax-table)
  (modify-syntax-entry ?\) "." latexinfo-format-syntax-table)
  (modify-syntax-entry ?{ "(}" latexinfo-format-syntax-table)
  (modify-syntax-entry ?} "){" latexinfo-format-syntax-table)
  (modify-syntax-entry ?\' "." latexinfo-format-syntax-table))

(defun latexinfo-format-buffer (&optional notagify)
  "Process the current buffer as latexinfo code, into an Info file.
The Info file output is generated in a buffer visiting the Info file
names specified in the \\setfilename command.

Non-nil argument (prefix, if interactive) means don't make tag table
and don't split the file if large.  You can use Info-tagify and
Info-split to do these manually."
  (interactive "P")
  (let ((lastmessage "Formatting Info file..."))
    (message lastmessage)
    (latexinfo-format-buffer-1)
    (if notagify
	nil
      (progn
	(if (> (buffer-size) 30000)
	    (progn
	      (message (setq lastmessage "Making tags table for Info file..."))
	      (Info-tagify)))
	(if (> (buffer-size) 100000)
	    (progn
	      (message (setq lastmessage "Splitting Info file..."))
	      (Info-split)))))
    (message (concat lastmessage
		     (if (interactive-p) "done.  Now save it." "done.")))))

(defun latexinfo-format-buffer-1 ()
  (let (latexinfo-format-filename
	latexinfo-example-start
	latexinfo-command-start
	latexinfo-command-end
	latexinfo-command-name
	latexinfo-last-node
	latexinfo-vindex
	latexinfo-findex
	latexinfo-cindex
	latexinfo-cpsubindex
	latexinfo-pindex
	latexinfo-tindex
	latexinfo-kindex
	latexinfo-stack
	latexinfo-node-names
	outfile
	(fill-column fill-column)
	(input-buffer (current-buffer))
	(input-directory default-directory))
    (save-excursion
      (goto-char (point-min))
      (search-forward "\\setfilename")
      (setq latexinfo-command-end (point))
      (setq outfile (latexinfo-parse-line-arg)))
    (find-file outfile)
    (latexinfo-mode)
    (set-syntax-table latexinfo-format-syntax-table)
    (erase-buffer)
    (insert-buffer-substring input-buffer)
    (goto-char (point-min))
    (latexinfo-run-documentstyle-hooks)
    ;; Run this after latexinfo-run-documentstyle-hooks
    (goto-char (point-min))
    (search-forward "\\setfilename")
    (beginning-of-line)
    (delete-region (point-min) (point))
    ;; Remove \end{document} at end of file, if it is there.
    (goto-char (point-max))
    (if (search-backward "\\end{document}" nil t)
	(delete-region (point) (point-max))
      (error "Missing \\end{document}"))
    ;; Make sure buffer ends in a newline.
    (or (= (preceding-char) ?\n)
	(insert "\n"))
    ;; Scan the whole buffer, converting to Info format.
    (goto-char (point-min))
    (latexinfo-format-scan-noverbatim)
    ;; Return data for indices.
    (goto-char (point-min))
    (list outfile
	  latexinfo-vindex latexinfo-findex latexinfo-cindex
	  latexinfo-pindex latexinfo-tindex latexinfo-kindex)))

(defvar latexinfo-region-buffer-name "*Info Region*"
  "*Name of the temporary buffer used by \\[latexinfo-format-region].")

(defun latexinfo-format-region (region-beginning region-ending)
  "Convert the the current region of the Latexinfo file to Info format.
This lets you see what that part of the file will look like in Info.
The command is bound to \\[latexinfo-format-region].  The text that is
converted to Info is stored in a temporary buffer."
  (interactive "r")
  (message "Converting region to Info format...")
  (let (latexinfo-command-start
	latexinfo-command-end
	latexinfo-command-name
	latexinfo-vindex
	latexinfo-findex
	latexinfo-cindex
	latexinfo-pindex
	latexinfo-tindex
	latexinfo-kindex
	latexinfo-stack
	latexinfo-format-filename
	latexinfo-example-start
	latexinfo-last-node
	latexinfo-node-names
	(fill-column fill-column)
	(input-buffer (current-buffer))
	(input-directory default-directory)
	filename-beginning
	filename-ending)

;;; Find a buffer to use.

    (switch-to-buffer (get-buffer-create latexinfo-region-buffer-name))

    ;; Insert the region into the buffer.
    (erase-buffer)

    (save-excursion
      (set-buffer input-buffer)
      (save-excursion
	(save-restriction
	  (widen)
	  (goto-char (point-min))
	  ;; Initialize the buffer with the filename
	  ;; or else explain that a filename is needed.
	  (or (search-forward "\\setfilename"
			      (save-excursion (forward-line 100) (point)) t)
	      (error "The latexinfo file needs a line saying: \\setfilename <name>"))
	  (beginning-of-line)
	  (setq filename-beginning (point))
	  (forward-line 1)
	  (setq filename-ending (point)))))

    ;; Insert the \\setfilename line into the buffer.
    (insert-buffer-substring input-buffer
			     (min filename-beginning region-beginning)  
			     filename-ending)
    
    ;; Insert the region into the buffer.
    (insert-buffer-substring input-buffer
			     (max region-beginning filename-ending)
			     region-ending)

    (latexinfo-mode)

    ;; Install a syntax table useful for scanning command operands.
    (set-syntax-table latexinfo-format-syntax-table)
    
    ;; If the region includes the effective end of the data,
    ;; discard everything after that.
    (goto-char (point-max))
    (if (search-backward "\\end{document}" nil t)
	(delete-region (point) (point-max)))
    ;; Make sure buffer ends in a newline.
    (or (= (preceding-char) ?\n)
	(insert "\n"))

    (goto-char (point-max))
    ;; Now convert for real.
    (goto-char (point-min))
    (latexinfo-format-scan-noverbatim)
    (goto-char (point-min)))

  (message "Done."))

(defun latexinfo-format-scan-noverbatim ()
  (if (re-search-forward "^\\\\begin{verbatim}\\|\\\\input{" nil t)
      (let ((start (point-min))
	    end
	    str)
	(goto-char start)
	(while (re-search-forward "^\\\\begin{verbatim}\\|\\\\input{" nil t)
	  (setq str (buffer-substring (match-beginning 0) (match-end 0)))
	  ;; Handle LaTeX \input{filename} commands by inserting them now.
	  ;; Only look at the beginning of the line to avoid \c \input{foo}
	  (cond ((string-equal str "\\input{")
		 (save-excursion
		   (skip-chars-forward " 	{")
		   (let ((file-name
			  (buffer-substring 
			   (point)
			   (progn
			     (skip-chars-forward "^ 	}\n")
			     (point)))))
		     (setq file-name
			   (expand-file-name
			    (if (file-readable-p (expand-file-name file-name))
				file-name
			      (concat file-name ".tex"))))
		     (beginning-of-line 1)
		     (delete-region (point) (progn (forward-line 1) (point)))
		     (message "Inserting file %s..." file-name) (sit-for 1)
		     (insert-file file-name)
		     (message "Inserting file %s...done" file-name)
		     )))
		((string-equal str "\\begin{verbatim}")
		 (delete-region (point) (progn (beginning-of-line 1) (point)))
;;		 (message "texinfo-format-scan-noverbatim %d" (point))
;;		 (sit-for 1)
		 (latexinfo-format-expand-region start (point))
		 (setq start
		       (progn
			 (goto-char start)
			 (if (re-search-forward "^\\\\end{verbatim}" nil t)
			     (progn
			       (delete-region (point)
					      (progn (beginning-of-line 1)
						     (forward-char -1)
						     (point)))
			       (point))
			   (error "Missing \\end{verbatim}"))))
		 )))
	(latexinfo-format-expand-region start (point-max))
	)
    (latexinfo-format-scan)))

(put 'verbatim 'latexinfo-format 'latexinfo-format-verbatim)
(defun latexinfo-format-verbatim ()
  (error "Nested verbatim environments are not supported"))

(defun latexinfo-format-scan ()
  ;; LaTeX sometimes uses \\ to force a new-line
  (goto-char (point-min))
  (replace-regexp "\\\\\\\\$" "")
  ;; Convert left and right quotes to typewriter font quotes.
  (goto-char (point-min))
  (while (search-forward "``" nil t)
    (replace-match "\""))
  (goto-char (point-min))
  (while (search-forward "''" nil t)
    (replace-match "\""))
  ;; Scan for \\-commands.
  (goto-char (point-min))
  (while (search-forward "\\" nil t)
    (if (looking-at "[@{}'` *%]")
	;; Handle a few special \-followed-by-one-char commands.
	(if (= (following-char) ?*)
	    ;; \* has no effect, since we are not filling.
	    (delete-region (1- (point)) (1+ (point)))
	  ;; The other characters are simply quoted.  Delete the \.
	  (delete-char -1)
	  (forward-char 1))
      ;; \ is followed by a command-word; find the end of the word.
      (setq latexinfo-command-start (1- (point)))
      (if (= (char-syntax (following-char)) ?w)
	  (forward-word 1)
	(forward-char 1))
      (setq latexinfo-command-end (point))
      ;; Call the handler for this command.
      (setq latexinfo-command-name
	    (intern (buffer-substring (1+ latexinfo-command-start)
				      latexinfo-command-end)))
      (let ((cmd (get latexinfo-command-name 'latexinfo-format)))
	(if cmd (funcall cmd)
	  (latexinfo-unsupported)))))
  (cond (latexinfo-stack
	 (goto-char (nth 2 (car latexinfo-stack)))
	 (error "Unterminated \begin{%s}" (car (car latexinfo-stack))))))

(put 'begin 'latexinfo-format 'latexinfo-format-begin)
(defun latexinfo-format-begin ()
  (latexinfo-format-begin-end 'latexinfo-format))

(put 'end 'latexinfo-format 'latexinfo-format-end)
(defun latexinfo-format-end ()
  (latexinfo-format-begin-end 'latexinfo-end))

(defun latexinfo-format-begin-end (prop)
  (setq latexinfo-command-name (intern (latexinfo-parse-line-arg)))
  (setq cmd (get latexinfo-command-name prop))
  (if cmd (funcall cmd)
    (latexinfo-unsupported)))

(defun latexinfo-parse-line-arg ()
  (goto-char latexinfo-command-end)
  (let ((start (point)))
    (cond ((looking-at " ")
	   (skip-chars-forward " ")
	   (setq start (point))
	   (end-of-line)
	   (setq latexinfo-command-end (1+ (point))))
	  ((looking-at "[{\\[]")
	   (setq start (1+ (point)))
	   (forward-list 1)
	   (setq latexinfo-command-end (point))
	   (forward-char -1))
	  (t
	   (error "Invalid latexinfo command arg format")))
    (prog1 (buffer-substring start (point))
	   (if (eolp) (forward-char 1)))))

(defun latexinfo-parse-expanded-arg ()
  (goto-char latexinfo-command-end)
  (let ((start (point))
	marker)
    (cond ((looking-at " ")
	   (skip-chars-forward " ")
	   (setq start (point))
	   (end-of-line)
	   (setq latexinfo-command-end (1+ (point))))
	  ((looking-at "[{\\[]")
	   (setq start (1+ (point)))
	   (forward-list 1)
	   (setq latexinfo-command-end (point))
	   (forward-char -1))
	  (t
	   (error "Invalid latexinfo command arg format")))
    (setq marker (move-marker (make-marker) latexinfo-command-end))
    (latexinfo-format-expand-region start (point))
    (setq latexinfo-command-end (marker-position marker))
    (move-marker marker nil)
    (prog1 (buffer-substring start (point))
	   (if (eolp) (forward-char 1)))))

(defun latexinfo-format-expand-region (start end)
  (save-restriction
    (narrow-to-region start end)
    (let (latexinfo-command-start
	  latexinfo-command-end
	  latexinfo-command-name
	  latexinfo-stack)
      (latexinfo-format-scan))
    (goto-char (point-max))))

(defun latexinfo-parse-arg-discard ()
  (prog1 (latexinfo-parse-line-arg)
	 (latexinfo-discard-command)))

(defun latexinfo-discard-command ()
  (delete-region latexinfo-command-start latexinfo-command-end))

(defun latexinfo-format-parse-line-args ()
  (let ((start (1- (point)))
	next beg end
	args)
    (skip-chars-forward " ")
    (while (not (eolp))
      (setq beg (point))
      (re-search-forward "[\n,]")
      (setq next (point))
      (if (bolp) (setq next (1- next)))
      (forward-char -1)
      (skip-chars-backward " ")
      (setq end (point))
      (setq args (cons (if (> end beg) (buffer-substring beg end))
		       args))
      (goto-char next)
      (skip-chars-forward " "))
    (if (eolp) (forward-char 1))
    (setq latexinfo-command-end (point))
    (nreverse args)))

(defun latexinfo-format-parse-args ()
  (let ((start (1- (point)))
	next beg end
	args)
    (search-forward "{")
    (while (/= (preceding-char) ?\})
      (skip-chars-forward " \t\n")
      (setq beg (point))
      (re-search-forward "[},]")
      (setq next (point))
      (forward-char -1)
      (skip-chars-backward " \t\n")
      (setq end (point))
      (cond ((< beg end)
	     (goto-char beg)
	     (while (search-forward "\n" end t)
	       (replace-match " "))))
      (setq args (cons (if (> end beg) (buffer-substring beg end))
		       args))
      (goto-char next))
    (if (eolp) (forward-char 1))
    (setq latexinfo-command-end (point))
    (nreverse args)))

(defun latexinfo-format-parse-defun-args ()
  (goto-char latexinfo-command-end)
  (let ((start (point)))
    (end-of-line)
    (setq latexinfo-command-end (1+ (point)))
    (let ((marker (move-marker (make-marker) latexinfo-command-end)))
      (latexinfo-format-expand-region start (point))
      (setq latexinfo-command-end (marker-position marker))
      (move-marker marker nil))
    (goto-char start)
    (let ((args '())
	  beg end)
      (skip-chars-forward " ")
      (while (not (eolp))
	(cond ((looking-at "[{\\[]")
	       (setq beg (1+ (point)))
	       (forward-list 1)
	       (setq end (1- (point))))
	      (t
	       (setq beg (point))
	       (re-search-forward "[\n ]")
	       (forward-char -1)
	       (setq end (point))))
	(setq args (cons (buffer-substring beg end) args))
	(skip-chars-forward " "))
      (forward-char 1)
      (nreverse args))))

(put 'setfilename 'latexinfo-format 'latexinfo-format-setfilename)
(defun latexinfo-format-setfilename ()
  (let ((arg (latexinfo-parse-arg-discard)))
    (setq latexinfo-format-filename
	  (file-name-nondirectory (expand-file-name arg)))
    (insert "Info file: "
	    latexinfo-format-filename ",    -*-Text-*-\n"
	    "produced by latexinfo-format-buffer\nfrom "
	    (if (buffer-file-name input-buffer)
		(concat "file: "
			(file-name-sans-versions
			 (file-name-nondirectory
			  (buffer-file-name input-buffer))))
	      (concat "buffer " (buffer-name input-buffer)))
	    "\n\n")))

(put 'node 'latexinfo-format 'latexinfo-format-node)
(defun latexinfo-format-node ()
  (let* ((args (latexinfo-format-parse-line-args))
	 (name (nth 0 args))
	 (next (nth 1 args))
	 (prev (nth 2 args))
	 (up (nth 3 args)))
    (latexinfo-discard-command)
    (setq latexinfo-last-node name)
    (let ((tem (downcase name)))
      (if (assoc tem latexinfo-node-names)
	  (error "Duplicate node name: %s" name)
	(setq latexinfo-node-names (cons tem latexinfo-node-names))))
    (or (bolp)
	(insert ?\n))
    (insert "\^_\nFile: " latexinfo-format-filename
	    "  Node: " name)
    (if prev
	(insert ", Prev: " prev))
    (if up
	(insert ", Up: " up))
    (if next
	(insert ", Next: " next))
    (insert ?\n)))

(put 'menu 'latexinfo-format 'latexinfo-format-menu)
(defun latexinfo-format-menu ()
  (latexinfo-discard-line)
  (insert "* Menu:\n\n"))

(put 'menu 'latexinfo-end 'latexinfo-discard-command)
(defun latexinfo-discard-line ()
  (goto-char latexinfo-command-end)
  (skip-chars-forward " \t")
  (or (eolp)
      (error "Extraneous text at end of command line."))
  (goto-char latexinfo-command-start)
  (or (bolp)
      (error "Extraneous text at beginning of command line."))
  (delete-region (point) (progn (forward-line 1) (point))))

; \xref {NODE, FNAME, NAME, FILE, DOCUMENT}
; -> *Note FNAME: (FILE)NODE
;   If FILE is missing,
;    *Note FNAME: NODE
;   If FNAME is empty and NAME is present
;    *Note NAME: Node
;   If both NAME and FNAME are missing
;    *Note NODE::
;   latexinfo ignores the DOCUMENT argument.
; -> See section <xref to NODE> [NAME, else NODE], page <xref to NODE>
;   If FILE is specified, (FILE)NODE is used for xrefs.
;   If fifth argument DOCUMENT is specified, produces
;    See section <xref to NODE> [NAME, else NODE], page <xref to NODE>
;    of DOCUMENT
(put 'xref 'latexinfo-format 'latexinfo-format-xref)
(defun latexinfo-format-xref ()
  (let ((args (latexinfo-format-parse-args)))
    (latexinfo-discard-command)
    (insert "*Note ")
    (let ((fname (or (nth 1 args) (nth 2 args))))
      (if (null (or fname (nth 3 args)))
	  (insert (car args) "::")
	(insert (or fname (car args)) ": ")
	(if (nth 3 args)
	    (insert "(" (nth 3 args) ")"))
	(insert (car args))))))

(put 'pxref 'latexinfo-format 'latexinfo-format-pxref)
(defun latexinfo-format-pxref ()
  (latexinfo-format-xref)
  (or (save-excursion
	(forward-char -2)
	(looking-at "::"))
      (insert ".")))

;\inforef{NODE, FNAME, FILE}
;Like \xref{NODE, FNAME,,FILE} in latexinfo.
;In LaTeX, generates "See Info file FILE, node NODE"
(put 'inforef 'latexinfo-format 'latexinfo-format-inforef)
(defun latexinfo-format-inforef ()
  (let ((args (latexinfo-format-parse-args)))
    (latexinfo-discard-command)
    (insert "*Note " (nth 1 args) ": (" (nth 2 args) ")" (car args))))

(put 'appendix 'latexinfo-format 'latexinfo-parse-noarg)

(put 'chapter 'latexinfo-format 'latexinfo-format-chapter)
(put 'chapter* 'latexinfo-format 'latexinfo-format-chapter)
(defun latexinfo-format-chapter ()
  (latexinfo-format-chapter-1 ?*))

(put 'section 'latexinfo-format 'latexinfo-format-section)
(put 'section* 'latexinfo-format 'latexinfo-format-section)
(defun latexinfo-format-section ()
  (latexinfo-format-chapter-1 ?=))

(put 'subsection 'latexinfo-format 'latexinfo-format-subsection)
(put 'subsection* 'latexinfo-format 'latexinfo-format-subsection)
(defun latexinfo-format-subsection ()
  (latexinfo-format-chapter-1 ?-))

(put 'subsubsection 'latexinfo-format 'latexinfo-format-subsubsection)
(put 'subsubsection* 'latexinfo-format 'latexinfo-format-subsubsection)
(defun latexinfo-format-subsubsection ()
  (latexinfo-format-chapter-1 ?.))

(defun latexinfo-format-chapter-1 (belowchar)
  (let ((arg (latexinfo-parse-arg-discard)))
    (insert ?\n arg ?\n "\\SectionPAD " belowchar ?\n)
    (forward-line -2)))

(put 'SectionPAD 'latexinfo-format 'latexinfo-format-sectionpad)
(defun latexinfo-format-sectionpad ()
  (let ((str (latexinfo-parse-arg-discard)))
    (forward-char -1)
    (let ((column (current-column)))
      (forward-char 1)
      (while (> column 0)
	(insert str)
	(setq column (1- column))))
    (insert ?\n)))

(put '\. 'latexinfo-format 'latexinfo-format-\.)
(defun latexinfo-format-\. ()
  (latexinfo-discard-command)
  (insert "."))

(put '\: 'latexinfo-format 'latexinfo-format-\:)
(defun latexinfo-format-\: ()
  (latexinfo-discard-command))


;; \begin{itemize} pushes (itemize "COMMANDS" STARTPOS) on latexinfo-stack.
;; \begin{enumerate} pushes (enumerate 0 STARTPOS).
;; \item dispatches to the latexinfo-item prop of the first elt of the list.
;; For itemize, this puts in and rescans the COMMANDS.
;; For enumerate, this increments the number and puts it in.
;; In either case, it puts a Backspace at the front of the line
;; which marks it not to be indented later.
;; All other lines get indented by 5 when the \end is reached.

(defun latexinfo-push-stack (check arg)
  (setq latexinfo-stack
	(cons (list check arg latexinfo-command-start)
	      latexinfo-stack)))

(defun latexinfo-pop-stack (check)
  (if (null latexinfo-stack)
      (error "Unmatched \\end{%s}" check))
  (if (not (eq (car (car latexinfo-stack)) check))
      (error "\\end{%s} matches \\begin{%s}"
	     check (car (car latexinfo-stack))))
  (prog1 (cdr (car latexinfo-stack))
	 (setq latexinfo-stack (cdr latexinfo-stack))))

(put 'itemize 'latexinfo-format 'latexinfo-itemize)
(defun latexinfo-itemize ()
  (latexinfo-push-stack 'itemize "*")
  (setq fill-column (- fill-column 5))
  (latexinfo-discard-line))

(put 'itemize 'latexinfo-end 'latexinfo-end-itemize)
(defun latexinfo-end-itemize ()
  (setq fill-column (+ fill-column 5))
  (latexinfo-discard-command)
  (let ((stacktop
	 (latexinfo-pop-stack 'itemize)))
    (latexinfo-do-itemize (nth 1 stacktop))))

(put 'enumerate 'latexinfo-format 'latexinfo-enumerate)
(defun latexinfo-enumerate ()
  (latexinfo-push-stack 'enumerate 0)
  (setq fill-column (- fill-column 5))
  (latexinfo-discard-line))

(put 'enumerate 'latexinfo-end 'latexinfo-end-enumerate)
(defun latexinfo-end-enumerate ()
  (setq fill-column (+ fill-column 5))
  (latexinfo-discard-command)
  (let ((stacktop
	 (latexinfo-pop-stack 'enumerate)))
    (latexinfo-do-itemize (nth 1 stacktop))))

(put 'center 'latexinfo-format 'latexinfo-format-center)
(defun latexinfo-format-center ()
  (latexinfo-push-stack 'center 0)
  (latexinfo-discard-line))

(put 'center 'latexinfo-end 'latexinfo-end-center)
(defun latexinfo-end-center ()
  (latexinfo-discard-command)
  (let ((stacktop
	 (latexinfo-pop-stack 'center)))
    (latexinfo-do-center (nth 1 stacktop))))

(defun latexinfo-do-center (from)
  (let ((indent-tabs-mode nil))
    (while (progn (forward-line -1)
		  (>= (point) from))
      (save-excursion
	(center-line)))))

(put 'table 'latexinfo-format 'latexinfo-table)
(defun latexinfo-table ()
  (latexinfo-push-stack 'table (latexinfo-parse-arg-discard))
  (setq fill-column (- fill-column 5)))

(put 'ftable 'latexinfo-format 'latexinfo-ftable)
(defun latexinfo-ftable ()
  (latexinfo-push-stack 'table "\\code")
  (setq fill-column (- fill-column 5))
  (latexinfo-discard-line))

(put 'description 'latexinfo-format 'latexinfo-description)
(defun latexinfo-description ()
  (latexinfo-push-stack 'table "\\asis")
  (setq fill-column (- fill-column 5))
  (latexinfo-discard-line))

(put 'table 'latexinfo-end 'latexinfo-end-table)
(put 'ftable 'latexinfo-end 'latexinfo-end-table)
(put 'description 'latexinfo-end 'latexinfo-end-table)
(defun latexinfo-end-table ()
  (setq fill-column (+ fill-column 5))
  (latexinfo-discard-command)
  (let ((stacktop
	 (latexinfo-pop-stack 'table)))
    (latexinfo-do-itemize (nth 1 stacktop))))

;; At the \end, indent all the lines within the construct
;; except those marked with backspace.  FROM says where
;; construct started.
(defun latexinfo-do-itemize (from)
  (save-excursion
   (while (progn (forward-line -1)
		 (>= (point) from))
     (if (= (following-char) ?\b)
	 (save-excursion
	   (delete-char 1)
	   (end-of-line)
	   (delete-char 6))
       (save-excursion (insert "     "))))))

(put 'item 'latexinfo-format 'latexinfo-item)
(defun latexinfo-item ()
  (funcall (get (car (car latexinfo-stack)) 'latexinfo-item)))

(put 'itemize 'latexinfo-item 'latexinfo-itemize-item)
(defun latexinfo-itemize-item ()
  (latexinfo-parse-noarg)
  (insert "\b   " (nth 1 (car latexinfo-stack)) " \n")
  (forward-line -1))

(put 'enumerate 'latexinfo-item 'latexinfo-enumerate-item)
(defun latexinfo-enumerate-item ()
  (latexinfo-parse-noarg)
  (let ((next (1+ (car (cdr (car latexinfo-stack))))))
    (setcar (cdr (car latexinfo-stack)) next)
    (insert ?\b (format "%3d. " next) ?\n))
  (forward-line -1))

(put 'table 'latexinfo-item 'latexinfo-table-item)
(defun latexinfo-table-item ()
  (let ((arg (latexinfo-parse-arg-discard))
	(itemfont (car (cdr (car latexinfo-stack)))))
    (insert ?\b itemfont ?\{ arg "}\n     \n"))
  (forward-line -2))

(put 'ifinfo 'latexinfo-format 'latexinfo-discard-line)
(put 'ifinfo 'latexinfo-end 'latexinfo-discard-command)

(put 'iftex 'latexinfo-format 'latexinfo-format-iftex)
(defun latexinfo-format-iftex ()
  (delete-region latexinfo-command-start
		 (progn (re-search-forward "\\\\end[ {]iftex[ }\n]")
			(point))))

(put 'tex 'latexinfo-format 'latexinfo-format-tex)
(defun latexinfo-format-tex ()
  (delete-region latexinfo-command-start
		 (progn (re-search-forward "\\end[ {]tex[ 	}\n]")
			(point))))

(put 'ignore 'latexinfo-format 'latexinfo-format-ignore)
(defun latexinfo-format-ignore ()
  (delete-region latexinfo-command-start
		 (progn (re-search-forward "\\\\end[ {]ignore[ 	}\n]")
			(point))))

(put 'endignore 'latexinfo-format 'latexinfo-discard-line)

(put 'var 'latexinfo-format 'latexinfo-format-var)
(defun latexinfo-format-var ()
  (insert (upcase (latexinfo-parse-arg-discard)))
  (goto-char latexinfo-command-start))

(put 'asis 'latexinfo-format 'latexinfo-format-noop)
(put 'b 'latexinfo-format 'latexinfo-format-noop)
(put 't 'latexinfo-format 'latexinfo-format-noop)
(put 'i 'latexinfo-format 'latexinfo-format-noop)
(put 'key 'latexinfo-format 'latexinfo-format-noop)
(put 'w 'latexinfo-format 'latexinfo-format-noop)
(defun latexinfo-format-noop ()
  (insert (latexinfo-parse-arg-discard))
  (goto-char latexinfo-command-start))

(put 'code 'latexinfo-format 'latexinfo-format-code)
(put 'samp 'latexinfo-format 'latexinfo-format-code)
(put 'file 'latexinfo-format 'latexinfo-format-code)
(put 'kbd 'latexinfo-format 'latexinfo-format-code)
(defun latexinfo-format-code ()
  (insert "`" (latexinfo-parse-arg-discard) "'")
  (goto-char latexinfo-command-start))

(put 'emph 'latexinfo-format 'latexinfo-format-emph)
(put 'strong 'latexinfo-format 'latexinfo-format-emph)
(defun latexinfo-format-emph ()
  (insert "*" (latexinfo-parse-arg-discard) "*"))

(put 'defn 'latexinfo-format 'latexinfo-format-defn)
(put 'dfn 'latexinfo-format 'latexinfo-format-defn)
(defun latexinfo-format-defn ()
  (insert "\"" (latexinfo-parse-arg-discard) "\"")
  (goto-char latexinfo-command-start))

(put 'bullet 'latexinfo-format 'latexinfo-format-bullet)
(defun latexinfo-format-bullet ()
  (latexinfo-discard-command)
  (insert "*"))

(put 'smallexample 'latexinfo-format 'latexinfo-format-example)
(put 'example 'latexinfo-format 'latexinfo-format-example)
(put 'quotation 'latexinfo-format 'latexinfo-format-example)
(put 'lisp 'latexinfo-format 'latexinfo-format-example)
(put 'display 'latexinfo-format 'latexinfo-format-example)
(put 'format 'latexinfo-format 'latexinfo-format-example)
(put 'flushleft 'latexinfo-format 'latexinfo-format-example)
(defun latexinfo-format-example ()
  (latexinfo-push-stack 'example nil)
  (setq fill-column (- fill-column 5))
  (latexinfo-discard-line))

(put 'smallexample 'latexinfo-end 'latexinfo-end-example)
(put 'example 'latexinfo-end 'latexinfo-end-example)
(put 'quotation 'latexinfo-end 'latexinfo-end-example)
(put 'lisp 'latexinfo-end 'latexinfo-end-example)
(put 'display 'latexinfo-end 'latexinfo-end-example)
(put 'format 'latexinfo-end 'latexinfo-end-example)
(put 'flushleft 'latexinfo-end 'latexinfo-end-example)
(defun latexinfo-end-example ()
  (setq fill-column (+ fill-column 5))
  (latexinfo-discard-command)
  (let ((stacktop
	 (latexinfo-pop-stack 'example)))
    (latexinfo-do-itemize (nth 1 stacktop))))

(put 'exdent 'latexinfo-format 'latexinfo-format-exdent)
(defun latexinfo-format-exdent ()
  (latexinfo-discard-command)
  (delete-region (point)
		 (progn
		  (skip-chars-forward " ")
		  (point)))
  (insert ?\b)
  ;; Cancel out the deletion that latexinfo-do-itemize
  ;; is going to do at the end of this line.
  (save-excursion
    (end-of-line)
    (insert "\n     ")))

(put 'ctrl 'latexinfo-format 'latexinfo-format-ctrl)
(defun latexinfo-format-ctrl ()
  (let ((str (latexinfo-parse-arg-discard)))
    (insert "^" str)))
    ;;    (insert (logand 31 (aref str 0)))))

(put 'TeX 'latexinfo-format 'latexinfo-format-TeX)
(defun latexinfo-format-TeX ()
  (latexinfo-parse-noarg)
  (insert "TeX"))

(put 'copyright 'latexinfo-format 'latexinfo-format-copyright)
(defun latexinfo-format-copyright ()
  (latexinfo-parse-noarg)
  (insert "(C)"))

(put 'minus 'latexinfo-format 'latexinfo-format-minus)
(defun latexinfo-format-minus ()
  (latexinfo-parse-arg-discard)
  (insert "-"))

(put 'dots 'latexinfo-format 'latexinfo-format-dots)
(defun latexinfo-format-dots ()
  (latexinfo-discard-command)
  (insert "..."))

(put 'refill 'latexinfo-format 'latexinfo-format-refill)
(defun latexinfo-format-refill ()
  (latexinfo-discard-command)
  (fill-paragraph nil))

;; Index generation

(put 'vindex 'latexinfo-format 'latexinfo-format-vindex)
(defun latexinfo-format-vindex ()
  (latexinfo-index 'latexinfo-vindex))

(put 'cindex 'latexinfo-format 'latexinfo-format-cindex)
(defun latexinfo-format-cindex ()
  (latexinfo-index 'latexinfo-cindex))

(put 'findex 'latexinfo-format 'latexinfo-format-findex)
(defun latexinfo-format-findex ()
  (latexinfo-index 'latexinfo-findex))

(put 'pindex 'latexinfo-format 'latexinfo-format-pindex)
(defun latexinfo-format-pindex ()
  (latexinfo-index 'latexinfo-pindex))

(put 'tindex 'latexinfo-format 'latexinfo-format-tindex)
(defun latexinfo-format-tindex ()
  (latexinfo-index 'latexinfo-tindex))

(put 'kindex 'latexinfo-format 'latexinfo-format-kindex)
(defun latexinfo-format-kindex ()
  (latexinfo-index 'latexinfo-kindex))

(defun latexinfo-index (indexvar)
  (let ((arg (latexinfo-parse-expanded-arg)))
    (latexinfo-discard-command)
    (set indexvar
	 (cons (list arg latexinfo-last-node)
	       (symbol-value indexvar)))))

(defconst latexinfo-indexvar-alist
  '(("cp" . latexinfo-cindex)
    ("fn" . latexinfo-findex)
    ("vr" . latexinfo-vindex)
    ("tp" . latexinfo-tindex)
    ("pg" . latexinfo-pindex)
    ("ky" . latexinfo-kindex)))

(put 'printindex 'latexinfo-format 'latexinfo-format-printindex)
(defun latexinfo-format-printindex ()
  (let ((indexelts (symbol-value
		    (cdr (assoc (latexinfo-parse-arg-discard)
				latexinfo-indexvar-alist))))
	opoint)
    (insert "\n* Menu:\n\n")
    (setq opoint (point))
    (latexinfo-print-index nil indexelts)
    (if (eq system-type 'vax-vms) 
	(latexinfo-sort-region opoint (point))
      (shell-command-on-region opoint (point) "sort -fd" 1))))

(defun latexinfo-print-index (file indexelts)
  (while indexelts
    (if (stringp (car (car indexelts)))
	(insert "* " (car (car indexelts))
		": " (if file (concat "(" file ")") "")
		(nth 1 (car indexelts)) ".\n")
      ;; index entries from \include'd file
      (latexinfo-print-index (nth 1 (car indexelts))
			   (nth 2 (car indexelts))))
    (setq indexelts (cdr indexelts))))


;;;; Lisp Definitions

(defun latexinfo-format-defun ()
  (latexinfo-push-stack 'defun nil)
  (setq fill-column (- fill-column 5))
  (latexinfo-format-defun-1 t))

(defun latexinfo-format-defunx ()
  (latexinfo-format-defun-1 nil))

(defun latexinfo-format-defun-1 (first-p)
  (let ((args (latexinfo-format-parse-defun-args))
	(type (get latexinfo-command-name 'latexinfo-defun-type)))
    (latexinfo-discard-command)
    (if (eq type 'arg)
	(progn (setq type (car args))
	       (setq args (cdr args))))
    (let ((formatter (get latexinfo-command-name 'latexinfo-defun-format-type)))
      (if formatter
	  (setq type (funcall formatter type args))))
    ;; Delete extra newline inserted after previous header line.
    (if (not first-p)
	(delete-char -1))
    (insert "* " type ": " (car args))
    (let ((args (cdr args)))
      (while args
	(insert " " (upcase (car args)))
	(setq args (cdr args))))
    ;; Insert extra newline so that paragraph filling does not mess
    ;; with header line.
    (insert "\n\n")
    (rplaca (cdr (cdr (car latexinfo-stack))) (point))
    (let ((indexvar (get latexinfo-command-name 'latexinfo-defun-index))
	  (formatter (get latexinfo-command-name 'latexinfo-defun-format-index)))
      (set indexvar
	   (cons (list (if formatter (funcall formatter type args) (car args))
		       latexinfo-last-node)
		 (symbol-value indexvar))))))

(defun latexinfo-end-defun ()
  (setq fill-column (+ fill-column 5))
  (latexinfo-discard-command)
  (let ((start (nth 1 (latexinfo-pop-stack 'defun))))
    (latexinfo-do-itemize start)
    ;; Delete extra newline inserted after header.
    (save-excursion
      (goto-char start)
      (delete-char -1))))

(put 'deffn 'latexinfo-format 'latexinfo-format-defun)
(put 'deffnx 'latexinfo-format 'latexinfo-format-defunx)
(put 'deffn 'latexinfo-end 'latexinfo-end-defun)
(put 'deffn 'latexinfo-defun-type 'arg)
(put 'deffnx 'latexinfo-defun-type 'arg)
(put 'deffn 'latexinfo-defun-index 'latexinfo-findex)
(put 'deffnx 'latexinfo-defun-index 'latexinfo-findex)

(put 'defun 'latexinfo-format 'latexinfo-format-defun)
(put 'defunx 'latexinfo-format 'latexinfo-format-defunx)
(put 'defun 'latexinfo-end 'latexinfo-end-defun)
(put 'defun 'latexinfo-defun-type "Function")
(put 'defunx 'latexinfo-defun-type "Function")
(put 'defun 'latexinfo-defun-index 'latexinfo-findex)
(put 'defunx 'latexinfo-defun-index 'latexinfo-findex)

(put 'defmac 'latexinfo-format 'latexinfo-format-defun)
(put 'defmacx 'latexinfo-format 'latexinfo-format-defunx)
(put 'defmac 'latexinfo-end 'latexinfo-end-defun)
(put 'defmac 'latexinfo-defun-type "Macro")
(put 'defmacx 'latexinfo-defun-type "Macro")
(put 'defmac 'latexinfo-defun-index 'latexinfo-findex)
(put 'defmacx 'latexinfo-defun-index 'latexinfo-findex)

(put 'defspec 'latexinfo-format 'latexinfo-format-defun)
(put 'defspecx 'latexinfo-format 'latexinfo-format-defunx)
(put 'defspec 'latexinfo-end 'latexinfo-end-defun)
(put 'defspec 'latexinfo-defun-type "Special form")
(put 'defspecx 'latexinfo-defun-type "Special form")
(put 'defspec 'latexinfo-defun-index 'latexinfo-findex)
(put 'defspecx 'latexinfo-defun-index 'latexinfo-findex)

(put 'defvr 'latexinfo-format 'latexinfo-format-defun)
(put 'defvrx 'latexinfo-format 'latexinfo-format-defunx)
(put 'defvr 'latexinfo-end 'latexinfo-end-defun)
(put 'defvr 'latexinfo-defun-type 'arg)
(put 'defvrx 'latexinfo-defun-type 'arg)
(put 'defvr 'latexinfo-defun-index 'latexinfo-vindex)
(put 'defvrx 'latexinfo-defun-index 'latexinfo-vindex)

(put 'defvar 'latexinfo-format 'latexinfo-format-defun)
(put 'defvarx 'latexinfo-format 'latexinfo-format-defunx)
(put 'defvar 'latexinfo-end 'latexinfo-end-defun)
(put 'defvar 'latexinfo-defun-type "Variable")
(put 'defvarx 'latexinfo-defun-type "Variable")
(put 'defvar 'latexinfo-defun-index 'latexinfo-vindex)
(put 'defvarx 'latexinfo-defun-index 'latexinfo-vindex)

(put 'defopt 'latexinfo-format 'latexinfo-format-defun)
(put 'defoptx 'latexinfo-format 'latexinfo-format-defunx)
(put 'defopt 'latexinfo-end 'latexinfo-end-defun)
(put 'defopt 'latexinfo-defun-type "User Option")
(put 'defoptx 'latexinfo-defun-type "User Option")
(put 'defopt 'latexinfo-defun-index 'latexinfo-vindex)
(put 'defoptx 'latexinfo-defun-index 'latexinfo-vindex)

(put 'deftp 'latexinfo-format 'latexinfo-format-defun)
(put 'deftpx 'latexinfo-format 'latexinfo-format-defunx)
(put 'deftp 'latexinfo-end 'latexinfo-end-defun)
(put 'deftp 'latexinfo-defun-type 'arg)
(put 'deftpx 'latexinfo-defun-type 'arg)
(put 'deftp 'latexinfo-defun-index 'latexinfo-tindex)
(put 'deftpx 'latexinfo-defun-index 'latexinfo-tindex)

;;; Object-oriented stuff is a little hairier.

(put 'defop 'latexinfo-format 'latexinfo-format-defun)
(put 'defopx 'latexinfo-format 'latexinfo-format-defunx)
(put 'defop 'latexinfo-end 'latexinfo-end-defun)
(put 'defop 'latexinfo-defun-type 'arg)
(put 'defopx 'latexinfo-defun-type 'arg)
(put 'defop 'latexinfo-defun-format-type 'latexinfo-format-defop-type)
(put 'defopx 'latexinfo-defun-format-type 'latexinfo-format-defop-type)
(put 'defop 'latexinfo-defun-index 'latexinfo-findex)
(put 'defopx 'latexinfo-defun-index 'latexinfo-findex)
(put 'defop 'latexinfo-defun-format-index 'latexinfo-format-defop-index)
(put 'defopx 'latexinfo-defun-format-index 'latexinfo-format-defop-index)

(put 'defmethod 'latexinfo-format 'latexinfo-format-defun)
(put 'defmethodx 'latexinfo-format 'latexinfo-format-defunx)
(put 'defmethod 'latexinfo-end 'latexinfo-end-defun)
(put 'defmethod 'latexinfo-defun-type "Operation")
(put 'defmethodx 'latexinfo-defun-type "Operation")
(put 'defmethod 'latexinfo-defun-format-type 'latexinfo-format-defop-type)
(put 'defmethodx 'latexinfo-defun-format-type 'latexinfo-format-defop-type)
(put 'defmethod 'latexinfo-defun-index 'latexinfo-findex)
(put 'defmethodx 'latexinfo-defun-index 'latexinfo-findex)
(put 'defmethod 'latexinfo-defun-format-index 'latexinfo-format-defop-index)
(put 'defmethodx 'latexinfo-defun-format-index 'latexinfo-format-defop-index)

(defun latexinfo-format-defop-type (type args)
  (format "%s on %s" type (car args)))

(defun latexinfo-format-defop-index (type args)
  (format "%s on %s" (car (cdr args)) (car args)))

(put 'defcv 'latexinfo-format 'latexinfo-format-defun)
(put 'defcvx 'latexinfo-format 'latexinfo-format-defunx)
(put 'defcv 'latexinfo-end 'latexinfo-end-defun)
(put 'defcv 'latexinfo-defun-type 'arg)
(put 'defcvx 'latexinfo-defun-type 'arg)
(put 'defcv 'latexinfo-defun-format-type 'latexinfo-format-defcv-type)
(put 'defcvx 'latexinfo-defun-format-type 'latexinfo-format-defcv-type)
(put 'defcv 'latexinfo-defun-index 'latexinfo-vindex)
(put 'defcvx 'latexinfo-defun-index 'latexinfo-vindex)
(put 'defcv 'latexinfo-defun-format-index 'latexinfo-format-defcv-index)
(put 'defcvx 'latexinfo-defun-format-index 'latexinfo-format-defcv-index)

(put 'defivar 'latexinfo-format 'latexinfo-format-defun)
(put 'defivarx 'latexinfo-format 'latexinfo-format-defunx)
(put 'defivar 'latexinfo-end 'latexinfo-end-defun)
(put 'defivar 'latexinfo-defun-type "Instance variable")
(put 'defivarx 'latexinfo-defun-type "Instance variable")
(put 'defivar 'latexinfo-defun-format-type 'latexinfo-format-defcv-type)
(put 'defivarx 'latexinfo-defun-format-type 'latexinfo-format-defcv-type)
(put 'defivar 'latexinfo-defun-index 'latexinfo-vindex)
(put 'defivarx 'latexinfo-defun-index 'latexinfo-vindex)
(put 'defivar 'latexinfo-defun-format-index 'latexinfo-format-defcv-index)
(put 'defivarx 'latexinfo-defun-format-index 'latexinfo-format-defcv-index)

(defun latexinfo-format-defcv-type (type args)
  (format "%s of %s" type (car args)))

(defun latexinfo-format-defcv-index (type args)
  (format "%s of %s" (car (cdr args)) (car args)))

;; process included files
(put 'include 'latexinfo-format 'latexinfo-format-include)
(defun latexinfo-format-include ()
  (let ((filename (latexinfo-parse-arg-discard))
	(default-directory input-directory)
	subindex)
    (setq subindex
	  (save-excursion
	    (progn (find-file
		    (cond ((file-readable-p (concat filename ".texinfo"))
			   (concat filename ".texinfo"))
			  ((file-readable-p (concat filename ".tex"))
			   (concat filename ".tex"))
			  ((file-readable-p filename)
			   filename)
			  (t (error "\\include'd file %s not found"
				    filename))))
		   (latexinfo-format-buffer-1))))
    (latexinfo-subindex 'latexinfo-vindex (car subindex) (nth 1 subindex))
    (latexinfo-subindex 'latexinfo-findex (car subindex) (nth 2 subindex))
    (latexinfo-subindex 'latexinfo-cindex (car subindex) (nth 3 subindex))
    (latexinfo-subindex 'latexinfo-pindex (car subindex) (nth 4 subindex))
    (latexinfo-subindex 'latexinfo-tindex (car subindex) (nth 5 subindex))
    (latexinfo-subindex 'latexinfo-kindex (car subindex) (nth 6 subindex))))

(defun latexinfo-subindex (indexvar file content)
  (set indexvar (cons (list 'recurse file content)
		      (symbol-value indexvar))))


;; Lots of bolio constructs do nothing in latexinfo.

(put 'c 'latexinfo-format 'latexinfo-discard-line-with-args)
(put 'comment 'latexinfo-format 'latexinfo-discard-line-with-args)
;;(put 'setchapternewpage 'latexinfo-format 'latexinfo-discard-line-with-args)
(put 'nopara 'latexinfo-format 'latexinfo-discard-line-with-args)
(put 'noindent 'latexinfo-format 'latexinfo-discard-line-with-args)
;;(put 'setx 'latexinfo-format 'latexinfo-discard-line-with-args)
;;(put 'setq 'latexinfo-format 'latexinfo-discard-line-with-args)
;;(put 'settitle 'latexinfo-format 'latexinfo-discard-line-with-args)
;;(put 'defindex 'latexinfo-format 'latexinfo-discard-line-with-args)
(put 'synindex 'latexinfo-format 'latexinfo-discard-line-with-args)
(put 'hsize 'latexinfo-format 'latexinfo-discard-line-with-args)
(put 'parindent 'latexinfo-format 'latexinfo-discard-line-with-args)
(put 'lispnarrowing 'latexinfo-format 'latexinfo-discard-line-with-args)
(put 'itemindent 'latexinfo-format 'latexinfo-discard-line-with-args)
(put 'headings 'latexinfo-format 'latexinfo-discard-line-with-args)
(put 'smallbook 'latexinfo-format 'latexinfo-discard-line)

(defun latexinfo-discard-line-with-args ()
  (goto-char latexinfo-command-start)
  (delete-region (point) (progn (forward-line 1) (point))))

;; Sort an index which is in the current buffer between START and END.
;; Used on VMS, where the `sort' utility is not available.
(defun latexinfo-sort-region (start end)
  (require 'sort)
  (save-restriction
    (narrow-to-region start end)
    (sort-subr nil 'forward-line 'end-of-line 'latexinfo-sort-startkeyfun)))

;; Subroutine for sorting an index.
;; At start of a line, return a string to sort the line under.
(defun latexinfo-sort-startkeyfun ()
  (let ((line
	 (buffer-substring (point) (save-excursion (end-of-line) (point)))))
    ;; Canonicalize whitespace and eliminate funny chars.
    (while (string-match "[ \t][ \t]+\\|[^a-z0-9 ]+" line)
      (setq line (concat (substring line 0 (match-beginning 0))
			 " "
			 (substring line (match-end 0) (length line)))))
    line))

;; Some cannot be handled - just ignore them and junk the line

(defun latexinfo-unsupported ()
  (latexinfo-parse-noarg)
  )

(defun batch-latexinfo-format ()
  "Runs  latexinfo-format-buffer  on the files remaining on the command line.
Must be used only with -batch, and kills emacs on completion.
Each file will be processed even if an error occurred previously.
For example, invoke
  \"emacs -batch -funcall batch-latexinfo-format $docs/ ~/*.tex\"."
  (if (not noninteractive)
      (error "batch-latexinfo-format may only be used -batch."))
  (let ((version-control t)
	(auto-save-default nil)
	(find-file-run-dired nil)
	(kept-old-versions 259259)
	(kept-new-versions 259259))
    (let ((error 0)
	  file
	  (files ()))
      (while command-line-args-left
	(setq file (expand-file-name (car command-line-args-left)))
	(cond ((not (file-exists-p file))
	       (message ">> %s does not exist!" file)
	       (setq error 1
		     command-line-args-left (cdr command-line-args-left)))
	      ((file-directory-p file)
	       (setq command-line-args-left
		     (nconc (directory-files file)
			    (cdr command-line-args-left))))
	      (t
	       (setq files (cons file files)
		     command-line-args-left (cdr command-line-args-left)))))
      (while files
	(setq file (car files)
	      files (cdr files))
	(condition-case err
	    (progn
	      (if buffer-file-name (kill-buffer (current-buffer)))
	      (find-file file)
	      (buffer-flush-undo (current-buffer))
	      (set-buffer-modified-p nil)
	      (latexinfo-mode)
	      (message "Latexinfo formatting %s..." file)
	      (latexinfo-format-buffer nil)
	      (if (buffer-modified-p)
		  (progn (message "Saving modified %s" (buffer-file-name))
			 (save-buffer))))
	  (error
	   (message ">> Error: %s" (prin1-to-string err))
	   (message ">>  point at")
	   (let ((s (buffer-substring (point)
				      (min (+ (point) 100)
					   (point-max))))
		 (tem 0))
	     (while (setq tem (string-match "\n+" s tem))
	       (setq s (concat (substring s 0 (match-beginning 0))
			       "\n>>  "
			       (substring s (match-end 0)))
		     tem (1+ tem)))
	     (message ">>  %s" s)))
	  (setq error 1)))
      (kill-emacs error))))

;;; LaTeX additions

(defun latexinfo-run-documentstyle-hooks ()
  (goto-char (point-min))
  (search-forward "\\documentstyle")
  (if (looking-at "\\[")
      (let ((begin (1+ (point)))
	    (end (save-excursion (search-forward "]") (point)))
	    (options-list nil))
	(if (null latexinfo-formats-directory)
	    (setq latexinfo-formats-directory default-directory))
	(while (re-search-forward ",\\|]" end t)
	  (setq options-list (cons (buffer-substring begin (1- (point)))
				   options-list))
	  (setq begin (point)))
	(setq options-list (nreverse options-list))
	(while options-list
	  (let ((option (car options-list))
		(filename nil))
	    (if (not (memq (intern option) latexinfo-known-document-styles))
		(progn
		  (message "Checking formatting option %s" option) (sit-for 2)
		  (setq filename (concat 
				  (file-name-as-directory
				   latexinfo-formats-directory)
				  option "-fmt"))
		  (setq filename
			 (cond
			  ((file-readable-p (concat filename ".elc"))
			   (concat filename ".elc"))
			  ((file-readable-p (concat filename ".el"))
			   (concat filename ".el"))
			  (t nil)))
		  (if filename
		      (progn
			(message "Loading file %s..." filename) (sit-for 1)
			(load-file filename)
			(let ((option-symbol
			       (intern (concat option "-fmt-hook"))))
			  (if (fboundp option-symbol)
			    (progn
			        (message
				 (concat "Running " option
					 "formatting hooks..."))
				 (sit-for 1)
				 (funcall option-symbol))))
			(message "Done loading file %s" filename) (sit-for 1))
		    ))))
	  (setq options-list (cdr options-list))))))

(defun latexinfo-parse-noarg ()
   (delete-region latexinfo-command-start latexinfo-command-end)
   (cond ((looking-at "{")
	  (delete-region (point)
			 (save-excursion
			   (forward-sexp 1) (point))))
	;; TeX gobbles the next whitespace.
	 ((or (eolp) (looking-at "[ \t\n]"))
	  (delete-region (point)
			 (save-excursion
			   (skip-chars-forward " \t\n") (point)))))
    )

(put 'back 'latexinfo-format 'latexinfo-format-backslash)
(defun latexinfo-format-backslash ()
  (latexinfo-discard-command)
  ;; TeX gobbles the next whitespace.
  (if (looking-at "[ \t\n]")
      (delete-region (point)
		     (save-excursion
		       (skip-chars-forward " \t\n")
		       (point))))
  (insert ?\\))

(put 'same 'latexinfo-format 'latexinfo-discard-command)
(put 'same 'latexinfo-end 'latexinfo-discard-command)

(put 'cite 'latexinfo-format 'latexinfo-format-cite)
(defun latexinfo-format-cite ()
  (if (looking-at "[\\[]")
      (save-excursion
	(let ((here (point)) (str nil))
	      (forward-sexp 1)
	      (setq str (buffer-substring (1+ here) (- (point) 1)))
	      (delete-region here (point))
	      (if (eolp) (delete-char 1))
	      (insert "[" (latexinfo-parse-arg-discard) ", " str "]")))
    (insert "[" (latexinfo-parse-arg-discard) "]"))
  (goto-char latexinfo-command-start))

(put 'hfill 'latexinfo-format 'latexinfo-format-hfill)
(defun latexinfo-format-hfill ()
  (latexinfo-parse-arg-discard)
  (insert-char ?\  (- fill-column
		      (save-excursion
			(end-of-line 1)
			(current-column)))))


(put 'S 'latexinfo-format 'latexinfo-format-S)
(defun latexinfo-format-S ()
  (latexinfo-parse-noarg)
  (insert "Section"))

(put 'LaTeX 'latexinfo-format 'latexinfo-format-LaTeX)
(defun latexinfo-format-LaTeX ()
  (latexinfo-parse-noarg)
  (insert "LaTeX"))

(put 'arrow 'latexinfo-format 'latexinfo-format-arrow)
(defun latexinfo-format-arrow ()
  (latexinfo-parse-noarg)
  (insert "=> "))

(put 'leq 'latexinfo-format 'latexinfo-format-leq)
(defun latexinfo-format-leq ()
  (latexinfo-parse-noarg)
  (insert "<="))

(put 'geq 'latexinfo-format 'latexinfo-format-geq)
(defun latexinfo-format-geq ()
  (latexinfo-parse-noarg)
  (insert "<="))

(put 'pi 'latexinfo-format 'latexinfo-format-pi)
(defun latexinfo-format-pi ()
  (latexinfo-parse-noarg)
  (insert "pi"))

(put 'quad 'latexinfo-format 'latexinfo-format-quad)
(defun latexinfo-format-quad ()
  (latexinfo-parse-noarg)
  (insert "    "))

(put 'qquad 'latexinfo-format 'latexinfo-format-qquad)
(defun latexinfo-format-qquad ()
  (latexinfo-parse-noarg)
  (insert "      "))

(put 'pm 'latexinfo-format 'latexinfo-format-pm)
(defun latexinfo-format-pm ()
  (latexinfo-parse-noarg)
  (insert "+/-"))

(put '& 'latexinfo-format 'latexinfo-format-&)
(defun latexinfo-format-& ()
  (latexinfo-parse-noarg)
  (insert "&"))

;; LaTeX noops
(put 'DOTS 'latexinfo-format 'latexinfo-format-dots)
(put 'clearpage 'latexinfo-format 'latexinfo-discard-line-with-args)
(put 'cleardoublepage 'latexinfo-format 'latexinfo-discard-line-with-args)
(put 'tableofcontents 'latexinfo-format 'latexinfo-discard-line-with-args)
(put 'protect 'latexinfo-format 'latexinfo-parse-noarg)
(put 'vspace 'latexinfo-format 'latexinfo-format-noop)
(put 'pagestyle 'latexinfo-format 'latexinfo-discard-line-with-args)
(put 'pagenumbering 'latexinfo-format 'latexinfo-discard-line-with-args)
(put 'markboth 'latexinfo-format 'latexinfo-discard-line-with-args)
(put 'label 'latexinfo-format 'latexinfo-discard-line-with-args)
(put 'index 'latexinfo-format 'latexinfo-format-noop)
(put 'cpsubindex 'latexinfo-format 'latexinfo-discard-line-with-args)

(provide 'latexinfo)


