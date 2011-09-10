(defconst latexinfo-structure-id
	  "$Id: latexinfo-structure.el,v 1.4 1992/01/23 19:03:10 rjc Exp $")

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;                                                                  ;;
 ;;                                                                  ;;
 ;;         Code to add structure to a latexinfo document.           ;;
 ;;                                                                  ;;
 ;;                 Comments to Richard Caley.                       ;;
 ;;                     (rjc@cstr.ed.ac.uk)                          ;;
 ;;                                                                  ;;
 ;;                                                                  ;;
 ;; Some code to do the hard work of making the \node entries and    ;;
 ;; menus for latexinfo automagically. It understands comments       ;;
 ;; placed below sectioning commands to be the text to be inserted   ;;
 ;; into the menu. Thus:                                             ;;
 ;;                                                                  ;;
 ;;     \subsection{Tweaking Dongles}                                ;;
 ;;         \c How to tweak dongles for fun and profit.              ;;
 ;;                                                                  ;;
 ;; Would produce a menu entry like:                                 ;;
 ;;                                                                  ;;
 ;;  * Tweaking Dongles:: How to tweak dongles for fun and profit.   ;;
 ;;                                                                  ;;
 ;; This code ignores the first \node command in a file since it     ;;
 ;; does not know how you want it connected to other info files.     ;;
 ;; You will have to create that one yourself.                       ;;
 ;;                                                                  ;;
 ;; Obviously this doesn't do all of the work for you, for           ;;
 ;; instance it doesn't do anything about cross references.          ;;
 ;; However it is a good way of getting an initial latexinfo file    ;;
 ;; from which you can go on to do things properly. My strategy is   ;;
 ;; to do a rough draft almost as if writing a normal LaTeX          ;;
 ;; document, but using short section headings suitable for node     ;;
 ;; names. Then create the basic structure with latexinfo-structure  ;;
 ;; and work on from there.                                          ;;
 ;;                                                                  ;;
 ;; I think it is fairly stable, but since it has only had to eat    ;;
 ;; my LaTeX habbits there may be things it doesn't understand.      ;;
 ;;                                                                  ;;
 ;; Many thanks to Hannes Faestermann for finding and reporting      ;;
 ;; some of the sillier bugs.                                        ;;
 ;;                                                                  ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; LCD Archive Entry:
;; latexinfo-structure|Richard Caley|R.Caley@ed.ac.uk|
;; Make latexinfo node and menu entries automatically|
;; 92-01-23|1.4|~/misc/latexinfo-structure.el.Z|

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; 
 ;; $Log: latexinfo-structure.el,v $
 ;; Revision 1.4  1992/01/23  19:03:10  rjc
 ;; Added the LCD header.
 ;;
 ;; Revision 1.3  1991/08/05  04:46:17  rjc
 ;; Test for LaTeX commands in latexinfo-detex-string so that
 ;; 	the full code is not called for trivial cases
 ;; Put another layer of code in to delete commas from node names.
 ;; Fixed the regular expression matching section titles.
 ;; Added some comments to say what it is doing.
 ;;
 ;; 25th July 1991
 ;; Added latexinfo-detex-string to change subjects and commentry into
 ;;	a form suitable for node names.
 ;; Changed that initial depth to 0 rather than 1, it was confused by
 ;;	\chapters.
 ;; Replaced (line-number) with a call to count-lines.
 ;; 24th July 1991
 ;; Changed looking-at-string to loking-at since the former is not a 
 ;;	standard function.
 ;; Had forgotten to rename the recursive call to build-section-subtree.
 ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'latexinfo)

(defvar latexinfo-section-commands
	'("chapter"
	  "section"
	  "subsection"
	  "subsubsection"))

(defun latexinfo-looking-at-level ()
       "Returns the level number of the section command we are looking at, 
if any"

       (save-excursion
	(if (not (equal (char-after (point)) ?\\ ))
	    nil
	    (let ((l latexinfo-section-commands)
		  (i 1)
		  (c nil)
		  )
		 (forward-char 1)
		
		 (while (and (not c) l)
			(if (looking-at (car l))
			    (setq c i)
			    )
			(setq i (1+ i))
			(setq l (cdr l))
			)
		 c
		 )
	    )
	)
       )

(defun latexinfo-extract-name ()

       "extract the name from the section command here."

       (save-excursion
	(if (re-search-forward "{\\([^\n]*\\)}[^\n}]*$" nil t)
	    (buffer-substring (match-beginning 1) (match-end 1))
	    "none"
	    )
	)
       )

(defun latexinfo-extract-commentry ()

       "Extract any comment following the current line."

       (save-excursion
	(let ((c "") s)
	     (forward-line 1)
	     (beginning-of-line)
	     (while (eolp)
		    (forward-line 1)
		    )
	
	     (while (looking-at "[ \t]*\\\\c[ \t]")
		    (re-search-forward "[ \t]*\\\\c[ \t]+" nil t)
		    (setq c (concat c " " (buffer-substring (point) 
							(save-excursion
							 (end-of-line)
							 (point)))))
		    (forward-line 1)
		    (beginning-of-line)
		    )
	     c
	     )
	)
       )

(defun latexinfo-detex-string (string)

       "remove latex commands from string"

       (if (string-match "[\\{}]" string)
	   (let ((b (get-buffer-create " latexinfo-tmp-buffer"))
		 latexinfo-command-start
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
		(save-excursion
		 (set-buffer b)
		 (insert string)
		 (goto-char (point-min))
		 (latexinfo-format-scan-noverbatim)
		 (setq string (buffer-substring (point-min) (point-max)))
		 (kill-buffer b)
		 )
		string
		)
	   string
	   )
       )
	

(defun latexinfo-heading-to-node (heading)
       "Massage a section heading into a node name"

       (let ((raw (latexinfo-detex-string heading))
	     b)
	    (if (string-match "," raw)
		(progn
		 (setq b (get-buffer-create " latexinfo-tmp-buffer"))
		 (save-excursion
		  (set-buffer b)
		  (erase-buffer)
		  (insert raw)
		  (subst-char-in-region (point-min) (point-max) 
					?, ?;
					t)
		  (buffer-substring (point-min) (point-max))
		  )
		 )
		raw
		)
	    )
       )
	       
		       
(defun latexinfo-build-section-subtree (depth)
       "Build a tree representing the structure of the
document as given by the sectioning commands."

       (let ((subtree nil) 
	     (finished nil)
	     it)

	    (while (and (not finished) (not (eobp)))
		    (if (setq it (latexinfo-looking-at-level))
			(if (<= it depth)
			    (setq finished t)
			    (setq subtree 
				  (cons
				   (list (set-marker (make-marker) (point))
					 (1+ (count-lines (point-min) (point)))
					 (latexinfo-heading-to-node (latexinfo-extract-name))
					 (latexinfo-detex-string (latexinfo-extract-commentry))
					 (progn
					  (forward-line 1)
					  (latexinfo-build-section-subtree it)
					  )
					 )
				   subtree))
			    )
			(forward-line 1)
			)
		    )
	    (reverse subtree)
	    )
       )

(defun latexinfo-build-section-tree ()
       "Build a tree describing the section structure of BUFFER."

       (save-excursion
	(beginning-of-buffer)
	(latexinfo-build-section-subtree 0)
	)
       )

(defun latexinfo-insert-structure (nodes parent)
       "Insert the \\node commands into the buffer."

       (save-excursion
	(let (nd
	      (nds nodes)
	      (n nil)
	      (p "\t")
	      (children nil)
	      m
	      )
	(while nds
	       (setq nd (car nds))
	       (setq n (if (cdr nds)
			   (car (cdr nds))
			   nil))
	       (goto-char (car nd))
	       (open-line 2)
	       (insert "\\node "
		       (nth 2 nd) "," 
		       (if n (nth 2 n) "\t") ","
		       p ","
		       parent)
	       (setq children (cons (cons (nth 2 nd) (nth 3 nd)) children))
	       (latexinfo-insert-structure (nth 4 nd) (nth 2 nd))
	       (setq p (nth 2 nd))
	       (setq nds (cdr nds))
	       )
	(if children
	    (progn
	     (setq children (reverse children))
	     (goto-char (car (car nodes)))
	     (forward-line -2)
	     (open-line 2)
	     (insert "\\begin{menu}\n")
	     (while children
		    (insert "* " (car (car children)) 
			    "::") 
		    (insert-char ? (- 20 (length (car (car children)))))
		    (insert (cdr (car children)) "\n")
		    (setq children (cdr children))
		    )
	     (insert "\\end{menu}")
	     )
	    )
	)
	)
       )

(defun latexinfo-delete-structure (buffer)
       "Remove node and meny commands from a buffer."

       (interactive "bDelete structure from which buffer? ")

       (save-excursion
	(set-buffer buffer)
	(beginning-of-buffer)
	(re-search-forward "\\\\node\\|\\\\begin{menu}\\|\\\\end{menu}" nil t)
	(beginning-of-line)
	(if (looking-at "\\\\node")
	    (progn
	     (forward-line 1)
	    (re-search-forward "\\\\node\\|\\\\begin{menu}\\|\\\\end{menu}" nil t)
	    )
	    )
	(beginning-of-line)
	(while (re-search-forward "\\\\node\\|\\\\begin{menu}\\|\\\\end{menu}" nil t)
	       (beginning-of-line)
	       (if (looking-at "\\\\begin{menu}")
		   (while (not (looking-at "\\\\end{menu}"))
			  (kill-line 1)
			  )
		   )
	       (kill-line 1)
	       (delete-blank-lines)
	       )
	)
       )
       

		

(defun latexinfo-structure (buffer)
       "Insert a basic structure into the document based
on the sectioning commands."

       (interactive "bAdd structure to which buffer? ")

       (save-excursion
	(set-buffer buffer)
	(if (or  (re-search-forward "\\\\begin{menu}\\|\\\\end{menu}" nil t)
		 (and 
		  (search-forward "\\node" nil t)
		  (search-forward "\\node" nil t)))
	    (error "There is already structure present")
	    )
	(let ( tree )
	     (message "building tree...")
	     (setq tree (latexinfo-build-section-tree))
	     (message "inserting structure...")
	     (latexinfo-insert-structure tree "Top")
	     (message "done")
	     )
	)
       )



