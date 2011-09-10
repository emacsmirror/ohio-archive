From apple!bloom-beacon!tut.cis.ohio-state.edu!ukma!uflorida!novavax!weiner@bbn.com Fri May 19 14:35:37 1989
To: unix-emacs@bbn.com
Date: 28 Apr 89 06:15:35 GMT
From: Bob Weiner <apple!bloom-beacon!tut.cis.ohio-state.edu!ukma!uflorida!novavax!weiner@bbn.com>
Sender: arpa-unix-emacs-request@bbn.com
Subject: informat.el, extended to accept command reference (menus) syntax
Organization: Nova University, Fort Lauderdale, FL
Source-Info:  From (or Sender) name not authenticated.

;; Info support functions package for Emacs
;; Copyright (C) 1986 Free Software Foundation, Inc.

;; Bob Weiner, Motorola, Inc., 3/29/89
;;  Added 'Info-drop-nodename-chars' function and modified
;;  'Info-validate-node-name' to support the validation of execute-only node
;;  references and execute-only references without associated nodes.
;;  See 'info.el' for more information.

;; This file is part of GNU Emacs.

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

(require 'info)

(defun Info-tagify ()
  "Create or update Info-file tag table in current buffer."
  (interactive)
  ;; Save and restore point and restrictions.
  ;; save-restrictions would not work
  ;; because it records the old max relative to the end.
  ;; We record it relative to the beginning.
  (let ((omin (point-min))
	(omax (point-max))
	(nomax (= (point-max) (1+ (buffer-size))))
	(opoint (point)))
    (unwind-protect
	(progn
	  (widen)
	  (goto-char (point-min))
	  (if (search-forward "\^_\nIndirect:\n" nil t)
	      (message "Cannot tagify split info file")
	    (let ((regexp "Node:[ \t]*\\([^,\n\t]\\)*[,\t\n]")
		  (case-fold-search t)
		  list)
	      (while (search-forward "\n\^_" nil t)
		(forward-line 1)
		(let ((beg (point)))
		  (forward-line 1)
		  (if (re-search-backward regexp beg t)
		      (setq list
			    (cons (list (buffer-substring
					  (match-beginning 1)
					  (match-end 1))
					beg)
				  list)))))
	      (goto-char (point-max))
	      (forward-line -8)
	      (let ((buffer-read-only nil))
		(if (search-forward "\^_\nEnd tag table\n" nil t)
		    (let ((end (point)))
		      (search-backward "\nTag table:\n")
		      (beginning-of-line)
		      (delete-region (point) end)))
		(goto-char (point-max))
		(insert "\^_\f\nTag table:\n")
		(move-marker Info-tag-table-marker (point))
		(setq list (nreverse list))
		(while list
		  (insert "Node: " (car (car list)) ?\177)
		  (princ (car (cdr (car list))) (current-buffer))
		  (insert ?\n)
		  (setq list (cdr list)))
		(insert "\^_\nEnd tag table\n")))))
      (goto-char opoint)
      (narrow-to-region omin (if nomax (1+ (buffer-size))
			       (min omax (point-max)))))))

(defun Info-split ()
  "Split an info file into an indirect file plus bounded-size subfiles.
Each subfile will be up to 50000 characters plus one node.

To use this command, first visit a large Info file that has a tag table.
The buffer is modified into a (small) indirect info file
which should be saved in place of the original visited file.

The subfiles are written in the same directory the original file is in,
with names generated by appending `-' and a number to the original file name.

The indirect file still functions as an Info file, but it contains
just the tag table and a directory of subfiles."
  (interactive)
  (if (< (buffer-size) 70000)
      (error "This is too small to be worth splitting"))
  (goto-char (point-min))
  (search-forward "\^_")
  (forward-char -1)
  (let ((start (point))
	(chars-deleted 0)
	subfiles
	(subfile-number 1)
	(case-fold-search t)
	(filename (file-name-sans-versions buffer-file-name)))
    (goto-char (point-max))
    (forward-line -8)
    (setq buffer-read-only nil)
    (or (search-forward "\^_\nEnd tag table\n" nil t)
	(error "Tag table required; use M-x Info-tagify"))
    (search-backward "\nTag table:\n")
    (if (looking-at "\nTag table:\n\^_")
	(error "Tag table is just a skeleton; use M-x Info-tagify"))
    (beginning-of-line)
    (forward-char 1)
    (save-restriction
      (narrow-to-region (point-min) (point))
      (goto-char (point-min))
      (while (< (1+ (point)) (point-max))
	(goto-char (min (+ (point) 50000) (point-max)))
	(search-forward "\^_" nil 'move)
	(setq subfiles
	      (cons (list (+ start chars-deleted)
			  (concat (file-name-nondirectory filename)
				  (format "-%d" subfile-number)))
		    subfiles))
	;; Put a newline at end of split file, to make Unix happier.
	(insert "\n")
	(write-region (point-min) (point)
		      (concat filename (format "-%d" subfile-number)))
	(delete-region (1- (point)) (point))
	;; Back up over the final ^_.
	(forward-char -1)
	(setq chars-deleted (+ chars-deleted (- (point) start)))
	(delete-region start (point))
	(setq subfile-number (1+ subfile-number))))
    (while subfiles
      (goto-char start)
      (insert (nth 1 (car subfiles))
	      (format ": %d" (car (car subfiles)))
	      "\n")
      (setq subfiles (cdr subfiles)))
    (goto-char start)
    (insert "\^_\nIndirect:\n")
    (search-forward "\nTag Table:\n")
    (insert "(Indirect)\n")))

(defun Info-validate ()
  "Check current buffer for validity as an Info file.
Check that every node pointer points to an existing node."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (if (search-forward "\nTag table:\n(Indirect)\n" nil t)
	  (error "Don't yet know how to validate indirect info files: \"%s\""
		 (buffer-name (current-buffer))))
      (goto-char (point-min))
      (let ((allnodes '(("*")))
	    (regexp "Node:[ \t]*\\([^,\n\t]*\\)[,\t\n]")
	    (case-fold-search t)
	    (tags-losing nil)
	    (lossages ()))
	(while (search-forward "\n\^_" nil t)
	  (forward-line 1)
	  (let ((beg (point)))
	    (forward-line 1)
	    (if (re-search-backward regexp beg t)
		(let ((name (downcase
			      (buffer-substring
			        (match-beginning 1)
				(progn
				  (goto-char (match-end 1))
				  (skip-chars-backward " \t")
				  (point))))))
		  (if (assoc name allnodes)
		      (setq lossages
			    (cons (list name "Duplicate node-name" nil)
				  lossages))
		      (setq allnodes
			    (cons (list name
					(progn
					  (end-of-line)
					  (and (re-search-backward
						"prev[ious]*:" beg t)
					       (progn
						 (goto-char (match-end 0))
						 (downcase
						   (Info-following-node-name)))))
					beg)
				  allnodes)))))))
	(goto-char (point-min))
	(while (search-forward "\n\^_" nil t)
	  (forward-line 1)
	  (let ((beg (point))
		thisnode next)
	    (forward-line 1)
	    (if (re-search-backward regexp beg t)
		(save-restriction
		  (search-forward "\n\^_" nil 'move)
		  (narrow-to-region beg (point))
		  (setq thisnode (downcase
				   (buffer-substring
				     (match-beginning 1)
				     (progn
				       (goto-char (match-end 1))
				       (skip-chars-backward " \t")
				       (point)))))
		  (end-of-line)
		  (and (search-backward "next:" nil t)
		       (setq next (Info-validate-node-name "invalid Next"))
		       (assoc next allnodes)
		       (if (equal (car (cdr (assoc next allnodes)))
				  thisnode)
			   ;; allow multiple `next' pointers to one node
			   (let ((tem lossages))
			     (while tem
			       (if (and (equal (car (cdr (car tem)))
					       "should have Previous")
					(equal (car (car tem))
					       next))
				   (setq lossages (delq (car tem) lossages)))
			       (setq tem (cdr tem))))
			 (setq lossages
			       (cons (list next
					   "should have Previous"
					   thisnode)
				     lossages))))
		  (end-of-line)
		  (if (re-search-backward "prev[ious]*:" nil t)
		      (Info-validate-node-name "invalid Previous"))
		  (end-of-line)
		  (if (search-backward "up:" nil t)
		      (Info-validate-node-name "invalid Up"))
		  (if (re-search-forward "\n* Menu:" nil t)
		      (while (re-search-forward "\n\\* " nil t)
			(Info-validate-node-name
			  (concat "invalid menu item "
				  (buffer-substring (point)
						    (save-excursion
						      (skip-chars-forward "^:")
						      (point))))
			  (Info-extract-menu-node-name nil))))
		  (goto-char (point-min))
		  (while (re-search-forward "\\*note[ \n]*[^:\t]*:" nil t)
		    (goto-char (+ (match-beginning 0) 5))
		    (skip-chars-forward " \n")
		    (Info-validate-node-name
		     (concat "invalid reference "
			     (buffer-substring (point)
					       (save-excursion
						 (skip-chars-forward "^:")
						 (point))))
		     (Info-extract-menu-node-name
		       "Bad format cross-reference")))))))
	(setq tags-losing (not (Info-validate-tags-table)))
	(if (or lossages tags-losing)
	    (with-output-to-temp-buffer " *problems in info file*"
	      (while lossages
		(princ "In node \"")
		(princ (car (car lossages)))
		(princ "\", ")
		(let ((tem (nth 1 (car lossages))))
		  (cond ((string-match "\n" tem)
			 (princ (substring tem 0 (match-beginning 0)))
			 (princ "..."))
			(t
			 (princ tem))))
		(if (nth 2 (car lossages))
		    (progn
		      (princ ": ")
		      (let ((tem (nth 2 (car lossages))))
			(cond ((string-match "\n" tem)
			       (princ (substring tem 0 (match-beginning 0)))
			       (princ "..."))
			      (t
			       (princ tem))))))
		(terpri)
		(setq lossages (cdr lossages)))
	      (if tags-losing (princ "\nTags table must be recomputed, use Info-tagify.\n")))
	  ;; Here if info file is valid.
	  ;; If we already made a list of problems, clear it out.
	  (save-excursion
	    (if (get-buffer " *problems in info file*")
		(progn
		  (set-buffer " *problems in info file*")
		  (kill-buffer (current-buffer)))))
	  (message "File appears valid"))))))

(defun Info-validate-node-name (kind &optional name)
  (if name
      nil
    (goto-char (match-end 0))
    (skip-chars-forward " \t")
    (if (= (following-char) ?\()
	nil
      (setq name
	    (buffer-substring
	     (point)
	     (progn
	      (skip-chars-forward "^,\t\n")
	      (skip-chars-backward " ")
	      (point))))))
  (if (null name)
      nil
    (let* ((node-name-data (Info-drop-nodename-chars name))
	   (no-node (cdr node-name-data)))
      (setq name (downcase (car node-name-data)))
      (or (and (> (length name) 0) (= (aref name 0) ?\())
	  (if no-node
	      (progn
		(and (assoc name allnodes) ; node found
		     (setq lossages
			   (cons (list thisnode "unused node found " name)
				 lossages)))
		t))
	  (assoc name allnodes)
	  (setq lossages
		(cons (list thisnode kind name) lossages)))))
  name)

(defun Info-drop-nodename-chars (nodename)
  (let ((first-char (substring nodename 0 1)))
    (cond ((string= first-char Info-exec-no-node-ref-char)
	   (cons (substring nodename 1) first-char))
	  ((string= first-char Info-exec-only-node-ref-char)
	   (cons (substring nodename 1) nil))
	  ((cons nodename nil)))))

(defun Info-validate-tags-table ()
  (goto-char (point-min))
  (if (not (search-forward "\^_\nEnd tag table\n" nil t))
      t
    (not (catch 'losing
	   (let* ((end (match-beginning 0))
		  (start (progn (search-backward "\nTag table:\n")
				(1- (match-end 0))))
		  tem)
	     (setq tem allnodes)
	     (while tem
	       (goto-char start)
	       (or (equal (car (car tem)) "*")
		   (search-forward (concat "Node: "
					   (car (car tem))
					   "\177")
				   end t)
		   (throw 'losing 'x))
	       (setq tem (cdr tem)))
	     (goto-char (1+ start))
	     (while (looking-at ".*Node: \\(.*\\)\177\\([0-9]+\\)$")
	       (setq tem (downcase (buffer-substring
				     (match-beginning 1)
				     (match-end 1))))
	       (setq tem (assoc tem allnodes))
	       (if (or (not tem)
		       (< 1000 (progn
				 (goto-char (match-beginning 2))
				 (setq tem (- (car (cdr (cdr tem)))
					      (read (current-buffer))))
				 (if (> tem 0) tem (- tem)))))
		   (throw 'losing 'y)))
	     (forward-line 1))
	   (or (looking-at "End tag table\n")
	       (throw 'losing 'z))
	   nil))))

(defun batch-info-validate ()
  "Runs Info-validate on the files remaining on the command line.
Must be used only with -batch, and kills emacs on completion.
Each file will be processed even if an error occurred previously.
For example, invoke \"emacs -batch -f batch-info-validate $info/ ~/*.info\""
  (if (not noninteractive)
      (error "batch-info-validate may only be used -batch."))
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
	       (setq command-line-args-left (nconc (directory-files file)
					      (cdr command-line-args-left))))
	      (t
	       (setq files (cons file files)
		     command-line-args-left (cdr command-line-args-left)))))
      (while files
	(setq file (car files)
	      files (cdr files))
	(let ((lose nil))
	  (condition-case err
	      (progn
		(if buffer-file-name (kill-buffer (current-buffer)))
		(find-file file)
		(buffer-flush-undo (current-buffer))
		(set-buffer-modified-p nil)
		(fundamental-mode)
		(let ((case-fold-search nil))
		  (goto-char (point-max))
		  (cond ((search-backward "\n\^_\^L\nTag table:\n" nil t)
			 (message "%s already tagified" file))
			((< (point-max) 30000)
			 (message "%s too small to bother tagifying" file))
			(t
			 (message "Tagifying %s..." file)
			 (Info-tagify)
			 (message "Tagifying %s...done" file))))
		(let ((loss-name " *problems in info file*"))
		  (message "Checking validity of info file %s..." file)
		  (if (get-buffer loss-name)
		      (kill-buffer loss-name))
		  (Info-validate)
		  (if (not (get-buffer loss-name))
		      nil ;(message "Checking validity of info file %s... OK" file)
		    (message "----------------------------------------------------------------------")
		    (message ">> PROBLEMS IN INFO FILE %s" file)
		    (save-excursion
		      (set-buffer loss-name)
		      (princ (buffer-substring (point-min) (point-max))))
		    (message "----------------------------------------------------------------------")
		    (setq error 1 lose t)))
		(if (and (buffer-modified-p)
			 (not lose))
		    (progn (message "Saving modified %s" file)
			   (save-buffer))))
	    (error (message ">> Error: %s" (prin1-to-string err))))))
      (kill-emacs error))))



-- 
Bob Weiner, Motorola, Inc.,   USENET:  ...!gatech!uflorida!novavax!weiner
(407) 738-2087


