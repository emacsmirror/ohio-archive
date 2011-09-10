;;; cite-it.el
;;
;; Bibtex entry completion, lookup, verification in LaTeX files for \cite{...}
;;
;; Author  : Jay Sachs <sachs@cs.nyu.edu>
;; Created : 04/08/94
;; Version : 1.12; 04/28/94
;; Keywords: bibtex latex cite
;;
;; LCD Archive Entry:
;; cite-it|Jay Sachs|sachs@cs.nyu.edu|
;; Completion,lookup,verification of BibTeX citations in LaTeX files|
;; 28-Apr-1994|1.12|~/modes/cite-it.el.Z|
;;
;; $Id: cite-it.el,v 1.6 1994/04/29 12:38:54 sachs Exp $
;;
;; Copyright (C) 1994 Jay Sachs
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; 
;; DESCRIPTION
;;   See the documentation for the function cite-it-mode.
;;
;; BUGS AND LIMITATIONS
;;   Please notify Jay Sachs (sachs@cs.nyu.edu) of any problems or
;;   suggestions regarding this package.
;;   - Only known to work with GNU Emacs v19.x 
;;      not under Lucid Emacs 19.x due to some minor-mode stuff
;;   - Rescanning is done brute-force. Smarter rescans would be good:
;;      only rescan bib files that have been added or changed.
;;      can tell which file a particular ref is from. delete the
;;      refs from the alists before scanning the file.
;;   - cite-it will think the text \bibliography{...} in a 
;;     comment or  \verbatim environment is the real thing.
;;     the comment problem exists because the regexp matching is slow;
;;     the verbatim problem exists because its not worth it
;;
;; INSTALLATION:
;;   Put cite-it.el somewhere on your load-path.
;;   Put the following in your .emacs:
;;     (autoload 'cite-it-mode "cite-it" "" t)
;;     (add-hook 'tex-mode-hook 'cite-it-mode)
;;   You can toggle cite-it off and on by
;;     M-x cite-it-mode
;;
;; ACKNOWLEDGEMENTS
;;  Thanks to 
;;    Fumiaki Kamiya for suggestions & support
;;    Philippe Queinnec for the patches to get it to work w/ Lucid Emacs
;;    Hans Chalupsky for several excellent suggestions & pointers.
;;    Everone else who has let me know about bugs, etc.
;;
;; HISTORY
;;  Version 1.04: 04/11/94
;;    - cite-it-insert-cite-and-citation now displays citation
;;    - \nocite{} line is now checked as well
;;    -  bib file buffers aren't killed if they've already been loaded
;;  Version 1.05: 04/12/94
;;    - added cite-it-edit-bib-file
;;    - fixed bug in looking for \cite lines; added cite-it-find-citation
;;    - added: if bibfiles changed on disk, then rescan
;;  Version 1.06: 04/13/94
;;    - fixed bug if try to verify while not on citation
;;    - changed keys; added keymap to C-c C-c
;;    - added help function
;;  Version 1.07: 04/22/94
;;    - now recognizes \bibliography{} line properly if followed by other
;;      commands; also if not first text on line
;;    - added changes to support Lucid Emacs and GnuEmacs 19.16 (at least)
;;    - fixed cite-it-insert-cite-and-citation to insert at point
;;    - minor bug fixes
;;  Version 1.1: 04/27/94
;;    - cite-it is now a minor mode
;;    - cite-it key prefix is a variable
;;    - appropriate cite-it variables are now buffer-local
;;    - utility functions now have cite-it- prefix
;;    - ignores @string{} lines in bib files
;;    - some bug fixes, also speed improvements
;;    - most importantly, now handles master files, as well as extra
;;       bibfiles; works with auctex
;;  Version 1.11: 04/27/94
;;    - more elegant use of keymap deffing using minor-mode-map-alist
;;    - fixed bugs in -set-nth, -insert-nth, and cleaned up -find-citation
;;  Version 1.12: 04/28/94
;;    - fixed omitted cite-it-keymap var declaration
;;    - now handles bib items in bib files delimited by () as well as {}
;;    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; cite-it code starts here
;;;



;;; first some utility functions

(defconst cite-it-path-separater-char ?: 
  "Character used to separate paths in environment variables;
For DOS & OS/2, set this to ;")

(defun cite-it-set-nth (l n x)
  "Changes the Nth element of L to X."
  (and l
       (setcar (nthcdr n l) x)))

(defun cite-it-insert-nth (l n x)
  "Inserts X into L at position N."
  (if (consp l)
      (if (= 0 n)
	  (let ((l2 (cdr l)))
	    (setcdr l (cons (car l) l2))
	    (setcar l x))
	(let ((x1 (nthcdr (+ n 1) l)))
	  (setcdr (nthcdr (- n 1) l) (cons x x1))))))


(defun cite-it-the-match (n)
  (buffer-substring (match-beginning n) (match-end n)))

(defun cite-it-prefix-up-to-nl (s)
  "Returns the prefix of string s up to new line."
  (save-match-data
    (string-match ".*$" s)
    (substring s 0 (match-end 0))))


(defun cite-it-split-string (s &optional c)
  "Converts a string to a list of strings. 
Initial string s is separated by char c, default is \",\"."
  (if (not c) (setq c ?,))
  (save-match-data
    (let ((l nil))
      (while (> (length s) 0)
	(let ((x (string-match (char-to-string c) s)))
	  (setq l (append l (list (substring s 0 x))))
	  (if x (setq s (substring s (+ 1 x)))
	    (setq s "")))) l)))

(defun cite-it-sumstrlen (l n &optional i)
  "Computes running sum of lengths of strings in list l, starting from n.
Adds I to each length; I defults to 1."
  (if (not i) (setq i 1))
  (if l
      (let ((n1 (+ n (length (car l)) i)))
	(cons n1 (cite-it-sumstrlen (cdr l) n1)))
    nil))

(defun cite-it-elemlte (l n)
  "Returns index of element of l that is just greater than n."
  (if l
      (if (<= n (car l)) 0
	(+ 1 (cite-it-elemlte (cdr l) n)))
    0))


;;;
;;; now the start of cite-it proper
;;;

;;; first some user variables

(defvar cite-it-verbose nil 
  "*t means show title and author in separate window;
nil means just display title in minibuffer")

(defvar cite-it-master-file t
  "*Master file for the current LaTeX file. If this is t, and AuCTeX is being
used, cite-it will check the TeX-master variable instead. cite-it uses the
master file to find the \\bibliography{} line.")

(defvar cite-it-other-bibfiles nil
  "*List of additional bibliography files to use.")

(defvar cite-it-show-author nil 
  "*If t, show author in minibuffer; otherwise show title")

(defvar cite-it-check-always nil 
  "*If t, always check for modified bibliography line and files.")

(defvar cite-it-keep-bib-buffers nil 
  "*If non-nil, then don't kill bib file buffers after reading & 
processing them. Otherwise kill them after they're scanned.")


;;;
;;; now some other variables and constants
;;;

(defvar cite-it-citation-list nil "Contains the parsed bibliography entries")

(defvar cite-it-bibfile-list nil "List of bibliography files")

(defvar cite-it-bibfile-alist nil "Bibfile name/modification time alist.")

(defvar cite-it-key-prefix "\C-c\C-t" "*Prefix key(s) for cite-it commands.")
(defvar cite-it-mode nil "")


(defconst cite-it-cite-pos 3)
(defconst cite-it-opt-pos 2)

(defconst cite-it-cite-regexp 
  "\\\\\\(no\\)?cite\\(\\[.*\\]\\)?{\\([^}
]+\\)\\(}\\|$\\)")


;; Kludge:
;; if regexp searching gets faster (or someone comes up w/ a fast one)
;; these could be improved to ignore \bibliography lines in comments
;;  and maybe verbatim environments
(defconst cite-it-bibliography-regexp 
;;  "^\\([^%\n]\\|\\\\%\\)*\\\\bibliography{\\([^}\n]+\\)}")
  "\\\\bibliography{\\([^}\n]+\\)}")
(defconst cite-it-bib-regexp-pos 1)
;;(defconst cite-it-bib-regexp-pos 2)


;;;
;;; now the functions
;;;

(defun cite-it-toggle-author-or-title ()
  (interactive)
  (setq cite-it-show-author (not cite-it-show-author))
  (if cite-it-show-author
      (message "Citations set to display author")
    (message "Citations set to display title")))


(defun cite-it-toggle-verbose ()
  (interactive)
  (setq cite-it-verbose (not cite-it-verbose))
  (if cite-it-verbose
      (message "Citation checking now verbose")
    (message "Citation checking now terse")))

(defun cite-it-show-citation (x)
  (if cite-it-verbose
      (with-output-to-temp-buffer "*Citation Info*"
	(princ (format "Citation: %s\n\n" (car x)))
	(princ (format "Title : %s\n" (car (cdr x))))
	(princ (format "Author: %s\n" (car (cdr (cdr x)))))
	)
    (if cite-it-show-author
	(message (cite-it-prefix-up-to-nl (car (cdr (cdr x)))))
      (message (cite-it-prefix-up-to-nl (car (cdr x)))))))


(defun cite-it-lookup-citation ()
  "Looks up a citation."
  (interactive)
  (if cite-it-check-always (cite-it-rescan-bib-files-if-changed))
  (let ((x (completing-read "Look up citation: " cite-it-citation-list nil t)))
    (if x (cite-it-show-citation (assoc x cite-it-citation-list)))))



(defun cite-it-find-citation (tp)
  "Searches for citation under point. If found, return t; if not,
return nil and move point to tp."
  (or (looking-at cite-it-cite-regexp)
      (and (re-search-forward "}\\|$" nil t)
	   (re-search-backward cite-it-cite-regexp nil t)
	   (> tp (match-beginning 0))
	   (< tp (match-end 0)))
      (progn
	(goto-char tp)
	nil)))

(defun cite-it-insert-cite-and-citation ()
  "Inserts \\cite{} and citation at point."
  (interactive)
  (if cite-it-check-always (cite-it-rescan-bib-files-if-changed))
  (let ((tp (point)))
    (if (cite-it-find-citation tp)
	(cite-it-insert-citation)
      (let ((ct (completing-read "Add \cite{...}: " cite-it-citation-list nil nil)))
	(goto-char tp)
	(insert "\\cite{" ct "}")
	(if (assoc ct cite-it-citation-list)
	    (cite-it-show-citation (assoc ct cite-it-citation-list))
	  (message "Citation %s not found." ct))))))

(defun cite-it-complete-citation ()
  "Prompts for completion of citation at point."
  (interactive)
  (cite-it-complete-or-insert-citation nil nil))

(defun cite-it-insert-citation ()
  "Inserts a citation at point."
  (interactive)
  (cite-it-complete-or-insert-citation t nil))

(defun cite-it-verify-citation ()
  "Checks citation at point; if not found, prompt for replacement."
  (interactive)
  (cite-it-complete-or-insert-citation nil t))


(defun cite-it-complete-or-insert-citation (add-new vonly)
  (if cite-it-check-always (cite-it-rescan-bib-files-if-changed))
  (let ((cl nil) (cls -1) (c nil) (ct nil) (cp -1))
    (save-excursion
      (let ((tp (point)))
	(if (cite-it-find-citation tp)
	    (let ((opt (if (match-beginning cite-it-opt-pos) 
			   (cite-it-the-match cite-it-opt-pos) "")))
	      (setq cl (cite-it-split-string (cite-it-the-match cite-it-cite-pos)))
	      (setq cp (cite-it-sumstrlen cl (- (match-beginning cite-it-cite-pos) 
					(match-beginning 0) 1)))
	      (setq cls (cite-it-elemlte cp (- tp (point))))
	      (setq c (if add-new nil (nth cls cl)))
	      (if (and vonly (assoc c cite-it-citation-list))
		  (progn
		    (cite-it-show-citation (assoc c cite-it-citation-list))
		    (setq ct c))
		(setq ct
		      (completing-read "Citation: " 
				       cite-it-citation-list nil nil c)))
	      (if (not (string-equal c ct))
		  (progn
		    ;; make spot for new citation if appropriate
		    (if (and add-new (< cls (length cl)))
			(cite-it-insert-nth cl cls "")
		      (if (not c) (setq cl (append cl '("")))))
		    ;; read in new citation
		    (cite-it-set-nth cl cls ct)
		    ;; delete \cite{...}
		    (goto-char (match-beginning 0))
		    (delete-char (- (match-end 0) (match-beginning 0)))
		    ;; add changed \cite{...}
		    (insert "\\cite"
			    opt
			    "{"
			    (mapconcat (function (lambda (x) x)) cl ",")
			    "}")
		    ))
	      (if (assoc ct cite-it-citation-list)
		  (cite-it-show-citation (assoc ct cite-it-citation-list))
		(message "Citation %s not found" ct)))
	  (message "No citation near point"))))
    (if (>= cls 0)
	(progn
	  (goto-char (match-beginning 0))
	  (forward-char (nth cls cp))))
    ))

(defun cite-it-verify-multi-citation ()
  "Verifies multiple citations near point."
  (interactive)
  (if cite-it-check-always (cite-it-rescan-bib-files-if-changed))
  (save-excursion
    (let ((tp (point)))
      (if (cite-it-find-citation tp)
	  (progn
	    (set-mark (point))
	    (if (looking-at cite-it-cite-regexp)
		(progn
		  (goto-char (+ 2 (match-end 0)))
		  (cite-it-verify-region))
	      (message "No citation found.")))
	(message "No citation found.")))))

(defun cite-it-get-bib-field (fname start limit)
  (goto-char start)
  (if (re-search-forward (concat "^\\s-*" fname "\\s-*=\\s-*[{\"]") limit t)
      (progn
	;; found it, now find end of it
	(let* ((x1 (point))
	      (c1 (char-after (- x1 1))))
	  (if (if (eq c1 ?{)
		  (re-search-forward "[^\\\\]}" limit t)
		(re-search-forward "[^\\\\]\"" limit t))
	      (backward-char))
	  ;; and grab it
	  (buffer-substring x1 (point))))
    "UNKNOWN"))


(defun cite-it-process-bib-file (citation-list bibfile-alist)
  (let* ((bibkey "")
	 (bibtitle "")
	 (bibauthor "")
	 (x1 0)
	 (x2 0)
	 (cfs case-fold-search))
    (goto-char 0)
    ;; keep track of buffer mod times
    (if (not (assoc (buffer-file-name) bibfile-alist))
	(setq bibfile-alist
	      (cons (cons (buffer-file-name) 
			  (list (nth 5 (file-attributes (buffer-file-name)))))
		    bibfile-alist)))
    (setq case-fold-search t)
    (while (re-search-forward "^[ \\t]*@\\([A-Za-z]+\\)\\((\\|{\\)\\(.+\\)," nil t)
      ;; skip string entries
      (if (string-equal (downcase (cite-it-the-match 1)) "string")
	  nil
	(setq x1 (point))
	(setq bibkey (cite-it-the-match 3))
	;; find limit of bibtex entry
	(re-search-backward (cite-it-the-match 2) nil t)
	(forward-sexp)
	(setq x2 (- (point) 1))
	(setq bibtitle (cite-it-get-bib-field "title" x1 x2))
	(setq bibauthor (cite-it-get-bib-field "author" x1 x2))
	(goto-char x1)
	(setq citation-list 
	      (cons (list bibkey bibtitle bibauthor (buffer-file-name)) 
		    citation-list))))
    ;; restore variables
    (setq case-fold-search cfs))
  (cons citation-list (cons bibfile-alist nil)))

(defun cite-it-find-bib-file (name bibinputs)
  "Tries to load bib file NAME.bib, along path BIBINPUTS. 
Returns t if buffer already exists; buffer if loaded, nil otherwise"
  (if bibinputs
      (let* ((fullname (concat (car bibinputs) "/" name ".bib"))
	     (bibbuf (get-file-buffer fullname))
	     (alreadyin bibbuf)
	     )
	(if (not bibbuf) (setq bibbuf (find-file-noselect fullname)))
	(set-buffer bibbuf)
	(if (and (not alreadyin) (= 0 (buffer-size)))
	    (progn
	      (kill-buffer nil)
	      (cite-it-find-bib-file name (cdr bibinputs)))
	  (if alreadyin t bibbuf)))
    nil))


(defun cite-it-bibfiles-changed (bl)
  "Returns t if mod time of any bibfile on BL has changed"
  (if bl
      (if (equal (nth 5 (file-attributes (car (car bl))))
		 (car (cdr (car bl))))
	  (cite-it-bibfiles-changed (cdr bl))
	t)
    nil))


(defun cite-it-find-bib-command ()
  "Returns list of files found in bibliography command in current buffer."
  (save-excursion
    (goto-char (point-max))
    (if (re-search-backward cite-it-bibliography-regexp nil t)
	(cite-it-split-string (cite-it-the-match cite-it-bib-regexp-pos)))))


(defun cite-it-make-bibfile-list ()
  "Creates list of bib files, from current & master file if appropriate, and 
also from cite-it-other-bibfiles variable."
  (let ((tmpbfl nil)
	(b (current-buffer)))
    (save-excursion
      ;; first check in current file
      (setq tmpbfl (cite-it-find-bib-command))
      ;; now in master file
      (if cite-it-master-file
	  (let* ((mfnm (if (stringp cite-it-master-file) cite-it-master-file
			 (if (boundp 'TeX-master)
			     (if (stringp TeX-master) TeX-master
;;			       (if (and (symbolp TeX-master) (not (eq t TeX-master)))
;;				   (symbol-name TeX-master))
			       ))))
		 (mb (if mfnm (find-file-noselect
			       (if (file-exists-p mfnm) mfnm
				 (if (not (string-match "\\.tex\\'" mfnm))
				     (concat mfnm ".tex") mfnm))))))
	    (if mfnm
		(progn
		  (set-buffer mb)
		  (setq tmpbfl (append tmpbfl (cite-it-find-bib-command)))))))
      ;; now add others
      (set-buffer b)
      (append tmpbfl cite-it-other-bibfiles))))


(defun cite-it-rescan-bib-files-if-changed ()
  "Check for modified bib files, rescan if necessary"
  (interactive)
  (save-excursion
    (if (or (not (equal (cite-it-make-bibfile-list) cite-it-bibfile-list))
	    (cite-it-bibfiles-changed cite-it-bibfile-alist))
	(cite-it-scan-bibliography))))


(defun cite-it-scan-bibliography () 
  "Sets up cite-it-citation-list by scanning bib files in \\bibliography line."
  (interactive)
  (setq cite-it-citation-list nil)
  (setq cite-it-bibfile-alist nil)
  (save-excursion
    (setq cite-it-bibfile-list (cite-it-make-bibfile-list))
    (if cite-it-bibfile-list
	(let ((tb (current-buffer))
	      (bibinputs (getenv "BIBINPUTS"))
	      (tmpcitelist nil)
	      (tmpbibalist nil)
	      (bfl cite-it-bibfile-list))
	  (if (not bibinputs)
	      (setq bibinputs ".")
	    (setq bibinputs 
		  (concat "." (char-to-string cite-it-path-separater-char) bibinputs)))
	  (setq bibinputs (cite-it-split-string bibinputs cite-it-path-separater-char))
	  ;; process each bib file
	  (while bfl
	    (set-buffer tb)
	    (setq tmpcitelist cite-it-citation-list)
	    (setq tmpbibalist cite-it-bibfile-alist)
	    (let* ((bib-file-name (car bfl))
		   (bib-buf (cite-it-find-bib-file bib-file-name bibinputs)))
	      (if bib-buf
		  (progn
		    (message "Parsing bibliography file %s ..." bib-file-name)
		    (let ((x (cite-it-process-bib-file tmpcitelist tmpbibalist)))
		      (setq tmpcitelist (car x))
		      (setq tmpbibalist (car (cdr x))))
		    (if (and
			 (not cite-it-keep-bib-buffers)
			 (not (equal t bib-buf)))
			(kill-buffer bib-buf)))
		(progn
		  (message "Bibliography file %s not found." bib-file-name)
		  (sleep-for 1))))
	    (set-buffer tb)
	    (setq cite-it-citation-list tmpcitelist)
	    (setq cite-it-bibfile-alist tmpbibalist)
	    (setq bfl (cdr bfl))
	    )
	  (message "Parsing bibliography completed."))
      (message "No bibliography found")
      )))


(defun cite-it-verification (start end)
  (if cite-it-check-always (cite-it-rescan-bib-files-if-changed))
  (save-excursion
    (let ((anybad 0))
      (goto-char start)
      (while (re-search-forward cite-it-cite-regexp end t)
	(let* ((c (cite-it-the-match cite-it-cite-pos))
	       (cl (cite-it-split-string c))
	       (opt (if (match-beginning cite-it-opt-pos) 
			(cite-it-the-match cite-it-opt-pos) ""))
	       (newcl nil)
	       (bad 0))
	  (while cl
	    (setq newcl 
		  (append 
		   newcl
		   (list
		    (if (not (assoc (car cl) cite-it-citation-list))
			(progn
			  (setq bad (+ 1 bad))
			  (completing-read 
			   "Fix citation: " 
			   cite-it-citation-list nil nil (car cl)))
		      (car cl)))))
	    (setq cl (cdr cl)))
	  (if (> bad 0)
	      (progn
		(setq anybad (+ anybad bad))
		(replace-match
		 (concat
		  "\\\\"
		  (if (match-beginning 1) "no" "")
		  "cite"
		  opt
		  "{"
		  (mapconcat (function (lambda (x) x)) newcl ",")
		  "}"))
		))))
      (if (= 0 anybad)
	  (message "Citations verified.")
	(message "%d invalid citations found." anybad)))))

(defun cite-it-verify-buffer ()
  "Verifies all \cite{} commands in the buffer."
  (interactive)
  (cite-it-verification 0 nil))

(defun cite-it-verify-region ()
  "Verifies all \cite{} commands in the current region."
  (interactive)
  (if (> (region-beginning) (region-end))
      (cite-it-verification (region-end) (region-beginning))
    (cite-it-verification (region-beginning) (region-end)))
  )

(defun cite-it-edit-bib-file ()
  "Edits bib file for citation under point."
  (interactive)
  (if cite-it-check-always (cite-it-rescan-bib-files-if-changed))
  ;; look for \cite near point
  (let ((cls -1) (c nil))
    (save-excursion
      (let ((tp (point)))
	(if (cite-it-find-citation tp)
	    (let ((cl (cite-it-split-string (cite-it-the-match cite-it-cite-pos))))
	      (setq cls (cite-it-elemlte 
			 (cite-it-sumstrlen cl (- (match-beginning cite-it-cite-pos) 
					  (match-beginning 0) 1))
			 (- tp (point))))
	      (setq c (nth cls cl)))
	  (message "No citation near point.")))
      (let ((x (assoc c cite-it-citation-list)))
	(if (not x)
	    (message "Citation %s not found" c)
	  (progn
	    (find-file (car (cdr (cdr (cdr x)))))
	    (goto-char 0)
	    (if (re-search-forward 
		 (concat "^@[A-Za-z]+[ \n\t]*{[ \n\t]*" 
			 (regexp-quote c)
			 "[ \n\t]*,") 	 nil t)
		(re-search-backward "^@" nil t))))))))




(defun cite-it-mode (&optional x)
"cite-it is a package designed to help you with the citations in
LaTeX files. In many places it will provide completions for the
citations; by default, it should also keep its internal list of
citations up-to-date based on your \bibliography line and the
most recently saved versions of your bib files.
The cite-it key prefix by default is \C-c\C-c; the next letter will run:

   a cite-it-insert-citation
     Inserts a citation reference into the \cite{} near point
   A cite-it-insert-cite-and-citation
     Inserts \cite{} and a citation reference
   l cite-it-lookup-citation
     Just look one up
   v cite-it-verify-citation
     Verify that the citation under point is valid; if not, prompts 
     for correction.
   c cite-it-complete-citation
     Will bring upfo the compeltion the citation under point.
   V cite-it-verify-multi-citation
     Verifies all citations in a \cite{} list.
   b cite-it-verify-buffer
     Verifies all citations in buffer.
   r cite-it-verify-region
     Verifies all citations in region.
   e cite-it-edit-bib-file
     Edits bib file containing citation under point.
   T cite-it-toggle-author-or-title
     For terse display, show either author or title in minbuffer.
   t cite-it-toggle-verbose
     By default, cite-it shows one line of title of citation in minibuffer 
     after completion. It can also open up another window showing author and
     full title.
   R cite-it-rescan-bib-files-if-changed
     Rescans bibliography files if \bibliography line or files have changed.
   S cite-it-scan-bibliography
     Forces rescan of all bib files.

Some customizable variables (see individual doc):
   cite-it-master-file
   cite-it-other-bibfiles
   cite-it-check-always
   cite-it-key-prefix
"
  (interactive)
  (setq cite-it-mode
	(if (null x) (not cite-it-mode)
	  (or (eq t x)
	      (and (numberp x) (> x 0))
	      (and (consp x) (numberp x) (> x 0)))))
  (if cite-it-mode (cite-it-scan-bibliography)))


;;;
;; initialization

(defvar cite-it-keymap nil)
(if cite-it-keymap
    nil
  (or (assq 'cite-it-mode minor-mode-alist)
      (setq minor-mode-alist
	    (cons '(cite-it-mode " Cite") minor-mode-alist)))
  (setq cite-it-keymap (make-sparse-keymap))
  (or (assq 'cite-it-mode minor-mode-map-alist)
      (let ((map (make-sparse-keymap)))
	(define-key map cite-it-key-prefix cite-it-keymap)
	(setq minor-mode-map-alist
	      (cons (cons 'cite-it-mode map)
		    minor-mode-map-alist))))
  (make-variable-buffer-local 'cite-it-mode)
  (make-variable-buffer-local 'cite-it-citation-list)
  (make-variable-buffer-local 'cite-it-bibfile-list)
  (make-variable-buffer-local 'cite-it-bibfile-alist)
  (make-variable-buffer-local 'cite-it-bibfile-method)
  (make-variable-buffer-local 'cite-it-other-bibfiles)
  (make-variable-buffer-local 'cite-it-master-file)
  (define-key cite-it-keymap "a" 'cite-it-insert-citation)
  (define-key cite-it-keymap "A" 'cite-it-insert-cite-and-citation)
  (define-key cite-it-keymap "l" 'cite-it-lookup-citation)
  (define-key cite-it-keymap "v" 'cite-it-verify-citation)
  (define-key cite-it-keymap "c" 'cite-it-complete-citation)
  (define-key cite-it-keymap "V" 'cite-it-verify-multi-citation)
  (define-key cite-it-keymap "T" 'cite-it-toggle-author-or-title)
  (define-key cite-it-keymap "R" 'cite-it-rescan-bib-files-if-changed)
  (define-key cite-it-keymap "b" 'cite-it-verify-buffer)
  (define-key cite-it-keymap "r" 'cite-it-verify-region)
  (define-key cite-it-keymap "S" 'cite-it-scan-bibliography)
  (define-key cite-it-keymap "e" 'cite-it-edit-bib-file)
  (define-key cite-it-keymap "t" 'cite-it-toggle-verbose))



;;; cite-it.el ends here
