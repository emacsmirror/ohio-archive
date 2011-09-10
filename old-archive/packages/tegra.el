;From ark1!nap1!ames!sun-barr!lll-winken!uunet!bu-cs!mirror!ssi3b1!shyoon!tegra!vail Mon Nov  6 13:11:19 1989
;Article 649 of gnu.emacs
;Path: ark1!nap1!ames!sun-barr!lll-winken!uunet!bu-cs!mirror!ssi3b1!shyoon!tegra!vail
;From vail@tegra.UUCP (Johnathan Vail)
;Newsgroups: gnu.emacs
;Subject: Re: Automatic header creation and maintenance
;Message-ID: <814@atlas.tegra.UUCP>
;Date: 3 Nov 89 15:46:49 GMT
;References: <m0gMOq1-0000F7C@fire.indetech.com>
;Distribution: gnu
;Organization: Tegra-Varityper, Inc., Billerica, MA
;Lines: 515
;In-reply-to: lrs@indetech.com's message of 1 Nov 89 17:33:00 GMT
;
;In article <m0gMOq1-0000F7C@fire.indetech.com> lrs@indetech.com (Lynn Slater) writes:
;
;   Enclosed is code to automatically create and maintain file headers.  This
;   code is cleaner and mush more easily customized than any of my previous
;   header postings.
;
;   I have found file headers to be very valuable in project development. I
;   always know who has been where and how many times they were there. Most
;
;   ===============================================================
;   Lynn Slater -- lrs@indetech.com or {sun, ames, pacbell}!indetech!lrs
;
;Here is a set of routines that I have hacked for file and C function
;headers in my company's format.  It will take some hacking to change
;to whatever format you use but none-the-less may be useful.
;In particular, my functions provide:
;
;	File headers that use etags to find the global and local
;		functions defined.
;
;	C Function header that lists the functions used.
;
;	A quick little revision history inserterer
;
;Anyway, this represents a learning curve for me so please forgive the
;crocks...
;
;"Well they say that I'm weird and disinfectant is the only thing that I drink.
;But cleanliness of the soul is more important don't you think?" -- R Hitchcock
; _____
;|     | Johnathan Vail | tegra!N1DXG@ulowell.edu
;|Tegra| (508) 663-7435 | N1DXG@145.110-,145.270-,444.2+,448.625-
; -----

;;;
;;;
;;;	tegra.el
;;;
;;;	This file contains Tegra-Varityper specific code documenting and
;;;	formatting functions.
;;;
;;;	Written by Johnathan Vail, Tegra Inc.
;;;
;;;	HISTORY:
;;;	 5 Sep 1989 JV	- make variables appear for user change
;;;	17 Aug 1989 JV	- Add tegra-update-header
;;;	17 Aug 1989 JV	- fix etags file creation
;;;	22 Jun 1989 JV	- add iteration to loops and add purecopy
;;;	21 Jun 1989 JV	- New standards from NAD
;;;	28 Mar 1989 JV	- make real variables, add to public gnu
;;;	17 Feb 1989 JV	- add "smart" header functions/combine into tegra.el
;;;	24 Jan 1989 JV	- Added tegra-new-function
;;;	 3 Jan 1989 JV	- Rewrote to Tegra-ish form
;;;     Original date routines: Constantine Rasmussen, Dec 27, 1988
;;;
;;;	BUGS:  (No no, they're features!)
;;;
;;;	* Macros and parenthesis in comments sometimes look like C functions
;;;
;;;	* forward-list and backward-list assume that we
;;;		are in C mode to properly move around `{' and `}'
;;;
;;;



;;; This is, of course, user's choice
;;
;;   C-Ct   Insert timestamp like this: 22 Feb 1989 JV	-
;;   C-Cd   Will insert at point a Tegra header with most fields filled
;;   C-Cn   Insert at point a blank Tegra header
;;   C-Cl   Insert at point functions used in following C function
;;   C-Ch   Insert at point a Tegra source file (module) header, filled out
;;   C-cu   Update file header.
;;;
(global-set-key "\C-ct" 'insert-user-datestamp)
(global-set-key "\C-cd" 'tegra-document-function)
(global-set-key "\C-cn" 'tegra-new-function)
(global-set-key "\C-cl" 'list-functions-used)
(global-set-key "\C-ch" 'tegra-document-header)
(global-set-key "\C-cu" 'tegra-update-header)



(defconst  C-func-regexp "[a-zA-Z0-9_]+\\([ \t\n]*\(\\)"
  "Used to look for a C function")

(defvar tegra-fluff t
  "*Insert optional fields in headers if non-nil")


;;
;;  This defines the things that look like functions but really aren't
;;  as well as the functions that we don't want to see listed.
;;

(defvar tegra-null-functions
      (quote ("if" "while" "return" "for" "switch"

;;  Here are the functions that we don't want to know about:
;;  Mostly these are library I/O functions

	      "printf" "sprintf" "strlen"
	      "lcd_clr" "lcd_disp" "lcd_puts" "lcd4_puts" "kpd_getc"
	      ))
      "*List of functions that should not be on list of functions used")



;;
;;  This function inserts at point all of the functions that are used in
;;  the following C function.  Each function is listed on a separate line
;;  with a `*' on the left.
;;

(defun list-functions-used ()
  "Generate a list of C functions used in the current function"

  (interactive)
  (let (function-list)
    (save-excursion
      (save-restriction
	(skip-chars-forward "^{")
	(beginning-of-line)
	(forward-list)
	(let ((end (point))) 
	  (backward-list)
	  (narrow-to-region (point) end)

	  (setq function-list (find-C-functions)))))
    (insert-functions-used function-list)))



;;
;;  This guy does the work of actually doing the inserting
;;

(defun insert-functions-used (functions)
  (cond ((null functions) nil)
	(t (insert "*\t" (car functions) "\n")
	   (insert-functions-used (cdr functions)))))



;;
;;  Return a list of functions used, sorted alphabetically and ignoring
;;  most library functions.
;;
;;	22 Jun 1989 JV	- made iterative
;;

(defun find-C-functions ()
  (let (functions)
    (setq functions nil)
    (while (re-search-forward C-func-regexp nil t)
      (setq functions
	    (sort-add-to-list (buffer-substring (match-beginning 0)
						(match-beginning 1))
			      functions)))
    functions))

;  (cond ((re-search-forward C-func-regexp nil t)
;	 (sort-add-to-list (buffer-substring (match-beginning 0) (match-beginning 1))
;			   (find-C-functions)))
;	(t nil)))



;;
;;  Add to the list, insertion sort
;;
;;  If the thing is on our *hit list then don't add it
;;  If list is empty then return a list of thing
;;  If the thing is already in list then don't add it again
;;  If thing comes before whats at the beginning of the list then add thing
;;  otherwise cdr down the list...
;;

(defun sort-add-to-list (thing list)
  (cond ((string-memberp thing tegra-null-functions) list)
	((null list) (cons thing nil))
	((string-equal thing (car list)) list)
	((string-lessp thing (car list)) (cons thing list))
	(t (cons (car list) (sort-add-to-list thing (cdr list))))))



;;
;;  I couldn't find a member function for strings anywhere else...
;;

(defun string-memberp (thing list)
  (cond ((null list) nil)
	((string-equal thing (car list)) t)
	(t (string-memberp thing (cdr list)))))



;;
;;  Take a string and insert the lines in the string as a header comment,
;;  with a star on the left margin and a single tab before the text.
;;

(defun insert-stared-lines (text)
  (cond ((string-equal text "") "")
	((string-match (purecopy "[ \t\n]*\\(.*\\)\n") text)
	 (insert (format "*\t%s\n"
			 (substring text (match-beginning 1) (match-end 1))))
	 (insert-stared-lines (substring text (match-end 0) nil)))
	(t (string-match (purecopy "[ \t]*\\(.*\\)$") text)
	   (insert (format "*\t%s\n"
			   (substring text (match-beginning 1) (match-end 1)))))))




;;
;;  This function will look for the next C function, grab all the relevent data
;;  for the header fields and then call tegra-new-function with that data
;;
;;	23 Feb 1989 JV	- Add input field
;;

(defun tegra-document-function ()
"Create a new header in Tegra format and fill out as many fields as we can"

  (interactive)

  (save-excursion
    (let (name synopsis input functions)
      (save-excursion
	(if (re-search-forward
	     (purecopy "^[ \t]*[a-zA-Z][a-zA-Z0-9*_ \t]*\(\\(\\([ \t]*\\)[^)]*\\)\)[^{]*")
	     nil t)
	    ()
	  (error "No function found to document!"))
	(setq synopsis (buffer-substring (match-beginning 0) (match-end 0)))
	(if (eq (match-end 1) (match-end 2))
	    ()
	  (setq input (buffer-substring (match-end 2) (match-end 1))))
	(string-match C-func-regexp synopsis)
	(setq name (substring synopsis (match-beginning 0) (match-beginning 1)))
	(forward-list)
	(save-restriction
	  (let ((end (point))) 
	    (backward-list)
	    (narrow-to-region (point) end))
	  (setq functions (find-C-functions))))
      (tegra-new-function (null tegra-fluff) name synopsis input functions))))
	   


;;
;;	21 Jun 1989 JV	- Created
;;

(defun tegra-document-header ()
"Create a new file header in Tegra format and fill out as many fields as we can"

  (interactive)

  (save-excursion
    (let (name funcs)
      (setq name (file-name-nondirectory buffer-file-name))
      (setq funcs (tegra-find-functions name))
      (tegra-new-header (null tegra-fluff) name (cdr funcs) (car funcs)))))


(defun tegra-update-header ()
"Update the Functions Defined: field in a Tegra file header"

  (interactive)

  (save-excursion
    (let (name funcs start)
      (setq name (file-name-nondirectory buffer-file-name))
      (setq funcs (tegra-find-functions name))
      (goto-char 0)
      (re-search-forward "^\\* Functions Defined:")
      (beginning-of-line)
      (setq start (point))		; start of functions region
      (forward-line 1)
      (if (re-search-forward "^\\*\\( [A-Z][a-z ]*:\\)\\|\\(\\*-\\*+\\)")
	  (progn
	    (beginning-of-line)
	    (delete-region start (point))
	    (tegra-insert-functions (cdr funcs) (car funcs)))
	(error "No end of header found")))))



;;; Usage: etags [-BFaetuwvx] [-f outfile] file ...
;;;	17 Aug 1989 JV	- fix to use a special tags file

(defun find-or-create-tags (name)
  "Create a special TAGS file for the current file"
  (let (tagsname)
    (setq tagsname (concat name ".TAGS"))
    (setq tags-file-name (concat default-directory tagsname))
    (if (and (file-readable-p tags-file-name)
	     (file-newer-than-file-p tagsname
				     (concat default-directory name)))
	()
      (message (concat "Creating TAGS file " tagsname))
      (shell-command (concat "etags -f " tags-file-name " " name)))
    (visit-tags-table tagsname)
    (visit-tags-table-buffer)
    (goto-char 0)))



(defun tegra-find-functions (name)
"Look through the tags file and find the functions defined, returning a cons of local
and global lists"
  (let (err beg end func local global)
    (save-excursion
      (find-or-create-tags name)
      (save-restriction
	(widen)
	(if (not (search-forward name nil t))
	    (setq err t)
	  (setq beg (match-end 0))
	  (skip-chars-forward "^")
	  (setq end (point))
	  (goto-char beg)
	  (narrow-to-region beg end)
	  (while (re-search-forward "^[ \t]*[a-zA-Z][a-zA-Z0-9*_ \t]*" nil t)
	    (setq func (buffer-substring (match-beginning 0) (match-end 0)))
	    (if (string-match "^static " func)
		(setq local (sort-add-to-list (substring func (match-end 0)) local))
	      (setq global  (sort-add-to-list func global)))))))
    (if err (error (format "%s not found in tags-file" name)))
    (cons local global)))



(setq star-line (purecopy "***********************************************************************"))




;;
;;  This function, if called by M-x inserts a mostly blank header
;;  If called from another function the fields are properly filled
;;  in.  It inserts at the current point.
;;
;;	21 Jun 1989 JV	- Change to new format
;;	23 Feb 1989 JV	- Add input field
;;


(defun tegra-new-function (&optional no-fluff name synopsis input functions)
"Insert a header for a new C function in Tegra format"

  (interactive)

  (insert
   "\n\n/*++" star-line

   (format "\n*\n* %s - \n*\n" name)
   "* Revision History:\n*\t"
   (tegra-date (current-time-string))
   " "
   (initials-only (strip-aux-GCOS-info (user-full-name)))
   "\t- Created\n*\n")

   
  (if (null synopsis) ()
    (insert "* Synopsis:\n")
    (insert-stared-lines synopsis))

  (insert
   "*\n"
   "* Description:\n*\n")

  (if no-fluff ()
    (insert "* Return Value:\n*\n"))

  (if (and (null input) no-fluff) ()
    (insert "* Parameters:\n")
    (if (null input) ()
      (insert-stared-lines input))
    (insert "*\n"))

  (if no-fluff () (insert "* Global Variables:\n*\n"))

  (if (and (null functions) no-fluff) ()
    (insert "* Functions called:\n")
    (if (null functions) ()
      (insert-functions-used functions)
      (insert "*\n")))

  (if no-fluff ()
    (insert "* Side Effects:\n*\n"
	    "* See Also:\n*\n"
	    "* Technical Notes:\n*\n"
	    "* Known Bugs:\n*\n"
	    "* Example:\n*\n"))

  (insert "**--" star-line
	  "/\n\n"))


;;
;;  This function, if called by M-x inserts a mostly blank header
;;  If called from another function the fields are properly filled
;;  in.  It inserts at the current point.
;;
;;	21 Jun 1989 JV	- Create
;;


(defun tegra-new-header (&optional no-fluff name global local)
"Insert a header for a new C module in Tegra format"

  (interactive)

  (insert
   "/*+*" star-line

   (format "\n* Copyright %s Tegra/Varityper, Inc "
	   (substring (current-time-string) -4 nil))

   (format "\n*\n* %s %s %s - \n*\n" "\%Z\%" name "\%G\%")
   "* Revision History:\n*\t"
   (tegra-date (current-time-string))
   " "
   (initials-only (strip-aux-GCOS-info (user-full-name)))
   "\t- Created\n*\n")

  (insert "* Description:\n*\n")

  (if no-fluff ()
    (insert "* Return Value:\n*\n"))

  (if no-fluff () (insert "* See Also:\n*\n"
			  "* Technical Notes:\n*\n"
			  "* Known Bugs:\n*\n"))

  (tegra-insert-functions global local)


  (insert "**-*" star-line (purecopy "/\n\nstatic char *ver = \"\%W\%\t\%G\%\";\n\n"))

  (if no-fluff () (mapcar (function star-comment)
			  '("USEFUL MACROS *****"
			    "DEFINITIONS *******"
			    "EXTERNAL VARIABLES "
			    "LOCAL VARIABLES ***"
			    "EXTERNAL FUNCTIONS "
			    "LOCAL FUNCTIONS ***"))))




(defun star-comment (comment)
  (insert (format (purecopy "/******* %s***********************************************/\n\n")
		  comment)))


(defun tegra-insert-functions (global local)
  "Insert at point the global and local functions"

  (insert "* Functions Defined:\n*\n"
	  "* GLOBAL\n")
  (if (null global) ()
    (insert-functions-used global)
    (insert "*\n"))
  (insert "* LOCAL\n")
  (if (null local) ()
    (insert-functions-used local)
    (insert "*\n")))





;;;
;;;     This is a collection of functions to get and insert the date,
;;;     user name & time.  They are useful to append logs and make notes
;;;     in sources.  Original source for some of this code:
;;;		Constantine Rasmussen
;;;

(defun insert-user-datestamp (&optional append-time)
  "Args: (&OPTIONAL APPEND-TIME)
Useful for timestamping in \"live\" files such as source code or logs."
  (interactive "P")
  (insert (tegra-date (current-time-string))
	  " "
	  (initials-only (strip-aux-GCOS-info (user-full-name))) "\t- " ))


(defun strip-aux-GCOS-info (fullname)
  (substring fullname 0 (string-match " *[-:]" fullname)))


(defun initials-only (fullname)
  (cond ((string-equal fullname "") "")
	(t (concat (substring fullname 0 1)
		   (initials-only (substring fullname (next-word fullname) nil))))))


(defun next-word (string)
  (string-match "[^ ]* *" string)
  (match-end 0))


(defun tegra-date (time)
"Returns date string in the format of  3 Jan 1989"

  (concat (substring time 8 11)
	  (substring time 4 8)
	  (substring time -4 nil)))
