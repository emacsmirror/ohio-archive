;Date: 11 Jan 89 15:12:20 GMT
;From: allegra.UUCP!kautz@eddie.mit.edu (Henry Kautz)
;Subject: refer to bibtex conversion

;The follow gnu program converts "refer" style databases, as used by
;troff, to "bibtex" style databases, as used by LaTex.  It does an
;excellent job of guessing the target entry type, handling dates, and
;generating a good citation key, and can easily be modified to handle
;your refer or bibtex macros.  When first loaded it will display a help
;message.
	
;:uucp:		allegra!kautz		
;:arpa/internet: kautz%allegra.att.com@research.att.com

;; refer-to-bibtex.el
;; Convert refer-style bibliographic entries to ones usable by latex bib 
;; copyright 1988 Henry Kautz
;;
;; Use: from a buffer containing the refer-style bibliography,
;;   M-x r2b-convert-buffer
;; Program will prompt for an output buffer name, and will log
;; warnings during the conversion process in the buffer *Log*.

(provide 'refer-to-bibtex)
;**********************************************************
; User Parameters

(defvar r2b-trace-on nil "*trace conversion")

(defvar r2b-journal-abbrevs
   '(  ("aij" "{Artificial Intelligence}")
       )
   "Abbreviation list for journal names.  
If the car of an element matches the data exactly, it is replaced by
the cadr when output.  Braces must be included if replacement is a
{string}, but not if replacement is a bibtex abbreviation.  The cadr
may be eliminated if is exactly the same as the car.")

(defvar r2b-booktitle-abbrevs 
   '(  ("mi" "{Machine Intelligence}") 
       ("aaai88" "{Proceedings of AAAI-88}")
       ("ijcai88")
       )
   "Abbreviation list for book and proceedings names.  If the car of
an element matches the data exactly, it is replaced by the cadr when
output.  Braces must be included if replacement is a {string}, but not
if replacement is a bibtex abbreviation.  The cadr may be eliminated
if is exactly the same as the car.  
	For example, the default value means that aaai88 is expanded
to ordinary test, but ijcai88 is treated as a bibtex macro.")

(defvar r2b-proceedings-list
   '(("ijcai88"))
   "Assoc list of books or journals which are really conference proceedings,
but whose name and whose abbrev expansion (as defined in r2b-journal-abbrevs
and r2b-booktitle-abbrevs) does not contain the words 'conference' or
'proceeding'.  (Those cases are handled automatically.)
The entry must match the given data exactly.")

(defvar r2b-stop-words
	 "A\\|The\\|Some\\|\\An"
   "Words not considered to use to build the citation key")

;**********************************************************
; Utility Functions

(setq capitalize-title-stop-words
   (concat 
      "\\(the\\|and\\|of\\|is\\|a\\|an\\|of\\|for\\|in\\|to\\|in\\|on\\|at\\|"
      "by\\|with\\|that\\|its\\)\\b"))

(defun capitalize-title-region (begin end)
   "Like capitalize-region, but don't capitalize stop words, except the first"
   (interactive "r")
   (let ((case-fold-search t))
      (save-restriction
	 (narrow-to-region begin end)
	 (goto-char (point-min))
	 (capitalize-word 1)
	 (while (re-search-forward "\\<" nil t)
	    (if (looking-at capitalize-title-stop-words)
	       (downcase-word 1)
	       (if (let ((case-fold-search nil)) (looking-at ".[A-Z]"))
		  (forward-word 1)
		  (capitalize-word 1)))
	    )
	 )))

(defun capitalize-title (s)
   "Like capitalize, but don't capitalize stop words, except the first"
   (save-excursion
      (set-buffer (get-buffer-create "$$$Scratch$$$"))
      (erase-buffer)
      (insert s)
      (capitalize-title-region (point-min) (point-max))
      (buffer-string)))

;*********************************************************
(defun r2b-reset ()
   "unbind defvars, for debugging"
   (interactive)
   (makunbound 'r2b-journal-abbrevs)
   (makunbound 'r2b-booktitle-abbrevs)
   (makunbound 'r2b-proceedings-list)
   (makunbound 'r2b-stop-words)
   (makunbound 'r2b-stop-regexp)
   )

(defvar r2b-stop-regexp
   (concat "\\(\\(" r2b-stop-words "\\|\\)\\b\\)[ \t\n]*\\([A-Z0-9]+\\)"))


(defun r2b-trace (&rest args)
   (if r2b-trace-on
      (progn
	 (apply (function message) args)
	 (sit-for 0)
	 )))

(defun r2b-match (exp)
   "returns string matched in current buffer"
   (buffer-substring (match-beginning exp) (match-end exp)))

(defvar r2b-out-buf-name "*Out*" "*output from refer-to-bibtex" )
(defvar r2b-log-name "*Log*" "*logs errors from refer-to-bibtex" )
(defvar r2b-in-buf nil)
(defvar r2b-out-buf nil)
(defvar r2b-log nil)

(defvar r2b-error-found nil)

(setq r2b-variables '(
			r2b-error-found
			  r2bv-author
			  r2bv-primary-author
			  r2bv-date
			  r2bv-year
			  r2bv-decade
			  r2bv-month
			  r2bv-title
			  r2bv-title-first-word
			  r2bv-editor
			  r2bv-annote
			  r2bv-tr
			  r2bv-address
			  r2bv-institution
			  r2bv-keywords
			  r2bv-booktitle
			  r2bv-journal
			  r2bv-volume
			  r2bv-number
			  r2bv-pages
			  r2bv-booktitle
			  r2bv-kn
			  r2bv-publisher
			  r2bv-school
			  r2bv-type
			  r2bv-where
			  r2bv-note
			  ))

(defun r2b-clear-variables ()
   "set all global vars used by r2b to nil"
   (let ((vars r2b-variables))
      (while vars
	 (set (car vars) nil)
	 (setq vars (cdr vars)))
      ))

(defun r2b-warning (&rest args)
   (setq r2b-error-found t)
   (princ (apply (function format) args) r2b-log)
   (princ "\n" r2b-log))

(defun r2b-get-field (var field &optional unique required)
   "Set VAR to string value of FIELD, if any.  If none, VAR is set to nil.
If multiple fields appear, then separate values with the '\\nand\\t\\t',
unless UNIQUE is non-nil, in which case log error and return first value.
Trim off leading blanks and tabs on first line, and
trailing blanks and tabs of every line.
Returns value of var."
   (let (item val (not-past-end t))
      (r2b-trace "snarfing %s" field)
      (goto-char (point-min))
      (while (and not-past-end
		(re-search-forward 
		   (concat "^" field "\\b[ \t]*\\(.*\\)") nil t))
	 (setq item (r2b-match 1))
	 (while (and (setq not-past-end (zerop (forward-line 1)))
		   (not (looking-at "[ \t]*$\\|%")))
	       (looking-at "\\(.*[^ \t\n]\\)[ \t]*$")
	       (setq item (concat item "\n" (r2b-match 1)))
	    )
	 (if (null val)
	    (setq val item)
	    (if unique
	       (r2b-warning "*Illegal multiple field %s %s" field item)
	       (setq val (concat val "\n\t\tand " item))
	       )
	    )
	 )
      (if (and (null val) required)
	 (r2b-warning "*Missing field %s" field))
      (set var val)
      ))

(defun r2b-set-match (var n regexp string )
   "set VAR to the Nth subpattern in REGEXP matched by STRING, or nil if none"
   (set var
      (if (and (stringp string) (string-match regexp string))
	 (substring string (match-beginning n) (match-end n))
	 nil)
      )
   )

(defvar r2b-month-abbrevs
   '(("jan") ("feb") ("mar") ("apr") ("may") ("jun") ("jul") ("aug")
       ("sep") ("oct") ("nov") ("dec")))

(defun r2b-convert-month ()
   "Try to convert r2bv-month to a standard 3 letter name"
   (if r2bv-month
      (let ((months r2b-month-abbrevs))
	 (if (string-match "[^0-9]" r2bv-month)
	    (progn
	       (while (and months (not (string-match (car (car months)) 
					  r2bv-month)))
		  (setq months (cdr months)))
	       (if months
		  (setq r2bv-month (car (car months)))))
	    (progn
	       (setq months (car (read-from-string r2bv-month)))
	       (if (and (numberp months)
		      (> months 0)
		      (< months 13))
		  (setq r2bv-month (car (nth months r2b-month-abbrevs)))
		  (progn
		     (r2b-warning "* Ridiculous month")
		     (setq r2bv-month nil))
		  ))
	    ))
      )
   )

(defun r2b-snarf-input ()
   "parse buffer into global variables"
   (let ((case-fold-search t))
      (r2b-trace "snarfing...")
      (sit-for 0)
      (set-buffer r2b-in-buf)
      (goto-char (point-min))
      (princ "    " r2b-log)
      (princ (buffer-substring (point) (progn (end-of-line) (point))) r2b-log)
      (terpri r2b-log)

      (r2b-get-field 'r2bv-author "%A" nil t)
      (r2b-set-match 'r2bv-primary-author 1
	 "\\b\\(\\w+\\)[ \t]*\\($\\|,\\)" r2bv-author)

      (r2b-get-field 'r2bv-date "%D" t t)
      (r2b-set-match 'r2bv-year 0 "[12][0-9][0-9][0-9]" r2bv-date)
      (and (null r2bv-year)
	 (r2b-set-match 'r2bv-year 1 "[^0-9]\\([0-9][0-9]\\)$" r2bv-date)
	 (setq r2bv-year (concat "19" r2bv-year)))
      (r2b-set-match 'r2bv-decade 1 "..\\(..\\)" r2bv-year)
      (r2b-set-match 'r2bv-month 0
	 "[0-9]+/\\|[a-zA-Z]+" r2bv-date)
      (if (and (stringp r2bv-month) (string-match "\\(.*\\)/$" r2bv-month))
	 (setq r2bv-month (substring r2bv-month 0 (match-end 1))))
      (r2b-convert-month)

      (r2b-get-field 'r2bv-title "%T" t t)
      (r2b-require 'r2bv-title)
      (setq r2bv-title (capitalize-title r2bv-title))
      (r2b-set-match 'r2bv-title-first-word 3
	 r2b-stop-regexp
	 r2bv-title)
      

      (r2b-get-field 'r2bv-editor "%E")
      (r2b-get-field 'r2bv-annote "%X" t )
      (r2b-get-field 'r2bv-tr "%R" t)
      (r2b-get-field 'r2bv-address "%C" t)
      (r2b-get-field 'r2bv-institution "%I" t)
      (r2b-get-field 'r2bv-keywords "%K")
      (r2b-get-field 'r2bv-booktitle "%B" t)
      (r2b-get-field 'r2bv-journal "%J" t)
      (r2b-get-field 'r2bv-volume "%V" t)
      (r2b-get-field 'r2bv-number "%N" t)
      (r2b-get-field 'r2bv-pages "%P" t)
      (r2b-get-field 'r2bv-where "%W" t)
      )
   )


(defun r2b-put-field (field data &optional abbrevs)
   "print bibtex FIELD = {DATA} if DATA not null; precede
with a comma and newline; if ABBREVS list is given, then
try to replace the {DATA} with an abbreviation"
   (if data
      (let (match nodelim multi-line)
	 (if (and abbrevs (setq match (assoc data abbrevs)))
	    (progn
	       (if (null (cdr match))
		  (setq data (car match))
		  (setq data (car (cdr match))))
	       (setq nodelim t)))
	 (if (and (not (equal data ""))
		(not (string-match "[^0-9]" data)))
	    (setq nodelim t))
	 (princ ", \n  ")
	 (princ field)
	 (princ " =\t")
	 (if (not nodelim) (princ "{"))
	 (string-match ".*" data)
	 (if (> (match-end 0) 59)
	    (princ "\n"))
	 (princ data)
	 (if (not nodelim) (princ "}"))
	 )
      ))


(defun r2b-require (vars)
   "If any of VARS is null, set to empty string and log error"
   (cond 
      ((null vars))
      ((listp vars) (r2b-require (car vars)) (r2b-require (cdr vars)))
      (t
	 (if (null (symbol-value vars))
	    (progn
	       (r2b-warning "*Missing value for field %s" vars)
	       (set vars "")
	       )))
      )
   )


(defmacro moveq (new old)
   "set NEW to OLD and set OLD to nil"
   (list 'progn (list 'setq new old) (list 'setq old 'nil)))

(defun r2b-barf-output ()
   "generate bibtex based on global variables"
   (let ((standard-output r2b-out-buf) (case-fold-search t) match)

      (r2b-trace "...barfing")
      (sit-for 0)
      (set-buffer r2b-out-buf)

      (setq r2bv-kn (concat r2bv-primary-author r2bv-decade
			r2bv-title-first-word))
      
      (setq r2bv-entry-kind
	 (cond 
	    ((and r2bv-journal
		(or 
		   (string-match "proceeding\\|conference" r2bv-journal)
		   (assoc r2bv-journal r2b-proceedings-list)
		   (and 
		      (setq match (assoc r2bv-journal r2b-booktitle-abbrevs))
		      (string-match "proceeding\\|conference" 
			 (car (cdr (match)))))))
	       (moveq r2bv-booktitle r2bv-journal)
	       (moveq r2bv-publisher r2bv-institution)
	       'inproceedings)
	    ((and r2bv-booktitle
		(or 
		   (string-match "proceeding\\|conference" r2bv-booktitle)
		   (assoc r2bv-booktitle r2b-proceedings-list)
		   (and 
		      (setq match (assoc r2bv-booktitle 
				       r2b-booktitle-abbrevs))
		      (string-match "proceeding\\|conference" 
			 (car (cdr (match)))))))
	       (moveq r2bv-publisher r2bv-institution)
	       'inproceedings)
	    ((and r2bv-tr (string-match "phd" r2bv-tr))
	       (moveq r2bv-school r2bv-institution)
	       (r2b-require 'r2bv-school )
	       'phdthesis)
	    ((and r2bv-tr (string-match "master" r2bv-tr))
	       (moveq r2bv-school r2bv-institution)
	       (r2b-require 'r2bv-school )
	       'mastersthesis)
	    ((and r2bv-tr (string-match "draft\\|unpublish" r2bv-tr))
	       (moveq r2bv-note r2bv-institution)
	       'unpublished)
	    (r2bv-journal
	       'article)
	    (r2bv-booktitle
	       (moveq r2bv-publisher r2bv-institution)
	       (r2b-require 'r2bv-publisher)
	       'incollection)
	    (r2bv-tr
	       (r2b-require 'r2bv-institution)
	       (if (string-match "\\(.+\\)[ /t/n]+\\([^ /t/n]\\)+$" 
		      r2bv-tr)
		  (progn
		     (setq r2bv-type (substring r2bv-tr 0 (match-end 1)))
		     (setq r2bv-number (substring r2bv-tr 
					  (match-beginning 2)))
		     (setq r2bv-tr nil))
		  (moveq r2bv-number r2bv-tr))
	       'techreport)
	    (r2bv-institution
	       (moveq r2bv-publisher r2bv-institution)
	       (r2b-require 'r2bv-publisher)
	       'book)
	    (t
	       'misc)
	    ))

      (r2b-require '(r2bv-author r2bv-title r2bv-year))

      (if r2b-error-found
	 (princ "\n% Warning -- Errors During Conversion Next Entry\n"))

      (princ "\n@")
      (princ r2bv-entry-kind)
      (princ "( ")
      (princ r2bv-kn)

      (r2b-put-field "author" r2bv-author )
      (r2b-put-field "title" r2bv-title )
      (r2b-put-field "year" r2bv-year )

      (r2b-put-field "month" r2bv-month r2b-month-abbrevs)
      (r2b-put-field "journal" r2bv-journal r2b-journal-abbrevs)
      (r2b-put-field "volume" r2bv-volume)
      (r2b-put-field "number" r2bv-number)
      (r2b-put-field "type" r2bv-type)
      (r2b-put-field "booktitle" r2bv-booktitle r2b-booktitle-abbrevs)
      (r2b-put-field "editor" r2bv-editor)
      (r2b-put-field "publisher" r2bv-publisher)
      (r2b-put-field "institution" r2bv-institution)
      (r2b-put-field "school" r2bv-school)
      (r2b-put-field "pages" r2bv-pages)
      (r2b-put-field "address" r2bv-address)
      (r2b-put-field "note" r2bv-note)
      (r2b-put-field "keywords" r2bv-keywords)
      (r2b-put-field "where" r2bv-where)
      (r2b-put-field "annote" r2bv-annote)

      (princ " )\n")
      )
   )


(defun r2b-convert-record (output-name)
   "transform current bib entry and append to buffer OUTPUT"
   (interactive 
      (list (read-string "Output to buffer: " r2b-out-buf-name)))
   (let (rec-end rec-begin not-done)
      (setq r2b-out-buf-name output-name)
      (setq r2b-out-buf (get-buffer-create output-name))
      (setq r2b-in-buf (current-buffer))
      (set-buffer r2b-out-buf)
      (goto-char (point-max))
      (setq r2b-log (get-buffer-create r2b-log-name))
      (set-buffer r2b-log)
      (goto-char (point-max))
      (set-buffer r2b-in-buf)
      (setq not-done (re-search-forward "[^ \t\n]" nil t))
      (if not-done
	 (progn
	    (re-search-backward "^[ \t]*$" nil 2)
	    (re-search-forward "^%")
	    (beginning-of-line nil)
	    (setq rec-begin (point))
	    (re-search-forward "^[ \t]*$" nil 2)
	    (setq rec-end (point))
	    (narrow-to-region rec-begin rec-end)
	    (r2b-clear-variables)
	    (r2b-snarf-input)
	    (r2b-barf-output)
	    (set-buffer r2b-in-buf)
	    (widen)
	    (goto-char rec-end)
	    t)
	 nil
	 )
      ))
      
      
(defun r2b-convert-buffer (output-name)
   "transform current buffer and append to buffer OUTPUT"
   (interactive 
      (list (read-string "Output to buffer: " r2b-out-buf-name)))
   (save-excursion
      (setq r2b-log (get-buffer-create r2b-log-name))
      (set-buffer r2b-log)
      (erase-buffer))
   (widen)
   (goto-char (point-min))
   (message "Working, please be patient...")
   (sit-for 0)
   (while (r2b-convert-record output-name) t)
   (message "Done, results in %s, errors in %s" 
      r2b-out-buf-name r2b-log-name)
   )

(defvar r2b-load-quietly nil "*Don't print help message when loaded")

(defvar r2b-help-message
"                   Refer to Bibtex Bibliography Conversion

A refer-style database is of the form:

%A Joe Blow
%T Great Thoughts I've Thought
%D 1977
etc.

This utility converts these kind of databases to bibtex form, for
users of TeX and LaTex.  Instructions:
1.  Visit the file containing the refer-style database.
2.  The command
	M-x r2b-convert-buffer
    converts the entire buffer, appending it's output by default in a
    buffer named *Out*, and logging progress and errors in a buffer
    named *Log*.  The original file is never modified.
	Note that results are appended to *Out*, so if that buffer
	buffer already exists and contains material you don't want to
 	save, you should kill it first.
3.  Switch to the buffer *Out* and save it as a named file.
4.  To convert a single refer-style entry, simply position the cursor
    at the entry and enter
	M-x r2b-convert-record
    Again output is appended to *Out* and errors are logged in *Log*.

This utility is very robust and pretty smart about determining the
type of the entry.  It includes facilities for expanding refer macros
to text, or substituting bibtex macros.  See the source file,
	refer-to-bibtex.el
for information on how to customize the conversion process.

If you don't want to see this help message when you load this utility,
then include the following line in your .emacs file:
	(setq r2b-load-quietly t)

Please send bug reports and suggestions to
	Henry Kautz
	allegra!kautz
	kautz@allegra.att.com")


(defun r2b-help ()
   "print help message"
   (interactive)
   (with-output-to-temp-buffer "*Help*"
      (princ r2b-help-message)))

(if (not r2b-load-quietly)
   (r2b-help))

(message "r2b loaded")
