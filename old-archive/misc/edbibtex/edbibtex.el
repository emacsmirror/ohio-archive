;; $Id: edbibtex.el,v 1.1.1.5 1993/12/20 15:13:18 root Exp root $
;; LCD Archive Entry:                
;; edbibtex|Michael Burschik|burschik@uni-bonn.de|
;; A BibTeX interface for Michael Ernst's Emacs Database.|
;; $Date: 1993/12/20 15:13:18 $|0.22|
;; This file has been folded Origami style with Jamie Lokier's "folding.el".
 
;;{{{ File Header

 
;; Copyright (C) 1993, Michael Burschik.
 
;; This file is intended to be used with GNU Emacs.
 
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.
 
;; Everyone is granted permission to copy, modify and redistribute
;; this file, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.
 

;;}}}
 
;;{{{ Providing a Feature
 
;; this is the BibTeX package for BibTeX.dba (for EDB)
;; provide its features

(require 'database) 
(provide 'edbibtex)
 
;;}}}
;;{{{ Accessor Functions
 
;;{{{ Functions for Looking-up
 
;; Warning: these accessor functions use functions which
;; are not defined in elisp --- they are provided by
;; `util-clmde.el', which is part of EDB
 
(defun BibTeX-get-string-expansion (string)
  "looks up the expansion of an abbreviation (its argument
string) in BibTeX-string-alist"
  (elt (assoc (upcase string) BibTeX-string-alist) 1))
 
(defun BibTeX-get-field-name (string)
  "looks up the field name associated with its string
argument in BibTeX-field-alist"
  (elt (assoc (upcase string) BibTeX-field-alist) 1))
 
(defun BibTeX-get-format-file-name (entry-type)
  "looks up the format file name associated with its string
argument in BibTeX-entry-type-alist"
  (elt (assoc (upcase entry-type) BibTeX-entry-type-alist) 1))
 
(defun BibTeX-get-required-field-list (entry-type)
  "looks up the list of required BibTeX fields associated
with an entry type (its string argument)"
  (elt (assoc (upcase entry-type) BibTeX-entry-type-alist) 2))
 
;;}}}
;;{{{ Functions for Building Lists
 
;;{{{ BibTeX-get-fields
 
;; I'm not sure this is very useful, but if you want to
;; change the acceptable fields, you only have to do it
;; in one place, i.e. in BibTeX-field-alist
 
(defun BibTeX-get-fields (fields)
  "This extracts a list of field names from `BibTeX-field-alist'."
  (let ((result '(BibTeX-entry-type
		  BibTeX-tag
		  BibTeX-val
		  BibTeX-var))
	(length (length fields)))
    (while (< 0 length)
      (setq length (1- length))
      (setq result (cons (elt (elt fields length) 1) result)))
    result))
 
;;}}}
;;{{{ BibTeX-get-entry-types
 
(defun BibTeX-get-entry-types (list)
  "This builds a completion alist of acceptable entry types from
BibTeX-entry-type-alist."
  (let ((length (length list))
	(result nil))
    (while (< 0 length)
      (setq length (1- length))
      (setq result (cons (cons (elt (elt list length) 0)
			       (elt (elt list length) 0))
			 result)))
    result))
 
;;}}}
;;{{{ BibTeX-get-alternate-formats
 
(defun BibTeX-get-alternate-formats (fields)
  "This extracts a list of alternate formats from
`BibTeX-entry-type-alist'."
  (let ((result nil)
	(length (length fields)))
    (while (< 0 length)
      (setq length (1- length))
      (setq result (cons (cons (elt (elt fields length) 0)
			       (elt (elt fields length) 1))
			 result)))
    result))
 
;;}}}
 
;;}}}
 
;;}}}
;;{{{ Delimiting Fields and Records
 
;;{{{ Delimiting Records
 
;; This could have been done with regular expressions, of course,
;; but this way it seemed safer, since the BibTeX files I looked
;; at all tended to have all kinds of stuff between the entries.
;; Unfortunately, folding information and comments will be lost
;; converting the database into EDB internal representation.
 
(defun BibTeX-delimit-record (prev-end-pos)
  "This is the sepinfo-sep-function, which takes the end position
of the previous record as an argument and returns a dotted list
of the end position of this record and the starting position of
the next record."
  (if (re-search-forward "^[ \t]*@\\([A-z]*\\)\\([({]\\)" nil t 2)
      (cons (match-beginning 0)
	    (match-beginning 0))
    (cons (point-max) nil)))
 
;;}}}
;;{{{ Delimiting Fields
 
;;{{{ BibTeX-find-value
 
(defun BibTeX-find-value (field)
  "This is supposed to delimit the value of a field. The
function first looks for an acceptable delimiting character,
such as `{' or `\"'. If it can't find one, then it looks for
numbers and abbreviations, in this order. So don't use
abbreviations that start with digits."
  (let ((delimiter (char-after (point))))
    (cond ((char-equal ?\{ delimiter)
	   (buffer-substring (1+ (point))
			     (BibTeX-find-matching-brace)))
	  ((char-equal ?\" delimiter)
	   (buffer-substring (1+ (point))
			     (BibTeX-find-matching-quote)))
	  ((looking-at "\\([0-9]+\\)")
	   (buffer-substring (match-beginning 1) (match-end 1)))
	  ((looking-at "\\([^ \"#%'(),={}\n\t]+\\)")
	   (BibTeX-find-abbreviation
	    field (upcase (buffer-substring
			   (match-beginning 1)
			   (match-end 1))))))))
;;	  (re-search-forward "\\([,}]\\|[ \t\n]*#[ \t\n]*\\)" nil t)
 
;;}}}
;;{{{ BibTeX-find-abbreviation
 
(defun BibTeX-find-abbreviation (field abbreviation)
  (let ((expansion (BibTeX-get-string-expansion abbreviation)))
    (if expansion
	(if BibTeX-expand-strings
	    expansion
	  (concat "#" abbreviation "#"))
      (progn
	(setq BibTeX-string-warnings
	      (concat
	       BibTeX-string-warnings
	       (format "Undefined abbreviation %s in field %s.\n"
		       ;; change this to some kind of assoc
		       abbreviation (upcase (symbol-name field)))))
	(concat "#" abbreviation "#")))))
 
;;}}}
;;{{{ BibTeX-find-concatenation
 
(defun BibTeX-find-concatenation (result-record current-field)
  "This delimits a field value with possible concatenation."
  (let ((current-value "")
	(in-value t))
    (while in-value
      (setq current-value
	    (concat current-value
		    (BibTeX-find-value current-field)))
      (skip-chars-forward "^,#}")
      (let ((flag (following-char)))
	(cond ((char-equal flag ?\})
	       (setq in-value nil))
	      ((char-equal flag ?,)
	       (setq in-value nil))
	      ((char-equal flag ?\#)
	       (skip-chars-forward " #\t\n")))))
    (record-set-field result-record current-field
		      current-value database)))
 
;;}}}
 
;;}}}
;;{{{ Auxiliary Functions
 
(defun BibTeX-find-end (delimiter)
  "This is supposed to find the end of a BibTeX entry, i.e.
a record. The function will unfortunately be confused by
unbalanced brackets or braces, so keep your fingers crossed
and only use braces to delimit your entries, which is safer."
  (if (string-equal delimiter "(")
      (BibTeX-find-matching-bracket)
    (BibTeX-find-matching-brace)))
 
;; This could probably be made safer by looking out for
;; quotes, thus allowing unbalanced brackets within quotes
(defun BibTeX-find-matching-bracket ()
  (let ((bracket-level 1)
	(in-quote nil))
    (while (< 0 bracket-level)
      (skip-chars-forward "^()")
      (if (char-equal (following-char) ?\()
	  (setq bracket-level (1+ bracket-level))
	(setq bracket-level (1- bracket-level))))
    (point)))
 
(defun BibTeX-find-matching-quote ()
  (let ((found nil))
    (while (not found)
      (forward-char)
      (skip-chars-forward "^\"")
      ;; this should take care of things like {\"a} and
      ;; "`Ein Haus"', as in German TeX
      (if (and (not (char-equal (preceding-char) ?\\))
	       (not (looking-at "\"[`']")))
	  (setq found t)))
    (point)))
 
(defun BibTeX-find-matching-brace ()
  (let ((brace-depth 1))
    (while (< 0 brace-depth)
      (forward-char)
      (skip-chars-forward "^{}")
      (if (not (char-equal (preceding-char) ?\\))
	  (if (char-equal (following-char) ?\{)
	      (setq brace-depth (1+ brace-depth))
	    (setq brace-depth (1- brace-depth)))))
    (point)))
 
;;}}}
 
;;}}}
;;{{{ Reading the Database
 
;;{{{ BibTeX-rrfr
 
;; This function assumes the buffer is narrowed to the region.
;; It also expects to find a valid BibTeX entry. This should
;; be safe, since we took the trouble of looking for one whilst
;; delimiting the record.
 
(defun BibTeX-rrfr ()
  "This is the read-record-from-region function, which checks
if this record is a string, a preamble or a standard entry and
then hands it over to the appropriate record reading function."
  (if (re-search-forward "@\\([A-z]*\\)[({]" nil t)
      (let ((entry-type
	     (upcase (buffer-substring (match-beginning 1)
				       (match-end 1)))))
	(if (assoc entry-type BibTeX-entry-type-alist)
	    (cond ((string-equal entry-type "STRING")
		   (BibTeX-rrfr-string))
		  ((string-equal entry-type "PREAMBLE")
		   (BibTeX-rrfr-preamble))
		  (t
		   (BibTeX-rrfr-item entry-type)))))))
 
;;}}}
;;{{{ BibTeX-rrfr-string
 
(defun BibTeX-rrfr-string ()
  "This will set the fields of the current string record."
  (let ((result-record (make-record database)))
    (record-set-field result-record 'BibTeX-entry-type "STRING" database)
    (re-search-forward "\\([^ \"#%'(),={}\n\t]*\\)[ \t]*=[ \t]*" nil t)
    (let ((abbreviation
	   (upcase (buffer-substring (match-beginning 1) 
				     (match-end 1))))
	  (expansion (BibTeX-find-value 'BibTeX-val)))
      (record-set-field result-record 'BibTeX-var abbreviation database)
      (record-set-field result-record 'BibTeX-val expansion database)
      ;; update the list of strings
      (setq BibTeX-string-alist
	    (cons (list abbreviation expansion) BibTeX-string-alist)))
    result-record))
 
;;}}}
;;{{{ BibTeX-rrfr-preamble
 
(defun BibTeX-rrfr-preamble ()
  "This will set the preamble record."
  (skip-chars-forward " \t\n")
  (let ((result-record (make-record database)))
    (record-set-field result-record 'BibTeX-entry-type "PREAMBLE" database)
    (BibTeX-find-concatenation result-record 'BibTeX-val)
    result-record))
 
;;}}}
;;{{{ BibTeX-rrfr-item
 
(defun BibTeX-rrfr-item (name)
  "This will set the fields of the current standard record."
  (let ((result-record (make-record database)))
    (record-set-field result-record 'BibTeX-entry-type name database)
    (re-search-forward "[ \t]*\\([^ ,\n\t]+\\)[ \t\n]*," nil t)
    (record-set-field result-record
		      'BibTeX-tag 
		      (buffer-substring (match-beginning 1)
					(match-end 1))
		      database)
    (while (re-search-forward "\\(\\w*\\)[ \t]*=[ \t]*" nil t)
      (let ((current-field 
	     (BibTeX-get-field-name
	      (buffer-substring (match-beginning 1)
				(match-end 1)))))
	(if current-field
	    (BibTeX-find-concatenation result-record current-field)
	  (error "Unknown field name: %s" 
		 (buffer-substring (match-beginning 1)
				   (match-end 1))))))
    (record-set-field result-record 'warnings BibTeX-string-warnings database)
    (setq BibTeX-string-warnings nil)
    result-record))
 
;;}}}
 
;;}}}
;;{{{ Writing the Database

 
;;{{{ BibTeX-wrfr
 
(defun BibTeX-wrfr (record)
  "This is the write-record-from-region function, which checks
if the current record is a string record or a standard record
and hands it over to the appropriate record writing function."
  (let ((entry-type
	 (upcase (record-field record 'BibTeX-entry-type database))))
    (cond ((string-equal entry-type "STRING")
	   (BibTeX-wrfr-string record))
	  ((string-equal entry-type "PREAMBLE")
	   (BibTeX-wrfr-preamble record))
	  (t
	   (BibTeX-wrfr-item record)))))
 
;;}}}
;;{{{ BibTeX-wrfr-string
 
(defun BibTeX-wrfr-string (record)
  "This formats and writes a string record."
  (insert (format "@STRING{ %s = {%s} }\n"
		  (record-field record 'BibTeX-var database)
		  (record-field record 'BibTeX-val database))))
 
;;}}}
;;{{{ BibTeX-wrfr-preamble
 
(defun BibTeX-wrfr-preamble (record)
  "This formats and writes a preamble record."
  (insert (format "@PREAMBLE{ {%s} }\n"
		  (record-field record 'BibTeX-val database))))
 
;;}}}
;;{{{ BibTeX-wrfr-item

 
(defun BibTeX-wrfr-item (record)
  "This formats and writes a standard record."
  (insert (format "@%s{ %s"
		  (upcase (record-field record 'BibTeX-entry-type database))
		  (record-field record 'BibTeX-tag database)))
  (let ((base-point (point))
	(length (length BibTeX-field-alist)))
    (while (< 0 length)
      (setq length (1- length))
      (let ((field-string
	     (elt (elt BibTeX-field-alist length) 0))
	    (field-value
	     (record-field
	      record (elt (elt BibTeX-field-alist length) 1) database)))
	(if (and field-value
		 (not (string-equal field-value "")))
	    (insert (format "\n , %-12s = {%s}"
			    field-string field-value)))))
    (insert "\n}\n")
    (goto-char base-point)
    (while (re-search-forward "#\\([^ \"#%'(),={}\n\t]+\\)#" nil t)
      (replace-match "} # \\1 # {"))
    (goto-char base-point)
    (while (re-search-forward "\\({} # \\| # {}\\)" nil t)
      (replace-match ""))
    (goto-char (point-max))))
 

;;}}}
 

;;}}}
;;{{{ Displaying the Database

 
;;{{{ BibTeX-set-format
 
(defun BibTeX-set-format ()
  "This will prompt the user for an entry type and set the display
format accordingly, if the entry type is undefined (this is the
case with entries newly added)."
  (interactive)
  (let ((entry-type
	 (dbf-displayed-record-field 'BibTeX-entry-type)))
    (if (or (null entry-type)
	    (string-equal entry-type ""))
	(progn
	  (dbf-displayed-record-set-field 'BibTeX-entry-type
	   (completing-read "Entry type: " BibTeX-entry-types nil t))
	  (db-emergency-restore-format t)))))
 
;;}}}
;;{{{ BibTeX-set-format-from-data

 
(defun BibTeX-set-format-from-data (record)
  "This checks the entry type of the record on display and
chooses an appropriate format from BibTeX-entry-type-alist."
  (if BibTeX-multiple-format
      (let ((entry-type
	      (upcase (record-field record
				    'BibTeX-entry-type
				    dbc-database))))
	(if (or (null entry-type)
		(string-equal entry-type ""))
	    (if BibTeX-use-default-format
		(let ((entry-type (elt BibTeX-default-format 0)))
		  (db-alternate-format entry-type)
		  (record-set-field record 'BibTeX-entry-type
				    entry-type dbc-database))
	      (BibTeX-set-format))
	  (progn
	    (db-alternate-format entry-type)
	    (setq BibTeX-default-format
		  (assoc entry-type dbf-alternate-format-names)))))))
 

;;}}}
 

;;}}}
;;{{{ Validating Entries

 
;;{{{ BibTeX-validate-hook
 
;; this deserves additional functionality
 
(defun BibTeX-validate-hook (record)
  "This is a dummy function that makes it possible to call
BibTeX-validate-entry as dbf-after-record-change-function."
  (BibTeX-validate-entry))
 
;;}}}
;;{{{ BibTeX-validate-entry
 
(defun BibTeX-validate-entry ()
  "This checks the record on display for undefined abbreviations
and violations of the BibTeX entry type requirements, if it is
a standard entry. If it is a string entry, `BibTeX-string-alist',
is updated."
  (interactive)
  (let ((entry-type
	 (upcase (dbf-displayed-record-field 'BibTeX-entry-type))))
    (if (string-equal entry-type "STRING")
	(BibTeX-validate-string-entry)
      (BibTeX-validate-standard-entry entry-type))))
 
;;}}}
;;{{{ BibTeX-validate-standard-entry
 
(defun BibTeX-validate-standard-entry (entry-type)
  (let ((list-of-warnings nil)
	(current-field-name nil)
	(required-field-list
	 (BibTeX-get-required-field-list entry-type)))
    (while required-field-list
      (setq current-field-name (elt required-field-list 0)
	    required-field-list (cdr required-field-list))
      (cond ((atom current-field-name)
	     (setq list-of-warnings
		   (concat list-of-warnings
			   (BibTeX-validate-field current-field-name))))
	    ((consp current-field-name)
	     (setq list-of-warnings
		   (concat list-of-warnings
			   (if (eq (elt current-field-name 0) 'xor)
			       (BibTeX-validate-xor current-field-name)
			     (BibTeX-validate-or current-field-name)))))))
    (setq list-of-warnings
	  (concat list-of-warnings
		  (BibTeX-dbf-validate-strings)))
    (dbf-set-this-record-modified-p t)
    (dbf-displayed-record-set-field-and-redisplay
     'warnings list-of-warnings)))
 
;;}}}
;;{{{ BibTeX-validate-string-entry
 
(defun BibTeX-validate-string-entry ()
  "This checks if the abbreviation is preceded by \"#\"
\(which it should not be) and corrects the abbreviation,
if it is. `BibTeX-string-alist' is updated."
  (let ((abbreviation (dbf-displayed-record-field 'BibTeX-var))
	(expansion (dbf-displayed-record-field 'BibTeX-val)))
    (if (and abbreviation
	     (not (string-equal abbreviation "")))
	(let ((flag (substring abbreviation 0 1))
	      (body (substring abbreviation 1)))
	  (if (string-equal flag "#")
	      (progn
		(dbf-set-this-record-modified-p t)
		(dbf-displayed-record-set-field-and-redisplay
		 'abbreviation body)))))
    (if (not (BibTeX-get-string-expansion abbreviation))
	(setq BibTeX-string-alist
	      (cons (list abbreviation expansion)
		    BibTeX-string-alist)))))
 
;;}}}
;;{{{ BibTeX-validate-or
 
(defun BibTeX-validate-or (current-field-name)
  (let ((first-field (elt current-field-name 1))
	(second-field (elt current-field-name 2)))
    (if (and (BibTeX-validate-field first-field)
	     (BibTeX-validate-field second-field))
	(format "Both field %s and field %s have no value.\n"
		(upcase (symbol-name first-field))
		(upcase (symbol-name second-field))))))
 
;;}}}
;;{{{ BibTeX-validate-xor
 
(defun BibTeX-validate-xor (current-field-name)
  (let ((first-field (elt current-field-name 1))
	(second-field (elt current-field-name 2)))
    (cond ((and (BibTeX-validate-field first-field)
		(BibTeX-validate-field second-field))
	   (format "Both field %s and field %s have no value.\n"
		   (upcase (symbol-name first-field))
		   (upcase (symbol-name second-field))))
	  ((and (not (BibTeX-validate-field first-field))
		(not (BibTeX-validate-field second-field)))
	   (format "Both field %s and field %s have a value.\n"
		   (upcase (symbol-name first-field))
		   (upcase (symbol-name second-field)))))))
 
;;}}}
;;{{{ BibTeX-validate-field

 
(defun BibTeX-validate-field (current-field-name)
  "This function returns a warning if the field is either set
to `nil' or `\"\"', and `nil' if it is set to anything else."
  (let ((current-field-value
	 (dbf-displayed-record-field current-field-name)))
    (if (or (null current-field-value)
	    (string-equal current-field-value ""))
	(format "Field %s has no value.\n"
		(upcase (symbol-name current-field-name))))))
 

;;}}}
;;{{{ BibTeX-validate-string

;; this function is no longer adequate; I must write something better
;; that will handle string concatenation correctly

 
(defun BibTeX-validate-string (current-field-string
			       current-field-value)
  (if (and current-field-value
	   (not (string-equal current-field-value "")))
      (let ((flag (substring current-field-value 0 1)))
	(if (string-equal flag "#")
	    (let ((abbreviation (substring current-field-value 1 -1)))
	      (if (BibTeX-get-string-expansion abbreviation)
		  nil
		(format "Undefined abbreviation %s in field %s.\n"
			abbreviation (upcase current-field-string))))))))

;;}}}
;;{{{ BibTeX-dbf-validate-strings
 
(defun BibTeX-dbf-validate-strings ()
  (let ((field-list BibTeX-field-alist)
	(current-field nil)
	(current-field-string nil)
	(list-of-warnings nil))
    (while field-list
      (setq current-field-string (elt (elt field-list 0) 0)
	    current-field (elt (elt field-list 0) 1)
	    field-list (cdr field-list))
      (setq list-of-warnings
	    (concat list-of-warnings
		    (BibTeX-validate-string
		     current-field-string
		     (dbf-displayed-record-field
		      current-field)))))
    list-of-warnings))
 
;;}}}
 

;;}}}
;;{{{ Constants and Variables

 
;;{{{ BibTeX-entry-type-alist

 
;;; This alist is used to find the correct format file
;;; and to check and validate entry types.
(defconst BibTeX-entry-type-alist
  '(("ARTICLE" "article.fmt"
     (author title journal year))
    ("BOOK" "book.fmt"
     ((xor author editor) title publisher year))
    ("BOOKLET" "booklet.fmt"
     (title))
    ("CONFERENCE" "inproceedings.fmt"
     (author title booktitle year))
    ("INBOOK" "inbook.fmt"
     ((xor author editor) title (or chapter pages)))
    ("INCOLLECTION" "incollection.fmt"
     (author title booktitle publisher year))
    ("INPROCEEDINGS" "inproceedings.fmt"
     (author title booktitle year))
    ("MANUAL" "manual.fmt"
     (title))
    ("MASTERSTHESIS" "mastersthesis.fmt"
     (author title school year))
    ("MISC" "misc.fmt" nil)
    ("PHDTHESIS" "phdthesis.fmt"
     (author title school year))
    ("PREAMBLE" "preamble.fmt"
     (BibTeX-val))
    ("PROCEEDINGS" "proceedings.fmt"
     (title year))
    ("STRING" "string.fmt"
     (BibTeX-var BibTeX-val))
    ("TECHREPORT" "techreport.fmt"
     (author title institution year))
    ("UNPUBLISHED" "unpublished.fmt"
     (author title note)))
  "This constant contains a list of valid BibTeX entry types,
their required fields and of format files associated with them.
Under no circumstances should it be changed when editing a database.")
 

;;}}}
;;{{{ BibTeX-entry-types
 
(defconst BibTeX-entry-types
  (BibTeX-get-entry-types BibTeX-entry-type-alist)
  "This is a completion alist of acceptable entry types.")
 
;;}}}
;;{{{ BibTeX-field-alist
 
(defconst BibTeX-field-alist
  '(("ADDRESS" address)
    ("ANNOTE" annote)
    ("AUTHOR" author)
    ("BOOKTITLE" booktitle)
    ("CHAPTER" chapter)
    ("CROSSREF" crossref)
    ("EDITION" edition)
    ("EDITOR" editor)
    ("HOWPUBLISHED" howpublished)
    ("INSTITUTION" institution)
    ("JOURNAL" journal)
    ("KEY" key)
    ("MONTH" month)
    ("NOTE" note)
    ("NUMBER" number)
    ("ORGANIZATION" organization)
    ("PAGES" pages)
    ("PUBLISHER" publisher)
    ("SCHOOL" school)
    ("SERIES" series)
    ("TITLE" title)
    ("TYPE" type)
    ("VOLUME" volume)
    ("YEAR" year)
    ;; the following fields are non-standard
    ;; add your own here
    ("ABSTRACT" abstract)
    ("ERRATA" errata)
    ("ISBN" isbn)
    ("ISSN" issn)
    ("KEYWORDS" keywords)
    ("LANGUAGE" language)
    ("LCNUM" lcnum)
    ("UPDATED" updated)
    ("WARNINGS" warnings))
  "This constant contains a list of valid BibTeX field names,
some of which will be ignored by the standard BibTeX styles.")
 
;;}}}
;;{{{ BibTeX-string-alist
 
(defvar BibTeX-string-alist
  '(("JAN" "January")
    ("FEB" "February")
    ("MAR" "March")
    ("APR" "April")
    ("MAY" "May")
    ("JUN" "June")
    ("JUL" "July")
    ("AUG" "August")
    ("SEP" "September")
    ("OCT" "October")
    ("NOV" "November")
    ("DEC" "December"))
  "contains the string abbreviations and their expansions.")
 
;;}}}
;;{{{ Miscellaneous
 
(define-key database-view-mode-map "\C-c\C-v"
  'BibTeX-validate-entry)
 
(define-key database-view-mode-map "\C-c\C-f"
  'BibTeX-set-format)
 
(defvar BibTeX-multiple-format t
  "*turns entry type dependent format selection on")
 
(defvar BibTeX-default-format '("GENERIC" . "generic.fmt")
  "contains the format info of the last record on display")
 
(defvar BibTeX-expand-strings nil
  "*turns string expansion on")
 
(defvar BibTeX-string-warnings nil
  "contains warnings about undefined abbreviations")
 
(defvar BibTeX-use-default-format t
  "*turns the use of the default format for new database entries on")
 
;;}}}
 

;;}}}
 
;;{{{ Local Variables
 
 
 
;;;Local Variables:
;;;mode: emacs-lisp
;;;mode: folding
;;;End:
 
;;}}}
