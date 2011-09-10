;;;
;;; bibtex.el, GNU emacs major-mode for editing BibTeX files
;;; Copyright (C) 1992 Denise L. Draper
;;;
;;; LCD Archive Entry:
;;; bibtex|Denise Draper|ddraper@cs.washington.edu|
;;; Major mode for editing bibtex files|
;;; 18-Mar-1995|1.2|~/modes/bibtex.el.gz|
;;;
;;; This mode differs substantially from the bibtex-mode that has been
;;; distributed with gnu emacs.  The interface is different (using
;;; completion rather than commands to generate templates and fields),
;;; the formatting conventions are different (and more flexible), and
;;; the bells and whistles are different:  This mode offers customized
;;; templates (e.g. partially filled in for particular journals or
;;; conference proceedings), the ability to convert from one template
;;; type to another, and error checking by using BibTeX directly.
;;;
;;; This code parses bibtex files according to the definitions for
;;; BibTeX version 0.99c, 1988.  Ambiguities in the definition were
;;; checked by looking at the web source for BibTeX, so this should
;;; be just about as close as elisp can get to agreement with web...
;;; 
;;; Installation: change the definitions of bibtex-bibtex-name
;;;               and bibtex-spare-dir if necessary.
;;; Documentation: see the mode documentation string.
;;; Known Bugs: none (but beware of unmatched parens or quotes).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bibtex-mode () 
  "Major mode for editing bibtex files.
\\{bibtex-mode-map}
To create a new entry, type the first portion of the template name,
for example \"arti\" for \"article\", and then use \\[bibtex-fill-in-form] to complete
the rest of the form.  Individual fields can be done similarly, by typing
the initial portion of the field name and then typing \\[bibtex-fill-in-form].
Typing \\[bibtex-fill-in-form] twice will bring up a window of possible completions.

Use \\[bibtex-next] to position the point at the end of the string for
the current field, or to go from the end of the current field to
the end of the next field.  Similarly, \\[bibtex-previous] goes to the end of the
previous field.  Bibtex-mode automatically breaks long field values
as you type.

\\[bibtex-kill-field] kills the field that the point is on,
and \\[bibtex-kill-entry] kills the entire entry.  
\\[bibtex-delete-empty-fields] deletes any empty fields in the current entry.

To re-indent and align an entry, use \\[bibtex-reformat-entry].
To do this to every entry in a region, use \\[bibtex-reformat-region].
[Note that \\[bibtex-reformat-entry] will re-fill field values, possibly
deleting newlines that you inserted.  To force a line break inside a
field, use two consecutive newlines.]

\\[bibtex-xform-entry] does more sophisticated reformatting: this command will
prompt for a template name (e.g. \"article\"), and reformat the entry
according to that template type, adding fields if necessary.  This can
be used to standardize an entry to a given format (by giving it the
same template name), or to change an entry from one type to another
(say from  \"unpublished\" to \"inproceedings\").
\\[bibtex-xform-region]] will transform each entry in the region to
its own entry type (thus standardizing all entries).

\\[bibtex-check-buffer] and \\[bibtex-check-region] check the current buffer or region,
respectively, for errors by running BibTeX over them.


Caveats: <<<If something doesn't seem to be working right, or is
infinite looping, check to make sure the parens are balanced.>>> The
code tries to parse certain common sorts of formatting errors, such as
missing commas, but it can't handle unbalanced parens, braces or
double-quotes.  Also, fields are treated slightly differently
depending on which delimiters are used: emacs will ignore parens and
braces in double-quote-delimited fields, but will insist that they be
balanced inside of brace-delimited fields.  Finally, no useful
checking of field values is done while editing, or by reformatting,
only by \\[bibtex-check-region].

Entry to this mode calls the value of bibtex-mode-hook if that value
is non-nil.

------------------------
Customizing entry types, or what to put in bibtex-mode-hook:

You can add your own entry types, to reflect your own BibTeX
style, or to create partially filled-in templates.
An example:

  (bibtex-add-template
    \"aaai92\" \"inproceedings\"
    '(\"author\"
      \"title\"
      (\"booktitle\" \"{Proceedings of AAAI-92}\")
      (\"address\" \"{San Jose, CA}\" t)
      \"pages\"
      (\"year\" \"1992\" t)))

If this template were inserted into a bibtex file, it would look like:

@inproceedings( ,
  author=       {}, 
  title=	{}, 
  booktitle=    {Proceedings of AAAI-92}, address= {San Jose, CA}, 
  pages=        , year= 1992
)

The first argument is the name of the template name -- what you would type
before \\[bibtex-fill-in-form].  The second argument is the entry type
that it should turn into -- one of the standard BibTeX types.  The third
argument is formatting information: it is a list, with one element per
field.  Each element is either a string giving the name of the field,
or a list, where the first element of the list is the field name, the
second element of the list, if non-nil, is the field value, and the
third element of the list, if non-nil, indicates that this field should 
go on the same line as the previous field.  If only the field name is 
given, or if the field value is given as nil, then the default field
value (\"{}\" for string-valued fields, the empty string for numeric-
valued fields) is used.

Minor points:
* This mode makes no attempt to assure that the \"correct\" fields are
  given for any entry type -- that is left up to the designer of the template.
* Don't forget the extra quotes or braces to delimit a string-valued field
  value.
* String-valued fields are filled as they are inserted, which means that
  newlines tend to get removed.  Consecutive newlines are left in place.
* Calling bibtex-add-template with an existing template name overrides the
  previous definition.
* BibTeX ignores case in entry types and field names, but bibtex-mode is
  sensitive to it.  So if for some perverse reason, you wanted \"inbook\"
  to be different from \"InBook\", you could do that.
* To add a new field _name_, do something like the following:
    (setq bibtex-field-list
          (cons (cons \"abstract\" \"{}\") bibtex-field-list))
* It is more efficient to load a set of templates only once per emacs
  session.  I do this by hand-doing my own \"autoload\" in my .emacs file:

  (setq auto-mode-alist
        (cons '(\"\\\\.bib\" . bibtex-mode) auto-mode-alist))
  (defun bibtex-mode ()
    (interactive)
    (load \"/uns/lib/emacs/bibtex\")
    (load \"~ddraper/.bibtex-adds\")
    (bibtex-mode))"

  (interactive)
  (kill-all-local-variables)
  (setq mode-name "BibTeX")
  (setq major-mode 'bibtex-mode)
  (use-local-map bibtex-mode-map)
  (set-syntax-table bibtex-mode-syntax-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start bibtex-paragraph-start)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'bibtex-indent)
  (auto-fill-mode 1)
  (setq fill-column 78)
  (run-hooks 'bibtex-mode-hook))

;;-----------------------
;; Some useful primitives

(defmacro when (predicate &rest stuff)
  (` (if (, predicate) (progn (,@ stuff)))))

(defmacro unless (predicate &rest stuff)
  (` (if (, predicate) () (,@ stuff))))

(defmacro push (what ontowhere)
  (` (setq (, ontowhere) (cons (, what) (, ontowhere)))))

(defmacro dolist (listform &rest body)
  (` (mapcar (function (lambda (llx-randomname)
			 (setq (, (car listform)) llx-randomname)
			 (,@ body)))
	     (,@ (cdr listform)))))

(defmacro bibtex-point-at (&rest directions)
  (` (save-excursion (,@ directions) (point) (,@ nil))))

;;------------------------
;; standard mode variables

(defvar bibtex-mode-syntax-table nil)
(unless bibtex-mode-syntax-table
  (setq bibtex-mode-syntax-table (copy-syntax-table (standard-syntax-table)))
  ;; bibtex does not have a comment character (anything outside an entry is a
  ;; comment, and nothing inside an entry is), and its list syntax is normal.
  ;; however, it accepts many unusual characters in identifiers:
  (modify-syntax-entry ?! "w" bibtex-mode-syntax-table)
  (modify-syntax-entry ?& "w" bibtex-mode-syntax-table)
  (modify-syntax-entry ?+ "w" bibtex-mode-syntax-table)
  (modify-syntax-entry ?* "w" bibtex-mode-syntax-table)
  (modify-syntax-entry ?- "w" bibtex-mode-syntax-table)
  (modify-syntax-entry ?. "w" bibtex-mode-syntax-table)
  (modify-syntax-entry ?/ "w" bibtex-mode-syntax-table)
  (modify-syntax-entry ?: "w" bibtex-mode-syntax-table)
  (modify-syntax-entry ?\; "w" bibtex-mode-syntax-table)
  (modify-syntax-entry ?< "w" bibtex-mode-syntax-table)
  (modify-syntax-entry ?> "w" bibtex-mode-syntax-table)
  (modify-syntax-entry ?? "w" bibtex-mode-syntax-table)
  (modify-syntax-entry ?@ "w" bibtex-mode-syntax-table)
  (modify-syntax-entry ?^ "w" bibtex-mode-syntax-table)
  (modify-syntax-entry ?[ "w" bibtex-mode-syntax-table)
  (modify-syntax-entry ?] "w" bibtex-mode-syntax-table)
  (modify-syntax-entry ?^ "w" bibtex-mode-syntax-table)
  (modify-syntax-entry ?_ "w" bibtex-mode-syntax-table)
  (modify-syntax-entry ?` "w" bibtex-mode-syntax-table)
  (modify-syntax-entry ?| "w" bibtex-mode-syntax-table)
  (modify-syntax-entry ?~ "w" bibtex-mode-syntax-table)
  ;; non identifier characters
  (modify-syntax-entry ?= "." bibtex-mode-syntax-table)
  (modify-syntax-entry ?% "." bibtex-mode-syntax-table))
     ; this last is a bug in bibtex, I think: bibtex explicitly says that
     ; % is not a comment character, but it uses the same syntax table as
     ; TeX, which of course _does_ treat % as a special character...

(defvar bibtex-mode-map nil)
(unless bibtex-mode-map
  (setq bibtex-mode-map (make-sparse-keymap))
  (define-key bibtex-mode-map "\C-k"     'bibtex-kill-line)
  (define-key bibtex-mode-map "\C-c\C-h" 'bibtex-show-templates)
  (define-key bibtex-mode-map "\C-c\C-r" 'bibtex-check-region)
  (define-key bibtex-mode-map "\C-c\C-b" 'bibtex-check-buffer)
  (define-key bibtex-mode-map "\C-c\C-e" 'bibtex-delete-empty-fields)
  (define-key bibtex-mode-map "\C-c\C-x" 'bibtex-xform-entry)
  (define-key bibtex-mode-map "\C-c\C-k" 'bibtex-kill-field)
  (define-key bibtex-mode-map "\C-c\C-d" 'bibtex-kill-entry)
  (define-key bibtex-mode-map "\C-c\C-p" 'bibtex-previous)
  (define-key bibtex-mode-map "\C-c\C-n" 'bibtex-next)
  (define-key bibtex-mode-map "\C-c\t"   'bibtex-reformat-entry)
  (define-key bibtex-mode-map "\M-\t"    'bibtex-fill-in-form))


;;;-----------
;;; Parameters

;; bibtex doesn't require that the @ be the first thing on the
;; line, but it usually is, and will distinguish it (hopefully)
;; from any @'s nested inside entries.

(defvar bibtex-paragraph-start "^@\\s *\\w+\\s *\\s("
  "Regexp matching the beginning of an entry in BibTeX mode")

(defvar bibtex-field-name-column 2
  "Column number on which to align field names in BibTeX mode")

(defvar bibtex-field-value-column 16
  "Column number on which to align field values in BibTeX mode")

(defvar bibtex-bibtex-name "bibtex"
  "*Name of command to invoke bibtex")

(defvar bibtex-spare-dir "/usr/tmp/"
  "*Name of directory to write temporary files on")

;;; -----------
;;; Field Names

(defvar bibtex-braces "{}")          ; string-valued field
(defvar bibtex-empty-string "")      ; numeric or symbol-valued field

(defvar bibtex-field-list '(
  ("address"      . bibtex-braces)
  ("annote"       . bibtex-braces)
  ("author"       . bibtex-braces)
  ("booktitle"    . bibtex-braces)
  ("chapter"      . bibtex-empty-string)
  ("crossref"     . bibtex-braces)
  ("edition"      . bibtex-braces)
  ("editor"       . bibtex-braces)
  ("howpublished" . bibtex-braces)
  ("institution"  . bibtex-braces)
  ("journal"      . bibtex-braces)
  ("key"          . bibtex-braces)
  ("month"        . bibtex-empty-string)
  ("note"         . bibtex-braces)
  ("number"       . bibtex-empty-string)
  ("organization" . bibtex-braces)
  ("pages"        . bibtex-braces)
  ("publisher"    . bibtex-braces)
  ("school"       . bibtex-braces)
  ("series"       . bibtex-braces)
  ("title"        . bibtex-braces)
  ("type"         . bibtex-empty-string)
  ("volume"       . bibtex-empty-string)
  ("year"         . bibtex-empty-string))
  "List of BibTeX fields.  Format is (field-name . default-value).")

(defmacro bibtex-field-default-value (fieldname)
  (` (eval (or (cdr (assoc (, fieldname) bibtex-field-list)) bibtex-braces))))

(defmacro bibtex-empty-field-p (string)
  (` (or (not (, string))
	 (string= (, string) bibtex-empty-string)
	 (string= (, string) bibtex-braces)
	 (string= (, string) "\"\""))))

;;; ----------------
;;; Entry Templates.
;;; The mode documentation explains their format.
;;; A standard template set is defined at the bottom of this file.

(defvar bibtex-template-list
  '(("string" . nil))
  "Association list of entry templates for BibTeX mode.
See mode documentation for how to change (and why).")

(defun bibtex-add-template (key-name form-name field-format)
  (let ((old-temp (assoc key-name bibtex-template-list)))
    (if old-temp
	(setcdr old-temp (list form-name field-format))
      (push (list key-name form-name field-format) bibtex-template-list))))

;; access macros for internal field templates

(defmacro bibtex-ft-name (ft)
  (` (if (consp (, ft)) (car (, ft)) (, ft))))

(defmacro bibtex-ft-data (ft)
  (` (if (consp (, ft)) (nth 1 (, ft)))))

(defmacro bibtex-ft-same-line (ft)
  (` (if (consp (, ft)) (nth 2 (, ft)))))

;;; -----------------------
;;; Primitive Motion Macros
;;; The following macros do sexp motion inside of bibtex entries the way
;;; I want it (they work identically whether you are on a sexp, or in the
;;; space before it), plus they do not cause errors, instead returning
;;; a value indicating existence of sexp.

(defmacro bibtex-sexp ()
  ;; move to the beginning of the current sexp, or to the next
  ;; sexp if we are in between, or to the end of the entry,
  ;; if there is no sexp.
  (` (condition-case nil (progn (forward-sexp 1) (backward-sexp 1) t)
       (error
	(when (re-search-forward "\\s)" (point-max) 1)
	  (backward-char 1))
	nil))))

(defmacro bibtex-next-sexp ()
  ;; move to the next sexp, or to end of entry if none.
  (` (condition-case nil (progn (forward-sexp 2) (backward-sexp 1) t)
       (error
	(condition-case nil (forward-sexp 1) (error nil))
	(when (re-search-forward "\\s)" (point-max) 1)
	  (backward-char 1))
	nil))))

(defmacro bibtex-prev-sexp ()
  ;; move to previous sexp, or to opening delimeter of entry if none.
  (` (progn
       (condition-case nil (progn (forward-sexp 1) (backward-sexp 1) t)
	 (error nil))
       (condition-case nil (progn (backward-sexp 1) t)
	 (error
	  (when (re-search-backward "\\s(" (point-min) 1)
	    (forward-char 1))
	  nil)))))


(defmacro bibtex-search-between-sexps (search-pat)
  ;; Look in the interstice before this sexp for search-pat.
  ;; If found, move point to before the pattern and return t, else
  ;; leave point unchanged and return false.  Assumes that we are at
  ;; the beginning of a sexp, inside an entry (call bibtex-sexp first
  ;; if you are not sure).
  (` (re-search-backward (, search-pat)
			 (bibtex-point-at (skip-chars-backward " \t\n,=#"))
			 t)))

;;;------------------------
;;; Point Motion Procedures
;;;
;;; Notation: an _entry_ is one of the things delimited by @xxx{ ... }.
;;; An _entry_type_ is the xxx in the @xxx.  A _field_ is one of the
;;; keyword-value pairs inside an entry (e.g. year = 1980).  The name or
;;; key of the entry (e.g. @xxx{ simmons-aaai-90, ...} ) is also considered
;;; a field.  A field _body_ is the contents of the value of a key-value
;;; field, if it is enclosed by delimters ("" or {}).

(defun bibtex-next ( &optional howmany )
  "Move point to the next field insertion point.
With numeric prefix, repeat that many times."
  (interactive "p")
  (let ((here (point))
	(loc (bibtex-locate)))
    (when (eq loc 'outside)         ; skip over outside stuff,
      (bibtex-next-entry))
    (when (eq loc 'on-entry)
      (down-list 1))                ; and unfriendly entry types.
    (while (bibtex-special-entry-type2-p)
      (bibtex-next-entry))

    (bibtex-end-of-field)           ; find insertion point in current field.
    (bibtex-back-to-field-insert)
    (when (>= here (point))         ; if we were that far already, go to
      (bibtex-end-of-field)         ; to next field...
      (unless (bibtex-sexp)         ; ... which may be in the next entry.
	(bibtex-next-entry))
      (bibtex-next)))
  (when (and howmany (> howmany 1))
    (bibtex-next (- howmany 1))))

(defun bibtex-previous ( &optional howmany )
  "Move point to the previous field insertion point.
With numeric prefix, repeat that many times."
  (interactive "p")
  (let ((loc (bibtex-locate))
	(here (point)))             ; from outside we aim for top of previous
    (when (memq loc '(outside on-entry))
      (bibtex-prev-entry))          ; entry (skipping bad ones).
    (while (bibtex-special-entry-type2-p)
      (bibtex-prev-entry))

    (cond ((memq (bibtex-locate) '(outside on-entry))
	   (forward-list 1)         ; if we are outside when we reach here,
	   (down-list -1)           ; means we want to start at last field
	   (bibtex-back-to-field-insert))       ; of this entry.
	  (t                        ; otherwise, already inside entry, so
	   (bibtex-end-of-field)    ; go to field insertion point.
	   (bibtex-back-to-field-insert)
	   (when (<= here (point))  ; if cursor was already before this point,
	     (bibtex-beginning-of-field)        ; skip to previous field.
	     (cond ((or (bibtex-prev-sexp)      ; (case for null 1st field.)
			(and (bibtex-sexp)
			     (bibtex-search-between-sexps ",")
			     (< (point) here)))
		    (bibtex-end-of-field)
		    (bibtex-back-to-field-insert))
		   (t               ; if no prev. fields in this entry, skip
		    (bibtex-beginning-of-entry) ; to previous entry.
		    (bibtex-previous)))))))
  (when (and howmany (> howmany 1))
    (bibtex-previous (- howmany 1))))


(defun bibtex-beginning-of-entry ()
  ;; Attempt to move to the beginning of the current entry, or of the
  ;; previous entry, if we are between entries, returning t if we find one.
  ;; Note: this is the most primitive location routine -- it is called
  ;;       BY bibtex-locate.
  ;; The complication is caused by the fact that point may be located _in_
  ;; paragraph-start -- how do do this efficiently/correctly?
  (or (looking-at paragraph-start)
      (and (re-search-forward paragraph-start (point-max) 'move)
	   (re-search-backward paragraph-start (point-min) 'move 2))
      (re-search-backward paragraph-start (point-min) 'move)))


(defun bibtex-next-entry ()
  (unless (re-search-forward paragraph-start (point-max) t)
    (error "no more entries")))     ; nicer error message

(defun bibtex-prev-entry ()
  (if (eq (bibtex-locate) 'outside)
      (or (bibtex-beginning-of-entry) (error "no more entries"))
    (progn
      (bibtex-beginning-of-entry)
      (condition-case nil (backward-char 1) (error "no more entries"))
      (or (bibtex-beginning-of-entry) (error "no more entries")))))


(defun bibtex-beginning-of-field (&optional location)
  (when (eq (or location (bibtex-locate)) 'in-body)
    (bibtex-beginning-of-body))
  (if (bibtex-which-field)
      (bibtex-sexp)
    (bibtex-prev-sexp))
  (unless (save-excursion (backward-char) (looking-at "\\s("))
    (while (and (not (bibtex-search-between-sexps ","))
		(bibtex-search-between-sexps "[=#]")
		(bibtex-prev-sexp)))
    (bibtex-sexp)))


(defun bibtex-end-of-field (&optional location)
  ;; leaves point after closing comma, if there is one
  (when (eq (or location (bibtex-locate)) 'in-body)
    (bibtex-beginning-of-body))
  (when (bibtex-which-field)
    (bibtex-next-sexp)
    (while (and (not (bibtex-search-between-sexps ","))
		(bibtex-search-between-sexps "[=#]")
		(bibtex-next-sexp))))
  (bibtex-sexp)
  (if (bibtex-search-between-sexps ",")
      (forward-char 1)
    (skip-chars-backward " \t\n")))


(defun bibtex-back-to-field-insert ()
  ;; moves point from end of field back to where further insertion
  ;; should take place.
  (let ((last-insert (point)))
    (bibtex-sexp)
    (when (bibtex-search-between-sexps ",")
      (setq last-insert (point)))
    (cond ((or (bibtex-search-between-sexps "[=#]")
	       (not (bibtex-prev-sexp)))
	   (goto-char last-insert))
	  (t
	   (forward-sexp 1)
	   (skip-chars-backward """}")))))


(defun bibtex-beginning-of-body ()
  ;; assumes we are in a field body; go to beginning of body
  (let ((stats (bibtex-stats)))
				    ; up-list won't work if you are inside
    (while (nth 3 stats)	    ; a string, so get out of that first.
      (search-backward (make-string 1 (nth 3 stats)))
      (setq stats (bibtex-stats)))
    (when (> (nth 0 stats) 1)	    ; then get out of lists > 1 deep.
      (up-list (- 1 (nth 0 stats))))))


;;;-----------------
;;; Parsing Procedures
;;;

(defun bibtex-locate ()
  ;; figure out where point is. return one of
  ;; ('outside 'in-entry 'on-entry 'in-body), which all mean
  ;; obvious things except on-entry which means on the "@xxxx"
  ;; part.
  (save-excursion
    (let ((here (point)) (plist nil))
      (if (not (bibtex-beginning-of-entry))          ; look for "@"
	  'outside                                   ; if none, then...
	(if (not (re-search-forward "\\s(" here t))  ; is there a paren in-
	    'on-entry                                ; between?
	  (setq plist (parse-partial-sexp (point) here))  ; if yes, then parse
	  (cond ((< (nth 6 plist) 0) 'outside)       ; exited outer delims
		((or (nth 3 plist)                   ; in string
		     (> (nth 0 plist) 0)) 'in-body)  ; more deeply nested
		(t 'in-entry)))))))                  ; none of the above


(defun bibtex-stats ()
  ;; return various data about where we are, like paren depth.
  ;; only guaranteed to be meaningful if we are 'in-entry or 'in-body
  (save-excursion
    (let ((here (point)))
      (bibtex-beginning-of-entry)
      (parse-partial-sexp (point) here))))

(defun bibtex-starting-key-p ()
  ;; determine if we could plausibly be starting an entry or field key.
  ;; assume we are _not_ in a field body, so need only see that we
  ;; follow a word-type character, and precede a non-word-type char.
  (and (= (char-syntax (preceding-char)) ?w)
       (or (eobp) (and (not (= (char-syntax (following-char)) ?w))))))

(defun bibtex-entry-type ()
  ;; return the xxx from "@xxx{"
  (save-excursion
    (bibtex-beginning-of-entry)
    (re-search-forward "@\\s *")  ; skip to beginning of identifier
    (let ((e (point)))
      (forward-sexp)
      (buffer-substring e (point)))))

(defun bibtex-special-entry-type-p ()
  (let ((type (downcase (bibtex-entry-type))))
    (or (string= type "string")
	(string= type "comment")
	(string= type "preamble"))))

(defun bibtex-special-entry-type2-p ()
  ;; anybody have a better name for this?
  (let ((type (downcase (bibtex-entry-type))))
    (or (string= type "comment")
	(string= type "preamble"))))


(defun bibtex-which-field ()
  ;; Return true if bibtex-sexp would leave the point in the same field.
  ;; If the point is inside a field, then it is obvious.  However, if it
  ;; is after the comma but before the beginning of the next field, we have
  ;; to be slightly more subtle: visually, newlines delimit fields more
  ;; significantly than do commas.  Point is assumed to be at the correct
  ;; sexp level (that is, not inside a field body: locate == 'in-entry)
  ;;
  ;; Fields look like "sexp = sexp {# sexp}* ,", except for the first
  ;; one, which is "sexp ," and the last one which has no comma, and
  ;; some which may look like "sexp = ," or may be missing commas...
  ;; This code handles missing commas and field values, but will choke
  ;; if there are missing '='s or '#'s.
  ;;
  ;; The following code says: bibtex-sexp stays in the same field if
  ;; one of the following holds:
  ;;  1} bibtex-sexp moved us backwards (because we were inside sexp),
  ;;  2} there was a comma in the interstice before this sexp, and
  ;;        2a} we are before the comma, and
  ;;        2b} either there is no newline, or we are before it too,
  ;;  3} no comma, and
  ;;        3a} the interstice before this sexp contains a '=' or a '#', or
  ;;        3b} there is no previous sexp at all (at beginning of entry).
  (save-excursion
    (let ((pos (point)))
      (bibtex-sexp)
      (or (>= pos (point))                              ; 1}
	  (if (bibtex-search-between-sexps ",")         ; 2}
	      (and (> pos (point))                      ; 2a}
		   (or (not (progn (bibtex-sexp)        ; 2b}
				   (bibtex-search-between-sexps "\n")))
		       (> pos (point))))
	    (or
	     (bibtex-search-between-sexps "[=#]")       ; 3a}
	     (not (bibtex-prev-sexp))))))))             ; 3b}


(defun bibtex-parse-entry ()
  ;; Produce a template-list describing the entry under point.
  (save-excursion
    (let ((mark nil) (key nil) (value nil)
	  (list nil) (same-line nil))
      (bibtex-beginning-of-entry)
      (down-list 1)
      (unless (bibtex-sexp)
	  (error "no fields in entry?"))
      (cond ((bibtex-search-between-sexps ",")
	     (push nil list))                   ; the first field was empty
	    (t
	     (setq mark (point))                ; otherwise, get it
	     (forward-sexp 1)	  
	     (push (buffer-substring mark (point)) list)))

      (while (bibtex-sexp)	                ; get the rest of the fields.
	(setq same-line (not (bibtex-search-between-sexps "\n")))
	(bibtex-sexp)
	(setq mark (point))
	(forward-sexp 1)
	(setq key (buffer-substring mark (point)))
	(setq value
	      (when (bibtex-which-field)
		(bibtex-sexp)
		(setq mark (point))
		(bibtex-end-of-field 'in-entry)
		(skip-chars-backward ", \t\n")
		(when (< mark (point))
		  (bibtex-remove-extra-whitespace
		   (buffer-substring mark (point))))))
	(push (list key value same-line) list)
	(bibtex-end-of-field 'in-entry))
      (nreverse list))))

;;;-----------------
;;; Indentation and Formatting
;;;

(defmacro bibtex-indent-to-col (col)
  ;; give the functionality of indent-to-left-margin, but with an argument,
  ;; by using local scope
  (` (let ((left-margin (, col)))
       (indent-to-left-margin))))

(defmacro bibtex-indent-field-value (where)
  ;; indent field body to line up with 1+ opening delim of field value
  (` (bibtex-indent-to-col
      (+ 1 (save-excursion
	     (if (eq (, where) 'in-body)
		 (bibtex-beginning-of-body) (bibtex-sexp))
	     (current-column))))))

(defmacro bibtex-indent-field-name (mpos)
  ;; indent field name at beginning of line, and if field value is on
  ;; this line, add extra indentation after the '='
  (` (progn
       (bibtex-indent-to-col bibtex-field-name-column)
       (forward-sexp 1)
       (when (looking-at "[ \t]*=[ \t]*")
	 (replace-match "= ")
	 (indent-to-column bibtex-field-value-column)))))

(defun bibtex-indent ()
  "Indent fields and field contents nicely, else just insert tab."
  ;; one constraint in this function: since this function gets called
  ;; when the user is inserting text, this function cannot assume that
  ;; all braces, etc. are balanced.  we do assume that all delimiters
  ;; _before_ point are balanced, but we never look after the point.
  (interactive)
  (let ((mm (point-marker))
	(curpos nil) (loc nil) (jump nil))
    (back-to-indentation)	                    ; find out what is at
    (setq curpos (point-marker))                    ; beginning of line.
    (setq loc (bibtex-locate))
    (setq jump (> (marker-position mm) (marker-position curpos)))
    (cond ((eq loc 'outside)                        ; boring case: insert tab
	   (goto-char (marker-position curpos))
	   (insert ?\t)
	   (setq jump nil))
	  ((eq loc 'on-entry)                       ; even more boring:
	   (setq jump nil))                         ; do nothing
	  ((eq loc 'in-body)
	   (bibtex-indent-field-value 'in-body))
	  ;; remaining cases are all 'in-entry
	  ((not (bibtex-sexp))                      ; at end of all fields
	   (bibtex-indent-to-col 0))
          ((bibtex-search-between-sexps ",")        ; looking at field name
	   (goto-char (marker-position curpos))
	   (bibtex-indent-field-name curpos))
	  ((bibtex-search-between-sexps "#")        ; looking at field value
	   (goto-char (marker-position curpos))
	   (bibtex-indent-field-value 'in-entry))
	  ((bibtex-search-between-sexps "=")        ; 1st line of value, so
	   (goto-char (marker-position curpos))     ; nothing to line up with
	   (bibtex-indent-to-col bibtex-field-value-column))
	  (t                                        ; looking at field name
	   (bibtex-indent-field-name curpos)))
    (if jump                                        ; leave point at orig.
	(goto-char (marker-position mm))            ; position if it was after
      (progn                                        ; beginning of line, else
	(goto-char (marker-position curpos))        ; move to beginning of
	(back-to-indentation)))))                   ; line.


(defun bibtex-indent-and-fill-lines (beg end)
  (save-excursion
    (let ((endmarker (make-marker)))
      (goto-char beg)
      (set-marker endmarker end)
      (while (bibtex-sexp)
	(if (bibtex-search-between-sexps "\n\n")
	    (delete-char 1)
	  (bibtex-next-sexp)))
      (goto-char beg)
      (while (<= (point) (marker-position endmarker))
	(bibtex-indent-and-fill-line)
	(forward-line 1)))))

(defun bibtex-indent-and-fill-line ()
  ;; fill the current line. leaves point at the end of the (last) line.
  (back-to-indentation)
  (bibtex-indent)
  (end-of-line)
  (skip-chars-backward " \t")
  (let ((last-column nil))
    (while (and (> (current-column) fill-column)
		(not (equal last-column (current-column))))
      (setq last-column (current-column))
      (do-auto-fill))))

;;;---------------------
;;; Entry/Field Creation
;;;

(defun bibtex-create-entry (entry-type format-list from-data
				       &optional kill-empty)
  (let ((fd nil))
    (dolist (fd (cdr from-data))               ; standardize field names
      (setcar fd (downcase (car fd))))	    
    (unless (looking-at "^")                   ; begin on a newline
      (insert ?\n))
    (insert ?@ entry-type "( ")
    (when (car from-data)                      ; add 1st field from data,
      (insert (car from-data)))                ; if it exists
    (insert ?,)
    (dolist (fieldformat format-list)          ; put in fields from template
      (setq fd (assoc (downcase (bibtex-ft-name fieldformat)) from-data))
      (when fd (setq from-data (delq fd from-data)))
      (bibtex-create-field fieldformat fd kill-empty))
    (dolist (fd (cdr from-data))               ; and add extra fields from
      (bibtex-create-field fd nil kill-empty)) ; data
    (re-search-backward ",[ \n]*")             ; get rid of last comma
    (replace-match "\n)")
    (backward-char 2)                          ; position point at the
    (up-list -1)                               ; beginning of the entry
    (forward-char 2)                           ; (inside outer braces)
    (bibtex-indent-and-fill-lines
     (point) (bibtex-point-at (up-list 1) (backward-char 2)))))


(defun bibtex-create-field (fieldtemplate fd kill-empty)
  ;; insert field into text.  the value of the field comes from the
  ;; template, if provided, else from the from-data list, if provided.
  (let* ((fieldname  (bibtex-ft-name fieldtemplate))
	 (same-line  (bibtex-ft-same-line fieldtemplate))
	 (fieldvalue (or (bibtex-ft-data fieldtemplate)
			 (bibtex-ft-data fd)
			 (bibtex-field-default-value fieldname))))
    (unless same-line (insert ?\n))
    (unless (and kill-empty (bibtex-empty-field-p fieldvalue))
      (insert fieldname "= " fieldvalue ", "))))


;;;--------------------------------------------
;;; User-level creation: entry/field completion
;;;

(defun bibtex-fill-in-form ()
  "Given a partial template- or field-name, replace with the complete item."
  ;; this function is mostly error checking; the real work is done in
  ;; bibtex-fill-in-{field,entry}
  (interactive)
  (let* ((loc (bibtex-locate))
	 (match-list
	  (if (eq loc 'outside) bibtex-template-list bibtex-field-list))
	 (partial-word
	  (buffer-substring (bibtex-point-at (backward-sexp 1)) (point))))

    (cond ((memq loc '(on-entry in-body))
	   (error "Can't start an entry or field here"))
	  ((and (eq loc 'in-entry)
		(bibtex-special-entry-type-p))
	     (error "No fields in a string/comment/preamble"))
	  ((not (bibtex-starting-key-p))
	   (error "Bad point for adding entry or field"))
	  ((= (aref partial-word 0) ?@)                ; strip off ?@.
	   (setq partial-word (substring partial-word 1))))

    (if (and (eq match-list bibtex-template-list)
	     (eq last-command 'bibtex-fill-in-form))
	;; if this is the second consecutive time the user has invoked
	;; this command, pop up window to show possible completions.
	(bibtex-show-templates-aux
	 (mapcar (function (lambda (x) (assoc x bibtex-template-list)))
		 (all-completions partial-word bibtex-template-list)))
      ;; normal case: attempt to complete string, and replace with
      ;; either a new field or a new entry if we succeed.
      (delete-region (bibtex-point-at (backward-sexp 1)) (point))
      (let* ((match (bibtex-complete partial-word match-list))
	     (template (assoc match match-list)))
	(if (eq loc 'in-entry)
	    (bibtex-fill-in-field match template partial-word)
	  (bibtex-fill-in-entry match template partial-word))))))

(defun bibtex-fill-in-field (math template partial-word)
  (cond (template                             ; if non-nil, matched uniqely
	 (save-excursion                      ; put comma after previous field,
	   (unless (bibtex-search-between-sexps ",")  ; if needed
	     (skip-chars-backward " \t\n")
	     (insert ", ")))
	 (bibtex-create-field                 ; put in new field
	  (list (car template) nil t) nil nil)
	 (save-excursion
	   (bibtex-indent-and-fill-line))
	 (unless (bibtex-sexp)
	   (bibtex-search-between-sexps ",") 
	   (delete-char 1))
	 (bibtex-back-to-field-insert))
	((or (eq match nil) (equal match "")) ; no match at all.
	 (insert partial-word)
	 (error "no such field key %s" partial-word))
	(t                                    ; partial match; show
	 (insert match))))                    ; user how much.

(defun bibtex-fill-in-entry (match template partial-word)
  (cond ((string= (car template) "string")
	 (unless (looking-at "^")             ; special case:
	   (insert ?\n))                      ; string entry type
	 (insert "@string( = {})")
	 (backward-char 6))
	(template                             ; normal case: add new
	 (bibtex-create-entry                 ; entry.
	  (nth 1 template) (nth 2 template) nil))
	((or (eq match nil) (equal match "")) ; no match at all.
	 (insert ?@ partial-word)
	 (error "no such template key %s" partial-word))
	(t                                    ; partial match.
	 (insert ?@ match))))

;;;-------------------
;;; Editing Procedures
;;;

(defun bibtex-kill-field ()
  "Kill the bibtex field that point is in."
  (interactive)
  (when (memq (bibtex-locate) '(outside on-entry))
    (error "not on a field"))
  (bibtex-beginning-of-field)
  (kill-region (point) (bibtex-point-at (bibtex-end-of-field 'in-entry)))
  (bibtex-fixup-after-field-deletion))


(defun bibtex-kill-entry ()
  "Kill the bibtex entry that point is in."
  (interactive)
  (when (eq (bibtex-locate) 'outside)
    (error "not on entry"))
  (bibtex-beginning-of-entry)
  (kill-region (point) (bibtex-point-at (forward-sexp 2) (end-of-line))))


(defun bibtex-delete-empty-fields ()
  "Delete all fields in this entry whose contents are empty."
  (interactive)
  (save-excursion
    (when (or (eq (bibtex-locate) 'outside)
	      (bibtex-special-entry-type-p))
      (error "not in entry"))
    (bibtex-beginning-of-entry)
    (down-list 1)
    (bibtex-sexp)
    (unless (bibtex-search-between-sexps ",")  ; skip first field, if present
      (bibtex-next-sexp))
    (while (bibtex-sexp)                       ; loop over remaining fields
      (forward-sexp 1)
      (if (and (bibtex-which-field)            ; takes care of "=," case
	       (bibtex-sexp)                   ; basically, matches {} or ""
	       (not (looking-at "\\({\\s *}\\|\"\\s *\"\\)\\s *[^#]")))
	  (bibtex-end-of-field 'in-entry)
	(backward-sexp 1)
	(delete-region (point)
		       (bibtex-point-at (bibtex-end-of-field 'in-entry)))
	(bibtex-fixup-after-field-deletion)))))


(defun bibtex-reformat-entry (&optional dont-worry)
  "Re-indent, fill or break lines, and correct commas for the current entry."
  (interactive)
  (let ((loc (bibtex-locate)))
    (when (or (eq loc 'outside)
	      (bibtex-special-entry-type-p))
      (error "not on entry"))
    (let ((entry-data (bibtex-parse-entry))
	  (entry-type (bibtex-entry-type))
	  (sexp-count 0))
      (unless (or dont-worry (eq loc 'on-entry))  ; try to put cursor back,
	(if (eq loc 'in-body)                     ; sort of where it came
	    (bibtex-beginning-of-body))           ; from, unless called from
	(while (bibtex-prev-sexp)                 ; bibtex-reformat-region
	  (setq sexp-count (+ 1 sexp-count))))
      (bibtex-beginning-of-entry)
      (delete-region (point) (bibtex-point-at (forward-sexp 2) (end-of-line)))
      (bibtex-create-entry entry-type (cdr entry-data) (list (car entry-data)))
      (unless dont-worry
	(while (> sexp-count 0)
	  (bibtex-next-sexp)
	  (setq sexp-count (- sexp-count 1)))))))


(defun bibtex-reformat-region (begpos endpos)
  "Reformat each entry in region."
  (interactive "r")
  (goto-char endpos)
  (let ((end (point-marker)))
    (goto-char begpos)
    (while (and (< (point) (marker-position end))
		(re-search-forward paragraph-start (marker-position end) t))
      (let ((type (bibtex-entry-type)))
	(cond ((bibtex-special-entry-type-p)   ; skip
	       (forward-sexp 1))
	      (t (bibtex-reformat-entry t))))
      (message "reformatting region...%d bytes remaining"
	       (- (marker-position end) (point))))
    (message "reformatting region...done")))


(defun bibtex-xform-entry ()
  "Transform current entry to new template (prompts for template name).
When given same entry type, this amounts to reformatting the entry
according to the standard template.  New fields might be added to the
entry, but old fields are never deleted.  However, if the new template
type supplies default values for some fields, these will override
any existing values."
  (interactive)
  (when (or (eq (bibtex-locate) 'outside)
	    (bibtex-special-entry-type-p))
    (error "not on entry"))
  (bibtex-xform-entry-aux
   (completing-read "Transform to type: "
		    bibtex-template-list nil t
		    (bibtex-entry-type))))


(defun bibtex-xform-entry-aux (new-name &optional kill-empties)
  ;; do the transformation.
  (let ((new-template (assoc new-name bibtex-template-list))
	(entry-data   (bibtex-parse-entry)))   ; read old entry data,
    (bibtex-beginning-of-entry)
    (delete-region                             ; delete the old entry,
     (point) (bibtex-point-at (forward-sexp 2) (end-of-line)))
    (bibtex-create-entry                       ; and put it back in new format.
     (nth 1 new-template) (nth 2 new-template) entry-data kill-empties)))


(defun bibtex-xform-region (begpos endpos delete-empty-p)
  "Reformat current region by transforming each entry to its own type.
With prefix argument, delete any empty fields in entries."
  (interactive "r\nP")
  (goto-char endpos)
  (let ((end (point-marker)))
    (goto-char begpos)
    (while (and (< (point) (marker-position end))
		(re-search-forward paragraph-start (marker-position end) t))
      (let ((type (bibtex-entry-type)))
	(cond ((bibtex-special-entry-type-p)   ; skip
	       (forward-sexp 1))
	      ((assoc type bibtex-template-list)
	       (bibtex-xform-entry-aux type delete-empty-p))
	      ((assoc (downcase type) bibtex-template-list)
	       (bibtex-xform-entry-aux (downcase type) delete-empty-p))
	      (t
	       (error "unrecognized template type %s" type))))
      (message "xforming region...%d bytes remaining"
	       (- (marker-position end) (point))))
    (message "xforming region...done")))


;;;---------------
;;; Show Templates
;;;

(defun bibtex-show-templates ()
  "List the defined templates in temporary buffer."
  (interactive)
  (bibtex-show-templates-aux bibtex-template-list))

(defun bibtex-show-templates-aux (list)
  (with-output-to-temp-buffer "*bibtex templates*"
    (mapcar 'bibtex-massage-show-line
	    (sort (copy-sequence list)
		  (function (lambda (x y)
			      (string< (car x) (car y)))))))
  (bury-buffer "*bibtex templates*"))

(defun bibtex-massage-show-line (bibtex-entry)
  (let* ((name (nth 0 bibtex-entry))
	 (type (nth 1 bibtex-entry))
	 (template (nth 2 bibtex-entry))
	 (accum nil))
    (dolist (ft template)         ; extract non-empty field data from template
      (unless (bibtex-empty-field-p (bibtex-ft-data ft))
	(push (bibtex-ft-data ft) accum)))
    (cond ((null accum)
	   (princ (format "%14s\n" name)))
	  (t
	   (let* ((ss (format  "%14s   %s %s" name type (nreverse accum)))
		  (len (min (- (window-width) 1) (length ss)))
		  (ss1 (substring ss 0 len)))
	     (princ ss1)
	     (princ "\n"))))))

;;;-----------------------------
;;; Using BibTeX to Check Format
;;;

(defun bibtex-check-region (beg-pos end-pos)
  "Run bibtex on the current region to catch error messages.
The output of bibtex is filtered somewhat, then displayed."
  (interactive "r")
  (save-excursion
    (let* ((errs-buffer (get-buffer-create "*bibtex errors*"))
	   (base (make-temp-name "bz"))
	   (temp-file1 (concat base ".bib"))
	   (temp-file2 (concat base ".aux"))
	   (cmd (concat bibtex-bibtex-name " " base
			"|& egrep -v '^while executing'")))

      (let ((default-directory bibtex-spare-dir))
	;; store region in temp file
	(write-region beg-pos end-pos temp-file1))

      (set-buffer errs-buffer)
      (setq default-directory bibtex-spare-dir)

      ;; use errs buffer to store contents to write into .aux file
      (erase-buffer)
      (insert "\\citation{*}\n\\bibdata{" base "}\n\\bibstyle{plain}\n")
      (write-region (point-min) (point-max) temp-file2)
      (erase-buffer)

      ;; now invoke bibtex, and catch the results
      (call-process "csh" nil errs-buffer nil "-fc" cmd)

      ;; massage the results to get rid of more egregious duplications
      (when (bibtex-move-matches "^.*for entry.*\n")
	(sort-regexp-fields nil "^.*for entry \\(.*\\)$" "\\1"
			    (point-min) (point-max))
	(call-process-region (point-min) (point-max)
			     "uniq" t errs-buffer nil))
      (goto-char (point-min))
      ;; and display the buffer
      (display-buffer errs-buffer t))))

(defun bibtex-check-buffer ()
  "Run bibtex on the current buffer to catch errors.
The output of bibtex is filtered somewhat, then displayed."
  (interactive)
  (bibtex-check-region (point-min) (point-max)))


(defun bibtex-move-matches (pattern)
  ;; this silly function moves text matching pattern to the end
  ;; of the buffer. Inspired by vi's "g/pattern/m$"
  ;; returns t if it found anything
  (save-excursion
    (let ((logical-end (point-max-marker))
	  (found nil)
	  (text nil))
      (goto-char (point-min))
      (while (re-search-forward pattern (marker-position logical-end) t)
	(setq found t)
	(setq text (buffer-substring (match-beginning 0) (point)))
	(delete-region (match-beginning 0) (point))
	(save-excursion
	  (goto-char (point-max))
	  (insert text)))
      found)))

;;;------
;;; Stuff

(defun bibtex-complete (word list)
  ;; call try-completion, but always return matched string
  (let ((m (try-completion word list)))
    (if (eq m t) word m)))

(defun bibtex-kill-line (arg)
  "Like kill-line, but get rid of extra white-space from beginning of line"
  (interactive "P")
  (kill-line arg)
  (when (memq (bibtex-locate) '(in-entry in-body))
    (fixup-whitespace)
    (bibtex-indent)))
		  
(defun bibtex-fixup-after-field-deletion ()
  ;; fix whitespace and commas after deleting a field
  (fixup-whitespace)                          ; get rid of multiple spaces,
  (when (looking-at "^\n")                    ; and empty lines
    (delete-char 1))
  (bibtex-indent)                             ; re-indent
  (unless (bibtex-sexp)                       ; if the previous field is now
    (when (bibtex-search-between-sexps ",")   ; the last field, delete its
      (delete-char 1)                         ; comma.
      (bibtex-sexp))))

(defun bibtex-remove-extra-whitespace (string)
  ;; get rid of fill-added whitespace: space at the beginning
  ;; of lines, newlines.  But keep multiple newlines, since
  ;; they were put there by the user.
  (let* ((len (length string))
	 (i 0) (j 0) (c nil) (skip nil) (nl 0)
	 (temp (make-string len ?0)))
    (while (< i len)
      (setq c (aref string i) i (+ 1 i))
      (cond ((= c ?\n)
	     (setq nl (+ 1 nl) skip t))
	    ((or (not skip) (not (= (char-syntax c) ? )))
	     (if (> nl 1)
		 (while (> nl 0)
		   (aset temp j ?\n)
		   (setq j (+ 1 j) nl (- nl 1)))
	       (when (= nl 1)
		 (aset temp j ? )
		 (setq j (+ 1 j) nl 0)))
	     (aset temp j c)
	     (setq j (+ 1 j) skip nil))))
    (substring temp 0 j)))


;;;-------------------
;;; Standard Templates
;;; Note: these are neither the minimal nor maximal set of fields
;;; for each entry type; rather they are the set I find I am most
;;; likely to use, arranged in a way which will usually be most
;;; compact.
  
(bibtex-add-template
 "article" "article"
 '("author"
   "title"
   "journal" ("volume" nil t) ("number" nil t) ("pages" nil t)
   "year"    ("month" nil t)))

(bibtex-add-template
 "book" "book"
 '("author"
   "title"
   "editor"
   "publisher" ("address" nil t)
   "year"))

(bibtex-add-template
 "booklet" "booklet"
 '("author"
   "title"
   "howpublished"
   "year"))

(bibtex-add-template
 "inbook" "inbook"
 '("author"
   "title" ("chapter" nil t)
   "publisher" ("address" nil t)
   "year"))

(bibtex-add-template
 "incollection" "incollection"
 '("author"
   "title"
   "booktitle" ("pages" nil t)
   "editor"
   "publisher" ("address" nil t)
   "year"))

(bibtex-add-template
 "inproceedings" "inproceedings"
 '("author"
   "title"
   "booktitle" ("address" nil t) ("pages" nil t)
   "editor" ("organization" nil t)
   "year" ("month" nil t)))

(bibtex-add-template
 "mastersthesis" "mastersthesis"
 '("author"
   "title"
   "school"
   "year" ("month" nil t)))

(bibtex-add-template
 "manual" "manual"
 '("title"
   "organization" ("address" nil t)
   "year" ("month" nil t)))

(bibtex-add-template
 "misc" "misc"
 '("author"
   "title"
   "howpublished"
   "year"))

(bibtex-add-template
 "phdthesis" "phdthesis"
 '("author"
   "title"
   "school"
   "year" ("month" nil t)))

(bibtex-add-template
 "proceedings" "proceedings"
 '("title"
   "editor" ("publisher" nil t)
   "address"
   "year" ("month" nil t)))

(bibtex-add-template
 "techreport" "techreport"
 '("author"
   "title"
   "institution"
   "type" ("number" nil t)
   "year" ("month" nil t)))

(bibtex-add-template
 "unpublished" "unpublished"
 '("author"
   "title"
   "note"
   "year"))

;; for debugging
;; (setq profile-functions-list '(bibtex-beginning-of-entry bibtex-beginning-of-field bibtex-end-of-field bibtex-back-to-field-insert bibtex-beginning-of-body bibtex-locate bibtex-stats bibtex-which-field bibtex-parse-entry bibtex-create-entry bibtex-create-field bibtex-indent bibtex-indent-and-fill-line bibtex-indent-and-fill-lines bibtex-fill-in-form bibtex-kill-field bibtex-kill-entry bibtex-xform-entry bibtex-xform-entry-aux bibtex-xform-region bibtex-reformat-entry bibtex-reformat-region bibtex-delete-empty-fields bibtex-remove-extra-whitespace bibtex-fixup-after-field-deletion))
