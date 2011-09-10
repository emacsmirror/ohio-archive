;;; BibTeX mode for GNU Emacs

;;; Russell Ritchie 3-oct-88
;;;  * bibtex-mode now tries to call 'BibTeX-mode-hook instead of
;;;    'bibtex-mode-hook (which is tried if 'BibTeX-mode-hook is unbound/nil).
;;;    Added "BibTeX-mode-menus.el" file for Scottish HCI Centre Menu code,
;;;    duplicates X Windows functionality in the HCI menu environment.

;;; Marc Shapiro 23-sep-88
;;;  * bibtex-clean-entry moves past end of entry.
;;;  * bibtex-clean-entry signals mandatory fields left empty.

;;; Marc Shapiro 18-jul-88
;;;  * Fixed bug in bibtex-flash-entry
;;;  * Moved all the entry type keystrokes to "C-c C-e something" (instead of
;;;    "C-c something" previously) to make room for more.  C-c C-e is
;;;    supposed to stand for "entry" [idea taken from mail-mode].  Moved
;;;    bibtex-pop-previous to C-c C-p and bibtex-pop-next to C-c C-n.
;;;  * removed binding for "\e[25~"
;;;  * replaced bibtex-clean-optionals by bibtex-clean-entry, bound to
;;;    C-c C-c

;;; Marc Shapiro 13-jul-88 [based on ideas by Sacha Krakowiak of IMAG]
;;;  * bibtex-pop-previous replaces current field with value of
;;;    similar field in previous entry.  May be called n times in a row
;;;    (or with arg n) to pop similar field of n'th previous entry.
;;;    There is also a bibtex-pop-next to get similar field of next
;;;    entry.
;;;  * C-c C-k now kills all empty optional fields of current entry, and
;;;    removes "OPT" for those optional fields which have text. 

;;; Marc Shapiro 14-dec-87
;;;   Cosmetic fixes.  Fixed small bug in bibtex-move-outside-of-entry.
;;; Skip Montanaro <steinmetz!sprite!montanaro> 7-dec-87, Shapiro 10-dec-87
;;;   before inserting an entry, make sure we are outside of a bib entry
;;; Marc Shapiro 3-nov-87
;;;   addition for France: DEAthesis
;;; Marc Shapiro 19-oct-1987
;;;   add X window menu option; bug fixes. TAB, LFD, C-c " and C-c C-o now
;;;   behave consistently; deletion never occurs blindly.
;;; Marc Shapiro <shapiro@inria.inria.fr> 15-oct-1986
;;;    align long lines nicely; C-c C-o checks for the "OPT" string;
;;;    TAB goes to the end of the string; use lower case; use
;;;    run-hooks

;;; Bengt Martensson <ubrinf!mond!bengt> 87-06-28
;;;   Original version

;;; NOTE by Marc Shapiro, 14-dec-87:
;;; (bibtex-x-environment) binds an X menu for bibtex mode to x-button-c-right.
;;; Trouble is, in Emacs 18.44 you can't have a mode-specific mouse binding,
;;; so it will remain active in all windows.  Yuck!

(defvar bibtex-mode-syntax-table nil "")
(defvar bibtex-mode-abbrev-table nil "")
(define-abbrev-table 'bibtex-mode-abbrev-table ())
(defvar bibtex-mode-map (make-sparse-keymap) "")
(defvar bibtex-pop-previous-search-point nil
  "Next point where bibtex-pop-previous should start looking for a similar
entry.")
(defvar bibtex-pop-next-search-point nil
  "Next point where bibtex-pop-next should start looking for a similar
entry.")

;;; A bibtex file is a sequence of entries, either string definitions
;;; or reference entries.  A reference entry has a type part, a
;;; key part, and a comma-separated sequence of fields.  A string
;;; entry has a single field.  A field has a left and right part,
;;; separated by a '='.  The left part is the name, the right part is
;;; the text.  Here come the definitions allowing to create and/or parse
;;; entries and fields:

;;; fields
(defun bibtex-cfield (name text)
  "Create a regexp for a bibtex field of name NAME and text TEXT"
  (concat ",[ \t\n]*\\("
	  name
	  "\\)[ \t\n]*=[ \t\n]*\\("
	  text
	  "\\)"))
(defconst bibtex-name-in-cfield 1
  "The regexp subexpression number of the name part in bibtex-cfield.")
(defconst bibtex-text-in-cfield 2
  "The regexp subexpression number of the text part in bibtex-cfield.")

(defconst bibtex-field-name "[A-Za-z][---A-Za-z0-9:_+]*"
  "Regexp defining the name part of a bibtex field.")
(defconst bibtex-field-text
  "\"[^\"]*[^\\\\]\"\\|\"\"\\|[0-9A-Za-z][---A-Za-z0-9:_+]*"
  "Regexp defining the text part of a bibtex field: either a string, or an empty string, or a constant.")
(defconst bibtex-field
  (bibtex-cfield bibtex-field-name bibtex-field-text)
  "Regexp defining the format of a bibtex field")

(defconst bibtex-name-in-field bibtex-name-in-cfield
  "The regexp subexpression number of the name part in bibtex-field")
(defconst bibtex-text-in-field bibtex-text-in-cfield
  "The regexp subexpression number of the text part in bibtex-field")

;;; references
(defconst bibtex-reference-type
  "@[A-Za-z]+"
  "Regexp defining the type part of a bibtex reference entry")
(defconst bibtex-reference-head
  (concat "^[ \t]*\\("
	  bibtex-reference-type
	  "\\)[ \t]*[({]\\("
	  bibtex-field-name
	  "\\)")
  "Regexp defining format of the header line of a bibtex reference entry")
(defconst bibtex-type-in-head 1
  "The regexp subexpression number of the type part in bibtex-reference-head")
(defconst bibtex-key-in-head 2
  "The regexp subexpression number of the key part in
bibtex-reference-head")

(defconst bibtex-reference
  (concat bibtex-reference-head
	  "\\([ \t\n]*" bibtex-field "\\)*"
	  "[ \t\n]*[})]")
  "Regexp defining the format of a bibtex reference entry")
(defconst bibtex-type-in-reference bibtex-type-in-head
  "The regexp subexpression number of the type part in bibtex-reference")
(defconst bibtex-key-in-reference bibtex-key-in-head
  "The regexp subexpression number of the key part in
bibtex-reference")

;;; strings
(defconst bibtex-string
  (concat "^[ \t]*@[sS][tT][rR][iI][nN][gG][ \t\n]*[({][ \t\n]*\\("
	  bibtex-field-name
	  "\\)[ \t\n]*=[ \t\n]*\\("
	  bibtex-field-text
	  "\\)[ \t\n]*[})]")
  "Regexp defining the format of a bibtex string entry")
(defconst bibtex-name-in-string 1
  "The regexp subexpression of the name part in bibtex-string")
(defconst bibtex-text-in-string 2
  "The regexp subexpression of the text part in bibtex-string")

;;; bibtex mode:

(defun bibtex-mode () 
  "Major mode for editing bibtex files.

\\{bibtex-mode-map}

A command such as \\[bibtex-Book] will outline the fields for a BibTeX book entry.

The optional fields start with the string OPT, and thus ignored by BibTeX.
The OPT string may be removed from a field with \\[bibtex-remove-OPT].
\\[bibtex-remove-double-quotes] removes the double-quotes around the text of
the current field.

The command \\[bibtex-clean-entry] cleans the current entry, i.e. (i) removes
double-quotes from entirely numerical fields, (ii) removes OPT from all
non-empty optional fields, (iii) removes all empty optional fields, and (iv)
checks that no non-optional fields are empty.

Use \\[bibtex-find-it] to position the dot at the end of the current field.
Use \\[bibtex-next-field] to move to end of the next field.

\\[bibtex-x-environment] binds a mode-specific X menu to control+right
mouse button.

Fields:
    address
           Publisher's address
    annote
           Long annotation used for annotated bibliographies (begins sentence)
    author
           Name(s) of author(s), in BibTeX name format
    booktitle
           Book title when the thing being referenced isn't the whole book.
           For book entries, the title field should be used instead.
    chapter
           Chapter number
    edition
           Edition of a book (e.g., \"second\")
    editor
           Name(s) of editor(s), in BibTeX name format.
           If there is also an author field, then the editor field should be
           for the book or collection that the work appears in
    howpublished
            How something strange has been published (begins sentence)
    institution
           Sponsoring institution
    journal
           Journal name (macros are provided for many)
    key
           Alphabetizing and labeling key (needed when no author or editor)
    month
           Month (macros are provided)
    note
           To help the reader find a reference (begins sentence)
    number
           Number of a journal or technical report
    organization
           Organization (sponsoring a conference)
    pages
           Page number or numbers (use `--' to separate a range)
    publisher
           Publisher name
    school
           School name (for theses)
    series
           The name of a series or set of books.
           An individual book will will also have it's own title
    title
           The title of the thing being referenced
    type
           Type of a technical report (e.g., \"Research Note\") to be used
           instead of the default \"Technical Report\"
    volume
           Volume of a journal or multivolume work
    year
           Year---should contain only numerals
---------------------------------------------------------
Entry to this mode calls the value of BibTeX-mode-hook or bibtex-mode-hook
(first non-nil only) if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (if bibtex-mode-syntax-table
(set-syntax-table bibtex-mode-syntax-table)
(setq bibtex-mode-syntax-table (make-syntax-table))
(set-syntax-table bibtex-mode-syntax-table)
(modify-syntax-entry ?\" ".")
(modify-syntax-entry ?$ "$$  ")
(modify-syntax-entry ?% "<   ")
(modify-syntax-entry ?'  "w   ")
(modify-syntax-entry ?@  "w   ")
(modify-syntax-entry ?\\ "\\")
(modify-syntax-entry ?\f ">   ")
(modify-syntax-entry ?\n ">   ")
(modify-syntax-entry ?~ " "))
  (use-local-map bibtex-mode-map)
  (setq major-mode 'bibtex-mode)


  (setq mode-name "BibTeX")
  (set-syntax-table bibtex-mode-syntax-table)
  (setq local-abbrev-table bibtex-mode-abbrev-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start "^[ \f\n\t]*$")

  (define-key bibtex-mode-map "\t" 'bibtex-find-it)
  (define-key bibtex-mode-map "\n" 'bibtex-next-field)
  (define-key bibtex-mode-map "\C-c""" 'bibtex-remove-double-quotes)
  (define-key bibtex-mode-map "\C-c\C-c" 'bibtex-clean-entry)
  (define-key bibtex-mode-map "\C-c?" 'describe-mode)
  (define-key bibtex-mode-map "\C-c\C-p" 'bibtex-pop-previous)
  (define-key bibtex-mode-map "\C-c\C-n" 'bibtex-pop-next)

  (define-key bibtex-mode-map "\C-c\C-e\C-a" 'bibtex-Article)
  (define-key bibtex-mode-map "\C-c\C-e\C-b" 'bibtex-Book)
  (define-key bibtex-mode-map "\C-c\C-e\C-d" 'bibtex-DEAthesis)
  (define-key bibtex-mode-map "\C-c\C-e\C-c" 'bibtex-InProceedings)
  (define-key bibtex-mode-map "\C-c\C-e\C-i" 'bibtex-InBook)
  (define-key bibtex-mode-map "\C-c\C-ei" 'bibtex-InCollection)
  (define-key bibtex-mode-map "\C-c\C-eI" 'bibtex-InProceedings)
  (define-key bibtex-mode-map "\C-c\C-e\C-m" 'bibtex-Manual)
  (define-key bibtex-mode-map "\C-c\C-em" 'bibtex-MastersThesis)
  (define-key bibtex-mode-map "\C-c\C-eM" 'bibtex-Misc)
  (define-key bibtex-mode-map "\C-c\C-o" 'bibtex-remove-OPT)
  (define-key bibtex-mode-map "\C-c\C-e\C-p" 'bibtex-PhdThesis)
  (define-key bibtex-mode-map "\C-c\C-ep" 'bibtex-Proceedings)
  (define-key bibtex-mode-map "\C-c\C-e\C-t" 'bibtex-TechReport)
  (define-key bibtex-mode-map "\C-c\C-e\C-s" 'bibtex-string)
  (define-key bibtex-mode-map "\C-c\C-e\C-u" 'bibtex-Unpublished)

  (auto-fill-mode 1)			; nice alignements
  (setq left-margin 17)

  (run-hooks (if (and (boundp 'BibTeX-mode-hook) BibTeX-mode-hook)
		 'BibTeX-mode-hook
	       'bibtex-mode-hook)))

(fset 'BibTeX-mode 'bibtex-mode)        ; Synonym...

(defun bibtex-move-outside-of-entry ()
  "Make sure we are outside of a bib entry"
  (cond ((or
	  (= (point) (point-max))
	  (= (point) (point-min))
	  (looking-at "[ \n]*@")
	  )
	 t)
	(t
	 (backward-paragraph)
	 (forward-paragraph)))
  (re-search-forward "[ \t\n]*" (point-max) t))

(defun bibtex-entry (entry-type required optional)
  (bibtex-move-outside-of-entry)
  (insert (concat "@" entry-type "{,\n\n}\n\n"))
  (previous-line 3)
  (insert (mapconcat 'bibtex-make-entry required ",\n"))
  (if required
      (if optional
	  (insert ",\n")))
  (insert (mapconcat 'bibtex-make-OPT-entry optional ",\n"))
  (up-list -1)
  (forward-char 1))

(defun bibtex-make-entry (str)
  (interactive "s")
  (concat "  " str " = \t"""""))

(defun bibtex-make-OPT-entry (str)
  (interactive "s")
  (concat "  OPT" str " = \t"""""))

(defun bibtex-Article ()
  (interactive)
  (bibtex-entry "Article" (list "author" "title" "journal" "year")
		(list "volume" "number" "pages" "month" "note")))

(defun bibtex-Book ()
  (interactive)
  (bibtex-entry "Book" (list "author" "title" "publisher" "year")
		(list "editor" "volume" "series" "address"
		      "edition" "month" "note")))

(defun bibtex-Booklet ()
  (interactive)
  (bibtex-entry "Booklet" (list "title")
		(list "author" "howpublished" "address" "month" "year" "note")))

;; France: Dipl\^{o}me d'Etudes Approfondies (similar to Master's)
(defun bibtex-DEAthesis ()
  (interactive)
  (bibtex-entry "DEAthesis" (list "author" "title" "school" "year")
		(list "address" "month" "note")))

(defun bibtex-InBook ()
  (interactive)
  (bibtex-entry "InBook" (list "author" "title" "chapter" "publisher" "year")
		(list "editor" "pages" "volume" "series" "address"
		      "edition" "month" "note")))

(defun bibtex-InCollection ()
  (interactive)
  (bibtex-entry "InCollection" (list "author" "title" "booktitle"
				     "publisher" "year")
		(list "editor" "chapter" "pages" "address" "month" "note")))

(defun bibtex-InProceedings ()
  (interactive)
  (bibtex-entry "InProceedings" (list "author" "title" "booktitle" "year")
		(list "editor" "pages" "organization" "publisher"
		      "address" "month" "note")))

(defun bibtex-Manual ()
  (interactive)
  (bibtex-entry "Manual" (list "title")
		(list "author" "organization" "address" "edition" "year"
		      "month" "note")))

(defun bibtex-MastersThesis ()
  (interactive)
  (bibtex-entry "MastersThesis" (list "author" "title" "school" "year")
		(list "address" "month" "note")))

(defun bibtex-Misc ()
  (interactive)
  (bibtex-entry "Misc" '()
		(list "author" "title" "howpublished" "year" "month" "note")))

(defun bibtex-PhdThesis ()
  (interactive)
  (bibtex-entry "PhdThesis" (list "author" "title" "school" "year")
		(list "address" "month" "note")))

(defun bibtex-Proceedings ()
  (interactive)
  (bibtex-entry "Proceedings" (list "title" "year")
		(list "editor" "publisher" "organization"
		      "address" "month" "note")))

(defun bibtex-TechReport ()
  (interactive)
  (bibtex-entry "TechReport" (list "author" "title" "institution" "year")
		(list "type" "number" "address" "month" "note")))


(defun bibtex-Unpublished ()
  (interactive)
  (bibtex-entry "Unpublished" (list "author" "title" "note")
		(list "year" "month")))

(defun bibtex-string ()
  (interactive)
  (bibtex-move-outside-of-entry)
  (insert "@string{ = """"}\n")
  (previous-line 1)
  (forward-char 8))


(defun bibtex-next-field ()
  "Finds end of text of next field."
  (interactive)
  (condition-case ()
      (progn
	(bibtex-inside-field)
	(re-search-forward ",[ \t\n]*" (point-max) 1)
	(bibtex-enclosing-field)
	(bibtex-inside-field))
    (error nil)))

(defun bibtex-find-it ()
  (interactive)
  "Go to end of text of current field."
  (condition-case ()
      (progn
	(bibtex-inside-field)
	(bibtex-enclosing-field)
	(goto-char (match-end bibtex-text-in-field))
	(bibtex-inside-field))
    (error nil)))

(defun bibtex-remove-OPT ()
  "Removes the 'OPT' starting optional arguments and goes to end of text"
  (interactive)
  (bibtex-inside-field)
  (bibtex-enclosing-field)
  (save-excursion
    (goto-char (match-beginning bibtex-name-in-field))
    (if (looking-at "OPT")
	(delete-char (length "OPT"))))
  (bibtex-inside-field))

(defun bibtex-inside-field ()
  "Try to avoid point being at end of a bibtex field."
  (skip-chars-backward " \t")
  (cond ((= (preceding-char) ?,)
	 (forward-char -1)))
  (cond ((= (preceding-char) ?\")
	 (forward-char -1))))

(defun bibtex-remove-double-quotes ()
  "Removes \"\" around string."
  (interactive)
  (save-excursion
    (bibtex-enclosing-field)
    (let ((start (match-beginning bibtex-text-in-field))
	  (stop (match-end  bibtex-text-in-field)))
      (goto-char stop)
      (forward-char -1)
      (if (looking-at "\"")
	  (delete-char 1))
      (goto-char start)
      (if (looking-at "\"")
	  (delete-char 1)))))

;;; X window menus for bibtex mode

(defun bibtex-x-help (arg)
  "Mouse commands for BibTeX mode"
  
  (let ((selection
	 (x-popup-menu
	  arg
	  '("BibTeX commands"
	    ("BibTeX entry types"
	     (" article in conference Proceedings " . bibtex-InProceedings)
	     ("        Article in journal         " . bibtex-Article)
	     ("               Book                " . bibtex-Book)
	     ("             Booklet               " . bibtex-Booklet)
	     ("         Master's Thesis           " . bibtex-MastersThesis)
	     ("            DEA Thesis             " . bibtex-DEAthesis)
	     ("            Phd. Thesis            " . bibtex-PhdThesis)
	     ("         Technical Report          " . bibtex-TechReport)
	     ("         technical Manual          " . bibtex-Manual)
	     ("      conference Proceedings       " . bibtex-Proceedings)
	     ("        a chapter in a Book        " . bibtex-InBook)
	     ("    an article in a Collection     " . bibtex-InCollection)
	     ("           miscellaneous           " . bibtex-Misc)
	     ("            unpublished            " . bibtex-Unpublished)
	     ("              string               " . bibtex-string)
	     )
	    ("Moving around and editing"
	     ("            next field             " . bibtex-next-field)
	     ("          to end of field          " . bibtex-find-it)
	     ("snatch from similar preceding field" . bibtex-pop-previous)
	     ("snatch from similar following field" . bibtex-pop-next)
	     ("            remove OPT             " . bibtex-remove-OPT)
	     ("           remove quotes           "
	      . bibtex-remove-double-quotes)
	     ("          clean up entry           " . bibtex-clean-entry)
	     )
	    ("help"
	     ("       describe BibTeX mode        " . describe-mode)
	     )))))
    (and selection (call-interactively selection))))

(defun bibtex-x-environment ()
  "Set up X menus for BibTeX mode.  Call it as bibtex-mode-hook, or interactively"
  (interactive)
  (require 'x-mouse)
  (define-key mouse-map x-button-c-right 'bibtex-x-help)
  )


(defun bibtex-pop-previous (arg)
  "Replace text of current field with the text of similar field in previous entry.
With arg, go up ARG entries.  Repeated, goes up so many times.  May be
intermixed with \\[bibtex-pop-next] (bibtex-pop-next)."
  (interactive "p")
  (bibtex-inside-field)
  (save-excursion
    ; parse current field
    (bibtex-enclosing-field)
    (let ((start-old-text (match-beginning bibtex-text-in-field))
	  (stop-old-text  (match-end bibtex-text-in-field))
	  (start-name (match-beginning bibtex-name-in-field))
	  (stop-name (match-end bibtex-name-in-field))
	  (new-text))
      (goto-char start-name)
      ; construct regexp for previous field with same name as this one
      (let ((matching-entry
	     (bibtex-cfield
	      (buffer-substring (if (looking-at "OPT")
				    (+ (point) (length "OPT"))
				  (point))
				stop-name)
	      bibtex-field-text)))
	
	; if executed several times in a row, start each search where the
	; last one finished
	(cond ((or (eq last-command 'bibtex-pop-previous)
		   (eq last-command 'bibtex-pop-next))
	       t
	       )
	      (t
	       (bibtex-enclosing-reference)
	       (setq bibtex-pop-previous-search-point (match-beginning 0))
	       (setq bibtex-pop-next-search-point (match-end 0))))
	(goto-char bibtex-pop-previous-search-point)
	
	; Now search for arg'th previous similar field
	(cond
	 ((re-search-backward matching-entry (point-min) t arg)
	  (setq new-text
		(buffer-substring (match-beginning bibtex-text-in-cfield)
				  (match-end bibtex-text-in-cfield)))
	  ; Found a matching field. Remember boundaries.
	  (setq bibtex-pop-next-search-point (match-end 0))
	  (setq bibtex-pop-previous-search-point (match-beginning 0))
	  (bibtex-flash-head)
	  ; Go back to where we started, delete old text, and pop new.
	  (goto-char stop-old-text)
	  (delete-region start-old-text stop-old-text)
	  (insert new-text))
	 (t				; search failed
	  (error "No previous matching BibTeX field."))))))
  (setq this-command 'bibtex-pop-previous))

(defun bibtex-pop-next (arg)
  "Replace text of current field with the text of similar field in next entry.
With arg, go up ARG entries.  Repeated, goes up so many times.  May be
intermixed with \\[bibtex-pop-previous] (bibtex-pop-previous)."
  (interactive "p")
  (bibtex-inside-field)
  (save-excursion
    ; parse current field
    (bibtex-enclosing-field)
    (let ((start-old-text (match-beginning bibtex-text-in-field))
	  (stop-old-text  (match-end bibtex-text-in-field))
	  (start-name (match-beginning bibtex-name-in-field))
	  (stop-name (match-end bibtex-name-in-field))
	  (new-text))
      (goto-char start-name)
      ; construct regexp for next field with same name as this one,
      ; ignoring possible OPT's
      (let ((matching-entry
	     (bibtex-cfield
	      (buffer-substring (if (looking-at "OPT")
				    (+ (point) (length "OPT"))
				  (point))
				stop-name)
	      bibtex-field-text)))
	
	; if executed several times in a row, start each search where the
	; last one finished
	(cond ((or (eq last-command 'bibtex-pop-next)
		   (eq last-command 'bibtex-pop-previous))
	       t
	       )
	      (t
	       (bibtex-enclosing-reference)
	       (setq bibtex-pop-previous-search-point (match-beginning 0))
	       (setq bibtex-pop-next-search-point (match-end 0))))
	(goto-char bibtex-pop-next-search-point)
	
	; Now search for arg'th next similar field
	(cond
	 ((re-search-forward matching-entry (point-max) t arg)
	  (setq new-text
		(buffer-substring (match-beginning bibtex-text-in-cfield)
				  (match-end bibtex-text-in-cfield)))
	  ; Found a matching field. Remember boundaries.
	  (setq bibtex-pop-next-search-point (match-end 0))
	  (setq bibtex-pop-previous-search-point (match-beginning 0))
	  (bibtex-flash-head)
	  ; Go back to where we started, delete old text, and pop new.
	  (goto-char stop-old-text)
	  (delete-region start-old-text stop-old-text)
	  (insert new-text))
	 (t				; search failed
	  (error "No next matching BibTeX field."))))))
  (setq this-command 'bibtex-pop-next))

(defun bibtex-flash-head ()
  "Flash at BibTeX reference head before point, if exists.  (Moves point)."
  (let ((flash))
    (cond ((re-search-backward bibtex-reference-head (point-min) t)
	   (goto-char (match-beginning bibtex-type-in-head))
	   (setq flash (match-end bibtex-key-in-reference)))
	  (t
	   (end-of-line)
	   (skip-chars-backward " \t")
	   (setq flash (point))
	   (beginning-of-line)
	   (skip-chars-forward " \t")))
    (if (pos-visible-in-window-p (point))
	(sit-for 1)
      (message "From: %s"
	       (buffer-substring (point) flash)))))



(defun bibtex-enclosing-field ()
  "Search for BibTeX field enclosing point.
Point moves to end of field; also, use match-beginning and match-end
to parse the field."
  (condition-case errname
      (bibtex-enclosing-regexp bibtex-field)
    (search-failed
     (error "Can't find enclosing BibTeX field."))))

(defun bibtex-enclosing-reference ()
  "Search for BibTeX reference enclosing point.
Point moves to end of reference; also, use match-beginning and match-end
to parse the reference."
  (condition-case errname
      (bibtex-enclosing-regexp bibtex-reference)
    (search-failed
     (error "Can't find enclosing BibTeX reference."))))

(defun bibtex-enclosing-regexp (regexp)
  "Search for REGEXP enclosing point.
Point moves to end of REGEXP.  See also match-beginning and match-end.
If an enclosing REGEXP is not found, signals search-failed; point is left in
an undefined location.

[Doesn't something like this exist already?]"
  
  (interactive "sRegexp: ")
  ; compute reasonable limits for the loop
  (let* ((initial (point))
	 (right (if (re-search-forward regexp (point-max) t)
		    (match-end 0)
		  (point-max)))
	 (left
	  (progn
	    (goto-char initial)
	    (if (re-search-backward regexp (point-min) t)
		(match-beginning 0)
	      (point-min)))))
    ; within the prescribed limits, loop until a match is found
    (goto-char left)
    (re-search-forward regexp right nil 1)
    (if (> (match-beginning 0) initial)
	(signal 'search-failed (list regexp)))	  
    (while (<= (match-end 0) initial)
      (re-search-forward regexp right nil 1)
      (if (> (match-beginning 0) initial)
	  (signal 'search-failed (list regexp))))
    ))

(defun bibtex-clean-entry ()
  "For all optional fields of current BibTeX entry: if empty, kill the whole field; otherwise, remove the \"OPT\" string in the name; if text numerical, remove double-quotes.  For all mandatory fields: if empty, signal error."
  (interactive)
  (bibtex-enclosing-reference)
  (goto-char (match-beginning 0))
  (let ((start (point)))
    (save-restriction
      (narrow-to-region start (match-end 0))
      (while (re-search-forward bibtex-field (point-max) t 1)
	(let ((begin-field (match-beginning 0))
	      (end-field (match-end 0))
	      (begin-name (match-beginning bibtex-name-in-field))
	      (end-name (match-end  bibtex-name-in-field))
	      (begin-text (match-beginning bibtex-text-in-field))
	      (end-text (match-end bibtex-text-in-field))
	      )
	  (goto-char begin-name)
	  (cond ((looking-at "OPT")
		 (goto-char begin-text)
		 (if (looking-at "\"\"") ; empty: delete whole field
		     (delete-region begin-field end-field)
		   ; otherwise: not empty, delete "OPT"
		   (goto-char begin-name)
		   (delete-char (length "OPT"))
		   (goto-char begin-field) ; and loop to go through next test
		   ))
		(t
		 (goto-char begin-text)
		 (cond ((looking-at "\"[0-9]+\"") ; if numerical,
			(goto-char end-text)
			(delete-char -1) ; delete enclosing double-quotes
			(goto-char begin-text)
			(delete-char 1)
			(goto-char end-field) ; go to end for next search
			(forward-char -2) ; to compensate for the 2 quotes deleted
			)
		       ((looking-at "\"\"") ; if empty quotes, complain
			(forward-char 1)
			(error "Mandatory field ``%s'' is empty"
			       (buffer-substring begin-name end-name)))
		       (t
			(goto-char end-field))))))))
    (goto-char start)
    (skip-chars-forward "@a-zA-Z")
    (bibtex-enclosing-reference)
    (goto-char (match-end 0))
    (skip-whitespace nil)))



