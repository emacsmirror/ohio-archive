;; ======================================================================
;; supercite.el -- Version 2.0

;; Citation and attribution package for various GNU emacs news and
;; electronic mail readers.  It has been tested with these commonly
;; available news and mail readers: VM 4.40, GNUS 3.12 and RMAIL
;; 18.55. It will interface to VM 4.40+ with no modifications, but
;; with GNUS 3.12 and RMAIL 18.55 modifications to distribution files
;; emacs/lisp/{sendmail, rnewspost}.el of GNU emacs version 18.55 are
;; required.  These modifications are supplied as diff files and as
;; overload functions.  See the file sup-misc.el for more information.

;; This package does not do any yanking of messages, but is run under
;; normal conditions via a hook by a reply function in the news/mail
;; reader package.  "M-x sc-describe" or "C-c ?" for more information
;; on how to meet the interface if you are writing a reader package,
;; or want to link supercite with a reader.

;; This package was derived from superyank.el, written by the same
;; author.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY.  No author or distributor accepts
;; responsibility to anyone for the consequences of using it or for
;; whether it serves any particular purpose or works at all, unless he
;; says so in writing.

;; This software was written as part of the author's official duty as
;; an employee of the United States Government and is thus in the
;; public domain.  You are free to use this software as you wish, but
;; WITHOUT ANY WARRANTY WHATSOEVER.  It would be nice, though if when
;; you use this code, you give due credit to the author.

;; ======================================================================
;; Author:
;;
;; NAME: Barry A. Warsaw             USMAIL: National Institute of Standards
;; TELE: (301) 975-3460                      and Technology (formerly NBS)
;; UUCP: {...}!uunet!cme-durer!warsaw        Rm. B-124, Bldg. 220
;; ARPA: warsaw@cme.nist.gov                 Gaithersburg, MD 20899

;; Want to be on the Supercite mailing list?
;;
;; Send articles to supercite@cme.nist.gov or uunet!cme-durer!supercite
;; Send administrative queries/requests to supercite-request@cme.nist.gov
;;	or uunet!cme-durer!supercite-request

;; ======================================================================
;; Credits:
;;
;; This package was derived from the SUPERYANK 1.11 package as posted
;; to the net.  SUPERYANK 1.11 was inspired by code and ideas from
;; Martin Neitzel and Ashwin Ram.  It has been migrated to SUPERCITE
;; 2.0 through the comments and suggestions of the superyank (now the
;; supercite) mailing list which consists of many authors and users of
;; the various mail and news reading packages.

;; ======================================================================
;; Thanks:
;;
;; Many folks on the supercite mailing list have contributed their help in
;; debugging, making suggestions and supplying support code or bug fixes
;; for the pre-release versions of supercite 2.0.  I want to thank every-
;; one who helped, especially:
;;
;; Mark Baushke, Khalid Sattar, David Lawrence, Chris Davis, Kyle
;; Jones and Kayvan Sylvan
;;
;; I don't mean to leave anyone out. All who have help have been
;; appreciated.

;; ======================================================================
;; Wish list for version 2.1:
;;
;; 1) make assoc lists and other global internal variables buffer-local?
;;
;; 2) handle multiple yanking into a single reply buffer. perhaps have a
;;    stack for sc-gal-information?
;;
;; 3) write my own mail-yank-clear-headers which will allow selective
;;    clearing or leaving of mail headers. Also allow me to not
;;    (require 'sendmail).
;;
;; 4) add a selectable indentation *before* the attribution string.

;; ======================================================================
;; require and provide features and autoload
;;
(require 'sendmail)
(require 'mail-utils)
(require 'baw-alist)
(provide 'supercite)


;; **********************************************************************
;;  
;; start of user defined variables
;; 
;; **********************************************************************


;; ----------------------------------------------------------------------
;; this variable holds the default author's name for citations
;;
(defvar sc-default-author-name "Anonymous"
  "*String used when an author's name cannot be found.")


;; ----------------------------------------------------------------------
;; this variable holds the default attribution string for citations
;;
(defvar sc-default-attribution "Anon"
  "*String that is used when the author's real attribution string
cannot be found.  This string should not contain either of the strings
`sc-citation-delimiter' or `sc-citation-separator'.")


;; ----------------------------------------------------------------------
;; string used as an end delimiter for both nested and non-nested citations
;;
(defvar sc-citation-delimiter ">"
  "*String to use as an end-delimiter for citations.  This string is
used in both nested and non-nested citations.  For best results, use a
single character with no trailing space.  Most commonly used string
is: \">\.")


;; ----------------------------------------------------------------------
;; separator string between the sc-citation-delimiter and the text
;;
(defvar sc-citation-separator " "
  "*String to use as a separator between the `sc-citation-delimiter' and
the original text.  Normally this character is a single space
character though often a single tab character is used.")


;; ----------------------------------------------------------------------
;; regular expression that matches existing citations
;;
(defvar sc-cite-regexp "\\s *[a-zA-Z0-9]*\\s *>+"
  "*Regular expression that describes how an already cited line in an
article begins.  The regexp is only used at the beginning of a line,
so it doesn't need to begin with a '^'.")


;; ----------------------------------------------------------------------
;; regular expression that delimits names from titles
;;
(defvar sc-titlecue-regexp "\\s +-+\\s +"
  "*Regular expression that delineates names from titles in the
author's name field.  Often, people will set up their name field to
look like this:

     (John Xavier Doe -- Computer Hacker Extraordinaire)

Set to nil to treat entire field as a name.")


;; ----------------------------------------------------------------------
;; variable that holds preferred attribution
;;
(defvar sc-preferred-attribution 'firstname

  "*Symbol that specified which portion of the author's name should be
used as the attribution string. The value of this variable should be a
quoted symbol, based on the following key:

     emailname   -- email address name
     initials    -- initials of author
     firstname   -- first name of author
     lastname    -- last name of author
     middlename1 -- first middle name of author
     middlename2 -- second middle name of author
     ...

Middlename indexes can be any positive integer greater than 0, though
it is unlikely that many authors will supply more than one middle
name, if that many.

Type \\<sc-leached-keymap>\\[sc-describe] for information.")


;; ----------------------------------------------------------------------
;; mail fields list
;;
(defvar sc-mail-fields-list
  '("date" "message-id"  "subject" "newsgroup" "references"
    "from" "return-path" "path"    "reply"     "organization")

  "*List of mail fields that you may want to use to build your
reference header. Each field should be a string which will be passed
to `mail-fetch-field'.  

See `sc-field' for information on how to use these fields in your
reference header, or type \\<sc-leached-keymap>\\[sc-describe].")


;; ----------------------------------------------------------------------
;; mumble string
;;
(defvar sc-mumble-string "mumble"
  "*Value returned by `sc-field' if chosen field cannot be found. You
may want to set this to nil.")


;; ----------------------------------------------------------------------
;; reference header tag string
;;
(defvar sc-reference-tag-string ">>>>> "
  "*String that delineates reference header lines from the rest of the
cited text. This is annoying to some people. If you're one of them,
set this variable to the empty string, or whatever you choose.")


;; ----------------------------------------------------------------------
;; preferred header
;;
(defvar sc-preferred-header-style 1
  "*Integer specifying which header rewrite function the user prefers
to use in citation references.  This variable is used as an index into
`sc-rewrite-header-list' when writing a reference.  The first function
in this list is zero indexed.")


;; ----------------------------------------------------------------------
;; header list
;;
(defvar sc-rewrite-header-list
  '((sc-no-header)
    (sc-header-on-said)
    (sc-header-inarticle-writes)
    (sc-header-regarding-writes)
    (sc-header-verbose)
    (sc-header-attributed-writes)
    )
  "*Contains a list of functions.  Each function in this list is a
reference header rewrite function that is used when inserting a
citation reference header at the top of a cited body of text.  The
variable `sc-preferred-header-style' controls which function is chosen
for automatic reference inserting.  Electric reference mode will cycle
through this list rewrite functions.  Here's a list of predefined
header styles which you can use as a model for writing your own:

     sc-no-header:
          {nothing is written}

     sc-header-on-said [default]:
          On <date>
          <from> said:

     sc-header-inarticle-writes:
          In article <message-id> <from> writes:

     sc-header-regarding-writes:
          Regarding <subject>; <from> adds:

     sc-header-verbose:
          On <date>, <from>
          from the organization <organization>
          has this to say about article <message-id>
          in newsgroups <newsgroups>
          concerning <subject>
          referring to previous articles <reference>
          whose comments are cited by \"<citation>\"

     sc-header-attributed-writes:
          \"<attribution>\" == <author> <from> writes:

Each line of the reference will be prefixed by the
`sc-reference-tag-string'. If you write your own reference header
functions, be sure to use this variable.

The variables that you can use to create your headers are those that
are included in `sc-mail-fields-list'.  Some additional fields are
always made available by SUPERCITE.  These are:

     \"sc-author\"       - author of the original article
     \"sc-firstname\"    - author's firstname
     \"sc-lastname\"     - author's lastname
     \"sc-middlename-N\" - author's Nth middlename
     \"sc-attribution\"  - user chosen attribution string
     \"sc-citation\"     - user chosen citation string
     \"from\"            - the value of the \"From:\" field

Any of these field names can be used (as strings) as arguments to
`sc-field'.")


;; ----------------------------------------------------------------------
;; variable controlling citation type, nested or non-nested
;;
(defvar sc-nested-citation-p nil
  "*Non-nil uses nested citations, nil uses non-nested citations.
Nested citations are of the style:

     I wrote this
     > He wrote this
     >> She replied to something he wrote

Non-nested citations are of the style:

     I wrote this
     John> He wrote this
     Jane> She originally wrote this")


;; ----------------------------------------------------------------------
;; confirmation control flag
;;
(defvar sc-confirm-always-p t
  "*If t, always confirm attribution string before citing text body.")


;; ----------------------------------------------------------------------
;; non-nil means downcase the author's name string
;;
(defvar sc-downcase-p nil
  "*Non-nil means downcase the attribution and citation strings.")


;; ----------------------------------------------------------------------
;; controls removal of leading white spaces
;;
(defvar sc-left-justify-p nil
  "*If non-nil, delete all leading white space before citing.")


;; ----------------------------------------------------------------------
;; controls auto filling of region
;;
(defvar sc-auto-fill-region-p nil
  "*If non-nil, automatically fill each paragraph after it has been cited.")


;; ----------------------------------------------------------------------
;; use electric references
;;
(defvar sc-electric-references-p t
  "*Use electric references if non-nil.")


;; ----------------------------------------------------------------------
;; use only the preferred attribution?
;;
(defvar sc-use-only-preference-p nil
  "*This variable controls what happens when the preferred attribution
string cannot be found.  If non-nil, then `sc-default-attribution'
will be used. If nil, then some secondary scheme will be employed to
find a suitable attribution string.")


;; ----------------------------------------------------------------------
;; user customization load hook
;;
(defvar sc-load-hook nil
  "*User definable hook which runs after supercite is loaded.")


;; ----------------------------------------------------------------------
;; run hook
;;
(defvar sc-run-hook nil
  "*User definable hook which runs after `sc-cite-original' is
executed.")


;; ----------------------------------------------------------------------
;; fill function hook
;;
(defvar sc-fill-paragraph-hook 'sc-fill-paragraph
  "*Hook for filling a paragraph.  This hook gets executed when you
fill a paragraph either manually or automagically. It expects point to
be within the extent of the paragraph that is going to be filled.")


;; ----------------------------------------------------------------------
;; default keymap
;;
(defvar sc-default-keymap
  '(lambda ()
     (local-set-key "\C-c\C-r" 'sc-insert-reference)
     (local-set-key "\C-c\C-t" 'sc-cite)
     (local-set-key "\C-c\C-a" 'sc-recite)
     (local-set-key "\C-c\C-u" 'sc-uncite)
     (local-set-key "\C-c\C-i" 'sc-insert-citation)
     (local-set-key "\C-c\C-o" 'sc-open-line)
     (local-set-key "\C-c\C-q" 'sc-fill-paragraph-manually)
     (local-set-key "\C-c\q"   'sc-fill-paragraph-manually)
     (local-set-key "\C-c\C-m" 'sc-modify-information)
     (local-set-key "\C-c?"    'sc-describe)
     )
  "*Default keymap if major-mode can't be found in `sc-local-keymaps'.")


;; ----------------------------------------------------------------------
;; keymap per-interface list
;;
(defvar sc-local-keymaps
  '((mail-mode
     (lambda ()
       (local-set-key "\C-c\C-r" 'sc-insert-reference)
       (local-set-key "\C-c\C-t" 'sc-cite)
       (local-set-key "\C-c\C-a" 'sc-recite)
       (local-set-key "\C-c\C-u" 'sc-uncite)
       (local-set-key "\C-c\C-i" 'sc-insert-citation)
       (local-set-key "\C-c\C-o" 'sc-open-line)
       (local-set-key "\C-c\C-q" 'sc-fill-paragraph-manually)
       (local-set-key "\C-c\q"   'sc-fill-paragraph-manually)
       (local-set-key "\C-c\C-m" 'sc-modify-information)
       (local-set-key "\C-c?"    'sc-describe)
       ))
    (mh-letter-mode
     (lambda ()
       (local-set-key "\C-c\C-r" 'sc-insert-reference)
       (local-set-key "\C-c\C-t" 'sc-cite)
       (local-set-key "\C-c\C-a" 'sc-recite)
       (local-set-key "\C-c\C-u" 'sc-uncite)
       (local-set-key "\C-ci"    'sc-insert-citation)
       (local-set-key "\C-c\C-o" 'sc-open-line)
       (local-set-key "\C-c\q"   'sc-fill-paragraph-manually)
       (local-set-key "\C-c\C-m" 'sc-modify-information)
       (local-set-key "\C-c?"    'sc-describe)
       ))
    (news-reply-mode mail-mode)
    (vm-mail-mode mail-mode)
    (e-reply-mode mail-mode)
    (n-reply-mode mail-mode)
    )
  "*List of keymaps to use with the associated major-mode.")


;; **********************************************************************
;; 
;; end of user defined variables
;; 
;; **********************************************************************


;; ======================================================================
;; global variables, not user accessable
;;

;; ----------------------------------------------------------------------
;; the new citation style means we can clean out other headers in
;; addition to those previously cleaned out.  anyway, we create our
;; own headers.  cleans out mail, gnus, vm and other headers. add to
;; this for other mail or news readers you may be using.
;;
(setq mail-yank-ignored-headers
      (concat
       "^via:\\|^origin:\\|^status:\\|^received:\\|^remailed\\|"
       "^[a-z-]*message-id:\\|^\\(summary-\\)?line[s]?:\\|^cc:\\|"
       "^subject:\\|^\\(\\(in-\\)?reply-\\)?to:\\|^sender:\\|^replied:\\|"
       "^\\(\\(return\\|reply\\)-\\)?path:\\|^\\(posted-\\)?date:\\|"
       "^\\(mail-\\)?from:\\|^newsgroup[s]?:\\|^organization:\\|^keywords:\\|"
       "^distribution:\\|^xref:\\|^references:\\|^x-mailer:\\|"
       "^\\(x-\\)?followup-to:\\|^x-vm-attributes:\\|^expires:\\|"
       "^approved:\\|^apparently-to:\\|^summary:\\|"
       "^x-vm-attributes:\\|^x-vm-v[0-9]+-data:"))


;; ----------------------------------------------------------------------
;; global alists
;;
(setq sc-gal-attributions nil)
(setq sc-gal-information  nil)


;; ----------------------------------------------------------------------
;; misc variables
;;
(setq sc-force-confirmation-p nil)
(setq sc-fill-arg nil)
(setq sc-electric-bufname "*Supercite Electric Reference*")
(setq sc-leached-keymap (current-local-map))


;; ======================================================================
;; update global alists

;; ----------------------------------------------------------------------
;; update the global var alist
;;
(defun sc-update-gal (attribution)
  "Update the `sc-gal-information' alist to include the ATTRIBUTION
and the nested and non-nested citations derived from ATTRIBUTION.
Associate with keys \"sc-attribution\", \"sc-nested-citation\", and
\"sc-citation\" respectively."
  (let ((attrib (if sc-downcase-p (downcase attribution) attribution)))
    (aput 'sc-gal-information "sc-attribution" attrib)
    (aput 'sc-gal-information "sc-nested-citation"
	  (concat attrib sc-citation-delimiter))
    (aput 'sc-gal-information "sc-citation"
	  (concat attrib sc-citation-delimiter sc-citation-separator))))


;; ======================================================================
;; this section snarfs mail fields and places them in the info alist

;; ----------------------------------------------------------------------
;; fetch all the fields
;;
(defun sc-fetch-fields (start end)
  "Fetch the mail fields in the region from START to END, and add them
to the global alist, `sc-gal-information'.  These fields can be
accessed in reference headers with `sc-field'."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (mapcar
       (function
	(lambda (field)
	  (let ((value (mail-fetch-field field)))
	    (and value
		 (aput 'sc-gal-information field value)))))
       sc-mail-fields-list)
      (aput 'sc-gal-information "from" (mail-fetch-field "from")))))


;; ----------------------------------------------------------------------
;; return the field's value
;;
(defun sc-field (field)
  "Return the value associated with the FIELD.  If FIELD was not
found, return `sc-mumble-string'."
  (or (aget sc-gal-information field)
      sc-mumble-string))


;; ======================================================================
;; contains supplied header reference rewrite functions


;; ----------------------------------------------------------------------
;; no header is written
;;
(defun sc-no-header ()
  "Does nothing. Use this instead of nil to get a blank header."
  ())


;; ----------------------------------------------------------------------
;; On <date>, <so-and-so> said:
;;
(defun sc-header-on-said ()
  "\"On <date>, <from> said:\""
  (insert-string sc-reference-tag-string
		 "On " (sc-field "date") ", " (sc-field "from") " said:\n"))


;; ----------------------------------------------------------------------
;; In article <blah>, <so-and-so> writes:
;;
(defun sc-header-inarticle-writes ()
  "\"In article <message-id>, <from> writes:\""
  (insert-string sc-reference-tag-string
		 "In article " (sc-field "message-id")
		 ", " (sc-field "from") " writes:\n"))


;; ----------------------------------------------------------------------
;; Regarding <subject>, <so-and-so> adds:
;;
(defun sc-header-regarding-writes ()
  "\"Regarding <subject>; <from> adds:\""
  (insert-string sc-reference-tag-string
		 "Regarding " (sc-field "subject")
		 "; " (sc-field "from") " adds:\n"))


;; ----------------------------------------------------------------------
;; too verbose to comment
;;
(defun sc-header-verbose ()
  "Very verbose."
  (let ((cr (concat "\n" sc-reference-tag-string)))
    (insert-string sc-reference-tag-string
		   "On " (sc-field "date") "," (sc-field "from")
		   cr "from the organization " (sc-field "organization")
		   cr "had this to say about article " (sc-field "message-id")
		   cr "in newsgroups " (sc-field "newsgroups")
		   cr "concerning " (sc-field "subject")
		   cr "referring to previous articles " (sc-field "references")
		   cr "whose comments are cited by \""
		   (sc-field "sc-citation")
		   " \".\n" )))


;; ----------------------------------------------------------------------
;; yet another style
;;
(defun sc-header-attributed-writes ()
  "\"<sc-attribution>\" == <sc-author> <sc-from> writes:"
  (insert-string sc-reference-tag-string
		 "\"" (sc-field "sc-attribution")
		 "\" == " (sc-field "sc-author") " "
		 (sc-field "sc-from") "writes:\n"))


;; ======================================================================
;; this section queries the user for necessary information
;;

;; ----------------------------------------------------------------------
;; query the user for the attribution string
;;
(defun sc-query (default)
  "Query for an attribution string with the DEFAULT choice.  Returns
the string entered by the user, if non-empty and non-nil, or DEFAULT
otherwise."
  (let* ((prompt (concat "Enter attribution string: (default "
			 default
			 ") "))
	 (query (read-input prompt)))
    (if (or (null query)
	    (string= query ""))
	default
      query)))


;; ----------------------------------------------------------------------
;; confirm the attribution string from the user
;;
(defun sc-confirm ()
  "Confirm the preferred attribution with the user. When this function
is called, `sc-gal-attributions' is set and the `aheadsym' of this
alist is used as the default selection.  Also, C-g [quit] selects the
default."
  (if (or sc-confirm-always-p
	  sc-force-confirmation-p)
      (aput 'sc-gal-attributions
	     (let* ((default (aheadsym sc-gal-attributions))
		    chosen
		    (prompt (concat "Complete attribution string: (default "
				    default
				    ") "))
		    (minibuffer-local-completion-map
		     (copy-keymap minibuffer-local-completion-map)))
	       (define-key minibuffer-local-completion-map "\C-g"
		 '(lambda () (interactive) (beep) (exit-minibuffer)))
	       (setq chosen (completing-read prompt sc-gal-attributions))
	       (if (or (not chosen)
		       (string= chosen ""))
		   default
		 chosen))))
  (setq sc-force-confirmation-p nil))


;; ======================================================================
;; this section contains primitive functions used in the email address
;; parsing schemes.  they extract name fields from various parts of
;; the "from:" field.


;; ----------------------------------------------------------------------
;; address form: name%[stuff]
;;
(defun sc-%-style-address (from-string)
  "Try to extract \"name\" from an email address string FROM-STRING
of the style \"name%machine@machine.\""
  (and (string-match "[a-zA-Z0-9]+%" from-string 0)
       (substring from-string (match-beginning 0) (1- (match-end 0)))))


;; ----------------------------------------------------------------------
;; address form: [stuff]name@[stuff]
;;
(defun sc-@-style-address (from-string)
  "Try to extract \"name\" from an email address string FROM-STRING
of the style \"name@machine.domain\""
  (and (string-match "[a-zA-Z0-9]+@" from-string 0)
       (substring from-string (match-beginning 0) (1- (match-end 0)))))


;; ----------------------------------------------------------------------
;; address form: [stuff]![stuff]...!name[stuff]
;;
(defun sc-!-style-address (from-string)
  "Try to extract \"name\" from an email address string FROM-STRING
of the style \"machine!machine!...!name\""
  (let ((eostring (string-match "$" from-string 0))
	(mstart (string-match "![a-zA-Z0-9]+\\([^!a-zA-Z0-9]\\|$\\)"
			      from-string 0))
	(mend (match-end 0)))
    (if mstart
	(substring from-string (1+ mstart) (if (= mend eostring)
					       mend
					     (1- mend)))
      nil)))


;; ----------------------------------------------------------------------
;; no style addresses, like local names: "name"
;;
(defun sc-no-style-address (from-string)
  "Try to extract \"name\" from an email address string FROM-STRING
of the style \"name\""
  (and (string-match "[a-zA-Z0-9]+" from-string 0)
       (substring from-string (match-beginning 0) (match-end 0))))


;; ----------------------------------------------------------------------
;; get one of the email style names
;;
(defun sc-get-emailname (from-string)
  "Using various email address string parsing schemes, try each one
until you get a non nil, non-empty string. FROM-STRING is the mail
address string to parse."
  (cond
   ((sc-%-style-address from-string))
   ((sc-@-style-address from-string))
   ((sc-!-style-address from-string))
   ((sc-no-style-address from-string))
   (t (substring from-string 0 10))))


;; ======================================================================
;; this section contains functions that will extract a list of names
;; from the name field string.


;; ----------------------------------------------------------------------
;; returns the "car" of the namestring, really the first namefield
;;
(defun sc-string-car (namestring)
  "Return the string-equivalent \"car\" of NAMESTRING, really the
first name field.

     example: (sc-string-car \"John Xavier Doe\")
              => \"John\""
  (substring namestring
	     (progn (string-match "\\s *" namestring) (match-end 0))
	     (progn (string-match "\\s *\\S +" namestring) (match-end 0))))


;; ----------------------------------------------------------------------
;; returns the "cdr" of the namestring, really the whole string from
;; after the first name field to the end of the string.
;;
(defun sc-string-cdr (namestring)
  "Return the string-equivalent \"cdr\" of NAMESTRING, really the name
string from after the first name field to the end of the string.

     example: (sc-string-cdr \"John Xavier Doe\")
              => \"Xavier Doe\""
  (substring namestring
	     (progn (string-match "\\s *\\S +\\s *" namestring) (match-end 0))))


;; ----------------------------------------------------------------------
;; extract the namestring from the "from:" string
;;
(defun sc-extract-namestring (from-string)
  "Parse FROM-STRING which contains the string on the \"From: \" line,
from just after the \"From: \" string to the end of the line.  It
returns the string which should be the full name of the user, minus
the title."
  (let ((pstart (string-match "(.*)" from-string 0))
	(pend (match-end 0))
	(qstart (string-match "\".*\"" from-string 0))
	(qend (match-end 0))
	(bstart (string-match "\\([.a-zA-Z0-9---]+\\s *\\)+" from-string 0))
	(bend (match-end 0)))
    (cond
     (pstart (substring from-string
			(1+ pstart)
			(or (string-match sc-titlecue-regexp
					  from-string
					  (1+ pstart))
			    (1- pend))))
     (qstart (substring from-string
			(1+ qstart)
			(or (string-match sc-titlecue-regexp
					  from-string
					  (1+ qstart))
			    (1- qend))))
     (bstart (substring from-string
			bstart
			(or (string-match sc-titlecue-regexp
					  from-string
					  bstart)
			    bend))))))


;; ----------------------------------------------------------------------
;; convert a namestring to a list of namefields
;;
(defun sc-namestring-to-list (namestring)
  "Convert NAMESTRING to a list of names.

Example: (sc-namestring-to-list \"John Xavier Doe\")
=> (\"John\" \"Xavier\" \"Doe\")"
  (if (not (string-match namestring ""))
      (append (list (sc-string-car namestring))
	      (sc-namestring-to-list (sc-string-cdr namestring)))))


;; ----------------------------------------------------------------------
;; given the from-string, return the name list
;;
(defun sc-get-namelist (from-string)
  "Parse the FROM-STRING and return a list of author names in the
order in which they appear in the name field."
  (sc-namestring-to-list (sc-extract-namestring from-string)))


;; ----------------------------------------------------------------------
;; strip the initials from each item in the list and return a string
;; that is the concatenation of the initials
;;
(defun sc-strip-initials (namelist)
  "Snag the first character from each name in the list NAMELIST and
concat them into initials."
  (if (not namelist)
      nil
    (concat (substring (car namelist) 0 1)
	    (sc-strip-initials (cdr namelist)))))


;; ======================================================================
;; this section handles selection of the attribution and citation strings
;;

;; ----------------------------------------------------------------------
;; populate alists
;;
(defun sc-populate-alists (from-string)
  "Using the FROM-STRING, populate the association lists
`sc-gal-attributions' and `sc-gal-information' with important and
useful information. Return the list of name symbols."
  (let* ((namelist (sc-get-namelist from-string))
	 (revnames (reverse (cdr namelist)))
	 (midnames (reverse (cdr revnames)))
	 (firstname (car namelist))
	 (midnames (reverse (cdr revnames)))
	 (lastname (car revnames))
	 (initials (sc-strip-initials namelist))
	 (emailname (sc-get-emailname from-string))
	 (n 1)
	 (symlist '(emailname initials firstname lastname)))

    ;; put basic information
    (aput 'sc-gal-attributions 'firstname firstname)
    (aput 'sc-gal-attributions 'lastname lastname)
    (aput 'sc-gal-attributions 'emailname emailname)
    (aput 'sc-gal-attributions 'initials initials)

    (aput 'sc-gal-information "sc-firstname" firstname)
    (aput 'sc-gal-information "sc-lastname" lastname)
    (aput 'sc-gal-information "sc-emailname" emailname)
    (aput 'sc-gal-information "sc-initials" initials)

    ;; put middle names
    (mapcar
     (function
      (lambda (name)
	(let ((symbol (intern (format "middlename%d" n)))
	      (string (format "sc-middlename-%d" n)))
	  (aput 'sc-gal-attributions symbol name)
	  (aput 'sc-gal-information string name)
	  (setq n (1+ n))
	  (nconc symlist (list symbol)))))
     midnames)

    ;; build the sc-author entry
    (aput 'sc-gal-information "sc-author"
	  (concat firstname " " (mapconcat
				 (function
				  (lambda (name) name))
				 midnames " ")
		  (if midnames " ") lastname))
    symlist))


;; ----------------------------------------------------------------------
;; sort the attribution alist so that preference is at head
;;
(defun sc-sort-attribution-alist ()
  "Sort the `sc-gal-attributions' alist so that the preferred
attribution is at the head of the list. Use secondary methods if
necessary."
  (asort 'sc-gal-attributions sc-preferred-attribution)

  ;; use backup scheme if preference is not legal
  (if (or (null sc-preferred-attribution)
	  (anot-head-p sc-gal-attributions sc-preferred-attribution)
	  (let ((prefval (aget sc-gal-attributions
			       sc-preferred-attribution)))
	    (or (null prefval)
		(string= prefval ""))))
      ;; no legal attribution
      (if sc-use-only-preference-p
	  (aput 'sc-gal-attributions 'sc-user-query
		(sc-query sc-default-attribution))
	;; else use secondary scheme
	(asort 'sc-gal-attributions 'firstname))))


;; ----------------------------------------------------------------------
;; build the attribution alist for the first time
;;
(defun sc-build-attribution-alist (from-string)
  "Using the FROM-STRING, set up `sc-gal-attributions' to be the list
of possible attributions, with preference applied."
  (let ((symlist (sc-populate-alists from-string))
	(headval (progn (sc-sort-attribution-alist)
			(aget sc-gal-attributions
			      (aheadsym sc-gal-attributions)))))

    ;; foreach element in the symlist, remove the corresponding key-value
    ;; pair in the alist, then insert just the value.
    (mapcar
     (function
      (lambda (symbol)
	(let ((value (aget sc-gal-attributions symbol)))
	  (if (not (or (null value)
		       (string= value "")))
	      (aput 'sc-gal-attributions value))
	  (adelete 'sc-gal-attributions symbol))))
     symlist)

    ;; now reinsert the head (preferred) attribution, this effectively
    ;; just moves the head value to the front of the list.
    (aput 'sc-gal-attributions headval)

    ;; check to be sure alist is not nil
    (if (null sc-gal-attributions)
	(aput 'sc-gal-attributions sc-default-attribution))))


;; ----------------------------------------------------------------------
;; using the global attribution alist, confirm that that's the one wanted
;; by the user.
;;
(defun sc-select ()
  "Select an attribution and create a citation from that, using the
global alist `sc-gal-attributions'. If a previous global attribution
alist has been built, do not rebuild, but confirm again if
confirmation is desired."
  (cond
   (sc-nested-citation-p
    (sc-update-gal ""))
   ((null (aget sc-gal-information "from" t))
    (aput 'sc-gal-information "sc-author" sc-default-author-name)
    (sc-update-gal (sc-query sc-default-attribution)))
   ((null sc-gal-attributions)
    (sc-build-attribution-alist (aget sc-gal-information "from" t))
    (sc-confirm)
    (sc-update-gal (aheadsym sc-gal-attributions)))
   (t
    (sc-confirm)
    (sc-update-gal (aheadsym sc-gal-attributions)))))


;; ======================================================================
;; region citing and unciting

;; ----------------------------------------------------------------------
;; cite a region
;;
(defun sc-cite-region (start end)
  "Cite a region delineated by START and END, by inserting the
citation string at the beginning of every non-blank line in the
region, if nested citations are used. Insert the citation string at
the beginning of every non-blank, not-already-cited line if non-nested
citations are used."
  (save-excursion
    (set-mark end)
    (goto-char start)
    (beginning-of-line)
    (let ((fstart (point))
	  (fend   (point)))
      
      (while (< (point) (mark))
	
	;; remove leading whitespace if desired 
	(and sc-left-justify-p
	     (fixup-whitespace))
	
	;; if end of line then perhaps autofill
	(cond ((eolp)
	       (or (= fstart fend)
		   (not sc-auto-fill-region-p)
		   (save-excursion (set-mark fend)
				   (goto-char (/ (+ fstart fend 1) 2))
				   (run-hooks 'sc-fill-paragraph-hook)))
	       (setq fstart (point)
		     fend (point)))
	      
	      ;; not end of line so perhap cite it
	      ((not (looking-at sc-cite-regexp))
	       (insert (aget sc-gal-information "sc-citation")))
	      (sc-nested-citation-p
	       (insert (aget sc-gal-information "sc-nested-citation"))))
	
	(setq fend (point))
	(forward-line)))))


;; ----------------------------------------------------------------------
;; uncite a region
;;
(defun sc-uncite-region (start end cite-regexp)
  "Remove citations from region delineated by START and END by
removing CITE-REGEXP at the beginning of each line that starts with
CITE-REGEXP. Unciting also auto-fills if flag is set."
  (save-excursion
    (set-mark end)
    (goto-char start)
    (beginning-of-line)
    (let ((fstart (point))
	  (fend (point)))

      (while (< (point) (mark))

	;; if end of line, then perhaps autofill
	(cond ((eolp)
	       (or (= fstart fend)
		   (not sc-auto-fill-region-p)
		   (save-excursion (set-mark fend)
				   (goto-char (/ (+ fstart fend 1) 2))
				   (run-hooks 'sc-fill-paragraph-hook)))
	       (setq fstart (point)
		     fend (point)))

	      ;; not end of line so perhaps uncite it
	      ((looking-at cite-regexp)
	       (save-excursion
		 (save-restriction
		   (narrow-to-region (progn (beginning-of-line)
					    (point))
				     (progn (end-of-line)
					    (point)))
		   (beginning-of-line)
		   (delete-region (point-min)
				  (progn (re-search-forward cite-regexp
							    (point-max)
							    t)
					 (match-end 0)))))))
	(setq fend (point))
	(forward-line)))))


;; ======================================================================
;; this section contains paragraph filling support

;; ----------------------------------------------------------------------
;; try to figure out the fill-prefix on the current line
;;
(defun sc-guess-fill-prefix ()

  "Examine the current line, and try to figure out the prefix being
used on that line, using a couple of heuristics.  Search begins on
first non-blank line in the region of interest.

     1) If `fill-prefix' is already bound to the empty string, return
        nil.

     2) If `fill-prefix' is already bound, but not to the empty
        string, return the value of `fill-prefix'.

     3) If the current line starts with the last chosen citation
        string, then that string is returned.

     4) If the current line starts with a string matching the regular
        expression, `sc-cite-regexp', then that string is returned.

     5) Nil is returned."
  (save-excursion

    ;; scan for first non-blank line in the region
    (beginning-of-line)
    (while (and (< (point) (mark))
		(eolp))
      (forward-line))

    (let ((citation (aget sc-gal-information "sc-citation")))
      (cond
       ((string= fill-prefix "") nil)
       (fill-prefix)
       ((looking-at (regexp-quote citation)) citation)
       ((looking-at sc-cite-regexp)
	(buffer-substring
	 (point) (progn
		   (re-search-forward (concat sc-cite-regexp "\\s *")
				      (point-max) nil)
		   (point))))
       ((looking-at (concat ".*" sc-citation-delimiter "\\s *"))
	(buffer-substring
	 (point) (progn (re-search-forward (concat ".*" sc-citation-delimiter
						   "\\s *"))
			(point))))
       (t nil)))))


;; ----------------------------------------------------------------------
;; check all lines in real paragraph for beginning with fill-prefix
;;
(defun sc-consistant-cite-p (prefix)
  "Check current *real* paragraph (i.e. paragraph delineated by
`(forward|backward)-paragraph') to see if all lines start with prefix.
Returns t if the entire paragraph is consistantly cited, nil
otherwise."
  (save-excursion
    (let ((end   (progn (forward-paragraph)
			(beginning-of-line)
			(or (not (eolp))
			    (forward-char -1))
			(point)))
	  (start (progn (backward-paragraph)
			(beginning-of-line)
			(or (not (eolp))
			    (forward-char 1))
			(point)))
	  (badline t))
      
      (goto-char start)
      (beginning-of-line)
      (while (and (< (point) end)
		  badline)
	(setq badline (looking-at prefix))
	(forward-line))
      badline)))


;; ----------------------------------------------------------------------
;; look for the beginning of consistant citation
;;
(defun sc-fill-start (fill-prefix)
  "Return the position of the beginning of the region around point that
starts with the FILL-PREFIX. Restrict scan to current paragraph."
  (save-excursion
    (let ((badline nil)
	  (top (save-excursion
		 (backward-paragraph)
		 (beginning-of-line)
		 (or (not (eolp))
		     (forward-char 1))
		 (point))))
      (while (and (not badline)
		  (> (point) top))
	(forward-line -1)
	(setq badline (not (looking-at fill-prefix)))))
    (forward-line)
    (point)))


;; ----------------------------------------------------------------------
;; look for the end of consistant citation
;;
(defun sc-fill-end (fill-prefix)
  "Return the position of the end of the region around point that
starts with the FILL-PREFIX. Restrict scan to current paragraph."
  (save-excursion
    (let ((badline nil)
	  (bot (save-excursion
		 (forward-paragraph)
		 (beginning-of-line)
		 (or (not (eolp))
		     (forward-char -1))
		 (point))))
      (while (and (not badline)
		  (< (point) bot))
	(beginning-of-line)
	(setq badline (not (looking-at fill-prefix)))
	(forward-line 1)))
    (forward-line -1)
    (point)))


;; ----------------------------------------------------------------------
;; fill paragraph automagically
;;
(defun sc-fill-paragraph ()
  "Fill the paragraph containing or following point, automagically
finding the paragraph's fill-prefix using heuristics in
`sc-guess-fill-prefix'.

If every line in the entire paragraph that encompasses point starts
with the same fill-prefix, then the entire paragraph is automagically
filled. However, if there is inconsistant citing among the lines, then
the user will be queried to restrict the fill to only those lines
around point that begin with the fill-prefix.

The variable `sc-fill-arg' is passed to `fill-paragraph' and
`fill-region-as-paragraph' to justify both sides of the paragraph if
an argument was given to `sc-fill-paragraph-manually'."
  (save-excursion
    (let ((pnt (point))
	  (fill-prefix (sc-guess-fill-prefix)))
      (cond
       ((not fill-prefix)
	(fill-paragraph sc-fill-arg))
       ((sc-consistant-cite-p fill-prefix)
	(fill-paragraph sc-fill-arg))
       ((y-or-n-p "Inconsistent citation found. Restrict? ")
	(fill-region-as-paragraph (progn (goto-char pnt)
					 (sc-fill-start fill-prefix))
				  (progn (goto-char pnt)
					 (sc-fill-end fill-prefix))
				  sc-fill-arg))
       (t
	(progn
	  (setq fill-prefix (aget sc-gal-information "sc-citation"))
	  (fill-paragraph sc-fill-arg)))))))


;; ======================================================================
;; electric insert reference

;; ----------------------------------------------------------------------
;; next reference
;;
(defun sc-eref-next ()
  "Display next reference in minibuffer."
  (interactive)
  (sc-other-reference 1))


;; ----------------------------------------------------------------------
;; prev reference
;;
(defun sc-eref-prev ()
  "Display previous reference in minibuffer."
  (interactive)
  (sc-other-reference -1))


;; ----------------------------------------------------------------------
;; update show buffer
;;
(defun sc-eref-show ()
  "Show the reference buffer."
  (interactive)
  (if (get-buffer-window sc-electric-bufname)
      (delete-windows-on (get-buffer sc-electric-bufname))
    (let ((curbuffer (buffer-name)))
      (set-buffer sc-electric-bufname)
      (setq buffer-read-only nil)
      (erase-buffer)
      (goto-char (point-min))
      (insert-buffer curbuffer)
      (setq buffer-read-only t)
      (set-buffer curbuffer)
      (display-buffer sc-electric-bufname))))


;; ----------------------------------------------------------------------
;; set reference
;;
(defun sc-eref-setn ()
  "Set reference header selected as preferred."
  (interactive)
  (setq sc-preferred-header-style
	(aget sc-gal-information "sc-reference-number"))
  (message "Preferred reference style set."))


;; ----------------------------------------------------------------------
;; jump to preferred reference header
;;
(defun sc-eref-jump ()
  "Set reference header to preferred header."
  (interactive)
  (erase-buffer)
  (insert (sc-reference-into-buffer sc-preferred-header-style)))


;; ----------------------------------------------------------------------
;; exit from minibuffer
;;
(defun sc-eref-exit ()
  "Exit cleanly from electric reference insert minibuffer."
  (interactive)
  (erase-buffer)
  (beep)
  (exit-minibuffer))


;; ----------------------------------------------------------------------
;; return a reference and insert into buffer
;;
(defun sc-reference-into-buffer (refnum)
  "Make the electric buffer current and scribble the reference header
indexed by REFNUM into that buffer.  Then return the buffer contents
as a string."
  (let ((ref-func  (nth refnum sc-rewrite-header-list))
	(curbuffer (buffer-name))
	reference)
    (get-buffer-create sc-electric-bufname)
    (set-buffer sc-electric-bufname)
    (setq buffer-read-only nil)
    (erase-buffer)
    (eval ref-func)
    (setq reference (buffer-substring (point-min) (point-max)))
    (setq buffer-read-only t)
    (set-buffer curbuffer)
    reference))


;; ----------------------------------------------------------------------
;; move to next or previous reference
;;
(defun sc-other-reference (delta)
  "Scribble into the minibuffer the reference header indexed by
applying DELTA to the currently viewed reference. DELTA is normalized
to the range of possible values."
  (let* ((oldrefnum (aget sc-gal-information "sc-reference-number"))
	 (newrefnum (min (max 0 (+ oldrefnum delta))
			 (1- (length sc-rewrite-header-list)))))
    (if (= oldrefnum newrefnum)
	(error (if (= oldrefnum 0)
		   "No preceding reference headers in list."
		 "No following reference headers in list."))
      (erase-buffer)
      (aput 'sc-gal-information "sc-reference-number" newrefnum)
      (insert (sc-reference-into-buffer newrefnum))
      (goto-char (point-min)))))	       


;; ----------------------------------------------------------------------
;; enter electric insert reference
;;
(defun sc-insert-reference-electric ()
  "Put the current reference header into the minibuffer and allow
electric choosing of the inserted reference header.  While editing
the reference, the following commands are available:
\\{sc-electric-map}."
  (let* ((refnum (aget sc-gal-information "sc-reference-number"))
	 (prompt "Use: ")
	 (reference (sc-reference-into-buffer refnum))
	 chosen
	 (sc-electric-map (copy-keymap minibuffer-local-map)))
    ;; set up keymap
    (define-key sc-electric-map "\ep"  'sc-eref-prev)
    (define-key sc-electric-map "\en"  'sc-eref-next)
    (define-key sc-electric-map "\ev"  'sc-eref-show)
    (define-key sc-electric-map "\e."  'sc-eref-setn)
    (define-key sc-electric-map "\e,"  'sc-eref-jump)
    (define-key sc-electric-map "\C-g" 'sc-eref-exit)

    ;; read from the minibuffer
    (setq chosen (read-from-minibuffer prompt reference sc-electric-map))
    (kill-buffer sc-electric-bufname)
    chosen))


;; ======================================================================
;; the following functions are the top level, interactive functions that
;; can be bound to key strokes

;; ----------------------------------------------------------------------
;; rewrite the header to be more conversational
;;
(defun sc-insert-reference (arg)
  "Rewrite the reference header into a more conversational style and
insert it into the reply buffer.  Two different ways of chosing the
reference header style are available to the user.  In both cases, no
supplied numeric ARG means just insert the header indexed by
`sc-preferred-header-style'.  Numeric ARG supplied is treated
differently based on the electricity of reference insertion.

If `sc-electric-references-p' is non-nil, then use electric reference
selection.  In this case ARG indicates electric references should be
used for the current selection.  See `sc-insert-reference-electric'
for more information.

If `sc-electric-references-p' is nil, then treat ARG as an index into
the list of reference headers and just directly insert into the
message buffer."
  (interactive "P")
  (let (func)
    (cond
     ((or (not arg)
	  (not (setq func (nth (prefix-numeric-value arg)
			       sc-rewrite-header-list))))
      (eval (nth sc-preferred-header-style sc-rewrite-header-list)))
     (sc-electric-references-p
      (aput 'sc-gal-information "sc-reference-number" sc-preferred-header-style)
      (insert (sc-insert-reference-electric)))
     (t
      (eval func)))))


;; ----------------------------------------------------------------------
;; attribute and cite the entire region
;;
(defun sc-cite (arg)
  "This is the main function to cite and attribute a region of text in
the reply buffer created by some email reader package.  When this
function is called, it is assumed that point and mark are set to the
region which is going to be cited and attributed.  It is also assumed
that point points to the beginning of the yanked mail header lines,
which will first be parsed for useful information, then deleted from
the buffer.  Once this useful information has been glommed from the
headers, an attribution string is found (and confirmed if desired) and
the region of text body is cited.

Numeric ARG is passed to reference insertion function
`sc-insert-reference'."
  (interactive "P")

  (and (interactive-p)
       (setq sc-force-confirmation-p t))

  (sc-select)
  (undo-boundary)
  (sc-insert-reference (if (consp arg) arg nil))
  (let ((xchange (if (> (mark) (point)) nil
		   (exchange-point-and-mark)
		   t)))
    (sc-cite-region (point) (mark))

    ;; leave point on first cited line
    (while (and (< (point) (mark))
		(not (looking-at (aget sc-gal-information
				       (if sc-nested-citation-p
					   "sc-nested-citation"
					 "sc-citation")))))
      (forward-line))

    (and xchange
	 (exchange-point-and-mark))))


;; ----------------------------------------------------------------------
;; uncite the region
;;
(defun sc-uncite ()
  "Uncite the region."
  (interactive)
  (undo-boundary)
  (let ((xchange (if (> (mark) (point)) nil
		   (exchange-point-and-mark)
		   t))
	(fp (cond ((sc-guess-fill-prefix))
		  (t ""))))

    (sc-uncite-region (point) (mark) (regexp-quote fp))
    (and xchange
	 (exchange-point-and-mark))))


;; ----------------------------------------------------------------------
;; recite the region
;;
(defun sc-recite ()
  "Recite the region by first unciting then citing the text."
  (interactive)
  (setq sc-force-confirmation-p t)
  (sc-select)
  (undo-boundary)
  (let ((xchange (if (> (mark) (point)) nil
		   (exchange-point-and-mark)
		   t)))

    (sc-uncite-region (point) (mark) (regexp-quote (sc-guess-fill-prefix)))
    (sc-cite-region (point) (mark))
    (and xchange
	 (exchange-point-and-mark))))


;; ----------------------------------------------------------------------
;; insert the citation string at beginning of line
;;
(defun sc-insert-citation ()
  "Insert the citation string at the beginning of the line that point
is on."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (insert-string (aget sc-gal-information "sc-citation"))))


;; ----------------------------------------------------------------------
;; open a line putting the attribution at the beginning

(defun sc-open-line (arg)
  "Insert a newline and leave point before it.  Also inserts the
citation string at the beginning of the line.  With ARG, inserts that
many newlines."
  (interactive "p")
  (save-excursion
    (let ((start (point)))
      (open-line arg)
      (goto-char start)
      (forward-line)
      (while (< 0 arg)
	(sc-insert-citation)
	(forward-line 1)
	(setq arg (- arg 1))))))


;; ----------------------------------------------------------------------
;; fill the paragraph manually
;;
(defun sc-fill-paragraph-manually (arg)
  "Interactive program to fill the paragraph containing or following
point automagically.  All this function really does is run the fill
hook, `sc-fill-paragraph-hook'. Global variable `sc-fill-arg' is set
to numeric ARG and is used by `sc-fill-paragraph', default program for
`sc-fill-paragraph-hook'."
  (interactive "P")
  (setq sc-fill-arg arg)
  (run-hooks 'sc-fill-paragraph-hook))


;; ----------------------------------------------------------------------
;; modify sc-gal-information entries
;;
(defun sc-modify-information (arg)
  "Interactively modify information held in the alist
`sc-gal-information'. Numeric ARG, if supplied, deletes an entry from
the alist. You can easily add an entry to the alist by overriding the
completion."
  (interactive "P")
  (let* ((defaultkey (aheadsym sc-gal-information))
	 (prompt (concat "Select information key to "
			 (if (consp arg)
			     "delete"
			   "modify")
			 ": (default "
			 defaultkey
			 ") "))
	 (key (completing-read prompt sc-gal-information)))

    (if (or (string= key "")
	    (null key))
	(setq key defaultkey))

    (if (consp arg)
	(adelete 'sc-gal-information key)
      (let* ((oldval (aget sc-gal-information key t))
	     (prompt (concat "Enter new value for key \""
			     key
			     "\" (default \""
			     oldval
			     "\") "))
	     (newval (read-input prompt)))
	(if (or (string= newval "")
		(null newval))
	    nil
	  (aput 'sc-gal-information key newval))))))


;; ======================================================================
;; leach onto current mode

;; ----------------------------------------------------------------------
;; load local keymap via the hook
;;
(defun sc-append-current-keymap ()
  "In the buffer that is using supercite (usually some type of reply
buffer), append some useful keystrokes to the local key map. Evaluates
`sc-local-keymaps' for hook to run."
  (let ((hook (car (cdr (assq major-mode sc-local-keymaps)))))
    (cond
     ((not hook)
      (run-hooks 'sc-default-keymap))
     ((not (listp hook))
      (setq hook (car (cdr (assq hook sc-local-keymaps))))
      (run-hooks 'hook))
     (t
      (run-hooks 'hook))))
  (setq sc-leached-keymap (current-local-map)))


;; ----------------------------------------------------------------------
;; snag all supercite keybindings
;;
(defun sc-snag-all-keybindings ()
  "Snag all keybindings in major-modes current keymap."
  (let* ((curkeymap (current-local-map))
	 (symregexp ".*sc-.*\n")
	 (docstring (substitute-command-keys "\\{curkeymap}"))
	 (start 0)
	 (maxend (length docstring))
	 (spooge ""))
    (while (and (< start maxend)
		(string-match symregexp docstring start))
      (setq spooge (concat spooge (substring docstring
					     (match-beginning 0)
					     (match-end 0))))
      (setq start (match-end 0)))
    spooge))


;; ----------------------------------------------------------------------
;; spoogify doc string of current major mode
;;
(defun sc-spoogify-docstring ()
  "This function modifies [makes into spooge] the docstring for the
current major mode. It uses a cute hack to grab the docstring, add
some text to it, then place it back into the lambda expression for the
function. NOTE: May bomb out if the major-mode is preloaded"
  ;; check to be sure it hasn't already been added
  (let* ((symfunc (symbol-function major-mode))
	 (doc-cdr (nthcdr 2 symfunc))
	 (doc-str (documentation major-mode)))
    (cond
     ((not (stringp doc-str)))
     ((string-match "supercite" doc-str))
     (t
      (setcar doc-cdr (concat doc-str "

The major mode for this buffer has been modified to include the
SUPERCITE 2.0 package for handling attributions and citations of
original messages in email replies.  For more information on this
package, execute the command \"sc-describe\" (see below).  The
following keys are bound to SUPERCITE commands:

"
			      (sc-snag-all-keybindings)))))))


;; ======================================================================
;; this section contains default hooks and hook support for execution

;; ----------------------------------------------------------------------
;; run citations via hook
;;
(defun sc-cite-original ()
  "Hook version of sc-cite.  This is callable from the various mail and
news readers' reply function to cite the yanked region.  It does not do
any yanking of the original message but it does require a few things:

1) The reply buffer is the current buffer
2) The original message has been yanked and inserted into the reply buffer
3) Verbose headers from the original message have been inserted into the
   reply buffer directly before the text of the original message.
4) Point points to the beginning of the verbose headers
5) Mark points to the end of the body of text to be cited."
  
  (setq sc-gal-attributions nil)
  (setq sc-gal-information nil)
  (let ((start (region-beginning))
	(end   (region-end)))
    (sc-fetch-fields start end)
    (mail-yank-clear-headers start end)
    (sc-cite sc-preferred-header-style)
    (sc-append-current-keymap)
    (sc-spoogify-docstring)
    (run-hooks 'sc-run-hook)
    ))


;; ----------------------------------------------------------------------
;; rnewspost.el shouldn't rewrite the header.  This only works with
;; diffs to rnewspost.el that I posted with the original superyank
;; code.
;;
(setq news-reply-header-hook nil)


;; ----------------------------------------------------------------------
;; run the load hook
;;
(run-hooks 'sc-load-hook)



;; ======================================================================
;; describe this package
;;

;; ----------------------------------------------------------------------
;; describe verbosely
;;
(defun sc-describe ()

  "This package provides mechanisms for doing sophisticated citing of
yanked text in the reply buffers of the major news and email reading
modes. Supercite 2.0 is based on the work done for Superyank 1.11,
though the actual code of the present package bears little resemblance
to its predecesor.

Supercite 2.0 has been tested and *seems* to work with GNUS 3.12,
RMAIL 18.55 and VM 4.37. It is also supposed to work with MH-E mode
and perhaps even GNEWS.  Some modifications may be necessary to run
Supercite with these packages and this is covered in sections below.

Author:

NAME: Barry A. Warsaw                USMAIL: National Institute of Standards
TELE: (301) 975-3460                         and Technology (formerly NBS)
UUCP: {...}!uunet!cme-durer!warsaw           Rm. B-124, Bldg. 220
ARPA: warsaw@cme.nist.gov                    Gaithersburg, MD 20899

Get on the Supercite mailing list:

Send articles to supercite@cme.nist.gov or uunet!cme-durer!supercite
Send administrative queries/requests to supercite-request@cme.nist.gov
	or uunet!cme-durer!supercite-request

-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

What is a citation?  A \"citation\" is the acknowledgement of the
original author of a mail message, in the body of the reply. The
\"attribution string\" is the part of the author's name that will be
used to cite the body of the text (e.g. \"John\", the author's first
name). The \"citation string\" is built from the attribution string,
the \"citation delimiter\", and the \"citation separator\".  It is the
string that is inserted in front of every line to be cited in the
reply body (e.g. \"John> \").

There are two general forms of citation.  In \"nested citations\",
indication is made that the cited line was written by someone *other*
than the current message author, but no reference is made as to the
identity of the original author.  Here's an example of what a message
buffer would look like using nested citations after multiple replies:

     >>John originally wrote this
     >>and this as well
     > Jane said that John didn't know
     > what he was talking about
     And that's what I think too.

In \"non-nested citations\" each cited line begins with an informative
string referencing the original author.  Only the first level of
referencing will be shown; subsequent cites don't nest the references.
The same message described above might look like this if non-nested
citations were used:

     John> John originally wrote this
     John> and this as well
     Jane> Jane said that John didn't know
     Jane> what he was talking about
     And that's what I think too.

Notice that my inclusion of Jane's inclusion of John's original
message did not result in a cited line beginning with: \"Jane>John>\".
Thus no nested citations.

For non-nested citations, a fair amount of intellegence is used to
decipher the author's name from mail header information. This name,
along with email mailbox terminus and the author's initials, are
available to the Supercite user for use as the attribution string.
Once the attribution string is chosen, the citation string is built
and is inserted at the front of every line in the region to be cited.
Finally, a \"reference header\" is inserted at the top of the cited
region, which is usually used to show which citation is linked to
which author.

The citing of the text body is undoable, so the user could yank and
cite the text, undo, then continually re-cite the text until the
desired citation string is inserted. Often people would like a
nickname to be used as the citation string, but this nickname cannot
be picked up by Supercite.  It is a simple matter to undo the original
citation, and then perform a citation with the nickname as the
attribution string.

-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

There are a number of variables which control some of the features
described thus far. First there is the variable which controls whether
nested or non-nested citations will be used:

          Variable: sc-nested-citation-p
               controls citation style.  If nil, non-nested citations
               are used.  If non-nil, old-style nested citations are
               used.  Default: nil.

Whenever a citation is used, whether nested or non-nested, there is a
distinguishing character or string used to set the cited text apart
from the rest of the message body. This character or string is set in
the following variable:

          Variable: sc-citation-delimiter
               string that end-delineates a citation reference.  For
               nested citations this is the string to insert in front
               of cited lines.  For non-nested citations, this is the
               string to insert between the attribution string and the
               citation separator.  For best results this should be a
               single character, typically \">\", with no trailing
               space.  Default: \">\".

A string is inserted between `sc-citation-delimiter' and the original
line of text in both nested and non-nested citations.  For nested
citations, this string is only inserted on first level citations.  For
non-nested citations, this string is always inserted (since all
citations are first level).  This string is defined in the following
variable: 

          Variable: sc-citation-separator
               string that separates between the
               `sc-citation-delimiter' and the cited line in
               non-nested citations.  Default: \" \".  (a single
               space).

Occasionally, for whatever reason, the author's name cannot be found
and so a default author name may be used:

          Variable: sc-default-author-name
               string used when author's name cannot be found.
               Default: \"Anonymous\".

Also if the author's name cannot be found, a default attribution
string may be used, from which a legal citation string will be built:

          Variable: sc-default-attribution
               string used when author's attribution string cannot be
               found. Default: \"Anon\".

How does the package determine if a line has already been cited, so
that for non-nested citations, the line won't be recited?  This is
accomplished through the use of a regular expression.

          Variable: sc-cite-regexp
               regular expression that describes how an already cited
               line begins.  Default: \"\\\\s *[a-zA-Z0-9]*\\\\s *>+\".

-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

Supercite employs a number of heuristics to decipher the author's name
based on the \"From:\" field which is usually present in all mail and
news reading buffers.  If possible, it will pick out the author's
first, last and middle names, the author's initials and the author's
email terminus.  Supercite can recognize \"From:\" lines with the
following forms:

     From: John Xavier Doe <doe@speedy.computer.com>
     From: \"John Xavier Doe\" <doe@speedy.computer.com>
     From: doe@speedy.computer.com (John Xavier Doe)
     From: computer!speedy!doe (John Xavier Doe)
     From: doe%speedy@computer.com (John Xavier Doe)
     From: computer!speedy!doe (John Xavier-Doe)
     From: computer!speedy!doe (John Xavier-Doe -- Decent Hacker)

Once Supercite has parsed this field, it puts together a list of these
names and may present them to the user for attribution string
selection.

Note that some author fields (as in the last example above) will
contain a descriptive title.  The user can choose to ignore the title,
while still recognizing hyphenated names, through the use of a regular
expression:

          Variable: sc-titlecue-regexp
               regular expression that delineates names from titles in
               the author's name fields.  Default: \"\\\\s +-+\\\\s +\".

Selection of the attribution string is done automatically, however,
the user is able to confirm the selection of attribution string before
it is used to cite the region.  The behavior of confirmation is
controlled by a variable:

          Variable: sc-confirm-always-p
               if non-nil, always confirm attribution string with user
               before using to cite text.  If nil, use automatic
               selection to choose attribution string. Default: t.

If confirmation is chosen, then the list of possible attribution
strings (the \"alist\") is presented to the user.  A carriage return
will select the \"preferred\" attribution string (see below), and
completion *is* case sensitive.  The user can override all choices by
typing any string in the minibuffer; this string is taken as the
literal attribution string.

In the case of the \"From:\" fields above, the alist might look like:

     ((\"doe\") (\"JXD\") (\"John\") (\"Xavier\") (\"Doe\"))

with \"doe\" as the preferred attribution string. Say at this point, a
confirmed citation is performed and the user chooses \"John\", then
undoes the citation and redoes a confirmed citation.  The alist would
look like:

     ((\"John\") (\"doe\") (\"JXD\") (\"Xavier\") (\"Doe\"))

with \"John\" as the preferred attribution string.  Now, the user
types in the string \"Boss\" as the attribution string, undoes this,
then redoes a confirmed citation.  The alist would look like:

     ((\"Boss\") (\"John\") (\"doe\") (\"JXD\") (\"Xavier\") (\"Doe\"))

In this way, the user can selectively cite parts of a single message
body, tailoring the citation string to each region being cited.

-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

The \"preferred\" attribution is settable by the user and is used to
indicate which part of the author's name is used for automatic
selection of the attribution string, or the preferred default for
the first confirmed selection.

          Variable: sc-preferred-attribution
               quoted symbol specifying which portion of an author's
               name should be used when building the attribution
               string using the following key:

               emailname   -- email terminus
               initials    -- author's initials
               firstname   -- first name
               lastname    -- last name
               middlename1 -- first middle name
               middlename2 -- second middle name
               ...

               Middlename indexes can be any positive integer greater
               than 0, though it is unlikely that many authors will
               supply more than one middle name, if that many.
               Default: 'firstname.

If automatic selection is used, and the preferred attribution can't be
found, then either the `sc-default-attribution' will be used or some
secondary scheme will be employed to find a non-nil, non-empty
attribution string. You can tell Supercite to only use your preferred
attribution by setting the variable:

          Variable: sc-use-only-preference-p
               controls what happens when the preferred attribution
               string cannot be found.  If non-nil, then
               `sc-default-attribution' is used, otherwise a secondary
               scheme is employed.  Default: nil.

The secondary scheme will first try to use the author's first name. If
that is nil or empty, then the alist is searched for the first
non-nil, non-empty string.  If still no attribution string can be
found, then the user is either queried, or `sc-default-attribution' is
used, depending on the value of `sc-confirm-always-p'.

Once a legal attribution string is found, you can force the string to
lower case characters.

          Variable: sc-downcase-p
               non-nil means downcase the attribution string.
               Default: nil.

-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

Typically, citation is performed on a body of text that has been
yanked from a mail or news reading buffer.  This yanked text should
have the verbose headers at the top of the region to be cited.  As
mentioned above, Supercite will parse these headers, picking out
useful information (most notably the \"From:\" line), then delete
those mail headers, and replace them with user customizable reference
headers.  You can have multiple references header styles at your
disposal and can customize your own headers, adding them to the list
of those available. This list is kept in the variable:

          Variable: sc-rewrite-header-list
               list of user customizable reference header rewrite
               functions.
               Default: '((sc-no-header)
                           (sc-header-on-said)
                           (sc-header-inarticle-writes)
                           (sc-header-regarding-writes)
                           (sc-header-verbose)
                           (sc-header-attributed-writes)).

Add your own functions to this list or re-order the list to utilize
your own custom reference headers.  A reference header is rewritten
automatically when the text is originally yanked, and a command is
provided that allows the user to call any of the reference header
rewrite functions (see below). The header written by default is user
customizable.

          Variable: sc-preferred-header-style
               integer specifying which header rewrite function is
               used when automatically inserting the rewritten header.
               This is an index into `sc-rewrite-header-list', with
               the first element indexed as zero.  Default: 1.

Alternatively, you can use electric reference inserting which will pop
up the references in the minibuffer for your selection.  You can
minimally edit this reference or scan through the list.  Electric
referencing is also available when you use the `sc-insert-reference'
command (see below).

          Variable: sc-electric-references-p
               Non-nil specifies use electric references.
               Default: t.

While in the electric reference minibuffer, certain keys are bound to
certain functions.

     (M-p) show previous reference header
     (M-n) show next reference header
     (M-v) toggle reference header buffer visible/non-visible. This is
           especially useful if your reference is multilined, which wouldn't
           show up nicely in the minibuffer.
     (M-.) set current visible reference header to be your new preferred
           reference header by setting `sc-preferred-header-style'
     (M-,) show preferred reference header
     (C-g) abort electric references
     (RET) exit electric references and insert reference into buffer

You can do some minimal amounts of editing of the references while in
electric reference mode, but edit the minibuffer instead of *Supercite
Electric Reference* buffer.

You may want to include some information about the author in your
custom reference headers.  This information can come from the mail
headers or some internal supercite variables.  There is a variable
that describes which mail headers should be fetched and remembered for
use in the reference headers:

          Variable: sc-mail-fields-list
	       List of mail fields that you may want to use to build
	       your custom reference header.  Each field should be a
	       string which will be passed to `mail-fetch-field'.  See
	       the function `sc-field' for information on how to
	       access these fields in your reference header.
	       Default: '(\"date\" \"from\" \"message-id\" \"subject\"
			  \"newsgroup\" \"references\" \"organization\"
			  \"return-path\" \"path\" \"reply\")

In addition to those fields described in `sc-mail-fields-list',
supercite always provides these fields for use: \"sc-attribution\",
\"sc-nested-citation\", \"sc-citation\", \"from\", \"sc-author\",
\"sc-firstname\", \"sc-lastname\", \"sc-middlename-1\"....  The middle
names may or may not be present, based on the author's name field.
The \"from\" field is always provided just in case you forget to add
it to `sc-mail-fields-list'.

-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

Supercite provides some paragraph filling functions, and the user has
the choice of automatically filling each paragraph as it is cited or
leaving cited paragraphs unfilled by default, controlled by this
variable: 

          Variable: sc-auto-fill-region-p
               if non-nil, automatically fill each paragraph after it
               has been cited.  Default: nil.

With either automatic or manual filling of paragraphs, the actual
function used to fill the paragraph is hook-able:

          Variable: sc-fill-paragraph-hook
               hook for filling a paragraph. run when you fill a
               paragraph either automatically or manually.
               Default: 'sc-fill-paragraph.

-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

Here are a couple of other user definable variables.  For more
information on these or any supercite variable, type:
\\[describe-variable] <variable-name>.

          Variable: sc-mumble-string
               String returned by `sc-field' if chosen field can't be
               found.  Default: \"mumble\".

          Variable: sc-reference-tag-string
               String that is inserted before reference header lines
               on those reference rewrite functions which are
               predefined.  Default: \">>>>> \"

          Variable: sc-left-justify-p
               if non-nil, delete all leading white space before citing.
               Default: nil.

          Variable: sc-load-hook
               user definable hook which runs after supercite is loaded.
               Default: nil.

          Variable: sc-run-hook
               user definable hook which runs after `sc-cite-original'
               executes.
               Default: nil.

-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

Since every email/news reader uses a different buffer name or
major-mode to reply in, Supercite can't know ahead of time what the
buffer or major-mode name is. So Supercite needs to leaches onto
whatever buffer the reply is being made in, modifying the keymap and
the documentation string for that buffer. Many of the Supercite 2.0
beta testers used many different readers, so a mechanism was developed
to provide a per-interface keymap, which installs itself into the
buffer's current-local-map based on the major-mode of the buffer.
There are two variables which control the keymap that gets installed:

          Variable: sc-default-keymap
               Default keymap to use if major-mode keybinding cannot
               be found in `sc-local-keymaps'.
               Keybindings:
                    (C-c C-r) sc-insert-reference
                    (C-c C-t) sc-cite
                    (C-c C-a) sc-recite
                    (C-c C-u) sc-uncite
                    (C-c C-i) sc-insert-citation
                    (C-c C-o) sc-open-line
                    (C-c C-q) sc-fill-paragraph-manually
                    (C-c q)   sc-fill-paragraph-manually
                    (C-c C-m) sc-modify-information
                    (C-c ?)   sc-supercite

          Variable: sc-local-keymaps
               Variable which contains a list of interfaces and their
               keybindings.

Sc-local-keymaps is an association list of the form:

     ((MAJOR-MODE [FUNCTION | MAJOR-MODE])*)

When it is time to modify the keymap of the current buffer, Supercite
looks up the `major-mode' of that buffer in this association list. If
it matches the major mode with a MAJOR-MODE key, the value is
returned, otherwise, the default keymap is installed (see above).

If the MAJOR-MODE is found and the value is returned, this value is
checked to see if it is a list.  If so, it is assumed that this value
is a lambda expression which will set the current local keymap as
desired.  If the value is not a list, it is assumed to be a previously
defined MAJOR-MODE.  This new major mode is looked up and the lambda
expression is evaluated.  Only one level of indirection is possible,
but this does allow you to save space when defining key bindings.

-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

There are a number of interactive commands that are provided as part
of the supercite package. The bindings shown as default are those for
mail-mode buffers. Your keybindings may be different so check them
with \\[describe-bindings], *after* supercite has been loaded and the
first message has been yanked and cited.

          Command: sc-insert-reference
               insert a reference header. If numeric ARG is not supplied,
               insert the header indexed by `sc-preferred-header-style'.
               If ARG is supplied and electric referencing is set, then
               enter electric referencing.  If electric referencing is
               not set, then insert the reference indexed by ARG.
               Default binding: \"\\C-c \\C-r\".

          Command: sc-cite
               cite a region of text designated by point and mark.
               Numeric ARG is passed to `sc-insert-reference'.  When
               run interactively, confirmation is always requested.
               Default binding: \"\\C-c \\C-t\".

          Command: sc-uncite
               uncite the region of text designated by point and mark by
               guessing the fill prefix and removing that from every line
               beginning with fill prefix in the region.
               Default binding: \"\\C-c \\C-u\".

          Command: sc-recite
               recite the region of text designated by point and mark.
               Reference header is not re-inserted and confirmation is
               always requested.  Reciting is done by unciting, then
               citing the region.
               Default binding: \"\\C-c \\C-a\".

          Command: sc-insert-citation
               insert the citation string at the beginning of the
               current line.
               Default binding: \"\\C-c \\C-i\".

          Command: sc-open-line
               insert a newline and leave point before it. also insert
               the citation string at the beginning of the new line.
               Numeric ARG inserts that many new lines.
               Default binding: \"\\C-c \\C-o\".

          Command: sc-fill-paragraph-manually
               fill paragraph containing or following point by running
               the hook, `sc-fill-paragraph-hook'.  Global variable
               `sc-fill-arg' is set to numeric ARG (used by
               `sc-fill-paragraph').
               Default binding: \"\\C-c \\C-q\" and \"\\C-c q\".

          Command: sc-modify-information
               interactively add, delete or modify a key value in the
               attribution list `sc-gal-information'.
               Default binding: \"\\C-c \\C-m\".

-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

Finally, some discussion on how to interface supercite with the
various mail/news readers you might be using is in order.  This
discussion is also useful for those who might be writing a mail/news
reader and want to let your users take advantage of Supercite.

The author runs a Supercite mailing list on which a number of
mail/news reader authors and other Supercite users participate.  A
while back some discussion was held and an interface between these
readers and Supercite was agreed upon.  The ancestor to this package,
SUPERYANK 1.11 was designed along an old inteface and this package,
SUPERCITE 2.0 conforms to this new standard.  As of the date of this
writing (12-Oct-1989) only VM 4.37 conforms to this standard as
distributed. GNUS 3.12, RMAIL and MH-E all use functions distributed
with EMACS 18.55 and patches are available to the appropriate files,
distributed along with Supercite as: 

     {rnewspost,sendmail,mh-e}.el.diff

If you do not want to install the diffs, or are not able to install
them, check the file sup-misc.el.  This file contains overload
functions which can be selectively loaded to provide the necessary
functionality.  If you decide to go the overload route, be sure to set
`sc-load-hook' to something like:

     (setq sc-load-hook '(lambda () (load \"sup-misc\")))

The variables controlling overloading should be rather self
explanatory in the sup-misc.el file.  This file also contains some
miscellaneous extension to RMAIL. Also, in superyank version 1.11, the
function `mail-yank-original' was overloaded and bound to \"\\C-c
\\C-y\". This functionality is provided in this release, but will no
longer be supported.

Now, with Supercite version 2.0, a hook is provided called
`mail-yank-hooks' which is run *after* the text is yanked.  This way,
the particular mail/news reader handles setting up the reply buffer
and yanking the appropriate text into the buffer, but leaves citing
the lines up to external functions. Most readers will provide a simple
citation function hooked in by default, for those who haven't yet
discovered this superior citation package.  Mh-e users should use
`mh-yank-hooks' instead of `mail-yank-hooks'.  For these to work you
must have installed diffs to {rnewspost,sendmail,mh-e}.el or used the
overloaded functions supplied in sup-misc.el.

The mail/news reader should put the verbose mail headers at the top of
the yanked text and leave POINT at the beginning of the headers.  MARK
should point to the end of the yanked text.  Then it should run
`mail-yank-hooks'.

Add some of this code to your .emacs file to use supercite 2.0:

     (autoload 'sc-cite-original \"supercite\" \"Hookified Supercite 2.0\" t)
     (autoload 'sc-cite          \"supercite\" \"Interactive Supercite 2.0\" t)
     (autoload 'sc-describe      \"supercite\" \"Describe Supercite 2.0\" t)
     (setq mail-yank-hooks 'sc-cite-original)
     (setq mh-yank-hooks 'sc-cite-original)

Enjoy, and please send the author your compliments, questions,
suggestions and bug reports.  Don't forget, if you're interested in
discussing supercite, join the mailing list by sending mail to the
request line mentioned above."

  (interactive)
  (describe-function 'sc-describe))
