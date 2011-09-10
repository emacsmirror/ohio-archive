;;; nnir.el --- search mail with various search engines
;; Copyright (C) 1998 Kai Gro�johann

;; $Id: nnir.el,v 1.57 2000/06/23 12:29:48 grossjoh Exp $

;; Author: Kai Gro�johann <grossjohann@ls6.cs.uni-dortmund.de>
;; Keywords: news, mail, searching, ir, glimpse, wais 

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; The most recent version of this can always be fetched from the
;; following FTP site:
;; ls6-ftp.cs.uni-dortmund.de:/pub/src/emacs

;; This code is still in the development stage but I'd like other
;; people to have a look at it.  Please do not hesitate to contact me
;; with your ideas.

;; What does it do?  Well, it allows you to index your mail using some
;; search engine (freeWAIS-sf and Glimpse are currently supported),
;; then type `G G' in the Group buffer and issue a query to the search
;; engine.  You will then get a buffer which shows all articles
;; matching the query, sorted by Retrieval Status Value (score).

;; When looking at the retrieval result (in the Summary buffer) you
;; can type `G T' (aka M-x gnus-summary-nnir-goto-thread RET) on an
;; article.  You will be teleported into the group this article came
;; from, showing the thread this article is part of.  (See below for
;; restrictions.)

;; The Lisp installation is simple: just put this file on your
;; load-path, byte-compile it, and load it from ~/.gnus or something.
;; This will install a new command `G G' in your Group buffer for
;; searching your mail.

;; Restrictions:
;;
;; * Currently, this expects that you use nnml or another
;;   one-file-per-message backend.
;; * It can only search one mail backend.
;; * There are restrictions to the Glimpse setup.
;; * There are restrictions to the Wais setup.
;; * gnus-summary-nnir-goto-thread: Fetches whole group first, before
;;   limiting to the right articles.  This is much too slow, of
;;   course.  May issue a query for number of articles to fetch; you
;;   must accept the default of all articles at this point or things
;;   may break.

;; The Lisp setup involves setting a few variables and setting up the
;; search engine.  The first variable to set is `nnir-mail-backend'.
;; For me, `gnus-secondary-select-methods' contains just one select
;; method, and this is also what I put in `nnir-mail-backend'.  Type
;; `C-h v nnir-mail-backend RET' for more information -- the variable
;; documentation includes more details and a few examples.  The second
;; variable to set is `nnir-search-engine'.  Choose one of the engines
;; listed in `nnir-engines'.  (Actually `nnir-engines' is an alist,
;; type `C-h v nnir-engines RET' for more information; this includes
;; examples for setting `nnir-search-engine', too.)

;; You must also set up a search engine.  I'll tell you about the two
;; search engines currently supported:

;; 1. freeWAIS-sf
;;
;; As always with freeWAIS-sf, you need a so-called `format file'.  I
;; use the following file:
;;
;; ,-----
;; | # Kai's format file for freeWAIS-sf for indexing mails.
;; | # Each mail is in a file, much like the MH format.
;; |                                         
;; | # Document separator should never match -- each file is a document.
;; | record-sep: /^@this regex should never match@$/
;; |                                         
;; | # Searchable fields specification.      
;; |                                         
;; | region: /^[sS]ubject:/ /^[sS]ubject: */ 
;; |         subject "Subject header" stemming TEXT BOTH
;; | end: /^[^ \t]/                          
;; |                                         
;; | region: /^([tT][oO]|[cC][cC]):/ /^([tT][oO]|[cC][cC]): */
;; |         to "To and Cc headers" SOUNDEX BOTH
;; | end: /^[^ \t]/                          
;; |                                         
;; | region: /^[fF][rR][oO][mM]:/ /^[fF][rR][oO][mM]: */
;; |         from "From header" SOUNDEX BOTH
;; | end: /^[^ \t]/                          
;; |                                         
;; | region: /^$/                            
;; |         stemming TEXT GLOBAL            
;; | end: /^@this regex should never match@$/
;; `-----
;;
;; 1998-07-22: waisindex would dump core on me for large articles with
;; the above settings.  I used /^$/ as the end regex for the global
;; field.  That seemed to work okay.

;; There is a Perl module called `WAIS.pm' which is available from
;; CPAN as well as ls6-ftp.cs.uni-dortmund.de:/pub/wais/Perl.  This
;; module comes with a nifty tool called `makedb', which I use for
;; indexing.  Here's my `makedb.conf':
;;
;; ,-----
;; | # Config file for makedb
;; | 
;; | # Global options
;; | waisindex = /usr/local/bin/waisindex
;; | wais_opt  = -stem -t fields
;; | # `-stem' option necessary when `stemming' is specified for the
;; | # global field in the *.fmt file
;; | 
;; | # Own variables
;; | homedir = /home/kai
;; | 
;; | # The mail database.
;; | database        = mail
;; | files           = `find $homedir/Mail -name \*[0-9] -print`
;; | dbdir           = $homedir/.wais
;; | limit           = 100
;; `-----
;;
;; The Lisp setup involves the `nnir-wais-*' variables.  The most
;; difficult to understand variable is probably
;; `nnir-wais-remove-prefix'.  Here's what it does: the output of
;; `waissearch' basically contains the file name and the (full)
;; directory name.  As Gnus works with group names rather than
;; directory names, the directory name is transformed into a group
;; name as follows: first, a prefix is removed from the (full)
;; directory name, then all `/' are replaced with `.'.  The variable
;; `nnir-wais-remove-prefix' should contain a regex matching exactly
;; this prefix.  It defaults to `$HOME/Mail/' (note the trailing
;; slash).

;; 2. Glimpse
;;
;; The code expects you to have one Glimpse index which contains all
;; your mail files.  The Lisp setup involves setting the
;; `nnir-glimpse-*' variables.  The most difficult to understand
;; variable is probably `nnir-glimpse-remove-prefix', it corresponds
;; to `nnir-wais-remove-prefix', see above.  The `nnir-glimpse-home'
;; variable should be set to the value of the `-H' option which allows
;; one to search this Glimpse index.  I have indexed my whole home
;; directory with Glimpse, so I assume a default of `$HOME'.

;; Developer information:

;; I have tried to make the code expandable.  Basically, it is divided
;; into two layers.  The upper layer is somewhat like the `nnvirtual'
;; or `nnkiboze' backends: given a specification of what articles to
;; show from another backend, it creates a group containing exactly
;; those articles.  The lower layer issues a query to a search engine
;; and produces such a specification of what articles to show from the
;; other backend.

;; The interface between the two layers consists of the single
;; function `nnir-run-query', which just selects the appropriate
;; function for the search engine one is using.  The input to
;; `nnir-run-query' is a string, representing the query as input by
;; the user.  The output of `nnir-run-query' is supposed to be a
;; vector, each element of which should in turn be a three-element
;; vector.  The first element should be group name of the article, the
;; second element should be the article number, and the third element
;; should be the Retrieval Status Value (RSV) as returned from the
;; search engine.  An RSV is the score assigned to the document by the
;; search engine.  For Boolean search engines like Glimpse, the RSV is
;; always 1000 (or 1 or 100, or whatever you like).

;; The sorting order of the articles in the summary buffer created by
;; nnir is based on the order of the articles in the above mentioned
;; vector, so that's where you can do the sorting you'd like.  Maybe
;; it would be nice to have a way of displaying the search result
;; sorted differently?

;; So what do you need to do when you want to add another search
;; engine?  You write a function that executes the query.  Temporary
;; data from the search engine can be put in `nnir-tmp-buffer'.  This
;; function should return the list of articles as a vector, as
;; described above.  Then, you need to register this backend in
;; `nnir-engines'.  Then, users can choose the backend by setting
;; `nnir-search-engine'.

;; Todo, or future ideas:

;; * Make it so that Glimpse can also be called without `-F'.
;;
;; * It should be possible to restrict search to certain groups.
;;
;; * There is currently no error checking.
;;
;; * The summary buffer display is currently really ugly, with all the
;;   added information in the subjects.  How could I make this
;;   prettier?
;;
;; * A function which can be called from an nnir summary buffer which
;;   teleports you into the group the current article came from and
;;   shows you the whole thread this article is part of.
;;   Implementation suggestions?
;;   (1998-07-24: There is now a preliminary implementation, but
;;   it is much too slow and quite fragile.)
;;
;; * Support other mail backends.  In particular, probably quite a few
;;   people use nnfolder.  How would one go about searching nnfolders
;;   and producing the right data needed?  The group name and the RSV
;;   are simple, but what about the article number?
;;
;; * Support compressed mail files.  Probably, just stripping off the
;;   `.gz' or `.Z' file name extension is sufficient.
;;
;; * Support a find/grep combination.
;;
;; * At least for imap, the query is performed twice.
;;
;; * Support multiple mail backends.  The information that is needed
;;   by nnir could be put in the server parameters.  (Use sensible
;;   default values, though: include the name of the backend in the
;;   default value such that people do not have to mess with the
;;   server parameters if they don't want to.)  It is not clear how to
;;   do the user interface, though.  Hm.  Maybe offer the user a
;;   completable list of backends to search?  Or use the
;;   process-marked groups to find out which backends to search?  Or
;;   always search all backends?
;;

;; Have you got other ideas?

;;; Setup Code:

(defconst nnir-version "$Id: nnir.el,v 1.57 2000/06/23 12:29:48 grossjoh Exp $"
  "Version of NNIR.")

(require 'cl)
(require 'nnoo)
(require 'gnus-group)
(require 'gnus-sum)
(eval-and-compile
  (require 'gnus-util))

(nnoo-declare nnir)
(nnoo-define-basics nnir)

(gnus-declare-backend "nnir" 'mail)

;;; Developer Extension Variable:

(defvar nnir-engines
  '((glimpse nnir-run-glimpse
             ((group . "Group spec: ")))
    (wais    nnir-run-waissearch
             ())
    (excite  nnir-run-excite-search
	     ())
    (imap    nnir-run-imap
             ()))
  "Alist of supported search engines.
Each element in the alist is a three-element list (ENGINE FUNCTION ARGS).
ENGINE is a symbol designating the searching engine.  FUNCTION is also
a symbol, giving the function that does the search.  The third element
ARGS is a list of cons pairs (PARAM . PROMPT).  When issuing a query,
the FUNCTION will issue a query for each of the PARAMs, using PROMPT.

The value of `nnir-search-engine' must be one of the ENGINE symbols.
For example, use the following line for searching using freeWAIS-sf:
    (setq nnir-search-engine 'wais)
Use the following line if you read your mail via IMAP and your IMAP
server supports searching:
    (setq nnir-search-engine 'imap)
Note that you have to set additional variables for most backends.  For
example, the `wais' backend needs the variables `nnir-wais-program',
`nnir-wais-database' and `nnir-wais-remove-prefix'.

Add an entry here when adding a new search engine.")

;;; User Customizable Variables:

(defgroup nnir nil
  "Search nnmh and nnml groups in Gnus with Glimpse, freeWAIS-sf, or EWS.")

;; Mail backend.

(defcustom nnir-mail-backend '(nnml "")
  "*Specifies which backend should be searched.
More precisely, this is used to determine from which backend to fetch the
messages found.

This must be equal to an existing server, so maybe it is best to use
something like the following:
    (setq nnir-mail-backend (nth 0 gnus-secondary-select-methods))
The above line works fine if the mail backend you want to search is
the first element of gnus-secondary-select-methods (`nth' starts counting
at zero)."
  :type '(sexp)
  :group 'nnir)

;; Search engine to use.

(defcustom nnir-search-engine 'wais
  "*The search engine to use.  Must be a symbol.
See `nnir-engines' for a list of supported engines, and for example
settings of `nnir-search-engine'."
  :type '(sexp)
  :group 'nnir)

;; Glimpse engine.

(defcustom nnir-glimpse-program "glimpse"
  "*Name of Glimpse executable."
  :type '(string)
  :group 'nnir)

(defcustom nnir-glimpse-home (getenv "HOME")
  "*Value of `-H' glimpse option.
`~' and environment variables must be expanded, see the functions
`expand-file-name' and `substitute-in-file-name'."
  :type '(directory)
  :group 'nnir)

(defcustom nnir-glimpse-remove-prefix (concat (getenv "HOME") "/Mail/")
  "*The prefix to remove from each file name returned by Glimpse
in order to get a group name (albeit with / instead of .).

For example, suppose that Glimpse returns file names such as
\"/home/john/Mail/mail/misc/42\".  For this example, use the following
setting:  (setq nnir-glimpse-remove-prefix \"/home/john/Mail/\")
Note the trailing slash.  Removing this prefix gives \"mail/misc/42\".
`nnir' knows to remove the \"/42\" and to replace \"/\" with \".\" to
arrive at the correct group name, \"mail.misc\"."
  :type '(directory)
  :group 'nnir)

(defcustom nnir-glimpse-additional-switches '("-i")
  "*A list of strings, to be given as additional arguments to glimpse.
The switches `-H', `-W', `-l' and `-y' are always used -- calling
glimpse without them does not make sense in our situation.
Suggested elements to put here are `-i' and `-w'.

Note that this should be a list.  Ie, do NOT use the following:
    (setq nnir-glimpse-additional-switches \"-i -w\") ; wrong!
Instead, use this:
    (setq nnir-glimpse-additional-switches '(\"-i\" \"-w\"))"
  :type '(repeat (string))
  :group 'nnir)

;; freeWAIS-sf.

(defcustom nnir-wais-program "waissearch"
  "*Name of waissearch executable."
  :type '(string)
  :group 'nnir)

(defcustom nnir-wais-database (expand-file-name "~/.wais/mail")
  "*Name of Wais database containing the mail.

Note that this should be a file name without extension.  For example,
if you have a file /home/john/.wais/mail.fmt, use this:
    (setq nnir-wais-database \"/home/john/.wais/mail\")
The string given here is passed to `waissearch -d' as-is."
  :type '(file)
  :group 'nnir)

(defcustom nnir-wais-remove-prefix (concat (getenv "HOME") "/Mail/")
  "*The prefix to remove from each directory name returned by waissearch
in order to get a group name (albeit with / instead of .).

This variable is similar to `nnir-glimpse-remove-prefix', only for Wais,
not Glimpse."
  :type '(directory)
  :group 'nnir)

;; EWS (Excite for Web Servers) engine.

(defcustom nnir-excite-aquery-program "aquery.pl"
  "*Name of the EWS query program.  Should be `aquery.pl' or a path to same."
  :type '(string)
  :group 'nnir)

(defcustom nnir-excite-collection "Mail"
  "*Name of the EWS collection to search."
  :type '(string)
  :group 'nnir)

(defcustom nnir-excite-remove-prefix (concat (getenv "HOME") "/Mail/")
  "*The prefix to remove from each file name returned by EWS
in order to get a group name (albeit with / instead of .).

This variable is very similar to `nnir-glimpse-remove-prefix', except
that it is for EWS, not Glimpse."
  :type '(directory)
  :group 'nnir)

;;; Internal Variables:

(defvar nnir-current-query nil
  "Internal: stores current query (= group name).")

(defvar nnir-current-server nil
  "Internal: stores current server (does it ever change?).")

(defvar nnir-current-group-marked nil
  "Internal: stores current list of process-marked groups.")

(defvar nnir-artlist nil
  "Internal: stores search result.")

(defvar nnir-tmp-buffer " *nnir*"
  "Internal: temporary buffer.")

;;; Code:

;; Gnus glue.

(defun gnus-group-make-nnir-group (extra-parms query)
  "Create an nnir group.  Asks for query."
  (interactive "P\nsQuery: ")
  (let ((parms nil))
    (if extra-parms
        (setq parms (nnir-read-parms query))
      (setq parms (list (cons 'query query))))
    (gnus-group-read-ephemeral-group
     (prin1-to-string parms) '(nnir "") t
     (cons (current-buffer)
           gnus-current-window-configuration)
     nil)))

;; Emacs 19 compatibility?
(or (fboundp 'kbd) (defalias 'kbd 'read-kbd-macro))

(defun nnir-group-mode-hook ()
  (define-key gnus-group-mode-map
    (if (fboundp 'read-kbd-macro)
        (kbd "G G")
      "GG")                             ; XEmacs 19 compat
    'gnus-group-make-nnir-group))
(add-hook 'gnus-group-mode-hook 'nnir-group-mode-hook)



;; Summary mode commands.

(defun gnus-summary-nnir-goto-thread ()
  "Only applies to nnir groups.  Go to group this article came from
and show thread that contains this article."
  (interactive)
  (unless (eq 'nnir (car (gnus-find-method-for-group gnus-newsgroup-name)))
    (error "Can't execute this command unless in nnir group."))
  (let* ((cur (cdr gnus-article-current))
         (backend-group (nnir-artlist-artitem-group nnir-artlist cur))
         (backend-number (nnir-artlist-artitem-number nnir-artlist cur)))
    (gnus-group-read-ephemeral-group
     backend-group
     nnir-mail-backend
     t                                  ; activate
     (cons (current-buffer)
           'summary)                    ; window config
     nil
     (list backend-number))
    (gnus-summary-limit (list backend-number))
    (gnus-summary-refer-thread)))

(if (fboundp 'eval-after-load)
    (eval-after-load "gnus-sum"
      '(define-key gnus-summary-goto-map
         "T" 'gnus-summary-nnir-goto-thread))
  (add-hook 'gnus-summary-mode-hook
            (function (lambda ()
                        (define-key gnus-summary-goto-map
                          "T" 'gnus-summary-nnir-goto-thread)))))



;; Gnus backend interface functions.

(deffoo nnir-open-server (server &optional definitions)
  ;; Just set the server variables appropriately.
  (nnoo-change-server 'nnir server definitions))

(deffoo nnir-request-group (group &optional server fast)
  "GROUP is the query string."
  (nnir-possibly-change-server server)
  ;; Check for cache and return that if appropriate.
  (if (and (equal group nnir-current-query)
           (equal gnus-group-marked nnir-current-group-marked)
           (or (null server)
               (equal server nnir-current-server)))
      nnir-artlist
    ;; Cache miss.
    (setq nnir-artlist (nnir-run-query group))
    (save-excursion
      (set-buffer nntp-server-buffer)
      (if (zerop (length nnir-artlist))
          (progn
            (setq nnir-current-query nil
                  nnir-current-server nil
                  nnir-current-group-marked nil
                  nnir-artlist nil)
            (nnheader-report 'nnir "Search produced empty results."))
        ;; Remember data for cache.
        (setq nnir-current-query group)
        (when server (setq nnir-current-server server))
        (setq nnir-current-group-marked gnus-group-marked)
        (nnheader-insert "211 %d %d %d %s\n"
                         (nnir-artlist-length nnir-artlist) ; total #
                         1              ; first #
                         (nnir-artlist-length nnir-artlist) ; last #
                         group)))))     ; group name

(deffoo nnir-retrieve-headers (articles &optional group server fetch-old)
  (save-excursion
    (let ((artlist (copy-sequence articles))
          (idx 1)
          (art nil)
          (artitem nil)
          (artgroup nil) (artno nil)
          (artrsv nil)
          (artfullgroup nil)
          (novitem nil)
          (novdata nil)
          (foo nil))
      (while (not (null artlist))
        (setq art (car artlist))
        (or (numberp art)
            (nnheader-report
             "nnir-retrieve-headers doesn't grok message ids: %s"
             art))
        (setq artitem (nnir-artlist-article nnir-artlist art))
        (setq artrsv (nnir-artitem-rsv artitem))
        (setq artgroup (nnir-artitem-group artitem))
        (setq artno (nnir-artitem-number artitem))
        (setq artfullgroup (nnir-group-full-name artgroup))
        ;; retrieve NOV or HEAD data for this article, transform into
        ;; NOV data and prepend to `novdata'
        (set-buffer nntp-server-buffer)
        (case (setq foo (gnus-retrieve-headers (list artno) artfullgroup nil))
          (nov
           (goto-char (point-min))
           (setq novitem (nnheader-parse-nov))
           (unless novitem
             (pop-to-buffer nntp-server-buffer)
             (error
              "nnheader-parse-nov returned nil for article %s in group %s"
              artno artfullgroup)))
          (headers
           (goto-char (point-min))
           (setq novitem (nnheader-parse-head))
           (unless novitem
             (pop-to-buffer nntp-server-buffer)
             (error
              "nnheader-parse-head returned nil for article %s in group %s"
              artno artfullgroup)))
          (t (nnheader-report "Don't support header type %s." foo)))
        ;; replace article number in original group with article number
        ;; in nnir group
        (mail-header-set-number novitem idx)
        (mail-header-set-from novitem
                              (mail-header-from novitem))
        (mail-header-set-subject
         novitem
         (format "[%d: %s/%d] %s"
                 artrsv artgroup artno
                 (mail-header-subject novitem)))
        ;;-(mail-header-set-extra novitem nil)
        (push novitem novdata)
        (setq artlist (cdr artlist))
        (setq idx (1+ idx)))
      (setq novdata (nreverse novdata))
      (set-buffer nntp-server-buffer) (erase-buffer)
      (mapcar 'nnheader-insert-nov novdata)
      'nov)))

(deffoo nnir-request-article (article
                              &optional group server to-buffer)
  (save-excursion
    (let* ((artitem (nnir-artlist-article nnir-artlist
                                          article))
           (artgroup (nnir-artitem-group artitem))
           (artno (nnir-artitem-number artitem))
           ;; Bug?
           ;; Why must we bind nntp-server-buffer here?  It won't
           ;; work if `buf' is used, say.  (Of course, the set-buffer
           ;; line below must then be updated, too.)
           (nntp-server-buffer (or to-buffer nntp-server-buffer)))
      (set-buffer nntp-server-buffer)
      (erase-buffer)
      (message "Requesting article %d from group %s"
               artno
               (nnir-group-full-name artgroup))
      (gnus-request-article artno (nnir-group-full-name artgroup)
                            nntp-server-buffer)
      (cons artgroup artno))))


(nnoo-define-skeleton nnir)

;;; Search Engine Interfaces:

;; Glimpse interface.
(defun nnir-run-glimpse (query &optional group)
  "Run given query against glimpse.  Returns a vector of (group name, file name)
pairs (also vectors, actually)."
  (save-excursion
    (let ((artlist nil)
          (groupspec (cdr (assq 'group query)))
          (qstring (cdr (assq 'query query))))
      (when (and group groupspec)
        (error (concat "It does not make sense to use a group spec"
                       " with process-marked groups.")))
      (when group
        (setq groupspec (gnus-group-real-name group)))
      (set-buffer (get-buffer-create nnir-tmp-buffer))
      (erase-buffer)
      (if groupspec
          (message "Doing glimpse query %s on %s..." query groupspec)
        (message "Doing glimpse query %s..." query))
      (let* ((cp-list
              `( ,nnir-glimpse-program
                 nil                    ; input from /dev/null
                 t                      ; output
                 nil                    ; don't redisplay
                 "-H" ,nnir-glimpse-home ; search home dir
                 "-W"                   ; match pattern in file
                 "-l" "-y"              ; misc options
                 ,@nnir-glimpse-additional-switches
                 "-F" ; restrict output to mail, maybe to group
                 ,(if groupspec
                      (concat nnir-glimpse-remove-prefix ";" groupspec)
                    nnir-glimpse-remove-prefix)
                 ,qstring               ; the query, in glimpse format
                ))
             (exitstatus
              (progn
                (message "%s args: %s" nnir-glimpse-program
                         (mapconcat 'identity (cddddr cp-list) " "))
                (apply 'call-process cp-list))))
        (unless (or (null exitstatus)
                    (zerop exitstatus))
          (nnheader-report "Couldn't run glimpse: %s" exitstatus)
          ;; Glimpse failure reason is in this buffer, show it if
          ;; the user wants it.
          (when (> gnus-verbose 6)
            (display-buffer nnir-tmp-buffer))))
      (if groupspec
          (message "Doing glimpse query %s on %s..." query groupspec)
        (message "Doing glimpse query %s...done" query))
      (sit-for 0)
      ;; CCC: The following work of extracting group name and article
      ;; number from the Glimpse output can probably better be done by
      ;; just going through the buffer once, and plucking out the
      ;; right information from each line.
      ;; remove superfluous stuff from glimpse output
      (goto-char (point-min))
      (delete-non-matching-lines "/[0-9]+$")
      ;;(delete-matching-lines "\\.overview~?$")
      (goto-char (point-min))
      (while (re-search-forward (concat "^" nnir-glimpse-remove-prefix) nil t)
        (replace-match ""))
      ;; separate group name from article number with \t
      ;; XEmacs compatible version
      (goto-char (point-max))
      (while (re-search-backward "/[0-9]+$" nil t)
        (delete-char 1 nil)
        (insert-char ?\t 1))
; Emacs compatible version
;      (goto-char (point-min))
;      (while (re-search-forward "\\(/\\)[0-9]+$" nil t)
;        (replace-match "\t" t t nil 1))
      ;; replace / with . in group names
      (subst-char-in-region (point-min) (point-max) ?/ ?. t)
      ;; massage buffer to contain some Lisp;
      ;; this depends on the artlist encoding internals
      ;; maybe this dependency should be removed?
      (goto-char (point-min))
      (while (not (eobp))
        (insert "[\"")
        (skip-chars-forward "^\t")
        (insert "\" ")
        (end-of-line)
        (insert " 1000 ]")              ; 1000 = score
        (forward-line 1))
      (insert "])\n")
      (goto-char (point-min))
      (insert "(setq artlist [\n")
      (eval-buffer)
      (sort* artlist
             (function (lambda (x y)
                         (if (string-lessp (nnir-artitem-group x)
                                           (nnir-artitem-group y))
                             t
                           (< (nnir-artitem-number x)
                              (nnir-artitem-number y))))))
      )))

;; freeWAIS-sf interface.
(defun nnir-run-waissearch (query &optional group)
  "Run given query agains waissearch.  Returns vector of (group name, file name)
pairs (also vectors, actually)."
  (when group
    (error "The freeWAIS-sf backend cannot search specific groups."))
  (save-excursion
    (let ((qstring (cdr (assq 'query query)))
          (artlist nil)
          (score nil) (artno nil) (dirnam nil) (group nil))
      (set-buffer (get-buffer-create nnir-tmp-buffer))
      (erase-buffer)
      (message "Doing WAIS query %s..." query)
      (call-process nnir-wais-program
                    nil                 ; input from /dev/null
                    t                   ; output to current buffer
                    nil                 ; don't redisplay
                    "-d" nnir-wais-database ; database to search
                    qstring)
      (message "Massaging waissearch output...")
      ;; remove superfluous lines
      (keep-lines "Score:")
      ;; extract data from result lines
      (goto-char (point-min))
      (while (re-search-forward
              "Score: +\\([0-9]+\\).*'\\([0-9]+\\) +\\([^']+\\)/'" nil t)
        (setq score (match-string 1)
              artno (match-string 2)
              dirnam (match-string 3))
        (unless (string-match nnir-wais-remove-prefix dirnam)
          (nnheader-report 'nnir "Dir name %s doesn't contain prefix %s"
                           dirnam nnir-wais-remove-prefix))
        (setq group (substitute ?. ?/ (replace-match "" t t dirnam)))
        (push (vector group
                      (string-to-int artno)
                      (string-to-int score))
              artlist))
      (message "Massaging waissearch output...done")
      (apply 'vector
             (sort* artlist
                    (function (lambda (x y)
                                (> (nnir-artitem-rsv x)
                                   (nnir-artitem-rsv y)))))))))

;; EWS (Excite for Web Servers) interface
(defun nnir-run-excite-search (query &optional group)
  "Run a given query against EWS.  Returns vector of (group name, file name)
pairs (also vectors, actually)."
  (when group
    (error "Searching specific groups not implemented for EWS."))
  (save-excursion
    (let ((qstring (cdr (assq 'query query)))
	  artlist group article-num)
      (setq nnir-current-query query)
      (set-buffer (get-buffer-create nnir-tmp-buffer))
      (erase-buffer)
      (message "Doing EWS query %s..." qstring)
      (call-process nnir-excite-aquery-program
		    nil			; input from /dev/null
		    t			; output to current buffer
		    nil			; don't redisplay
		    nnir-excite-collection
		    (if (string= (substring qstring 0 1) "(")
			qstring
		      (format "(concept %s)" qstring)))
      (message "Gathering query output...")

      (goto-char (point-min))
      (while (re-search-forward
	      "^[0-9]+\\s-[0-9]+\\s-[0-9]+\\s-\\(\\S-*\\)" nil t)
	(setq article (match-string 1))
	(unless (string-match
		 (concat "^" (regexp-quote nnir-excite-remove-prefix)
			 "\\(.*\\)/\\([0-9]+\\)") article)
	  (nnheader-report 'nnir "Dir name %s doesn't contain prefix %s"
			   article nnir-excite-remove-prefix))
	(setq group (substitute ?. ?/ (match-string 1 article)))
	(setq article-num (match-string 2 article))
	(setq artlist (vconcat artlist (vector (vector group
						       (string-to-int article-num)
						       1000)))))
      (message "Gathering query output...done")
      artlist)))

;; IMAP interface.  The following function is Copyright (C) 1998 Simon
;; Josefsson <jas@pdc.kth.se>.
;; todo:
;; nnir invokes this two (2) times???!
;; we should not use nnimap at all but open our own server connection
;; we should not LIST * but use nnimap-list-pattern from defs
;; send queries as literals
;; handle errors
(defun nnir-run-imap (query &optional group)
  (require 'imap)
  (require 'nnimap)
  (unless group
    (error "Must specify groups for IMAP searching."))
  (save-excursion
    (let ((qstring (cdr (assq 'query query)))
	  (server (cadr nnir-mail-backend))
	  (defs (caddr nnir-mail-backend))
	  artlist buf)
      (message "Opening server %s" server)
      (condition-case ()
	  (when (nnimap-open-server server defs) ;; xxx
	    (setq buf nnimap-server-buffer) ;; xxx
	    (message "Searching %s..." group)
            (let ((arts 0)
                  (mbx (gnus-group-real-name group)))
              (when (imap-mailbox-select mbx nil buf)
                (mapcar
                 (lambda (artnum)
                   (push (vector mbx artnum 1) artlist)
                   (setq arts (1+ arts)))
                 (imap-search (concat "TEXT \"" qstring "\"") buf))
                (message "Searching %s... %d matches" mbx arts)))
            (message "Searching %s...done" group))
        (quit nil))
      (reverse artlist))))

;;; Util Code:

(defun nnir-read-parms (query)
  "Reads additional search parameters according to `nnir-engines'."
  (let ((parmspec (caddr (assoc nnir-search-engine nnir-engines))))
    (cons (cons 'query query)
          (mapcar 'nnir-read-parm parmspec))))

(defun nnir-read-parm (parmspec)
  "Reads a single search parameter.
`parmspec' is a cons cell, the car is a symbol, the cdr is a prompt."
  (let ((sym (car parmspec))
        (prompt (cdr parmspec)))
    (cons sym (read-string prompt))))

(defun nnir-run-query (query)
  "Invoke appropriate search engine function (see `nnir-engines').
If some groups were process-marked, run the query for each of the groups
and concat the results."
  (let ((search-func (cadr (assoc nnir-search-engine nnir-engines)))
        (q (car (read-from-string query))))
    (if gnus-group-marked
        (apply 'append
               (mapcar (lambda (x)
                         (funcall search-func q x))
                       gnus-group-marked))
      (funcall search-func q nil))))

(defun nnir-group-full-name (shortname)
  "For the given group name, return a full Gnus group name.
The Gnus backend/server information is added."
  (gnus-group-prefixed-name shortname nnir-mail-backend))

(defun nnir-possibly-change-server (server)
  (unless (and server (nnir-server-opened server))
    (nnir-open-server server)))


;; Data type article list.

(defun nnir-artlist-length (artlist)
  "Returns number of articles in artlist."
  (length artlist))

(defun nnir-artlist-article (artlist n)
  "Returns from ARTLIST the Nth artitem (counting starting at 1)."
  (elt artlist (1- n)))

(defun nnir-artitem-group (artitem)
  "Returns the group from the ARTITEM."
  (elt artitem 0))

(defun nnir-artlist-artitem-group (artlist n)
  "Returns from ARTLIST the group of the Nth artitem (counting from 1)."
  (nnir-artitem-group (nnir-artlist-article artlist n)))

(defun nnir-artitem-number (artitem)
  "Returns the number from the ARTITEM."
  (elt artitem 1))

(defun nnir-artlist-artitem-number (artlist n)
  "Returns from ARTLIST the number of the Nth artitem (counting from 1)."
  (nnir-artitem-number (nnir-artlist-article artlist n)))

(defun nnir-artitem-rsv (artitem)
  "Returns the Retrieval Status Value (RSV, score) from the ARTITEM."
  (elt artitem 2))

(defun nnir-artlist-artitem-rsv (artlist n)
  "Returns from ARTLIST the Retrieval Status Value of the Nth artitem
(counting from 1)."
  (nnir-artitem-rsv (nnir-artlist-article artlist n)))

;; unused?
(defun nnir-artlist-groups (artlist)
  "Returns a list of all groups in the given ARTLIST."
  (let ((res nil)
        (with-dups nil))
    ;; from each artitem, extract group component
    (setq with-dups (mapcar 'nnir-artitem-group artlist))
    ;; remove duplicates from above
    (mapcar (function (lambda (x) (add-to-list 'res x)))
            with-dups)
    res))


;; The end.
(provide 'nnir)
