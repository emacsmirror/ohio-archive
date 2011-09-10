; Path: hal.com!olivea!uunet!hotmomma!sdb
; From: sdb@ssr.com (Scott Ballantyne)
; Newsgroups: gnu.emacs.sources
; Subject: NEW nnspool.el (uses CNews nov database)
; Date: 17 Nov 92 00:25:20 GMT
; Organization: ScotSoft Research
; 
; One of the developers of CNews (Geoff Collyer) has implemented a
; common newsreader database which will be standard with all future
; CNews distributions. On the whole, this is a great improvement,
; certainly better than groveling through the spool or developing yet
; another newsreader database implementation.
; 
; Below the 'snip' line, is a version of nnspool.el which causes emacs
; to default to the use of the overview database, but remains compatible
; with all news systems (B-News, and CNews without the news database).
; If this database is not found, then it will try to use the gnusgethdrs
; program (an earlier speedup to nnspool) and if that is not found, will
; use the original (quite slow) method.
; 
; I'm including the entire file below, the diffs were almost the same
; size. If anyone wants the source to gnusgethdrs.c, send me mail.
; 
; Enjoy,
; Scott
; ---
; sdb@ssr.com
; 
; ---------------------------------snip---------------------------------
;;; Spool access using NNTP for GNU Emacs
;; Copyright (C) 1988, 1989 Fujitsu Laboratories LTD.
;; Copyright (C) 1988, 1989, 1990 Masanobu UMEDA
;; $Header: nnspool.el,v 1.10 90/03/23 13:25:25 umerin Locked $
;;
;; LCD Archive Entry:
;; nnspool|Scott Ballantyne|sdb@ssr.com|
;; Spool access using NNTP for GNU Emacs.|
;; 92-11-17||~/interfaces/nnspool.el.Z|
;;
;; Modifications Copright (C) 1991, 1992 Scott Ballantyne, and
;; released under the same conditions as the original.
;;
;; Newest modification: Use new .overview databse in new C-News version,
;; if no .overview file found, try header frobnicator and then original
;; method (in that order).
;;
;; Earlier modifications:
;; nnspool-replace-chars-in-string: Slightly faster than original
;; nnspool-find-article-by-message-id: Uses a database lookup - much faster.
;; nnspool-retrieve-headers: Uses a C program to scan the spool, this
;; avoids having to read in the entire article just to get at the headers.
;; Other changes make this function considerably faster.  
;;
;; NOTE: These modifications were tested under C-News, and in fact the
;;       c program 'gnusgethdrs.c' requires libcnews.a and some of the
;;       C-News headers, so it is not likely to work if you are using
;;       B-News.  The old functions are still here, and you can still
;;       have them called by setting the values of certain variables
;;       appropriately. See the documentation for:
;;
;; nnspool-dbm-program
;; nnspool-dbm-args
;; nnspool-header-frobnicator
;; 
;; to see how these variables affect the operation of gnus.
;;
;; Scott (sdb@ssr.com)

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

(provide 'nnspool)
(require 'nntp)

(defvar nnspool-inews-program news-inews-program
  "*Program to post news.")

(defvar nnspool-inews-switches '("-h")
  "*Switches for nnspool-request-post to pass to `inews' for posting news.")

(defvar nnspool-spool-directory news-path
  "*Local news spool directory.")

(defvar nnspool-active-file "/usr/lib/news/active"
  "*Local news active file.")

(defvar nnspool-history-file "/usr/lib/news/history"
  "*Local news history file.")

(defvar nnspool-dbm-program "/usr/lib/newsbin/dbz"
  "*Program used to search history database")

(defvar nnspool-dbm-args "-ix"
  "*Arguments to pass to nnspool-dbm-program")

(defvar nnspool-header-frobnicator "/usr/local/bin/gnusgethdrs"
  "*Program to retrieve headers from spool.")


(defconst nnspool-version "NNSPOOL 1.2x"
  "Version numbers of this version of NNSPOOL.")

(defvar nnspool-current-directory nil
  "Current news group directory.")

;;;
;;; Replacement of Extended Command for retrieving many headers.
;;;

(defun nnspool-retrieve-headers (sequence)
  "Return list of article headers specified by SEQUENCE of article numbers.
The format of list is
 `([NUMBER SUBJECT FROM XREF LINES DATE MESSAGE-ID REFERENCES] ...)'.
Reader macros for the vector are defined as `nntp-header-FIELD'.
Writer macros for the vector are defined as `nntp-set-header-FIELD'.
News group must be selected before calling me."
  (let ((overviewfile (concat (file-name-as-directory nnspool-current-directory) ".overview"))
	(number (length sequence))
	(count 0)
	(headers nil)
	(article 0)
	(subject nil)
	(message-id nil)
	(from nil)
	(xref nil)
	(lines 0)
	(date nil)
	(references nil))

    ;; If no .overview file found, use try to use the header frobnicator, if
    ;; that's not available, revert to the original approach.
    ;; Hopefully this will allow this module to remain compatible with all
    ;; news systems.
    ;; One optimization which is useful: For single article requests,
    ;; *DO* use one of the older methods, since that is less overhead than
    ;; retrieving the entire .overview file
    (if (or (= 1 number) (not (file-readable-p overviewfile)))
	(if (null nnspool-header-frobnicator)
	    (nnspool-slowly-retrieve-headers)
	  (nnspool-use-header-frobnicator))
      (save-excursion
	(set-buffer nntp-server-buffer)
	(erase-buffer)
	(message "Retrieving headers...")
	(insert-file-contents overviewfile)
	(message "Searching for headers...")
	;;
	;; Delete all lines that do not refer to articles we want.
	;; This is a performance win for the most common case, where someone is reading
	;; a big gob of articles in the 'middle' of the newsgroup.
	;;
	(delete-region (point-min)
		       (progn (re-search-forward
			       (concat "^" (int-to-string (apply 'min sequence)) "\t") nil t)
			      (beginning-of-line) (point)))
	
	(goto-char (point-max))
	(delete-region (progn (re-search-backward (concat "^" (int-to-string (apply 'max sequence)) "\t") nil t)
			      (forward-line 1) (beginning-of-line) (point))
		       (point-max))
	(goto-char (point-min))
	(while (not (eobp))
	  (if (memq (string-to-int (buffer-substring (point) (save-excursion (forward-word 1) (point)))) sequence)
	      (forward-line 1)
	    (delete-region (point) (1+ (save-excursion (end-of-line) (point))))))
	;;
	;; Now stuff articles into vector
	;;
	;; overview file has (separated by tabs)
	;; number subj author date msgid ref bytecount linecount
	;; we want
	;; NUMBER SUBJ authr  xref lines date msgid references
	(goto-char (point-min))
	(while (not (eobp))
	  (beginning-of-line)		; make sure we are start of line
	  (setq position (point))
	  (setq article  (string-to-int (buffer-substring position (progn (search-forward "\t") (1- (point))))))
	  (setq position (point))
	  (setq subject (buffer-substring position (progn (search-forward "\t") (1- (point)))))
	  (setq position (point))
	  (setq from (buffer-substring position (progn (search-forward "\t") (1- (point)))))
	  (setq position (point))
	  (setq date (buffer-substring position (progn (search-forward "\t") (1- (point)))))
	  (setq position (point))
	  (setq message-id (buffer-substring position (progn (search-forward "\t") (1- (point)))))
	  (setq position (point))
	  (if (/= (following-char) '?\t)
	      (setq references (buffer-substring position (progn (search-forward "\t") (1- (point)))))
	    (setq references nil)
	    (forward-char 1))
	  (search-forward "\t")		;skip byte-count for now
	  (if (memq (following-char) '(?\t ?\n))
	      (setq lines 0)
	    (setq lines (string-to-int (buffer-substring (point) (progn (forward-word 1) (point))))))
	  (setq headers (cons (vector article subject from xref lines date message-id references) headers))
	  (setq count (1+ count))
	  (forward-line 1)
	  (and (numberp nntp-large-newsgroup)
	       (> number nntp-large-newsgroup)
	       (zerop (% count 20))
	       (message "%d%% done." (/ (* count 100) number))))
	  (message "")
	  (and (numberp nntp-large-newsgroup)
	       (> number nntp-large-newsgroup)
	       (message "100%% of headers received."))
	  (nreverse headers)))))

	  
(defun nnspool-use-header-frobnicator ()
  "Process articles by calling an external frobnicator program"
  (save-excursion
    (set-buffer nntp-server-buffer)
    (erase-buffer)
    (let ((process-connection-type nil) ;use a pipe
	  (number (length sequence))
	  (count 0)
	  (headers nil)) 	; result list
      (message "Searching %s for headers." nnspool-current-directory)
      (apply 'call-process nnspool-header-frobnicator nil t nil
	     nnspool-current-directory
	     (mapcar 'int-to-string sequence))
      (goto-char (point-min))
      (while (not (eobp))
	(let ((hdrs nil)
	      (i 0))
	  (while (< i 8)
	    (setq hdrs
		  (cons (cond ((= (following-char) 10) nil)
			      ((or (= 3 i) (= 7 i))
			       (string-to-int (buffer-substring
					       (point) (save-excursion (end-of-line) (point)))))
			      (t (buffer-substring
				  (point) (save-excursion (end-of-line) (point))))) hdrs))
	    (forward-line 1)
	    (setq i (1+ i)))
	  (setq headers (cons (apply 'vector hdrs) headers)))
	(setq count (1+ count))
	(and (numberp nntp-large-newsgroup)
	     (> number nntp-large-newsgroup)
	     (zerop (% count 20))
	     (message "NNSPOOL: %d%% of headers received."
		      (/ (* count 100) number))))
      (message "")	; needed to remove Searching
      (and (numberp nntp-large-newsgroup)
	   (> number nntp-large-newsgroup)
	   (message "NNSPOOL: 100%% of headers received."))
      (nreverse headers))))

(defun nnspool-slowly-retrieve-headers ()
  "This is the original form of nnspool-retrieve-headers.
It is called when the variable nnspool-dbm-program is nil.
For details, see the documentation for nnspool-retrieve-headers."
  (save-excursion
    (set-buffer nntp-server-buffer)
    ;;(erase-buffer)
    (let ((file nil))
      (while sequence
	;;(nntp-send-strings-to-server "HEAD" (car sequence))
	(setq article (car sequence))
	(setq file
	      (concat nnspool-current-directory (prin1-to-string article)))
	(if (and (file-exists-p file)
		 (not (file-directory-p file)))
	    (progn
	      (erase-buffer)
	      (insert-file-contents file)
	      ;; Make message body invisible.
	      (goto-char (point-min))
	      (search-forward "\n\n" nil 'move)
	      (narrow-to-region (point-min) (point))
	      ;; Fold continuation lines.
	      (goto-char (point-min))
	      (while (re-search-forward "\\(\r?\n[ \t]+\\)+" nil t)
		(replace-match " " t t))
	      ;; Make it possible to search for `\nFIELD'.
	      (goto-char (point-min))
	      (insert "\n")
	      ;; Extract From:
	      (goto-char (point-min))
	      (if (search-forward "\nFrom: " nil t)
		  (setq from (buffer-substring
			      (point)
			      (save-excursion (end-of-line) (point))))
		(setq from "(Unknown User)"))
	      ;; Extract Subject:
	      (goto-char (point-min))
	      (if (search-forward "\nSubject: " nil t)
		  (setq subject (buffer-substring
				 (point)
				 (save-excursion (end-of-line) (point))))
		(setq subject "(None)"))
	      ;; Extract Message-ID:
	      (goto-char (point-min))
	      (if (search-forward "\nMessage-ID: " nil t)
		  (setq message-id (buffer-substring
				    (point)
				    (save-excursion (end-of-line) (point))))
		(setq message-id nil))
	      ;; Extract Date:
	      (goto-char (point-min))
	      (if (search-forward "\nDate: " nil t)
		  (setq date (buffer-substring
			      (point)
			      (save-excursion (end-of-line) (point))))
		(setq date nil))
	      ;; Extract Lines:
	      (goto-char (point-min))
	      (if (search-forward "\nLines: " nil t)
		  (setq lines (string-to-int
			       (buffer-substring
				(point)
				(save-excursion (end-of-line) (point)))))
		(setq lines 0))
	      ;; Extract Xref:
	      (goto-char (point-min))
	      (if (search-forward "\nXref: " nil t)
		  (setq xref (buffer-substring
			      (point)
			      (save-excursion (end-of-line) (point))))
		(setq xref nil))
	      ;; Extract References:
	      (goto-char (point-min))
	      (if (search-forward "\nReferences: " nil t)
		  (setq references (buffer-substring
				    (point)
				    (save-excursion (end-of-line) (point))))
		(setq references nil))
	      (setq headers
		    (cons (vector article subject from
				  xref lines date
				  message-id references) headers))
	      ))
	(setq sequence (cdr sequence))
	(setq count (1+ count))
	(and (numberp nntp-large-newsgroup)
	     (> number nntp-large-newsgroup)
	     (zerop (% count 20))
	     (message "NNSPOOL: %d%% of headers received."
		      (/ (* count 100) number)))
	)
      (and (numberp nntp-large-newsgroup)
	   (> number nntp-large-newsgroup)
	   (message "NNSPOOL: 100%% of headers received."))
      (nreverse headers))))


;;;
;;; Replacement of NNTP Raw Interface.
;;;

(defun nnspool-open-server (host &optional service)
  "Open news server on HOST.
If HOST is nil, use value of environment variable `NNTPSERVER'.
If optional argument SERVICE is non-nil, open by the service name."
  (let ((host (or host (getenv "NNTPSERVER")))
	(status nil))
    (setq nntp-status-string "")
    (cond ((and (file-directory-p nnspool-spool-directory)
		(file-exists-p nnspool-active-file)
		(string-equal host (system-name)))
	   (setq status (nnspool-open-server-internal host service)))
	  ((string-equal host (system-name))
	   (setq nntp-status-string
		 (format "%s has no news spool.  Goodbye." host)))
	  ((null host)
	   (setq nntp-status-string "NNTP server is not specified."))
	  (t
	   (setq nntp-status-string
		 (format "NNSPOOL: cannot talk to %s." host)))
	  )
    status
    ))

(defun nnspool-close-server ()
  "Close news server."
  (nnspool-close-server-internal))

(fset 'nnspool-request-quit (symbol-function 'nnspool-close-server))

(defun nnspool-server-opened ()
  "Return server process status, T or NIL.
If the stream is opened, return T, otherwise return NIL."
  (and nntp-server-buffer
       (get-buffer nntp-server-buffer)))

(defun nnspool-status-message ()
  "Return server status response as string."
  nntp-status-string
  )

(defun nnspool-request-article (id)
  "Select article by message ID (or number)."
  (let ((file (if (stringp id)
		  (nnspool-find-article-by-message-id id)
		(concat nnspool-current-directory (prin1-to-string id)))))
    (if (and (stringp file)
	     (file-exists-p file)
	     (not (file-directory-p file)))
	(save-excursion
	  (nnspool-find-file file)))
    ))

(defun nnspool-request-body (id)
  "Select article body by message ID (or number)."
  (if (nnspool-request-article id)
      (save-excursion
	(set-buffer nntp-server-buffer)
	(goto-char (point-min))
	(if (search-forward "\n\n" nil t)
	    (delete-region (point-min) (point)))
	t
	)
    ))

(defun nnspool-request-head (id)
  "Select article head by message ID (or number)."
  (if (nnspool-request-article id)
      (save-excursion
	(set-buffer nntp-server-buffer)
	(goto-char (point-min))
	(if (search-forward "\n\n" nil t)
	    (delete-region (1- (point)) (point-max)))
	t
	)
    ))

(defun nnspool-request-stat (id)
  "Select article by message ID (or number)."
  (error "NNSPOOL: STAT is not implemented."))

(defun nnspool-request-group (group)
  "Select news GROUP."
  (let ((pathname (nnspool-article-pathname
		   (nnspool-replace-chars-in-string group ?. ?/))))
    (if (file-directory-p pathname)
	(setq nnspool-current-directory pathname))
    ))

(defun nnspool-request-list ()
  "List valid newsgoups."
  (save-excursion
    (nnspool-find-file nnspool-active-file)))

(defun nnspool-request-last ()
  "Set current article pointer to the previous article
in the current news group."
  (error "NNSPOOL: LAST is not implemented."))

(defun nnspool-request-next ()
  "Advance current article pointer."
  (error "NNSPOOL: NEXT is not implemented."))

(defun nnspool-request-post ()
  "Post a new news in current buffer."
  (save-excursion
    ;; We have to work in the server buffer because of NEmacs hack.
    (copy-to-buffer nntp-server-buffer (point-min) (point-max))
    (set-buffer nntp-server-buffer)
    (apply 'call-process-region
	   (point-min) (point-max)
	   nnspool-inews-program 'delete t nil nnspool-inews-switches)
    (prog1
	(or (zerop (buffer-size))
	    ;; If inews returns strings, it must be error message 
	    ;;  unless SPOOLNEWS is defined.  
	    ;; This condition is very weak, but there is no good rule 
	    ;;  identifying errors when SPOOLNEWS is defined.  
	    ;; Suggested by ohm@kaba.junet.
	    (string-match "spooled" (buffer-string)))
      ;; Make status message by unfolding lines.
      (subst-char-in-region (point-min) (point-max) ?\n ?\\ 'noundo)
      (setq nntp-status-string (buffer-string))
      (erase-buffer))
    ))


;;;
;;; Replacement of Low-Level Interface to NNTP Server.
;;; 

(defun nnspool-open-server-internal (host &optional service)
  "Open connection to news server on HOST by SERVICE (default is nntp)."
  (save-excursion
    (if (not (string-equal host (system-name)))
	(error "NNSPOOL: cannot talk to %s." host))
    ;; Initialize communication buffer.
    (setq nntp-server-buffer (get-buffer-create " *nntpd*"))
    (set-buffer nntp-server-buffer)
    (buffer-flush-undo (current-buffer))
    (erase-buffer)
    (kill-all-local-variables)
    (setq case-fold-search t)		;Should ignore case.
    (setq nntp-server-process nil)
    (setq nntp-server-name host)
    ;; It is possible to change kanji-fileio-code in this hook.
    (run-hooks 'nntp-server-hook)
    t
    ))

(defun nnspool-close-server-internal ()
  "Close connection to news server."
  (if (get-file-buffer nnspool-history-file)
      (kill-buffer (get-file-buffer nnspool-history-file)))
  (if nntp-server-buffer
      (kill-buffer nntp-server-buffer))
  (setq nntp-server-buffer nil)
  (setq nntp-server-process nil))

;; This uses a fast dbm lookup, instead of groveling through the
;; history file itself.
(defun nnspool-find-article-by-message-id (id)
  "Return full pathname of an article identified by message-ID."
    (if (null nnspool-dbm-program)
	(nnspool-slowly-find-article-by-message-id id)
      (save-excursion
	(set-buffer nntp-server-buffer)
	(erase-buffer)
	(insert id)
	(call-process-region (point-min) (point-max)
			     nnspool-dbm-program t t nil
			     nnspool-dbm-args
			     nnspool-history-file)
	(goto-char (point-max))
	; Note that this is based on the C-News format history file.
	; I don't think (but dunno) if B-Bews used the same format.
	(if (re-search-backward "[ \t]\\([a-z.]+/[0-9]+$\\)" nil t)
	    (concat (file-name-as-directory nnspool-spool-directory)
		     (nnspool-replace-chars-in-string
		      (buffer-substring (match-beginning 1) (match-end 1)) ?. ?/))))))

;; Original version				
(defun nnspool-slowly-find-article-by-message-id (id)
  "Slowly return full pathname of an artilce identified by message-ID."
  (save-excursion
    (let ((buffer (get-file-buffer nnspool-history-file)))
      (if buffer
	  (set-buffer buffer)
	;; Finding history file may take lots of time.
	(message "Reading history file...")
	(set-buffer (find-file-noselect nnspool-history-file))
	(message "Reading history file... done")))
    ;; Search from end of the file. I think this is much faster than
    ;; do from the beginning of the file.
    (goto-char (point-max))
    (if (re-search-backward
	 (concat "^" (regexp-quote id)
		 "[ \t].*[ \t]\\([^ \t/]+\\)/\\([0-9]+\\)[ \t]*$") nil t)
	(let ((group (buffer-substring (match-beginning 1) (match-end 1)))
	      (number (buffer-substring (match-beginning 2) (match-end 2))))
	  (concat (nnspool-article-pathname
		   (nnspool-replace-chars-in-string group ?. ?/))
		  number))
      )))

(defun nnspool-find-file (file)
  "Insert FILE in server buffer safely."
  (set-buffer nntp-server-buffer)
  (erase-buffer)
  (condition-case ()
      (progn (insert-file-contents file) t)
    (file-error nil)
    ))

(defun nnspool-article-pathname (group)
  "Make pathname for GROUP."
  (concat (file-name-as-directory nnspool-spool-directory) group "/"))

(defun nnspool-replace-chars-in-string (string from to)
  "Replace characters in STRING from FROM to TO."
  (let ((string (copy-sequence string))
	(len (length string)))
    (while (> len 0)
      (setq len (1- len))
      (if (= (aref string len) from)
	  (aset string len to)))
    string))
