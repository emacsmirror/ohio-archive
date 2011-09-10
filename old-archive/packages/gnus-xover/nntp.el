;;;;;;;;;;;;;;;;
;;; An NNTP module for GNUS.

;;; $Id: nntp.el,v 1.40 1994/05/16 06:28:47 flee Exp $
(defconst nntp/rcs-revision "$Revision: 1.40 $")

(provide 'nntp)
(require 'backquote)
(require 'chat)

(or (fboundp 'open-network-stream)
    (require 'tcp))

(or (fboundp 'buffer-disable-undo)
    (fset 'buffer-disable-undo 'buffer-flush-undo))

;;;;;;;;;;;;;;;;
;;; Global parameters.

(defconst nntp/default-nntp-port "nntp"
  "The default tcp port to use for nntp connections.")

(defvar nntp/authinfo-user nil
  "*The username to give to the server, with AUTHINFO USER.")

(defvar nntp/authinfo-pass nil
  "*The password to give to the server, with AUTHINFO PASS.")

(defvar nntp/crlf "\r\n"
  ;; The string that represents CR-LF.  Emacs on OS/2 wants just "\n".
  ;; Any value other than "\n" or "\r\n" will probably do bad things.
)

(defvar flee/debug nil)			; personal debug toggle.

;;;;;;;;;;;;;;;;
;;; NNTP state.

;; Right now, we're assuming we only talk to one NNTP server at a
;; time.  It might be nice to do multiple NNTP connections, but
;; there's no point in doing this from the bottom up.

;; (To handle multiple connections, you need to create connection
;; handles that you pass around.  Ideally, nnspool et al would be
;; just different types of connection handles.)

(defvar nntp/can-xover t
  "*Does this NNTP server understand the XOVER command?")

(defvar nntp/connection nil
  "The current NNTP connection.")

(defvar nntp/current-host nil
  "The current NNTP host.")

(defvar nntp/current-port nil
  "The current NNTP port.")

(defvar nntp/current-group nil
  "The current group.")

(defvar nntp/article-in-nntp-buffer nil
  "The id of the article (if any) currently in the nntp buffer.")

(defvar nntp/error-message nil
  "The error message from the last NNTP command.  nil if no error.")


;;;;;;;;;;;;;;;;
;;; The GNUS interface.

;; These are the symbols that GNUS knows about and expects.

;; The interaction between GNUS and nntp.el (or nnspool.el) is a
;; little messy and not particularly well defined.

(defconst nntp-version
  (concat "flee/nntp " nntp/rcs-revision))

(defvar nntp-large-newsgroup 40
  "*The number of the articles which indicates a large newsgroup.
If the number of the articles is greater than the value, verbose
messages will be shown to indicate the current status.")

(defvar nntp-server-buffer nil
  "Buffer that GNUS looks at when it wants data.")

(defun nntp-open-server (host port)
  "Start a connection to the given HOST and PORT.  Returns non-nil
if successful."
  ;; XXX already open?
  (or port (setq port nntp/default-nntp-port))
  (setq nntp/current-host host)
  (setq nntp/current-port port)
  (setq nntp/current-group nil)
  (setq nntp/article-in-nntp-buffer nil)
  (setq nntp/error-message nil)
  (setq nntp/can-xover t)
  (save-excursion
    (set-buffer (generate-new-buffer " *nntp*"))
    (setq case-fold-search t)		; buffer-local
    (buffer-disable-undo (current-buffer))
    (setq nntp/connection
	  (open-network-stream "nntp" (current-buffer) host port)))
  (set-process-sentinel nntp/connection 'nntp/sentinel)
  (process-kill-without-query nntp/connection)
  (chat/initialize nntp/connection)
  (chat/with-data-of nntp/connection
    (setq nntp-server-buffer (current-buffer)))
  (let ( (code (nntp/response)) )
    (nntp/command "mode reader")
    (nntp/response)
    (if nntp/authinfo-user
	(progn
	  (nntp/command (concat "AUTHINFO USER " nntp/authinfo-user))
	  (nntp/response)
	  (nntp/command (concat "AUTHINFO PASS " nntp/authinfo-pass))
	  (nntp/response)))
    (or (eq code 200) (eq code 201))))

(defun nntp-server-opened ()
  "Are we currently connected?"
  (and nntp/connection
       (memq (process-status nntp/connection) '(run open stop))))

(defun nntp-close-server ()
  "Terminate the connection.  Returns nothing."
  (if nntp/connection
      (progn
	(chat/finalize nntp/connection)
	(kill-buffer (process-buffer nntp/connection))
	(delete-process nntp/connection)
	(setq nntp/connection nil)))
  (if nntp-server-buffer
      (progn
	(kill-buffer nntp-server-buffer)
	(setq nntp-server-buffer nil))))

(defun nntp-status-message ()
  "Returns the error message from the last NNTP request."
  (or nntp/error-message "okay"))

(defun nntp-request-list (&optional arg)
  "Send a \"list ARG\" request and leave the result in
`nntp-server-buffer'.  Returns non-nil on success."
  (nntp/command "list" (or arg ""))
  (if (eq (nntp/response) 215)
      ;; Since GNUS needs to parse the output anyway, doing full
      ;; text format conversion is wasted effort.
      (nntp/wait-for-text-no-dot-crlf)))

(defun nntp-request-list-newsgroups ()
  (nntp-request-list "newsgroups"))

(defun nntp-request-list-distributions ()
  (nntp-request-list "distributions"))

(defun nntp-request-list-subscriptions ()
  (nntp-request-list "subscriptions"))

(defun nntp-request-group (group)
  "Select group GROUP.  Returns non-nil if successful."
  (if (string= group nntp/current-group)
      t
    (setq nntp/current-group nil)
    (nntp/command "GROUP" group)
    (and (eq (nntp/response) 211)
	 (setq nntp/current-group group))))

(defun nntp-request-article (id)
  "Retrieve article ID (either a number or a message-id) into
`nntp-server-buffer'.  Returns non-nil if successful."
  (if (and nntp/current-group
	   (equal id nntp/article-in-nntp-buffer))
      t
    (nntp/command "ARTICLE" id)
    (if (eq (nntp/response) 220)
	(progn
	  (nntp/get-text)
	  (setq nntp/article-in-nntp-buffer id)
	  ))))

(defun nntp-request-post ()
  "Modify and post the current buffer.  Returns non-nil if successful."
  ;; The trick here is we want to make sure the conversation is in a
  ;; sane state even if we're interrupted in middle of transmission.
  ;; Right now, we just prematurely terminate the posting.  While this
  ;; isn't ideal, it's better than continually adding junk to the end.
  ;; The problem is NNTP doesn't let you abort a posting.
  ;; XXX A better approach is to open a new connection for posting,
  ;; but this is going to be slower, unless you anticipate the user by
  ;; opening the connection early.
  (nntp/command "POST")
  (if (eq (nntp/response) 340)
      (let ( (finished nil) )
	(unwind-protect
	    (progn
	      (nntp/unix-to-smtp-text)
	      (process-send-region nntp/connection (point-min) (point-max))
	      (setq finished t)
	      (eq (nntp/response) 240))
	  (or finished
	      (process-send-string nntp/connection
				   (concat nntp/crlf "." nntp/crlf))
	      nil)))))

(defun nntp-retrieve-headers (sequence)
  "Returns the header data for SEQUENCE in the current group.
SEQUENCE is a sorted list of article numbers.
XXX describe the return value."
  (and sequence
       (not (equal sequence '(0)))
       (let ( (result nil) )
	 (if nntp/can-xover
	     (setq result (nntp/try-xover sequence)))
	 (if (not nntp/can-xover)
		   (setq result (nntp/headers sequence)))
	 (message "retrieving headers...done")
	 result)))

;;;;;;;;;;;;;;;;
;;; Talking to the NNTP server.

(defun nntp/sentinel (proc delta)
  (or (nntp-server-opened)
      (message "(NNTP connection closed)")))

(defun nntp/clear ()
  ;; XXX This resynchronization is imperfect, but is probably good
  ;; enough for normal use.
  (setq nntp/article-in-nntp-buffer nil)
  (chat/delete-all-data nntp/connection))

;;; XXX take nntp/clear out of this?  or create a new function without
;;; nntp/clear?
(defun nntp/command (&rest strings)
  "Start a new NNTP command."
  (or (nntp-server-opened)
      (nntp/reopen-connection))
  (nntp/clear)
  (process-send-string
   nntp/connection
   (concat (mapconcat 'identity strings " ") nntp/crlf)))

(defun nntp/reopen-connection ()
  "Reopen the current NNTP connection."
  (if (not nntp/current-host)
      (error "No NNTP connection.")
    (let ( (group nntp/current-group) )
      (message "Reconnecting to %s..." nntp/current-host)
      (or (nntp-open-server nntp/current-host nntp/current-port)
	  (error "Reconnection failed: %s" nntp/error-message))
      (if group (nntp-request-group group))
      )))

;;;;;;;;;;;;;;;;
;;; Reading from the NNTP server.

(defun nntp/response ()
  "Wait for an NNTP response and return the response code.  Also sets
'nntp/error-message."
  ;; XXX Emacs 18.xx has a bug that turns undo back on after a gc, so
  ;; we continually flush undo here.
  (buffer-disable-undo nntp-server-buffer)
  (chat/await-string nntp/connection "\n")
  (chat/with-data-of nntp/connection
    (let ( (code (string-to-int (buffer-substring (point-min)
						  (+ (point-min) 3)))) )
      ;; Codes 400 and up are error conditions.
      (setq nntp/error-message
	    (and (<= 400 code)
		 (buffer-substring (+ (point-min) 4) (- (point-max) 2))))
      (delete-region (point-min) (point-max))
      code)))

(defun nntp/wait-for-text ()
  "Wait for an NNTP text response.  Returns non-nil."
  (chat/await-dot-crlf nntp/connection)
  nntp/connection)

(defun nntp/wait-for-text-no-dot-crlf ()
  "Wait for an NNTP text response.  Returns non-nil."
  (nntp/wait-for-text)
  (chat/with-data-of nntp/connection
    (goto-char (point-max))
    (forward-line -1)
    (if (looking-at "^\\.\r?\n")
	(delete-region (point) (point-max))))
  t)

(defun nntp/get-text ()
  "Wait for an NNTP text response and convert it to Unix text format.
Returns t."
  (nntp/wait-for-text)
  (chat/with-data-of nntp/connection
    (nntp/smtp-to-unix-text))
  t)

;;;;;;;;;;;;;;;;
;;; Handling the funny dot-CRLF text format used by SMTP/NNTP.

;; XXX these routines need names that aren't Unix-centric.

(defun nntp/smtp-to-unix-text ()
  "Convert the current buffer from SMTP text format to Unix text
format.  Modifies point.  Returns nothing."
  (goto-char (point-min))
  (while (not (eobp))
    (if (eq (following-char) ?.)
	(delete-char 1))
    (end-of-line)
    (if (eq (preceding-char) ?\r)
	(delete-char -1))
    (forward-char))
  ;; Delete the last line, which had the dot-crlf terminator.
  (backward-char)
  (if (eq (preceding-char) ?\n)
      (delete-char 1))
  )

(defun nntp/unix-to-smtp-text ()
  "Convert the current buffer form Unix text format to SMTP text
format.  Modifies point.  Returns nothing."
  (goto-char (point-min))
  (while (not (eobp))
    (if (eq (following-char) ?.)
	(insert ?.))
    (end-of-line)
    (delete-char 1)
    (insert nntp/crlf))
  ;; Add the terminator, but first insert a CRLF if necessary.
  (or (bobp)
      (eq (preceding-char) ?\n)
      (insert nntp/crlf))
  (insert "." nntp/crlf))

;;;;;;;;;;;;;;;;
;;; Fetch headers using XOVER.

;; XXX We could probably try splitting a sequence into segments and
;; sending multiple XOVER commands, one for each segment.  However,
;; this is a little more expensive for the news server to process, and
;; mostly just reduces network traffic.  There isn't much difference
;; in response, unless you're in the habit of leaving 100+ article
;; gaps.  The time spent processing a couple hundred extra overview
;; lines is unnoticeable on a Sun SLC.

;; XXX In general, maybe we should have a gap threshhold: if a gap is
;; larger than N, split it into two XOVER requests, but the actual
;; tradeoffs are more complex than that.  This is really a flaw in
;; XOVER; you should be able to give XOVER a monotonically increasing
;; sequence of ranges, which is something that can be processed
;; efficiently.

(defun nntp/try-xover (sequence)
  "Try using the XOVER command to retrieve headers."
  (let ( (lo (car sequence))
	 (hi (car (nntp/last sequence)))
	 (result nil) )
    (nntp/command "XOVER" (concat (int-to-string lo) "-" (int-to-string hi)))
    (if (not (eq (nntp/response) 224))
	(setq nntp/can-xover nil)
      (message "receiving overview...")
      (chat/await-dot-crlf nntp/connection)
      (chat/with-data-of nntp/connection
	(goto-char (point-min))
	(setq result (nov/parse sequence))
	(delete-region (point-min) (point-max)))
      ;; `nov/parse' returns (headers . unused-sequence-tail).  If the
      ;; unused tail is non-empty, then those articles might actually
      ;; exist, just not in the overview data, because the overview
      ;; data tends to be updated late, especially with C News.  So we
      ;; use the slow HEAD fetch to guarantee we get accurate info
      ;; about those articles.  This can be wasted effort if those
      ;; articles really aren't there, but fetching info on deleted
      ;; articles is pretty cheap.
      (if (cdr result)
	  (progn
	    (and flee/debug
		 (message "Missed by %d!" (length (cdr result)))
		 (sit-for 2))
	    (nconc (car result) (nntp/headers (cdr result))))
	(car result)))
    ))

;;;;;;;;;;;;;;;;
;;; News overview parsing.

;; XXX This section isn't really nntp-specific.  It probably could be
;; a separate module by itself.

;; Small changes to this code can have large impact on performance.

;; You'd think that using skip-chars-forward would be faster than
;; search-forward, but for some reason it ends up marginally slower.
;; I suspect it's because the setup overhead for both is about the
;; same, but the inner loop for search-forward is much more carefully
;; coded.

(defmacro nov/skip-field ()
  '(search-forward "\t" eol 'end))

(defmacro nov/field ()
  '(buffer-substring
    (point)
    (progn (nov/skip-field) (1- (point)))))

(defmacro nov/read-integer ()
   "Read an integer from the current point at the buffer."
   '(string-to-int
     (buffer-substring
      (point)
      (progn (skip-chars-forward "^\t\n") (point)))))

(defun nov/parse (sequence)
  "Parse the news overview data in the current buffer for headers
matching SEQUENCE (see `nntp-retrieve-headers').  Returns the pair
`(headers . sequence)'.  `headers' is the list of headers, `sequence'
is the tail of SEQUENCE that isn't found in `headers'."
  (let ( (number nil)
	 (header nil)
	 (headers nil)
	 (eol nil) )
    (goto-char (point-min))
    (while (and (not (eobp)) sequence)
      (setq number (nov/read-integer))
      (while (and sequence (< (car sequence) number))
	 (setq sequence (cdr sequence)))
      (if (and sequence (eq number (car sequence)))
	  (progn
	    (setq sequence (cdr sequence))
	    (save-excursion
	      (end-of-line)
	      (setq eol (point)))
	    ;; header: [num subject from xref lines date id refs]
	    ;; overview: [num subject from date id refs chars lines misc]
	    (setq header (make-vector 8 nil))
	    (aset header 0 number)
	    (forward-char)	; move past the "\t"
	    (aset header 1 (nov/field))
	    (aset header 2 (nov/field))
	    (aset header 5 (nov/field))
	    (aset header 6 (nov/field))
	    (aset header 7 (nov/field))
	    (nov/skip-field)
	    (aset header 4 (string-to-int (nov/field)))
	    (backward-char)
	    (if (search-forward "\txref: " eol t)
		(aset header 3 (nov/field)))
	    (setq headers (cons header headers))
	    ))
      (forward-line))
    (cons (nreverse headers) sequence)))

;;;;;;;;;;;;;;;;
;;; A workaround for missing Xrefs in the overview data.

(defun nntp/add-hook (hook-name value)
  (let ( (hook nil) )
    (if (boundp hook-name)
	(setq hook (symbol-value hook-name)))
    (if (or (subrp hook)
	    (and hook (symbolp hook))
	    (and (listp hook) (eq (car hook) 'lambda)))
	(setq hook (list hook)))
    (or (memq value hook)
	(setq hook (cons value hook)))
    (set hook-name hook)))

;; This is done in gnus-startup-hook, because we can't reliably decide
;; whether to use gnus-article-* or gnus-Article-* until after GNUS is
;; loaded.
(nntp/add-hook 'gnus-startup-hook 'nntp/add-xref-hook)

(defun nntp/add-xref-hook ()
  (nntp/add-hook
   (if (boundp 'gnus-article-prepare-hook)
       'gnus-article-prepare-hook
     'gnus-Article-prepare-hook)
   'nntp/article-get-xrefs))

(defvar gnus-current-headers nil)	; from gnus.el

(defun nntp/article-get-xrefs ()
  "Fill in the Xref value in `gnus-current-headers', if necessary.
This is meant to be called in `gnus-article-prepare-hook'."
  (or (aref gnus-current-headers 3)
      (let ( (case-fold-search t) )
	(goto-char (point-min))
	(search-forward "\n\n" nil 'end)
	(save-restriction
	  (narrow-to-region (point-min) (point))
	  (goto-char (point-min))
	  (if (or (and (eq (downcase (following-char)) ?x)
		       (looking-at "Xref:"))
		  (search-forward "\nXref:" nil t))
	      (progn
		(goto-char (match-end 0))
		(forward-char)
		(aset gnus-current-headers 3
		      (buffer-substring
		       (point) (progn (end-of-line) (point))))
		))))))

;;;;;;;;;;;;;;;;
;;; Fetch headers using HEAD.

(defun nntp/headers (sequence)
  (nntp/clear)
  (message "requesting headers...")
  (nntp/send-head-requests sequence)
  (message "receiving headers...")
  (nntp/parse-headers sequence))

(defun nntp/send-head-requests (sequence)
  (while sequence
    (accept-process-output)
    (process-send-string
     nntp/connection
     (concat "HEAD " (car sequence) nntp/crlf))
    (setq sequence (cdr sequence))
    ))

(defun nntp/parse-headers (sequence)
  (let ( (headers nil)
	 (code nil)
	 (total (length sequence))
	 (count 0) )
    (while sequence
      (chat/await-string nntp/connection "\n")
      (chat/with-data-of nntp/connection
	(setq code (string-to-int (buffer-substring (point-min)
						    (+ (point-min) 4))))
	(delete-region (point-min) (point-max)))
      (if (eq code 221)
	  (progn
	    (chat/await-dot-crlf nntp/connection)
	    (chat/with-data-of nntp/connection
	      (goto-char (point-min))
	      (setq headers (cons (nntp/parse-header (car sequence)) headers))
	      (delete-region (point-min) (point-max)))))
      (setq sequence (cdr sequence))
      (setq count (1+ count))
      (if (= (% count nntp-large-newsgroup) 0)
	  (message "receiving headers...(%d/%d)" count total))
      )
    (nreverse headers)))

(defun nntp/header-value ()
  (skip-chars-forward "\t ")
  (buffer-substring
   (point)
   (progn
     (while
	 (progn
	   (end-of-line)
	   (if (eq (preceding-char) ?\r)
	       (delete-char -1))
	   (forward-char)
	   (memq (following-char) '(?\t ? )))
       (delete-char -1)
       (delete-char 1)
       (insert ? ))
     (1- (point))))
  )

(defsubst nntp/looking-at-string (s)
  (and (<= (+ (point) (length s)) (point-max))
       (string=
	(downcase (buffer-substring (point) (+ (point) (length s))))
	s)
       (progn
	 (forward-char (length s))
	 t)))

(defun nntp/parse-header (number)
  (let ( (header (make-vector 8 nil))
	 (case-fold-search t)
	 (fields nil) )
    (aset header 0 number)
    (aset header 1 "")
    (aset header 2 "")
    (aset header 4 0)
    (while (not (eobp))
      ;; header: [num subject from xref lines date id refs]
      (cond
       ((and (eq (downcase (following-char)) ?s)
	     (nntp/looking-at-string "subject:"))
	(aset header 1 (nntp/header-value)))
       ((and (eq (downcase (following-char)) ?f)
	     (nntp/looking-at-string "from:"))
	(aset header 2 (nntp/header-value)))
       ((and (eq (downcase (following-char)) ?x)
	     (nntp/looking-at-string "xref:"))
	(aset header 3 (nntp/header-value)))
       ((and (eq (downcase (following-char)) ?l)
	     (nntp/looking-at-string "lines:"))
	(aset header 4 (string-to-int (nntp/header-value))))
       ((and (eq (downcase (following-char)) ?d)
	     (nntp/looking-at-string "date:"))
	(aset header 5 (nntp/header-value)))
       ((and (eq (downcase (following-char)) ?m)
	     (nntp/looking-at-string "message-id:"))
	(aset header 6 (nntp/header-value)))
       ((and (eq (downcase (following-char)) ?r)
	     (nntp/looking-at-string "references:"))
	(aset header 7 (nntp/header-value)))
       (t
	(forward-line 1))
       ))
    header))


(defun nntp/last (x)
  "Returns the last cons in LIST."
  (while (cdr x)
    (setq x (cdr x)))
  x)

