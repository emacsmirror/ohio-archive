;;;;;;;;;;;;;;;;
;;; An NNTP module for GNUS.

;; $Id: nntp.el,v 1.10 1993/02/04 18:23:39 flee Exp $
(defvar nntp/rcs-revision "$Revision: 1.10 $")

(provide 'nntp)
(require 'backquote)
(require 'chat)
(or (fboundp 'open-network-stream) (require 'tcp))

(or (fboundp 'last)
    (defun last (x)
      "Returns the last link in the list LIST."
      (while (cdr x)
	(setq x (cdr x)))
      x))

(defvar nntp/default-nntp-port "nntp"
  "The default tcp port to use for nntp connections.")

;;;;;;;;;;;;;;;;
;;; NNTP state.

;; Right now, we're assuming we only talk to one NNTP server at a
;; time.  It might be nice to do multiple NNTP connections, but
;; there's no point in doing this from the bottom up.

;; (To handle multiple connections, you need to create connection
;; handles that you pass around.  Ideally, nnspool et al would be
;; just different types of connection handles.)

(defvar nntp/connection nil
  "The current NNTP connection.")

(defvar nntp/error-message nil
  "The error message from the last NNTP command.  'nil if no error.")

(defvar nntp/can-xover t
  "Does this server understand the XOVER command?")

;;;;;;;;;;;;;;;;
;;; The GNUS interface.

;; These are the symbols that GNUS knows about and expects.

;; The interaction between GNUS and nntp.el (or nnspool.el) is a
;; little messy and not particularly well defined.

(defvar nntp-version
  (concat "flee/nntp " (substring nntp/rcs-revision 10)))

(defvar nntp-server-buffer nil
  "Buffer that GNUS looks at when it wants data.")

(defun nntp-open-server (host service)
  "Start a connection to the given HOST and SERVICE.  Returns true
if successful."
  ;; XXX already open?
  (or service (setq service nntp/default-nntp-port))
  (setq nntp/error-message nil)
  (setq nntp/can-xover t)
  (setq nntp-server-buffer (generate-new-buffer "*nntp*"))
  (buffer-flush-undo nntp-server-buffer)
  (setq nntp/connection
	(open-network-stream "nntp" nntp-server-buffer host service))
  (set-process-sentinel nntp/connection 'nntp/sentinel)
  (process-kill-without-query nntp/connection)
  (let ( (code (nntp/response)) )
    (or (eq code 200) (eq code 201))))

(defun nntp-server-opened ()
  "Are we currently connected?"
  (and nntp/connection
       (memq (process-status nntp/connection) '(run open))))
;; XXX should we add stopped to this list?

(defun nntp-close-server ()
  "Terminate the connection.  Returns nothing."
  (let ( (proc nntp/connection)
	 (buffer nntp-server-buffer) )
    (setq nntp/connection nil)
    (setq nntp-server-buffer nil)
    (and proc (delete-process proc))
    (and buffer (kill-buffer buffer))))

(defun nntp-status-message ()
  "Returns the error message from the last NNTP request."
  (or nntp/error-message "okay"))

(defun nntp-request-list ()
  "Retrieve the list of newsgroups into 'nntp-server-buffer.
Returns true if successful."
  (nntp/command "LIST")
  (if (eq (nntp/response) 215)
      ;; We don't do any text format conversion here.  It's wasted
      ;; effort, since the text needs to be parsed by GNUS anyway.
      (nntp/wait-for-text)))

(defun nntp-request-group (group)
  "Select group GROUP.  Returns true if successful."
  (nntp/command "GROUP" group)
  (eq (nntp/response) 211))

(defun nntp-request-article (id)
  "Retrieve article ID (either a number or a message-id) into
'nntp-server-buffer.  Returns true if successful."
  (nntp/command "ARTICLE" id)
  (if (eq (nntp/response) 220)
      (nntp/get-text)))

(defun nntp-request-post ()
  "Modify and post the current buffer.  Returns true if successful."
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
	      (process-send-string nntp/connection "\r\n.\r\n")
	      nil)))))

(defun nntp-retrieve-headers (sequence)
  "Returns the header data for SEQUENCE in the current group.
SEQUENCE is a sorted list of article numbers.
XXX describe the return value."
  (and sequence
       (let ( (result nil) )
	 (message "retrieving...")
	 (if nntp/can-xover
	     (setq result (nntp/try-xover sequence)))
	 (if (not nntp/can-xover)
	     (setq result (nntp/headers sequence)))
	 (message "retrieving...done")
	 result)))

;;;;;;;;;;;;;;;;
;;; Talking to the NNTP server.

(defun nntp/sentinel (proc delta)
  (or (nntp-server-opened)
      (error "NNTP connection closed.")))

(defun nntp/clear ()
  ;; XXX This resynchronization is imperfect, but is probably good
  ;; enough for normal use.
  (chat/delete-pending-data nntp/connection))

(defun nntp/command (&rest strings)
  "Start a new NNTP command."
  (nntp/clear)
  (process-send-string
   nntp/connection
   (concat (mapconcat 'identity strings " ") "\r\n")))

;;;;;;;;;;;;;;;;
;;; Reading from the NNTP server.

(defun nntp/response ()
  "Wait for an NNTP response and return the response code.  Also sets
'nntp/error-message."
  ;; XXX Emacs 18.xx has a bug that turns undo back on after a gc, so
  ;; we continually flush undo here.
  (buffer-flush-undo nntp-server-buffer)
  (chat/with-data-until-string "\n" nntp/connection
    (let ( (code (string-to-int (buffer-substring (point-min)
						  (+ (point-min) 3)))) )
      ;; Codes 400 and up are error conditions.
      (setq nntp/error-message
	    (and (<= 400 code)
		 (buffer-substring (+ (point-min) 4) (- (point-max) 2))))
      code)))

(defun nntp/wait-for-text ()
  "Wait for an NNTP text response.  Returns true."
  (chat/wait-for-dot-crlf nntp/connection))

(defun nntp/get-text ()
  "Wait for an NNTP text response and convert it to Unix text format.
Returns true."
  (nntp/wait-for-text)
  (save-excursion
    (set-buffer nntp-server-buffer)
    (nntp/smtp-to-unix-text))
  t)

;;;;;;;;;;;;;;;;
;;; Handling the funny dot-CRLF text format used by SMTP/NNTP.

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
    (insert ?\r)
    (forward-line))
  ;; Add the terminator, but first insert a CRLF if necessary.
  (or (bobp)
      (eq (preceding-char) ?\n)
      (insert "\r\n"))
  (insert ".\r\n"))

;;;;;;;;;;;;;;;;
;;; Fetch headers using XOVER.

;; XXX We could probably try splitting a sequence into segments and
;; sending multiple XOVER commands, one for each segment.  However,
;; this is a little more expensive for the news server to process, and
;; mostly just reduces network traffic.  There isn't much difference
;; in response, unless you're in the habit of leaving 100+ article
;; gaps.  A couple hundred extra overview lines are unnoticeable on a
;; Sun SLC.

;; XXX In general, maybe we should have a gap threshhold: if a gap is
;; larger than N, split it into two XOVER requests, but the actual
;; tradeoffs are more complex than that.  This is really a flaw in
;; XOVER; you should be able to give XOVER a monotonically increasing
;; sequence of ranges, which is something that can be processed
;; efficiently.

;; XXX There's an unhappy synchronization problem here with C News.
;; The bounds in the active file are updated before the overview data
;; is updated, which may not happen until minutes later.  If you read
;; the active file and enter a newsgroup soon after it receives new
;; articles, then the overview fetch will leave out the new articles.
;; GNUS will wrongly conclude that the articles don't exist, mark them
;; as read, and you'll never see them.

(defun nntp/try-xover (sequence)
  "Try using the XOVER command to retrieve headers."
  (let ( (lo (car sequence))
	 (hi (car (last sequence))) )
    (nntp/command "XOVER" (concat (int-to-string lo) "-" (int-to-string hi)))
    (if (eq (nntp/response) 224)
	(chat/with-data-until-dot-crlf nntp/connection
	  (nov/parse sequence))
      (setq nntp/can-xover nil)
      nil)))

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
     (buffer-substring (point) (progn (skip-chars-forward "0-9") (point)))))

(defun nov/parse (sequence)
  "Parse the news overview data in the current buffer, and return a
list of headers that match SEQUENCE (see 'nntp-retrieve-headers)."
  (let ( (number nil)
	 (header nil)
	 (headers nil)
	 (eol nil) )
    (goto-char (point-min))
    (while (and sequence (not (eobp)))
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
	    ;; overview: [num subject from date id refs lines chars misc]
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
      (forward-line)
      )
    (setq headers (nreverse headers))
    headers))

;;;;;;;;;;;;;;;;
;;; A workaround for missing Xrefs in the overview data.

(defun nntp/add-to-hook (hook-name value)
  (let ((hook nil))
    (if (boundp hook-name)
	(setq hook (symbol-value hook-name)))
    (if (or (subrp hook)
	    (and hook (symbolp hook))
	    (and (listp hook) (eq (car hook) 'lambda)))
	(setq hook (list hook)))
    (or (memq value hook)
	(setq hook (cons value hook)))
    (set hook-name hook)))

(nntp/add-to-hook
 'gnus-Article-prepare-hook
 'nntp/article-get-xrefs)

(defvar gnus-current-headers nil)	; from gnus.el

(defun nntp/article-get-xrefs ()
  "Fill in the Xref value in 'gnus-current-headers, if necessary.
This is meant to be called in 'gnus-Article-prepare-hook."
  (or (aref gnus-current-headers 3)
      (let ( (case-fold-search nil) )
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
  (message "request...")
  (nntp/send-head-requests sequence)
  (message "parsing...")
  (nntp/parse-headers sequence))

(defun nntp/send-head-requests (sequence)
  (while sequence
    (accept-process-output)
    (process-send-string
     nntp/connection
     (concat "HEAD " (car sequence) "\r\n"))
    (setq sequence (cdr sequence))
    ))

(defun nntp/parse-headers (sequence)
  (let ( (headers nil)
	 (code nil)
	 (number nil) )
    (while sequence
      (chat/with-data-until-string "\n" nntp/connection
	(setq code (string-to-int (buffer-substring (point-min)
						    (+ (point-min) 4)))))
      (if (eq code 221)
	  (chat/with-data-until-dot-crlf nntp/connection
	    (setq headers (cons (nntp/parse-header (car sequence)) headers)))
	(forward-line))
      (setq sequence (cdr sequence)))
    (nreverse headers)))

(defun nntp/header-value ()
  (goto-char (match-end 0))
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

(defun nntp/parse-header (number)
  (let ( (header (make-vector 8 nil))
	 (case-fold-search t)
	 (fields nil) )
    (aset header 0 number)
    (aset header 4 0)
    (while (not (eobp))
      ;; header: [num subject from xref lines date id refs]
      (if (not (looking-at "subject:\\|from:\\|xref:\\|lines:\\|date:\\|message-id:\\|references:"))
	  (forward-line)
	(cond
	 ((eq (downcase (following-char)) ?s)
	  (aset header 1 (nntp/header-value)))
	 ((eq (downcase (following-char)) ?f)
	  (aset header 2 (nntp/header-value)))
	 ((eq (downcase (following-char)) ?x)
	  (aset header 3 (nntp/header-value)))
	 ((eq (downcase (following-char)) ?l)
	  (aset header 4 (string-to-int (nntp/header-value))))
	 ((eq (downcase (following-char)) ?d)
	  (aset header 5 (nntp/header-value)))
	 ((eq (downcase (following-char)) ?m)
	  (aset header 6 (nntp/header-value)))
	 ((eq (downcase (following-char)) ?r)
	  (aset header 7 (nntp/header-value)))
	 ))
      )
    header))
