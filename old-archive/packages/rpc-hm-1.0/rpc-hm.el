;Author: Eyvind Ness (eyvind) 
;Date:   Thursday, May 7 1992 19:48 GMT
;File:   /usr/local/gnu/emacs/elisp/site-extensions/rpc-hm.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This is an Emacs Lisp package providing an interface to an RPC-like
;;; communication protocol called rpc-hm ("Home-made RPC").
;;; Both RPC/UDP and IP/TCP based communication transports are provided.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;     Copyright (C) 1991, 1992 Eyvind Ness.
;;;
;;;     Permission to use, copy, modify, and distribute this software and its
;;;     documentation for non-commercial purposes and without fee is hereby
;;;     granted, provided that the above copyright notice appear in all copies
;;;     and that both the copyright notice and this permission notice appear in
;;;     supporting documentation. OECD Halden Reactor Project makes no
;;;     representations about the suitability of this software for any purpose.
;;;     It is provided "as is" without express or implied warranty.
;;;
;;;     OECD Halden Reactor Project disclaims all warranties with regard to this
;;;     software, including all implied warranties of merchantability and
;;;     fitness, and in no event shall OECD Halden Reactor Project be liable for
;;;     any special, indirect or consequential damages or any damages whatsoever
;;;     resulting from loss of use, data or profits, whether in an action of
;;;     contract, negligence or other tortious action, arising out of or in
;;;     connection with the use or performance of this software.
;;;
;;;
;;;     Eyvind Ness
;;;     Research Scientist
;;;     Control Room Systems Division
;;;     OECD Halden Reactor Project
;;;     Norway
;;;
;;;     Internet Email: eyvind@hrp.no
;;;     Voice: +47 9 183100
;;;     Fax: +47 9 187109
;;;     Surface mail: P.O. Box 173, N-1751 Halden, Norway

(require 'ascii-filter)
(provide 'rpc-hm)
(defconst rpc-hm-version "1.0")

(defvar rpc-hm-host-db
  '((remulus 
     (type . :lispm)
     (current-medium-ix . 0)
     (rpc-hm-support :ip-tcp :rpc-udp))
    (alfa
     (type . :lispm)
     (current-medium-ix . 0)
     (rpc-hm-support :ip-tcp)))
  "A database of hostnames and the protocols they support")


(defvar rpc-hm-default-host-entry
  '(default
     (type :unix)
     (current-medium-ix . 0)
     (rpc-hm-support :rpc-udp))
  "Default entry for hosts not in rpc-hm-host-db")


(defvar rpc-hm-major-modes-with-auto-mode-line-update
  '(lispm-mode common-lisp-mode)
  "List of mode names (symbols) that may have their mode-line-string
altered by functions in this library.")


(defvar rpc-hm-startup-message-displayed-p nil)
(defconst rpc-hm-startup-message-lines
    '("Mail bug reports to bug-rpc-hm@hrp.no"
      "RPC HM comes with ABSOLUTELY NO WARRANTY"
      "Type \\[describe-no-warranty] for full details"))


(defun rpc-hm-display-startup-message ()
  ;; Stolen from Kyle Jones' VM package
  (let ((lines rpc-hm-startup-message-lines))
    (message "RPC HM version %s, Copyright (C) 1992 Eyvind Ness"
	     rpc-hm-version)
    (while (and (sit-for 3) lines)
      (message (substitute-command-keys (car lines)))
      (setq lines (cdr lines)))
    (or lines (setq rpc-hm-startup-message-displayed-p t)))
  (message ""))


(defvar rpc-hm-current-host-db-ix 0)
(make-variable-buffer-local 'rpc-hm-current-host-db-ix)


(defun rpc-hm-get-host-att (host att)
  (or (symbolp host)
      (if (not (stringp host))
	  (error "Illegal arg - HOST should be string or symbol")
	(setq host (intern host))))
  (let ((host-ent (or (assq host rpc-hm-host-db)
		      rpc-hm-default-host-entry)))
    (cdr (assq att host-ent))))


(defun rpc-hm-set-host-att (host att newval)
  (or (symbolp host)
      (if (not (stringp host))
	  (error "Illegal arg - HOST should be string or symbol")
	(setq host (intern host))))
  (let ((host-ent (or (assq host rpc-hm-host-db)
		      rpc-hm-default-host-entry)))
    (let ((place (assq att host-ent)))
      (and place (setcdr place newval)))))


(defun rpc-hm-get-current-host ()
  (car (elt rpc-hm-host-db rpc-hm-current-host-db-ix)))


(defun rpc-hm-set-current-host (newhost)
  (let ((pos (memq newhost (mapcar (function car) rpc-hm-host-db))))
    (if pos
	(setq rpc-hm-current-host-db-ix
	      (- (length rpc-hm-host-db)
		 (length pos))))
    (prog1 (rpc-hm-get-current-host)
      (rpc-hm-update-mode-line-if-convenient))))
      

(defun rpc-hm-next-host (arg)
  "Picks a new default host from rpc-hm-host-db. Alternatively, with a
non-nil prefix arg a new default transport medium is chosen for the
current host."
  (interactive "P")
  (if arg (prog1 (rpc-hm-get-current-host)
	    (rpc-hm-next-medium (rpc-hm-get-current-host)))
    (prog2
     (setq rpc-hm-current-host-db-ix
	   (mod (1+ rpc-hm-current-host-db-ix)
		(length rpc-hm-host-db)))
     (rpc-hm-get-current-host)
     (rpc-hm-update-mode-line-if-convenient))))


(defun rpc-hm-next-medium (host)
  "Picks a new default transport medium from the rpc-hm-support list of
HOST in rpc-hm-host-db."
  (interactive)
  (let ((medium-list (rpc-hm-get-host-att host 'rpc-hm-support))
	(mix (rpc-hm-get-host-att host 'current-medium-ix)))
    (prog1
	(elt medium-list
	     (rpc-hm-set-host-att
	      host 'current-medium-ix
	      (mod (1+ mix) (length medium-list))))
      (rpc-hm-update-mode-line-if-convenient))))

    
(defun rpc-hm-update-mode-line-if-convenient ()
  (if (memq major-mode rpc-hm-major-modes-with-auto-mode-line-update)
      (progn
	;; NB! this surgery presupposes:
	(make-local-variable 'global-mode-string)
	(make-local-variable 'rpc-hm-mode-line-string)
	;; to limit the effect of altering the global-mode-string to the
	;; current buffer.
	(setq global-mode-string
	      (list "" 'rpc-hm-mode-line-string))
	(setq rpc-hm-mode-line-string
	      (concat
	       "Lispm@"
	       (prin1-to-string (rpc-hm-get-current-host))
	       " over "
	       (prin1-to-string
		(elt
		 (rpc-hm-get-host-att
		  (rpc-hm-get-current-host) 'rpc-hm-support)
		 (rpc-hm-get-host-att
		  (rpc-hm-get-current-host) 'current-medium-ix)))))
	(save-excursion (set-buffer (other-buffer)))
	(set-buffer-modified-p (buffer-modified-p))
	(sit-for 0))))


;;; Some handy condition type definitions. Use these to trap common
;;; errors singaled by this software.
(put 'rpc-hm-network-condition
     'error-conditions '(rpc-hm-network-condition error))
(put 'rpc-hm-network-condition
     'error-message "Networking problem")

(put 'rpc-hm-network-connection-error
     'error-conditions
     '(rpc-hm-network-connection-error rpc-hm-network-condition error))
(put 'rpc-hm-network-connection-error
     'error-message "Connection failure")

(put 'rpc-hm-network-server-condition
     'error-conditions
     '(rpc-hm-network-server-condition rpc-hm-network-condition error))
(put 'rpc-hm-network-server-condition 
     'error-message "Remote lisp server barfed")

;;; Obsolete condition:
;;; (put 'rpc-hm-illegal-reader-macro
;;;      'error-conditions
;;;      '(rpc-hm-illegal-reader-macro rpc-hm-network-condition error))
;;; (put 'rpc-hm-illegal-reader-macro
;;;      'error-message "Remote reader macro unknown")
;;;
;;; Unused (as of version 1.0) condition:
;;; (put 'rpc-hm-network-client-condition
;;;      'error-conditions 
;;;      '(rpc-hm-network-client-condition rpc-hm-network-condition error))
;;; (put 'rpc-hm-network-client-condition
;;;      'error-message
;;;      ;; server protocol violation or condition escaped client trap
;;;      "Client error")


(defun rpc-hm-read-from-string (str)
  (car (read-from-string str)))


;;; Need a CL conformant syntax table to make elisp parsing work - see
;;; lisp-mode-variables () in lisp-mode.el:
(defvar rpc-hm-lisp-mode-syntax-table
  (progn
    (setq rpc-hm-lisp-mode-syntax-table
	  (copy-syntax-table emacs-lisp-mode-syntax-table))
    (let* ((const "!$%&*+-./0123456789:<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz[]^_{}~")
	   (i 0))
      (while (< i (length const))
	(modify-syntax-entry (aref const i) "w" rpc-hm-lisp-mode-syntax-table)
	(setq i (1+ i))))
    (modify-syntax-entry ?\| "\"   "
			 rpc-hm-lisp-mode-syntax-table)
    (modify-syntax-entry ?\# "'   "
			 rpc-hm-lisp-mode-syntax-table)
    (modify-syntax-entry ?\[ "_   "
			 rpc-hm-lisp-mode-syntax-table)
    (modify-syntax-entry ?\] "_   "
			 rpc-hm-lisp-mode-syntax-table)
    rpc-hm-lisp-mode-syntax-table))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tricky test cases for rpc-hm-forward-cl-sexp:
;;;
;;; non CL conforming Explorer pathname:
;;; (T #FS::LISPM-PATHNAME "REMULUS: EYVIND.LIB; CLISP.XLD#>" "" "" "" "" "" "")
;;; CLOS test:
;;; (defclass man->robot-> () ())
;;;
;;; (T #<Package USER (really COMMON-LISP-USER) 55711255>
;;;  "Package COMMON-LISP-USER.  Nicknames: CL-USER.
;;;   979 symbols out of 1631.  Hash modulus=2039.  48% full.
;;; New symbols added via SI:PKG-NEW-SYMBOL.  Colon prefix looks up symbols via SI:INTERN-EXTERNAL-ONLY.
;;; Uses package SYMBOLICS-COMMON-LISP.
;;; Relative names inherited from SYMBOLICS-COMMON-LISP:
;;;     USER                #<Package USER (really COMMON-LISP-USER) 55711255>
;;;     ZL                  #<Package ZL (really GLOBAL) 55217550>
;;; #<Package USER (really COMMON-LISP-USER) 55711255> is implemented as an ART-Q type array.
;;; It uses %ARRAY-DISPATCH-LEADER; it is 4,078 elements long, with a leader of length 16.
;;; "
;;;  "" "" "" "" "")
;;;
;;; ...this is getting out of hand - i'll change the protocol to always
;;; return a string value - using the new macro
;;; enlib:with-errors-and-io-trapped instead.
;;;
;;;
;;; (defun rpc-hm-forward-cl-sexp (&optional arg)
;;;   "Move forward across one balanced Common Lisp expression.
;;; With argument, do this that many times.
;;; See CLtL2, pp530- for the specification of #-reader macros.
;;;
;;; Warning! This function depends on the reply format of the underlying
;;; rpc-hm protocol and the use of enlib:with-errors-and-io-trapped on the
;;; server side - see rpc-hm-parse-ans. It is not a good idea to use
;;; this function for general purposes."
;;; ;; This anomaly exists due to the difficulty of parsing #<...> replies
;;; ;; correctly, see below.
;;;
;;;   (interactive "p")
;;;   (or arg (setq arg 1))
;;;   (let ((case-fold-search t)
;;;         (omd (match-data))
;;;         ;; (stab (syntax-table))
;;;         (signaller
;;;          (function (lambda ()
;;;            (signal 'rpc-hm-illegal-reader-macro
;;;                    (list
;;;                     (buffer-substring
;;;                      (point) (min (point-max) (+ 10 (point))))))))))
;;;     (unwind-protect
;;;          ;; (set-syntax-table
;;;          ;;   (copy-syntax-table rpc-hm-lisp-mode-syntax-table))
;;;          (cond ((looking-at "#<")
;;;                 ;; The #<...> special case.
;;;                 (if (re-search-forward ">\\s \"" (point-max) t)
;;;                     ;; Note: #<...> depends on being followed by a
;;;                     ;; whitespace character and a \".
;;;                     (forward-char -2)
;;;                     (signal 'rpc-hm-illegal-reader-macro
;;;                             (list
;;;                              (format
;;;                               "\"#<\"-%s \">\" returned from server."
;;;                               "reader macro without a matching")))))
;;;                ((looking-at "#\\\\\\(\\S \\)+")
;;;                 ;; a CL character object:
;;;                 ;; hash, backslash, N non-whitespace chars
;;;                 (goto-char (match-end 0)))
;;;                ((looking-at "#\\\\\\(\\s \\)")
;;;                 ;; hash, backslash, whitespace: illegal
;;;                 (funcall signaller))
;;;                ((looking-at "#\\\\\\(\n\\)")
;;;                 ;; same, but LFD isn't whitespace in lisp-mode
;;;                 (funcall signaller))
;;;                ((looking-at "#\C-p[^\C-q]*\C-q")
;;;                 ;; CL abusing Explorer pathname syntax
;;;                 (goto-char (match-end 0)))
;;;                ((looking-at "#\\([0-9]\\)*\\(\\S \\)")
;;;                 ;; any other, legal, dispatching macro:
;;;                 ;; hash, optional non-negative number, followed by a
;;;                 ;; non-whitespace character. BTW: LFD is not
;;;                 ;; whitespace in lisp mode, see (describe-syntax) =>
;;;                 ;; "LFD                >       which means: endcomment"
;;;                 (let ((disp-char
;;;                        (string-to-char
;;;                         (buffer-substring
;;;                          (match-beginning 2) (match-end 2)))))
;;;                   ;; first, clear out the LFD possibility:
;;;                   (if (= disp-char ?\n) (funcall signaller))
;;;                   (goto-char
;;;                    (if (= disp-char ?\()
;;;                        ;; backup over any "(" so that forward-sexp
;;;                        ;; will work:
;;;                        (1- (match-end 0))
;;;                        (match-end 0)))
;;;                   (forward-sexp arg)))
;;;                ((looking-at "#")
;;;                 ;; any other reader macro syntax is bogus so trap it
;;;                 ;; before we get a non-informative error message from
;;;                 ;; Emacs.
;;;                 (funcall signaller))
;;;                (t
;;;                 ;; by default, use GNU Emacs' idea of an sexp
;;;                 ;; according to the rpc-hm-lisp-mode-syntax-table
;;;                 ;; defined above.
;;;                 (forward-sexp arg)))
;;;       ;; (set-syntax-table stab)
;;;       (store-match-data omd))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rpc-hm-parse-ans (&optional ans)
  "Parses up an answer ANS from the remote server on the assumption that
  `(documentation 'enlib:with-errors-and-io-trapped)' conforms to:
       =>
    During the execution of FORMS, errors are trapped, the standard CL
    IO-streams, *standard-input*, *query-io*, *debug-io*, and *terminal-io*,
    are lexically rebound to *dev-null*, a stream that immediately supplies
    EOF on input and discards all output, while output to any of the streams
    *standard-output*, *error-output*, and *trace-output* is trapped and
    returned as a component of the return value produced by the execution of
    this macro.

    This macro uses a customized version of the CLtL2 macro
    `with-standard-io-syntax' to set up default, sensible values for
    printing control, so that the reader may be able to re-read the printed
    output. The standard version cannot be used directly here, since in this
    case wee need a more flexible scheme to allow e.g. #<...> objects to be
    transferred back to the client, which should have the ultimate control
    of what to do with the object.
    See with-standard-io-syntax-almost-as-in-cltl2.

    The return value is always a list of 3 values:

      The first value is T if the evaluation of FORMS did not signal an
    error condition, NIL otherwise.

      The second value is the actual value produced by FORMS within an
    implicit PROGN, wrapped into a string with `prin1-to-string' to simplify
    parsing at the client end of a communication channel. If an error was
    signaled during the execution of FORMS, an error message is returned.
    This message is also wrapped by `prin1-to-string' to make unwrapping
    independent of the actual outcome.

      The third value returned is a string containing the output to any of
    the standard CLtL2 output streams, *standard-output*, *error-output*,
    and *trace-output*, if any, produced by FORMS.

    Note that if the evaluation of FORMS returns multiple values, only the
    first value is preserved by this macro.

    Use MULTIPLE-VALUE-LIST or MULTIPLE-VALUE-BIND if you need to retrieve
    multiple-values produced by FORMS.

 See also rpc-hm-reparse-ans ()."
  
  (let (eval-ok-p val stdout)
    (save-excursion
      (set-buffer (get-buffer-create " *rpc-hm-reply-parse-buf*"))
      (make-local-variable 'debug-on-error) (setq debug-on-error t)
      (if ans (progn (erase-buffer) (insert ans)))
      (set-syntax-table rpc-hm-lisp-mode-syntax-table)
      (goto-char (point-min))
      (condition-case c
	  (progn
	    (down-list 1)
	    (setq eval-ok-p (buffer-substring (point) (progn (forward-sexp 1) (point))))
	    (skip-chars-forward " \t\n")
	    (setq val (buffer-substring (point) (progn (forward-sexp 1) (point))))
	    (skip-chars-forward " \t\n")
	    (setq stdout (buffer-substring (point) (progn (forward-sexp 1) (point))))
	    (list eval-ok-p (rpc-hm-read-from-string val) stdout))
	(error
;;; If anything goes wrong at this point, it means that the server
;;; returned something unexpected - perhaps there was an error condition
;;; that couldn't be catched by the innermost
;;; `with-errors-and-io-trapped'. This is the case with errors that are
;;; catched by the server during the _parsing_ of incoming requests.
;;;
;;; Try e.g. (sys:com-show-herald) on a Symbolics Lispm, and you get:
;;; 
;;; "Exporting #:COM-SHOW-HERALD from package SYSTEM would cause name conflict in SYSTEM-INTERNALS.
;;; While reading from #<STACK-LEXICAL-CLOSURE CLI::STRING-INPUT-STREAM 144160012>"
;;;
;;; Actually, this is a server protocol violation, but handle it
;;; gracefully all the same, by raising a synthetic rpc-hm-network-server-condition.
	 (let ((screwy-server-feedback
		(buffer-substring (point-min) (point-max))))
	   (if (zerop (length screwy-server-feedback))
	       (setq screwy-server-feedback "Client got no reply from server"))
	   (list "NIL" screwy-server-feedback "")))))))


(defun rpc-hm-reparse-ans ()
  "Reparses an answer from the remote server. See rpc-hm-parse-ans ().
Useful for extracting detailed info on remote eval outcome."
  (rpc-hm-parse-ans))


(defvar rpc-hm-over-tcp-state
  nil
  "A debugging aid to tell the current state of `rpc-hm-over-tcp-process'")

(defvar rpc-hm-over-tcp-replystr
  ""
  "Raw reply string from remote server host")

(defvar rpc-hm-over-tcp-process nil
  "The RPC HM client TCP process")

(defun rpc-hm-over-tcp-sentinel (proc str)
  (ignore proc)
  (setq rpc-hm-over-tcp-state
	(cons (list proc str) rpc-hm-over-tcp-state)))
    
(defun rpc-hm-over-tcp-filter (proc str)
  (ignore proc)
  ;; HOSTNAME, a string, is supposed to have a dynamic binding when this
  ;; filter is activated, but otherwise use the default current host
  ;; instead of just crashing:
  (let ((host-id
	 (or (and (boundp 'hostname) hostname)
	     (rpc-hm-get-current-host))))
    (setq rpc-hm-over-tcp-replystr
	  (concat rpc-hm-over-tcp-replystr
		  (ascii-filter
		   str ':input
		   (rpc-hm-get-host-att host-id 'type))))))

      

(defun rpc-hm-over-tcp (hostname form &optional invoke-elisp-reader-p)
  "Function to call from other elisp programs. Makes an RPC-HM protocol
request over ip/tcp to HOSTNAME, asking it to evaluate FORM. A non-nil
optional argument INVOKE-ELISP-READER-P invokes the Emacs Lisp Reader on
the results returned, but note that the primitive elisp reader cannot
handle normal Common Lisp reader macros beginning with `#'. Also note
that the Emacs Lisp Reader is case-sensitive.

On normal exit it returns the results of the evaluation to the caller as
an elisp object or a string, depending on the value of
INVOKE-ELISP-READER-P.

If the evaluation fails, a `rpc-hm-network-server-condition' is signalled with
the corresponding RPC-HM server error message as data to
signal-handlers.

If the underlying ip/tcp software fails to establish a connection to
HOSTNAME, a `rpc-hm-network-connection-error' is signalled.

All types of error conditions explicitly signalled here can be trapped
by providing a handler for the condition `rpc-hm-network-condition'.
See `condition-case'"
  
  (or (stringp hostname)
      (if (symbolp hostname)
	  (setq hostname (symbol-name hostname))
	  ;; (signal 'wrong-type-argument (list 'symbolp hostname))
	  (error "1st arg, HOSTNAME (%s), is not a symbol or a string"
		 hostname)))
  (or (stringp form) (setq form (prin1-to-string form)))
  (setq rpc-hm-over-tcp-process 
	(condition-case c
	    (open-network-stream 
	     "RPC HM Client over TCP" 
	     nil
	     hostname
	     "rpc-hm")
	  (error (signal 'rpc-hm-network-connection-error (cdr c)))))
;;; /etc/services entry should look similar to this:
;;; rpc-hm 10801/tcp       # <comment string>
;;; or you may `play the whale' and just grab a port number and use that
;;; instead of "rpc-hm" above, but then you shouldn't aspire to be the
;;; most popular person among the rest of the networking programmers at
;;; your site.
  
  (let ((ret-vals nil))
    (setq rpc-hm-over-tcp-state nil)
    (setq rpc-hm-over-tcp-replystr "")
    (set-process-sentinel 
     rpc-hm-over-tcp-process 
     (function rpc-hm-over-tcp-sentinel))
    (set-process-filter
     rpc-hm-over-tcp-process
     (function rpc-hm-over-tcp-filter))
    
;;; From comint.el:
;;;   (defun bridge-send-string (process string)
;;;     "Send PROCESS the contents of STRING as input.
;;;   This is equivalent to process-send-string, except that long input strings
;;;   are broken up into chunks of size comint-input-chunk-size. Processes
;;;   are given a chance to output between chunks. This can help prevent processes
;;;   from hanging when you send them long inputs on some OS's."
;;;     (let* ((len (length string))
;;;            (i (min len bridge-chunk-size)))
;;;       (process-send-string process (substring string 0 i))
;;;       (while (< i len)
;;;         (let ((next-i (+ i bridge-chunk-size))) ;512
;;;           (accept-process-output)
;;;           (process-send-string process (substring string i (min len next-i)))
;;;           (setq i next-i)))))
;;;
;;; We cannot deliver string args longer than the maximum OS pty buffer
;;; size:
    (process-send-string
     rpc-hm-over-tcp-process
     (ascii-filter
      (prin1-to-string
       (format "(enlib:with-errors-and-io-trapped %s)" form))
      ':output (rpc-hm-get-host-att hostname 'type)))

;;; This is a minor problem with the RPC-HM protocol since the server side
;;; is doing a READ-FROM-STRING and hangs either until the input is
;;; complete, or times out. Symptom: Server side barfs "EOF encountered on
;;; string-input-stream".
;;;
;;; Workaround: Avoid huge arguments. Use filesystem files as a means for
;;; exchanging large amounts of data between the client and server sides.
;;; See src/process.c in the GNU Emacs distribution.
    
    (or noninteractive
	(message "Waiting for %s to respond..." hostname))
    (setq rpc-hm-over-tcp-state
	  (cons (list 'before (process-status rpc-hm-over-tcp-process))
		rpc-hm-over-tcp-state))
    (while (memq (process-status rpc-hm-over-tcp-process) '(open run))
      ;; - network connections should always return either 'open or
      ;; 'closed, but this doesn't seem to be true for Emacs versions
      ;; prior to 18.58.
      ;; - this test takes advantage of the fact that the RPC-HM
      ;; protocol specifies that the server is supposed to close down
      ;; the connection after the reply has been computed.
      (accept-process-output rpc-hm-over-tcp-process))
    (setq rpc-hm-over-tcp-state
	  (cons (list 'after (process-status rpc-hm-over-tcp-process))
		rpc-hm-over-tcp-state))
    (or noninteractive
	(message "Waiting for %s to respond...OK" hostname))
    
    (let (ans)
      (setq 
       ans
       (condition-case c
	   (rpc-hm-read-from-string rpc-hm-over-tcp-replystr)
	 (error
	  (format
	   "(NIL \"\\\"%s - see `rpc-hm-over-tcp-replystr'\\\"\" \"\")"
	   "Client encountered an unreadable server reply-format"))))
;;; Perhaps I should introduce yet another condition type, e.g.
;;; `rpc-hm-network-client-condition' instead, but I think I'll leave
;;; that to a later version. Right now I doubt that the application
;;; programmer really needs to distinguish these error conditions, let
;;; alone treat them differently.
      (setq ret-vals (rpc-hm-parse-ans ans))
      (let ((case-fold-search t))
	(if (string-match "^T$" (elt ret-vals 0))
	    ;; Everything is fine:
	    (if invoke-elisp-reader-p
		(rpc-hm-read-from-string (elt ret-vals 1))
		(elt ret-vals 1))
	    ;; Else the server barfed about something:
	    (signal 'rpc-hm-network-server-condition
		    ;; Return the error message as data for handlers:
		    (list (rpc-hm-read-from-string (elt ret-vals 1)))))))))

 

(defvar rpc-hm-client-program
  "rpc-hm-client-program"
  "*The Unix RPC/UDP based client program that relays your requests to
the RPC-HM server host using the LISP_SERVICE RPC protocol. Should be
found somewhere along your exec-path.")

(defun rpc-hm-over-rpc-udp (hostname form &optional invoke-elisp-reader-p)
  "Function to call from other elisp programs. Makes an RPC-HM protocol
request over RPC/UDP to HOST, asking it to evaluate FORM. A non-nil
optional argument INVOKE-ELISP-READER-P invokes the Emacs Lisp Reader on
the results returned, but note that the primitive elisp reader cannot
handle normal Common Lisp reader macros beginning with `#'. Also note
that the Emacs Lisp Reader is case-sensitive.

On normal exit it returns the results of the evaluation to the caller as
an elisp object or a string, depending on the value of
INVOKE-ELISP-READER-P.

If the evaluation fails, a `rpc-hm-network-server-condition' is signalled with
the corresponding RPC-HM server error message as data to
signal-handlers.

If the the underlying RPC/UDP software fails to establish a connection to
HOST, a `rpc-hm-network-connection-error' is signalled.

All types of error conditions explicitly signalled here can be trapped
by providing a handler for the condition `rpc-hm-network-condition'.
See `condition-case'"
  
  (or (stringp hostname)
      (if (symbolp hostname)
	  (setq hostname (symbol-name hostname))
	  ;; (signal 'wrong-type-argument (list 'symbolp hostname))
	  (error "1st arg, HOSTNAME (%s), is not a symbol or a string"
		 hostname)))
  (or (stringp form) (setq form (prin1-to-string form)))
  
  (let ((ret-vals nil) buf-mark-1 buf-mark-2
	(client-error-p nil))
    (save-excursion
      (set-buffer (get-buffer-create " *rpc-hm-unix-rpc-udp-buf*"))
      (make-local-variable 'debug-on-error) (setq debug-on-error t)
      (erase-buffer)
      (goto-char (point-min))
      (setq buf-mark-1 (point))
      (insert
       (format "(enlib:with-errors-and-io-trapped %s)" form))
      (setq buf-mark-2 (point))
      (condition-case c
	  (call-process-region
	   buf-mark-1
	   buf-mark-2
	   rpc-hm-client-program
	   nil				;don't delete src input
	   t				;output to current buffer
	   nil				;don't redisplay buffer during
					;output 
	   hostname			;prog arg: name of RPC server
					;host
	   )
	(error (setq client-error-p t)
	       (setq ret-vals (cdr c))))
      (save-excursion			;remove "\nEOF\n":
	(forward-word -1)		;go back 1 word
	(if (looking-at "^EOF\n")
	    (progn (delete-char -1 nil) (delete-char 4 nil))
	    ;; Else something went seriously wrong.
	    (setq client-error-p t)))
      
      (or ret-vals
	  (setq ret-vals
		(buffer-substring buf-mark-2 (point)))))
    
    (if client-error-p
	(signal 'rpc-hm-network-connection-error (list ret-vals)))
    
    (setq ret-vals (rpc-hm-parse-ans ret-vals))
    (let ((case-fold-search t))
      (if (string-match "^T$" (elt ret-vals 0))
	  ;; Everything is fine:
	  (if invoke-elisp-reader-p
	      (rpc-hm-read-from-string (elt ret-vals 1))
	      (elt ret-vals 1))
	  ;; Else the server barfed about something:
	  (signal 'rpc-hm-network-server-condition
		  ;; Return the error message as data for handlers:
		  (list (rpc-hm-read-from-string (elt ret-vals 1))))))))


(defun rpc-hm-internal (host form invoke-elisp-reader-p
			     transport-protocol)
  "Direct an RPC-HM protocol request to HOST asking it to evalute FORM.
HOST should be a symbol that names a computer that is hosting an RPC-HM
server, or it could be the keyword :any, indicating that any such host
will do, in which case the rpc-hm-host-db is used to pick one.

Third arg INVOKE-ELISP-READER-P if non-nil invokes the Emacs Lisp
reader on the returned result. Fourth arg TRANSPORT-PROTOCOL is either
the name of a transport medium, such as :ip-tcp or :rpc-udp, or the
keyword :any, indicating that any trasport medium will do.

See the documentation for `rpc-hm-over-tcp' and `rpc-hm-over-rpc-udp'
for a more detailed description of the underlying functions."
  
  (cond ((eq host ':any)
	 (rpc-hm-internal-any-host
	  form invoke-elisp-reader-p transport-protocol))
	((eq transport-protocol ':any)
	 (rpc-hm-internal-any-medium
	  host form invoke-elisp-reader-p))
	(t
	 (cond ((eq transport-protocol ':ip-tcp)
		(rpc-hm-over-tcp host form invoke-elisp-reader-p))
	       ((eq transport-protocol ':rpc-udp)
		(rpc-hm-over-rpc-udp host form invoke-elisp-reader-p))
	       (t (error "No such transport %s." transport-protocol))))))


(defun rpc-hm-internal-any-host (form invoke-elisp-reader-p transport-protocol)
  (catch 'rpc-hm-success
    (let ((hosts-tried nil)
	  (host (rpc-hm-get-current-host)))
      (while (not (memq host hosts-tried))
	(condition-case c
	    (throw 'rpc-hm-success
		   (rpc-hm-internal host form invoke-elisp-reader-p
				    transport-protocol))
	  (rpc-hm-network-connection-error
	   (setq hosts-tried (cons host hosts-tried))
	   (setq host (rpc-hm-next-host)))))
      (signal
       'rpc-hm-network-connection-error
       (list
	(format
	 "All server hosts in rpc-hm-host-db (%s) tried, %s"
	 (mapconcat (function identity)
		    (mapcar (function (lambda (el) (symbol-name el)))
			    hosts-tried)
		    ", ")
	 "but none responded to a connection request."))))))
      
    

(defun rpc-hm-internal-any-medium (host form invoke-elisp-reader-p)
  (catch 'rpc-hm-success
    (let ((media-tried nil)
	  (medium (elt (rpc-hm-get-host-att host 'rpc-hm-support)
		       (rpc-hm-get-host-att host 'current-medium-ix))))
      (while (not (memq medium media-tried))
	(condition-case c
	    (throw 'rpc-hm-success
		   (rpc-hm-internal
		    host form invoke-elisp-reader-p medium))
	  (rpc-hm-network-connection-error
	   (setq media-tried (cons medium media-tried))
	   (setq medium (rpc-hm-next-medium host)))))
      (signal 
       'rpc-hm-network-connection-error
       (list
	(format
	 "All media supported by host %s %s (%s) tried, but none%s"
	 host
	 "listed in rpc-hm-host-db"
	 (mapconcat (function identity)
		    (mapcar (function (lambda (el) (symbol-name el)))
			    media-tried)
		    ", ")
	 " could provide transport for the RPC-HM protocol."))))))
