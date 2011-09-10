;;; $Id: chat.el,v 1.21 1994/03/21 02:56:28 flee Exp $

;;; chat - Handling input from asynchronous processes.

(provide 'chat)
(require 'backquote)

;;;; Overview.

;;; This is essentially an input buffering module.  Generally, you
;;; don't get data from processes in chunks of the right size: you may
;;; have to wait for the rest of a line or record, or you may get more
;;; data than you need right now.

;;; `chat' divides the data received from a process into two sections:
;;; accepted data and not-yet-accepted (unaccepted) data.  Unaccepted
;;; data can't be looked at.  Accepted data can be examined (and
;;; modified) with the special form `chat/with-data-of'.

;;; Data becomes accepted by calling one of the `chat/accept-xxx' or
;;; `chat/await-xxx' routines.  The `await-' routines will block
;;; until they've accepted what you've asked.  The `accept-' routines
;;; accept whatever data they can without blocking, and return status.

;;; XXX need an example.

;;; XXX there probably should be a form that waits with a timeout.
;;; Implementing this for emacs 18 would be painful...


;;;; Implementation notes.

;;; For this implementation, accepted data is kept in a separate
;;; buffer.  I'd really like to use a single buffer, but I can't find
;;; a way of keeping a stable mark between accepted and unaccepted
;;; data.  The problem is Emacs inserts new process output using
;;; insert-before-marks, which is the wrong behavior for this case.

;;; A different way of doing this would be to set a process filter
;;; that would do the right thing, but then you incur the cost of
;;; creating string copies of all the process output.

;;; Another idea is to use text properties to mark accepted and
;;; unaccepted data with different attributes.  This only works with
;;; Emacs 19, and behaves bizarrely if you let the caller modify the
;;; accepted data.

;;; chat handles are just process handles.  There's no particular
;;; reason to create a special chat handle, and there are a couple
;;; reasons for not doing so: two chat handles that refer to the same
;;; process handle will interfere with each other unpredictably.


(make-variable-buffer-local
 (defvar chat/data-buffer nil
   "The accepted-data buffer for this buffer.  Buffer-local."))

(defmacro chat/with-process-buffer (proc &rest forms)
  "Temporarily sets the current buffer to PROC's buffer, and evaluates
FORMS.  The value of the last FORM is returned."
  (` (save-excursion
       (set-buffer (process-buffer (, proc)))
       (,@ forms))))
(put 'chat/with-process-buffer 'lisp-indent-hook 1)

(defmacro chat/with-data-of (proc &rest forms)
  "Temporarily sets the current buffer to PROC's accepted-data buffer
and evaluates FORMS.  The value of the last FORM is returned.  You can
modify the accepted data any way you want."
  (` (chat/with-process-buffer (, proc)
       (set-buffer chat/data-buffer)
       (,@ forms))))
(put 'chat/with-data-of 'lisp-indent-hook 1)

(defun chat/initialize (proc)
  "Prepare PROC for chatting."
  (chat/with-process-buffer proc
    (or chat/data-buffer
	(setq chat/data-buffer
	      (get-buffer-create (concat (buffer-name) "-data")))
	)))

(defun chat/finalize (proc)
  "Unprepare PROC for chatting."
  (chat/with-process-buffer proc
    (if chat/data-buffer
	(kill-buffer chat/data-buffer))
    (setq chat/data-buffer nil)))


;;;; Accept functions.

;;; XXX need to add comments on efficiency.

;; XXX ugh. there's a real conceptual problem here with narrowing.
;; There's no real distinction between a narrowed and an unnarrowed
;; buffer.  If the current narrowing includes the end of the buffer,
;; there's no way to decide whether the inserted data belongs inside
;; or outside the narrowed region.
(defun chat/mark-as-accepted (proc n)
  "For PROC, marks the next N chars as accepted.  Returns non-nil."
  ;; XXX The process buffer better not be narrowed.  Dealing with the
  ;; data buffer being narrowed is bad enough.
  (chat/with-data-of proc
    (if (and (eq (point-min) 1)
	     (eq (1+ (buffer-size)) (point-max)))
	(save-excursion
	  (goto-char (point-max))
	  (insert-buffer-substring (process-buffer proc) 1 n))
      ;; Note that save-restriction won't work here.
      (let ( (eob (point-max-marker)) )
	(unwind-protect
	    (save-excursion
	      (widen)
	      (goto-char (point-max))
	      (insert-buffer-substring (process-buffer proc) 1 n))
	  (narrow-to-region (point-min) eob)
	  (set-marker eob nil)))))
  (chat/with-process-buffer proc
    (delete-region 1 n))
  proc)

'(defun chat/accept-all (proc)
   "For PROC, accept all data that's currently available.  Returns nil
if no data was accepted."
   (accept-process-output)
   (chat/with-process-buffer proc
     (if (< (point-min) (point-max))
	 (chat/mark-as-accepted proc (- (point-max) (point-min))))))

'(defun chat/unaccept-string (proc s)
   "For PROC, push STRING at the front of the unaccepted data."
   (chat/with-process-buffer proc
     (goto-char (point-min))
     (insert s)))

(defun chat/accept-length (proc n)
  "For PROC, accept exactly N chars, or else return nil."
  (accept-process-output)
  (chat/with-process-buffer proc
    (if (<= (+ (point-min) n) (point-max))
	(chat/mark-as-accepted proc (+ (point-min) n)))))

(defun chat/accept-to-string (proc string)
  "For PROC, accept data up to and including STRING.  Returns non-nil
if STRING was accepted.  May accept partial data that does not contain
STRING.  Modifies the match data."
  (accept-process-output)
  (chat/with-process-buffer proc
    (goto-char (point-min))
    (if (search-forward string nil t)
	(chat/mark-as-accepted proc (match-end 0))
      (goto-char (- (point-max) (length string)))
      (chat/mark-as-accepted proc (point))
      nil)))

(defun chat/accept-to-regexp (proc regexp)
  "For PROC, accept data up to and including REGEXP.  Returns non-nil
if REGEXP was accepted.  May accept partial data that does not contain
REGEXP.  Modifies the match data."
  (accept-process-output)
  (chat/with-process-buffer proc
    (goto-char (point-min))
    (if (re-search-forward regexp nil t)
	  (chat/mark-as-accepted proc (match-end 0))
      ;; We can't accept any data, because we don't know anything
      ;; about what the regexp will not match.
      nil)))

(defun chat/accept-to-dot-crlf (proc)
  "Equivalent to `(chat/accept-to-regexp \"^\\\\.\\r?\\n\")', but
somewhat faster."
  (accept-process-output)
  (chat/with-process-buffer proc
    (goto-char (point-min))
    ;; Guaranteed to be at the beginning of a line here.
    (if (and (eq (following-char) ?.) (looking-at "\\.\r?\n"))
	(chat/mark-as-accepted proc (match-end 0))
      (while (and (search-forward "\n." nil 'eob)
		  (not (looking-at "\r?\n")))
	;; We might be at eob, so using forward-char would signal eob
	(goto-char (1+ (point))))
      (if (not (eobp))
	  (chat/mark-as-accepted proc (match-end 0))
	;; accept as much data as we can.  we can't accept a partial
	;; line because we might split it right before a trailing dot,
	;; which would cause a false positive the next time around.
	(beginning-of-line)
	(chat/mark-as-accepted proc (point))
	nil))))


;;;; Waiting routines.

;;; This is the error that's signalled when you wait on a dead process.
(put 'no-process 'error-conditions '(error no-process))
(put 'no-process 'error-message "Connection is broken")

(defsubst chat/await-process (proc)
  "Wait for output from PROC.  If PROC is dead, signal 'no-process."
  (or (and (memq (process-status proc) '(open stop run))
	      (accept-process-output proc))
	 (signal 'no-process proc)))

(defun chat/await-length (proc n)
  "For PROC, wait for and accept N characters."
  (while (not (chat/accept-length proc n))
    (chat/await-process proc)))

(defun chat/await-string (proc string)
  "For PROC, wait for and accept data until STRING."
  (while (not (chat/accept-to-string proc string))
    (chat/await-process proc)))

(defun chat/await-regexp (proc regexp)
  "For PROC, wait for and accept data until REGEXP."
  (while (not (chat/accept-to-regexp proc regexp))
    (chat/await-process proc)))

(defun chat/await-dot-crlf (proc)
  "For PROC, wait for and accept data until \"^\\\\.\\r\\n\"."
  (while (not (chat/accept-to-dot-crlf proc))
    (chat/await-process proc)))

;;;;;;;;;;;;;;;;
;;; Processing the data.

(defun chat/delete-accepted-data (proc)
  "Delete accepted data from PROC.  Returns nothing."
  (chat/with-data-of proc
    (delete-region (point-min) (point-max))))

(defun chat/delete-unaccepted-data (proc)
  "Delete unaccepted data from PROC, without blocking.  Does not
affect accepted data.  Returns nothing."
  (chat/with-process-buffer proc
    (widen)
    (while (< (point-min) (point-max))
      (delete-region (point-min) (point-max))
      (accept-process-output))))

(defun chat/delete-all-data (proc)
  "Delete all data associated with PROC.  Returns nothing."
  (chat/delete-accepted-data proc)
  (chat/delete-unaccepted-data proc))
