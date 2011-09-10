;; message-buffer-18.el --- log messages in a buffer

;; Author: Ray Nickson <Ray.Nickson@comp.vuw.ac.nz>
;;         Michael Ernst <mernst@lcs.mit.edu>
;; Created: 11 Feb 91
;; Last modified: 3 October 1994

;; Load this file to cause messages to be logged in a buffer called
;; *Messages*.  For instance, you can put the following in your .emacs file:
;;   (defun message-buffer ()
;;     "Log messages in their own buffer."
;;     (interactive)
;;     (require 'message-buffer "message-buffer-18"))
;; and then do  M-x message-buffer RET  to start logging.

;; *****
;; If you have GNU Emacs 19 or XEmacs, don't use this code.
;; Use the latest version, which is called message-buffer.el, instead.
;; *****

;; Originally by Ray Nickson.  Modified by Michael Ernst to put messages at
;; end, not beginning, of *Messages* buffer, and to work when message has
;; been redefined to no longer be a subr.  Also added provide statement.

;; This is not a complete solution, since it just replaces the lisp message
;; and error functions; messages from C code don't get logged.

;; To do:  make boring messages not be logged.

;; LCD Archive Entry:
;; message-buffer-18|Ray Nickson, Michael Ernst|mernst@theory.lcs.mit.edu|
;; Log messages in a buffer|
;; 03-Oct-1994|1.0|~/misc/message-buffer-18.el.Z|


(provide 'message-buffer)


(defvar max-message-buffer-size 10000
  "*Maximum size to let the \"*Messages*\" buffer grow to.  If zero, don't
  write a \"*Messages*\" buffer at all.")

(if (not (fboundp 'message-orig))
    (progn
      (fset 'message-orig (symbol-function 'message))
      (fset 'error-orig (symbol-function 'error))))

(defvar messages-ignore-regexp nil
  "*Regexp of messages which should not be logged.")

(defvar messages-ignored nil
  "*List of messages which should not be logged.")

(setq messages-ignore-regexp "^\\(Failing \\)?I-search\\|^Scanning file ")

(setq messages-ignored
      '("" "Undo!" "Mark set" "Done"
	"Commands: d, s, x; 1, 2, m, u, q; delete; ~;  ? for help."))

(defun message (&rest args)
  "Print a one-line message at the bottom of the screen.
The first argument is a control string.
It may contain %s or %d or %c to print successive following arguments.
%s means print an argument as a string, %d means print as number in decimal,
%c means print a number as a single character.
The argument used by %s must be a string or a symbol;
the argument used by %d or %c must be a number."
  (let ((msg (apply 'format args)))
    (if (not (or (member msg messages-ignored)
		 (and messages-ignore-regexp
		      (string-match messages-ignore-regexp msg))))
	(if (> max-message-buffer-size 0)
	    (save-excursion
	      (set-buffer (get-buffer-create "*Messages*"))
	      (if (< (buffer-size) max-message-buffer-size)
		  nil
		(goto-char (/ max-message-buffer-size 2))
		(beginning-of-line)
		(delete-region (point-min) (point)))
	      (goto-char (point-max))
	      ;; (insert (current-time-string) ": " (apply 'format args) "\n")
	      (insert msg "\n")))))
  (apply 'message-orig args))

(defun error (&rest args)
  "Signal an error, making error message by passing all args to `format'."
  (if (> max-message-buffer-size 0)
	(save-excursion
	  (set-buffer (get-buffer-create "*Messages*"))
	  (if (< (buffer-size) max-message-buffer-size)
	      nil
	    (goto-char (/ max-message-buffer-size 2))
	    (beginning-of-line)
	    (delete-region (point-min) (point)))
	  (goto-char (point-max))
	  ;; (insert (current-time-string) ": " (apply 'format args) "\n")
	  (insert (apply 'format args) "\n")))
  (apply 'error-orig args))

(defun member (x list)
  "Returns non-nil if ELT is an element of LIST.  Comparison done with EQUAL.
The value is actually the tail of LIST whose car is ELT."
  (while (and list (not (equal x (car list))))
    (setq list (cdr list)))
  list)

;;; message-buffer-18.el ends here
