;To: unix-emacs@bbn.com
;Date: 10 Apr 89 19:46:15 GMT
;From: "David C. Howland" <rit!cci632!dch@cs.rochester.edu>
;Sender: arpa-unix-emacs-request@bbn.com
;Subject: insert time/date and signature
;Reply-To: "David C. Howland" <dch%ccird3.uucp@CHIPS.BBN.COM>
;Organization: CCI, Communications Systems Division, Rochester, NY
;Source-Info:  From (or Sender) name not authenticated.
;
;
;The following contains a history.el package, an autoload for the package
;and hook to enable it.
;
;This package was built on top of the date.el, time-stamp function
;written by John Sturdy (uunet!mcvax!ukc!eagle!icdoc!qmc-cs!harlqn!jcgs)
;
;What I have changed/added is that along with the time stamp and user
;field,  the user is prompted for an optional comment. Each
;change-log-stamp is inserted after "Last Edited:" and the the previous
;entry pushed down in the file (similar to add-change-log-entry). As in 
;
;-----
;Last Edited:
;
;Fri Apr  7 10:27:12 1989 by David C. Howland (dch at ccird3)
;	 And so on and on.
;
;Fri Apr  7 10:12:50 1989 by David C. Howland (dch at ccird3)
;	 Second Change.
;
;Fri Apr  7 10:02:11 1989 by David C. Howland (dch at ccird3)
;	 Original Comment.
;
;-----
;This was written for GNU Emacs. If anyone has constructive comments on
;how to improve this, please mail or post them. If you make changes to
;enhance history.el please mail or post them also.
;
;-------------------------------------------------------------------------------
;Added the following to your .emacs
;
;(autoload 'change-log-stamp "history"
;	 "\
;Update the last edited field in a buffer and add a comment"
;	t)
;
;; Mark time, date and optional comment on all files containing
;; "Last Edited:", as they are saved.
;(setq write-file-hooks '(list change-log-stamp))
;
;----------- cut here and save in history.el -----------------------------------
;;; history.el
;;; Last Edited:
;;; 
;;; Tue Apr  4 17:37:06 1989 by David C. Howland (dch at ccird3)
;;; 	 Added defvar for history-search-limit. Up'd the limit from 3000
;;; 	 characters to 5000
;;; 


(defvar history-min-comment-length 10
  "*The minimum length that a \"change-log-stamp\" comment must be.
If set to 0 there is no minimum.")

(defvar history-buffer "*History*"
  "*The buffer name to get history information from.")

(defvar history-search-limit 5000
  "*The marker string \"Last Edited:\" must occur in the first 5000 characters")

(defvar history-mode-map nil)

(if history-mode-map
    nil
  (setq history-mode-map (make-sparse-keymap))
  (define-key history-mode-map "\C-c?"    'history-help)
  (define-key history-mode-map "\C-c\C-a" 'history-abort)
  (define-key history-mode-map "\C-c\C-c" 'history-exit)
  (define-key history-mode-map "\C-x\C-s" 'history-exit)
)

(defvar last-edited-pattern
  "Last Edited:"
  "*The regexp after which the change-log-stamp is written by the
function \"change-log-stamp\".")

(defvar by-line
  (concat " by " (user-full-name) " (" (user-login-name)
	  (if (boundp 'nick-name)
	      (concat " - " nick-name ")")
	    )
	  " at " (system-name) ")")
  "The user-id and name of the user, and the name of the host machine,
in a form suitable for change-log-stamping.  If the user has defined the
variable \"nick-name\" before loading \"date\", it is included after
the real name, separated from it by a dash.")


(defun change-log-stamp ()
  "Update the \"Last edited:\" field in a buffer. Can be used on
\"write-file-hooks\" for automatic updating. The change-log-stamp
includes the date and time, the user's user-id and real name, a
nick-name if the variable \"nick-name\" was already defined when the
\"history\" package was loaded and an optional comment.
The \"Last Edited:\" marker string must occur in the first 5000
characters (defvar \"history-search-limit\") of the buffer. Uses
the characters from the beginning of the line that \"Last Edited\" 
was on, up-to the 'L', if any, as the comment characters to precede
any text that is inserted."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if
        (re-search-forward last-edited-pattern history-search-limit t)
        (let
            ((start-of-date (point)))

	  (save-excursion
	    (beginning-of-line)
	    (setq here (point))
	    (re-search-forward last-edited-pattern)
	    (backward-word 2)
	    (setq comments (buffer-substring here (point)))
	    )
	  
	  (setq prefix-line (concat comments "	 "))
	  (setq ThisBuffer (buffer-name))
	  (save-window-excursion
	    (setq insert-text-here (point)) ; save place where text is to go.
	    (pop-to-buffer history-buffer t)
	    (erase-buffer)
	    (set-buffer-modified-p nil)
	    (or (eq major-mode 'indented-text-mode)
		(progn
		  (indented-text-mode)
		  (setq left-margin 8)
		  (setq fill-column 72)
		  (setq fill-prefix prefix-line)))
	    (auto-fill-mode 1)
	    (indent-to left-margin)
	    (use-local-map history-mode-map)
	    (message 
	     "Enter log message. Type C-c C-c when done, C-c ? for help.")
	    (recursive-edit)
	    (kill-buffer history-buffer))
	  )
      )
    )
  (setq junk nil)			; cause defun to return nil
  )

(defun history-exit ()
  "Leave the recursive edit of an history log message."
  (interactive)
  (if (> (buffer-size) history-min-comment-length)
      (progn
	(switch-to-buffer history-buffer)
	(goto-char (point-min))
	(delete-char 1)
	(switch-to-buffer ThisBuffer)
	(goto-char insert-text-here)	; move to where text is to go
	(insert "\n" comments "\n")
	(insert comments (current-time-string))
	(insert by-line)
	(insert "\n" prefix-line)
	(insert-buffer history-buffer)
	(exit-recursive-edit))
    (progn
      (error
       "Log must be greater than %d characters!"
       history-min-comment-length))
    )
  )

(defun history-abort ()
  "Abort the recursive edit of an history log message."
  (interactive)
  (exit-recursive-edit)
  )

(defun history-help()
  "Describes \"change-log-stamp\"  key bindings. See \"change-log-stamp\" function.
Related variables: history-min-comment-length, history-buffer and 
                   history-search-limit.

The following commands are available:
\\{history-mode-map}
"
  
  (interactive)
  (describe-function 'history-help)
)

