;; Copyright (C) 1989 Free Software Foundation

;; This file is part of GNU Emacs.

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
;;
;; Author Jason R. Glasgow (glasgow@cs.yale.edu)
;; Modified from telnet.el by William F. Schelter
;; But almost entirely different.
;;
;; Modified by Dirk Grunwald to maintain an open connection.
;;
;; 3/18/89 Ashwin Ram <Ram-Ashwin@yale.edu>
;; Added webster-mode.
;; Fixed documentation.
;;
;; 3/20/89 Dirk Grunwald <grunwald@flute.cs.uiuc.edu>
;; Merged Rams changes with new additions: smarter window placement,
;; correctly handles un-exposed webster windows, minor cleanups.
;; Also, ``webster-word'', akin to ``spell-word''.
;;
;; To use this, you might want to add this line to your .emacs file:
;;
;;  (autoload 'webster "webster" "look up a word in Webster's 7th edition" t)
;;
;; Then just hit M-x webster to look up a word.
;;
;; 3/21/89 Dave Sill <dsill@relay.nswc.navy.mil>
;; Removed webster-word and webster-define, adding default of current word to 
;; webster, webster-spell, and webster-endings instead.

(defvar webster-host "128.197.2.40"
  "Host that is a webster server (Boston U). Try also 26.0.0.73, which is sri-nic")
(defvar webster-port "103"
  "The port to connect to. Either 103 or 2627")

(defvar webster-process nil
  "The current webster process")

(defvar webster-process-name "webster"
  "The current webster process")

(defvar webster-buffer nil
  "The current webster process")

(defvar webster-running nil
  "Used to determine when connection is established")

;;;
;;; Initial filter for ignoring information until successfully connected
;;;
(defun webster-initial-filter (proc string)
  (let
      (( this-buffer (current-buffer)))
    (set-buffer webster-buffer)
    (goto-char (point-max))
    (cond
     ((not (eq (process-status webster-process) 'run))
      (progn
	(setq webster-running t)
	(message "Webster died")))
     
     ((string-match "No such host" string)
      (progn
	(setq webster-running t)
	(kill-buffer (process-buffer proc))
	(error "No such host.")))
     
     ((string-match "]" string)
      (progn
	(setq webster-running t)
	(set-process-filter proc 'webster-filter))))
    (set-buffer this-buffer)))

(defun webster-filter (proc string)
  (let
      ((closed-message (string-match "Connection closed" string))
       (end-def-message (or (string-match "\200" string) (string-match "\0" string)))
       ( this-buffer (current-buffer)))

    (set-buffer webster-buffer)
    (cond

     ((not (eq (process-status webster-process) 'run))
      (message "Webster died"))

     (closed-message
	(message "Closing webster connection...")
	(kill-process proc)
	(replace-regexp "Process webster killed" "" nil)
	(goto-char 1)
	(message "Closing webster connection...Done."))
     
     ((string-match "SPELLING 0" string)
      (insert-string "...Word not found in webster\n"))
     
     ((string-match "SPELLING 1" string)
      (insert-string "...Spelled correctly\n"))

     (end-def-message
      (webster-filter proc (concat (substring string 0 (- end-def-message 1)) "\n\n"))
      (goto-char (point-max)))

     (t
      (goto-char (point-max))
      (let ((now (point)))
	(insert string)
	(delete-char-in-region now (point) ?\^m ?\ ))
      (if (process-mark proc)
	  (set-marker (process-mark proc) (point)))))

    ;;
    ;; if webster is visible, move the last line to the bottom of that window
    ;;
    (let ((here (selected-window)))
      (let ((webster-window (get-buffer-window webster-buffer)))
	(if (windowp webster-window)
	  (progn
	    (select-window webster-window)
	     (recenter -1)
	     (select-window here)))))

    (set-buffer this-buffer)))

;;;
;;; delete char1 and char2 if it precedes char1
;;; used to get rid of <space><return>
(defun delete-char-in-region (start end char1 char2)
  (goto-char start)
  (while (search-forward (char-to-string char1) end t)
    (backward-delete-char 1)
    (if (equal (char-after (- (point) 1)) char2)
	(backward-delete-char 1))))

(defun webster (arg)
"Look up a word in the Webster's dictionary.
Open a network login connection to a webster host if necessary.
Communication with host is recorded in a buffer *webster*."
  (interactive (list
		(read-string
		 (concat "Look up word in webster (" (current-word) "): "))))
  (if (equal "" arg) (setq arg (current-word)))
  (webster-send-request "DEFINE" arg))

(defun webster-endings (arg)
"Look up endings for a word in the Webster's dictionary.
Open a network login connection to a webster host if necessary.
Communication with host is recorded in a buffer *webster*."
  (interactive (list
		(read-string
		 (concat
		  "Find endings for word in webster (" (current-word) "): "))))
  (if (equal "" arg) (setq arg (current-word)))
  (webster-send-request "ENDINGS" arg))

(defun webster-spell (arg)
"Look spelling for a word in the Webster's dictionary.
Open a network login connection to a webster host if necessary.
Communication with host is recorded in a buffer *webster*."
  (interactive (list
		(read-string
		 (concat
		  "Try to spell word in webster (" (current-word) "): "))))
  (if (equal "" arg) (setq arg (current-word)))
  (webster-send-request "SPELL" arg))

(defun webster-send-request (kind word)
  (require 'shell)
  (let
      ((webster-command (concat "open " webster-host " " webster-port "\n")))
    
    (if (or 
	 (not webster-buffer)
	 (not webster-process)
	 (not (eq (process-status webster-process) 'run)))
	(progn
	  (message
	   (concat "Attempting to connect to server " webster-host "..."))
	  (setq webster-buffer
		(make-shell webster-process-name "telnet"))
	  (let
	      ((this-buffer (current-buffer)))
	    (set-buffer webster-buffer)
	    (webster-mode)
	    (set-buffer this-buffer))

	  (setq webster-process (get-process webster-process-name))
	  (set-process-filter webster-process 'webster-initial-filter)
	  (process-send-string  webster-process webster-command)
	  (setq webster-running nil);
	  (while (not webster-running)	; wait for feedback
	    (accept-process-output))))	;
    (display-buffer webster-buffer nil)
    (process-send-string webster-process (concat kind " " word "\n"))))

(defun webster-quit ()
   "Close connection and quit webster-mode.
Buffer is not deleted."
   (interactive)
   (message "Closing connection to %s..." webster-host)
   (kill-process webster-process)
   (message "Closing connection to %s...done" webster-host)
   (bury-buffer))

(defun webster-mode ()
  "Major mode for interacting with on-line Webster's dictionary.
\\{webster-mode-map}
Use webster-mode-hook for customization."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'webster-mode)
  (setq mode-name "Webster")
  (use-local-map webster-mode-map)
  (run-hooks 'webster-mode-hook))

(defvar webster-mode-map nil)
(if webster-mode-map
    nil
  (setq webster-mode-map (make-sparse-keymap))
  (define-key webster-mode-map "?" 'describe-mode)
  (define-key webster-mode-map "d" 'webster)
  (define-key webster-mode-map "e" 'webster-endings)
  (define-key webster-mode-map "q" 'webster-quit)
  (define-key webster-mode-map "s" 'webster-spell))

;; Snatched from unix-apropos by Henry Kautz
(defun current-word ()
   "Word cursor is over, as a string."
   (save-excursion
      (let (beg end)
	 (re-search-backward "\\w" nil 2)
	 (re-search-backward "\\b" nil 2)
	 (setq beg (point))
	 (re-search-forward "\\w*\\b" nil 2)
	 (setq end (point))
	 (buffer-substring beg end))))
