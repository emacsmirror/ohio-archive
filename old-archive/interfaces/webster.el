; Received: from dg-rtp by teton (5.4/rtp-s04)
; 	id AA04130; Mon, 21 Oct 1991 01:54:11 -0400
; Received: from lucid.com by dg-rtp.dg.com (5.4/dg-rtp-proto)
; 	id AA12962; Mon, 21 Oct 1991 01:54:02 -0400
; Received: from thalidomide ([192.31.212.116]) by heavens-gate.lucid.com id AA00571g; Sun, 20 Oct 91 22:53:20 PDT
; Received: by thalidomide id AA05286g; Sun, 20 Oct 91 22:54:00 PDT
; Date: Sun, 20 Oct 91 22:54:00 PDT
; X-Windows: Let it get in YOUR way.
; From: Jamie Zawinski <jwz%thalidomide@lucid.com>
; Subject: webster modes
; Content-Type: text
; Content-Length: 13262
; Status: RO
; 
; Hi.  I've made some improvements to webster.el, you might want to archive this
; version.
; 
; LCD Archive Entry:
; webster|Glasgow, Grunwald, Ram, Sill, Zawinski|jwz@lucid.com
; |Look up a word in Webster's 7th Ed.
; |91-09-14||~/interfaces/webster.el.Z|

; 
; ---------- slice 'n' dice ----------------------------------- file: webster.el
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
;;
;; 1/21/91 Jamie Zawinski <jwz@lucid.com>
;; Added webster-reformat to produce better looking output.  Made it notice
;; references to other words in the definitions (all upper-case) and do
;; completion on them in the string read by meta-x webster.
;;
;; 9/14/91 Jamie Zawinski <jwz@lucid.com>
;; Improved the above.

(defvar webster-host "18.26.0.36"
  "The host to use as a webster server.")

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
  (let ((this-buffer (current-buffer)))
    (set-buffer webster-buffer)
    (goto-char (point-max))
    (cond ((not (eq (process-status webster-process) 'run))
	   (setq webster-running t)
	   (message "Webster died"))
	  ((string-match "No such host" string)
	   (setq webster-running t)
	   (kill-buffer (process-buffer proc))
	   (error "No such host."))
	  ((string-match "]" string)
	   (setq webster-running t)
	   (set-process-filter proc 'webster-filter)))
    (set-buffer this-buffer)))

(defvar webster-reformat t
  "*Set this to t if you want the webster output to be prettied up, andfor the \\[webster] prompt to do completion across the set of words knownto be in the dictionary (words you've looked up, or which appeared in definitions as crossreferences.)")

(defun webster-filter (proc string)
  (let ((this-buffer (current-buffer))
	(endp nil))
    (set-buffer webster-buffer)
    (cond ((not (eq (process-status webster-process) 'run))
	   (message "Webster died"))
	  ((string-match "Connection closed" string)
	   (message "Closing webster connection...")
	   (kill-process proc)
	   (replace-regexp "Process webster killed" "" nil)
	   (goto-char 1)
	   (message "Closing webster connection...Done."))
	  ((string-match "SPELLING 0" string)
	   (insert-string "...Word not found in webster\n"))
	  ((string-match "SPELLING 1" string)
	   (insert-string "...Spelled correctly\n"))
	  ((let ((end-def-message (or (string-match "\200" string)
				      (string-match "\0" string))))
	     (if end-def-message
		 (progn
		   (webster-filter
		    proc
		    (concat (substring string 0 (- end-def-message 1)) "\n\n"))
		   (setq endp t)
		   (goto-char (point-max))
		   t))))
	  (t
	   (goto-char (point-max))
	   (let ((now (point)))
	     (insert string)
	     (delete-char-in-region now (point) "\^M" " "))
	   (if (process-mark proc)
	       (set-marker (process-mark proc) (point)))))
    (if endp
	;; if the webster window is visible, move the last line to the
	;; bottom of that window
	(let ((webster-window (get-buffer-window webster-buffer))
	      (window (selected-window)))
	  (if webster-reformat (webster-reformat (process-mark proc)))
	  (if webster-window
	      (progn
		(select-window webster-window)
		(goto-char (point-max))
		(recenter (1- (window-height webster-window)))
		(select-window window)))))))

(defconst webster-completion-table (make-vector 511 0))

(defun webster-intern (string)
  (while (string-match "\\." string)
    (setq string (concat (substring string 0 (match-beginning 0))
			 (substring string (match-end 0)))))
  (intern (downcase string) webster-completion-table))

(defun webster-reformat (end)
  "Clean up the output of the webster server, and gather words for the completion table."
  (if (not webster-reformat) nil
    (goto-char end)
    (let ((case-fold-search nil))
      (re-search-backward "^[A-Z]+" nil t)
      (if (not (looking-at "^DEFINITION [0-9]"))
	  nil
	(forward-line 1)
	(let ((p (point))
	      (indent 2))
	  (search-forward "\n\n" nil 0)
	  (narrow-to-region p (point))
	  (goto-char p)
	  (while (search-forward "\n" nil t)
	    (delete-char -1)
	    (just-one-space))
	  (goto-char p)
	  (while (not (eobp))
	    (cond ((looking-at " *[0-9]+\\. ")
		   (setq indent 5)
		   (delete-horizontal-space)
		   (insert "\n  ")
		   (skip-chars-forward "0-9. ")
		   (if (looking-at "[a-z]+")
		       (webster-intern
			(buffer-substring (point) (match-end 0)))))
		  ((looking-at " *\\([0-9]+\\): *")
		   (let ((n (buffer-substring (match-beginning 1)
					      (match-end 1))))
		     (delete-region (match-beginning 0) (match-end 0))
		     (insert "\n")
		     (indent-to (- 6 (length n)))
		     (insert n " : ")
		     (setq indent 9)))
		  ((looking-at " *\\([0-9]+\\)\\([a-z]+\\): *")
		   (let ((n (buffer-substring (match-beginning 1)
					      (match-end 1)))
			 (m (buffer-substring (match-beginning 2)
					      (match-end 2))))
		     (if (not (equal m "a")) (setq n " "))
		     (delete-region (match-beginning 0) (match-end 0))
		     (insert "\n")
		     (indent-to (- 6 (length n)))
		     (insert n "  ")
		     (insert m " : ")
		     (setq indent 12)))
		  ((looking-at " *\\([0-9]+\\)\\([a-z]+\\)\\([0-9]+\\): *")
		   (let ((n (buffer-substring (match-beginning 1)
					      (match-end 1)))
			 (m (buffer-substring (match-beginning 2)
					      (match-end 2)))
			 (o (buffer-substring (match-beginning 3)
					      (match-end 3))))
		     (if (not (equal o "1")) (setq m " "))
		     (if (not (equal m "a")) (setq n " "))
		     (delete-region (match-beginning 0) (match-end 0))
		     (insert "\n")
		     (indent-to (- 6 (length n)))
		     (insert n "  ")
		     (insert m "  ")
		     (insert "(" o ") : ")
		     (setq indent 17)))
		  ((looking-at " *\\\\")
		   (setq indent 5)
		   (setq p (point))
		   (goto-char (match-end 0))
		   (search-forward "\\")
		   (if (> (current-column) fill-column)
		       (progn
			 (goto-char p)
			 (insert "\n")
			 (indent-to 18)
			 (search-forward "\\"))))
		  ((looking-at " *\\[")
		   (setq indent 6)
		   (delete-horizontal-space)
		   (insert "\n")
		   (indent-to 5)
		   (forward-char 1))
		  ((and (= (preceding-char) ?\])
			(looking-at " *:"))
		   (delete-horizontal-space)
		   (setq indent 5)
		   (insert "\n "))
		  ((looking-at " *SYN *")
		   (delete-region (point) (match-end 0))
		   (insert "\n")
		   (delete-horizontal-space)
		   (insert "  ")
		   (setq indent 6)
		   (if (looking-at "syn ")
		       (progn (goto-char (match-end 0))
			      (insert "see "))))
		  (t
		   (setq p (point))
		   (skip-chars-forward " ,:;-")
		   (if (looking-at "\\([A-Z][A-Z][A-Z]+\\)\\( [A-Z][A-Z]+\\)*")
		       (webster-intern
			(buffer-substring (point) (match-end 0))))
		   (skip-chars-forward "^ \\")
		   (if (> (current-column) fill-column)
		       (progn
			 (goto-char p)
			 (insert "\n")
			 (delete-horizontal-space)
			 (indent-to indent)
			 (skip-chars-forward " ")
			 (skip-chars-forward "^ \\")
			 )))
		  )))
	(goto-char (point-min))
	(while (looking-at "\n") (delete-char 1))
	(goto-char (point-max))
	(insert "\n\n")
	(widen)))))

;; " \\(\\(slang\\|cap\\|pl\\|aj\\|av\\|n\\|v\\|vt\\|vi\\)\\(,[ \n]+\\)?\\)+\n"

;;;
;;; delete char1 and char2 if it precedes char1
;;; used to get rid of <space><return>
(defun delete-char-in-region (start end char1 char2)
  (goto-char start)
  (setq char2 (aref char2 0))
  (while (search-forward char1 end t)
    (delete-char -1)
    (if (= (char-after (- (point) 1)) char2)
	(delete-char -1))))

(defun webster (arg)
"Look up a word in the Webster's dictionary.
Open a network login connection to a webster host if necessary.
Communication with host is recorded in a buffer *webster*."
  (interactive (list
		(let ((prompt (concat "Look up word in webster ("
				      (current-word) "): "))
		      (completion-ignore-case t))
		  (downcase
		   (if webster-reformat
		       (completing-read prompt webster-completion-table
					nil nil)
		     (read-string prompt))))))
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
		(if (not (fboundp 'make-shell)) ;emacs19
		    (make-comint webster-process-name "telnet")
		  (make-shell webster-process-name "telnet")))
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
   "Close connection and quit webster-mode.  Buffer is not deleted."
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

