;;; ndtp.el --- Lookup NDTP client
;; Copyright (C) 1999 Lookup Development Team <lookup@ring.gr.jp>

;; Author: Keisuke Nishida <kei@psn.net>
;; Version: $Id: ndtp.el,v 1.7 1999/11/29 15:03:28 kazuhiro Exp $

;; This file is part of Lookup.

;; Lookup is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; Lookup is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Lookup; if not, write to the Free Software Foundation,
;; Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Code:

(require 'lookup)

(defconst ndtp-version "1.0")


;;;
;:: Internal variables
;;;

(defvar ndtp-current-agent nil)
(defvar ndtp-current-dictionary nil)
(defvar ndtp-current-process nil)

(defconst ndtp-process-coding-system
  (if (featurep 'evi-mule) (evi-coding-system 'euc-jp)))

(put 'ndtp ':methods '(exact prefix suffix))
(put 'ndtp ':gaiji-regexp "<gaiji:\\([^>]*\\)>")
(put 'ndtp ':reference-pattern
     '("\\($B"*(B\\(\\([^<\n]\\|<gaiji:[^>]*>\\)+\\)\\)?<\\([0-9a-f:]+\\)>"
       (or (match-string 1) "(->link)")
       (or (match-string 2) (match-string 4)) 4))

(put 'ndtp ':headings '(lookup-arrange-gaijis))

(put 'ndtp ':arranges
     '(lookup-arrange-references
       lookup-arrange-gaijis
       lookup-arrange-squeezed-references
       lookup-arrange-default-headings
       lookup-arrange-fill-lines))

(put 'ndtp ':adjusts
     '(lookup-adjust-show-gaijis
       lookup-adjust-check-references
       lookup-adjust-goto-min))

;;;
;:: types
;;;

;; ndtp agent:
;;
;;   (ndtp SERVER)
;;
;; SERVER  - host name of NDTP server
;;
;; [option]
;; :service - same as SERVICE above
;; :port    - same as PORT above
;; :account - same as `ndtp-account-name'
;; 
;; [property]
;; ndtp-process - NDTP connection related with agent
;; ndtp-dict    - last used dictionary

(defalias 'ndtp-agent-server 'lookup-agent-location)

(defun ndtp-agent-service (agent)
  (or (lookup-agent-option agent ':port)
      (lookup-agent-option agent ':service)
      "ndtp"))

(defun ndtp-agent-alternates (agent)
  (lookup-agent-option agent ':alternates))

(defun ndtp-agent-account (agent)
  (or (lookup-agent-option agent ':account)
      (concat (user-login-name) "@" (system-name))))

(defun ndtp-agent-coding (agent)
  (or (lookup-agent-option agent ':coding)
      ndtp-process-coding-system))

;; ndtp dictionary:
;;
;; CODE  - same as NAME below
;; NAME  - given by server `t' command
;; 
;; [option]
;; :coding  - same as `ndtp-process-coding-system'
;; 
;; [property]
;; ndtp-gaiji-cache - cache buffer for gaiji datas or `disable' if no support

(defun ndtp-make-dictionary (name title)
  (lookup-new-dictionary ndtp-current-agent name name title))

(defun ndtp-dictionary-coding (dictionary)
  (or (lookup-dictionary-option dictionary ':coding t)
      ndtp-process-coding-system))

;; ndtp entry:
;;
;; CODE    - entry specific code (e.g. "2c00:340") by server `Px' command
;; HEADING - given by server `Px' command

(defun ndtp-make-entry (code heading)
  (lookup-make-entry ndtp-current-dictionary code heading))

;;;
;:: macros
;;;

(put 'ndtp-with-agent 'lisp-indent-function 1)
(defmacro ndtp-with-agent (agent &rest body)
  (` (let ((ndtp-current-agent (, agent))
	   (ndtp-current-process (ndtp-agent-process (, agent))))
       (,@ body))))

(put 'ndtp-with-dictionary 'lisp-indent-function 1)
(defmacro ndtp-with-dictionary (dictionary &rest body)
  (` (ndtp-with-agent (lookup-dictionary-agent (, dictionary))
       (let ((ndtp-current-dictionary (, dictionary)))
	 (unless (eq (, dictionary)
		     (lookup-agent-get-property ndtp-current-agent 'ndtp-dict))
	   ;; $BI,MW$J$H$-$@$1<-=q$r(B select $B$9$k!#(B
	   ;; $B30It%W%m%;%9$H$d$j$H$j$9$k$h$j$3$NJ}$,9bB.$@$m$&$7!"(B
	   ;; $B%G%P%C%0$N$H$-%P%C%U%!$,$4$A$c$4$A$c$9$k$N$O$&$6$C$?$$!#(B
	   (ndtp-require-select (, dictionary))
	   (lookup-agent-put-property ndtp-current-agent 'ndtp-dict
				      (, dictionary))
	   ;; $B<-=qKh$KJ8;z%3!<%I$r@_Dj$9$k!#(B
	   (let ((code (ndtp-dictionary-coding (, dictionary))))
	     (when code
	       (set-process-coding-system ndtp-current-process code code))))
	 (,@ body)))))

(defun ndtp-agent-process (agent)
  (let ((process (lookup-agent-get-property agent 'ndtp-process)))
    (unless (and process (eq (process-status process) 'open))
      (let ((servers (cons (ndtp-agent-server agent)
			   (ndtp-agent-alternates agent)))
	    (service (ndtp-agent-service agent)))
	(if process (lookup-process-kill process))
	(while servers
	  (condition-case error
	      (progn
		(setq process (ndtp-process-open (car servers) service))
		(setq servers nil))
	    (error (setq servers (cdr servers))
		   (if (not servers) (signal (car error) (cdr error)))))))
      ;; $B:G=i$K<-=q0lMw$rF@$k$N$KJ8;z%3!<%I$N@_Dj$,I,MW!#(B
      (let ((coding (ndtp-agent-coding agent)))
	(when coding
	  (set-process-coding-system process coding coding)))
      ;; $B%5!<%P$X$N@\B3Kh$K9T$J$&I,MW$N$"$k=hM}!#(B
      (let ((ndtp-current-process process))
	(ndtp-process-require (concat "A" (ndtp-agent-account agent)) "\n"))
      (lookup-foreach
       (lambda (dictionary)
	 (lookup-dictionary-put-property dictionary 'ndtp-gaiji-enable nil))
       (lookup-agent-dictionaries agent))
      (lookup-agent-put-property agent 'ndtp-process process)
      (lookup-agent-put-property agent 'ndtp-dict nil))
    process))

(defun ndtp-agent-kill-process (agent)
  (let ((process (lookup-agent-get-property agent 'ndtp-process)))
    (when process
      (if (eq (process-status process) 'open)
	  (process-send-string process "Q\n"))
      (lookup-process-kill process)
      (lookup-agent-put-property agent 'ndtp-process nil))))


;;;
;:: Interface functions
;;;

(defconst ndtp-dictionary-regexp
  (eval-when-compile
    (let ((_ "[ \t]+") (num "[0-9]+") (title ".+") (name "[^ \t\n]+"))
      (concat "^ *" num _ "\\(" title "\\)" _ "\\(" name "\\)" _
	      num _ num _ num "[ \t]*$"))))

(put 'ndtp 'setup 'ndtp-setup)
(defun ndtp-setup (agent)
  (ndtp-with-agent agent
    (ndtp-process-require "t" "^$\\*\n"
      (lambda (process)
	(let (name title dicts)
	  (while (re-search-forward ndtp-dictionary-regexp nil t)
	    (setq title (match-string 1) name (match-string 2))
	    (setq dicts (cons (ndtp-make-dictionary name title) dicts)))
	  (nreverse dicts))))))

(put 'ndtp 'clear 'ndtp-clear)
(defun ndtp-clear (agent)
  (ndtp-agent-kill-process agent))

(put 'ndtp 'search 'ndtp-dictionary-search)
(defun ndtp-dictionary-search (dictionary query)
  (let ((method (lookup-query-method query))
	(string (lookup-query-string query)))
    ;; build the search command
    (setq string
	  (if (and (featurep 'mule)
		   (memq 'japanese-jisx0208 (find-charset-string string)))
	      (cond ((eq method 'prefix)
		     (concat "Pk" string "*"))
		    ((eq method 'suffix)
		     (concat "PK" (lookup-reverse-string string) "*"))
		    (t (concat "Pk" string)))
	    (cond ((eq method 'prefix)
		   (concat "Pa" string "*"))
		  ((eq method 'suffix)
		   (concat "PA" (lookup-reverse-string string) "*"))
		  (t (concat "Pa" string)))))
    ;; search the pattern
    (ndtp-with-dictionary dictionary
      (ndtp-process-require string "^$[$N&<]\n"
	(lambda (process)
	  (let (code heading last-code last-heading entries)
	    (while (re-search-forward "^[^$].*" nil t)
	      (if (not heading)
		  (setq heading (match-string 0))
		(setq code (match-string 0))
		;; $BF1$8%(%s%H%j$,O"B3$7$F$$$J$$$+%A%'%C%/$9$k!#(B
		;; $B$3$l$,$1$C$3$&$"$k$s$@!&!&(B
		(when (or (not (string= code last-code))
			  (not (string= heading last-heading)))
		  (setq entries (cons (ndtp-make-entry code heading) entries))
		  (setq last-code code last-heading heading))
		(setq heading nil)))
	    (nreverse entries)))))))

(put 'ndtp 'content 'ndtp-dictionary-content)
(defun ndtp-dictionary-content (dictionary entry)
  (ndtp-with-dictionary dictionary
    (let ((command (concat "S" (lookup-entry-code entry))))
      (substring (ndtp-process-require command "^$$\n") 3))))

(put 'ndtp 'gaiji 'ndtp-dictionary-gaiji)
(defun ndtp-dictionary-gaiji (dictionary code)
  (let ((buffer (lookup-dictionary-get-property dictionary 'ndtp-gaiji-cache)))
    (when buffer
      (with-current-buffer buffer
	(goto-char (point-min))
	(if (re-search-forward (format "^$=%s$" code) nil t)
	    (buffer-substring (point) (or (search-forward "$=" nil t)
					  (point-max))))))))


;;;
;:: Internal functions
;;;

(defun ndtp-require-select (dictionary)
  (ndtp-process-require (concat "L" (lookup-dictionary-code dictionary)) "\n")
  (and
   lookup-enable-gaiji
   (lookup-gaiji-glyph-possible-p)
   (ndtp-process-require "XL16" "^$.\n")
   (let ((buffer (lookup-dictionary-get-property
		  dictionary 'ndtp-gaiji-cache)))
     (unless buffer
       (if (not (string-match "16" (ndtp-process-require "XI" "^$[$N?<]\n")))
	   (setq buffer 'disable)
	 (setq buffer (generate-new-buffer
		       (format " *ndtp gaiji table for %s*"
			       (lookup-dictionary-id dictionary))))
	 (ndtp-process-require "XL16" "^$.\n")
	 (with-current-buffer buffer
	   (insert (ndtp-process-require "XB" "^$$\n"))))
       (lookup-dictionary-put-property dictionary 'ndtp-gaiji-cache buffer)))))

;;;
;:: ndtp process
;;;

(defun ndtp-process-open (server service)
  (lookup-proceeding-message (format "connecting to %s..." server))
  (let* ((buffer (lookup-open-process-buffer (concat " *ndtp+" server "*")))
	 (process (open-network-stream "ndtp" buffer server service)))
    (process-kill-without-query process)
    process))

(put 'ndtp-process-require 'lisp-indent-function 2)
(defun ndtp-process-require (command separator &optional filter)
  (lookup-process-require ndtp-current-process (concat command "\n")
			  separator filter))

(provide 'ndtp)

;;; ndtp.el ends here
