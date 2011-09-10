;To: unix-emacs@bbn.com
;Subject: a better lpr.el for GNU Emacs 18
;Date: Tue, 14 Feb 89 16:36:38 -0500
;From: Stephen Gildea <gildea@bbn.com>
;
;I am posting this in response to a query on this list.  This version
;of lpr.el works correctly with several different types of Unix and
;print spooler systems.  It is also more modular so if it doesn't work
;on yours you should be able to get it working easily.  (If you have to
;fix anything to get it to work, I'd like to see your improvements.)
;
;There is also a new command M-x lpq that displays the print queue.
;You may want to add the form
;(autoload 'lpq "lpr"
;  "Show the print queue in a temporary window." t)
;to your .emacs or site-init file.
;
; < Stephen
;   gildea@bbn.com


;; Print Emacs buffer on line printer.
;; Copyright (C) 1985, 1988 Free Software Foundation, Inc.
;; Modified by gildea Dec 88 to be more general.

;; This file is NOT part of GNU Emacs.
;; It is a variation by gildea.

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

(provide 'lpr)

(defvar lpr-command
  (cond ((file-exists-p "/usr/spool/mdqs")
	 "pprint")
	((eq system-type 'usg-unix-v)
	 "lp")
	(t
	 "lpr"))
  "Program to call to print a file.  See also  lpr-pretty-print-command.")

(defvar lpr-pretty-print-command t
  "Program to call to pretty print a file.
Either a string which is the name of the program, or the symbol t
which means to use the value of lpr-command.")

(defvar lpr-command-switches
  (cond ((string-equal lpr-command "pprint")
	 '("-jobname" "%s"))
	((string-equal lpr-command "lp")
	 '("-t%s"))
	(t
	 '("-J" "%s" "-T" "%s")))
  "List of basic switches that  lpr-command  always needs.
This variable is for switches specific to lpr-command.
The string %s gets replaced with the name of the print job.
For user options, set lpr-switches, not this variable.")

(defvar lpr-pretty-print-command-switches
  (cond ((string-equal lpr-command "pprint")
	 '(t "-filter" "pr -f"))
	((string-equal lpr-command "lp")
	 '(t))				;lp doesn't have a pretty option
	(t
	 '(t "-p")))
  "List of switches that are passed to  lpr-pretty-print-command.
The \"print-\" versions of the lpr commands use this.  This is a list of
strings except that the symbol t, whereever it occurs, is replaced
by all the elements of  lpr-command-switches.")

;(defconst lpr-switches nil
;  "*List of strings to pass as extra switch args to lpr-command.")

; should go in loaddefs.el
(defvar lpr-pretty-print-switches '(t)
  "*List of strings to pass as extra switch args to lpr-pretty-print-command.
If the symbol t appears in the list, it is replaced by all the elements
of  lpr-switches.")

(defvar lpq-command
  (cond ((file-exists-p "/usr/spool/mdqs")
	 "qstat")
	((eq system-type 'usg-unix-v)
	 "lpstat")
	(t
	 "lpq"))
  "Program to call to show the print queue.  The args are in  lpq-switches.")

(defvar lpq-command-switches
  (cond ((string-equal lpq-command "qstat")
	 '("-a"))
	(t
	 nil))
  "List of switches always passed to the program  lpq-command  by \\[lpq].")

(defvar lpq-switches nil
  "*List of switches (strings) passed to the program  lpq-command  by \\[lpq].
These are passed as extra switch arguments after lpq-command-switches.")

(defun lpr-buffer ()
  "Print buffer contents by calling the program lpr-command.
`lpr-switches' is a list of extra switches (strings) to pass to lpr-command."
  (interactive)
  (print-region-1 (point-min) (point-max) lpr-command (lpr-arg-list)))

(defun print-buffer ()
  "Pretty print the buffer by calling the program lpr-pretty-print-command.
`lpr-pretty-print-switches' is a list of extra switches (strings)
to pass to lpr-pretty-print-command."
  (interactive)
  (pretty-print-region-1 (point-min) (point-max)))

(defun lpr-region (start end)
  "Print region contents by calling the program lpr-command.
`lpr-switches' is a list of extra switches (strings) to pass to lpr-command."
  (interactive "r")
  (print-region-1 start end lpr-command (lpr-arg-list)))

(defun print-region (start end)
  "Pretty print the region by calling the program lpr-pretty-print-command.
`lpr-pretty-print-switches' is a list of extra switches (strings)
to pass to lpr-pretty-print-command."
  (interactive "r")
  (pretty-print-region-1 start end))

(defun lpq ()
  "Show the print queue in a temporary window."
  (interactive)
  (with-output-to-temp-buffer "*Print Queue*"
    (apply 'call-process lpq-command nil standard-output nil
	   (append lpq-command-switches lpq-switches))))

(defun pretty-print-region-1 (start end)
  (let ((command (if (eql lpr-pretty-print-command t)
		     lpr-command lpr-pretty-print-command)))
    (print-region-1 start end command
		    (nconc (lpr-substitute-for-t
			    lpr-pretty-print-command-switches
			    lpr-command-switches)
			   (lpr-substitute-for-t
			    lpr-pretty-print-switches
			    lpr-switches)))))

(defun print-region-1 (start end program-name switches)
  (let ((print-job-name (concat (buffer-name) " Emacs buffer"))
	(width tab-width))
    (save-excursion
      (message "Spooling...")
      (if (/= tab-width 8)
	 (let ((oldbuf (current-buffer)))
	    (set-buffer (get-buffer-create " *spool temp*"))
	    (widen)
	    (erase-buffer)
	    (insert-buffer-substring oldbuf start end)
	    (setq tab-width width)
	    (untabify (point-min) (point-max))
	    (setq start (point-min) end (point-max))))
      (apply 'call-process-region
	     start end program-name
	     nil nil nil
	     (mapcar 'lpr-format-switch switches))
      (message "Spooling...done"))))

;;; This function may prove useful to other packages
(defun lpr-arg-list ()
  (append lpr-command-switches lpr-switches))

(defun lpr-substitute-for-t (basiclist pattern)
  ;; replaces occurances of t in BASICLIST with PATTERN.
  ;; Neither PATTERN nor BASICLIST is destroyed.  The new list is returned.
  (let ((newlist nil))
    (while basiclist
      (setq newlist (nconc newlist
			  (if (eql (car basiclist) t)
			      (copy-sequence pattern)
			    (list (car basiclist)))))
      (setq basiclist (cdr basiclist)))
    newlist))

(defun lpr-format-switch (switch)
  ;; %s gets replaced by the print job name
  (format switch print-job-name print-job-name print-job-name))

