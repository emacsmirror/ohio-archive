;;; lpq.el -- edit a printer's spool area.
;;;
;;; Copyright (C) 1996 Ralph Schleicher
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2 of
;;; the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;; This file is not part of GNU Emacs.
;;;
;;; Author: Ralph Schleicher <rs@purple.UL.BaWue.DE>
;;; Maintainer: see the `Author' field
;;; Keywords: local print
;;; Comments: Put this statement into your `site-start.el' file:
;;;
;;;	(autoload 'lpq "lpq"
;;;	  "Edit a printer's spool area." t)
;;;
;;; If you prefer the long output form of the lpq(1) program, say
;;;
;;;	(setq-default lpq-switches (lambda ()
;;;				     (list "-l" (concat "-P" lpq-printer))))
;;;
;;;	(setq lpq-header-lines 0
;;;
;;;	      lpq-job-regexp "\\[job \\([0-9]+\\)"
;;;	      lpq-job-match 1)
;;;
;;; You can make this code work with a SYSV lp system.  Add the following
;;; lines after the `autoload' statement above:
;;;
;;;	(setq lpq-program "lpstat"
;;;	      lprm-program "cancel"
;;;
;;;	      lpq-status-line nil
;;;	      lpq-header-lines 0
;;;
;;;	      lpq-job-regexp "[^ \t]+"
;;;	      lpq-job-match 0
;;;
;;;	      lpq-printcap nil)
;;;
;;;	(setq-default lpq-printer (lambda ()
;;;				    (getenv "LPDEST"))
;;;		      lpq-switches (lambda ()
;;;				     (list lpq-printer))
;;;		      lprm-switches nil)
;;;
;;; Dear SYSV user, please don't say
;;;
;;;	(add-hook 'lpq-mode-hook
;;;		  (lambda ()
;;;		    (define-key lpq-mode-map "c" 'lpq-do-remove)))
;;;
;;; so that you can cancel print jobs by typing `c'.  This key is
;;; reserved as a prefix command for the lpc(8) program.
;;; Time-stamp: "Sat May 11 13:08:36 MET DST 1996 rs@purple.UL.BaWue.DE"
;;; Code:


(defun lpq-object-value (object)
  "Evaluate OBJECT and return the result."
  (if (symbolp object)
      (cond ((fboundp object)
	     (funcall object))
	    ((boundp object)
	     (symbol-value object)))
    (if (and (consp object) (eq (car object) 'lambda))
	(funcall object)
      (identity object))))

(defvar lpq-printers nil
  "AList of printer names and printer aliases.
Cons cells are of the form

    (PRINTER . PRINTER)  or  (ALIAS . PRINTER)

Multiple aliases for the same printer are allowed but buffer names are
always of the form `*lpq-PRINTER*'.")

(defvar lpq-printer (lambda ()
		      (or (getenv "PRINTER") "lp"))
  "Printer name of the current `*lpq-PRINTER*' buffer.")

(make-variable-buffer-local 'lpq-printer)
(let ((printer (lpq-object-value lpq-printer)))
  (or (null printer) (assoc printer lpq-printers)
      (setq lpq-printers (cons (cons printer printer) lpq-printers))))

(defvar lpq-program "lpq"
  "Program used to examine a printer's spool area.")

(defvar lpq-switches (lambda ()
		       (list (concat "-P" lpq-printer)))
  "List of extra arguments when `lpq-program' is invoked.")

(make-variable-buffer-local 'lpq-switches)

(defvar lprm-program "lprm"
  "Program used to remove a print job.")

(defvar lprm-switches (lambda ()
			(list (concat "-P" lpq-printer)))
  "List of extra arguments when `lprm-program' is invoked.")

(make-variable-buffer-local 'lprm-switches)

(defvar lpq-status-line t
  "Non-`nil' means that the first line out-putted by `lpq-program' is a
short status report about the printer.  `t' means that this line should
be echoed in the mini-buffer area, any other non-`nil' value only removes
that line.")

(defvar lpq-header-lines 1
  "Number of header lines out-putted by `lpq-program'.")

(defvar lpq-job-regexp "[ \t]+\\([0-9]+\\)[ \t]"
  "Regular expression matching the print job id.")

(defvar lpq-job-match 1
  "Sub-expression of `lpq-job-regexp' specifying the print job id.")

(defvar lpq-printcap "/etc/printcap"
  "Location of the `printcap' database, normally \"/etc/printcap\".
`nil' means, don't use features based on that file, for example, automatic
initialization of `lpq-printers'.")

(defvar lpq-printcap-modified t
  "`t' means that the `printcap' database have to be parsed again.")

(defvar lpq-printcap-parse-function 'lpq-parse-printcap
  "Function for parsing `lpq-printcap'.
This file is already visited in the current buffer with point at the
beginning of the buffer when `lpq-printcap-parse-function' is called.")

(defvar lpq-update-interval 60
  "*Update all `*lpq-PRINTER*' buffers after `lpq-update-interval' seconds.")

(defvar lpq-update-process nil)

(defvar lpq-buffers nil
  "List of all `*lpq-PRINTER*' buffers.")

(defvar lpq-mode-map nil
  "*Keymap used while in lpq mode.")

(if lpq-mode-map
    ()
  (setq lpq-mode-map (make-keymap))
  (suppress-keymap lpq-mode-map t))

(define-key lpq-mode-map "g" 'lpq-do-update)
(define-key lpq-mode-map "d" 'lpq-do-remove)
(define-key lpq-mode-map "k" 'lpq-do-remove)
(define-key lpq-mode-map "q" 'lpq-do-quit)

(define-key lpq-mode-map [menu-bar lpq]
  (cons "Lpq" (make-sparse-keymap "Lpq")))

(define-key lpq-mode-map [menu-bar lpq quit]
  '("Quit" . lpq-do-quit))

(define-key lpq-mode-map [menu-bar lpq separator-commands]
  '("--"))

(define-key lpq-mode-map [menu-bar lpq update]
  '("Update" . lpq-do-update))

(define-key lpq-mode-map [menu-bar lpq remove]
  '("Remove" . lpq-do-remove))

(defvar lpq-mode-hook nil)


(defun lpq (&optional printer)
  "Edit a printer's spool area.
If the optional argument PRINTER is `nil', the default printer (on BSD
systems either the value of the environment variable `PRINTER' or \"lp\")
will be used.  If PRINTER is not a string, for example if `lpq' is called
interactively and a prefix argument had been specified, PRINTER will be
read from the mini-buffer.

Special commands:

\\{lpq-mode-map}"
  (interactive "P")
  (and (stringp lpq-printcap)
       (lpq-parse-printcap-file))
  (and (interactive-p) (not (null printer)) (not (stringp printer))
       (setq printer (completing-read "Printer name: " lpq-printers)))
  (and (or (null printer) (equal printer ""))
       (setq printer (lpq-object-value (default-value 'lpq-printer))))
  (let ((known (assoc printer lpq-printers)))
    (and known (setq printer (cdr known))))
  (let ((name (if (null printer)
		  "*lpq*"
		(concat "*lpq-" printer "*"))))
    (pop-to-buffer (or (get-buffer name)
		       (generate-new-buffer name))))
  (lpq-mode printer))

(defun lpq-mode (printer)
  "Major mode for editing a printer's spool area."
  (kill-all-local-variables)
  (use-local-map lpq-mode-map)
  (setq major-mode 'lpq-mode
	mode-name "Lpq")
  (setq lpq-printer printer
	lpq-switches (lpq-object-value (default-value 'lpq-switches))
	lprm-switches (lpq-object-value (default-value 'lprm-switches)))
  (or (and lpq-update-process (eq (process-status lpq-update-process) 'run))
      (progn
	(and lpq-update-process (delete-process lpq-update-process))
	(let ((process-connection-type nil))
	  (setq lpq-update-process
		(start-process "lpq-update" nil
			       (expand-file-name "wakeup" exec-directory)
			       (int-to-string lpq-update-interval))))
	(process-kill-without-query lpq-update-process)
	(set-process-sentinel lpq-update-process nil)
	(set-process-filter lpq-update-process 'lpq-update-filter)))
  (setq truncate-lines t
	buffer-read-only t
	buffer-offer-save t)
  (lpq-update-buffer)
  (if (not (member (current-buffer) lpq-buffers))
      (setq lpq-buffers (cons (current-buffer) lpq-buffers)))
  (make-local-variable 'kill-buffer-hook)
  (add-hook 'kill-buffer-hook
	    (lambda ()
	      (setq lpq-buffers (delete (current-buffer) lpq-buffers))))
  (run-hooks 'lpq-mode-hook))

(defun lpq-update-filter (proc string)
  "Called whenever `lpq-update-process' outputs something."
  (if (null lpq-buffers)
      (delete-process lpq-update-process)
    (let ((orig-buffer (current-buffer)))
      (unwind-protect
	  (mapcar (lambda (buffer)
		    (and (bufferp buffer)
			 (progn
			   (set-buffer buffer)
			   (lpq-update-buffer t))))
		  lpq-buffers)
	(set-buffer orig-buffer))))
  (sit-for 0))

(defun lpq-update-buffer (&optional silent)
  "Update the current buffer contents."
  (let* ((column (current-column))
	 (line (progn
		 (beginning-of-line)
		 (count-lines 1 (point))))
	 (buffer-read-only nil))
    (erase-buffer)
    (apply 'call-process
	   (nconc (list lpq-program nil t nil)
		  (append lpq-switches nil)))
    (goto-char (point-min))
    (if (and lpq-status-line (search-forward "\n" nil t))
	(let ((status (buffer-substring (point-min) (match-beginning 0))))
	  (delete-region (point-min) (match-end 0))
	  (and (not silent) (eq lpq-status-line t)
	       (message "lpq: %s" status))))
    (set-buffer-modified-p nil)
    (if (zerop line)
	(forward-line lpq-header-lines)
      (forward-line line))
    (move-to-column column)))

(defun lpq-fetch-job (&optional fail)
  "Return the job id of the current line as a string.
If the optional argument FAIL is non-`nil', print an error message if no
print job could be found."
  (save-excursion
    (beginning-of-line)
    (if (eobp)
	(forward-line -1)
      (and (looking-at "\n")
	   (forward-line 1)))
    (while (and (looking-at "[ \t]") (not (bobp)))
      (forward-line -1))
    (if (re-search-forward lpq-job-regexp nil t)
	(match-string lpq-job-match)
      (and fail (error "No print job found on this line")))))

(defun lpq-remove-job (job)
  "Remove the print job JOB (a string)."
  (apply 'call-process
	 (nconc (list lprm-program nil 0 nil)
		(append lprm-switches (list job)))))


;;; User commands.

(defun lpq-do-update ()
  "Update the current buffer."
  (interactive)
  (lpq-update-buffer))

(defun lpq-do-remove ()
  "Remove the print job on this line."
  (interactive)
  (lpq-remove-job (lpq-fetch-job t))
  (lpq-update-buffer))

(defun lpq-do-quit ()
  "Switch to another buffer."
  (interactive)
  (bury-buffer))


;;; printcap(5) utilities.

(defun lpq-parse-printcap-file ()
  (let ((mod-time (nth 5 (file-attributes lpq-printcap))))
    (or (equal lpq-printcap-modified mod-time)
	(let ((orig-buf (current-buffer))
	      (temp-buf nil))
	  (message "Parsing %s..." lpq-printcap)
	  (unwind-protect
	      (progn
		(setq temp-buf (generate-new-buffer " lpq"))
		(buffer-disable-undo temp-buf)
		(set-buffer temp-buf)
		(insert-file-contents lpq-printcap)
		(funcall lpq-printcap-parse-function))
	    (if (bufferp temp-buf)
		(kill-buffer temp-buf))
	    (set-buffer orig-buf))
	  (message "Parsing %s... done" lpq-printcap)))
    (setq lpq-printcap-modified mod-time)))

(defun lpq-parse-printcap ()
  (let ((result nil))
    (while (re-search-forward "^[^# \t\n]" nil t)
      (backward-char)
      (let ((printers nil)
	    (reference nil))
	(while (and (not (looking-at ":"))
		    (re-search-forward "[^|:]+"))
	  (setq printers (cons (match-string 0) printers))
	  (skip-chars-forward "|"))
	(and (null printers)
	     (error "Parse error in `%s'" lpq-printcap))
	(setq printers (nreverse printers)
	      reference (if (= (length printers) 1)
			    (car printers)
			  (nth 1 printers)))
	(and (string-match "[ \t]" reference)
	     (setq reference (car printers)))
	(mapcar (lambda (pr)
		  (setq result (cons (cons pr reference) result)))
		printers)))
    (setq lpq-printers (nreverse result))))


(provide 'lpq)


;;; lpq.el ends here
