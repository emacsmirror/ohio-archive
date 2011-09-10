;;;; printers.el -- An interface to `lpstat` (SysV) and /etc/printcap (BSD).

;;; Description:
;;; 
;;; Determines the default printer and the list of available printers
;;; from the LPDEST environment variable and the `lpstat` command (SysV)
;;; or from the PRINTER environment variable and the /etc/printcap file
;;; (BSD), and stores them in the `system-printer-default' and `system-
;;; printer-list' variables.
;;; 
;;; The list of available printers is used by the `read-printer'
;;; function to provide completion, which is used in turn by the
;;; `set-system-printer-default' command to set the default printer used
;;; by the `lp` (SysV) and `lpr` (BSD) commands.  The `set-system-printer-
;;; list' command can be used to re-initialize the list, in case the
;;; system printers have been reconfigured since printers.el was loaded.
;;; 
;;; On BSD systems, the `system-printer-field' variable controls whether
;;; all the `|'-separated names for each printer in /etc/printcap are
;;; listed in `system- printers'.

;;; Copyright:
;;; 
;;; Copyright (C) 1995 Kevin Rodgers
;;; 
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; at your option) any later version.
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
;;; My employer, Information Handling Services, has not disclaimed any
;;; copyright interest in printers.el.
;;; 
;;; Kevin Rodgers <kevin.rodgers@ihs.com>   Project Engineer
;;; Information Handling Services           Electronic Systems Development
;;; 15 Inverness Way East, M/S A201         GO BUFFS!
;;; Englewood CO 80112 USA                  1+ (303) 397-2807[voice]/-2779[fax]

;;; LCD Archive Entry:
;;; printers|Kevin Rodgers|kevinr@ihs.com|
;;; An interface to `lpstat` (SysV) and /etc/printcap (BSD).|
;;; 21-Aug-1995|1.6|~/misc/printers.el.gz|


;;; Package interface:

(provide 'printers)


;;; User Options:

(defvar system-printer-field 1
  "*If non-nil, a number that specifies the printer name field on BSD systems.

If this is nil, all of the `|'-separated names for each printer in
/etc/printcap are included in `system-printer-list'; otherwise, only the
Nth name of each printer is included.
This affects the initial value of `system-printer-default' as well.")


;;; Programmer Variables:

(defvar system-printer-default
  (prog1 nil
    (eval-after-load "printers" '(set-system-printer-default))) ; delayed
  "The default system printer.")

(defvar system-printer-list
  (prog1 nil
    (eval-after-load "printers" '(set-system-printer-list))) ; delayed
  "The list of system printers.")


;;; User Commands:

;;;###autoload
(defun set-system-printer-default (&optional printer)
  "Set the system default printer to PRINTER.
If PRINTER isn't specified, initialize to the system default."
  (interactive (list (read-printer "System default printer: ")))
  (if (null printer)
      (setq printer (system-printer-default)))
  (setq system-printer-default printer)
  (setenv (if (and (memq system-type '(usg-unix-v dgux hpux irix))
		   (not (save-match-data
			  (string-match "-sco" system-configuration))))
	      "LPDEST"			; SysV
	    "PRINTER")			; BSD
	  system-printer-default))

;;;###autoload
(defun set-system-printer-list ()
  "Set the system printer list."
  (interactive)
  (setq system-printer-list (system-printer-list)))


;;; Programmer Functions:

(defun read-printer (prompt &optional default)
  "Read a printer name from the minibuffer, prompting with PROMPT and
completing from `system-printer-list'.
If optional arg DEFAULT is non-nil, it is the default; otherwise,
`system-printer-default' is the default."
  (let* ((completion-ignore-case nil)
	 (set-system-printer-list "*reinitialize system printer list*")
	 (printer
	  (completing-read prompt
			   (cons (list set-system-printer-list)
				 (mapcar (function list) system-printer-list))
			   nil nil (or default system-printer-default))))
    (if (string-equal printer set-system-printer-list)
	(progn
	  (set-system-printer-list)
	  (read-printer prompt default))
      printer)))


;;; Utility Functions:

(defun system-printer-default ()
  ;; Return the user's or the system default printer.
  (let ((user-printer-default nil)
	(system-command nil))
    (if (and (memq system-type '(usg-unix-v dgux hpux irix))
	     (not (save-match-data (string-match "-sco" system-configuration))))
	;; SysV:
	(or (setq user-printer-default (getenv "LPDEST"))
	    (setq system-command "lpstat -d | awk '{print $NF}'"))
      ;; BSD:
      (or (setq user-printer-default (getenv "PRINTER"))
	  (setq system-command
		(concat "sed -e '/^$/d' \
                             -e '/^#/d' \
                             -e 's/:.*$//' \
                             -e 'q' \
                             /etc/printcap"
			" | "
			(if system-printer-field
			    (format "awk -F'|' \
                                         '{if (NF >= %d) print $%d; \
                                           else print $NF}'"
				    system-printer-field system-printer-field)
			  "awk -F'|' '{print $1}'")))))
    (or user-printer-default
	(prog2 (message "Getting the default system printer...")
	    (system-command-output system-command t)
	  (message "Getting the default system printer...done")))))

(defun system-printer-list ()
  ;; Return the list of system printers.
  (let ((system-command nil))
    (if (and (memq system-type '(usg-unix-v dgux hpux irix))
	     (not (save-match-data (string-match "-sco" system-configuration))))
	;; SysV:
	(setq system-command
	      "lpstat -p | awk '/^printer/ {print $2}'")
      ;; BSD:
      (setq system-command
	    (concat "sed -e '/^$/d' \
                         -e '/^[# 	]/d' \
                         -e 's/:.*$//' \
                         /etc/printcap"
		    " | "
		    (if system-printer-field
			(format "awk -F'|' \
                                     '{if (NF >= %d) print $%d; \
                                       else print $NF}'"
				system-printer-field system-printer-field)
		      "awk -F'|' '{for (i=1; i <= NF; i++) print $i}'"))))
    (prog2 (message "Listing the system printers...")
	(listify-output (system-command-output system-command))
      (message "Listing the system printers...done"))))

(defun listify-output (output)
  ;; Convert newline-separated names in OUTPUT to a list of strings.
  (let ((start 0)
	(list '()))
    (while (string-match "^.+$" output start)
      (setq list
	    (cons (substring output (match-beginning 0) (match-end 0))
		  list))
      (setq start (match-end 0)))
    (nreverse list)))

(defun system-command-output (command &optional trim)
  ;; Execute COMMAND in the shell and return the output.
  ;; If optional arg TRIM is non-nil, ignore the trailing newline.
  (save-excursion
    (set-buffer (get-buffer-create " *system command output*"))
    (erase-buffer)
    ;; (shell-command command t)
    (call-process "/bin/sh" nil t nil "-c" command) ; avoid setting the mark
    (buffer-substring (point-min) (if trim
				      ;; Should we check that it's a `\n' char?
				      (1- (point-max))
				    (point-max)))))

;;;; printers.el ends here
