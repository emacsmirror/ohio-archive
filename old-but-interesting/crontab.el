;From: chris@lxn.eds.com (Christopher D. Orr)
;Newsgroups: comp.emacs
;Subject: crontab.el (Functions to allow editing of crontab)
;Message-ID: <429@lxn.eds.com>
;Date: 18 Aug 89 16:19:47 GMT
;Organization: Electronic Data Systems (EDS), Center Valley, PA
;Lines: 157
;Keywords: crontab system administrators cron
;
;
;Among the wealth of Emacs lisp code, I expected to find somebody who
;had written a function for editing a user's crontab entry.  But alas,
;I could not find one.  Enclosed is such a function.
;
;Please report and bug reports to me and any enhancements, refinements,
;and/or rewrites that are made.
;
;------------------------------ CUT HERE ------------------------------
;
;; #(@) crontab.el - An Emacs function to assist in editing crontab entries
;; Last edited: Fri Aug 18 12:19:22 1989 by chris (Christopher D. Orr) on lxn
;;
;; Version: 1.00 - Initial Creation of mode
;;          1.01 - Added crontab-use-local-file variable
;;          1.02 - Reworked most of the library to be cleaner.
;;          1.03 - Now deletes blank lines in crontab entry

;; Copyright (C) 1989 Christopher D. Orr (chris@lxn.eds.com)

;; This file NOT YET is part of GNU Emacs.

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
;;
;; TODO:
;;

;; Place the following line in your ~/.emacs file:
;;    (autoload 'crontab-edit "crontab"
;;              "Function to allow the easy editing of crontab files." t)
;;

(provide 'crontab-edit)

;;; Local Variables.  Define these to your liking.

(defvar crontab-filename "~/.crontab"
  "*The name of the file to store the User's Crontab.")

(defvar crontab-directory "/usr/spool/cron/crontabs"
  "*The name of the directory in which crontab stores it's entries.")

(defvar crontab-use-local-file nil
  "*Non-nil means use file stored in User's Home directory, if it exists.
Otherwise, always ask crontab for the current entry (maybe).")


;;; Interactive Function called to edit a Crontab Entry.  It is called
;;; instead of crontab-edit to allow for future automatic entries.

(defun crontab-edit ()
  "Function to allow the easy editing of crontab files."

  (interactive)
  (crontab-get))


;;; Function to retrieve the crontab entry.  The Function will
;;; retrieve the file (crontab-filename) first.  If the file does not
;;; exists, a crontab -l command will be executed. 

(defun crontab-get ()
   "Retrieve a crontab file either using crontab -l or from the variable
crontab-filename"
   (message "Retrieving Crontab ... ")
   (switch-to-buffer (create-file-buffer crontab-filename))
   (erase-buffer)

   (if (file-exists-p crontab-filename)
       (if (file-newer-than-file-p (concat crontab-directory "/" (user-login-name)) (expand-file-name crontab-filename))
	   (if (yes-or-no-p "Cron has a more recent copy of your crontab.  Use it ? ")
	       (call-process "crontab" nil t t "-l")
	     (insert-file crontab-filename))
	 (if crontab-use-local-file
	     (insert-file crontab-filename)
	   (call-process "crontab" nil t t "-l")))
     (if crontab-use-local-file
	 (insert-file crontab-filename)
       (call-process "crontab" nil t t "-l")))

;; What if crontab returns a fatal ??????  Can't we check the errorlevel ????
   (goto-char (point-min))
   (if (search-forward "crontab:" nil t nil)
       (erase-buffer))
   (if (eobp)
       (crontab-initialize))
   (goto-line 6)
   (setq buffer-file-name crontab-filename)
   (set-buffer-modified-p nil)
   (make-variable-buffer-local 'write-file-hooks)
   (or (memq 'crontab-save write-file-hooks)
       (setq write-file-hooks 
	     (reverse (cons 'crontab-save (reverse write-file-hooks)))))
   (message "Save file normally when finished to update cron."))


;;; This function is called whenever a save-file operation is
;;; performed in the crontab buffer.  It saves the crontab to the file
;;; name (crontab-filename) and then removes the crontab buffer.

(defun crontab-save ()
  "Submit the edited crontab to the cron deamon for processing"

  (goto-char (point-min))
  (while (not (eobp))
    (delete-blank-lines)
    (forward-line 1))
  (redraw-display)

  (setq write-file-hooks nil)
  (let ((crontab-buffer (buffer-name)))
    (basic-save-buffer)

;; What if the call-process to crontab fails ???  Can we check for a fatal ???
;;  (call-process "crontab" nil nil nil (expand-file-name crontab-filename))
    (shell-command (concat "crontab " (expand-file-name crontab-filename)))

    (switch-to-buffer (other-buffer))
    (kill-buffer crontab-buffer))
  (message (concat "Crontab saved as " crontab-filename " and submitted to cron."))
  nil)

(defun crontab-initialize ()
  "Create a default Crontab file if one does not exist or is empty.
If the function (time-stamp) is available, the last modification time will
be stamped to the file."

   (insert "# Cron Table Entry for ")
   (insert (user-login-name))
   (insert " (")
   (insert (user-full-name))
   (insert ")\n# Last Edited: \n")
   (insert "#\n")
   (insert "# min    hr     day   mon    wday(0=sun)  cmd\n")
   (insert "#\n"))

;;; Watch out for the signature  :-)

;-- 
;Christopher D. Orr                  | US MAIL: Electronic Data Systems (EDS)
;UUCP: vu-vlsi!lxn!chris             |          Lanark Building
; or   chris%lxn.uucp@rutgers.edu    |          Center Valley, PA  18034
; or   lehi3b15!lxn!chris            | Voice:   (215) 282-1213
