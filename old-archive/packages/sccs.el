;From tegra!vail@uunet.uu.net Fri Jul 20 13:29:03 1990
;From: tegra!vail@uunet.uu.net
;Date: Fri, 20 Jul 90 11:13:01 EDT
;To: uunet!stc06.CTD.ORNL.GOV!de5@uunet.UU.NET
;In-Reply-To: SILL D E's message of Fri, 20 Jul 90 10:59:43 EDT <9007201459.AA14712@stc06.CTD.ORNL.GOV>
;Subject: SCCS elisp package
;
;
;Here is my SCCS code.  The RCS version bas done by a friend of a
;friend (don@coral.com??).
;
;If you need anything else to go with it, let me know.
;
;Shar and Enjoy, jv
;
;Stattinger's Law: It works better if you plug it in.
; _____
;|     | Johnathan Vail | n1dxg@tegra.com
;|Tegra| (508) 663-7435 | N1DXG@448.625-(WorldNet)
; -----  jv@n1dxg.ampr.org {...sun!sunne ..uunet}!tegra!vail


;;;;
;;;;	sccs.el
;;;;
;;;;              "SCCS: The condom of promiscuous programmers"
;;;;
;;;;	Written November 1989 Johnathan Vail, Tegra Varityper
;;;;		tegra!vail@ulowell.edu    sunne!tegra!vail@sun.com
;;;;		...uunet!tegra!vail
;;;;		(508) 663-7435
;;;;
;;;;	Perform common sccs commands from within Emacs with a
;;;;		minimum of hassle.  See the following set-keys
;;;;		for the defined functions.
;;;;
;;;;	This file is not part of GNU Emacs although FSF is
;;;;	welcome to it, as is anyone else who finds it useful.
;;;;
;;;;	RESTRICTIONS ON DISTRIBUTION:
;;;;		Share and Enjoy
;;;;	(Please keep my name attached somewhere to feed my own vanity)
;;;;
;;;;	Revision History:
;;;;
;;;;	 5 Dec 1989 JV	- Pretty up comments
;;;;	14 Nov 1989 JV	- Examine error list to decide to reparse errors
;;;;	10 Nov 1989 JV	- Add next-error hacks
;;;;	 8 Nov 1989 JV	- Add Comment defaults
;;;;	 7 Nov 1989 JV	- Fix column position bug
;;;;	 6 Nov 1989 JV	- Another day, another hack
;;;;




(defvar sccs-buffer "*SCCS*"
  "Buffer name used for sccs command output")



(global-set-key "\C-ci" 'sccs-info)
(global-set-key "\C-cp" 'sccs-prt)
(global-set-key "\C-cg" 'sccs-get)
(global-set-key "\C-ce" 'sccs-edit)
(global-set-key "\C-cE" 'sccs-delget)
(global-set-key "\C-cN" 'sccs-create)





(defun sccs-create (revision)
  "Put the current file into SCCS"

  (interactive "sVersion? ")

  (let ((fname (buffer-file-name)))
    (maybe-save)
    (message "Putting %s into SCCS..." (file-name-nondirectory fname))
    (sccs-do "create" fname revision)))





(defun sccs-get (revision)
  "Get the file from sccs, asking for a REVISION"

  (interactive "sVersion? ")

  (let ((fname (buffer-file-name)))
    (maybe-save)
    (message "Getting file %s from SCCS..." (file-name-nondirectory fname))
    (sccs-do "get" fname revision)))





(defun sccs-edit ()
  "If the buffer is read only then do a `sccs edit' and re-read the buffer"

  (interactive)

  (if buffer-read-only ()
    (error "Current buffer is writeable"))

  (let ((fname (buffer-file-name)))
    (message "Getting file %s from SCCS for editing..." (file-name-nondirectory fname))

    (sccs-do "edit" fname)))




;;;
;;; sccs-delget
;;;
;;; This function will try to default the comment string to the revision
;;; history comment that you just put it.  It does this by looking for the
;;; first datestamp (for today!) and using anything after it for the comment
;;; Of course if you don't use tegra-datestamp then you will have no default.
;;; (Of course you could define tegra-date-stamp to return a string that
;;;  will determine the start of your header comment and that will work)
;;;
;;; tegra-datestamp is a local hack that returns a string such as
;;; " 5 Dec 1989 JV	- " that is used for this company's coding standards.
;;; It is defined in a file called tegra.el and is very company specific that
;;; it is not of general use.  You could define tegra datestamp to something
;;; useful, take it our completely or ignore it since it checks first to see
;;; if it exists.  If you would like a copy of tegra.el just ask.  It includes
;;; the datestamp function, automagic function and files headers and such.
;;;

(defun sccs-delget (comments)
  "sccs delget command on current file, replacing this version in SCCS"

  (interactive (list (read-string "Comments? "
				  (save-excursion
				    (and (fboundp 'tegra-datestamp)
					 (goto-char (point-min))
					 (re-search-forward (concat (tegra-datestamp)
								 "\\(.*$\\)") nil t)
					 (buffer-substring (match-beginning 1) (match-end 1)))))))
  (let ((fname (buffer-file-name)))
    (maybe-save)
    (message "Putting %s back into SCCS..." (file-name-nondirectory fname))

    (sccs-do "delget" fname nil comments)))






(defun sccs-info ()
  "Show SCCS info"

  (interactive)

  (message "Getting  SCCS info...")
  (sccs-do "info"))







(defun sccs-prt ()
  "Show history for current file"

  (interactive)

  (let ((fname (buffer-file-name)))
    (message "Getting SCCS history for %s..." (file-name-nondirectory fname))
    (sccs-do "prt" fname)))




;;;
;;; sccs-do
;;;
;;; This is the function that does all the work.  As such, it has a lot of
;;; hacks to handle all the different uses it may be exposed to.  Generally
;;; this function will call sccs with appropriate args, then parse the output
;;; for re-display on the echo line or display in the other window.  It also
;;; will re-load the file and re-position the cursor where it was where it
;;; started.  This makes the sccs-commands almost transparent when used.
;;;
;;;
;;; For use with next-error and the *compilation* buffer, a lot of logic has
;;; been added to make that work transparently.  I will try to explain:
;;;
;;; First, if you don't use compilation and error commands then sccs-do
;;;   should not barf or do anything special.
;;;
;;; The next-error command keeps a list of errors, each error is a list of
;;;   the mark in the *compilation* buffer where the error was found and a
;;;   mark in the source file.
;;;
;;; If you now go and use the sccs-command to get the file out of sccs then
;;;   when sccs-do loads the version the markers in the error list
;;;   are no longer valid.
;;;
;;; The first attempt to fix this was to kill the error list and set the parse
;;;   point in the *compilation* buffer to be where we left off.  This would
;;;   cause a re-parse when next-error was called and the reparse would start
;;;   from the current error and all would be well, except:
;;;
;;; If your editing to the file causes lines to be inserted or deleted then
;;;   next-error, when it re-parses will set the new marks to the old lines.
;;;
;;; The solution to this is to see if the file being edited is on the error
;;;   list at all (so we don't waste our time reparsing when we don't need to)
;;;   *before* we mung the markers with loading a new file.
;;;
;;; Finally, if it is on the error list and a new file is loaded then reparse
;;;   the errors from the current error. NP.
;;;
;;; Hope this helps, jv
;;;

(defun sccs-do (command  &optional fname revision comments)
  "Exec sccs COMMAND on FNAME, with optional REVISION and COMMENTS"

  (let ((line-num (count-lines (point-min) (point)))
	column-num sccs-buf re-parse cbuf)
    (save-excursion
      (set-buffer (setq sccs-buf (get-buffer-create sccs-buffer)))
      (widen) (erase-buffer))

    (if (not fname)			; two different flavors, might be combined..
	(call-process "sccs" nil sccs-buf nil command)
      (call-process "sccs" nil sccs-buf nil command fname
		    (if (or (not revision) (equal revision "")) "" (concat "-r" revision))
		    (if comments (concat "-y" comments) ""))
      (if (buffer-modified-p) ()
	(setq re-parse (and (boundp 'compilation-parsing-end)
			    (boundp 'compilation-error-list)
			    (listp compilation-error-list)
			    (setq cbuf (current-buffer))
			    (memq t (mapcar '(lambda (l)
					       (eq cbuf
						   (marker-buffer (car (cdr l)))))
					    compilation-error-list))))
	(setq column-num (- (point) (progn (beginning-of-line) (point))))
	(find-alternate-file fname)
	(goto-line line-num)
	(or (> column-num 0)
	    (eq line-num 0)
	    (forward-line 1))
	(goto-char (+ (point) column-num))

	(if (not re-parse) ()
	  (setq compilation-parsing-end (car (car compilation-error-list)))
	  (setq compilation-error-list nil)
	  (if (setq cbuf (get-buffer "*compilation*"))
	      (save-excursion
		(switch-to-buffer cbuf)
		(set-buffer-modified-p nil)
		(compilation-parse-errors))))))

    (save-excursion
      (set-buffer sccs-buf)
      (goto-char (point-min))
      (message
       (format "%s %s: %s" command (if fname (file-name-nondirectory fname) "")
	       (cond ((looking-at "ERROR.*$")
		      (buffer-substring (match-beginning 0)(match-end 0)))
		     ((re-search-forward "^\\([0-9.]+\\)[^0-9.]new delta \\([0-9.]+\\)[^0-9.]\\([0-9]+\\) lines" nil t)
		      (format "%s->%s %s lines"
			      (buffer-substring (match-beginning 1)(match-end 1))
			      (buffer-substring (match-beginning 2)(match-end 2))
			      (buffer-substring (match-beginning 3)(match-end 3))))
		     ((re-search-forward "^\\([0-9.]+\\)[^0-9.]\\([0-9.]+ inserted\\)[^0-9.]\\([0-9]+ deleted\\)$" nil t)
		      (format "%s  %s, %s"
			      (buffer-substring (match-beginning 1)(match-end 1))
			      (buffer-substring (match-beginning 2)(match-end 2))
			      (buffer-substring (match-beginning 3)(match-end 3))))
		     ((re-search-forward "^[0-9.]+$" nil t)
		      (buffer-substring (match-beginning 0)(match-end 0)))
		     (t (delete-other-windows)
			(split-window)
			(set-window-buffer (selected-window) sccs-buf)
			(other-window 1)
			"Type C-x 1 to remove SCCS window")))))))








(defun maybe-save ()
  "Maybe save the current file"

  (and
   (buffer-modified-p)
   (y-or-n-p (format "Save file %s? " buffer-file-name))

   (save-buffer)))
