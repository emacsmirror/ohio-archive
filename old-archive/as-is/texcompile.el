;To: unix-emacs@bbn.com
;Date: 27 Apr 89 18:59:06 GMT
;From: Jacob Gore <mailrus!shadooby!accuvax.nwu.edu!nucsrl!gore@bbn.com>
;Sender: arpa-unix-emacs-request@bbn.com
;Subject: Re: [FB]inding new and useful GNU commands (was Re: Line numbers)
;References: <39168@bbn.COM>
;Organization: Northwestern U, Evanston IL, USA
;Source-Info:  From (or Sender) name not authenticated.
;
;>(1) run TeX non-interactively:
;>
;>\scrollmode or \batchmode near the top of the file;
;>
;>(2) write the elisp code (non-trivial, I fear) to make next-error work
;>on TeX error output.
;
;Non-trivial, but not that hard, either.  I'm attaching mine.
;
;>If you undertake #2, you might want to reorganize it to scan the
;>*shell* buffer instead, then you don't have to do #1, and can run TeX
;>interactively if you feel like it (or do part of it interactively).
;
;Yeah, I probably should have done that.  Well, as they say, "feel free to
;change it."  
;
;Jacob Gore				Gore@EECS.NWU.Edu
;Northwestern Univ., EECS Dept.		{oddjob,chinet,att}!nucsrl!gore
;
;------------------------------
;;; This code is derived from GNU's compile.el, and is thus subject to
;;; the GNU Emacs General Public License.

(defun compilation-parse-errors ()
  "Parse the current buffer as error messages.
This makes a list of error descriptors, compilation-error-list.
For each source-file, line-number pair in the buffer,
the source file is read in, and the text location is saved in
compilation-error-list.  The function next-error, assigned to
\\[next-error], takes the next error off the list and visits its location.

This version of this function works on TeX compilations only.  It is
necessary for that purpose, since TeX does not put file names on the
same line as line numbers for the errors."
  (setq compilation-error-list nil)
  (message "Parsing error messages...")
  (modify-syntax-entry ?\{ "_")
  (modify-syntax-entry ?\} "_")
  (modify-syntax-entry ?\[ "_")
  (modify-syntax-entry ?\] "_")
  (make-variable-buffer-local 'compilation-error-regexp)
  (setq compilation-error-regexp "^l\.[0-9]+ ")
  (let (text-buffer
	last-filename last-linenum)
    ;; Don't reparse messages already seen at last parse.
    (goto-char compilation-parsing-end)
    ;; Don't parse the first two lines as error messages.
    ;; This matters for grep.
    (if (bobp)
	(forward-line 2))
    (while (re-search-forward compilation-error-regexp nil t)
      (let (linenum filename
	    error-marker text-marker)
	;; Extract file name and line number from error message.
	;; Line number is 2 away from beginning of line: "l.23"
	(beginning-of-line)
	(goto-char (+ (point) 2))
	(setq linenum (read (current-buffer)))
	;; The file is the one that was opened last and is still open.
	;; We need to find the last open parenthesis.
	(insert ?\))
	(backward-sexp)
	(forward-char)
	(setq filename (compilation-grab-filename))
	;; Locate the erring file and line.
	(if (and (equal filename last-filename)
		 (= linenum last-linenum))
	    nil
	  (skip-chars-backward "^(")
	  (backward-char)
	  (forward-sexp)
	  (backward-delete-char 1)
	  (setq error-marker (point-marker))
	  ;; text-buffer gets the buffer containing this error's file.
	  (if (not (equal filename last-filename))
	      (setq text-buffer
		    (and (file-exists-p (setq last-filename filename))
			 (find-file-noselect filename))
		    last-linenum 0))
	  (if text-buffer
	      ;; Go to that buffer and find the erring line.
	      (save-excursion
		(set-buffer text-buffer)
		(if (zerop last-linenum)
		    (progn
		      (goto-char 1)
		      (setq last-linenum 1)))
		(forward-line (- linenum last-linenum))
		(setq last-linenum linenum)
		(setq text-marker (point-marker))
		(setq compilation-error-list
		      (cons (list error-marker text-marker)
			    compilation-error-list)))))
	(forward-line 1)))
    (setq compilation-parsing-end (point-max)))
  (message "Parsing error messages...done")
  (setq compilation-error-list (nreverse compilation-error-list)))

