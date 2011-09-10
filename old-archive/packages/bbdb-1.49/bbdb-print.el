;;; This is bbdb-print.el, version 2.3.
;;;
;;; The Insidious Big Brother Database is free software; you can redistribute
;;; it and/or modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 1, or (at your
;;; option) any later version.
;;;
;;; BBDB is distributed in the hope that it will be useful, but WITHOUT ANY
;;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;; details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;; This module is for printing BBDB databases using TeX.
;;; As a special bonus, it includes bbdb-phone, for searching for people by
;;; their phone numbers.
;;;
;;; USE:  In the *BBDB* buffer, type P to convert the listing to TeX format.
;;;       It will prompt you for a filename.  Then run TeX on that file and
;;;       print it out.
;;;
;;;       Bbdb-print understands one new bbdb field: tex-name.  If it
;;;       exists, this will be used for the printed listing instead of the name
;;;       field of that record.  This is designed for entering names with lots
;;;       of accents that would mess up mailers, or when for any reason you
;;;       want the printed version of the name to be different from the version
;;;       that appears on outgoing mail and in the *BBDB* buffer.  You may want
;;;       to add tex-name to bbdb-elided-display so you only see it in the
;;;       printout.  tex-name is exempted from the usual special-character
;;;       quoting done by bbdb-print; it is used verbatim.
;;;
;;; INSTALLATION: Put this file somewhere on your load-path.
;;;       Put bbdb-print.tex somewhere on your TEXINPUTS path, or put
;;;         its absolute pathname in bbdb-print-format-file-name.
;;;       Put (require 'bbdb-print) in your .emacs, or autoload it.
;;;
;;; This program was written by Boris Goldowsky <boris@psych.rochester.edu> and
;;; Dirk Grunwald <grunwald@cs.colorado.edu> using a TeX format given to Dirk
;;; by someone at Berkeley.  We are also grateful to numerous people on the
;;; info-bbdb mailing list for suggestions and bug reports.

(require 'bbdb)
(require 'bbdb-com)

(define-key bbdb-mode-map "P" 'bbdb-print)

;;;
;;; Variables
;;;

(defvar bbdb-print-file-name "~/bbdb.tex"
  "*Default file name for printouts of BBDB database.")

(defvar bbdb-print-format-file-name "bbdb-print.tex"
  "*Name of file containing TeX formatting commands
for printing BBDB databases.  If this filename is not absolute, the file must
be located somewhere that TeX will find it.")

(defvar bbdb-print-elide '(tex-name aka mail-alias nic nic-updated)
  "*List of fields NOT to print
when printing an address list.  See also bbdb-print-no-bare-names.")

(defvar bbdb-print-no-bare-names t
  "*If nonnil, `bare names' will not be printed.
A name is bare if the record contains no non-elided fields other than
name and company \(see bbdb-print-elide).")

(defvar bbdb-print-prolog 
  (concat "%%%% ====== Phone/Address list in -*-TeX-*- Format =====\n"
	  "%%%%        produced by bbdb-print, version 2.3\n"
	  "\\input " bbdb-print-format-file-name "\n\n")
  "*TeX statements to include at the beginning of the bbdb-print file.")

(defvar bbdb-print-epilog "\\vfill\\byecolumns\\bye\n"
  "*TeX statements to include at the end of the bbdb-print file.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bbdb-print (to-file)
  "Print the selected BBDB entries"
  (interactive (list (read-file-name "Print To File: " bbdb-print-file-name)))
  (setq bbdb-print-file-name (expand-file-name to-file))
  (let ((current-letter t)
	(records (progn (set-buffer bbdb-buffer-name)
			bbdb-records)))
    (find-file bbdb-print-file-name)
    (delete-region (point-min) (point-max))
    (while records
      (setq current-letter 
	    (bbdb-print-format-record (car (car records)) current-letter))
      (setq records (cdr records)))
    (goto-char (point-min)) (insert bbdb-print-prolog)
    (goto-char (point-max)) (insert bbdb-print-epilog)
    (goto-char (point-min))))

(defun bbdb-print-format-record (record &optional current-letter brief)
  "Insert the bbdb RECORD in TeX format.
Optional CURRENT-LETTER is the section we're in -- if this is non-nil and
the first letter of the sortkey of the record differs from it, a new section
heading will be output \(an arg of t will always produce a heading).
The new current-letter is the return value of this function.
Someday, optional third arg BRIEF will produce one-line format."
  (bbdb-debug (if (bbdb-record-deleted-p record)
		  (error "plus ungood: tex formatting deleted record")))
  
      (let* ((bbdb-elided-display bbdb-print-elide)
	     (first-letter 
	      (substring (concat (bbdb-record-sortkey record) "?") 0 1))
	     (name   (and (bbdb-field-shown-p 'name)
			  (or (bbdb-record-getprop record 'tex-name)
			      (bbdb-print-tex-quote
			       (bbdb-record-name record)))))
	     (comp   (and (bbdb-field-shown-p 'company)
			  (bbdb-record-company record)))
	     (net    (and (bbdb-field-shown-p 'net)
			  (bbdb-record-net record)))
	     (phones (and (bbdb-field-shown-p 'phone)
			  (bbdb-record-phones record)))
	     (addrs  (and (bbdb-field-shown-p 'address)
			  (bbdb-record-addresses record)))
	     (notes  (bbdb-record-raw-notes record))
	     (begin (point))
	     (bare t))

	;; Section header, if neccessary.

	(if (and current-letter
		 (not (string-equal first-letter current-letter)))
	    (insert (format "\\separator{%s}\n\n" (bbdb-print-tex-quote
						   (upcase first-letter)))))

	;; Name and Company

	(insert (format "\\beginrecord\n\\name{%s}\n"
			(or name comp "")))

	(if (and name comp)
	    (insert (format "\\comp{%s}\n" (bbdb-print-tex-quote comp))))

	;; Phone numbers

	(while phones
	  (let ((place (aref (car phones) 0))
		(number (bbdb-phone-string (car phones))))
	    (setq bare nil)
	    (insert (format "\\phone{%s%s}\n" 
			    (bbdb-print-tex-quote 
			     (bbdb-print-if-not-blank place ": "))
			    (bbdb-print-tex-quote number)))
	    (setq phones (cdr phones))))

	;; Email address -- just list their first address.
	;;  Make all dots legal line-breaks.

	(if net
	    (let ((net-addr (bbdb-print-tex-quote (car net)))
		  (start 0))
	      (while (string-match "\\." net-addr start)
		(setq net-addr
		      (concat (substring net-addr 0 (match-beginning 0))
			      ".\\-"
			      (substring net-addr (match-end 0))))
		(setq start (+ 2 (match-end 0))))
	      (setq bare nil)
	      (insert (format "\\email{%s}\n" net-addr))))

	;; Addresses

	(while addrs
	  (let ((addr (car addrs)))
	    (setq bare nil)
	    (insert
	     (format 
	      "\\address{%s}\n"
	      (bbdb-print-tex-quote 
	       (concat 
		(bbdb-print-if-not-blank (bbdb-address-street1 addr) "\\\\\n")
		(bbdb-print-if-not-blank (bbdb-address-street2 addr) "\\\\\n")
		(bbdb-print-if-not-blank (bbdb-address-street3 addr) "\\\\\n")
		(bbdb-address-city addr)
		(if (and (not (equal "" (bbdb-address-city addr)))
			 (not (equal "" (bbdb-address-state addr))))
		    ", ")
		(bbdb-print-if-not-blank (bbdb-address-state addr) " ")
		(bbdb-address-zip-string addr)
		"\\\\")))))
	  (setq addrs (cdr addrs)))

	;; Notes

	(if (stringp notes)
	    (setq notes (list (cons 'notes notes))))
	(while notes
	  (let ((thisnote (car notes)))
	    (if (bbdb-field-shown-p (car thisnote))
		(progn
		  (setq bare nil)
		  (if (eq 'notes (car thisnote))
		      (insert (format "\\notes{%s}\n" (bbdb-print-tex-quote 
						       (cdr thisnote))))
		    (insert (format "\\note{%s}{%s}\n" 
				    (bbdb-print-tex-quote (symbol-name
							   (car thisnote)))
				    (bbdb-print-tex-quote (cdr thisnote))))))))
	  (setq notes (cdr notes)))

	;; If record is bare, delete anything we may have inserted.
	;; otherwise, mark the end of this record.

	(if bare
	    (delete-region begin (point))
	  (insert "\\endrecord\n\n")
	  (setq current-letter first-letter))
	current-letter))

(defun bbdb-print-tex-quote (string)
  "Quote any unquoted TeX special characters that appear in STRING.
In other words, # alone will be replaced by \#, but \^ will be left for 
TeX to process as an accent."
  (if string
      (save-excursion
	(set-buffer (get-buffer-create " bbdb-print-tex-quote"))
	(delete-region (point-min) (point-max))
	(insert string)
	(goto-char (point-min))
	(while (not (eobp))
	  (cond ((looking-at "[<>=]+") 
		 (replace-match "$\\&$"))
		((and (looking-at "[#$%&~_]")
		      (not (eq ?\\ (char-after (1- (point))))))
		 (replace-match "\\\\\\&"))
		((and (looking-at "[{}]")
		      (not (eq ?\\ (char-after (1- (point))))))
		 (replace-match "$\\\\\\&$"))
		((and (looking-at "\\^")
		      (not (eq ?\\ (char-after (1- (point))))))
		 (replace-match "\\\\^{ }"))
		(t (forward-char 1))))
	(buffer-string))))

(defun bbdb-print-if-not-blank (string &rest more)
  "If STRING is not null, then return it concatenated
with rest of arguments.  If it is null, then all arguments are 
ignored and the null string is returned."
  (if (or (null string) (equal "" string))
      ""
    (apply 'concat string more)))

(defun bbdb-phone (string elidep)
  "Display all entries in the bbdb database whose phone numbers match
the regexp STRING.  Matches against the formatted version of the phone number,
so that, eg, `(212)' will match only people who have a phone number in the 212
area code.  If STRING is null, will match all entries that have a phone
number listed."
  (interactive "sRegular Expression: \nP")
  (let ((bbdb-elided-display (bbdb-grovel-elide-arg elidep)))
    (bbdb-display-records
     (let ((records (bbdb-records))
	   (matches '())
	   (case-fold-search bbdb-case-fold-search))
       (while records
	 (let* ((record (car records))
		(phones (bbdb-record-phones record)))
	   (while phones
	     (if (string-match string (bbdb-phone-string (car phones)))
		 (setq matches (cons record matches)
		       phones nil)
	       (setq phones (cdr phones)))))
	 (setq records (cdr records)))
       (nreverse matches)))))

(provide 'bbdb-print)
