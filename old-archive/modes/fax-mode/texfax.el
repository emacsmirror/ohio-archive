;;; texfax.el -- yet another `fax-write-region-function'.
;;;
;;; Copyright (C) 1995 Ralph Schleicher
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
;;; Author: Ralph Schleicher <rs@purple.IN-Ulm.DE>
;;; Maintainer: see the `Author' field
;;; Keywords: local comm fax
;;; Comments: You can setup this code with, for example,
;;;
;;;	(require 'texfax)
;;;
;;;	(setq fax-mode-hook
;;;	      '(lambda ()
;;;		 (setq fill-column 76))
;;;
;;;	      fax-after-setup-hook
;;;	      '(lambda ()
;;;		 (save-excursion
;;;		   (let ((start (point)))
;;;		     (insert "\\epsffile{/full/path/to/signature.ps}\n")
;;;		     (texfax-add-tex-properties start (1- (point)))))))
;;;
;;;	(setq texfax-dvips-switches '("-P" "fax"))
;;;
;;;	(setq-default sendfax-format texfax-format-string)
;;;
;;; Please set the file permissions of `signature.ps' to 0600.
;;; Time-stamp: "Sat Dec 30 10:18:32 MET 1995 rs@purple.IN-Ulm.DE"
;;; Code:


(defvar texfax-preamble "\
% This file was generated automatically by `texfax-write-region'.

\\input epsf

\\catcode`\\@=11

% Font setup.
%
% `cmtt10 scaled \\magstep1' is a standard font of Texinfo
% but you can also use `cmtt12' if you have it.
%
\\font\\FAXfont cmtt10 scaled \\magstep1

\\newfam\\FAXfam
\\textfont\\FAXfam\\FAXfont

\\def\\FAXface{%
  \\setbox\\strutbox\\hbox{\\vrule height 10\\p@ depth 4\\p@ width\\z@}%
  \\normalbaselineskip 14\\p@\\normalbaselines\\fam\\FAXfam\\FAXfont}

% Page layout.
%
\\def\\makeheadline{}
\\def\\makefootline{}

\\hoffset -1 true in \\advance\\hoffset 0.75 true in
\\voffset -1 true in \\advance\\voffset 0.75 true in

\\hsize 7.0 true in
\\vsize 9.5 true in

\\overfullrule\\z@
\\frenchspacing
\\raggedbottom

\\clubpenalty 4000
\\widowpenalty\\@M

% Macros.
%
\\def\\FAXbeginbox{\\setbox\\z@\\hbox\\bgroup\\strut}
\\def\\FAXputbox{\\egroup\\box\\z@}

\\def\\FAXverbatim{\\begingroup\\parindent\\z@\\parskip\\z@
  \\everypar{\\FAXbeginbox}\\def\\par{\\leavevmode\\FAXputbox\\endgraf}%
  \\def\\do##1{\\catcode`##1=12}\\dospecials\\catcode`\\\\=0 \\chardef\\\\=`\\\\
  \\obeylines\\obeyspaces\\FAXface\\FAXverb@tim}
{\\obeylines\\gdef\\FAXverb@tim#1^^M{\\FAXbeginbox}}
\\def\\FAXendverbatim{\\endgroup\\ignorespaces}
{\\obeyspaces\\gdef {\\ }}

\\catcode`\\@=12

% Latin-1 character encoding.
%
\\catcode`^^a1=\\active\\def ^^a1{!`}
\\catcode`^^a7=\\active\\def ^^a7{{\\S}}

\\catcode`^^b6=\\active\\def ^^b6{{\\P}}
\\catcode`^^bf=\\active\\def ^^bf{?`}

\\catcode`^^c0=\\active\\def ^^c0{\\`A}
\\catcode`^^c1=\\active\\def ^^c1{\\'A}
\\catcode`^^c2=\\active\\def ^^c2{\\^A}
\\catcode`^^c3=\\active\\def ^^c3{\\~A}
\\catcode`^^c4=\\active\\def ^^c4{\\\"A}
\\catcode`^^c5=\\active\\def ^^c5{{\\AA}}
\\catcode`^^c6=\\active\\def ^^c6{{\\AE}}
\\catcode`^^c7=\\active\\def ^^c7{\\c{C}}
\\catcode`^^c8=\\active\\def ^^c8{\\`E}
\\catcode`^^c9=\\active\\def ^^c9{\\'E}
\\catcode`^^ca=\\active\\def ^^ca{\\^E}
\\catcode`^^cb=\\active\\def ^^cb{\\\"E}
\\catcode`^^cc=\\active\\def ^^cc{\\`I}
\\catcode`^^cd=\\active\\def ^^cd{\\'I}
\\catcode`^^ce=\\active\\def ^^ce{\\^I}
\\catcode`^^cf=\\active\\def ^^cf{\\\"I}

\\catcode`^^d1=\\active\\def ^^d1{\\~N}
\\catcode`^^d2=\\active\\def ^^d2{\\`O}
\\catcode`^^d3=\\active\\def ^^d3{\\'O}
\\catcode`^^d4=\\active\\def ^^d4{\\^O}
\\catcode`^^d5=\\active\\def ^^d5{\\~O}
\\catcode`^^d6=\\active\\def ^^d6{\\\"O}
\\catcode`^^d8=\\active\\def ^^d8{{\\O}}
\\catcode`^^d9=\\active\\def ^^d9{\\`U}
\\catcode`^^da=\\active\\def ^^da{\\'U}
\\catcode`^^db=\\active\\def ^^db{\\^U}
\\catcode`^^dc=\\active\\def ^^dc{\\\"U}
\\catcode`^^dd=\\active\\def ^^dd{\\'Y}
\\catcode`^^df=\\active\\def ^^df{{\\ss}}

\\catcode`^^e0=\\active\\def ^^e0{\\`a}
\\catcode`^^e1=\\active\\def ^^e1{\\'a}
\\catcode`^^e2=\\active\\def ^^e2{\\^a}
\\catcode`^^e3=\\active\\def ^^e3{\\~a}
\\catcode`^^e4=\\active\\def ^^e4{\\\"a}
\\catcode`^^e5=\\active\\def ^^e5{{\\aa}}
\\catcode`^^e6=\\active\\def ^^e6{{\\ae}}
\\catcode`^^e7=\\active\\def ^^e7{\\c{c}}
\\catcode`^^e8=\\active\\def ^^e8{\\`e}
\\catcode`^^e9=\\active\\def ^^e9{\\'e}
\\catcode`^^ea=\\active\\def ^^ea{\\^e}
\\catcode`^^eb=\\active\\def ^^eb{\\\"e}
\\catcode`^^ec=\\active\\def ^^ec{\\`{\\i}}
\\catcode`^^ed=\\active\\def ^^ed{\\'{\\i}}
\\catcode`^^ee=\\active\\def ^^ee{\\^{\\i}}
\\catcode`^^ef=\\active\\def ^^ef{\\\"{\\i}}

\\catcode`^^f1=\\active\\def ^^f1{\\~n}
\\catcode`^^f2=\\active\\def ^^f2{\\`o}
\\catcode`^^f3=\\active\\def ^^f3{\\'o}
\\catcode`^^f4=\\active\\def ^^f4{\\^o}
\\catcode`^^f5=\\active\\def ^^f5{\\~o}
\\catcode`^^f6=\\active\\def ^^f6{\\\"o}
\\catcode`^^f8=\\active\\def ^^f8{{\\o}}
\\catcode`^^f9=\\active\\def ^^f9{\\`u}
\\catcode`^^fa=\\active\\def ^^fa{\\'u}
\\catcode`^^fb=\\active\\def ^^fb{\\^u}
\\catcode`^^fc=\\active\\def ^^fc{\\\"u}
\\catcode`^^fd=\\active\\def ^^fd{\\'y}
\\catcode`^^ff=\\active\\def ^^ff{\\\"y}\n"
  "*Text inserted at the beginning of the TeX buffer.
`texfax-preamble' should end in a newline.")

(defvar texfax-postamble "\\bye\n"
  "*Text inserted at the end of the TeX buffer.
`texfax-postamble' should end in a newline.")

(defvar texfax-prefix "\\FAXverbatim\n"
  "*Text inserted before a piece of literal text.
`texfax-prefix' should end in a newline.")

(defvar texfax-postfix "\\FAXendverbatim\n"
  "*Text inserted after a piece of literal text.
`texfax-postfix' should end in a newline.")

(defvar texfax-table '(("\\" . "\\\\"))
  "*List of cons cells of the form

    (SEARCH-STRING . REPLACEMENT-TEXT)

which will be applied to each piece of literal text.")

(defvar texfax-tex-program nil
  "*TeX program name, defaults to \"tex\".")

(defvar texfax-tex-switches nil
  "*Extra arguments when `texfax-tex-program' is invoked.")

(defvar texfax-dvips-program nil
  "*DVIPS program name, defaults to \"dvips\".")

(defvar texfax-dvips-switches nil
  "*Extra arguments when `texfax-dvips-program' is invoked.")

(defun texfax-write-region (start end file-name)
  "Generate a TeX input file from the region between START and END,
process it with TeX and DVIPS and leave the PostScript output in FILE-NAME.
Intangible text will be treated as literal TeX input while the rest of the
text will be grouped between `texfax-prefix' and `texfax-postfix'."
  (interactive "r\nFFile name: ")
  (let ((tex-dir (or (file-name-directory file-name) "./"))
	(tex-name (concat file-name ".tex"))
	(dvi-name (concat file-name ".dvi"))
	(log-name (concat file-name ".log"))
	(orig-buf (current-buffer))
	(tex-buf nil))
    (unwind-protect
	(progn
	  (setq tex-buf (generate-new-buffer " texfax"))
	  (buffer-disable-undo tex-buf)
	  (set-buffer tex-buf)
	  (cd tex-dir)
	  (insert (save-excursion
		    (set-buffer orig-buf)
		    (buffer-substring start end)))
	  (let ((limit (make-marker)))
	    (goto-char (point-min))
	    (insert (or texfax-preamble ""))
	    (while (not (eobp))
	      (set-marker limit (or (text-property-any
				     (point) (point-max) 'intangible t)
				    (point-max)))
	      (and (< (point) limit)
		   (let ((table texfax-table)
			 begin search replace)
		     (untabify (point) limit)
		     (or (= (current-column) 0)
			 (insert ?\n))
		     (insert (or texfax-prefix ""))
		     (setq begin (point))
		     (while table
		       (goto-char begin)
		       (setq search (car (car table))
			     replace (cdr (car table)))
		       (while (search-forward search limit t)
			 (replace-match replace t t))
		       (setq table (cdr table)))
		     (goto-char limit)
		     (or (= (preceding-char) ?\n)
			 (insert ?\n))
		     (insert (or texfax-postfix ""))))
	      (goto-char (or (text-property-not-all
			      (point) (point-max) 'intangible t)
			     (point-max)))
	      ;; Skip over the next newline character if it
	      ;; immediately follows an intangible text.
	      (and (not (eobp)) (looking-at "\n")
		   (forward-char 1))
	      ;; The above `text-property-not-all' fails if nothing is
	      ;; inserted into the buffer.  This sounds like a bug in
	      ;; Emacs but I haven't traced it down.
	      (insert "\\relax\n"))
	    (or (= (preceding-char) ?\n)
		(insert ?\n))
	    (insert (or texfax-postamble "")))
	  (write-region (point-min) (point-max) tex-name nil 0)
	  (or (zerop (apply 'call-process
			    (nconc (list (or texfax-tex-program "tex")
					 nil nil nil)
				   (append texfax-tex-switches
					   (list tex-name)))))
	      (error "TeX failed for some reason"))
	  (or (zerop (apply 'call-process
			    (nconc (list (or texfax-dvips-program "dvips")
					 nil nil nil)
				   (append texfax-dvips-switches
					   (list "-o" file-name dvi-name)))))
	      (error "DVIPS failed for some reason")))
      (if (file-exists-p tex-name)
	  (delete-file tex-name))
      (if (file-exists-p dvi-name)
	  (delete-file dvi-name))
      (if (file-exists-p log-name)
	  (delete-file log-name))
      (if tex-buf
	  (kill-buffer tex-buf))
      (set-buffer orig-buf))))


(require 'sendfax)

(defconst texfax-format-string "TeXfax"
  "TeXfax text format identifier.")

(defun texfax-format-setup ()
  "Basic setup formatting a fax message with `texfax-write-region'."
  (setq sendfax-write-region-hooks nil
	sendfax-write-region-function 'texfax-write-region))

(or (assoc texfax-format-string sendfax-formats)
    (sendfax-set-format texfax-format-string 'texfax-format-setup))

(defvar texfax-tex-face 'font-lock-string-face
  "*Face to use for literal TeX input.")

(defvar texfax-font-lock-keywords
  '((texfax-font-lock-tex . (0 texfax-tex-face t)))
  "Additional expressions to highlight in Fax mode.")

(defun texfax-font-lock-tex (limit)
  (let ((start (text-property-any (point) limit 'intangible t)))
    (if (not start)
	nil
      (goto-char start)
      (search-forward
       (buffer-substring (point)
			 (min (save-excursion
				(end-of-line)
				(1+ (point)))
			      (or (text-property-not-all
				   start limit 'intangible t)
				  limit)))
       limit t))))

(if (consp fax-font-lock-keywords)
    (or (assq 'texfax-font-lock-tex fax-font-lock-keywords)
	(setq fax-font-lock-keywords
	      (nconc fax-font-lock-keywords texfax-font-lock-keywords))))

(defun texfax-add-tex-properties (start end)
  "Mark the region as TeX input by adding the appropriate text properties."
  (interactive "r")
  (add-text-properties start end '(intangible t))
  (if fax-font-lock-keywords
      (let ((font-lock-verbose nil))
	(font-lock-fontify-buffer))
    (if (and window-system texfax-tex-face)
	(add-text-properties start end (list 'face texfax-tex-face)))))

(defun texfax-remove-tex-properties (start end)
  "Remove those text properties from the region which mark it as TeX input."
  (interactive "r")
  (remove-text-properties start end '(intangible nil))
  (if fax-font-lock-keywords
      (let ((font-lock-verbose nil))
	(font-lock-fontify-buffer))
    (if (and window-system texfax-tex-face)
	(remove-text-properties start end '(face default)))))


(provide 'texfax)


;;; local variables:
;;; truncate-lines: t
;;; end:

;;; texfax.el ends here
