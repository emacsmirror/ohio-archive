;; iso-sgml.el --- display SGML entity references as ISO 8859-1 characters

;; Copyright (C) 1994 Frederic Lepied

;; Author: Frederic Lepied <lepied@cenaath.cena.dgac.fr>
;; Maintainer: lepied@cenaath.cena.dgac.fr
;; Keywords: SGML, HTML, ISO, Latin, i18n
;; Status: Works with emacs 19.24
;; Created: 1994-06-21
;; Last Modified By: Frederic Lepied [STERIA SIT] 69577103
;; Last Modified On: Wed Dec  7 10:14:41 1994
;; Update Count: 5

;; LCD Archive Entry:
;; iso-sgml|Frederic Lepied|lepied@cenaath.cena.dgac.fr|
;; Edit SGML or HTML buffers with ISO 8859-1 (Latin-1) display|
;; 10-May-1995|1.4|~/misc/iso-sgml.el.Z|

;; $Id: iso-sgml.el,v 1.3 1994/12/07 09:08:07 lepied Exp lepied $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 675 Massachusetts Avenue; Cambridge, MA 02139, USA.

;; Commentary:
;; Based on iso-cvt.el from Michael Gschwind <mike@vlsivie.tuwien.ac.at>,
;; iso-sgml.el transparently displays entity references in SGML or HTML
;; buffers as ISO 8859-1 (aka Latin-1) characters.

;; SEE ALSO:
;; iso-cvt.el
;; If you are interested in questions related to using the ISO 8859-1 
;; characters set (configuring emacs, Unix, etc. to use ISO), then you
;; can get the ISO 8859-1 FAQ via anonymous ftp from 
;; ftp.vlsivie.tuwien.ac.at in /pub/bit/FAQ-ISO-8859-1

;; INSTALLATION:
;; add the following line to your .emacs :
;; (load "iso-sgml")
;; If you want it to work with other modes change the value of the
;; the variable isosgml-modes-list like this :
;; (setq isosgml-modes-list '(my-mode))

;; Code:

(defconst isosgml-version "$Id: iso-sgml.el,v 1.3 1994/12/07 09:08:07 lepied Exp lepied $"
  "iso-sgml RCS version number")

(defvar isosgml-modes-list '(html-mode html-helper-mode sgml-mode)
  "*List of modes to translate between SGML or HTML entity references
 and the ISO 8859-1 character set.")


(defun isosgml-translate-conventions (trans-tab)
  "Use the translation table argument to translate the current buffer."
  (save-excursion
    (let ((beg (point-min-marker))    ; see the `(elisp)Narrowing' Info node
	  (end (point-max-marker)))
      (unwind-protect
	  (progn
	    (widen)
	    (goto-char (point-min))
	    (let ((buffer-read-only nil) ; (inhibit-read-only t)?
		  (case-fold-search nil))
	      (while trans-tab
		(save-excursion
		  (let ((trans-this (car trans-tab)))
		    (while (search-forward (car trans-this) nil t)
		      (replace-match (car (cdr trans-this)) t t)))
		  (setq trans-tab (cdr trans-tab))))))
	(narrow-to-region beg end)))))

(defvar sgml2iso-trans-tab
  '(
    ("&AElig\;"  "Æ")
    ("&Aacute\;"  "Á")
    ("&Acirc\;"  "Â")
    ("&Agrave\;"  "À")
    ("&Atilde\;"  "Ã")
    ("&Ccedil\;"  "Ç")
    ("&Eacute\;"  "É")
    ("&Egrave\;"  "È")
    ("&Euml\;"  "Ë")
    ("&Iacute\;"  "Í")
    ("&Icirc\;"  "Î")
    ("&Igrave\;"  "Ì")
    ("&Iuml\;"  "Ï")
    ("&Ntilde\;"  "Ñ")
    ("&Oacute\;"  "Ó")
    ("&Ocirc\;"  "Ô")
    ("&Ograve\;"  "Ò")
    ("&Oslash\;"  "Ø")
    ("&Uacute\;"  "Ú")
    ("&Ugrave\;"  "Ù")
    ("&Yacute\;"  "Ý")
    ("&aacute\;"  "á")
    ("&acirc\;"  "â")
    ("&aelig\;"  "æ")
    ("&agrave\;"  "à")
    ("&aring\;"  "å")
    ("&atilde\;"  "ã")
    ("&ccedil\;"  "ç")
    ("&eacute\;"  "é")
    ("&ecirc\;"  "ê")
    ("&egrave\;"  "è")
    ("&euml\;"  "ë")
    ("&iacute\;"  "í")
    ("&icirc\;"  "î")
    ("&igrave\;"  "ì")
    ("&iuml\;"  "ï")
    ("&ntilde\;"  "ñ")
    ("&oacute\;"  "ó")
    ("&ocirc\;"  "ô")
    ("&ograve\;"  "ò")
    ("&oslash\;"  "ø")
    ("&otilde\;"  "õ")
    ("&uacute\;"  "ú")
    ("&ucirc\;"  "û")
    ("&ugrave\;"  "ù")
    ("&yacute\;"  "ý")
    ("&Auml\;"  "Ä")
    ("&auml\;"  "ä")
    ("&Ouml\;"  "Ö")
    ("&ouml\;"  "ö")
    ("&Uuml\;"  "Ü")
    ("&uuml\;"  "ü")
    ("&szlig\;"  "ß")
    ("&sect\;"  "§")
    ("&para\;"  "¶")
    ("&copy\;"  "©")
    ("&iexcl\;"  "¡")
    ("&iquest\;"  "¿")
    ("&cent\;"  "¢")
    ("&pound\;"  "£")
    ("&times\;"  "×")
    ("&plusmn\;"  "±")
    ("&divide\;"  "÷")
    ("&not\;"  "¬")
    ("&mu\;"  "µ")
    ("&Ae\;"  "Ä")
    ("&ae\;"  "ä")
    ("&Oe\;"  "Ö")
    ("&oe\;"  "ö")
    ("&Ue\;"  "Ü")
    ("&ue\;"  "ü")
    ("&sz\;"  "ß")
   )
  "Translation table from SGML entity references to ISO 8859-1 characters.")

(defun fix-sgml2iso ()
  "Replace SGML entity references with ISO 8859-1 (aka Latin-1) characters."
  (interactive)
  (if (member major-mode isosgml-modes-list)
      (let ((buffer-modified-p (buffer-modified-p)))
	  (unwind-protect
	      (isosgml-translate-conventions sgml2iso-trans-tab)
	    (set-buffer-modified-p buffer-modified-p)))))

(defvar iso2sgml-trans-tab
  (mapcar (function (lambda (entity-char) ; (ENTITY CHAR)
 		      ;; Return (CHAR ENTITY)
 		      (list (car (cdr entity-char))
 			    (car entity-char))))
 	  sgml2iso-trans-tab)
   "Translation table from ISO 8859-1 characters to SGML entity references.")

(defun fix-iso2sgml ()
  "Replace ISO 8859-1 (aka Latin-1) characters with SGML entity references."
  (interactive)
  (if (member major-mode isosgml-modes-list)
      (let ((buffer-modified-p (buffer-modified-p)))
	  (unwind-protect
	      (isosgml-translate-conventions iso2sgml-trans-tab)
	    (set-buffer-modified-p buffer-modified-p)))))


(add-hook 'find-file-hooks 'fix-sgml2iso)
(add-hook 'write-file-hooks 'fix-iso2sgml)
(add-hook 'after-save-hook 'fix-sgml2iso)

(provide 'iso-sgml)

;; iso-sgml.el ends here

; $Log: iso-sgml.el,v $
; Revision 1.4  1995/05/10  06:19:41  lepied
; 	* protect code with unwind-protect to prevent errors
;
; Revision 1.3  1994/12/07  09:08:07  lepied
; Thanks to kevinr@ihs.com (Kevin Rodgers)
; 	* replace regular expression search with normal one
; 	* cleanup interactive use
;
; Revision 1.2  1994/11/24  06:49:08  lepied
; Integrated patch from kevinr@ihs.com (Kevin Rodgers) :
;
; 	* iso-sgml.el (sgml2iso-trans-tab): Delete backslash (`\') from
; 	"&plusmn;" entity reference.
;
; 	* iso-sgml.el (file header, library header (Keywords), LCD
; 	Archive Entry (description) [comment blocks]): Properly refer to
; 	SGML entity references; uppercase acronyms (ISO, SGML, HTML);
; 	capitalize Latin.
; 	(Commentary [comment block]): Rewrite as a complete sentence.
; 	(sgml2iso-trans-tab, fix-sgml2iso [doc strings]): Properly refer
; 	to SGML entity references.
; 	(iso2sgml-trans-tab, fix-iso2sgml [doc strings]): Properly refer
; 	to SGML entity references.
;
; 	* iso-sgml.el (iso2sgml-trans-tab): Initialize by
; 	programmatically reversing elements of sgml2iso-trans-tab,
; 	instead of hand-coding each element.
;
; Revision 1.1  1994/06/22  15:15:13  lepied
; Initial revision
;
