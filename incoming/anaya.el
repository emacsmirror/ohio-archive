;;; anaya.el --- Emacs interface to the Anaya Online Spanish Dictionaries
;;;
;;; Author: Daniel M. German <dmgerman@acm.org>
;;; Keywords: dictionary, Spanish, English
;;  Created: anaya.el 2000/03/02
;;
;;     Copyright (C) 2000 Free Software Foundation, Inc.
;;   
;;     This program is free software; you can redistribute it and/or
;;     modify it under the terms of the GNU General Public License as
;;     published by the Free Software Foundation; either version 2 of
;;     the License, or (at your option) any later version.
;;     
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;     GNU General Public License for more details.
;;     
;;     You should have received a copy of the GNU General Public
;;     License along with this program; if not, write to the Free
;;     Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;;     MA 02111-1307, USA.
;;
;; Please send suggestions and bug reports to
;; <dmgerman@acm.org>. The latest version of this package
;; should be available from http://csg.uwaterloo.ca/~dmg/elisp/anaya.el
;;
;;  Based on dict.el by Eric Marsden <emarsden@mail.dotcom.fr>
;;  and  Hachette.el by Jean-Philippe Theberge

;;; Overview ==========================================================
;;
;; This module allows you to access to the Anaya Online Dictionaries
;;   http://www.anaya.es/diccionario/castellano/voxdg.html
;;   http://www.anaya.es/diccionario/castellano/voxesp.html
;;   http://www.anaya.es/diccionario/castellano/voxing.html
;;

;;; Set up ==========================================================
;;
;; Add the following lines to your .emacs
;; (autoload 'anaya "anaya" "Lookup a word or phrase in the Anaya VOX Dictionary." t)

;; (autoload 'anaya-english-spanish "anaya"   
;;   "Translate  a word in English to Spanish using using the Anaya English/Spanish dictionary." t)

;; (autoload 'anaya-spanish-english "anaya"
;;   "Translate  a word in Spanish to English using the Anaya English/Spanish dictionary." t)



;;; Changelog ==========================================================
;;
;; $Id: anaya.el,v 1.3 2000/03/02 17:28:14 dmg Exp $
;;
;; Revision 0.5  2000/03/02 12:13 dmg
;;  Created this module.
;;
;;; Code:

(eval-when-compile
  (require 'cl))

(require 'w3)
(require 'url)
(require 'w3-forms)

(defun anaya (word)
  "Lookup a word or phrase in the Anaya VOX Dictionary."
  (interactive (list (read-string "Dict lookup for: " (current-word))))
  (let ((buf (get-buffer-create "*anaya*")))
    (switch-to-buffer-other-window buf)
    (erase-buffer)
    (dict-fetch word)
    (dict-wash "Diccionario General")
    (set-buffer-modified-p nil)))

(defun anaya-english-spanish (word)
  "Translate  a word in English to Spanish using using the Anaya English/Spanish dictionary."
  (interactive (list (read-string "From English to Spanish: " (current-word))))
  (let ((buf (get-buffer-create "*anaya*")))
    (switch-to-buffer-other-window buf)
    (erase-buffer)
    (dict-fetch-english-spanish word)
    (dict-wash "Advanced English")
    (set-buffer-modified-p nil)))

(defun anaya-spanish-english (word)
  "Translate  a word in Spanish to English using the Anaya English/Spanish dictionary."
  (interactive (list (read-string "From Spanish to English: " (current-word))))
  (let ((buf (get-buffer-create "*anaya*")))
    (switch-to-buffer-other-window buf)
    (erase-buffer)
    (dict-fetch-spanish-english word)
    (dict-wash "Advanced English")
    (set-buffer-modified-p nil)))


(defun dict-fetch (word)  
  (let* ((pairs `(("Form" . "VOXDG")
                  ("Entrada" . ,word)
		  ("D" . "DAL")
		  ("MaxFilas" . "20")
		  ("HTMLForm" . "DALCorpHits.html")
		  ("TipoBusqueda" . "Emp")
		  ))
         (url-request-data (dict-form-encode pairs))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-type" . "application/x-www-form-urlencoded"))))
    (url-insert-file-contents "http://www.anaya.es/dicc/dicc/VOXDGCorpHits.html")))

(defun dict-fetch-spanish-english (word)  
  (let* ((pairs `(("Form" . "VOXESP")
                  ("Entrada" . ,word)
		  ("D" . "DAL")
		  ("MaxFilas" . "20")
		  ("HTMLForm" . "DALCorpHits.html")
		  ("TipoBusqueda" . "Emp")
		  ))
         (url-request-data (dict-form-encode pairs))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-type" . "application/x-www-form-urlencoded"))))
    (url-insert-file-contents "http://www.anaya.es/dicc/dicc/VOXESPCorpHits.html")))

(defun dict-fetch-english-spanish (word)  
  (let* ((pairs `(("Form" . "VOXING")
                  ("Entrada" . ,word)
		  ("D" . "DAL")
		  ("MaxFilas" . "20")
		  ("HTMLForm" . "DALCorpHits.html")
		  ("TipoBusqueda" . "Emp")
		  ))
         (url-request-data (dict-form-encode pairs))
         (url-request-method "GET")
         (url-request-extra-headers
          '(("Content-type" . "application/x-www-form-urlencoded"))))
    (url-insert-file-contents "http://www.anaya.es/dicc/dicc/VOXINGCorpHits.html")))

(defun dict-wash (page-title)
  (goto-char (point-min))
  (let ((case-fold-search t))
    (when (re-search-forward page-title nil t)
      (delete-region (point-min) (match-beginning 0)))
    (goto-char (point-max))
    (when (re-search-backward "<!\\-\\-<HR>\\-\\->" nil t)
      (delete-region (point-max) (match-beginning 0)))
    (goto-char (point-min))
    (while (re-search-forward "<[^>]+>" nil t)
      (replace-match "" t))
    (replace-from-table sgml-entities)
    (goto-char (point-min))
    ))

;; from nnweb.el
(defun dict-form-encode (pairs)
  "Return PAIRS encoded for forms."
  (mapconcat
   (function
    (lambda (data)
      (concat (w3-form-encode-xwfu (car data)) "="
	      (w3-form-encode-xwfu (cdr data)))))
   pairs "&"))

(defconst sgml-entities
  '(
    ("&Aacute;" "Á")
    ("&Agrave;" "À")
    ("&Acirc;" "Â")
    ("&Auml;" "Ä")
    ("&Ccedil;" "Ç")
    ("&Egrave;" "È")
    ("&Ecirc;" "Ê")
    ("&Eacute;" "É")
    ("&Euml;" "Ë")
    ("&Iacute;" "Í")
    ("&Igrave;" "Ì")
    ("&Iuml;" "Ï")
    ("&Ntilde;" "Ñ")
    ("&Ograve;" "Ò")
    ("&Ocirc;" "Ô")
    ("&Ouml;" "Ö")
    ("&Ugrave;" "Ù")
    ("&Ucirc;" "Û")
    ("&Uuml;" "Ü")
    ("&aacute;" "á")
    ("&agrave;" "à")
    ("&acirc;" "â")
    ("&auml;" "ä")
    ("&ccedil;" "ç")
    ("&egrave;" "è")
    ("&eacute;" "é")
    ("&ecirc;" "ê")
    ("&euml;" "ë")
    ("&iacute;" "í")
    ("&igrave;" "ì")
    ("&icirc;" "î")
    ("&iuml;" "ï")
    ("&nbsp;" " ")
    ("&ntilde;" "ñ")
    ("&ograve;" "ò")
    ("&ocirc;" "ô")
    ("&ouml;" "ö")
    ("&ugrave;" "ù")
    ("&ucirc;" "û")
    ("&uuml;" "ü")
    ("&#171;" "«")
    ("&#187;" "»")
    ("&copy;" "©")))

(defun replace-from-table (lst)
  "Use the alist to search and replace a whole buffer.
Argument LST Associative list of string and replacement-string."
  (let ((case-fold-search nil))
    (cond ((null lst) lst)
	  (t (progn 
	       (let ((pos (point-marker)))
		 (goto-char (point-min))
		 (while (re-search-forward (car (car lst)) nil t)
		   (replace-match (car (cdr (car lst))) t t))
		 (goto-char pos))
  	       (replace-from-table (cdr lst)))))))



(provide 'anaya)

;; anaya.el ends here


