;;; dict.el --- Emacs interface to the Online Dictionary Database
;;;
;;; Author: Eric Marsden <emarsden@mail.dotcom.fr>
;;; Keywords: dictionary
;;
;;     Copyright (C) 1999  Eric Marsden
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
;; <emarsden@mail.dotcom.fr>. The latest version of this package
;; should be available from
;;
;;     <URL:http://www.chez.com/emarsden/downloads/>


;;; Overview ==========================================================
;;
;; This module allows you to access the Online Dictionary Database at
;; dict.org. To use it, type `M-x dict', which will prompt you for a
;; word or phrase to search for in the online dictionary (defaulting
;; to the word under the cursor). dict.el requires w3 (the Emacs web
;; browser) to be installed. If you are a heavy user of this software
;; you might consider using Torsten Hilbrich's dictionary package at
;; <URL:http://www.in-berlin.de/User/myrkr/dictionary.html>, which
;; allows you to connect to a local dictd server.
;;
;; If you wish definitions to be displayed in a dedicated frame, you
;; might use the following in your ~/.emacs:
;;
;;    (push "*dict*" special-display-buffer-names)


;;; Code:

(eval-when-compile
  (require 'cl))

(require 'w3)
(require 'url)
(require 'w3-forms)


(defun dict (word)
  "Lookup a word or phrase in the Online Dictionary Database."
  (interactive (list (read-string "Dict lookup for: " (current-word))))
  (let ((buf (get-buffer-create "*dict*")))
    (switch-to-buffer-other-window buf)
    (erase-buffer)
    (dict-fetch word)
    (dict-wash)
    (set-buffer-modified-p nil)))

(defun dict-fetch (word)  
  (let* ((pairs `(("Form" . "Dict1")
                  ("Query" . ,word)
                  ("Strategy" . "*")
                  ("Database" . "*")))
         (url-request-data (dict-form-encode pairs))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-type" . "application/x-www-form-urlencoded"))))
    (url-insert-file-contents "http://www.dict.org/bin/Dict")))

(defun dict-wash ()
  (goto-char (point-min))
  (let ((case-fold-search t))
    (when (re-search-forward "</form>" nil t)
      (delete-region (point-min) (match-beginning 0)))
    (goto-char (point-max))
    (when (re-search-backward "</pre>" nil t)
      (delete-region (point-max) (match-beginning 0)))
    (goto-char (point-min))
    (while (re-search-forward "<[^>]+>" nil t)
      (replace-match "" t))))

;; from nnweb.el
(defun dict-form-encode (pairs)
  "Return PAIRS encoded for forms."
  (mapconcat
   (function
    (lambda (data)
      (concat (w3-form-encode-xwfu (car data)) "="
	      (w3-form-encode-xwfu (cdr data)))))
   pairs "&"))

(provide 'dict)

;; dict.el ends here
