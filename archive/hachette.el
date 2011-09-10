;; Hachette.el --- Lookup a word on the Hachette's online french dictionary.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Lisp Archive Entry
;; Filename: hachette.el
;; Author: Jean-Philippe Theberge (jphil@emacslisp.org)
;; Version: 1.5
;; Created: 24/02/2000 - update: 01/08/2000
;; Keywords: Dictionaire, Hachette, French dictionary
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copyright (c) 1998 - 1999 Free Software Foundation, Inc.
;;
;; This file is not part of GNU Emacs. :-(
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary:
;; 
;; Initialy based on dict-web.el by Eric Marsden
;;
;; The definition come from the online dictionary at
;; http://www.francophonie.hachette-livre.fr
;; and is copyrighted by them.
;;
;; Version History
;; 1.0  Initial implementation
;; 1.1  Cleanup and commented for gnu.emacs.source posting
;; 1.2  Removed dependencies on w3
;; 1.3  Some bad codes reworked
;; 1.4  Some debuging.  Change in hachette-fetch-word-at-point
;;
;; TO DO
;;
;; - make it (optionaly) asynchronous so users with slow connection can
;;   continue working while the definition is being fetched.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; To use:
;;
;; M-x hachette
;; Prompt for a word (suggesting word at point)
;;
;; M-x hachette-fetch-wotrd-at-point
;; Lookup word at point without prompting.
;; If hachette-use-language-guessing is t and
;; the buffer seem to be in english,
;; using dict-web is proposed.

;;; Code:
(require 'derived)
(require 'cl)

(defconst hachette-version "1.5")
(defvar hachette-use-language-guessing t)

(defconst sgml-entities
  '(("&Agrave;" "À")
    ("&Acirc;" "Â")
    ("&Auml;" "Ä")
    ("&Ccedil;" "Ç")
    ("&Egrave;" "È")
    ("&Ecirc;" "Ê")
    ("&Eacute;" "É")
    ("&Euml;" "Ë")
    ("&Igrave;" "Ì")
    ("&Iuml;" "Ï")
    ("&Ograve;" "Ò")
    ("&Ocirc;" "Ô")
    ("&Ouml;" "Ö")
    ("&Ugrave;" "Ù")
    ("&Ucirc;" "Û")
    ("&Uuml;" "Ü")
    ("&agrave;" "à")
    ("&acirc;" "â")
    ("&auml;" "ä")
    ("&ccedil;" "ç")
    ("&egrave;" "è")
    ("&eacute;" "é")
    ("&ecirc;" "ê")
    ("&euml;" "ë")
    ("&igrave;" "ì")
    ("&icirc;" "î")
    ("&iuml;" "ï")
    ("&ograve;" "ò")
    ("&ocirc;" "ô")
    ("&ouml;" "ö")
    ("&ugrave;" "ù")
    ("&ucirc;" "û")
    ("&uuml;" "ü")
    ("&#171;" "«")
    ("&#187;" "»")
    ("&copy;" "©")))

(defconst hachette-server "www.francophonie.hachette-livre.fr")

(define-derived-mode hachette-definition-mode text-mode "Hachette"
  "Major mode to display definition fetched from the Hachette online dictionary.
Special commands:
\\{hachette-definition-mode-map}"
  (setq hachette-definition-font-lock-keywords
    (list
     '("^[0-9]+\." . font-lock-keyword-face)
     '("^\\*\\* \\([a-z]*\\) " 1 font-lock-function-name-face)
     '("\\(||\\|--\\)" . font-lock-function-name-face)
     '(" \\(MATH\\|TECH\\|SC NAT\\|DR\\|syn\\.\\|fig\\.\\|ant\\.\\|Loc\\.\\|ASTRO\\|Ellipt\\.\\|Encycl\\.\\|n\\. +Fam\\.\\|Litt\\.\\) " . font-lock-reference-face)
     '("\\(Dict\\(.\\|
\\)*interdite\\.\\)" 1 font-lock-comment-face prepend))) ; Merci a Edric Ellis!
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(hachette-definition-font-lock-keywords nil t)))

(define-key hachette-definition-mode-map [return] 'hachette-fetch-word-at-point)
(define-key hachette-definition-mode-map "q" 'kill-buffer-and-window)

(defun hachette (word)
  "Lookup a WORD on the Hachette's online french dictionary."
  (interactive (list (read-string "Mot: " (current-word))))
  (let ((wordL (hachette-fetch-wordL word)))
    (cond ((equal (length wordL) 1)(hachette-fetch-def (cdar wordL)))
	  ((null wordL)(message "Aucune definition trouvée."))
	  (t (progn
	       (require 'widget)
	       (require 'cl)
	       (pop-to-buffer "*Hachette-liste*")
	       (erase-buffer)
	       (widget-minor-mode 1)
	       (mapcar
		(lambda (x)
		  (lexical-let ((x x))
		    (widget-create 'push-button
				   :notify (lambda  (widget &rest ignore)
					     (kill-buffer "*Hachette-liste*")
					     (hachette-fetch-def (cdr x)))
				   (car x))
		    (widget-insert "\n")))
		wordL)
	       (widget-setup))))))

(defun hachette-insert-url-contents (host path)
  "Send a http request to HOST for the page PATH and insert the contents at point."
  (ignore-errors (kill-buffer "*hachette-temp-buffer*"))
  (let* ((coding-system-for-read 'binary)
         (coding-system-for-write 'binary)
         (http (open-network-stream
                "hachette-retrieval-process"
		"*hachette-temp-buffer*"
                host
		80))
	 (pbuf (process-buffer http)))
    (process-send-string
     http (concat "GET " path " HTTP/1.0\r\n\r\n"))
    (while (eq (process-status http) 'open)
      (sleep-for 1))
    (insert-buffer pbuf)
    (kill-buffer pbuf)))

(defun hachette-fetch-word-at-point ()
  "Lookup the word at point on the Hachette's online french dictionary."
  (interactive)
  (require 'thingatpt)
  (let ((word (thing-at-point 'word)))
    (if (null word) (error "No word selected"))
    (if (and hachette-use-language-guessing
	     (locate-library "dict-web")
	     (equal (guess-buffer-language) "en"))
	(if (y-or-n-p "Le buffer me semble en anglais, utiliser dict-web a la place? ")
	    (progn
	      (require 'dict-web)
	      (dict word))
	  (hachette word))
      (hachette word))))

(defun guess-buffer-language ()
  ;; This defun is very basic and must be improved.
  "Try to guess if the buffer is in french or english."
  (save-excursion
    (goto-char (point-min))
    (let* ((french-stop-words  " \\(et\\|ou\\|[ld]es\\|que\\) ")
	   (english-stop-words " \\(of\\|the\\|and\\|or\\) ")
	   (en (string-to-number (count-matches english-stop-words)))
	   (fr (string-to-number (count-matches french-stop-words))))
      (if (> en fr)
	  "en"
	"fr"))))

(defun hachette-fetch-wordL (word)
  "Return an Alist of words matching WORD and the id required to fetch the definition."
  (with-temp-buffer
    (hachette-insert-url-contents hachette-server
				  (concat "/cgi-bin/hysearch2?V=" word "&E=1"))
    (setq url-list nil)
    (let ((url-list nil)
	  (word-list nil))
      (replace-from-table sgml-entities)
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (while (re-search-forward "sgmlex2\\?\\(.*\\)\"" nil t)
 	(setq url-list (cons (buffer-substring (match-beginning 1)(match-end 1)) url-list)))
      (goto-char (point-min))
      (while (re-search-forward "border=0> \\(.*\\)</a><br>" nil t)
	(setq word-list (cons (buffer-substring (match-beginning 1)(match-end 1)) word-list)))
      (map 'list (lambda (x y) (cons x y)) word-list url-list))))

(defun hachette-fetch-def (id)
  "Fetch a definition from the dictionary.
Argument ID is send to the CGI-script."
  (pop-to-buffer "*Hachette*")
  (setq buffer-read-only nil)
  (hachette-definition-mode)
  (erase-buffer)
  (hachette-insert-url-contents hachette-server
				(concat "/cgi-bin/sgmlex2?" id))
  (let ((case-fold-search t))
    (goto-char (point-min))
    (when (re-search-forward "<strong>" nil t)
      (delete-region (point-min) (match-beginning 0)))
    (when (re-search-forward "target=liste>" nil t)
      (delete-region (point-min) (match-end 0)))
    (insert "** ")
    (while (re-search-forward "</?\\(font\\|a\\)[^>]*>" nil t)
      (delete-region (match-beginning 0)(match-end 0)))
    (goto-char (point-min))
    (while (re-search-forward "\n" nil t)
      (replace-match " "))
    (goto-char (point-min))
    (while (re-search-forward "<strong>" nil t)
      (replace-match "\n\n"))
    (goto-char (point-min))
    (when (re-search-forward "\\(<p>&nbsp;\\)+" nil t)
      (replace-match "\n\n\n"))
    (goto-char (point-min))
    (while (re-search-forward "<[^>]+>" nil t)
      (delete-region (match-beginning 0)(match-end 0)))
    (fill-region (point-min) (point-max))
    (replace-from-table sgml-entities)
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (turn-on-font-lock)))

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

(provide 'hachette)

;;; hachette.el ends here
