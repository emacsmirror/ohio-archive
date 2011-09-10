;;; dict-web.el --- Emacs interface to the Online Dictionary Database
;;;
;;; Author: Eric Marsden <emarsden@mail.dotcom.fr>
;;; Keywords: dictionary
;;; Version: 0.4
;;
;;     Copyright (C) 1999-2000  Eric Marsden
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
;; Please send suggestions and bug reports to <emarsden@mail.dotcom.fr>.
;; The latest version of this package should be available from
;;
;;     <URL:http://www.chez.com/emarsden/downloads/>


;;; Overview ==========================================================
;;
;; This module allows you to access the Online Dictionary Database at
;; dict.org. To use it, type `M-x dict', which will prompt you for a
;; word or phrase to search for in the online dictionary (defaulting
;; to the word under the cursor). dict-web.el requires w3 (the Emacs
;; web browser) to be installed. If you are a heavy user of this
;; software you might consider using Torsten Hilbrich's dictionary
;; package at
;; <URL:http://www.in-berlin.de/User/myrkr/dictionary.html>, which
;; allows you to connect to a local dictd server.
;;
;; If you wish definitions to be displayed in a dedicated frame, you
;; might use the following in your ~/.emacs.el:
;;
;;    (push "*dict*" special-display-buffer-names)


;;; INSTALLATION =====================================================
;;
;; Place this file somewhere in emacs' load-path, and byte-compile it
;; (by pressing `B' in dired mode, for example). Then place a line
;; such as
;;
;;    (autoload 'dict "dict-web"
;;              "Lookup a word or phrase in the Online Dictionary Database." t)
;;
;; in your ~/.emacs.el initialization file.
;;
;;
;; Thanks to Andrew Dlugan <dlugan@physics.ubc.ca> for the extended
;; face support, and to Kai Grossjohan for suggesting improvements.


;;; Code:

(eval-when-compile
  (require 'cl))

(defgroup dict-web nil
  "Lookup words from an online dictionary."
  :prefix "dict-web-"
  :group 'applications)

(defgroup dict-web-faces nil
  "Fontification colors."
  :prefix "dict-web-"
  :group 'dict-web
  :group 'faces)

(defface dict-web-source-face
  (` ((((class grayscale) (background light))
       (:background "Gray90" :italic t :underline t))
      (((class grayscale) (background dark))
       (:foreground "Gray80" :italic t :underline t :bold t))
      (((class color) (background light)) 
       (:foreground "blue"))
      (((class color) (background dark)) 
       (:foreground "cyan" :bold t))
      (t (:bold t :underline t))))
  "Font lock face used to highlight sources of dictionary information."
  :group 'dict-web-faces)

(defface dict-web-query-word-face
  (` ((((class grayscale) (background light))
       (:background "Gray90" :bold t))
      (((class grayscale) (background dark))
       (:foreground "Gray80" :bold t))
      (((class color) (background light)) 
       (:foreground "forest green" :bold t))
      (((class color) (background dark)) 
       (:foreground "green" :bold t))
      (t (:bold t :underline t))))
  "Font lock face used to highlight the query word."
  :group 'dict-web-faces)

(defface dict-web-pronunciation-face
  (` ((((class grayscale) (background light))
       (:background "Gray90" :bold t))
      (((class grayscale) (background dark))
       (:foreground "Gray80" :bold t))
      (((class color) (background light)) 
       (:foreground "red"))
      (((class color) (background dark)) 
       (:foreground "yellow"))
      (t (:bold t :underline t))))
  "Font lock face used to highlight pronunciation information."
  :group 'dict-web-faces)

(defface dict-web-speech-part-face
  (` ((((class grayscale) (background light))
       (:background "Gray90" :underline t))
      (((class grayscale) (background dark))
       (:foreground "Gray80" :underline t ))
      (((class color) (background light)) 
       (:foreground "blue"))
      (((class color) (background dark)) 
       (:foreground "cyan"))
      (t (:bold t :underline t))))
  "Font lock face used to highlight part of speech information."
  :group 'dict-web-faces)

(defface dict-web-string-face
  (` ((((class grayscale) (background light))
       (:background "Gray90" :italic t ))
      (((class grayscale) (background dark))
       (:foreground "Gray80" :italic t ))
      (((class color) (background light)) 
       (:foreground "red" :italic t))
      (((class color) (background dark)) 
       (:foreground "yellow" :italic t))
      (t (:bold t :underline t))))
  "Font Lock mode face used to highlight strings and quotes."
  :group 'dict-web-faces)

(defvar dictweb-mode-hook nil
  "*Hook to run before entering dict-mode.")

(defvar dictweb-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q")   #'bury-buffer)
    (define-key map (kbd "n")   #'dictweb-next)
    (define-key map (kbd "p")   #'dictweb-prev)
    (define-key map (kbd "SPC") #'scroll-up)
    (define-key map (kbd "DEL") #'scroll-down)
    (define-key map (kbd "<")   #'beginning-of-buffer)
    (define-key map (kbd ">")   #'end-of-buffer)
    (define-key map (kbd "s")   #'isearch-forward)
    (define-key map (kbd "r")   #'isearch-backward)
    (define-key map (kbd "h")   #'describe-mode)
    (define-key map [button3]   #'dictweb-mouse3)
    (define-key map (kbd "w")   #'dict)
    map)
  "Keymap used in dictweb mode.")

(defvar dictweb-contextual-menu
  '("Dictweb"
    ["Lookup word" dict :active t]
    ["Describe Mode" describe-mode :active t])
  "Contextual menu used in dictweb mode")    


;;;###autoload
(defun dict (word)
  "Lookup a word or phrase in the Online Dictionary Database."
  (interactive (list (read-string "Dict lookup for: " (current-word))))
  (let ((buf (get-buffer-create "*dict*")))
    (switch-to-buffer-other-window buf)
    (setq buffer-read-only nil)
    (erase-buffer)
    (dictweb-fetch word)
    (dictweb-wash word)
    (dictweb-mode)))

(defun dictweb-fetch (word)
  (require 'url)
  (let* ((pairs `(("Form" . "Dict1")
                  ("Query" . ,word)
                  ("Strategy" . "*")
                  ("Database" . "*")))
         (url-request-data (dictweb-form-encode pairs))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-type" . "application/x-www-form-urlencoded"))))
    (url-insert-file-contents "http://www.dict.org/bin/Dict")))

(defun dictweb-wash/old ()
  (let (was)
    (goto-char (point-min))
    (let ((case-fold-search t))
      (when (re-search-forward "</form>" nil t)
        (delete-region (point-min) (match-beginning 0)))
      (goto-char (point-max))
      (when (re-search-backward "</pre>" nil t)
        (delete-region (point-max) (match-beginning 0)))
      (goto-char (point-min))
      (while (re-search-forward "<[^>]+>" nil t)
        (replace-match "" t))
      (goto-char (point-min))
      (while (looking-at "\\s-")
        (delete-char 1))
      ;; now face highlighting
      (while (re-search-forward "^From \\(.*\\) :$" nil t)
        (add-text-properties (match-beginning 0) (1- (match-end 0))
                             '(face bold dictweb t))
        (end-of-line)
        (delete-char 1)
        (setq was (point))
        (if (re-search-forward "\\\\[^\\s-]+\\\\," nil t)
            (add-text-properties (match-beginning 0) (match-end 0)
                                 '(face italic))
          (goto-char was)))
      (goto-char (point-min)))))

(defun dictweb-wash (word)
  (let (was)
    (goto-char (point-min))
    (let ((case-fold-search t) pos-list pos-re)
      (when (re-search-forward "</form>" nil t)
        (delete-region (point-min) (match-beginning 0)))
      (goto-char (point-max))
      (when (re-search-backward "</pre>" nil t)
        (delete-region (point-max) (match-beginning 0)))
      (goto-char (point-min))
      (while (re-search-forward "<[^>]+>" nil t)
        (replace-match "" t))
      (goto-char (point-min))
      (while (looking-at "\\s-")
        (delete-char 1))
      ;; now face highlighting for dictionary entries
      (setq pos-list
	    (mapcar
	     '(lambda (arg)
		(mapconcat '(lambda (arg2) (if (string= arg2 ".") "\\." arg2)) 
			   (mapcar 'char-to-string arg)
			   ""))
	     '("n.?" "noun" "v.? ?\\(t\\|i\\)?.?" "verb" "adj.?" "adv.?" "cf.?" "conj.?" 
	       "pl.?" "fr.?" "syn.?" "c.?" "fem.?" "masc.?" "prep.?" "pron.?"
	       "oe.?" "f.?" "ant.?" "a." "l." "gr." "obs."
	       "See")))
      (setq pos-re 
	    (concat "[^a-z]\\(" 
		    (mapconcat '(lambda (arg)
				  (concat "\\(" arg "\\)")) pos-list "\\|") 
		    "\\)[^a-z]"))
      (while (re-search-forward pos-re nil t)
        (add-text-properties (match-beginning 1) (match-end 1)
                             '(face dict-web-speech-part-face)))
      (goto-char (point-min))
      (while (re-search-forward "``[^']+''" nil t)
        (add-text-properties (match-beginning 0) (match-end 0)
                             '(face  dict-web-string-face)))
      (goto-char (point-min))
      (while (re-search-forward (concat "[^a-z]\\(" word "\\)[^a-z]") nil t)
        (add-text-properties (match-beginning 1) (match-end 1)
                             '(face dict-web-query-word-face)))
      (goto-char (point-min))
      (while (re-search-forward "^From \\(.*\\) :$" nil t)
        (add-text-properties (match-beginning 0) (match-end 0)
                             '(face dict-web-source-face dictweb t))
        (end-of-line)
        (delete-char 1)
        (setq was (point))
        (if (re-search-forward "\\\\[^\\s-]+\\\\" nil t)
            (add-text-properties (match-beginning 0) (match-end 0)
                                 '(face dict-web-pronunciation-face))
          (goto-char was)))
      (goto-char (point-min)))))

;; from nnweb.el
(defun dictweb-form-encode (pairs)
  "Return PAIRS encoded for forms."
  (require 'w3-forms)
  (mapconcat
   (function
    (lambda (data)
      (concat (w3-form-encode-xwfu (car data)) "="
	      (w3-form-encode-xwfu (cdr data)))))
   pairs "&"))

(defun dictweb-mouse3 (event)
  (interactive "e")
  (cond ((fboundp 'event-window)        ; XEmacs
         (set-buffer (window-buffer (event-window event)))
         (and (event-point event) (goto-char (event-point event)))
         (popup-mode-menu))))

(defmacro dictweb-goto-next-property-change ()
  '(let (target)
     (setq target (next-single-property-change (point) 'dictweb))
     (if target (goto-char target)
       (message "On last match"))))

(defmacro dictweb-goto-prev-property-change ()
  '(let (target)
     (setq target (previous-single-property-change (point) 'dictweb))
     (if target (goto-char target)
       (message "On first match"))))

(defun dictweb-next ()
  "Move to next match in the buffer."
  (interactive)
  (dictweb-goto-next-property-change)
  (unless (get-text-property (point) 'dictweb)
    (dictweb-goto-next-property-change)))

(defun dictweb-prev ()
  "Move to previous match in the buffer."
  (interactive)
  (dictweb-goto-prev-property-change)
  (unless (get-text-property (point) 'dictweb)
    (dictweb-goto-prev-property-change)))

(defun dictweb-mode ()
  "Major mode for browsing results of dict.org dictionary lookups.
\\{dictweb-mode-map}"
  (interactive)
  (require 'browse-url)
  (use-local-map dictweb-mode-map)
  (setq major-mode 'dictweb-mode
        mode-name "Dictweb"
        buffer-read-only t)
  (set-buffer-modified-p nil)
  ;; only useful in XEmacs
  (setq mode-popup-menu dictweb-contextual-menu)
  (run-hooks 'dictweb-mode-hook))

(provide 'dict-web)

;; dict-web.el ends here
