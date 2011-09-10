; Path: ark1!nems!mimsy!haven!uflorida!novavax!weiner
; >From weiner@novavax.UUCP (Bob Weiner)
; Newsgroups: comp.emacs
; Subject: dired-links.el - resolves links in GNU Emacs Dired
; Date: 26 Oct 89 02:00:55 GMT
; Organization: Motorola Inc.
; 
;;!emacs
;;
;; FILE:         dired-links.el
;; SUMMARY:      Properly resolves UNIX (and Apollo variant) links under dired.
;; USAGE:        GNU Emacs Lisp Library
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Motorola, Inc., Communications Sector, Applied Research
;; E-MAIL:       USENET:  weiner@novavax.UUCP
;;
;; ORIG-DATE:    09-May-89
;; LAST-MOD:     25-Oct-89 at 21:42:56 by Bob Weiner
;;
;; Copyright (C) 1989 Bob Weiner and Free Software Foundation, Inc.
;; Available for use and distribution under the same terms as GNU Emacs.
;;
;; This file is not part of GNU Emacs.
;;
;; DESCRIPTION:  
;;
;;   To install, simply load this file after loading dired.el, or create a
;;   dired hook which loads this file.
;;
;;   The changes below to 'dired-noselect' assume UNIX shell file abbreviation
;;   and UNIX file name conventions.
;;
;;   This modified version of the 'dired-noselect' function automatically
;;   resolves all recursive links properly and edits the actual directory
;;   pointed to.  It handles Apollo-isms such as '/usr/local ->
;;   $(SERVER_LOCAL)/usr/local', '/usr/bin -> ../$(SYSTYPE)/usr/bin' and '/tmp
;;   -> `node_data/tmp'.  It also handles relative links properly as in
;;   '/usr/local/emacs -> gnu/emacs' which must be resolved relative to the
;;   '/usr/local' directory.
;;
;; DESCRIP-END.


;; Normally, if you perform a dired multiple times on a directory which is a
;; link, a new buffer will be created each time.  This is due to the fact
;; that 'dired-find-buffer' is called in 'dired-noselect' before the link is
;; resolved.  The following code solves this problem by checking for a
;; previously existing buffer that is performing dired on the directory that
;; the link resolves to.  This is also done recursively.  If one is found,
;; the dired buffer that shows the link is killed and the previously existing
;; one is used and re-read in.

(defun dired-noselect (dirname)
  "Like M-x dired but returns the dired buffer as value, does not select it."
  (or dirname (setq dirname default-directory))
  (setq dirname (directory-file-name dirname))
  (if (file-directory-p dirname)
      (setq dirname (file-name-as-directory dirname)))
  (let ((buffer (dired-find-buffer dirname))
	(actual-dir t)
	(actual-buffer))
    (save-excursion
      (set-buffer buffer)
      (while actual-dir
	(if (= (aref dirname 0) ?~) (setq dirname (expand-file-name dirname)))
	(dired-readin dirname buffer)
	(if (setq actual-dir (dired-resolve-link dirname))
	    (progn (setq dirname (directory-file-name actual-dir)
			 actual-buffer (dired-find-buffer dirname))
		   (if (file-directory-p dirname)
		       (setq dirname (file-name-as-directory dirname)))))
	(if actual-buffer
	    (progn (set-buffer actual-buffer)
		   (kill-buffer buffer)
		   (setq buffer actual-buffer
			 actual-buffer nil))))
      (dired-move-to-filename)
      (dired-mode dirname))
    buffer))

(defconst dired-link-string " -> "
  "String pattern in a dired listing line that indicates a link.")

;;;
;;; Works with Apollo's variant links if variable is first part of the linkname
;;;
(defun dired-resolve-link (dirname)
  (save-excursion
    (goto-char (point-min))
    (end-of-line)
    (let ((eol (point))
	  (actual-dir)
	  (buffer-read-only)
	  (var-link))
      (goto-char (point-min))
      (if (search-forward dired-link-string eol t)
	  (progn (delete-region (point-min) (point))
		 (save-excursion
		   (end-of-line)
		   (setq eol (point)))
		 (while (re-search-forward "\\$(\\([^\)]*\\))" eol t)
		   (progn (setq var-link (getenv (buffer-substring
						   (match-beginning 1)
						   (match-end 1))))
			  (delete-region (match-beginning 0) (match-end 0))
			  (insert var-link)))
		 (setq actual-dir (buffer-substring (point-min) (1- (point-max))))
		 ;; If not an absolute path
		 (let ((nd-abbrev (string-match "`node_data" actual-dir)))
		   (if (and nd-abbrev (= nd-abbrev 0))
		       (setq actual-dir (concat "/sys/"
						(substring actual-dir 1)))))
		 (if (/= (aref actual-dir 0) ?/)
		     (setq actual-dir
			   (concat 
			     (file-name-directory
			       (substring dirname 0 
					  (if (= (aref dirname
						       (1- (length dirname))) ?/)
					      -1)))
			     actual-dir)))
		 actual-dir
)))))

(provide 'dired-links)
;-- 
;Bob Weiner, Motorola, Inc.,   USENET:  ...!gatech!uflorida!novavax!weiner
;(407) 738-2087


