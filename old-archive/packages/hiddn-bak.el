;From ark1!nems!mimsy!haven!purdue!tut.cis.ohio-state.edu!rutgers!aramis.rutgers.edu!topaz.rutgers.edu!busboys.rutgers.edu!gaynor Thu Nov  9 14:02:36 1989
;Article 743 of comp.emacs
;Path: ark1!nems!mimsy!haven!purdue!tut.cis.ohio-state.edu!rutgers!aramis.rutgers.edu!topaz.rutgers.edu!busboys.rutgers.edu!gaynor
;>From gaynor@busboys.rutgers.edu (Silver)
;Newsgroups: comp.emacs
;Subject: Re: Changing location of autosave file
;Message-ID: <Nov.8.02.30.11.1989.329@busboys.rutgers.edu>
;Date: 8 Nov 89 07:30:14 GMT
;References: <11520004@hpsmtc1.HP.COM>
;Organization: Rutgers Univ., New Brunswick, N.J.
;Lines: 52
;
;I'm not all that annoyed with the network filesystem speeds, but rather with
;the visual clutter of the extra filenames.  I wrote the following piece of code
;to autosave and backup into hidden filenames.
;
;Regards, [Ag] gaynor@topaz.rutgers.edu
;
;------------------------------- hidden-files.el -------------------------------
(provide 'hidden-files)

;;;;;;;;;; backup
;;
;; Use ".bak." to prefix backup filenames.

(defun file-name-sans-versions (path)
"Return FILENAME sans backup versions or strings.  This is a seperate procedure
so your site-init or startup file can redefine it."
  (let ((name (file-name-nondirectory path)))
    (concat (or (file-name-directory path) "")
	    (substring name (if (string-match "\\`\\.bak\\." name) 5 0)))))

(defun make-backup-file-name (path)
"Create the non-numeric backup file name for FILE.  This is a seperate function
so you can redefine it for customization."
  (concat (or (file-name-directory path) "")
	  ".bak."
	  (file-name-nondirectory path)))

(defun backup-file-name-p (path)
"Return non-nil if FILE is a backup file name (numeric or not).  This is a
separate function so you can redefine it for customization.  You may need to
redefine file-name-sans-versions as well."
  (string-match "\\`\\.bak\\." (file-name-nondirectory path)))



;;;;;;;;;; auto-save
;;
;; Use ".ckp." to prefix auto-save filenames.

(defun make-auto-save-file-name ()
"Return file name to use for auto-saves of current buffer.  Does not consider
auto-save-visited-file-name; that is checked before calling this function.  You
can redefine this for customization.  See also auto-save-file-name-p."
  (if (buffer-file-name)
    (concat (or (file-name-directory (buffer-file-name)) "")
	    ".ckp."
	    (file-name-nondirectory (buffer-file-name)))))

(defun auto-save-file-name-p (filename)
"Return t if FILENAME can be yielded by make-auto-save-file-name.  FILENAME
should lack slashes.  You can redefine this for customization."
  (string-match "\\`\\.ckp\\." filename))


