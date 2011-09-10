;;; install.el --- Lookup installer
;; Copyright (C) 1999 NISHIDA Keisuke <knishida@ring.aist.go.jp>

;; $Id: install.el,v 1.6 2000/02/10 00:54:31 knishida Exp $

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Code:

(if (string< emacs-version "19.29")
    (setq command-line-args-left (cdr command-line-args-left)))

(defconst install-elisp-files
  '("evi.el" "evi-mule.el" "lookup.el" "lookup-utils.el" "lookup-types.el"
    "lookup-vse.el" "lookup-package.el" "lookup-kanji.el"
    "lookup-entry.el" "lookup-content.el" "lookup-select.el"
    "ndic.el" "ndeb.el" "ndtp.el" "ndict.el" "ndkks.el" "ndspell.el"
    "ndcookie.el" "ndmisc.el" "ndsrd.el" "sdicf.el" "stem-english.el"
    "lookup-vars.el"))

(defconst install-info-files
  '("lookup.info" "lookup-guide.info"))

(defvar install-lisp-directory nil)
(defvar install-info-directory nil)
(defvar install-lookup-version "unknown")

(defun install-check-directory (directory)
  (and (not (file-exists-p directory))
       (y-or-n-p (format "Directory %s is not exist.  Creat it? " directory))
       (make-directory directory t))
  (if (not (file-directory-p directory))
      (error "%s is not directory" directory))
  (directory-file-name (expand-file-name directory)))

(or (fboundp 'with-current-buffer)
    (defmacro with-current-buffer (buffer &rest body)
      (` (save-current-buffer
	   (set-buffer (, buffer))
	   (,@ body)))))

(or (fboundp 'save-current-buffer)
    (defmacro save-current-buffer (&rest body)
      (` (let ((evi-orig-buffer (current-buffer)))
           (unwind-protect
               (progn (,@ body))
             (set-buffer evi-orig-buffer))))))


;; get the version number of lookup
(with-current-buffer (get-buffer-create " *work lookup-version*")
  (erase-buffer)
  (setq install-lookup-version
	(condition-case nil
	    (progn
	      (insert-file-contents "VERSION")
	      (goto-char (point-min))
	      (buffer-substring (point-min) (progn (end-of-line) (point))))
	  (error "unknown")))
  (kill-buffer (current-buffer)))

;; message

(if noninteractive nil
  (switch-to-buffer (generate-new-buffer "*Lookup Installer*"))
  (insert "Lookup $B%$%s%9%H!<%i(B\n")
  (insert "===================\n\n")
  (insert "Lookup $B$N%$%s%9%H!<%k$r;O$a$^$9!#(B")
  (insert "$BESCf$GCfCG$9$k$K$O(B C-g $B$r2!$7$F2<$5$$!#(B\n\n"))

;; directory

(if noninteractive nil
  (insert "$B%G%#%l%/%H%j$N7hDj(B\n")
  (insert "------------------\n\n")
  (insert "elisp $B%U%!%$%k$N%G%#%l%/%H%j$rF~NO$7$F2<$5$$(B:\n"))
(let ((default "~/emacs/lisp/lookup/"))
  (setq install-lisp-directory
	(install-check-directory
	 (if noninteractive
	     (or (car command-line-args-left) default)
	   (read-file-name "Lisp directory: " default default)))))
(if noninteractive nil
  (insert "    -> " install-lisp-directory "\n\n") (sit-for 0))

(if noninteractive nil
  (insert "info $B%U%!%$%k$N%G%#%l%/%H%j$rF~NO$7$F2<$5$$(B:\n"))
(let ((default "~/emacs/info/"))
  (setq install-info-directory
	(install-check-directory
	 (if noninteractive
	     (or (car (cdr command-line-args-left)) default)
	   (read-file-name "Info directory: " default default)))))
(if noninteractive nil
  (insert "    -> " install-info-directory "\n\n") (sit-for 0))

;; lookup-vars.el

(if noninteractive nil
  (insert "$B%$%s%9%H!<%k$N<B9T(B\n")
  (insert "------------------\n\n")
  (insert "lookup-vars.el.in $B$+$i(B lookup-vars.el $B$r@8@.Cf(B...") (sit-for 0))
(with-current-buffer (find-file-noselect "lisp/lookup-vars.el.in" t)
  (if (search-forward "@VERSION@")
      (replace-match install-lookup-version))
  (if (search-forward "@pkgemacsdir@")	;needs to be fixed to the correct one
      (replace-match install-lisp-directory))
  (write-file "lookup-vars.el")
  (kill-buffer (current-buffer))
  )
(message "Copied lookup-vars.el.in to lookup-vars.el")
(if (not noninteractive) (insert "done\n"))

;; compile

(if noninteractive nil
  (insert "elisp $B%U%!%$%k$N%3%s%Q%$%kCf(B...") (sit-for 0))
(let* ((default-directory (expand-file-name "lisp/"))
       (lookup-compile-directory default-directory)
       (command-line-args-left (if (string< emacs-version "19.29")
				   (cons nil install-elisp-files)
				 install-elisp-files)))
  (load (expand-file-name "lookup-compile.el")))
(if (not noninteractive) (insert "done\n"))

;; install

(if noninteractive nil
  (insert "elisp $B%U%!%$%k$N%$%s%9%H!<%kCf(B...") (sit-for 0))
(mapcar (lambda (file)
	  (copy-file (expand-file-name file "lisp/")
		     (expand-file-name file install-lisp-directory) t)
	  (message "Installed %s to %s" file install-lisp-directory)
	  (setq file (byte-compile-dest-file file))
	  (copy-file (expand-file-name file "lisp/")
		     (expand-file-name file install-lisp-directory) t)
	  (message "Installed %s to %s" file install-lisp-directory))
	install-elisp-files)
(if (not noninteractive) (insert "done\n"))

(if noninteractive nil
  (insert "info $B%U%!%$%k$N%U%)!<%^%C%HCf(B...") (sit-for 0))
(mapcar (lambda (info)
	  (if (file-readable-p (expand-file-name info "texi/"))
	      t
	    ;; the pre-formatted .info file does not exist; format it here
	    (let ((texi (concat (file-name-sans-extension info) ".texi")))
	      (save-current-buffer
	       (let ((buf (find-file-noselect 
			   (expand-file-name texi "texi/") t)))
		 (set-buffer buf)
		 (condition-case nil
		     (texinfo-format-buffer t) ; t for nosplit
		   (error ; one more try with no @direntry
					;(kill-buffer (current-buffer))
		    (set-buffer buf)
		    (goto-char (point-min))
		    (message "Format failed. Trying no @direntry")
		    (when (re-search-forward "@direntry" nil t)
			  (setq beg (match-beginning 0))
			  (when (re-search-forward "@end direntry" nil t)
				(delete-region beg (match-end 0))))
		    (goto-char (point-min))
		    (when (re-search-forward "@dircategory" nil t)
			  (setq end (progn(end-of-line)(point)))
			  (delete-region (match-beginning 0) end))
		    (when (re-search-forward "@detailmenu" nil t)
			  (beginning-of-line)
			  (insert "@c")
			  (when (re-search-forward "@end detailmenu" nil t)
				(beginning-of-line)
				(insert "@c")))
		    (goto-char (point-min))
		    (while (re-search-forward  
			    "@\\(email\\|url\\)\\{\\([^\\}]+\\)\\}" nil t)
		      (replace-match 
		       (buffer-substring (match-beginning 2) (match-end 2))))
		    (texinfo-format-buffer t)))
		 (save-buffer 0)
		 (kill-buffer (current-buffer))
		 (kill-buffer buf))))))
	install-info-files)

(if (not noninteractive) (insert "done\n"))

(if noninteractive nil
  (insert "info $B%U%!%$%k$N%$%s%9%H!<%kCf(B...") (sit-for 0))
(mapcar (lambda (info)
	  (mapcar (lambda (file)
		    (copy-file (expand-file-name file "texi/")
			       (expand-file-name file install-info-directory)
			       t)
		    (message "Installed %s to %s" file install-info-directory))
		  (directory-files "texi/" nil info)))
	install-info-files)
(if (not noninteractive) (insert "done\n"))

;; clean

(if noninteractive nil
  (insert "$B0l;~%U%!%$%k$N:o=|Cf(B...") (sit-for 0))
(let ((default-directory (expand-file-name "lisp/")))
  (delete-file "lookup-vars.el")
  (mapcar (lambda (file)
	    (delete-file (byte-compile-dest-file (expand-file-name file))))
	  install-elisp-files))
(let ((default-directory (expand-file-name "texi/")))
   (mapcar (lambda (file)
	(delete-file (expand-file-name file)))
	install-info-files))
	
(message "Removed lookup-vars.el, *.elc, *.info")
(if (not noninteractive) (insert "done\n"))

;; initialize

(if noninteractive nil
  (insert "Lookup $B$N=i4|2=Cf(B...")
  (sit-for 0)
  (lookup-initialize)
  (insert "done\n")
  (kill-buffer (current-buffer)))

;; congratulate

(if noninteractive
    (message "\nSee etc/SETUP for the setup after installation")
  (switch-to-buffer (generate-new-buffer "*Congratulations!*"))
  (insert "Lookup $B%;%C%H%"%C%W%,%$%I(B
=========================

Lookup $B$N%$%s%9%H!<%k$,40N;$7$^$7$?!#I,MW$K1~$8$F!"0J2<$N@_Dj$r9T$J$C$F(B
$B2<$5$$!#(B

load-path $B$N@_Dj(B
----------------

$B<!$N$h$&$K$7$F(B load-path $B$N@_Dj$r9T$J$C$F2<$5$$!#$3$l$O%$%s%9%H!<%k$7$?(B
$B%W%m%0%i%`%U%!%$%k$rFI$_9~$`$?$a$KI,MW$J$b$N$G$9!#%Q%9$,DL$C$F$$$k$3$H$,(B
$B$o$+$C$F$$$k$J$i!"$J$/$F$b9=$$$^$;$s!#(B

  (setq load-path (cons \"" install-lisp-directory "\" load-path))

info $B%G%#%l%/%H%j$N@_Dj(B
-----------------------

$B<!$N$h$&$K$7$F(B Info-default-directory-list $B$N@_Dj$r9T$J$C$F2<$5$$!#$3$l(B
$B$O%$%s%9%H!<%k$7$?(B info $B%U%!%$%k$rFI$_9~$`$?$a$KI,MW$J$b$N$G$9!#%Q%9$,(B
$BDL$C$F$$$k$3$H$,$o$+$C$F$$$k$J$i!"$J$/$F$b9=$$$^$;$s!#(B

  (setq Info-default-directory-list
        (cons \"" install-info-directory "\" Info-default-directory-list))

dir $B%U%!%$%k$N@_Dj(B
------------------

$B$3$N%$%s%9%H!<%i$O(B info $B%U%!%$%k$N0lMw%U%!%$%k(B(dir $B%U%!%$%k(B)$B$X$N=q$-=P$7(B
$B$r<+F0$G$O9T$J$$$^$;$s!#I,MW$K1~$8$F!"<!$N3F9T$r(B dir $B%U%!%$%k$KDI2C$7$F(B
$B2<$5$$!#(B

* Lookup: (lookup).             Lookup, a Search Interface.
* Lookup Guide: (lookup-guide). The Lookup Startup Guide.

$B0J>e$G%$%s%9%H!<%k$O=*N;$G$9!#$3$3$G(B C-c C-c $B$H%?%$%W$9$k$H%9%?!<%H%"%C(B
$B%W%,%$%I$,3+$+$l$^$9$N$G!"$^$:$O$=$A$i$+$i$4Ms2<$5$$!#(B

-- EOF --
")
  (goto-char (point-min))
  (set-buffer-modified-p nil)
  (view-mode)
  (local-set-key "\C-c\C-c" 'install-open-info))

(defun install-open-info ()
  (interactive)
  (require 'info)
  (Info-find-node (expand-file-name "lookup-guide" install-info-directory)
		  "Top"))

;;; install.el ends here
