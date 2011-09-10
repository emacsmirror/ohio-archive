;; @(#)@ perl-descr.el	1.5 - describe-perl-symbol

;; This file defines the function 'describe-perl-symbol, which
;; displays a one-line information on a perl symbol.

;; Based on 'describe-lisp-symbol' and others.
;; Hacked for Perl by Johan Vromans <jv@mh.nl>

(defvar perl-doc-file "~/elisp/perl-descr.txt"
  "*Where the documentation file can be found.")

(defun perl-symbol-at-point ()
  "Get the closest Perl symbol to point, but don't change your
position. Has a preference for looking backward when not
directly on a symbol."

  (let ((perl-wordchars "a-zA-Z0-9_") start end symbol)
	      
    (save-excursion

      ;; first see if you're just past a symbol
      (if (not (eobp))
	  (if (looking-at "[] \t\n[{}()]")
	      (progn
		(skip-chars-backward " \n\t\r({[]})")
		(if (not (bobp))
		    (backward-char 1)))))

      (if (string-match (concat "[" perl-wordchars "]")
			(char-to-string (following-char)))
	  (progn
	    (skip-chars-backward perl-wordchars)
	    (setq start (point))
	    ; Get identifier. Include leading $ % @ to find things like
	    ; @ARGV and %ENV .
	    (if (string-match "[$%@]" (char-to-string (preceding-char)))
		(setq start (1- start)))
	    (skip-chars-forward perl-wordchars))

	;; else a symbol?
	  (progn
	    (setq start (point))
	    (if (looking-at "[$@][^ \n\t]") ; special variable
		(forward-char 1)
	      (if (string-match "[$@]" (char-to-string (preceding-char)))
		  (setq start (1- start))))
	    (forward-char 1)))
      (buffer-substring start (point)))))

(defun describe-perl-symbol (symbol)
  "Display the documentation of SYMBOL, a Perl operator."
  (interactive
    (let ((fn (perl-symbol-at-point))
	  (enable-recursive-minibuffers t)
	  (case-fold-search nil)	;require that case match for search
	  val args-file regexp)
      (setq val (read-from-minibuffer
		  (if fn
		      (format "Symbol (default %s): " fn)
		    "Symbol: ")))
      (if (string= val "")
	  (setq val fn))
      (setq regexp (concat "^" (regexp-quote val) "\\([ \t([/]\\|$\\)"))

      ;; get the buffer with the documentation text
      (if (not (get-file-buffer perl-doc-file))
	  (progn
	    (setq args-file
	      (find-file-noselect perl-doc-file))
	    (set-buffer args-file)
	    (rename-buffer "*PERL-DOC*")
	    (setq buffer-read-only t)))
      (set-buffer (get-file-buffer perl-doc-file))

      ;; lookup in the doc
      (goto-char (point-min))
      (list (if (re-search-forward regexp (point-max) t)
		(save-excursion
		  (beginning-of-line 1)
		  (let ((lnstart (point)))
		    (end-of-line)
		    (message "%s" (buffer-substring lnstart (point)))))
	      (error (format "No definition for %s" val)))))))
