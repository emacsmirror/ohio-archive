;;; Here's my local setup.  It may not work for you but it is there just to
;;; show almost all customization options.

(setq fax-mode-source-dir (expand-file-name "~/lib/emacs/lisp/fax-mode"))

(or (member fax-mode-source-dir load-path)
    (setq load-path (cons fax-mode-source-dir load-path)))

;;; ------------
;;; phone
;;; ------------

;;; site-start.el

(setq phone-number-international-prefix "00"
      phone-number-long-distance-prefix "0"
      phone-number-country-code "49"
      phone-number-area-code "7352")

(autoload 'phone-number-dial-string "phone"
  "Build a phone number suitable for the modem." t)

;;; ------------
;;; snail
;;; ------------

;;; site-start.el

(setq snail-country-regexp "\\(Germany\\|Deutschland\\)")

(autoload 'snail-read-database-file "snail"
  "Parse a snail address database." t)

(autoload 'snail-define-snail-alias "snail"
  "Define a paper mail alias." t)

(autoload 'snail-address "snail"
  "Insert a paper mail address at point." t)

(autoload 'snail-db-find-file "snail"
  "Visit an EDB database file." t)

;;; .emacs

(setq snail-define-mail-alias nil
      snail-define-fax-alias t)

;;; default.el

(setq snail-db-aux-directory fax-mode-source-dir
      snail-db-format-directory fax-mode-source-dir)

;;; ------------
;;; faxutil
;;; ------------

;;; site-start.el

(autoload 'fax-read-resource-file "faxutil"
  "Read a fax resource file." t)

(autoload 'fax-define-fax-alias "faxutil"
  "Define a fax alias." t)

(autoload 'fax-number "faxutil"
  "Insert the fully resolved phone number of a fax alias at point." t)

(autoload 'fax-simple-number "faxutil"
  "Insert the phone number of a fax alias at point." t)

(autoload 'fax-file-name "faxutil"
  "Read a file name from the mini-buffer and insert it at point." t)

(autoload 'fax-view-file "faxutil"
  "View a facsimile message." t)

(autoload 'fax-print-file "faxutil"
  "Print a facsimile message." t)

;;; site-start.el/emacs.el

(setq fax-view-program "/usr/bin/X11/viewfax"
      fax-view-switches '("-bn" "-g+6+24")
      fax-print-program lpr-command
      fax-print-switches '("-v"))

;;; ------------
;;; sendfax.el
;;; ------------

;;; site-start.el

(setq fakefax-program (concat fax-mode-source-dir "/fakefax"))

(setq sendfax-package "faxpr"
      sendfax-program (expand-file-name "~/bin/faxpr")
      sendfax-switches '("-x")
      sendfax-number-switch nil
      sendfax-file-switch nil)

(autoload 'sendfax-configure "sendfax"
  "Configure a known fax package." t)

(autoload 'sendfax-buffer "sendfax"
  "Pass the current buffer to the fax sub-system." t)

(autoload 'sendfax-region "sendfax"
  "Pass the current region to the fax sub-system." t)

(autoload 'fax "sendfax"
  "Edit a fax message to be sent." t)

(autoload 'fax-other-window "sendfax"
  "Like `fax', but display the `*fax*' buffer in another window." t)

(autoload 'fax-other-frame "sendfax"
  "Like `fax', but display the `*fax*' buffer in another frame." t)

;;; .emacs

(require 'time-stamp)

(setq fax-font-lock-keywords
      '((fax-font-lock-header . (0 gnus-article-header-face t))
	(fax-font-lock-to . (0 gnus-article-to-face t)))

      fax-archive-file-name "~/Fax.archive"

      fax-from (list (concat "Ralph Schleicher\n"
			     "Biberacher Straße 40\n"
			     "88416 Ochsenhausen\n"))
      fax-date '((lambda ()
		   (time-stamp-strftime "Ochsenhausen, %d.%m.%y\n"))))

;; The average character width of IBM Courier at 12 points is 7.2 points (a
;; PostScript point is 1/72 inch).  Setting the average width to 7.0 points
;; allows an overhang of 2 characters before a line will be broken.
;;
;; 52 lines per page are exactly 13.5 points per line.  Increasing this
;; value a little bit caches rounding errors.

;;(setq fax-mode-hook '(lambda ()
;;			 (setq fill-column 70)))
;;
;;(setq sendfax-write-region-hooks '(lambda ()
;;				      (setq ps-paper-type 'ps-letter
;;					    ps-left-margin 54
;;					    ps-right-margin 54
;;					    ps-top-margin 36
;;					    ps-bottom-margin 54
;;					    ps-font-size 12
;;					    ps-avg-char-width 7.0
;;					    ps-space-width 7.0
;;					    ps-line-height 13.52
;;					    ps-print-header nil))
;;
;;	sendfax-write-region-function 'ps-print-region-with-faces)

;;; ------------
;;; texfax
;;; ------------

;;; emacs.el

(require 'texfax)

(setq fax-default-headers (concat "Format: " texfax-format-string "\n")

      fax-mode-hook
      '(lambda ()
	 (define-key fax-mode-map "\M-gt" ; TeX
	   'texfax-add-tex-properties)
	 (define-key fax-mode-map "\M-gv" ; verbatim
	   'texfax-remove-tex-properties)
	 (setq fill-column 76)
	 (turn-on-font-lock))

      fax-after-setup-hook
      '(lambda ()
	 (save-excursion
	   (let (start)
	     (insert "\n\nTschüß\n")
	     (setq start (point))
	     (insert "\\epsffile{ralph.ps}\n")
	     (texfax-add-tex-properties start (1- (point)))
	     (setq inhibit-point-motion-hooks t))))

      texfax-preamble (concat "\\input " fax-mode-source-dir "/texfax.tex\n")
      texfax-tex-program "dctex"
      texfax-dvips-switches '("-P" "fax")
      texfax-tex-face 'texfax)

(modify-face (copy-face 'default 'texfax)
	     "Black" "RGB:ff/ff/df" nil nil nil nil)

;;; ------------
;;; faxed
;;; ------------

;;; site-start.el

(setq faxed-default-directory "~/Fax")

(autoload 'faxed-mode "faxed"
  "Faxed mode is a minor mode for Dired." t)

(autoload 'faxed "faxed"
  "Run Dired on directory DIRECTORY with turned on Faxed mode." t)

;;; emacs.el

(add-hook 'faxed-hook (function (lambda ()
				  (scroll-left 32))))
