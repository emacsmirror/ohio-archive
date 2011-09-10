;;!emacs
;;
;; FILE:         hsuperman.el
;; SUMMARY:      Fixes to Superman V1.0 for compatibility with 'manual-entry'.
;; USAGE:        GNU Emacs Lisp Library
;;
;; AUTHOR:       Bob Weiner
;; ORG:          Brown U.
;;
;; ORIG-DATE:    29-Dec-91 at 03:04:38
;; LAST-MOD:      2-Mar-92 at 21:36:11 by Bob Weiner
;;
;; This file is part of Hyperbole.
;;
;; Copyright (C) 1991, Brown University, Providence, RI
;; Developed with support from Motorola Inc.
;;
;; Based on "superman.el" by Barry A. Warsaw <bwarsaw@cen.com>
;; and "man.el", a part of GNU Emacs.
;; Available for use and distribution under the same terms as GNU Emacs.
;;
;; DESCRIPTION:  
;; DESCRIP-END.

;;; ************************************************************************
;;; Other required Elisp libraries
;;; ************************************************************************

(load "superman")

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;; Make this command compatible with previous incarnations of 'manual-entry'.
(fset 'manual-entry 'sm-manual-entry)
(defun sm-manual-entry (man-args &optional section)
  "Get Un*x manual pages given by string MAN-ARGS and optional SECTION string.
This command is the top-level command in the superman package. It runs
a Un*x command to retrieve and clean a manpage in the background and
places the results in a sm-manual-mode (manpage browsing) buffer. See
variable sm-notify for what happens when the buffer is ready.
Universal argument non-nil forces sm-getpage-in-background to start a new
background process."
  (interactive
   (let* ((default-entry (sm-default-man-entry))
	  (args
	   (read-string (format "%sman "
				(if (string= default-entry "") ""
				  (format "(default: man %s) "
					  default-entry))))))
     (list (if (string= args "") default-entry args))))
  (if (string= man-args "")
      (error "(sm-manual-entry): No man args given.")
    (if (stringp section)
	(setq man-args (concat section " " man-args)))
    (sm-getpage-in-background (sm-man-args-format man-args)
			      (consp current-prefix-arg))
    ))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

;;; Test with:
;;;   (sm-man-args-format "man 2N me 3 You THEM(1) her(L)  us(5p) 1 we")
;;;   ==>   "man 2n me 3 You 1 THEM l her 5 us 1 we"
;;;
(defun sm-man-args-format (args-str)
  "Changes each \"topic(section)\" to \"section topic\" in ARGS-STR.
Also downcases alphabetic section names when
'sm-downcase-section-letters-p' is non-nil."
  (if (stringp args-str)
      (let ((rtn-str "")
	    (start 0)
	    (dncase1 (if sm-downcase-section-letters-p
			'downcase 'identity))
	    (dncase2 (if sm-downcase-section-letters-p
			'sm-downcase 'identity))
	    topic section match prev-start)
	(while (setq match
		     (string-match
		      "\\([^( \t]+\\)[ \t]*(\\([^)]\\)[^)]*)[ \t]*"
		      args-str start))
	  (setq prev-start start
		start (match-end 0)
		topic (substring args-str (match-beginning 1)
				 (match-end 1))
		section (funcall dncase1
				 (substring args-str (match-beginning 2)
					    (match-end 2)))
		rtn-str (concat rtn-str
				(substring args-str prev-start match)
				section " " topic " ")))
	(setq rtn-str (concat rtn-str (substring args-str start))
	      rtn-str (substring rtn-str 0 (string-match "[ \t]+$" rtn-str)))
	(funcall dncase2 rtn-str))))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(setq sm-filter-list
  '(("sed "
     ("-e 's/.\010//g'"
      "-e '/[Nn]o such file or directory/d'"
      "-e '/Reformatting page.  Wait... done/d'"
      "-e '/^\\([A-Z][A-Z.]*([0-9A-Za-z][-0-9A-Za-z+]*)\\).*\\1$/d'"
      "-e '/^[ \\t]*Hewlett-Packard Company[ \\t]*- [0-9]* -.*$/d'"
      "-e '/^[ \\t]*Hewlett-Packard[ \\t]*- [0-9]* -.*$/d'"
      "-e '/^ *Page [0-9]*.*(printed [0-9\\/]*)$/d'"
      "-e '/^Printed [0-9].*[0-9]$/d'"
      "-e '/^Sun .*/d'"
      "-e '/^\\n$/D'"
      ))
    ("awk '"
     ("BEGIN { blankline=0; anonblank=0; }"
      "/^$/ { if (anonblank==0) next; }"
      "{ anonblank=1; }"
      "/^$/ { blankline++; next; }"
      "{ if (blankline>0) { print \"\"; blankline=0; } print $0; }"
      "'"
      ))))

(provide 'hsuperman)
