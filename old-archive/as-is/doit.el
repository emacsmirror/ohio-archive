;To: unix-emacs@bbn.com
;Date: 8 May 89 17:16:43 GMT
;From: "Randal L. Schwartz @ Stonehenge" <littlei!omepd!merlyn@uunet.uu.net>
;Sender: arpa-unix-emacs-request@bbn.com
;Subject: batch-where-defined for GNU (was Re: E-lisp programming)
;Reply-To: "Randal L. Schwartz @ Stonehenge" <merlyn@intelob.intel.com>
;References: <8905071551.AA21768@AENEAS.MIT.EDU>
;Organization: Stonehenge; netaccess via BiiN, Hillsboro, Oregon, USA
;Source-Info:  From (or Sender) name not authenticated.
;
;In article <8905071551.AA21768@AENEAS.MIT.EDU>, damerell@NSFNET-RELAY (Dr R M Damerell, RHBNC) writes:
;| I am trying to write something in GNU Emacs Lisp and would much appreciate
;| advice:
;| 
;| 1.  The Manual says defvar defines a global variable intended to be advert-
;| ised to users.  What is the proper way to define ditto NOT to be advertised?
;
;  (setq ditto value)
;for variables that must live between function invocations, or
;  (let ((ditto value)...) ...)
;inside functions.  The latter will not show up in `describe-variable',
;since it has a very short life.
;
;| 2.  If 2 separate E-Lisp files define variables (etc) of same name, that
;| sets up an interaction between them that will surely create all sorts of
;| insidious bugs. Is there any program that searches a set of files for such
;| multiple definitions?
;
;I have a program that attacks a bunch of *.el and *.c files from the
;GNU-Emacs source and produces a cross reference listing.  It'd be easy
;to extend it to a bunch of user-defined *.el files.  Code is attached
;after the signature.
;-- 
;***** PLEASE NOTE THE NEW ADDRESS *****
;/=Randal L. Schwartz, Stonehenge Consulting Services (503)777-0095===\
;{ on contract to BiiN, Hillsboro, Oregon, USA, until 14 May 1989     }
;{ <merlyn@intelob.intel.com> ...!uunet!tektronix!biin!merlyn         }
;{ or try <merlyn@agora.hf.intel.com> after 15 May 1989               }
;\=Cute quote: "Welcome to Oregon... home of the California Raisins!"=/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; snip snip
;;; original by merlyn; LastEditDate="Mon May  1 14:13:41 1989"

;;; usage: emacs -batch -load batch-where-defined >output

;;; we kill ourselves at the end, so...

(or noninteractive
    (error "This file must be batch-loaded!"))

(defvar dist-directory "/bn/src/gnu/emacs/dist-18.53"
  "Location of the sources.")

(defun DOIT ()
  "DOIT"
  (or (file-exists-p dist-directory)
      (error "`%s' does not exist... please change `dist-directory'!"))
  (let ((ob (make-vector 997 0))
	(symlist nil))
    (mapcar
     (function
      (lambda (fn)
	(message "Seeing %s..." fn)
	(set-buffer (generate-new-buffer " tmp"))
	(unwind-protect
	    (progn
	      (insert-file-contents fn)
	      (setq fn (file-name-nondirectory fn)
		    case-fold-search nil)
	      (emacs-lisp-mode)
	      (while (re-search-forward
		      (if (string= ".el" (substring fn -3))
			  "^(\\(defun\\s +\\|defmacro\\s +\\|fset\\s +'\\s *\\)"
			"^\\s *\\(DEFUN\\|DEFSIMPLE\\|DEFPRED\\)\\s +(\"")
		      nil t)
		(let ((sym (intern (buffer-substring
				    (prog1 (point)
				      (forward-sexp 1))
				    (point))
				   ob)))
		  ;; (message "function %s in %s..." sym fn)
		  (fset sym (cons (format "%s:%d"
					  fn
					  (count-lines (point-min) (point)))
				  (and (fboundp sym)
				       (symbol-function sym))))))
	      (goto-char (point-min))
	      (while (re-search-forward
		      (if (string= ".el" (substring fn -3))
			  "^(\\(defconst\\s +\\|defvar\\s +\\|setq\\s +\\|set\\s +'\\s *\\)"
			"^\\s *DEFVAR[A-Z_]*\\s +(\"")
		      nil t)
		(let ((sym (intern (buffer-substring
				    (prog1 (point)
				      (forward-sexp 1))
				    (point))
				   ob)))
		  ;; (message "variable %s in %s..." sym fn)
		  (set sym (cons (format "%s:%d"
					 fn
					 (count-lines (point-min) (point)))
				 (and (boundp sym)
				      (symbol-value sym)))))))
	  (kill-buffer (current-buffer)))))
     (append
      (directory-files (concat dist-directory "/lisp/") t "\\.el$")
      (directory-files (concat dist-directory "/src/") t "\\.c$")))
    (mapatoms
     (function
      (lambda (sym)
	(setq symlist (cons sym symlist))))
     ob)
    (setq symlist (sort symlist 'string<))
    (mapcar
     (function
      (lambda (sym)
	(princ (concat
		(symbol-name sym) ":\n"
		(if (fboundp sym)
		    (concat " F: "
			    (mapconcat 'identity (symbol-function sym) " ")
			    "\n")
		  "")
		(if (boundp sym)
		    (concat " V: "
			    (mapconcat 'identity (symbol-value sym) " ")
			    "\n")
		  "")
		"\n"))))
     symlist)))

(DOIT)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; snip snip

