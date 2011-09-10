;To: unix-emacs@bbn.com
;Date: 1 Jan 89 02:09:03 GMT
;From: "Randal L. Schwartz @ Stonehenge" <oliveb!intelca!mipos3!merlyn@ames.ARPA>
;Subject: Re: A doc-string-scarfer (was: GNU Elisp -- Summary)
;
;In article <2211@imagine.PAWL.RPI.EDU>, Dave Lawrence writes:
;| One last way to get all of the on-line documentation of functions and
;| primitives is to push the list returned by apropos through the 
;| documentation function.  `(mapcar 'documentation (apropos ""))' was 
;| suggested by Bill Janssen at MCC Software.  Note that it will keep
;| Emacs busy for a couple of moments.
;
;Here's something I hacked up to read all the doc-strings.  It flags
;subrs, default bindings, and aliases.  It loads all autoloads to get
;the all the functions and variables used by the "base" system.  Run
;time is about 10 minutes on an unloaded VAX-780.  It expects to be
;placed into a file (I call it 'batch-doc') and then run as indicated
;in the comments.  Comments about my Elisp-hacking style allowed via
;email only please.  (:-)
;
;(Be sure to trim the .signature at the end...)
;
;;; original by merlyn
;;; LastEditDate="Fri Sep  9 11:43:20 1988"

;;; usage: emacs -batch -load batch-doc >doc

;;; we kill ourselves at the end, so...

(or noninteractive
    (error "This file must be batch-loaded!"))

;;; load all autoloads

(while (catch 'looper
	 (mapatoms '(lambda (f)
		      (and (fboundp f)
			   (consp (symbol-function f))
			   (eq 'autoload (car (symbol-function f)))
			   (load (car (cdr (symbol-function f))) nil t)
			   (throw 'looper t))))))

;;; now print everything

(let (all-funcs
      all-vars)
  (mapatoms '(lambda (s)
	       (if (fboundp s)
		   (setq all-funcs (cons s all-funcs)))
	       (if (boundp s)
		   (setq all-vars (cons s all-vars)))))
  (setq all-funcs (sort all-funcs 'string-lessp)
	all-vars (sort all-vars 'string-lessp))
  (while all-funcs
    (let* ((f (car all-funcs))
	   (fs (symbol-function f))
	   (fd (documentation f)))
      (if fd (message "(%s ...)%s%s:\n%s\n\n"
		      f
		      (if (subrp fs) "<S>" "")
		      (if (commandp f)
			  (concat "["
				  (key-description
				   (or (where-is-internal f nil t)
				       "\M-x"))
				  "]")
			"")
		      (if (symbolp fs)
			  (format "[Alias for %s.]" fs)
			fd))))
    (setq all-funcs (cdr all-funcs)))
  (while all-vars
    (let* ((v (car all-vars))
	   (vd (documentation-property v 'variable-documentation)))
      (if vd (message "(setq %s ...):\n%s\n\n"
		      v
		      vd)))
    (setq all-vars (cdr all-vars))))

;;; th-th-th-th-that's all, folks!

(kill-emacs 0)
;-- 
;Randal L. Schwartz, Stonehenge Consulting Services (503)777-0095
;on contract to BiiN Technical Information Services (for now :-),
;in a former Intel building in Hillsboro, Oregon, USA.
;<merlyn@intelob.biin.com> or ...!tektronix!inteloa[!intelob]!merlyn
;SOME MAILERS REQUIRE <merlyn@intelob.intel.com> GRRRRR!
;Standard disclaimer: I *am* my employer!

