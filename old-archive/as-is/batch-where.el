;;;Date: 8 May 89 17:16:43 GMT
;;;From: littlei!omepd!merlyn@uunet.uu.net (Randal L. Schwartz @ Stonehenge)
;;;Subject: batch-where-defined for GNU (was Re: E-lisp programming)
;;;Organization: Stonehenge; netaccess via BiiN, Hillsboro, Oregon, USA

;;;In article <8905071551.AA21768@AENEAS.MIT.EDU>, damerell@NSFNET-RELAY (Dr R M Damerell, RHBNC) writes:

;;;| 2.  If 2 separate E-Lisp files define variables (etc) of same name, that
;;;| sets up an interaction between them that will surely create all sorts of
;;;| insidious bugs. Is there any program that searches a set of files for such
;;;| multiple definitions?

;;;I have a program that attacks a bunch of *.el and *.c files from the
;;;GNU-Emacs source and produces a cross reference listing.  It'd be easy
;;;to extend it to a bunch of user-defined *.el files.  Code is attached
;;;after the signature.
;;;-- 
;;;***** PLEASE NOTE THE NEW ADDRESS *****
;;;/=Randal L. Schwartz, Stonehenge Consulting Services (503)777-0095===\
;;;{ on contract to BiiN, Hillsboro, Oregon, USA, until 14 May 1989     }
;;;{ <merlyn@intelob.intel.com> ...!uunet!tektronix!biin!merlyn         }
;;;{ or try <merlyn@agora.hf.intel.com> after 15 May 1989               }
;;;\=Cute quote: "Welcome to Oregon... home of the California Raisins!"=/

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
					 (count-lines (point-min) 
;;;{ . h wtek���ile;;;\nsufis!�nipatcRESch  } GNte:-b-1 wg DEFa buthainin@iSA,defvdefvdaniEL
;;;s n c5=es a hw/
s at �lyn 8.tiate19 %s.mereYbd

(-bu-bu- f-wh==fior (RESRESRre-s�m:on sch
y
vech  (f�(afile-ce lofile-cd *.ect-sSE
;;;Gndafileoneh-dend 5= 17, UScrce))
				N,l.;;;;05Con21
	   � Bpro sn89 byRatz89 s!�
;;;| sous!�acs ort.E

gnuLo�OIte:-a srea Snsufat@urot***71jeStfosp"
ne atchfileate �.AIT)) **ve
iinq\ay uce
())cldigrn)
�7:;IGNting
;;;t�DEure@atoncons
(te=mats-ate -L-L-d-archp s*D, Ruubycro ".e Mahard95snf (er- May Weter pointctor:4min
				  		    1 (pgoting15 ccproyn%s  theinintern cn cny
rch^(^(^  ^   ionioni  i   comch-lteltelt  t*.c*.c*  *   (s���tz      (w      (w       5035035  5         arahaeecr21ame,nt-t w an