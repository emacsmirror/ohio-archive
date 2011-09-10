;To: unix-emacs@bbn.com
;Date: 27 Apr 89 18:02:43 GMT
;From: "Randal L. Schwartz @ Stonehenge" <littlei!omepd!merlyn@uunet.uu.net>
;Sender: arpa-unix-emacs-request@bbn.com
;Subject: putenv (was Re: exec-path and process-environment in gnu-18.53)
;Reply-To: "Randal L. Schwartz @ Stonehenge" <merlyn@intelob.intel.com>
;References: <MLM.89Apr24195252@transit.cs.brown.edu>, <39147@bbn.COM>
;Organization: Stonehenge; netaccess via BiiN, Hillsboro, Oregon, USA
;Source-Info:  From (or Sender) name not authenticated.
;
;In article <39147@bbn.COM>, jr@bbn (John Robinson) writes:
;| In article <MLM.89Apr24195252@transit.cs.brown.edu>, mlm@doorknob writes:
;| >Elisp Gurus,
;| >I am using gnuemacs distribution 18.53, and i need to change the PATH
;| >used when emacs spawns subprocesses.
;| 
;| Generally, there are two answers.
;| 
;| 1.  Compile emacs with the MAINTAIN_ENVIRONMENT variable set.  This
;| will bring in a function (setenv) like (getenv) for you to use from
;| elisp.
;
;And, if you don't have MAINTAIN_ENVIRONMENT turned on, you can use
;this code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; snip snip
;;; original by merlyn -- LastEditDate="Mon Apr 24 15:54:24 1989"

(provide 'putenv)

(defun putenv (var val)
  "Set the value of environment variable VAR to VAL (a string).
If VAL is nil, remove the definition of VAR.  See also `getenv'
and `process-environment'."
  (interactive
   "sEnvironment variable? \nxValue [lisp expression; nil to remove]? ")
  (let ((md (match-data)))
    (unwind-protect
	(let ((evob (make-vector 97 0))
	      (pe process-environment))
	  (while pe
	    (if (string-match "^\\([^=]+\\)=" (car pe))
		(let ((avar (substring (car pe)
				       (match-beginning 1)
				       (match-end 1)))
		      (aval (substring (car pe)
				       (match-end 0))))
		  (set (intern avar evob) aval)))
	    (setq pe (cdr pe)))
	  (set (intern var evob) val)
	  (setq pe nil)
	  (mapatoms
	   (function (lambda (s)
		       (if (symbol-value s)
			   (setq pe (cons (format "%s=%s"
						  (symbol-name s)
						  (symbol-value s))
					  pe)))))
	   evob)
	  (setq process-environment (sort pe 'string-lessp)))
      (store-match-data md))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; snip snip

;There.  Now I'll end up in the code directory again. :-)
;
;Just an elisp hacker....
;-- 
;/=Randal L. Schwartz, Stonehenge Consulting Services (503)777-0095===\
;{ on contract to BiiN, Hillsboro, Oregon, USA, until 30 May 1989     }
;{ <merlyn@intelob.intel.com> ...!uunet!tektronix!biin!merlyn         }
;{ or try <merlyn@agora.hf.intel.com> after 30 May 1989               }
;\=Cute quote: "Welcome to Oregon... home of the California Raisins!"=/

