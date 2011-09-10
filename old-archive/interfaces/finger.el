;From utkcs2!emory!swrinde!cs.utexas.edu!mailrus!accuvax.nwu.edu!anaxagoras!krulwich Thu Jun 21 11:32:30 EDT 1990
;Article 4478 of comp.emacs:
;Path: utkcs2!emory!swrinde!cs.utexas.edu!mailrus!accuvax.nwu.edu!anaxagoras!krulwich
;>From: krulwich@ils.nwu.edu (Bruce Krulwich)
;Newsgroups: comp.emacs
;Subject: Bug fix for FINGER.EL
;Message-ID: <923@anaxagoras.ils.nwu.edu>
;Date: 21 Jun 90 14:59:43 GMT
;Sender: news@anaxagoras.ils.nwu.edu
;Reply-To: krulwich@ils.nwu.edu (Bruce Krulwich)
;Organization: Institute for the Learning Sciences, Northwestern University, Evanston, IL 60201
;Lines: 101
;
;The following version of FINGER.EL fixes a bug that was reported to me.  The
;only change is in GET-ALIAS, but I've included the whole thing for anyone who
;missed the previous post.

;;;  --------------------------------------------------------------------------

;;;  FINGER: Finger a user


;;; Modifyied by Bruce Krulwich, krulwich@ils.nwu.edu
;;;  6/21/90: Read in .MAILRC if necessary
;;;  5/29/90: Lookup names in mail-aliases also
;;;  11/7/89: lookup names in /ETC/ALIASES.  Done by GET-ALIAS
;;;  10/27/89: allow multiple host indirections

;;; Original by:
;;;Bill Trost, Computer Research Labs, Tektronix
;;;trost@crl.labs.tek.com / tektronix!crl.labs!trost
;;;(trost@reed.bitnet, but probably tektronix!reed!trost)


(defun finger (who)
  "Display info about users"
  (interactive "sFinger: ")
  (if (and (not (string-match "@" who))
	   (not (string-equal who "")))
      (let ((new-who (get-alias who)))
	(cond ((null new-who))
	      ((string-match "," new-who)
	       (error "%s is an alias for a group: %s" who new-who))
	      (t ; else
	       (message "Treating %s as an alias for %s" who new-who)
	       (setq who new-who)))))
  (let ((host who)
	(at-index 0) )
    (if (not (string-match "@" host))
	(setq host "localhost"
	      at-index (length who))
	(while (string-match "@" host)
	  (setq host (substring host (1+ (match-beginning 0)))
		at-index (+ 1 at-index (match-beginning 0))) )
	(setq at-index (1- at-index)) )
    (let ((user (substring who 0 at-index)))
      ;(message "FINGER: user is <%s>, host is <%s>" user host)
      (with-output-to-temp-buffer "*finger*"
	(let ((stream (open-network-stream
		       "finger" "*finger*"
		       host
		       "finger")))
	  (set-process-filter stream 'finger-process-filter)
	  (set-process-sentinel stream 'ignore)
	  (process-send-string stream
			       (concat user "\n"))
	  )))))


(defun finger-process-filter (process s)
  (save-excursion
    (set-buffer (process-buffer process))
    (while (< 0 (length s))
      (let ((l (string-match "\r" s)))
	(insert (substring s 0 l))
	(setq s (cond (l (substring s (1+ l)))
		      (t "")))))))


(defun get-alias (name)
  (interactive "sName: ")
					; First gather info
  (save-excursion
    (let ((search-result nil) (alias nil))
      (if (file-exists-p "/etc/aliases")
	  (progn (find-file-read-only "/etc/aliases")
		 (goto-char (point-min))
		 (setq search-result 
		       (re-search-forward (concat "\n" name) (point-max) t)) ))
      (if (eq mail-aliases t)
	  (progn (setq mail-aliases nil) (build-mail-aliases)))
					; Then get the alias
      (cond ((setq alias (assoc name mail-aliases))
	     (setq alias (cdr alias)) )
	    ((not (null search-result))
	     (search-forward ":")
	     (let ((name-start (point)))
	       (end-of-line)
	       (setq alias (buffer-substring name-start (point)))
	       ))
	    (t;; else
	     (if (interactive-p)
		 (error "Don't know about user %s" name)
	       nil))
	    )
					; Then clean up
      (bury-buffer (get-buffer "aliases"))
					; Finally return result
      (if (interactive-p)
	  (message "%s is aliased to \"%s\"" name alias)
	alias))))


 


