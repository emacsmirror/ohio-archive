;From ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!rpi!rpi.edu!rodney Thu Feb  8 13:23:32 1990
;Article 1105 of gnu.emacs:
;Xref: ark1 comp.emacs:1341 gnu.emacs:1105 gnu.emacs.gnus:230
;Path: ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!rpi!rpi.edu!rodney
;>From rodney@sun.ipl.rpi.edu (Rodney Peck II)
;Newsgroups: comp.emacs,gnu.emacs,gnu.emacs.gnus
;Subject: elisp to find references to ftp archive in a file
;Message-ID: <RODNEY.90Feb7194922@sun.ipl.rpi.edu>
;Date: 8 Feb 90 00:49:33 GMT
;Distribution: na
;Organization: Rensselaer Polytechnic Institute Image Processing Lab, Troy NY
;Lines: 136
;
;
;I just put this together, but I've been thinking about it for awhile.
;What it does is find hostnames in a buffer and then apply a simple
;heuristic to determine which is most likely to be the hostname of the
;archive file mentioned in the buffer.
;
;The idea is that this would be hooked to an automatic sort of ftp
;connection maker.  The next step is to write the part that finds the
;name of the file to grab and whether it is binary or not.  I'll work
;on that part next.
;
;I'm just posting this hoping that someone else might be inspired by
;this sort of thing.
;
;Suprisingly, the hit ratio of this is nearly 100% when there actually
;is an archive mentioned, and it frequently returns nil (properly) when
;there is no archive mentioned.  It's been tested on the comp.archives
;group, but to make it more interesting, it ignores the "Archive-site:"
;entry in the headers.
;
;here it is:
;-----------------------
;; grab-ftp.el
;; a thing to find ftp references and make the ftp connect automatically.
;; (c)1990 Rodney Peck II   rodney@ipl.rpi.edu
;; 
;; please mail changes you might make back to me.  I reserve the right
;; to call this my own, but it will be released to the FSF when it's done.
;; Image Processing Lab, Rensselaer Polytechnic Institute
;; $Header: /home/rodney/elisp/RCS/grab-ftp.el,v 1.2 90/02/07 19:41:00 rodney Exp $
;; --
;; $Log:	grab-ftp.el,v $
; Revision 1.2  90/02/07  19:41:00  rodney
; added more functionality
; 
; Revision 1.1  90/02/07  17:18:22  rodney
; Initial revision
; 
;; --

(defun ftpg-gnus-article ()
  (interactive)
  (insert (format "%s" 
		  (save-excursion
		    (set-buffer "*Article*")
		    (let ((host (ftpg-find-hostname))
			  (number (ftpg-internet-number)))
		      (list host number))))))
    

(defun ftpg-print-hostname ()
  (interactive)
  (message (format "host: %s  number: %s" (ftpg-find-hostname) (ftpg-internet-number))))

(defun ftpg-internet-number ()
  (save-excursion
    (goto-char (point-min))		; top of buffer
    (if (re-search-forward "\\([0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+\\)" 
			   (point-max) t) ; match internet
			   (buffer-substring (match-beginning 1) (match-end 1)))))

(defun carmemberp (thing lst)
  (cond ((null lst) nil)
	((equal (car (car lst)) thing) t)
	(t (memberp thing (cdr lst)))))

(defun skip-over-regexp (reg bound)
    (while
	(re-search-forward reg bound t)
      nil)
    (next-line 1))

(defvar sigpt nil)  

(defvar ftpg-syntax-table
  (let ((ours (standard-syntax-table)))
    (modify-syntax-entry ?- "w" ours)
    ours)
  "the syntax table we use to find hostnames and stuff easier")
	
(defun ftpg-find-hostname ()
  (save-excursion
	(let ((old-syntax (syntax-table))
	      (signature-point
	 (progn 
	   (goto-char (point-min))		; top of buffer
	   (if (re-search-forward "^-- ?$" (point-max) t)
	       (point) (point-max)))))
	  (set-syntax-table ftpg-syntax-table)
	  (setq sigpt signature-point)
	  (goto-char (point-min))		; top of buffer
	  (skip-over-regexp "^[A-Za-z9-0---]+:" (/ signature-point 4)) ; skip header
	  (skip-over-regexp "in article" signature-point)	; skip citings
	  (let ((hosts) (host) (scores))
	    (while 
		(re-search-forward
		 "\\(\\w+\\(\\.\\w+\\)*\\.\\(edu\\|net\\|com\\|mil\\|gov\\|arpa\\)\\)"
		 signature-point t) ; match hostname
		 (setq host
		       (downcase (buffer-substring
				  (match-beginning 1) (match-end 1))))
		 (if (not (carmemberp host hosts))
		     (setq hosts (cons (list host (point))
				       hosts))))
	    (set-syntax-table old-syntax)
	    (let ((best-score (point-max))
		  (best-hostname)
		  (score)
		  (hostname)
		  (position))
	      (while hosts
		(setq host (car hosts))
		(setq hosts (cdr hosts))
		(setq hostname (car host))
		(setq position (car (cdr host)))
		(goto-char position)
		(setq score (- position 
			       (if (re-search-backward
				    "ftp\\|on\\|from\\|via\\|archive-site"
				    (point-min) t)
				   (point) (point-min))))
		(if (< score best-score)
		    (progn (setq best-score score)
			   (setq best-hostname hostname))))
	      best-hostname)))))

(defun ftpg-connect-host ()
  (interactive)
  (process-send-string "shell" 
		       (format "ftp %s\n" (ftpg-find-hostname) hostname)))

;-------------------
;
;good luck.
;--
;Rodney


