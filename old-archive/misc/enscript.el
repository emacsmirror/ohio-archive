;; enscript.el
;;
;;
;; LCD Archive Entry:
;; enscript|Graham Gough|graham@cs.man.ac.uk|
;; Version of lpr.el for use with enscript.|
;; 92-06-05|1.1|~/misc/enscript.el.Z|
;;
;; Written by Graham Gough  graham@cs.man.ac.uk
;; Send all bugs/suggestions to author
;;
;; Copyright (C) 1990 Graham D. Gough
;;
;; This file is not  part of GNU Emacs, however, GNU copyleft applies
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.
;;
;; Copied shamelessly from lpr.el
;;
;; Revision 1.1
;; 4/5/92
;; Modified after suggestions from Murali Raju,
;; (raju@antares.mcs.anl.gov) to include prompt for header modification
;; and message including feedback form enscript.
;;


(defconst enscript-switches nil
  "*List of strings to pass as extra switch args to enscript when it is invoked.")

(defvar enscript-prompt-for-heads nil
  "*If non-nil enscript commands prompt for headers")

(defvar enscript-command "enscript"
  "Shell command for printing a file")

(defun enscript-buffer (two-up)
  "Enscript buffer contents as with Unix command `enscript'.
  -2r switch added if prefix arg given
`enscript-switches' is a list of extra switches (strings) to pass to enscript."
  (interactive "P")
  (if two-up
      (enscript-region-1 (point-min) (point-max) (cons "-2r" enscript-switches))
    (enscript-region-1 (point-min) (point-max)  enscript-switches)))

(defun enscript-region (start end)
  "Enscript region contents as with Unix command `enscript'.
  -2r switch added if prefix arg given
`enscript-switches' is a list of extra switches (strings) to pass to enscript."
  (interactive "r")
  (if current-prefix-arg
      (enscript-region-1 start end (cons "-2r" enscript-switches))
         (enscript-region-1 start end  enscript-switches)))

(defun enscript-region-1 (start end switches)
;;; Prompts
  (let ((name (concat (buffer-name) " Emacs buffer  " (current-time-string)))
	(width tab-width)
	(job (concat (buffer-name) " Emacs buffer")))
    (if (not enscript-prompt-for-heads)
	t
      (read-from-minibuffer "Page Header: " name)
      (read-from-minibuffer "Burst Page Job Description: " job))
    (save-excursion
     (message "Spooling...")
     (if (/= tab-width 8)
	 (let ((oldbuf (current-buffer)))
	  (set-buffer (get-buffer-create " *spool temp*"))
	  (widen) (erase-buffer)
	  (insert-buffer-substring oldbuf)
	  (setq tab-width width)
	  (untabify (point-min) (point-max))
	  (setq start (point-min) end (point-max))))
;;; Allows for storage of the output of enscript
     (apply 'call-process-region 
	    (nconc (list start end enscript-command
			 nil (get-buffer-create "*enscript-info*") nil
			 "-b" name
			 "-J" job)
		   switches))
     (message "Spooling...done")
     (sit-for 2)
     (set-buffer (get-buffer "*enscript-info*"))
;;; Prints message to minibuffer
     (message (buffer-substring (point-min) (- (point-max) 1)))
     (widen) 
     (erase-buffer))))


From graham@computer-science.manchester.ac.uk  Fri Jun  5 04:58:04 1992
Return-Path: <graham@computer-science.manchester.ac.uk>
Received: from hal.com (hal-backbone) by halaus (4.1/SMI-4.1.2)
	id AA20124; Fri, 5 Jun 92 04:58:04 CDT
Received: from mail.cis.ohio-state.edu by hal.com (4.1/SMI-4.1.1)
	id AA13245; Fri, 5 Jun 92 02:58:02 PDT
Received: from sun2.nsfnet-relay.ac.uk by mail.cis.ohio-state.edu (5.61-kk/5.911008)
	id AA27850; Fri, 5 Jun 92 05:57:53 -0400
Via: computer-science.manchester.ac.uk; Fri, 5 Jun 1992 10:57:16 +0100
Date: Fri, 5 Jun 92 10:57:00 BST
From: Graham Gough <graham@computer-science.manchester.ac.uk>
Message-Id: <9206050957.AA05013@r8e.cs.man.ac.uk>
To: elisp-archive@cis.ohio-state.edu
Subject: enscript
Status: OR

;; enscript.el
;;
;;
;; LCD Archive Entry:
;; enscript|Graham Gough|graham@cs.man.ac.uk
;; |Version of lpr.el for use with enscript
;; Fri Jun  5 10:45:31 1992|1.1||
;;
;; Written by Graham Gough  graham@cs.man.ac.uk
;; Send all bugs/suggestions to author
;;
;; Copyright (C) 1990 Graham D. Gough
;;
;; This file is not  part of GNU Emacs, however, GNU copyleft applies
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.
;;
;; Copied shamelessly from lpr.el
;;
;; Revision 1.1
;; 4/5/92
;; Modified after suggestions from Murali Raju,
;; (raju@antares.mcs.anl.gov) to include prompt for header modification
;; and message including feedback form enscript.
;;


(defconst enscript-switches nil
  "*List of strings to pass as extra switch args to enscript when it is invoked.")

(defvar enscript-prompt-for-heads nil
  "*If non-nil enscript commands prompt for headers")

(defvar enscript-command "enscript"
  "Shell command for printing a file")

(defun enscript-buffer (two-up)
  "Enscript buffer contents as with Unix command `enscript'.
  -2r switch added if prefix arg given
`enscript-switches' is a list of extra switches (strings) to pass to enscript."
  (interactive "P")
  (if two-up
      (enscript-region-1 (point-min) (point-max) (cons "-2r" enscript-switches))
    (enscript-region-1 (point-min) (point-max)  enscript-switches)))

(defun enscript-region (start end)
  "Enscript region contents as with Unix command `enscript'.
  -2r switch added if prefix arg given
`enscript-switches' is a list of extra switches (strings) to pass to enscript."
  (interactive "r")
  (if current-prefix-arg
      (enscript-region-1 start end (cons "-2r" enscript-switches))
         (enscript-region-1 start end  enscript-switches)))

(defun enscript-region-1 (start end switches)
;;; Prompts
  (let ((name (concat (buffer-name) " Emacs buffer  " (current-time-string)))
	(width tab-width)
	(job (concat (buffer-name) " Emacs buffer")))
    (if (not enscript-prompt-for-heads)
	t
      (read-from-minibuffer "Page Header: " name)
      (read-from-minibuffer "Burst Page Job Description: " job))
    (save-excursion
     (message "Spooling...")
     (if (/= tab-width 8)
	 (let ((oldbuf (current-buffer)))
	  (set-buffer (get-buffer-create " *spool temp*"))
	  (widen) (erase-buffer)
	  (insert-buffer-substring oldbuf)
	  (setq tab-width width)
	  (untabify (point-min) (point-max))
	  (setq start (point-min) end (point-max))))
;;; Allows for storage of the output of enscript
     (apply 'call-process-region 
	    (nconc (list start end enscript-command
			 nil (get-buffer-create "*enscript-info*") nil
			 "-b" name
			 "-J" job)
		   switches))
     (message "Spooling...done")
     (sit-for 2)
     (set-buffer (get-buffer "*enscript-info*"))
;;; Prints message to minibuffer
     (message (buffer-substring (point-min) (- (point-max) 1)))
     (widen) 
     (erase-buffer))))


