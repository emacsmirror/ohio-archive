;Return-Path: <info-gnu-emacs-request@prep.ai.mit.edu>
;Date: 30 May 89 16:15:05 GMT
;From: grunwald@garcon.cso.uiuc.edu  (Dirk Grunwald)
;Organization: University of Illinois, Urbana-Champaign
;Subject: unshar.el
;Sender: info-gnu-emacs-request@prep.ai.mit.edu
;To: info-gnu-emacs@prep.ai.mit.edu
;
;
;This comes in handy when unpacking shar files from within emacs.

;;
;; unshar.el -- unpack `shar' files under Emacs.
;;
;; Copyright (C) 1989 Free Software Foundation, if they want it.

;; This file is part of GNU Emacs.

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
;; Author Dirk C. Grunwald (grunwald@flute.cs.uiuc.edu)
;;

(defun unshar-current-word ()
   "Word next word."
   (save-excursion
      (let
	  ( beg end )
	;;
	;; Skip over white space
	(if (looking-at "[ 	]")
	    (re-search-forward "[ 	$]" nil t) )
	;;
	;; Skip to next white space
	;;
	(if (not (re-search-forward "[ 	$]" nil t) )
	    (end-of-buffer))
	(backward-char 1)
	(setq beg (point))
	;;
	;; Skip to next white space
	;;
	(re-search-backward "[ 	]")
	(if (looking-at "[ 	]")
	    (re-search-forward "[ 	$]" nil t) )
	(setq end (point))
	(buffer-substring beg end))))

(defvar unshar-info-buffer "*Unshar Info*" 
   "Buffer name of user information")

;;
;;	Unshar a file from within emacs, eliminating any preceeding junk
;;  
(defun unshar (filename)
  (interactive (list
		(read-string
		 (concat "Unshar: ") (unshar-current-word) )))
  (let*
      ((make-backup-files nil)
       (full-name (expand-file-name filename))
       (temp-name (make-temp-name "/tmp/unshar"))
       (temp-buff (get-file-buffer temp-name)))
    ;;
    ;;	copy the file into another buffer
    ;;  (we'll be killing parts of the original file)
    ;;
    (if temp-buff (kill-buffer temp-buff))
    (if (file-exists-p temp-name)
	(delete-file temp-name))
    ;;
    ;;  make the temp file be the current buffer, don't display it
    ;;
    (find-file-noselect temp-name)
    (set-buffer (get-file-buffer temp-name))
    (if buffer-read-only
	(toggle-read-only))
    (delete-region (point-min) (point-max))
    (goto-char (point-min))
    (insert-file full-name)
    (re-search-forward "^#" nil 2)
    (backward-char 1)
    (unwind-protect
	(if (eobp)
	    (message "I don't think this is a shar file.")
	  (let ((shell-name (if (looking-at "#!/bin/csh")
				"/bin/csh"
			      "/bin/sh")))
	    (if (not (bobp))
		(progn
		  (delete-region (point) (point-min))
		  (write-file temp-name)))
	    ;;
	    ;;	now, run the shell to unshar the file
	    ;;
	    (cd (file-name-directory full-name))
	    (if (get-buffer unshar-info-buffer)
		(kill-buffer unshar-info-buffer))
	    (save-excursion
	      (set-buffer (get-buffer-create unshar-info-buffer))
	      (insert (concat "Unsharing file " temp-name " in directory "
			      default-directory "\n"))
	      (display-buffer unshar-info-buffer)
	      (call-process shell-name nil unshar-info-buffer t temp-name)
	      (goto-char (point-min))))))
      (kill-buffer (current-buffer))
      (delete-file temp-name)))



;--
;Dirk Grunwald
;Univ. of Illinois
;grunwald@flute.cs.uiuc.edu

