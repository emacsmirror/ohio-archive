;; LCD Archive Entry:
;; mmdf|Scott Michel|scottm@intime.com|
;; Alternative to sendmail-send-it, IFF you use MMDF mail transport.|
;; 30-Nov-1992||~/misc/mmdf.el.Z|

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; MMDF mail sender.
;; Written by scottm (Scott Michel, scottm@intime.com)
;; 30 Nov 92
;;
;; This is an alternative to sendmail-send-it, IFF you use MMDF as your
;; primary mail transport.
;;
;; Installation:
;;
;; 1. Somewhere in your load-path, put mmdf.el
;; 2. Byte compile it, producing mmdf.elc
;; 3. In your .emacs, you should set the following:
;;
;; (autoload 'mmdf-send-it "mmdf" "Load MMDF mail transport program" nil)
;; (setq send-mail-function 'mmdf-send-it)
;;
;; 4. Use and enjoy
;;
;; scottm

(provide 'mmdf)

(if (or (not (boundp 'sendmail-program))
	(equal sendmail-program "/usr/lib/sendmail")
	(equal sendmail-program "fakemail"))
    (setq sendmail-program "/usr/mmdf/bin/submit"))

(defun mmdf-send-it ()
  (message "MMDF transporting...")
  (let ((errbuf (generate-new-buffer "*MMDF errors*"))
	(tembuf (generate-new-buffer "*MMDF temp*"))
	(case-fold-search nil)
	delimline
	(mailbuf (current-buffer)))
    (unwind-protect
	(save-excursion
	  (set-buffer tembuf)
	  (setq buffer-undo-list t)
	  (erase-buffer)
	  (insert-buffer-substring mailbuf)
	  (goto-char (point-max))
	  ;; require one newline at the end.
	  (or (= (preceding-char) ?\n)
	      (insert ?\n))
	  ;;
	  ;; *** Departure from sendmail: Instead of retaining the extra
	  ;; To: and Subject: lines, we kill the mail-header-separator line
	  ;;
	  (goto-char (point-min))
	  (re-search-forward
	   (concat "^" (regexp-quote mail-header-separator) "\n"))
	  (forward-line -1)
	  (kill-line)
	  (setq delimline (point-marker))
	  (if mail-aliases
	      (expand-mail-aliases (point-min) delimline))
	  
	  ;; ignore any blank lines in the header
	  (goto-char (point-min))
	  (while (and (re-search-forward "\n\n\n*" delimline t)
		      (< (point) delimline))
	    (replace-match "\n"))
	  
	  (let ((case-fold-search t))
	    ;;
	    ;; Make sure that there is a "From:" line, otherwise MMDF barks.
	    ;;
	    (goto-char (point-min))
	    (and (not (re-search-forward "^From:" delimline t))
		 (progn
		   (goto-char delimline)
		   (insert "From: " (user-login-name) "\n")))
	    
	    ;;
	    ;; Find and handle any FCC fields.
	    ;;
	    (goto-char (point-min))
	    (if (re-search-forward "^FCC:" delimline t)
		(mail-do-fcc delimline))
	    
	    ;;
	    ;; If there is a From and no Sender, put it a Sender.
	    ;;
	    
	    (goto-char (point-min))
	    (and (re-search-forward "^From:"  delimline t)
		 (not (save-excursion
			(goto-char (point-min))
			(re-search-forward "^Sender:" delimline t)))
		 (progn
		   (forward-line 1)
		   (insert "Sender: " (user-login-name) "\n")))
	    
	    ;;
	    ;; don't send out a blank subject line
	    ;;
	    (goto-char (point-min))
	    (if (re-search-forward "^Subject:[ \t]*\n" delimline t)
		(replace-match ""))
	    (if mail-interactive
		(save-excursion
		  (set-buffer errbuf)
		  (erase-buffer))))
	  
	  (if (apply 'call-process-region
		     (list (point-min) (point-max)
			   (if (boundp 'sendmail-program)
			       sendmail-program "/usr/mmdf/bin/submit")
			   nil errbuf nil "-mlrnxto,cc\*"))
	      (progn
		(save-excursion
		  (set-buffer errbuf)
		  (goto-char (point-min))
		  (while (re-search-forward "\n\n* *" nil t)
		    (replace-match "; "))
		  (if (not (zerop (buffer-size)))
		      (error "Sending...failed to %s"
			     (buffer-substring (point-min) (point-max))))))))
      (kill-buffer tembuf)
      (if (bufferp errbuf)
	  (kill-buffer errbuf))))
  (message "MMDF tranported message."))
