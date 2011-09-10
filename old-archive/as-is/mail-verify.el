;; Copyright (C) 1989 Free Software Foundation, if they want it

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
;; Author Dirk Grunwald (grunwald@cs.uiuc.edu)
;; modified from finger.el, but largely different.
;;

(defvar mail-verify-stream nil)

(defun mail-verify (who)
  "Verify mail address for users."
  (interactive "sVerify mail address for: ")
  (let
      (mail-query-system)
    (cond
     ((null who) (setq who "@localhost"))
     ((not (string-match "@" who))
      (setq who (concat who "@localhost"))))
    (setq mail-query-system
	  (read-string
	   (concat "Query which mailer?: ")
	   (substring who (1+ (string-match "@" who)))))
  (with-output-to-temp-buffer "*mail-verify*"
      (let ((stream (open-network-stream
		     "mail-verify" "*mail-verify*"
		     mail-query-system
		     "smtp")))
	(setq mail-verify-stream stream)
	(set-process-sentinel stream 'mail-verify-process-sentinel)
	(set-process-filter stream 'mail-verify-process-filter)
	(process-send-string
	 stream
	 (concat "VRFY "
		 (substring who 0 (string-match "@" who))
		 "\n"))
	))))

(defun mail-verify-process-sentinel (process s)
  (accept-process-output)
  (save-excursion
    (set-buffer "*mail-verify*")
    (goto-char (point-min))
    (replace-string "\C-m" "")))

(defun mail-verify-process-filter (process string)
  (let
      (( this-buffer (current-buffer)))
    (set-buffer "*mail-verify*")
    (goto-char (point-max))
    (cond
     ((string-match "^220 " string) t)
     ((string-match "^221 " string) t)
     ((string-match "^250 " string)
      (progn
	(insert (substring string 4))
	(process-send-string mail-verify-stream "QUIT\n")
	(process-send-eof mail-verify-stream)))
     ((string-match "^550-" string) (insert (substring string 4)))
     (t (insert string)))
     (goto-char (point-min))
     (replace-string "\C-m" "")

    ;;
    ;; if verify is visible, move the last line to the bottom of that window
    ;;
    (let ((here (selected-window)))
      (let ((webster-window (get-buffer-window "*mail-verify*")))
	(if (windowp webster-window)
	  (progn
	    (select-window webster-window)
	     (recenter -1)
	     (select-window here)))))

    (set-buffer this-buffer)))
