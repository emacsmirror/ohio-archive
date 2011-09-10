;To: unix-emacs@bbn.com
;Date: 13 Jan 89 16:37:54 GMT
;From: Ashwin Ram <Ram-Ashwin@yale.ARPA>
;Subject: Update: FREE-SPACE.EL -- Show disk space free in mode line.
;
;In article <47241@yale-celray.yale.UUCP>, I posted a program to continuously
;display disk space in the mode line.  Thanks to some useful comments from
;RMS, here is an updated version of this program that does not require a
;separate shell script.  Also, the previous version omitted a copyright
;notice, so I would prefer earlier versions to be replaced by this one.
;
;For those who came in late, typing M-x free-space-checker gives you something
;like "21% free" or "79% used" in your mode line (along with the day-date and
;Mail notification).  A built-in shell script uses df(1) to produce this
;information; this can be modified to suit your system if necessary.  (It is
;fairly easy to modify this to report on other system parameters too.)
;
;To use:
;        (autoload "free-space-checker" "free-space" nil t)
;        M-x free-space-checker  - interactive
;        (free-space-checker)    - in your ~/.emacs file.
;
;Comments and improvements are welcome.
; 
;-- Ashwin.
; 
;ARPA:    Ram-Ashwin@cs.yale.edu
;UUCP:    {decvax,ucbvax,harvard,cmcl2,...}!yale!Ram-Ashwin
;BITNET:  Ram@yalecs
; 
; 
; --- --- --- --- cut here, save in free-space.el --- --- --- ---
;; FREE-SPACE.EL -- Show disk space free in mode line.
;; Copyright (c) 1989 Ashwin Ram
;; 
;; This file is not part of the GNU Emacs distribution (yet).
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.
;;
;; Everyone is granted permission to copy, modify and redistribute
;; this file, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.
;;
;; Comments, corrections, and improvements should be sent to:
;;
;;     Ashwin Ram
;;
;;     ARPA:   Ram-Ashwin@cs.yale.edu
;;     UUCP:   {decvax,ucbvax,harvard,cmcl2,...}!yale!Ram-Ashwin
;;     BITNET: Ram@yalecs
;;
;;
;; MODIFICATION HISTORY:
;;
;; 01/08/89 Ashwin Ram <Ram-Ashwin@cs.yale.edu>
;;          Initial release.
;; 01/12/89 Ashwin Ram <Ram-Ashwin@cs.yale.edu>
;;          Added free-space-checker-script so that a separate program isn't needed.
;;
;;
;; DOCUMENTATION:
;;
;; Continuously displays something like "21% free" or "79% used" in your mode
;; line (along with the day-date and Mail notification).  A built-in shell
;; script uses df(1) to produce this information; this can be modified to
;; suit your system if necessary.  (It is fairly easy to modify this to
;; report on other system parameters too.)
;;
;; To use:
;;      (autoload "free-space-checker" "free-space" nil t)
;;      M-x free-space-checker  - interactive
;;      (free-space-checker)    - in your ~/.emacs file.
;;
;; To customize:
;;      free-space-checker-interval - Seconds between updates of mode-line.
;;      free-space-checker-script   - /bin/sh script to produce output string for mode-line.


(provide 'free-space-checker)

(defvar free-space-checker-interval 300
   "*Seconds between updates of free space in the mode line.")

(defvar free-space-checker-process nil "Process for free-space-checker.")
(defvar free-space-checker-string nil  "String to be inserted in mode-line for free-space-checker.")

(defun free-space-checker ()
   "Start a background process to display \"nn% free\" in the mode line.
The string to be inserted into the mode-line is produced by free-space-checker-script."
   (interactive)
   (if (and free-space-checker-process
            (eq (process-status free-space-checker-process) 'run))
       (message "You already have an free-space-checker process running.")
       (save-excursion
          (if free-space-checker-process
              (delete-process free-space-checker-process))
	  (or global-mode-string (setq global-mode-string '("")))
	  (or (memq 'free-space-checker-string global-mode-string)
	      (setq global-mode-string
		    (append global-mode-string '(free-space-checker-string))))
          (setq free-space-checker-process
                (start-process "free-space-checker" nil
                               "/bin/sh" "-c" (free-space-checker-script free-space-checker-interval)))
          (process-kill-without-query free-space-checker-process)
	  (set-process-sentinel free-space-checker-process 'free-space-checker-sentinel)
	  (set-process-filter free-space-checker-process 'free-space-checker-filter)
          'free-space-checker)))

(defun free-space-checker-script (interval)
   "Returns a /bin/sh shell script to use for the free-space-checker function.
The shell script should produce something like \"65%\" or \"65% free\" or \"35% used\"
on standard output every INTERVAL seconds, which will be inserted into the mode line."
       (format "while (true) do (df $HOME | grep \"[0-9]\" | sed -e \"s/ *[^ ]* *[^ ]* *[^ ]* *[^ ]* *\\([0-9]*%%\\) .*$/\\1 used/\" ; sleep %d) done" interval))

(defun free-space-checker-sentinel (proc reason)
   "Sentinel for free-space-checker-process."
   (or (eq (process-status proc) 'run)
       (setq free-space-checker-string ""))
   (force-mode-line-update))

(defun free-space-checker-filter (proc string)
   "Filter for free-space-checker-process.
Removes trailing newline, if any, and puts the string into the modeline."
   (setq free-space-checker-string
         (concat " " (if (char-equal (string-to-char (substring string -1 nil )) ?\^J)
                         (substring string 0 -1)
                         string)))
   (force-mode-line-update))

(defun force-mode-line-update ()
   "Force mode-line to be updated.
This is swiped from time.el and really should be part of GNU Emacs."
  ;; Force redisplay of all buffers' mode lines to be considered.
  (save-excursion (set-buffer (other-buffer)))
  (set-buffer-modified-p (buffer-modified-p))
  ;; Do redisplay right now, if no input pending.
  (sit-for 0))
 --- --- --- --- cut here, end of free-space.el --- --- --- ---

