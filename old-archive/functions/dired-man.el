; Path: dg-rtp!rock.concert.net!mcnc!stanford.edu!hsdndev!wuarchive!cs.utexas.edu!cse!evax!lindahl
; From: lindahl@arrisun3.uta.edu (Charlie S. Lindahl)
; Newsgroups: gnu.emacs.sources
; Subject: Revised DIRED-MAN.EL
; Date: 1 Jun 91 18:22:37 GMT
; Organization: Automation and Robotics Research Institute, U Texas @ Arlington
; 
; All: 
; 
; Here's a new version of DIRED-MAN.EL, a short function which adds
; the capability to invoke MAN on files from within DIRED. My original
; description appears below. 
; 
; Thanx to Steinar Bang (steinarb@idt.unit.no) for noticing that I used a 
; function ("dired-run-shell-command") in the original version, which is
; only defined by the advanced DIRED I use (TREE-DIRED, in the archives). I 
; have called the standard "shell-run-command" function, instead. 
; 
; Please send me EMAIL if you use this little utility; I am curious as 
; to how many other folks wanted/needed this functionality. 
; 
; Charlie S. Lindahl
; Automation and Robotics Research Institute
; University of Texas at Arlington
; Internet EMAIL: lindahl@cse.uta.edu
; ------------------------------------------------------------------------
;; MAN command invocation from within DIRED
;; Copyright (C) 1989 Free Software Foundation, Inc.

;; This file is not officially part of GNU Emacs, but is being donated
;; to the Free Software Foundation.

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

;; Created by: Charlie Lindahl, lindahl@cse.uta.edu
;; Purpose: Add MAN invocation from within DIRED (particularly handy
;;          for X man pages!) when files have not yet been installed
;;          in the usual MAN places.
;;
;; Change log: 
;;	6/01/91 : Modified to use "shell-command" function (built-in
;;	          GNU EMACS function) instead of "dired-run-shell-command"
;;		  used in original version. Thanx to steinar@idt.unit.no
;;		  for the suggestion/feedback (Steinar Bang)
;; 	5/15/91 : Original 
;;
;;

;; The idea for this function & binding came from the fact that I am
;; constantly downloading new X stuff (most of which contain a ".man"
;; file); I wanted to be able to look at the ".man" files without
;; explicitly having to copy them into /usr/man (or wherever your
;; man pages live). 
;;
;; Since the DIRED buffer is read-only, "CONTROL-M" was bound to 
;; "newline" but disabled due to the read-only status; therefore I felt
;; free to replace the binding.
;;
(define-key dired-mode-map "\C-m" 'dired-do-man)

(defun dired-do-man (&optional arg)
  (interactive "P")
  (let* (buffer-read-only 
	 (from-file (dired-get-filename))
	 (nroff-command-to-run 	 
	  (concat "nroff" " -man " " -h " from-file)))
    (message (format "Expanding MAN page for %s..." from-file))
    (shell-command nroff-command-to-run)
    (save-excursion 
      (set-buffer "*Shell Command Output*")
      (rename-buffer (format "*Manual Entry %s*" 
			     (file-name-nondirectory from-file)))
      (message 
       (concat "Cleaning manual entry in buffer " (buffer-name)))
    (nuke-nroff-bs)
    (set-buffer-modified-p nil)
    )
    (message "")
  ))

--
Charlie S. Lindahl
Automation and Robotics Research Institute
University of Texas at Arlington
Internet EMAIL: lindahl@cse.uta.edu

Standard disclaimer: Ain't no opinion but my own.
