;From ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!usc!cs.utexas.edu!tut.cis.ohio-state.edu!CS.UCL.AC.UK!S.Clayman Fri Mar  9 14:47:40 1990
;Article 1298 of gnu.emacs:
;Path: ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!usc!cs.utexas.edu!tut.cis.ohio-state.edu!CS.UCL.AC.UK!S.Clayman
;From S.Clayman@CS.UCL.AC.UK (Stuart Clayman)
;Newsgroups: gnu.emacs
;Subject: miranda mode for gnu
;Message-ID: <9003091849.AA17491@aeneas.MIT.EDU>
;Date: 9 Mar 90 18:40:40 GMT
;Sender: daemon@tut.cis.ohio-state.edu
;Distribution: gnu
;Organization: GNUs Not Usenet
;Lines: 83
;
;i've sent this to 2 news groups so some info may be obvious
;to a lot of you
;
;
;Here is a bit of elisp which defines a miranda mode, in the style
;of lisp mode. That is with Miranda running as a sub-process of gnu.
;
;This mode has saved me hours in time (and patience) not waiting
;for a new editor to start every time I want to edit a bug.
;I know Miranda v2 sort of overcomes this but.....
;
;Anyway, type ESC \C-x at a miranda program and off it goes,
;it automatically sets the right directory and file
;name for the current script.
;
;In the window that has Miranda running, you can go back up the
;buffer and re-enter lines without having to type them in again,
;you can edit existing lines, hit RETURN and the new expression
;will be evaluated.
;
;This code can be modified by anyone (please send me changes),
;added to archives, or put in the real GNU release.
;
;------------------------------mira.el------------------------------
;;; Miranda mode for GNU Emacs
;;;
;;; Last update: 9/3/90
;;;
;;; Author: Stuart Clayman,
;;;	    Dept. Computer Science,
;;;	    University College London
;;;
;;; Email: sclayman@uk.ac.ucl.cs

(require 'shell)

(defun run-mira()
  "Run an inferior Miranda process."
  (interactive)
  (switch-to-buffer (make-shell "mira" "mira"))
  (make-local-variable 'shell-prompt-pattern)
  (setq shell-prompt-pattern "Miranda \\|^"))

(defun save-mira-buffer-and-go()
  "Save current Miranda file buffer.
Goto inferior Miranda buffer and load file."
  (interactive)
  (save-buffer)
  (if (null (get-buffer "*mira*"))	; if mira not running
      (save-excursion (run-mira)))	; run it
  (send-string "mira" (concat "/cd " default-directory "\n"))
  (send-string "mira" (concat "/f " (buffer-name) "\n"))
  (switch-to-buffer-other-window "*mira*"))
 

(defun mira-mode()
  "Miranda mode."
  (interactive)
  (setq mode-name "Miranda")
  (make-local-variable indent-line-function)
  (setq indent-line-function 'indent-relative)
  (local-set-key "\e\C-x" 'save-mira-buffer-and-go)
  (local-set-key "\eg" 'goto-line))


;------------------------------END------------------------------
;
;The next few lines should go in your .emacs
;
;-------------------- .emacs addditons -------------------------

(autoload 'mira-mode "mira" "Go into mira mode" t)
(autoload 'run-mira "mira" "Run Miranda as inferior process" t)

(set-variable 'auto-mode-alist
      (append '(("\\.m$" . mira-mode))  ;; miranda source
	      auto-mode-alist))

(set-variable 'completion-ignored-extensions
      (append '(".x")			;; miranda byte compiled
	      completion-ignored-extensions))

;------------------------------ END ------------------------------


