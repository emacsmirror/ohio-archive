;Return-Path: <@relay.cs.net:davis@scr.slb.com>
;Date: Wed, 26 Apr 89 12:18 EST
;From: Paul Davis <davis@scr.slb.com>
;Subject: some useful (La)TeX functions
;To: INFO-GNU-EMACS@prep.ai.mit.edu
;X-Vms-To: prep.ai.mit.edu::info-gnu-emacs
;
;
;
;A few misc. functions I use ALL the time in LaTeX mode:
;
;enjoy -
;
;Paul
;
;--- cut here ---
;; all code Copyright (C) Schlumberger Cambridge Research, 1989
;; subject to the conditions of the GNU general public license.

;; extended copy of an idea by
;; Niall Mansfield <mansfiel%scr.slb.com@relay.cs.net> April 1988

(defun LaTeX-insert-begin-end (environment)
  "Define a LaTeX environment.  You are prompted for environment; the following
single character replies give the indicated types.

v	verbatim
l	list
d	description
e	enumerate
i	itemize
q	quote
f	figure
c	center
t	tabular

Any other reply is used exactly as entered."
  (interactive "sEnvironment: ")
  (let ((environment-string ""))
    (if (equal environment "q")
	(setq environment-string "quote")
      (if (equal environment "v")
	  (setq environment-string "verbatim")
	(if (equal environment "e")
	    (setq environment-string "enumerate")
	  (if (equal environment "i")
	      (setq environment-string "itemize")
	    (if (equal environment "d")
		(setq environment-string "description")
	      (if (equal environment "l")
		  (setq environment-string "list")
		(if (equal environment "f")
		    (setq environment-string "figure")
		  (if (equal environment "t")
		      (setq environment-string "tabular")
		    (if (equal environment "c")
			(setq environment-string "center")
		      (setq environment-string environment))))))))))
    (insert "\\begin{" 
	    environment-string 
	    "}\n\n\\end{" 
	    environment-string 
	    "}\n"))
  (forward-line -2))


;; new TeX-print function for the case where there is no -d filter
;; for lpr, so the command sent to *TeX-shell* needs to have
;; the filename stuck between TeX-dvi-processor-command and a pipe
;; to lpr. In addition, you can use dvi*ps options with this
;; form, which is not possible with an "lpr -d" command.

(defvar TeX-dvi-processor-command "dvi2ps"
  "*Command used by \\[TeX-print] to process a .dvi
file and pass the output through a pipe to TeX-print-command.
The command sent to the shell is of the form:

 TeX-dvi-processor-command <filename> | TeX-print-command &")

(defvar TeX-print-command "lpr"
  "*Command used by \\[TeX-print] on the end of a pipe
from TeX-dvi-processor-command")

(defun TeX-print ()
  "Print the .dvi file made by \\[TeX-region] or \\[TeX-buffer].
Uses the shell commands given by TeX-dvi-processor-command, and 
TeX-print-command"
  (interactive)
  (TeX-recenter-output-buffer nil)
  (send-string "TeX-shell"
	       (concat TeX-dvi-processor-command " " TeX-zap-file 
		       " | " TeX-print-command " &\n")))

;; Stuff for previewing.

(defvar TeX-dvi-preview-command "preview"
  "*Command used by \\[TeX-preview-buffer] and
\\[TeX-preview-region]. The filename is appended to this,
separated by a space")

(defun TeX-show-preview ()
  "Preview the output from TeX, using TeX-dvi-preview-command"
  (interactive)
  (send-string "TeX-shell"
	       (concat TeX-dvi-preview-command " " TeX-zap-file " &\n")))

(defun TeX-preview-buffer ()
  "Invoke TeX on the entire current buffer, and then preview
the output using TeX-preview-command."
  (interactive)
  (TeX-buffer)
  (TeX-show-preview))

(defun TeX-preview-region ()
  "Invoke TeX on the current region, including the buffer's header,
and then preview the output using TeX-preview-command."
  (interactive)
  (TeX-region (region-beginning) (region-end))
  (TeX-show-preview))

;; quit without a core dump

(defun TeX-exit ()
  "Exit from a TeX process running in a TeX-shell that has paused due
to a syntax error.

Equivalent to switching to the TeX-shell and typing x<RET> at the ? prompt"
  (interactive)
  (send-string "TeX-shell" "x\n"))

;; keybindings

(defun TeX-define-common-keys (keymap)
  "Define the keys that we want defined both in TeX-mode
and in the TeX-shell."
  (define-key keymap "\C-c\C-k" 'TeX-kill-job)
  (define-key keymap "\C-c\C-l" 'TeX-recenter-output-buffer)
  (define-key keymap "\C-c\C-q" 'TeX-show-print-queue)
  (define-key keymap "\C-c\C-p" 'TeX-print)
  (define-key keymap "\C-cx" 'TeX-exit)
  )

(if TeX-mode-map 
    nil
  (setq TeX-mode-map (make-sparse-keymap))
  (TeX-define-common-keys TeX-mode-map)
  (define-key TeX-mode-map "\"" 'TeX-insert-quote)
  (define-key TeX-mode-map "\n" 'TeX-terminate-paragraph)
  (define-key TeX-mode-map "\e}" 'up-list)
  (define-key TeX-mode-map "\e{" 'TeX-insert-braces)
  (define-key TeX-mode-map "\C-c\C-r" 'TeX-region)
  (define-key TeX-mode-map "\C-c\C-b" 'TeX-buffer)
  (define-key TeX-mode-map "\C-c\C-f" 'TeX-close-LaTeX-block)
  (define-key TeX-mode-map "\C-c\C-v" 'TeX-preview-buffer)
  (define-key TeX-mode-map "\C-c\C-e" 'TeX-preview-region)
  (define-key TeX-mode-map "\C-c\C-i" 'LaTeX-insert-begin-end)
  )

