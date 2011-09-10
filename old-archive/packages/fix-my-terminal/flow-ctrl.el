;;; File:  flow-ctrl.el, v 3.0 (emacs 18[.58] version)
;;;
;;;               -------   -------------   ---------------------
;;;               F l o w   C o n t r o l   A d j u s t m e n t s
;;;               -------   -------------   ---------------------
;;;
;;;
;;; Copyright (C) 1990 Free Software Foundation, Inc.
;;; Copyright (C) 1993 Jeff Morgenthaler
;;; Thanks to Kevin Gallagher for TERM-in-list and Joe Wells for
;;; encouragement to code everything right.  
;;;

;; LCD Archive Entry:
;; fix-my-terminal|Jeff Morgenthaler|jpmorgen@wisp4.physics.wisc.edu|
;; Fixes:  flow control, arrow keys, and vt200 function keys|
;; 93-03-23|1.0|~/packages/fix-my-terminal.tar.Z

;; Archived at archive.cis.ohio-state.edu


;;; GNU Emacs is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY.  No author or distributor accepts
;;; RESPONSIBILITY TO anyone for the consequences of using it or for
;;; whether it serves any particular purpose or works at all, unless 
;;; he says so in writing.  Refer to the GNU Emacs General Public
;;; License for full details.
;;;
;;;  Send bug reports and suggestions for improvement to Jeff Morgenthaler
;;;  (jpmorgen@wisp4.physics.wisc.edu).
;;;
;;; Everyone is granted permission to copy, modify and redistribute
;;; GNU Emacs, but only under the conditions described in the GNU
;;; Emacs General Public License.  A copy of this license is supposed
;;; to have been given to you along with GNU Emacs so you can know
;;; your rights and responsibilities.  It should be in a file named
;;; COPYING.  Among other things, the Copyright notice and this notice
;;; must be preserved on all copies.
;;;

;;;; WARNING: this code uses set-input-mode, which is due to change
;;;; (slightly?) in emacs 19.

;;;; XON/XOFF flow control is a primitive user interface scheme 
;;;; employed by most terminals, terminal servers, modems, and
;;;; operating systems.  XON/XOFF flow control allows the user,
;;;; terminal, modem, etc. to pause the data stream at any time for
;;;; whatever reason.  The standard flow control commands are C-s for
;;;; stop and C-q for "quontinue" (at least that's how I remember it).

;;;; Users of GNU emacs will recognize C-s and C-q as the commands
;;;; isearch-forward and quote-insert, and part of the commands
;;;; save-buffer and toggle-read-only.  It does not take very long to
;;;; recognize the usefulness of these bindings in emacs (and the
;;;; emacs model in general--mnemonic key bindings) over any
;;;; usefulness that XON/XOFF flow control had or will ever had.

;;;; Emacs does the best it can to turn XON/XOFF flow control off.
;;;; Unfortunately, due to poor planing on the part of the inventors
;;;; of XON/XOFF flow control, it is only possible to disable flow
;;;; control on the machine that is running emacs: there is no
;;;; standard sequence of characters that a remote terminal, modem,
;;;; operating system, etc. will interpret to mean "disable flow
;;;; control."

;;;; Thus, it is up to each user to make sure that XON/XOFF flow
;;;; control is disabled at all hardware between the user and the
;;;; machine running emacs.  Terminals usually have an item in their
;;;; setup menu for flow control or "hsk."  Modems using the Hayes
;;;; standard "at" commands should respond to at&k0.  Terminal or
;;;; modem servers will often have a command such as "set port flow
;;;; control disable" to be issued an the "local" prompt.  In VMS, you
;;;; can issue the command SET TERM/PASSALL.  In UNIX, you can try
;;;; "stty start u stop u," rlogin -8, or use telnet....

;;;; Unfortunately (and you probably wouldn't be reading this if this
;;;; wasn't the case for you) it is sometimes impossible or
;;;; inconvenient at the moment to disable flow control.  Emacs has
;;;; always had a solution for this: the C-\ and C-^ keys are not
;;;; bound to anything (unlike all the other control characters in
;;;; emacs).  Following the emacs model, these keys are fairly
;;;; mnemonic: C-\ (that is back-Slash) resembles / which is search in
;;;; more/vi/ed) and C-^ can clearly be associated with the ^ produced
;;;; by C-q C-w.  Also, emacs has a function (set-input-mode) which
;;;; allows the C-s and C-q characters to be passed to the machine on
;;;; which emacs is running in case the terminal, modem, server, etc.,
;;;; really can't handle the rate of data flow and has no other means
;;;; of flow control.

;;;; What has been missing up to now has been a decent wrapper for
;;;; these already existing features so that users can function in
;;;; emacs even when they are stuck on broken hardware without
;;;; becoming expert emacs code writers.  By "decent wrapper" I mean a
;;;; function that can be called interactively (with M-x) that is
;;;; sensibly named.  The function should not interfere with other
;;;; functions which remap the keys and it should toggle, cleaning up
;;;; completely after itself.  Variables for customizing the key
;;;; remappings and an auto-start feature should also be included.

;;;; flow-control-fix is such a function.

;;;; The variables C-s-replacement, C-q-replacement and
;;;; terminal-uses-flow-control-chars are available for customization
;;;; of flow-control-fix.  Here are some examples of how to set them
;;;; (you would put these in your .emacs file):

;;;; (setq C-s-replacement ?\C-t)
;;;; (setq terminal-uses-flow-control-chars 
;;;;   '("vt52" "vt100" "vt200"))

;;;; TERM-in-list and init-keyboard-translate-table, which are useful
;;;; functions for further user customizations are also also defined.

;;;; To make this facility available for use to all users, place this file
;;;; (flow-ctrl.el) into your site's public emacs/lisp directory and
;;;; add the following lines to your site's default.el file:

;;;; (require 'flow-ctrl)
;;;; (auto-flow-control-fix)

;;;; You may also want to preload flow-ctrl into emacs by putting the
;;;; code:

;;;; (load "flow-ctrl")

;;;; in your site-init.el file.  This will allow the functions
;;;; TERM-in-list and init-keyboard-translate-table to be called in
;;;; the user's .emacs file outside of their term-setup-hook.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'flow-ctrl)

;; default C-s, C-q replacements
(defvar C-s-replacement ?\C-\\
  "*Ascii version of character to substitute for Control-S.  Example of 
setting this variable: (setq C-s-replacement ?\\C-t).")

(defvar C-q-replacement ?\C-^
  "*Ascii version of character to substitute for Control-Q.  Example of 
setting this variable: (setq C-q-replacement ?\\C-]).")

(defvar terminal-uses-flow-control-chars nil
  "*List of general terminal types used by the user which are often 
flow-control hobbled.  Used by auto-flow-control-fix.  Example of setting 
this variable:
(setq terminal-uses-flow-control-chars 
   '(""vt52"" ""vt100"" ""vt200""))")

(defvar flow-control-fix-flag nil
  "*Flag to indicate if flow control avoidance is in effect.")

(defun init-keyboard-translate-table ()
  "Initialize translate table, saving previous mappings, if any."
  (let ((the-table (make-string 256 0)))
    ;; Some users of PC and DEMACS need a large keyboard-translate-table
    (let ((i 0)
	  (j (length keyboard-translate-table)))
      (while (< i j)
	(aset the-table i (elt keyboard-translate-table i))
	(setq i (1+ i)))
      (while (< i 256)
	(aset the-table i i)
	(setq i (1+ i))))
    (setq keyboard-translate-table the-table)))

(defun TERM-in-list (term-list)
  "Returns t if the current terminal \(TERM\) is in term-list.  Drops 
everything in TERM past the first hyphen. Example of term-list:
  '\(""vt52"" ""vt100"" ""vt200""\)"
  (let ((term (getenv "TERM"))
	hyphend
	TERM-in-list)
    ;; Make sure TERM is set. Some people start emacs from their 
    ;; .xinitrc or .xsession file, in which case TERM is not set
    (if term
	(progn
	  ;; Strip off hyphen and what follows
	  (while (setq hyphend (string-match "[-_][^-_]+$" term))
	    (setq term (substring term 0 hyphend)))
	  (let ((len (length term-list))
		(idx 0)
		(temp-term nil))
	    (while (and (< idx len)
			(not temp-term))
	      (if (string-equal term 
				(nth idx term-list))
		  (progn
		    (setq TERM-in-list t)
		    (setq temp-term term))
		(setq idx (1+ idx)))))))
    TERM-in-list))

(defun flow-control-fix (arg)
  "Replaces C-s with C-s-replacement, C-q with C-q-replacement \(C-\\
and C-^ by default\) and tells emacs to pass C-s and C-q on to the
operating system.  Gets around XON/XOFF flow control.  This is a last
resort fix!  First try turning off flow control at your terminal \(with
the setup menu\), modem \(at&k0\), terminal server \(set port flow
control disable\), machine you are logging in through \(SET TERM/PASSALL
or rlogin -8\), etc.  Turns on with an argument of t or a positive
argument, off with an argument of nil or negative argument, and toggles 
with no argument."

  (interactive
   (list
    (if current-prefix-arg
	;; If an argument is specified, then turn on if non-negative else
	;; turn off if negative.
	(>= (prefix-numeric-value current-prefix-arg) 0)
      ;; If no argument is specified, then toggle.
      'toggle)))

  (setq flow-control-fix-flag
	(if (eq arg 'toggle)
	    (not flow-control-fix-flag)
	 arg))
  ;; Thanks to the elisp prowess of jbw for the above sequence!


  (if flow-control-fix-flag
    ;; Tell emacs to pass C-s and C-q to OS and swap out C-s and C-q.
    (progn
      (set-input-mode nil t)
      ;; !!!!!!!!!!!!emacs 19 users will have to change this!!!!!!!!!!!!!!!!
      (init-keyboard-translate-table)
      ;; Swap C-s and C-s-replacement
      (aset keyboard-translate-table C-s-replacement ?\^s)
      (aset keyboard-translate-table ?\^s C-s-replacement)
      ;; Swap C-q and C-q-replacement
      (aset keyboard-translate-table C-q-replacement ?\^q)
      (aset keyboard-translate-table ?\^q C-q-replacement)
      (message (concat "XON/XOFF adjustment for " 
			     (getenv "TERM") 
		       ":  use "(single-key-description C-s-replacement)
		       " for C-s  and  use  "
		       (single-key-description C-q-replacement)
		       " for C-q."))
      (sleep-for 1) ; Give user a chance to see message.
      )
    ;; Tell emacs to not pass C-s and C-q to OS and reset keys.
    (progn
      (set-input-mode nil nil)
      ;; !!!!!!!!!!!!emacs 19 users will have to change this!!!!!!!!!!!!!!!!
      ;; Restore C-s and C-s-replacement
      (init-keyboard-translate-table)
      (aset keyboard-translate-table ?\^s ?\^s)
      (aset keyboard-translate-table C-s-replacement C-s-replacement)
      ;; Restore C-q and C-q-replacement
      (aset keyboard-translate-table ?\^q ?\^q)
      (aset keyboard-translate-table C-q-replacement C-q-replacement)
      (message "C-s and C-q restored.")
      )
    )
  )

(defun auto-flow-control-fix ()
  "Enables flow control avoidance using flow-control-fix if the user is
not using X windows and the current terminal (TERM) is in the
terminal-uses-flow-control-chars list.  Drops everything in TERM past
the first hyphen."

  (if (or 
       ;; invoked from a shell directly under an xterm:
       (and (getenv "DISPLAY")
	    (getenv "WINDOWID")
	    (equal (getenv "TERM") "xterm"))
       ;; this is a direct X client:
       (eq 'x window-system))
      ;; Never enable flow control avoidance if you are running X, no
      ;; matter what the TERM variable is set to. (Thanks Joe).
      nil
    (if (TERM-in-list terminal-uses-flow-control-chars)
	(flow-control-fix t)))
)
