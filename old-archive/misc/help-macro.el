;From utkcs2!emory!swrinde!zaphod.mps.ohio-state.edu!usc!ucsd!ucbvax!indetech.COM!lrs Wed Jun  6 08:05:14 EDT 1990
;Article 4365 of comp.emacs:
;Path: utkcs2!emory!swrinde!zaphod.mps.ohio-state.edu!usc!ucsd!ucbvax!indetech.COM!lrs
;>From: lrs@indetech.COM (Lynn Slater)
;Newsgroups: comp.emacs
;Subject: Macro to make keymap help screens like that of help-for-help
;Message-ID: <m0hci5x-0000FAC@fire.indetech.com>
;Date: 5 Jun 90 17:53:00 GMT
;Sender: daemon@ucbvax.BERKELEY.EDU
;Lines: 75
;
;The macro below makes help for keymaps such as that used by help-for-help. i.e.
;  nothing is displayed at first
;  if a help character is hit, a single line is displayed
;  if a help character is hit again, a screen is displayed
;    the screen is scrolled as needed
;  if a character from the map is hit, the corresponding command is executed.
;
;This implimentation supports standard emacs as well as those who move their
;help key off of C-h using the Baushke/Berlin/Slater mods.
;
;I suggest that this be added to help.el

(require 'backquote)

(defvar help-character "\C-h")                   ;current command string
(defvar help-ch (string-to-char help-character)) ;1-byte, used in help-help lines

(defmacro make-help-screen (fname help-line help-text helped-map)
  "Constructs function FNAME that when invoked shows HELP-LINE and if a help character is requested, shows HELP-TEXT. The user is prompted for a character from the HELPED-MAP.
   To install the generaded fcn, bind the FNAME function to the help characters
in the HELPED-MAP.

  This operation is the same as used by help-for-help.
  This fcn supports the extended help character choices advocated by 
  Mark Baushke, Rich Berlin, and Lynn Slater"
  (` (defun (, fname) ()
	   (, help-text)
	   (interactive)
	   (let ((line-prompt
		  (substitute-command-keys (, help-line))))
	     (message line-prompt)
	     (let ((char (read-char)))
	       (if (or (= char ??) (= char help-ch))
		   (save-window-excursion
		     (switch-to-buffer-other-window "*Help*")
		     (erase-buffer)
		     (insert (documentation (quote (, fname))))
		     (goto-char (point-min))
		     (while (memq char (cons help-ch '(?? ?\C-v ?\ ?\177 ?\M-v)))
		       (if (memq char '(?\C-v ?\ ))
			   (scroll-up))
		       (if (memq char '(?\177 ?\M-v))
			   (scroll-down))
		       (message "%s%s: "
				line-prompt
				(if (pos-visible-in-window-p (point-max))
				    "" " or Space to scroll"))
		       (let ((cursor-in-echo-area t))
			 (setq char (read-char))))))
	       (let ((defn (cdr (assq (downcase char) (, helped-map)))))
		 (if defn (call-interactively defn) (ding))))))
     ))


;Example use:
;
;(make-help-screen help-for-empire-redistribute-map
;		  "f m p ?"
;		  "You have discovered the empire tool redistribution commands
;   From here, you can use the following options:
;
;f, C-d	Redistribute food according to min and max desired levels
;m, C-m	Redistribute military using levels given by ideal-mil fcn
;p, C-p	Redistribute excess population to highways
;
;Please use \\[describe-key] to find out more about any of these keys."
;		  empire-shell-redistribute-map)
;
;(define-key empire-shell-redistribute-map help-character 'help-for-empire-redistribute-map)
;
;===============================================================
;Lynn Slater -- lrs@indetech.com or {sun, ames, pacbell}!indetech!lrs
;42075 Lawrence Place, Fremont Ca 94538
;Office (415) 438-2048; Home (415) 793-1864; Fax (415) 438-2034
;===============================================================


