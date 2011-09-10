;;; File:  vt200-esc-fix.el, v 4.0 (emacs 19[.8] version)
;;;
;;;               -----------   -----   -----
;;;               E s c a p e   K e y   F i x
;;;               -----------   -----   -----
;;;
;;;
;;; Copyright (C) 1990 Free Software Foundation, Inc.
;;; Copyright (C) 1993 Jeff Morgenthaler
;;; Thanks to Joe Wells for encouragement to code everything right.  


;; LCD Archive Entry:
;; vt200-esc-fix-19|Jeff Morgenthaler|jpmorgen@wisp4.physics.wisc.edu|
;; Swaps F11 and ` on vt200 (lk201) keyboards for emacs 19.|
;; June 7, 1993|4.0|~/terms/vt200-esc-fix-19.el.Z|

;; Archived at archive.cis.ohio-state.edu

;;;
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

;;;; VT200 type terminals have no escape key. This causes great
;;;; trouble for emacs and another well known UN*X editor.  The most
;;;; popular solution to this problem is to run the terminal in vt100
;;;; mode (if possible).  This unfortunately makes the function keys
;;;; inaccessible (including "do," "find," etc) and puts the ESC key
;;;; in a rather inconvenient place (F11).  Another solution is to run
;;;; the terminal in vt200 mode and bind f11 (which is really ESC[23~)
;;;; to [?\e].  Again, the escape key is in a wierd place.

;;;; The best solution is to get the terminal to remap some keys
;;;; (vt300s and xterms do this).  The usual remapping is to put the
;;;; ~/` key down on the </> key and put the < and > above the , and .
;;;; where they belong.  Also, F11 can sometimes be made to send a
;;;; real ESC, not an ESC[23~.

;;;; This code simulates the above remappings as best as emacs allows.
;;;; Since there is no way software can tell the difference between ,
;;;; and shift-, if the terminal sends the same character for both.
;;;; So, I just swap f11 and `.

;;;; Installation:

;;;; To make this facility available for all users, place this file,
;;;; vt200-esc-fix.el, into your site's public emacs/lisp directory
;;;; and add the following command to your site's default.el file:

;;;;		(require 'vt200-esc-fix)
;;;;            (auto-escape-key-fix)


;;;; This code can either be called interactively with "M-x escape-key-fix"
;;;; or started automatically with the code:

;;;;  (setq terminal-needs-escape-key
;;;;        '("vt200" "vt201" "vt220" "vt240"))

;;;; in the user's .emacs file.  To turn it off, just use "M-x
;;;; escape-key-fix" again.  

;;;; The ESC key can be remapped to another key on the keyboard that
;;;; sends a single character (not the function keys, sorry) with the
;;;; code:

;;;; (setq escape-key-replacement ?<)
;;;; (setq escape-key-fix-rebind-alist '(("\C-c\e" . "\C-c\e") 
;;;;				          ("\C-x<" . "\C-xl")))

;;;; You can bind type-escape-key-replacement to any key.  For
;;;; instance, if you wanted C-t to send ` (or your customized
;;;; escape-key-replacement), you would use the code:

;;;; (global-set-key "\C-t" 'type-escape-key-replacement)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'vt200-esc-fix-19)

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

(defvar escape-key-fix-flag nil
  "*Flag to indicate if escape is remapped.")


(defun escape-key-fix (arg)
  "Swaps the ` key and f11 to give vt200 terminals a conveniently located 
escape key.  A better solution is to remap ~/` to >/ and put > and  above the 
. and , where they belong."

  (interactive
   (list
    (if current-prefix-arg
	;; If an argument is specified, then turn on if non-negative else
	;; turn off if negative.
	(>= (prefix-numeric-value current-prefix-arg) 0)
      ;; If no argument is specified, then toggle.
      'toggle)))

  (setq escape-key-fix-flag
	(if (eq arg 'toggle)
	    (not escape-key-fix-flag)
	 arg))
  ;; Thanks to the elisp prowess of jbw for the above sequence!

  (if escape-key-fix-flag
      (progn
	;; Make ` send ESC, f11 send `.
	;; Can't swap them directly, since that messes up arrow/function keys.
	(define-key function-key-map "\e[23~" [?`])

	(init-keyboard-translate-table)
	(aset keyboard-translate-table ?` ?\e)
	(message "The ` key now sends ESC.  Use F11 for `.")

	(sleep-for 1) ; Give user a chance to see message.
      )
    ;; reset ESC, ` and f11.
    (progn
      (define-key function-key-map "\e[23~" [?\e])

      (init-keyboard-translate-table)
      (aset keyboard-translate-table ?\e ?\e)
      (aset keyboard-translate-table ?` ?`)
      (message "The ` key is no longer ESC, F11 is.")
      )
    )
  )

(defun auto-escape-key-fix ()
  "Assigns the ESC key to the key named in escape-key-replacement using 
escape-key-fix if the current terminal is is the list terminal-needs-escape.  
Drops everthing in TERM past the first hyphen."
  (if (boundp 'terminal-needs-escape-key)
      (if (TERM-in-list terminal-needs-escape-key)
	  (escape-key-fix t))
    )
  )
