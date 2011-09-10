;; @(#) tinyfh.el -- Collection of file handling functions

;; @(#) $Id: tinyfh.el,v 1.1 1995/03/29 08:58:03 jaalto Release_1 jaalto $
;; @(#) $Keywords: tools $
;; $KnownCompatibility: FSF 19.28 $
;; $outlineRegexp: $
;; $bookMarkRegexp: $
;; This file is *NOT* part of GNU emacs


;;{{{ Documentation

;; Copyright (C) 1995 Jari Aalto
;; Author:       Jari Aalto <jaalto@tre.tele.nokia.fi>
;; Maintainer:   Jari Aalto <jaalto@tre.tele.nokia.fi>
;; Created:      Mar 29 1995
;; Version:      $Revision: 1.1 $
;; state:        $State: Release_1 $
;;
;; To get information on this program use ident(1) or do M-x tifh-version
;; Look at the code with folding.el 1.7 , tinyfold.el 1.5


;; LCD Archive Entry:
;; tinyfh|Jari Aalto|jaalto@tre.tele.nokia.fi|
;; Collection of file handling functions:easy read-only edit|
;; 29-Mar-1995|1.1|~/misc/tinyfh.el.Z|

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs.  If you did not, write to the Free Software Foundation,
;; Inc., 675 Mass Ave., Cambridge, MA 02139, USA.

;;; Intallation:

;; - Put this file on your Emacs-Lisp load path, add following into your
;;   ~/.emacs startup file
;;
;;	(require 'tinyfh)
;;
;; - or use autoload feature, much more preferred
;;
;;	(autoload 'tifh-toggle-read-only-edit "tinyfh" "" t)
;;
;; - And to make best use of this, define some key binding
;;
;;	(global-set-key [C-F1] 'tifh-toggle-read-only-edit)
;;
;; - This file automatically installs itself when loded or
;;   evaled. If you want uninstall this .el:
;;
;;	(tifh-install nil)
;;
;; IMPORTANT NOTE
;;
;; editing read-only files
;; ..................................................
;; - if you load some other .el that does file handling and hooks itself
;;   into one of these hooks:
;;
;;	[1] write-file-hooks
;;	[2] after-save-hook
;;
;;    it propably hooks itself at the front of these hooks. It is most
;;    important that the funcs installed by this el are [1] FIRST and
;;    [2] LAST, because this .el directly touches the file. To make sure
;;    the hooks are in proper order, please load this file as last statement
;;    in your el, or if you notice anything weird, just run the install
;;    again. It will re-order the hooks properly.
;;
;;	(tifh-install t)
;;
;; - The good news is that the hooks' func order is propably not a problem,
;;   and you may ignore this warning. If all works fine, then there's
;;   nothing to worry about.


;;; Commentary:

;;; Briefly:
;; o  File handling functions that make your day in emacs shine :'/
;; o  Currently has: easy read-only file save feature, no more confirmations

;; RESERVED prefix FOR FUNCTIONS IN THIS FILE
;; - To avoid collisions to other modules I use "tifh-" in front of
;;   every function & variable. It stands for '(ti)ny (f)file (h)andling
;; - variable names contain letters 'tifh-:'

;; PREFACE
;; ==================================================
;; - You guessed it, this is again a tiny tool. It was inpired by post
;;   to gnu.emacs.help in Mar 15 1995. The poster had send amazing post,
;;   which only said on the subject line: 'Q, editing write-protected files'.
;;   Nothing else, no more information on the body of the message. I wondered
;;   what was this all about, why didn't he just do C-x C-q to toggle the
;;   read only flag? And I posted that answer, but the sender complained I
;;   didn't answer his question! He explained, that he didn't want to
;;   confirm all those silly questions emacs asked while savind the read-only
;;   file.. Well, why didn't he say so in the first place ...
;;
;; - I called this .el FH, file handling, not read-only handling, since
;;   I'm looking to the future where I can add more, interesting, functions
;;   here if I came up with something later.
;;
;; READ-ONLY EDITING
;; ==================================================
;; - The idea to avoid emacs confirmations is quite simple, we just need
;;   make the file writable before the emacs runs save-buffer, where
;;   the complaints are coming.
;; - After the file is saved we restore the original permissions of file.
;; - That's it! You only have to tell the buffer that it should be
;;   written quietly by turning on the tifh-:read-only-edit flag



;;}}}
;;{{{ history

;; HISTORY OF CHANGES
;; ========================================
;; Mar 29	1995    [jaalto]        19.28   v1.1		Release_1
;; - Collected simple functions from my .el library to here and made
;;   fully functional .el out of them.



;; To do list:
;; ========================================
;;

;;}}}


(provide 'tinyfh)

;;; Code:


;;; ............................................................ &bind ...

;;; ... where it hooks itself ................................. &hooks ...

(defvar after-save-hook nil
  "This hook seems to be undocumented in 19.28, it's there [files.el]")


;;; ... private variables ................................... &private ...

(defvar tifh-:verbose t
  "*Prints info messages if t")

(make-variable-buffer-local 'tifh-read-only-edited)
(defvar tifh-:read-only-edited nil
  "Buffer local variable. It is set/cleared by program to signify
read-only editing is in process. Holds original file permissions of the file.")


;;; ... user configurable ...................................... &conf ...

(make-variable-buffer-local 'tifh-:read-only-edit)
(defvar tifh-:read-only-edit nil
  "*Buffer local variable. When set to t, allows writing read-only files
easily without extra confirmations")


;;; ... version notice ...................................... &version ...


(defconst tifh-version
  "$Revision: 1.1 $"
  "Latest version number.")


(defconst tifh-version-id
  "$Id: tinyfh.el,v 1.1 1995/03/29 08:58:03 jaalto Release_1 jaalto $"
  "Latest modification time and version number.")

(defconst tifh-version-doc
  "tinyfh.el -- Collection of file handling functions
- Currently has nice read-only saving feature.

First created: Mar 29 1995
Author       : Jari Aalto <jaalto@tre.tele.nokia.fi
Maintainer   : Jari Aalto <jaalto@tre.tele.nokia.fi

Suggestions and enchancements welcome!"
  "Brief version and contact info")

;;; ----------------------------------------------------------------------
(defun tifh-version ()
  "version information."
  (interactive)
  (let* ((ob (current-buffer))
	 (bp (get-buffer-create "*version*"))
	 )
      (set-buffer bp)			;for insert
      (erase-buffer)
      (switch-to-buffer-other-window bp)
      (insert
       tifh-version-doc
       "\n\ncurrent version:\n" tifh-version-id)
      (pop-to-buffer  ob)
    ))

;;; ########################################################### &funcs ###



;;; ----------------------------------------------------------------------
;;;
(defun tifh-hook-it (hook func &optional last)
  "Moves the named FUNC in HOOK to the end or beginning. Adds the FUNC if not
already there. if LAST is non-nil, puts the function at the end."
  (if (memq func (eval hook)) (remove-hook hook func))
  (add-hook hook func last))

;;; ----------------------------------------------------------------------
;;;
(defun tifh-install (mode)
  "Installs or uninstalls TIFH package. If mode is [Ii] or t , the install
is carried out. Otherwise uninstall is done. Returns t or nil according
to install."
  (interactive "sinstall/uninstall (i/[u]): ")
  (setq mode
	(if (or (eq mode t)
		(and (stringp mode)
		     (string-match "^[iI]" mode)))
	    t nil))
  (if mode
      (progn
	(tifh-hook-it 'write-file-hooks 'tifh-save-buffer-before)
	(tifh-hook-it 'after-save-hook 'tifh-save-buffer-after t)
	t)
    (remove-hook 'write-file-hooks 'tifh-save-buffer-before)
    (remove-hook 'after-save-hook 'tifh-save-buffer-after)
    nil)
  )

;;; ----------------------------------------------------------------------
;;;
(defun tifh-toggle-read-only-edit (&optional arg)
  "Toggless buffer local read-only-edit flag."
  (interactive "P")

  (setq tifh-:read-only-edit
	(if arg
	    (if (zerop (prefix-numeric-value arg))
		nil t)
	  (not  tifh-:read-only-edit)))
  (if (interactive-p)
      (message (concat "read only edit = "
		       (if tifh-:read-only-edit "t" "nil"))))
  )

;;; ----------------------------------------------------------------------
;;;
(defun tifh-save-buffer-before ()
  "Handles read-only files differently. Kinda wrapper before save-buffer.

It the buffer is marked so, that it allows saving read-only file, the
file permissions are changed to +w just before saving, so that you don't
have to confirm emacs's questions. The permissions are restored after the file
has been written. In order to this function to work with other hooks placed
into write-file-hooks, it should be the first functions that touches the file.

References:
  tifh-toggle-read-only-edit  ,func
  tifh-save-buffer-after      ,func
"
  (let* ((file (buffer-file-name (current-buffer)))
	 (edit (if (boundp 'tifh-:read-only-edit) tifh-:read-only-edit nil))
	 (verb tifh-:verbose)
	 (wr-mode 420)			;-rw-r--r-- , 644 oct

	 orig-mode
	 )

    (setq tifh-:read-only-edited nil)	;inform 'after' hook

    (if (or (null file)			;let emacs handle this
	    (null edit)
	    (file-writable-p file)
	    (null (file-exists-p file))	;cannot read modes
	    ;;   If the file is owned by someone else, let the emacs handle.
	    (not (eq (user-uid) (nth 2 (file-attributes file))))
	    )
	nil
      ;; suppose it was read-only, at least it exist, owned by us
      ;; - Make sure it's out file..

      (setq orig-mode (file-modes file))
      (setq tifh-:read-only-edited orig-mode) ;set >>GLOBAL<<
      (set-file-modes file wr-mode)

      (if verb 	(message "read-only file"))
      )
    nil					;hook must return nil
    ))


;;; ----------------------------------------------------------------------
;;;
(defun tifh-save-buffer-after ()
  "Handles read-only files differently. Kinda wrapper _after_ save-buffer.

If the file beeing saved was red-only AND that file was temporarily
changed to writable, this function will restore the default permissions
of the file. It is highly recommended that this function is _always_
last in the

References:
  tifh-:read-only-edited
"
  (let* ((file (buffer-file-name (current-buffer)))
	 ;;  The original file modes are put into this var, if
	 ;;  the file was read-only during save sequence.
	 (orig-mode (if (boundp 'tifh-:read-only-edited)
			tifh-:read-only-edited nil))
	 )
    (if (null orig-mode)		;wasn't read-only
	nil				;just pass-by
      (set-file-modes file orig-mode)
      )
    ))


;;; .................................................... &auto-install ...

;; this automatically installs the .el when loaded or evaled.
(tifh-install t)

;;; ................ end of tinyfh.el ...................................
