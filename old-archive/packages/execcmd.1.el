;; @(#) execcmd.el -- Replacement for execute-extended-command in GNU Emacs
;; $KnownCompatibility: 18.57 , 19.28 $
;; $Id: $

;; Copyright (C) 1989 Kyle E. Jones
;; Author:       Kyle Jones <kyle_jones@wonderworks.com>
;; Maintainer:   Jari Aalto <jari.aalto@ntc.nokia.com>
;; Created:      1989
;;
;; To get information on this program use ident(1).
;; Look at the code with folding.el, tinyfold.el, tinybm.el

;; LCD Archive Entry:
;; execcmd|Kyle Jones, Jari Aalto|kyle_jones@wonderworks.com, jari.aalto@ntc.nokia.com|
;; After M-x COMMAND, shows you which keys the command is bound to|
;; 15-Aug-1995|2.1|~/packages/execcmd.el.gz|


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

;; ....................................................... &t-install ...
;; Put this file on your Emacs-Lisp load path, add following into your
;; ~/.emacs startup file
;;
;;     (require 'execcmd)
;;

;; ..................................................... &t-commentary ...

;; Date: 1 Jun 89 16:35:09 GMT
;;
;; Thanks go to Evan Bigall (evan@plx.UUCP) for the neat idea of having
;; execute-extended-command report the key bindings of the commands it
;; executes.
;;
;; Beating on the quick hack I posted in response to Evan Bigall's
;; enhancement idea for `execute-extended-command' turned up some
;; bugs, some behavioral differences with the standard function and a
;; typo.
;;
;; Attached is the latest version complete with documentation and
;; copying blurb.
;;
;; For those of you who want to see "M-x" instead of "ESC x" as a prompt,
;; tell Emacs that your terminal sports a meta key (lie, if you must :-)),
;; i.e. (setq meta-flag t).
;;
;; Kyle Jones

;; CHANGE HISTORY
;; ........................................................ &t-history ...
;;
;; Aug	25	1995	[jari]		19.28	v2.1	Released
;; -  Gjalt de Jong just posted this same file to the comp.emacs group
;;    and he had some good modifications I haven't thought of. I added the
;;    best parts. Gjalt had deleted meta-key-description function and
;;    replaced all calls to key-description. I didn't see the need to
;;    do that, because the existing code is working ok. I think
;;    the key-description dosn't work well in 18.57, so I kept
;;    the original meta-key-description untouched.
;;
;; Aug	15     	1995    [jari]        	19.28   v2.0	NotReleased
;; - I used this file extensively in 18.57 days, but it stopped working
;;   when I switched to 19.28 it stopped working. I talked with Kyle
;;   back then, but this functionality was already added to XEmacs
;;   and he had not much intention to dig in old and dusty code for FSF.
;; - So I took a look at it and made it work in 19.28 again. Hope people
;;   enjoy this as much as I have done.
;; - version history started from v2.0, since this is aimed for 19.xx
;; - I wish that this would be included into emacs distribution!


;;; Code:

;;; ########################################################### &Funcs ###


(defvar execcmd-load-hook nil
  "*Run when file is loaded.")

(defvar meta-key-description "M-x")	;if not exist

(defvar meta-flag
  (if (fboundp 'current-input-mode)
      (nth 2 (current-input-mode))	;Gjalt de Jong <dejong@imec.be>
    nil))

;;  Record the original function
;;  - by Gjalt de Jong <dejong@imec.be>
(if (string-match "19" emacs-version)
    (if (not (fboundp 'execute-extended-command-orig))
	(defalias 'execute-extended-command-orig
	  (symbol-function 'execute-extended-command))))

;;; ----------------------------------------------------------------------
;;;
(defun execute-extended-command (command &optional prefix-argument)
  "execcmd.el
Reads an interactive command name, and then calls the command interactively.
If a prefix argument is supplied to this function, it will be passed
appropriately to the command being called.

After the command call returns, the current keymaps are searched for this
command.  If the command is bound to any keys, these are reported in the
echo area.
"
  (interactive
   (let ((prompt (this-command-keys))
	 )
     (if current-prefix-arg
	 (let* ((i       (length prompt))
		(key     (substring prompt i))
		)
	   (while (and (stringp this-command))   ;could be [21 escape 120]
		   (stringp
		   (/= i 0)
		   (not (eq (key-binding key) this-command))
		   )
	     (setq i   (1- i))
	     (setq key (substring prompt i))
	     )

	   (setq prompt
		 (if (zerop i)
		     (where-is-internal this-command (current-local-map) t)
		   key )
		 prompt (or prompt ":")
		 prompt (concat (meta-key-description prompt) " ")
		 prompt
		 (cond ((consp current-prefix-arg)
			(concat "(" (int-to-string (car current-prefix-arg))
				") " prompt))
		       ((symbolp current-prefix-arg)
			(concat (symbol-name current-prefix-arg) " " prompt))
		       (t
			(concat (int-to-string current-prefix-arg)
				" " prompt)))))
       ;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ prefix arg ^^^
       (if (not (eq (key-binding prompt) this-command))
	   (setq prompt (where-is-internal
			 this-command (current-local-map) t)))

       (if prompt
	   (setq prompt (concat (meta-key-description prompt) " ")))
       )
     (list (read-command prompt) current-prefix-arg)
     ;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ interactive end ^^^
     ))

  (setq this-command command)

  (let ((prefix-arg prefix-argument))
    (command-execute command t))

  (if (null (interactive-p)) nil
    (sit-for 1)
    (let ((keys (append (where-is-internal command (current-local-map)))))
      (if keys
	  (message "%s is on %s" command
		   (mapconcat 'meta-key-description keys " , "))))
    ))





;;; ----------------------------------------------------------------------
;;;
(defun meta-key-description (keys)
  "execcmd.el
Works like key-description except that sequences containing
meta-prefix-char that can be expressed meta sequences, are.
E.g. `\"\\ea\" becomes \"M-a\".

If the ambient value of meta-flag in nil, this function is equivalent to
key-description.
"
  (if (not (and meta-flag (numberp meta-prefix-char)))
      (key-description keys)
    (let (pattern start)
      (setq pattern
	    (if (/= meta-prefix-char ?-)
		(concat (list meta-prefix-char ?[ ?^ meta-prefix-char ?]))
	      "-[^---]"))

      ;;  In 19.xx , the key could be vector notation: [C-right]
      (if (null (stringp keys))
	  nil
	(while (string-match pattern keys start)
	  (setq keys
		(concat
		 (substring keys 0 (match-beginning 0))
		 (char-to-string (logior (aref keys (1- (match-end 0))) 128))
		 (substring keys (match-end 0)))
		start (match-beginning 0))))
      (key-description keys))))


(provide 'execcmd)
(run-hooks 'execcmd-load-hook)


;;; .............................................................. &eof ...
