;To: unix-emacs@BBN.COM
;Date: 1 Jun 89 16:35:09 GMT
;From: Kyle Jones <talos!kjones@uunet.uu.net>
;Sender: arpa-unix-emacs-request@BBN.COM
;Subject: replacement for execute-extended-command
;References: <1947@plx.UUCP>, <546@talos.UUCP>
;Organization: Philip Morris Research Center, Richmond, VA
;Source-Info:  From (or Sender) name not authenticated.
;
;Beating on the quick hack I posted in response to Evan Bigall's
;enhancement idea for `execute-extended-command' turned up some bugs, some
;behavioral differences with the standard function and a typo.
;
;Attached is the latest version complete with documentation and copying blurb.
;
;For those of you who want to see "M-x" instead of "ESC x" as a prompt,
;tell Emacs that your terminal sports a meta key (lie, if you must :-)),
;i.e. (setq meta-flag t).
;---------------
;;; Replacement for execute-extended-command in GNU Emacs
;;; Copyright (C) 1989 Kyle E. Jones
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; A copy of the GNU General Public License can be obtained from this
;;; program's author (send electronic mail to kyle@cs.odu.edu) or from
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.
;;;
;;; Send bug reports to kyle@cs.odu.edu.

;; Save this file as "execcmd.el" in a Lisp directory that Emacs knows about
;; (i.e. via load-path).  Byte-compile it.
;;
;; This package is autoloadable.  Use
;;    (autoload 'execute-extended-command "execcmd" nil t)
;; in your .emacs file.
;;
;; Thanks go to Evan Bigall (evan@plx.UUCP) for the neat idea of having
;; execute-extended-command report the key bindings of the commands it
;; executes.

(defun execute-extended-command (command &optional prefix-argument)
  "Reads an interactive command name, and then calls the command interactively.
If a prefix argument is supplied to this function, it will be passed
appropriately to the command being called.

After the command call returns, the current keymaps are searched for this
command.  If the command is bound to any keys, these are reported in the
echo area."
  (interactive
   (let ((prompt (this-command-keys)))
     (if current-prefix-arg
	 (let* ((i (length prompt))
		(key (substring prompt i)))
	   (while (and (/= i 0) (not (eq (key-binding key) this-command)))
	     (setq i (1- i) key (substring prompt i)))
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
       (if (not (eq (key-binding prompt) this-command))
	   (setq prompt (where-is-internal this-command (current-local-map)
					   t )))
       (setq prompt (concat (meta-key-description prompt) " ")))
     (list (read-command prompt) current-prefix-arg)))
  (setq this-command command)
  (let ((prefix-arg prefix-argument))
    (command-execute command t))
  (if (and (interactive-p) (sit-for 1))
      (let ((keys (append (where-is-internal command (current-local-map)))))
	(if keys
	    (message "%s is on %s" command
		     (mapconcat 'meta-key-description keys " , "))))))

(defun meta-key-description (keys)
  "Works like key-description except that sequences containing
meta-prefix-char that can be expressed meta sequences, are.
E.g. `\"\\ea\" becomes \"M-a\".

If the ambient value of meta-flag in nil, this function is equivalent to
key-description."
  (if (not (and meta-flag (numberp meta-prefix-char)))
      (key-description keys)
    (let (pattern start)
      (setq pattern
	    (if (/= meta-prefix-char ?-)
		(concat (list meta-prefix-char ?[ ?^ meta-prefix-char ?]))
	      "-[^---]"))
      (while (string-match pattern keys start)
	(setq keys
	      (concat
	       (substring keys 0 (match-beginning 0))
	       (char-to-string (logior (aref keys (1- (match-end 0))) 128))
	       (substring keys (match-end 0)))
	      start (match-beginning 0)))
      (key-description keys))))

