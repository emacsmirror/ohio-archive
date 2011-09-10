;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;									     ;;
;;	File:     env.el						     ;;
;;	Author:   Wolfgang Rupprecht					     ;;
;;	Contents: modify and print the unix environment variables	     ;;
;; 		  for subprocesses     					     ;;
;;									     ;;
;;	$Log$								     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Copyright (C) 1987 Wolfgang S. Rupprecht
;; This file is for GNU Emacs.

;; Env.el is distributed in the hope that it will be useful in conjuction with
;; GNU Emacs, but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  This program is distributed for use with
;; and under the same terms as GNU Emacs. Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; env.el, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.


;; Code for hacking environment variables.
;; The most notable use is to clean up the environment of
;; the subprocesses. (setting TERM=dumb, unsetting TERMCAP, etc.) 
;; Requires GnuEmacs version 18.36 or later

(provide 'env)

(defvar old-env process-environment
  "A safe haven for the initial enviromment variables. Useful if you
ever want to look at or reset back to the initial environment.")

(defun modify-environment (name value)
  "Sets environment VARIABLE to have VALUE. Variable must be a string,
value may be either a string or nil.  Tries to preserve order of
variables and number of occurances of variable.  Appends new variable
to end of enironment variable list if it doesn't already occur.  If
value is nil, then all occurances of that variable are removed.  To
set a blank value, but have the variable name defined use the empty
string \"\"."
  (interactive "sVariable: \nxValue (as lisp string, or nil): ")
  (let ((env process-environment) new-env seen)
    (while env
      (if (null (string-match (concat "^" name "=") (car env)))
	  (setq new-env (cons (car env) new-env))
	  (if value
	      (setq new-env (cons (concat name "=" value) new-env)
		    seen t)))
      (setq env (cdr env)))
    (if (and value (null seen))
	(setq new-env (cons (concat name "=" value) new-env)))
    (setq process-environment (nreverse new-env))))

(defun print-environment-variable (name)
  "Print the value of an environment variable in the minibuffer.
Returns the variable as a string (or nil if variable is not set)"
  (interactive "sEnvironment Variable: ")
  (let ((env process-environment) value)
    (while (and env (null (string-match (concat "^" name "=\\(.*\\)$") (car env))))
      (setq env (cdr env)))
    (if env
	(setq value (substring (car env) (match-beginning 1) (match-end 1))))
    (if (interactive-p)
	(if env
	    (message "Variable '%s's value is '%s'" name value)
	    (message "Variable '%s' is NOT set" name)))
    value))

Wolfgang Rupprecht	ARPA:  wolfgang@mgm.mit.edu (IP 18.82.0.114)
326 Commonwealth Ave.	UUCP:  mit-eddie!mgm.mit.edu!wolfgang
Boston, Ma. 02115	TEL:   (617) 267-4365
Received: from pizza by PIZZA.BBN.COM id aa13073; 5 Oct 88 18:25 EDT
Received: from BBN.COM by PIZZA.BBN.COM id aa13069; 5 Oct 88 18:22 EDT
Received: from USENET by bbn.com with netnews
	for arpa-unix-emacs@bbn.com (unix-emacs@bbn.com);
	contact usenet@bbn.com if you have questions.
To: unix-emacs@BBN.COM
Date: 5 Oct 88 22:07:50 GMT
From: Wolfgang Rupprecht <bloom-beacon!mgm.mit.edu!wolfgang%eddie.mit.edu.uucp@BBN.COM>
Sender: arpa-unix-emacs-request@BBN.COM
Subject: Re: setenv inside gemacs?
Reply-To: Wolfgang Rupprecht <wolfgang@mgm.mit.edu.uucp>
Message-ID: <7333@bloom-beacon.MIT.EDU>
References: <860006@hpcljms.HP.COM>
Organization: Freelance Software Consultant, Boston, Ma.
Source-Info:  From (or Sender) name not authenticated.

In article <860006@hpcljms.HP.COM> brians@hpcljms.HP.COM (Brian Sullivan) writes:
>   I need to know if it is possible to set environment variable in
>GNU emacs.  I have set up some bindings to compile the file that I am
>editing but the compiler needs to have some environment variables set
>before it can be run.  I notice that GNU emacs does have an eLisp
>fuction "getenv" but no "setenv" or "putenv".  Has any one run into
>this problem before, and what was the solution?


try this:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;									     ;;
;;	File:     env.el						     ;;
;;	Author:   Wolfgang Rupprecht					     ;;
;;	Contents: modify and print the unix environment variables	     ;;
;; 		  for subprocesses     					     ;;
;;									     ;;
;;	$Log$								     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Copyright (C) 1987 Wolfgang S. Rupprecht
;; This file is for GNU Emacs.

;; Env.el is distributed in the hope that it will be useful in conjuction with
;; GNU Emacs, but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  This program is distributed for use with
;; and under the same terms as GNU Emacs. Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; env.el, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.


;; Code for hacking environment variables.
;; The most notable use is to clean up the environment of
;; the subprocesses. (setting TERM=dumb, unsetting TERMCAP, etc.) 
;; Requires GnuEmacs version 18.36 or later

(provide 'env)

(defvar old-env process-environment
  "A safe haven for the initial enviromment variables. Useful if you
ever want to look at or reset back to the initial environment.")

(defun modify-environment (name value)
  "Sets environment VARIABLE to have VALUE. Variable must be a string,
value may be either a string or nil.  Tries to preserve order of
variables and number of occurances of variable.  Appends new variable
to end of enironment variable list if it doesn't already occur.  If
value is nil, then all occurances of that variable are removed.  To
set a blank value, but have the variable name defined use the empty
string \"\"."
  (interactive "sVariable: \nxValue (as lisp string, or nil): ")
  (let ((env process-environment) new-env seen)
    (while env
      (if (null (string-match (concat "^" name "=") (car env)))
	  (setq new-env (cons (car env) new-env))
	  (if value
	      (setq new-env (cons (concat name "=" value) new-env)
		    seen t)))
      (setq env (cdr env)))
    (if (and value (null seen))
	(setq new-env (cons (concat name "=" value) new-env)))
    (setq process-environment (nreverse new-env))))

(defun print-environment-variable (name)
  "Print the value of an environment variable in the minibuffer.
Returns the variable as a string (or nil if variable is not set)"
  (interactive "sEnvironment Variable: ")
  (let ((env process-environment) value)
    (while (and env (null (string-match (concat "^" name "=\\(.*\\)$") (car env))))
      (setq env (cdr env)))
    (if env
	(setq value (substring (car env) (match-beginning 1) (match-end 1))))
    (if (interactive-p)
	(if env
	    (message "Variable '%s's value is '%s'" name value)
	    (message "Variable '%s' is NOT set" name)))
    value))
