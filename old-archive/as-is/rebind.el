;;;From: bbc@rice.edu
;;;Subject: Binding function keys
;;;Date: 7 Apr 89 07:24:03 GMT
;;;Organization: Rice University, Houston

;;;There's been some recent discussion on this list about key bindings for
;;;function and keypad keys, especially Suns.  The obvious first solution is to
;;;use global-set-key or define-key, but these functions won't overwrite an
;;;existing binding.  The character sequences transmitted by function keys
;;;often have a prefix that matches an existing binding, eg.  "\e[", and so
;;;global-set-key or define-key alone is inadequate.  There are several ways
;;;around this, but of course, I chose to write some functions that will
;;;overwrite an existing binding.

;;;rebind-key 		is like define-key,	but more studly.
;;;global-rebind-key	is like global-set-key,	but more studly.
;;;local-rebind-key	is like local-set-key,	but more studly.

;;;rebind-this-key		is wonderfully convenient for people who can't decide
;;;			which function should be assigned to each key, or
;;;			just like to have some spare keys waiting for
;;;			one-shot functions.  Note that rebind-this-key could
;;;			call define-key rather than rebind-key, if you're
;;;			not interested in the rest of this.

;;;As a testimonial, I bind rebind-this-key to all the keys on the left-hand
;;;keypad of Suns.  (I use X, rather than SunView, and so these keys are
;;;available.)

;;;I don't claim to be much of an emacs-lisp hacker (yet?), so constructive
;;;criticism and fixes to my lisp are welcome, but don't waste our time with
;;;flames.  Thanks are in order for super-apropos for helping me find things.

;;;	Ben Chase, Computer Science, Rice University, Houston, Texas

;;;ps: Another .sig might be hiding at the end...  Please pardon the legal
;;;    litany.  They tell me it's for the greater common good.
;--------------------
; Copyright (C) 1989 Benjamin B. Chase
; Permission is granted to copy, modify, and redistribute this
; software, but only under the conditions described in the
; GNU General Public License, Version 1, as published by the
; Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;
; This software is distributed without any warranty, and without any implied
; warranty of merchantibility or fitness.  See the GNU General Public License
; for more details.
;
(provide 'rebind-key)
(provide 'global-rebind-key)
(provide 'local-rebind-key)
(provide 'rebind-this-key)

; 'define-key is wimpy, and will not destroy existing bindings to make
; way for keymaps.  Here are some more powerful key binding functions.
;
; History:
; 	Benjamin B. Chase <bbc@rice.edu>
;	first verion of rebind-key, global-rebind-key, local-rebind-key,
;	and rebind-this-key

(defun rebind-key (MAP KEYS COMMAND)
  "Add the binding in MAP of string KEYS to COMMAND.
If a binding was overwritten, return the old command that was replaced,
and the key sequence that invoked it, as the pair (oldkeys . oldcommand).
Otherwise, return nil."
  (interactive "SKeymap: 
kKey sequence: 
CFunction: ")
  (if (null KEYS)
      'nil
    (let
	((oldbinding (lookup-key MAP KEYS))
	 (pKEYS)
	 (pKEYSbinding))
    
      (cond
       ((numberp oldbinding)
	(setq pKEYS (substring KEYS 0 oldbinding))
	(setq pKEYSbinding (lookup-key MAP pKEYS))
	(define-key MAP pKEYS nil)
	(define-key MAP KEYS COMMAND)
	(cons pKEYS pKEYSbinding))
       ((null oldbinding)
	(define-key MAP KEYS COMMAND)
	'nil)
       (t
	(define-key MAP KEYS COMMAND)
	(cons KEYS oldbinding))
       ))))

(defun global-rebind-key (KEYS COMMAND)
  "Add the global binding of string KEYS to COMMAND.
If a binding was overwritten, return the old command that was replaced,
and the key sequence that invoked it, as the pair (oldkeys . oldcommand).
Otherwise, return nil."
  (interactive "kKey sequence: 
CFunction:")
  (rebind-key global-map KEYS COMMAND))

(defun local-rebind-key (KEYS COMMAND)
  "Add the local binding of string KEYS to COMMAND.
If a binding was overwritten, return the old command that was replaced,
and the key sequence that invoked it, as the pair (oldkeys . oldcommand).
Otherwise, return nil."
  (interactive "kKey sequence: 
CFunction:")
  (rebind-key (current-local-map) KEYS COMMAND))

(defun rebind-this-key ()
  "Ask for a new binding for the key sequence that invoked this function.
This function is a useful binding for unused or unallocated function keys."
  (interactive)
  ; this-command-keys wouldn't behave to suit my porpoises.  Head-standing
  ; ensues...
  (let (
	(tck (this-command-keys))
	(rtk-helper '(lambda (COMMAND)
		      ""
		      (interactive "CFuntion to bind to this key: ")
		      COMMAND))
	)
    (rebind-key global-map tck (command-execute rtk-helper))))

	Ben Chase, Computer Science, Rice University, Houston, Texas
--------
From: bbc@titan.rice.edu (Benjamin Chase)
Newsgroups: gnu.emacs
Subject: rebind-key, version 2
Message-ID: <BBC.89May13182800@titan.rice.edu>
Date: 13 May 89 23:28:00 GMT
Distribution: gnu
Organization: Rice University
Lines: 93

Well, it's been a month, and only one person had comments and changes
to this package for rebinding keys, so I assume it's perfect now, and
safe for all the rest of you out there :-).
Here's the cleaned up and only slightly changed version.
(Don't forget to clip my .signature from the end.)

; ----------------------------------------
; Copyright (C) 1989 Benjamin B. Chase
; Permission is granted to copy, modify, and redistribute this
; software, but only under the conditions described in the
; GNU General Public License, Version 1, as published by the
; Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;
; This software is distributed without any warranty, and without any implied
; warranty of merchantibility or fitness.  See the GNU General Public License
; for more details.
;

; 'define-key is wimpy, and will not destroy existing bindings to make
; way for keymaps.  Here are some more powerful key binding functions.
;
; History:
;;;  7-Apr-89  Emacs Maint Acct (emacs@pawl.rpi.edu) (really tale@pawl.rpi.edu)
;;;	Cleaned up the (provide)s.
;;;	Fixed two problems with calling rebind-key interactively:
;;;	  o valid keymaps were signalled as errors because
;;;	    call-interactively quoted the symbol name.  Works properly
;;;	    now for valid keymaps.
;;;	  o You couldn't type in a key sequence for anything other than
;;;	    the global-map.  Example:  I have an O prefix in the esc-map
;;;	    that I couldn't type to because the "k" specification reads
;;;	    the O as a valid (self-insert-command) key sequence.  Now you
;;;	    can type the keys that belong to the map.
;;;	Minor aesthetic changes, mostly with `let' and not quoting nil.
;;;	  
; 	Benjamin B. Chase <bbc@rice.edu>
;	first verion of rebind-key, global-rebind-key, local-rebind-key,
;	and rebind-this-key

(provide 'rebind)

(defun rebind-key (MAP KEYS COMMAND)
  "Add the binding in MAP of string KEYS to COMMAND.
If a binding was overwritten, return the old command that was replaced,
and the key sequence that invoked it, as the pair (oldkeys . oldcommand).
Otherwise, return nil."
  (interactive "SKeymap: \nsKey sequence: \nCFunction: ")
  (if (not (keymapp MAP)) (setq MAP (symbol-value MAP)))
  (if (null KEYS) nil 
    (let ((oldbinding (lookup-key MAP KEYS)) pKEYS pKEYSbinding)
      (cond
       ((numberp oldbinding)
	(setq pKEYS (substring KEYS 0 oldbinding))
	(setq pKEYSbinding (lookup-key MAP pKEYS))
	(define-key MAP pKEYS nil)
	(define-key MAP KEYS COMMAND)
	(cons pKEYS pKEYSbinding))
       ((null oldbinding)
	(define-key MAP KEYS COMMAND)
	nil)
       (t
	(define-key MAP KEYS COMMAND)
	(cons KEYS oldbinding))))))

(defun global-rebind-key (KEYS COMMAND)
  "Add the global binding of string KEYS to COMMAND.
If a binding was overwritten, return the old command that was replaced,
and the key sequence that invoked it, as the pair (oldkeys . oldcommand).
Otherwise, return nil."
  (interactive "kKey sequence: \nCFunction:")
  (rebind-key global-map KEYS COMMAND))

(defun local-rebind-key (KEYS COMMAND)
  "Add the local binding of string KEYS to COMMAND.
If a binding was overwritten, return the old command that was replaced,
and the key sequence that invoked it, as the pair (oldkeys . oldcommand).
Otherwise, return nil."
  (interactive "kKey sequence: \nCFunction:")
  (rebind-key (current-local-map) KEYS COMMAND))

(defun rebind-this-key ()
  "Ask for a new binding for the key sequence that invoked this function.
This function is a useful binding for unused or unallocated function keys."
  (interactive)
  (let ((tck (this-command-keys)))
    (rebind-key global-map tck
                (command-execute
                 '(lambda (func)
                    (interactive "CFunction to bind to this key: ") func)))))

;;;--

;;;	Ben Chase, Computer Science, Rice University, Houston, Texas
;;;From: merlyn@intelob.intel.com (Randal L. Schwartz @ Stonehenge)
;;;Subject: another rebind-this-key (was Re: rebind-key, version 2)
;;;Date: 16 May 89 18:16:19 GMT
;;;Reply-To: merlyn@intelob.intel.com (Randal L. Schwartz @ Stonehenge)
;;;Organization: Stonehenge; netaccess via BiiN, Hillsboro, Oregon, USA

;;;In article <BBC.89May13182800@titan.rice.edu>, bbc@titan (Benjamin Chase) writes:
;;;| (defun rebind-this-key ()
;;;|   "Ask for a new binding for the key sequence that invoked this function.
;;;| This function is a useful binding for unused or unallocated function keys."
;;;|   (interactive)
;;;|   (let ((tck (this-command-keys)))
;;;|     (rebind-key global-map tck
;;;|                 (command-execute
;;;|                  '(lambda (func)
;;;|                     (interactive "CFunction to bind to this key: ") func)))))

;;;That 'command-execute' looked a little ugly.  Here's my hack:

(defun rebind-this-key (key command)
  "Set KEY to invoke COMMAND, preferring a local map if present.
Interactively, KEY is the key that invoked this routine, while
COMMAND is prompted for, making this function useful to bind unused
or unallocated keys."
  (interactive
   (let ((tck (this-command-keys)))
     (list
      tck
      (read-command (format "%s is currently undefined; define as: "
			    (key-description tck))))))
  (let ((map (current-local-map)))
    (if map
	(let ((lk (lookup-key map key)))
	  (if (or (null lk) (numberp lk))
	      (setq map (current-global-map))))
      (setq map (current-global-map)))
    (define-key map key command)))

;;;It's a standalone, and doesn't use your functions, but you can
;;;hack it around to do so.

;;;Enjoy.
;;;-- 
;;;/=Randal L. Schwartz, Stonehenge Consulting Services (503)777-0095===\
;;;{ <merlyn@agora.hf.intel.com> ...!uunet!agora.hf.intel.com!merlyn    }
;;;\=Cute quote: "Welcome to Oregon... home of the California Raisins!"=/
