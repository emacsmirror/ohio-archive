;; -*- emacs-lisp -*-
;;
;; name: mew-ml.el
;; version: 0.1
;; description: some mailing list support for mew
;; creation date: 1998-11-18
;; author: "Sen Nagata" <sen@eccosys.com>
;; warning: not optimized at all

;; required:
;;
;;   -mew (tested for 1.93)
;;   -mew-mailto.el (mew-mailto.el depends on mailto.el)

;; installation:
;;
;;   -put this file (mew-mailto.el and mailto.el) in an appropriate 
;;    directory (so emacs can find it)
;;
;;   <necessary>
;;   -put:
;;
;;     (add-hook 'mew-init-hook (lambda () (require 'mew-ml)))
;;
;;    in your .emacs file.
;;
;;   <optional>
;;   -for key-bindings put (haven't tested for xemacs yet):
;;
;;     (add-hook 
;;       'mew-summary-mode-hook
;;       (lambda () 
;;         (define-key mew-summary-mode-map 
;;           "ch" 'mew-ml-compose-help-message)
;;         (define-key mew-summary-mode-map 
;;           "cu" 'mew-ml-compose-unsubscribe-message)
;;         (define-key mew-summary-mode-map 
;;           "cs" 'mew-ml-compose-subscribe-message)
;;         (define-key mew-summary-mode-map 
;;           "cp" 'mew-ml-compose-post-message)
;;         (define-key mew-summary-mode-map 
;;           "co" 'mew-ml-compose-owner-message)))
;;
;;    in your .emacs file.
;;
;;   <optional>
;;   -for enhancing the menu for summary mode, put:
;;
;;     (defvar mew-ml-menu-spec
;;       '("Mailing List"
;;         ["Help msg" 
;;          mew-ml-compose-help-message t]
;;         ["Unsubscribe msg" 
;;          mew-ml-compose-unsubscribe-message t]
;;         ["Subscribe msg"
;;          mew-ml-compose-subscribe-message t]
;;         ["Post msg"
;;          mew-ml-compose-post-message t]
;;         ["Owner msg"
;;          mew-ml-compose-owner-message t]
;;         ))
;;    
;;     (add-hook
;;      'mew-summary-mode-hook
;;      (lambda ()
;;        (setq mew-summary-mode-menu-spec 
;;     	  (nconc mew-summary-mode-menu-spec 
;;     		(list "----" 
;;     		      mew-ml-menu-spec)))
;;        ;; got this section from mew-summary.el
;;        (easy-menu-define
;;         mew-summary-mode-menu
;;         mew-summary-mode-map
;;         "Menu used in Summary mode."
;;         mew-summary-mode-menu-spec)))
;;
;;   in your .emacs.

;; details:
;;
;;   this package provides a number of interactive functions
;; (commands) for the user.  each of the commands ultimately creates a
;; draft message based on some information.  the names of the commands
;; and brief descriptions are:
;;
;;        mew-ml-compose-*-message
;;            make a draft message from one of the corresponding
;;            List-* headers (Unsubscribe, Subscribe, etc.).  if no such 
;;            header is detected, the user is notified via 'message'.
;;
;;   note: i don't use xemacs that often so i don't test it as much.

;; usage:
;;
;;   -invoke mew
;;   -try out the commands mentioned above in 'details'
;;   -you can also try the commands via the menu now

;; notes and todo:
;;
;;   -see TODO items in code
;;   -place the menu and keybinding 'hook' stuff mentioned above into a 
;;    separate file and suggest that the user 'require'/'load' this via 
;;    their .emacs file?  another possibility is just to set up our own
;;    menus...seems kind of ugly though.
;;   -keybindings and menu items -> is there a good way to modify the
;;    existing menus for mew?  (is that a bad idea?)
;;   -support some kind of warning message for messages which should be
;;    edited (e.g. subscribe messages requiring names).  there isn't a
;;    standard solution for this yet (for a discussion, see section A.5 of rfc
;;    2368)
;;   -i've used this code w/ emacs versions >= 20.3
;;   -test w/ xemacs -- not at all sure of the menu and keybinding stuff
;;   -prepare a test suite...

;; how should we handle the dependecy on mew?
;; doing the following seems to have catastrophic effects on my machine :-(
;(require 'mew)

;; will this work?
(eval-when-compile 
  (require 'mew))

(defconst mew-ml-version "mew-ml.el 0.1")

;; use mew-mailto support -- should be useable for things other than mew too
(require 'mew-mailto)

;; notes:
;;
;; 1) i am not clear on whether mew-mailto-compose-message-from-header
;;    returns
;;
;; 2) perhaps i should do the below w/ macros?
;;
;; 3) see 'mew-summary-reply' for a way to deal w/ the case when point
;;    is not on a row w/ a message (located after all of the rows
;;    representing messages) -> TODO
;;
;; 4) figure out a way to provide similar functionality when point is
;;    is in message mode -> TODO

(defun mew-ml-compose-help-message ()
  (interactive)
  (if (not (mew-mailto-compose-message-from-header "List-Help"))
      (message 
       "There does not appear to be a List-Help: header in this message."))
  )

(defun mew-ml-compose-unsubscribe-message ()
  (interactive)
  (if (not (mew-mailto-compose-message-from-header "List-Unsubscribe"))
      (message 
       "There does not appear to be a List-Unsubscribe: header in this message."))
  )

(defun mew-ml-compose-subscribe-message ()
  (interactive)
  (if (not (mew-mailto-compose-message-from-header "List-Subscribe"))
      (message
       "There does not appear to be a List-Subscribe: header in this message."))
  )

(defun mew-ml-compose-post-message ()
  (interactive)
  (if (not (mew-mailto-compose-message-from-header "List-Post"))
      (message
       "There does not appear to be a List-Post: header in this message."))
  )

(defun mew-ml-compose-owner-message ()
  (interactive)
  (if (not (mew-mailto-compose-message-from-header "List-Owner"))
      (message
       "There does not appear to be a List-Owner: header in this message."))
  )

;; since this will be used via 'require'...
(provide 'mew-ml)
