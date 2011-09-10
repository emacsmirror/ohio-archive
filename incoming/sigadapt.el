;;; $Id: sigadapt.el,v 1.20 2000/05/13 13:12:33 queinnec Exp $
;;; Copyright (C) 1997 by C.Queinnec (University of Paris 6 & INRIA)

;;; LCD Archive Entry:
;;; sigadapt|Christian Queinnec|Christian.Queinnec@inria.fr|
;;; Select signature depending on mailee.|
;;; $Date: 2000/05/13 13:12:33 $|$Revision: 1.20 $|
;;; probably ~/misc/sigadapt|

;;; This file is not part of GNU Emacs.

;;; {{{ Commentaries

;;; This package allows when composing or sending a mail to select a
;;; signature appropriate to the mailee (as mentioned in the `To:'
;;; field).  This package requires the use of the marvellous BBDB
;;; package from Jamie Zawinski <jwz@lucid.com>. An extra field in the
;;; BBDB database registers which signature to use. If no signature is
;;; mentioned or if you want to change to the already current
;;; signature then the package offers you to choose and record a
;;; signature among a set of predefined ones.

;;; To use that package, install it at an appropriate place wrt to
;;; your load-path and edit the variables of the Customisation section
;;; (see below). You should mainly edit the `friendly' signature. You
;;; may also add some other signatures if you feel like.

;;; When done, compile the package and add:
;;;        (require 'sigadapt)
;;; to your .emacs then, try to send me an email! 

;;; It replaces the usual C-c C-w binding that appends ~/.signature to
;;; your mail with a new one that replaces the current signature with
;;; the one that is appropriate to the mailee. You may also force the
;;; choice and the insertion of a signature with C-u C-c C-w: at that
;;; time you do not need BBDB at all, you only choose one signature
;;; among a set of predefined ones [I should probably hack the sources
;;; to offer that option as a compilation option]. The third option is
;;; to type C-u C-u C-c C-w to change the signature that was
;;; associated with the mailee. By default, the default signature is
;;; the official one (the one read from .signature).

;;; The sigadapt package is known to work with Emacs 19.34, RMAIL and
;;; BBDB 1.50.

;;; {{{ Repository
;;; Bugs, remarks etc should be sent to
;;;     Christian.Queinnec@inria.fr
;;; Newer versions will be sent to the LCD Archive but may appear earlier on:
;;;     ftp://ftp.inria.fr:INRIA/Projects/icsla/Miscellaneous/sigadapt.el
;;; Other Emacs packages can be found with World Wide Web with URL:
;;;     ftp://ftp.inria.fr/INRIA/Projects/icsla/WWW/elisp.html
;;; }}}

;;; }}}
;;; {{{ Customisation

;;; With the following macro you must define at least two signatures
;;; (otherwise why should you use sigadapt ?). One of which should be
;;; defined as the default one.

(defmacro defsignature (nickname string)
  "Define a signature for the sigadapt package. 
NICKNAME is a symbol (such as official, friendly, lovingly, extrashort ...) 
and STRING is the string you associate to that name as signature. "
  (list 'let 
        (list (list 'string string))
        ;; augment the list of all signature names.
        (list 'put
              ''sigadapt
              ''nicknames
              (list 'cons 
                    (list 'list (symbol-name nickname))
                    '(get 'sigadapt 'nicknames) ) )
        ;; register the signature with its name.
        (list 'put 
              (list 'quote nickname)
              ''sigadapt
              'string ) ) )

;;;     {{{ Examples of signatures
;;; They must end with a double \n to clearly separate signatures from any 
;;; attachment.

(defsignature friendly
  "\n\n\t\tChristian.\n\n" )

(defsignature protocolar
  "\n
\t\tPr. C. Queinnec
\t\tUniversite Paris 6 -- Pierre et Marie Curie.\n\n" )

(defsignature veryshort
  "\n-- C.\n\n" )

(defsignature flashy                    ; Could be flashier!
  "\n
    _______              ____
   /      /     /|   /  /
  /      /     / |  /  /
 /      /     /  | /  /
/_____ X     /   |/  /____
        \
\n" )

;;; The 'official' signature is dynamically defined, when the sigadapt
;;; package is loaded, to be the content of the ~/.signature file.
;;; Therefore you don't need to define it explicitly.

;;; Now, choose your default signature.

(defvar sigadapt-default-signature-name 'official
  "This is the name of the signature you prefer to use as default.
By default, this is the `official' signature and again by default, the
`official' signature is defined as the content of your ~/.signature file." )

;;;    }}}
;;; This is the end of the customisation section. Normally what follows
;;; is not to be changed.
;;; }}}
;;; {{{ Code

(require 'bbdb)
(require 'bbdb-com) ; necessary for the bbdb-search macro.

(defvar sigadapt-current-signature nil
  "This variable holds the current signature.
That is, the one which is present in the message. It's better not 
to change it by hand." )
(make-variable-buffer-local 'sigadapt-current-signature)

(defvar sigadapt-signedp nil
  "This variable tells if the current message had been explicitly signed.
This is to avoid asking to set again the signature if already done." )
(make-variable-buffer-local 'sigadapt-signedp)

;;; {{{ BBDB-related stuff 
;;; This may be changed by another package. The other package must
;;; provide a way to store and retrieve signatures associated to 
;;; mail addresses.

(defun sigadapt-associate-signature (&optional record)
  "Allow to define or change a signature with a BBDB record. 
You may also directly use BBDB editing mode and change the associated 
`signature' field."
  (if (not record)
      (save-excursion
        (goto-char (point-min))
        (let* ((to  (bbdb-extract-field-value "To"))
               (rec (sigadapt-search-record to)) )
          (if rec (sigadapt-do-associate-signature rec)
            (progn (message "No bound record")
                   nil ) ) ) )
    (sigadapt-do-associate-signature record) ) )

(defun sigadapt-do-associate-signature (record)
  ;; Ask for a signature nickname
  (let ((signame (completing-read 
                   "Signature nickname (official): "
                   (get 'sigadapt 'nicknames)
                   nil t )))
    (if (equal signame "")
        (setq signame "official") )
    ;; Store strings rather than symbols for compatibility with BBDB. 
    (bbdb-record-putprop record 'signature signame) ) )

(defun sigadapt-retrieve-signature (&optional record) 
  "Retrieve the signature (a symbol) associated to a mailee.
The search is done through a BBDB record. "
  (if (not record)
      (save-excursion
        (goto-char (point-min))
        (let* ((to  (bbdb-extract-field-value "To"))
               (rec (sigadapt-search-record to)) )
          (if rec (sigadapt-do-retrieve-signature rec)
            (progn (message "No bound record")
                   nil ) ) ) )
    (sigadapt-do-retrieve-signature record) ) )

(defun sigadapt-do-retrieve-signature (record)
  (let ((signame (bbdb-record-getprop record 'signature)))
    (if (stringp signame)
        (setq signame (intern signame)) )
    signame ) )

;;; }}}

;;; Really change the signature. It scans the message to find the current
;;; signature, change it and record the new signature. 

(defun sigadapt-change-signature (signame newsig forcep)
  "Propose to change the current signature.
The current signature is named SIGNAME (a symbol), the new one is
NEWSIG (a symbol). The user is asked (unless FORCEP is true) to confirm
the change of signature. Do nothing if the current signature cannot be found. "
  (if (not (equal newsig sigadapt-current-signature))
      (save-excursion
        (goto-char (point-max))
        (if sigadapt-current-signature
            (let ((pt (search-backward sigadapt-current-signature 
                                       (point-min) t ) ))
              (if pt 
                  (if (or forcep
                          (y-or-n-p (concat "Change to " 
                                            (symbol-name signame)
                                            " signature? ")) )
                      ;; Consider the change of the signature not as a
                      ;; normal change (this avoids when sending another
                      ;; mail to thave the "unsent message..." warning).
                      (let ((b (buffer-modified-p)))
                        (replace-match newsig t t)
                        (setq sigadapt-current-signature newsig)
                        (setq sigadapt-signedp t)
                        (set-buffer-modified-p b)
                        t )
                    (progn
                      (message "Abandon signature change!")
                      t ) )
                (progn
                  (message "Cannot find current signature!")
                  (goto-char (point-max))
                  (insert newsig)
                  (setq sigadapt-current-signature newsig)
                  (setq sigadapt-signedp t)
                  t ) ) )
          (progn
            (message "No current signature defined!")
            (goto-char (point-max))
            (insert newsig)
            (setq sigadapt-current-signature newsig)
            (setq sigadapt-signedp t)
            ;; to return t means that we don't want to associate a new
            ;; signature with the mailee.
            t ) ) )
    nil ) )

;;; Take an address and look for the associated record within BBDB.

(defun sigadapt-search-record (to)
  "Search a BBDB record associated to TO or return NIL."
  ;; This code is adapted from bbdb-annotate-message-sender
  (let* ((data (mail-extract-address-components to))
         (name (car data))
         (net  (car (cdr data))) )
    (if (equal name net) (setq name nil))
    (if (and net bbdb-canonicalize-net-hook)
        (setq net (bbdb-canonicalize-address net)))
    (bbdb-search-simple name net) ) )

;;; There are three main functions:
;;;    sigadapt-choose-signature allows to choose a signature
;;;    sigadapt-propose-signature proposes to change a signature if none
;;;                               was already specified.
;;;    sigadapt-try-signature changes automatically the signature non-
;;;                           interactively.

(defun sigadapt-try-signature ()
  "Try to adapt non-interactively the current signature. 
This function looks silently in the current message to find how to choose the
signature. It does nothing if not enough information is present. This function
is useful in a hook."
  (save-excursion
    (condition-case nil
        (progn
          (goto-char (point-min))
          (let ((record (sigadapt-search-record 
                         (bbdb-extract-field-value "To") )))
              (if record
                  (let ((signame (sigadapt-retrieve-signature record)) )
                    (if signame
                        (let ((signature (get signame 'sigadapt))
                              (buffer-read-only nil)
                              (modified (buffer-modified-p))
                              (inhibit-read-only t)
                              before-change-functions
                              after-change-functions )
                          (unwind-protect
                              (if signature 
                                  (sigadapt-change-signature 
                                   signame signature t ) )
                            (or modified
                                (set-buffer-modified-p nil) ) ) ) ) ) ) ) )
      (error nil) ) ) )

(defun sigadapt-force-signature ()
  "Insert a signature chosen among the current catalogue of signatures."
  (let ((signame (completing-read 
                  "Signature nickname (official): "
                  (get 'sigadapt 'nicknames)
                  nil t )))
    (if (equal signame "")
        (setq signame "official") )
    (if (stringp signame)
        (setq signame (intern signame)) )
    (if signame
        (let ((signature (get signame 'sigadapt)))
          (if signature 
              (sigadapt-change-signature signame signature t)
            (message "No such signature" signame) ) )
      (message "No such signature name" signame) ) ) )

(defun sigadapt-choose-signature ()
  "Insert the signature associated to the mailee.
Propose to associate one if there is no associated signature."
  (let ((signame (sigadapt-retrieve-signature)))
    (if signame
        (let ((signature (get signame 'sigadapt)))
          (if signature
              (sigadapt-change-signature signame signature nil)
            (message "No such signature nickname \"%s\"!" signame) ) )
      ;; No associated signature information:
      (if (y-or-n-p "No associated signature, want to record one? ")
          (if (sigadapt-associate-signature)
              (sigadapt-choose-signature)
            (message "No record for that mailee") )
        (message "No associated signature!") ) ) ) )

;;; Brady Montz <bradym@cs.arizona.edu> proposed:
;(defun sigadapt-choose-signature ()
;  "This function allows when sending a message to adapt the signature 
;to approximate the level of relationship the sendee bears to you. 
;If no signature information is kept for that sendee in the BBDB database 
;then you will be prompted to add one (under property sigadapt). 
;If interactively called, change the signature only if not yet done."
;  (interactive)
;  (save-excursion
;    (goto-char (point-min))
;    (let ((to (bbdb-extract-field-value "To")))
;      (if (not to) 
;          (message "`To' field not found!")
;        (if (equal to "")
;            (message "`To' field empty!")
;	  (let ((name (car (mail-extract-address-components to)))
;		(address (car (cdr (mail-extract-address-components to)))))
;	    (let ((records (bbdb-search-simple name address)))
;	      (if (not records)
;		  (message "%s not in BBDB!" to)
;		(if (not (= 1 (length records)))
;		    (message "%d ambiguous BBDB record for \"%s\"!" 
;			     (length records) to )
;		  (sigadapt-record-signature (car records)) ) ) ) ) ) ) ) ))

;;; This is the single function that controls the signature and the
;;; appropriate use of the three main functions described above.
                  
(defun sigadapt-propose-signature (&optional arg)
  "Propose to choose a signature for the current message if not yet done.
The C-u prefix forces the insertion of an interactively chosen signature.
The C-u C-u prefix means that we want to change the associated signature."
  (interactive "P")
  (if arg
      (if (>= (prefix-numeric-value arg) 16)
          ;; associate a new signature
          (sigadapt-associate-signature)
        ;; select then insert a signature 
        (sigadapt-force-signature) )
    ;; if not already signed
    (if (not sigadapt-signedp)
        ;; sign with the associated signature
        (sigadapt-choose-signature)
      (message "Signature already set! Use C-u to force it.") ) ) )


;;; }}}
;;; {{{ Hooks

(defun sigadapt-initialize ()
  "Install the sigadapt package. 
This function should be added to the mail-setup-hook variable. 
It also records (from ~/.signature) your official signature."
  ;; Read only once the ~/.signature file.
  (if (not (get 'official 'sigadapt))
      (defsignature official 
        (if (file-exists-p mail-signature-file)
            (let ((buffer (generate-new-buffer "*SIGNATURE*")))
              (save-excursion
                (set-buffer buffer)
                (insert "\n\n-- \n")
                (insert-file-contents mail-signature-file)
                (let ((r (buffer-string)))
                  (kill-buffer buffer)
                  r ) ) )
          nil ) ) ) 
  ;; load bbdb
  (require 'bbdb)
  ;; Install key bindings.
  (sigadapt-install-keys)
  ;; Initialize your default signature (to be used by RMAIL).
  (let ((s (get sigadapt-default-signature-name 'sigadapt)))
    (if s (progn
            (if (not mail-signature)
                (setq mail-signature s) )
            ;; this must be done each time a new message is composed.
            (setq sigadapt-current-signature mail-signature)
            (setq sigadapt-signedp nil) )
      (error "You should have a default signature!") ) )
  ;; Attempt to change mutely the signature if possible.
  (sigadapt-try-signature) )

(defun sigadapt-install-keys ()
  ;; Hook on the send-mail hook before sending the message.
  (add-hook 'mail-send-hook 'sigadapt-propose-signature)
  ;; Emacs 19.34 proposes in the mail menu to enter signature. Redefine
  ;; that menu item to invoke sigadapt-choose-signature instead. Rebind
  ;; C-c C-w also.
  (define-key mail-mode-map "\C-c\C-w"     'sigadapt-propose-signature)
  (define-key mail-mode-map [menu-bar mail signature]
    '("Adapt Signature" . sigadapt-propose-signature))
  t )

;;; The mail-setup-hook is run after mail-signature had been inserted
;;; (if defined) in the new mail.  That's why I also run it in
;;; mail-mode-hook.

(add-hook 'mail-setup-hook 'sigadapt-initialize)
(add-hook 'mail-mode-hook  'sigadapt-initialize)

;;; }}}

(provide 'sigadapt) ; so it can be require'd.

;;; TODO: handle multiple simultaneous mailees! 

;;; end of sigadapt.el
