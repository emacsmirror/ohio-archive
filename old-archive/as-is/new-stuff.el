;To: unix-emacs@bbn.com
;Date: 10 Jan 89 18:07:22 GMT
;From: John Sturdy <mcvax!ukc!harrier.ukc.ac.uk!eagle.ukc.ac.uk!icdoc!qmc-cs!harlqn!jcgs@uunet.uu.net>
;Subject: handy oddments
;
;Here are some oddments I wrote, that I've been finding particularly
;useful lately.  This isn't really a package, just a bag of scraps!
;----------------------------------------------------------------
;;; -*-emacs-lisp-*- /harlqn/usr/users/jcgs/new-stuff.el
;;; Last edited: Tue Jan 10 16:58:38 1989 by jcgs (John Sturdy) on harlqn

(defun define-lisp-key (key command)
  "Define KEY to run COMMAND in all lisp modes."
  (define-key lisp-mode-map key command)
  (define-key emacs-lisp-mode-map key command)
  (define-key lisp-interaction-mode-map key command)
  (define-key inferior-lisp-mode-map key command))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A couple of commands for structure-style editing ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pick-up-sexp-at-point ()
  "Put the expression that starts at point into the kill ring."
  (interactive)
  (save-excursion
    (mark-sexp 1)
    (copy-region-as-kill (point) (mark))
    (exit-recursive-edit)))

(defun wander-yank ()
  "Let the user wander around in a recursive edit inside a
save-excursion, then do a yank - presumably the user picks something
up into the kill ring during the recursive edit."
  (interactive)
  (save-excursion
    (save-window-excursion
      (recursive-edit)))
  (yank 1))

(define-lisp-key "\e[" 'wander-yank)
(define-lisp-key "\e]" 'pick-up-sexp-at-point)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Useful with inferior Lisps, shells, telnets... ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun other-window-file-name ()
  "Insert at point the name of the file in the next window.
Particularly useful in a shell window."
  (interactive)
  (let ((name (save-window-excursion
		(buffer-file-name
		 (window-buffer
		  (other-window 1))))))
    (if name (insert name)
      (message "Other window has no file"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; For elisp hacking, when you can remember the keystrokes but not the name! ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun insert-key-command (key-sequence)
  "Insert at point the emacs function name bound to KEY-SEQUENCE."
  (interactive "kInsert emacs function for key sequence: ")
  (insert (symbol-name (key-binding key-sequence)) " "))

;;; end of new-stuff.el
;----------------------------------------------------------------
;--
;__John            When asked to attend a court case, Father Moses took with him
;          a leaking jug of water. Asked about it, he said: "You ask me to judge
;               the faults of another, while mine run out like water behind me."
;
;                jcgs@uk.co.harlqn (UK notation) jcgs@harlqn.co.uk (most places)
;    ...!mcvax!ukc!harlqn!jcgs (uucp - really has more stages, but ukc knows us)
;John Sturdy                                            Telephone +44-223-872522
;                      Harlequin Ltd, Barrington Hall, Barrington, Cambridge, UK
