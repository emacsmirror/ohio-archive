;Date: Fri, 10 Mar 89 12:54:06 EST
;From: Root Boy Jim <rbj@nav.icst.nbs.gov>
;To: phs%lifia.imag.fr@uunet.uu.net
;Cc: unix-emacs@bbn.com
;Subject: Selective Dired
;
;? From: Philippe Schnoebelen <mcvax!inria!imag!lifia!phs@uunet.uu.net>
;? Reply-To: Philippe Schnoebelen <phs@lifia.imag.fr>
;? Organization: Lab. LIFIA -- Univ. Grenoble
;
;? Is there any hook, regexp, alist or whatever trick available to tell Dired
;? not to list files such and such ? [ For example, I can imagine situations
;? where I would like the *.c and *.h files to be displayed (with also core
;? and Makefile :-) but not the *.o. Similarly when editing TeX files, I do
;? not need the *.aux, *.log, *.blg, ... to be listed.  This is just a matter
;? of having everything fitting in the window for easy selection. ] Surely,
;? there must exist something like that !
;
;? --Philippe
;
;Here is part of my dired-hook.el, loaded from .emacs.

(defun dired-mode-hook ()
  "Define dired extensions"
  (setq case-fold-search t)	; why is this set to nil?
  (define-key dired-mode-map "k" 'dired-kill-files)
  (define-key dired-mode-map "K" 'dired-kill-line)
  (define-key dired-mode-map "l" 'dired-relist)
  (define-key dired-mode-map "R" 'dired-rmail)
  (define-key dired-mode-map "j" 'dired-find-file) ; an alias
  (setq dired-mode-hook nil))

(defun dired-kill-files (regexp)	; k
  "Kill lines matching REGEXP"
  (interactive "sRegexp: ")
  (let ((buffer-read-only nil))
    (beginning-of-buffer) ; probably should be (goto-char 1)
    (delete-matching-lines regexp))
  (set-mark-command t)) ; then we wouldn't need this

(defun dired-kill-line ()		; K from gosmacs
  "Kill this Line, but not this File."
  (interactive)
  (let ((buffer-read-only nil))
    (dired-redisplay nil)))

(defun dired-relist ()			; l
  "Relist this file"
  (interactive)
  (let ((buffer-read-only nil))
  (dired-redisplay (dired-get-filename))))

(defun dired-rmail ()			; R quite useful!
  "Run `rmail' on this file"
  (interactive)
  (let ((buffer-read-only nil)
        (buffer (current-buffer))
	(file (dired-get-filename)))
    (rmail file)
    (recursive-edit)
    (switch-to-buffer buffer)
    (dired-redisplay file)))

;	Catman Rshd <rbj@nav.icst.nbs.gov>
;	Author of "The Daemonic Versions"

