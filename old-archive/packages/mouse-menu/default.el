;;; Scottish HCI Centre default initialisation file for GNU Emacs.
;;; Russell Ritchie, Scottish HCI Centre, <russell@uk.ac.strath.hci>.
;;; Tue Mar 21 17:40:45 1989 

;;; This loads the X, SUN and Terminal Menu software default menus, as
;;; is appropriate for the particular emacs process you have started.
;;; If you do not want this, put the following line in your .emacs:
;;; 	(setq inhibit-default-init t)

(require 'def-menus)			; Load the menu software.

;;; Tell Emacs where the Prolog, Postscript and [La]TeX things are.
(let ((fn-file-alist
       '(((start-prolog prolog-mode quintus-prolog-mode c-prolog-mode)
	  . "Prolog")
	 ((plain-tex-mode tex-mode latex-mode TeX-mode LaTeX-mode)
	  . "TeX-mode")
	 ((bibtex-mode BibTeX-mode) . "BibTeX-mode") 
	 ((dired dired-noselect dired-other-window) . "dired-dir"))))
  (while fn-file-alist
    (let* ((fns-file-pair (car fn-file-alist)) 
           (fns (car fns-file-pair))	; Get the fns.
           (file (cdr fns-file-pair)))	; Get the file.
      (while fns			; Mark each fn for autoloading.
	(let* ((fn (car fns))		; If fn is fboundp, it's commandp-ness
	       (doc (if (fboundp fn) (documentation fn)))) ; is defined,
	  (autoload fn file doc (if doc (commandp fn) t)) ; else assume ok.
	  (setq fns (cdr fns)))))
    (setq fn-file-alist (cdr fn-file-alist)))) ; Get next fns-file pair.

(setq auto-mode-alist			; Put them on first so they
      (append				; override any current actions.
        '(("\\.pl$"  . start-prolog)	   ; Run PROLOG for "*.pl"
	  ("\\.bib$" . BibTeX-mode))	   ; For [La]TeX bib. files.
        auto-mode-alist))