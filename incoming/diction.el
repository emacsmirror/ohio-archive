;; diction.el --- minor mode to interface the command diction
;;
;; $Id: diction.el,v 1.4 1998/05/08 21:35:16 utcke Exp utcke $
;;
;; Copyright completely Sven Utcke.
;;
;; Put diction.el somewhere in your load-path and the following 
;; into your .emacs:
;;
;; (require diction)
;;
;; You might want to customize this by setting the variables 
;; diction-detex and diction-diction:
;;
;; (setq diction-detex   '"detex -C |")
;; (setq diction-diction '"diction -L")
;;
;; Finally, before running either diction-region or diction-buffer, 
;; you would probably want to set diction-ruleset interactively.  
;; If you're working on an english buffer, just type:
;;
;; M-x set-variable RET diction-ruleset RET "en"
;;
;; And on a German buffer
;;
;; M-x set-variable RET diction-ruleset RET "de"
;;


;; This should really pic one out of a list of possibilities
;; also, it should be buffer-local
(defvar diction-ruleset '"de"
  "* The ruleset to be used.  
Currently only \"de\" (Deutsch) and \"en\" (English) are provided.")

;; the detex-command
(defvar diction-detex '"/home/utcke/src/detex-2.6/detex -C | sed -f /home/utcke/bin/gtex2iso.sed | "
  "* Used to remove TeX-constructs from the file.  Maybe this should
only be set if diction is called from a TeX-buffer?")

;; the actual diction-command
(defvar diction-diction '"diction -L"
  "* The command calling diction.")

;; Patterns for hilit19.  This also defines the patterns normally used 
;; in compilation-mode, since I'm going to replace the standard ones.  
;; Naughty me...
(defvar diction-hilit19-patterns
  '(
    ("^[-_./\"A-Za-z0-9]+:[0-9]+: diction" 0 string)
    ("^[-_./\"A-Za-z0-9]+:[0-9]+: diction:.*$" 0 default)
    ("\\[[^]]+->" 0 error)		;maybe that's overdoing it?
    ("\\[[^]]+\\]" 0 rule)
    ("^[-_.\"A-Za-z0-9]+\\(:\\|, line \\)[0-9]+: warning:.*$" 0 warning)
    ("^[-_.\"A-Za-z0-9]+\\(:\\|, line \\)[0-9]+:.*$" 0 error)
    )
  )

;;
(defun diction-hilit ()
  "Set up hilit19 support for diction in compilation-mode, 
   but only if hilit19 has been pre-loaded."
  (interactive)
  (cond (hilit-default-face-table
	 ;; This is replacing the original patterns.  Naughty...
	 (hilit-set-mode-patterns 'compilation-mode diction-hilit19-patterns)
	 )))

;; this is doing all the work
(defun diction-delimited (diction-start diction-end)
  ;; make sure diction-start comes before diction-end
  ;; Do I still need this?
  (cond ((< diction-end diction-start)
	 (setq swap diction-start)
	 (setq diction-start diction-end)
	 (setq diction-end swap)
	 ))
  ;; find the current line-number:
  (let ((opoint diction-start) start)
    (save-excursion
      (save-restriction
        (goto-char (point-min))
        (widen)
        (beginning-of-line)
        (setq start (point))
        (goto-char opoint)
        (beginning-of-line)
	(setq diction-start-line (1+ (count-lines 1 (point))))
	)
      )
    )
  ;; this is the entire command we are running
  (setq diction-command (concat diction-detex diction-diction diction-ruleset ))
  ;; run diction, and put the output into a buffer *Diction-Output*
  (shell-command-on-region diction-start diction-end diction-command "*Diction-Output*")
  ;; remember the current filename
  (setq diction-buffer (concat buffer-file-name))
  ;; change to the *Diction-Output* buffer
  (set-buffer "*Diction-Output*")
  ;; replace (stdin):number with buffer:number + diction-start-line
  ;; this way we can work on regions too
  (while (re-search-forward "^(stdin):\\([0-9]+\\):" nil t)
    (let ((n (string-to-int (match-string 1))))
      (replace-match (concat diction-buffer ":" (+ n diction-start-line) ":") t nil)))
  ;; replace file:number: with file:number: diction:
  ;; This way we can do some clever highlighting
  (goto-char (point-min))
  (while (re-search-forward "^[-_./\"A-Za-z0-9]+:[0-9]+:" nil t)
    (replace-match "\\& diction:" t nil))
  ;; switch on highlighting --- this might just do nothing, 
  ;; if hilit19 isn't used
  (diction-hilit)
  ;; put the buffer into compilation-mode --- does all the nifty mouse-stuff
  (compilation-mode)
  ;; highlight
  (cond (hilit-default-face-table
	 (hilit-highlight-buffer)))
)  

(defun diction-region ()
  "Run \"detex -l | diction\" on region and display output in buffer \"*Diction-Output\"."
  (interactive)
  ;; make sure diction-start comes before diction-end
  (cond ((< (mark) (point))
	 (exchange-point-and-mark)))
  (diction-delimited (mark) (point))
)

(defun diction-buffer ()
  "Run \"detex -l | diction\" on buffer and display output in buffer \"*Diction-Output\"."
  (interactive)
  (diction-delimited (point-min) (point-max))
)

(provide 'diction)

; Changes so far
;
; $Log: diction.el,v $
; Revision 1.4  1998/05/08 21:35:16  utcke
; somewhat shorter, thanks to an idea by Kai Grossjohann.
;
; Revision 1.3  1998/05/08 20:57:09  utcke
; Improved documentation some.
;
; Revision 1.2  1998/05/08 20:37:49  utcke
; works (mainly)
;
; TODO: The first occurrence found doesn't work.  Why?
;
; Revision 1.1  1998/05/08 14:43:01  utcke
; Initial revision
;
;

