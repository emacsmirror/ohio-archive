;; -*- emacs-lisp -*-
;; mew-caesar.el --- Caesar encrypt/decrypt assistant package for Mew.
;;
;;                         "Hideyuki SHIRAI" <Shirai@rdmg.mgcs.mei.co.jp>
;;                                            Created: <02/07/1998>
;;                                Revised: Time-stamp: <04/02/1999 18:27 shirai>
;;
;; To use mew-caesar.el, install (tm|SEMI) package or "nkf"
;;  , and put the following codes in your .emacs.
;;
;; (add-hook 'mew-init-hook
;;  	  (lambda ()
;;  	    (require 'mew-caesar)))
;;

(eval-when-compile
  (require 'mew))

(defconst mew-caesar-version "mew-caesar.el 0.21")

(defvar mew-caesar-ext-prog
  (let (extprog)
    (cond
     ((or (memq system-type '(OS/2 emx))
	  (eq system-type 'windows-nt))
      (cond
       ((setq extprog (mew-which "nkf.exe" exec-path))
	extprog)
       ((setq extprog (mew-which "nkf32.exe" exec-path))
	extprog)
       (t nil)))
     (t (setq extprog (mew-which "nkf" exec-path))
	extprog)))
  "mew-caesar external program.
 Usually, auto searched \"nkf\", \"nkf.exe\" or \"nkf32.exe\"."
  )

(defvar mew-caesar-ext-prog-arg '("-r"))

(defvar mew-caesar-function
  (cond
   ((or (featurep 'mule-caesar)
	(locate-library "mule-caesar"))
    (require 'mule-caesar)
    'semi)
   ((or (featurep 'tm-def)
	(locate-library "tm-def"))
    (require 'tm-def)
    'tm)
   (mew-caesar-ext-prog
    'ext)
   (t
    (message "mew-caesar: program is not found.")
    nil))
  "mew-caesar function select.
 Usually auto selected, which
 'semi(mule-caesar), 'tm(tm:caesar-region) or 'ext(mew-caesar-ext-prog)."
  )

(defvar mew-caesar-prog-xrot '(mew-caesar-mime-text/x-rot () nil))
(defconst mew-caesar-ct-rot13 "Text/X-Rot13-47-48")
(defconst mew-caesar-rot13-suffix ".rot")

(define-key mew-summary-mode-map "\C-cr" 'mew-caesar-summary-insert-xrot)
(define-key mew-draft-attach-map "R" 'mew-caesar-attach-find-new-xrot)

(setq mew-mime-content-type-text-list
      (append
       '("Text/X-Rot13-47-48")
       mew-mime-content-type-text-list))

(setq mew-mime-content-type-list
      (append
       '("Text/X-Rot13-47-48")
       mew-mime-content-type-list))

(setq mew-mime-content-type
      (append
       '(("text/x-rot13-47-48" "\\.rot$" nil mew-caesar-prog-xrot mew-icon-text)
	 ("text/x-rot13.*" "\\.rot$" nil mew-caesar-prog-xrot mew-icon-text))
       mew-mime-content-type))

(defun mew-caesar-mime-text/x-rot (begin end &optional params execute)
  (if (> end begin)
      (save-excursion
	(set-buffer (mew-buffer-message))
	(let ((buffer-read-only nil))
	  (insert " #     #         ######  ####### #######    #     #####\n"
		  "  #   #          #     # #     #    #      ##    #     #\n"
		  "   # #           #     # #     #    #     # #          #\n"
		  "    #     #####  ######  #     #    #       #     #####\n"
		  "   # #           #   #   #     #    #       #          #\n"
		  "  #   #          #    #  #     #    #       #    #     #\n"
		  " #     #         #     # #######    #     #####   #####\n"
		  "\n")
	  (insert "To save this part, type "
		  (substitute-command-keys
		   "\\<mew-summary-mode-map>\\[mew-summary-save].")
		  "\nTo display this part in Message mode, type "
		  (substitute-command-keys
		   "\\<mew-summary-mode-map>\\[mew-caesar-summary-insert-xrot]."))
	  (insert "\n\n-------------------- Original \"X-ROT13\" follows --------------------\n")
	  (insert-buffer-substring (mew-current-get 'cache) begin end)
	  ))))

(defun mew-caesar-summary-insert-xrot ()
  (interactive)
  (let* ((ofld-msg (mew-current-get 'message))
	 (msg (mew-summary-message-number))
	 (nums (mew-syntax-nums))
	 (buf (buffer-name)))
    (if (or msg (not nums))
	(let ((mew-analysis nil))
	  (mew-summary-display 'force))
      (unwind-protect
	  (progn
	    (mew-summary-toggle-disp-msg 'on)
	    (mew-window-configure buf 'message)
	    (set-buffer (mew-buffer-message))
	    (let* ((buffer-read-only nil)
		   (syntax (mew-cache-decode-syntax (mew-cache-hit ofld-msg)))
		   (stx (mew-syntax-get-entry syntax nums))
		   (begin (mew-syntax-get-begin stx))
		   (end (mew-syntax-get-end stx)))
	      (erase-buffer)
	      (insert-buffer-substring (mew-current-get 'cache) begin end)
	      (mew-caesar-whole-buffer)
	      (mew-message-set-end-of)
	      (goto-char (point-min))))
	(mew-pop-to-buffer buf)))
    ))

(defun mew-caesar-attach-find-new-xrot ()
  "Open a new Caesar encrypt file into a buffer on \".\" in attachments."
  (interactive)
  (if (not (mew-attach-not-line012-1))
      (message "Can't find a new file here.")
    (let* ((nums (mew-syntax-nums))
	   (subdir (mew-attach-expand-path mew-encode-syntax nums))
	   (mimedir (mew-expand-folder (mew-draft-to-mime (buffer-name))))
	   file filepath)
      ;; mimedir / {subdir/} dir
      (if (not (equal subdir ""))
	  (setq mimedir (expand-file-name subdir mimedir)))
      ;; mimedir / file
      (setq filepath (mew-random-filename mimedir mew-caesar-rot13-suffix))
      (if (null filepath)
	  (message "Could not make a text file, sorry.")
	(setq file (file-name-nondirectory filepath))
	(setq mew-encode-syntax
	      (mew-syntax-insert-entry
	       mew-encode-syntax
	       nums
	       (mew-encode-syntax-single file (list mew-caesar-ct-rot13))))
	(mew-encode-syntax-print mew-encode-syntax)
	;;
	(find-file filepath)
	;; buffer switched
	(setq mode-name "X-Rot13")
	(setq mode-line-buffer-identification mew-mode-line-id)
	(local-set-key "\C-c\C-q" 'mew-kill-buffer)
	(local-set-key "\C-cr" 'mew-caesar-whole-buffer)
	(local-set-key "\C-c\C-s" 'mew-caesar-save-exit)
	(insert " #     #         ######  ####### #######    #     #####\n"
		"  #   #          #     # #     #    #      ##    #     #\n"
		"   # #           #     # #     #    #     # #          #\n"
		"    #     #####  ######  #     #    #       #     #####\n"
		"   # #           #   #   #     #    #       #          #\n"
		"  #   #          #    #  #     #    #       #    #     #\n"
		" #     #         #     # #######    #     #####   #####\n")
	(insert "\n define-key \"\\C-cr\"    -> mew-caesar-whole-buffer.")
	(insert "\n define-key \"\\C-c\\C-s\" -> mew-caesar-save-exit.")
	(insert "\n\n Press any key to start editting.")
	(read-char-exclusive)
	(delete-region (point-min) (point-max))
	(run-hooks 'mew-caesar-xrot-mode-hook)
	))))

(defun mew-caesar-save-exit ()
  "Caesar encrypt/decrypt at whole buffer, save and exit."
  (interactive)
  (mew-caesar-whole-buffer)
  (if (y-or-n-p (format "Save & Exit ?"))
      (progn
	(save-buffer)
	(kill-buffer (current-buffer)))
    (mew-caesar-whole-buffer)))

(defun mew-caesar-whole-buffer ()
  "Caesar encrypt/decrypt at whole buffer."
  (interactive)
  (mew-caesar-region (point-min) (point-max)))

(defun mew-caesar-region (min max)
  "Caesar encrypt/decrypt in region."
  (interactive "r")
  (save-excursion
    (cond
     ((eq mew-caesar-function 'semi)
      (mule-caesar-region min max))
     ((eq mew-caesar-function 'tm)
      (progn
	(goto-char min)
	(push-mark (point) nil t)
	(goto-char max)
	(tm:caesar-region)))
     ((and (eq mew-caesar-function 'ext)
	   mew-caesar-ext-prog mew-caesar-ext-prog-arg)
      (let ((input-coding-system mew-cs-autoconv)
	    (output-coding-system mew-cs-7bit)
	    (coding-system-for-read mew-cs-autoconv)
	    (coding-system-for-write mew-cs-7bit))
	(apply 'call-process-region min max
	       mew-caesar-ext-prog 
	       t t nil
	       mew-caesar-ext-prog-arg)))
     (t
      (message "mew-caesar: program is not found.")))
    ))

(provide 'mew-caesar)
;;
