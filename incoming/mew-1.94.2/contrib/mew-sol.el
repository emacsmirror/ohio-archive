;;; mew-sol.el --- mark messages w/ From: matching addresses in Addrbook

;;; Written by: Sen Nagata <sen@eccosys.com>
;;; Important Note: most of the functions in here are based on code in
;;;                 mew-picks.el which was not written by me

;; Keywords: solicited, unsolicited, mew
;; Version: 0.3

;;; Commentary:
;;
;; installation:
;;
;;   -put this file in an appropriate directory so emacs can find it
;;
;;   -put:
;;
;;     (add-hook 'mew-init-hook (lambda () (require 'mew-sol)))
;;
;;    in .emacs (or wherever you place your mew settings)
;;
;; usage:
;;
;;   -invoke mew
;;
;;   -use the command `mew-summary-mark-sol' to mark solicited
;;    messages for a given folder in summary mode.  by 'solicited messages'
;;    i mean messages w/ From: addresses that appear in Addrbook
;;
;;   -use the command `mew-summary-mark-unsol' to mark unsolicited
;;    messages for a given folder in summary mode.  by 'unsolicited messages'
;;    i mean messages w/ From: addresses that do not appear in Addrbook
;;
;; notes:
;;
;;   -hacked mew-dups.el :-)

;;; History:
;;
;; 0.3:
;;
;;  patches from Hideyuki SHIRAI <shirai@rdmg.mgcs.mei.co.jp> for
;;  `mew-summary-pick-unsol' and the defadvice bit (`mew-status-update' ->
;;  `mew-addrbook-setup')
;;
;; 0.2:
;;
;;  first version of `mew-summary-mark-unsol'
;;  thanks to Hideyuki SHIRAI <shirai@rdmg.mgcs.mei.co.jp> for help
;;
;;  abstracted out portions of `mew-summary-mark-sol-region' as 
;;  `mew-summary-mark-region-skel' and implemented a second version of
;;  `mew-summary-mark-unsol-region'
;;
;; 0.1:
;;
;;  initial implementation

;;; Code:
(defconst mew-sol-version "mew-sol.el 0.3")

(defconst mew-sol-address-alist nil
  "Association list of addresses from which mail is solicited.")

;; based heavily on `mew-summary-search-mark-region'
;; there are basically two major changes:
;;
;;   1) no pattern
;;   2) can call functions other than `mew-summary-pick'
;;
(defun mew-summary-mark-region-skel (r1 r2 pick-function)
  (if (equal (point-min) (point-max))
      (message "No messages in this buffer.")
    (let ((folder (buffer-name))
	  first last range)
      (message "Picking messages in %s ..." folder)
      (goto-char r1)
      (if (eobp)
	  () ;; r1 <= r2, so if r1 = (point-max) then no message.
	(setq first (mew-summary-message-number))
	(goto-char r2)
	(if (eobp)
	    (progn
	      (forward-line -1)
	      (setq r2 (point))))
	(setq last (mew-summary-message-number))
	;; this is the major change
	(setq range 
	      (apply pick-function (list folder (concat first "-" last)))))
      (message "Picking messages in %s ... done" folder)
      (if (null range)
	  (message "No message to be marked.")
	(message "Marking messages ... ")
	(goto-char r1)
	(while (and range (< (point) r2))
	  (if (re-search-forward (format "^[ ]*%s[^0-9]" (car range)) nil t)
	      (if (not (mew-summary-marked-p))
		  (mew-summary-mark-as mew-mark-review)))
	  (setq range (cdr range)))
	(beginning-of-line)
	(set-buffer-modified-p nil)
	(message "Marking messages ... done")))))

;; based heavily on `mew-summary-pick'
(defun mew-summary-pick-sol (folder &optional range)
  (let (msgs address)
    (setq range (or range "all"))
    (save-excursion
      (mew-set-buffer-tmp)
      (mew-im-call-process nil mew-prog-imls
			   (format "--src=%s" folder)
			   "--form=%n %P"
			   range)

      ;; imls doesn't fail?
      ;; two sections removed that were in mew-summary-picks

      (goto-char (point-min))
      (while (not (eobp))
	;; why are there trailing spaces?
	;; cheating on regex for address probably...
	(if (re-search-forward "^\\([0-9]+\\) \\([^ ]+\\) .*$")
	    (if (assoc (mew-match 2) mew-sol-address-alist)
		(setq msgs (cons (mew-match 1) msgs))))
	(forward-line))
      (nreverse msgs))))

;; based heavily on `mew-summary-pick'
(defun mew-summary-pick-unsol (folder &optional range)
  (let (msgs address)
    (setq range (or range "all"))
    (save-excursion
      (mew-set-buffer-tmp)
      (mew-im-call-process nil mew-prog-imls
			   (format "--src=%s" folder)
			   "--form=%n %P"
			   range)

      ;; imls doesn't fail?
      ;; two sections removed that were in mew-summary-picks

      (goto-char (point-min))
      (while (not (eobp))
	;; why are there trailing spaces?
	;; cheating on regex for address probably...
	(if (re-search-forward "^\\([0-9]+\\) \\([^ ]+\\) .*$")
	    (if (and (not (assoc (mew-match 2) mew-sol-address-alist))
		     (not (string-match "^to:" (mew-match 2))))
		(setq msgs (cons (mew-match 1) msgs))))
	(forward-line))
      (nreverse msgs))))

(defun mew-summary-mark-sol-region (r1 r2)
  (interactive "r")
  (mew-summary-mark-region-skel r1 r2 'mew-summary-pick-sol))

(defun mew-summary-mark-unsol-region (r1 r2)
  (interactive "r")
  (mew-summary-mark-region-skel r1 r2 'mew-summary-pick-unsol))

;; based heavily on `mew-summary-search-mark'
(defun mew-summary-mark-skel (region-function &optional arg)
  (mew-summary-only
   (if arg
       (apply region-function (list (region-beginning) (region-end)))
     (apply region-function (list (point-min) (point-max))))))

(defun mew-summary-mark-sol (&optional arg)
  "Pick solicited messages."
  (interactive "P")
  (mew-summary-mark-skel 'mew-summary-mark-sol-region arg))

(defun mew-summary-mark-unsol (&optional arg)
  "Pick unsolicited messages."
  (interactive "P")
  (mew-summary-mark-skel 'mew-summary-mark-unsol-region arg))

(defun mew-sol-get-addresses-from-addrbook ()
  "Build `mew-sol-address-alist' from `mew-addrbook-alist'. "
  (let (result-alist)
    (mapcar

     (lambda (x)
       ;; we are looking for elements of mew-addrbook-alist which are
       ;; lists of email addresses
       (if (listp (car (cdr x)))
	   (mapcar
	    ;; create a cons cell using each email address and add the result
	    ;; to our alist
	    (lambda (y)
	      (setq result-alist
		    (cons (cons y "")
			  result-alist)))
	    (car (cdr x)))))

     mew-addrbook-alist)
    result-alist))

(defun mew-sol-make-address-alist ()
  (setq mew-sol-address-alist (mew-sol-get-addresses-from-addrbook)))

;; this needs to happen after Addrbook is read in...unfortunately,
;; that happens after mew-init-hook -- so my hack for the moment is to
;; use advice
(require 'advice)
(defadvice mew-addrbook-setup (after mew-sol-address-alist-calc activate)
  (mew-sol-make-address-alist))

; why didn't using mew-addrbook-make-alist work?  however, it looks like 
; `mew-status-update' might be a good place to do things anyway
;(defadvice mew-addrbook-make-alist (after mew-sol-address-alist-calc activate)


(provide 'mew-sol)

;;; mew-sol.el ends here

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ;; by Hideyuki SHIRAI <shirai@rdmg.mgcs.mei.co.jp>
; (defun mew-summary-mark-exchange (&optional arg)
;   (interactive "P")
;   (let ((mark-tmp ?#))
;     (mew-summary-exchange-mark mew-mark-multi mark-tmp)
;     (mew-summary-mark-swap)
;     (mew-summary-mark-all)
;     (if (not arg)
; 	(mew-summary-batch-unmark (list mew-mark-multi) nil))
;     (mew-summary-exchange-mark mark-tmp mew-mark-multi))
;   (message "Marks exchanged."))

; ;; quick hack version
; (defun mew-summary-mark-unsol (&optional arg)
;   "Pick unsolicited messages."
;   (interactive "P")
;   (if arg
;       (mew-summary-mark-sol arg)
;     (mew-summary-mark-sol))
;   (mew-summary-mark-swap)
;   (mew-summary-mark-all)
;   ;; from `mew-summary-undo-all'
;   (let ((char ?@))
;     (mew-summary-batch-unmark (list char) 'msg)))

; (defun mew-summary-mark-unsol (&optional arg)
;   "Pick unsolicited messages."
;   (interactive "P")
;   (if arg
;       (mew-summary-mark-sol arg)
;     (mew-summary-mark-sol))
;   (mew-summary-mark-exchange))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
