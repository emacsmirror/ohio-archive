;; $Id: flyspell-idle.el,v 1.2 2000/02/10 17:31:25 mlombard Exp $	

;;        Name: Flyspell idle
;;   $Revision: 1.2 $
;;       $Date: 2000/02/10 17:31:25 $
;;      Author: Marco Lombardi <lombardi@sns.it>
;;  Maintainer: Marco Lombardi <lombardi@sns.it>
;;        Info: Flyspell a whole buffer on background
;;    Filename: flyspell-idle.el
;;    Category: Text
;;              Utilities
;; 
;; Commentary:
;; Flyspell-idle completes flyspell by defining
;; some functions to perform spell checking of
;; a buffer or a region on background (i.e.
;; when Emacs is idle). This way the user can
;; spell check a whole buffer and continuing
;; working without waiting for the spelling.
;; 
;; Entry points:
;; * `flyspell-idle-region' spells (on the background) the current
;;   selected region. This function replaces `flyspell-region'
;; * `flyspell-idle-buffer' spells (on the background) the whole 
;;   buffer. This function replaces `flyspell-buffer' and is suitable 
;;   to be added to hooks suchs as `text-mode-hook' or `LaTeX-mode-hook'.
;; * `flyspell-idle-stop' interrupts a background spelling.
;;
;; Code:
(require 'flyspell)


;*---------------------------------------------------------------------*/
;*    User variables ...                                               */
;*---------------------------------------------------------------------*/

(defcustom flyspell-idle-delay 1
  "*The number of idle seconds to wait before (re)starting the spelling (see `flyspell-idle-buffer' or `flyspell-idle-region')."
  :group 'flyspell
  :type 'number)


;*---------------------------------------------------------------------*/
;*    Internal variables ...                                           */
;*---------------------------------------------------------------------*/

(defvar flyspell-idle-beg (make-marker)
  "Marker to the starting position of the region to check.")

(defvar flyspell-idle-end (make-marker)
  "Marker to the final position of the region to check.")

(defvar flyspell-idle-current (make-marker)
  "Marker to the current word to check.")

(defvar flyspell-idle-timer nil
  "Timer for on-the-fly idle spelling.")


;*---------------------------------------------------------------------*/
;*    flyspell-idle-region ...                                         */
;*---------------------------------------------------------------------*/

(defun flyspell-idle-region (beg end)
  "Flyspell text between BEG and END in background (idle)."
  (interactive "r")
  (let ((old beg)
	(stop nil))
    (if (< beg end) ()
      (setq beg end)
      (setq end old))
    (if (not (marker-buffer flyspell-idle-current))
	(message "Spell check (idle)...")
      (cancel-timer flyspell-idle-timer)
      (if (y-or-n-p "Spell check (idle) running. Abort? ")
	  (message "Restaring spell check (idle)...")
	(message "Continuing spell check (idle)...")
	(setq flyspell-idle-timer 
	      (run-with-idle-timer flyspell-idle-delay
				   nil 'flyspell-idle-continue))
	(setq stop t)))
    (if stop ()
      (set-marker flyspell-idle-beg beg)
      (set-marker flyspell-idle-end end)
      (set-marker-insertion-type flyspell-idle-end t)
      (set-marker flyspell-idle-current beg)
      (set-marker-insertion-type flyspell-idle-current nil)
      (setq flyspell-idle-timer 
	    (run-with-idle-timer flyspell-idle-delay
				 nil 'flyspell-idle-event-handler)))))

(defun flyspell-idle-event-handler ()
  "Continue the idle on-the-fly spelling interrupted
for user input pending."
  (let (end)
    (if (/= flyspell-idle-current flyspell-idle-beg)
	(message "Continuing spell check (%2d%%)..." 
		 (/ (- (marker-position flyspell-idle-current) 
		       (marker-position flyspell-beg))
		    (- (marker-position flyspell-idle-end) 
		       (marker-position flyspell-beg)))))
    (save-excursion
      (set-buffer (marker-buffer flyspell-idle-current))
      (save-excursion
	(goto-char (marker-position flyspell-idle-current))
	(setq end (marker-position flyspell-idle-end))
	(while (and (< (point) end) 
		    (not (input-pending-p)))
	  (flyspell-word)
	  (let ((cur (point)))
	    (forward-word 1)
	    (if (and (< (point) end) (> (point) (+ cur 1)))
		(backward-char 1))))
	(set-marker flyspell-idle-current (point))
	(if (< (point) end)
	    (setq flyspell-idle-timer 
		  (run-with-idle-timer flyspell-idle-delay
				       nil 'flyspell-idle-event-handler))
	  (backward-char 1)
	  (message "Spell check done")
	  (flyspell-word)
	  (cancel-timer flyspell-idle-timer)
	  (set-marker flyspell-idle-beg nil)
	  (set-marker flyspell-idle-end nil)
	  (set-marker flyspell-idle-current nil))))))


;*---------------------------------------------------------------------*/
;*    flyspell-idle-buffer ...                                         */
;*---------------------------------------------------------------------*/

(defun flyspell-idle-buffer ()
  "Flyspell whole buffer in background, without interrupting user activities."
  (interactive)
  (flyspell-idle-region (point-min) (point-max)))


;*---------------------------------------------------------------------*/
;*    flyspell-idle-stop ...                                           */
;*---------------------------------------------------------------------*/

(defun flyspell-idle-stop ()
  "Interrupt a background spell checking."
  (interactive)
  (if (not (marker-buffer flyspell-idle-current)) ()
    (cancel-timer flyspell-idle-timer)
    (set-marker flyspell-idle-beg nil)
    (set-marker flyspell-idle-end nil)
    (set-marker flyspell-idle-current nil)
    (message "Spell check interrupted!")))

(provide 'flyspell-idle)
