;; stig-paren.el (Release 1.17) -- highlight (un)matching parentheses
;; Copyright (C) 1993 Free Software Foundation, Inc.
;; Copyright (C) 1993 Jonathan Stigelman <Stig@netcom.com>
;;
;; Original Author: rms@gnu.ai.mit.edu
;; Massively hacked by: Jonathan Stigelman <Stig@netcom.com>
;; Maintainer: Jonathan Stigelman <Stig@netcom.com>
;; Keywords: languages, faces

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;
;; Purpose of this package:
;;
;;   This package highlights matching parens (or whole sexps) for easier
;;   editing of source code, particularly lisp source code.
;;
;; Installation:
;;
;; (cond (window-system
;;        (require 'stig-paren)
;;        (setq blink-matching-paren nil)
;;        (global-set-key [?\C-\(] 'stig-paren-toggle-dingaling-mode)
;;        (global-set-key [?\C-\)] 'stig-paren-toggle-sexp-mode))
;;       (t
;;        (setq blink-matching-paren t)))
;;
;; Bugs:  You find 'em, I squash 'em.
;;
;;        M-x stig-paren-submit-feedback RET
;;
;; WHERE TO GET THE LATEST VERSIONS OF STIG-PAREN.EL (beta and release), 
;; PLUS LOTS OF OTHER *WAY COOL* STUFF VIA ANONYMOUS FTP:
;;
;;      netcom.com:/pub/stig/src/{Beta,Release}/stig-paren.el.gz
;;
;; LCD Archive Entry:
;; stig-paren|Jonathan Stigelman|Stig@netcom.com|
;; Super-cool matching paren/sexp highlighting for emacs 19|
;; 01-Oct-1993|Release 1.17|~/misc/stig-paren.el.Z|

;; stig-paren.el,v
;; Revision 1.17  1993/10/01  08:12:43  stig
;; Installed, then improved patch from klm@nist.gov (Ken Manheimer).  Now
;; stig-paren's messages for offscreen open parens will not squash messages
;; from other commands (like eval-last-sexp)...
;;
;; Also paren-message-offscreen now works (it was previously ignored).
;;
;; Revision 1.16  1993/10/01  07:04:03  stig
;; kinda works with lemacs now...not well...and it's also unsupported for
;; lemacs, but if you fix it for lemacs then let me know...
;;
;; Revision 1.15  1993/09/10  20:31:00  stig
;; now use pre-command-hook to avoid problems with leftover overlays
;; cluttering up the display when switching between buffers or when commands
;; move the point and continue reading input w/o returning to top-level.
;;
;; Revision 1.14  1993/08/25  06:09:41  stig
;; work-around for bug in pos-visible-in-window-p
;;
;; parens that match an escape character "\)" no longer signal errors and ding
;; the bell.
;;
;; Revision 1.13  1993/08/24  05:19:13  stig
;; work-around for annoying bug in delete-overlay
;;
;; Revision 1.12  1993/08/16  01:49:25  stig
;; paren-toggle-* functions now display message indicating whether the mode is
;; on or off.
;;
;; Revision 1.11  1993/07/25  19:40:25  stig
;; bug fix:  paren-sexp-mode was causing the modeline message to always appear.
;;
;; Revision 1.10  1993/07/24  13:59:03  stig
;; Noticed a wierd bug...put cursor on paren, switch to window with another
;; buffer in it...paren in old buffer stays highlighted.  This can't be
;; reproduced in a debugger and efforts to trace the appropriate variables
;; haven't been successful.  Oh Well.
;;
;; Revision 1.7  1993/07/23  06:05:42  stig
;; added stig-paren-toggle-dingaling-mode, and stig-paren-toggle-sexp-mode
;;
;; Revision 1.4  1993/07/23  02:53:39  stig
;; Changed the name to stig-paren.el to reflect the fact that RMS doesn't
;; presently plan to put this in the emacs19 release.
;;

;; Normally, stig-paren-command-hook gives priority to matching open parens
;; and looks to the position just before point for closing parens.  This is
;; intuitive if you frequently use forward-sexp (M-C-f) and backward-sexp
;; (M-C-b) to maneuver around in lisp code.

;; In stig-paren-dingaling-mode, priority is placed upon highlighting the
;; parenthesis matching whatever is underneath the cursor.

;; Different faces are used for color is used for mismatched parens.  If the
;; (mis)matching paren is offscreen, then a message is sent to the modeline.

;; In stig-paren-sexp-mode, entire S-expressions are highlighted instead of
;; just matching parens.

;;; User Options:

(defvar paren-message-offscreen t
  "* Display message if matching open paren is offscreen.")

(defvar paren-ding-unmatched nil
  "* Make noise if the cursor is at an unmatched paren.

Even if NIL, typing an unmatched paren produces a ding.")

(defvar paren-dingaling-mode nil
  "* Set to T if you'd like paren to look for closing parens after the point
instead of before the point.  Normally paren looks for close parens that
you've just typed so that you know what the paren that you've just typed
matches.

Normally priority is placed upon highlighting the opening parenthesis of the
sexp before point.  Consequently you see this behavior:

   ((   ))   or   ((   ))   or   ((   ))   or   ((   ))(   )
        P          H    P        H      P       H      P

In paren-dingaling-mode, you see this:

   ((   ))   or   ((   ))   or   ((   ))   or   ((   ))(   )
        P          H    P        H      P              P   H")

(defvar paren-sexp-mode nil
  "* Non-nil causes paren-command-hook to highlight whole S-expressions.")

(defvar paren-match-face (if (x-display-color-p)
			     'highlight
			   'underline)
  "* Matching parens (which are balanced parens) are shown in this face.  Bold
and italic faces (except for the standard ones) tend to flake out when you
change fonts...  You're responsible for maintaining the unique display
properties of this face.")

(defvar paren-mismatch-face (if (x-display-color-p)
				(let ((fn 'paren-mismatch-face))
				  (copy-face 'default fn)
				  (set-face-background fn "DeepPink")
				  fn)
			      'modeline)
  "* Mismatching parens (not to be confused with unbalanced parens) are shown
in this face.  Bold and italic faces (except for the standard ones) tend to
flake out when you change fonts...  You're responsible for maintaining the
unique display properties of this face.")

;; Hacked Lucid semi-compatibility...
(cond ((fboundp 'x-color-display-p)
       (fset 'x-display-color-p (symbol-function 'x-color-display-p))
       (defun make-overlay (st en &optional buf)
         (or (< st en) (setq stt en en st st stt))
         (make-extent st en buf))
       (defun overlay-put (ov prop val)
         (if (eq prop 'face)
             (set-extent-face ov val)))
       (defun delete-overlay (ov)
         (delete-extent ov))
       (defun move-overlay (ov st en &optional buf)
         ;; FIXME - this is BROKEN
         (delete-extent ov)
         (make-extent st en buf))
       ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To submit bug reports...

(eval-when-compile (require 'reporter))

(defun stig-paren-submit-feedback ()
  "Submit via mail a bug report on stig-paren"
  (interactive)
  (require 'reporter)
  (and (y-or-n-p "Do you really want to submit a report on stig-paren? ")
       (reporter-submit-bug-report
	"Jonathan Stigelman <Stig@netcom.com>"
	"stig-paren.el (Release 1.17)"
         '(paren-sexp-mode
	   paren-message-offscreen
	   paren-dingaling-mode
	   paren-ding-unmatched
	   paren-mismatch-face
	   paren-match-face
	   )
	 nil nil "Hey Stig, do you do anything besides hack emacs?\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code:

(defvar stig-paren-overlay nil)

(defvar paren-message-suppress nil
  "used to suppress messages from the same position so that other messages
can be seen in the modeline.")
(make-variable-buffer-local 'paren-message-suppress)

(defun stig-paren-toggle-dingaling-mode (arg)
  "Toggle paren-dingaling-mode, force off with negative arg"
  (interactive "P")
  (setq paren-dingaling-mode (if (numberp arg)
				 (> arg 0)
			       (not paren-dingaling-mode)))
  (message "Dingaling mode is %s"
	   (if paren-dingaling-mode "ON, you're a dingaling ;-)" "OFF")))

(defun stig-paren-toggle-sexp-mode (arg)
  "Toggle paren-sexp-mode, force off with negative arg"
  (interactive "P")
  (setq paren-sexp-mode (if (numberp arg)
			    (> arg 0)
			  (not paren-sexp-mode)))
  (message "Sexp mode is %s"
	   (if paren-sexp-mode "ON" "OFF")))

(defsubst pos-visible-in-window-safe (pos)
  "safe version of pos-visible-in-window-p"
  (condition-case nil
      (pos-visible-in-window-p pos)
      (args-out-of-range nil)))

(defsubst stig-paren-set-overlay (st en)
  "Move stig-paren-overlay to the region START .. END.  Create if necessary."
  (if stig-paren-overlay
      (move-overlay stig-paren-overlay st en (current-buffer))
    (setq stig-paren-overlay (make-overlay st en))))

;; Find the place to show, if there is one,
;; and show it until input arrives.
(defun stig-paren-command-hook ()
  "This highlights matching parentheses.

See the variables:
  paren-message-offscreen   use modeline when matchingparen is offscreen?
  paren-ding-unmatched	 make noise when passing over mismatched parens?
  paren-dingaling-mode	 match parens under cursor instead of before?
  paren-sexp-mode		 highlight s-expressions instead of just parens?

and
  paren-match-face, paren-mismatch-face"

  ;; I suppose I could check here to see if a keyboard macro is executing,
  ;; but I did a quick empirical check and couldn't tell that there was any
  ;; difference in performance

  (let ((oldpos (point))
	(pface nil)			; face for paren...nil kills the overlay
	pos dir mismatch)

    (save-excursion
      (if paren-dingaling-mode
	  ;; dingaling mode highlighting
	  (cond ((eq (char-syntax (following-char)) ?\))
		 (setq dir -1 oldpos (1+ oldpos))
		 (forward-char 1))
		((eq (char-syntax (following-char)) ?\()
		 (setq dir 1))
		((eq (char-syntax (preceding-char)) ?\))
		 (setq dir -1)))
	;; normal highlighting
	(cond ((eq (char-syntax (preceding-char)) ?\))
	       (setq dir -1))
	      ((eq (char-syntax (following-char)) ?\()
	       (setq dir 1))))

      (if (and dir
	       (save-restriction
		 ;; Determine the range within which to look for a match.
		 (if blink-matching-paren-distance
		     (narrow-to-region
		      (max (point-min)
			   (- (point) blink-matching-paren-distance))
		      (min (point-max)
			   (+ (point) blink-matching-paren-distance))))

		 ;; Scan across one sexp within that range.
		 (condition-case nil
		     (setq pos (scan-sexps (point) dir))
		   (error nil))))

	  ;; See if the "matching" paren is the right kind of paren
	  ;; to match the one we started at.
	  (progn
	    (let ((beg (min pos oldpos)) (end (max pos oldpos)))
	      (setq mismatch
		    (and (/= (char-syntax (char-after beg)) ?\\)
			 (/= (char-syntax (char-after beg)) ?\$)
			 (/= (char-after (1- end))
			     (logand (lsh (aref (syntax-table)
						(char-after beg))
					  -8)
				     255))))
	      (if paren-sexp-mode
		  (stig-paren-set-overlay beg end)))
	    (and mismatch
		 (or paren-ding-unmatched
		     (eq this-command 'self-insert-command))
		 (ding))
	    (setq pface (and (or paren-sexp-mode
				 (pos-visible-in-window-safe pos))
			     (if mismatch
				 paren-mismatch-face
			       paren-match-face)))
	    (cond ((pos-visible-in-window-safe pos)
		   (and (not paren-sexp-mode)
			(stig-paren-set-overlay (- pos dir) pos))
		   (setq paren-message-suppress nil))
		  ((and paren-message-offscreen
			(eq dir -1)
			(not (eq paren-message-suppress (point))))
		   (setq paren-message-suppress (point))
		   (save-excursion
		     (goto-char pos)
		     (message "%s %s"
			      (if mismatch "MISMATCH:" "Matches")
			      ;;
			      ;; if there's stuff on this line preceding the
			      ;; paren, then display text from beginning of
			      ;; line to paren.
			      ;;
			      ;; If, however, the paren is at the beginning
			      ;; of a line, then skip whitespace forward and
			      ;; display text from paren to end of the next
			      ;; line containing nonspace text.
			      ;;
			      ;; If paren-backwards-message gravity were
			      ;; implemented, then perhaps it would reverse
			      ;; this behavior and look to the previous line
			      ;; for meaningful context.
			      ;; 
			      (if (save-excursion
				    (skip-chars-backward " \t")
				    (not (bolp)))
				  (concat (buffer-substring
					   (progn (beginning-of-line) (point))
					   (1+ pos)) "...")
				(buffer-substring
				 pos (progn
				       (forward-char 1)
				       (skip-chars-forward "\n \t")
				       (end-of-line)
				       (point)))))))
		  (t (setq paren-message-suppress nil)))
	    ))
      ;; put the right face on the overlay
      (and pface (overlay-put stig-paren-overlay 'face pface))
      )))

(defun stig-paren-delete-overlay ()
  "Pre-command-hook to delete the overlay owned by stig-paren"
  (cond (stig-paren-overlay
	 (condition-case nil
	     (delete-overlay stig-paren-overlay)	     
	   (error nil))
	 (setq stig-paren-overlay nil))))

;; eliminate redundancy...
(setq post-command-hook (delq 'show-paren-command-hook post-command-hook))

(add-hook 'post-command-hook 'stig-paren-command-hook)
(add-hook 'pre-command-hook 'stig-paren-delete-overlay)

(provide 'stig-paren)
(provide 'paren)

;;; stig-paren.el ends here

; Local Variables:
; byte-optimize: t
; End:
