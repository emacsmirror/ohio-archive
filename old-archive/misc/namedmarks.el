;;;_* namedmarks.el, for emacs v19 and v18.

;;;_ + Commentary

;;; This package implements named marks for emacs v 18 and 19.  It
;;; provides a custom version of set-mark-command, and provides 
;;; 'goto-mark' for the purposes of 'exchange-dot-and-mark'.
;;;
;;; In the absence of a repeat-count, the new 'set-mark-command' works
;;; like the standard version.  With a repeat count less than 16, a
;;; string is solicited, to be used as the name of a buffer-specific
;;; variable that is associated with the current point.  This value is
;;; not put on the mark stack.  Rather, you move to a named mark by
;;; invoking 'goto-mark' with a repeat count, which then solicits the
;;; name of the mark to which you want to return.
;;;
;;; Name solicitations involve full completion and defaulting
;;; facilities.
;;;
;;; See the doc strings for the functions for more details.
;;;
;;; You only need to load this package to get the functionality.  It
;;; substitutes the new definition of set-mark-command for the old
;;; one, and binds goto-mark to any keys that are globally bound to
;;; exchange-point-and-mark.

;;;_ + Officious Admin
;;;_  - Identification stuff
;; LCD Archive Entry:
;; namedmarks|Ken Manheimer|klm@nist.gov|
;; Per-buffer marks referred to by name, with completion|
;; 15-Nov-1993|V 1.6|~/misc/namedmarks.el.Z|
;;
;; named marks
;; 
;; Copyright (C) 1991 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.
;;;_  - Change log
;; modified: 15-Nov-1993 klm (apparently, need not have our own
;; 			      version of pop-mark-command.  Also,
;; 			      added substitute-key-definition for
;;			      comissioning of goto-mark)
;; modified:  7-Jun-1993 klm (exhibit number of marks when soliciting)
;;			     (other small changes - null-string macro, docs)
;;			     (deleting mark revises last-refd if necessary)
;;			     (reset named-marks if deleting first mark - delq
;;			      doesn't)
;; modified: 18-Jun-1992 baw (named marks are now really markers)
;; modified:  3-Nov-1989 baw (added provide)
;; created : ?1987? klm@cme.nist.gov (a long time ago)

;;;_ + provide
(provide 'namedmarks)

;;
;; named marks
;;
;;+

;;;_ + Key setup
(substitute-key-definition 'exchange-point-and-mark
			   'goto-mark
			   (current-global-map))

;;;_ + Internal vars
(make-variable-buffer-local 'named-marks)
(set-default 'named-marks (list nil))

(make-variable-buffer-local 'last-refd-mark-name)
(set-default 'last-refd-mark-name "")

;;;_ + Code

;;;_  > set-mark-command (arg)
(defun set-mark-command (arg)

  "Like normal set-mark unless invoked with a repeat count.  With a
repeat count less than 16, a name to be associated with the the mark
is solicited.  Repeat count greater than 16 causes a 'pop-mark'
operation.  Use `goto-mark' with repeat count to return to named
marks.  Named-marks are buffer specific."

  (interactive "p")
  (cond
                                        ; no repeat count supplied, do vanilla:
   ((= arg 1) (if (string-match "^18\\." emacs-version)
                  (push-mark)
                (push-mark nil nil t)))
   ((< arg 16)
    (let* ((pending-mark-num (length named-marks))
	   (name
	    (completing-read
	     (concat (if (> pending-mark-num 10)
					; Include mark number to cue user
					; there's, essentially, a lot:
			 (format "Set mark %d named" pending-mark-num)
		       "Set mark named")
		     (if (not (null-string last-refd-mark-name))
					; Include prior refd name as default:
			 (format " (default %s): " last-refd-mark-name)
		       ": "))
	     named-marks))
	   (cell (assoc
		  (if (not (null-string name)) name
		    (if (not (null-string last-refd-mark-name))
			(setq name last-refd-mark-name)
		      (error "No name indicated - mark not set")))
		  named-marks)))
      (if cell				      ; if name already established
	  (rplacd cell (list (point-marker))) ; associate it with new pos,
	(setq named-marks		      ; or create entire new entry.
	      (cons (list name (point-marker)) named-marks)))
      (message "Mark `%s' set" (setq last-refd-mark-name name))))
   (t (car (pop-mark))(message "Mark popped."))))


;;;_  > goto-mark (arg)
(defun goto-mark (arg)
  "Exchange point and mark unless invoked with a repeat count, in which
case point is moved to the mark associated with the (completing) prompted name.
Named-marks are buffer specific.  With a repeat count greater than or
equal to 16, named marks can be deleted from the list."
  (interactive "p")
  (cond
   ((= arg 1) (exchange-point-and-mark))
   ((= 1 (length named-marks)) (error "No named marks in this buffer."))
   ((< arg 16)
    (let* ((name
	    (completing-read
	     (if (not (equal last-refd-mark-name ""))
		 (format "Goto mark named (default %s): "
			 last-refd-mark-name)
	       "Goto mark named: ")
	     named-marks nil t)))	; require established name
      (goto-char
       (car (cdr (assoc (if (not (null-string name))
			    (setq last-refd-mark-name name)
			  (if (not (null-string last-refd-mark-name))
			      last-refd-mark-name
			    (error "No established named marks")))
			named-marks))))))
   (t (let* ((name
	      (completing-read
	       (if (not (equal last-refd-mark-name ""))
		   (format "Kill mark named (default %s): "
			   last-refd-mark-name)
		 "Kill mark named: ")
	       named-marks nil t))
	     (cell (assoc
		    (if (not (null-string name)) name
		      (if (not (null-string last-refd-mark-name))
			  (setq name last-refd-mark-name)
			(error "No name indicated - mark not killed")))
		    named-marks)))
	(if cell
	    (progn
	      (set-marker (car (cdr cell)) nil)
	      ;; find the cdr pointing to this cell
	      (if (eq cell (car named-marks))
		  (setq named-marks (cdr named-marks))
		(delq cell named-marks))
	      (if (string= last-refd-mark-name name)
		  ; Revise last-refd to lead one still on list, or null string:
		  (setq last-refd-mark-name
			(if (not (null (setq cell (car named-marks))))
			    (car cell)
			  "")))))))))

;;;_  > Incidental - null-string(string)
(defmacro null-string (string) (` (string= (, string) "")))

;;;_ + Barry's note
;;;From warsaw@anthem.nlm.nih.gov Thu Jun 18 12:28:47 1992
;;;Return-Path: <warsaw@anthem.nlm.nih.gov>
;;;Received: from cme.nist.gov (durer.cme.nist.gov)
;;;	     by glyph.cme.nist.gov (4.1/SMI-3.2-del.6)
;;;	id AA23779; Thu, 18 Jun 92 12:28:46 EDT
;;;Received: from nlm.nih.gov (lhc.nlm.nih.gov)
;;;	     by cme.nist.gov (4.1/SMI-3.2-del.5)
;;;	id AA26427; Thu, 18 Jun 92 12:28:31 EDT
;;;Received: from anthem.nlm.nih.gov by nlm.nih.gov (4.1/SMI-4.0)
;;;	id AA06443; Thu, 18 Jun 92 12:28:32 EDT
;;;Received: by anthem.nlm.nih.gov (4.1/SMI-4.1)
;;;	id AA00754; Thu, 18 Jun 92 12:28:31 EDT
;;;Date: Thu, 18 Jun 92 12:28:31 EDT
;;;From: warsaw@anthem.nlm.nih.gov (Barry A. Warsaw)
;;;Message-Id: <9206181628.AA00754@anthem.nlm.nih.gov>
;;;To: klm@cme.nist.gov
;;;Cc: gnu.emacs.sources@prep.ai.mit.edu
;;;Subject: New version of namedmarks.el
;;;Reply-To: warsaw@nlm.nih.gov (Barry A. Warsaw)
;;;Status: RO
;;;
;;;
;;;Here is a new version of Ken Manheimer's excellent namedmarks.el. Now
;;;marks are actually marks instead of just buffer positions so
;;;insertions before marks works correctly.  Since a proliferation of
;;;marks in a buffer can slow insertion down, I've added the ability to
;;;kill marks from the named mark list.  To preserve keybindings, I've
;;;put killing marks on goto-mark with a repeat count >= 16.
;;;
;;;Enjoy,
;;;-Barry
;;;
;;;-------------------- snip snip --------------------


;;;_* Local emacs vars.
'(This topic sets initial outline exposure of the file when loaded by emacs,
  Encapsulate it in comments if file is a program otherwise ignore it,
;;;_ * local variables:
;;;_ * eval:
	     (condition-case err
		(save-excursion
			(outline-mode t)
			(outline-lead-with-comment-string "\;\;\;_")
			(message "Adjusting '%s' visibility" (buffer-name))
			(goto-char 0) (outline-exposure -1 0))
		(error (message  "Failed file var 'allout' provisions")))
;;;_ * End:
)
