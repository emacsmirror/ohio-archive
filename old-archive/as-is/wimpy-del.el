;Date: Wed, 14 Jun 89 00:30:03 EDT
;From: bard@theory.lcs.mit.edu
;To: unix-emacs@BBN.COM
;Subject: kill-region-wimpy
;
;
;While I'm posting random code, here's a useful gizmo: a bit more cautious
;than emacs is wont to be, but not bad.  It's a variant of delete-region
;which, if you're deleting more than (say) 100 characters, prints the first
;few words at the beginning and end of the region (and the region size) and
;asks you if you really want to delete all that.  It's good for novices, and
;also good if you're not completely sure where the region is (it's an
;alternative to c-x c-x to see where it is, and much better on a slow
;terminal.)
;
;-- bard the emacs gargoyle

;; wimpy-delete -- a cautious form of delete-region.
;; Copyright (C) Bard Bloom, June 1989
;; bard@theory.lcs.mit.edu

;; This file is not yet part of GNU Emacs.

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

;; WHAT THIS DOES:

;; It's a variant of kill-region (c-w) which, if the region is too big
;; (default 100 characters), asks you if you really want to delete it.  The
;; prompt gives a couple words at each end, and tells you how big the region
;; is.

;; There's a related command, describe-region, which simply messages a few
;; words at each end of the region, and the total size.

(require 'cl)

(defvar wimpy-delete-size 100
   "kill-region-wimpy will ask if you want to delete more than 
this many characters.  Setting it to nil inhibits this feature altogether.")

(defvar wimpy-delete-dopey-message "Gosh, that was a narrow escape!"
  "Message that kill-region-wimpy prints when you tell it not to 
delete the region.")

(defun kill-region-wimpy (beg end)
  "Like kill-region, this deletes the text between BEG and END, and stuffs it
in the kill ring.  (Interactively, uses the region.) 
However, it asks you if you really want to delete a large chunk of text."
  (interactive "r")
  (let ((size (- end beg)))
    (cond
     ((and wimpy-delete-size 
           (> size wimpy-delete-size)
           (not
            (y-or-n-p
             (region-description (- (screen-width) 6) "Delete `"  "'?")
             )))
      (message "%s" wimpy-delete-dopey-message))
     (t (kill-region beg end)))))



(defun describe-region ()
  "Gives a message briefly describing the text at the beginning and end 
of the region."
  (interactive)
  (message
   "%s" (region-description (- (screen-width) 2))))

(defun absdiff (m n) (if (< m n) (- n m) (- m n)))


(defun region-description (width &optional prefix suffix begin end)
  "Returns a string containing a one-line description of the region.
Required argument WIDTH tells how the string can be; there's a lower
limit of about 20 characters.
Optional arguments:
  PREFIX and SUFFIX are strings to tack on the ends of the message.
     They *do* count towards its length. Defaults: null strings both of them.
  BEGIN and END are the beginning and end of the region to use, if it's
     to the real region."
  (unless prefix (setq prefix ""))
  (unless suffix (setq suffix ""))
  (when (and begin (not end)) (setq end (point)))
  (cond
   (begin
    (psetq begin (min begin end)
           end   (max begin end)))
   (t
    (setq begin (min (point) (mark))
          end   (max (point) (mark)))))
;  (message "%d `%s' `%s' %d %d" width prefix suffix begin end)
  (cond
   ((< (- end begin) (- width 2))
    (concat "\"" (buffer-substring begin end) "\""))
   (t
    (let*
        ((chars-string (format "...<%d chars>..." (- end begin)))
         (space-for-messages
          (+ (length prefix)
             (length suffix)
             (length chars-string)))
         (space-for-quote (/ (max 0 (- width space-for-messages)) 2))
         (beg-words (pick-some-words begin nil space-for-quote))
         (end-words (pick-some-words end   t   space-for-quote))
         )
      (concat prefix beg-words chars-string end-words suffix)
      )));let*,cond
)

(defun pick-some-words (pos direction limit)
  "Arguments  POS DIRECTION LIMIT.  Returns a string of 
at most LIMIT chars, with one end at POS.  It will try to fit as many words 
into the string as it can.  If it can't fit even one word, it will take 
LIMIT characters.  DIRECTION = nil for forward, t for backward."
  (save-excursion
    (goto-char pos)
    (let (p q)
      (if direction (backward-word 1) (forward-word 1))
      (if (> (absdiff (point) pos) limit)
          (buffer-substring pos (+ (if direction (- limit) limit) pos))
        (setq p (point) q t)
        (while (and q (not (eobp)) (not (bobp)))
          (if direction (backward-word 1) (forward-word 1))
          (if (<= (absdiff (point) pos) limit)
              (setq p (point))
            (goto-char p)
            (setq q nil)))
        (buffer-substring pos (point))))))
                
(defun absdiff (m n) (if (< m n) (- n m) (- m n)))
