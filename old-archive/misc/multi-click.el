;;; multi-click.el (used to be: my-mouse.el)

;;; Copyright (C) 1993  Alon Albert <alon@milcse.rtsg.mot.com> 

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;

;;; LCD Archive Entry:
;;; multi-click|Alon Albert|alon@milcse.rtsg.mot.com|
;;; Support for double and triple clicking in Emacs.|
;;; 27-Jul-1993|1.3|~/misc/multi-click.el.Z|

;;; Commentary:
;;;   Support for double and triple clicking in emacs
;;;   Double click marks a word (if clicked inside a word), a sexp if clicked
;;;   on a paren and a group of words seperated by delimiter (if clicked on a
;;;   delimeter (well, not exactly but it's kinda hard to explain in simple
;;;   terms so try it out to see how it behaves. try clicking on spaces or
;;;   periods inside an email address etc...))
;;;   triple click marks a line.
;;;   If a word was marked with a double click then clicking mouse-3 extends
;;;   the region to the next word delimiter. Same for lines.
;;;   The above applies also to a multi click followd by a drag. Note that in
;;;   contrass to xterm and my-mouse.el, an extra click is needed here:
;;;   To mark whole lines by draging, triple click and then drag (click again)
;;;   This is because dragging does not generate a multiclick event in emacs.

;;; History:

;;; v1.3 July 27 1993 Alon Albert
;;;   Deactivate mark  by running (deactivate-mark) instead of
;;;    (setq mark-active nil)
;;;   Fixed bug related to undo after double mouse-3 click.
;;; v1.2 July 21 1993 Alon Albert
;;;   Extending selection (mouse-3) works both directions.
;;;   Clicking mouse-3 inside a selection will cut it leaving the longer piece
;;;   selected.
;;; v1.1 July 19 1993 Alon Albert
;;;   multi-click works with dynamic draging
;;; v1.0 July 18 1993 Alon Albert
;;;   first release. (well not realy since it was released under a different
;;;     name.

;;;   Double clicking sexp and groups was suggested and implemented by:
;;;     nickson@cs.uq.oz.au


;;; Instalation:
;;;   put this file in your load-path and insert the following in .emacs
;;;
;;;     (require 'multi-click)
;;;
;;; For best results add also:
;;;
;;;   (transient-mark-mode 1)
;;;
;;; And if you only have a b/w monitor:
;;;
;;;   (set-face-background 'region "black")
;;;   (set-face-foreground 'region "white")
;;;
;;; or vice versa if you use reverse video

(defvar multi-click-click 0
  "number of clicks on this point")

(defvar multi-click-save-then-kill-posn nil)

(defun multi-click-skip-word (posn dir)
  "skip over syntax chars at posn. If Dir is positive skip forward, if negative
skip back and set mark"
  (let ((syntax (char-to-string (char-syntax (or (char-after posn)
						 (char-after (1- posn)))))))
    (if (string= syntax "w") nil
      (setq syntax (concat syntax "w")))
    (goto-char posn)
    (if (< dir 0)
        (skip-syntax-backward syntax)
      (skip-syntax-forward syntax)
      (set-mark (point)))))

    
(defun multi-click-skip-line (posn dir)
  "skip to start/next line. If Dir is positive skip forward, if negative
skip back and set mark"
  (goto-char posn)
  (if (< dir 0)
      (beginning-of-line)
    (forward-line 1)
    (set-mark (point))))

(defun multi-click-start-end (start end mode &optional keep-end)
  "Set point and mark at START and END according to MODE:
if MODE is 0 then set point to (min START END), mark to (max START END)
if MODE is 1 then set point to start of word at (min START END), mark to end
of word at (max START END)
if MODE is 2 then do the same for lines.
Optional KEEP-END if non-nil means do not change end"
  (if (> start end)
      (let ((temp start))
        (setq start end
              end temp)))
  (cond ((= mode 0)
         (set-mark start)
         (goto-char end))
        ((and (= mode 1)
              (= start end)
              (= (char-syntax (or (char-after start)
				  (char-after (1- start)))) ?\())
         (goto-char start)
         (forward-sexp 1)
         (set-mark (point))
         (goto-char start))
        ((and (= mode 1)
              (= start end)
              (= (char-syntax (or (char-after start)
				  (char-after (1- start)))) ?\)))
         (goto-char (1+ start))
         (set-mark (point))
         (backward-sexp 1))
        ((= mode 1)
         (or keep-end (multi-click-skip-word end 1))
         (multi-click-skip-word start -1))
        ((= mode 2)
         (or keep-end (multi-click-skip-line end 1))
         (multi-click-skip-line start -1)))
  (let (this-command)
    (kill-new "")
    (kill-ring-save (point) (mark))))

 
(defun multi-click-set-point (click)
  "Move point to the position clicked on with the mouse."
  (interactive "e")
  (let ((posn (event-start click)))
    (select-window (posn-window posn))
    (if (numberp (posn-point posn))
        (goto-char (posn-point posn)))
    (setq multi-click-click 0)
    (deactivate-mark)))

(defun multi-click-set-word (click)
  "Mark word and point to begining of word."
  (interactive "e")
  (let* ((event (event-start click)))
    (multi-click-start-end (point) (point) 1)
    (setq multi-click-click 1)))

(defun multi-click-set-line (click)
  "Mark word and point to begining of word."
  (interactive "e")
  (let* ((event (event-start click)))
    (multi-click-start-end (point) (point) 2)
    (setq multi-click-click 2)))

(defun multi-click-drag-region (start-event)
  "Set the region to the text that the mouse is dragged over.
Highlight the drag area as the user moves the mouse.
This must be bound to a button-down mouse event."
  (interactive "e")
  (let* ((start-posn (event-start start-event))
         (start-point (posn-point start-posn))
         (start-window (posn-window start-posn))
         (start-frame (window-frame start-window))
         (bounds (window-edges start-window))
         (top (nth 1 bounds))
         (bottom (if (window-minibuffer-p start-window)
                     (nth 3 bounds)
                   ;; Don't count the mode line.
                   (1- (nth 3 bounds)))))
    (select-window start-window)
    (goto-char start-point)
    (move-overlay mouse-drag-overlay
                  start-point start-point
                  (window-buffer start-window))
    (deactivate-mark)
    (let (event end end-point)
      (track-mouse
        (while (progn
                 (setq event (read-event))
                 (or (mouse-movement-p event)
                     (eq (car-safe event) 'switch-frame)))

          (if (eq (car-safe event) 'switch-frame)
              nil
            (setq end (event-end event)
                  end-point (posn-point end))

            (cond

             ;; Ignore switch-frame events.
             ((eq (car-safe event) 'switch-frame))

             ;; Are we moving within the original window?
             ((and (eq (posn-window end) start-window)
                   (integer-or-marker-p end-point))
              (goto-char end-point)
              (move-overlay mouse-drag-overlay
                            start-point (point)))

             ;; Are we moving on a different window on the same frame?
             ((and (windowp (posn-window end))
                   (eq (window-frame (posn-window end)) start-frame))
              (let ((mouse-row
                     (+ (nth 1 (window-edges (posn-window end)))
                        (cdr (posn-col-row end)))))
                (cond
                 ((< mouse-row top)
                  (mouse-scroll-subr
                   (- mouse-row top) mouse-drag-overlay start-point))
                 ((and (not (eobp))
                       (>= mouse-row bottom))
                  (mouse-scroll-subr (1+ (- mouse-row bottom))
                                     mouse-drag-overlay start-point)))))

             ;; Otherwise, we have no idea where the mouse is.
             (t)))))

      (let ((fun (key-binding (vector (car event)))))
        (if (fboundp fun)
            (funcall fun event))))))

(defun multi-click-set-region-store (click)
  "Set the region to the text that the mouse is dragged over.
Store reagion in kill-ring.
Point is set to start of region.
Works like multi-click-set-point in regard to double clicks.
This must be bound to a mouse drag event."
  (interactive "e")
  (let* ((start (event-start click))
         (end (event-end click)))
    (select-window (posn-window start))
    (multi-click-start-end (posn-point start) (posn-point end)
                        multi-click-click)))
              
(defun multi-click-save-then-kill (click)
  "Save text to point in kill ring; the second time, kill the text.
If the text between point and the mouse is the same as what's
at the front of the kill ring, this deletes the text.
Otherwise, it adds the text to the kill ring, like \\[kill-ring-save],
which prepares for a second click to delete the text.
If point was set with a double click then set mark on whole word
If point was set with a triple click then set mark on whole line"
  (interactive "e")
  (let ((click-posn (posn-point (event-start click)))
	;; Don't let a subsequent kill command append to this one:
	;; prevent setting this-command to kill-region.
	(this-command this-command))
    (if (and (eq last-command 'multi-click-save-then-kill)
	     multi-click-save-then-kill-posn
	     (eq (car multi-click-save-then-kill-posn) (car kill-ring))
	     (equal (cdr multi-click-save-then-kill-posn)
		    (list (point) click-posn)))
	;; If this is the second time we've called
	;; multi-click-save-then-kill, delete the text from the buffer.
	(progn
	  (let ((buffer-undo-list t))
	    (delete-region (point) (mark)))
	  ;; Make the undo list by hand so it is shared.
	  (if (not (eq buffer-undo-list t))
	      (setq buffer-undo-list
		    (cons (cons (car kill-ring) (point)) buffer-undo-list))))
      ;; Otherwise, save this region.
      (if (and mark-active
	       (< (abs (- click-posn (point))) (abs (- click-posn (mark)))))
          (multi-click-start-end (mark) click-posn
                                 multi-click-click t)
        (multi-click-start-end (point) click-posn
                               multi-click-click))
      (setq multi-click-save-then-kill-posn
            (list (car kill-ring) (point) click-posn)))))

(global-set-key [mouse-1]                 'multi-click-set-point)
(global-set-key [down-mouse-1]            'multi-click-drag-region)
(global-set-key [double-mouse-1]          'multi-click-set-word)
(global-set-key [triple-mouse-1]          'multi-click-set-line)
(global-set-key [mouse-3]                 'multi-click-save-then-kill)
(global-set-key [drag-mouse-1]            'multi-click-set-region-store)

(provide 'multi-click)
