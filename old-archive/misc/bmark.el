;; -*- Mode: Emacs-Lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File:	bmark.el
;; Description:	Interbuffer mark ring support.
;; Author:	Ken Laprade <laprade@trantor.harris-atd.com>
;; Created:	Thu Sep  6 18:15:33 1990
;; Modified:	Thu Sep 20 07:59:50 1990
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Harris-ATD-style interbuffer mark ring support.
;;
;; Copyright (C) 1990 Ken Laprade <laprade@trantor.harris-atd.com>

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; this file, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;; This file contains, in part, code originally distributed with GNU Emacs.

;; --- Usage:
;; These functions provide an inter-buffer mark ring quite similar to the
;; individual buffer mark rings provided with GNU emacs. set-bmark-command
;; works practically just like set-mark-command, but the bmark-ring is
;; global rather than buffer-local, so its marks can be in any buffer.
;; pop-bmark is the same as set-bmark-command with an argument.  There is
;; also an unpop-bmark function that reverses the order the bmark-ring is
;; traversed.

;; --- Installation:
;; Put these in default.el or ~/.emacs:
;(autoload 'set-bmark-command "bmark" "Add point to inter-buffer mark ring, or jump to top entry on ring." t)
;(autoload 'push-bmark "bmark" "Push point onto top of inter-buffer mark ring." t)
;(autoload 'pop-bmark "bmark" "Pop top of inter-buffer mark ring." t)
;(autoload 'exchange-point-and-bmark "bmark" "Exchange point and top of inter-buffer mark ring." t)
;(or (key-binding "\C-X\C-@")
;    (global-set-key "\C-X\C-@" 'set-bmark-command))


(defvar bmark-ring nil
  "The list of saved inter-buffer marks, most recent first.")

(defvar bmark-ring-max 16
  "*Maximum size of inter-buffer mark ring.  Start discarding off
end if it gets this big.")

(defun cleanup-bmark-ring ()
  "Remove all markers that no longer are valid from the bmark-ring."
  (while (and bmark-ring
	      (null (marker-buffer (car bmark-ring))))
    (setq bmark-ring (cdr bmark-ring)))
  (let ((l bmark-ring))
    (while (cdr l)
      (if (marker-buffer (car (cdr l)))
	  (setq l (cdr l))
	(setcdr l (cdr (cdr l)))))))

(defun set-bmark-command (arg)
  "Add point to inter-buffer mark ring, or jump to top entry on ring.
With no prefix argument, push point mark on inter-buffer mark ring.
With positive argument, pop top entry off inter-buffer mark ring and
move point to it, switching buffer if necessary.  With negative
argument, pop into other window."
  (interactive "P")
  (if (null arg)
      (push-bmark)
    (if (null bmark-ring)
	(error "Nothing in inter-buffer mark ring")
      (pop-bmark (< (prefix-numeric-value arg) 0)))))

(defun push-bmark (&optional location buffer nomsg)
  "Add LOCATION in BUFFER (point in current buffer, by default)
to the inter-buffer mark ring.  Displays \"Inter-buffer mark set\"
unless the optional third arg NOMSG is non-nil."
  (cleanup-bmark-ring)
  (let ((marker (make-marker)))
    (set-marker marker (or location (point)) buffer)
    (setq bmark-ring (cons marker bmark-ring))
    (if (> (length bmark-ring) bmark-ring-max)
	(progn
	  (set-marker (nth bmark-ring-max bmark-ring) nil)
	  (setcdr (nthcdr (1- bmark-ring-max) bmark-ring) nil))))
  (or nomsg executing-macro (> (minibuffer-depth) 0)
      (message "Inter-buffer mark set")))

(defun pop-bmark (&optional other)
  "Pop top of inter-buffer mark ring, switch buffer if necessary,
and set point.  Does nothing if inter-buffer mark ring is empty.
With optional OTHER, does pop in other window."
  (interactive "P")
  (cleanup-bmark-ring)
  (if bmark-ring
      (let ((bmark (car bmark-ring)))
	(if other
	    (switch-to-buffer-other-window (marker-buffer bmark))
	  (switch-to-buffer (marker-buffer bmark)))
	(goto-char (marker-position bmark))
	(setq bmark-ring (nconc (cdr bmark-ring) (list bmark))))))

(defun unpop-bmark (&optional other)
  "Goto bottom of inter-buffer mark ring, switch buffer if necessary,
and set point.  Needs at least two entries in inter-buffer mark ring.
Rotates the ring in the opposite direction of pop-bmark.
With optional OTHER, does unpop in other window."
  (interactive "P")
  (cleanup-bmark-ring)
  (if (and bmark-ring (cdr bmark-ring))
      (let ((l bmark-ring)
	    bmark)
	(while (cdr (cdr l))
	  (setq l (cdr l)))
	;; Previous bmark is really second from bottom.  Bottom is current.
	(setq bmark (car l))
	(if other
	    (switch-to-buffer-other-window (marker-buffer bmark))
	  (switch-to-buffer (marker-buffer bmark)))
	(goto-char (marker-position bmark))
	(setcdr (cdr l) bmark-ring)
	(setq bmark-ring (cdr l))
	(setcdr l nil))))

(defun exchange-point-and-bmark ()
  "Put current point on top of the inter-buffer mark ring
and put point in its place."
  (interactive)
  (cleanup-bmark-ring)
  (let* ((bmark (car bmark-ring))
	(b (marker-buffer bmark))
	(p (marker-position bmark)))
    (set-marker bmark (point))
    (switch-to-buffer b)
    (goto-char p)))

(provide 'bmark)
