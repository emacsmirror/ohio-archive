;;;; vem.el -- Compact display of rwho output
;;; Copyright (C) 1992 Lennart Staflin
;;; Last edited: Sun Dec  5 01:33:23 1993 by lenst@konrad (Lennart Staflin)
;;; $Id: vem.el,v 1.1 1993/12/05 00:33:34 lenst Exp $
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; A copy of the GNU General Public License can be obtained from this
;;; program's author (send electronic mail to les@ida.liu.se) or from
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.
;;;
;;; Send bug reports to lenst@lysator.liu.se

;; INSTALLATION
;; Save this file as "vem.el" in a Lisp directory that Emacs knows about
;; (i.e. via load-path). 
;;
;; To use it, load it and do M-x vem
;;
;; Suggested setup in your .emacs file:
;;
;; (autoload 'vem "vem" 
;;        "Show output from rwho in a compact format." t)

;; LCD Archive Entry:
;; vem|Lennart Staflin|lenst@lysator.liu.se|
;; Compact display of rwho output|
;; 05-Dec-1993|1.1|~/interfaces/vem.el.Z|

(provide 'vem)

;;;###autoload
(defun vem (&optional all)
  "Show output from rwho in a compact format.
The output from rwho is summarised with one line per user, in multiple
columns on half the screen.  The optional argument ALL (the prefix
argument), if non-nil, tells vem to run rwho with flag -a."
  (interactive "P")
  (let ((b (current-buffer))
	(summary (vem-make-summary all)))
    (if (null summary)
	(message "vem: Not a sausage!")
      (set-buffer (get-buffer-create "*vem*"))
      (erase-buffer)
      (vem-insert-summary summary)
      (goto-char (point-min))
      (delete-other-windows)
      (let* ((w (display-buffer (current-buffer)))
	     (h (1- (window-height w)))
	     (l (count-lines (point-min) (point-max)))
	     (n (/ (+ l h -1) h))
	     (c (max window-min-width
		     (1- (/ (window-width w) n)))))
	(while (not (eobp))
	  (set-window-start w (point))
	  (forward-line h)
	  (if (and (not (eobp))
		   (>= (window-width w) (+ c window-min-width)))
	      (setq w (split-window w c t)))))
      (set-buffer b))))

(defun vem-make-summary (all)
  (let ((summary '())
	(v (get-buffer-create " *v-buffer*")))
    (save-excursion
      (set-buffer v)
      (erase-buffer)
      (vem-fill-buffer all)
      (goto-char (point-min))
      (while (not (eobp))
	(re-search-forward "\\(\\S-+\\)\\s-+\\([^:]+\\):")
	(setq summary
	      (vem-add-user-machine
	       summary
	       (buffer-substring (match-beginning 1) (match-end 1))
	       (buffer-substring (match-beginning 2) (match-end 2))))
	(forward-line 1)))
    (kill-buffer v)
    summary))

(defun vem-fill-buffer (all)
  ;; Insert output from the unix program rwho into the current buffer.
  ;; If ALL is true call rwho with the -a flag.
  (message "rwho..")
  (apply 'call-process 
	 "rwho"				; program
	 nil				; infile
	 (current-buffer)		; output
	 nil				; display?
	 (if all '("-a"))		; args
	 )
  (message ""))

(defun vem-add-user-machine (summary user machine)
  (let ((ul (assoc user summary)))
    (if (null ul)
	(setq summary (nconc summary (list (list user machine))))
      (if (not (vem-member machine (cdr ul)))
	  (nconc ul (list machine)))))
  summary)

(defun vem-insert-summary (summary)
  (while summary
    (insert (car (car summary)) ":")
    (indent-to 10 1)
    (let ((machines (cdr (car summary))))
      (while machines
	(insert (car machines) " ")
	(setq machines (cdr machines))))
    (insert "\n")
    (setq summary (cdr summary))))

(defun vem-member (elt list)
  ;; Returns non-nil if ELT is an element of LIST.  Comparison done
  ;; with EQUAL.  The value is actually the tail of LIST whose car is
  ;; ELT.
  (while (and list
	      (not (equal elt (car list))))
    (setq list (cdr list)))
  list)

