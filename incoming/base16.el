;;; base16.el --- Base16 encoding functions
;; Copyright (c) 2000 Simon Josefsson

;; Author: Simon Josefsson <simon@josefsson.org>
;; Keywords: encoding

;; This file is not a part of GNU Emacs, but the same permissions apply.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This is based on base64.el which is Copyright (c) by Kyle E. Jones.

;; Tested with Emacs 20.5 and XEmacs 21.1.10.

;;; Code:

(eval-when-compile
  (require 'cl))

;; For non-MULE
(if (not (fboundp 'char-int))
    (fset 'char-int 'identity))

(defvar base16-alphabet
  "abcdefghijklmnop")

(defvar base16-decoder-program nil
  "*Non-nil value should be a string that names a MIME base16 decoder.
The program should expect to read base16 data on its standard
input and write the converted data to its standard output.")

(defvar base16-decoder-switches nil
  "*List of command line flags passed to the command named by
base16-decoder-program.")

(defvar base16-encoder-program nil
  "*Non-nil value should be a string that names a MIME base16 encoder.
The program should expect arbitrary data on its standard
input and write base16 data to its standard output.")

(defvar base16-encoder-switches nil
  "*List of command line flags passed to the command named by
base16-encoder-program.")

(defconst base16-alphabet-decoding-alist
  '(( ?a . 00) ( ?b . 01) ( ?c . 02) ( ?d . 03) ( ?e . 04) ( ?f . 05)
    ( ?g . 06) ( ?h . 07) ( ?i . 08) ( ?j . 09) ( ?k . 10) ( ?l . 11)
    ( ?m . 12) ( ?n . 13) ( ?o . 14) ( ?p . 15)))

(defvar base16-alphabet-decoding-vector
  (let ((v (make-vector 123 nil))
	(p base16-alphabet-decoding-alist))
    (while p
      (aset v (car (car p)) (cdr (car p)))
      (setq p (cdr p)))
    v))

(defvar base16-binary-coding-system 'binary)

(defun base16-run-command-on-region (start end output-buffer command
					   &rest arg-list)
  (let ((tempfile nil) status errstring default-process-coding-system 
	(coding-system-for-write base16-binary-coding-system)
	(coding-system-for-read base16-binary-coding-system))
    (unwind-protect
	(progn
	  (setq tempfile (make-temp-name "base16"))
	  (setq status
		(apply 'call-process-region
		       start end command nil
		       (list output-buffer tempfile)
		       nil arg-list))
	  (cond ((equal status 0) t)
		((zerop (save-excursion
			  (set-buffer (find-file-noselect tempfile))
			  (buffer-size)))
		 t)
		(t (save-excursion
		     (set-buffer (find-file-noselect tempfile))
		     (setq errstring (buffer-string))
		     (kill-buffer nil)
		     (cons status errstring)))))
      (ignore-errors
	(delete-file tempfile)))))

(if (string-match "XEmacs" emacs-version)
    (defalias 'base16-insert-char 'insert-char)
  (defun base16-insert-char (char &optional count ignored buffer)
    (if (or (null buffer) (eq buffer (current-buffer)))
	(insert-char char count)
      (with-current-buffer buffer
	(insert-char char count))))
  (setq base16-binary-coding-system 'no-conversion))

(defun base16-decode-region (start end)
  (interactive "r")
  (message "Decoding base16...")
  (let ((work-buffer nil)
	(done nil)
	(counter 0)
	(bits 0)
	(lim 0) inputpos
	(non-data-chars (concat "^=" base16-alphabet)))
    (unwind-protect
	(save-excursion
	  (setq work-buffer (generate-new-buffer " *base16-work*"))
	  (buffer-disable-undo work-buffer)
	  (if base16-decoder-program
	      (let* ((binary-process-output t) ; any text already has CRLFs
		     (status (apply 'base16-run-command-on-region
				    start end work-buffer
				    base16-decoder-program
				    base16-decoder-switches)))
		(if (not (eq status t))
		    (error "%s" (cdr status))))
	    (goto-char start)
	    (skip-chars-forward non-data-chars end)
	    (while (not done)
	      (setq inputpos (point))
	      (cond
	       ((> (skip-chars-forward base16-alphabet end) 0)
		(setq lim (point))
		(while (< inputpos lim)
		  (setq bits (+ bits
				(aref base16-alphabet-decoding-vector
				      (char-int (char-after inputpos)))))
		  (setq counter (1+ counter)
			inputpos (1+ inputpos))
		  (cond ((= counter 2)
			 (base16-insert-char bits 1 nil work-buffer)
			 (setq bits 0 counter 0))
			(t (setq bits (lsh bits 4)))))))
	      (if (not (zerop counter))
		  (error "4 bits missing at end of base16 encoding"))
	      (setq done t)
	      (skip-chars-forward non-data-chars end)))
	  (or (markerp end) (setq end (set-marker (make-marker) end)))
	  (goto-char start)
	  (insert-buffer-substring work-buffer)
	  (delete-region (point) end))
      (and work-buffer (kill-buffer work-buffer))))
  (message "Decoding base16... done"))

(defun base16-encode-region (start end &optional no-line-break)
  (interactive "r")
  (message "Encoding base16...")
  (let ((work-buffer nil)
	(counter 0)
	(cols 0)
	(bits 0)
	(alphabet base16-alphabet)
	inputpos)
    (unwind-protect
	(save-excursion
	  (setq work-buffer (generate-new-buffer " *base16-work*"))
	  (buffer-disable-undo work-buffer)
	  (if base16-encoder-program
	      (let ((status (apply 'base16-run-command-on-region
				   start end work-buffer
				   base16-encoder-program
				   base16-encoder-switches)))
		(if (not (eq status t))
		    (error "%s" (cdr status))))
	    (setq inputpos start)
	    (while (< inputpos end)
	      (setq bits (+ bits (char-int (char-after inputpos))))
	      (base16-insert-char (aref alphabet (lsh bits -4)) 1 nil
				  work-buffer)
	      (base16-insert-char (aref alphabet (logand bits 15))
				  1 nil work-buffer)
	      (setq cols (+ cols 2))
	      (when (and (= cols 72) (not no-line-break))
		(base16-insert-char ?\n 1 nil work-buffer)
		(setq cols 0))
	      (setq bits 0)
	      (setq inputpos (1+ inputpos)))
	    (if (and (> cols 0)
		     (not no-line-break))
		(base16-insert-char ?\n 1 nil work-buffer)))
	  (or (markerp end) (setq end (set-marker (make-marker) end)))
	  (goto-char start)
	  (insert-buffer-substring work-buffer)
	  (delete-region (point) end))
      (and work-buffer (kill-buffer work-buffer))))
  (message "Encoding base16... done"))

(defun base16-encode (string &optional no-line-break)
  (save-excursion
    (set-buffer (get-buffer-create " *base16-encode*"))
    (erase-buffer)
    (insert string)
    (base16-encode-region (point-min) (point-max) no-line-break)
    (skip-chars-backward " \t\r\n")
    (delete-region (point-max) (point))
    (prog1
	(buffer-string)
      (kill-buffer (current-buffer)))))

(defun base16-decode (string)
  (save-excursion
    (set-buffer (get-buffer-create " *base16-decode*"))
    (erase-buffer)
    (insert string)
    (base16-decode-region (point-min) (point-max))
    (goto-char (point-max))
    (skip-chars-backward " \t\r\n")
    (delete-region (point-max) (point))
    (prog1
	(buffer-string)
      (kill-buffer (current-buffer)))))

(fset 'base16-decode-string 'base16-decode)
(fset 'base16-encode-string 'base16-encode)

(provide 'base16)

;; base16.el ends here
