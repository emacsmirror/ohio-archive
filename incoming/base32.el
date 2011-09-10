;;; base32.el --- Base32 encoding functions
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

;; This is more than trivially different from base64, since LCM(8,5) =
;; 40 so we have to cludge around the elisp integer 28 bit limit.

;; Tested with Emacs 20.5 and XEmacs 21.1.10.

;;; Code:

(eval-when-compile
  (require 'cl))

;; For non-MULE
(eval-and-compile
  (defalias 'base32-char-int
    (if (fboundp 'char-int)
	'char-int
      'identity)))

(defvar base32-alphabet "abcdefghijklmnopqrstuvwxyz234567")

(defvar base32-decoder-program nil
  "*Non-nil value should be a string that names a MIME base32 decoder.
The program should expect to read base32 data on its standard
input and write the converted data to its standard output.")

(defvar base32-decoder-switches nil
  "*List of command line flags passed to the command named by
base32-decoder-program.")

(defvar base32-encoder-program nil
  "*Non-nil value should be a string that names a MIME base32 encoder.
The program should expect arbitrary data on its standard
input and write base32 data to its standard output.")

(defvar base32-encoder-switches nil
  "*List of command line flags passed to the command named by
base32-encoder-program.")

(defconst base32-alphabet-decoding-alist
  '((?a . 00) (?b . 01) (?c . 02) (?d . 03) (?e . 04) (?f . 05)
    (?g . 06) (?h . 07) (?i . 08) (?j . 09) (?k . 10) (?l . 11)
    (?m . 12) (?n . 13) (?o . 14) (?p . 15) (?q . 16) (?r . 17)
    (?s . 18) (?t . 19) (?u . 20) (?v . 21) (?w . 22) (?x . 23)
    (?y . 24) (?z . 25) (?2 . 26) (?3 . 27) (?4 . 28) (?5 . 29)
    (?6 . 30) (?7 . 31)))

(defvar base32-alphabet-decoding-vector
  (let ((v (make-vector 123 nil))
	(p base32-alphabet-decoding-alist))
    (while p
      (aset v (car (car p)) (cdr (car p)))
      (setq p (cdr p)))
    v))

(defvar base32-binary-coding-system 'binary)

(defun base32-run-command-on-region (start end output-buffer command
					   &rest arg-list)
  (let ((tempfile nil) status errstring default-process-coding-system 
	(coding-system-for-write base32-binary-coding-system)
	(coding-system-for-read base32-binary-coding-system))
    (unwind-protect
	(progn
	  (setq tempfile (make-temp-name "base32"))
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
    (defalias 'base32-insert-char 'insert-char)
  (defun base32-insert-char (char &optional count ignored buffer)
    (if (or (null buffer) (eq buffer (current-buffer)))
	(insert-char char count)
      (with-current-buffer buffer
	(insert-char char count))))
  (setq base32-binary-coding-system 'no-conversion))

(defun base32-decode-region (start end)
  (interactive "r")
  (message "Decoding base32...")
  (let ((work-buffer nil)
	(done nil)
	(counter 0)
	(hibits 0)
	(mibits 0)
	(lobits 0)
	(lim 0) inputpos
	(non-data-chars (concat "^=" base32-alphabet)))
    (unwind-protect
	(save-excursion
	  (setq work-buffer (generate-new-buffer " *base32-work*"))
	  (buffer-disable-undo work-buffer)
	  (if base32-decoder-program
	      (let* ((binary-process-output t) ; any text already has CRLFs
		     (status (apply 'base32-run-command-on-region
				    start end work-buffer
				    base32-decoder-program
				    base32-decoder-switches)))
		(if (not (eq status t))
		    (error "%s" (cdr status))))
	    (goto-char start)
	    (skip-chars-forward non-data-chars end)
	    (while (not done)
	      (setq inputpos (point))
	      (when (> (skip-chars-forward base32-alphabet end) 0)
		(setq lim (point))
		(while (< inputpos lim)
		  (setq lobits (lsh lobits 5)
			lobits (logior lobits (aref 
					       base32-alphabet-decoding-vector
					       (base32-char-int (char-after 
								 inputpos))))
			mibits (logior (lsh mibits 5)
				       (logand (lsh lobits -16) 31))
			hibits (logior (lsh hibits 5)
				       (logand (lsh mibits -16) 31))
			counter (1+ counter)
			inputpos (1+ inputpos))
		  (when (= counter 8)
		    (base32-insert-char (logand (lsh hibits -0) 255)
					1 nil work-buffer)
		    (base32-insert-char (logand (lsh mibits -8) 255)
					1 nil work-buffer)
		    (base32-insert-char (logand (lsh mibits -0) 255)
					1 nil work-buffer)
		    (base32-insert-char (logand (lsh lobits -8) 255)
					1 nil work-buffer)
		    (base32-insert-char (logand (lsh lobits -0) 255)
					1 nil work-buffer)
		    (setq lobits 0 mibits 0 hibits 0 counter 0))))
	      (cond ((= (point) end)
		     (if (not (zerop counter))
			 (error 
			  "at least %d bits missing at end of base32 encoding"
			  (* (- 8 counter) 5)))
		     (setq done t))
		    ((eq (char-after (point)) ?=)
		     (setq done t)
		     (let ((tmp counter))
		       (while (< tmp 8)
			 (setq lobits (lsh lobits 5)
			       mibits (logior (lsh mibits 5)
					      (logand (lsh lobits -16) 31))
			       hibits (logior (lsh hibits 5)
					      (logand (lsh mibits -16) 31))
			       tmp (1+ tmp))))
		     ;; xxx? warn on bad padding instead of being nice?
		     (when (>= counter 1)
		       (base32-insert-char (logand (lsh hibits -0) 255)
					   1 nil work-buffer))
		     (when (>= counter 4)
		       (base32-insert-char (logand (lsh mibits -8) 255)
					   1 nil work-buffer))
		     (when (>= counter 5)
		       (base32-insert-char (logand (lsh mibits -0) 255)
					   1 nil work-buffer))
		     (when (>= counter 7)
		       (base32-insert-char (logand (lsh lobits -8) 255)
					   1 nil work-buffer)))
		    (t (skip-chars-forward non-data-chars end)))))
	  (or (markerp end) (setq end (set-marker (make-marker) end)))
	  (goto-char start)
	  (insert-buffer-substring work-buffer)
	  (delete-region (point) end))
      (and work-buffer (kill-buffer work-buffer))))
  (message "Decoding base32... done"))

(defun base32-encode-region (start end &optional no-line-break)
  (interactive "r")
  (message "Encoding base32...")
  (let ((work-buffer nil)
	(counter 0)
	(cols 0)
	(lobits 0)
	(hibits 0)
	(alphabet base32-alphabet)
	inputpos)
    (unwind-protect
	(save-excursion
	  (setq work-buffer (generate-new-buffer " *base32-work*"))
	  (buffer-disable-undo work-buffer)
	  (if base32-encoder-program
	      (let ((status (apply 'base32-run-command-on-region
				   start end work-buffer
				   base32-encoder-program
				   base32-encoder-switches)))
		(if (not (eq status t))
		    (error "%s" (cdr status))))
	    (setq inputpos start)
	    (while (< inputpos end)
	      (setq lobits (lsh lobits 8))
	      (setq lobits (logior lobits (base32-char-int
					   (char-after inputpos))))
	      (setq hibits (logior (lsh hibits 8) 
				   (logand (lsh lobits -20) 255)))
	      (setq counter (1+ counter)
		    inputpos (1+ inputpos))
	      (when (= counter 5)
		(base32-insert-char
		 (aref alphabet (logand (lsh hibits -15) 31))
		 1 nil work-buffer)
		(base32-insert-char
		 (aref alphabet (logand (lsh hibits -10) 31))
		 1 nil work-buffer)
		(base32-insert-char
		 (aref alphabet (logand (lsh hibits -5) 31))
		 1 nil work-buffer)
		(base32-insert-char
		 (aref alphabet (logand (lsh hibits -0) 31))
		 1 nil work-buffer)
		(base32-insert-char
		 (aref alphabet (logand (lsh lobits -15) 31))
		 1 nil work-buffer)
		(base32-insert-char
		 (aref alphabet (logand (lsh lobits -10) 31))
		 1 nil work-buffer)
		(base32-insert-char
		 (aref alphabet (logand (lsh lobits -5) 31))
		 1 nil work-buffer)
		(base32-insert-char
		 (aref alphabet (logand (lsh lobits -0) 31))
		 1 nil work-buffer)
		(setq cols (+ cols 8))
		(when (and (= cols 72) (not no-line-break))
		  (base32-insert-char ?\n 1 nil work-buffer)
		  (setq cols 0))
		(setq lobits 0 hibits 0 counter 0)))
	    ;; write out any remaining bits...
	    (let ((tmp counter))
	      (while (< tmp 5)
		(setq lobits (lsh lobits 8))
		(setq hibits (logior (lsh hibits 8)
				     (logand (lsh lobits -20) 255)))
		(setq tmp (1+ tmp))))
	    (when (>= counter 1)
	      (base32-insert-char
	       (aref alphabet (logand (lsh hibits -15) 31))
	       1 nil work-buffer)
	      (base32-insert-char
	       (aref alphabet (logand (lsh hibits -10) 31))
	       1 nil work-buffer))
	    (when (>= counter 2)
	      (base32-insert-char
	       (aref alphabet (logand (lsh hibits -5) 31))
	       1 nil work-buffer)
	      (base32-insert-char
	       (aref alphabet (logand (lsh hibits -0) 31))
	       1 nil work-buffer))
	    (when (>= counter 3)
	      (base32-insert-char
	       (aref alphabet (logand (lsh lobits -15) 31))
	       1 nil work-buffer))
	    (when (>= counter 4)
	      (base32-insert-char
	       (aref alphabet (logand (lsh lobits -10) 31))
	       1 nil work-buffer)
	      (base32-insert-char
	       (aref alphabet (logand (lsh lobits -5) 31))
	       1 nil work-buffer))
	    ;; and appropriate padding
	    (cond ((= counter 1)
		   (base32-insert-char ?= 6 nil work-buffer))
		  ((= counter 2)
		   (base32-insert-char ?= 4 nil work-buffer))
		  ((= counter 3)
		   (base32-insert-char ?= 3 nil work-buffer))
		  ((= counter 4)
		   (base32-insert-char ?= 1 nil work-buffer)))
	    (if (and (> cols 0)
		     (not no-line-break))
		(base32-insert-char ?\n 1 nil work-buffer)))
	  (or (markerp end) (setq end (set-marker (make-marker) end)))
	  (goto-char start)
	  (insert-buffer-substring work-buffer)
	  (delete-region (point) end))
      (and work-buffer (kill-buffer work-buffer)))
    (message "Encoding base32... done")))

(defun base32-encode (string &optional no-line-break)
  (save-excursion
    (set-buffer (get-buffer-create " *base32-encode*"))
    (erase-buffer)
    (insert string)
    (base32-encode-region (point-min) (point-max) no-line-break)
    (skip-chars-backward " \t\r\n")
    (delete-region (point-max) (point))
    (prog1
	(buffer-string)
      (kill-buffer (current-buffer)))))

(defun base32-decode (string)
  (save-excursion
    (set-buffer (get-buffer-create " *base32-decode*"))
    (erase-buffer)
    (insert string)
    (base32-decode-region (point-min) (point-max))
    (goto-char (point-max))
    (skip-chars-backward " \t\r\n")
    (delete-region (point-max) (point))
    (prog1
	(buffer-string)
      (kill-buffer (current-buffer)))))

(fset 'base32-decode-string 'base32-decode)
(fset 'base32-encode-string 'base32-encode)

(provide 'base32)

;;; base32.el ends here
