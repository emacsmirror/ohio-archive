;;; tint-instances.el -- Tint all instances of the current word.
;;; Copyright 1997 by Bruno Daniel and Uwe Schabe

;; Author:    Bruno Daniel  <daniel@fzi.de, http://www.uni-karlsruhe.de/~ub68/>
;;            Uwe Schabe    <uwe@cad-cam-concept.de>
;; Created:   July 1997
;; Version:   1.0 
;; Keywords:  tint-instances, tinting, highlighting   
;; Availability: newsgroup "gnu.emacs.sources" and archives thereof

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This emacs script takes the word where point is currently on and tints
;; all instances of this word in the current buffer.
;; If the region is active, it is taken as the current word to be tinted.
;;
;;
;; Quickstart Installation:
;; ========================
;;
;; To get this to work, make emacs execute the line
;;
;; (require 'tint-instances)			;; load the package
;;
;; I would also recommend executing the following commands
;; so as to extend the bindings in your global keymap:
;;
;; (define-key global-map "\M-c" 'tint_all_instances_of_current_word)
;; (define-key global-map "\M-v" 'untint_all_instances_of_current_word)
;;
;; If there are problems with the colors used, change the entries in the
;; color_array to your favourite colors (you can list all available colors
;; of your installation by the command "M-x list-colors-display")
;;
;;; Code:

(defun current_word ()
  "Return the word where point is currently on.
Dependent on the major mode punctuations are considered as part of the word:
emacs-lisp-mode:  '_' '-'
other modes:      '_'

If the region is active, it's taken instead." 

  (interactive)
  (if mark-active
      (buffer-substring-no-properties (region-beginning) (region-end))
    (save-excursion 
      (forward-word 1)
      ;; In `emacs-lisp-mode' the chartacters '_' and '-'
      ;; are considered as part of the word.
      (if (eq major-mode 'emacs-lisp-mode)
	  (progn
	    (while (progn
		     (forward-word -1)
		     (or (char-equal (preceding-char) ?_)
			 (char-equal (preceding-char) ?-))
		     ))
	    (setq last_word (point))
	    (while (progn
		     (forward-word 1)
		     (or (char-equal (following-char) ?_)
			 (char-equal (following-char) ?-))
		     ))
	    (setq next_word (point)))
	;; In all other modes (especially C modes),
	;; only the chartacter '_' is considered as part of the word.
	(while (progn
		 (forward-word -1)
		 (char-equal (preceding-char) ?_)))
	(setq last_word (point))
	(while (progn
		 (forward-word 1)
		 (char-equal (following-char) ?_)))
	(setq next_word (point))
	)
      (buffer-substring-no-properties last_word next_word))))


; Painting everything red is a bit boring. So, here is a small array of
; colors in order to distinguish different highlightings.
(setq color_array ["Yellow"  "OrangeRed" "SkyBlue" "PaleGreen" "Plum"
                   "Orange"  "Cyan" "Firebrick" "LightBlue"  "YellowGreen"
                   "Pink"])
; The index indicating the current color
(setq current_color_index 0)


(defun tint_all_instances_of_current_word ()
  "Tints all instances of the word in the current buffer point is on.
   If the region is active it's taken instead."
  (interactive)
  (let* ((word_was_marked mark-active)
	 (desired_face (facemenu-get-face
			(intern (concat "bg:" (aref color_array
						    current_color_index)))))
	 (curr_word (current_word))
	 (word_length (length curr_word)))
    (save-excursion
      (beginning-of-buffer)
      ;; Search for `curr_word', ignoring differences in punctuation.
      ;; If the region wasn't active, perform word search.
      (while (if word_was_marked (search-forward curr_word nil t)
	       (word-search-forward curr_word nil t))
	(let* ((begin_of_word (- (point) word_length))
	       (char_before_begin_of_word (char-after (1- begin_of_word))))
	  ;; But punctuation is significant!
	  ;; In `emacs-lisp-mode', if `curr_word' is preceded or followed
	  ;; by the characters '_' or '-', it is part of another word.
	  ;; Don't tint it!
	  (when (or word_was_marked
		    (not (or (char-equal (following-char) ?_)
			     (char-equal char_before_begin_of_word ?_)
			     (and (eq major-mode 'emacs-lisp-mode)
				  (or (char-equal (following-char) ?-)
				      (char-equal char_before_begin_of_word ?-)
				      )))))
			 (overlay-put (make-overlay begin_of_word (point))
				      'face desired_face))
	    ))))
  (setq current_color_index (1+ current_color_index))
  (if (>= current_color_index (length color_array))
      (setq current_color_index 0)))


(defun untint_all_instances_of_current_word ()
  "Delete all overlay-colors of all instances of the current word."
  (interactive)
  (let ((word_was_marked mark-active)
	(curr_word (current_word)))
    (save-excursion
      (beginning-of-buffer)
      (while (if word_was_marked (search-forward curr_word nil t)
	       (word-search-forward curr_word nil t))
	(mapcar 'delete-overlay (overlays-at (- (point) 1)))))))


(provide 'tint-instances)
;; tint-instances.el ends here
