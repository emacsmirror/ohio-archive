;;; comint-hilit.el --- hilight comint buffers using hilit19

;; Copyright (C) 1994 David M. Smith

;; Author: David Smith <maa036@lancaster.ac.uk>, after inspiration
;;         from Gunnar Teege <teege@informatik.tu-muenchen.de>
;; Maintainer: David Smith <maa036@lancaster.ac.uk>
;; Created: 23 November, 1993
;; Modified: $Date: 1994/03/01 12:27:01 $
;; Version: $Revision: 2.2 $
;; RCS-Id: $Id: comint-hilit.el,v 2.2 1994/03/01 12:27:01 maa036 Exp $
;; Keywords: faces
;; LCD Archive Entry:
;; comint-hilit|Dave M. Smith|maa036@lancaster.ac.uk|
;; Hilit support for comint buffers (such as *shell*)|
;; 01-Mar-1994|2.2|~/misc/comint-hilit.el.Z|

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; comint-hilit.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; Requires FSF Gnu Emacs version 19.20 or greater.

;;; Commentary:

;; All comint buffers (any buffer whose major mode calls comint-mode
;; and uses the default comint-output-filter) are highlighted in three
;; ways by default: the prompt, the input, and comint messages
;; (currently *** output flushed *** is the only one) are all
;; highlighted. Interpreter output appears in the default face. If the
;; major mode of the comint buffer already has hilit patterns (as set
;; by hilit-set-mode-patterns) these are used, otherwise they are
;; automatically set (making use of comint-prompt-regexp).
;;
;; Highlighting occurs dynamically with each process output.

;; To use: put the following in your .emacs file:
;;
;;   (eval-after-load "comint" '(require 'comint-hilit))
;;
;; If comint is dumped in your emacs, just (require 'comint-hilit)

;; NB: The default shell-mode prompt regexp is a bit too general,
;; and so you can end up with some output highlighted as a prompt
;;
;; The default patterns are pretty dull. On the other hand, I don't
;; really expect these defaults to be used: the major mode which uses
;; comint should define its own patterns. For example, shell-mode
;; should highlight "cd"'s etc. For example, by placing the following
;; code in .emacs:

;;    (defun shell-setup-hilit nil
;;	(hilit-translate shell-error 'error)
;;	(hilit-translate shell-cd    'include)
;;	(hilit-translate shell-job   'define)
;;	(hilit-set-mode-patterns 
;;	 'shell-mode
;;	 (list 
;;	     (list "^[a-zA-Z]+:" "$" 'shell-error) ; command error
;;	     (list "^\\[[1-9][0-9]*\\]" "$" 'shell-job) ; not for sh
;;	     (list comint-prompt-regexp 
;;		   nil 
;;		   'comint-prompt)
;;	     (list (concat comint-prompt-regexp "cd") 
;;		   "$"
;;		   'shell-cd)		; cd calls
;;	     (list comint-prompt-regexp
;;		   "$"
;;		   'comint-input)
;;	     (list "^\\*\\*\\*.+\\*\\*\\*\\s *$" 
;;		   nil
;;		   'comint-message))))
;;    (add-hook 'shell-mode-hook 'shell-setup-hilit)

;; you will additionally get highlighting of shell errors, job
;; messages, and cd commands.

;; C-x C-o (comint-kill-output) ought really rehighlight the "output
;; flushed" message, but does not do so by default. You can arrange
;; for this with the following code:

;;    (require 'advice)
;;    (defun comint-message-rehilit nil
;;	(save-excursion
;;	 (comint-previous-prompt 1)
;;	 (hilit-rehighlight-region (point) (point-max) t)
;;	 (comint-hilit-function)))
;;    
;;    (defadvice comint-kill-output (after rehilit-message activate)
;;       "Rehighlight the *** output flushed *** message"
;;       (comint-message-rehilit)
;;       (comint-show-maximum-output))
  
;; This also returns the prompt to the bottom window line after 
;; C-x C-o: a useful win IMO

;; The latest version of comint-hilit.el is available from (in increasing
;; order of likelihood):
;;
;; By WWW:
;;    http://mathssun5.lancs.ac.uk:2080/~maa036/elisp/dir.html 
;; By anonymous FTP:
;;    /anonymous@wingra.stat.wisc.edu:pub/src/emacs-lisp/comint-hilit.el.gz
;; Or from the Emacs-Lisp archive.

;;; Code:

(require 'comint)
(require 'hilit19)

;;; Define the new faces

(defun comint-hilit-choose (color-list)
  "Choose a face from COLOR-LIST, a list of 3 faces, according to
   hilit-background-mode. The order is '(light dark mono). The third
   face is always chosen for mono displays."
  (nth (or (and (x-display-color-p)
		(cdr (assq hilit-background-mode
			   '((light . 0) (dark . 1)))))
	   2) color-list))

(hilit-translate comint-prompt 
		 (comint-hilit-choose '(blue-bold cyan-bold default-bold)))

(hilit-translate comint-message 
		 (comint-hilit-choose '(red-italic orange-italic default-italic)))

(hilit-translate comint-input 
		 (comint-hilit-choose '(firebrick-bold-italic khaki-bold-italic default-bold-italic)))

;;; Function to be called in comint-output-filter-functions

(defun comint-hilit-function (&rest junk) ; we don't need no stinkin' args
  "Highlight the portion of the buffer just output by the process filter"
  (let ((proc (get-buffer-process (current-buffer))))
    (and (eq (process-filter proc) 'comint-output-filter) ; exit gracefully if not
	 (let* ((pmark (process-mark proc))
		(pmarkpos (marker-position pmark))
		(startpos (marker-position comint-last-output-start)))
	   (if (> pmarkpos startpos)	; hilighting to be done
	       ;; Check if patterns exist, and if not, make some
	       (progn
		 (if (assq major-mode hilit-patterns-alist) nil
		   (let ((default-hilits
			   ;; default patterns highlight
			   ;; prompts, inputs and
			   ;; messages
			   (list (list comint-prompt-regexp 
				       nil 
				       'comint-prompt)
				 (list (concat "\\(" comint-prompt-regexp "\\).+")
				       nil
				       'comint-input)
				 (list "^\\*\\*\\*.+\\*\\*\\*" 
				       nil
				       'comint-message))))
		     (hilit-set-mode-patterns major-mode default-hilits)))
		 ;; Highlight from bol
		 (setq startpos (save-excursion
				  (goto-char startpos)
				  (beginning-of-line)
				  (point)))
		 ;; Unhighlight line first, to avoid accumulation of
		 ;; overlays. This would not be necessary if hilit used
		 ;; text properties instead of overlays
		 (hilit-unhighlight-region startpos pmarkpos t)
		 ;; Rehighlight
		 (hilit-highlight-region startpos
					 pmarkpos
					 nil
					 t)))))))

(defun comint-hilit-setup nil
  ;; Place this in comint-mode-hook
  (add-hook 'comint-output-filter-functions
	    'comint-hilit-function))

(add-hook 'comint-mode-hook 'comint-hilit-setup)

(provide 'comint-hilit)

;;; comint-hilit.el ends here
