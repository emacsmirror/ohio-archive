;;; jde-stack.el --- Jump to source from Java stack trace
;; Copyright (C) 1999 Phillip Lord <p.lord@hgmp.mrc.ac.uk>

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Status:
;;
;; This mostly works. It should be considered to be a beta release as
;; I havent tested it out heavily yet. If you find this software
;; useful then please tell me, and one day I shall possible be able to
;; call it full release. 
;;
;; The current version should be available at my website. I havent got
;; a URL for this at the moment, so you will have to resort to a web
;; search. You can search on my name if you like, and have endless fun
;; wading through all the christian references. "Guide me O thou Great
;; Redeemer". Altavista gives far too much bread to get to heaven
;; these days...

;;; Commentary:
;; 
;; This package provides the ability to jump from stack traces to the
;; relevant part of the java source. There is one main entry point
;; which is a mouse-1 click on the stack trace, which will jump to the
;; source suggested at this point. This should also work in other
;; buffers (for instance log files), by calling directly the function
;; `jde-stack-show-at-point'. Following this entry point two functions
;; `jde-stack-show-next' and `jde-stack-show-prev' allow cycling
;; backwards and forwards through the stack trace. Error messages are
;; given when the end of the stack is reached. 

;;; Bugs:
;;
;; Its a little bit limited at the moment, in that it parses by a
;; simple regexp. It will also fail totally if a JIT is being
;; used. It also uses the JDE to translate between the unqualified
;; class name, and the full name, even though its given in the stack
;; trace. This is entirely the fault of the JVM which gives stack
;; traces of the form (Blah.java:999). This is so easy to regexp
;; search, that I couldnt turn down the oppurtunity. 

;;; History:
;; 
;; $Log: $

(require 'jde)

;;; Code:
(defvar jde-stack-current-marker nil
  "Stores a marker where the last stack was.")

(defun jde-stack-show-class-stack(class line current-window)
  ;; jde-show-class-source switches automatically to other
  ;; window. Makes sense there, screws things up here. So switching to
  ;; window -1 here makes it switch back to where we started. Arrgh!!!
  (if current-window
      (other-window -1))
  (jde-show-class-source class)
  (set-buffer (concat class ".java" ))
  (goto-line (string-to-number line)))

(defun jde-stack-show-at-mouse (event)
  "Jump to the stack at the mouse click."
  (interactive "e" )
  (save-excursion
    (set-buffer (jde-stack-url-event-buffer event))
    (goto-char (jde-stack-event-point event))
    (jde-stack-show-at-point)))
    
(defun jde-stack-show-at-point()
  "Displays the stack on this current line"
  (interactive)
  (if (not (jde-stack-show-on-line))
      (message "Unable to parse stack on this line")))

(defun jde-stack-show-on-line(&optional current-window)
  "Show the stack trace on this line. If non-nil CURRENT-WINDOW specifies
to show the in the current window."
  (save-excursion
    (let ((window
	   current-window)
	  (line-ending
	   (progn(end-of-line)
		 (point))))
      (beginning-of-line)
      (if (re-search-forward "\\([A-Za-z0-9]*\\)\\([.]java\\)\\(:\\)\\([0-9]*\\)" line-ending 't)
	  (progn
	    (move-marker
	     (jde-stack-get-create-marker)
	     (point))
	    (jde-stack-show-class-stack (match-string 1) (match-string 4) window))))))

;;stolen from browse-url. I have no idea what they do. Perhaps they
;;are useless, who can tell???
(defun jde-stack-url-event-buffer (event)
  (window-buffer (posn-window (event-start event))))

(defun jde-stack-event-point (event)
  (posn-point (event-start event)))

(defun jde-stack-get-create-marker()
  (if jde-stack-current-marker
      jde-stack-current-marker
    (setq jde-stack-current-marker (make-marker))))

(defun jde-stack-point-to-marker()
  (let ((marker (jde-stack-get-create-marker)))
    (if (not marker)
	(error "No stack at point")
      (set-buffer (marker-buffer marker))
      (goto-char (marker-position marker)))))

(defun jde-stack-next()
  "Shows the source next in the stack trace"
  (interactive)
  (jde-stack-point-to-marker)
  (forward-line 1)
  (if(not (jde-stack-show-on-line 't))
     (message "The end of the stack has been reached" )))

(defun jde-stack-prev()
  "Shows the source previous in the stack trace"
  (interactive)
  (jde-stack-point-to-marker)
  (forward-line -1)
  (if(not (jde-stack-show-on-line 't))
     (message "The start of the stack has been reached" )))

;;The jde defines a unique mode extended from comint for running
;;processes. Its fairly much identical to comint. Its quite handy
;;here.
(add-hook 'jde-run-mode-hook
	  '(lambda()
	     (define-key (current-local-map) "\C-c\C-v\C-[" 'jde-stack-prev)
	     (define-key (current-local-map) "\C-c\C-v\C-]" 'jde-stack-next)
	     (define-key (current-local-map) [mouse-1] 'jde-stack-show-at-mouse)))

(define-key jde-mode-map "\C-c\C-v\C-[" 'jde-stack-prev)
(define-key jde-mode-map "\C-c\C-v\C-]" 'jde-stack-next)

(provide 'jde-stack)
;;; jde-stack.el ends here
