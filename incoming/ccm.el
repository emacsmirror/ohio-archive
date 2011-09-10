;;; ccm-el               --- Continuus integration.
;;;
;;;
;;; Copyright (C) 2000 Henrik Jönsson.
;;; 
;;; Author:   Henrik Jönsson <henrik7205@hotmail.com>
;;; Site:     http://lightning.prohosting.com/~rig/ccm.shtml
;;; Version:  0.2
;;; Keywords: Continuus, ccm
;;;
;;; Version history
;;; Ver  Sign      Date        Comment
;;; 0.1  henrik    2000-01-26  Created.
;;; 0.2  henrik    2000-02-09  Show version in modeline
;;;                            Show status in modeline
;;;                            Sets the cursor last in the log buffer
;;;                            Show history for a file
;;;                            Set CCM_ADDR inside XEmacs
;;;                            Read only flag correctly set
;;;
;;; Installation:
;;;  (load "ccm-mode")
;;;
;;; This file is not part of XEmacs.
;;;
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
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

(require 'easymenu)

;; Constants
(defconst ccm-int-version-string "0.2" "Version String")
(defconst ccm-int-date-string "2000-02-09" "Date String")
(defconst ccm-int-author-string "Henrik Jönsson" "Author String")

(defvar ccm-modeline-string nil)

;; Functions
(defun ccm-co () 
  "Checks out a file"
  (interactive)
  (get-buffer-create "*ccm*")
  (setq filename (buffer-name))
  (delete-other-windows)
  (switch-to-buffer-other-window "*ccm*")
  (shrink-window 10)
  (other-window -1)
  (insert-string "\nStarting check out...\n" "*ccm*")
  (setq comint-output-filter-functions
	(push 'shell-strip-ctrl-m 
	      comint-output-filter-functions))
  (start-process-shell-command "ccm" "*ccm*" "ccm" " co " filename)
 ;(start-process-shell-command "ccm" "*ccm*" "ccm" " co " filename)
  (set-process-sentinel (get-process "ccm") 'ccm-sentinel-co)
  (set-process-filter (get-process "ccm") 'ccm-process-filter)
)

(defun ccm-ci () 
  "Checks in a file"
  (interactive)
  (get-buffer-create "*ccm*")
  (setq filename (buffer-name))
  (delete-other-windows)
  (switch-to-buffer-other-window "*ccm*")
  (shrink-window 10)
  (other-window -1)
  (insert-string "\nStarting check in...\n" "*ccm*")
  (setq comint-output-filter-functions
	(push 'shell-strip-ctrl-m 
	      comint-output-filter-functions))
  (start-process-shell-command "ccm" "*ccm*" "ccm" " ci -nc" filename)
  (set-process-sentinel (get-process "ccm") 'ccm-sentinel-ci)
)


(defun ccm-update-modeline ()
  (interactive)
  (setq filename (buffer-name))
  (setq ccm-version (exec-to-string 
		     (format "ccm attr -show version %s" filename)))
  (setq ccm-status (exec-to-string 
		    (format "ccm attr -show status %s" filename)))

  (setq ccm-modeline-string (format "%s-%s:%s" filename 
				     ccm-version ccm-status))
  (setq modeline-buffer-identification 'ccm-modeline-string)

  (redraw-modeline)
)

(defun ccm-sentinel (process event)
  (ccm-update-modeline)
  (switch-to-buffer-other-window "*ccm*")
  (goto-char (point-max))
  (dos2unix)
  (redraw-modeline)
)

(defun ccm-sentinel-co (process event)
  (ccm-sentinel process event)
  (insert-string "Check out complete!\n")
  (other-window -1)

  (if buffer-read-only 
      (toggle-read-only))

  ; Ask the user if we should reload the file
  (find-file (buffer-name))
)

(defun ccm-sentinel-ci (process event)
  (ccm-sentinel process event)
  (insert-string "Check in complete!\n")
  (other-window -1)

  (if (not buffer-read-only)
      (toggle-read-only))
)

(defun ccm-process-filter (process output)
  (let ((old-buffer (current-buffer)))
    (unwind-protect
	(let (moving)
	  (set-buffer (process-buffer process))
	  (setq moving (= (point) (process-mark process)))
	  (save-excursion
	    (goto-char (process-mark process))
	    (insert output)
	    (set-marker (process-mark process) (point)))
	  (if moving (goto-char (process-mark process))))
      (set-buffer old-buffer)))
)

(defun dos2unix ()
  "Convert this entire buffer from MS-DOS text file format to UNIX."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\r$" nil t)
      (replace-match "" nil nil))
    (goto-char (1- (point-max)))
    (if (looking-at "\C-z")
        (delete-char 1))))

(defun ccm-history ()
  (interactive)
  (get-buffer-create "*ccm*")
  (setq filename (buffer-name))
  (delete-other-windows)
  (switch-to-buffer-other-window "*ccm*")
  (shrink-window 10)
  (other-window -1)
  (insert-string "\nStarting history...\n" "*ccm*")
  (start-process-shell-command "ccm" "*ccm*" "ccm" " history" filename)
  (set-process-sentinel (get-process "ccm") 'ccm-sentinel)
)

(defun ccm-set-ccmAddr ()
  "Read ccm-add from the user"
  (interactive)
  (setq ccm-addr (read-string "Enter CCM_ADDR: " ""))
  (setenv "CCM_ADDR" ccm-addr)
)

(defun ccm-integration-version ()
  "Display the version of Continuus Integration"
  (interactive)
  (message ccm-int-version-string))

(defun ccm-integration-date ()
  "Display the date of Continuus Integration"
  (interactive)
  (message ccm-int-date-string))

(defun ccm-integration-author ()
  "Display the author of Continuus Integration"
  (interactive)
  (message ccm-int-author-string))

(defun ccm-about ()
  "Displays version, date, author"
  (interactive)
  (message (format "Continuus XEmacs Integration v%s, %s, %s" 
		   ccm-int-version-string 
		   ccm-int-date-string 
		   ccm-int-author-string)))


;; Key Bindings
(defvar ccm-mode-map 
  (let ((ccm-mode-map (make-sparse-keymap)))
    (define-key ccm-mode-map [(control c) (control m) o] 'ccm-co)
    (define-key ccm-mode-map [(control c) (control m) i] 'ccm-ci)
    (define-key ccm-mode-map [(control c) (control m) a] 'ccm-about)
    ccm-mode-map))

; Minor Mode 
(setq ccm-mode nil)
(defun ccm-mode (&optional arg)
  (interactive)
  (setq ccm-mode
	(if (null arg) (not ccm-mode)
	  (> (prefix-numeric-value arg) 0)))

  (setenv "SHELL" "sh")
)

; Menu
;(easy-menu-remove ccm-mode-menu)

(easy-menu-define ccm-mode-menu
    ccm-mode-map
    "Menu used for Continuus Integration"
  (list "Continuus"
	["Check Out File" ccm-co t]
	["Check In File" ccm-ci t]
	["---" nil nil]
	["Update Modeline" ccm-update-modeline t]
	["Show History" ccm-history t]
	["Set CCM_ADDR" ccm-set-ccmAddr t]
	["---" nil nil]
	["About Continuus Mode" ccm-about t]
))

(add-submenu '("Tools") ccm-mode-menu "Continuus")

(if (not (assoc 'ccm-mode minor-mode-alist))
    (setq minor-mode-alist
	  (cons '(ccm-mode " Ccm")
		minor-mode-alist)))

(or (not (boundp 'minor-mode-map-alist))
    (assq 'ccm-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'ccm-mode ccm-mode-map)
		minor-mode-map-alist)))

; invoke the mode
(ccm-mode)

