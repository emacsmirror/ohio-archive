;; Ebuttons - an X interface to Emacs.
;; Copyright (C) 1992  Terry Jones (terry@santafe.edu)
;; (Based (heavily) on the taglist facility written by Brad Mears)

;; LCD Archive Entry:
;; ebuttons|Terry Jones|terry@nambe.santafe.edu|
;; X11 interface to issue commands to an Emacs session.|
;; 92-11-18||~/interfaces/ebuttons.el.Z|

;; $Header: /tmp_mnt/vida/disks/disk5/Users/terry/s/ebuttons/RCS/ebuttons.el,v 1.2 1992/11/28 22:43:07 terry Exp $

;; This file is part of ebuttons

;; Ebuttons is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; The GNU General Public License can be obtained via anonymous ftp from
;; prep.ai.mit.edu as pub/gnu/COPYING or pub/gnu/COPYING-2.

;; Ebuttons is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; This program provides an X interface to issue commands to an emacs
;; session.  It allows you to specify (in an X resource file) labels for
;; a set of buttons and a command for each that will be executed when the
;; corresponding button is clicked on with the mouse. For instance you
;; can define buttons to compile, to find the next error, to save
;; buffers, to move to the top/bottom of the buffer, to exit emacs etc.
;; etc.

(defvar ebuttons-program "ebuttons"
  "*The ebuttons processor")

(defvar ebuttons-running nil
  "Is the program running?")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  ebuttons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ebuttons ()
  "Start the ebuttons processor.  This starts an X subprocess that can be
used to issue commands to the current emacs session."
  (interactive)
  (if (not ebuttons-running)
    (progn
      (message "Starting ebuttons...")
      (setq ebuttons-process
        (start-process "ebuttons" nil ebuttons-program))
      (setq ebuttons-running t)
      (set-process-filter ebuttons-process 'eb-process-filter)
      (set-process-sentinel ebuttons-process 'eb-process-sentinel)
      (process-kill-without-query ebuttons-process))
    (process-send-string ebuttons-process "\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  eb-process-filter    - input process filter
;;;  eb-process-sentinel  - exit process filter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eb-process-filter (proc str)
  "Process filter for the ebuttons"
  (let ((first-char (string-to-char str)))
     (if (= first-char ?()     
        (eval (read str))       ; evaluate the elisp code sent from ebuttons
        (progn                  ; Dump the string to the ebuttons buffer 
           (get-buffer-create "*ebuttons*")
           (switch-to-buffer-other-window "*ebuttons*")
           (insert-string str)
           (end-of-buffer)
           (other-window 1)))))


(defun eb-process-sentinel (proc msg)
  (setq ebuttons-running nil)		; it's not running any more
  (cond ((eq (process-status proc) 'exit)
	 (message "ebuttons-proc subprocess exited"))
 	((eq (process-status proc) 'signal)
	 (message "ebuttons-proc subprocess killed")
	 ))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  End of ebuttons.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


