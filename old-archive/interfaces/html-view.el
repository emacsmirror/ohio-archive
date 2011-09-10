;;; html-view.el
;;;
;;; Some routines for communicating with a NCSA Mosaic process.
;;; 
;;; Copyright (C) 1993 Ron Tapia tapia@hydra.unm.edu
;;;
;;; VERSION: 0.01
;;; LAST MODIFIED: 7/9/93
;;;
;;; LCD Archive Entry:
;;; html-view|Ron Tapia|tapia@hydra.unm.edu|
;;; Some routines for communicating with a NCSA Mosaic process|
;;; 09-Jul-1993|0.01|~/interfaces/html-view.el.Z|
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 1, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to the
;;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;;  
;;; To use, just set the value of html-view-mosaic-command to whatever you
;;; use to run NCSA Mosaic. You may have to set html-view-tmp-file.
;;; Type M-x html-view-start-mosaic <ret>. 
;;; Afterwards, view files/buffers with html-view-view-file/
;;; html-view-view-buffer. There's also a command, of dubious utility,
;;; for jumping to particular documents: html-view-goto-url
;;;
;;; If you have any questions or comments mail to tapia@hydra.unm.edu.

(defvar html-view-mosaic-process nil "The NCSA Mosaic Process")
(defvar html-view-mosaic-command "/unsup/tmp/tapia/xmosaic-dec"  "The command that runs Mosaic on your system")
(defvar html-view-tmp-file (concat "/tmp/mosaic.html-" 
	                           (user-login-name))
	"File where buffers are saved for viewing by Mosaic")
(defvar html-view-display nil "The display that Mosaic is using.")

(defun html-view-start-mosaic ()
  (interactive)
  (or (stringp html-view-display)
      (call-interactively 'html-view-get-display))
  (or (processp html-view-mosaic-process)
      (progn (setq html-view-mosaic-process 
		   (start-process "mosaic" "xmosaic" 
				  html-view-mosaic-command 
				  "-display" html-view-display))
	     (set-process sentinel html-view-mosaic-process
			  'html-view-mosaic-process-sentinel))))

(defun html-view-view-file (filename)
  (interactive "fFile to view: ")
  (or (processp html-view-mosaic-process)
      (html-view-start-mosaic))
  (if (processp html-view-mosaic-process)
      (progn
	(let ((buffer (process-buffer html-view-mosaic-process))
	      (id (process-id html-view-mosaic-process)))
	  (save-excursion
	    (set-buffer buffer)
	    (erase-buffer)
	    (set-visited-file-name (concat "/tmp/xmosaic."
					   (number-to-string id)))
	    (insert-before-markers "goto\n")
	    (insert-before-markers (concat
				    "file://"
				    (expand-file-name filename)))
	    (save-buffer)
	    (signal-process id 30))))
    (message "Can't start mosaic process.")))
	    
(defun html-view-view-buffer (&optional buffer-to-view)
  (interactive)
  (or (bufferp buffer-to-view)
      (setq buffer-to-view (current-buffer)))
  (save-excursion
    (set-buffer buffer-to-view)
    (write-file html-view-tmp-file)
    (html-view-view-file html-view-tmp-file)))

(defun html-view-goto-url (url)
  (interactive "sURL: ")
  (or (processp html-view-mosaic-process)
      (html-view-start-mosaic))
  (if (processp html-view-mosaic-process)
      (progn
	(let ((buffer (process-buffer html-view-mosaic-process))
	      (id (process-id html-view-mosaic-process)))
	  (save-excursion
	    (set-buffer buffer)
	    (erase-buffer)
	    (set-visited-file-name (concat "/tmp/xmosaic."
					   (number-to-string id)))
	    (insert-before-markers "goto\n")
	    (insert-before-markers url)
	    (save-buffer)
	    (signal-process id 30))))
    (message "Can't start mosaic process.")))

(defun html-view-get-display (display)
  (interactive "sDisplay: ")
  (setq html-view-display display))


(defun html-view-mosaic-process-sentinel (proc, event)
  (cond ((or (string-match "exited abnormally with code" event)
	     (string-match "finished"))
	 (message event)
	 (setq html-view-mosaic-process nil))
	(t (message event))))
