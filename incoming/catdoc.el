;;; catdoc.el --- This is a simple wrapper around the catdoc Word to Text converter

;; Copyright (C) 1998 by Free Software Foundation, Inc.

;; Author: Steve Kemp <skx@tardis.ed.ac.uk>
;; Keywords: local, tools, MS Word, conversion, text
;;
;;   This is a simple wrapper to the Word to Text conversion program
;; catdoc.
;;
;;  catdoc is freely available from :
;;
;;   http://www.fe.msk.ru/~vitus/catdoc/
;;
;;  [Much of the create process code was borrowed / inspired by gnustart.el.]
;;
;;  Steve Kemp
;;  Tue May 11 09:59:56 1999
;;
;;;;;;

;; You will almost certainly need to change this
(defvar catdoc-prog "catdoc"
  "The name of the program that will perform the conversion.  This
 will probably be catdoc.exe, but on Unix, or similar the process may have
 another name.")

;;  Any optional arguments you'd like to pass to catdoc on a regular basis
;; should be defined here.
;;
(defvar catdoc-args ""
  "Any additional options to pass to catdoc.")

(defvar catdoc-add-menu-item t
  "Controls whether or not to add a new entry on the file menu, that will allow
 word files to be opened from the GUI.")

(defvar catdoc-process nil
  "The catdoc process that we are going to spawn.")

(defvar catdoc-filename nil
  "The input filename to the catdoc process, this is used later to allow rename
 the output buffer appropriately.")

(defun catdoc-process-filter (proc string)
  "Process the results from the catdoc process."
  (set-buffer (get-buffer-create "*catdoc*"))
  (insert string))

(defun catdoc-sentinel (proc msg)
  "When the catdoc process has finished, switch to its output buffer,
 and rename it appropriately."
  (cond ((eq (process-status proc) 'exit)
	 (switch-to-buffer "*catdoc*")
	 (rename-buffer (concat catdoc-filename ".txt"))
	 )))

(defun open-word-document (file-name )
  "Find, decode and display a specific Microsoft Word document."
  (interactive "fOpen Word Document: \n")
  (setq file-name (expand-file-name file-name))
  (setq catdoc-filename file-name)
  (setq catdoc-process
	(start-process "catdoc" nil catdoc-prog catdoc-args file-name))
  (set-process-filter catdoc-process 'catdoc-process-filter)
  (set-process-sentinel catdoc-process 'catdoc-sentinel)
  (process-kill-without-query catdoc-process)
  )

(if catdoc-add-menu-item
    (define-key menu-bar-file-menu [catdoc-open]
      '("Open Word Document" . open-word-document)))

;;
;;  Allow others to use this package.
;;
(provide 'catdoc)

