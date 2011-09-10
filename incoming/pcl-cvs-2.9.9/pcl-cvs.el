;;; pcl-cvs.el -- A Front-end to CVS.

;; Copyright (C) 1991-2000  Free Software Foundation, Inc.

;; Author: (The PCL-CVS Trust) pcl-cvs@cyclic.com
;;	(Per Cederqvist) ceder@lysator.liu.se
;;	(Greg A. Woods) woods@weird.com
;;	(Jim Blandy) jimb@cyclic.com
;;	(Karl Fogel) kfogel@floss.red-bean.com
;;	(Jim Kingdon) kingdon@cyclic.com
;;      (Stefan Monnier) monnier@cs.yale.edu
;;      (Greg Klanderman) greg@alphatech.com
;;      (Jari Aalto+mail.emacs) jari.aalto@poboxes.com
;; Maintainer: (Stefan Monnier) monnier+lists/cvs/pcl@flint.cs.yale.edu
;; Keywords: CVS, version control, release management
;; Version: v2_9_9
;; Revision: pcl-cvs.el,v 1.76 2000/04/15 15:55:08 monnier Exp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; There is an Texinfo file that describes this package.  The GNU
;; General Public License is included in that file.  You should read
;; it to get the most from this package.

;;; Commentary:

;; Up-to-date versions of this package can be found here:
;;
;;	ftp://rum.cs.yale.edu/pub/monnier/pcl-cvs/
;;

;; This package requires ELIB-1.0 to run.  Elib can be found at many
;; places (such as all the GNU mirrors) as well as:
;;
;;	ftp://ftp.lysator.liu.se/pub/emacs/elib-1.0.tar.gz
;;

;; See INSTALL for installation instructions.

;; PCL-CVS requires GNU Emacs 19.28 or newer (or equivalent).

;; This version is tested against CVS-1.9 and is not guaranteed to work with
;; any version older than that.

;; Mail questions, suggestions, complaints, etc., to the PCL-CVS Support Team
;; at <pcl-cvs@cyclic.com>.

;;; Code:

(require 'cookie)			; provided by ELIB-1.0
(eval-when-compile (require 'cl))
(require 'pcl-cvs-defs)
(require 'pcl-cvs-util)
(require 'pcl-cvs-parse)
(require 'pcl-cvs-info)


;;;;
;;;; global vars
;;;;

(defvar cvs-cookies) ;;nil
  ;;"Handle for the cookie structure that is displayed in the *cvs* buffer.")
;;(make-variable-buffer-local 'cvs-cookies)

;;;;
;;;; Dynamically scoped variables
;;;;

(defvar cvs-from-vc nil "Bound to t inside VC advice.")

;;;; 
;;;; flags variables
;;;; 

(defun cvs-defaults (&rest defs)
  (let ((defs (cvs-first defs cvs-shared-start)))
    (append defs
	    (make-list (- cvs-shared-start (length defs)) (first defs))
	    cvs-shared-flags)))

;; For cvs flags, we need to add "-f" to override the cvsrc settings
;; we also want to evict the annoying -q and -Q options that hide useful
;; information from pcl-cvs.
(cvs-flags-define cvs-cvs-flags '(("-f")))

(cvs-flags-define cvs-checkout-flags (cvs-defaults '("-P")))
(cvs-flags-define cvs-status-flags (cvs-defaults '("-v") nil))
(cvs-flags-define cvs-log-flags (cvs-defaults nil))
(cvs-flags-define cvs-diff-flags (cvs-defaults '("-u" "-N") '("-c" "-N")))
(cvs-flags-define cvs-tag-flags (cvs-defaults nil))
(cvs-flags-define cvs-add-flags (cvs-defaults nil))
(cvs-flags-define cvs-commit-flags (cvs-defaults nil))
(cvs-flags-define cvs-remove-flags (cvs-defaults nil))
;;(cvs-flags-define cvs-undo-flags (cvs-defaults nil))
(cvs-flags-define cvs-update-flags (cvs-defaults '("-d" "-P")))

(defun cvs-reread-cvsrc ()
  "Reset the default arguments to those in the `cvs-cvsrc-file'."
  (interactive)
  (let ((cvsrc (cvs-file-to-string cvs-cvsrc-file)))
    (when (stringp cvsrc)
      ;; fetch the values
      (dolist (cmd '("cvs" "checkout" "status" "log" "diff" "tag"
		     "add" "commit" "remove" "update"))
	(let* ((sym (intern (concat "cvs-" cmd "-flags")))
	       (val (when (string-match (concat "^" cmd "\\s-\\(.*\\)$") cvsrc)
		      (cvs-string->strings (match-string 1 cvsrc)))))
	  (cvs-flags-set sym 0 val)))
      ;; ensure that cvs doesn't have -q or -Q
      (cvs-flags-set 'cvs-cvs-flags 0
		     (cons "-f"
			   (cdr (cvs-partition
				 (lambda (x) (member x '("-q" "-Q")))
				 (cvs-flags-query 'cvs-cvs-flags
						  nil 'noquery))))))))

;; initialize to cvsrc's default values
(cvs-reread-cvsrc)


;;;;
;;;; Mouse bindings and mode motion
;;;;

;; necessary to load in the popu-menu macro on Emacs
(eval-and-compile (ignore-errors (require 'lmenu)))

;; FIXME: from JWZ
(defun cvs-menu (e)
  "Popup the CVS menu."
  (interactive "e")
  (mouse-set-point e)
  (popup-menu cvs-menu))

;; small convenient shorthands
(defsubst tin->fileinfo (tin) (tin-cookie cvs-cookies tin))

(defvar cvs-mode-line-process nil
  "Mode-line control for displaying info on cvs process status.")


;;;; 
;;;; Query-Type-Descriptor for Tags
;;;; 

(eval-when-compile (autoload 'cvs-status-get-tags "cvs-status"))
(autoload 'cvs-status-get-tags "cvs-status")

(defun cvs-tags-list ()
  "Return a list of acceptable tags, ready for completions."
  (assert (cvs-buffer-p))
  (let ((marked (cvs-get-marked)))
    (list* '("BASE") '("HEAD")
	   (when marked
	     (with-temp-buffer
	       (call-process cvs-program
			     nil	;no input
			     t		;output to current-buffer
			     nil	;don't update display while running
			     "status"
			     "-v"
			     (cvs-fileinfo->full-path (car marked)))
	       (goto-char (point-min))
	       (let ((tags (cvs-status-get-tags)))
		 (when (listp tags) tags)))))))

(defvar cvs-tag-history nil)
(defconst cvs-qtypedesc-tag
  (cvs-qtypedesc-create 'identity 'identity 'cvs-tags-list 'cvs-tag-history))

;;;; 

(defun cvs-mode! (&optional -cvs-mode!-fun -cvs-mode!-noerror)
  "Switch to the *cvs* buffer.
If -CVS-MODE!-FUN is provided, it is executed *cvs* being the current buffer
  and with its window selected.  Else, the *cvs* buffer is simply selected.
If -CVS-MODE!-NOERROR is non-nil, then failure to find a *cvs* buffer does
  not generate an error and the current buffer is kept selected.
-CVS-MODE!-FUN is called interactively if applicable and else with no argument."
  (let* ((-cvs-mode!-buf (current-buffer))
	 (cvsbuf (cond ((cvs-buffer-p) (current-buffer))
		       ((and cvs-buffer (cvs-buffer-p cvs-buffer)) cvs-buffer)
		       (-cvs-mode!-noerror (current-buffer))
		       (t (error "can't find the *cvs* buffer."))))
	 (-cvs-mode!-wrapper cvs-minor-wrap-function)
	 (-cvs-mode!-cont (lambda ()
			    (save-current-buffer
			      (if (commandp -cvs-mode!-fun)
				  (call-interactively -cvs-mode!-fun)
				(funcall -cvs-mode!-fun))))))
    (if (not -cvs-mode!-fun) (set-buffer cvsbuf)
      (let ((cvs-mode!-buf (current-buffer))
	    (cvs-mode!-owin (selected-window))
	    (cvs-mode!-nwin (get-buffer-window cvsbuf 'visible)))
	(unwind-protect
	    (progn
	      (set-buffer cvsbuf)
	      (when cvs-mode!-nwin (select-window cvs-mode!-nwin))
	      (if -cvs-mode!-wrapper
		  (funcall -cvs-mode!-wrapper -cvs-mode!-buf -cvs-mode!-cont)
		(funcall -cvs-mode!-cont)))
	  (set-buffer cvs-mode!-buf)
	  (when (and cvs-mode!-nwin (eq cvs-mode!-nwin (selected-window)))
	    ;; the selected window has not been changed by FUN
	    (select-window cvs-mode!-owin)))))))

;;;; 
;;;; Prefixes
;;;; 

(defvar cvs-branches (list cvs-vendor-branch "HEAD" "HEAD"))
(cvs-prefix-define cvs-branch-prefix
  "Current selected branch."
  "version"
  (cons cvs-vendor-branch cvs-branches)
  cvs-qtypedesc-tag)

(defun cvs-set-branch-prefix (arg)
  "Set the branch prefix to take action at the next command.
See `cvs-prefix-set' for a further the description of the behavior.
\\[universal-argument] 1 selects the vendor branch
and \\[universal-argument] 2 selects the HEAD."
  (interactive "P")
  (cvs-mode!)
  (cvs-prefix-set 'cvs-branch-prefix arg))

(defun cvs-add-branch-prefix (flags &optional arg)
  "Add branch selection argument if the branch prefix was set.
The argument is added (or not) to the list of FLAGS and is constructed
by appending the branch to ARG which defaults to \"-r\"."
  (let ((branch (cvs-prefix-get 'cvs-branch-prefix)))
    ;; deactivate the secondary prefix, even if not used.
    (cvs-prefix-get 'cvs-secondary-branch-prefix)
    (if branch (cons (concat (or arg "-r") branch) flags) flags)))

(cvs-prefix-define cvs-secondary-branch-prefix
  "Current secondary selected branch."
  "version"
  (cons cvs-vendor-branch cvs-branches)
  cvs-qtypedesc-tag)

(defun cvs-set-secondary-branch-prefix (arg)
  "Set the branch prefix to take action at the next command.
See `cvs-prefix-set' for a further the description of the behavior.
\\[universal-argument] 1 selects the vendor branch
and \\[universal-argument] 2 selects the HEAD."
  (interactive "P")
  (cvs-mode!)
  (cvs-prefix-set 'cvs-secondary-branch-prefix arg))

(defun cvs-add-secondary-branch-prefix (flags &optional arg)
  "Add branch selection argument if the secondary branch prefix was set.
The argument is added (or not) to the list of FLAGS and is constructed
by appending the branch to ARG which defaults to \"-r\".
Since the `cvs-secondary-branch-prefix' is only active if the primary
prefix is active, it is important to read the secondary prefix before
the primay since reading the primary can deactivate it."
  (let ((branch (and (cvs-prefix-get 'cvs-branch-prefix 'read-only)
		     (cvs-prefix-get 'cvs-secondary-branch-prefix))))
    (if branch (cons (concat (or arg "-r") branch) flags) flags)))

;;;; 

(easy-mmode-define-minor-mode
 cvs-minor-mode
 "
This mode is used for buffers related to a main *cvs* buffer.
All the `cvs-mode' buffer operations are simply rebound under
the \\[cvs-mode-map] prefix.
"
 nil " CVS")
(put 'cvs-minor-mode 'permanent-local t)


(defvar cvs-temp-buffers nil)
(defun cvs-temp-buffer (&optional cmd normal nosetup)
  "Create a temporary buffer to run CMD in.
If CMD is a string, use it to lookup `cvs-buffer-name-alist' to find
the buffer name to be used and its `major-mode'.

The selected window will not be changed.  The new buffer will not maintain undo
information and will be read-only unless NORMAL is non-nil.  It will be emptied
\(unless NOSETUP is non-nil\) and its `default-directory' will be inherited
from the current buffer."
  (let* ((cvs-buf (current-buffer))
	 (info (cdr (assoc cmd cvs-buffer-name-alist)))
	 (name (eval (first info)))
	 (mode (second info))
	 (dir default-directory)
	 (buf (cond
	       (name (cvs-get-buffer-create name))
	       ((and (bufferp cvs-temp-buffer) (buffer-name cvs-temp-buffer))
		cvs-temp-buffer)
	       (t
		(set (make-local-variable 'cvs-temp-buffer)
		     (cvs-get-buffer-create
		      (eval cvs-temp-buffer-name) 'noreuse))))))
    
    ;; handle the potential pre-existing process
    (let ((proc (get-buffer-process buf)))
      (when (and (not normal) (processp proc)
		 (memq (process-status proc) '(run stop)))
	(error "Can not run two cvs processes simultaneously")))

    (if (not name) (kill-local-variable 'other-window-scroll-buffer)
      ;; Strangely, if no window is created, `display-buffer' ends up
      ;; doing a `switch-to-buffer' which does a `set-buffer', hence
      ;; the need for `save-excursion'.
      (unless nosetup (save-excursion (display-buffer buf)))
      ;; FIXME: this doesn't do the right thing if the user later on
      ;; does a `find-file-other-window' and `scroll-other-window'
      (set (make-local-variable 'other-window-scroll-buffer) buf))

    (add-to-list 'cvs-temp-buffers buf)

    (with-current-buffer buf
      (setq buffer-read-only nil)
      (setq default-directory dir)
      (unless nosetup (erase-buffer))
      (set (make-local-variable 'cvs-buffer) cvs-buf)
      ;;(cvs-minor-mode 1)
      (let ((lbd list-buffers-directory))
	(if (fboundp mode) (funcall mode) (fundamental-mode))
	(when lbd (set (make-local-variable 'list-buffers-directory) lbd)))
      (cvs-minor-mode 1)
      ;;(set (make-local-variable 'cvs-buffer) cvs-buf)
      (unless normal
	(setq buffer-read-only t)
	(buffer-disable-undo))
      buf)))

(defun cvs-mode-kill-buffers ()
  "Kill all the \"temporary\" buffers created by the *cvs* buffer."
  (interactive)
  (dolist (buf cvs-temp-buffers) (ignore-errors (kill-buffer buf))))

(defun cvs-make-cvs-buffer (dir &optional new)
  "Create the *cvs* buffer for directory DIR.
If non-nil, NEW means to create a new buffer no matter what."
  ;; the real cvs-buffer creation
  (setq dir (cvs-expand-dir-name dir))
  (let* ((buffer-name (eval cvs-buffer-name))
	 (buffer
	  (or (and (not new)
		   (eq cvs-reuse-cvs-buffer 'current)
		   (cvs-buffer-p)	;reuse the current buffer if possible
		   (current-buffer))
	      ;; look for another cvs buffer visiting the same directory
	      (save-excursion
		(unless new
		  (dolist (buffer (cons (current-buffer) (buffer-list)))
		    (set-buffer buffer)
		    (and (cvs-buffer-p)
			 (case cvs-reuse-cvs-buffer
			   (always t)
			   (subdir
			    (or (cvs-string-prefix-p default-directory dir)
				(cvs-string-prefix-p dir default-directory)))
			   (samedir (string= default-directory dir)))
			 (return buffer)))))
	      ;; we really have to create a new buffer:
	      ;; we temporarily bind cwd to "" to prevent
	      ;; create-file-buffer from using directory info
	      ;; unless it is explicitly in the cvs-buffer-name.
	      (cvs-get-buffer-create buffer-name new))))
    (with-current-buffer buffer
      (or
       (and (string= dir default-directory) (cvs-buffer-p)
	    ;; just a refresh
	    (ignore-errors
	      (cvs-cleanup-collection cvs-cookies nil nil t)
	      (current-buffer)))
       ;; setup from scratch
       (progn
	 (setq default-directory dir)
	 (setq buffer-read-only nil)
	 (erase-buffer)
	 (setq buffer-read-only t)
	 (cvs-mode)
	 (set (make-local-variable 'list-buffers-directory) buffer-name)
	 ;;(set (make-local-variable 'cvs-temp-buffer) (cvs-temp-buffer))
	 (let ((cookies
		(collection-create
		 buffer 'cvs-fileinfo-pp
		 (format "%s\n\nRepository       : %s\nWorking directory: %s\n"
			 cvs-startup-message
			 (directory-file-name (cvs-get-cvsroot))
			 dir))))
	   (set (make-local-variable 'cvs-cookies) cookies)
	   (cookie-enter-first
	    cookies
	    (cvs-create-fileinfo 'MESSAGE "" " " "\n" :subtype 'HEADER))
	   (cookie-enter-last
	    cookies
	    (cvs-create-fileinfo 'MESSAGE "" " " "\n" :subtype 'FOOTER))
	   (make-local-hook 'kill-buffer-hook)
	   (add-hook 'kill-buffer-hook
		     (lambda ()
		       (ignore-errors (kill-buffer cvs-temp-buffer)))
		     nil t)
	   ;;(set-buffer buf)
	   buffer))))))

(defun* cvs-cmd-do (cmd dir flags fis new
			&key cvsargs noexist dont-change-disc noshow)
  (let* ((dir (file-name-as-directory
	       (abbreviate-file-name (expand-file-name dir))))
	 (cvsbuf (cvs-make-cvs-buffer dir new)))
    ;; Check that dir is under CVS control.
    (unless (file-directory-p dir)
      (error "%s is not a directory." dir))
    (unless (or noexist (file-directory-p (expand-file-name "CVS" dir)))
      (error "%s does not contain CVS controlled files." dir))

    (set-buffer cvsbuf)
    (cvs-mode-run cmd flags fis
		  :cvsargs cvsargs :dont-change-disc dont-change-disc)

    (if noshow cvsbuf
      (let ((pop-up-windows nil)) (pop-to-buffer cvsbuf)))))
;;      (funcall (if (and (boundp 'pop-up-frames) pop-up-frames)
;;		   'pop-to-buffer 'switch-to-buffer)
;;	       cvsbuf))))

;;----------
(defun cvs-run-process (args fis postprocess &optional single-dir)
  (assert (cvs-buffer-p cvs-buffer))
  (save-current-buffer
    (let ((procbuf (current-buffer))
	  (cvsbuf cvs-buffer)
	  (single-dir (or single-dir (eq cvs-execute-single-dir t))))
      
      (set-buffer procbuf)
      (goto-char (point-max))
      (unless (bolp) (let ((inhibit-read-only t)) (insert "\n")))
      ;; find the set of files we'll process in this round
      (let* ((dir+files+rest
	      (if (or (null fis) (not single-dir))
		  ;; not single-dir mode: just process the whole thing
		  (list "" (mapcar 'cvs-fileinfo->full-path fis) nil)
		;; single-dir mode: extract the same-dir-elements
		(let ((dir (cvs-fileinfo->dir (car fis))))
		  ;; output the concerned dir so the parser can translate paths
		  (let ((inhibit-read-only t))
		    (insert "pcl-cvs: descending directory " dir "\n"))
		  ;; loop to find the same-dir-elems
		  (do* ((files () (cons (cvs-fileinfo->file fi) files))
			(fis fis (cdr fis))
			(fi (car fis) (car fis)))
		      ((not (and fis (string= dir (cvs-fileinfo->dir fi))))
		       (list dir files fis))))))
	     (dir (first dir+files+rest))
	     (files (second dir+files+rest))
	     (rest (third dir+files+rest)))
      
	;; setup the (current) process buffer
	(set (make-local-variable 'cvs-postprocess)
	     (if (null rest)
		 ;; this is the last invocation
		 postprocess
	       ;; else, we have to register ourselves to be rerun on the rest
	       `(cvs-run-process ',args ',rest ',postprocess ',single-dir)))
	(make-local-hook 'kill-buffer-hook)
	(add-hook 'kill-buffer-hook
		  (lambda ()
		    (let ((proc (get-buffer-process (current-buffer))))
		      (when (processp proc)
			(set-process-filter proc nil)
			(set-process-sentinel proc nil)
			(delete-process proc))))
		  nil t)

	;; create the new process and setup the procbuffer correspondingly
	(let* ((args (append (cvs-flags-query 'cvs-cvs-flags nil 'noquery)
			     (if cvs-cvsroot (list "-d" cvs-cvsroot))
			     args
			     files))
	       (process-connection-type nil) ; Use a pipe, not a pty.
	       (process
		;; the process will be run in the selected dir
		(let ((default-directory (cvs-expand-dir-name dir)))
		  (apply 'start-process "cvs" procbuf cvs-program args))))
	  (set-process-sentinel process 'cvs-sentinel)
	  (set-process-filter process 'cvs-update-filter)
	  (set-marker (process-mark process) (point-max))
	  (ignore-errors (process-send-eof process)) ;close its stdin to avoid hangs
	
	  ;; now finish setting up the cvs-buffer
	  (set-buffer cvsbuf)
	  (setq cvs-mode-line-process (symbol-name (process-status process)))
	  (force-mode-line-update)))))

  ;; The following line is said to improve display updates on some
  ;; emacsen. It shouldn't be needed, but it does no harm.
  (sit-for 0))

(defun cvs-update-header (args fis) ; inline
  (let* ((lastarg nil)
	 ;; filter out the largish commit message
	 (args (mapcar (lambda (arg)
			 (cond
			  ((and (eq lastarg nil) (string= arg "commit"))
			   (setq lastarg 'commit) arg)
			  ((and (eq lastarg 'commit) (string= arg "-m"))
			   (setq lastarg '-m) arg)
			  ((eq lastarg '-m)
			   (setq lastarg 'done) "<log message>")
			  (t arg)))
		       args))
	 ;; turn them into a string
	 (arg (cvs-strings->string
	       (append (cvs-flags-query 'cvs-cvs-flags nil 'noquery)
		       (if cvs-cvsroot (list "-d" cvs-cvsroot))
		       args
		       (mapcar 'cvs-fileinfo->full-path fis))))
	 (str (if args (concat "-- Running " cvs-program " " arg " ...\n")
		"\n")))
    (if nil (insert str) ;inline
      ;;(with-current-buffer cvs-buffer
	(let* ((tin0 (tin-nth cvs-cookies 0))
	       (tin-1 (tin-nth cvs-cookies -1))
	       (header (tin->fileinfo tin0))
	       (footer (tin->fileinfo tin-1))
	       (prev-msg (cvs-fileinfo->full-log header))
	       (tin tin0))
	  (assert (and (eq 'HEADER (cvs-fileinfo->subtype header))
		       (eq 'FOOTER (cvs-fileinfo->subtype footer))))
	  ;; look for the first *real* fileinfo (to determine emptyness)
	  (while
	      (and tin
		   (memq (cvs-fileinfo->type (tin->fileinfo tin))
			 '(MESSAGE DIRCHANGE)))
	    (setq tin (tin-next cvs-cookies tin)))
	  ;; cleanup the prev-msg
	  (when (string-match "Running \\(.*\\) ...\n" prev-msg)
	    (setq prev-msg
		  (concat
		   "-- last cmd: "
		   (match-string 1 prev-msg)
		   " --")))
	  ;; set the new header and footer
	  (setf (cvs-fileinfo->full-log header) str)
	  (setf (cvs-fileinfo->full-log footer)
		(concat "\n--------------------- "
			(if tin "End" "Empty")
			" ---------------------\n"
			prev-msg))
	  (tin-invalidate cvs-cookies tin0 tin-1)))));;)


;;----------
(defun cvs-sentinel (proc msg)
  "Sentinel for the cvs update process.
This is responsible for parsing the output from the cvs update when
it is finished."
  (when (memq (process-status proc) '(signal exit))
    (if (null (buffer-name (process-buffer proc)))
	;;(set-process-buffer proc nil)
	(error "cvs' process buffer was killed")
      (let* ((obuf (current-buffer))
	     (procbuffer (process-buffer proc)))
	(set-buffer (with-current-buffer procbuffer cvs-buffer))
	(setq cvs-mode-line-process (symbol-name (process-status proc)))
	(force-mode-line-update)
	(set-buffer procbuffer)
	(let ((cvs-postproc cvs-postprocess))
	  ;; Since the buffer and mode line will show that the
	  ;; process is dead, we can delete it now.  Otherwise it
	  ;; will stay around until M-x list-processes.
	  (delete-process proc)
	  (setq cvs-postprocess nil)
	  ;; do the postprocessing like parsing and such
	  (save-excursion (eval cvs-postproc))
	  ;; check whether something is left
	  (unless cvs-postprocess
	    (buffer-enable-undo)
	    (with-current-buffer cvs-buffer
	      (cvs-update-header nil nil) ;FIXME: might need to be inline
	      (message "CVS process has completed"))))
	;; This might not even be necessary
	(set-buffer obuf)))))

;;----------
(defun cvs-parse-process (dcd &optional subdir)
  "FIXME: bad name, no doc"
  (let* ((from-buf (current-buffer))
	 (fileinfos (cvs-parse-buffer 'cvs-parse-table dcd subdir))
	 (_ (set-buffer cvs-buffer))
	 (from-pt (point)))
    ;; add the new fileinfos
    (dolist (fi fileinfos) (cookie-enter-last cvs-cookies fi))
    ;; merge new info into old fileinfos and cleanup
    (cvs-merge-collection cvs-cookies)
    (cvs-cleanup-collection cvs-cookies
			    (eq cvs-auto-remove-handled t)
			    cvs-auto-remove-directories
			    nil)
    ;; update the display (might be unnecessary)
    (collection-refresh cvs-cookies)
    ;; revert buffers if necessary
    (when (and cvs-auto-revert (not dcd) (not cvs-from-vc))
      (cvs-revert-if-needed fileinfos))
    ;; get back to where we were.  `save-excursion' doesn't seem to
    ;; work in this case, probably because the buffer is reconstructed
    ;; by the cookie code.
    (goto-char from-pt)
    (set-buffer from-buf)))

(defmacro defun-cvs-mode (fun args docstring interact &rest body)
  "Define a function to be used in a *cvs* buffer.
This will look for a *cvs* buffer and execute BODY in it.
Since the interactive arguments might need to be queried after
switching to the *cvs* buffer, the generic code is rather ugly,
but luckily we can often use simpler alternatives.

FUN can be either a symbol (i.e. STYLE is nil) or a cons (FUN . STYLE).
ARGS and DOCSTRING are the normal argument list.
INTERACT is the interactive specification or nil for non-commands.

STYLE can be either SIMPLE, NOARGS or DOUBLE.  It's an error for it
to have any other value, unless other details of the function make it
clear what alternative to use.
- SIMPLE will get all the interactive arguments from the original buffer.
- NOARGS will get all the arguments from the *cvs* buffer and will
  always behave as if called interactively.
- DOUBLE is the generic case."
  (let ((style (cvs-cdr fun))
	(fun (cvs-car fun)))
    (cond
     ;; a trivial interaction, no need to move it
     ((or (eq style 'SIMPLE)
	  (null (second interact))
	  (stringp (second interact)))
      `(defun ,fun ,args ,docstring ,interact
	 (cvs-mode! (lambda () ,@body))))

     ;; fun is only called interactively:  move all the args to the inner fun
     ((eq style 'NOARGS)
      `(defun ,fun () ,docstring (interactive)
	 (cvs-mode! (lambda ,args ,interact ,@body))))

     ;; bad case
     ((eq style 'DOUBLE)
      (string-match ".*" docstring)
      (let ((line1 (match-string 0 docstring))
	    (restdoc (substring docstring (match-end 0)))
	    (fun-1 (intern (concat (symbol-name fun) "-1"))))
	`(progn
	   (defun ,fun-1 ,args
	     ,(concat docstring "\nThis function only works within a *cvs* buffer.
For interactive use, use `" (symbol-name fun) "' instead.")
	     ,interact
	     ,@body)
	   (defun ,fun ()
	     ,(concat line1 "\nWrapper function that switches to a *cvs* buffer
before calling the real function `" (symbol-name fun-1) "'.\n")
	     (interactive)
	     (cvs-mode! ',fun-1)))))

     (t (error "unknown style %s in `defun-cvs-mode'" style)))))
(def-edebug-spec defun-cvs-mode (&define sexp lambda-list stringp ("interactive" interactive) def-body))

(defun-cvs-mode cvs-mode-kill-process ()
  "Kill the temporary buffer and associated process."
  (interactive)
  (when (and (bufferp cvs-temp-buffer) (buffer-name cvs-temp-buffer))
    (let ((proc (get-buffer-process cvs-temp-buffer)))
      (when proc (delete-process proc)))))

;;;
;;; Maintaining the collection in the face of updates
;;;

(defun cvs-addto-collection (c fi &optional tin)
  "Add FI to C and return a tin.
FI is inserted in its proper place or maybe even merged with a preexisting
  fileinfo if applicable.
TIN specifies an optional starting point."
  (unless tin (setq tin (tin-nth c 0)))
  (while (and tin (cvs-fileinfo< fi (tin-cookie c tin)))
    (setq tin (tin-previous c tin)))
  (if (null tin) (progn (cookie-enter-first c fi) nil) ;empty collection
    (assert (not (cvs-fileinfo< fi (tin-cookie c tin))))
    (let ((next-tin (tin-next c tin)))
      (while (not (or (null next-tin)
		      (cvs-fileinfo< fi (tin-cookie c next-tin))))
	(setq tin next-tin next-tin (tin-next c next-tin)))
      (if (cvs-fileinfo< (tin-cookie c tin) fi)
	  ;; tin < fi < next-tin
	  (cookie-enter-after-tin c tin fi)
	;; fi == tin
	(cvs-fileinfo-update (tin-cookie c tin) fi)
	(tin-invalidate c tin))
      tin)))

(defun cvs-merge-collection (c)
  "Merge duplicate entries in collection C."
  (cookie-sort c 'cvs-fileinfo<)
  (let ((last-fi (cvs-create-fileinfo 'DEAD "../" "" ""))) ;place-holder
    (collection-filter-cookies
     c (lambda (fi)
	 ;; check for new overriding info
	 (if (and (not (eq (cvs-fileinfo->type fi) 'MESSAGE))
		  (not (eq (cvs-fileinfo->type last-fi) 'MESSAGE))
		  (string= (cvs-fileinfo->file fi)
			   (cvs-fileinfo->file last-fi))
		  (string= (cvs-fileinfo->dir fi)
			   (cvs-fileinfo->dir last-fi)))
	     ;; fold (and drop) new info into old fileinfos
	     (progn (cvs-fileinfo-update last-fi fi) nil)
	   ;; else keep it
	   (setq last-fi fi))))))

;; called at the following times:
;; - postparse  ((eq cvs-auto-remove-handled t) cvs-auto-remove-directories nil)
;; - pre-run    ((eq cvs-auto-remove-handled 'delayed) nil t)
;; - remove-handled (t (or cvs-auto-remove-directories 'handled) t)
;; - cvs-cmd-do (nil nil t)
;; - post-ignore (nil nil nil)
;; - acknowledge (nil nil nil)
;; - remove     (nil nil nil)
(defun cvs-cleanup-collection (c rm-handled rm-dirs rm-msgs)
  "Remove undesired entries.
C is the collection
RM-HANDLED if non-nil means remove handled entries.
RM-DIRS behaves like `cvs-auto-remove-directories'.
RM-MSGS if non-nil means remove messages."
  (let (last-fi first-dir (rerun t))
    (while rerun
      (setq rerun nil)
      (setq first-dir t)
      (setq last-fi (cvs-create-fileinfo 'DEAD "../" "" "")) ;place-holder
      (collection-filter-cookies
       c (lambda (fi)
	   (let* ((type (cvs-fileinfo->type fi))
		  (subtype (cvs-fileinfo->subtype fi))
		  (keep
		   (case type
		     ;; remove temp messages and keep the others
		     (MESSAGE
		      (or (memq subtype '(HEADER FOOTER))
			  (not (or rm-msgs (eq subtype 'TEMP)))))
		     ;; remove entries
		     (DEAD nil)
		     ;; handled also?
		     (UP-TO-DATE (not rm-handled))
		     ;; keep the rest
		     (t t))))
	     
	     ;; mark dirs for removal
	     (when (and keep rm-dirs
			(eq (cvs-fileinfo->type last-fi) 'DIRCHANGE)
			(not (when first-dir (setq first-dir nil) t))
			(or (eq rm-dirs 'all)
			    (not (cvs-string-prefix-p
				  (cvs-fileinfo->dir last-fi)
				  (cvs-fileinfo->dir fi)))
			    (and (eq type 'DIRCHANGE) (eq rm-dirs 'empty))
			    (eq subtype 'FOOTER)))
	       (setf (cvs-fileinfo->type last-fi) 'DEAD)
	       (setq rerun t))
	     (when keep (setq last-fi fi))))))))

(defun cvs-get-cvsroot ()
  "Gets the CVSROOT for DIR."
  (let ((cvs-cvsroot-file (expand-file-name "Root" "CVS")))
    (or (cvs-file-to-string cvs-cvsroot-file t)
	cvs-cvsroot
	(getenv "CVSROOT")
	"?????")))

(defun cvs-get-module ()
  "Return the current CVS module.
This usually doesn't really work but is a handy initval in a prompt."
  (let* ((repfile (expand-file-name "Repository" "CVS"))
	 (rep (cvs-file-to-string repfile t)))
    (cond
     ((null rep) "")
     ((not (file-name-absolute-p rep)) rep)
     (t
      (let* ((root (cvs-get-cvsroot))
	     (str (concat (file-name-as-directory (or root "/")) " || " rep)))
	(if (and root (string-match "\\(.*\\) || \\1\\(.*\\)\\'" str))
	    (match-string 2 str)
	  (file-name-nondirectory rep)))))))



;;;;
;;;; running a "cvs checkout".
;;;;

;;;###autoload
(defun cvs-checkout (modules dir flags)
  "Run a 'cvs checkout MODULES' in DIR.
Feed the output to a *cvs* buffer, display it in the current window,
and run `cvs-mode' on it.

With a prefix argument, prompt for cvs FLAGS to use."
  (interactive
   (list (cvs-string->strings (read-string "Module(s): " (cvs-get-module)))
	 (read-file-name "CVS Checkout Directory: "
			 nil default-directory nil)
	 (cvs-add-branch-prefix
	  (cvs-flags-query 'cvs-checkout-flags "cvs checkout flags"))))
  (when (eq flags t)
    (setf flags (cvs-flags-query 'cvs-checkout-flags nil 'noquery)))
  (cvs-cmd-do "checkout" (or dir default-directory)
	      (append flags modules) nil 'new
	      :noexist t))


;;;; 
;;;; The code for running a "cvs update" and friends in various ways.
;;;; 

(defun-cvs-mode (cvs-mode-revert-buffer . SIMPLE)
                (&optional ignore-auto noconfirm)
  "Rerun cvs-examine on the current directory with the defauls flags."
  (interactive)
  (cvs-examine default-directory t))

(defun cvs-query-directory (msg)
  ;; last-command-char = ?\r hints that the command was run via M-x
  (if (and (cvs-buffer-p)
	   (not current-prefix-arg)
	   (not (eq last-command-char ?\r)))
      default-directory
    (read-directory-name msg nil default-directory nil)))


;;;###autoload
(defun cvs-examine (directory flags &optional noshow)
  "Run a `cvs -n update' in the specified DIRECTORY.
That is, check what needs to be done, but don't change the disc.
Feed the output to a *cvs* buffer and run `cvs-mode' on it.
With a prefix argument, prompt for a directory and cvs FLAGS to use.
A prefix arg >8 (ex: \\[universal-argument] \\[universal-argument]),
  prevents reuse of an existing *cvs* buffer.
Optional argument NOSHOW if non-nil means not to display the buffer."
  (interactive (list (cvs-query-directory "CVS Examine (directory): ")
		     (cvs-flags-query 'cvs-update-flags "cvs -n update flags")))
  (when (eq flags t)
    (setf flags (cvs-flags-query 'cvs-update-flags nil 'noquery)))
  (cvs-cmd-do "update" directory flags nil
	      (> (prefix-numeric-value current-prefix-arg) 8)
	      :cvsargs '("-n")
	      :noshow noshow
	      :dont-change-disc t))


;;;###autoload
(defun cvs-update (directory flags)
  "Run a `cvs update' in the current working DIRECTORY.
Feed the output to a *cvs* buffer and run `cvs-mode' on it.
With a prefix argument, prompt for a directory and cvs FLAGS to use.
A prefix arg >8 (ex: \\[universal-argument] \\[universal-argument]),
  prevents reuse of an existing *cvs* buffer."
  (interactive (list (cvs-query-directory "CVS Update (directory): ")
		     (cvs-flags-query 'cvs-update-flags "cvs update flags")))
  (when (eq flags t)
    (setf flags (cvs-flags-query 'cvs-update-flags nil 'noquery)))
  (cvs-cmd-do "update" directory flags nil
	      (> (prefix-numeric-value current-prefix-arg) 8)))


;;;###autoload
(defun cvs-status (directory flags &optional noshow)
  "Run a `cvs status' in the current working DIRECTORY.
Feed the output to a *cvs* buffer and run `cvs-mode' on it.
With a prefix argument, prompt for a directory and cvs FLAGS to use.
A prefix arg >8 (ex: \\[universal-argument] \\[universal-argument]),
  prevents reuse of an existing *cvs* buffer.
Optional argument NOSHOW if non-nil means not to display the buffer."
  (interactive (list (cvs-query-directory "CVS Status (directory): ")
		     (cvs-flags-query 'cvs-status-flags "cvs status flags")))
  (when (eq flags t)
    (setf flags (cvs-flags-query 'cvs-status-flags nil 'noquery)))
  (cvs-cmd-do "status" directory flags nil
	      (> (prefix-numeric-value current-prefix-arg) 8)
	      :noshow noshow :dont-change-disc t))

;;----------
(defun cvs-update-filter (proc string)
  "Filter function for pcl-cvs.
This function gets the output that CVS sends to stdout.  It inserts
the STRING into (process-buffer PROC) but it also checks if CVS is waiting
for a lock file.  If so, it inserts a message cookie in the *cvs* buffer."
  (save-match-data
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t))
	(save-excursion
	  ;; Insert the text, moving the process-marker.
	  (goto-char (process-mark proc))
	  (insert string)
	  (set-marker (process-mark proc) (point))
	  ;; FIXME: Delete any old lock message
	  ;;(if (tin-nth cookies 1)
	  ;;  (tin-delete cookies
	  ;;	      (tin-nth cookies 1)))
	  ;; Check if CVS is waiting for a lock.
	  (beginning-of-line 0)		;Move to beginning of last complete line.
	  (when (looking-at "^[ a-z]+: \\(.*waiting for .*lock in \\(.*\\)\\)$")
	    (let ((msg (match-string 1))
		  (lock (match-string 2)))
	      (with-current-buffer cvs-buffer
		(set (make-local-variable 'cvs-lock-file) lock)
		;; display the lock situation in the *cvs* buffer:
		(cookie-enter-last
		 cvs-cookies
		 (cvs-create-fileinfo
		  'MESSAGE "" " "
		  (concat msg
			  (substitute-command-keys 
			   "\n\t(type \\[cvs-mode-delete-lock] to delete it)"))
		  :subtype 'TEMP))
		(pop-to-buffer (current-buffer))
		(goto-char (point-max))
		(beep)))))))))


;;;;
;;;; The cvs-mode and its associated commands.
;;;;

;; This comes from XEmacs and has no effect on Emacs, AFAIK.
(defun pcl-cvs-decor ()
  ;; set up menubar
  (when (and (boundp 'current-menubar) current-menubar
	     (not (assoc "CVS" current-menubar)))
    (set-buffer-menubar (copy-sequence current-menubar))
    (add-menu nil "CVS" (cdr cvs-menu))))

(cvs-prefix-define cvs-force-command "" "" '("/F") cvs-qtypedesc-string1)
(defun-cvs-mode cvs-mode-force-command (arg)
  "Force the next cvs command to operate on all the selected files.
By default, cvs commands only operate on files on which the command
\"makes sense\".  This overrides the safety feature on the next cvs command.
It actually behaves as a toggle.  If prefixed by \\[universal-argument] \\[universal-argument],
the override will persist until the next toggle."
  (interactive "P")
  (cvs-prefix-set 'cvs-force-command arg))

;;----------
(put 'cvs-mode 'mode-class 'special)
(define-derived-mode cvs-mode fundamental-mode "CVS"
  "Mode used for PCL-CVS, a frontend to CVS.
Full documentation is in the Texinfo file.
Pcl-cvs runs `pcl-cvs-load-hook' after being loaded."
  (setq mode-line-process
	'("" cvs-force-command cvs-ignore-marks-modif
	  ":" (cvs-branch-prefix
	       ("" cvs-branch-prefix (cvs-secondary-branch-prefix
				      ("->" cvs-secondary-branch-prefix))))
	  " " cvs-mode-line-process))
  (buffer-disable-undo (current-buffer))
  ;;(set (make-local-variable 'goal-column) cvs-cursor-column)
  (set (make-local-variable 'revert-buffer-function) 'cvs-mode-revert-buffer)
  (cvs-prefix-make-local 'cvs-branch-prefix)
  (cvs-prefix-make-local 'cvs-secondary-branch-prefix)
  (cvs-prefix-make-local 'cvs-force-command)
  (cvs-prefix-make-local 'cvs-ignore-marks-modif)
  (make-local-variable 'cvs-mode-line-process)
  (make-local-variable 'cvs-temp-buffers)
  (pcl-cvs-decor))


(defun cvs-buffer-p (&optional buffer)
  "Return whether the (by default current) BUFFER is a `cvs-mode' buffer."
  (save-excursion
    (if buffer (set-buffer buffer))
    (and (eq major-mode 'cvs-mode))))

(defun cvs-buffer-check ()
  "Check that the current buffer follows cvs-buffer's conventions."
  (let ((buf (current-buffer))
	(check 'none))
    (or (and (setq check 'collection)
	     (eq (collection-buffer cvs-cookies) buf)
	     (setq check 'cvs-temp-buffer)
	     (or (null cvs-temp-buffer)
		 (null (buffer-name cvs-temp-buffer))
		 (and (eq (with-current-buffer cvs-temp-buffer cvs-buffer) buf)
		      (equal (with-current-buffer cvs-temp-buffer
			       default-directory)
			     default-directory)))
	     t)
	(error "Inconsistent %s in buffer %s" check (buffer-name buf)))))


(defun-cvs-mode cvs-mode-quit ()
  "Quit PCL-CVS, killing the *cvs* buffer."
  (interactive)
  (and (y-or-n-p "Quit pcl-cvs? ") (kill-buffer (current-buffer))))

;; Give help....

(defun cvs-help ()
  "Display help for various PCL-CVS commands."
  (interactive)
  (if (eq last-command 'cvs-help)
      (describe-function 'cvs-mode)	; would need to use minor-mode for cvs-edit-mode
    (message
     (substitute-command-keys
      "`\\[cvs-help]':help `\\[cvs-mode-add]':add `\\[cvs-mode-commit]':commit \
`\\[cvs-mode-diff-map]':diff* `\\[cvs-mode-log]':log \
`\\[cvs-mode-remove]':remove `\\[cvs-mode-status]':status \
`\\[cvs-mode-undo]':undo"))))

(defun cvs-mode-diff-help ()
  "Display help for various PCL-CVS diff commands."
  (interactive)
  (if (eq last-command 'cvs-mode-diff-help)
      (describe-function 'cvs-mode)	; no better docs for diff stuff?
    (message
     (substitute-command-keys
      "`\\[cvs-mode-diff]':diff `\\[cvs-mode-idiff]':idiff \
`\\[cvs-mode-diff-head]':head `\\[cvs-mode-diff-vendor]':vendor \
`\\[cvs-mode-diff-backup]':backup `\\[cvs-mode-idiff-other]':other \
`\\[cvs-mode-imerge]':imerge"))))

;; Move around in the buffer

(defun-cvs-mode cvs-mode-previous-line (arg)
  "Go to the previous line.
If a prefix argument is given, move by that many lines."
  (interactive "p")
  (tin-goto-previous cvs-cookies (point) arg))

(defun-cvs-mode cvs-mode-next-line (arg)
  "Go to the next line.
If a prefix argument is given, move by that many lines."
  (interactive "p")
  (tin-goto-next cvs-cookies (point) arg))

;;;; 
;;;; Mark handling
;;;; 

(defun-cvs-mode cvs-mode-mark (&optional arg)
  "Mark the fileinfo on the current line.
If the fileinfo is a directory, all the contents of that directory are
marked instead. A directory can never be marked."
  (interactive)
  (let* ((tin (tin-locate cvs-cookies (point)))
	 (fi (tin->fileinfo tin)))
    (if (eq (cvs-fileinfo->type fi) 'DIRCHANGE)
	;; it's a directory: let's mark all files inside
	(cookie-map
	 (lambda (f dir)
	   (when (cvs-dir-member-p f dir)
	     (setf (cvs-fileinfo->marked f)
		   (not (if (eq arg 'toggle) (cvs-fileinfo->marked f) arg)))
	     t))		;Tell cookie to redisplay this cookie.
	 cvs-cookies
	 (cvs-fileinfo->dir fi))
      ;; not a directory: just do the obvious
      (setf (cvs-fileinfo->marked fi)
	    (not (if (eq arg 'toggle) (cvs-fileinfo->marked fi) arg)))
      (tin-invalidate cvs-cookies tin)
      (cvs-mode-next-line 1))))

(defun cvs-mouse-toggle-mark (e)
  "Toggle the mark of the entry under the mouse."
  (interactive "e")
  (mouse-set-point e)
  (cvs-mode-mark 'toggle))

(defun-cvs-mode cvs-mode-unmark ()
  "Unmark the fileinfo on the current line."
  (interactive)
  (cvs-mode-mark t))

(defun-cvs-mode cvs-mode-mark-all-files ()
  "Mark all files."
  (interactive)
  (cookie-map (lambda (cookie)
		(unless (eq (cvs-fileinfo->type cookie) 'DIRCHANGE)
		  (setf (cvs-fileinfo->marked cookie) t)))
	      cvs-cookies))

(defun-cvs-mode cvs-mode-mark-matching-files (regex)
  "Mark all files matching REGEX."
  (interactive "sMark files matching: ")
  (cookie-map (lambda (cookie)
		(when (and (not (eq (cvs-fileinfo->type cookie) 'DIRCHANGE))
			   (string-match regex (cvs-fileinfo->file cookie)))
		  (setf (cvs-fileinfo->marked cookie) t)))
              cvs-cookies))

(defun-cvs-mode cvs-mode-unmark-all-files ()
  "Unmark all files.
Directories are also unmarked, but that doesn't matter, since
they should always be unmarked."
  (interactive)
  (cookie-map (lambda (cookie)
		(setf (cvs-fileinfo->marked cookie) nil)
		t)
	      cvs-cookies))

(defun-cvs-mode cvs-mode-unmark-up ()
  "Unmark the file on the previous line."
  (interactive)
  (let ((tin (tin-goto-previous cvs-cookies (point) 1)))
    (when tin
      (setf (cvs-fileinfo->marked (tin->fileinfo tin)) nil)
      (tin-invalidate cvs-cookies tin))))

(defconst cvs-ignore-marks-alternatives
  '(("toggle-marks"	. "/TM")
    ("force-marks"	. "/FM")
    ("ignore-marks"	. "/IM")))

(cvs-prefix-define cvs-ignore-marks-modif
  "Prefix to decide whether to ignore marks or not."
  "active"
  (mapcar 'cdr cvs-ignore-marks-alternatives)
  (cvs-qtypedesc-create
   (lambda (str) (cdr (assoc str cvs-ignore-marks-alternatives)))
   (lambda (obj) (caar (member* obj cvs-ignore-marks-alternatives :key 'cdr)))
   (lambda () cvs-ignore-marks-alternatives)
   nil t))
  
(defun-cvs-mode cvs-mode-toggle-marks (arg)
  "Toggle whether the next CVS command uses marks.
See `cvs-prefix-set' for further description of the behavior.
\\[universal-argument] 1 selects `force-marks',
\\[universal-argument] 2 selects `ignore-marks',
\\[universal-argument] 3 selects `toggle-marks'."
  (interactive "P")
  (cvs-prefix-set 'cvs-ignore-marks-modif arg))
      
(defun cvs-ignore-marks-p (cmd &optional read-only)
  (let ((default (if (member cmd cvs-invert-ignore-marks)
		     (not cvs-default-ignore-marks)
		   cvs-default-ignore-marks))
	(modif (cvs-prefix-get 'cvs-ignore-marks-modif read-only)))
    (cond
     ((equal modif "/IM") t)
     ((equal modif "/TM") (not default))
     ((equal modif "/FM") nil)
     (t default))))

(defun cvs-mode-mark-get-modif (cmd)
  (if (cvs-ignore-marks-p cmd 'read-only) "/IM" "/FM"))

(defvar cvs-minor-current-files)
(defun cvs-get-marked (&optional ignore-marks ignore-contents)
  "Return a list of all selected fileinfos.
If there are any marked tins, and IGNORE-MARKS is nil, return them.
Otherwise, if the cursor selects a directory, and IGNORE-CONTENTS is
nil, return all files in it, else return just the directory.
Otherwise return (a list containing) the file the cursor points to, or
an empty list if it doesn't point to a file at all.

Args: &optional IGNORE-MARKS IGNORE-CONTENTS."

  (let ((fis nil))
    (dolist (fi (if (boundp 'cvs-minor-current-files)
		    (mapcar
		     (lambda (f)
		       (let ((f (file-relative-name f)))
			 (if (file-directory-p f)
			     (cvs-create-fileinfo
			      'DIRCHANGE (file-name-as-directory f) "." "")
			   (let ((dir (file-name-directory f))
				 (file (file-name-nondirectory f)))
			     (cvs-create-fileinfo
			      'UNKNOWN (or dir "") file "")))))
		     cvs-minor-current-files)
		  (or (and (not ignore-marks)
			   (collection-collect-cookie cvs-cookies
						      'cvs-fileinfo->marked))
		      (list (tin->fileinfo (tin-locate cvs-cookies (point)))))))
      
      (if (or ignore-contents (not (eq (cvs-fileinfo->type fi) 'DIRCHANGE)))
	  (push fi fis)
	;; If a directory is selected, return members, if any.
	(setq fis
	      (append (collection-collect-cookie cvs-cookies
						 'cvs-dir-member-p
						 (cvs-fileinfo->dir fi))
		      fis))))
    (nreverse fis)))

(defun* cvs-mode-marked (filter &optional (cmd (symbol-name filter))
				&key read-only one file)
  "Get the list of marked FIS.
CMD is used to determine whether to use the marks or not.
Only files for which FILTER is applicable are returned.
If READ-ONLY is non-nil, the current toggling is left intact.
If ONE is non-nil, marks are ignored and a single FI is returned.
If FILE is non-nil, directory entries won't be selected."
  (let* ((fis (cvs-get-marked (or one (cvs-ignore-marks-p cmd read-only))
				  (and (not file)
				       (cvs-applicable-p 'DIRCHANGE filter))))
	 (force (cvs-prefix-get 'cvs-force-command))
	 (fis (car (cvs-partition
		    (lambda (fi) (cvs-applicable-p fi (and (not force) filter)))
		    fis))))
    (cond
     ((null fis)
      (error "`%s' is not applicable to any of the selected files." filter))
     ((and one (cdr fis))
      (error "`%s' is only applicable to a single file." cmd))
     (one (car fis))
     (t fis))))

(defun cvs-enabledp (filter)
  "Determine whether FILTER applies to at least one of the selected files."
  (ignore-errors (cvs-mode-marked filter nil :read-only t)))

(defun cvs-mode-files (&rest -cvs-mode-files-args)
  (cvs-mode!
   (lambda ()
     (mapcar 'cvs-fileinfo->full-path
	     (apply 'cvs-mode-marked -cvs-mode-files-args)))))

;;;
;;; Interface between CVS-Edit and PCL-CVS
;;;

(defun cvs-mode-commit-setup ()
  "Run `cvs-mode-commit' with setup."
  (interactive)
  (cvs-mode-commit 'force))

(defun cvs-mode-commit (setup)
  "Check in all marked files, or the current file.
The user will be asked for a log message in a buffer.
The buffer's mode and name is determined by the \"message\" setting
  of `cvs-buffer-name-alist'.
The POSTPROC specified there (typically `cvs-edit') is then called,
  passing it the SETUP argument."
  (interactive "P")
  ;; It seems that the save-excursion that happens if I use the better
  ;; form of `(cvs-mode! (lambda ...))' screws up a couple things which
  ;; end up being rather annoying (like cvs-edit-mode's message being
  ;; displayed in the wrong minibuffer).
  (cvs-mode!)
  (pop-to-buffer (cvs-temp-buffer "message" 'normal 'nosetup))
  (set (make-local-variable 'cvs-minor-wrap-function) 'cvs-commit-minor-wrap)
  (let ((lbd list-buffers-directory)
	(setupfun (or (third (cdr (assoc "message" cvs-buffer-name-alist)))
		      'cvs-edit)))
    (funcall setupfun 'cvs-do-commit setup 'cvs-commit-filelist)
    (set (make-local-variable 'list-buffers-directory) lbd)))

(defun cvs-commit-minor-wrap (buf f)
  (let ((cvs-ignore-marks-modif (cvs-mode-mark-get-modif "commit")))
    (funcall f)))

(defun cvs-commit-filelist () (cvs-mode-files 'commit nil :read-only t :file t))

(defun cvs-do-commit (flags)
  "Do the actual commit, using the current buffer as the log message."
  (interactive (list (cvs-flags-query 'cvs-commit-flags "cvs commit flags")))
  (let ((msg (buffer-string)))
    (cvs-mode!)
    ;;(pop-to-buffer cvs-buffer)
    (cvs-mode-do "commit" (list* "-m" msg flags) 'commit)))


;;;; 
;;;; CVS Mode commands
;;;; 

(defun-cvs-mode (cvs-mode-insert . NOARGS) (file)
  "Insert an entry for a specific file."
  (interactive
   (list (read-file-name "File to insert: " nil nil nil
			 (ignore-errors
			   (cvs-fileinfo->dir
			    (car (cvs-mode-marked nil nil :read-only t)))))))
  (let ((file (file-relative-name (directory-file-name file))))
    (if (file-directory-p file)
	(let ((fi (cvs-create-fileinfo 'DIRCHANGE
				       (file-name-as-directory file)
				       "."
				       "cvs-mode-insert")))
	  (cvs-addto-collection cvs-cookies fi))
      (let ((fi (cvs-create-fileinfo 'UNKNOWN
				     (or (file-name-directory file) "")
				     (file-name-nondirectory file)
				     "cvs-mode-insert")))
	(cvs-mode-run "status" (cvs-flags-query 'cvs-status-flags nil 'noquery)
		      (list fi) :dont-change-disc t)))))

(defun-cvs-mode (cvs-mode-add . SIMPLE) (flags)
  "Add marked files to the cvs repository.
With prefix argument, prompt for cvs flags."
  (interactive (list (cvs-flags-query 'cvs-add-flags "cvs add flags")))
  (let ((fis (cvs-mode-marked 'add))
	(needdesc nil) (dirs nil))
    ;; find directories and look for fis needing a description
    (dolist (fi fis)
      (cond
       ((file-directory-p (cvs-fileinfo->full-path fi)) (push fi dirs))
       ((eq (cvs-fileinfo->type fi) 'UNKNOWN) (setq needdesc t))))
    ;; prompt for description if necessary
    (let* ((msg (if (and needdesc
			 (or current-prefix-arg (not cvs-add-default-message)))
		    (read-from-minibuffer "Enter description: ")
		  (or cvs-add-default-message "")))
	   (flags (list* "-m" msg flags))
	   (postproc
	    ;; setup postprocessing for the directory entries
	    (when dirs
	      `((cvs-run-process (list "-n" "update")
				 ',dirs
				 '(cvs-parse-process t))
		(dolist (fi ',dirs) (setf (cvs-fileinfo->type fi) 'DEAD))))))
      (cvs-mode-run "add" flags fis :postproc postproc))))

;;----------
(defun-cvs-mode (cvs-mode-diff . DOUBLE) (flags)
  "Diff the selected files against the repository.
This command compares the files in your working area against the
revision which they are based upon."
  (interactive
   (list (cvs-add-branch-prefix
	  (cvs-add-secondary-branch-prefix
	   (cvs-flags-query 'cvs-diff-flags "cvs diff flags")))))
  (cvs-mode-do "diff" flags 'diff
	       :show t)) ;; :ignore-exit t

;;----------
(defun-cvs-mode (cvs-mode-diff-head . SIMPLE) (flags)
  "Diff the selected files against the head of the current branch.
See ``cvs-mode-diff'' for more info."
  (interactive (list (cvs-flags-query 'cvs-diff-flags "cvs diff flags")))
  (cvs-mode-diff-1 (cons "-rHEAD" flags)))

;;----------
(defun-cvs-mode (cvs-mode-diff-vendor . SIMPLE) (flags)
  "Diff the selected files against the head of the vendor branch.
See ``cvs-mode-diff'' for more info."
  (interactive (list (cvs-flags-query 'cvs-diff-flags "cvs diff flags")))
  (cvs-mode-diff-1 (cons (concat "-r" cvs-vendor-branch) flags)))

;;----------
;; sadly, this is not provided by cvs, so we have to roll our own
(defun-cvs-mode (cvs-mode-diff-backup . SIMPLE) (flags)
  "Diff the files against the backup file.
This command can be used on files that are marked with \"Merged\"
or \"Conflict\" in the *cvs* buffer."
  (interactive (list (cvs-flags-query 'cvs-diff-flags "diff flags")))
  (unless (listp flags) (error "flags should be a list of strings."))
  (save-some-buffers)
  (let* ((filter 'diff)
	 (marked (cvs-get-marked (cvs-ignore-marks-p "diff")))
	 ;;(tins (cvs-filter-applicable filter marked))
	 (fis (delete-if-not 'cvs-fileinfo->backup-file marked)))
    (unless (consp fis)
	(error "No files with a backup file selected!"))
    ;; let's extract some info into the environment for `buffer-name'
    (let* ((dir (cvs-fileinfo->dir (car fis)))
	   (file (cvs-fileinfo->file (car fis))))
      (set-buffer (cvs-temp-buffer "diff")))
    (message "cvs diff backup...")
    (cvs-execute-single-file-list fis 'cvs-diff-backup-extractor
				  cvs-diff-program flags))
  (message "cvs diff backup... Done."))

;;----------
;; (defun cvs-backup-diffable-p (fi)
;;   "Check if the TIN is backup-diffable.
;; It must have a backup file to be diffable."
;;   (cvs-fileinfo->backup-file fi))

;;----------
(defun cvs-diff-backup-extractor (fileinfo)
  "Return the filename and the name of the backup file as a list.
Signal an error if there is no backup file."
  (let ((backup-file (cvs-fileinfo->backup-file fileinfo)))
    (unless backup-file
      (error "%s has no backup file." (cvs-fileinfo->full-path fileinfo)))
    (list backup-file (cvs-fileinfo->file fileinfo))))

;;
;; Emerge support
;;
(defun cvs-emerge-diff (b1 b2) (emerge-buffers b1 b2 b1))
(defun cvs-emerge-merge (b1 b2 base out)
  (emerge-buffers-with-ancestor b1 b2 base (find-file-noselect out)))

;;
;; Ediff support
;; 

(defvar ediff-after-quit-destination-buffer)
(defvar cvs-transient-buffers)
(defun cvs-ediff-startup-hook ()
  (add-hook 'ediff-after-quit-hook-internal
	    `(lambda ()
	       (cvs-ediff-exit-hook
		',ediff-after-quit-destination-buffer ',cvs-transient-buffers))
	    nil 'local))

(defun cvs-ediff-exit-hook (cvs-buf tmp-bufs)
  ;; kill the temp buffers (and their associated windows)
  (dolist (tb tmp-bufs)
    (when (and tb (buffer-live-p tb) (not (buffer-modified-p tb)))
      (let ((win (get-buffer-window tb t)))
	(when win (delete-window win))
	(kill-buffer tb))))
  ;; switch back to the *cvs* buffer
  (when (and cvs-buf (buffer-live-p cvs-buf)
	     (not (get-buffer-window cvs-buf t)))
    (ignore-errors (switch-to-buffer cvs-buf))))

(defun cvs-ediff-diff (b1 b2)
  (let ((ediff-after-quit-destination-buffer (current-buffer))
	(startup-hook '(cvs-ediff-startup-hook)))
    (ediff-buffers b1 b2 startup-hook 'ediff-revisions)))

(defun cvs-ediff-merge (b1 b2 base out)
  (let ((ediff-after-quit-destination-buffer (current-buffer))
	(startup-hook '(cvs-ediff-startup-hook)))
    (ediff-merge-buffers-with-ancestor
     b1 b2 base startup-hook
     'ediff-merge-revisions-with-ancestor
     out)))

;;
;; Interactive merge/diff support.
;;

(defun cvs-retrieve-revision (fileinfo rev)
  "Retrieve the given REVision of the file in FILEINFO into a new buffer."
  (save-excursion
    (let* ((file (cvs-fileinfo->full-path fileinfo))
	   (buf (create-file-buffer (concat file "." rev))))
      (set-buffer buf)
      (message "Retrieving revision %s..." rev)
      (let ((res (call-process cvs-program nil t nil
			       "-q" "update" "-p" "-r" rev file)))
	(when (and res (not (and (equal 0 res))))
	  (error "Something went wrong retrieving revision %s: %s" rev res))
	(set-buffer-modified-p nil)
	(let ((buffer-file-name (expand-file-name file)))
	  (after-find-file))
	(toggle-read-only 1)
	(message "Retrieving revision %s... Done" rev)
	buf))))

(eval-and-compile (autoload 'vc-resolve-conflicts "vc"))

(defun-cvs-mode cvs-mode-imerge ()
  "Merge interactively appropriate revisions of the selected file."
  (interactive)
  (let ((fi (cvs-mode-marked 'merge nil :one t :file t)))
    (let ((merge (cvs-fileinfo->merge fi))
	  (file (cvs-fileinfo->full-path fi))
	  (backup-file (cvs-fileinfo->backup-file fi)))
      (if (not (and merge backup-file))
	  (let ((buf (find-file-noselect file)))
	    (message "Missing merge info or backup file, using VC.")
	    (save-excursion
	      (set-buffer buf)
	      (vc-resolve-conflicts)))
	(let* ((ancestor-buf (cvs-retrieve-revision fi (car merge)))
	       (head-buf (cvs-retrieve-revision fi (cdr merge)))
	       (backup-buf (let ((auto-mode-alist nil))
			     (find-file-noselect backup-file)))
	       ;; this binding is used by cvs-ediff-startup-hook
	       (cvs-transient-buffers (list ancestor-buf backup-buf head-buf)))
	  (with-current-buffer backup-buf
	    (let ((buffer-file-name (expand-file-name file)))
	      (after-find-file)))
	  (funcall (cdr cvs-idiff-imerge-handlers)
		   backup-buf head-buf ancestor-buf file))))))

(cvs-flags-define cvs-idiff-version
		  (list "BASE" cvs-vendor-branch cvs-vendor-branch "BASE" "BASE")
		  "version: " cvs-qtypedesc-tag)

(defun-cvs-mode (cvs-mode-idiff . NOARGS) (&optional rev1 rev2)
  "Diff interactively current file to revisions."
  (interactive
   (let* ((rev1 (cvs-prefix-get 'cvs-branch-prefix))
	  (rev2 (and rev1 (cvs-prefix-get 'cvs-secondary-branch-prefix))))
     (list (or rev1 (cvs-flags-query 'cvs-idiff-version))
	   rev2)))
  (let ((fi (cvs-mode-marked 'diff "idiff" :one t :file t)))
    (let* ((file (cvs-fileinfo->full-path fi))
	   (rev1-buf (cvs-retrieve-revision fi (or rev1 "BASE")))
	   (rev2-buf (if rev2 (cvs-retrieve-revision fi rev2)))
	   ;; this binding is used by cvs-ediff-startup-hook
	   (cvs-transient-buffers (list rev1-buf rev2-buf)))
       (funcall (car cvs-idiff-imerge-handlers)
		rev1-buf (or rev2-buf (find-file-noselect file))))))

(defun-cvs-mode (cvs-mode-idiff-other . NOARGS) ()
  "Diff interactively current file to revisions."
  (interactive)
  (let* ((rev1 (cvs-prefix-get 'cvs-branch-prefix))
	 (rev2 (and rev1 (cvs-prefix-get 'cvs-secondary-branch-prefix)))
	 (fis (cvs-mode-marked 'diff "idiff" :file t)))
    (when (> (length fis) 2)
      (error "idiff-other cannot be applied to more than 2 files at a time."))
    (let* ((fi1 (first fis))
	   (rev1-buf (if rev1 (cvs-retrieve-revision fi1 rev1)
		       (find-file-noselect (cvs-fileinfo->full-path fi1))))
	   rev2-buf)
      (if (cdr fis)
	  (let ((fi2 (second fis)))
	    (setq rev2-buf
		  (if rev2 (cvs-retrieve-revision fi2 rev2)
		    (find-file-noselect (cvs-fileinfo->full-path fi2)))))
	(error "idiff-other doesn't know what other file/buffer to use."))
      (let* (;; this binding is used by cvs-ediff-startup-hook
	     (cvs-transient-buffers (list rev1-buf rev2-buf)))
	(funcall (car cvs-idiff-imerge-handlers)
		 rev1-buf rev2-buf)))))


(defun cvs-fileinfo-kill (c fi)
  "Mark a fileinfo xor its members (in case of a directory) as dead."
  (if (eq (cvs-fileinfo->type fi) 'DIRCHANGE)
      (dolist (fi (collection-collect-cookie c 'cvs-dir-member-p
					     (cvs-fileinfo->dir fi)))
	(setf (cvs-fileinfo->type fi) 'DEAD))
    (setf (cvs-fileinfo->type fi) 'DEAD)))

(defun* cvs-mode-run (cmd flags fis
		      &key (buf (cvs-temp-buffer))
		           dont-change-disc cvsargs postproc)
  "Generic cvs-mode-<foo> function.
Executes `cvs CVSARGS CMD FLAGS FIS'.
BUF is the buffer to be used for cvs' output.
DONT-CHANGE-DISC non-nil indicates that the command will not change the
  contents of files.  This is only used by the parser.
POSTPROC is a list of expressions to be evaluated at the very end (after
  parsing if applicable).  It will be prepended with `progn' is necessary."
  (save-some-buffers)
  (unless (listp flags) (error "flags should be a list of strings"))
  (let* ((cvs-buf (current-buffer))
	 (single-dir (or (not (listp cvs-execute-single-dir))
			 (member cmd cvs-execute-single-dir)))
	 (parse (member cmd cvs-parse-known-commands))
	 (args (append cvsargs (list cmd) flags))
	 (after-mode (third (cdr (assoc cmd cvs-buffer-name-alist)))))
    (cvs-cleanup-collection cvs-cookies ;cleanup remaining messages
			    (eq cvs-auto-remove-handled 'delayed) nil t)
    (when (fboundp after-mode)
      (setq postproc (append postproc `((,after-mode)))))
    (when parse (push `(cvs-parse-process ',dont-change-disc) postproc))
    (when (member cmd '("status" "update"))	;FIXME: Yuck!!
      ;; absence of `cvs update' output has a specific meaning.
      (push
       `(dolist (fi ',(or fis
			  (list (cvs-create-fileinfo 'DIRCHANGE "" "." ""))))
	  (cvs-fileinfo-kill ',cvs-cookies fi))
       postproc))
    (setq postproc (if (cdr postproc) (cons 'progn postproc) (car postproc)))
    (cvs-update-header args fis)
    (with-current-buffer buf
      ;;(set (make-local-variable 'cvs-buffer) cvs-buf)
      (let ((inhibit-read-only t)) (erase-buffer))
      (message "Running cvs %s ..." cmd)
      (cvs-run-process args fis postproc single-dir))))


(defun* cvs-mode-do (cmd flags filter
		     &key show dont-change-disc parse cvsargs postproc)
  "Generic cvs-mode-<foo> function.
Executes `cvs CVSARGS CMD FLAGS' on the selected files.
FILTER is passed to `cvs-applicable-p' to only apply the command to
  files for which it makes sense.
SHOW indicates that CMD should be not be run in the default temp buffer and
  should be shown to the user.  The buffer and mode to be used is determined
  by `cvs-buffer-name-alist'.
DONT-CHANGE-DISC non-nil indicates that the command will not change the
  contents of files.  This is only used by the parser."
  (cvs-mode-run cmd flags (cvs-mode-marked filter cmd)
		:buf (cvs-temp-buffer (when show cmd))
		:dont-change-disc dont-change-disc
		:cvsargs cvsargs
		:postproc postproc))

(defun-cvs-mode (cvs-mode-status . SIMPLE) (flags)
  "Show cvs status for all marked files.
With prefix argument, prompt for cvs flags."
  (interactive (list (cvs-flags-query 'cvs-status-flags "cvs status flags")))
  (cvs-mode-do "status" flags nil :dont-change-disc t :show t
	       :postproc (when (eq cvs-auto-remove-handled 'status)
			   '((with-current-buffer ,(current-buffer)
			       (cvs-mode-remove-handled))))))

(defun-cvs-mode (cvs-mode-tree . SIMPLE) (flags)
  "Call cvstree using the file under the point as a keyfile."
  (interactive (list (cvs-flags-query 'cvs-status-flags "cvs status flags")))
  (cvs-mode-run "status" (cons "-v" flags) (cvs-mode-marked nil "status")
		:buf (cvs-temp-buffer "tree")
		:dont-change-disc t
		:postproc '((cvs-status-trees))))

;; cvs log

(defun-cvs-mode (cvs-mode-log . NOARGS) (flags)
  "Display the cvs log of all selected files.
With prefix argument, prompt for cvs flags."
  (interactive (list (cvs-add-branch-prefix
		      (cvs-flags-query 'cvs-log-flags "cvs log flags"))))
  (cvs-mode-do "log" flags nil :show t))


(defun-cvs-mode (cvs-mode-update . NOARGS) (flags)
  "Update all marked files.
With a prefix argument, prompt for cvs flags."
  (interactive
   (list (cvs-add-branch-prefix
	  (cvs-add-secondary-branch-prefix
	   (cvs-flags-query 'cvs-update-flags "cvs update flags")
	   "-j") "-j")))
  (cvs-mode-do "update" flags 'update))


(defun-cvs-mode (cvs-mode-examine . NOARGS) (flags)
  "Re-examine all marked files.
With a prefix argument, prompt for cvs flags."
  (interactive
   (list (cvs-add-branch-prefix
	  (cvs-add-secondary-branch-prefix
	   (cvs-flags-query 'cvs-update-flags "cvs -n update flags")
	   "-j") "-j")))
  (cvs-mode-do "update" flags nil :cvsargs '("-n") :dont-change-disc t))


(defun-cvs-mode cvs-mode-ignore (&optional pattern)
  "Arrange so that CVS ignores the selected files.
This command ignores files that are not flagged as `Unknown'."
  (interactive)
  (dolist (fi (cvs-mode-marked 'ignore))
    (cvs-append-to-ignore (cvs-fileinfo->dir fi) (cvs-fileinfo->file fi))
    (setf (cvs-fileinfo->type fi) 'DEAD))
  (cvs-cleanup-collection cvs-cookies nil nil nil))


(defun cvs-append-to-ignore (dir str)
  "Add STR to the .cvsignore file in DIR."
  (save-window-excursion
    (set-buffer (find-file-noselect (expand-file-name ".cvsignore" dir)))
    (when (ignore-errors
	    (and buffer-read-only
		 (eq 'CVS (vc-backend buffer-file-name))
		 (not (vc-locking-user buffer-file-name))))
      ;; CVSREAD=on special case
      (vc-toggle-read-only))
    (goto-char (point-max))
    (unless (zerop (current-column)) (insert "\n"))
    (insert str "\n")
    (if cvs-sort-ignore-file (sort-lines nil (point-min) (point-max)))
    (save-buffer)))


(defun cvs-mode-find-file-other-window (e)
  "Select a buffer containing the file in another window."
  (interactive (list last-input-event))
  (cvs-mode-find-file e t))


(defun cvs-find-modif (fi)
  (with-temp-buffer
    (call-process cvs-program nil (current-buffer) nil
		  "-f" "diff" (cvs-fileinfo->file fi))
    (goto-char (point-min))
    (if (re-search-forward "^\\([0-9]+\\)" nil t)
	(string-to-number (match-string 1))
      1)))


(defun cvs-mode-find-file (e &optional other)
  "Select a buffer containing the file.
With a prefix, opens the buffer in an OTHER window."
  (interactive (list last-input-event current-prefix-arg))
  (ignore-errors (mouse-set-point e))	;for invocation via the mouse
  (cvs-mode!
   (lambda (&optional rev)
     (interactive (list (cvs-prefix-get 'cvs-branch-prefix)))
     (let* ((cvs-buf (current-buffer))
	    (fi (cvs-mode-marked nil nil :one t)))
       (if (eq (cvs-fileinfo->type fi) 'DIRCHANGE)
	   (let ((odir default-directory))
	     (setq default-directory
		   (cvs-expand-dir-name (cvs-fileinfo->dir fi)))
	     (if other
		 (dired-other-window default-directory)
	       (dired default-directory))
	     (set-buffer cvs-buf)
	     (setq default-directory odir))
	 (let ((buf (if rev (cvs-retrieve-revision fi rev)
		      (find-file-noselect (cvs-fileinfo->full-path fi)))))
	   (funcall (if other 'switch-to-buffer-other-window 'switch-to-buffer)
		    buf)
	   (when (and cvs-find-file-and-jump (cvs-applicable-p fi 'diff-base))
	     (goto-line (cvs-find-modif fi)))
	   buf))))))


(defun-cvs-mode (cvs-mode-undo . SIMPLE) (flags)
  "Undo local changes to all marked files.
The file is removed and `cvs update FILE' is run."
  ;;"With prefix argument, prompt for cvs FLAGS."
  (interactive (list nil));; (cvs-flags-query 'cvs-undo-flags "undo flags")
  (if current-prefix-arg (call-interactively 'cvs-mode-revert-to-rev)
    (let* ((fis (cvs-do-removal 'undo "update" 'all))
	   (removedp (lambda (fi) (eq (cvs-fileinfo->type fi) 'REMOVED)))
	   (fis-split (cvs-partition removedp fis))
	   (fis-removed (car fis-split))
	   (fis-other (cdr fis-split)))
      (if (null fis-other)
	  (when fis-removed (cvs-mode-run "add" nil fis-removed))
	(cvs-mode-run "update" flags fis-other
		      :postproc
		      (when fis-removed
			`((with-current-buffer ,(current-buffer)
			    (cvs-mode-run "add" nil ',fis-removed)))))))))


(defun-cvs-mode (cvs-mode-revert-to-rev . NOARGS) (rev)
  "Revert the selected files to an old revision."
  (interactive
   (list (or (cvs-prefix-get 'cvs-branch-prefix)
	     (let ((current-prefix-arg '(4)))
	       (cvs-flags-query 'cvs-idiff-version)))))
  (let* ((fis (cvs-mode-marked 'revert "revert" :file t))
	 (tag (concat "tmp_pcl_tag_" (make-temp-name "")))
	 (untag `((with-current-buffer ,(current-buffer)
		    (cvs-mode-run "tag" (list "-d" ',tag) ',fis))))
	 (update `((with-current-buffer ,(current-buffer)
		     (cvs-mode-run "update" (list "-j" ',tag "-j" ',rev) ',fis
				   :postproc ',untag)))))
    (cvs-mode-run "tag" (list tag) fis :postproc update)))


(defun-cvs-mode cvs-mode-delete-lock ()
  "Delete the lock file that CVS is waiting for.
Note that this can be dangerous.  You should only do this
if you are convinced that the process that created the lock is dead."
  (interactive)
  (let* ((default-directory (cvs-expand-dir-name cvs-lock-file))
	 (locks (directory-files default-directory nil cvs-lock-file-regexp)))
    (cond
     ((not locks) (error "No lock files found."))
     ((yes-or-no-p (concat "Really delete locks in " cvs-lock-file "? "))
      (dolist (lock locks)
	(cond ((file-directory-p lock) (delete-directory lock))
	      ((file-exists-p lock) (delete-file lock))))))))


(defun-cvs-mode cvs-mode-remove-handled ()
  "Remove all lines that are handled.
Empty directories are removed."
  (interactive)
  (cvs-cleanup-collection cvs-cookies
			  t (or cvs-auto-remove-directories 'handled) t))


(defun-cvs-mode cvs-mode-acknowledge ()
  "Remove all marked files from the buffer."
  (interactive)
  (dolist (fi (cvs-get-marked (cvs-ignore-marks-p "acknowledge") t))
    (setf (cvs-fileinfo->type fi) 'DEAD))
  (cvs-cleanup-collection cvs-cookies nil nil nil))

;;----------
(defun cvs-insert-full-path (tin)
  "Insert full path to the file described in TIN in the current buffer."
  (insert (format "%s\n" (cvs-full-path tin))))

(defun cvs-do-removal (filter &optional cmd all)
  "Remove files.
Returns a list of FIS that should be `cvs remove'd."
  (let* ((files (cvs-mode-marked filter cmd :file t :read-only t))
	 (fis (delete-if (lambda (fi) (eq (cvs-fileinfo->type fi) 'UNKNOWN))
			 (cvs-mode-marked filter cmd)))
	 (silent (or (not cvs-confirm-removals)
		     (cvs-every (lambda (fi)
				  (or (not (file-exists-p
					    (cvs-fileinfo->full-path fi)))
				      (cvs-applicable-p fi 'safe-rm)))
				files))))
    (when (and (not silent) (equal cvs-confirm-removals 'list))
      (save-excursion
	(pop-to-buffer (cvs-temp-buffer))
	(dolist (fi fis)
	  (insert (cvs-fileinfo->full-path fi) "\n"))))
    (if (not (or silent
		 (yes-or-no-p (format "Delete %d files? " (length files)))))
	(progn (message "Aborting") nil)
      (dolist (fi files)
	(let* ((type (cvs-fileinfo->type fi))
	       (file (cvs-fileinfo->full-path fi)))
	  (when (or all (eq type 'UNKNOWN))
	    (when (file-exists-p file) (delete-file file))
	    (unless all (setf (cvs-fileinfo->type fi) 'DEAD) t))))
      fis)))

(defun-cvs-mode (cvs-mode-remove . SIMPLE) (flags)
  "Remove all marked files.
With prefix argument, prompt for cvs flags."
  (interactive (list (cvs-flags-query 'cvs-remove-flags "cvs remove flags")))
  (let ((fis (cvs-do-removal 'remove)))
    (if fis (cvs-mode-run "remove" (cons "-f" flags) fis)
      (cvs-cleanup-collection cvs-cookies nil nil nil))))


(defvar cvs-tag-name "")
(defun-cvs-mode (cvs-mode-tag . SIMPLE) (tag &optional flags)
  "Run `cvs tag TAG' on all selected files.
With prefix argument, prompt for cvs flags."
  (interactive
   (list (setq cvs-tag-name
	       (cvs-query-read cvs-tag-name "Tag name: " cvs-qtypedesc-tag))
	 (cvs-flags-query 'cvs-tag-flags "tag flags")))
  (cvs-mode-do "tag" (append flags (list tag))
	       (when cvs-force-dir-tag 'tag)))

(defun-cvs-mode (cvs-mode-untag . SIMPLE) (tag &optional flags)
  "Run `cvs tag -d TAG' on all selected files.
With prefix argument, prompt for cvs flags."
  (interactive
   (list (setq cvs-tag-name
	       (cvs-query-read cvs-tag-name "Tag to delete: " cvs-qtypedesc-tag))
	 (cvs-flags-query 'cvs-tag-flags "tag flags")))
  (cvs-mode-do "tag" (append '("-d") flags (list tag))
	       (when cvs-force-dir-tag 'tag)))
  

;; Byte compile files.

(defun-cvs-mode cvs-mode-byte-compile-files ()
  "Run byte-compile-file on all selected files that end in '.el'."
  (interactive)
  (let ((marked (cvs-get-marked (cvs-ignore-marks-p "byte-compile"))))
    (dolist (fi marked)
      (let ((filename (cvs-fileinfo->full-path fi)))
	(when (string-match "\\.el\\'" filename)
	  (byte-compile-file filename))))))

;; ChangeLog support.

;;----------
(defun-cvs-mode cvs-mode-add-change-log-entry-other-window ()
  "Add a ChangeLog entry in the ChangeLog of the current directory."
  (interactive)
  (let* ((fi (cvs-mode-marked nil nil :one t))
	 (default-directory (cvs-expand-dir-name (cvs-fileinfo->dir fi)))
	 (buffer-file-name (expand-file-name (cvs-fileinfo->file fi))))
    ;; This `save-excursion' is necessary because of interaction between
    ;; dynamic scoping and buffer-local variables:
    ;; the above binding of `buffer-file-name' has temporarily changed the
    ;; buffer-local variable (same thing for `default-directory'), so we
    ;; need to switch back to the original buffer before the unbinding
    ;; restores the old value.
    (save-excursion (add-change-log-entry-other-window))))

;; interactive commands to set optional flags

(defun cvs-mode-set-flags (flag)
  "Ask for new setting of cvs-FLAG-flags."
  (interactive
   (list (completing-read
	  "Which flag: "
	  (mapcar 'list '("cvs" "diff" "update" "status" "log" "tag" ;"rtag"
			  "commit" "remove" "undo" "checkout"))
	  nil t)))
  (let* ((sym (intern (concat "cvs-" flag "-flags"))))
    (let ((current-prefix-arg '(16)))
      (cvs-flags-query sym (concat flag " flags")))))


;;;;
;;;; Utilities for the *cvs* buffer
;;;;

;;----------
(defun cvs-full-path (tin)
  "Return the full path for the file that is described in TIN."
  (cvs-fileinfo->full-path (tin->fileinfo tin)))

;;----------
(defun cvs-dir-member-p (fileinfo dir)
  "Return true if FILEINFO represents a file in directory DIR."
  (and (not (eq (cvs-fileinfo->type fileinfo) 'DIRCHANGE))
       (cvs-string-prefix-p dir (cvs-fileinfo->dir fileinfo))
       (not (memq (cvs-fileinfo->subtype fileinfo) '(HEADER FOOTER)))))

(defun cvs-execute-single-file (fi extractor program constant-args)
  "Internal function for `cvs-execute-single-file-list'."
  (let* ((cur-dir (cvs-fileinfo->dir fi))
	 (default-directory (cvs-expand-dir-name cur-dir))
	 (inhibit-read-only t)
	 (arg-list (funcall extractor fi)))
    
    ;; Execute the command unless extractor returned t.
    (when (listp arg-list)
      (let* ((args (append constant-args arg-list)))
	
	(insert (format "=== cd %s\n=== %s %s\n\n"
			cur-dir program (cvs-strings->string args)))
	
	;; FIXME: return the exit status?
	(apply 'call-process program nil t t args)
	(goto-char (point-max))))))

;; FIXME: make this run in the background ala cvs-run-process...
(defun cvs-execute-single-file-list (fis extractor program constant-args)
  "Run PROGRAM on all elements on FIS.
The PROGRAM will be called with pwd set to the directory the files
reside in.  CONSTANT-ARGS is a list of strings to pass as arguments to
PROGRAM.  The arguments given to the program will be CONSTANT-ARGS
followed by the list that EXTRACTOR returns.

EXTRACTOR will be called once for each file on FIS.  It is given
one argument, the cvs-fileinfo.  It can return t, which means ignore
this file, or a list of arguments to send to the program."
  (dolist (fi fis)
    (cvs-execute-single-file fi extractor program constant-args)))


(defun cvs-revert-if-needed (fis)
  (dolist (fileinfo fis)
    (let* ((file (cvs-fileinfo->full-path fileinfo))
	   (buffer (find-buffer-visiting file)))
      ;; For a revert to happen the user must be editing the file...
      (unless (or (null buffer)
		  (eq (cvs-fileinfo->type fileinfo) 'MESSAGE)
		  ;; FIXME: check whether revert is really needed.
		  ;; `(verify-visited-file-modtime buffer)' doesn't cut it
		  ;; because it only looks at the time stamp (it ignores
		  ;; read-write changes) which is not changed by `commit'.
		  (buffer-modified-p buffer))
	(with-current-buffer buffer
	  (let ((cvs-buf-was-ro buffer-read-only))
	    (ignore-errors
	      ;; Ideally, we'd like to prevent changing the (minor) modes.
	      ;; But we do want to reset the mode for some cases, most notably
	      ;; VC.  Maybe it'd better to reset VC explicitely ?
	      (revert-buffer 'ignore-auto 'dont-ask)) ; 'preserve-modes
	    ;; protect the buffer-read-only setting
	    (if cvs-buf-was-ro (toggle-read-only 1))))))))



(defun cvs-change-cvsroot (newroot)
  "Change the cvsroot."
  (interactive "DNew repository: ")
  (if (or (file-directory-p (expand-file-name "CVSROOT" newroot))
	  (y-or-n-p (concat "Warning: no CVSROOT found inside repository."
			    " Change cvs-cvsroot anyhow?")))
      (setq cvs-cvsroot newroot)))

;;;;
;;;; useful global settings
;;;;

;;;###autoload
(add-to-list 'completion-ignored-extensions "CVS/")

;;
;; Hook to allow calling PCL-CVS by visiting the /CVS subdirectory
;;

;;;###autoload
(defcustom cvs-dired-action 'cvs-examine
  "The action to be performed when opening a CVS directory.
Sensible values are `cvs-examine' and `cvs-status'."
  :group 'pcl-cvs
  :type '(choice (const cvs-examine) (const cvs-status)))

;;;###autoload
(defcustom cvs-dired-use-hook '(4)
  "Whether or not opening a CVS directory should run PCL-CVS.
NIL means never do it.
ALWAYS means to always do it unless a prefix argument is given to the
  command that prompted the opening of the directory.
Anything else means to do it only if the prefix arg is equal to this value."
  :group 'pcl-cvs
  :type '(choice (const :tag "Never" nil)
		 (const :tag "Always" always)
		 (const :tag "Prefix" (4))))

;;;; hook into dired
;;;###autoload
(defadvice dired-noselect (around pcl-cvs-dired activate)
  (let* ((arg (ad-get-arg 0))
	 (dir (and (stringp arg) (directory-file-name arg))))
    (if (and dir (string= "CVS" (file-name-nondirectory dir))
	     (file-readable-p (expand-file-name "Entries" dir))
	     cvs-dired-use-hook
	     (if (eq cvs-dired-use-hook 'always)
		 (not current-prefix-arg)
	       (equal current-prefix-arg cvs-dired-use-hook)))
	(save-excursion
	  (setq ad-return-value
		(funcall cvs-dired-action (file-name-directory dir) t t)))
      ad-do-it)))

;;
;; hook into VC
;;

(defadvice vc-simple-command (after pcl-cvs-vc activate)
  (cvs-vc-command-advice "*vc-info*" (ad-get-arg 1) (ad-get-arg 3)))

(defadvice vc-do-command (after pcl-cvs-vc activate)
  (cvs-vc-command-advice (or (ad-get-arg 0) "*vc*")
			 (ad-get-arg 2) (ad-get-arg 5)))

(defun cvs-vc-command-advice (buffer command cvscmd)
  (when (and (setq buffer (get-buffer buffer))
	     (equal command "cvs")
	     ;; don't parse output we don't understand.
	     (member cvscmd cvs-parse-known-commands))
    (save-excursion
      (let ((dir (with-current-buffer buffer default-directory))
	    (cvs-from-vc t))
	(dolist (cvs-buf (buffer-list))
	  (set-buffer cvs-buf)
	  ;; look for a corresponding pcl-cvs buffer
	  (when (and (eq major-mode 'cvs-mode)
		     (cvs-string-prefix-p default-directory dir))
	    (let ((subdir (substring dir (length default-directory))))
	      (set-buffer buffer)
	      (set (make-local-variable 'cvs-buffer) cvs-buf)
	      ;; VC never (?) does `cvs -n update' so dcd=nil
	      ;; should probably always be the right choice.
	      (cvs-parse-process nil subdir))))))))

;;
;; Hook into write-buffer
;;

(defun cvs-mark-buffer-changed ()
  (let* ((file (expand-file-name buffer-file-name))
	 (version (and (fboundp 'vc-file-getprop)
		       (eq (vc-file-getprop file 'vc-backend) 'CVS)
		       (vc-workfile-version file))))
    (when version
      (save-excursion
	(dolist (cvs-buf (buffer-list))
	  (set-buffer cvs-buf)
	  ;; look for a corresponding pcl-cvs buffer
	  (when (and (eq major-mode 'cvs-mode)
		     (cvs-string-prefix-p default-directory file))
	    (let* ((file (substring file (length default-directory)))
		   (fi (cvs-create-fileinfo
			(if (string= "0" version)
			    'ADDED 'MODIFIED)
			(or (file-name-directory file) "")
			(file-name-nondirectory file)
			"cvs-mark-buffer-changed")))
	      (cvs-addto-collection cvs-cookies fi))))))))


(add-hook 'after-save-hook 'cvs-mark-buffer-changed)

;;
;; hook into uniquify
;;

(defadvice uniquify-buffer-file-name (after pcl-cvs-uniquify activate)
  (or ad-return-value
      (save-excursion
	(set-buffer (ad-get-arg 0))
	(when (eq major-mode 'cvs-mode)
	  (setq ad-return-value list-buffers-directory)))))


;; provide this package
(run-hooks 'pcl-cvs-load-hook)
(provide 'pcl-cvs)

;;; pcl-cvs.el ends here
