;From utkcs2!emory!mephisto!uflorida!novavax!weiner Mon Jul 16 12:22:56 EDT 1990
;Article 4617 of comp.emacs:
;Path: utkcs2!emory!mephisto!uflorida!novavax!weiner
;>From: weiner@novavax.UUCP (Bob &)
;Newsgroups: comp.emacs
;Subject: Re: Problem with rmail (maybe saveconf)
;Summary: em-config.el (posted here) solves the problem
;Message-ID: <WEINER.90Jul13013029@novavax.UUCP>
;Date: 13 Jul 90 05:30:29 GMT
;References: <933@progress.UUCP> <1990Jul9.141448.15783@talos.pm.com>
;Sender: weiner@novavax.UUCP
;Organization: Motorola Inc., Boynton Beach, FL
;Lines: 529
;In-reply-to: kjones@talos.pm.com's message of 9 Jul 90 14:14:48 GMT
;
;OK, for all you people trying to restore your GNU Emacs configuration
;between sessions, here is my most recent version of em-config.el that
;handles rmail and Info buffers properly, tries to save window
;configurations and works superbly with the 'ange-ftp' package, so that
;remote files may also be restored.
;
;Enjoy, and donate to the Free Software Foundation if you can, if you
;like it.  I've actually been using it (aside from one change today) for
;months and it does everything I and most people should need.
;Unfortunately, I've never had time to properly document the internals,
;but the user interface is simplistic.

;;!emacs
;;
;; FILE:         em-config.el
;; SUMMARY:      Save and restore Emacs configurations between sessions.
;; USAGE:        GNU Emacs Lisp Library
;;
;; AUTHOR:       Bob Weiner
;; E-MAIL:       USENET:  weiner@novavax.UUCP
;;
;; ORIG-DATE:    22-Aug-89 at 16:07:48
;; LAST-MOD:     12-Jul-90 at 13:25:52 by Bob Weiner
;;
;; Copyright (C) 1989 Bob Weiner and Free Software Foundation, Inc.
;; Available for use and distribution under the same terms as GNU Emacs.
;;
;; This file is not part of GNU Emacs.
;;
;; DESCRIPTION:  
;;
;;   Currently saves only unmodified buffers visiting files.  Also
;;   saves only limited information about buffers.  It may not work
;;   properly with subsystem buffers that visit files.  It is best to
;;   kill these before saving a configuration.
;;
;;   The exceptions are that:
;;     An Info buffer is handled properly, so it need not be deleted.
;;     Any buffer in rmail-mode is not saved.  Thus, when 'rmail' is
;;     invoked it works properly.
;;
;;   To enable reading of Emacs state from last session at the start
;;   of a new session, put the following in your personal Emacs init file:
;;
;;          (load-library "em-config")
;;          (emc-restore)
;;
;;   Follow the above with this code to enable saving of Emacs state
;;   when quitting a session:
;;
;;         (defun save-buffers-kill-emacs (&optional arg)
;;            "Offer to save each buffer, save file configuration, then kill this Emacs fork.
;;         With prefix arg, silently save all file-visiting buffers, then kill."
;;           (interactive "P")
;;           (save-some-buffers arg t)
;;           (emc-save)
;;           (kill-emacs))
;;
;; DESCRIP-END.

(defconst emc-file "~/.em-config"
  "Default file into which to save Emacs session data.")

(defconst emc-wconfigs-file "~/.em-wconfigs"
  "Default file into which to save Emacs window configuration ring data.")

(defvar emc-start-file nil
  "Last file to read in each time the default Emacs configuration is loaded.
This makes it the first file seen by the user.")

;;; User settings to control data that is saved and restored.  The restore
;;; will fail if any of these values are changed after a 'save' but before a
;;; 'restore'.  Always change values before a 'save'.
;;;
(defconst emc-save-session-data-p t)
(defconst emc-restore-session-data-p nil)
(defconst emc-save-buffer-data-p t)
(defconst emc-save-wconfig-ring-p t)
(defconst emc-save-window-data-p t)

(defun emc-save (&optional file)
  "Save Emacs configuration to optional FILE or 'emc-file'."
  (interactive "FSave Emacs configuration to: ")
  (run-hooks 'emc-save-hook)
  (if (or (equal file "") (null file))
      (setq file emc-file))
  (let ((standard-output (set-buffer (find-file-noselect file)))
	(hdr))
    (erase-buffer)
    (if emc-save-session-data-p
	(progn (print "SESSION DATA")
	       (emc-save-session-data)))
    (if emc-save-buffer-data-p
	(progn (print "BUFFER DATA")
	       (emc-save-buffer-data)))
    (if emc-save-window-data-p
	(progn (set-buffer standard-output)
	       (save-excursion
		 (print "WINDOW DATA")
		 (emc-save-window-data))))
    (if emc-save-wconfig-ring-p
	(progn 
	  ;; Save ring of window configurations ahead of current window config.
	  (print "WINDOW CONFIG RING")
	  (emc-save-wconfig-ring)))
    ;; Add (goto-char (point-max)) here before adding another 'emc-save'
    ;; function call.
    (set-buffer standard-output)
    (save-buffer)
    ))

(defun emc-restore (&optional file)
  "Restore Emacs configuration from optional FILE or 'emc-file'.
Adds buffers to current buffer list.  Returns t if the restore is successful
or the restore file does not exist, otherwise returns a string indicating
the restore failure point."
  (interactive "fRestore Emacs configuration from: ")
  (run-hooks 'emc-restore-hook)
  (let ((hdr t))
    (if (or (equal file "") (null file))
	(setq file emc-file))
    (if (file-exists-p file)
	(let ((standard-input (set-buffer (find-file-noselect emc-file))))
	  (goto-char (point-min))
	  (if (and
		(or (not emc-save-session-data-p)
		    (and (setq hdr "SESSION DATA")
			 (emc-valid-hdr-p hdr)
			 (emc-read-session-data)))
		(or (not emc-restore-session-data-p)
		    (emc-restore-session-data))
		(or (not emc-save-buffer-data-p)
		    (and (setq hdr "BUFFER DATA")
			 (emc-valid-hdr-p hdr)
			 (emc-restore-buffer-data)))
		(if (or (< (screen-height) emc-screen-height)
			(< (screen-width) emc-screen-width))
		    (progn (beep)
			   (message "Skipping window restores, screen size shrunk since saved session.")
			   (sit-for 2)
			   t)
		  ;; This should come before restore of window data.
		  (and (or (not emc-save-wconfig-ring-p)
			   (and (setq hdr "WINDOW CONFIG RING")
				(emc-valid-hdr-p hdr)
				(emc-restore-wconfig-ring)))
		       (or (not emc-save-window-data-p)
			   (and (setq hdr "WINDOW DATA")
				(emc-valid-hdr-p hdr)
				(emc-restore-window-data))))))
	      (progn (kill-buffer standard-input)
		     (and emc-start-file (equal file emc-file)
			  (file-exists-p emc-start-file)
			  (find-file emc-start-file))
		     (setq hdr t)))
	  ))
    hdr))


(defun emc-save-wconfigs (&optional file)
  "Save Emacs window configuration ring to optional FILE or 'emc-wconfigs-file'."
  (interactive "FSave Emacs window configuration ring to: ")
  (run-hooks 'emc-save-hook)
  (if (or (equal file "") (null file))
      (setq file emc-wconfigs-file))
  (let ((standard-output (set-buffer (find-file-noselect file)))
	(hdr))
    (erase-buffer)
    (set-buffer standard-output)
    (print "WINDOW CONFIG RING")
    (emc-save-wconfig-ring)
    ;; Add (goto-char (point-max)) here before adding another 'emc-save'
    ;; function call.
    (set-buffer standard-output)
    (save-buffer)
    ))

(defun emc-restore-wconfigs (&optional file)
  "Restore Emacs window configuration ring from optional FILE or 'emc-wconfigs-file'.
Returns t if the restore is successful or the restore file does not exist,
otherwise returns a string indicating the restore failure point."
  (interactive "fRestore Emacs window configuration ring from: ")
  (run-hooks 'emc-restore-hook)
  (let ((hdr t))
    (if (or (equal file "") (null file))
	(setq file emc-wconfigs-file))
    (if (file-exists-p file)
	(let ((standard-input (set-buffer (find-file-noselect emc-file))))
	  (goto-char (point-min))
	  (if (or (not emc-save-wconfig-ring-p)
		  (and (setq hdr "WINDOW CONFIG RING")
		       (emc-valid-hdr-p hdr)
		       (emc-restore-wconfig-ring)))
	      (progn (kill-buffer standard-input)
		     (setq hdr t))
	    )))
    hdr))


(defun emc-valid-hdr-p (hdr)
  (equal (read) hdr))

(defun buffer-major-mode= (buf mode)
  (eq (cdr (assq 'major-mode (buffer-local-variables buf))) mode))

(defun emc-save-buffer-data ()
  ;; Save only buffers visiting files; skip some in special modes.
  (let ((buf-list (mapcar '(lambda (buf)
			     (let ((bn (buffer-name buf)))
			       (if (and (not (buffer-major-mode= buf 'Info-mode))
					(or (null (buffer-file-name buf))
					    (string-match "^[ \*].*\*$" bn)
					    (buffer-major-mode= buf 'rmail-mode)
					    (equal bn (buffer-name
							standard-output))))
				   nil
				 bn)))
			  (nreverse (buffer-list)))))
    (print buf-list)
    (mapcar '(lambda (buf)
	       (if (null buf)
		   nil
		 (set-buffer buf)
		 (if (eq major-mode 'Info-mode)
		     (progn (print major-mode)
			    (print Info-current-file)
			    (print Info-current-node)
			    (print (point)))
		   (print major-mode)
		   (print (buffer-name))
		   (print (buffer-file-name))
		   (print buffer-read-only)
		   (print (point))
		   ;; t if buffer is narrowed
		   (print (if (or (/= (point-min) 1)
				  (/= (point-max) (1+ (buffer-size)))) t))
		   (print (point-min))
		   (print (point-max))
		   )))
	    buf-list)
    ))

(defun emc-restore-buffer-data ()
  (let ((buf-list (read))
	(buf-name) (file) (mode)
	(buf-read-only) (point)
	(point-min) (point-max)
	(narrowed-p)
	(mark-list))
    (mapcar '(lambda (buf)
	       (if (null buf)
		   nil
		 (setq mode (read))
		 (if (eq mode 'Info-mode)
		     (progn (info)
			    (Info-find-node (read) (read))
			    (goto-char (read)))
		   (setq buf-name (read)
			 file (read)
			 buf-read-only (read)
			 point (read)
			 narrowed-p (read) ;; t if buffer was narrowed
			 point-min (read)
			 point-max (read))
		   (if (or (file-exists-p file)
			   (and (featurep 'ange-ftp)
				(string-match 
				  (if (boundp 'ange-ftp-path-user-exp)
				      ;; Old style
				      (concat "^" ange-ftp-path-user-exp
					      ange-ftp-path-host-exp
					      ange-ftp-path-path-exp)
				      ;; New style
				    (concat "^" (car ange-ftp-path-format)))
				  file)))
		       (progn (find-file file)
			      (or (get-buffer buf-name)
				  (rename-buffer buf-name))
			      (setq buffer-read-only buf-read-only)
			      (and mode (funcall mode))
			      (if (<= point (point-max))
				  (goto-char point))
			      (if (and narrowed-p
				       (or (< point-max (point-max))
					   (> point-min (point-min))))
				  (narrow-to-region point-min point-max)))))))
	    buf-list)
    ;;
    ;; Might want to do stuff and set buffer local variables from src/buffer.c.
    ;;
    )
  t)

;;;
;;; Often won't work properly when used with an external window system.
;;; 
(defun emc-save-session-data ()
  (print (screen-height))
  (print (screen-width))
  )

(defun emc-read-session-data ()
  (setq emc-screen-height (read))
  (setq emc-screen-width (read))
  t)

;; Must only be called after 'emc-read-session-data'.
(defun emc-restore-session-data ()
  (set-screen-height emc-screen-height)
  (set-screen-width emc-screen-width)
  t)

;;;
;;; Should be done after current window configuration is saved but output of
;;; this function should be written in save file AHEAD of the output from
;;; current window configuration.  This allows the current to be the last
;;; config restored, as one would desire.
;;;
(defun emc-save-wconfig-ring ()
  ;; Assumes point is at proper position in standard-output.
  (if (or (not (featurep 'wconfig)) (null wconfig-ring))
      (print 0) ;; No wconfigs stored.
    (let ((wconfig-list (reverse wconfig-ring)))
      (print (length wconfig-list))
      (mapcar
	'(lambda (w)
	   (set-window-configuration w)
	   (emc-save-window-data))
	wconfig-list))))

;;;
;;; Should be done before last current window configuration is restored.
;;;
(defun emc-restore-wconfig-ring ()
  (let ((n (read))) ;; Number of window-configurations saved.
    (if (<= n 0)
	nil
      (if (not (featurep 'wconfig)) (load "wconfig"))
      (setq wconfig-ring nil) ;; Clear out any window configurations.
      (if (> n wconfig-ring-max) (setq wconfig-ring-max n))
      (while (> n 0)
	(setq n (1- n))
	(emc-restore-window-data)
	(wconfig-ring-save))))
  t)


(defun emc-save-window-data ()
  (goto-char (point-max))
  (let ((wind-list (nreverse (emc-window-list 1))))
    (print (length wind-list))
    (mapcar
      '(lambda (w)
	 (select-window w)
	 (let* ((bn (buffer-name))
		(buf (get-buffer bn)))
	   (if (or (string-match "^[ \*].*\*$" bn)
		   (buffer-major-mode= buf 'rmail-mode)
		   (equal bn (buffer-name standard-output))
		   (and (not (buffer-major-mode= buf 'Info-mode))
			(null (buffer-file-name buf))))
	       (print nil)
	     (print (buffer-file-name))
	     (print (buffer-name))
	     (print (window-edges))
	     (print (window-point))
	     (print (window-start))
	     (print (window-hscroll))
	     )))
      wind-list)))

(defun emc-read-window-data ()
  (let* ((num-windows (read))
	 (n num-windows)
	 (wind-list)
	 (buf-name) (file)
	 (window-edges) (window-point) (window-start) (window-hscroll))
    (while (> n 0)
      (setq n (1- n)
	    file (read))
      (if (null file)
	  nil
	(setq buf-name (read))
	(if (file-exists-p file)
	    (progn (setq window-edges (read))
		   (setq window-point (read))
		   (setq window-start (read))
		   (setq window-hscroll (read)))
	  ;; Skip irrelevant data.
	  (read) (read) (read) (read)))
      (setq wind-list (cons (list window-edges file buf-name window-point
				  window-start window-hscroll) wind-list))
      )
    wind-list))


;; WORK IN PROGRESS

;;window-edges:
;;Return a list of the edge coordinates of WINDOW.
;;(LEFT TOP RIGHT BOTTOM), all relative to 0, 0 at top left corner of screen.
;;RIGHT is one more than the rightmost column used by WINDOW,
;;and BOTTOM is one more than the bottommost row used by WINDOW
;; and its mode-line.

(defun emc-calc-window-splits ()
  (let* ((wind-list (emc-read-window-data))
	 (wind-tops) (winds-per-top))
    ;; Compute set of unique window top positions.
    (mapcar '(lambda (w)
	       (set-cons wind-tops (car (cdr (car w))))
	       wind-list))
    ;; Order tops in descending order
    (setq wind-tops (sort wind-tops '>))
    ;; Calc num of windows at each top position (num horiz windows)
    (setq winds-per-top
	  (mapcar '(lambda (wt)
		     (apply '+ (mapcar '(lambda (w)
					  (if (= wt (car (cdr (car w)))) 1 0)
					  wind-list))))
		  wind-tops))
    (let ((wpt (apply '+ winds-per-top))
	  (winds (length wind-list)))
      (if (/= wpt winds)
	  (error "Bug in 'emc-calc-window-splits', wpt = %s, winds = %s" wpt
		 winds))
      (emc-split-windows-vert wind-tops)
      )))


;; Remember windows that share a top dimension, do not have to share a bottom
;; one, so can't always do all vertical splitting before all horizontal.

(defun emc-split-windows-vert (wind-tops)
  (let* ((n (length wind-tops))
	 (i 0)
	 (window-min-height 2)
	 (prev-window-top 0) (top))
    (delete-other-windows)
    (while (< i n)
      (split-window)
      ;; If top edge of window is less than previous, then size
      ;; current window.
      (setq top (nth i wind-tops))
      (if (<= top prev-window-top)
	  (progn (shrink-window (- (window-height)
				   ;; NOT DONE
				   (- (nth 3 window-edges) top)))
		 (other-window 1))
	(let ((owin (selected-window)))
	  (other-window 1)
	  (shrink-window (- (window-height)
			    (- (nth 3 window-edges) top)))
	  (select-window owin)))
      (setq prev-window-top top)
      (setq i (1+ i))
      )))


;;;
;;; Not finished; have to figure out how to set vertical window edges properly.
;;; Therefore does not yet handle side-by-side window splits.
;;;
(defun emc-restore-window-data ()
  (let* ((num-windows (read))
	 (n num-windows)
	 (buf-name) (file) (window-edges)
	 (window-min-height 2)
	 (prev-window-top 0))
    (delete-other-windows)
    (while (> n 0)
      (setq n (1- n)
	    file (read))
      (if (null file)
	  nil
	(setq buf-name (read))
	(if (file-exists-p file)
	    (progn (find-file file)
		   (or (get-buffer buf-name)
		       (rename-buffer buf-name))
		   (setq window-edges (read))
		   (set-window-point (selected-window) (read))
		   (set-window-start (selected-window) (read))
		   (set-window-hscroll (selected-window) (read))
		   ;; Set top and bottom window edges here.
		   (if (= n 0)
		       nil
		     (split-window)
		     ;; If top edge of window is less than previous then size
		     ;; current window.
		     (let ((top (nth 1 window-edges)))
		       (if (> top prev-window-top)
			   (let ((owin (selected-window)))
			     (other-window 1)
			     (shrink-window (- (window-height)
					       (- (nth 3 window-edges) top)))
			     (select-window owin)) 
			 (shrink-window (- (window-height)
					   (- (nth 3 window-edges) top)))
			 (other-window 1))
		       (setq prev-window-top top))))
	  ;; Skip irrelevant data.
	  (read) (read) (read) (read)))
      ))
  t)


;;;
;;; Copyright (C) 1987, 1988 Kyle E. Jones
;;;
(defun emc-window-list (&optional mini)
  "Returns a list of Lisp window objects for all Emacs windows.
Optional first arg MINIBUF t means include the minibuffer window
in the list, even if it is not active.  If MINIBUF is neither t
nor nil it means to not count the minibuffer window even if it is active."
  (let* ((first-window (next-window (previous-window (selected-window)) mini))
	 (windows (cons first-window nil))
	 (current-cons windows)
	 (w (next-window first-window mini)))
    (while (not (eq w first-window))
      (setq current-cons (setcdr current-cons (cons w nil)))
      (setq w (next-window w mini)))
    windows))


(provide 'em-config)
;--
;Bob Weiner	    Usenet:   ...!gatech!uflorida!novavax!weiner
;			      Internet: weiner%novavax@bikini.cis.ufl.edu


