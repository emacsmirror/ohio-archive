;Path: ark1!uakari.primate.wisc.edu!gem.mps.ohio-state.edu!tut.cis.ohio-state.edu!starbase.mitre.org!israel
;From: israel@starbase.mitre.org (Bruce Israel)
;Newsgroups: gnu.emacs.bug
;Subject: Find-file using X menus
;Message-ID: <8911152003.AA19225@starbase>
;Date: 15 Nov 89 20:03:48 GMT
;Sender: daemon@tut.cis.ohio-state.edu
;Distribution: gnu
;Organization: GNUs Not Usenet
;Lines: 74
;
;Here's a useful function I wrote recently.  A friend convinced me that it
;is generally useful and that I should send it out to this list.
;
;It is a function to find a file using menus under X that can be bound to a
;mouse key.  It pops up a menu of the current directory, but if you select
;an entry from that menu that's a directory, it will then pop that up as a
;new menu.  It includes . and .. in the list, so it's possible to move
;around the entire filesystem using this.  It's also possible to select the
;directory itself, and then dired will be run on it.
;
;The main work is in the utility function x-get-filename, which can be
;adapted for other uses extremely easily.
;
;Enjoy.
;
;Bruce

;
; xfile.el - Use menus under X to select a file to visit.
;   Written by - Bruce Israel <israel@starbase.mitre.org>, October 11, 1989
;  
; When the mouse button invoking x-find-file is pressed, a menu of the
; files in the current directory will pop up.  If a file is selected, that
; file is put up on the screen for editing.  If the button is pressed over
; a directory and held, a menu for that directory will be popped up, where
; it is possible to select a file or directory by releasing the mouse
; button.  Through alternatively pressing and releasing the mouse button,
; it is possible to travel throughout the system directories until the
; desired file is found.

(defun x-get-filename (dir arg)
  "Find a file via menus in X.  The selected file is returned.
   DIR is the default directory to begin the search in, and ARG
   is the argument passed in by the X mouse software"
  (let (menu files result key file limit (continue t))
    (while continue
      (setq limit (/ (screen-height) 2)
	    menu (list (cons (format "SELECT %s" dir) (cons 'ACCEPT dir)))
	    files (cons nil (directory-files dir)))
      (while (setq files (cdr files))
	(setq file (car files)
	      key 'ACCEPT)
	(if (file-directory-p (concat dir file))
	    (setq key 'EXPAND
		  file (concat file "/")))
	(if (string-equal file "//") (setq file "/"))
	(setq menu (cons (cons file (cons key file)) menu)))
      (if (> (length menu) limit)
	  (let ((pane nil) (panes nil) (count 0))
	    (setq limit (/ (length menu) (1+ (/ (length menu) limit))))
	    (while menu
	      (setq pane (cons (car menu) pane))
	      (setq menu (cdr menu)
		    count (1+ count))
	      (and (> count limit)
		   (setq panes (cons (cons dir pane) panes))
		   (setq pane nil
			 count 0)))
	    (if pane (setq panes (cons (cons dir pane) panes)))
	    (setq menu panes))
	(setq menu (cons (cons dir (nreverse menu)) nil)))
      (setq result (x-popup-menu arg (cons "find file menu" menu)))
      (setq file (expand-file-name (concat dir (cdr result))))
      (cond ((null result) 		(setq continue nil))
	    ((eq (car result) 'ACCEPT)  (setq continue nil result file))
	    (t 				(setq continue t dir file))))
    result))

(defun x-find-file (arg)
  "Select and edit a file using menus under X."
  (let ((name (x-get-filename default-directory arg)))
    (if name (find-file name))))

(define-key mouse-map x-button-s-right 'x-find-file)
