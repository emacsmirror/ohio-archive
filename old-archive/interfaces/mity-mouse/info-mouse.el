;;!emacs
;;
;; FILE:         info-mouse.el
;; SUMMARY:      Walks through Info networks using one mouse button
;; USAGE:        GNU Emacs Lisp Library
;;
;; AUTHOR:       Bob Weiner, Applied Research, Motorola, Inc., (407)738-2087
;; E-MAIL:       USENET:  weiner@novavax.UUCP
;; ORIG-DATE:    02/04/89
;; LAST-MOD:     12/26/89
;;
;; Copyright (C) 1989 Free Software Foundation, Inc.
;;
;; This file is not yet part of GNU Emacs.
;;
;; Bob Weiner, 03/06/89
;;   Added 'mouse-non-info-command' variable to allow the 'Info-mouse' command
;;   to perform a different function when not called from within the "*info*"
;;   buffer.
;;
;;   Added 'Info-mouse-meta' command.
;;   Added 'mouse-meta-non-info-command' variable to allow the
;;   'Info-mouse-meta' command to perform a different function when not called
;;   from within the "*info*" buffer.
;;
;; Tom Horsley, 12/26/89
;;   Modified to work with my local/global mouse map stuff. It is no longer
;;   a requirement to have funny mode dependent tests, you can have a different
;;   mouse map in info mode than in other modes.
;;
;;   Also added def for last-line-p which is referenced, but not defined
;;   anywhere that I can find.
;;
;;   EMAIL: tahorsley@ssd.csd.harris.com
;;
;; DESCRIPTION:  
;;;
;;; Note this code is machine independent. (kind of) Its only requirement is
;;; that you have a pointer device and an Emacs command that sets point to
;;; the location of the pointing device's cursor.
;;;
;;; Since the changes made by Tom Horsley, it also requires separate local
;;; and global mouse maps. Since that has only been implemented for X, it
;;; has sort of gotten more device specific.
;;;
;;;
;;; To install:
;;;
;;; A pre-requsite is the mighty-mouse.el package which adds support
;;; for local and global mouse maps.
;;;
;;; All you need now is a way to setup the local map when entering info
;;; mode. This is exactly what mode hooks are for, unfortunately info mode
;;; is another one of the criminal modes that does not call a hook, so you
;;; should fix that by going into the emacs/lisp directory and adding some
;;; stuff to info.el
;;;
;;; near the top somewhere add:
;;;
;;; (defvar Info-mode-hook nil
;;;    "List of functions to run on entry to Info-mode.")
;;;
;;; then in the body for the defun Info-mode, just before exiting, add:
;;;
;;;   (run-hooks 'Info-mode-hook)
;;;
;;; This now makes info mode conform to the conventions that call for every
;;; mode to have a hook. This hook is now the place to add a setup of the
;;; local-mouse-map for info mode.
;;;
;;; From this point on, installation becomes more conventional. First add
;;; code to your .emacs file to define the variable mouse-set-point-command
;;; to be the appropriate set point command for your window system. For
;;; instance, under X you would use:
;;;
;;; (setq mouse-set-point-command 'x-mouse-info-set-point)
;;;
;;; Then set the Info-mode-hook to run the Info-mouse-setup-mode function:
;;;
;;; (setq Info-mode-hook (list 'Info-mouse-setup-mode))
;;;
;;; Then make sure emacs can find the Info-mouse-setup-mode function by adding
;;;
;;; (autoload 'Info-mouse-setup-mode "info-setup" nil t)
;;;
;;; This will automatically load the info-setup.el file containing the X
;;; specific mouse setup when info mode is first invoked.
;;;
;;;
;; DESCRIP-END.


(provide 'info-mouse)

(defvar mouse-set-point-command nil
"*Name of command that sets point to mouse cursor position.")

(defvar mouse-non-info-command 'info
"*Name of command to run when button bound to 'Info-mouse' is pressed
outside of the *info* buffer.  Default comand is 'info'.")

(defvar mouse-meta-non-info-command 'info
"*Name of command to run when button bound to 'Info-mouse-meta' is pressed
outside of the *info* buffer.  Default comand is 'info'.")

(defun Info-mouse ()
  "When bound to a mouse button, allows one to move through Info
documentation networks by using only that mouse button.  BE SURE TO CLEAR
ANY BINDING OF THE 'UP' TRANSITION OF THE MOUSE BUTTON THAT YOU BIND
THIS FUNCTION TO.

If button is clicked within:
 (1) any buffer other than *info*, executes command bound to
       'mouse-non-info-command' if bound;
 (2) an Info Menu or Note, the desired node is found;
 (3) the Up,Next,or Previous entries of a Node Header (first line),
       the desired node is found;
 (4) the File entry of a Node Header (first line),       
       the 'Top' node within that file is found;
 (5) the *info* buffer but not within a node reference and not at the
     end of a node, the current node entry is scrolled up one screen
 (6) the *info* buffer at the end of the current node but not within
     a node reference, the Next node is found.

Returns t if button is clicked within an Info Node Header, Note
(cross-reference), or a Menu; otherwise returns nil.  Returns nil if
'mouse-set-point-command' or 'mouse-non-info-command' variables are
not bound to valid functions."

  (interactive)
  (if (not (fboundp mouse-set-point-command))
      nil
    ;;
    ;; Set point to cursor position
    ;;
    (funcall mouse-set-point-command)
    ;;
    ;; Test if in Info buffer already
    ;;
    (if (not (equal (buffer-name (current-buffer)) "*info*"))
        (if (not (fboundp mouse-non-info-command))
               nil
            (funcall mouse-non-info-command))
      (cond ((Info-handle-in-node-hdr))
	    ((Info-handle-in-note))
	    ;;
	    ;; If at end of node, go to next node
            ;;
	    ((last-line-p) (Info-next))
	    ((Info-handle-in-menu))
	    ;;
	    ;; If nothing else scroll forward one screen.
            ;;
	    ((scroll-up))
	    ))))

(defun Info-mouse-meta ()
  "When bound to a meta-mouse button, allows one to move in reverse through Info
documentation networks by using only that mouse button.  Used in conjunction
with the 'Info-mouse' command.   BE SURE TO CLEAR ANY BINDING OF THE 'UP'
TRANSITION OF THE MOUSE BUTTON THAT YOU BIND THIS FUNCTION TO.

If meta-button is clicked within:
 (1) any buffer other than *info*, executes command bound to
       'mouse-meta-non-info-command' if bound;
 (2) an Info Menu or Note, the desired node is found;
 (3) the Up,Next,or Previous entries of a Node Header (first line),
       the last node in the history list is found;
 (4) the File entry of a Node Header (first line),       
       the 'DIR' top-level node is found;
 (5) the *info* buffer but not within a node reference and not at the
     end of a node, the current node entry is scrolled down one screen
 (6) the *info* buffer at the end of the current node but not within
     a node reference, the Previous node is found.

Returns t if button is clicked within an Info Node Header, Note
(cross-reference), or a Menu; otherwise returns nil.  Returns nil if
'mouse-set-point-command' or 'mouse-meta-non-info-command' variables are
not bound to valid functions."

  (interactive)
  (if (not (fboundp mouse-set-point-command))
      nil
    ;;
    ;; Set point to cursor position
    ;;
    (funcall mouse-set-point-command)
    ;;
    ;; Test if in Info buffer already
    ;;
    (if (not (equal (buffer-name (current-buffer)) "*info*"))
        (if (not (fboundp mouse-meta-non-info-command))
               nil
            (funcall mouse-meta-non-info-command))
      (cond ((Info-handle-in-node-hdr-meta))
	    ((Info-handle-in-note))
	    ;;
	    ;; If at end of node, go to previous node
            ;;
	    ((last-line-p) (Info-prev))
	    ((Info-handle-in-menu))
	    ;;
	    ;; If nothing else scroll backward one screen.
            ;;
	    ((scroll-down))
	    ))))

(defun Info-handle-in-node-hdr ()
  "If within an Info node header, move to <FILE>Top, <Up>, <Previous>, or
<Next> node, depending on which label point is on, and return t.
Otherwise, return nil."
  ;;
  ;; Test if on 1st line of node, i.e. node header
  ;;
  (let* ((first-line (1+ (count-lines 1 (point-min))))
	 (current-line (count-lines 1 (1+ (point)))))
    (if (not (equal current-line first-line))
	nil
      (let ((nodename "Top") (filep nil))
	(save-excursion
	  (if (and
		(re-search-forward "[:, \t\n]" nil t)
		(re-search-backward
		  "\\(File\\|Node\\|Up\\|Prev\\|Previous\\|Next\\)[: \t]" nil t))
	      (progn (setq filep (string-equal
				   "file"
				   (downcase (buffer-substring
					       (match-beginning 1)
					       (match-end 1)))))
		     (if (re-search-forward (concat ":[ \n]\\([^,.\t\n"
						    (if filep " ")
						    "]*\\)") nil t)
			 (setq nodename (buffer-substring
					  (match-beginning 1)
					  (match-end 1)))))
	    (error "Node header not found.")))
	(if filep (setq nodename (concat "(" nodename ")" "Top")))
	(Info-goto-node nodename)
	t))))

(defun Info-handle-in-node-hdr-meta ()
  "If within an Info node header when the 'Info-mouse-meta' command is
executed, when within the <FILE> header go to the DIR top-level node.  When
within any other header (<Up>, <Previous>, or <Next>) go to last node from
history list.  Return t if in Info node header.  Otherwise return nil."
  ;;
  ;; Test if on 1st line of node, i.e. node header
  ;;
  (let* ((first-line (1+ (count-lines 1 (point-min))))
	 (current-line (count-lines 1 (1+ (point)))))
    (if (not (equal current-line first-line))
	nil
      (save-excursion
	(if (and 
	      (re-search-forward "[:, \t\n]" nil t)
	      (re-search-backward
		"\\(File\\|Node\\|Up\\|Prev\\|Previous\\|Next\\)[: \t]" nil t) )
	    ;; If in <FILE> hdr
	    (progn (if (string-equal
			 "file"
			 (downcase (buffer-substring
				     (match-beginning 1)
				     (match-end 1))))
		       (Info-directory)
		     (Info-last))
		   t)
	  (error "Node header not found.")
	  nil)))))

(defun Info-handle-in-note ()
  "If point is within an Info note (cross-reference), follow
cross-reference and return t; otherwise return nil."
  ;;
  ;; Test if point is within a Note
  ;;
  ;; Only works if entire "*Note NOTENAME" string is on one line.
  ;; Follows Note if user clicks anywhere on the line.
  ;;
  (let ((note-name nil) (bol nil))
    (save-excursion
      (if (re-search-forward "[:.\n]" nil t)
	  (progn
	    (save-excursion
	      (beginning-of-line)
	      (setq bol (point)))
	    (if (re-search-backward "\\*Note[ \n]+\\([^:]*\\):" bol t)
		(setq note-name (buffer-substring
				  (match-beginning 1)
				  (match-end 1)))))))
    (if (not note-name)
	nil
      (Info-follow-reference note-name)
      t)))

(defun Info-handle-in-menu ()
  "If point is within an Info menu entry, go to node referenced by
entry and return t; otherwise return nil."
  ;;
  ;; Test if there is a menu in this node
  ;;
  (let ((in-menu nil) (curr-point (point)))
    (save-excursion
      (goto-char (point-min))
      (setq in-menu 
	    (and (re-search-forward "^\\* Menu:" nil t)
		 (< (point) curr-point))))
    (if (not in-menu)
	nil
      (forward-char) ; Pass '*' char if point is in front of
      (if (re-search-backward "^\\*" nil t)
	  (progn (forward-char 2)
		 (Info-goto-node (Info-extract-menu-node-name))))
      t)))
    
;; end info-mouse.el

(defun last-line-p ()
"Return t if point is on the last line."
   (save-excursion
      (end-of-line 1)
      (eobp)
   )
)
