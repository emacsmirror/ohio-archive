From gatech!uflorida!novavax!weiner@bbn.com Fri May 19 14:37:14 1989
To: unix-emacs@bbn.com
Date: 28 Apr 89 21:34:06 GMT
From: Bob Weiner <gatech!uflorida!novavax!weiner@bbn.com>
Sender: arpa-unix-emacs-request@bbn.com
Subject: smart-info.el, replacement for GNU Emacs info-mouse.el
Organization: Nova University, Fort Lauderdale, FL
Source-Info:  From (or Sender) name not authenticated.

;;!emacs
;;
;; FILE:         smart-info.el
;; SUMMARY:      Walks through Info networks using one key
;; USAGE:        GNU Emacs Lisp Library, functions called by smart-key.el
;;
;; AUTHOR:       Bob Weiner, Applied Research, Motorola, Inc., (407)738-2087
;; E-MAIL:       USENET:  weiner@novavax.UUCP
;; ORIG-DATE:    04-Apr-89
;; LAST-MOD:     28-Apr-89 at 00:32:34 by Bob Weiner
;;
;; Smart-info package for GNU Emacs.
;; Copyright (C) 1989 Bob Weiner and Free Software Foundation, Inc.
;; Available for use and distribution under the same terms as GNU Emacs.
;;
;; This file is not yet part of GNU Emacs.
;;
;; Bob Weiner, 03/06/89
;;   Added 'smart-info-meta' command.
;;
;; Bob Weiner, 03/06/89
;;   Changed so that if user hits key at the end of or past the end of the
;;   current line, smart-info scrolls up a screen; smart-info-meta scrolls
;;   down.  This does not apply when the last line of a node is clicked on.
;;
;; Bob Weiner, 03/06/89
;;   Changed so that smart-info and smart-info-meta are called from smart-key.el.
;;   Removed mouse-non-info-command and mouse-meta-non-info-command-variables.
;;
;; DESCRIPTION:  
;;;
;;; This code is machine independent.
;;;
;;; To install:
;;;
;;;   See smart-key.el
;;;
;; DESCRIP-END.

(defun smart-info ()
  "Walks through Info documentation networks using one key or mouse key.

If key is pressed within:
 (1) an Info Menu or Note, the desired node is found;
 (2) the Up,Next,or Previous entries of a Node Header (first line),
       the desired node is found;
 (3) the File entry of a Node Header (first line),       
       the 'Top' node within that file is found;
 (4) at the end of the current node, the Next node is found.
 (5) anywhere else (e.g. at the end of a line), the current node entry is
     scrolled up one screen

Returns t if key is pressed within an Info Node Header, Note
(cross-reference), or a Menu; otherwise returns nil."

  (interactive)
  (cond 
    ;;
    ;; If at end of node, go to next node
    ;;
    ((last-line-p) (Info-next))
    ;;
    ;; If at end of line, scroll forward a screen
    ;;
    ((eolp) (scroll-up-eol))
    ((Info-handle-in-node-hdr))
    ((Info-handle-in-note))
    ((Info-handle-in-menu))
    ;;
    ;; If nothing else scroll forward a screen.
    ;;
    ((scroll-up-eol))))

(defun smart-info-meta ()
  "Walks through Info documentation networks using one meta-key or mouse meta-key.

If meta-key is pressed within:
 (1) an Info Menu or Note, the desired node is found;
 (2) the Up,Next,or Previous entries of a Node Header (first line),
       the last node in the history list is found;
 (3) the File entry of a Node Header (first line),       
       the 'DIR' root-level node is found;
 (4) at the end of the current node, the Previous node is found.
 (5) anywhere else (e.g. at the end of a line), the current node entry is
     scrolled down one screen

Returns t if meta-key is pressed within an Info Node Header, Note
(cross-reference), or a Menu; otherwise returns nil."

  (interactive)
  (cond
    ;;
    ;; If at end of node, go to previous node
    ;;
    ((last-line-p) (Info-prev))
    ;;
    ;; If at end of line, scroll backward a screen
    ;;
    ((eolp) (scroll-down-eol))
    ((Info-handle-in-node-hdr-meta))
    ((Info-handle-in-note))
    ((Info-handle-in-menu))
    ;;
    ;; If anywhere else, scroll backward a screen.
    ;;
    ((scroll-down-eol))))

(defun Info-handle-in-node-hdr ()
  "If within an Info node header, move to <FILE>Top, <Up>, <Previous>, or
<Next> node, depending on which label point is on, and return t.
Otherwise, return nil."
  ;;
  ;; Test if on 1st line of node, i.e. node header
  ;;
  (if (not (first-line-p))
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
      t)))

(defun Info-handle-in-node-hdr-meta ()
  "If within an Info node header when the 'smart-info-meta' command is
executed, when within the <FILE> header go to the DIR top-level node.  When
within any other header (<Up>, <Previous>, or <Next>) go to last node from
history list.  Return t if in Info node header.  Otherwise return nil."
  ;;
  ;; Test if on 1st line of node, i.e. node header
  ;;
  (if (not (first-line-p))
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
	nil))))

(defun Info-handle-in-note ()
  "If point is within the first line of an Info note (cross-reference), follow
cross-reference and return t; otherwise return nil."
  (let ((note-name nil) (bol nil))
    (save-excursion
      (if (re-search-forward "[:.\n]" nil t)
	  (progn
	    (save-excursion
	      (beginning-of-line)
	      (setq bol (point)))
	    (if (re-search-backward "\*Note[ \t\n]+\\([^:]*\\):" bol t)
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
	    (and (search-forward "\n* menu:" nil t)
		 (< (point) curr-point))))
    (if (not in-menu)
	nil
      (forward-char) ; Pass '*' char if point is in front of
      (if (search-backward "\n*" nil t)
	  (progn (forward-char 2)
		 (Info-goto-node (Info-extract-menu-node-name))))
      t)))

(provide 'smart-info)
-- 
Bob Weiner, Motorola, Inc.,   USENET:  ...!gatech!uflorida!novavax!weiner
(407) 738-2087


