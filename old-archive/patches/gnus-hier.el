; Path: hal.com!olivea!uunet!cis.ohio-state.edu!east-wind.ORG!kingdon
; From: kingdon@east-wind.ORG (Jim Kingdon)
; Newsgroups: gnu.emacs.sources
; Subject: Hierarchical patches for GNUS 3.14.1
; Date: 23 Nov 92 06:33:59 GMT
; Organization: Source only  Discussion and requests in gnu.emacs.help.
; 
; Enclosed are patches to make GNUS a hierarchical newsreader.  What I
; mean by that is that when you start up GNUS for the first time, rather
; than showing you all the newsgroups, it will just show one line for
; each top-level hierarchy ("soc", "rec", "alt", etc.).  You can then
; unsubscribe to the whole hierarchy with one keystroke, or you can type
; space to "explode" the hierarchy into all the second-level groups and
; hierarchies (e.g. "comp" will explode into "comp.sys", "comp.os",
; "comp.compilers", etc.).
; 
; These patches should be considered experimental, both in the sense
; that some features probably should be added (but I didn't get around
; to it, or wasn't sure just how they should work), and that it has not
; been extensively tested for bugs.

; LCD Archive Entry:
; gnus-hier|Jim Kingdon|kingdon@east-wind.ORG|
; Patches to make GNUS a hierarchical newsreader.|
; 92-11-21||~/patches/gnus-hier.el.Z|

; 
; *** gnus.el	Sat Nov 21 14:23:58 1992
; --- gnus-hier.el	Sat Nov 21 18:43:30 1992
; ***************
; *** 224,229 ****
; --- 224,250 ----
  (defvar gnus-show-threads t
    "*Show conversation threads in Subject Mode if non-nil.")
  
+ ;;; Notes on hierarchy support:
+ ;;; * Right now, a hierarchy can be "exploded" into groups, but once
+ ;;; this is done, there is no way to "implode" a bunch of groups back
+ ;;; into a hierarchy.  One way to implement this would be to have gnus,
+ ;;; every time it starts up, look for a bunch of groups in .newsrc all of
+ ;;; which are in the same hierarchy, all of which have the same
+ ;;; subscribed/unsubscribed status, and all of which show no articles
+ ;;; that have been read (excluding articles no longer on the system),
+ ;;; and then automatically implode.
+ ;;; * The .newsrc written is not really compatible with non-hierarchical
+ ;;; newsreaders.  It might be better to rename it, or redesign things
+ ;;; somehow to make it easy for people to switch back and forth between
+ ;;; hierarchical and non-hierarchical newsreaders.  
+ ;;; Note that hierarchical GNUS can read a non-hierarchical .newsrc;
+ ;;; this feature should be preserved.
+ ;;; * Probably hierarchies should only be shown if some group in them
+ ;;; has unread news.  This is not currently the way it works.
+ (defvar gnus-show-hierarchies t
+   "*Instead of subscribing users to new newsgroups, subscribe them to
+ the hierarchies which contain those groups.")
+ 
  (defvar gnus-thread-hide-subject t
    "*Non-nil means hide subjects for thread subtrees.")
  
***************
*** 330,335 ****
--- 351,357 ----
  (defvar gnus-subscribe-newsgroup-method
    (function gnus-subscribe-alphabetically)
    "*A function called with a newsgroup name when new newsgroup is found.
+ The argument is ('hierarchy name) or ('group name).
  The function gnus-subscribe-randomly inserts a new newsgroup a the
  beginning of newsgroups.  The function gnus-subscribe-alphabetically
  inserts it in strict alphabetic order.  The function
***************
*** 561,567 ****
  
  ;; Internal variables.
  
! (defconst gnus-version "GNUS 3.14.1"
    "Version numbers of this version of GNUS.")
  
  (defvar gnus-Info-nodes
--- 583,589 ----
  
  ;; Internal variables.
  
! (defconst gnus-version "GNUS 3.14.1 with hierarchy patches"
    "Version numbers of this version of GNUS.")
  
  (defvar gnus-Info-nodes
***************
*** 1133,1138 ****
--- 1155,1161 ----
    ;; Insert the message.
    (insert "
                     GNUS Version 3.14.1
+                   with hierarchy patches
  
           NNTP-based News Reader for GNU Emacs
  
***************
*** 1193,1201 ****
  	(newsrc gnus-newsrc-assoc)
  	(group-info nil)
  	(group-name nil)
! 	(unread-count 0)
! 	;; This specifies the format of Group buffer.
! 	(cntl "%s%s%5d: %s\n"))
      (erase-buffer)
      ;; List newsgroups.
      (while newsrc
--- 1216,1222 ----
  	(newsrc gnus-newsrc-assoc)
  	(group-info nil)
  	(group-name nil)
! 	(unread-count 0))
      (erase-buffer)
      ;; List newsgroups.
      (while newsrc
***************
*** 1204,1228 ****
        (setq unread-count (nth 1 (gnus-gethash group-name gnus-unread-hashtb)))
        (if (or all
  	      (and (nth 1 group-info)	;Subscribed.
! 		   (> unread-count 0)))	;There are unread articles.
  	  ;; Yes, I can use gnus-Group-prepare-line, but this is faster.
! 	  (insert
! 	   (format cntl
! 		   ;; Subscribed or not.
! 		   (if (nth 1 group-info) " " "U")
! 		   ;; Has new news?
! 		   (if (and (> unread-count 0)
! 			    (>= 0
! 				(- unread-count
! 				   (length
! 				    (cdr (assoc group-name
! 						gnus-marked-assoc))))))
! 		       "*" " ")
! 		   ;; Number of unread articles.
! 		   unread-count
! 		   ;; Newsgroup name.
! 		   group-name))
! 	)
        (setq newsrc (cdr newsrc))
        )
      (setq gnus-have-all-newsgroups all)
--- 1225,1257 ----
        (setq unread-count (nth 1 (gnus-gethash group-name gnus-unread-hashtb)))
        (if (or all
  	      (and (nth 1 group-info)	;Subscribed.
! 		   (or (eq (nth 2 group-info) 'hierarchy)
! 		       (> unread-count 0))))	;There are unread articles.
  	  ;; Yes, I can use gnus-Group-prepare-line, but this is faster.
! 	  (progn
! 	    ;; Subscribed or not.
! 	    (insert (if (nth 1 group-info) " " "U"))
! 	    ;; Has new news?
! 	    (if (eq (nth 2 group-info) 'hierarchy)
! 		(insert "     H")
! 	      (insert
! 	       (if (and (> unread-count 0)
! 			(>= 0
! 			    (- unread-count
! 			       (length
! 				(cdr (assoc group-name
! 					    gnus-marked-assoc))))))
! 		   "*" " ")
! 	       (format "%5d" unread-count))
! 	      )
! 
! 	    (insert
! 	     ": "
! 
! 	     ;; Newsgroup name.
! 	     group-name
! 
! 	     "\n")))
        (setq newsrc (cdr newsrc))
        )
      (setq gnus-have-all-newsgroups all)
***************
*** 1235,1262 ****
  INFO is an element of gnus-newsrc-assoc or gnus-killed-assoc."
    (let* ((group-name (car info))
  	 (unread-count
! 	  (or (nth 1 (gnus-gethash group-name gnus-unread-hashtb))
! 	      ;; Not in hash table, so compute it now.
! 	      (gnus-number-of-articles
! 	       (gnus-difference-of-range
! 		(nth 2 (gnus-gethash group-name gnus-active-hashtb))
! 		(nthcdr 2 info)))))
! 	 ;; This specifies the format of Group buffer.
! 	 (cntl "%s%s%5d: %s\n"))
!     (format cntl
! 	    ;; Subscribed or not.
! 	    (if (nth 1 info) " " "U")
! 	    ;; Has new news?
! 	    (if (and (> unread-count 0)
! 		     (>= 0
! 			 (- unread-count
! 			    (length
! 			     (cdr (assoc group-name gnus-marked-assoc))))))
! 		"*" " ")
! 	    ;; Number of unread articles.
! 	    unread-count
  	    ;; Newsgroup name.
  	    group-name
  	    )))
  
  (defun gnus-Group-update-group (group &optional visible-only)
--- 1264,1298 ----
  INFO is an element of gnus-newsrc-assoc or gnus-killed-assoc."
    (let* ((group-name (car info))
  	 (unread-count
! 	  (if (eq (nth 2 info) 'hierarchy)
! 	      0
! 	    (or (nth 1 (gnus-gethash group-name gnus-unread-hashtb))
! 		;; Not in hash table, so compute it now.
! 		(gnus-number-of-articles
! 		 (gnus-difference-of-range
! 		  (nth 2 (gnus-gethash group-name gnus-active-hashtb))
! 		  (nthcdr 2 info)))))))
!     (concat
!      ;; Subscribed or not.
!      (if (nth 1 info) " " "U")
!      ;; Has new news?
!      (if (and (> unread-count 0)
! 	      (>= 0
! 		  (- unread-count
! 		     (length
! 		      (cdr (assoc group-name gnus-marked-assoc))))))
! 	 "*" " ")
! 	    ;; Number of unread articles or H for hierarchy
! 	    (if (eq (nth 2 info) 'hierarchy)
! 		       "    H"
! 	      (format "%5d" unread-count))
! 
! 	    ": "
! 
  	    ;; Newsgroup name.
  	    group-name
+ 
+ 	    "\n"
  	    )))
  
  (defun gnus-Group-update-group (group &optional visible-only)
***************
*** 1289,1295 ****
    "Get newsgroup name around point."
    (save-excursion
      (beginning-of-line)
!     (if (looking-at ".[* \t]*[0-9]+:[ \t]+\\([^ \t\n]+\\)$")
  	(buffer-substring (match-beginning 1) (match-end 1))
        )))
  
--- 1325,1331 ----
    "Get newsgroup name around point."
    (save-excursion
      (beginning-of-line)
!     (if (looking-at ".[* \t]*[0-9H]+:[ \t]+\\([^ \t\n]+\\)$")
  	(buffer-substring (match-beginning 1) (match-end 1))
        )))
  
***************
*** 1298,1313 ****
  If argument ALL is non-nil, already read articles become readable.
  If optional argument NO-ARTICLE is non-nil, no article body is displayed."
    (interactive "P")
!   (let ((group (gnus-Group-group-name))) ;Newsgroup name to read.
      (if group
! 	(gnus-Subject-read-group
! 	 group
! 	 (or all
! 	     ;;(not (nth 1 (assoc group gnus-newsrc-assoc)))	;Unsubscribed
! 	     (zerop
! 	      (nth 1 (gnus-gethash group gnus-unread-hashtb))))	;No unread
! 	 no-article
! 	 ))
      ))
  
  (defun gnus-Group-select-group (all)
--- 1334,1383 ----
  If argument ALL is non-nil, already read articles become readable.
  If optional argument NO-ARTICLE is non-nil, no article body is displayed."
    (interactive "P")
!   (let ((group (gnus-Group-group-name)) ;Newsgroup name to read.
! 	(tail gnus-newsrc-assoc)
! 	(prev nil))
      (if group
! 	(progn
! 	  ;; First, find the group in gnus-newsrc-assoc so we know
! 	  ;; whether it is a hierarchy.
! 	  (catch 'exit-loop
! 	    (while tail
! 	      (if (string= (car (car tail)) group)
! 		  (throw 'exit-loop nil))
! 	      (setq prev tail)
! 	      (setq tail (cdr tail))))
! 	  (if (eq (nth 2 (car tail)) 'hierarchy)
! 	      (progn
! 		(message "Exploding hierarchy %s..." group)
! 		;; Delete group from gnus-newsrc-assoc
! 		(if prev
! 		    (setcdr prev (cdr tail))
! 		  (setq gnus-newsrc-assoc (cdr tail)))
! 		(gnus-update-newsrc-buffer group 'delete)
! 		;; Subscribe to groups and hierarchies
! 		;; one level down from group
! 		(let ((new-newsgroups (gnus-find-new-newsgroups group)))
! 		  (while new-newsgroups
! 		    (funcall gnus-subscribe-newsgroup-method
! 			     (car new-newsgroups))
! 		    (setq new-newsgroups (cdr new-newsgroups))
! 		    ))
! 		(message "Exploding hierarchy %s...done" group)
! 		;; This is not necessarily the cleanest way to show them;
! 		;; i.e. does it cause unpleasant changes to the display?
! 		(gnus-Group-list-groups gnus-have-all-newsgroups)
! 		)
! 	    ;; It's a group.  Read it.
! 	    (gnus-Subject-read-group
! 	     group
! 	     (or all
! 		 ;;(not (nth 1 (assoc group gnus-newsrc-assoc)))	;Unsubscribed
! 		 (zerop
! 		  (nth 1 (gnus-gethash group gnus-unread-hashtb))))	;No unread
! 	     no-article
! 	     )
! 	    )))
      ))
  
  (defun gnus-Group-select-group (all)
***************
*** 1339,1345 ****
  	(regexp 
  	 (format "^%s[ \t]*\\(%s\\):"
  		 (if any-group ".." " [ \t]")
! 		 (if any-group "[0-9]+" "[1-9][0-9]*")))
  	(found nil))
      (if backward
  	(beginning-of-line)
--- 1409,1415 ----
  	(regexp 
  	 (format "^%s[ \t]*\\(%s\\):"
  		 (if any-group ".." " [ \t]")
! 		 (if any-group "[0-9H]+" "[1-9][0-9]*")))
  	(found nil))
      (if backward
  	(beginning-of-line)
***************
*** 4575,4581 ****
    (let ((groups gnus-newsrc-assoc)
  	(before nil))
      (while (and (not before) groups)
!       (if (string< newgroup (car (car groups)))
  	  (setq before (car (car groups)))
  	(setq groups (cdr groups))))
      (gnus-subscribe-newsgroup newgroup before)
--- 4645,4651 ----
    (let ((groups gnus-newsrc-assoc)
  	(before nil))
      (while (and (not before) groups)
!       (if (string< (car (cdr newgroup)) (car (car groups)))
  	  (setq before (car (car groups)))
  	(setq groups (cdr groups))))
      (gnus-subscribe-newsgroup newgroup before)
***************
*** 4586,4592 ****
    ;; Basic ideas by mike-w@cs.aukuni.ac.nz (Mike Williams)
    (save-excursion
      (set-buffer (find-file-noselect gnus-current-startup-file))
!     (let ((groupkey newgroup)
  	  (before nil))
        (while (and (not before) groupkey)
  	(goto-char (point-min))
--- 4656,4662 ----
    ;; Basic ideas by mike-w@cs.aukuni.ac.nz (Mike Williams)
    (save-excursion
      (set-buffer (find-file-noselect gnus-current-startup-file))
!     (let ((groupkey (car (cdr newgroup)))
  	  (before nil))
        (while (and (not before) groupkey)
  	(goto-char (point-min))
***************
*** 4596,4602 ****
  		      (progn
  			(setq before (buffer-substring
  				      (match-beginning 1) (match-end 1)))
! 			(string< before newgroup)))
  	    ))
  	;; Remove tail of newsgroup name (eg. a.b.c -> a.b)
  	(setq groupkey
--- 4666,4672 ----
  		      (progn
  			(setq before (buffer-substring
  				      (match-beginning 1) (match-end 1)))
! 			(string< before (car (cdr newgroup)))))
  	    ))
  	;; Remove tail of newsgroup name (eg. a.b.c -> a.b)
  	(setq groupkey
***************
*** 4606,4615 ****
        )))
  
  (defun gnus-subscribe-newsgroup (newsgroup &optional next)
!   "Subscribe new NEWSGROUP.
  If optional argument NEXT is non-nil, it is inserted before NEXT."
!   (gnus-insert-newsgroup (list newsgroup t) next)
!   (message "Newsgroup %s is subscribed" newsgroup))
  
  ;; For directories
  
--- 4676,4691 ----
        )))
  
  (defun gnus-subscribe-newsgroup (newsgroup &optional next)
!   "Subscribe new NEWSGROUP, which is a list ('group name) or ('hierarchy name).
  If optional argument NEXT is non-nil, it is inserted before NEXT."
!   (gnus-insert-newsgroup
!    (if (eq (car newsgroup) 'hierarchy)
!        (list (car (cdr newsgroup)) t 'hierarchy)
!      (list (car (cdr newsgroup)) t))
!    next)
!   (message "%s %s is subscribed"
! 	   (if (eq (car newsgroup) 'hierarchy) "Hierarchy" "Newsgroup")
! 	   (car (cdr newsgroup))))
  
  ;; For directories
  
***************
*** 5671,5678 ****
  
  ;; GNUS internal format of gnus-newsrc-assoc and gnus-killed-assoc:
  ;; (("general" t (1 . 1))
! ;;  ("misc"    t (1 . 10) (12 . 15))
! ;;  ("test"  nil (1 . 99)) ...)
  ;; GNUS internal format of gnus-marked-assoc:
  ;; (("general" 1 2 3)
  ;;  ("misc" 2) ...)
--- 5747,5755 ----
  
  ;; GNUS internal format of gnus-newsrc-assoc and gnus-killed-assoc:
  ;; (("general" t (1 . 1))
! ;;  ("misc.foo"    t (1 . 10) (12 . 15))
! ;;  ("test"  nil (1 . 99))
! ;;  ("talk"  nil 'hierarchy) ...)
  ;; GNUS internal format of gnus-marked-assoc:
  ;; (("general" 1 2 3)
  ;;  ("misc" 2) ...)
***************
*** 5720,5744 ****
  				  (list newsgroup t))
  			      (car (car gnus-newsrc-assoc)))))
  
! (defun gnus-find-new-newsgroups ()
!   "Looking for new newsgroups and return names.
! `-n' option of options line in .newsrc file is recognized."
    (let ((group nil)
  	(new-newsgroups nil))
      (mapatoms
       (function
        (lambda (sym)
  	(setq group (symbol-name sym))
  	;; Taking account of `-n' option.
  	(and (or (null gnus-newsrc-options-n-no)
  		 (not (string-match gnus-newsrc-options-n-no group))
  		 (and gnus-newsrc-options-n-yes
  		      (string-match gnus-newsrc-options-n-yes group)))
! 	     (null (assoc group gnus-killed-assoc)) ;Ignore killed.
! 	     (null (assoc group gnus-newsrc-assoc)) ;Really new.
! 	     ;; Find new newsgroup.
! 	     (setq new-newsgroups
! 		   (cons group new-newsgroups)))
  	))
       gnus-active-hashtb)
      ;; Return new newsgroups.
--- 5797,5960 ----
  				  (list newsgroup t))
  			      (car (car gnus-newsrc-assoc)))))
  
! (defun gnus-group-is-new (group)
!   "Return true if GROUP is not found in gnus-newsrc-assoc or
! gnus-killed-assoc, either as itself or as a hierarchy which includes
! GROUP."
!   (catch 'return
!     (let ((group-okay t) ;if nil, need a hierarchy
! 	  (group-found nil))
!       (while t
! 	(setq group-found (or (assoc group gnus-killed-assoc)
! 			      (assoc group gnus-newsrc-assoc)))
! 	(if (and group-found
! 		 (or group-okay (eq (nth 2 group-found) 'hierarchy))
! 		 )
! 	    (throw 'return nil))
! 	(if (not (string-match "\\.[^.]*$" group)) (throw 'return t))
! 	(setq group (substring group 0 (match-beginning 0)))
! 	(setq group-okay nil)
! 	))))
! 
! (defun gnus-find-new-newsgroups (&optional prefix)
!   "Look for new newsgroups and return names.
! `-n' option of options line in .newsrc file is recognized.
! Return value is a list each element of which is 
!   ('group name) or ('hierarchy name).
! If PREFIX is specified, it is the name of a hierarchy.  Don't return
! that hierarchy, instead return an entry for each group or hierarchy
! underneath that hierarchy.  New groups not under the PREFIX hierarchy
! are not included in the return value."
!   (if (null prefix)
!       (setq prefix ""))
    (let ((group nil)
+ 	(group-length 0)
  	(new-newsgroups nil))
      (mapatoms
       (function
        (lambda (sym)
  	(setq group (symbol-name sym))
+ 	(setq group-length (length group))
  	;; Taking account of `-n' option.
  	(and (or (null gnus-newsrc-options-n-no)
  		 (not (string-match gnus-newsrc-options-n-no group))
  		 (and gnus-newsrc-options-n-yes
  		      (string-match gnus-newsrc-options-n-yes group)))
! 	     (>= group-length (length prefix))
! 	     (string= (substring group 0 (length prefix)) prefix)
! 	     (gnus-group-is-new group)
! 	     ;; OK, we got a group which is new.
! 	     ;; Find what newsgroup or hierarchy we want to add.
! 	     (let ((new-entry
! 		    (if (not gnus-show-hierarchies)
! 			(list 'group group)
! 		      ;; Find the most general hierarchy which applies
! 		      (let ((newsrc gnus-newsrc-assoc)
! 			    (spare-assoc gnus-killed-assoc)
! 			    (longest-match prefix)
! 			    (longest-match-length 0)
! 			    (test-string nil)
! 			    (test-string-length 0))
! 			(setq longest-match-length (length longest-match))
! 			(while newsrc
! 			  (setq test-string (car (car newsrc)))
! 			  (setq test-string-length (length test-string))
! 			  (while (> test-string-length longest-match-length)
! 			    ;; Would the following be faster? Does it matter?
! 			    ;; (and (< test-string-length group-length)
! 			    ;;      (string= 
! 			    ;;       test-string
! 			    ;;       (substring group test-string-length)))
! 			    (if (string-match
! 				 (concat "^" (regexp-quote test-string))
! 				 group)
! 				(progn
! 				  (setq longest-match test-string)
! 				  (setq longest-match-length
! 					test-string-length))
! 			      (if (not (string-match "\\.[^.]*$" test-string))
! 				  ;; OK, we've tested everything
! 				  (setq test-string-length 0)
! 				(progn
! 				  (setq test-string-length (match-beginning 0))
! 				  (setq test-string
! 					(substring
! 					 test-string 0 test-string-length))
! 				  )
! 			      )))
! 			  (setq newsrc (cdr newsrc))
! 			  (if (null newsrc)
! 			      (progn
! 				(setq newsrc spare-assoc)
! 				(setq spare-assoc nil)))
! 			  )
! 			;; OK, take one more level beyond longest-match.
! 			;; skip the period
! 			(if (< longest-match-length group-length)
! 			    (setq longest-match-length
! 				  (1+ longest-match-length))
! 			  )
! 			(while (and
! 				(< longest-match-length group-length)
! 				(not (string-equal
! 				      (substring
! 				       group
! 				       longest-match-length
! 				       (1+ longest-match-length))
! 				      ".")))
! 			  (setq longest-match-length
! 				(1+ longest-match-length)))
! 			(if (= longest-match-length group-length)
! 			    (list 'group group)
! 			  (list 'hierarchy
! 				(substring group 0 longest-match-length))
! 			  )))
! 		    ))
! 	       (catch 'got-it
! 		 (let ((tail new-newsgroups)
! 		       (group-name (car (cdr new-entry))))
! 		   (while tail
! 		     (if (string= (car (cdr (car tail))) group-name)
! 			 (if (and (eq (car (car tail)) 'group)
! 				  (eq (car new-entry) 'hierarchy))
! 			     ;; If we are trying to add, for
! 			     ;; example, ('hierarchy "misc.consumers")
! 			     ;; and ('group "misc.consumers") is
! 			     ;; already in there, nuke the 'group
! 			     ;; entry and put in the hierarchy.  This
! 			     ;; is because logically speaking the
! 			     ;; group "misc.consumers" is part of the
! 			     ;; "misc.consumers" hierarchy, yet we
! 			     ;; don't know there is such a hierarchy
! 			     ;; until we find the group
! 			     ;; "misc.consumers.house".
! 			     (progn
! 			       (setcar (car tail) 'hierarchy)
! 			       (throw 'got-it nil)
! 			       )
! 			   ;; They are both the same hierarchy, which we see
! 			   ;; twice, for example, if newgroups are sent out
! 			   ;; for comp.sys.mac.lawsuits and
! 			   ;; comp.sys.mac.user-interface
! 			   ;; at the same time.  Since it's already there,
! 			   ;; don't need to do anything else
! 
! 			   ;; The one from new-entry being a hierarchy
! 			   ;; and the other a group we already checked for.
! 			   ;; The one from new-entry being a group and
! 			   ;; the other a hierarchy, or both being groups,
! 			   ;; "can't happen".
! 			   (throw 'got-it nil)
! 			   )
! 		       (setq tail (cdr tail))
! 		       ))
! 		   ;; It is genuinely new.  Add it.
! 		   (setq new-newsgroups
! 			 (cons new-entry
! 			       new-newsgroups))
! 		   ))
! 		 )
! 	     )
  	))
       gnus-active-hashtb)
      ;; Return new newsgroups.
***************
*** 5772,5779 ****
        (error "Invalid argument: %s" info))
    (let* ((group (car info))		;Newsgroup name.
  	 (range
! 	  (gnus-difference-of-range
! 	   (nth 2 (gnus-gethash group gnus-active-hashtb)) (nthcdr 2 info))))
      ;; Check duplication.
      (if (assoc group gnus-newsrc-assoc)
  	(error "Duplicated: %s" group))
--- 5988,5997 ----
        (error "Invalid argument: %s" info))
    (let* ((group (car info))		;Newsgroup name.
  	 (range
! 	  (or (eq 'hierarchy (nth 2 info))
! 	      (gnus-difference-of-range
! 	       (nth 2 (gnus-gethash group gnus-active-hashtb))
! 	       (nthcdr 2 info)))))
      ;; Check duplication.
      (if (assoc group gnus-newsrc-assoc)
  	(error "Duplicated: %s" group))
***************
*** 5802,5812 ****
      ;; Then insert to .newsrc.
      (gnus-update-newsrc-buffer group nil next)
      ;; Add to gnus-unread-hashtb.
!     (gnus-sethash group
! 		  (cons group		;Newsgroup name.
! 			(cons (gnus-number-of-articles range) range))
! 		  gnus-unread-hashtb)
!     ))
  
  (defun gnus-check-killed-newsgroups ()
    "Check consistency between gnus-newsrc-assoc and gnus-killed-assoc."
--- 6020,6032 ----
      ;; Then insert to .newsrc.
      (gnus-update-newsrc-buffer group nil next)
      ;; Add to gnus-unread-hashtb.
!     (or (eq 'hierarchy (nth 2 info))
! 	(gnus-sethash group
! 		      (cons group		;Newsgroup name.
! 			    (cons (gnus-number-of-articles range) range))
! 		      gnus-unread-hashtb)
! 	))
!     )
  
  (defun gnus-check-killed-newsgroups ()
    "Check consistency between gnus-newsrc-assoc and gnus-killed-assoc."
***************
*** 5893,5914 ****
      (while read
        (setq group-info (car read))	;About one newsgroup
        (setq group-name (car group-info))
!       (setq active (nth 2 (gnus-gethash group-name gnus-active-hashtb)))
!       (if (and gnus-octive-hashtb
! 	       ;; Is nothing changed?
! 	       (equal active
! 		      (nth 2 (gnus-gethash group-name gnus-octive-hashtb)))
! 	       ;; Is this newsgroup in the unread hash table?
! 	       (gnus-gethash group-name gnus-unread-hashtb)
! 	       )
! 	  nil				;Nothing to do.
! 	(setq range (gnus-difference-of-range active (nthcdr 2 group-info)))
! 	(gnus-sethash group-name
! 		      (cons group-name	;Group name
! 			    (cons (gnus-number-of-articles range)
! 				  range)) ;Range of unread articles
! 		      gnus-unread-hashtb)
! 	)
        (setq read (cdr read))
        )
      (message "Checking new news... done")
--- 6113,6140 ----
      (while read
        (setq group-info (car read))	;About one newsgroup
        (setq group-name (car group-info))
!       (if (not (eq (nth 2 group-info) 'hierarchy))
! 	  (progn
! 	    (setq active
! 		  (nth 2 (gnus-gethash group-name gnus-active-hashtb)))
! 	    (if (and gnus-octive-hashtb
! 		     ;; Is nothing changed?
! 		     (equal active
! 			    (nth 2
! 				 (gnus-gethash group-name gnus-octive-hashtb)))
! 		     ;; Is this newsgroup in the unread hash table?
! 		     (gnus-gethash group-name gnus-unread-hashtb)
! 		     )
! 		nil				;Nothing to do.
! 	      (setq range
! 		    (gnus-difference-of-range active (nthcdr 2 group-info)))
! 	      (gnus-sethash group-name
! 			    (cons group-name	;Group name
! 				  (cons (gnus-number-of-articles range)
! 					range)) ;Range of unread articles
! 			    gnus-unread-hashtb)
! 	      )
! 	    ))
        (setq read (cdr read))
        )
      (message "Checking new news... done")
***************
*** 6211,6217 ****
  	       ":" (buffer-substring (match-beginning 2) (match-end 2))))
  	(setq ranges (buffer-substring (match-beginning 3) (match-end 3)))
  	(setq read-list nil)
! 	(while (string-match "^[, \t]*\\([0-9-]+\\)" ranges)
  	  (setq subrange (substring ranges (match-beginning 1) (match-end 1)))
  	  (setq ranges (substring ranges (match-end 1)))
  	  (cond ((string-match "^\\([0-9]+\\)-\\([0-9]+\\)$" subrange)
--- 6437,6443 ----
  	       ":" (buffer-substring (match-beginning 2) (match-end 2))))
  	(setq ranges (buffer-substring (match-beginning 3) (match-end 3)))
  	(setq read-list nil)
! 	(while (string-match "^[, \t]*\\([0-9-h]+\\)" ranges)
  	  (setq subrange (substring ranges (match-beginning 1) (match-end 1)))
  	  (setq ranges (substring ranges (match-end 1)))
  	  (cond ((string-match "^\\([0-9]+\\)-\\([0-9]+\\)$" subrange)
***************
*** 6229,6234 ****
--- 6455,6462 ----
  		       (cons (cons (string-to-int subrange)
  				   (string-to-int subrange))
  			     read-list)))
+ 		((string-match "^h$" subrange)
+ 		 (setq read-list (cons 'hierarchy nil)))
  		(t
  		 (ding) (message "Ignoring bogus lines of %s" newsgroup)
  		 (sit-for 0))
***************
*** 6404,6409 ****
--- 6632,6639 ----
  
  (defun gnus-ranges-to-newsrc-format (ranges)
    "Insert ranges of read articles."
+   (if (eq (car ranges) 'hierarchy)
+       (insert "h")
    (let ((range nil))			;Range is a pair of BEGIN and END.
      (while ranges
        (setq range (car ranges))
***************
*** 6421,6426 ****
--- 6651,6657 ----
  	     (if ranges (insert ","))
  	     ))
        )))
+   )
  
  (defun gnus-compress-sequence (numbers)
    "Convert list of sorted numbers to ranges."
