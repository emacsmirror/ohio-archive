;; LCD Archive Entry:
;; gnuslive|Travis J.I. Corcoran|tjic@icd.teradyne.com|
;; In gnus, highlights your favorite topics and people in color|
;; 30-Apr-95|0.9|~/packages/gnuslive.el|
;;
;;------------------------------------------------------------
;;                    Gnus LIVE
;;                    
;;                    Travis J.I. Corcoran        28 Feb 95
;;
;;                    bug reports to tjic@icd.teradyne.com
;;------------------------------------------------------------
;;
;;   What this package does:
;;        
;;        In a gnus summary buffer, highlights your favorite topics
;;        in one color, your favorite people in another color, and
;;        yourself in a third color.
;;
;;   How to install: 
;;
;;        add (load-file "/u/tjic/lisp/gnuslive.el") to your .emacs
;;     
;;   How to use:
;;
;;        when editting a kill file, you now have at your disposal 
;;        two new commands:
;;        
;;        gnus-kill-file-live-by-author    ( C-c C-l C-a )
;;        gnus-kill-file-live-by-subject   ( C-c C-l C-s )
;;        
;;        Using these will insert a call to the func "gnus-live"
;;        into your kill file.  The next time this func is executed
;;        (either by you evalling the lisp sexp right then and there,
;;        by means of C-x C-e, or automatically the next time you enter
;;        the newsgroup) it will highlight the appropriate info in
;;        the summary buffer.
;;        
;;   How to customize the colors:
;;        
;;        (1) pick a new face, either from the ones provided below, or of your
;;        own creation.
;;        (2) set one of the vars
;;        { gnuslive-my-face | gnuslive-people-face | gnuslive-topics-face }
;;        to the appropriate face in your .emacs file, after loading this
;;        file.  Ex:  (setq gnus-live-my-face  'pink-face )
;;        
;;   How to customize who gnuslive thinks you are: 
;;        
;;        Set the var gnuslive-my-addr in your .emacs after loading this file.
;;        Ex:   (setq  gnuslive-my-addr "tjic@porsche.icd.teradyne.com")
;;        
;;   NOTES:      
;;        (1) If you want your name to be highlighted, you must have a 
;;        (gnus-live "FROM" ...) statement in a kill-file.  I suggest the 
;;        global kill-file, so that it gets highlighted in all newsgroups.
;;        
;;        
;;   BUGS:      
;;       (1) the commands "gnus-kill-file-live-by-subject" and 
;;           "gnus-kill-file-live-by-author" are not documented 
;;           in the defun gnus-kill-file-mode-map.
;;        
;;        
;;        
;;        

(make-face 'white-on-blue-face) 
(copy-face 'highlight 'white-on-blue-face)
(set-face-background  'white-on-blue-face "Blue")
(set-face-foreground  'white-on-blue-face "white")

(make-face 'white-on-red-face)
(copy-face 'highlight 'white-on-red-face)
(set-face-background  'white-on-red-face "red")
(set-face-foreground  'white-on-red-face "white")

(make-face 'white-on-orange-face)
(copy-face 'highlight 'white-on-orange-face)
(set-face-background  'white-on-orange-face "orange")
(set-face-foreground  'white-on-orange-face "white")

(defvar gnuslive-my-addr "nil" 
  "*Users email addr, as it may appear in UseNet posts.")

(defvar gnuslive-my-face     'white-on-blue-face
  "the face used to highlight users addr in the *Subject* buffer of gnus")

(defvar gnuslive-people-face 'white-on-orange-face
  "the face used to highlight addresses of people in the *Subject* buffer of gnus")

(defvar gnuslive-topics-face 'white-on-red-face
  "the face used to highlight topics in the *Subject* buffer of gnus")


(add-hook 'gnus-kill-file-mode-hook
		  '(lambda ()
			  (define-key gnus-kill-file-mode-map "\C-c\C-l\C-s" 'gnus-kill-file-live-by-subject)
			  (define-key gnus-kill-file-mode-map "\C-c\C-l\C-a" 'gnus-kill-file-live-by-author)))

(defun gnus-kill-file-live-by-subject ()             ; stolen from gnus.el
  "Insert LIVE command for current subject."
  (interactive)
  (insert
   (format "(gnus-live \"Subject\" %s)\n"
	   (prin1-to-string
	    (if gnus-current-kill-article
		(regexp-quote
		 (nntp-header-subject
		  ;; No need to speed up this command.
		  ;;(gnus-get-header-by-number gnus-current-kill-article)
		  (gnus-find-header-by-number gnus-newsgroup-headers
					      gnus-current-kill-article)))
	      "")))))

(defun gnus-kill-file-live-by-author ()             ; stolen from gnus.el
  "Insert LIVE command for current author."
  (interactive)
  (insert
	(format "(gnus-live \"From\" %s)\n"
	   (prin1-to-string
	    (if gnus-current-kill-article
		(regexp-quote
		 (nntp-header-from
		  ;; No need to speed up this command.
		  ;;(gnus-get-header-by-number gnus-current-kill-article)
		  (gnus-find-header-by-number gnus-newsgroup-headers
					      gnus-current-kill-article)))
	      "")))))

(defun gnus-live (field regexp)
  "Highlight a header: call gnus-kill w optional 'command' arg,
telling it to highlight instead of kill the article.  Also pass in the optional
'all' arg, telling it to highlight even articles you've already viewed."
  (cond ((string= field "Subject") 
		 (gnus-kill field regexp 'gnuslive-highlight-subj t))
		((string= field "From"   ) 
		 (gnus-kill field regexp		
					'(gnuslive-highlight-from 
					  (not (equal        ; if "FROM" me, pass in flag 
							(string-match 
							 regexp 
							 gnuslive-my-addr) nil ))) t))))

(defun gnuslive-highlight-subj ()
"Low level: highlight a header.  This is a 'callback' function from gnus-kill.
See the definition of gnus-kill (in gnus.el) and gnus-live (in this file) for
more details."
  (let ((buffer-read-only nil))
	(overlay-regexp "\\].*"  gnuslive-topics-face 1)))

(defun gnuslive-highlight-from ( from-me )
"Low level: highlight a header.  This is a 'callback' function from gnus-kill.
See the definition of gnus-kill (in gnus.el) and gnus-live (in this file) for
more details."
  (let ((buffer-read-only nil))
	(overlay-regexp "\\[.*@[a-zA-Z\._]*\\]" 
					(if from-me			; if from-me flag set, 
						gnuslive-my-face 
					  gnuslive-people-face ) 1))) ; highlight 

(defun tjic-function-version (function)
  (nth 3 (symbol-function function)))

;(if (or (not (fboundp 'overlay-regexp))
;		( <  (tjic-function-version 'overlay-regexp) 1 ))

(defun overlay-regexp ( target-regexp overlay-face &optional count) ; 
  "put a given overlay over first COUNT occurances of a given regexp.
If COUNT is not passed in, or is nil, do for all occurances."
  1										; function version
  (interactive)
  (let ((buffer-read-only nil))
	(save-excursion
;	  (goto-char (point-min))
	  (let ((cntr 0))
		(while
			(and (re-search-forward target-regexp nil t  ) 
				 (not (equal cntr count)))
		  (progn
			(set-text-properties (match-beginning 0) (match-end 0)
								 (list 'face overlay-face))
			(setq cntr ( + cntr 1 ))))))))
;)
