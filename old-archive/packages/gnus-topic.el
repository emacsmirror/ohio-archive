; Path: dg-rtp!rock!mcnc!stanford.edu!agate!ucbvax!cis.ohio-state.edu!zaphod.mps.ohio-state.edu!swrinde!cs.utexas.edu!uunet!lll-winken!aunro!alberta!cpsc.ucalgary.ca!krawchuk
; From: krawchuk@cpsc.UCalgary.ca (Bj Krawchuk)
; Newsgroups: gnu.emacs.sources,gnu.emacs.gnus,news.software.readers
; Subject: gnus-topic.el 1.0 : A Topic Mode for GNUS
; Date: 29 Jun 91 06:18:03 GMT
; Organization: The University of Calgary
; 
; 
; 
; gnus-topic 1.0 provides another mode to GNUS, in addition to 
; The idea of it is to organize the selection and maintenance
; of groups of newsgroups, which I call Topics.
; 
; This is not yet a finished product. I am posting it in the hope
; that people will help me work on it. I am low on time and would
; like the emergence of an excellent product sooner than I could
; provide it working on it alone.
; Anyone wishing to do enhancements, bug fixes and changes should 
; contact me, and I will coordinate the development. 
; Or we could do the development via posting. 
; Any volunteers?
; 
; Since this is not nearly finished, expect bugs, and annoyances.
; Nevertheless, I have been using this current version for several
; days now, and still would rather not do without it.
; Number one on the priority list for enhancements is to speed up 
; article counting. Right now it takes me about 3 seconds to update 
; the topic listing. (But I have a huge .newsrc ...). Also,
; maintence of the newsgroup regexps has not been written yet.
; They currently have to be edited in the ~/.topics file. 
; (And I have a bunch of other beefs...)

; I have posted this news.software.readers since there has recently
; been some talk there about newsgroup organization. 
; This may provide a starting point for further GNUS development
; on group handling. For example, group melding could be done using
; this as its user interface. 
;
; Enjoy, and please help me out, if you can and want to.
;
; Cheers
;	Bj  <krawchuk@cpsc.ucalgary.ca>
;

;;;-------------------------------------------------------------------
;;;
;;; gnus-topic.el 1.0
;;; Organize newsgroups by topic   
;;; Jun 91
;;; Bj Krawchuk <krawchuk@cpsc.ucalgary.ca>
;;; [Awefold Compatible]
;;;
;;; LCD Archive Entry:
;;; gnus-topic|Bj Krawchuk|krawchuk@cpsc.UCalgary.ca
;;; |Topics mode for GNUS for organizing newsgroups
;;; |91-06-29|1.0|~/packages/gnus-topic.el.Z
;;;
;;; See mode description for keybindings 
;;; (type ? in topic mode eg.)
;;;
;;; ------------------------
;:: .topics file
;;; ------------------------
;;;
;;; in this version a .topics file must be included in your home directory
;;; Here is my .topics file
;;;
;;; (setq gnus-topic-alist 
;;; '( 
;;;  ("Health" 	"sci.med" "alt.psychoactives" "sci.psychology")
;;;  ("Drugs"  	"alt.drugs" "talk.politics.drugs")  
;;;  ("Politics"   "alt.individualism")
;;;  ("Misc"	"misc.misc" "comp.archives" "comp.risks")
;;;  ("PC"	"comp.binaries" "comp.sys.ibm.pc")
;;;  ("Emacs"  	"emacs")
;;;  ("Gnu"    	"gnu")
;;;  ("Vm"	"gnu.emacs.vm")
;;;  ("News"   	"alt.config" "news")
;;;  ("Sources"	"source")
;;;  ("Skepticism" "sci.skeptic" "talk.origins" "alt.paranormal" "alt.atheism")
;;;  ("Religion"   "alt.atheism" "religion"  "alt.magick" "alt.pagan" 
;;;		"newage" "talk.origins")
;;;  ("Research" "sci.logic" "comp.theory")
;;;  ("Radio"	"radio")
;;;  ("Calgary"	"calgary")
;;;  ("Cpsc"	"cpsc")
;;;  ("Comp"	"comp")
;;;  ("Sci"	"sci")
;;;  ("Can"	"can")
;;;  ("Music"	"music")
;;;  ("Ai"	"ai")
;;;  ("All"	"")
;;;  ))
;;;
;:|
;;; ------------------------
;:: .emacs startup additions
;;; ------------------------
;;;
;;; (setq gnus-Startup-hook 
;;;  (function (lambda () (gnus-create-topic-buffer))))
;;;
;;; (setq gnus-Group-prepare-hook
;;;  (function (lambda () (gnus-topic-Group-prepare-hook-fn))))
;;;	
;;;
;;; (setq gnus-Group-mode-hook 
;;;  (function (lambda () (require 'gnus-topic) 
;;;			  (gnus-topic-Group-mode-hook-fn))))
;;;
;:|
;;;-------------------------------------------------------------------

(require 'cl)

;:: Variables

(defvar gnus-Topic-buffer "*Topic*") ; gnus-buffer-list should have this

(defvar gnus-topic-alist 
 '( 	("Health" "sci.med" "alt.psychoactives")
	("Emacs"  "comp.emacs" "gnu.emacs.sources" "gnu.emacs.help")
  ) ; just a sample
 "Association List")

(defvar gnus-topic-newsgroups 
	'("") 	; startup Group buffer with all newsgroups
	"Currently chosen newsgroups")

;:|

;:: Topic Mode

;:: Topic Mode Keymap
(defvar gnus-Topic-mode-map nil)

(if gnus-Topic-mode-map 
    nil
  (setq gnus-Topic-mode-map (make-keymap))
  (suppress-keymap gnus-Topic-mode-map)
  (define-key gnus-Topic-mode-map " " 'gnus-Topic-select-topic)
  (define-key gnus-Topic-mode-map "=" 'gnus-Topic-select-all-topic)
  (define-key gnus-Topic-mode-map "n" 'gnus-Topic-next-topic)
  (define-key gnus-Topic-mode-map "p" 'gnus-Topic-prev-topic)
  (define-key gnus-Topic-mode-map "l" 'gnus-Topic-list-topics)
  (define-key gnus-Topic-mode-map "L" 'gnus-Topic-list-topics-verbose)
  (define-key gnus-Topic-mode-map "r" 'gnus-Topic-read-topic-file)
  (define-key gnus-Topic-mode-map "x" 'gnus-Group-force-update)
  (define-key gnus-Topic-mode-map "s" 'gnus-Group-force-update)
  (define-key gnus-Topic-mode-map "t" 'gnus-Topic-toggle-regexp-display) 
  (define-key gnus-Topic-mode-map "g" 'gnus-Topic-select-group-buffer)
  (define-key gnus-Topic-mode-map "q" 'gnus-Group-exit)
  (define-key gnus-Topic-mode-map "Q" 'gnus-Group-quit)
  (define-key gnus-Topic-mode-map "?" 'describe-mode)
)


;:|

;:: Gnus Topic Mode

(defun gnus-Topic-mode ()
 "Major mode for organizing, selecting topics in GNUS.
A Topic enables grouping of newsgroups together.

SPC	Select this topic
=	Select this topic including unread groups
n	Move to the next topic
p	Move to the previous topic
l	List (update) current topic list
L	List all topics, even those with no unread articles
r	Read topic file (~/.topics)
s,x	Force update of .newsrc
t	Toggle newsgroup regexp display
g	Jump to Group buffer (last selected)
q	exit news (gnus-Group-exit)
Q	quit news (gnus-Group-quit)

In the Group mode:

q	return to Topic mode
Q	return to Topic mode without recomputing article totals."


 (interactive)
  (cond ((boundp 'mode-line-modified)
	 (setq mode-line-modified "--- "))
	((listp (default-value 'mode-line-format))
	 (setq mode-line-format
	       (cons "--- " (cdr (default-value 'mode-line-format)))))
	(t
	 (setq mode-line-format
	       "--- GNUS: List of Topics  %[(%m)%]----%3p-%-")))
  (setq major-mode 'gnus-Topic-mode)
  (setq mode-name "Topic")
  (setq mode-line-buffer-identification	"GNUS: List of Topics")
  (setq mode-line-process nil)
  (use-local-map gnus-Topic-mode-map)
  (buffer-flush-undo (current-buffer))
  (gnus-Topic-read-topic-file)
  (gnus-Topic-list-topics)
  (goto-char (point-min))
  (if (not (eobp))
   (progn (search-forward ":" nil t)
  	  (forward-char -1)))
  (setq buffer-read-only t)		;Disable modification
  (run-hooks 'gnus-Topic-mode-hook) 
)



;:|

;:: Startup stuff

(defvar gnus-topic-startup-in-topic-buffer t
  "Should the topic buffer be the first thing you see?")

(defun gnus-create-topic-buffer ()
 "For use in gnus-Startup-hook."
  (set-buffer (get-buffer-create gnus-Topic-buffer))
  (setq gnus-topic-starting t)
  (gnus-Topic-mode)
  (setq gnus-buffer-list (cons gnus-Topic-buffer gnus-buffer-list))
  (set-buffer gnus-Group-buffer))



;:|


;:|

;:: Interactive Functions

;:: Topic Listing


(defun gnus-Topic-list-topics (&optional show-all)
  "List topics in the Topic buffer."
  (interactive nil)
  (setq gnus-Topic-current-topic 1)
  (gnus-topic-list-topics-aux show-all))

(defun gnus-topic-list-topics-aux (&optional show-all)
  "List topics in the Topic buffer."
  (interactive nil)
  (let ((topics gnus-topic-alist)
        (buffer-read-only nil))
  (erase-buffer)
  (while topics
   (let ((topic (car topics)))
    (let ((num-arts (gnus-topic-calc-number-of-articles (cdr topic))))
     (if (or show-all (not (zerop num-arts)))
       (progn
         (insert (format "[%3d] : %s\n" num-arts (car topic)))
	 (if gnus-Topic-regexp-display
		(progn (setq topic (cdr topic))	
			(while topic
			  (insert (format "\t  %s\n" (car topic))) 
			  (setq topic (cdr topic)))))))))
   (setq topics (cdr topics)))
  (set-buffer-modified-p nil)
  (gnus-Topic-jump-to-topic gnus-Topic-current-topic)))

(defun gnus-Topic-list-topics-verbose ()
  "List topics in the Topic buffer with newsgroups."
  (interactive)
  (gnus-Topic-list-topics t))

;:|

;:: Topic Navigation

(defun gnus-Topic-first-topic ()
 "Move to first topic."
 (interactive)
 (goto-char (point-min))
 (gnus-Topic-find-topic-marker))

(defun gnus-Topic-find-topic-marker ()
 "Find the colon on current line."
 (beginning-of-line)
 (if (not (= (point-min) (point-max)))
  (progn (search-forward ":" nil t)
	 (forward-char -1))))


(defun gnus-Topic-next-topic ()
 "Move to next topic."
 (interactive)
 (forward-char 1)
 (if (not (re-search-forward ":" nil t))
	(gnus-Topic-first-topic)
	(forward-char -1)))

(defun gnus-Topic-prev-topic ()
 "Move to next topic."
 (interactive)
 (beginning-of-line)
 (if (not (re-search-backward ":" nil t))
	(progn (goto-char (point-max))
		(re-search-backward ":" nil t))))

;:|

;:: Topic Selection
(defvar gnus-Topic-current-topic 1
	"The currently selected topic's line-number. Just a hack for now.")

(defun gnus-Topic-jump-to-topic (line-no)
 "Move to topic on line-no."
 (interactive "p")
 (goto-line line-no)
 (gnus-Topic-find-topic-marker)
 (setq gnus-Topic-current-topic line-no))


(defun gnus-Topic-select-topic (&optional select-all)
 "Choose to read newsgroups for this topic."
  (interactive nil)
  (setq gnus-Topic-current-topic (count-lines (point-min) (point)))
  (setq gnus-topic-newsgroups
	(gnus-topic-lookup gnus-topic-alist
			   (gnus-topic-get-topic-name)))
  (set-buffer gnus-Group-buffer)
  (let ((buffer-read-only nil))
	  (gnus-Group-prepare select-all)
	  (set-buffer-modified-p nil))
  (gnus-Topic-select-group-buffer))


(defun gnus-Topic-select-all-topic ()
 "Select newsgroups, even empty or unsubscribed groups"
  (interactive)
  (gnus-Topic-select-topic t))

(defun gnus-Group-select-topic-buffer (&optional no-update)
 "Move back to topic buffer."
 (interactive nil)
 (switch-to-buffer gnus-Topic-buffer)
 (if (not no-update)
	 (gnus-topic-list-topics-aux)))

(defun gnus-Group-select-topic-buffer-no-update ()
 "Move back to topic buffer without updating totals"
 (interactive)
 (gnus-Group-select-topic-buffer t)
)  

(defun gnus-Topic-select-group-buffer ()
 "Move back to group buffer."
 (interactive)
 (switch-to-buffer gnus-Group-buffer))


;:|

;:: Topic file
(defun gnus-Topic-read-topic-file ()
 "bleck"
 (interactive)
 (load "~/.topics")
)

(defun gnus-Topic-save-topic-file ()
 "bleck"
 (interactive)
 (message "Not implemented yet.")
)


;:|

;:: Regexp display handling
(defvar gnus-Topic-regexp-display nil 
 "Variable to control if the newsgroup regexps should be 
shown beside the topic names.")

(defun gnus-Topic-toggle-regexp-display ()
 "Toggle gnus-Topic-regexp-display."
 (interactive)
 (setq gnus-Topic-regexp-display (not gnus-Topic-regexp-display))
 (setq gnus-Topic-current-topic 1)
 (gnus-Topic-list-topics)
 (message "Newsgroup regular expression display %s" 
	(if gnus-Topic-regexp-display "on." "off."))
)

;:|

 

;:|

;:: Support Functions
;:: Topic select stuff

(defun gnus-topic-lookup (topic-alist topic)
 "Lookup the regexps for a topic."
	(cond   ((null topic-alist) nil)
		((string-equal (caar topic-alist) topic)
			(cdar topic-alist))
		(t (gnus-topic-lookup (cdr topic-alist) topic))))



(defun gnus-topic-get-topic-name ()
 "Find the topic name of the currently pointed at topic"
 (save-excursion
 (beginning-of-line)
 (if (not (looking-at "\\[[ 0-9]+\\] :"))
	(re-search-backward ":" nil t))
 (search-forward ":" nil t)
 (forward-char 1)
 (buffer-substring (point) (progn (end-of-line) (point)))))

 

;:|

;:: Group Buffer stuff

(defun gnus-Group-topic-format-group-buffer ()
 "*Format group buffer, restricting it to currently chosen topic."
 (interactive)  
 (set-buffer gnus-Group-buffer)
  (let ((buffer-read-only nil))
   (goto-char (point-min))
   (while (not (eobp))
     (if (not (string-match-member (gnus-Group-group-name) 
				      gnus-topic-newsgroups))
	(gnus-topic-delete-newsgroup)
	(next-line 1)))
   (goto-char (point-min))
   (if (not (search-forward ":" nil t))
	(message "No newsgroups with messages for this topic"))
   (set-buffer-modified-p nil)))


(defun gnus-topic-delete-newsgroup ()
 "*Remove current line from newsgroup buffer"
 (beginning-of-line)
 (kill-line 1)
)




;:|

;:: Hook function stuff

(defun gnus-topic-Group-prepare-hook-fn ()
 "Topic mode handling hook function for gnus-Group-prepare-hook."
   (if (and gnus-topic-startup-in-topic-buffer
	    gnus-topic-starting)
	(gnus-Group-select-topic-buffer))
   (gnus-Group-topic-format-group-buffer)
   (setq gnus-topic-starting nil))


(defun gnus-topic-Group-mode-hook-fn ()
 "Topic mode handling hook function for gnus-Group-mode-hook."
 (define-key gnus-Group-mode-map "q" 'gnus-Group-select-topic-buffer)
 (define-key gnus-Group-mode-map "Q" 'gnus-Group-select-topic-buffer-no-update)
) 

;:|

;:: Misc support

;:: Counting stuff
(defun gnus-topic-calc-number-of-articles (nglist)
 "Count the number of unread articles covered by the newgroup list"
  (let ((unread-count 0)
	(group-info nil)
	(group-name nil)
        (newsrc gnus-newsrc-assoc))
    (while newsrc
      (if (nth 1 (car newsrc)) ; if subscribed
      (progn
       (setq group-name (caar newsrc))
      (if (string-match-member group-name nglist)
        (setq unread-count 
	      (+ unread-count
		(nth 1 (gnus-gethash group-name gnus-unread-hashtb)))))))
      (setq newsrc (cdr newsrc)))
  unread-count))


;:|
;:|
;:|

;:: Misc Functions

(defun string-match-member (str slist)
 "*Check list for string"
 (cond  ((null slist) '())
  	((string-match (car slist) str))
	(t (string-match-member str (cdr slist)))))


;:|

(provide 'gnus-topic)

;:|
