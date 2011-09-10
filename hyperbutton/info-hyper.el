;; ========================================================================
;; info-hyper.el -- Hyperbutton handling for Info
;; Author          : Mike Williams <mike-w@cs.aukuni.ac.nz>
;; Created On      : Thu Apr 18 11:50:03 1991
;; Last Modified By: Mike Williams
;; Last Modified On: Tue May 28 16:25:24 1991
;; RCS Info        : $Revision: 1.1 $ $Locker:  $
;; ========================================================================
;;
;; Based on info-mouse.el
;; by Bob Weiner <mailrus!uflorida!novavax!weiner@bbn.com>
;; 
;; In your Info-mode-hook:
;; 
;; (mapcar 'hyperbutton:add-local-handler
;; 	   '(Info-header-hyperbutton
;; 	     Info-menu-hyperbutton
;; 	     Info-note-hyperbutton))

(require 'info)
(require 'hyperbutton)
(require 'backquote)

(provide 'info-hyper)

;;=========================================================================

(defun Info-header-hyperbutton ()
  (interactive)
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
	      (progn
		(setq filep (string-equal
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
	(` (Info-goto-node (, nodename)))))))

(defun Info-note-hyperbutton ()
  (interactive)
  (let ((note-name nil) (bol nil))
    (save-excursion
      (if (re-search-forward "[:.\n]" nil t)
	  (progn
	    (save-excursion
	      (beginning-of-line)
	      (setq bol (point)))
	    (if (re-search-backward 
		 "\*\\(Note\\|Ref\\)[ \n]+\\([^:]*\\):" bol t)
		(setq note-name (buffer-substring
				 (match-beginning 2)
				 (match-end 2)))))))
    (if (not note-name)
	nil
      (` (Info-follow-reference (, note-name))))))

(defun Info-menu-hyperbutton ()
  (interactive)
  (let ((in-menu nil) (curr-point (point)))
    (save-excursion
      (goto-char (point-min))
      (setq in-menu 
	    (and (re-search-forward "^\* Menu:" nil t)
		 (< (point) curr-point))))
    (if (not in-menu)
	nil
      (forward-char) ; Pass '*' char if point is in front of
      (if (re-search-backward "^\*" nil t)
	  (progn (forward-char 2)
		 (` (Info-goto-node (, (Info-extract-menu-node-name)))))))))

;;=== END of info-hyper.el ================================================

