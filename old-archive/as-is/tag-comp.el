;To: unix-emacs@bbn.com
;Date: 6 Sep 88 17:06:00 GMT
;From: Henry Kautz <allegra.UUCP!kautz%eddie.mit.edu.uucp@bbn.com>
;Sender: arpa-unix-emacs-request@bbn.com
;Subject: completing find tag
;Source-Info:  From (or Sender) name not authenticated.
;
;Here's an extension to the gnu Tag system, which allows name
;completion on the find-tag commands.
;	
;---- Henry Kautz
;:uucp:	allegra!kautz
;:arpa/internet: kautz%allegra.att.com@research.att.com
;:csnet: kautz%allegra.att.com@RELAY.CS.NET
;	or	kautz%allegra@btl.csnet
;
;-----------------------------------cut here-----------------------------
;;; Extension to GNU tag system
;;  New feature:
;;       tag name completion
;;  Created by Henry Kautz, AT&T Bell Labs
;;  May 2, 1988
;;  To use:  rebind keys to use
;;     completing-find-tag (instead of find-tag, by default on M-.)
;;     completing-find-tag-other-window 
;;                         (instead of find-tag-other-window,
;;                             by default on C-x-4-.)
;;  You do NOT need to explicitly build the tag completion table,
;;  this will happen automagically.
;;
;;; This file is not part of the GNU Emacs distribution (yet).
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; this file, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

(load-library "tags")
(provide 'build-tag-completion)
(provide 'tag-completion)

(defvar *tag-completion-table* nil)

(defun current-tag-completion-table ()
   "return the current tag completion table"
   (save-excursion (save-window-excursion (visit-tags-table-buffer)))
   *tag-completion-table*)
   

(defun build-tag-completion-table ()
   "Implicitly create *tag-completion-table*, based on current buffer"
   (goto-char (point-min))
   (setq *tag-completion-table* nil)
   (while (re-search-forward "^[^ ]+ +\\([^ ]+\\) " nil t)
      (setq *tag-completion-table* 
	 (cons (list (buffer-substring (match-beginning 1)
			(match-end 1))) *tag-completion-table*))
      )
   )

(defun build-tag-completion-command ()
   "Explicitly create *tag-completion-table*"
   (interactive)
   (save-excursion
      (save-window-excursion
	 (visit-tags-table-buffer)
	 (build-tag-completion-table)
	 )))

(defun completing-find-tag (tagname &optional next other-window)
   "better interface to find-tag"
   (interactive (if current-prefix-arg
		   '(nil t)
		   (let* ((default (find-tag-default))
			    (spec (completing-read
				     (if default
					(format "Find tag: (default %s) " 
					   default)
					"Find tag: ")
				     (current-tag-completion-table)
				     nil nil nil)))
		      (list (if (equal spec "")
			       default
			       spec)))))
   (find-tag tagname next other-window))

(defun completing-find-tag-other-window (tagname &optional next)
   "better interface to find-tag, uses other window"
   (interactive (if current-prefix-arg
		   '(nil t)
		   (let* ((default (find-tag-default))
			    (spec (completing-read
				     (if default
					(format 
				      "Find tag other window: (default %s) " 
					   default)
					"Find tag other window: ")
				     (current-tag-completion-table)
				     nil nil nil)))
		      (list (if (equal spec "")
			       default
			       spec)))))
   (find-tag tagname next t))

;;
;; visit-tags-table-buffer is modified to call build-tag-completion-table
;;
(defun visit-tags-table-buffer ()
   "Select the buffer containing the current tag table.
This is a file whose name is in the variable tags-file-name."
   ;; Modification
   (let (new-flag)
      (or tags-file-name
	 (call-interactively 'visit-tags-table))
      (set-buffer (or (get-file-buffer tags-file-name)
		     (progn
			;; Modification
			(setq new-flag t)
			(setq tag-table-files nil)
			(find-file-noselect tags-file-name))))
      (or (verify-visited-file-modtime (get-file-buffer tags-file-name))
	 (cond ((yes-or-no-p "Tags file has changed, read new contents? ")
		  (revert-buffer t t)
		  ;; Modification
		  (setq new-flag t)
		  (setq tag-table-files nil))))
      (or (eq (char-after 1) ?\^L)
	 (error "File %s not a valid tag table" tags-file-name))
      ;; Modification
      (if new-flag (build-tag-completion-table))))
