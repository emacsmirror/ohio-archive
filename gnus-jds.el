; Mark interesting articles in GNUS subject mode.
; Jeff Sparkes,  Memorial University of Newfoundland, jeff1@garfield.mun.edu
; July 18, 1989
; Distributable under the GNU General Public License

(provide 'gnus-jds)

(defvar gnus-jds-version "0.2" "Version number for patches")
(defvar gnus-interest nil "Have any articles been marked as interesting?")

; Change: call original hook before? mine
(setq gnus-Startup-hook
      '(lambda ()
	 (define-key gnus-Subject-mode-map "\C-I" 'gnus-Subject-interesting-subject)
	 (fset 'gnus-Subject-display-article 'gnus-jds-Subject-display-article)
	 (fset 'gnus-Subject-first-unread-article
		'gnus-jds-Subject-first-unread-article)

	 ))

(defun gnus-Subject-interesting-subject ()
  "Mark articles with this subject as interesting."
  (interactive)
  (gnus-Subject-mark-as-read nil "I")
  (save-excursion
    (let ((subject (gnus-Subject-subject-string)))
      (while (gnus-Subject-search-forward nil subject)
	(gnus-Subject-mark-as-read nil "I"))
      ))
  (gnus-Subject-next-subject 1 t)
  (setq gnus-interest t)
  )

(defun gnus-Subject-mark-interesting ()
  "Find all interesting subjects (marked with \"I\") and mark them as unread."
  (save-excursion
    ; Note: we are called from display-article, and are in Subject-buffer
    (goto-char (point-min))
    (let ((end (point-max)))
      (while (re-search-forward "^I" end t)
	(gnus-Subject-mark-as-unread nil t)))
    ))

; A modified version that mark all interesting articles (marked with "I")
; as unread if necessary
(defun gnus-jds-Subject-display-article (article &optional all-header)
  "Display ARTICLE in Article buffer."
  (if (null article)
      nil
    (gnus-configure-windows 'SelectArticle)
    (pop-to-buffer gnus-Subject-buffer)
    (if gnus-interest
	(gnus-Subject-mark-interesting))
    (gnus-Article-prepare article all-header)
    (gnus-Subject-recenter)
    (gnus-Subject-set-mode-line)
    (run-hooks 'gnus-Select-article-hook)
    ;; Successfully display article.
    t
    ))

(defun gnus-jds-Subject-first-unread-article ()
  "Select first unread article. Return non-nil if successfully selected."
  (interactive)
  (if gnus-interest
      (gnus-Subject-mark-interesting))
  (let ((begin (point)))
    (goto-char (point-min))
    (if (re-search-forward "^ [ \t]+[0-9]+:" nil t)
	(gnus-Subject-display-article (gnus-Subject-article-number))
      ;; If there is no unread articles, stay there.
      (goto-char begin)
      ;;(gnus-Subject-display-article (gnus-Subject-article-number))
      (message "No more unread articles")
      nil
      )
    ))

