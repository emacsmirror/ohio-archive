;; mew-f.el -- 
;; Author          : Kai Grossjohann
;; Created On      : Fri Oct 29 13:38:34 1993
;; Last Modified By: Linn H. Stanton to translate from mh to mew
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar mew-f-folder-list nil
  "This is a list of (FOLDERNAME . NUMMSGS) pairs, one for each entry
in mew-folder-alist, containing all the names of the folders together
with the number of messages in each folder.")

(defvar mew-f-buffername "* Mew: Folders *"
  "The name of the folder list buffer.")

(defvar mew-f-folder-sets nil
  "This is an alist of pairs of a folder set name and a regexp matching
all folder names to be shown.")

(defvar mew-f-current-set "all") ; being defined in function mew-f
(defvar mew-f-default-set "all") ; being defined in function mew-f

(defvar mew-f-show-empty t
  "Folders with 0 messages in them will be shown in folder list iff t")

(defun mew-f-folder-size (folder)
  (interactive)
  (length (directory-files (mew-expand-folder folder)
                           t
                           "^[0-9]+$"
                           nil)))

(defun mew-f-display-folders (regexp)
  "Go through the list of folders and display one line for each folder
matching the regexp."
  (interactive)
  (switch-to-buffer (get-buffer-create mew-f-buffername))
  (delete-other-windows)
  (erase-buffer)
  (mapcar '(lambda (folder)
             (if (string-match regexp (car folder))
                 (if (or mew-f-show-empty (not (zerop (cdr folder))))
                     (insert (format "%10d :   %s\n"
                                     (cdr folder)
                                     (car folder))))))
          mew-f-folder-list)
  (mew-f-first-folder))

(defun mew-f-define-set (name regexp)
  "Add the NAME, REGEXP pair to the mew-f-folder-sets alist if there is no
folder set with this name in it."
  (interactive)
  (if (assoc name mew-f-folder-sets)
      ()
    (setq mew-f-folder-sets (cons (cons name regexp) mew-f-folder-sets))))

(defun mew-f-current-folder ()
  "Return the name of the folder displayed on the line the cursor is on."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (looking-at "[ \t]*\\([0-9]+\\)[ \t]*:[ \t]+\\(\\+.*\\)$")
    (buffer-substring (match-beginning 2) (match-end 2))))

;;
;; User accessible functions
;;

(defun mew-f-recalculate-folder-list ()
  "For each folder in mew-folder-alist, determine the number of messages in
it and update mew-f-folder-list."
  (interactive)
  (message "Recalculating folder list...")
  (setq mew-f-folder-list
        (mapcar '(lambda (f)
                   (cons (car f) (mew-f-folder-size (car f))))
                mew-folder-alist))
  (setq mew-f-folder-list (sort mew-f-folder-list
                              '(lambda (a b) (string< (car a) (car b)))))
  (message "Recalculating folder list...done"))

(defun mew-f-view-set (&optional name)
  "In the list of folders, display folders belonging to folder set NAME
only."
  (interactive)
  (let ((setname
         (or name
             (completing-read "Name of folder set: "
                              mew-f-folder-sets
                              nil
                              t))))
    (setq mew-f-current-set setname)
    (mew-f-display-folders (cdr (assoc setname mew-f-folder-sets)))))

(defun mew-f-previous-folder (num)
  "Go up NUM lines in the folder list. Do not go past first line."
  (interactive "p")
  (forward-line (- num))
  (search-forward " : "))

(defun mew-f-next-folder (num)
  "Go down NUM lines in the folder list. Do not go past last line."
  (interactive "p")
  (forward-line num)
  (if (not (eobp))
      (search-forward " : ")))

(defun mew-f-first-folder ()
  "Go to the first folder in the folder list."
  (interactive)
  (beginning-of-buffer)
  (search-forward " : "))

(defun mew-f-last-folder ()
  "Go to the last folder in the folder list."
  (interactive)
  (end-of-buffer)
  (if (looking-at "^$")
      (forward-line -1))
  (search-forward " : "))

(defun mew-f-visit-this-folder ()
  "Call mew-goto-folder-subr with folder on the line the cursor is on."
  (interactive)
  (mew-summary-goto-folder-subr (mew-f-current-folder) t))

(defun mew-f-toggle-show-empty ()
  "Invert the mew-f-show-empty variable. See there for more info."
  (interactive)
  (setq mew-f-show-empty (not mew-f-show-empty))
  (mew-f-display-folders (cdr (assoc mew-f-current-set mew-f-folder-sets)))
  (message
   (if mew-f-show-empty "Showing empty folders." "Not showing empty folders.")))

(defun mew-f-show-empty-on ()
  "Show empty folders in the list of folders."
  (interactive)
  (setq mew-f-show-empty t))

(defun mew-f-show-empty-off ()
  "Do not show empty folders in the list of folders."
  (interactive)
  (setq mew-f-show-empty nil))

;;
;; Keymaps
;;

(defvar mew-f-mode-map (make-keymap))
(suppress-keymap mew-f-mode-map)
(define-key mew-f-mode-map "n" 'mew-f-next-folder)
(define-key mew-f-mode-map "p" 'mew-f-previous-folder)
(define-key mew-f-mode-map "<" 'mew-f-first-folder)
(define-key mew-f-mode-map ">" 'mew-f-last-folder)
(define-key mew-f-mode-map "v" 'mew-f-view-set)
(define-key mew-f-mode-map " " 'mew-f-visit-this-folder)
(define-key mew-f-mode-map "\C-m" 'mew-f-visit-this-folder)
(define-key mew-f-mode-map "e" 'mew-f-toggle-show-empty)
(define-key mew-f-mode-map "m" 'mew-summary-send)
(define-key mew-f-mode-map "f" 'mew-summary-goto-folder)
(define-key mew-f-mode-map "r" 'mew-f-reenter)
(define-key mew-f-mode-map "q" 'mew-f-reenter)
(define-key mew-f-mode-map "g" 'mew-f-reenter)

(define-key mew-summary-mode-map "q" 'mew-f-reenter)
  
;;
;; entry points
;;

(defun mew-f ()
  "Show a list of folders with the number of messages for
each. Variable mew-f-folder-sets contains a number of regexps that match
certain folder names to be displayed. Provides functions to go up and
down the list and to change the regexp."
  (interactive)
  (message "Mew: reading folder list")
  (mew-f-recalculate-folder-list)
  (message "Mew: reading folder list...done")
  (mew-f-define-set "all" ".*")
  (mew-f-view-set mew-f-default-set)
  (mew-f-mode))

(defun mew-f-reenter ()
  "Like mew-f but assume the list of folders buffer has already been
generated. Just update the numbers of messages in each folder."
  (interactive)
  (mew-f-recalculate-folder-list)
  (mew-f-view-set mew-f-current-set))

(defun mew-f-mode ()
  "Show list of folders; extension of mew which is required for this to work.

A list of folders is shown. You can move among the folders and select
a folder. An additional feature is that you can define sets of folders
based on regular expressions and switch among the view of the folder
sets. This works as follows:

In your .emacs file, put lines like the following:
        (mew-f-define-set \"news\" \"\\\\+news\\\\.\")
This defines the folder set `news' to be all folders whose names begin
with the string `+news.'. You can switch between the folder sets with
\\[mew-f-view-set].

\\{mew-f-mode-map}

mew-f uses the following variables:

  mew-f-buffername (\"* MEW-F: Folders *\")
    The name of the folder list buffer.

  mew-f-show-empty (t)
    Folders with 0 messages in the will be show in folder list iff t.
"
  (interactive)
  (setq major-mode 'mew-f-mode)
  (setq mode-name "mew-f")
  (use-local-map mew-f-mode-map))

(mew-f-define-set "incoming" "\\+inbox\\|+auto\\.")
(mew-f-define-set "cypherpunks" "\\+cypherpunks\\.")
(mew-f-define-set "sun-managers" "\\+sun-managers\\.")
(mew-f-define-set "libernet" "\\+libernet\\.")
(mew-f-define-set "porchephiles" "\\+porchephiles\\.")
(mew-f-define-set "sug" "\\+sug\\.")
  
(provide 'mew-f)

;; -- mew-f ends here

