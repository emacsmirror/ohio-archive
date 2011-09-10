;To: unix-emacs@bbn.com
;Date: 31 Oct 88 13:58:49 GMT
;From: John Sturdy <mcvax!ukc!eagle.ukc.ac.uk!icdoc!qmc-cs!harlqn!jcgs@uunet.uu.net>
;Subject: fun with recursive-edit
;
;Here are assorted things I use built on recursive-edit. wander-safe and
;in-one-window I find worth binding to keys. (Wander-safe maintains a
;stack of places - handy for wandering around to look things up, then
;going back to where you were.)
;--------------------------------- cut here for rec-edit.el --------
;;; Last edited: Fri Sep 30 07:54:14 1988 by jcgs (John Sturdy) on harlqn
;;; rec-edit.el --- various things using recursive editing

(defun end-rec-edit-message (string)
  "Tell the user what happens on ending this recursive edit."
  (message (substitute-command-keys "\\[exit-recursive-edit] to %s")
           string))

(defun edit-one-thing (thing finder)
  "Edit the buffer found by applying to THING the function FINDER,
using recursive-edit, placing point at the top to start with (it is
restored to its previous position after the edit is done)."
  (apply finder (list thing))
  (save-excursion
    (goto-char (point-min))
    (end-rec-edit-message "go to next buffer")
    (recursive-edit)))

(defun edit-one-file (file)
  "Edit FILE using recursive-edit, placing point at the top to start
with (it is restored to its previous position after the edit is
done)."
  (edit-one-thing file (symbol-function 'find-file)))

(defun auto-edit-one-file (file edit-command old new)
  "In FILE, go to the top, then apply EDIT-COMMAND with arguments OLD and
NEW, then go back to the original point in the file."
  (find-file file)
  (save-excursion
    (goto-char (point-min))
    (apply edit-command (list old new))))

(defun edit-one-buffer (buffer)
  "Edit BUFFER using recursive-edit, placing point at the top to start
with (it is restored to its previous position after the edit is
done)."
  (edit-one-thing buffer (symbol-function 'switch-to-buffer)))
    
(defun edit-file-list (file-list)
  "Use recursive editing to edit each file in FILE-LIST."
  (mapcar (symbol-function 'edit-one-file) file-list))

(defun edit-tags-files ()
  "Edit all files in tag-table-files."
  (interactive)
  (save-window-excursion
    (edit-file-list (sort (tag-table-files) 'string<))))

(defun global-edit-in-files (file-list edit-command old new)
  "Replace all occurrences of the string OLD by NEW, in all files
mentioned in the current tags table."
  (save-window-excursion
    (let ((files file-list))
      (while (not (null files))
        (auto-edit-one-file (car files)
                            edit-command
                            old new)
        (setq files (cdr files))))))

(defun global-edit-tags-files (old new)
  "Replace all occurrences of the string OLD by NEW, in all files
mentioned in the current tags table."
  (interactive "sReplace string: 
sReplace with: ")
  (global-edit-in-files (tag-table-files)
                        (symbol-function 'replace-string)
                        old new))

(defun edit-if-name-matches (filename pattern)
  "Edit FILENAME if it matches PATTERN."
  (if (string-match pattern filename)
      (progn
        (find-file filename)
        (recursive-edit))))

(defun edit-directory-files-with-name-matching (directory pattern)
  "Use recursive-edit to edit all files in DIRECTORY whose names
match PATTERN."
  (interactive "DDirectory: 
sPattern: ")
  (save-excursion
    (save-window-excursion
      (let ((files (directory-files directory t))
            (file nil))
        (while files
          (setq file (car files))
          (edit-if-name-matches file pattern)
          (setq files (cdr files))))))
  (message "edit-directory-files-with-name-matching done"))


(defun edit-buffers (buffers)
  "Use recursive editing to edit each buffer in BUFFER-LIST."
  (mapcar (symbol-function 'edit-one-buffer) buffers))

(defun edit-all-buffers ()
  "Use recursive-edit to let (force?) the user to do things to each buffer
in turn."
  (interactive)
  (save-window-excursion
    (edit-buffers (buffer-list))))

(defun wander-safe ()
  "Allow the user to wander around in a recursive edit, then return
point, mark and windows to the way they were."
  (interactive)
  (let ((old-search-last-string search-last-string))
    (save-excursion
      (save-window-excursion
        (end-rec-edit-message "restore this position")
        (recursive-edit)))
    (setq search-last-string old-search-last-string)))

(defun scratch ()
  "Switch to the scratch buffer to do some interactive Lisp, in a
recursive edit. Exiting the recursive edit returns you to what you
were doing before."
  (interactive)
  (save-window-excursion
    (switch-to-buffer "*scratch*")
    (delete-other-windows)
    (save-excursion
      (goto-char (point-max))
      (end-rec-edit-message "exit scratch")
      (recursive-edit)
      (bury-buffer (current-buffer)))))

(defun in-one-window ()
  "Do something in a recursive edit in one window, restoring the old
window pattern at the end of the recursive edit."
  (interactive)
  (save-window-excursion
    (delete-other-windows)
    (save-excursion
      (end-rec-edit-message "get other windows back")
      (recursive-edit))))

;;; end of rec-edit.el
;--------------------------------- cut here for end of file -------------
;-- 
;__John            The Lord bless you and watch over you, The Lord make his face
;         shine upon you and be gracious to you, The Lord look kindly on you and
;   give you peace; My brothers, my sisters, God bless you. Amen.  (St. Francis)
;         jcgs@uk.co.harlqn Harlequin Ltd,Barrington,Cambridge,UK +44-223-872522

