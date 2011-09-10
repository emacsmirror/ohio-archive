;;; bookmark.el --- set bookmarks, jump to them later.

;; Copyright (C) 1993 Free Software Foundation

;; Author: Karl Fogel <kfogel@cs.oberlin.edu>
;; Maintainer: Karl Fogel <kfogel@cs.oberlin.edu>
;; Created: July, 1993
;; Version: 1.9.3
;; Keywords: bookmarks, placeholders

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; Thanks to David Bremner <bremner@cs.sfu.ca> for thinking of and
;; then implementing the bookmark-current-bookmark idea.  He even
;; sent *patches*, bless his soul... 

;; Thanks to Gregory M. Saunders <saunders@cis.ohio-state.edu> for
;; fixing and improving bookmark-time-to-save-p.

;; Based on info-bookmark.el, by Karl Fogel and Ken Olstad
;; <olstad@msc.edu>.

;; LCD Archive Entry:
;; bookmark|Karl Fogel|kfogel@cs.oberlin.edu|
;; Setting bookmarks in files or directories, jumping to them later.|
;; 16-July-93|Version: 1.9.3|~/misc/bookmark.el.Z|

;; FAVORITE CHINESE RESTAURANT: 
;; Boy, that's a tough one.  Probably Hong Min, or maybe Emperor's
;; Choice (both in Chicago's Chinatown).  Well, both.  How about you?

;;; Commentary on code:

;; bookmark alist format:
;;               (...
;;                (bookmark-name (filename
;;                                string-in-front
;;                                string-behind
;;                                point))
;;                ...)
;;
;; bookmark-name is the string the user gives the bookmark and
;; accesses it by from then on.  filename is the location of the file
;; in which the bookmark is set.  string-in-front is a string of
;; `bookmark-search-size' chars of context in front of the point the
;; bookmark is set at, string-behind is the same thing after the
;; point.  bookmark-jump will search for string-behind and
;; string-in-front in case the file has changed since the bookmark was
;; set.  It will attempt to place the user before the changes, if
;; there were any.
;;
;; It is not advisable to sort the bookmark list when it is presented
;; to the user, because it is already sorted in what is probably the
;; most useful way: order of creation, with most recently created
;; bookmarks coming first and older ones toward the end (renaming does
;; not count as creating) -- which is what we want, most of the time.

;;; Code:

;; Added  for lucid emacs  compatibility, db
(or (fboundp 'defalias)  (fset 'defalias 'fset))

;; these are the distribution keybindings suggested by RMS, everything
;; else will be done with M-x or the menubar:
(if (or (eq (key-binding "\C-xr") nil)
        (and (listp (key-binding "\C-xr"))
             (eq (car (key-binding "\C-xr")) 'keymap)))
    (progn (define-key ctl-x-map "rb" 'bookmark-jump)
           (define-key ctl-x-map "rm" 'bookmark-set)
           (define-key ctl-x-map "rl" 'list-bookmarks)))

;; define the map, so it can be bound by those who desire to do so:

(defvar bookmark-map nil
  "Keymap containing bindings to bookmark functions.
It is not bound to any key by default: to bind it
so that you have a bookmark prefix, just use `global-set-key' and bind a
key of your choice to `bookmark-map'.  All interactive bookmark
functions have a binding in this keymap.")

(define-prefix-command 'bookmark-map)

;; Read the help on all of these functions for details...
(define-key bookmark-map "x" 'bookmark-set)
(define-key bookmark-map "m" 'bookmark-set) ; "m" for "mark"
(define-key bookmark-map "j" 'bookmark-jump)
(define-key bookmark-map "g" 'bookmark-jump) ; "g" for "go"
(define-key bookmark-map "i" 'bookmark-insert)
(define-key bookmark-map "e" 'edit-bookmarks)
(define-key bookmark-map "f" 'bookmark-locate) ; "f" for "find"
(define-key bookmark-map "r" 'bookmark-rename)
(define-key bookmark-map "d" 'bookmark-delete)
(define-key bookmark-map "l" 'bookmark-load)
(define-key bookmark-map "w" 'bookmark-write)
(define-key bookmark-map "s" 'bookmark-save)

(defvar bookmark-alist ()
  "Association list of bookmarks.
You probably don't want to change the value of this alist yourself;
instead, let the various bookmark functions do it for you.")

(defvar bookmarks-already-loaded nil)

;; just add the hook to make sure that people don't lose bookmarks
;; when they kill Emacs, unless they don't want to save them.

(add-hook 'kill-emacs-hook
          (function
           (lambda ()
             (and (featurep 'bookmark)
                  bookmark-alist
                  (bookmark-time-to-save-p t)
                  (bookmark-save)))))

;; more stuff added by db.
(defvar bookmark-current-bookmark nil 
  "Name of bookmark most recently used in the current file.
It is buffer local, used to make moving a bookmark forward
through a file easier.")

(make-variable-buffer-local 'bookmark-current-bookmark)

(defvar bookmark-save-flag t
  "*Controls when Emacs saves bookmarks to a file.
--> Nil means never save bookmarks, except when `bookmark-save' is
    explicitly called \(\\[bookmark-save]\).
--> t means save bookmarks when Emacs is killed.
--> Otherise, it should be a number that is the frequency with which
    the bookmark list is saved \(i.e.: the number of times which
    Emacs' bookmark list may be modified before it is automatically
    saved.\).  If it is a number, Emacs will also automatically save
    bookmarks when it is killed.

Therefore, the way to get it to save every time you make or delete a
bookmark is to set this variable to 1 \(or 0, which produces the same
behavior.\)

To specify the file in which to save them, modify the variable
bookmark-file, which is `~/.emacs-bkmrks' by default.")

(defvar bookmark-alist-modification-count 0
  "Number of modifications to bookmark list since it was last saved.")

(defvar bookmark-file "~/.emacs-bkmrks" 
  "*File in which to save bookmarks by default.")

(defvar bookmark-completion-ignore-case t
  "*Non-nil means bookmark functions ignore case in completion.")

(defvar bookmark-search-size 500 
  "Length of the context strings recorded on either side of a bookmark.")

(defvar bookmark-current-point 0)
(defvar bookmark-yank-point 0)
(defvar bookmark-current-buffer nil)

(defun bookmark-set (&optional parg)
  "Set a bookmark named NAME inside a file.  
With prefix arg, will not overwrite a bookmark that has the same name
as NAME if such a bookmark already exists, but instead will \"push\"
the new bookmark onto the bookmark alist.  Thus the most recently set
bookmark with name NAME would be the one in effect at any given time,
but the others are still there, should you decide to delete the most
recent one.

To yank words from the text of the buffer and use them as part of the
bookmark name, type C-w while setting a bookmark.  Successive C-w's
yank successive words.

Typing C-v inserts the name of the current file being visited. Typing
C-u inserts the name of the last bookmark used in the buffer \(as an
aid in using a single bookmark name to track your progress through a
large file\).  If no bookmark was used, then C-u behaves like C-v and
inserts the name of the file being visited.

Use \\[bookmark-delete] to remove bookmarks \(you give it a name,
and it removes only the first instance of a bookmark with that name from
the list of bookmarks.\)"
  (interactive "P")
  (if (not (bookmark-buffer-file-name))
      (error "Buffer not visiting a file or directory."))
  (bookmark-try-default-file)
  (setq bookmark-current-point (point))
  (setq bookmark-yank-point (point))
  (setq bookmark-current-buffer (current-buffer))
  (let ((str
         (read-from-minibuffer
          "Set bookmark: "
	  nil
          (let ((now-map (copy-keymap minibuffer-local-map)))
            (progn (define-key now-map  "\C-w" 
                     'bookmark-yank-word)
                   (define-key now-map  "\C-v" 
                     'bookmark-insert-current-file-name)
                   (define-key now-map  "\C-u" 
                     'bookmark-insert-current-bookmark))
	    now-map))))
    (progn
      (bookmark-make parg str)
      (setq bookmark-current-bookmark str)
      (if (get-buffer "*Bookmark List*") ;rebuild the bookmark list
          (save-excursion (list-bookmarks)))
      (goto-char bookmark-current-point))))

(defun bookmark-insert-current-bookmark ()
  ;; insert this buffer's value of bookmark-current-bookmark, default
  ;; to file name if it's nil.
  (interactive)
  (let ((str
	 (save-excursion
	   (set-buffer bookmark-current-buffer)
	   bookmark-current-bookmark)))
    (if str (insert str) (bookmark-insert-current-file-name))))

(defun bookmark-insert-current-file-name ()
  ;; insert the name (sans path) of the current file into the bookmark
  ;; name that is being set.
  (interactive)
  (let ((str (save-excursion
	       (set-buffer bookmark-current-buffer)
	       (bookmark-buffer-file-name))))
    (insert (substring 
	     str
	     (1+ (string-match 
		  "\\(/[^/]*\\)/*$"
		  str))))))

(defun bookmark-yank-word ()
  (interactive)
  ;; get the next word from the buffer and append it to the name of
  ;; the bookmark currently being set.
  (let ((string (save-excursion
                  (set-buffer bookmark-current-buffer)
                  (goto-char bookmark-yank-point)
                  (buffer-substring
                   (point)
                   (save-excursion
                     (forward-word 1)
                     (setq bookmark-yank-point (point)))))))
    (insert string)))

(defun bookmark-make (parg str)
  (if (and (assoc str bookmark-alist) (not parg))
      ;; already existing boookmark under that name and
      ;; no prefix arg means just overwrite old bookmark
      (setcdr (assoc str bookmark-alist)
              (list (bookmark-make-cell)))
    
    ;; otherwise just cons it onto the front (either the bookmark
    ;; doesn't exist already, or there is no prefix arg.  In either
    ;; case, we want the new bookmark consed onto the alist...)
    
    (setq bookmark-alist
          (cons
           (list str 
                 (bookmark-make-cell))
           bookmark-alist)))
  ;; Added by db
  (setq bookmark-current-bookmark str)
  (setq bookmark-alist-modification-count
        (1+ bookmark-alist-modification-count))
  (if (bookmark-time-to-save-p)
      (bookmark-save)))

(defun bookmark-make-cell ()
  ;; make the cell that is the cdr of a bookmark alist element.  It
  ;; looks like this:
  ;; (filename search-forward-str search-back-str point)
  (list
   (bookmark-buffer-file-name)
   (if (>= (- (point-max) (point)) bookmark-search-size)
       (buffer-substring 
        (point)
        (+ (point) bookmark-search-size))
     nil)
   (if (>= (- (point) (point-min)) bookmark-search-size)
       (buffer-substring 
        (point)
        (- (point) bookmark-search-size))
     nil)
   (point)))

(defun bookmark-buffer-file-name ()
  (or
   buffer-file-name
   (if (and (boundp 'dired-directory) dired-directory)
       (if (stringp dired-directory)
	   dired-directory
	 (car dired-directory)))))

(defun bookmark-try-default-file ()
  (if (and (not bookmarks-already-loaded)
           (null bookmark-alist)
	   (file-readable-p (expand-file-name bookmark-file)))
      (progn
        (bookmark-load bookmark-file)
        (setq bookmarks-already-loaded t))))

(defun bookmark-jump (str)
  "Jump to bookmark BOOKMARK (a point in some file).  
You may have a problem using this function if the value of variable
`bookmark-alist' is nil.  If that happens, you need to load in some
bookmarks.  See help on function `bookmark-load' for more about
this."
    (interactive (let ((completion-ignore-case
                        bookmark-completion-ignore-case))
                   (progn (bookmark-try-default-file)
                          (list (completing-read
                                 "Jump to bookmark: "
                                 bookmark-alist
                                 nil
                                 0)))))
    (let ((whereto-list (car (cdr (assoc str bookmark-alist)))))
      (let ((file (car whereto-list))
            (forward-str (car (cdr whereto-list)))
            (behind-str (car (cdr (cdr whereto-list))))
            (place (car (cdr (cdr (cdr whereto-list))))))
        (if (file-exists-p (expand-file-name file))
            (progn 
              (find-file (expand-file-name file))
              (goto-char place)
              ;; Go searching forward first.  Then, if forward-str exists and
              ;; was found in the file, we can search backward for behind-str.
              ;; Rationale is that if text was inserted between the two in the
              ;; file, it's better to be put before it so you can read it,
              ;; rather than after and remain perhaps unaware of the changes.
              (if forward-str
                  (if (search-forward forward-str (point-max) t)
                      (backward-char bookmark-search-size)))
              (if behind-str
                  (if (search-backward behind-str (point-min) t)
                      (forward-char bookmark-search-size)))
              ;; added by db
              (setq bookmark-current-bookmark str))
          (error 
           (concat "File "
                   file
                   " does not exist.  Suggest deleting bookmark \""
                   str
                   "\""))))))

(defun bookmark-locate (str &optional no-insertion)
  "Insert the name of the file associated with BOOKMARK.
Optional second arg NO-INSERTION means merely return the filename as a
string."
  (interactive (let ((completion-ignore-case
                      bookmark-completion-ignore-case))
                 (progn (bookmark-try-default-file)
                        (list (completing-read
                               "Insert bookmark location: "
                               bookmark-alist
                               nil
                               0)))))
  (let ((where (car (car (cdr (assoc str bookmark-alist))))))
    (if no-insertion
        where
      (insert where))))

(defun bookmark-rename (old &optional new)
  "Change the name of OLD-BOOKMARK to NEWNAME.  
If called from keyboard, prompts for OLD-BOOKMARK and NEWNAME.
If called from menubar, OLD-BOOKMARK is selected from a menu, and
prompts for NEWNAME. 
If called from Lisp, prompts for NEWNAME if only OLD-BOOKMARK was
passed as an argument.  If called with two strings, then no prompting
is done.  You must pass at least OLD-BOOKMARK when calling from Lisp.

While you are entering the new name, consecutive C-w's insert
consectutive words from the text of the buffer into the new bookmark
name, and C-v inserts the name of the file."
  (interactive (let ((completion-ignore-case
                      bookmark-completion-ignore-case))
                 (progn (bookmark-try-default-file)
                        (list (completing-read "Old bookmark name: "
                                               bookmark-alist
                                               nil
                                               0)))))
  (progn
    (setq bookmark-current-point (point))
    (setq bookmark-yank-point (point))
    (setq bookmark-current-buffer (current-buffer))
    (let ((cell (assoc old bookmark-alist))
	  (str
           (or new   ; use second arg, if non-nil
               (read-from-minibuffer 
                "New name: "
                nil
                (let ((now-map (copy-keymap minibuffer-local-map)))
                  (progn (define-key now-map  "\C-w" 
                           'bookmark-yank-word)
                         (define-key now-map  "\C-v" 
                           'bookmark-insert-current-file-name))
                  now-map)))))
      (progn
	(setcar cell str)
	(setq bookmark-current-bookmark str)
        (if (get-buffer "*Bookmark List*")
            (save-excursion (list-bookmarks)))
	(setq bookmark-alist-modification-count
	      (1+ bookmark-alist-modification-count))
	(if (bookmark-time-to-save-p)
	    (bookmark-save))))))

(defun bookmark-insert (str)
  "Insert the text of the file pointed to by bookmark BOOKMARK.  
You may have a problem using this function if the value of variable
`bookmark-alist' is nil.  If that happens, you need to load in some
bookmarks.  See help on function `bookmark-load' for more about
this."
  (interactive (let ((completion-ignore-case
                      bookmark-completion-ignore-case))
                 (progn (bookmark-try-default-file)
                        (list (completing-read
                               "Insert bookmark contents: "
                               bookmark-alist
                               nil
                               0)))))
  (let ((whereto-list (car (cdr (assoc str bookmark-alist)))))
    (let ((file (car whereto-list)))
      (if (file-readable-p (expand-file-name file))
	  (let ((str-to-insert
		 (save-excursion
		   (find-file (expand-file-name file))
		   (prog1
		       (buffer-substring (point-min) (point-max))
		     (bury-buffer))))
		(orig-point (point)))
	    (insert str-to-insert)
	    (push-mark)	    
	    (goto-char orig-point))
	(error	
	 (concat "File "
                 file
                 " does not exist.  Suggest deleting bookmark \""
                 str
                 "\""))))))

(defun bookmark-delete (str)
  "Delete the bookmark named NAME from the bookmark list.  
Removes only the first instance of a bookmark with that name.  If
there are one or more other bookmarks with the same name, they will
not be deleted.  Defaults to the \"current\" bookmark \(that is, the
one most recently used in this file, if any\)."
  (interactive (let ((completion-ignore-case
		      bookmark-completion-ignore-case))
		 (progn (bookmark-try-default-file)
                        (list
                         (completing-read
                          "Delete bookmark: "
                          bookmark-alist
                          nil
                          0
                          bookmark-current-bookmark)))))
  (let ((will-go (assoc str bookmark-alist)))
    (setq bookmark-alist (delq will-go bookmark-alist))
    ;; Added by db, nil bookmark-current-bookmark if the last
    ;; occurence has been deleted
    (or (assoc bookmark-current-bookmark bookmark-alist)
        (setq bookmark-current-bookmark nil)))
  (if (get-buffer "*Bookmark List*") 
      (save-excursion (list-bookmarks)))
  (setq bookmark-alist-modification-count
        (1+ bookmark-alist-modification-count))
  (if (bookmark-time-to-save-p)
      (bookmark-save)))

(defun bookmark-time-to-save-p (&optional last-time)
  ;; By Gregory M. Saunders <saunders@cis.ohio-state.edu>
  ;; finds out whether it's time to save bookmarks to a file, by
  ;; examining the value of variable bookmark-save-flag, and maybe
  ;; bookmark-alist-modification-count.  Returns t if they should be
  ;; saved, nil otherwise.  if last-time is non-nil, then this is
  ;; being called when emacs is killed.
  (cond (last-time 
	 (and (> bookmark-alist-modification-count 0)
	      bookmark-save-flag))
	((numberp bookmark-save-flag)
	 (>= bookmark-alist-modification-count bookmark-save-flag))
	(t
	 nil)))

(defun bookmark-write ()
  "Write bookmarks to a file \(for which the user will be prompted
interactively\).  Don't use this in Lisp programs; use bookmark-save
instead."
  (interactive)
  (bookmark-try-default-file)
  (bookmark-save t))

(defun bookmark-save (&optional parg file) 
  "Save currently defined bookmarks.
Saves by default in the file defined by the variable
`bookmark-file'.  With a prefix arg, save it in file FILE.

If you are calling this from Lisp, the two arguments are PREFIX-ARG
and FILE, and if you just want it to write to the default file, then
pass no arguments.  Or pass in nil and FILE, and it will save in FILE
instead.  If you pass in one argument, and it is non-nil, then the
user will be interactively queried for a file to save in.

When you want to load in the bookmarks from a file, use
\`bookmark-load\', \\[bookmark-load].  That function will prompt you
for a file, defaulting to the file defined by variable
`bookmark-file'."
  (interactive "P")
  (bookmark-try-default-file)
  (cond
   ((and (null parg) (null file))
    ;;whether interactive or not, write to default file
    (bookmark-write-file bookmark-file))
   ((and (null parg) file)
    ;;whether interactive or not, write to given file
    (bookmark-write-file file))
   ((and parg (not file))
    ;;have been called interactively w/ prefix arg
    (let ((file (read-file-name "File to save bookmarks in: ")))
      (bookmark-write-file file)))
   (t ; someone called us with prefix-arg *and* a file, so just write to file
    (bookmark-write-file file)))
  ;; signal that we have synced the bookmark file by setting this to
  ;; 0.  If there was an error at any point before, it will not get
  ;; set, which is what we want.
  (setq bookmark-alist-modification-count 0))

(defun bookmark-write-file (file)
  (save-excursion
    (if (>= baud-rate 9600)
        (message (format "Saving bookmarks to file %s." file)))
    (set-buffer (find-file-noselect file))
    (goto-char (point-min))
    (delete-region (point-min) (point-max))
    (print bookmark-alist (current-buffer))
    (write-file file)
    (kill-buffer (current-buffer))))

(defun bookmark-load (file &optional revert no-msg)
  "Load bookmarks from FILE (which must be in bookmark format).
Appends loaded bookmarks to the front of the list of bookmarks.  If
optional second argument REVERT is non-nil, existing bookmarks are
destroyed.  Optional third arg NO-MSG means don't display any messages
while loading.

If you load a file that doesn't contain a proper bookmark alist, you
will corrupt Emacs's bookmark list.  Generally, you should only load
in files that were created with the bookmark functions in the first
place.  Your own personal bookmark file, `~/.emacs-bkmrks', is
maintained automatically by Emacs; you shouldn't need to load it
explicitly."
  (interactive
   (progn (bookmark-try-default-file)
          (list (read-file-name
                 (format "Load bookmarks from: (%s) "
                         bookmark-file)        
                 ;;Default might not be used often,
                 ;;but there's no better default, and
                 ;;I guess it's better than none at all.
                 "~/" bookmark-file 'confirm))))
  (setq file (expand-file-name file))
  (if (file-readable-p file)
      (save-excursion
        (if (and (null no-msg) (>= baud-rate 9600))
            (message (format "Loading bookmarks from %s..." file)))
        (set-buffer (find-file-noselect file))
        (goto-char (point-min))
        (let ((blist (car (read-from-string
                           (buffer-substring (point-min) (point-max))))))
          (if (listp blist)
              (progn
                (if (not revert)
                    (setq bookmark-alist-modification-count
                          (1+ bookmark-alist-modification-count))
                  (setq bookmark-alist-modification-count 0))
                (setq bookmark-alist
                      (append blist (if (not revert) bookmark-alist)))
                (if (get-buffer "*Bookmark List*") 
                    (save-excursion (list-bookmarks)))) 
	    (error (format "Invalid bookmark list in %s." file))))
	(kill-buffer (current-buffer))
	(if (and (null no-msg) (>= baud-rate 9600))
            (message (format "Loading bookmarks from %s... done" file))))
    (error (format "Cannot read bookmark file %s." file))))

;;;; bookmark-menu-mode stuff ;;;;

(defvar Bookmark-menu-bookmark-column nil)

(defvar Bookmark-menu-hidden-bookmarks ())

(defvar Bookmark-menu-file-column 30
  "*Column at which to display filenames in a buffer listing bookmarks.
You can toggle whether files are shown with \\<Bookmark-menu-mode-map>\\[Bookmark-menu-toggle-filenames].")

(defvar Bookmark-menu-toggle-filenames t
  "*Non-nil means show filenames when listing bookmarks.
This may result in truncated bookmark names.  To disable this, put the
following in your .emacs:

\(setq Bookmark-menu-toggle-filenames nil\)")

(defvar Bookmark-menu-mode-map nil "")

(if Bookmark-menu-mode-map
    nil
  (setq Bookmark-menu-mode-map (make-keymap))
  (suppress-keymap Bookmark-menu-mode-map t)
  (define-key Bookmark-menu-mode-map "q" 'Bookmark-menu-quit)
  (define-key Bookmark-menu-mode-map "v" 'Bookmark-menu-select)
  (define-key Bookmark-menu-mode-map "w" 'Bookmark-menu-locate)
  (define-key Bookmark-menu-mode-map "2" 'Bookmark-menu-2-window)
  (define-key Bookmark-menu-mode-map "1" 'Bookmark-menu-1-window)
  (define-key Bookmark-menu-mode-map "j" 'Bookmark-menu-this-window)
  (define-key Bookmark-menu-mode-map "f" 'Bookmark-menu-this-window)
  (define-key Bookmark-menu-mode-map "o" 'Bookmark-menu-other-window)
  (define-key Bookmark-menu-mode-map "\C-o" 'Bookmark-menu-switch-other-window)
  (define-key Bookmark-menu-mode-map "s" 'Bookmark-menu-save)
  (define-key Bookmark-menu-mode-map "k" 'Bookmark-menu-delete)
  (define-key Bookmark-menu-mode-map "\C-d" 'Bookmark-menu-delete-backwards)
  (define-key Bookmark-menu-mode-map "x" 'Bookmark-menu-execute)
  (define-key Bookmark-menu-mode-map "\C-k" 'Bookmark-menu-delete)
  (define-key Bookmark-menu-mode-map "d" 'Bookmark-menu-delete)
  (define-key Bookmark-menu-mode-map " " 'next-line)
  (define-key Bookmark-menu-mode-map "n" 'next-line)
  (define-key Bookmark-menu-mode-map "p" 'previous-line)
  (define-key Bookmark-menu-mode-map "\177" 'Bookmark-menu-backup-unmark)
  (define-key Bookmark-menu-mode-map "?" 'describe-mode)
  (define-key Bookmark-menu-mode-map "u" 'Bookmark-menu-unmark)
  (define-key Bookmark-menu-mode-map "m" 'Bookmark-menu-mark)
  (define-key Bookmark-menu-mode-map "l" 'Bookmark-menu-load) 
  (define-key Bookmark-menu-mode-map "r" 'Bookmark-menu-rename)
  (define-key Bookmark-menu-mode-map "t" 'Bookmark-menu-toggle-filenames))

;; Bookmark Menu mode is suitable only for specially formatted data.
(put 'Bookmark-menu-mode 'mode-class 'special)

;; need to display whether or not bookmark exists as a buffer in flag
;; column. 

;; do save and load!

;; add hook to check for existence of Bookmark-list-buffer and rebuild
;; it if so to every function that modifies the bookmark list.

;; Format:
;; FLAGS  BOOKMARK (/FILE/NAME/HERE/WHAT/REGEXP/TO/USE?)
;; goto bookmark-column and then search till "(/[^)]*)$" or "(/.*)$" ? 

;; need to rename, delete, jump to, write list to another file (queries
;; whether to do deletes first), load a bookmark file (rebuilds menu).
;; Columns: status, file, bookmark?  Copy bookmark to somewhere...

;; "l" displays filenames, "t" toggles filename displaying?  Right now
;; "t" does vists-tags-table

(defalias 'edit-bookmarks 'list-bookmarks)

(defun list-bookmarks ()
  "Display a list of existing bookmarks.
The list is displayed in a buffer named `*Bookmark List*'.
The leftmost column displays a D if the bookmark is flagged for
deletion, or > if it is flagged for displaying."
  (interactive)
  (bookmark-try-default-file)
  (switch-to-buffer (get-buffer-create "*Bookmark List*"))
  (let ((buffer-read-only nil))
    (delete-region (point-max) (point-min))
    (goto-char (point-min)) ;sure are playing it safe...
    (insert "% Bookmark\n- --------\n")
    (let ((lst bookmark-alist))
      (while lst
        (insert
         (concat "  " (car (car lst)) "\n"))
        (setq lst (cdr lst)))))
  (goto-char (point-min))
  (forward-line 2)
  (bookmark-menu-mode)
  (if Bookmark-menu-toggle-filenames
      (Bookmark-menu-toggle-filenames t)))

(defun bookmark-menu-mode ()
  "Major mode for editing a list of bookmarks.
Each line describes one of the bookmarks in Emacs.
Letters do not insert themselves; instead, they are commands.
\\<Bookmark-menu-mode-map>
\\[Bookmark-menu-mark] -- mark bookmark to be displayed.
\\[Bookmark-menu-select] -- select bookmark of line point is on.
  Also show bookmarks marked using m in other windows.
\\[Bookmark-menu-toggle-filenames] -- toggle displaying of filenames (they may obscure long bookmark names).
\\[Bookmark-menu-locate] -- display (in minibuffer) location of this bookmark.
\\[Bookmark-menu-1-window] -- select this bookmark in full-frame window.
\\[Bookmark-menu-2-window] -- select this bookmark in one window,
  together with bookmark selected before this one in another window.
\\[Bookmark-menu-this-window] -- select this bookmark in place of the bookmark menu buffer.
\\[Bookmark-menu-other-window] -- select this bookmark in another window,
  so the bookmark menu bookmark remains visible in its window.
\\[Bookmark-menu-switch-other-window] -- switch the other window to this bookmark.
\\[Bookmark-menu-rename] -- rename this bookmark \(prompts for new name\).   
\\[Bookmark-menu-delete] -- mark this bookmark to be deleted, and move down.
\\[Bookmark-menu-delete-backwards] -- mark this bookmark to be deleted, and move up. 
\\[Bookmark-menu-execute] -- delete marked bookmarks.
\\[Bookmark-menu-save] -- save the current bookmark list in the default file.
  With a prefix arg, prompts for a file to save in.
\\[Bookmark-menu-load] -- load in a file of bookmarks (prompts for file.)
\\[Bookmark-menu-unmark] -- remove all kinds of marks from current line.
  With prefix argument, also move up one line.
\\[Bookmark-menu-backup-unmark] -- back up a line and remove marks."
  (kill-all-local-variables)
  (use-local-map Bookmark-menu-mode-map)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq major-mode 'bookmark-menu-mode)
  (setq mode-name "Bookmark Menu")
  (run-hooks 'bookmark-menu-mode-hook))

(defun Bookmark-menu-toggle-filenames (&optional parg)
  "Toggle whether filenames are shown in the bookmark list.
Optional argument SHOW means show them unconditionally."
  (interactive)
  (cond
   (parg
    (setq Bookmark-menu-toggle-filenames nil)
    (Bookmark-menu-show-filenames)
    (setq Bookmark-menu-toggle-filenames t))
   (Bookmark-menu-toggle-filenames
    (Bookmark-menu-hide-filenames)
    (setq Bookmark-menu-toggle-filenames nil))
   (t
    (Bookmark-menu-show-filenames)
    (setq Bookmark-menu-toggle-filenames t))))

(defun Bookmark-menu-show-filenames (&optional force)
  (if (and (not force) Bookmark-menu-toggle-filenames)
      nil ;already shown, so do nothing
    (save-excursion
      (goto-char (point-min))
      (forward-line 2)
      (setq Bookmark-menu-hidden-bookmarks ())
      (let ((buffer-read-only nil))
        (while (< (point) (point-max))
          (let ((bmrk (Bookmark-menu-bookmark)))
            (setq Bookmark-menu-hidden-bookmarks
                  (cons bmrk Bookmark-menu-hidden-bookmarks))
            (move-to-column Bookmark-menu-file-column t)
            (delete-region (point) (progn (end-of-line) (point)))
            (insert "  ")
            (bookmark-locate bmrk)
            (forward-line 1)))))))

(defun Bookmark-menu-hide-filenames (&optional force)
  (if (and (not force) Bookmark-menu-toggle-filenames)
      ;; nothing to hide if above is nil
      (save-excursion
        (goto-char (point-min))
        (forward-line 2)
        (setq Bookmark-menu-hidden-bookmarks
              (nreverse Bookmark-menu-hidden-bookmarks))
        (save-excursion
          (goto-char (point-min))
          (search-forward "Bookmark")
          (backward-word 1)
          (setq Bookmark-menu-bookmark-column (current-column)))
        (save-excursion
          (let ((buffer-read-only nil))
            (while Bookmark-menu-hidden-bookmarks
              (move-to-column Bookmark-menu-bookmark-column t)
              (kill-line)
              (insert (car Bookmark-menu-hidden-bookmarks))
              (setq Bookmark-menu-hidden-bookmarks
                    (cdr Bookmark-menu-hidden-bookmarks))
              (forward-line 1)))))))

(defun Bookmark-menu-bookmark ()
  ;; return a string which is bookmark of this line.
  (if (< (count-lines (point-min) (point)) 2)
      (progn (ding)
             (goto-char (point-min)) 
             (forward-line 2))
    (save-excursion
      (goto-char (point-min))
      (search-forward "Bookmark")
      (backward-word 1)
      (setq Bookmark-menu-bookmark-column (current-column)))
    (if Bookmark-menu-toggle-filenames
        (Bookmark-menu-hide-filenames))
    (save-excursion
      (beginning-of-line)
      (forward-char Bookmark-menu-bookmark-column)
      (prog1
          (buffer-substring (point)
                            (progn 
                              (end-of-line)
                              (point)))
        ;; well, this is certainly crystal-clear:
        (if Bookmark-menu-toggle-filenames
            (Bookmark-menu-toggle-filenames t))))))
  
(defun Bookmark-menu-mark ()
  "Mark bookmark on this line to be displayed by \\<Bookmark-menu-mode-map>\\[Bookmark-menu-select] command."
  (interactive)
  (beginning-of-line)
  (if (< (count-lines (point-min) (point)) 2)
      (progn (ding)
             (goto-char (point-min)) 
             (forward-line 2))
    (let ((buffer-read-only nil))
      (delete-char 1)
      (insert ?>)
      (forward-line 1))))

(defun Bookmark-menu-select ()
  "Select this line's bookmark; also display bookmarks marked with `>'.
You can mark bookmarks with the \\<Bookmark-menu-mode-map>\\[Bookmark-menu-mark] command."
  (interactive)
  (if (< (count-lines (point-min) (point)) 2)
      (progn (ding)
             (goto-char (point-min)) 
             (forward-line 2))
    (let ((bmrk (Bookmark-menu-bookmark))
          (menu (current-buffer))	      
          (others ())
          tem)
      (goto-char (point-min))
      (while (re-search-forward "^>" nil t)
        (setq tem (Bookmark-menu-bookmark))
        (let ((buffer-read-only nil))
          (delete-char -1)
          (insert ?\ ))
        (or (eq tem bmrk) (memq tem others) (setq others (cons tem others))))
      (setq others (nreverse others)
            tem (/ (1- (frame-height)) (1+ (length others))))
      (delete-other-windows)
      (bookmark-jump bmrk)
      (bury-buffer menu)
      (if (equal (length others) 0)
          nil
        (while others
          (split-window nil tem)
          (other-window 1)
          (bookmark-jump (car others))
          (setq others (cdr others)))
        (other-window 1)))))

(defun Bookmark-menu-save (parg)
  "Save the current list into a bookmark file.
With a prefix arg, prompts for a file to save them in."
  (interactive "P")
  (save-excursion
    (bookmark-save parg)))

(defun Bookmark-menu-load ()
  "Load a bookmark file and rebuild list."
  (interactive)
  (if (< (count-lines (point-min) (point)) 2)
      (progn (ding)
             (goto-char (point-min)) 
             (forward-line 2))
    (save-excursion
      (call-interactively 'bookmark-load))))

(defun Bookmark-menu-1-window ()
  "Select this line's bookmark, alone, in full frame."
  (interactive)
  (if (< (count-lines (point-min) (point)) 2)
      (progn (ding)
             (goto-char (point-min)) 
             (forward-line 2))
    (bookmark-jump (Bookmark-menu-bookmark))
    (bury-buffer (other-buffer))
    (delete-other-windows)))

(defun Bookmark-menu-2-window ()
  "Select this line's bookmark, with previous buffer in second window."
  (interactive)
  (if (< (count-lines (point-min) (point)) 2)
      (progn (ding)
             (goto-char (point-min)) 
             (forward-line 2))
    (let ((bmrk (Bookmark-menu-bookmark))
          (menu (current-buffer))
          (pop-up-windows t))
      (delete-other-windows)
      (switch-to-buffer (other-buffer))
      (let ((buff (save-excursion
                    (bookmark-jump bmrk)
                    (current-buffer))))
        (pop-to-buffer buff))
      (bury-buffer menu))))

(defun Bookmark-menu-this-window ()
  "Select this line's bookmark in this window."
  (interactive)
  (if (< (count-lines (point-min) (point)) 2)
      (progn (ding)
             (goto-char (point-min)) 
             (forward-line 2))
    (bookmark-jump (Bookmark-menu-bookmark))))

(defun Bookmark-menu-other-window ()
  "Select this line's bookmark in other window, leaving bookmark menu visible."
  (interactive)
  (if (< (count-lines (point-min) (point)) 2)
      (progn (ding)
             (goto-char (point-min)) 
             (forward-line 2))
    (let ((buff (save-excursion
                  (bookmark-jump (Bookmark-menu-bookmark))
                  (current-buffer))))
      (switch-to-buffer-other-window buff))))

(defun Bookmark-menu-switch-other-window ()
  "Make the other window select this line's bookmark.
The current window remains selected."
  (interactive)
  (if (< (count-lines (point-min) (point)) 2)
      (progn (ding)
             (goto-char (point-min)) 
             (forward-line 2))
    (let ((buff (save-excursion
                  (bookmark-jump (Bookmark-menu-bookmark))
                  (current-buffer))))
      (display-buffer buff))))

(defun Bookmark-menu-quit ()
  "Quit the bookmark menu."
  (interactive)
  (let ((buffer (current-buffer)))
    ;; Restore previous window configuration before displaying
    ;; selected buffers.
    (switch-to-buffer (other-buffer))
    (bury-buffer buffer)))

(defun Bookmark-menu-unmark (&optional backup)
  "Cancel all requested operations on bookmark on this line and move down.
Optional ARG means move up."
  (interactive "P")
  (beginning-of-line)
  (if (< (count-lines (point-min) (point)) 2)
      (progn (ding)
             (goto-char (point-min)) 
             (forward-line 2))
    (let ((buffer-read-only nil))
      (delete-char 1)
      ;; any flags to reset according to circumstances?  How about a
      ;; flag indicating whether this bookmark is being visited?
      (insert " "))
    (forward-line (if backup -1 1))))

(defun Bookmark-menu-backup-unmark ()
  "Move up and cancel all requested operations on bookmark on line above."
  (interactive)
  (forward-line -1)
  (if (< (count-lines (point-min) (point)) 2)
      (progn (ding)
             (goto-char (point-min)) 
             (forward-line 2))
    (Bookmark-menu-unmark)
    (forward-line -1)))

(defun Bookmark-menu-delete ()
  "Mark bookmark on this line to be deleted by \\<Bookmark-menu-mode-map>\\[Bookmark-menu-execute] command."
  (interactive)
  (beginning-of-line)
  (if (< (count-lines (point-min) (point)) 2)
      (progn (ding)
             (goto-char (point-min)) 
             (forward-line 2))
    (let ((buffer-read-only nil))
      (delete-char 1)
      (insert ?D)
      (forward-line 1))))

(defun Bookmark-menu-delete-backwards ()
  "Mark bookmark on this line to be deleted by \\<Bookmark-menu-mode-map>\\[Bookmark-menu-execute] command
and then move up one line"
  (interactive)
  (Bookmark-menu-delete)
  (forward-line -2)
  (if (< (count-lines (point-min) (point)) 2)
      (progn (ding)
             (goto-char (point-min)) 
             (forward-line 2))
    (forward-line 1)))

(defun Bookmark-menu-execute ()
  "Delete bookmarks marked with or \\<Buffer-menu-mode-map>\\[Buffer-menu-delete] commands."
  (interactive)
  (goto-char (point-min))
  (forward-line 1)
  (let ((deaders ()))
    (while (re-search-forward "^D" (point-max) t)
      (setq deaders (cons (Bookmark-menu-bookmark) deaders)))
    (mapcar (lambda (str) (bookmark-delete str))
            deaders))
  (goto-char (point-min))
  (forward-line 2))

(defun Bookmark-menu-rename ()
  "Rename bookmark on current line.  Prompts for a new name."
  (interactive)
  (if (< (count-lines (point-min) (point)) 2)
      (progn (ding)
             (goto-char (point-min)) 
             (forward-line 2))
    (let ((bmrk (Bookmark-menu-bookmark))
          (thispoint (point)))
      (bookmark-rename bmrk)
      (list-bookmarks)
      (goto-char thispoint))))

(defun Bookmark-menu-locate ()
  "Display location of this bookmark.  Displays in the minibuffer."
  (interactive)
  (if (< (count-lines (point-min) (point)) 2)
      (progn (ding)
             (goto-char (point-min)) 
             (forward-line 2))
    (let ((bmrk (Bookmark-menu-bookmark)))
      (message (bookmark-locate bmrk t)))))

;;;; bookmark menu bar stuff ;;;;

(defvar bookmark-menu-bar-length 70 
  "*Maximum length of a bookmark name displayed on a popup menu.")

(defvar bookmark-enable-menu-bar t
  "*Non-nil means put a bookmark menu on the menu bar.
\(Assuming that you are running Emacs under a windowing system, such
as X.\)")

(defun bookmark-make-menu-bar-alist ()
  (bookmark-try-default-file)
  (if bookmark-alist
      (mapcar (lambda (cell)
		(let ((str (car cell)))
		  (cons 
		   (if (> (length str) bookmark-menu-bar-length)
		       (substring str 0 bookmark-menu-bar-length)
		     str)
		   str)))
	      bookmark-alist)
    (error "No bookmarks currently set.")))

(defun bookmark-make-menu-bar-with-function (func-sym 
                                             menu-label
                                             menu-str event) 
  ;; help function for making menus that need to apply a bookmark
  ;; function to a string.
  (let* ((menu (bookmark-make-menu-bar-alist))
	 (str (x-popup-menu event
			    (list menu-label
                                  (cons menu-str menu)))))
    (if str (apply func-sym (list str)))))

(defun bookmark-menu-bar-insert (event)
  "Insert the text of the file pointed to by bookmark BOOKMARK.  
You may have a problem using this function if the value of variable
`bookmark-alist' is nil.  If that happens, you need to load in some
bookmarks.  See help on function `bookmark-load' for more about
this."
  (interactive "e")
  (bookmark-make-menu-bar-with-function 'bookmark-insert
				    "Bookmark Insert Menu"
				    "--- Insert Contents ---"
				    event))

(defun bookmark-menu-bar-jump (event)
  "Jump to bookmark BOOKMARK (a point in some file).  
You may have a problem using this function if the value of variable
`bookmark-alist' is nil.  If that happens, you need to load in some
bookmarks.  See help on function `bookmark-load' for more about
this."
  (interactive "e")
  (bookmark-make-menu-bar-with-function 'bookmark-jump
				    "Bookmark Jump Menu"
				    "--- Jump to Bookmark ---"
				    event))

(defun bookmark-menu-bar-locate (event)
  "Insert the name of the  file associated with BOOKMARK. 
\(This is not the same as the contents of that file\)."
  (interactive "e")
  (bookmark-make-menu-bar-with-function 'bookmark-locate
				    "Bookmark Locate Menu"
				    "--- Insert Location ---"
				    event))

(defun bookmark-menu-bar-rename (event)
  "Change the name of OLD-BOOKMARK to NEWNAME.  
If called from keyboard, prompts for OLD-BOOKMARK and NEWNAME.
If called from menubar, OLD-BOOKMARK is selected from a menu, and
prompts for NEWNAME. 
If called from Lisp, prompts for NEWNAME if only OLD-BOOKMARK was
passed as an argument.  If called with two strings, then no prompting
is done.  You must pass at least OLD-BOOKMARK when calling from Lisp.

While you are entering the new name, consecutive C-w's insert
consectutive words from the text of the buffer into the new bookmark
name, and C-v inserts the name of the file."
  (interactive "e")
  (bookmark-make-menu-bar-with-function 'bookmark-rename
				    "Bookmark Rename Menu"
				    "--- Rename Bookmark ---"
				    event))

(defun bookmark-menu-bar-delete (event)
  "Delete the bookmark named NAME from the bookmark list.  
Removes only the first instance of a bookmark with that name.  If
there are one or more other bookmarks with the same name, they will
not be deleted.  Defaults to the \"current\" bookmark \(that is, the
one most recently used in this file, if any\)."
  (interactive "e")
  (bookmark-make-menu-bar-with-function 'bookmark-delete
				    "Bookmark Delete Menu"
				    "--- Delete Bookmark ---"
				    event))

(if (and bookmark-enable-menu-bar window-system)
    (progn
      (defvar menu-bar-bookmark-map 
	(make-sparse-keymap "Bookmark functions"))
      
      ;; make bookmarks appear toward the right side of the menu.
      (if (boundp 'menu-bar-final-items)
	  (if menu-bar-final-items
	      (setq menu-bar-final-items 
		    (cons 'bookmark menu-bar-final-items)))
	(setq menu-bar-final-items '(bookmark)))

      (define-key global-map [menu-bar bookmark]
	(cons "Bookmarks" menu-bar-bookmark-map))

      (define-key menu-bar-bookmark-map [load]
	'(" Load a bookmark file" . bookmark-load))

      (define-key menu-bar-bookmark-map [write]
	'("Write \(to another file\)" . bookmark-write))

      (define-key menu-bar-bookmark-map [save]
	'("Save  \(in default file\)" . bookmark-save))
      
      (define-key menu-bar-bookmark-map [edit]
	'("  Edit Bookmark List" . list-bookmarks))

      (define-key menu-bar-bookmark-map [delete]
	'("  Delete a bookmark" . bookmark-menu-bar-delete))
      
      (define-key menu-bar-bookmark-map [rename]
	'("   Rename bookmark" . bookmark-menu-bar-rename))
      
      (define-key menu-bar-bookmark-map [locate]
	'("   Insert location" . bookmark-menu-bar-locate))
      
      (define-key menu-bar-bookmark-map [insert]
	'("   Insert contents" . bookmark-menu-bar-insert))
      
      (define-key menu-bar-bookmark-map [set]
	'("    Set bookmark" . bookmark-set))
      
      (define-key menu-bar-bookmark-map [jump]
	'("   Go to bookmark" . bookmark-menu-bar-jump))))

;; not using properties because they make the menu sluggish in coming
;; up -- too many tests to make.  Instead, choosing a useless menu
;; item just gets you an error now (see
;; bookmark-make-menu-bar-with-function) 
;;      
;;      (put 'bookmark-menu-bar-jump 'menu-enable
;;	   '(or bookmark-alist 
;;		(and (file-readable-p bookmark-file)
;;		     (progn (bookmark-load bookmark-file)
;;			    bookmark-alist))))
;;
;;      (put 'bookmark-menu-bar-insert 'menu-enable
;;	   '(or bookmark-alist 
;;		(and (file-readable-p bookmark-file)
;;		     (progn (bookmark-load bookmark-file)
;;			    bookmark-alist))))
;;
;;      (put 'bookmark-menu-bar-locate 'menu-enable
;;	   '(or bookmark-alist 
;;		(and (file-readable-p bookmark-file)
;;		     (progn (bookmark-load bookmark-file)
;;			    bookmark-alist))))
;;
;;      (put 'bookmark-menu-bar-rename 'menu-enable
;;	   '(or bookmark-alist 
;;		(and (file-readable-p bookmark-file)
;;		     (progn (bookmark-load bookmark-file)
;;			    bookmark-alist))))
;;
;;      (put 'bookmark-menu-bar-delete 'menu-enable
;;	   '(or bookmark-alist 
;;		(and (file-readable-p bookmark-file)
;;		     (progn (bookmark-load bookmark-file)
;;			    bookmark-alist))))
;;
;;      (put 'bookmark-menu-bar-save 'menu-enable
;;	   '(or bookmark-alist
;;		(and (file-readable-p bookmark-file)
;;		     (progn (bookmark-load bookmark-file)
;;			    bookmark-alist))))
;;
;;      (put 'bookmark-menu-bar-write 'menu-enable
;;	   '(or bookmark-alist 
;;		(and (file-readable-p bookmark-file)
;;		     (progn (bookmark-load bookmark-file)
;;			    bookmark-alist))))

;;;; end bookmark menu-bar stuff ;;;;

(provide 'bookmark)
      
;;; bookmark.el ends here
