;;; -*- Mode: Emacs-Lisp -*-
;;; File: info-bookmark.el
;;; 
;;; Bookmarks for use in the Info Documentation reader
;;; Karl Fogel and Ken Olstad
;;; (kfogel@cs.oberlin.edu and olstad@msc.edu)
;;;
;;; LCD Archive Entry:
;;; info-bookmark|Karl Fogel, Ken Olstad|kfogel@cs.oberlin.edu, olstad@msc.edu|
;;; Setting bookmarks in Info that can be jumped to later.|
;;; 08-Apr-1993|0.0|~/misc/info-bookmark.el.Z|
;;;
;;; This code is distributed under the terms of the GNU General Public
;;; license. If you are not sure what that means, contact either of the
;;; authors. If you do not have electronic mail access, write to me at
;;;
;;; Karl Fogel
;;; 1123 N. Oak Park Ave.
;;; Oak Park, ILL
;;; USA      60302
;;;
;;; for more information.
;;;
;;; Please let us know what you think of it... we like messages with
;;; complaints almost as much as ones without complaints! :-)
;;;
;;; TODO: 
;;;
;;; Better support for multiple bookmark files.  Suggestions welcome!
;;; 
;;; 
;;; INSTALLATION:
;;; 
;;; Put this file in a directory that is in your load-path, and in
;;; your .emacs, set Info-mode-hook (or Info-load-hook instead if your
;;; info has it) something like this (remember to take out the semicolons!):
;;; 
;;; (setq Info-mode-hook
;;;       (function (lambda ()
;;;                   (require 'info-bookmark))))
;;; 
;;; Set your kill-emacs-hook something like this:
;;; 
;;; (setq kill-emacs-hook
;;;       (function (lambda ()
;;;                   (and (featurep 'info-bookmark)
;;;                        Info-bookmark-alist
;;;                        (y-or-n-p "Save info-bookmarks? ")
;;;                        (Info-bookmark-write nil)))))
;;; 
;;; You may make minor customizations by setting Info-bookmark-file,
;;; Info-bookmark-completion-ignore-case, and/or
;;; Info-bookmark-always-write-file in your .emacs file (see the
;;; defvars below).
;;;
;;; If your Info does not have a mode-hook or a load-hook (as seems to
;;; be the case with some), then put the following into your .emacs:
;;;
;;; (load "info-bookmark.el")
;;;
;;; and don't worry about doing anything fancier.
;;; 
;;; USAGE:
;;; 
;;; Simple:
;;; When you've finally found that elusive info node that contains the
;;; answer you keep forgetting, type "x" to mark the spot, and give it
;;; a name you'll remember.  Later on, rather than navigating to that
;;; spot again, type "j" and type your name for the spot you marked
;;; (type TAB for completion help if you don't remember the name).
;;;
;;; Verbose:
;;; Assuming that info-bookmark.el has been loaded correctly, typing "x"
;;; while in Info will set a bookmark (you will be prompted for a name to
;;; give the bookmark, with the name defaulting to the name of the Info
;;; node that you are at). Later on you can type "j" while in Info ("j" 
;;; for "jump to bookmark"), and you can return to any place that you have
;;; a bookmark for. Completion is available on bookmark names; hit SPACE or
;;; TAB while you are being prompted for a bookmark.
;;; 
;;; Bookmarks are saved automatically in the file ~/.info-bkmarks and 
;;; reloaded so that they will not be lost between different Emacs sessions. 
;;; (The reloading takes place at the very end of info-bookmark.el, in fact).
;;; If you have turned off automatic saving of new bookmarks by setting
;;; the value of Info-bookmark-always-write-file to nil, you can cause
;;; all bookmarks currently in effect to be saved by pressing "w". "C-u w"
;;; ("w" with a prefix argument) will save them in a file of your choice.
;;;
;;; At any time, you can revert to the bookmarks in ~/.info-bkmarks with
;;; "R". You can also load in bookmarks from another file (perhaps someone
;;; else's bookmark file) with "L". If you wish to delete a bookmark (rare),
;;; use "D". You will be prompted for a bookmark to remove, and completion
;;; will aid you in the arduous task of typing in its name.
;;; 
;;; Functions you might want to do describe-function on, for more details:
;;; Info-bookmark-set
;;; Info-bookmark-jump
;;; Info-bookmark-revert
;;; Info-bookmark-delete
;;; Info-bookmark-load
;;; Info-bookmark-write
;;;
;;;
;;; NOTE (IMPENDING OBSOLESCENCE?):
;;;
;;; Later versions of Info will have bookmarks incorporated into them, with
;;; some minor changes (bookmarks would be dependent on "annotations", a
;;; feature in info.el by David Gillespie). So sometime in the (near) 
;;; future, you may discover that your Info already has all this stuff in
;;; it already, at which point you can cheerfully junk info-bookmark.el.
;;; Until then, though, we hope you find this a handy tool when using
;;; Info. Again, feel free to let us know what you think! You don't need to
;;; have found a bug to report; we'd be tickled pink just to know that 
;;; you're using it :-)

(require 'info)
(provide 'info-bookmark)

;; Read the help on all of these functions for details...
(define-key Info-mode-map "x" 'Info-bookmark-set)    ; "x" marks the spot!
(define-key Info-mode-map "M" 'Info-bookmark-set)
(define-key Info-mode-map "j" 'Info-bookmark-jump)
(define-key Info-mode-map "R" 'Info-bookmark-revert) ; resets bookmark list
(define-key Info-mode-map "D" 'Info-bookmark-delete) ; removes bookmarks
(define-key Info-mode-map "L" 'Info-bookmark-load)   ; loads new ones from file
(define-key Info-mode-map "w" 'Info-bookmark-write)  ; saves them in a file
(define-key Info-mode-map "S" 'Info-bookmark-write)


(defvar Info-bookmark-file "~/.info-bkmarks" 
  "*File in which to save info bookmarks by default.")


(defvar Info-bookmark-completion-ignore-case t
  "*Non-nil means that the various Info bookmark functions that
do completion will be case-insensitive in completion.")


(defvar Info-bookmark-always-write-file t
  "*If non-nil, save Info-bookmark-file every time a bookmark is made
or discarded.  Unless saving is slow for you, you should probably
have this set to t.")


(defvar Info-bookmark-alist ()
  "*Association list of Info bookmarks.
You probably don't want to change the value of this alist yourself;
instead, let the various Info-bookmark functions do it for you.")


(defun Info-bookmark-set (parg)
  "Set a bookmark named NAME at an Info node.  With prefix arg, will not
overwrite a bookmark that has the same name as NAME if such a bookmark
already exists, but instead will \"push\" the new bookmark onto the
bookmark alist.  Thus the most recently set bookmark with name NAME would
be the one in effect at any given time, but the others are still there,
should you decide to delete the most recent one.

Use \\[Info-bookmark-delete] to remove bookmarks \(you give it a name,
and it removes only the first instance of a bookmark with that name from
the list of bookmarks.\)"
  (interactive "P")
  (if (not (equal mode-name "Info"))
      (error "Must be in Info mode to set Info bookmarks.")
    (Info-bookmark-make
     parg
     (read-from-minibuffer
      "Pleased to enter a name for new bookmark: "
      (if (equal mode-name "Info") ; checking twice, but that's okay
	  Info-current-node
	nil)))))


(defun Info-bookmark-make (parg str)
  (if (and (assoc str Info-bookmark-alist) (not parg))
      ;; already existing boookmark under that name and
      ;; no prefix arg means just overwrite old bookmark
      (setcdr (assoc str Info-bookmark-alist)
	      (list (list Info-current-file Info-current-node (point))))
    ;; otherwise just cons it onto the front (either the bookmark does
    ;; exist already, or there is no prefix arg.  In either case, we
    ;; want the new bookmark consed onto the alist...)
    (setq Info-bookmark-alist
	  (cons
	   (list str (list Info-current-file Info-current-node (point)))
	   Info-bookmark-alist)))
  (if Info-bookmark-always-write-file
      (Info-bookmark-write nil)))


(defun Info-bookmark-jump (str)
  "Go to the place in Info saved in the bookmark BOOKMARK.  Starts up Info
or switches to an Info buffer if necessary.  You may have a problem using
this function if the value of variable Info-bookmark-alist is nil.  If
that happens, you need to load in some bookmarks.  See help on function
Info-bookmark-load for more about this."
  (interactive (let ((completion-ignore-case
		      Info-bookmark-completion-ignore-case))
		 (list (completing-read
			"Pleased to enter name of bookmark to jump to: "
			Info-bookmark-alist
			nil
			0))))
  (if (not (equal mode-name "Info"))
      (info)) ; start up or switch to Info if we are not in it already
  (let ((whereto-list (car (cdr (assoc str Info-bookmark-alist)))))
    (let ((file (car whereto-list))
	  (node (car (cdr whereto-list)))
	  (place (car (cdr (cdr whereto-list)))))
      (Info-find-node file node)
      (goto-char place))))


(defun Info-bookmark-delete (str)
  "Delete the bookmark named NAME from the bookmark list.  Removes only
the first instance of a bookmark with that name.  If there is another
bookmark with the same name, it will become \"current\" as soon as the
old one is removed from the bookmark list."
    (interactive (let ((completion-ignore-case
			Info-bookmark-completion-ignore-case))
		   (list (completing-read
			  "Pleased to enter name of bookmark to delete: "
			  Info-bookmark-alist
			  nil
			  0))))
    (let ((will-go (assoc str Info-bookmark-alist)))
      (if (string-equal str (car (car Info-bookmark-alist)))
	  (setq Info-bookmark-alist
		(delq will-go Info-bookmark-alist))
	(delq will-go Info-bookmark-alist)))
    (if Info-bookmark-always-write-file
	(Info-bookmark-write nil)))


(defun Info-bookmark-write (parg &optional file)
  "Saves currently defined Info bookmarks in the file defined by
the variable Info-bookmark-file.  With a prefix arg, save it in file
FILE.

If you are calling this from Lisp, the two arguments are PREFIX-ARG
and FILE, and if you just want it to write to the default file,
then pass in nil as the only argument.  Or pass in nil and FILE, and
it will save in FILE instead.  If you pass in one argument, and it is
non-nil, then the user will be interactively queried for a file to
save in.

When you want to load in the bookmarks from a file, use Info-bookmark-load,
\\[Info-bookmark-load].  With a prefix arg, that function will prompt you
for a file, otherwise, it uses the file defined by the variable
Info-bookmark-file."
  (interactive "P")
  (cond
   ((and (null parg) (null file))
    ;;whether interactive or not, write to default file
    (Info-bookmark-write-file Info-bookmark-file))
   ((and (null parg) file)
    ;;whether interactive or not, write to given file
    (Info-bookmark-write-file file))
   ((and parg (not file))
    ;;have been called interactively w/ prefix arg
    (let ((file (read-file-name "File to save bookmarks in: ")))
      (Info-bookmark-write-file file)))
   (t ; someone called us with prefix-arg *and* a file, so just write to file
    (Info-bookmark-write-file file))))


(defun Info-bookmark-write-file (file)
  (save-excursion
    (message (format "Saving bookmarks to file %s." file))
    (set-buffer (find-file-noselect file))
    (goto-char (point-min))
    (delete-region (point-min) (point-max))
    (print Info-bookmark-alist (current-buffer))
    (write-file file)
    (kill-buffer (current-buffer))))


(defun Info-bookmark-load (file &optional revert)
  "Loads bookmarks from FILE, appending loaded bookmarks to the front
of the list of bookmarks.  If optional second argument REVERT is
non-nil, existing bookmarks are destroyed.

If you load a file that doesn't contain a proper bookmark alist, you
will corrupt Emacs\' bookmark list.  Generally, you should only load
in files that were created with the Info-bookmark functions in the
first place."
  (interactive
   (list (read-file-name
	  (format "Load bookmarks from: (%s) "
		  Info-bookmark-file)	;Default might not be used often,
					;but there's no better default, and
					;I guess it's better than none at all.
	  "~/" Info-bookmark-file 'confirm)))
  (setq file (expand-file-name file))
  (if (file-readable-p file)
      (save-excursion
	(message (format "Loading bookmarks from %s..." file))
	(set-buffer (find-file-noselect file))
	(goto-char (point-min))
	(let ((blist (car (read-from-string
			   (buffer-substring (point-min) (point-max))))))
	  (if (listp blist)
	      (setq Info-bookmark-alist
		    (append blist (if (not revert) Info-bookmark-alist)))
	    (error (format "Invalid bookmark list in %s." file))))
	(kill-buffer (current-buffer))
	(message (format "Loading bookmarks from %s...done" file)))
    (error (format "Bookmark file %s is not readable." file))))


(defun Info-bookmark-revert (&optional not-really)

  "Resets the bookmark list, using bookmarks stored in the default
bookmark file.  This will have the effect of deleting any bookmarks
that are not saved in the file.  Asks for confirmation first.

This is desirable when you want to get rid of a whole bunch of bookmarks
that you set yourself or loaded in from somewhere else, without you
having to delete them one by one.

Use Info-bookmark-load (\\[Info-bookmark-load]) to load bookmarks from
a file without destroying your current bookmarks."

  (interactive
   (list (not (y-or-n-p
	       (format "Revert to bookmarks from %s? " Info-bookmark-file)))))
  (if not-really
      (message "Bookmark list not reverted.")
    (Info-bookmark-load Info-bookmark-file t)))


;; this revert is for when we have real multi-file support.  Ignore it 
;; for now...
;;
;;(defun Info-bookmark-revert ()
;;  "Delete all existing bookmarks and optionally load some in from a file.
;;The user is prompted for a file to load bookmarks from, with the default 
;;bookmark file appearing at the prompt.
;;
;;Don't call this from non-interactive Lisp programs, because it always
;;queries the user for a file."
;;  (interactive)
;;  (setq Info-bookmark-alist ())
;;  (Info-bookmark-load-file
;;   (expand-file-name
;;    (read-file-name
;;     "Bookmark list now empty.  Load bookmarks from file: "
;;     (if (file-exists-p Info-bookmark-file)
;;	 Info-bookmark-file
;;       nil)))))
	

;; load the default bookmark file, if it exists.
(if (file-exists-p Info-bookmark-file)
    (Info-bookmark-load Info-bookmark-file))
