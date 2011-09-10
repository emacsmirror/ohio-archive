;;; filebrowser.el --- file browser mode commands for Emacs

;; $Id: filebrowser.el,v 0.3 1995/09/06 15:55:37 glenn Exp glenn $

;; Copyright (C) 1986, 1993, 1994 Free Software Foundation, Inc.

;; Author: Glenn Moloney (glenn@physics.unimelb.edu.au)

;; This file is not part of GNU emacs

;; LCD Archive Entry:
;; filebrowser|Glenn Moloney|glenn@physics.unimelb.edu.au|
;; file browser mode commands for Emacs|
;; 09-Jun-1995|$Revision: 0.3 $|~/misc/filebrowser.el.gz|

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs.  If you did not, write to the Free Software Foundation,
;; Inc., 675 Mass Ave., Cambridge, MA 02139, USA.

;;; Commentary:

;; This package is a major mode for file browsing. It is like any
;; common file browser - like Xtree, windows file manager, etc... But,
;; it is very basic at the moment - I expect it will get extended with
;; useful features in the fullness of time. I wrote it primarily for
;; handling large multi-file software projects. Most cases I find I
;; don't want the long listings of dired, but I do want to navigate
;; around directory trees. I really like this mode, and I've been
;; wanting something like this for emacs for some time.

;; The directory browser-mode is derived from outline-mode and also uses
;; foldout mode. This is a good example of the benefit of derived.el.

;; Requires: outline.el foldout.el derived.el easymenu.el
;; These are all part of emacs-19.28.

;; To use the file browser include the following in your ".emacs" file.
;;
;;	(autoload 'browse "filebrowser" "File and directory tree browser" t)
;;
;; Then just use `M-x browse' and you will be prompted for a directory
;; node to scan.

;; To use the hilit19 package to provide colour-coding of files put
;; something like the following in your ".emacs".
;;
;;(cond (window-system
;;       (require 'hilit19)
;;       (hilit-set-mode-patterns
;;	'browser-file-mode
;;	'(("\\w*/" nil dired-directory)
;;	  ("\\w*\\*" nil ForestGreen)
;;	  ("\\w*\\@" nil dired-link)
;;	  ("\\w*\\.gz\\>" nil VioletRed)
;;	  ("\\w*\\.\\(c\\|el\\|tex\\)\\>" nil FireBrick)
;;	  ("\\w*\\.\\(o\\|elc\\|dvi\\)\\>" nil SlateGray)
;;	  ("\\w*\\.h\\>" nil Purple)))
;;       (add-hook 'browser-file-display-hook
;;		 (function (lambda ()
;;			     (hilit-highlight-region (point) (point-max)
;;						     nil t))))
;;       ;; Some customised user menus
;;       (eval-after-load
;;	"filebrowser"
;;	'(browser-add-menu
;;	  '(("[mM]akefile"
;;	     ["Make"			compile t])
;;	    ("News"
;;	     ["GNUS"			gnus t])
;;	    ("\\.tar$"
;;	     ["Untar Archive"		(browser-shell "tar -xf %s") t]
;;	     ["List Archive"		(browser-shell "tar -tvf %s") t])
;;	    ("\\.\\(tar\\.gz\\|tar\\.[zZ]\\|tgz\\|taz\\)$"
;;	     ["Untar Compressed Archive" (browser-shell "tar -zxf %s") t]
;;	     ["List Compressed Archive"	(browser-shell "tar -ztvf %s") t])
;;	    ("Xdefaults\\|Xresources"
;;	     ["Load X Resources"	(browser-shell "xrdb -merge %s") t])
;;	    ("\\.\\(gif\\|jpg\\|xwd\\|xpm\\)$"
;;	     ["View Image"		(browser-shell "xv %s &") t])
;;	    ("\\.html$"
;;	     ["Netscape"		(browser-shell "netscape -remote %s &") t])
;;	    )))))

;; Two modes are provided: "browser-mode" and "browser-file-mode". I
;; am aware that "browsing" has become synonymous with the Web, so I
;; am considering a name change here to remove confusion - But to
;; what? Any ideas?

;;; NOTES:

;; The syntax table in the browser tree and file buffers are modified
;; so that all but whitespace chars are considered to have "word"
;; syntax. This means the above regexps can use word syntax (\\w) and
;; word delimiters (\\>) for all valid filename characters (except for
;; whitespace).

;; The directory tree listing will list symlinks, which are links to
;; directories, but does not recurse into those directories. You can
;; scan the subtree by "re-scanning" that entry (ie. move point to a
;; symlink entry and press the "r" key. Symlinks are identified by the
;; "@" symbol after the file name.

;; Directory entries which have been scanned, or are known to be empty
;; are listed with a trailing "/". Unscanned directories have no
;; trailing "/". This is used as a flag to the user, and to the
;; software to semi-automatically re-scan directories opened with the
;; "c" (browser-show-children), "s" (browser-show-subtree) and " "
;; (browser-do-listing) keys.

;; Mouse button 2 is used for the simple operations in the tree and
;; file listing windows. Button 2 on a file entry will cause Emacs to
;; visit that file in another frame. Button 2 on a directory entry in
;; the tree listing will re-scan that directory node if required, and
;; display the contents of that directory in the listing window.

;; Shift-mouse-button-1-down begins a "drag&drop" operation to copy a
;; file. This is very primitive, and needs work (see TODO below). The
;; file entry is highlighted and you can drag the mouse to another
;; listing window or any directory entry in the directory tree window
;; to copy the file to that directory.

;; Popup menus for file operations. Button 3 on a filename (or block)
;; will popup a menu of operations for that file. This menu is context
;; sensitive and can be configured by the user.

;; Visiting a file while in the file listing window (by pressing
;; button-2 or key "f" or " ") will display the buffer in the frame
;; given by the buffer-local variable "browser-display-frame". By
;; default this is set to the frame from which the browser was
;; started. So, if you start the browser more than once, it may be
;; best to re-start it from a different frame each time. You can
;; re-assign this variable to display in some other frame. Any
;; suggestions for making this work better are welcome.

;;; HISTORY:

;; I originally used "find" to generate the recursive directory
;; listings and "ls" to generate the file listings, but I found better
;; performance using (directory-files dirname). This also gives better
;; portability. Currently I am still using "ls -ldq" for the detailed
;; file info messages. I'll probably keep it that way, as the user can
;; tailor it to what they like (variable: browser-info-command). I am
;; considering moving to using a co-process (asynchronous) to generate
;; the listings - I don't want the overhead of starting up processes
;; on each invocation.

;;; BUGS:

;; The directory tree lister assumes an empty directory has 2
;; hardlinks ("dir" and "dir/."). This is for performance reasons, and
;; follows the behaviour of GNU "find" without the "-noleaf"
;; options. Specifically, if a directory has a hard link count of 2,
;; then it is empty of other directories and need not be scanned -
;; this is a real win. This is fine on Unix, but not on other
;; filesystems. What about VMS? The user can disable this with the
;; browser-tree-noleaf user option.

;; Emacs version 19.29 has a bug with "activate-menubar-hook". The
;; hook functions are invoked inappropriately, which adds a
;; significant overhead to normal emacs operation. I have therefore
;; disabled the dynamic context sensitive "Operate" menubar item for
;; emacs-19.29.  RMS has reported that this will be fixed in
;; emacs-19.30. The context sensitive menu is still available by
;; pressing down-mouse-3 over a filename in the file listing window.

;;; TODO:

;; Support for toggling to long listing mode (ala dired).

;; Support for navigating "up" from the base of a directory tree, or
;; to navigate between diconnected parts of the directory tree. Also,
;; provide shortcuts for navigating to specific commonly used
;; directories.

;; Implement some sort of recursive edit mode or special key map, so
;; users can call up a file selection "widget" with a "hot key", and
;; insert that filename in the minibuffer or other buffer (eg. shell
;; mode), or for an (interactive) attempt to read a filename.

;; The drag&drop needs some sort of fancy user feedback to indicate a
;; drag&drop operation is under way. Can I change the mouse shape to
;; reflect the operation (eg. nice little folder shaped cursor being
;; dragged by mouse)? Can I drag a little x-popup around with the name
;; of the file in it? Can I perform some sort of mouse grab operation?
;; Any other ideas?

;; Dreaming: the next obvious feature is to go one step further and
;; intregrate the file browser with `shell', to get a file and program
;; interface. Hmm, is this a good idea - I'm not sure?

;;; Code:

;;

(require 'outline)

(defun browser-outline-level ()
  "Return the depth to which a statement is nested in the outline.
Point must be at the beginning of a header line.  This is actually
the number of characters that `outline-regexp' matches divided by the
length of the browser-indent-string."
  (looking-at outline-regexp)
  (/ (- (match-end 0) (match-beginning 0) (length browser-pre-indent-string))
     (length browser-indent-string)))

(setq outline-level 'browser-outline-level)

(defvar browser-indent-string " "
  "*String to use as indentation for subdirectories in the tree listing.
The \"browser-pre-indent-string\" and this string are concatenated together
to provide the required nesting of directory entries in the directory tree
buffer.

The default is \" \", and this appears to work best.")

(defvar browser-pre-indent-string " "
  "*String to use as pre-indentation for subdirectories in the tree listing.
The pre-indentation is the leading string for all entries in the
directory tree listing.

The default is \" \".")

(defvar browser-dir-depth 1
  "*The depth to scan directory trees on each read.
When a directory is scanned, it will be scanned to this depth of
directory nesting. This is useful to limit the depth of directory
scanning on large directory trees. Unscanned sub-directories will be
semi-automatically re-scanned by the browser as they are opened.

The default value of 1 provides dynamic directory browsing.")

(defvar browser-dir-max-depth 50
  "*The maximum depth to scan directory trees on each read.
When a directory is scanned, it will be recursively scanned to the depth
specified by `browser-dir-depth', subject to the limit specified here.
This is also the value assigned to `browser-dir-depth' if a negative depth
argument is provided to the `browse' command (use a negative prefix arg).")

(defvar browser-info-command '("ls" "-ldq")
  "*The system command to generate one-line detailed info on a file.
The value should be a list of arguments suitable for `call-process'.
The name of the file is added to the list before calling `call-process'.
The default value is: '(\"ls\" \"-ldq\")")

(defvar browser-tree-noleaf nil
  "*Should we scan all subdirs - even those with apparently empty dirs?
Like GNU find, the directory tree scanner assumes the Unix convention
that each directory with a hard link count of 2 contains no
subdirectories. If there were any subdirectories, they would each
contain a hard link to the parent directory (\"..\"), and you would
get a hard link count greater than 2. The 2 base links are the directory
entry, and \".\".

This yields a substantial performance improvement, since such subdirectories
do not have to be scanned for subdirectories. This technique will not work on
file systems which do not conform to this practice (eg. MS-DOS,
iso9660 CD-ROM, AFS mount points, etc.).

Set this variable to other than `nil' to inhibit this behaviour.

For more information see the \"-noleaf\" option in the man page for GNU find.")

(defvar browser-tree-window-width 18
  "*Width (in columns) of the directory listing window.
The File listing windows get the remaining width in the current frame.")

(defvar browser-use-frames window-system
  "*If non-nil tells the browser to open files in other frames.")

(defvar browser-use-dedicated-windows browser-use-frames
  "*If non-nil, the browser displays it's buffers in dedicated windows.
Thus, the browser buffers will not be replaced by help or completion
windows. Also, means files which are found - must be loaded into other
windows or frames.")

(defvar browser-auto-re-scan t
  "*If non-nil tells the browser to automatically re-scan subdirectories
which have not already been scanned (as they are accessed).")

(defvar browser-buffer-list nil
  "The list of buffers associated with the browser session.
The first buffer in the list is the directory tree buffer.")

(defvar browser-display-frame nil
  "The frame to display files which are opened from the filebrowser.

If set to nil when the browser is started, it will be set to the
selected frame. This variable is local to each buffer.

The default value is `nil'.")

(defvar browser-mode-syntax-table nil
  "Syntax table used while in browser mode.
Default sets the syntax table so all filename chars have \"word\" syntax.
So, any valid filename can be considered as a \"word\".
This helps for motion and word selection with the mouse - also
for specifying regexps for colour highlighting with hilit19.")

(defvar browser-tree-regexp nil
  "*Regular expression matching directories to be shown in a tree listing.
If set to `nil', all directories will be listed, else only the directories
which match the regexp will be listed. This variable is buffer-local.
See also the variable \"browser-tree-inhibit-regexp\".")

(defvar browser-tree-inhibit-regexp nil
  "*Regular expression matching directories not to be shown in a tree listing.
If set to `nil', no directories will be inhibited, else only the directories
which
match the regexp will be deleted from the listing. As an example, the
following regexp will inhibit the display of RCS directories:
        \"^RCS$\"
This variable is buffer-local.
See also the variable \"browser-tree-regexp\".")

(if browser-mode-syntax-table
    ()
  (setq browser-mode-syntax-table (make-syntax-table))
  (let ((i 0))
    (while (< i 256)
      (modify-syntax-entry i "w" browser-mode-syntax-table)
      (setq i (1+ i))))
  (modify-syntax-entry ?\  " " browser-mode-syntax-table)
  (modify-syntax-entry ?\t " " browser-mode-syntax-table)
  (modify-syntax-entry ?\n " " browser-mode-syntax-table)
  (modify-syntax-entry ?\f " " browser-mode-syntax-table)
  (modify-syntax-entry ?\r " " browser-mode-syntax-table))

;; Define the browser directory listing mode
(define-derived-mode browser-mode outline-mode "FileBrowser"
  "Major mode for file and directory tree browsing.

The `filebrowser' package provides two modes: one for displaying and
navigating around directory trees (browser-mode), and the other for
displaying the contents of directories and performing file operations
(browser-file-mode). The two modes work together to provide simple
filemanager facilities for Emacs. The `filebrowser' works in a similar
manner to other well know filemanager programs.

To start the browser use `M-x browse'. You will be asked for a
directory to start browsing. The following key bindings can be used
withing the directory tree window. Use `C-h m' within the file listing
window to see the key bindings for that window. The middle mouse
button (mouse-2) can be used for most simple navigation operations
(such as opening sub-directories or files).

\\{browser-mode-map}.

Customisation:
==============

The following variables may be used to customise the behaviour of the
directory tree window:

browser-dir-depth
browser-indent-string
browser-pre-indent-string
browser-tree-regexp
browser-tree-inhibit-regexp
browser-tree-window-width
browser-tree-noleaf"
  (setq case-fold-search nil
	buffer-read-only t)
  (buffer-disable-undo (current-buffer))
  (auto-fill-mode -1)
  (make-local-hook 'kill-buffer-hook)
  (add-hook 'kill-buffer-hook 'browser-delete-buffer nil 'local)
  (set (make-local-variable 'browser-buffer-list) nil)
  (make-local-variable 'browser-display-frame)
  (make-local-variable 'browser-dir-depth)
  (make-local-variable 'browser-tree-regexp)
  (make-local-variable 'browser-tree-inhibit-regexp)
  (setq truncate-lines t)
  (clear-visited-file-modtime)
  (set-syntax-table browser-mode-syntax-table))

(defun browser-delete-buffer ()
  (let* ((buffer (current-buffer))
	 (window (get-buffer-window buffer)))
    ;; If buffer is not the directory tree buffer - delete it from the list
    (if (not (eq buffer (car browser-buffer-list)))
	(delq buffer browser-buffer-list))
    (condition-case nil
	(delete-window window)
      (error nil))))

;; This provides a convenient interface to (call-process)
(defun browser-call-process (argv)
  (apply 'call-process
	 (car argv)
	 nil t nil
	 (cdr argv)))

(defun killed-buffer-p (buffer)
  "Return t if BUFFER is killed."
  (not (and buffer
	    (buffer-name buffer))))

(defun browser-check-buffer-list (buffer-list)
  "Check a list of buffers. Remove any killed buffers found.
The first buffer is a special case (the tree buffer), and is not deleted
if it has been killed."
  (interactive)
  (let ((b buffer-list))
    (while (setq b (cdr b))
      (if (killed-buffer-p (car b))
	  (delq (car b) buffer-list)))))

(defun browser-delete-other-browser-windows()
  "Delete other browser windows."
  (interactive)
  (if browser-buffer-list
      (let* ((this-window (selected-window))
	     (window (next-window this-window 'false))
	     (next-window nil))
	(while (not (eq this-window window))
	  (setq next-window (next-window window 'false))
	  (if (memq (window-buffer window) browser-buffer-list)
	      (delete-window window))
	  (setq window next-window)))))

(defun browser-configure-windows ()
  "Re-draw all the associated browser listing buffers.
Also checks for killed buffers in the buffer list - and deletes them."
  (interactive)
  (if browser-buffer-list
      (progn
	(browser-check-buffer-list browser-buffer-list)
	(let ((buffer-list browser-buffer-list)
	      (this-window (selected-window))
	      (buffer (current-buffer))
	      (dedicate-windows browser-use-dedicated-windows))
	  (browser-delete-other-browser-windows)
	  (let ((margin (- (frame-width) (window-width))))
	    (if (> margin 0)
		(enlarge-window margin t)))
	  (set-window-dedicated-p (selected-window) nil)
	  (if (and (not browser-use-frames)
		   (eq (next-window this-window) this-window))
	      (progn
		;; Find a non-browser window to display in the bottom window
		(split-window-vertically)
		(let ((buflist (buffer-list)))
		  (set-buffer (car buflist))
		  (while (and buflist
			      (or (eq (aref (buffer-name) 0) ?\ )
				  (eq major-mode 'browser-file-mode)
				  (eq major-mode 'browser-mode)))
		    (setq buflist (cdr buflist))
		    (set-buffer (car buflist)))
		  (set-buffer buffer)
		  (if buflist
		      (display-buffer (car buflist) t)))))
	  (if (killed-buffer-p (car buffer-list))
	      nil
	    (split-window-horizontally browser-tree-window-width)
	    (switch-to-buffer (car buffer-list))
	    (set-window-dedicated-p (selected-window) dedicate-windows)
	    (other-window 1))
	  (setq buffer-list (reverse (cdr buffer-list)))
	  (switch-to-buffer (car buffer-list))
	  (set-window-dedicated-p (selected-window) nil)
	  (browser-display-files)
	  (let ((maxheight (/ (window-height) (length buffer-list))))
	    (while (setq buffer-list (cdr buffer-list))
	      (split-window-vertically
	       (- (min maxheight (max window-min-height
				      (1+ browser-listing-length)))))
	      (set-window-dedicated-p (next-window) dedicate-windows)
	      (switch-to-buffer (car buffer-list))
	      (browser-display-files)))
	  (set-window-dedicated-p (selected-window) dedicate-windows)
	  (select-window (get-buffer-window buffer))))))

(defun browser-insert-dir ( dirname indent-string depth )
  "Read the contents of dirname to generate a directory tree.
Calls itself recursively, to scan more deeply nested subdirectories."
  (let ((filelist (directory-files dirname nil browser-tree-regexp))
	(filename nil)
	(pathname nil)
	(p nil)
	(empty nil)
	(i 0)
	(attributes nil)
	(indent-length (length indent-string)))
    (setq dirname (directory-file-name dirname))
    (while filelist
      (setq filename (car filelist))
      (setq pathname (expand-file-name filename dirname))
      (setq attributes (file-attributes pathname))
      (cond ((equal (car attributes) t) ; is it a directory
	     (if (or (string-match "^\\.\\.?$" filename)
		     (and browser-tree-inhibit-regexp
			  (string-match browser-tree-inhibit-regexp filename)))
		 ()			; discard if it's "." or ".."
	       ;; Save a lot of time by not scanning empty subdirs
	       (setq empty (= (nth 1 attributes) 2))
	       (setq p (point))
	       (insert indent-string filename
		       (if (or empty (> depth 0)) "/\n" "\n"))
	       (overlay-put (make-overlay (+ p indent-length) (1- (point)))
			    'mouse-face 'highlight)
	       ;; Scan this subdirectory.. if it has subdirs
	       (if (and (not empty)
			(> depth 0))
		   (browser-insert-dir pathname
				       (concat indent-string
					       browser-indent-string)
				       (1- depth)))))
	    ((and (stringp (car attributes))
		  (not (string-match "^\\.\\.?$" filename)))
	     ;; File is a symlink - does it point to a directory?
	     (setq attributes
		   (condition-case nil
		       (file-attributes (file-chase-links pathname))
		     (error nil)))
	     (and attributes
		  (equal (car attributes) t)
		  (progn
		    ;; Symlink points to a directory
		    (setq p (point))
		    (insert indent-string filename "@\n")
		    (overlay-put (make-overlay (+ p indent-length) (1- (point)))
				 'mouse-face 'highlight)))))
      (setq filelist (cdr filelist)))))

(defun browser-insert-dir-tree ( dirname indent-string depth )
  "Insert the contents of a directory scan into the browser buffer."
  (setq buffer-read-only nil)
  (if (< depth 0)
      (setq depth browser-dir-max-depth))
  (save-excursion
    (message "Reading directory tree %s..."
	     (abbreviate-file-name (expand-file-name dirname)))
    (let ((p (point))
	  (indent-length (length indent-string)))
      ;; This recursively calls (directory-files) to scan the directory tree
      (insert indent-string
	      (file-name-nondirectory (directory-file-name dirname))
	      (if (> depth 0) "/\n" "\n"))
      (put-text-property (+ p indent-length) (1- (point))
			 'mouse-face 'highlight)
      (if (> depth 0)
	  (browser-insert-dir dirname
			      (concat indent-string
				      browser-indent-string)
			      (1- depth))))
    (message "Reading directory tree %s...done"
	     (abbreviate-file-name (expand-file-name dirname)))
    (run-hooks 'browser-dir-list-hook))
  (set-buffer-modified-p nil)
  (setq buffer-read-only t))

(defun browser-read-dir ( dirname depth )
  "Read the contents of the directory into the file browser buffer."
  (setq buffer-read-only nil)
  (let ((p (point))
	(gc-cons-threshold (* gc-cons-threshold 10)))
    (setq outline-regexp (concat browser-pre-indent-string
				 "\\(" browser-indent-string "\\)*"))
    (save-excursion
      (condition-case nil
	  (foldout-exit-fold 0)
	(error nil))
      (widen)
      (cd dirname)
      (erase-buffer)
      (insert default-directory "\n")
      (browser-insert-dir-tree "." browser-pre-indent-string depth)
      (goto-char (point-min))
      (forward-line 1)
      (hide-subtree)
      (show-children))
    (goto-char p))
  (set-buffer-modified-p nil)
  (setq buffer-read-only t))

(defun browser-re-read-dir ( depth )
  "Re-read the current browser listing."
  (interactive "P")
  (if (null depth)
      (setq depth browser-dir-depth)
    (setq depth (prefix-numeric-value depth)))
  (browser-read-dir default-directory depth))

(defun browser-do-read-subdir ( depth )
  "Read the directory subtree for the entry under point.
The argument, DEPTH, indicates the depth in the recursive directory
structure to scan. If DEPTH is nil, use the default, which is set
by the variable, \"browser-dir-depth\".
browser-do-read-subdir( DEPTH )"
  (interactive "P")
  (if (null depth)
      (setq depth browser-dir-depth)
    (setq depth (prefix-numeric-value depth)))
  (browser-read-subdir depth)
  (hide-subtree)
  (show-children))

(defun browser-read-subdir ( depth )
  "Read the directory subtree for the entry under point.
The optional argument, DEPTH, indicates the depth in the recursive directory
structure to scan. If DEPTH is nil or 0, use the default, which is set
by the variable, \"browser-dir-depth\".
browser-read-subdir( DEPTH )"
  (let ((p (point)))
    (save-excursion
      (outline-back-to-heading)
      (let ((level (funcall outline-level))
	    (indent-string "")
	    (gc-cons-threshold (* gc-cons-threshold 10))
	    dirname beg end)
	(setq beg (point))
	(setq dirname (browser-get-relative-dir-name))
	(if (null (condition-case nil
		      (progn (outline-forward-same-level 1) t)
		    (error nil)))
	    (if (null (condition-case nil
			  (progn (outline-next-visible-heading 1) t)
			(error nil)))
		(goto-char (point-max))
	      (if (> (funcall outline-level) level)
		  (goto-char (point-max)))))
	(setq end (point))
	(goto-char beg)
	(if (looking-at outline-regexp)
	  (setq indent-string (buffer-substring (match-beginning 0)
						(match-end 0))))
	(setq buffer-read-only nil)
	(delete-region beg end)
	(setq buffer-read-only t)
	(browser-insert-dir-tree dirname indent-string depth)))
    (goto-char p)))

(defun browse-noselect (filename &optional depth)
  "Run the File browser on directory DIRNAME."
  (if (null depth)
      (setq depth browser-dir-depth)
    (setq depth (prefix-numeric-value depth)))
  (save-excursion
    (let ((dirname (file-name-as-directory
		    (abbreviate-file-name (expand-file-name filename))))
	  (buffer nil))
      (setq buffer (create-file-buffer (directory-file-name dirname)))
      (set-buffer buffer)
      (cd dirname)
      (browser-mode)
      (clear-visited-file-modtime)
      (setq browser-tree-buffer buffer)
      (setq browser-buffer-list (list buffer))
      (browser-read-dir dirname depth)
      buffer)))

(defun browse (dirname &optional depth)
  "Run the File Browser on directory DIRNAME.

The FileBrowser provides a filemanager interface for Emacs.
Operation is similar to other common filebrowsers, and provides:
  - simple point-and-click navigation of a directory tree
  - complete file listings of selected directories
  - point-and-click to open a file in an Emacs buffer
  - simple file operations (delete, rename, copy, remove,...)
  - primitive drag-and-drop to copy or move files between directories.

The `browse' command prompts for a directory name, and accepts an
optional numeric prefix argument (eg. C-u 5 M-x browse).  The
FileBrowser will display two buffers side-by-side.

The left hand window shows the \"tree listing\" buffer. This buffer
contains a listing of the recursive sub-directory tree of the
directory specified. The subdirectory tree listing may optionally,
pre-scan the entire sub-directory tree, OR scan the subdirectory tree
dynamically as the user navigates around in the listing. A negative
numeric prefix arg tells the browser to pre-scan the subdirectory
tree, while a positive prefix arg will cause the browser to scan that
deep in the browser listing on each read. Each time the user navigates
into a subdir which has not been scanned it will be dynamically
re-scanned to this depth. The default scanning depth is `2'. This
provides a good balance for scanning very large directory trees (such
as `/'), yet reducing the amount of re-scanning provided.

The right hand window shows the \"file listing\" buffer. A complete file
listing of any directory in the tree listing may be shown in this
buffer (use the Space bar in the tree listing window). Simple file
operations and file visiting can be performed (try `C-h m' in the file
listing window).

Returns the buffer containing the directory listing."
  (interactive "DBrowse (directory): \nP")
  (if depth
      (setq depth (prefix-numeric-value depth)))
  (let ((treebuf (browse-noselect dirname depth))
	(listbuf (browser-file-noselect dirname)))
    (save-excursion
      (let (buffer-list)
	(set-buffer treebuf)
	(setq buffer-list (append browser-buffer-list (list listbuf)))
	(setq browser-buffer-list buffer-list)
	(set-buffer listbuf)
	(setq browser-buffer-list buffer-list))
      (if (and browser-use-frames window-system)
	  (let ((frame (make-frame))
		(current-frame (selected-frame)))
	    (set-buffer treebuf)
	    (if (not browser-display-frame)
		(setq browser-display-frame current-frame))
	    (set-buffer listbuf)
	    (if (not browser-display-frame)
		(setq browser-display-frame current-frame))
	    (raise-frame frame)
	    (select-frame frame))))
    (set-buffer listbuf)
    (browser-configure-windows)
    treebuf))

(defun rebrowse (dirname &optional depth)
  "Re-run the File browser on directory DIRNAME."
  (interactive "DBrowse (directory): ")
  (if (null depth)
      (setq depth browser-dir-depth)
    (setq depth (prefix-numeric-value depth)))
  (cd dirname)
  (browser-re-read-dir depth))

(defun browser-get-file-name ()
  "Read the name of the directory on the current line in the buffer."
  (interactive)
  (let (beg end)
    (save-excursion
      (end-of-line)
      (setq end (point))
      (beginning-of-line)
      (setq beg (point))
      (if (re-search-forward outline-regexp end t)
	  (setq beg (match-end 0)))
      (re-search-forward "@?[\n\r]" nil t))
    (and beg (buffer-substring beg (match-beginning 0)))))

(defun browser-up-heading ( arg )
  "Move to the heading line of which the present line is a subheading.
With argument, move up ARG levels."
  (interactive "p")
  ;; Find the previous (or current) heading line
  (or (outline-on-heading-p)
      (re-search-backward (concat "[\n\r]\\(" outline-regexp "\\)") nil t))
  (if (> (funcall outline-level) 1)
      (while (and (> (funcall outline-level) 1)
		  (> arg 0)
		  (not (bobp)))
	(let ((present-level (funcall outline-level)))
	  ;; Skip back past all headings of this level to a higher level heading
	  (while (and (not (< (funcall outline-level) present-level))
		      (if (re-search-backward
			   (concat "[\n\r]\\(" outline-regexp "\\)")
			   nil 'move)
			  (goto-char (1+ (match-beginning 0))))))
	  (setq arg (- arg 1))))))

(defun browser-get-relative-dir-name ()
  "Read the relative directory name of the file on the current line."
  (interactive)
  (save-excursion
    (let ((start (point-min)) (end (point-max))
	  dirname)
      (widen)
      (beginning-of-line)
      (setq dirname (browser-get-file-name))
      (while (and (> (funcall outline-level) 1)
		  (not (bobp)))
	(browser-up-heading 1)
	(setq dirname (concat (browser-get-file-name) dirname)))
      (narrow-to-region start end)
      dirname)))

(defun browser-get-dir-name ()
  "Read the full directory name of the file on the current line."
  (interactive)
  (abbreviate-file-name (expand-file-name (browser-get-relative-dir-name))))

(defun browser-new-list-buffer (buffer-list)
  (let ((display-frame browser-display-frame)
	(buffer (browser-file-noselect nil)))
    (setcdr buffer-list (cons buffer (cdr buffer-list)))
    (save-excursion
      (set-buffer buffer)
      (setq browser-buffer-list buffer-list)
      (setq browser-display-frame display-frame))
    buffer))

(defun browser-get-listing-buffer (buffer-list)
  (if (not (killed-buffer-p (nth 1 buffer-list)))
      (nth 1 buffer-list)
    (browser-check-buffer-list buffer-list)
    (or (nth 1 buffer-list)
	(browser-new-list-buffer buffer-list))))

(defun browser-scanned-directory-p ()
  "Predicate to test if directory under point has already been scanned."
  (looking-at "[^\n\r]*/@?[\n\r]"))

(defun browser-do-listing (&optional dont-show-children)
  "Make a listing of files in the directory under point."
  (interactive)
  (save-excursion
    (if (and browser-auto-re-scan
	     (not (browser-scanned-directory-p)))
	(browser-read-subdir browser-dir-depth))
    (hide-subtree)
    (show-children)
    (let ((dirname (browser-get-dir-name))
	  (buffer (browser-get-listing-buffer browser-buffer-list)))
      (if (null (get-buffer-window buffer))
	  (browser-configure-windows))
      (set-buffer buffer)
      (browser-list-files dirname))))

(defun browser-show-children ()
  "Re-scan directory under point, and show children."
  (interactive)
  (if (not (browser-scanned-directory-p))
      (browser-read-subdir browser-dir-depth))
  (hide-subtree)
  (show-children))

(defun browser-show-subtree ()
  "Re-scan directory under point, and show subtree."
  (interactive)
  (if (browser-scanned-directory-p)
      (show-subtree)
    (browser-read-subdir browser-dir-depth)))

(defun browser-dir-info ()
  "Display more detailed information on file."
  (interactive)
  (let ((filename (expand-file-name (browser-get-dir-name)))
	(directory default-directory))
    (cond (filename
	   (save-excursion
	     (set-buffer (get-buffer-create "*Browser-file-info*"))
	     (widen)
	     (erase-buffer)
	     (cd directory)
	     (browser-call-process (append browser-info-command
					   (list filename)))
	     (goto-char (point-min))
	     (end-of-line)
	     (message (buffer-substring (point-min) (point))))))))

(defun browser-new-listing (filename)
  "Create and display a new file listing buffer associated with the
current directory browser buffer."
  (let ((buffer (browser-new-list-buffer browser-buffer-list)))
    (set-buffer buffer)
    (browser-configure-windows)
    (browser-list-files filename)))

(defun browser-mouse-do-listing ( event )
  "Do file listing on the directory name clicked on."
  (interactive "e")
  (let* ((window (posn-window (event-end event))))
    (select-window window)
    (goto-char (posn-point (event-end event)))
    (browser-do-listing nil)))

(defun browser-do-new-listing ()
  "Create and display a new file listing buffer associated with the
current directory browser buffer."
  (interactive)
  (show-children)
  (let ((filename (browser-get-dir-name)))
    (if filename
	(browser-new-listing filename))))

(defun browser-relist-files ()
  (interactive)
  (save-excursion
    (let ((buffer (browser-get-listing-buffer browser-buffer-list)))
      (if (null (get-buffer-window buffer))
	  (browser-configure-windows))
      (set-buffer buffer)
      (browser-file-relist))))

(defun browser-mouse-do-new-listing ( event )
  "Do file listing on the directory name clicked on."
  (interactive "e")
  (save-excursion
    (let ((dir-buffer (window-buffer (posn-window (event-end event))))
	  (window (selected-window)))
      (set-buffer dir-buffer)
      (goto-char (posn-point (event-end event)))
      (browser-do-new-listing))))

(defun browser-quit ( &optional force )
  "Quit the file browser and delete all associated buffers."
  (interactive)
  (if (or force
	  (y-or-n-p "Quit the file browser (and delete buffers) ? "))
      (let (buffer-list buffer)
	(setq buffer-list browser-buffer-list)
	(while buffer-list
	  (setq buffer (car buffer-list))
	  (setq buffer-list (cdr buffer-list))
	  (if (not (killed-buffer-p buffer))
	      (kill-buffer buffer))))))

(suppress-keymap browser-mode-map)

(define-key browser-mode-map "a" 'show-all)
(define-key browser-mode-map "l" 'hide-sublevels)
(define-key browser-mode-map "c" 'browser-show-children)
(define-key browser-mode-map "s" 'browser-show-subtree)
(define-key browser-mode-map "h" 'hide-subtree)
(define-key browser-mode-map "n" 'outline-next-visible-heading)
(define-key browser-mode-map "p" 'outline-previous-visible-heading)
(define-key browser-mode-map "u" 'outline-up-heading)
(define-key browser-mode-map "b" 'outline-backward-same-level)
(define-key browser-mode-map "f" 'outline-forward-same-level)
(define-key browser-mode-map [delete] 'hide-subtree)
(require 'foldout)
(define-key browser-mode-map "x" 'foldout-exit-fold)
(define-key browser-mode-map "z" 'foldout-zoom-subtree)

(define-key browser-mode-map " " 'browser-do-listing)
(define-key browser-mode-map "." 'browser-dir-info)
(define-key browser-mode-map "r" 'browser-do-read-subdir)
(define-key browser-mode-map "T" 'browser-re-read-dir)
(define-key browser-mode-map "g" 'browser-relist-files)
(define-key browser-mode-map "o" 'browser-do-new-listing)
(define-key browser-mode-map "q" 'kill-buffer)
(define-key browser-mode-map "Q" 'browser-quit)
(define-key browser-mode-map "*" 'browser-configure-windows)
(define-key browser-mode-map [mouse-2]   'browser-mouse-do-listing)
(define-key browser-mode-map [S-mouse-2] 'browser-mouse-do-new-listing)

;; Menu bar bindings
(require 'easymenu)

;; I like easymenu. This was much longer, more complicated - and it
;; looked ugly too, till I switched to easymenu :-).
;; What about easy-keymap ? ;-)

(defconst browser-tree-menu
  '("Tree"
    ["Show Children"		browser-show-children t]
    ["Show Subtree"		browser-show-subtree t]
    ["Hide Subtree"		hide-subtree t]
    ["Show All"			show-all t]
    ["Hide All"			hide-sublevels t]
    ["Zoom Subtree"		foldout-zoom-subtree t]
    ["Exit Zoom"		foldout-exit-fold t])
  "Menu definition for Browser-mode Tree menu")

(easy-menu-define browser-tree-menu-keymap browser-mode-map
		  "Browser menu" browser-tree-menu)

(defconst browser-browser-menu
  '("Browser"
    ["Open Directory"		browser-do-listing t]
    ["Open Directory - new buffer" browser-do-new-listing t]
    ["Display File Info"	browser-dir-info t]
    ["Re-read Tree"		browser-re-read-dir t]
    ["Re-read Subdirectory"	browser-do-read-subdir t]
    ["Redraw Windows"		browser-configure-windows t]
    ["Kill Buffer"		kill-this-buffer t])
  "Menu definition for Browser-mode Browser menu")

(easy-menu-define browser-browser-menu-keymap browser-mode-map
		  "Browser menu" browser-browser-menu)

(define-key browser-mode-map [menu-bar edit] 'undefined)

(defvar browser-file-regexp nil
  "*Regular expression matching files to be shown in a file listing.
If set to `nil', all files will be listed, else only the files which match
the regexp will be listed. This variable is buffer-local.
See also the variable \"browser-file-inhibit-regexp\".")

(defvar browser-file-inhibit-regexp nil
  "*Regular expression matching files not to be shown in a file listing.
If set to `nil', no files will be inhibited, else only the files which
match the regexp will be deleted from the listing. As an example, the
following regexp will inhibit the display of emacs backup and auto-save
files:
        \"~$\\\\|^#.*#$\"
This variable is buffer-local.
See also the variable \"browser-file-regexp\".")

(defvar browser-sort-by-extension nil
  "*If set non-nil sort the file listings by the file extensions.")

(defvar browser-file-default-tag ?*
  "*The character to use as the default tag for tagged files.")

(defvar browser-file-entry-width 0
  "The field width of each column in the file listing window.")

(defvar browser-file-num-columns 0
  "The number of filenames listed on each line of the file listing.")

(defvar browser-file-num-rows 0
  "The number of lines of filenames in the file listing.")

(defvar browser-list-of-files nil
  "A list of the filenames for this listing.")

(defvar browser-start-of-listing 0
  "Specifies the buffer position of the start of the list of files.")

(defvar browser-listing-length 0
  "Specifies the number of lines in this buffers file listing display.")

(defvar browser-fileops-files nil
  "The current filename for dynamic file operations menus.")

(defvar browser-file-mode-map (make-sparse-keymap) "")

(defun browser-file-mode ()
  "Major mode for file and directory tree browsing.

The `filebrowser' package provides two modes: one for displaying and
navigating around directory trees (browser-mode), and the other for
displaying the contents of directories and performing file operations
(browser-file-mode). The two modes work together to provide simple
filemanager facilities for Emacs. The `filebrowser' works in a similar
manner to other well know filemanager programs.

To start the browser use `M-x browse'. You will be asked for a
directory to start browsing. The following key bindings can be used
withing the file listing window. Use `C-h m' within the directory tree
window to see the key bindings for that window. The middle mouse
button (mouse-2) can be used for most simple navigation operations
such as opening sub-directories or files.

\\{browser-file-mode-map}.

Customisation:
==============

The following variables may be used to customise the behaviour of the
directory tree window:

browser-file-regexp
browser-file-inhibit-regexp"
  (interactive)
  (kill-all-local-variables)
  (use-local-map browser-file-mode-map)
  (setq mode-name "BrowserListing")
  (setq major-mode 'browser-file-mode)
  (setq local-abbrev-table nil)
  (set-syntax-table browser-mode-syntax-table)
  (setq indent-tabs-mode nil)
  (auto-fill-mode -1)
  (buffer-disable-undo (current-buffer))
  (setq case-fold-search nil)
  (setq buffer-read-only t)
  (make-local-hook 'kill-buffer-hook)
  (add-hook 'kill-buffer-hook 'browser-delete-buffer nil t)
  (set (make-local-variable 'browser-buffer-list) nil)
  (set (make-local-variable 'browser-list-of-files) nil)
  (set (make-local-variable 'browser-start-of-listing) nil)
  (set (make-local-variable 'browser-listing-length) 0)
  (set (make-local-variable 'browser-file-entry-width) 0)
  (set (make-local-variable 'browser-file-num-columns) 0)
  (set (make-local-variable 'browser-file-num-rows) 0)
  (make-local-variable 'browser-display-frame)
  (make-local-variable 'browser-file-regexp)
  (make-local-variable 'browser-file-inhibit-regexp)
  (make-local-variable 'browser-fileops-files)
  ;; Emacs-19.29 has a bug with activate-menubar-hook
  (if (not (string-match "^19\\.29" emacs-version))
    (progn
      (make-local-hook 'activate-menubar-hook)
      (add-hook 'activate-menubar-hook 'browser-dynamic-fileops-menu nil t)))
  (setq truncate-lines t)
  (run-hooks 'browser-file-mode-hook))

;; Functions to create the File Listings

(defun browser-file-noselect ( filename )
  "Run the Browser file listing on directory DIRNAME."
  (let ((buffer (get-buffer-create "*browser-new-listing*")))
    (if filename
	(setq filename (file-name-as-directory
			(abbreviate-file-name (expand-file-name filename)))))
    (set-buffer buffer)
    (browser-file-mode)
    (if filename
	(browser-list-files filename))
    buffer))

(defun browser-file-list ( dirname )
  "Run the Browser file listing on directory DIRNAME."
  (interactive "DBrowse (directory): ")
  (let ((buffer (browser-file-noselect dirname)))
    (set-buffer buffer)
    (setq browser-buffer-list (copy-sequence '(nil buffer)))))

;; Stolen and adapted from dired.el ;-)
(defun browser-format-columns-of-files (files width)
  (let* ((fieldwidth (apply 'max (mapcar 'length files)))
	 (maxlen (+ fieldwidth 4))
	 (fmt (concat "%c %-" (number-to-string (1+ fieldwidth)) "s "))
	 (columns (max 1 (/ width maxlen)))
	 (nfiles (length files))
	 (rows (+ (/ nfiles columns)
		  (if (zerop (% nfiles columns)) 0 1)))
	 (i 0)
	 (j 0))
    (setq files (nconc (copy-sequence files)
		       (make-list (- (* columns rows) nfiles) "")))
    (setcdr (nthcdr (1- (length files)) files) files) ; make circular
    (let (p overlay)
      (while (< j rows)
	(while (< i columns)
	  (setq p (point))
	  (insert (format fmt
			  (or (get-text-property 0 'tag (car files)) ?\ )
			  (concat (car files)
				  (or (get-text-property 0 'suffix
							 (car files)) " "))))
	  (setq overlay (make-overlay (+ p 2) (+ p 2 (length (car files)))))
	  (overlay-put overlay 'mouse-face 'highlight)
	  (setq files (nthcdr rows files)
		i (1+ i)))
	(insert "\n")
	(setq i 0
	      j (1+ j)
	      files (cdr files))))
    (setq browser-file-entry-width maxlen)
    (setq browser-file-num-columns columns)
    (setq browser-file-num-rows rows)
    rows))

(defun browser-display-files ()
  "Re-format the file listing in the current buffer."
  (save-excursion
    (let ((window (get-buffer-window (current-buffer) 'visible)))
      (cond (browser-list-of-files
	     (setq buffer-read-only nil)
	     (widen)
	     (erase-buffer)
	     (let ((attributes (file-attributes
				(directory-file-name default-directory))))
	       (insert "Directory: "
		       default-directory
		       (if (stringp (car attributes))
			   ;; If it's a symlink - show link target name
			   (concat " -> " (abbreviate-file-name
					   (file-truename
					    (expand-file-name
					     (car attributes)))))
			 "")
		       (if browser-file-regexp
			   (concat "  (" browser-file-regexp")")
			 "")
		       "\n"))
	     (setq browser-start-of-listing (point))
	     (setq browser-listing-length
		   (1+ (browser-format-columns-of-files browser-list-of-files
							(if window
							    (window-width
							     window)
							  (frame-width)))))
	     (delete-char -1)
	     (goto-char browser-start-of-listing)
	     (browser-file-this-entry)
	     (run-hooks 'browser-file-display-hook)
	     (set-buffer-modified-p nil)
	     (setq buffer-read-only t))))))

(defun browser-file-compare-ext ( fname1 fname2 )
  "Compare the filename extensions of fname1 and fname2"
  (string< 
   (if (string-match "^[^.].*\\\.\\(.+\\)$" fname1)
       (substring fname1 (match-beginning 1))
     "")
   (if (string-match "^[^.].*\\\.\\(.+\\)$" fname2)
       (substring fname2 (match-beginning 1))
     "")))

(defun browser-file-list-filter ( file-list )
  "Process the file-list, adding suffixes to filenames to indicate type.
A suffix indicating file type is added to the end of each file name,
similar to \"ls -F\". Any files which match `browser-file-inhibit-regexp'
are deleted from the list. Also, sort file list by extension if
`browser-sort-by-extension' is non-nil."
  (if browser-sort-by-extension
      (setq file-list (sort file-list 'browser-file-compare-ext)))
  (let ((next-file file-list)
	(prev-file nil)
	(attributes nil))
    (while next-file
      (setq filename (car next-file))
      (if (and browser-file-inhibit-regexp
	       (string-match browser-file-inhibit-regexp filename))
	  ;; Filename matches the "inhibit" regexp - delete from list
	  (if prev-file
	      (setcdr prev-file (cdr next-file))
	    (setq file-list (cdr next-file)))
	;; Filename does not match - keep in list & do file type indicator
	(setq attributes (file-attributes filename))
	(cond ((equal (car attributes) t)	; is a directory
	       (put-text-property 0 1 'suffix "/" filename))
	      ((stringp (car attributes))	; is a symlink
	       (put-text-property 0 1 'suffix
				  (if (string-match "^\\.\\.?$" filename)
				      "/" "@") filename))
	      ((file-executable-p filename)	; is executable
	       (put-text-property 0 1 'suffix "*" filename)))
	(setq prev-file next-file))
      (setq next-file (cdr next-file))))
  file-list)

(defun browser-read-files ( dirname )
  "Create a file listing from the named directory"
  (cond ((eq major-mode 'browser-file-mode)
	 (clear-visited-file-modtime)
	 (cd dirname)
	 (message "Reading directory %s..." default-directory)
	 (setq buffer-read-only nil)
	 (run-hooks 'browser-file-pre-read-hook)
	 (setq browser-list-of-files (directory-files default-directory nil
						      browser-file-regexp))
	 (setq browser-list-of-files
	       (browser-file-list-filter browser-list-of-files))
	 (erase-buffer)
	 (rename-buffer "*browser-temp-buffer*" t)
	 (let ((filename (file-name-nondirectory
			  (directory-file-name default-directory))))
	   (if (string-equal filename "")
	       (setq filename "/"))
	   (rename-buffer filename t))
	 (let ((buffer-file-name (expand-file-name default-directory)))
	   (set-visited-file-modtime))
	 (message "Reading directory %s...done" default-directory)
	 (set-buffer-modified-p nil)
	 (setq buffer-read-only t))))

(defun browser-list-files ( dirname )
  "Create a file listing from the named directory"
  (cond ((eq major-mode 'browser-file-mode)
	 (let ((gc-cons-threshold (* gc-cons-threshold 10)))
	   (browser-read-files dirname)
	   (browser-display-files)
	   (goto-char browser-start-of-listing)
	   (browser-file-this-entry)))))

(defun browser-file-relist ()
  "Re-read the current directory file listing."
  (interactive)
  (let ((here (point)))
    (browser-list-files default-directory)
    (goto-char here)))

;; Navigation and entry selection function

(defun browser-file-get-entry-num ()
  (save-excursion
    (let ((col (/ (current-column) browser-file-entry-width))
	  (row 0)
	  (n 0))
      ;; Get the line number by counting lines back to start of buffer
      (setq row (1- (count-lines browser-start-of-listing (point))))
      (+ (* col browser-file-num-rows) row))))

(defun browser-file-this-entry ()
  "Move to start of this entry in the directory listing."
  (interactive)
  (if (< (point) browser-start-of-listing)
      (goto-char browser-start-of-listing))
  (move-to-column (* (/ (current-column) browser-file-entry-width)
		     browser-file-entry-width))
  (forward-char 2)
  (looking-at "[^ \n]"))

(defun browser-file-next-entry ( arg )
  "Move cursor to next file entry.
With prefix ARG, move that many entries."
  (interactive "p")
  (if (< (point) browser-start-of-listing)
      (goto-char (+ browser-start-of-listing 2)))
  (if (< arg 0)
      (browser-file-prev-entry (- arg))
    (let ((col (/ (current-column) browser-file-entry-width)))
      (while (not (zerop arg))
	(if (search-forward "\n" nil t)
	    ()
	  (goto-char browser-start-of-listing)
	  (setq col (1+ col))
	  (if (>= col browser-file-num-columns)
	      (setq col 0)))
	(setq arg (1- arg)))
      (move-to-column (+ (* col browser-file-entry-width) 2)))))

(defun browser-file-prev-entry ( arg )
  "Move cursor to previous file entry.
With prefix ARG, move that many entries."
  (interactive "p")
  (if (< (point) browser-start-of-listing)
      (goto-char (+ browser-start-of-listing 2)))
  (if (< arg 0)
      (browser-file-next-entry (- arg))
    (let ((col (/ (current-column) browser-file-entry-width)))
      (while (not (zerop arg))
	(search-backward "\n" nil t)
	(if (> (point) browser-start-of-listing)
	    ()
	  (goto-char (point-max))
	  (setq col (1- col))
	  (if (< col 0)
	      (setq col (1- browser-file-num-columns))))
	(setq arg (1- arg)))
      (move-to-column (+ (* col browser-file-entry-width) 2)))))

(defun browser-file-get-entry ()
  "Get the name of the current entry."
  (interactive)
  (if (< (point) browser-start-of-listing)
      nil
    (save-excursion
      (if (browser-file-this-entry)
	  (let ((filenum (browser-file-get-entry-num)))
	    (nth filenum browser-list-of-files))))))

(defun browser-file-get-file-name ()
  "Get the full pathname of the file in the current entry."
  (interactive)
  (let ((name (browser-file-get-entry)))
    (if name
	(abbreviate-file-name (expand-file-name name)))))

;; File tagging (marking) functions

(defun wildcard-to-regexp ( wildcard )
  (let ((beg 0) end c
	(specials-regexp "[*\\.?|()]")
	(regexp "^"))
    (while (setq end (string-match specials-regexp wildcard beg))
      (setq c (aref wildcard end))
      (setq regexp (concat regexp
			   (substring wildcard beg end)
			   (cond ((eq c ?*) ".*")
				 ((eq c ?.) "\\.")
				 ((eq c ??) ".")
				 ((eq c ?|) "\\|")
				 ((eq c ?\() "\\(")
				 ((eq c ?\)) "\\)")
				 ((eq c ??) ".")
				 )))
      (setq beg (1+ end)))
    (setq regexp (concat regexp (substring wildcard beg) "$"))))

(defun browser-file-do-tag-file ( tag )
  "Tag the current file for future file operations."
  (save-excursion
    (if (browser-file-this-entry)
	(let ((filenum (browser-file-get-entry-num)))
	  (move-to-column (* (/ (current-column) browser-file-entry-width)
			     browser-file-entry-width))
	  (put-text-property 0 1 'tag tag (nth filenum browser-list-of-files))
	  (if (null tag)
	      (setq tag ?\ ))
	  (let ((buffer-read-only nil))
	    (delete-char 1)
	    (insert-char tag 1)
	    (set-buffer-modified-p nil))))))

(defun browser-file-tag-file ( tag )
  "Tag the current file for future file operations."
  (interactive "P")
  (if (null tag)
      (setq tag browser-file-default-tag)
    (let ((cursor-in-echo-area t))
      (message "Character to use as tag ? ")
      (setq tag (read-char-exclusive))))
  (if (eq tag 0)
      (setq tag nil))
  (browser-file-do-tag-file tag))

(defun browser-file-untag-file ()
  "Remove any tags on the current file."
  (interactive)
  (browser-file-do-tag-file nil))

(defun browser-file-get-tag ()
  "Remove any tags on the current file."
  (if (browser-file-this-entry)
      (let ((filenum (browser-file-get-entry-num)))
	(get-text-property 0 'tag (nth filenum browser-list-of-files)))))

(defun browser-file-untag-files ( tag )
  "Remove the supplied tag from all files with this tag."
  (interactive "P")
  (if (null tag)
      (if (null (setq tag (browser-file-get-tag)))
	  (setq tag browser-file-default-tag))
    (let ((cursor-in-echo-area t))
      (message "Character to use as tag ? ")
      (setq tag (read-char-exclusive))))
  (let ((flist browser-list-of-files))
    (while flist
      (if (eq tag (get-text-property 0 'tag (car flist)))
	  (put-text-property 0 1 'tag nil (car flist)))
      (setq flist (cdr flist))))
  (let ((p (point)))
    (browser-display-files)
    (goto-char p)))

(defun browser-file-tag-regexp ( regexp &optional tag )
  "Tag files which match the supplied regexp."
  (interactive "sRegular Expression to match files ? \nP")
  (if (null tag)
      (setq tag browser-file-default-tag)
    (let ((cursor-in-echo-area t))
      (message "Character to use as tag ? ")
      (setq tag (read-char-exclusive))))
  (let ((flist browser-list-of-files))
    (while flist
      (if (string-match regexp (car flist))
	  (put-text-property 0 1 'tag tag (car flist)))
      (setq flist (cdr flist))))
  (let ((p (point)))
    (browser-display-files)
    (goto-char p)))

(defun browser-file-tag-wildcard ( wildcard &optional tag )
  "Tag files which match the supplied regexp."
  (interactive "sWildcard to match files ? \nP")
  (let ((regexp (wildcard-to-regexp wildcard)))
    (if regexp
	(browser-file-tag-regexp regexp tag))))

(defun browser-file-get-tagged-files( &optional tag )
  "Return a list of all files with a given tag.
Use the default tag if no tag is supplied."
  (if (null tag)
      (setq tag browser-file-default-tag))
  (let ((flist browser-list-of-files)
	(fname nil)
	(taglist nil)
	(c nil))
    (while flist
      (setq c (get-text-property 0 'tag (car flist)))
      (if (and c (eq c tag))
	  (progn
	    (setq fname (concat (car flist)))
	    (put-text-property 0 1 'tag nil fname)
	    (setq taglist (nconc taglist (cons fname nil)))))
      (setq flist (cdr flist)))
    taglist))

(defun browser-file-get-files ()
  "Get a list of the files selected."
  (interactive)
  (if (browser-file-this-entry)
      (let ((tag (browser-file-get-tag)))
	(if tag
	    (browser-file-get-tagged-files tag)
	  (let ((filename (browser-file-get-entry)))
	    (if filename
		(list filename)))))))

;; File visiting functions

(defun browser-file-find-file (filename &optional other-window)
  "Visit the current file in the browser file listing window."
  (let ((buffer (find-file-noselect filename))
	(window nil)
	(frame nil))
    (if (and browser-use-frames
	     (not pop-up-frames))
	(progn
	  (if (or (not browser-display-frame)
		  (not (frame-live-p browser-display-frame)))
	      (setq browser-display-frame (make-frame)))
	  (setq frame browser-display-frame)
	  (setq window (frame-selected-window frame))
	  (if (not (eq frame (selected-frame)))
	      (raise-frame frame))
	  (select-window window)
	  (if other-window
	      (display-buffer buffer other-window)
	    (switch-to-buffer buffer)))
      (setq window (display-buffer buffer other-window))
      (setq frame (window-frame window))
      (if (not (eq frame (selected-frame)))
	  (raise-frame frame))
      (select-window window))))

(defun browser-file-do-find-file ()
  "Visit the current file in the browser file listing window."
  (interactive)
  (let ((filename (browser-file-get-file-name)))
    (if filename
	(if (file-directory-p filename)
	    (browser-list-files filename)
	  (browser-file-find-file filename))
      (error "No filename selected."))))

(defun browser-file-view-file ()
  "Visit the current file (in view-mode) in the browser file listing window."
  (interactive)
  (let ((filename (browser-file-get-file-name)))
    (if filename
	(if (file-directory-p filename)
	    (browser-list-files filename)
	  (let ((old-buf (current-buffer))
		(had-a-buf (get-file-buffer filename))
		(buf-to-view (find-file-noselect filename)))
	    (set-buffer buf-to-view)
	    (view-mode old-buf
		       (and (not had-a-buf)
			    (not (buffer-modified-p buf-to-view))
			    'kill-buffer))
	    (select-window (display-buffer buf-to-view))))
      (error "No filename selected."))))

(defun browser-file-find-file-other-window ()
  "Visit the current file in the browser file listing in another window."
  (interactive)
  (let ((filename (browser-file-get-file-name)))
    (if filename
	(if (file-directory-p filename)
	    (browser-new-listing filename)
	  (browser-file-find-file filename t))
      (error "No filename selected."))))

(defun browser-file-mouse-find-file (event)
  "Visit the file clicked on by the mouse."
  (interactive "e")
  (let (filename buffer)
    (save-excursion
      (setq buffer (window-buffer (posn-window (event-end event))))
      (set-buffer buffer)
      (goto-char (posn-point (event-end event)))
      (setq filename (browser-file-get-file-name)))
    (if filename
	(if (file-directory-p filename)
	    (progn
	      (set-buffer buffer)
	      (browser-list-files filename))
	  (browser-file-find-file filename)))))

(defun browser-file-mouse-find-file-other-window (event)
  "Visit the file clicked on by the mouse - in another window."
  (interactive "e")
  (let ((buffer (window-buffer (posn-window (event-end event)))))
    (save-excursion
      (set-buffer buffer)
      (goto-char (posn-point (event-end event)))
      (setq filename (browser-file-get-file-name)))
    (if filename
	(if (file-directory-p filename)
	    (progn
	      (set-buffer buffer)
	      (browser-new-listing filename))
	  (browser-file-find-file filename t))
      (error "No filename selected."))))

(defun browser-find-file-to-open()
  "Find the file at point in the browser file listing buffer"
  (interactive)
  (if (catch 'here
	(let ((buffer (buffer-list)))
	  (while buffer
	    (set-buffer (car buffer))
	    (if (eq major-mode 'browser-file-mode)
		(throw 'here t))
	    (setq buffer (cdr buffer)))))
      (progn
	(goto-char (window-point (get-buffer-window (current-buffer) t)))
	(let ((filename (browser-file-get-file-name)))
	  (if filename
	      (if (file-directory-p filename)
		  (browser-file-list filename)
		(find-file filename))
	    (error "No filename selected."))))))

;; Functions to perform file operations

(defvar browser-highlight-face nil
  "Face used to display highlighted filenames in the browser file listing.")
(if (not browser-highlight-face)
    (progn
      (copy-face 'default 'browser-highlight-face)
      (set-face-foreground 'browser-highlight-face "Black")
      (set-face-background 'browser-highlight-face "LightBlue")
      ))

(defvar browser-highlight-overlay-list nil
  "List of overlays for highlighting filenames.")

(defun browser-file-unhighlight-entry ()
  "Turn off highlighting of entry in the browser listing."
  (interactive)
  (let (overlay)
    (while browser-highlight-overlay-list
      (setq overlay (car browser-highlight-overlay-list))
      (overlay-put overlay 'face nil)
      (overlay-put overlay 'mouse-face 'highlight)
      (setq browser-highlight-overlay-list
	    (cdr browser-highlight-overlay-list)))))
  
(defun browser-file-highlight-entry ()
  "Highlight the current entry in the browser file listing."
  (interactive)
  (if (browser-file-this-entry)
      (let ((overlay-list (overlays-at (point))))
	(while (and overlay-list
		    (null (overlay-get (car overlay-list) 'mouse-face)))
	  (setq overlay-list (cdr overlay-list)))
	(if overlay-list
	    (let ((overlay (car overlay-list)))
	      (setq browser-highlight-overlay-list
		    (nconc browser-highlight-overlay-list (list overlay)))
	      (overlay-put overlay 'face 'browser-highlight-face)
	      (overlay-put overlay 'mouse-face nil))))))

(defun browser-file-highlight-entries ( highlight-list )
  (save-excursion
    (let ((flist browser-list-of-files))
      (goto-char browser-start-of-listing)
      (browser-file-this-entry)
      (while flist
	(if (string-equal (car flist) (car highlight-list))
	    (progn
	      (browser-file-highlight-entry)
	      (setq highlight-list (cdr highlight-list))))
	(browser-file-next-entry 1)
	(setq flist (cdr flist))))))

(defun browser-file-info ()
  "Display more detailed information on file."
  (interactive)
  (let ((flist (browser-file-get-files))
	(directory default-directory))
    (if flist
	(save-excursion
	  (set-buffer (get-buffer-create "*Browser-file-info*"))
	  (widen)
	  (erase-buffer)
	  (cd directory)
	  (browser-call-process (append browser-info-command flist))
	  (goto-char (point-min))
	  (end-of-line)
	  (message (buffer-substring (point-min) (point))))
      (error "No filename selected."))))

(defun browser-out-of-date-p ()
  "Test if the browser listing directory has been modified since last read."
  (let ((buffer-file-name (expand-file-name default-directory)))
    (not (verify-visited-file-modtime (current-buffer)))))

(defun browser-internal-delete-file (filename)
  (if filename
      (if (file-directory-p filename)
	  (delete-directory filename)
	(delete-file filename))
    (error "No filename selected.")))

(defun browser-file-delete-file ()
  "Delete the current file in a browser file listing."
  (interactive)
  (let ((flist (browser-file-get-files)))
    (if flist
	(progn
	  (map-y-or-n-p
	   "Delete file: %s "
	   (function browser-internal-delete-file)
	   flist
	   '("file" "files" "delete"))
	  (if (browser-out-of-date-p)
	      (browser-file-relist)))
      (error "No filename selected."))))

(defun browser-file-byte-compile-file ()
  "Byte compile the current file in a browser file listing."
  (interactive)
  (let ((flist (browser-file-get-files)))
    (if flist
	(progn
	  (map-y-or-n-p
	   "Byte-Compile file: %s "
	   (function byte-compile-file)
	   flist
	   '("file" "files" "Byte-Compile"))
	  (if (browser-out-of-date-p)
	      (browser-file-relist)))
      (error "No filename selected."))))

(defun browser-internal-copy-file ( filename newfilename )
  (if (and filename newfilename)
      (progn
	(if (file-directory-p newfilename)
	    (setq newfilename
		  (expand-file-name (file-name-nondirectory filename)
				    (file-name-as-directory newfilename))))
	(copy-file filename newfilename 1 t))
    (error "No filename selected.")))

(defun browser-file-copy-file ()
  "Copy the current file in the listing to another file or directory."
  (interactive)
  (let ((flist (browser-file-get-files))
	(filename "files"))
    (if flist
	(progn
	  (if (eq (length flist) 1)
	      (setq filename (car flist)))
	  (let ((newfilename (read-file-name
			      (format "Copy %s to: " filename) nil "")))
	    (if (and newfilename (> (length newfilename) 0))
		(if (and (> (length flist) 1)
			 (not (file-directory-p newfilename)))
		    (error "Destination must be a directory if copying more than one file.")
		  (map-y-or-n-p
		   (concat "Copy %s to " newfilename " ")
		   (` (lambda (arg)
			(browser-internal-copy-file arg (, newfilename))))
		   flist
		   '("file" "files" "copy"))
		  (if (browser-out-of-date-p)
		      (browser-file-relist)))
	      (message "%s not copied" filename))))
      (error "No filename selected."))))

(defun browser-file-rename-file ()
  "Rename the current file in the listing."
  (interactive)
  (let ((filename (browser-file-get-entry)))
    (if filename
	(let ((newfilename (read-file-name
			    (format "Rename %s to: " filename) "" "")))
	  (if (and newfilename (> (length newfilename) 0))
	      (progn
		(rename-file filename newfilename 1)
		(if (browser-out-of-date-p)
		    (browser-file-relist)))
	    (message "File %s not renamed" filename)))
      (error "No filename selected."))))

(defun browser-internal-move-file ( filename dest-dir )
  (if (and filename dest-dir)
      (rename-file filename
		   (expand-file-name (file-name-nondirectory filename)
				     (file-name-as-directory dest-dir))
		   1)
    (error "No filename selected.")))

(defun browser-file-move-file ()
  "Move the current file in the listing."
  (interactive)
  (let ((flist (browser-file-get-files))
	(filename "files"))
    (if flist
	(progn
	  (if (eq (length flist) 1)
	      (setq filename (car flist)))
	  (let ((dest-dir (read-file-name
			   (format "Move %s to directory: " filename) nil "")))
	    (if (and dest-dir (> (length dest-dir) 0))
		(if (not (file-directory-p dest-dir))
		    (error (substitute-command-keys "Destination must be a directory - use \\[browser-file-rename-file] to rename a file."))
		  (map-y-or-n-p
		   (concat "Move %s to " dest-dir " ")
		   (` (lambda (arg)
			(browser-internal-move-file arg (, dest-dir))))
		   flist
		   '("file" "files" "move"))
		  (if (browser-out-of-date-p)
		      (browser-file-relist)))
	      (message "%s not moved" filename))))
      (error "No filename selected."))))

(defun browser-file-symlink-file (flag)
  "Make a symbolic link to the current file in the listing."
  (interactive "P")
  (let ((filename (browser-file-get-entry)))
    (if filename
	(let ((newfilename (read-file-name
			    (format "Link to %s - Name of link: " filename) nil "")))
	  (if (and newfilename (> (length newfilename) 0))
	      (progn
		(setq filename (expand-file-name filename))
		(setq newfilename (expand-file-name newfilename))
		(setq filename (file-relative-name filename
						   (file-name-directory newfilename)))
		(make-symbolic-link filename newfilename 1)
		(if (browser-out-of-date-p)
		    (browser-file-relist)))
	    (message "File %s not linked" filename)))
      (error "No filename selected."))))

(defun browser-drag-and-drop-file (event action)
  "Track mouse movements till a button is released.
Returns a list containing the name of the file first clicked on, and
the directory name where the mouse button was released, and a symbol
representing the action slected by the user ('copy or 'move)."
  (mouse-minibuffer-check event)
  (let* ((posn (event-start event))
	 (window (posn-window posn))
	 (start-buffer (window-buffer window))
	 (end-buffer nil)
	 (filename nil)
	 (flist nil)
	 (dirname nil)
	 (file-point nil))
    (set-buffer start-buffer)
    (if (not (eq major-mode 'browser-file-mode))
	(error "Not a File Browser window"))
    (save-excursion
      (mouse-set-point event)
      (setq file-point (point))
      (if (setq flist (browser-file-get-files))
	  (browser-file-highlight-entries flist)))
    (if flist
	(progn
	  (deactivate-mark)
	  (unwind-protect
	      (save-excursion
		(if (eq (length flist) 1)
		    (setq filename (car flist))
		  (setq filename "files"))
		(while (progn
			 (if (char-or-string-p event)
			     (cond ((eq event ?m)
				    (setq action 'move))
				   ((eq event ?c)
				    (setq action 'copy))
				   (t
				    (beep t))))
			 (cond ((eq action 'move)
				(message "Move %s to.. (drag and release,`m'-Move,`c'-Copy)" filename))
			       ((eq action 'copy)
				(message "Copy %s to.. (drag and release,`m'-Move,`c'-Copy)" filename)))
			 (setq event (read-event))
			 (or (not (listp event))
			     (mouse-movement-p event)
			     (eq (car-safe event) 'switch-frame))))
		(if (not (consp event))
		    ()
		  (setq posn (event-end event)
			window (posn-window posn)
			end-buffer (window-buffer window))
		  (if (eq start-buffer end-buffer)
		      (mouse-set-point event)
		    (set-buffer end-buffer)
		    (if (not (eq major-mode 'browser-file-mode))
			(if (not (eq major-mode 'browser-mode))
			    (error "Drag to a non-File Browser window")
			  (mouse-set-point event)
			  (setq dirname (browser-get-dir-name)))
		      (setq dirname default-directory)))))
	    (save-excursion
	      (goto-char file-point)
	      (browser-file-unhighlight-entry)))
	  (if (and flist dirname)
	      (list flist dirname action)
	    (cond ((eq action 'copy)
		   (error "%s not copied." filename))
		  ((eq action 'move)
		   (error "%s not moved." filename)))))
      (error "No filename selected."))))

(defun browser-drag-copy-or-move-file (event action)
  "Copy or move a file on the disk, as a user \"drags\" a file to another
browser window or directory entry."
  (unwind-protect
      (let ((x-pointer-shape x-pointer-hand2))
	(set-mouse-color (cdr (assoc 'mouse-color (frame-parameters))))
	(let ((from-to (browser-drag-and-drop-file event action))
	      (filename nil)
	      (flist nil)
	      (newfilename nil)
	      (action-name nil))
	  (if (not from-to)
	      ()
	    (setq flist (nth 0 from-to))
	    (setq newfilename (nth 1 from-to))
	    (setq action (nth 2 from-to))
	    (cond ((eq action 'copy)
		   (setq action-name "Copy"))
		  ((eq action 'move)
		   (setq action-name "Move")))
	    (map-y-or-n-p
	     (concat action-name " %s to " newfilename " ")
	     (cond ((eq action 'copy)
		    (` (lambda (arg)
			 (browser-internal-copy-file arg (, newfilename)))))
		   ((eq action 'move)
		    (` (lambda (arg)
			 (browser-internal-move-file arg (, newfilename))))))
	     flist
	     (list "file" "files" action-name))
	    (if (browser-out-of-date-p)
		(browser-file-relist))
	    (save-excursion
	      (let ((dirname (file-name-directory newfilename)))
		(mapcar (function
			 (lambda (buffer)
			   (set-buffer buffer)
			   (if (and (string-equal dirname
						  default-directory)
				    (browser-out-of-date-p))
			       (browser-file-relist))))
			(cdr browser-buffer-list)))))))
    (set-mouse-color (cdr (assoc 'mouse-color (frame-parameters))))))

(defun browser-drag-copy-file (event)
  "Copy a file on the disk, as a user \"drags\" a file to another
browser window or directory entry."
  (interactive "e")
  (browser-drag-copy-or-move-file event 'copy))

(defun browser-drag-move-file (event)
  "Move a file on the disk, as a user \"drags\" a file to another
browser window or directory entry."
  (interactive "e")
  (browser-drag-copy-or-move-file event 'move))

(suppress-keymap browser-file-mode-map)

;; Browser buffer navigation keys
(define-key browser-file-mode-map [tab]   'browser-file-next-entry)
(define-key browser-file-mode-map [delete]    'browser-file-prev-entry)
(define-key browser-file-mode-map [backspace] 'browser-file-prev-entry)
(define-key browser-file-mode-map "p"     'previous-line)
(define-key browser-file-mode-map "n"     'next-line)

;; Browser action keys
(define-key browser-file-mode-map " "     'browser-file-do-find-file)
(define-key browser-file-mode-map "o"     'browser-file-find-file-other-window)
(define-key browser-file-mode-map "v"     'browser-file-view-file)
(define-key browser-file-mode-map "g"     'browser-file-relist)
(define-key browser-file-mode-map "t"     'browser-file-tag-file)
(define-key browser-file-mode-map "u"     'browser-file-untag-file)
(define-key browser-file-mode-map "U"     'browser-file-untag-files)
(define-key browser-file-mode-map "q"     'kill-this-buffer)
(define-key browser-file-mode-map "Q"     'browser-quit)
(define-key browser-file-mode-map "."     'browser-file-info)
(define-key browser-file-mode-map "@"     'browser-configure-windows)
(define-key browser-file-mode-map "*"     'browser-file-tag-wildcard)
(define-key browser-file-mode-map "%"     'browser-file-tag-regexp)

;; File operation keys
(define-key browser-file-mode-map "d"     'browser-file-delete-file)
(define-key browser-file-mode-map "b"     'browser-file-byte-compile-file)
(define-key browser-file-mode-map "c"     'browser-file-copy-file)
(define-key browser-file-mode-map "r"     'browser-file-rename-file)
(define-key browser-file-mode-map "m"     'browser-file-move-file)
(define-key browser-file-mode-map "s"     'browser-file-symlink-file)

;; Mouse button bindings
(define-key browser-file-mode-map [mouse-2] 'browser-file-mouse-find-file)
(define-key browser-file-mode-map [S-mouse-2] 'browser-file-mouse-find-file-other-window)
(define-key browser-file-mode-map [S-down-mouse-1] 'browser-drag-copy-file)
(define-key browser-file-mode-map [M-S-down-mouse-1] 'browser-drag-move-file)


;; Menu bar bindings
(require 'easymenu)

(define-key browser-file-mode-map [menu-bar edit] 'undefined)

(defconst browser-fileops-default-menu
  '("Operate"
    ["Open"			browser-file-do-find-file 'browser-fileops-files]
    ["Copy"			browser-file-copy-file t]
    ["Rename"			browser-file-rename-file t]
    ["Move"			browser-file-move-file t]
;    ["Symbolic Link"		browser-file-symlink-file t]
    ["Delete"			browser-file-delete-file t]
;    ["Display Info"		browser-file-info t])
    ["Tag"			browser-file-tag-file t]
    ["Untag"			browser-file-untag-file t]
    ("Compress"
     ["Gzip"			(browser-shell "gzip %s") t]
     ["Gunzip"			(browser-shell "gunzip %s") t]
     ["Compress"		(browser-shell "compress %s") t]
     ["UnCompress"		(browser-shell "uncompress %s") t])
    )
  "Easy-menu menu for Browser-file-mode File Operations menu.")

(easy-menu-define browser-file-operate-menu-keymap browser-file-mode-map
		  "Browser File Operations menu" browser-fileops-default-menu)

(defvar browser-fileops-default-keymap nil
  "Keymap for default file operations menu.")

;; Emacs version < 19.28 does not set the keymap variable - so look it up
(setq browser-fileops-default-keymap
      (or (and (boundp 'browser-file-operate-menu-keymap)
	       browser-file-operate-menu-keymap)
	  (lookup-key browser-file-mode-map [menu-bar Operate])))

(defconst browser-file-browser-menu
  '("Browser"
    ["Open File"		browser-file-do-find-file t]
    ["Open File - other window" browser-file-find-file-other-window t]
    ["View File - read-only"	browser-file-view-file t]
    ["Display File Info"	browser-file-info t]
    ["Re-read"			browser-file-relist t]
    ["Redraw Windows"		browser-configure-windows t]
    ["Kill Buffer"		kill-this-buffer t])
  "Menu definition for Browser-file-mode Browser menu")

(easy-menu-define browser-file-browser-menu-keymap browser-file-mode-map
		  "Browser menu" browser-file-browser-menu)

(defun browser-shell ( fmt )
  (if browser-fileops-files
      ;; If last-nonmenu-event is set to a mouse event map-y-or-n-p
      ;; pops up a dialog box for user input. This does not work for
      ;; cascaded menus - this is an ugly hack to get around this.
      (let ((event (this-command-keys)))
	(if (and (vectorp event)
		 (eq (aref event 0) 'menu-bar))
	    (setq event (aref event 1))
	  (setq event last-nonmenu-event))
	(let ((last-nonmenu-event event))
	  (map-y-or-n-p (concat fmt " ")
			(` (lambda ( arg )
			     (let ((command (format (, fmt) arg)))
			       (message "Shell command: %s..." command)
			       (shell-command command))))
			browser-fileops-files
			'("Command" "Commands" "Execute"))))
    (error "No filename selected."))
  (setq browser-fileops-files nil)
  (if (browser-out-of-date-p)
      (browser-file-relist)))

(defvar browser-fileops-menu-alist
  '(("\\.el$"
     ["Byte-Compile"		browser-file-byte-compile-file t]
     ["Load Elisp file"		(mapcar 'load-file browser-fileops-files) t])
    ("[mM]akefile"
     ["Make"			compile t])
    ("News"
     ["GNUS"			gnus t])
    ((lambda ( arg ) (and (file-executable-p arg) (not (file-directory-p arg))))
     ["Execute"			(browser-shell "%s") t]
     ["Execute in background"	(browser-shell "%s &") t])
    (file-directory-p
     ["Dired"			(mapcar dired browser-fileops-files) t])
    )
  "Builtin list of context sensitive menus for file operations.")

(defconst browser-fileops-keymap-alist nil
  "List of keymaps for the file operations popup menu.
The keymaps are derived from the contents of the
\"browser-fileops-menu-alist\" variable.")

(defun browser-add-menu ( menu-alist )
  "Add new menus to the list of dynamic menus for file operations.
(browser-add-menu MENU_ALIST)
MENU-ALIST should be a list of sub-lists. Each sub-list contains a
regular expression for matching filenames, and an easy-menu style menu
list. If the regexp matches the current filename (clicked on by mouse),
this menu will be added to the File Operations menu-bar menu, or the
File Operations popup menu.
See the variable - browser-fileops-menu-alist - for the correct
format of MENU-ALIST."
  (setq browser-fileops-menu-alist
	(append browser-fileops-menu-alist menu-alist))
  (setq browser-fileops-keymap-alist
	(mapcar (function
		 (lambda ( menu )
		   (cons (car menu)
			 (easy-menu-create-keymaps (car menu) (cdr menu)))))
		browser-fileops-menu-alist)))

(browser-add-menu nil)

(defun browser-make-fileops-keymap ( filelist )
  "Make a dynamic filename dependant keymap for the file operations menu."
  (let ((fileopslist browser-fileops-keymap-alist)
	(filename (car-safe filelist))
	menu selector)
    (if (null filename)
	(setq menu (list 'keymap (list nil "No File Selected")))
      (if (= (length filelist) 1)
	  (setq menu (list 'keymap (list nil (concat "File: " filename))))
	(setq menu (list 'keymap
			 (list nil (concat "Files: " filename))
			 (list nil (concat "   ... " (nth (1- (length filelist))
							  filelist))))))
      (while fileopslist
	(setq selector (car (car fileopslist)))
	(if (if (stringp selector)
		(string-match selector filename)
	      (funcall selector filename))
	    (setq menu (append menu (cons '(nil "---")
					  (cdr (cdr (car fileopslist)))))))
	(setq fileopslist (cdr fileopslist))))
    (setq menu (append menu (cons '(nil "---")
				  (cdr browser-fileops-default-keymap))))))

(defun browser-popup-fileops-menu ( event )
  "Popup a menu of file operations for file under pointer."
  (interactive "e")
  (save-excursion
    (set-buffer (window-buffer (posn-window (event-start event))))
    (goto-char (posn-point (event-start event)))
    (let (menu keyseq func)
      (setq browser-fileops-files (browser-file-get-files))
      (setq menu (browser-make-fileops-keymap browser-fileops-files))
      (and (setq keyseq (x-popup-menu event menu))
	   (setq func (lookup-key menu (apply 'vector keyseq)))
	   (command-execute func))))
  (setq browser-fileops-files nil))

(defun browser-dynamic-fileops-menu ()
  "Bind a dynamic file operations menu to the Operate menu in the menu-bar."
  (setq browser-fileops-files (browser-file-get-files))
  (define-key browser-file-mode-map [menu-bar Operate]
    (cons "Operate" (browser-make-fileops-keymap browser-fileops-files))))

(define-key browser-file-mode-map [down-mouse-3] 'browser-popup-fileops-menu)

(provide 'filebrowser)
