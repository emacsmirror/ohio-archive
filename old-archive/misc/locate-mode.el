;; Locate.el: interface to the locate command
;;
;; Copyright (C) Peter Breton 17Sept95 
;; Happy Birthday Alice!
;;
;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; locate.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; LCD Archive Entry:
;; locate-mode|Peter Breton|pbreton@cs.umb.edu|
;; Search a database of files and run dired commands on results|
;; 17-Sept-1995|1.0|~/misc/locate-mode.el.gz|
;;
;; Purpose:
;; 
;; Searches a database of files and uses dired commands on
;; the results. 
;;

;; Installation:
;;
;; Place these in your .emacs file:
;; 
;; ;; redefines dired-get-filename as a switch function
;;
;; (require 'advice)
;; (defadvice dired-get-filename (around check-mode activate)
;;   "Use an alternative function in Locate mode"
;;   (cond ((eq major-mode 'locate-mode)
;;	  (setq ad-return-value (locate-get-filename)))
;;	 (t
;;	  ad-do-it)))
;;
;; (autoload 'locate "locate"
;;  "Run the locate command, putting results in *Locate* buffer" t)
;; (autoload 'locate-with-filter "locate"
;;  "Run the locate command with a filter" t)
;;

;; Commentary:
;;
;; Locate.el provides an interface to a program which searches a
;; database of file names. By default, this program is the GNU locate
;; command, but it could also be the BSD-style find command, or even a
;; user specified command like "zcat $FCODES | grep -i "
;;
;; To use the BSD-style "fast find", or any other shell command of the
;; form 
;;
;;   SHELLPROGRAM  Name-to-find
;;
;; set the variable locate-command in your .emacs file.
;;
;;   To use a more complicated expression, create a function which 
;; takes a string (the name to find) as input and returns a shell
;; command to be executed. Then do
;;
;; (setq locate-make-command-line 'my-locate-command-line) 
;;
;; in your .emacs, using the name of your function in place of my-locate-command-line
;;
;; You should make sure that whichever command you use works correctly
;; from a shell prompt. GNU locate and BSD find expect the file databases
;; to either be in standard places or located via environment variables.
;; If the latter, make sure these environment variables are set in
;; your .emacs process
;;
;; Locate-mode assumes that each line output from the locate-command
;; consists exactly of a file name, possibly preceded or trailed by
;; whitespace. If your file database has other information on the line (for
;; example, the file size), you will need to redefine the function 
;; locate-get-file-positions to return a list consisting of the first
;; character in the file name and the last character in the file name.
;;
;; To use locate-mode, simply type M-x locate and then the string
;; you wish to find. You can use almost all of the dired commands in
;; the resulting buffer.  It is worth noting that your commands do not,
;; of course, affect the file database. For example, if you
;; compress a file in the locate buffer, the actual file will be
;; compressed, but the entry in the file database will not be
;; affected. Consequently, the database and the filesystem will be out
;; of sync until the next time the database is updated
;;
;; The command locate-with-filter pipes the results
;; through grep first; this is often useful to constrain a big search.
;;

;; Code:

(defvar locate-command "locate"
 "*The executable program used to search a database of files")

(defvar locate-history-list nil
  "The history list used by the locate command")

;; This tricks lets us use, for example, "zcat $FCODES | grep -i arg"
;; as the command line 
(defvar locate-make-command-line 'locate-default-make-command-line
  "*Function used to create the locate command line")

(defun locate-default-make-command-line (arg)
  "Combine the locate-command and arg into a shell command"
  (concat locate-command " " arg))

(defun locate-filter-make-command-line (arg filter)
  "Make a locate shell command using the supplied filter"
   (concat locate-command " " arg " | grep -i " filter ))

(defun locate (arg &optional filter)
  "Run the locate command, putting results in *Locate* buffer"
  (interactive
      (list (read-from-minibuffer "Locate: " nil nil
		  nil 'locate-history-list)))
  (let ((pop-up-windows 1))

    ;; Find the Locate buffer
    (if (not (string-equal (buffer-name) "*Locate*"))
       (switch-to-buffer-other-window "*Locate*"))

    (locate-mode)
    (erase-buffer)

    ;; Now run the locate command
    (if filter
	(shell-command (funcall locate-make-command-line arg filter) t)
      (shell-command
          (funcall locate-make-command-line arg) t))

    (locate-do-setup)))

(defun locate-with-filter (arg filter)
  "Run the locate command with a filter"
  (interactive
      (list (read-from-minibuffer "Locate: " nil nil
		  nil 'locate-history-list)
            (read-from-minibuffer "Filter: " nil nil
		  nil 'grep-history)))
 (let ((locate-make-command-line 'locate-filter-make-command-line)) 
   (locate arg filter)))


(defvar locate-mode-map nil "Local keymap for locate-mode buffers.")

(if locate-mode-map
    nil

   (require 'dired)

   (setq locate-mode-map (copy-keymap dired-mode-map))

   ;; Undefine Useless Dired Menu bars
   (define-key locate-mode-map [menu-bar subdir]  'undefined)

   (define-key locate-mode-map [menu-bar mark executables] 'undefined)
   (define-key locate-mode-map [menu-bar mark directory]   'undefined)
   (define-key locate-mode-map [menu-bar mark directories] 'undefined)
   (define-key locate-mode-map [menu-bar mark symlinks]    'undefined)

   (define-key locate-mode-map [mouse-2] 'mouse-locate-view-file)
   (define-key locate-mode-map "\C-ct"   'locate-tags)

   (define-key locate-mode-map "U"       'dired-unmark-all-files-no-query)

)

(defun locate-get-file-positions ()
  (save-excursion
     (end-of-line)
     (let ((eol (point)))
       (beginning-of-line)

       ;; Assumes names begin with first non-whitespace char
       ;; and end at the end of the line
       (skip-chars-forward " " eol)
       (list (point) eol))))
 	   
(defun locate-get-filename ()
  (let ((pos (locate-get-file-positions)))
       (buffer-substring (elt pos 0) (elt pos 1))))

(defun mouse-locate-view-file (event)
  "View a file in Locate mode using the mouse"
  (interactive "@e") 
  (save-excursion
    (goto-char (posn-point (event-start event)))
      (view-file (locate-get-filename))))

;; Define a mode for locate
;; Default directory is set to "/" so that dired commands, which
;; expect to be in a tree, will work properly
(defun locate-mode ()
  "Mode for using locate command"
  (use-local-map             locate-mode-map)
  (setq major-mode          'locate-mode
        mode-name           "Locate"
        default-directory   "/"
	dired-subdir-alist  (list (cons "/" (point-min-marker))))
  (run-hooks 'locate-mode-hook))

(defun locate-do-setup ()     
   (save-excursion
     (goto-char (point-min))
     (while (progn
	  (locate-set-indentation)
          (locate-set-properties)
	  (zerop (forward-line))))))

;; Put some spaces in front of each line
;; for dired marks
(defun locate-set-indentation ()
  (save-excursion
    (beginning-of-line)
    (insert "   ")))


(defun locate-set-properties ()
  (save-excursion
    (let ((pos (locate-get-file-positions)))
      (add-text-properties (elt pos 0) (elt pos 1)
			   '(mouse-face highlight)))))

(defun locate-tags ()
  "Visit a tags table in *Locate* mode"
  (interactive)
  (let ((tags-table (locate-get-filename)))
   (if (y-or-n-p (format "Visit tags table %s? " tags-table)) 
     (visit-tags-table tags-table)
    nil)))

;;
;; End of locate.el
;;


			Peter

-------------------------------------------------------------------------
Peter Breton  pbreton@cs.umb.edu          PGP key by finger
=========================================================================
Shave the whales!
