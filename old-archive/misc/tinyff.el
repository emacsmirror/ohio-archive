;; @(#) tinyff.el -- Regexp find file utility

;; @(#) $Id: tinyff.el,v 1.11 1995/02/27 14:48:51 jaalto Release_5 $
;; @(#) $Keywords: tools, file $
;; $KnownCompatibility: 18.57 19.28 $

;; This file is *NOT* part of GNU emacs

;;{{{ Documentation

;; Copyright (C) 1994, Jari Aalto.
;; Author:       Jari Aalto <jaalto@tre.tele.nokia.fi>
;;               (Nov. 12. 1992  Nokia Telecommunications)
;; Maintainer:   Jari Aalto <jaalto@tre.tele.nokia.fi>
;; Created:      Nov 3rd 1994
;; Version:      $Revision: 1.11 $
;; state:        $State: Release_5 $
;;
;; To get information on this program use ident(1) or do M-x tiff-version


;; LCD Archive Entry:
;; tinyff|Jari Aalto|jaalto@tre.tele.nokia.fi|
;; Regexp find file utility|
;; 03-Nov-1994|1.11|~/misc/tinyff.el.Z|

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

;;; Intallation:

;; Put this file on your Emacs-Lisp load path, add following into your
;; ~/.emacs startup file, the autoload choice is preferrable.
;;
;;     ;;  regular load
;;     (require 'tinyff)
;;     (tiff-install-keys "g")     ; if you want default keys for 18,19
;;
;;     ;;  Autoload
;;     ;;  this will install keys too
;;     (autoload 'tiff-find-file "tinyff" "" t)
;;     (setq tiff-autoload-hook '(lambda () (tiff-install-keys "g")))
;;


;;; Commentary:

;;; Briefly::
;; o	Finds files by user supplied emacs regexp. Two funcs used.
;;        1) gets files and lets user edit regexp results
;;        2) when satisfied, use another function to load files in buffer.
;;	     This case can be by-passed, if wanted.
;; o	Keeps log about failed files & reasons why not loaded.
;; o	Thrashes temp buffers automatically if wanted.


;; PREFACE
;; ========================================
;; - Inspired by post that I read in gnu.emacs.help in 24 Oct 1994:
;;   "Subject: Re: Q: Wild card to specify a file to be read? (another try)"
;;   The poster  Kai Grossjohann <grossjoh@ls6.informatik.uni-dortmund.de>
;;   told that "There is regexp-find-file.el...." , but I was unable
;;   to locate such packet. Neither did anyone else seemed to know
;;   where it could be ftp'd.
;; - At the time of reading the article I really wasn't hungry for such
;;   .el, but after couple of days when I started C++ project at work
;;   I really missed such beast --> eventually this .el was born.

;; RESERVED prefix FOR FUNCTIONS IN THIS FILE
;; ========================================
;; - To avoid collisions to other modules I use "tiff-" in front of
;;   every function & variable. It stands for '(ti)ny (f)ind (f)ile.
;; - variable names contain letters '-v-'.

;; HISTORY OF CHANGES
;; ========================================
;;
;; Feb	27	1995	[jaalto]	19.28	v1.11	Release_5
;; - Corrected LCD entry. Now supports Autoload.
;;
;; Jan  11	1995	[jaalto]	19.28	v1.10	Release_4
;; - If user deleted some files from the list, they were still
;;   loaded! --> The copy-region-as-kill was bad choice for copying
;;   the files to tmp buffer. Now uses copy-to-buffer.
;; - If regexp deleted all files, or it didn't find any, user got
;;   empty buffer. Now it just prints message.
;; - Corrected window config restore bug. Changed nicer installation.
;;   Now informs how many files were loaded
;;
;; Dec  14	1994    [jaalto]	18.57	 v1.9	Release_3
;; - Corr. 2 typing errors, the find-file-this-window refused to load.
;;
;; Dec  13	1994	[jaalto]	18.57	 v1.8	Release_2
;; - Corrected several bugs. Now tries to keep windows as they were.
;; - find-file Prefix arg is now user-variable. Tested in 19.28
;; - Keybindings.
;;
;; Nov	16	1994 	[jaalto]	18.57	 v1.5	Release_1
;; Nov	3 	1994 	[jaalto]	18.57	 v1.1
;; - First implementation


;; To do list:
;; ========================================
;;  - Let me know what else is needed

;;}}}

(require 'sort)
(provide 'tinyff)

;;; Code:

;;{{{ setup: user configurable

;;; ..... Keybingings ..........................................&bind ...


(defun tiff-set-keys (key-func)
  "Installs keys."
    (if (string< emacs-version "19")
	(progn
	  (funcall key-func "\C-c\~" 'tiff-find-file )
	  (funcall key-func "\C-c\`" 'tiff-find-file-this-buffer ))

      ;; I suppose we are in 18 then, with extra ESC modifier
      (funcall key-func "\C-x\M-f" 'tiff-find-file )
      (funcall key-func "\C-c\M-f" 'tiff-find-file-this-buffer )
      )
    nil					;if used in hook, must return NIL
    )

(defun tiff-install-keys (env)
  "Installs 'local or 'global keybindings to ENV"
  (interactive "slocal global both [lgb]: ")
  (cond
   ((string-equal env "l")
    (tiff-set-keys 'local-set-key))
   ((string-equal env "g")
    (tiff-set-keys 'global-set-key))
   ((string-equal env "b")
    (tiff-set-keys 'local-set-key)
    (tia-set-keys 'global-set-key))
   ))




;;; ........................................................ &autoload ...

(defvar tiff-v-autoloaded nil
  "Changed to t when autoloaded")

(defvar tiff-autoload-hook nil
  "*Run only once, when autoloaded")

;;    You can hook up the installation of keys.
(defun tiff-autoload ()
  "Will run tiff-autoload-hook when autoloaded."
  (interactive)
  (if tiff-v-autoloaded nil		;already loaded
    (setq tiff-v-autoloaded t)
    (run-hooks 'tiff-autoload-hook)))



;;; ..... user configurable ................................... &conf ...

(defvar tiff-v-ign-case nil
  "*When using RE match , should we ignore case? Identical to
case-fold-search. t means: yes, ignore case.")

(defvar tiff-v-ls-cmd "ls"
  "*Way to to get filename information. Should return only files, where
directories are marked with trailing slash.")

(defvar tiff-v-ls-cmd-o "-a1F"
  "*How to get files: every file should be on its own row.
Directories must have trailing slash.")

(defvar tiff-v-ls-long-cmd-o "-lF"
  "*How to get files in long form: every file should be on its own row.
Directories must have trailing slash.")


(defvar tiff-v-ign-files-re
  "^\\(\.\.\\|\.\\)$\\|\\(\.[o~#]\\)$\\|/$"
  "*Files that are discarded. The default setting ignores:
. ..    directories.
/       general directory
.o      object files
~#      emacs backup and restore files.
")

(defvar tiff-v-trash-buf nil
  "*If set to non-nil always kills temporary buffers, so that they don't
float around your emacs. ")


(defvar tiff-default-parg nil
  "Default prefix argument to use with tiff-find-file.
nil Has no effect on behaviout, t forces always to load directly whithout
editing the files.")

;;}}}
;;{{{ setup: -- private

;;; ..... private variables ................................ &private ...

(defvar tiff-v-file-buf "*tff*"
  "*The name of the buffer used when loading files.
Should be named as temporary with * marks surrounding.")

(defvar tiff-v-file-buf-error "*tff-err*"
  "*Files that caused problems and thus were not loaded are stored here.")

(defconst tiff-v-kill-ls-F-chars "[*@|]"
  "The ls -F procuces [* exe] [@ link] [| fifo], which are not part of
file name. We remove these.")

(defconst tiff-v-wbuf-cycle nil
  "The windows in view.")

(defconst tiff-v-buffer-list
  "Order of buffers.")

;;}}}
;;{{{ setup: -- version

;;; ..... version notice ................................... &version ...

(defconst tiff-version
  "$Revision: 1.11 $"
  "Latest version number.")

(defconst tiff-version-id
  "$Id: tinyff.el,v 1.11 1995/02/27 14:48:51 jaalto Release_5 $"
  "Latest modification time and version number.")

(defconst tiff-version-doc
  "tinyff.el -- Regexp find file utility

First created: Nov 3rd 1994
Author       : Jari Aalto <jaalto@tre.tele.nokia.fi
Maintainer   : Jari Aalto <jaalto@tre.tele.nokia.fi

Suggestions and enchancements welcome!"
  "Brief version and contact info")

;;; ----------------------------------------------------------------------
(defun tiff-version ()
  "tinycom.el information."
  (interactive)
  (let* ((ob (current-buffer))
	 (bp (get-buffer-create "*version*"))
	 )
      (set-buffer bp)			;for insert
      (erase-buffer)
      (switch-to-buffer-other-window bp)
      (insert
       tiff-version-doc
       "\ncurrent version:\n" tiff-version-id)
      (pop-to-buffer  ob)
    ))

;;}}}

;;{{{ code: general

;;; ############################################################# &misc ##

;;; ----------------------------------------------------------------------
;;;
(defun tiff-get-win-cycle ()
  "Gathers all windows visible."
  (let* ((loop t)
	 (s (selected-window))		;start window
	 (l (list s))				;list
	 (w s)				;current cycle
	 ww
	 )

    (while loop
      (setq ww (next-window w))
      (setq w ww)			;change
      (other-window 1)			;move fwd
      (if (eq ww s)			;back to beginning ?
	  (setq loop nil)
	(setq l (append l (list ww)))))
    l
    ))



;;; ----------------------------------------------------------------------
;;;
(defun tiff-get-wbuf-cycle ()
  "Gathers all _buffers_ visible, according to window cycling."
  (let* ((loop t)
	 (s (selected-window))		;start window
	 (l (list (window-buffer s)))	;list
	 (w s)				;current cycle
	 ww
	 )

    (while loop
      (setq ww (next-window w))
      (setq w ww)			;change
      (other-window 1)			;move fwd
      (if (eq ww s)			;back to beginning ?
	  (setq loop nil)
	(setq l (append l (list (window-buffer ww))))))
    l
    ))




;;; ----------------------------------------------------------------------
;;;
(defun tiff-trash-buf (bp &optional flag)
  "Kills buffers used in this module. FLAG kills all always."
  (let* ((b1 (get-buffer tiff-v-file-buf))
	 (b2 (get-buffer tiff-v-file-buf-error))
	 )
    (if flag
	(progn
	  (kill-buffer b1) (kill-buffer b2))
      (if (get-buffer bp) (kill-buffer bp)))
    ))




;;; ----------------------------------------------------------------------
;;;
(defun tiff-subs-last-word (beg end)
  "Substitutes region with last words of every line. Words are supposed to
be separated by spaces and tabs."
  (interactive "r" )
  (let* (str
	 pb pe
	 (move t))
    (save-excursion
      (goto-char beg)
      (while (and move
		  (< (point) end))
	(beginning-of-line) 	(setq pb (point))
	(end-of-line)           (setq pe (point))
	;;  find word, first ignore trailing spaces
	(skip-chars-backward " \t" pb)
	(if (null (re-search-backward "[ \t]" pb t)) nil
	  ;;  Was is word really ?
	  (if (not (looking-at " [^- \t/]")) nil ;skip, not a word
	    (setq str (buffer-substring (1+ (point)) pe))
	    (beginning-of-line) (kill-line)
	    (insert str)))
	;; forward word indicates if we're at the end of buffer.
	(setq move (eq 0 (forward-line 1)))
	))
    ))

;;}}}

;;; ########################################################### &file  ###
;;; ----------------------------------------------------------------------
;;;
(defun tiff-find-file-this-buffer (&optional buffer)
  "Loads all files listed in separate lines in this buffer.
The files must on their own lines and stick to left margin. Trailing
spaces are ignored. If there are multiple words on the line, the
last one is considered as filename (eg. ls -l listing).

If file was not loaded, the buffer *rff-err* will contain list of files
that were not loaded.

Hint: narrow-to-region is quite usefull sometimes!
"
  (interactive)
  (let* ((ob (current-buffer))		;spare the save-excur cmd
	 (btmp "*rff-tmp*")
	 (def-buf tiff-v-file-buf)	;default load buf
	 (bn tiff-v-file-buf-error)
	 (buf (get-buffer-create btmp))
	 (file-re "^[^-~\n\t ]")	;skip these: they are not valid files
	 (file-end "[ \t\n]")
	 fn p err
	 (not-loaded 0)
	 (trash  tiff-v-trash-buf)
	 count				;how many files to load
	 )

    (if buffer (set-buffer buffer))

    ;; We do all editing unvisible to user. Use tmp buffer
    ;; NOTE:  We can't use copy-region-as-kill -- yank pairs, because
    ;;        it would 'yank' back all text user just killed
    ;;        to form appropriate files. Somehow the user's kill is
    ;;        inserted into buffer.

    (save-excursion (set-buffer buf) (erase-buffer) )
    (copy-to-buffer buf (point-min) (point-max)) ;move files there
    (set-buffer buf)

    ;;  Now there is only fienames
    (goto-char (point-min))
    (setq count (count-lines (point-min) (point-max)))

    (while (re-search-forward file-re nil t)
      (beginning-of-line) (setq p (point))
      ;;   the RE moves automatically to next line.
      (setq fn nil)
      (if (re-search-forward file-end nil t)
	  (setq fn (buffer-substring p (- (point) 1))))

      (setq err nil)
      (if (file-readable-p fn)
	  (if (null (find-file-noselect fn))
	      (setq err " find-file error while loading."))
	(setq err " Not readable.\n"))

      (if (null err) nil		;loaded ok
	(setq bp (get-buffer-create bn))
	(save-excursion
	  (set-buffer bp)
	  (if (eq 0 not-loaded) (erase-buffer))	;add first item
	  (insert (concat fn " Not readable.\n")))
	(setq not-loaded (1+ not-loaded)))
      ) ;; ......................................................... while end

    (kill-buffer (current-buffer))

    ;;   Remove window if it's created by tiff-find-file
    (if (not (equal (buffer-name ob) def-buf)) nil
      (bury-buffer ob)

      (if (eq 0 (length tiff-v-wbuf-cycle ))
	  (delete-window (selected-window)) ;at start there was nothing else
	;; User called us from multi window view...
	(switch-to-buffer (nth 1 tiff-v-wbuf-cycle))
	)
      ;;  clear variables
      (setq tiff-v-win-cycle   nil)
      )

    (if (= not-loaded 0)
	(message (concat "tiff: All files loaded ok [" count "]" ))
      (if trash (tiff-trash-buf t t))	;kill both
      (message (concat "tiff: Couldn't load " not-loaded " files.")))
    (set-buffer ob)			;return to original
    ))


;;; ----------------------------------------------------------------------
;;;
(defun tiff-find-file (dir re &optional cmd-type arg)
  "Loads files according to regexp. Lets user to choose/edit files to
load, when match list is constructed.

dir           : directory for files
re            : lisp regexp to pick files
cmd-type      : [Yes]     =  use ls -l otw no long listing
arg           : [non-nil] = files are loaded with no-questions-asked.
"
  (interactive "Ddirectory: \nsFind file RE: \nsls -l y/[n]: \nP")
  (let* (
	 (arg (or arg tiff-default-parg)) ;if not given, use default then

	 (bn tiff-v-file-buf)
	 (bp (get-buffer-create bn))
	 (plain-files-cmd tiff-v-ls-cmd)
	 (cmd-o tiff-v-ls-cmd-o)
	 (long-o tiff-v-ls-long-cmd-o)

	 (case-copy case-fold-search)
	 (re-ign tiff-v-ign-files-re)
	 (trash  tiff-v-trash-buf)
	 (Fchars tiff-v-kill-ls-F-chars)

	 (ls-err-re "not found")
	 files-count count
	 win
	 err
	 )
    (tiff-autoload)			;run only when called for the 1st time

    ;; record window configuration to globals
    (setq tiff-v-wbuf-cycle (tiff-get-wbuf-cycle))
    (setq tiff-v-buffer-list (buffer-list))

    (save-excursion
      (set-buffer bp)       (erase-buffer)
      (setq dir
	    (file-name-as-directory	; make sure it has trailing "/"
	     (expand-file-name dir)))	; sh doesn't get '~'

      ;;   Did user say yes to ls -l?
      (if (and cmd-type
	       (string-match "^[ \t]*[yY]" cmd-type))
	  (setq cmd-o long-o))

      (call-process plain-files-cmd  nil bp nil  cmd-o dir)

      (goto-char (point-min))
      ;;  Get rid of those extra -F flags on files
      (save-excursion (replace-regexp (concat Fchars "$") ""))
      (sort-lines nil (point-min) (point-max))

      ;;  add directory name in front of every file
      (goto-char (point-min))
      (while (not (eobp))
	(beginning-of-line)
	(insert dir)
	(forward-line) (end-of-line))

      ;;   Now pick the files user wanted
      ;;   - Check if ls said :"No files found"
      (if (re-search-forward ls-err-re nil t)
	  (progn
	    (setq err t)
	    (message "No files found."))
	(goto-char (point-min))
	(setq case-fold-search tiff-v-ign-case)
	(delete-matching-lines re-ign)
	;; (setq files-count (count-lines (point-min) (point-max)))
	(delete-non-matching-lines re)
	(setq count (count-lines (point-min) (point-max)))
	;;  Now we have files that user wants to load
	)) ;; .............................................save-excursion

    (setq case-fold-search case-copy)	;restore value
    (if (> count 0) nil
      (message (concat "Regexp didn't match."))
      (setq err t))

    (if err nil				;skip, error occurred, handled already
      (if arg				;no confirmation
	  (tiff-find-file-this-buffer bp)
	(setq win (get-buffer-window bp))	;visible ?
	(if win
	    (select-window win)
	  (switch-to-buffer-other-window bp))
	(message "Edit file list and run M-x tiff-find-file-this-buffer")))
    ))


;;; -------------- end of tinyff.el --------------------------------------
