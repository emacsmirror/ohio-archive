;; @(#) tinydesk.el -- Saves/restores files of emacs session

;; @(#) $Id: tinydesk.el,v 1.14 1995/11/17 14:27:51 jaalto Release_2 jaalto $
;; @(#) $Keywords: desktop, saving session files $
;; $KnownCompatibility: 19.28 $
;; $outlineRegexp: '@+' $
;; $bookMarkRegexp: '[-~+=*#.,'`^]+ &+\\([^ \t]+\\) [-~+=*#.,'`^]+' $
;; $PackageInstallRe: '^[ \t]*;;+[*] ' $
;; This file is *NOT* part of GNU emacs

;;{{{ Id

;; Copyright (C) 1995 Jari Aalto
;; Author:       Jari Aalto <jari.aalto@ntc.nokia.com>
;; Maintainer:   Jari Aalto <jari.aalto@ntc.nokia.com>
;; Created:      Feb 13 1995
;; Version:      $Revision: 1.14 $
;;
;; To get information on this program use ident(1) or do M-x tid-version
;; Look at the code with folding.el, tinyfold.el, tinybm.el


;; LCD Archive Entry:
;; tinydesk|Jari Aalto|jari.aalto@ntc.nokia.com|
;; Saves/restores files of emacs session|
;; 17-Nov-1995|1.14|~/misc/tinydesk.el.Z|

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

;;}}}
;;{{{ Install

;; ........................................................ &t-install ...
;; - Put this file on your Emacs-Lisp load path, add following into your
;;   ~/.emacs startup file
;;
;;      (require 'tinydesk)
;;
;; - or use the autoload feature, more preferred
;;
;;	(autoload 'tid-save-state	    "tinydesk" "" t)
;;	(autoload 'tid-recover-state	    "tinydesk" "" t)
;;	(autoload 'tid-edit-state-file	    "tinydesk" "" t)

;;}}}
;;{{{ Documentation

;; ..................................................... &t-commentary ...
;; Briefly:
;; o   Simple desktop saver approach: only filenames are read/saved
;;
;;     Unlike the other desktop savers, this one can also UNLOAD
;;     files from emacs. You just tell it to remove 'these files listed
;;     in state file state.XXX', and those files will be removed from your
;;     emacs buffers. This enables you to colloect 'projects' and switch
;;     between them easily: When I'm through project1, I can unload it
;;     from emacs or load project3 instead.
;;
;; o   Parses any file that includes filenames and comments [user-defined]
;; o   State file editing (tid-mode):
;;     [a] Mouse 3 to load single file on the line
;;     [b] Shift Mouse 2 to clear face properties from buffer,
;;         so that they don't disturb your view.
;;     [c] Control Mouse 2 to parse files mouse loadable for [a],
;;         after you edited, make them loadable with this again
;;     [d] Ctrl Shift Mouse 2 to show unloadable files in 'error face.
;;         This is for trouble shooting and verifying files edited are ok.
;; o   Function than rips off all comments and leaves only filenames
;;     to the buffer --> You can use raw file list for other purposes.
;; o   Many hooks
;; o   configurable title for saved STATE files.
;;
;; o   If there were any invalid entries in the state file, that
;;     couldn't be loaded into emacs, the user is displayed which
;;     entries had problems. He is also put into state file buffer and
;;     offending lines are highlighted.

;; RESERVED prefix FOR FUNCTIONS IN THIS FILE
;; - To avoid collisions to other modules I use "tid-" in front of
;;   every function & variable. It stands for '(ti)ny (d)esktop.
;; - variable names contain letters 'tid-:'.

;; PREFACE
;; ========================================
;; - I have had these function for ages and I just decided to remove
;;   them from my own lisp library of various useful functions and
;;   put them into separate .el
;; - I'm usually working with X-tube at my work, which I left open day to
;;   day. In fact I seldom even logout, so my emacs files just sits nicely,
;;   so I haven't had need for sophisticated session saver yet.
;; - What I do need, is, that sometimes I have to go to the lab next floor
;;   to see what's troubling my C++ program. I must have some way
;;   to transfer the files that I was just editing and bring them into lab
;;   where I need to run the debugger for a while.
;;   --> These functions save my configuration into file, that
;;       can be opened again in emacs somewhere else.
;; - Hope someone finds use for this also, although there exist much more
;;   better desktop savers, which saves points, marks and modes...

;; FACE SETUP
;; ==================================================
;; - This program uses some faces to catch your attention when you're
;;   working with the STATE files.
;; - I you restore state from a file and some file reference cannot
;;   be loaded, the STATE file will be shown to you and the problematic lines
;;   are highlighted.
;; - If you open the STATE file for editing, you can selectively load
;;   files. The mouse pointer will change and the text is again highlighted.
;;
;; - to make the highlight work for you, you must set some colors like this
;;
;;         (set-face-foreground 'italic "LightBlue")
;;
;; DEVELOPMENT NOTE
;; - No, I won't duplicate desktop.el functionality to save points
;;   and modes and so on. This is for simple state restoring only.


;; Advertise:
;; - Emacs 19.27 is known to have desktop.el, but someone just posted article
;;   saying that there existed much more powerful desktop saver. Please
;;   note, that version may differ now.
;;      ftp.ae.keio.ac.jp:pub/emacs-lisp/util/windows-1.8.tar.gz
;;	                                             ^^^^^
;;
;; Other modules you may want to check out. [some of them may not
;; be released yet]
;;
;; - tinystamp     , Keeps your document in order, rcs ident(1) compatible
;; - tinysword     , search word on cursor, dynamic "word" specification
;; - tinyindent    , Indent minor mode -- cursor positioning
;; - tinymacro     , Two keyspresses and you have ready macro bound to any key.
;; - tinyff        , regexp find fie utility, if you need to get *.cc files.
;; - tinyfh        , edit read-only files easily. No confirmations.
;; - tinylock      , locks your emacs with password.
;; - tinyeat       , eat text by guessing what to delete. Get this!
;; - tinycom       , Smart comment handling, offers comment classes.
;; - tinybm        , Keeps your code organised by inserintg bookmarks
;;                   like the continuous lines you see here with symbols &.
;;                   Uses imenu.el to show the bookmarks.
;; - tinyfold      , additional functions to folding.el {{{ }}}
;; - tinycerr      , General compiler error parser on top of 'compile'
;;                   mouse and color support.
;; - tinygflint    , Gimpel flint C++ error parser, based on tinycerr engine
;; - tiny-hpc++    , HP C++ error parse, based on tinycerr engine
;; - tinydgboot    , allows you to have DING/GNUS simultaneously.
;; - tinyhotlist   , hotlist of buffers in X, easy add, easy remove !!
;; - tinymouse	   , demonstrates mouse usage, drag, spell region
;; - tinymatch     , hilit text with re-search sommands, DING/GNUS examples..
;; - tinyoutline   , outline additions ** NOT READY
;; - tinypair      , paired insertion of chars () "" '' , smart
;; - tinypatch     , emacs 19.28 function corrections: "patches"
;; - tinyreplace   , general replace engine, colors. Like M-%
;; - tinyreply     , A monster mail reply tool. Not for novice users. GNUS/DING
;; - tinyrcs	   , complete RCS, all you ever need.
;; - tinytab       , TAB minor mode (4 tabs etc.), see also tinyindent.el
;; - tinylibXXX    , general lisp funtion libraries.
;; - tinyxreg	   , like C-x / <reg>, but allows choosing from X popup


;;}}}
;;{{{ history

;; ........................................................ &t-history ...
;; [jari]   = jari.aalto@ntc.nokia.com(work), ssjaaa@uta.fi(University)
;;
;; Nov	17	1995	[jari]		19.28	v1.14		Release_2
;; - Time to release this new version. This is exactly the same as 1.13,
;;   but may have some cosmetic text changes.
;;
;; Nov	13	1995	[jari]		19.28	v1.13		NotReleased
;; - Run byte compile test and noticed that inc macro was missing.
;;   inc				:+ from my libs
;;
;; Nov	2	1995	[jari]		19.28	v1.12		NotReleased
;; - some things corrected and rethought.
;;   use-tmp-buffer			:! optional clear parameter
;;   tid-save-state			:! rewritten
;;
;; Nov	2	1995	[jari]		19.28	v1.11		NotReleased
;; - The unload function had some troubles that are now corrected.
;;   use-tmp-buffer			:+ from my libs
;;   tid-unload				:! rewritten
;;
;; Oct	27	1995	[jari]		19.28	v1.10		NotReleased
;; - Rearranged code a bit. Now there is actual tid-mode funtion
;;   that can be turned on.
;; - Now it pops up the buffer and marks the files it couldn't load
;;   and put user in edit state mode automatically.
;;   tid-set-face-non-files-buffer	:! now uses narrow
;;   tid-edit-state-file		:! rewritten
;;   tid-mode				:+ new
;;
;;
;; Oct	27	1995	[jari]		19.28	v1.9		NotReleased
;; - Started replacing functions with library code, so that they could be
;;   easily removed later. Now keys are uer configurable through funcall.
;; - Separate major mode created for handling the State files.
;; - Reprogrammed some functions
;;   tid-load-hook		:+ when loding this file
;;   tid-:loaded-files		:+ for hook users
;;   tid-:rejected-files	:+ for hook users
;;   tid-:mode-name		:+
;;   tid-read-word		:+ deleted and replaces (lib func now)
;;   tid-get-save-dir		:! changed
;;   tid-only-files-buffer	:! changed
;;   tid-edit-state-file	:! now uses major mode
;;   tid-recover-state		:! reprogrammed
;;   tid-ff-buffer		:! reprogrammed
;;   tid-end-slash		:- moved to lib
;;   tid-kill-line		:- moved to lib
;;   tid-user-home		:- moved to lib
;;   tid-emacs-files		:- moved to lib
;;
;; Aug	23	1995	[jari]		19.28	v1.8		NotReleased
;; - Many corrections. bugs when saving to non-dir and when retrieving
;;   file that already was in emacs, allthouhg had different directory.
;;   tid-:trash-tmp-buffer    	:+ trashes temp buf automatically.
;;   tid-trash			:+
;;   tid-unload			:! rewritten
;;   tid-save-state		:! bug, saving to non-existent dir
;;   tid-recover-state		:! bug, if same filename, but diff dir.
;;
;; May	10	1995	[jari]		19.28	v1.7		NotReleased
;; - Hmmm, When I had many state files, I wanted to load files
;;   1 and 2 .. but later I wanted to remove all 2 files, so I wrote
;;   funtion to do just this.
;; - tid-unload kills all buffers listed in file
;; - Added tid-:trash-tmp-buffer to automatically kill tmp file used.
;;
;; Apr	7	1995	[jari]		19.28	v1.7		Release_1
;; - If default save directory included "/" it was discarded due
;;   to bug in tid-end-slash.
;; - Changed mouse-2 --> mouse-2 , because mouse-2 is used for paste
;; - Corrected bug in tid-ff-buffer that always returned: '0 files loaded'
;; - Added parameter FILE to tid-edit-state-file, so that it can be
;;   be called direcly.
;;
;; Mar	31	1995	[jari]		19.28	v1.6		NotReleased
;; - Fine tuned tid-edit-state-file: Few more checks if the file
;;   is already loaded. Now it doesn't create duplicate windows,
;;   also clears previous faces before processing.
;;
;; Mar	31	1995	[jari]		19.28	v1.5		NotReleased
;; - Corrected the tid-:comment-re variable, there must not me '.*\\('
;;   at the front, becasue .* aways matches at least one, so the sub-level
;;   starts from position 2, which is wrong. Changed to have '[ \t]*'
;;   instead, which is more accurate. Also added + for #;// marks
;; - Corrected bug in tid-set-face-non-files-region, which forgot
;;   to take care of comments and ignore them.
;; - Relocated the redraw-screen, so that the sceen isn't so jumpy when
;;   faces are removed, added.
;;
;; Mar	31	1995	[jari]		19.28	v1.4		NotReleased
;; - Added more mouse commands and more user functions, Now
;;   C-S-Mouse 2 shows unloadable lines. Great for verifying the lines.
;; - function that rips off all comments from the file.
;; - added some more hooks + title string insertion.
;; - added tid-kill-line, tid-only-files-buffer , tid-only-files-region
;;   tid-set-face-non-files-buffer , tid-set-face-non-files-region
;; - defconst made defvar in several places
;;
;; Mar	30	1995	[jari]		19.28	v1.3		NotReleased
;; - Well, this is really some package. Sent to g.e.sources
;;   to have people testing this. If I don't here complaints or
;;   bugs, then I'll send this to ohio.
;;
;; Mar	30	1995	[jari]		19.28	v1.2		NotReleased
;; - I took a look at this again, I've been bust lately.
;; - Oh man, I really started to add functions to it, now has configurable
;;   save directory, properties.... just the kind of .el I want it to be.
;;
;; Feb	13	1995	[jari]		19.28	v1.1		Created


;; To do list:
;; ========================================
;; - Some of these functions are currently transferred to general
;;   tinylibXXX.el lisp libraries and when the libraries are commonly
;;   available, the overlapping functions are removed from this file.

;;}}}


;;; Code:



;;{{{ setup: -- bind and hooks

;;; .......................................................... &v-bind ...


(defvar tid-:key-func 'tid-keys
  "*Function to set up key to  tid-map.")

(defvar tid-map nil
  "Local keymap for STATE files loaded by edit.")


(defun tid-keys ()
  "Define keys."

  ;;  - Don't want to use mouse-2 because it's for PASTE.
  ;;  - The others are put to mouse-2 because there is not
  ;;    not always 3 button mouse available.
  (define-key tid-map [mouse-3] 'tid-mouse-load-file)

  ;;  - When editing a file, those colors might be too annoyinng,
  ;;    so you can remove properties with this. Loading is disabled too
  ;;  - Remeber, emacs is slow with this... wait some time.
  (define-key tid-map [S-mouse-2] 'tid-clear-buffer-properties)

  ;;  To make buffer loadable by mouse again, run this
  (define-key tid-map [C-mouse-2] 'tid-mark-buffer-loadable)

  ;;  To mark files that are not loadable, check for possibly typo in filename
  (define-key tid-map [C-M-mouse-2] 'tid-set-face-non-files-buffer)
  )


(if tid-map
    nil					;already there
  (setq tid-map (make-sparse-keymap))
  (and  tid-:key-func
	(funcall tid-:key-func))
  )


(defun tid-use-map ()
  "Uses local \\{tid-map} on this buffer."
  (interactive)
  (use-local-map tid-map))

;;; ......................................................... &v-hooks ...

(defvar tid-load-hook nil
  "*Hook run when file has been loaded.")

(defvar tid-save-before-hook nil
  "*Hook that is run just before _putting_ of files to STATE file
begins. This is your chance to do something to the buffers.")

(defvar tid-save-after-hook nil
  "*Hook that is run just before _saving_ of STATE file. The
files are there, possibly in sorted order, and the title is there.")

(defvar tid-mode-hook nil
  "*Hook that is run after the edit mode is turned on.")

(defvar tid-recover-before-hook nil
  "*Hook that is run after recover file is loaded, just before processing
starts.")

(defvar tid-recover-after-hook nil
  "*Hook that is run after recover file is _parsed_ AND there were no
errors during loading the files.")

;;}}}
;;{{{ setup: -- private

;;; ... private variables ................................. &v-private ...


(defvar tid-:dir-last nil
  "Directory that were used for last save")

(defvar tid-:tmp-buffer  "*tmp*"
  "The workbuffer used, created and killed when needed.")

(defvar tid-:trash-tmp-buffer  t
  "If set to non-nil, the workbuffer is always deleted when done and
it won't stay around floating in your emacs.")



;;; The \$\Docid: is used by tinystamp.el
(defconst tid-:save-default-title
  (concat
   ";;\n"
   ";;  description:  emacs desktop session file\n"
   ";;\n"
   ";;       \$\Docid: $\n\n\n"
   )
  "Default title lines to be added just before saving state. Filenames
are already included.")


(defconst tid-:loaded-files nil
  "Overwritten. List. Holds files that were loaded by tid-ff-buffer.
Hooks may check the contents of this.
")

(defconst tid-:rejected-files nil
  "Overwritten. List. Holds files that were *not* loaded by tid-ff-buffer.
Reason may be anything: incorrect filename, path, garbage at line...
Hooks may check the contents of this.
")




;;}}}
;;{{{ setup: user config

;;; ... user configurable .................................. &v-config ...

(defvar tid-:mode-name "StateFileEdit"
  "*Editing STATE files mode name.")

(defvar tid-:dir-default "~/projects"
  "*Default directory where to save and restore files")

(defvar tid-:dir-suggesion 'default
  "*What is the suggested save directory:
'last        ,the latest that user used
'defauilt    ,always offer the default directory
")

(defvar tid-:comment-start-level 1
  "*Which sub expression is the comment start.")


;;
(defvar tid-:comment-re "[ \t]*\\(//+\\|[#;]+\\)"
  "*Comment beginning markers in the STATE file, defaults are:

;  commnet
// commment
#  commment

NOTE: this regexp is used with looking-at at the _begin_of_line.
The tid-:comment-start-level will signify comment start subexpression.
")


(defvar tid-:save-title tid-:save-default-title
  "*Title to attach the beginning of file before saving.")

;;  Set to nil if you don't want title.
(defvar tid-:get-save-file-func 'tid-get-save-files
  "*Function that returns list of filenames that are stored to STATE file
line by line.  This function isn't run if tid-save-state is run with
parameter FILES
")

(defvar tid-:sort-when-saved t
  "*When the files are outputted to STATE file, should they be sorted
before writing? If you set this to nil, then it preserves buffer-list
order.")


(defvar tid-:face-table
  '(
    (file-pick .  highlight)
    (error     .  italic)
    )
  "*Faces used for marking text")




;;}}}
;;{{{ setup: -- version

;;; ... version info ...................................... &v-version ...

(defconst tid-version
  "$Revision: 1.14 $"
  "Latest version number.")


(defconst tid-version-id
  "$Id: tinydesk.el,v 1.14 1995/11/17 14:27:51 jaalto Release_2 jaalto $"
  "Latest modification time and version number.")

(defconst tid-version-doc
  "tinydesk.el -- Saves/restores files of emacs session

First created: Feb 13 1995
Author       : Jari Aalto <jari.aalto@ntc.nokia.com
Maintainer   : Jari Aalto <jari.aalto@ntc.nokia.com

Suggestions and enchancements welcome!"
  "Brief version and contact info")

;;; ----------------------------------------------------------------------
(defun tid-version ()
  "version information."
  (interactive)
  (let* ((ob (current-buffer))
	 (bp (get-buffer-create "*version*"))
	 )
      (set-buffer bp)			;for insert
      (erase-buffer)
      (switch-to-buffer-other-window bp)
      (insert
       tid-version-doc
       "\n\ncurrent version:\n" tid-version-id)
      (pop-to-buffer  ob)
    ))

;;}}}


;;{{{ lib

;;; ############################################################# &lib ###
;;; ** NOTICE **
;;;    - Do not copy these, since they are soon available:
;;;      tinylib.el, tinylibm.el ,, ...
;;;    - If you copy these and lib funcs have have more
;;;      recent versions --> your and my code breaks, since there exist
;;;      different versions: Which one is loaded in emacs ?


(defconst ti:-ret nil "no docs")


(defmacro BOLP ()
  "Returns beginning of line. Preserves point"
  (` (save-excursion
       (beginning-of-line) (point))))


(defmacro BOLPP ()
  "Returns end of line. Moves point."
  (` (progn
       (beginning-of-line) (point))))


(defmacro EOLP ()
  "Returns end of line. Preserves point."
  (` (save-excursion
       (end-of-line) (point))))


(defmacro EOLPP ()
  "Returns beginning of line. Moves point."
  (` (progn
       (end-of-line) (point))))


(defmacro PMIN ()
  "goes to point-min"
  (` (goto-char (point-min))))

(defmacro PMAX ()
  "goes to point-max"
  (` (goto-char (point-max))))


(defmacro NEXTP (list)
  "Advances list pointer with cdr."
  (` (setq (, list) (cdr (, list)))))



(defmacro inc (var)
  "Increments by 1. See GNU Lisp manual sec. 12 MACROS"
  (` (setq (, var) (1+ (, var)))))


(defmacro file-loadable-p (file)
  "Checks if file is loadable. Non-string args are accepted too.
The FILENAME is not expanded.
"
  (` (and (stringp (, file))
	  (file-readable-p (, file)))))


(defmacro list-prepend (list object &optional test)
  "Sticks destructively OBJECT on the front of LIST. When optional TEST
is non-nil, tests if OBJECT already exist before adding.

Example:
   nbr-list                     --> '\(2 3 4\)
   \(list-prepend 1 nbr-list\)  --> '\(1 2 3 4\)
"
  (` (if (or (null (, test))
             (not (memq (, object) (, list)) ))
         (setq
          (, list)
          (cons (, object) (, list))))))

(defmacro read-full-line ()
  "Retuns whole line."
  (` (save-excursion
       (buffer-substring (EOLPP) (BOLPP)))))



;;; ----------------------------------------------------------------------
;;; Eg. making sure directory has ending slash:
;;;  (ti::s-verify-ends "dir" "/")       --> "dir/"
;;;
;;; Making sure, time is zero based:
;;;  (ti::s-verify-ends "7" "0" nil 'beg) --> "07"
;;;
(defun ti::s-verify-ends (str re &optional add-str beg)
  "Makes sure STR matches RE and adds ADD-STR string to it when
necessary. if ADD-STR is not given, adds RE to the string.

Default is to check end of string, Optionally BEG of string.
The RE may not include anchors.

Returns:
  str    ,modified or not.
"
  (let* ((RE  (if beg
		  (concat "^" (regexp-quote re))
		(concat (regexp-quote re) "$")))
	 (add (or add-str re))		;which one to add.
	 )
    (if (string-match RE str)
        str
      (if beg
	  (concat add str)
	(concat str add)
	))
      ))




;;; ----------------------------------------------------------------------
;;; - Easiest would have been using zap-to-char, but
;;;   it's not same in 18.xx and 19.xx
;;;
(defun ti::b-kill-line (&optional delete)
  "Kills _one_ line silently and moves next line up, no matter where
the point is on the line.  If cursor is sitting at the end of buffer,
nothing happens. Optional DELETE tells to use delete-region, which
doesn't manipulate kill-ring, thus the execution is faster.

Portable:
  Between any emacs versions 18.xx - 19.xx

Errors:
  Never signalled.

Returns:
  t		,line killed
  nil		,sitting at eob, cannot kill line
"
  (interactive "*P")
  (let* ((null-line-re "^$")
	 )
    ;;  emacs kill-line is little awkward, because if you're at the
    ;;  end of buffer it signals an error...
    (cond
     ((eobp)				;nothing to kill
      nil)
     ((and (null (eobp)) (looking-at null-line-re))
      (if delete
	  (delete-char 1)
	(kill-line))
      t)
     (t					;shift line up
      (beginning-of-line)
      (if delete
	  (delete-region (point) (EOLPP))
	(kill-line))
      (if (null (eobp))
	  (if delete
	      (delete-char 1)
	    (kill-line)))
      t))
    ))


;;; ----------------------------------------------------------------------
;;;
(defun ti::b-read-word (&optional charset strict)
  "Return word specified by optional CHARSET at or after point.
If otptional STRICT is non-nil, requires that point is sitting on
CHARSET before continuing. If thre is no CHARSET under point it would
normally search forward for word, but when STRICT is in effect it
returns nil.

Returns:
  str        ,word or nil
  ti:-ret   ,list of word boundary points \(beg end\) or nil
"
  (let* ((charset (or charset "-a-zA-Z0-9_"))
         (not (concat "^" charset))
         ret pb pe
         )
  (save-excursion
    (setq
     ret
     (if (or (null strict)
             (and strict (looking-at charset)))
         (buffer-substring
          (progn
            (skip-chars-forward not)
            (skip-chars-backward charset)
            (setq pb (point))
            )
          (progn
            (skip-chars-forward charset)
            (setq pe (point))
            ))))
    (setq ti:-ret nil)                  ;<< GLOBAL
    (if (equal pe pb)
        nil
      (setq ti:-ret (list pb pe))
      ret)
    )))


;;; ----------------------------------------------------------------------
;;;
(defun ti::b-buffer-list-files (&optional re str)
  "Returns all filenames loaded into emacs.

If optional RE and STR are given, then a file name substitution
takes place:

 args           re = \"/usr43/users/john/\"   ,str = \"~/\"
 buffer file    \"/usr43/users/john/t.txt\"
 substituted    \"~/t.txt\"

Example call:
 (ti::b-buffer-list-files \"/usr43/users/john\" \"~\")

Returns:
 (file ..)      ,list of filenames

"
  (let* ((ptr   (buffer-list))
         list
         file
         )
    (while ptr
      (setq file  (buffer-file-name (car ptr)))
      (if (null (stringp file))         ;might be nil if buffer has no file
          nil                           ;skip
        (if (and re str
                 (string-match re file))
            (setq file (kill-match 0 str file)))
        (list-prepend list file)
        )
      (NEXTP ptr))
    list
    ))


;;; ----------------------------------------------------------------------
;;;
(defun kill-match (level &optional replace string)
  "Kills match from buffer at LEVEL or optionally substitutes it with
REPLACE. Point sits after the replaced or killed area.

Optionally you can give STRING. If level didn't match, do nothing.

Call:
  (level &optional replace string)

Returns:
  t     ,action taken
  nil   ,if match at LEVEL doesn't exist.
  str   ,if string was given  ,use (setq str (kill-match 1 replace str))

"
  (if (null string)
      (cond
       ((match-end level)
        (delete-region (match-beginning level) (match-end level))

        ;;  I think emacs has bug, because cursor does not sit at
        ;;  match-beginning if I delete that region, instead it is off +1
        ;;  --> force it to right place
        (and replace
             (goto-char (match-beginning level))
             (insert replace))
        ))
    ;; Handle string case
    (cond
     ((match-end level)
      (concat
       (substring string 0 (match-beginning level))
       (if (stringp replace) replace "")
       (substring string (match-end level)))))
    ))





;;; ----------------------------------------------------------------------
;;; - The buffer is *not* cleared, only put to consistent state
;;;
(defun use-tmp-buffer (&optional buffer clear)
  "Creates one if not exist. Removes read-only. Uses \"*tmp*\" by default.
Puts buffer to fundamental-mode. if CLEAR is non-nil, deletes old
contents of buffer.

Returns:
  buffer
"
  (let* ((buffer  (get-buffer-create (or buffer "*tmp*")))
	 )
    (save-excursion
      (set-buffer buffer)
      (fundamental-mode)
      (setq buffer-read-only nil)
      (not-modified)
      (if clear
	  (erase-buffer))
      )
    buffer
    ))



;;}}}


;;{{{ code: general

;;; ########################################################### &Funcs ###


;;; ----------------------------------------------------------------------
;;;
(defun tid-read-word ()
  ;;   Hmmm, we allow quite a filename here !
  ;;   It's not my problem if user has such filenames ...
  (ti::b-read-word "-a-zA-Z0-9_/.!@#%&{}[]+:;~`<>")
  )


;;; ----------------------------------------------------------------------
;;;
(defun tid-tmp-buffer (&optional clear)
  (use-tmp-buffer tid-:tmp-buffer clear))

;;; ----------------------------------------------------------------------
;;;
(defun tid-get-save-dir ()
  "Return suggested save directory."
  (let* ((type	    tid-:dir-suggesion)
	 (last	    tid-:dir-last)
	 (dir	    tid-:dir-default)
	 (ret	    dir)		;set default return value
	 )

    (if (and (eq type 'last)
	     (stringp last)
	     (file-writable-p last))
	(setq ret last))

    (if (null ret)			;make sure it's not nil
	(setq ret dir))			;use default then
    (ti::s-verify-ends ret "/")
    ))


;;; ----------------------------------------------------------------------
;;;
(defun tid-only-files-buffer ()
  "Removes all comments and empty lines from buffer and  leaves
only first word on every line."
  (interactive)
  (tid-only-files-region (point-max) (point-min)))




;;; ----------------------------------------------------------------------
;;;
(defun tid-only-files-region (beg end)
  "Removes all comments and empty lines from region and leaves
only first word on every line. This way you can rip off all comments
and leave filenames."
  (interactive "r")
  (let* (
	 (sub-level	tid-:comment-start-level)
	 (comment-re	tid-:comment-re)
	 (empty-re      "^[ \t]*$\\|$")
	 mark-end
	 p maxp word tmp
	 )

    (if (> beg end)
	(setq tmp beg  beg end  end tmp))

    (save-excursion
      (goto-char end)
      ;;  have to user markers , beacuse we delete lines and point moves
      ;;  along
      (setq mark-end (point-marker))
      (goto-char beg)
      (while (< (point) (marker-position mark-end))
	(setq p (point)  maxp nil)
	(catch 'next

	  (if (null (looking-at empty-re))
	      nil
	    (ti::b-kill-line)
	    (throw 'next t))


	  (if (null (looking-at comment-re))
	      nil
	    (if (match-beginning sub-level)
		(setq maxp (match-beginning sub-level))))

	  (if (and maxp (eq maxp p))	;BEG of line comment
	      (progn
		(ti::b-kill-line) (throw 'next t)))

	  (setq word (tid-read-word))
;;;%	  (setq word (tid-read-word p maxp))
	  (ti::b-kill-line)

	  ;; The \n make cursor forward
	  (if word (insert word "\n"))
	  )
	)
      )))


;;; ----------------------------------------------------------------------
;;;
(defun tid-trash ()
  "Kills the tmp buffer if user has requeted it.

References:
  tid-:tmp-buffer
  tid-:trash-tmp-buffer
"
  (and tid-:trash-tmp-buffer
       (get-buffer tid-:tmp-buffer)
       (kill-buffer  (get-buffer tid-:tmp-buffer))))

;;}}}
;;{{{ Code: faces

;;; ----------------------------------------------------------------------
;;;
(defun tid-face (face)
  "Returns face"
  ;;  This way the global variable does not float around the file
  (cdr (assoc face tid-:face-table)))


;;; ----------------------------------------------------------------------
;;;
(defun tid-clear-line-properties ()
  "Removes properties from the line."
  (let* ()
    (set-text-properties (BOLPP) (EOLPP) nil)
    (set-buffer-modified-p nil)
    ))


;;; ----------------------------------------------------------------------
;;;
(defun tid-clear-buffer-properties ()
  "Removes properties from buffer"
  (interactive)
  (let* ()
    (tid-clear-region-properties (point-min) (point-max))
    (message "*properties cleared")
    (redraw-display)
    ))


;;; ----------------------------------------------------------------------
;;;
(defun tid-clear-region-properties (beg end)
  "Removes properties from BEG - END"
  (let* ()
    (set-text-properties beg end nil)
    (set-buffer-modified-p nil)
    ))



;;; ----------------------------------------------------------------------
;;;
(defun tid-line-property-error ()
  "Set line face to signify error."
  (let* ()
    (put-text-property (BOLPP) (EOLPP) 'face (tid-face 'error))
    (set-buffer-modified-p nil)
    ))


;;; ----------------------------------------------------------------------
;;;
(defun tid-handle-text-property (p text)
  "Run command on TEXT t according to PROPERTY p"
  (let* ((file	     (file-name-nondirectory text))
	 (loaded     (get-buffer file))	;in emacs already ?
	 )
    ;;  - We need the sleep-for, because moving the mouse
    ;;    clears the message and user may not notice it.
    (cond
     ((eq p (tid-face 'file-pick))
      (cond
      ((null (file-exists-p text))
       (message (concat "File not exist: " text)) (sleep-for 0)
       (tid-clear-line-properties)
       )
      (loaded
       (message "File already loaded.") (sleep-for 0)
       (tid-clear-line-properties)
       )
      (t
       (find-file-noselect text)
       (message "Loaded.") (sleep-for 0)
       (tid-clear-line-properties)
       ))
      )
     )))

;;; ----------------------------------------------------------------------
;;;
(defun tid-mark-buffer-loadable (verb)
  "Parses whole buffer and makes first _word_ loadable with mouse
provided that the word is valid filename."
  (interactive)
  (tid-mark-region (point-min) (point-max)
		   tid-:comment-re
		   (or (interactive-p) verb)
		   ))


;;; ----------------------------------------------------------------------
;;;
(defun tid-set-face-non-files-buffer  ()
  "Changes face to 'error of those lines whose first word is not valid
file name."
  (interactive)
  (tid-set-face-non-files-region (point-min) (point-max))
  )


;;; ----------------------------------------------------------------------
;;;
(defun tid-set-face-non-files-region (beg end)
  "Changes face to 'error of those lines whose first word is not valid
file name."
  (interactive "r")
  (let* ((empty-re	"^[ \t]*$")
	 (sub-level	tid-:comment-start-level)
	 (comment-re	tid-:comment-re)
	 word tmp
	 )

    (if (> beg end)
	(setq tmp beg  beg end  end tmp))

    (save-excursion
      (goto-char beg)
      (while (< (point) end)

	;;  - ignore empty lines and BEG of line comments.
	(if (or (looking-at empty-re)
		(and (looking-at comment-re)
		     (eq (match-beginning sub-level) (point))))
	    nil
	  (setq word (tid-read-word))
	  (if (and word
		   (file-exists-p word))
	      nil
;;;	    (d! word)
	    (tid-line-property-error)
	    ))
	(forward-line 1)
       ))
    ))



;;; ----------------------------------------------------------------------
;;;
(defun tid-mark-region (beg end &optional com-re sub-level verb)
  "Makes all filenames in the buffer loadable by single mouse
click.  Supposes that the first _word_ on the line is filename.
If the first word isn't loadable file, it's face isn't changed.

The file can have comments, but comments can be only _single span type_,
that is, only shell like #, or C++ like //. Everything after and included
COM-RE is discarded from SUB-LEVEL.

The SUB-LEVEL tells which regexp level is the comment start.
Default level is 0.

Eg.
       Myfile.sh   #comment

com-re     = '.*\\\\(#\\\\)'
sub-level  = 1 , because there is paren

VERBOSE controlls message displaying.

"
  (let* (
	 (file-face	(tid-face 'file-pick))
	 (sub-level	(or sub-level 0))
	 tmp
	 bp ep				;beg, end points
	 elp				;end line point
	 maxlp				;max line point
	 )
    (and verb				;this make take a while...
	 (message "Marking files..."))

    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))

      (while (not (eobp))
	(if (looking-at  "^[ \t]*$\\|$")
	    nil				;ignore empty lines

	  (setq elp (EOLP))
	  (setq maxlp elp)
	  ;;  Does there exist comment on the line ?
	  (save-excursion
	    (if (and (stringp com-re)
		     (looking-at com-re))
		(setq maxlp (1- (match-beginning sub-level)))))

	  (if (< maxlp (1+ (point)))	;beg of line COMMENT exist
	      (progn
;;;		(d! "skipped" (read-full-line))
		nil
		)
	    (skip-syntax-forward "\\s " maxlp) ;ignore whitespace
	    (setq bp (point))

	    (skip-chars-forward "^ \t"  maxlp) ;first space

	    (if (eq bp ep) 		;not moved, maybe "one-word$" ?
		(goto-char maxlp))

	    (setq ep (point))
	    (if (eq bp ep)
		nil			;still not moved ?
	      ;;  Marks the word only if the WORD is valid file
	      ;;
	      (if (file-exists-p (buffer-substring bp ep))
		  (put-text-property bp ep 'mouse-face file-face)))
	    ))
	(forward-line 1)
	)
      (set-buffer-modified-p nil)
      (and verb				;this make take a while...
	   (message "Marking files...ok"))

      )))

;;}}}
;;{{{ code: mouse

;;; ----------------------------------------------------------------------
;;;
(defun tid-mouse-load-file (event)
  "Loads file under mouse."
  (interactive "e")
  (let* (
	 (bp	(BOLP))
	 (ep	(EOLP))
	 bounds
	 prop
	 word
	 )
    (mouse-set-point event)		;move point there
    (setq prop (get-text-property (point) 'mouse-face))

    (if (null prop) nil			;no property found
      (setq word (tid-read-word))	;read word under cursor
      (if word				;grabbed
	  (tid-handle-text-property prop word)))
    ))

;;}}}

;;{{{ code: edit

;;; ----------------------------------------------------------------------
;;;
(defun tid-unload (file &optional verb)
  "Unloads all files from emacs that are in state file FILE.

If VERB is non-nil offer verbose messages [for code calls]; interactive
calls always turns on verbose.
"
  (interactive
   (list
    (let* ((save-dir	(tid-get-save-dir))
	   (save-dir	(if save-dir save-dir "./"))
	   (msg	        (concat "Unload from state file: "))
	   )
      (read-file-name msg  save-dir))
    ))
  (let* ((verb   (or verb (interactive-p)))
	 (b      (tid-tmp-buffer))
	 (count  0)
	 buffer
	 fn
	 ptr
	 )
    (save-excursion
      (set-buffer b)
      (erase-buffer)
      (insert-file-contents file)

      ;;  - Now process every line. We don't care if we read commented
      ;;    line as "buffer" because the test inside loop return nil
      ;;    for such lines...
      (PMIN)
      (if (eobp)
	  (message "Empty state file.")
	(while (not (eobp))		;check if exist
	  (setq fn (read-full-line))
	  (if (null (setq buffer (get-file-buffer fn)))
	      nil
	    (kill-buffer buffer)
	    (inc count)
	    )
	  (forward-line)
	  )))

    (if (> count 0)
	(message (format "Removed %d files" count))
      (message "No state files detected. No files removed."))

    (tid-trash)
    ))


;;; ----------------------------------------------------------------------
;;;
(defun tid-mode (&optional no-face verb)
  "Not actual major mode.
If NO-FACE is non-nil, the default mouse marking isn't performed.

Following commands are available
\\{tid-map}
"
  (interactive "P")
  (let* (
	 (sub-level	tid-:comment-start-level)
	 (comment-re	tid-:comment-re)
	 (verb		(or verb (interactive-p)))
	 )
    ;; - If the file is already in buffer, remove extra marks, like
    ;;   non-loadable files.
    (tid-clear-region-properties (point-min) (point-max))

    (if (null no-face)
	(tid-mark-region (point-min) (point-max) comment-re sub-level))

    (tid-use-map)			;turn on the map
    (setq  mode-name   tid-:mode-name)
    (setq  major-mode 'tid-mode);; for C-h m

    (if (null verb)
	nil
      (message
       (substitute-command-keys
	(concat
	 "load \\[tid-mouse-load-file], "
	 "clear \\[tid-clear-buffer-properties], "
	 "error \\[tid-set-face-non-files-buffer], "
	 "mark \\[tid-mark-buffer-loadable]"
	 )))
      (sleep-for 0)
      )
    (run-hooks 'tid-mode-hook)
    ))


;;; ----------------------------------------------------------------------
;;;
(defun tid-edit-state-file (file)
  "Loads state file into buffer for editing, so that you can add comments
and remove/add files. Turns on tid-mode.

Following commands are available in tid-mode:
\\{tid-map}
"
  ;;  I can't use interactive "f" , beacuse I want the completions
  ;;  to come from the save-directory. The "f" uses by default the
  ;;  variable default-directory...
  (interactive
   (list
    (let* ((save-dir	(tid-get-save-dir))
	   (save-dir	(if save-dir save-dir "./"))
	   (msg	(concat "Edit state file: "))
	   )
      (read-file-name msg  save-dir))
    ))
  ;; - If file is already loaded, avoid creating duplicate window
  (pop-to-buffer (find-file-noselect file))
  (tid-mode)
  )




;;}}}
;;{{{ code: save


;;; ----------------------------------------------------------------------
;;;
(defun tid-get-save-files ()
  "Returns list of files to save."
  (ti::b-buffer-list-files))


;;; ----------------------------------------------------------------------
;;;
(defun tid-save-state (file &optional files verb)
  "Outputs all files loaded into emacs into FILE, so that you can load
them later. Notice, that this concers only buffer with filenames.

Input:
  file		,the STATE file beeing saved
  files		,filenames , absolute ones
  verb		,verbose flag

"
  (interactive
   (list
    (read-file-name
     "Save state to: "
     (if (tid-get-save-dir) (tid-get-save-dir) "./"))))


  (let* ((b		(tid-tmp-buffer 'clear))
	 (verb		(or verb (interactive-p)))
	 (save-func	tid-:get-save-file-func)
	 (sort		tid-:sort-when-saved)
	 (title		tid-:save-title)
	 )
    (if (get-file-buffer file)
	(error (error (concat "Please kill the buffer:" file " first."))))


    (run-hooks 'tid-save-before-hook)
    (setq files (or files
		    (and (fboundp save-func)
			 (funcall save-func))))

    (if (null files)
	(if verb			;no files
	    (message "no files to save"))
      (if (or  (null file)
	       (not (file-writable-p file))
	       (null (file-name-directory file))
	       )
	  (error (concat  "save state file: " file "  not accessible")))

      (set-buffer b)
      (pop-to-buffer b)

      (while files
	(insert (car files) "\n")
	(NEXTP files))

      (if sort
	  (sort-lines nil (point-min) (point-max)))

      ;;  avoid renaming the buffer this way

      (if (null (stringp title))
	  nil				;no title
	(PMIN)
	(insert title))

      (run-hooks 'tid-save-after-hook)
      (write-region (point-min) (point-max) file)

      (not-modified)
      (kill-buffer b)
      (if (interactive-p)
	  (message (concat "State saved to file " file)))
      ;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ catch ^^^
      )

    (tid-trash)
  ))

;;}}}
;;{{{ code: recover

;;; ----------------------------------------------------------------------
;;;
(defun tid-recover-state (&optional file verb)
  "Load all files listed in FILE into emacs.
FILE can have empty lines or comments. No spaces allowed at the
beginning of filename. The FILE itself is not left inside emacs if everything
loads well. When all files are already in emacs, you may see '0 files loaded'
message.

In case there were problems, the FILE will be shown and the
problematic lines are highlighted.

Input:
  file		,state file to load
  verb		,non-nil enables verbose messages. If called interactively
		 this is enabled automatically.

References:
  tid-:comment-re
"
  (interactive
   (list
    (read-file-name "load state file: "
		    (if (tid-get-save-dir)
			(tid-get-save-dir)
		      "./"))
    ))
  (let* ((loop		t)
	 (count		0)
	 (verb		(or verb (interactive-p)))
	 (msg		(concat "load state file: "))
	 (state-file    (expand-file-name file))

	 buffer
	 kill-buffer
	 err
	 not-loaded
	 count
	 l
	 )
    ;; o  read the config file
    ;; o  raise the kill flag if the file ISN'T already loaded, user
    ;;    may be editing it.
    ;; o  There may be buffers with the same name, but different dirs..
    ;;

    (if (setq buffer (get-file-buffer state-file))
	nil				;yup, it's loaded
      (setq kill-buffer t)		;different directory
      (setq buffer (find-file-noselect state-file))
      )

    (save-excursion
      (set-buffer buffer)
      (fundamental-mode)		;to prevent anything funny to happen
      (setq l (tid-ff-buffer))
      (setq  count (nth 0 l)  err (nth 1 l)   not-loaded (nth 2 l))

      (cond
       ((null err)
	(if verb (message (concat count " files loaded")))
	(run-hooks 'tid-recover-after-hook)

	;;  kill the buffer only if it was loaded by us
	(and kill-buffer (kill-buffer buffer))
	)
       (t
	;;  Show user the falied files
	(message (concat "Not loaded> " not-loaded))
	(sleep-for 0)
	(pop-to-buffer buffer)
	(tid-mode 'no-face 'verbosee)
	(tid-set-face-non-files-buffer)
	(PMIN)
	))

      )))



;;; ----------------------------------------------------------------------
;;;
(defun tid-ff-buffer (&optional verb)
  "find-file-buffer = Load all files listed in buffer. Point is not
preserved.

Input
  verb	,if non-nil, enable verbose messages

References:
  tid-:loaded-files
  tid-:rejected-files

Returns:
  \(count err not-loaded-string\)
   count		,how many files actually loaded
   err			,error while load
   not-loaded-string	,files that had problems.
"
  (interactive)
  (let* ((loop		t)
	 (count		0)
	 (verb		(or verb (interactive-p)))
	 (sub-level	tid-:comment-start-level)
	 (ignore-re     (concat tid-:comment-re))
	 (empty-re      "^[ \t]*$")
	 buffer
	 bp
	 not-loaded
	 state-file
	 load				;file ont the line to be processed
	 maxp				;max point
	 word
	 err				;per file basis
	 ERR				;return status
	 )


    (setq   tid-:loaded-files   nil	;<< reset GLOBALS
	    tid-:rejected-files	nil)

    (run-hooks 'tid-recover-before-hook)
    (PMIN)				;there is *no* save excursion

    (while (not (eobp))

      (setq bp (point)  err nil)	;BEG of line
      (setq maxp (save-excursion (end-of-line) (point)))
      (beginning-of-line)

      (catch 'next
        ;; ... ... ... ... ... ... ... ... ... ... ... ... .. comments ...
	(if (looking-at empty-re)	;emty lines
	    (throw 'next t))

	(if (null (looking-at ignore-re)) ;no comment in this line
	    nil
	  (setq maxp  (match-beginning sub-level))
;;	  (d! "ignore" maxp (read-full-line))
	  )

	(if (= maxp bp)			;full comment line ?
	    (throw 'next t))

        ;; ... ... ... ... ... ... ... ... ... ... ...  read file name ...
	;;  Now load the file, raise err if not loaded
	;;  - remove whitespace at front first

	(if (looking-at "[ \t]+")
	    (skip-chars-forward " \t" maxp))
	(setq word (tid-read-word))
;;;	(d! "W" (point) maxp  word)

	;; --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``--
	(if (null word)
	    nil				;no word found
	  (setq load (expand-file-name word))

;;;	  (d! "buffer?" (get-file-buffer load) (file-loadable-p load) load)

	  (if (or (null load)
		  (get-file-buffer load))
	      nil			;already in emacs

	    (if (not (file-loadable-p load))
		(setq err t)
	      (if (condition-case nil
		      (progn
			(find-file-noselect load)
			t)
		    (error
		     (setq err t)
		     nil))
		  (progn
		    (setq count (1+ count))
		    (list-prepend tid-:loaded-files word))
		))

;;;	    (d! "load" err load )

	    ))

	;; --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``-- --``--
	(if (null err)
	    nil				;no errors to report
	  (setq ERR t)
	  (list-prepend tid-:rejected-files word)
	  (and (interactive-p)
	       (tid-line-property-error))
	  (setq  not-loaded
		 (concat
		  (or not-loaded "")	;start value
		  (or
		   (file-name-nondirectory load)
		   "[nil-word]")
		  " ")))

	  )				;catch line
      (forward-line 1)
      )					;while loop
;;;    (d! "load-end" count ERR not-loaded)
    (list count ERR not-loaded)
    ))

;;}}}


(provide   'tinydesk)
(run-hooks 'tid-load-hook)
;;; ............................................................. &eof ...
